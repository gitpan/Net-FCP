=head1 NAME

Net::FCP - http://freenet.sf.net client protocol

=head1 SYNOPSIS

 use Net::FCP;

 my $fcp = new Net::FCP;

 my $ni = $fcp->txn_node_info->result;
 my $ni = $fcp->node_info;

=head1 DESCRIPTION

See L<http://freenet.sourceforge.net/index.php?page=fcp> for a description
of what the messages do. I am too lazy to document all this here.

=head1 WARNING

This module is alpha. While it probably won't destroy (much :) of your
data, it currently works only with the Event module (alkthough the event
mechanism is fully pluggable).

=head2 THE Net::FCP CLASS

=over 4

=cut

package Net::FCP;

use Carp;
use IO::Socket::INET;

$VERSION = 0.02;

sub event_reg_cb {
   my ($obj) = @_;
   require Event;

   $obj->{eventdata} = Event->io (
      fd   => $obj->{fh},
      poll => 'r',
      cb   => sub {
         $obj->fh_ready;
      },
   );
}

sub event_unreg_cb {
   $_[0]{eventdata}
      and (delete $_[0]{eventdata})->cancel;
}

sub event_wait_cb {
   Event::one_event();
}

$regcb = \&event_reg_cb;
$unregcb = \&event_unreg_cb;
$waitcb = \&event_wait_cb;

sub touc($) {
   local $_ = shift;
   1 while s/((?:^|_)(?:svk|chk|uri)(?:_|$))/\U$1/;
   s/(?:^|_)(.)/\U$1/g;
   $_;
}

sub tolc($) {
   local $_ = shift;
   s/(?<=[a-z])(?=[A-Z])/_/g;
   lc $_;
}

=item $fcp = new Net::FCP [host => $host][, port => $port]

Create a new virtual FCP connection to the given host and port (default
127.0.0.1:8481, or the environment variables C<FREDHOST> and C<FREDPORT>).

Connections are virtual because no persistent physical connection is
established. However, the existance of the node is checked by executing a
C<ClientHello> transaction.

=cut

sub new {
   my $class = shift;
   my $self = bless { @_ }, $class;

   $self->{host} ||= $ENV{FREDHOST} || "127.0.0.1";
   $self->{port} ||= $ENV{FREDPORt} || 8481;

   $self->{nodehello} = $self->client_hello
      or croak "unable to get nodehello from node\n";

   $self;
}

=item $txn = $fcp->txn(type => attr => val,...)

The low-level interface to transactions. Don't use it.

=cut

sub txn {
   my ($self, $type, %attr) = @_;

   $type = touc $type;

   my $txn = "Net::FCP::Txn::$type"->new(fcp => $self, type => tolc $type, attr => \%attr);

   $txn;
}

sub _txn($&) {
   my ($name, $sub) = @_;
   *{"$name\_txn"} = $sub;
   *{$name} = sub { $sub->(@_)->result };
}

=item $txn = $fcp->txn_client_hello

=item $nodehello = $fcp->client_hello

Executes a ClientHello request and returns it's results.

   {
     max_file_size => "5f5e100",
     node => "Fred,0.6,1.46,7050"
     protocol => "1.2",
   }

=cut

_txn client_hello => sub {
   my ($self) = @_;

   $self->txn ("client_hello");
};

=item $txn = $fcp->txn_client_info

=item $nodeinfo = $fcp->client_info

Executes a ClientInfo request and returns it's results.

   {
     active_jobs => "1f",
     allocated_memory => "bde0000",
     architecture => "i386",
     available_threads => 17,
     datastore_free => "5ce03400",
     datastore_max => "2540be400",
     datastore_used => "1f72bb000",
     estimated_load => 52,
     free_memory => "5cc0148",
     is_transient => "false",
     java_name => "Java HotSpot(_T_M) Server VM",
     java_vendor => "http://www.blackdown.org/",
     java_version => "Blackdown-1.4.1-01",
     least_recent_timestamp => "f41538b878",
     max_file_size => "5f5e100",
     most_recent_timestamp => "f77e2cc520"
     node_address => "1.2.3.4",
     node_port => 369,
     operating_system => "Linux",
     operating_system_version => "2.4.20",
     routing_time => "a5",
   }

=cut

_txn client_info => sub {
   my ($self) = @_;

   $self->txn ("client_info");
};

=item $txn = $fcp->txn_generate_chk ($metadata, $data)

=item $uri = $fcp->generate_chk ($metadata, $data)

Creates a new CHK, given the metadata and data. UNTESTED.

=cut

_txn generate_chk => sub {
   my ($self, $metadata, $data) = @_;

   $self->txn (generate_chk => data => "$data$metadata", meta_data_length => length $metadata);
};

=item $txn = $fcp->txn_generate_svk_pair

=item ($public, $private) = @{ $fcp->generate_svk_pair }

Creates a new SVK pair. Returns an arrayref.

   [
     "hKs0-WDQA4pVZyMPKNFsK1zapWY",
     "ZnmvMITaTXBMFGl4~jrjuyWxOWg"
   ]

=cut

_txn generate_svk_pair => sub {
   my ($self) = @_;

   $self->txn ("generate_svk_pair");
};

=item $txn = $fcp->txn_insert_private_key ($private)

=item $uri = $fcp->insert_private_key ($private)

Inserts a private key. $private can be either an insert URI (must start
with freenet:SSK@) or a raw private key (i.e. the private value you get back
from C<generate_svk_pair>).

Returns the public key.

UNTESTED.

=cut

_txn insert_private_key => sub {
   my ($self, $privkey) = @_;

   $self->txn (invert_private_key => private => $privkey);
};

=item $txn = $fcp->txn_get_size ($uri)

=item $length = $fcp->get_size ($uri)

Finds and returns the size (rounded up to the nearest power of two) of the
given document.

UNTESTED.

=cut

_txn get_size => sub {
   my ($self, $uri) = @_;

   $self->txn (get_size => URI => $uri);
};

=item $txn = $fcp->txn_client_get ($uri [, $htl = 15 [, $removelocal = 0]])

=item ($data, $metadata) = @{ $fcp->client_get ($uri, $htl, $removelocal)

Fetches a (small, as it should fit into memory) file from freenet.

Due to the overhead, a better method to download big fiels should be used.

  my ($data, $meta) = @{
     $fcp->client_get (
        "freenet:CHK@hdXaxkwZ9rA8-SidT0AN-bniQlgPAwI,XdCDmBuGsd-ulqbLnZ8v~w"
     )
  };

=cut

_txn client_get => sub {
   my ($self, $uri, $htl, $removelocal) = @_;

   $self->txn (client_get => URI => $uri, hops_to_live => ($htl || 15), remove_local => $removelocal*1);
};

=item MISSING: ClientPut

=back

=head2 THE Net::FCP::Txn CLASS

All requests (or transactions) are executed in a asynchroneous way (LIE:
uploads are blocking). For each request, a C<Net::FCP::Txn> object is
created (worse: a tcp connection is created, too).

For each request there is actually a different subclass (and it's possible
to subclass these, although of course not documented).

The most interesting method is C<result>.

=over 4

=cut

package Net::FCP::Txn;

=item new arg => val,...

Creates a new C<Net::FCP::Txn> object. Not normally used.

=cut

sub new {
   my $class = shift;
   my $self = bless { @_ }, $class;

   my $attr = "";
   my $data = delete $self->{attr}{data};

   while (my ($k, $v) = each %{$self->{attr}}) {
      $attr .= (Net::FCP::touc $k) . "=$v\012"
   }

   if (defined $data) {
      $attr .= "DataLength=" . (length $data) . "\012";
      $data = "Data\012$data";
   } else {
      $data = "EndMessage\012";
   }

   my $fh = new IO::Socket::INET
      PeerHost => $self->{fcp}{host},
      PeerPort => $self->{fcp}{port}
      or Carp::croak "FCP::txn: unable to connect to $self->{fcp}{host}:$self->{fcp}{port}: $!\n";

   binmode $fh, ":raw";

   if (0) {
      print
         Net::FCP::touc $self->{type}, "\012",
         $attr,
         $data, "\012";
   }

   print $fh
      "\x00\x00", "\x00\x02", # SESSID, PRESID
      Net::FCP::touc $self->{type}, "\012",
      $attr,
      $data;

   #$fh->shutdown (1); # freenet buggy?, well, it's java...
   
   $self->{fh} = $fh;
   
   $Net::FCP::regcb->($self);
   
   $self;
}

sub fh_ready {
   my ($self) = @_;

   if (sysread $self->{fh}, $self->{buf}, 65536, length $self->{buf}) {
      for (;;) {
         if ($self->{datalen}) {
            if (length $self->{buf} >= $self->{datalen}) {
               $self->rcv_data (substr $self->{buf}, 0, $self->{datalen}, "");
            } else {
               last;
            }
         } elsif ($self->{buf} =~ s/^DataChunk\015?\012Length=([0-9a-fA-F]+)\015?\012Data\015?\012//) {
            $self->{datalen} = hex $1;
         } elsif ($self->{buf} =~ s/^([a-zA-Z]+)\015?\012(.*?)\015?\012EndMessage\015?\012//s) {
            $self->rcv ($1, {
                  map { my ($a, $b) = split /=/, $_, 2; ((Net::FCP::tolc $a), $b) }
                      split /\015?\012/, $2
            });
         } else {
            last;
         }
      }
   } else {
      $Net::FCP::unregcb->($self);
      delete $self->{fh};
      $self->eof;
   }
}

sub rcv_data {
   my ($self, $chunk) = @_;

   $self->{data} .= $chunk;
}

sub rcv {
   my ($self, $type, $attr) = @_;

   $type = Net::FCP::tolc $type;

   #use PApp::Util; warn PApp::Util::dumpval [$type, $attr];

   if (my $method = $self->can("rcv_$type")) {
      $method->($self, $attr, $type);
   } else {
      warn "received unexpected reply type '$type' for '$self->{type}', ignoring\n";
   }
}

sub set_result {
   my ($self, $result) = @_;

   $self->{result} = $result unless exists $self->{result};
}

sub eof {
   my ($self) = @_;
   $self->set_result;
}

=item $result = $txn->result

Waits until a result is available and then returns it.

This waiting is (depending on your event model) not very efficient, as it
is done outside the "mainloop".

=cut

sub result {
   my ($self) = @_;

   $Net::FCP::waitcb->() while !exists $self->{result};

   return $self->{result};
}

sub DESTROY {
   $Net::FCP::unregcb->($_[0]);
}

package Net::FCP::Txn::ClientHello;

use base Net::FCP::Txn;

sub rcv_node_hello {
   my ($self, $attr) = @_;

   $self->set_result ($attr);
}

package Net::FCP::Txn::ClientInfo;

use base Net::FCP::Txn;

sub rcv_node_info {
   my ($self, $attr) = @_;

   $self->set_result ($attr);
}

package Net::FCP::Txn::GenerateCHK;

use base Net::FCP::Txn;

sub rcv_success {
   my ($self, $attr) = @_;

   $self->set_result ($attr);
}

package Net::FCP::Txn::GenerateSVKPair;

use base Net::FCP::Txn;

sub rcv_success {
   my ($self, $attr) = @_;

   $self->set_result ([$attr->{PublicKey}, $attr->{PrivateKey}]);
}

package Net::FCP::Txn::InvertPrivateKey;

use base Net::FCP::Txn;

sub rcv_success {
   my ($self, $attr) = @_;

   $self->set_result ($attr->{PublicKey});
}

package Net::FCP::Txn::GetSize;

use base Net::FCP::Txn;

sub rcv_success {
   my ($self, $attr) = @_;

   $self->set_result ($attr->{Length});
}

package Net::FCP::Txn::ClientGet;

use base Net::FCP::Txn;

sub rcv_data_found {
   my ($self, $attr) = @_;

   $self->{datalength} = hex $attr->{data_length};
   $self->{metalength} = hex $attr->{meta_data_length};
}

sub eof {
   my ($self) = @_;
   #use PApp::Util; warn PApp::Util::dumpval $self;
   my $data = delete $self->{data};
   $self->set_result ([
      (substr $data, 0, $self->{datalength}-$self->{metalength}),
      (substr $data, $self->{datalength}-$self->{metalength}),
   ]);
}

=back

=head1 SEE ALSO

L<http://freenet.sf.net>.

=head1 BUGS

=head1 AUTHOR

 Marc Lehmann <pcg@goof.com>
 http://www.goof.com/pcg/marc/

=cut

1;

