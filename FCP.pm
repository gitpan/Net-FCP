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
data, it currently falls short of what it should provide (intelligent uri
following, splitfile downloads, healing...)

=head2 IMPORT TAGS

Nothing much can be "imported" from this module right now. There are,
however, certain "import tags" that can be used to select the event model
to be used.

Event models are implemented as modules under the C<Net::FCP::Event::xyz>
class, where C<xyz> is the event model to use. The default is C<Event> (or
later C<Auto>).

The import tag to use is named C<event=xyz>, e.g. C<event=Event>,
C<event=Glib> etc.

You should specify the event module to use only in the main program.

=head2 THE Net::FCP CLASS

=over 4

=cut

package Net::FCP;

use Carp;

$VERSION = 0.06;

no warnings;

our $EVENT = Net::FCP::Event::Auto::;
$EVENT = Net::FCP::Event::Event;#d#

sub import {
   shift;

   for (@_) {
      if (/^event=(\w+)$/) {
         $EVENT = "Net::FCP::Event::$1";
      }
   }
   eval "require $EVENT";
   die $@ if $@;
}

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

=item $meta = Net::FCP::parse_metadata $string

Parse a metadata string and return it.

The metadata will be a hashref with key C<version> (containing
the mandatory version header entries).

All other headers are represented by arrayrefs (they can be repeated).

Since this is confusing, here is a rather verbose example of a parsed
manifest:

   (
      version => { revision => 1 },
      document => [
                    {
                      "info.format" => "image/jpeg",
                      name => "background.jpg",
                      "redirect.target" => "freenet:CHK\@ZcagI,ra726bSw"
                    },
                    {
                      "info.format" => "text/html",
                      name => ".next",
                      "redirect.target" => "freenet:SSK\@ilUPAgM/TFEE/3"
                    },
                    {
                      "info.format" => "text/html",
                      "redirect.target" => "freenet:CHK\@8M8Po8ucwI,8xA"
                    }
                  ]
   )

=cut

sub parse_metadata {
   my $meta;

   my $data = shift;
   if ($data =~ /^Version\015?\012/gc) {
      my $hdr = $meta->{version} = {};

      for (;;) {
         while ($data =~ /\G([^=\015\012]+)=([^\015\012]*)\015?\012/gc) {
            my ($k, $v) = ($1, $2);
            my @p = split /\./, tolc $k, 3;

            $hdr->{$p[0]}               = $v if @p == 1; # lamest code I ever wrote
            $hdr->{$p[0]}{$p[1]}        = $v if @p == 2;
            $hdr->{$p[0]}{$p[1]}{$p[2]} = $v if @p == 3;
            die "FATAL: 4+ dot metadata"     if @p >= 4;
         }

         if ($data =~ /\GEndPart\015?\012/gc) {
            # nop
         } elsif ($data =~ /\GEnd\015?\012/gc) {
            last;
         } elsif ($data =~ /\G([A-Za-z0-9.\-]+)\015?\012/gcs) {
            push @{$meta->{tolc $1}}, $hdr = {};
         } elsif ($data =~ /\G(.*)/gcs) {
            die "metadata format error ($1)";
         }
      }
   }

   #$meta->{tail} = substr $data, pos $data;

   $meta;
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
   $self->{port} ||= $ENV{FREDPORT} || 8481;

   #$self->{nodehello} = $self->client_hello
   #   or croak "unable to get nodehello from node\n";

   $self;
}

sub progress {
   my ($self, $txn, $type, $attr) = @_;
   warn "progress<$txn,$type," . (join ":", %$attr) . ">\n";
}

=item $txn = $fcp->txn(type => attr => val,...)

The low-level interface to transactions. Don't use it.

Here are some examples of using transactions:

The blocking case, no (visible) transactions involved:

   my $nodehello = $fcp->client_hello;

A transaction used in a blocking fashion:
   
   my $txn = $fcp->txn_client_hello;
   ...
   my $nodehello = $txn->result;

Or shorter:

   my $nodehello = $fcp->txn_client_hello->result;

Setting callbacks:

   $fcp->txn_client_hello->cb(
      sub { my $nodehello => $_[0]->result }
   );

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

   $self->txn (generate_chk => data => "$data$metadata", metadata_length => length $metadata);
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

=item ($metadata, $data) = @{ $fcp->client_get ($uri, $htl, $removelocal)

Fetches a (small, as it should fit into memory) file from
freenet. C<$meta> is the metadata (as returned by C<parse_metadata> or
C<undef>).

Due to the overhead, a better method to download big files should be used.

  my ($meta, $data) = @{
     $fcp->client_get (
        "freenet:CHK@hdXaxkwZ9rA8-SidT0AN-bniQlgPAwI,XdCDmBuGsd-ulqbLnZ8v~w"
     )
  };

=cut

_txn client_get => sub {
   my ($self, $uri, $htl, $removelocal) = @_;

   $self->txn (client_get => URI => $uri, hops_to_live => ($htl || 15), remove_local_key => $removelocal ? "true" : "false");
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

use Fcntl;
use Socket;

=item new arg => val,...

Creates a new C<Net::FCP::Txn> object. Not normally used.

=cut

sub new {
   my $class = shift;
   my $self = bless { @_ }, $class;

   $self->{signal} = $EVENT->new_signal;

   $self->{fcp}{txn}{$self} = $self;

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

   socket my $fh, PF_INET, SOCK_STREAM, 0
      or Carp::croak "unable to create new tcp socket: $!";
   binmode $fh, ":raw";
   fcntl $fh, F_SETFL, O_NONBLOCK;
   connect $fh, (sockaddr_in $self->{fcp}{port}, inet_aton $self->{fcp}{host})
      and !$!{EWOULDBLOCK}
      and !$!{EINPROGRESS}
      and Carp::croak "FCP::txn: unable to connect to $self->{fcp}{host}:$self->{fcp}{port}: $!\n";

   $self->{sbuf} = 
      "\x00\x00\x00\x02"
      . Net::FCP::touc $self->{type}
      . "\012$attr$data";

   #$fh->shutdown (1); # freenet buggy?, well, it's java...
   
   $self->{fh} = $fh;
   
   $self->{w} = $EVENT->new_from_fh ($fh)->cb(sub { $self->fh_ready_w })->poll(0, 1, 1);
   
   $self;
}

=item $txn = $txn->cb ($coderef)

Sets a callback to be called when the request is finished. The coderef
will be called with the txn as it's sole argument, so it has to call
C<result> itself.

Returns the txn object, useful for chaining.

Example:

   $fcp->txn_client_get ("freenet:CHK....")
      ->userdata ("ehrm")
      ->cb(sub {
         my $data = shift->result;
      });

=cut

sub cb($$) {
   my ($self, $cb) = @_;
   $self->{cb} = $cb;
   $self;
}

=item $txn = $txn->userdata ([$userdata])

Set user-specific data. This is useful in progress callbacks. The data can be accessed
using C<< $txn->{userdata} >>.

Returns the txn object, useful for chaining.

=cut

sub userdata($$) {
   my ($self, $data) = @_;
   $self->{userdata} = $data;
   $self;
}

sub fh_ready_w {
   my ($self) = @_;

   my $len = syswrite $self->{fh}, $self->{sbuf};

   if ($len > 0) {
      substr $self->{sbuf}, 0, $len, "";
      unless (length $self->{sbuf}) {
         fcntl $self->{fh}, F_SETFL, 0;
         $self->{w}->cb(sub { $self->fh_ready_r })->poll (1, 0, 1);
      }
   } elsif (defined $len) {
      $self->throw (Net::FCP::Exception->new (network_error => { reason => "unexpected end of file while writing" }));
   } else {
      $self->throw (Net::FCP::Exception->new (network_error => { reason => "$!" }));
   }
}

sub fh_ready_r {
   my ($self) = @_;

   if (sysread $self->{fh}, $self->{buf}, 65536, length $self->{buf}) {
      for (;;) {
         if ($self->{datalen}) {
            #warn "expecting new datachunk $self->{datalen}, got ".(length $self->{buf})."\n";#d#
            if (length $self->{buf} >= $self->{datalen}) {
               $self->rcv_data (substr $self->{buf}, 0, delete $self->{datalen}, "");
            } else {
               last;
            }
         } elsif ($self->{buf} =~ s/^DataChunk\015?\012Length=([0-9a-fA-F]+)\015?\012Data\015?\012//) {
            $self->{datalen} = hex $1;
            #warn "expecting new datachunk $self->{datalen}\n";#d#
         } elsif ($self->{buf} =~ s/^([a-zA-Z]+)\015?\012(?:(.+?)\015?\012)?EndMessage\015?\012//s) {
            $self->rcv ($1, {
                  map { my ($a, $b) = split /=/, $_, 2; ((Net::FCP::tolc $a), $b) }
                      split /\015?\012/, $2
            });
         } else {
            last;
         }
      }
   } else {
      $self->eof;
   }
}

sub rcv_data {
   my ($self, $chunk) = @_;

   $self->{data} .= $chunk;

   $self->progress ("data", { chunk => length $chunk, total => length $self->{data}, end => $self->{datalength} });
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

# used as a default exception thrower
sub rcv_throw_exception {
   my ($self, $attr, $type) = @_;
   $self->throw (Net::FCP::Exception->new ($type, $attr));
}

*rcv_failed       = \&Net::FCP::Txn::rcv_throw_exception;
*rcv_format_error = \&Net::FCP::Txn::rcv_throw_exception;

sub throw {
   my ($self, $exc) = @_;

   $self->{exception} = $exc;
   $self->set_result (1);
   $self->eof; # must be last to avoid loops
}

sub set_result {
   my ($self, $result) = @_;

   unless (exists $self->{result}) {
      $self->{result} = $result;
      $self->{cb}->($self) if exists $self->{cb};
      $self->{signal}->send;
   }
}

sub eof {
   my ($self) = @_;

   delete $self->{w};
   delete $self->{fh};

   delete $self->{fcp}{txn}{$self};

   $self->set_result; # just in case
}

sub progress {
   my ($self, $type, $attr) = @_;
   $self->{fcp}->progress ($self, $type, $attr);
}

=item $result = $txn->result

Waits until a result is available and then returns it.

This waiting is (depending on your event model) not very efficient, as it
is done outside the "mainloop".

=cut

sub result {
   my ($self) = @_;

   $self->{signal}->wait while !exists $self->{result};

   die $self->{exception} if $self->{exception};

   return $self->{result};
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

package Net::FCP::Txn::GetPut;

# base class for get and put

use base Net::FCP::Txn;

*rcv_uri_error        = \&Net::FCP::Txn::rcv_throw_exception;
*rcv_route_not_found  = \&Net::FCP::Txn::rcv_throw_exception;

sub rcv_restarted {
   my ($self, $attr, $type) = @_;

   delete $self->{datalength};
   delete $self->{metalength};
   delete $self->{data};

   $self->progress ($type, $attr);
}

package Net::FCP::Txn::ClientGet;

use base Net::FCP::Txn::GetPut;

*rcv_data_not_found = \&Net::FCP::Txn::rcv_throw_exception;

sub rcv_data_found {
   my ($self, $attr, $type) = @_;

   $self->progress ($type, $attr);

   $self->{datalength} = hex $attr->{data_length};
   $self->{metalength} = hex $attr->{metadata_length};
}

sub eof {
   my ($self) = @_;

   if ($self->{datalength} == length $self->{data}) {
      my $data = delete $self->{data};
      my $meta = Net::FCP::parse_metadata substr $data, 0, $self->{metalength}, "";

      $self->set_result ([$meta, $data]);
   } elsif (!exists $self->{result}) {
      $self->throw (Net::FCP::Exception->new (short_data => {
                                                 reason   => "unexpected eof or internal node error",
                                                 received => length $self->{data},
                                                 expected => $self->{datalength},
                                              }));
   }
}

package Net::FCP::Txn::ClientPut;

use base Net::FCP::Txn::GetPut;

*rcv_size_error    = \&Net::FCP::Txn::rcv_throw_exception;
*rcv_key_collision = \&Net::FCP::Txn::rcv_throw_exception;

sub rcv_pending {
   my ($self, $attr, $type) = @_;
   $self->progress ($type, $attr);
}

sub rcv_success {
   my ($self, $attr, $type) = @_;
   $self->set_result ($attr);
}

package Net::FCP::Exception;

use overload
   '""' => sub {
      "Net::FCP::Exception<<$_[0][0]," . (join ":", %{$_[0][1]}) . ">>\n";
   };

sub new {
   my ($class, $type, $attr) = @_;

   bless [Net::FCP::tolc $type, { %$attr }], $class;
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

