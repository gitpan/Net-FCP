=head1 NAME

Net::FCP::Util - utility functions.

=head1 SYNOPSIS

 use Net::FCP::Util;


=head1 DESCRIPTION

=back

=over 4

=cut

package Net::FCP::Util;

use Carp ();
use Digest::SHA1;
use MIME::Base64 ();

no warnings;

sub log2($$) { # n, minlog
   my ($n, $b) = @_;

   $b++ while 1 << $b < $n;

   $b;
}

=item decode_base64 $string

Decode freenet's perverted version of base64.

=cut

sub decode_base64($) {
   my $s = shift;

   $s =~ y%~\-%+/%;
   MIME::Base64::decode_base64 "$s======";
}

=item encode_base64 $data

Encode into freenet's perverted version of base64.

=cut

sub encode_base64($) {
   my $s = MIME::Base64::encode_base64 shift, "";
   $s =~ s/=+$//;
   $s =~ y%+/%~\-%;
   $s;
}

=item generate_chk_key $metadata, $data

Generate and return they key portion that would be used in the CHK (as
binary). This can be used to verify contents of a CHK, since this key is a
hash over the data.

(This function assumes a 128 bit key, which seems standard in freenet).

=cut

sub generate_chk_key($$) {
   my $d = new Digest::SHA1;

   $d->add ($_[0]);
   $d->add ($_[1]);
   $d = $d->digest;

   my $k = new Digest::SHA1;
   $k->add ("\x00" x 1); # only one iteration
   $k->add ($d);
   
   substr $k->digest, 0, 16; # extract leading 128 bit
}

=item extract_chk_key $uri

Extract the key portion of a CHK (in binary). Useful to compare against
the output of generate_chk_key.

=cut

sub extract_chk_key($) {
   $_[0] =~ /CHK\@[a-zA-Z0-9~\-]{31},([a-zA-Z0-9~\-]{22})/
      or Carp::croak "unable to parse CHK key from '$_[0]'";

   decode_base64 $1;
}

=back

=head1 SEE ALSO

L<Net::FCP>.

=head1 BUGS

Not heavily tested.

=head1 AUTHOR

 Marc Lehmann <pcg@goof.com>
 http://www.goof.com/pcg/marc/

=cut

1;

