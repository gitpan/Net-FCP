package Net::FCP::Event::Tk;

use Tk ();

my $mw;

sub new_from_fh {
   my ($class, $fh) = @_;

   bless { fh => $fh }, $class;
}

sub cb {
   my ($self, $cb) = @_;
   $self->{cb} = $cb;
   $self;
}

sub poll {
   my ($self, $r, $w, $e) = @_;

   # we ignore $e, it's impossible
   
   my $cb = \$self->{cb}; # avoid $self-reference

   Tk::Event::IO::fileevent undef, $self->{fh}, readable => ""; # a mess!
   Tk::Event::IO::fileevent undef, $self->{fh}, writable => ""; # even more so!

   Tk::Event::IO::fileevent undef, $self->{fh}, readable => sub { $$cb->() } if $r;
   Tk::Event::IO::fileevent undef, $self->{fh}, writable => sub { $$cb->() } if $w;

   $self;
}

sub DESTROY {
   my ($self) = @_;

   if ($self->{fh}) {
      Tk::Event::IO::fileevent undef, $self->{fh}, readable => "";
      Tk::Event::IO::fileevent undef, $self->{fh}, writable => "";
   }

   undef $mw;
}

#############

sub new_signal {
   my $class = shift;

   bless \my $x, $class;
}

sub send {
   ${$_[0]}++;
}

sub wait {
   unless ($mw) {
      $mw = new MainWindow;
      $mw->withdraw;
   }

   Tk::DoOneEvent (0) while !${$_[0]};
}

1;

