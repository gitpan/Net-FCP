package Net::FCP::Event::Event;

use Event ();

sub new_from_fh {
   my ($class, $fh) = @_;
   bless \(my $x = Event->io (
         fd   => $fh,
         poll => 'e',
         cb   => sub { die "no callback set for watcher" },
      )), $class;
}

sub cb {
   my ($self, $cb) = @_;
   $$self->cb ($cb);
   $self;
}

sub poll {
   my ($self, $r, $w, $e) = @_;
   $$self->poll (
     ($r ? "r" : "") .
     ($w ? "w" : "") .
     ($e ? "e" : "")
   );
   $self;
}

sub DESTROY {
   my ($self) = @_;

   $$self->cancel;
}

#############

sub new_signal {
   my $class = shift;

   bless \my $x, $class;
}

sub send {
   # nop
}

sub wait {
   Event::one_event();
}

1;

