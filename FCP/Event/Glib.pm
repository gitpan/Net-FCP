package Net::FCP::Event::Glib;

use Glib ();

my $maincontext = Glib::MainContext->default;

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

   remove Glib::Source delete $self->{source} if $self->{source};
   
   my @cond;
   push @cond, "in"  if $r;
   push @cond, "out" if $w;
   push @cond, "err" if $e;

   my $cb = \$self->{cb}; # avoid $self-reference

   $self->{source} = add_watch Glib::IO fileno $self->{fh}, \@cond, sub {
      $$cb->(); 1;
   };

   $self;
}

sub DESTROY {
   my ($self) = @_;

   remove Glib::Source delete $self->{source} if $self->{source};
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
   $maincontext->iteration (1) while !${$_[0]};
}

1;

