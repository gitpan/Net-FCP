package Net::FCP::Event::Event;

require Event;

sub reg_r_cb {
   my ($class, $obj) = @_;
   require Event;

   $obj->{eventdata}[0] = Event->io (
      fd   => $obj->{fh},
      poll => 'r',
      cb   => sub {
         $obj->fh_ready;
      },
   );
}

sub unreg_r_cb {
   $_[1]{eventdata}[0]
      and (delete $_[1]{eventdata}[0])->cancel;
}

sub wait_event {
   Event::one_event();
}

1;
