package Net::FCP::Event::Coro;

use base Net::FCP::Event::Event;

use Coro;
use Coro::Event;
use Coro::Signal;

#############################################################################

sub new_signal {
   new Coro::Signal;
}

1;

