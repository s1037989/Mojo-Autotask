use Mojo::Base -base;
use Test::Mojo;

use Mojo::Autotask;
use Mojo::Util 'dumper';

my $at = Mojo::Autotask->new(username => $ENV{AUTOTASK_USERNAME}, password => $ENV{AUTOTASK_PASSWORD});
warn $at->get_threshold_and_usage_info->{EntityReturnInfoResults}->{EntityReturnInfo}->{Message};
warn $at->ec->open_ticket_detail(TicketNumber => 'T20181231.0001');
warn time;
my $cache = $at->cache->expires(86_400);
warn $at->as_collection($cache->query(Account => [
  {
    name => 'AccountName',
    expressions => [{op => 'BeginsWith', value => 'b'}]
  },
]))->size;
warn scalar @{$cache->query('Account')};
warn time;
