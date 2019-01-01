package Mojo::Autotask::Limits;
use Mojo::Base -base;

use Scalar::Util 'blessed';
use Time::Piece;

has 'now';

has Account => 13;
has AccountNote => 12;
has AccountToDo => 12;
has Appointment => 12;
has BillingItem => 12;
has Contact => undef;
has ContractCost => 48;
has ContractMilestone => 48;
has ContractNote => 48;
has Currency => undef;
has InstalledProduct => 12;
has Invoice => 12;
has Opportunity => 48;
has Phase => 24;
has Project => 24;
has ProjectCost => 24;
has ProjectNote => 24;
has PurchaseOrder => 48;
has Quote => 48;
has QuoteTemplate => undef;
has Service => undef;
has ServiceBundle => undef;
has ServiceCall => 12;
has Task => 12;
has TaskNote => 12;
has Ticket => 12;
has TicketCost => 12;
has TicketNote => 12;
has TimeEntry => 12;
has UserDefinedFieldDefinition => undef;
has UserDefinedFieldListItem => undef;

sub grep {
  my $self = shift;
  return map { {name => $_->[0], expressions => [{op => $_->[1], value => $_->[2]}]} } @_;
}

sub in_list {
  my ($self, $name, $op) = (shift, shift, shift);
  return
  {
    elements => [
      {
        name => $name,
        expressions => [{op => $op, value => "$_[0]"}]
      }
    ]
  },
  (map {
    {
      operator => 'OR',
      elements => [
        {
          name => $name,
          expressions => [{op => $op, value => "$_"}]
        }
      ]
    }
  } grep { $_ } @_[1..199])
}

sub limit {
  my ($self, $entity) = @_;
  return () unless $entity;
  my %limits = (
    Account => 'CreateDate',
    AccountToDo => 'CreateDateTime',
    Appointment => 'CreateDateTime',
    BillingItem => 'ItemDate',
    Contact => 'CreateDate',
    ContractCost => 'CreateDate',
    ContractMilestone => 'CreateDate',
    ContractNote => 'LastActivityDate',
    Currency => 'LastModifiedDateTime',
    InstalledProduct => 'CreateDate',
    Invoice => 'CreateDateTime',
    Opportunity => 'CreateDate',
    Phase => 'CreateDate',
    Project => 'CreateDateTime',
    ProjectCost => 'CreateDate',
    ProjectNote => 'LastActivityDate',
    PurchaseOrder => 'CreateDateTime',
    Quote => 'CreateDate',
    QuoteTemplate => 'CreateDate',
    Service => 'CreateDate',
    ServiceBundle => 'CreateDate',
    ServiceCall => 'CreateDateTime',
    Task => 'CreateDateTime',
    TaskNote => 'LastActivityDate',
    Ticket => 'CreateDate',
    TicketCost => 'CreateDate',
    TicketNote => 'LastActivityDate',
    TimeEntry => 'CreateDateTime',
    UserDefinedFieldDefinition => 'CreateDate',
    UserDefinedFieldListItem => 'CreateDate',
  );
  return () unless my $name = $limits{$entity};
  return () unless my $months = $self->$entity;
  my $now = $self->now || localtime;
  return (
    {name => $name, expressions => [{op => 'GreaterThanOrEquals', value => $now->add_months($months * -1)->strftime('%Y-%m-01T00:00:00')}]},
    {name => $name, expressions => [{op => 'LessThan', value => $now->add_months(1)->strftime('%Y-%m-01T00:00:00')}]},
  );
}

sub since {
  my ($self, $entity, $t) = @_;
  return () unless blessed($t) && $t->isa('Time::Piece');
  if ( $entity eq 'Account' ) {
    return {name => 'LastActivityDate', expressions => [{op => 'GreaterThanOrEquals', value => $t->datetime}]};
  } elsif ( $entity eq 'AccountNote' ) {
    return {name => 'LastModifiedDate', expressions => [{op => 'GreaterThanOrEquals', value => $t->datetime}]};
  } elsif ( $entity eq 'AccountToDo' ) {
    return {name => 'LastModifiedDate', expressions => [{op => 'GreaterThanOrEquals', value => $t->datetime}]};
  } elsif ( $entity eq 'Contact' ) { # LOW: OR with LastModifiedDate
    return {name => 'LastActivityDate', expressions => [{op => 'GreaterThanOrEquals', value => $t->datetime}]};
  } elsif ( $entity eq 'ContractNote' ) {
    return {name => 'LastActivityDate', expressions => [{op => 'GreaterThanOrEquals', value => $t->datetime}]};
  } elsif ( $entity eq 'Opportunity' ) {
    return {name => 'LastActivity', expressions => [{op => 'GreaterThanOrEquals', value => $t->datetime}]};
  } elsif ( $entity eq 'Phase' ) {
    return {name => 'LastActivityDateTime', expressions => [{op => 'GreaterThanOrEquals', value => $t->datetime}]};
  } elsif ( $entity eq 'ProjectNote' ) {
    return {name => 'LastActivityDate', expressions => [{op => 'GreaterThanOrEquals', value => $t->datetime}]};
  } elsif ( $entity eq 'Service' ) {
    return {name => 'LastModifiedDate', expressions => [{op => 'GreaterThanOrEquals', value => $t->datetime}]};
  } elsif ( $entity eq 'ServiceBundle' ) {
    return {name => 'LastModifiedDate', expressions => [{op => 'GreaterThanOrEquals', value => $t->datetime}]};
  } elsif ( $entity eq 'ServiceCall' ) {
    return {name => 'LastModifiedDateTime', expressions => [{op => 'GreaterThanOrEquals', value => $t->datetime}]};
  } elsif ( $entity eq 'Task' ) {
    return {name => 'LastActivityDateTime', expressions => [{op => 'GreaterThanOrEquals', value => $t->datetime}]};
  } elsif ( $entity eq 'TaskNote' ) {
    return {name => 'LastActivityDate', expressions => [{op => 'GreaterThanOrEquals', value => $t->datetime}]};
  } elsif ( $entity eq 'Ticket' ) {
    return {name => 'LastActivityDate', expressions => [{op => 'GreaterThanOrEquals', value => $t->datetime}]};
  } elsif ( $entity eq 'TicketNote' ) {
    return {name => 'LastActivityDate', expressions => [{op => 'GreaterThanOrEquals', value => $t->datetime}]};
  } elsif ( $entity eq 'TimeEntry' ) {
    return {name => 'LastModifiedDateTime', expressions => [{op => 'GreaterThanOrEquals', value => $t->datetime}]};
  }
  return ();
}

1;

=head1 NAME

Mojo::Autotask::Limits - Build query filters for L<Mojo::Autotask>

=cut
