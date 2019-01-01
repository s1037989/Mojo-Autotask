package Mojo::Autotask::ExecuteCommand;
use Mojo::Base 'Mojo::EventEmitter';

use Mojo::URL;
use Mojo::Util 'camelize';
use Mojolicious::Validator;

use Carp;
use Scalar::Util 'blessed';

has zone => sub { die };
has ec_url => sub { shift->zone->path('/Autotask/AutotaskExtend/ExecuteCommand.aspx') };

sub AUTOLOAD {
  my ($self, @args) = @_;

  my ($package, $method) = our $AUTOLOAD =~ /^(.+)::(.+)$/;
  Carp::croak "Undefined subroutine &${package}::$method called"
    unless blessed $self && $self->isa(__PACKAGE__);

  my $validator  = Mojolicious::Validator->new;
  my $v = $validator->validation;

  $v->input({Code => camelize($method), @args});
  $v->required('Code')->in(qw/OpenTicketDetail OpenProject OpenTicketTime OpenTaskDetail OpenAppointment OpenKBArticle OpenOpportunity OpenContract OpenToDo OpenSalesOrder OpenServiceCall OpenAccount OpenContact EditTimeEntry EditAccount EditContact NewAccountNote NewContact NewTicket EditInstalledProduct NewInstalledProduct TimeOffRequest IPDW/);
  return if $v->has_error;
  return $v->has_error if $v->param('Code') eq 'OpenTicketDetail' && not grep { $v->optional($_)->is_valid } qw/TicketID TicketNumber GlobalTaskID/;
  return $v->has_error if $v->param('Code') eq 'OpenTaskDetail' && not grep { $v->optional($_)->is_valid } qw/TaskID/;
  return $v->has_error if $v->param('Code') eq 'OpenProject' && not grep { $v->optional($_)->is_valid } qw/ProjectID/;
  return $v->has_error if $v->param('Code') eq 'OpenTicketTime' && not grep { $v->optional($_)->is_valid } qw/TicketID/;
  return $v->has_error if $v->param('Code') eq 'OpenAppointment' && not grep { $v->optional($_)->is_valid } qw/AppointmentID/;
  return $v->has_error if $v->param('Code') eq 'OpenKBArticle' && not grep { $v->optional($_)->is_valid } qw/ID/;
  return $v->has_error if $v->param('Code') eq 'OpenOpportunity' && not grep { $v->optional($_)->is_valid } qw/OpportunityID/;
  return $v->has_error if $v->param('Code') eq 'OpenContract' && not grep { $v->optional($_)->is_valid } qw/ContractID/;
  return $v->has_error if $v->param('Code') eq 'OpenToDo' && not grep { $v->optional($_)->is_valid } qw/ToDoID/;
  return $v->has_error if $v->param('Code') eq 'OpenSalesOrder' && not grep { $v->optional($_)->is_valid } qw/SalesOrderID/;
  return $v->has_error if $v->param('Code') eq 'OpenServiceCall' && not grep { $v->optional($_)->is_valid } qw/ServiceCallID/;
  return $v->has_error if $v->param('Code') eq 'OpenAccount' && not grep { $v->optional($_)->is_valid } qw/Phone AccountID AccountName/;
  return $v->has_error if $v->param('Code') eq 'OpenContact' && not grep { $v->optional($_)->is_valid } qw/Email ContactID/;
  return $v->has_error if $v->param('Code') eq 'EditTimeEntry' && not grep { $v->optional($_)->is_valid } qw/WorkEntryID/;
  return $v->has_error if $v->param('Code') eq 'EditAccount' && not grep { $v->optional($_)->is_valid } qw/Phone AccountID/;
  return $v->has_error if $v->param('Code') eq 'EditContact' && not grep { $v->optional($_)->is_valid } qw/ContactID Email FirstName LastName/;
  return $v->has_error if $v->param('Code') eq 'EditInstalledProduct' && not grep { $v->optional($_)->is_valid } qw/InstalledProductID/;
  return $v->has_error if $v->param('Code') eq 'NewInstalledProduct' && not grep { $v->optional($_)->is_valid } qw/Phone AccountID/;
  return $v->has_error if $v->param('Code') eq 'NewAccountNote' && not grep { $v->optional($_)->is_valid } qw/AccountID/;
  return $v->has_error if $v->param('Code') eq 'NewContact' && not grep { $v->optional($_)->is_valid } qw/Phone AccountID/;
  return $v->has_error if $v->param('Code') eq 'NewTicket' && not grep { $v->optional($_)->is_valid } qw/Phone AccountID AccountName GlobalTaskID InstalledProductID/;
  return $v->has_error if $v->param('Code') eq 'TimeOffRequest' && not grep { $v->optional($_)->is_valid } qw/ResourceID ApproverID Tier/;
  return $v->has_error if $v->param('Code') eq 'IPDW' && not grep { $v->optional($_)->is_valid } qw/wizard/;

  #warn Mojo::Util::dumper($v->output);
  my $url = Mojo::URL->new($self->ec_url)->query($v->output);
  $self->emit(ec => $url);
  return $url;
}

1;

=head1 NAME

Mojo::Autotask::ExecuteCommand - Mojo interface to the Autotask ExecuteCommand API

=cut
