package Mojo::Autotask::Role::Query;
use Mojo::Base -role;

use Mojo::Util 'dumper';

use Data::Table;

sub to_date {
  my ($self, $format) = @_;
  $format ||= '%m/%d/%Y %H:%M:%S';
  $self->each(sub{
    my $h = $_;
    $h->{$_} = $h->{$_}->strftime($format) foreach grep { ref $h->{$_} eq 'Time::Piece' } keys %$h;
  });
}

sub lookup {
  my ($self, $ws, @args) = @_;
  return $self unless $ws;
  my $data = {};
  $self->map(sub {
    foreach my $arg ( @args ) {
      if ( ref $arg eq 'ARRAY' || ! ref $arg ) {
        foreach my $col ( ref $arg ? @$arg : $arg ) {
          last unless $col;
          $col.='_ref' unless $col =~ /_ref$/;
          next unless $_->{$col};
          my ($entity, $id) = split /:/, $_->{$col};
          next unless $entity && defined $id;
          $data->{$entity} ||= $ws->query($entity)->hashify('id');
          foreach my $l ( keys %{$data->{$entity}->{$id}} ) {
            $_->{"${col}_$l"} = $data->{$entity}->{$id}->{$l};
          }
        }
      } elsif ( ref $arg eq 'HASH' ) {
        while ( my ($col, $options) = each %$arg ) {
          last unless $col;
          $col.='_ref' unless $col =~ /_ref$/;
          next unless $_->{$col};
          my ($entity, $id) = split /:/, $_->{$col};
          next unless $entity && defined $id;
          $data->{$entity} ||= $ws->query($entity, grep { ref eq 'HASH' } ref $options eq 'ARRAY' ? @$options : $options)->hashify('id');
          foreach my $l ( (grep { ref eq 'ARRAY' || ! ref } ref $options eq 'ARRAY' ? @$options : $options) || keys %{$data->{$entity}->{$id}} ) {
            $_->{"${col}_$l"} = $data->{$entity}->{$id}->{$l};
          }
        }
      }
    }
  });
  return $self;
}

# tablify() will always return a Mojo::ByteStream which can be tap()'d
# $c->grep()->tablify($columns, [qw/+Sparklines +Text/], sub{$_->group->pivot->text(sub{$_->add("ABC")})->tap(sub{$ua->post('/upload') and $sendgrid->send})->to_string;
sub tablify {
  my ($self, $columns) = (shift, shift);
  @$columns = map { $_ } sort keys %{$self->first} unless $columns;
  Role::Tiny->apply_roles_to_package('Data::Table', 'Mojo::Base::Role::Base');
  return Data::Table->new($self->map(sub{my $data=$_; ref $data eq 'ARRAY' ? [@$_[0..$#$columns-1]] : [map{$data->{$_}}@$columns]})->to_array, $columns);
}

1;
