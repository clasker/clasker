#!/usr/bin/env perl
#perl clasker-ticket.pl create -P description='"fdsafsafdA"' -P noseque=algo
#((noseque . algo)(description . "fdsafsafdA"))
# TODO 02:55 <davazp> -description "xx"   === -P description '"xx"'
# TODO obey paths

use Getopt::Long;
use strict;
use warnings;
use Data::Dumper;

sub escape {
  my $str = shift;
  $str =~ s/\n/\\n/gm;
  $str;
}

sub property {
  my ($prop_name, $prop_value) = @_;
  $prop_value = escape($prop_value);
  qq|($prop_name . $prop_value)|;
}

sub create_ticket {
  my $ticket = shift;
  open my $fh , '>', "clasker-ticket".time  or die "fdsaf";
  print $fh $ticket;
  close $fh;
}
sub create {
  my %options = @_;
  my $ticket = '(';
  while (my ($k,$v) = each %options) {
    $ticket .= property( $k, $v);
  }
  $ticket .= ")\n";
  create_ticket($ticket);
}

my %props;

my $path = $ENV{CLASKER_PATH} || '~/.clasker./';
GetOptions("s|path=s" => \$path, "P|property=s" => \%props);

my $actions = { create => \&create };

my $action = shift;
die "Unrecognized action" unless exists $actions->{$action};
print $actions->{$action}->(%props) , "\n";
