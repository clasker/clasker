#!/usr/bin/env perl
# Copyright (C) 2012  Raimon Grau

# Author: Raimon Grau <raimonster@gmail.com>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

use Getopt::Long;
use File::Spec;
use strict;
use warnings;
use Fcntl;
use Data::Dumper;
use File::Temp;
use File::Glob ':glob';

sub create_unique_file{
  my $path = shift;
  my $fh;
  my $template = 'clasker-ticket-'.time.'-XXXX';
  return File::Temp->new(TEMPLATE => $template, DIR => $path, UNLINK => 0);
}
sub escape {
  my $str = shift;
  $str =~ s/\n/\\n/gm;
  $str;
}

sub description {
  my ($prop_value) = @_;
  generic_property('description' , '"'. escape($prop_value) . '"' );
}

sub generic_property {
  my ($prop_name, $prop_value) = @_;
  qq|($prop_name . $prop_value)|;
}

sub create_ticket {
  my ($ticket, $path) = @_;
  my $filepath= File::Spec->join($path, 'export');
  my $fh = create_unique_file($filepath);
  print $fh $ticket;
  close $fh;
  print "created => $ticket\n";
}

sub create {
  my %options = @_;
  my $description = delete $options{description};
  my $path = bsd_glob(delete $options{path}, GLOB_TILDE);
  die "description is required to create a ticket" unless $description;
  my $ticket = '(';
  $ticket.= description($description);
  while (my ($k,$v) = each %{$options{property}}) {
    $ticket .= generic_property( $k, $v);
  }
  $ticket .= ")\n";
  create_ticket($ticket, $path);
}

sub help {
  print <<EOH
Usage: $0 action params

Actions:
  create - creates a new ticket with description and properties a
  help   - shows this message

Params:
  -P|property=string    - adds new property with value
  -D|description=string - sets the description of the ticket
  -S|path=string        - overwrites CLASKER_PATH option

Examples:
  create a ticket
    perl clasker-ticket.pl create -D 'hello\\nthis is a description'  -P archived=t
EOH
}

my $actions = {
               create => \&create,
               help => \&help};
my %options = ('path' => $ENV{CLASKER_PATH} || '~/.clasker.d/');

GetOptions(\%options, 'path|S=s', "description|D=s", "property|P=s%");

my $action = shift;
die "Unrecognized action" unless exists $actions->{$action};
print $actions->{$action}->(%options) , "\n";
