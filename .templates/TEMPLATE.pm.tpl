#!/usr/bin/env perl
# @(#)TEMPLATE.pm.tpl
# @author (>>>USER_NAME<<<) <(>>>AUTHOR<<<)>

package (>>>FILE_SANS<<<);

=pod

=head1 NAME

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 AUTHOR

(>>>USER_NAME<<<) <(>>>AUTHOR<<<)>

=head1 FUNCTIONS


=cut

use strict;
use warnings;

=head2 new

 Title   : new
 Usage   : (>>>FILE_SANS<<<)->new();
 Function: 
 Returns : A new (>>>FILE_SANS<<<) object
 Args    : 

=cut

sub new {

    my $class = shift;
    my %args = @_;
    my $self = {};
    bless $self, ref $class  || $class;

    (>>>POINT<<<)

    return $self;
}

# must return success
1;
