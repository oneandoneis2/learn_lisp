#!/usr/bin/env perl
use strict;
use warnings;
use feature 'say';

sub cons {
    my ($fst, $snd) = @_;
    return {fst => $fst, snd => $snd, id => 'cons' };
}

sub car { shift->{fst} }

sub cdr { shift->{snd} }

sub is_cons { my $c = shift; (ref $c) =~ /HASH/ && $c->{id} =~ /cons/ }

sub print_cons {
    my $c = shift;
    my $acc = '';
    my $iter;
    $iter = sub {
        my ($cons, $new) = @_;
        # If cons is just a scalar, dump it to acc and we're done
        unless (ref $cons) {
            $acc .= "$cons";
            return;
        }
        $acc .= '(' if $new;
        if (is_cons(car($cons))) {
            # car is cons, so starts a new chain
            $iter->( car($cons), 1 )
        }
        else {
            $acc .= car($cons);
        }
        if (is_cons(cdr($cons))) {
            # cdr is cons, so we're into list format
            $acc .= ' ';
            $acc .= $iter->( cdr($cons), 0 )
        }
        elsif (!defined cdr($cons)) {
            # cdr contains undef, end of list
        }
        else {
            # cdr is non-cons, so dot-notation
            $acc .= ' . ';
            $acc .= $iter->( cdr($cons), 0 ) || ''
        }
        $acc .= ')' if $new;
        ''; # String malformed without this, don't ask me why..
    };
    $iter->($c,1);
    $acc;
}

my $test = cons(1, 2);
say print_cons car($test);
say print_cons cdr($test);
say print_cons ($test);

my $chain = cons(1, $test);
say print_cons car($chain);
say print_cons cdr($chain);
say print_cons($chain);

my $list = cons(1, cons(2, cons(3)));
say print_cons car($list);
say print_cons cdr($list);
say print_cons($list);
