#!perl

use strict;
use warnings;
use autodie;
use experimentals;
use lib '../lib';
use YAGL;
use Data::Dumper;

sub main {
    my $g     = YAGL->new;
    my @words = slurp('../data/words.dat');

=pod

First, we sort the words based on their "checksums" (really, sum of
C<chr()> values).  See the C<chksum()> subroutine for the details.

=cut

    my %chksum;
    for my $word (@words) {
        $chksum{$word} = chksum($word);
    }
    @words = sort { $chksum{$a} <=> $chksum{$b} } @words;

=pod

Next, we load the graph up with the words.  For each word, we store
its checksum in a vertex attribute.  (Note that it's not clear yet if
this is actually needed.)

=cut

    for my $word (@words) {
        $g->add_vertex($word);
        $g->set_vertex_attribute( $word, { chksum => $chksum{$word} } );
    }

=pod

Next, we add edges to the graph.  An edge is added between two words
if the words in question differ by only one character.

=cut

    for ( my $i = 0 ; $i < @words ; $i++ ) {
        for ( my $j = 0 ; $j < $i ; $j++ ) {
            my ( $u, $v ) = ( $words[$i], $words[$j] );
            if ( differ_by_one_char( $u, $v ) ) {
                my $v_sum = $g->get_vertex_attribute( $v, 'chksum' );
                my $u_sum = $g->get_vertex_attribute( $u, 'chksum' );
                my $weight =
                  $v_sum > $u_sum ? $v_sum - $u_sum : $u_sum - $v_sum;
                $g->add_edge( $u, $v, { weight => $weight } );
            }
        }
    }

=pod

Now that the graph is built and populated, we can start operating on
it.  We will now look for a path from a starting word to some other
word by traversing a "word ladder", which in graph terms means we will
find the shortest path (based on the "checksums" calculated above)
using Dijkstra's algorithm.

=cut

    my $start = 'words';
    my $end   = 'graph';

    my @path = $g->dijkstra( $start, $end );

    say qq[PATH: ], Dumper \@path;

    my $gv = undef;
    do {
        for my $p (@path) {
            my $vertex = $p->{vertex};
            $g->set_vertex_color( $vertex, 'red' );
        }

        my $viz = $g->to_graphviz;
        open my $fh, '>', 'ladders.dot' or die $!;
        say $fh $viz;
        close $fh;

        system qq[dot -O -Tjpg ladders.dot];
    } if $gv;
}

sub chksum {
    ## String -> Integer
    my $word = shift;
    my $sum  = 0;
    for my $c ( split //, $word ) {
        $sum += ord($c);
    }
    return $sum;
}

sub slurp {
    ## Pathname -> Array
    my $f = shift;
    open my $in, '<', $f;
    local $/;
    my @lines = split /\n/, <$in>;
    close $in;
    return @lines;
}

=pod

Given two strings, the C<differ_by_one_char()> subroutine returns the
number of characters by which they differ.

Note that this subroutine is B<very> slow, and needs to be improved.
In a recent "word ladder" run with the 5757-vertex WORDS graph from
the Stanford Graphbase, of 522s of runtime, this subroutine cost 316s
(316/522 = ~61\%).  In another, the total runtime was 147s, with this
subroutine costing 110s (~75\%).

Some ideas based on looking at the SGB code:

First, we don't need to check every word against every other word; we
only need to check a word W against all of the previous words W-1,
W-2, ..., W-n.

How do we know which words are "previous"?  Because a sorting step is
required.

We must sort the words by weight (what we are calling here C<\$chksum>
before we start working with them.

Once that is done, we then iterate over the words and put values into
one of five (5) different hash tables.  The values in these hash
tables are of the form:

    " ords" "w rds" "wo ds" "wor s" "word "

One entry will be in each hash table.  Each hash table will contain
all of the variants that have the "blank spot" at the index C<\$i>
that corresponds to the example above (i.e., C<0 .. 4>).

=cut

sub differ_by_one_char {
    ## String String -> Boolean
    my ( $word_1, $word_2 ) = @_;
    state %seen;

    # Let's see if using C<unpack()> makes things any better.
    my @word_1 = unpack( "C*", $word_1 );
    my @word_2 = unpack( "C*", $word_2 );

    my $count = 0;
    for ( my $i = 0 ; $i < @word_1 ; $i++ ) {
        $count++ if $word_1[$i] ne $word_2[$i];
        return   if $count == 2;
    }
    $count == 1 ? 1 : undef;
}

=pod

Finally, we run the program.

=cut

main();
