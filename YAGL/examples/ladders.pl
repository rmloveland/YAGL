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

First, we load the graph with 100 of the 5-letter words.  For each
word, we store its checksum in a vertex attribute.

=cut

    my $iters = 0;
  WORD: for my $word (@words) {

        # say qq[WORD: $word];
        last WORD if $iters == 10_000;
        my $chksum = chksum($word);
        $g->add_vertex($word);
        $g->set_vertex_attribute( $word, { chksum => $chksum } );
        $iters++;
    }

=pod

Next, we add edges to the graph.  The criterion for adding an edge to
the graph is that the (2) words in question differ by only (1)
character.

=cut

    my @vertices = $g->get_vertices;
  VERTEX: for my $v (@vertices) {
        for my $u (@vertices) {
            next VERTEX if $v eq $u;
            if ( differ_by_one_char( $v, $u ) ) {
                my $v_sum = $g->get_vertex_attribute( $v, 'chksum' );
                my $u_sum = $g->get_vertex_attribute( $u, 'chksum' );
                my $weight =
                  $v_sum > $u_sum ? $v_sum - $u_sum : $u_sum - $v_sum;
                $g->add_edge( $v, $u, { weight => $weight } );
            }
        }
    }

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

=item differ_by_one_char

Given two strings, return the number of characters by which they differ.

Note that this subroutine is B<very> slow, and needs to be improved.
In a recent "word ladder" run with the 5757-vertex WORDS graph from
the Stanford Graphbase, of 522s of runtime, this subroutine cost 316s
(316/522 = ~61%).  In another, the total runtime was 147s, with this
subroutine costing 110s (~75%).

Some ideas based on looking at the SGB code:

First, we don't need to check every word against every other word; we only need to check a word W against all of the previous words W-1, W-2, ..., W-n.

How do we know which words are "previous"?  Because a sorting step is required.

We must sort the words by weight (what we are calling here C<$chksum> before we start working with them.

Once that is done, we then iterate over the words and put values into one of five (5) different hash tables.  The values in these hash tables are of the form:

    " ords" "w rds" "wo ds" "wor s" "word "

One entry will be in each hash table.  Each hash table will contain all of the variants that have the "blank spot" at the index C<$i> that corresponds to the example above (i.e., C<0 .. 4>).

=cut

sub differ_by_one_char {
    ## String String -> Boolean
    my ( $word_1, $word_2 ) = @_;
    state %seen;

    # Let's see if using C<unpack()> makes things any better.
    my @word_1 = unpack( "C*", $word_1 );
    my @word_2 = unpack( "C*", $word_2 );

    # if ( ( exists $seen{$word_1} ) ) {
    #     @word_1 = @{ $seen{$word_1} };
    # }
    # else {
    #     @word_1 = split //, $word_1;
    #     $seen{$word_1} = \@word_1;
    # }

    # if ( ( exists $seen{$word_2} ) ) {
    #     @word_2 = @{ $seen{$word_2} };
    # }
    # else {
    #     @word_2 = split //, $word_2;
    #     $seen{$word_2} = \@word_2;
    # }

    my $count = 0;
    for ( my $i = 0 ; $i < @word_1 ; $i++ ) {
        $count++ if $word_1[$i] ne $word_2[$i];
        return   if $count == 2;
    }
    $count == 1 ? 1 : undef;
}

main();

=pod

=head1 LADDERS - Find word ladders using the words data set from the Stanford GraphBase

Our example word ladder (taken from the SGB book) is:

    words - woods - goods - goads - grads - grade - grape - graph

=head2 FIRST RUN, Tuesday, July 28, 2020 (evening)

'words' ... 'graph'

[
  {
    'vertex' => 'words',
    'distance' => 0
  },
  {
    'distance' => 3,
    'vertex' => 'woods'
  },
  {
    'distance' => 19,
    'vertex' => 'goods'
  },
  {
    'vertex' => 'goads',
    'distance' => 33
  },
  {
    'vertex' => 'grads',
    'distance' => 36
  },
  {
    'vertex' => 'grade',
    'distance' => 50
  },
  {
    'distance' => 62,
    'vertex' => 'grape'
  },
  {
    'distance' => 65,
    'vertex' => 'graph'
  }
];

522s total runtime
  316s difference()
  105s get_vertices()
   30s  sort() (inside get_vertices)

Top 15 Subroutines Calls 	P 	F 	Exclusive Time 	Inclusive Time 	Subroutine
16568646	1	1	316s	316s	    main::difference
5759	3	2	74.9s	105s	    YAGL::get_vertices
5759	1	1	30.4s	30.4s	    YAGL::CORE:sort (opcode)
28270	2	1	760ms	1.41s	    YAGL::_add_neighbor
28270	1	1	438ms	438ms	    YAGL::set_edge_attribute
14135	1	1	281ms	1.74s	    YAGL::add_edge
1	1	1	161ms	270ms	    YAGL::dijkstra
28270	2	1	159ms	159ms	    YAGL::get_vertex_attribute
39784	3	1	145ms	145ms	    YAGL::has_vertex
31571	2	1	111ms	111ms	    YAGL::get_neighbors
5757	1	1	93.4ms	111ms	    YAGL::set_vertex_attribute
5757	1	1	59.5ms	59.5ms	    main::chksum
5757	1	1	56.6ms	71.8ms	    YAGL::add_vertex
14135	1	1	53.8ms	53.8ms	    YAGL::is_directed
1	1	1	40.4ms	149ms	GraphViz::BEGIN@10

=head2 SECOND RUN, Wed Jul 29 6:14:32 2020

  'words' ... 'graph'

This time I added some memoization to the vertex traversal, and
reduced the calls to get_vertices to 1.

words - woods - goods - goads - grads - grade - grape - graph

[
  {
    'vertex' => 'words',
    'distance' => 0
  },
  {
    'distance' => 3,
    'vertex' => 'woods'
  },
  {
    'distance' => 19,
    'vertex' => 'goods'
  },
  {
    'vertex' => 'goads',
    'distance' => 33
  },
  {
    'vertex' => 'grads',
    'distance' => 36
  },
  {
    'distance' => 50,
    'vertex' => 'grade'
  },
  {
    'distance' => 62,
    'vertex' => 'grape'
  },
  {
    'vertex' => 'graph',
    'distance' => 65
  }
]

Unfortunately, it looks like the "magic" memoizer actually screwed me in this case:

Top 15 Subroutines Calls 	P 	F 	Exclusive Time 	Inclusive Time 	Subroutine
16568646	1	1	381s	750s	     Memoize::_memoizer
16568646	1	1	370s	370s	        main::difference
16568646	1	1	117s	867s	     Memoize::__ANON__[(eval 14)[Memoize.pm:73]:1]
28270	2	1	882ms	1.59s	        YAGL::_add_neighbor
28270	1	1	480ms	480ms	        YAGL::set_edge_attribute
14135	1	1	366ms	2.03s	        YAGL::add_edge
28270	2	1	169ms	169ms	        YAGL::get_vertex_attribute
39784	3	1	147ms	147ms	        YAGL::has_vertex
31571	2	1	116ms	116ms	        YAGL::get_neighbors
1	1	1	96.5ms	194ms	        YAGL::dijkstra
14135	1	1	65.7ms	65.7ms	        YAGL::is_directed
5757	1	1	62.1ms	73.1ms	        YAGL::set_vertex_attribute
1	1	1	55.5ms	211ms	    GraphViz::BEGIN@10
5757	1	1	47.6ms	47.6ms	        main::chksum
1	1	1	42.2ms	42.3ms	Data::Dumper::Dumpxs (xsub)

=head2 THIRD RUN, Fri Jul 31 02:39:23 2020

(This is not really the third run.)

Calls 	P 	F 	Exclusive Time 	Inclusive Time 	Subroutine
16568646	1	1	232s	232s	                      main::differ_by_one_char
28270	2	1	834ms	1.54s	                      YAGL::_add_neighbor
28270	1	1	478ms	478ms	                      YAGL::set_edge_attribute
14135	1	1	318ms	1.91s	                      YAGL::add_edge
1	1	1	214ms	358ms	                      YAGL::dijkstra
28270	2	1	166ms	166ms	                      YAGL::get_vertex_attribute
39784	3	1	156ms	156ms	                      YAGL::has_vertex
1	1	1	143ms	151ms	         IPC::Run::Win32IO::BEGIN@29
1	1	1	139ms	139ms	autodie::Scope::GuardStack::BEGIN@6
31571	2	1	123ms	123ms	                      YAGL::get_neighbors
1	1	1	105ms	202ms	                      YAGL::BEGIN@7
1	1	1	99.1ms	101ms	     IPC::Run::Win32Helper::BEGIN@41
5757	1	1	92.6ms	111ms	                      YAGL::set_vertex_attribute
1	1	1	91.8ms	917ms	                  GraphViz::BEGIN@10
1	1	1	89.4ms	151ms	                      main::BEGIN@6

=cut
