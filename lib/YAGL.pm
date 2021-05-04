package YAGL;

use strict;
use warnings;
use feature qw/ say state current_sub /;
use Smart::Match;
use Text::CSV;
use GraphViz;
use Hash::PriorityQueue;
use Storable;

use constant DEBUG => undef;

our $VERSION = '0.1';
no warnings 'experimental';    # For Smart::Match

=pod

=encoding UTF-8

=head1 NAME

YAGL - Yet Another Graph Library

=head1 VERSION

version 0.1

=head1 SYNOPSIS

    use YAGL;

    my $g = YAGL->new;

    # Populate the graph with 124 vertices, with randomly allocated
    # weighted edges between some of the vertices. The 'p' argument is
    # the probability that any given node will *not* be connected to
    # another node.

    $g->generate_random_vertices(
        { n => 124, p => 0.1, max_weight => 100_000 } );

    # Add vertices to the graph.

    $g->add_vertex('abc123');
    $g->add_vertex('xyz789');
    $g->add_vertex('I_AM_A_TEST');

    # Add edges to the graph.  You can store arbitrary attributes on
    # edges in hashrefs.

    $g->add_edge( 'abc123', 'xyz789', { weight => 1_000_000 } );
    $g->add_edge( 'I_AM_A_TEST', 'abc123', { weight => 12345 } );

    # Write the graph out to a CSV file.  This file can be read back
    # in later with the 'read_csv' method.

    $g->write_csv('foo.csv');

    # Pick a start and end vertex at random from the graph.

    my @vertices = $g->get_vertices;

    my $i     = int rand @vertices;
    my $j     = int rand @vertices;
    my $start = $vertices[$i];
    my $end   = $vertices[$j];

    # Using breadth-first search, find a path between the start and
    # end vertices, if any such path exists.  Otherwise, this method
    # returns undef.

    my @path;
    @path = $g->find_path_between( $start, $end );

    # Get a string representation of the graph in the graphviz
    # language for passing along to graphviz tools like `dot`.

    my $dot_string = $g->to_graphviz;

=head1 DESCRIPTION

This module implements a number of algorithms on directed and
undirected graphs.  Features include:

=over

=item * Breadth-first search of unweighted graphs to find the shortest
path in terms of number of nodes.

=item * Dijkstra's algorithm for finding the shortest path through a
weighted (directed or undirected) graph.

=item * Graph coloring for undirected graphs.

=item * Serializing graphs to and from CSV files.  This is very useful
for testing.

=item * Generating random graphs.

=item * Automated tests for all features.

=back

For an interesting example, see the file C<examples/ladders.pl>, which
is a "port" to Perl of the C<LADDERS> program from the book I<The Stanford
GraphBase> by Donald E. Knuth.

Note that this library is still in development.

=head1 GRAPH INITIALIZATION AND RANDOMIZATION

=over

=item new

Initialize a new graph.

    my $g = YAGL->new;

To make it directed, pass 'is_directed => 1' as an argument.

    my $g = YAGL->new(is_directed => 1);

=cut

sub new {
    my ($self, @args) = @_;
    my $graph = {};

    my %args = @args;
    $graph->{_INTERNAL}->{is_directed} = $args{is_directed};

    $graph->{_INTERNAL}->{edge_attrs}   = {};
    $graph->{_INTERNAL}->{vertex_attrs} = {};

    bless $graph, $self;
    return $graph;
}

=item generate_random_vertices

Generate a bunch of vertices with random names, and distribute edges
randomly among them.

Arguments:

=over

=item n

Number of vertices.

=item p

Probability that any given node will B<not> be connected to another node.

=item max_weight

The maximum weight of any vertex.  Vertex weights are randomly
generated up to this number.

=back

=back

=cut

sub generate_random_vertices {
    ## HashRef -> State!
    my ($self, $args) = @_;

    my $n = $args->{n};

    # As we loop through the nodes, for each node A, this is the
    # probability that another randomly selected node B is NOT
    # connected to A.  In other words, this is the probability that
    # there is NOT an edge A-B.
    my $p          = $args->{p};
    my $max_weight = $args->{max_weight};

    my %seen;

    for my $node (1 .. $n) {
        my $name = $self->_make_vertex_name;
        redo if $seen{$name};
        $seen{$name}++;
    }

    my @nodes = keys %seen;

    my @pairs;

    for my $node (@nodes) {
        my $maybe_neighbor = $nodes[rand $#nodes];
        next if $maybe_neighbor eq $node;
        my $connection_prob = rand 1;
        my $dist            = int rand $max_weight;
        if ($connection_prob > $p) {
            push @pairs, [$node, $maybe_neighbor, $dist];
            unless ($self->is_directed) {
                push @pairs, [$maybe_neighbor, $node, $dist];
            }
        }
        redo if rand 1 > 0.8;    # Sometimes, add more neighbors to this node.
    }

    for my $pair (@pairs) {
        $self->add_edge($pair->[0], $pair->[1], {weight => $pair->[2]});
    }
}

=head1 GRAPH SERIALIZATION

=over

=item write_csv

Write a CSV representation of this graph out to a (named) file.

=cut

sub write_csv {
    ## Filename -> State! IO!
    my ($self, $f) = @_;

    open my $fh, '>:encoding(utf8)', $f or die "Can't open file '$f': $!\n";

    say $fh qq[node,neighbor,weight,is_directed];

    my @vertices = $self->get_vertices;

    for my $vertex (@vertices) {
        my $neighbors = $self->get_neighbors($vertex);
        for my $neighbor (@$neighbors) {
            next unless defined $neighbor;
            my $weight
              = $self->get_edge_attribute($vertex, $neighbor, 'weight') || 0;
            my @cols = ($vertex, $neighbor, $weight);
            $self->is_directed ? push @cols, '1' : push @cols, '0';
            my $line = join ',', @cols;
            say $fh $line;
        }
    }
    close $fh;
}

=item read_lst

Read in a *.lst file that represents a graph (from L<https://hog.grinvin.org>).

=cut

sub read_lst {
    my ($self, $lst_file) = @_;

    my $csv_file = qq[$lst_file.csv];
    open my $output_fh, '>', $csv_file
       or die "Can't open file '$csv_file': $!\n";

    open my $input_fh, '<', $lst_file
      or die "Can't open file '$lst_file': $!\n";

    say $output_fh qq[node,neighbor,weight,is_directed];
    while (my $line = <$input_fh>) {
        next unless $line =~ /[A-Z0-9]: [A-Z0-9]+/;    # Skip empty lines.
        my ($node, $neighbors) = split /:/, $line;
        chomp($node)      if defined $node;
        chomp($neighbors) if defined $neighbors;
        my @neighbors = grep { $_ ne '' } split / /, $neighbors;

        for my $n (@neighbors) {
            say $output_fh qq["$node","$n",0,0];
        }
    }
    close $input_fh;
    close $output_fh;

    $self->read_csv($csv_file);
}

=item read_hcp

Read in a *.hcp file that represents a graph (from L<https://sites.flinders.edu.au/flinders-hamiltonian-cycle-project/fhcp-challenge-set>).

=cut

sub read_hcp {
    my ($self, $hcp_file) = @_;

    my $csv_file = qq[$hcp_file.csv];
    open my $output_fh, '>', $csv_file;
    open my $input_fh,  '<', $hcp_file
      or die "Can't open file '$hcp_file': $!\n";
    say $output_fh qq[node,neighbor,weight,is_directed];

  LINE: while (my $line = <$input_fh>) {
        next LINE unless $line =~ /^([0-9]+) ([0-9]+)/;
        my ($e1, $e2) = ($1, $2);
        say $output_fh qq["$e1","$e2",0,0];
        say $output_fh qq["$e2","$e1",0,0];
    }
    close $input_fh;
    close $output_fh;

    $self->read_csv($csv_file);
}

=item read_csv

Read in a CSV file that represents a graph.

=cut

sub read_csv {
    ## Filename -> State! IO!
    my ($self, $f) = @_;

    my $csv = Text::CSV->new({binary => 1});

    open my $fh, "<:encoding(utf8)", $f or die "Can't open file '$f': $!\n";

    my %seen;
  LINE: while (my $line = $csv->getline($fh)) {
        my @cols        = @$line;
        my $vertex      = $cols[0];
        my $neighbor    = $cols[1];
        my $weight      = $cols[2];
        my $is_directed = $cols[3];

        next LINE if $vertex eq 'node';
        if ($self->is_directed) {
            if ($seen{$neighbor . $vertex}) {
                next LINE;
            }
        }

        die qq[Directed graph cannot read in serialized undirected graph\n]
          if ($self->is_directed && !$is_directed);

        die qq[Undirected graph cannot read in serialized directed graph\n]
          if (!$self->is_directed && $is_directed);
        $self->add_edge($vertex, $neighbor, {weight => $weight});
        $seen{$neighbor . $vertex}++;
    }
}

=item to_graphviz

Generate a Graphviz representation of this graph (really, a string).

=cut

sub to_graphviz {
    ## ArrayRef -> String
    my ($self) = @_;

    my %seen;

    my $gv = GraphViz->new(directed => $self->is_directed, style => 'filled');

    for my $vertex ($self->get_vertices) {
        my $vertex_color = $self->get_vertex_color($vertex);
        $gv->add_node($vertex, style => 'filled', fillcolor => $vertex_color);
        my $neighbors = $self->get_neighbors($vertex);

        for my $neighbor (@$neighbors) {
            my $edge_weight
              = $self->get_edge_attribute($vertex, $neighbor, 'weight');
            my $edge_color
              = $self->get_edge_attribute($vertex, $neighbor, 'color');
            my $penwidth = $edge_color ? "5" : "";
            my $vertex_color = $self->get_vertex_color($neighbor);
            $gv->add_node($neighbor, fillcolor => $vertex_color);
            $gv->add_edge(
                $vertex, $neighbor,
                label    => $edge_weight,
                color    => $edge_color,
                penwidth => $penwidth,
            ) unless $seen{$vertex . $neighbor};
            $seen{$neighbor . $vertex}++;
            $seen{$vertex . $neighbor}++;
        }
    }

    return $gv->as_png;
}

=item draw

Given a file name, dumps a representation of the graph (as GraphViz)
and uses C<dot> to build an image of the graph.  All of this happens
in C<$TMPDIR>.

    $g->draw('24-ham-00');
    # To view the file, open $TMPDIR/24-ham-00.svg

=cut

sub draw {
    ## String -> State! IO!
    my ($self, $basename) = @_;

    die qq[draw() must be passed a filename argument!] unless $basename;

    my $tmpdir   = $ENV{TMPDIR} || '/tmp';
    my $filename = qq[$tmpdir/$basename.png];
    my $viz      = $self->to_graphviz;
    open my $fh, '>', $filename or die $!;
    say $fh $viz;
    close $fh;
}

=back

=head1 BOOLEAN METHODS

=over

=item is_empty

Returns true if the graph is empty - that is, if it has no vertices.

=cut

sub is_empty {
    ## -> Boolean
    my $self     = shift;
    my @vertices = $self->get_vertices;

    if (scalar @vertices >= 1) {
        return;
    }
    else {
        return 1;
    }
}

=item is_complete

Return true if this is a complete graph.  A complete graph is one
wherein each vertex is connected to every other vertex.

=cut

sub is_complete {
    my $self = shift;

    my @vertices = $self->get_vertices;
    my $v        = pop @vertices;

    my $neighbors = $self->get_neighbors($v);

    @vertices = sort { ($a || '') cmp($b || '') } @vertices;
    my @neighbors = sort { ($a || '') cmp($b || '') } @$neighbors;

    return 1 if @vertices ~~ @neighbors;

    return;
}

=item is_tree

Return true if this graph is a tree.  A graph is a tree if its number
of edges is one fewer than its number of vertices.  This definition is
taken from Even's book I<Graph Algorithms>.

=cut

sub is_tree {
    my ($self) = @_;

    return unless $self->is_connected;

    my @e = $self->get_edges;
    my @v = $self->get_vertices;

    my $e = @e;
    my $v = @v;

    return unless $e == $v - 1;
}

=item is_connected

Return true if for each vertex A in this graph, there is a path
between A and every other vertex in the graph.

=cut

sub is_connected {
    my ($self) = @_;

    die qq[is_connected() - not implemented for directed graphs]
      if $self->is_directed;

    my @vertices = $self->get_vertices;

    my $start = pop @vertices;

    for my $v (@vertices) {
        return unless $self->find_path_between($start, $v);
    }
    return 1;
}

=item has_cycle

Return true if there is a cycle in this graph; in other words, if this
graph is not a tree.

=cut

sub has_cycle {
    my ($self) = @_;
    return $self->is_tree ? undef : 1;
}

=item is_colored

Return true if this graph has already been colored using the
C<color_vertices> method.

=cut

sub is_colored {
    ## -> Number
    my ($self) = @_;
    my @vertices = $self->get_vertices;
    my @colors = grep { $self->get_vertex_color($_) } @vertices;

    return scalar @vertices == scalar @colors;
}

=item is_directed

Return true if this is a directed graph.  Graphs can only be marked as
directed during object initialization, by setting the C<is_directed>
argument to C<new>.

=back

=cut

sub is_directed {
    my ($self) = @_;
    return $self->{_INTERNAL}->{is_directed};
}

=head1 METHODS ON VERTICES

=over

=item add_vertex

Add a vertex V to this graph, if it does not already exist.  Return
C<undef> if V already exists.

    $g->add_vertex('s');

=cut

sub add_vertex {
    ## String -> State!
    my ($self, $vertex) = @_;
    return if $self->has_vertex($vertex);
    $self->{$vertex} = [];
}

=item add_vertices

Add multiple vertices to this graph.  Takes an array as its argument.

    my @to_add = qw/a b c d e f/;
    $g->add_vertices(@to_add);

=cut

sub add_vertices {
    my ($self, @vertices) = @_;
    $self->add_vertex($_) for @vertices;
}

=item get_neighbors

Given a vertex in the graph, get its neighbors - that is, the other
vertices to which it is connected.

    $g->get_neighbors('s');

=cut

sub get_neighbors {
    ## String -> ArrayRef
    my ($self, $vertex) = @_;

    if (exists $self->{$vertex}) {
        return $self->{$vertex} if defined $self->{$vertex};
    }
    else {
        return;
    }
}

=item has_vertex

Return true if the vertex in question is a part of the graph.

    $g->has_vertex('a');

=cut

sub has_vertex {
    ## String -> Boolean
    my ($self, $vertex) = @_;
    if (exists $self->{$vertex} && defined $self->{$vertex}) {
        return 1;
    }
    return;
}

=item remove_vertex

Remove the named vertex from the graph, if it exists.

    $g->remove_vertex('s');

Note that removing a vertex will also delete all edges (and edge
attributes) between the given vertex and its former neighbors.

=cut

sub remove_vertex {
    ## String -> State!
    my ($self, $vertex) = @_;

    my $neighbors = $self->get_neighbors($vertex);

    # Our general strategy for deleting things is to set the vertex's
    # position in the array of arrays graph representation to undef.
    #
    # In this pass, we delete all edges between each of this vertex's
    # neighbor and the vertex. Note the order of the arguments:
    #
    # - Because we are deleting *this* vertex (and *not* the
    # neighbor), we have the neighbor (which is not being deleted) set
    # its connection to this vertex to undef (i.e., deleted).
    #
    # - Then, we delete any edge attributes that exist between the two
    # vertices (since there is no edge there anymore).
    for my $neighbor (@$neighbors) {
        $self->_remove_neighbor($neighbor, $vertex);
        $self->delete_edge_attributes($vertex, $neighbor);
    }

    # Then, we delete the "root" reference to the vertex by setting it
    # to undef.
    if (exists $self->{$vertex}) {
        delete $self->{$vertex};
    }
}

=item get_vertices

Return a list of the vertices in the graph.

    my @v = $g->get_vertices;

=cut

sub get_vertices {
    ## -> Array
    my $self = shift;
    my @vertices;
    for my $vertex (keys %$self) {
        next unless defined $vertex;
        next if $vertex eq '_INTERNAL';
        push @vertices, $vertex;
    }

    # TODO(rml): Remove this call to C<sort>.  It has a large
    # performance cost for large graphs.  On a recent "word ladder"
    # run with the 5757-vertex WORDS graph from the Stanford
    # Graphbase, of 522s of runtime, this call to C<sort> cost 30s
    # (30/522 = ~6%).

    # AFAICT there is no good reason for the sorting; it was done to
    # get some tests to pass -- likely the graph cloning equality
    # tests, if memory serves.  Therefore the action item is to remove
    # this call to C<sort>, see what breaks in the tests, and fix it.

    @vertices = sort @vertices;
    return @vertices;
}

=item get_degree

Given a vertex V, return the degree of that vertex -- that is, the
number of edges between V and other vertices (its neighbors).

=cut

sub get_degree {
    my ($self, $vertex) = @_;
    if ($self->has_vertex($vertex)) {
        my $neighbors = $self->get_neighbors($vertex);
        return scalar @$neighbors;
    }
    return;
}

=item set_vertex_attribute

Given a vertex V, store a hashref of attributes about that vertex.

=cut

sub set_vertex_attribute {
    ## String HashRef -> State!
    my ($self, $vertex, $new_attrs) = @_;
    return unless $self->has_vertex($vertex);

    # Attributes hashref already exists, so we add to it.  NOTE:
    # this is a hash so the update is destructive.
    for (my ($k, $v) = each %$new_attrs) {
        next unless defined $k;
        next if $k eq '';
        $self->{_INTERNAL}->{vertex_attrs}->{$vertex}->{$k} = $v;
    }
}

=item get_vertex_attribute

Given a vertex V and an attribute string, retrieve the value of that attribute.

    my $weight = $g->get_vertex_attribute('s', 'weight');
    # 123

=cut

sub get_vertex_attribute {
    ## String String -> Value OR undef
    my ($self, $vertex, $attribute) = @_;
    return $self->{_INTERNAL}->{vertex_attrs}->{$vertex}->{$attribute};
}

=item get_vertex_attributes

Given a vertex V, return all of the vertex's attributes, whatever they
are.  Reads from the object's internal hashref, so beware: these
values could be anything.

    my $attrs = $g->get_vertex_attributes('s');

=cut

sub get_vertex_attributes {
    ## String -> HashRef OR undef
    my ($self, $vertex) = @_;
    return unless $self->has_vertex($vertex);
    return $self->{_INTERNAL}->{vertex_attrs}->{$vertex};
}

=item delete_vertex_attributes

Given a vertex V, delete all of its attributes (if any).

    $g->delete_vertex_attributes('s');

=cut

sub delete_vertex_attributes {
    ## String -> Undefined OR State!
    my ($self, $vertex) = @_;
    return unless $self->has_vertex($vertex);
    delete $self->{_INTERNAL}->{vertex_attrs}->{$vertex};
}

=item set_vertex_color

Given a vertex V and some color C, sets a 'color' attribute on
V. Shorthand for using C<set_vertex_attribute>.

    $g->set_vertex_color('s', 'red');

=cut

sub set_vertex_color {
    ## String String -> Undefined OR State!
    my ($self, $vertex, $color) = @_;
    $self->set_vertex_attribute($vertex, {color => $color});
}

=item get_vertex_color

Given a vertex V, get its color (if any).  Shorthand for calling
C<get_vertex_attribute>.

    $g->get_vertex_color('s');

=back

=cut

sub get_vertex_color {
    ## String -> String OR Undefined
    my ($self, $vertex) = @_;
    $self->get_vertex_attribute($vertex, 'color');
}

=head1 METHODS ON EDGES

=over

=item get_edge

Get the edge between two vertices, A and B.  Return C<undef> if no
such edge exists.  If the edge does exist, return an array reference
containing A, B, and a (possibly empty) hash reference of edge
attributes.

    my $edge = $g->get_edge('s', 'a');

=cut

sub get_edge {
    ## String String -> ArrayRef
    my ($self, $a, $b) = @_;

    return unless $self->edge_between($a, $b);

    my $attrs = $self->get_edge_attributes($a, $b);

    return [$a, $b, $attrs];
}

=item get_edges

Get a list containing all of the edges in the graph.  Specifically,
this will be a list of array references, with the contents of each
array reference as described in the documentation for C<get_edge()>.

    my @edges = $g->get_edges;

=cut

sub get_edges {
    ## -> Array
    my ($self) = @_;

    my @vertices = $self->get_vertices;
    my @answer;
    my %seen;

    for my $vertex (@vertices) {
        my $neighbors = $self->get_neighbors($vertex);

        for my $neighbor (@$neighbors) {
            next unless defined $neighbor;
            next if $seen{$vertex . $neighbor};
            push @answer, $self->get_edge($vertex, $neighbor);
            $seen{$vertex . $neighbor}++;
            $seen{$neighbor . $vertex}++;
        }
    }

    @answer = sort { $a->[0] lt $b->[0] } @answer;
    return @answer;
}

=item edge_between

Given two vertices A and B, return something truthy if there exists an
edge between A and B.  Otherwise, return C<undef>.

    if ($g->edge_between('s', 'a')) {
      say 'Yes';
    }

=cut

sub edge_between {
    ## String String -> Boolean
    my ($self, $a, $b) = @_;

    return unless (defined $a && defined $b);
    return 1 if $a eq $b;

    my $neighbors = $self->get_neighbors($a);
    if ($b ~~ @$neighbors) {
        return 1;
    }
    else { return; }
}

=item get_edge_attributes

Given two vertices A and B that have an edge between them, return
whatever attributes are stored for that edge.  Note that this can be
any arbitrary Perl data structure that could be stored in a hash
reference.

=cut

sub get_edge_attributes {
    ## String String -> HashRef OR undef
    my ($self, $start, $end) = @_;

    my $pairkey = $start . $end;
    return $self->{_INTERNAL}->{edge_attrs}->{$pairkey};
}

=item get_edge_attribute

Given two vertices A and B that have an edge between them, and a
specific (text) attribute T, return whatever values are associated
with T for that edge.  For example, a (numeric) weight.

    my $edge_weight = $g->get_edge_attribute('s', 'a', 'weight');

=cut

sub get_edge_attribute {
    ## String String String -> Value OR undef
    my ($self, $start, $end, $attribute) = @_;

    my $pairkey = $start . $end;
    return $self->{_INTERNAL}->{edge_attrs}->{$pairkey}->{$attribute};
}

=item get_edge_weight

Shortcut for the following call to C<get_edge_attribute()>.

    my $edge_weight = $g->get_edge_attribute('s', 'a', 'weight');

=cut

sub get_edge_weight {
    ## String String -> Value OR undef
    my ($self, $start, $end, $attribute) = @_;

    return $self->get_edge_attribute($start, $end, 'weight');
}

=item set_edge_attribute

Given two vertices A and B that have an edge between them, store a
specific attribute key-value pair (a hash reference) that you want to
associate with that edge.

    my $edge_weight = $g->set_edge_attribute('s', 'a', { weight => 123 });

=cut

sub set_edge_attribute {
    ## String String HashRef -> State!
    my ($self, $start, $end, $new_attrs) = @_;

    my $pairkey1 = $start . $end;
    my $pairkey2 = $end . $start;

    # Attributes hashref already exists, so we add to it.  NOTE:
    # this is a hash so the update is destructive.
    for (my ($k, $v) = each %$new_attrs) {
        next unless defined $k;
        next if $k eq '';
        $self->{_INTERNAL}->{edge_attrs}->{$pairkey1}->{$k} = $v;
        $self->{_INTERNAL}->{edge_attrs}->{$pairkey2}->{$k} = $v;
    }
}

=item delete_edge_attributes

Given two vertices A and B that have an edge between them, delete all
of the attributes (weight, color, etc.) associated with that edge.

    $g->delete_edge_attributes('s', 'a');

=cut

sub delete_edge_attributes {
    ## String String -> Undefined OR State!
    my ($self, $start, $end) = @_;
    return unless defined $start && defined $end;

    my $pairkey1 = $start . $end;
    my $pairkey2 = $end . $start;
    return
      unless (exists $self->{_INTERNAL}->{edge_attrs}->{$pairkey1}
        && exists $self->{_INTERNAL}->{edge_attrs}->{$pairkey2});
    delete $self->{_INTERNAL}->{edge_attrs}->{$pairkey1};
    delete $self->{_INTERNAL}->{edge_attrs}->{$pairkey2};
}

=item add_edge

Given two vertices A and B, add an edge between them, as well as a
hash reference containing any attributes that should be associated
with that edge.  Note that if either of the vertices do not yet exist,
they will be created.

    $g->add_edge('s', 'a', { name => 'my great edge'});

=cut

sub add_edge {
    ## String String -> State!
    my ($self, $v1, $v2, $attrs) = @_;
    $self->_add_neighbor($v1, [$v2], $attrs);
    $self->_add_neighbor($v2, [$v1], $attrs) unless $self->is_directed;
}

=item add_edges

Given a list of array references that describe vertices in the format

    [['a', 'b', { weight => 123 }], ... ]

add all of the edges listed, as well as the attributes that should be
associated with each edge.  Note that if either of the vertices do not
yet exist, they will be created.

    $g->add_edge('s', 'a', { name => 'my great edge'});

=cut

sub add_edges {
    my ($self, @edges) = @_;

    for my $elem (@edges) {
        my ($a, $b, $attrs) = @$elem;
        $self->add_edge($a, $b, $attrs);
    }
}

=item remove_edge

Given two vertices A and B, remove the edge (if any) between them, as
well as any associated attributes.

    $g->remove_edge('s', 'a');

=back

=cut

sub remove_edge {
    ## String String -> Boolean State! OR Undef
    my ($self, $a, $b) = @_;

    return unless $self->edge_between($a, $b);

    # We delete A from B's list of neighbors, and delete B from A's
    # list of neighbors.  Then, we delete any edge attributes, since
    # said edge no longer exists.

    $self->_remove_neighbor($a, $b);
    $self->_remove_neighbor($b, $a);
    $self->delete_edge_attributes($a, $b);

    return 1;
}

=head1 PATH SEARCH METHODS

=over

=item dijkstra

Given two vertices START and END on a graph with weighted edges, find
the shortest path between them using Dijkstra's algorithm.

    $g->dijkstra($a, $b);

=cut

sub dijkstra {
    ## String String -> Array
    my ($self, $start, $end) = @_;

    return () unless defined $start && defined $end;

    my @path;
    my @queue;
    my %seen;
    my $heap = Hash::PriorityQueue->new;
    my $st   = {};

    $st->{$start}->{distance} = 0;
    $st->{$start}->{prev}     = undef;

    for my $vertex ($self->get_vertices) {
        next if $vertex eq $start;
        $st->{$vertex}->{distance} = 1_000_000;
        $st->{$vertex}->{prev}     = undef;
    }

    $heap->insert($start, $st->{$start}->{distance});

    while (my $v = $heap->pop()) {
        my $neighbors = $self->get_neighbors($v);

        for my $neighbor (@$neighbors) {
            next if $seen{$neighbor};
            $seen{$neighbor}++;

            # In this block, we are setting up the information we will
            # need to answer the question "Have we found a new
            # shortest path (by distance)?"
            my $distance_to_self         = $st->{$v}->{distance};
            my $old_distance_to_neighbor = $st->{$neighbor}->{distance};
            my $neighbor_edge_weight
              = $self->get_edge_attribute($v, $neighbor, 'weight');
            my $new_distance_to_neighbor
              = $distance_to_self + $neighbor_edge_weight;

            # This is the core of Dijkstra's algorithm: Have we
            # discovered a path whose distance to the neighbor is
            # shorter than the previously discovered path's distance?
            # If yes, we update the spanning tree with this new path
            # information.
            if ($new_distance_to_neighbor < $old_distance_to_neighbor) {
                $st->{$neighbor}->{distance} = $new_distance_to_neighbor;
                $st->{$neighbor}->{prev}     = $v;
            }

            if ($neighbor eq $end) {
                @path = $self->_st_walk($st, $start, $end);
                return @path;
            }
            else {
                $heap->insert($neighbor, $st->{$neighbor}->{distance});
            }
        }

        $seen{$v}++;
    }
    return ();
}

=item has_walk

Given a list of vertices, determine whether they constitute a walk.
Given an optional argument, will determine if it is a closed walk.

    $walk = qw/a f d b c e l m j k i h g/;
    $g->has_walk($walk, {closed => 1});

=cut

sub has_walk {
    ## ArrayRef HashRef -> Boolean
    my ($self, $walk, $args) = @_;

    my $len = @$walk - 1;

    my $closed;
    $closed = $args->{closed} if $args;

    # Short-circuit on whether it's a closed walk.
    if ($closed) {
        return unless $self->edge_between($walk->[0], $walk->[$len]);
    }

    for (my $i = 0; $i <= $len; $i++) {
        my $j = $i + 1;
        last if $j > $len;
        return unless $self->edge_between($walk->[$i], $walk->[$i + 1]);
    }

    return 1;
}

=item find_path_between

Given two vertices START and END in an unweighted graph, find the
shortest path between them using breadth-first search.

=cut

sub find_path_between {
    ## String String -> Array
    my ($self, $start, $end) = @_;

    return () unless defined $start && defined $end;

    my @path;     # Path so far
    my @queue;    # Vertices still to visit.
    my %seen;     # Vertices already seen.
    my $found;    # Whether we have found the wanted vertex.
    my $st = {};  # Spanning tree, used to find paths.

    if ($start eq $end) {
        push @path, $start;
        return @path;
    }

    push @queue, $start;
    $seen{$start}++;

    while (@queue) {

        my $v = shift @queue;

        my $neighbors = $self->get_neighbors($v);

        for my $neighbor (@$neighbors) {
            next unless defined $neighbor;
            next if $seen{$neighbor};
            $st->{$neighbor}->{prev} = $v;
            if ($neighbor eq $end) {
                $found++;
                @path = $self->_st_walk($st, $start, $end);
                return @path;
            }
            else {
                push @queue, $neighbor;
            }
            $seen{$neighbor}++;
        }
    }
    return $found ? @path : ();
}

=item mst

The F<mst> method finds the minimum spanning tree of the current graph
object.  As such, it takes no arguments; instead, it searches for the
lowest-weight edge in the graph, chooses a vertex from one end of that
edge as the starting vertex, and builds the spanning tree from there.

=cut

sub mst {
    ## -> YAGL OR Undef
    my ($self) = @_;

    return unless $self->is_connected;

    my @queue;
    my %seen;
    my $heap = Hash::PriorityQueue->new;

    my @vertices = $self->get_vertices;
    my @edges    = $self->get_edges;

    # TODO(rml): This method should throw an error if the edge does
    # not have a weight attribute.  Or perhaps it should assume a
    # weight of 0 if none is found?  Since that's what our CSV format
    # does.

    @edges = sort { $a->[2]->{weight} <=> $b->[2]->{weight} } @edges;
    my $start = $edges[0]->[1];

    my $mst = YAGL->new;

    $mst->add_vertex($start);
    $mst->set_vertex_attribute($start, {distance => 0});

    for my $vertex (@vertices) {
        next if $vertex eq $start;
        $mst->add_vertex($vertex);
        $mst->set_vertex_attribute($vertex, {distance => 1_000_000});
    }

    $heap->insert($start, $mst->get_vertex_attribute($start, 'distance'));

    while (my $v = $heap->pop()) {
        my $neighbors = $self->get_neighbors($v);

        for my $neighbor (@$neighbors) {
            next if $seen{$neighbor};
            $seen{$neighbor}++;

            # In this block, we are setting up the information we will
            # need to answer the question "Have we found a new
            # shortest path (by distance)?"
            my $distance_to_self = $mst->get_vertex_attribute($v, 'distance');
            my $old_distance_to_neighbor
              = $mst->get_vertex_attribute($neighbor, 'distance');
            my $neighbor_edge_weight
              = $self->get_edge_attribute($v, $neighbor, 'weight');
            my $new_distance_to_neighbor
              = $distance_to_self + $neighbor_edge_weight;

            # This is the core of Jarnik-Prim (as well as Dijkstra's)
            # algorithm: Have we discovered a path whose distance to
            # the neighbor is shorter than the previously discovered
            # path's distance?  If yes, we update the spanning tree
            # with this new path information.
            if ($new_distance_to_neighbor < $old_distance_to_neighbor) {
                $mst->set_vertex_attribute($neighbor,
                    {distance => $new_distance_to_neighbor});
                $mst->add_edge($v, $neighbor,
                    {weight => $neighbor_edge_weight});
            }

            if (   $mst->is_connected
                && scalar $mst->get_vertices == scalar @vertices
                && $mst->is_tree)
            {
                return $mst;
            }
            else {
                $heap->insert($neighbor,
                    $mst->get_vertex_attribute($neighbor, 'distance'));
            }
        }
        $seen{$v}++;
    }
    return;
}

=item dfs

The F<dfs> method performs depth-first-search on the graph beginning
at the vertex START; for each vertex visited by the search, invoke
C<$sub>.

=cut

sub dfs {
    ## String Function -> Array State!
    my ($self, $start, $sub) = @_;
    return () unless defined $start;

    my $dfs = sub {
        my ($self, $current, $sub, $seen) = @_;

        $seen->{$current}++;
        $sub->($current);
        my $neighbors = $self->get_neighbors($current);
        for my $neighbor (@$neighbors) {
            next unless defined $neighbor;
            unless ($seen->{$neighbor}) {
                $self->set_edge_attribute($current, $neighbor,
                    {color => 'red'});
                __SUB__->($self, $neighbor, $sub, $seen);
            }
        }
    };
    my $seen = {};
    $dfs->($self, $start, $sub, $seen);

    # Deal with any unconnected vertices.
    my @unseen = grep { !exists $seen->{$_} } $self->get_vertices;

    for my $u (@unseen) {
        next if $seen->{$u};
        $dfs->($self, $u, $sub, $seen);
    }
}

=item connected_components

The F<connected_components> method returns the connected components of
the graph, as a list of lists.

If there are no connected components, it will return an empty list:

    []

If there is only one connected component, it will return a list with
one element: a list of the vertices of the connected component:

    [['a', 'b', 'c', 'd']]

If there are I<n> connected components, it will return a list with
I<n> elements:

    [['a', 'b'], ['c', 'd'], ['e', 'f', 'g']]

=cut

# TODO(rml): Design a better output format.  This should probably be
# returning YAGL objects so the caller can do graph operations on them
# if they want to.

sub connected_components {
    my ($self) = @_;

    my @components;

    my $lambda = sub {
        my ($current) = @_;

        my $delim = 'XXX';
        push @components, $delim unless @components;
        if ($components[$#components] eq $delim) {
            push @components, $current;
        }
        elsif ($self->find_path_between($current, $components[$#components]))
        {
            push @components, $current;
        }
        else {
            push @components, $delim;
            push @components, $current;
        }
    };

    my @vertices = sort { $self->get_degree($a) <=> $self->get_degree($b) }
      $self->get_vertices;
    my $start = $vertices[0];
    $self->dfs($start, $lambda);

    my @p1 = grep { $_ ne '' } split /XXX/, join ' ', @components;
    my @answer;
    for my $piece (@p1) {
        my @parts = grep { $_ ne '' } split / +/, $piece;
        push @answer, \@parts;
    }

    return @answer;
}

=item exhaustive_search

The F<exhaustive_search> method performs an exhaustive search of all
trees in the graph.  The way this works is very close to the algorithm
for depth-first-search, except that F<exhaustive_search> unmarks the
vertices it has visited after the recursive self-call; this is
described in more detail on p.623 of Sedgewick's I<Algorithms>, 2nd
ed.

It takes two arguments: the name of the starting vertex, and an
optional subroutine argument.

    $g->exhaustive_search('a', sub { say $_[0] });

=cut

sub exhaustive_search {
    my ($self, $start, $sub) = @_;

    return () unless defined $start;

    my $search = sub {
        my ($self, $current, $sub, $seen, $path) = @_;
        $seen->{$current}++;
        state $backtracked;

        my $len = @$path - 1;
        my $last;
        $last = $path->[$len] if $path->[$len];

        if (DEBUG) {
            say
              qq[exhaustive_search(): choice point is '$last', adding '$current']
              if $last && $backtracked;
            say qq[exhaustive_search(): adding '$current']
              unless $backtracked;
        }
        push @$path, $current;

        say qq[exhaustive_search(): PATH -> @$path] if DEBUG;

        # The subroutine operates on the current vertex $current as
        # well as looking at the "path so far".  That way, the
        # subroutine can be used as a predicate to determine a search
        # cutoff property as described on p.28 of Knuth's v04f05.

        # TODO(rml): The behavior described above still doesn't quite
        # work as expected yet.  The branch below shows how the
        # subroutine can "signal into" this method that it wants to
        # cutoff/prune the current branch of the search tree and try
        # something else.  However, the subroutine is still getting
        # called more than once (e.g. 15 times out of 8000), for
        # reasons I don't understand.  Why 15 and not just 1?

        if ($sub) {
            my $rv = $sub->($current, $path);
            if (defined $rv && $rv == -1) {

                # This cutoff optimization brings the number of subroutine
                # calls from ~8000 to ~16 in Test 5 of
                # 28-house-of-graphs-lst-file-format.t, a savings of about
                # 99.8 percent!
                return;
            }
        }

        my $neighbors = $self->get_neighbors($current);
        for my $neighbor (@$neighbors) {
            next unless defined $neighbor;
            unless ($seen->{$neighbor}) {
                __SUB__->($self, $neighbor, $sub, $seen, $path);
                if (DEBUG) {
                    say
                      qq[exhaustive_search(): backtracking from '$neighbor'];
                }
                delete $seen->{$neighbor};
                pop @$path;
                $backtracked = 1 if DEBUG;
            }
        }
    };
    my $seen = {};
    my $path = [];
    $search->($self, $start, $sub, $seen, $path);
}

=item _visit

The F<_visit> method below visiting the vertices of the graph using
the following procedure: to process some vertex I<V>, visit vertex
I<V>; then visit each child of I<V>, applying this visiting procedure
recursively, and returning to I<V> afterward.

The behavior above is described on p.630 of Sedgewick's I<Algorithms>,
2nd ed., as part of an algorithm for finding solutions to the
Traveling Salesman Problem: given an MST, produce a tour by visiting
the nodes of the tree using the procedure this method implements
(described above).  We use this to find open and closed Hamiltonian
walks (also known as paths and cycles) in the C<hamiltonian_walk>
method.

=cut

sub _visit {
    my ($self, $start, $sub, $path) = @_;

    state $calls = 0;
    $calls++;

    say qq[_visit: $calls calls] if DEBUG;

    return () unless defined $start;

    my @queue;      # Vertices still to visit.
    state %seen;    # Vertices already seen.

    push @queue, $start;
    $seen{$start}++;

    while (@queue) {
        my $v = pop @queue;
        $sub->($v);
        push @$path, $v;

        my $neighbors = $self->get_neighbors($v);
        for my $neighbor (@$neighbors) {
            next unless defined $neighbor;
            next if $seen{$neighbor};
            push @queue, $neighbor;
            $self->_visit($neighbor, $sub, $path);
            $seen{$neighbor}++;
        }
    }
    return @$path;
}

=item hamiltonian_walks

The C<hamiltonian_walks> method does an exhaustive search to find all
of the open or closed Hamiltonian walks on the graph, if they exist.
It takes an optional C<closed> argument to determine which type to
look for.

    $g->hamiltonian_walks(closed => 1);
    $g->hamiltonian_walks;      # Finds open walks by default.

=cut

sub hamiltonian_walks {
    ## Array -> Array State!
    my ($self, @args) = @_;

    my %args   = @args;
    my $closed = $args{closed};

    my $allow_reversals = $args{allow_reversals};

    my $n_solutions;
    if (exists $args{n_solutions}) {
        $n_solutions = $args{n_solutions};
    }    # 1,..,_n_ OR undef
    $n_solutions = 1_000_000
      unless defined $n_solutions;    # undef = all solutions

    my @vertices   = $self->get_vertices;
    my $n_vertices = @vertices;
    my $start;

    # We can easily disqualify a graph as not having a closed Hamiltonian
    # walk if it has any vertex with a degree of less than two (that
    # is, if it has any leaves or entirely disconnected vertices).
    if ($closed) {
        for my $v (@vertices) {
            return if $self->get_degree($v) < 2;
        }
    }

    # If the graph is a tree, we need to select a leaf node.
    my $is_tree = $self->is_tree;

    # However, if we're looking for a closed walk in a tree, none
    # exists.
    return if $is_tree && $closed;

    if ($is_tree) {
      LOOP: for my $v (@vertices) {
            if ($self->get_degree($v) == 1) {
                $start = $v;
                last LOOP;
            }
        }
    }
    else {
        $start = $vertices[0];
    }

    my @hams;

    my $lambda = sub {
        ## String ArrayRef -> State!
        my ($current, $path) = @_;

        state $calls = 0;
        state %seen;
        $calls++;
        say qq[hamiltonian_walks(): calls -> $calls] if DEBUG;

        # Bail out early if we've already found the desired number of
        # solutions.

        if (@hams == $n_solutions) {
            if (DEBUG) {
                say
                  qq[hamiltonian_walks(): found $n_solutions solutions after $calls calls!];
            }
            return -1;
        }

        if (@$path == $n_vertices) {
            if ($self->has_walk($path, {closed => $closed})) {
                say qq[hamiltonian_paths(): found a path -> @$path] if DEBUG;
                if ($allow_reversals) {
                    push @hams, [@$path];
                }
                else {
                    my @p  = @$path;
                    my $p1 = join '-', @p[1 .. $#p];
                    my $p2 = join '-', reverse @p[1 .. $#p];
                    unless (exists $seen{$p1} || exists $seen{$p2}) {
                        push @hams, [@$path];
                    }
                    $seen{$p1}++;
                    $seen{$p2}++;
                }
            }
        }
    };

    $self->exhaustive_search($start, $lambda);
    return @hams;
}

=item is_planar

The C<is_planar> method tests whether a graph is planar.

=back

=cut

# TODO(rml): Add a citation for this algorithm, I think it might be
# from I<Graph Algorithms> by S. Even.

sub is_planar {
    my ($self) = @_;

    my $edge_count   = $self->get_edges;
    my $vertex_count = $self->get_vertices;

    if ($edge_count > (3 * $vertex_count)) {
        return;
    }
}

=head1 GRAPH CLONING (OBJECT COPYING) AND EQUALITY CHECKS

=over

=cut

=item clone

Given a graph object, the C<clone> method makes a fresh copy of that object.

=cut

sub clone {
    my ($self) = @_;
    my $copy = Storable::dclone($self);
    return $copy;
}

=item equals

Given two graphs I<A> and I<B>, The C<equals> method checks to see
whether they are identical.  It checks the edges, vertices, and edge
attributes to do so.

=back

=cut

sub equals {
    my ($self, $other) = @_;

    return unless $self->isa('YAGL') && $other->isa('YAGL');

    my @xs = $self->get_vertices;
    my @ys = $other->get_vertices;

    return unless @xs ~~ @ys;

    my @es = $self->get_edges;
    my @fs = $other->get_edges;

    return unless @es ~~ @fs;

    my $self_attrs  = $self->_edge_attrs;
    my $other_attrs = $other->_edge_attrs;

    return unless %$self_attrs ~~ %$other_attrs;

    # TODO(rml): This method should also check vertex attributes.

    return 1;
}

=head1 INTERNAL HELPER METHODS

=over

=item _add_neighbor

The C<_add_neighbor> method is the internal helper used to add an edge
(and any edge attributes) between two vertices.

=cut

sub _add_neighbor {
    ## String ArrayRef HashRef -> State!
    my ($self, $vertex, $new_neighbor, $edge_attrs) = @_;

    unless (ref($new_neighbor) eq 'ARRAY') {
        my ($package, $filename, $line) = caller();
        die <<"EOF";
on line $line of file $filename:
  $package\:\:_add_neighbor('$vertex', '$new_neighbor', '$edge_attrs'):
    expected arrayref, got '$new_neighbor'
EOF
    }

    if ($self->has_vertex($vertex)) {
        my $neighbors = $self->get_neighbors($vertex);
        for my $value (@$new_neighbor) {
            push @$neighbors, $value unless $value ~~ @$neighbors;
        }
        $self->{$vertex} = $neighbors;
    }
    else {
        $self->{$vertex} = $new_neighbor;
    }
    $self->set_edge_attribute($vertex, $new_neighbor->[0], $edge_attrs);
}

=item _remove_neighbor

The C<_remove_neighbor> method is an internal helper used for deleting
an edge between two vertices.

=cut

sub _remove_neighbor {
    ## String String -> State! OR Undef
    my ($self, $vertex, $neighbor) = @_;

    return unless $self->edge_between($vertex, $neighbor);

    # Graphs are represented as a hash of arrays that look like the
    # following:
    #
    # my $example = {
    #      's' => [ 'a', 'd' ],
    #      'a' => [ 's', 'b', 'd' ],
    #      'b' => [ 'a', 'c', 'e' ],
    #      'c' => [ 'b' ],
    #      'd' => [ 's', 'a', 'e' ],
    #      'e' => [ 'b', 'd', 'f' ],
    # };
    #
    # To delete a specific neighbor, we have to walk this vertex's
    # list of neighbors (skipping any already deleted neighbors) and
    # set the neighbor's value to undef.

    return unless $self->has_vertex($vertex);
    my $neighbors = $self->get_neighbors($vertex);

    for (my $i = 0; $i <= @$neighbors; $i++) {
        my $this = $self->{$vertex}->[$i];
        next unless defined $this;
        if ($this eq $neighbor) {
            $self->{$vertex}->[$i] = undef;
        }
    }
}

=item _st_walk

The C<_st_walk> method is used internally for building walks (paths)
along spanning trees, such as are built inside C<find_path_between>
and C<dijkstra>.

=cut

sub _st_walk {
    ## String String HashRef -> Array
    my ($self, $st, $start, $end) = @_;

    my @path;

    if (exists $st->{$start}->{distance}) {
        push @path, {vertex => $end, distance => $st->{$end}->{distance}};
        my $prev = $st->{$end}->{prev};

        while (1) {
            if ($prev eq $start) {

                push @path,
                  {vertex => $prev, distance => $st->{$prev}->{distance}};
                last;
            }
            push @path,
              {vertex => $prev, distance => $st->{$prev}->{distance}};
            $prev = $st->{$prev}->{prev};
            next;
        }
    }
    else {
        push @path, $end;
        my $prev = $st->{$end}->{prev};
        while (1) {
            if ($prev eq $start) {
                push @path, $start;
                last;
            }
            push @path, $prev;
            $prev = $st->{$prev}->{prev};
            next;
        }
    }
    return reverse @path;
}

=item _edge_attrs

The C<_edge_attrs> method is an internal helper that returns all of
the graph's edge attributes.

=cut

sub _edge_attrs {
    my ($self) = @_;
    return $self->{_INTERNAL}->{edge_attrs};
}

=item _vertex_attrs

The C<_vertex_attrs> method is an internal helper that returns all of
the graph's vertex attributes.

=cut

sub _vertex_attrs {
    my ($self) = @_;
    return $self->{_INTERNAL}->{vertex_attrs};
}

=item _make_vertex_name

The C<_make_vertex_name> method is used to generate random vertex
names, such as when generating random graphs.

=back

=cut

sub _make_vertex_name {
    ## -> String
    my $n     = int rand 10000;
    my $chars = qq[a b c d e f g h i j k l m n o p q r s t u v w x y z];
    my @chars = split / /, $chars;

    my $i  = rand scalar @chars;
    my $c1 = $chars[$i];
    my $c2 = $chars[rand scalar @chars];

    return qq[$c1$c2$n];
}

=head1 GRAPH COLORING METHODS

=over

=item get_color_degree

The C<get_color_degree> method returns the "color degree" of a vertex:
that is, how many colors its neighbors have.

=cut

sub get_color_degree {
    ## String -> Integer
    my ($self, $vertex) = @_;
    my $count = 0;
    my @colors;
    my $neighbors = $self->get_neighbors($vertex);
    for my $neighbor (@$neighbors) {
        my $color = $self->get_vertex_color($neighbor);
        if ($color) {
            $count++;
            push @colors, $color;
        }
    }
    return ($count, @colors);
}

=item color_vertices

The C<color_vertices> method colors the vertices of the graph using
the algorithm due to Brelaz, as described in Skiena, I<Implementing
Discrete Mathematics>.  Specifically:

=over

=item 1. Number the colors from 1 to k.

=item 2. Color the vertex of largest degree with color 1.

=item 3. Then repeatedly select the vertex with highest I<color
degree>, where the color degree is the number of adjacent vertices
which have already been colored, and color it with the smallest
possible color.

=back

=cut

sub color_vertices {
    ## -> State!
    my ($self) = @_;

    if ($self->is_directed) {
        my ($package, $filename, $line) = caller();
        die <<"EOF";
on line $line of file $filename:
  $package\:\:_color_vertices():
    is not implemented for directed graphs!
EOF
    }

    my @colors
      = qw/ violet indigo orange yellow blue green red/;  # Ordered by indices
    my @vertices_by_degree
      = sort { $self->get_degree($a) > $self->get_degree($b) }
      $self->get_vertices;

    my $v = pop @vertices_by_degree;
    $self->set_vertex_color($v, $colors[0]);

    my @vertices_by_color_degree
      = sort { $self->get_color_degree($a) > $self->get_color_degree($b) }
      $self->get_vertices;

    while (my $v = pop @vertices_by_color_degree) {
        my ($count, @adjacent_colors) = $self->get_color_degree($v);
        for my $color (@colors) {
            $self->set_vertex_color($v, $color)
              unless $color ~~ @adjacent_colors;
        }
        @vertices_by_color_degree
          = sort { $self->get_color_degree($a) > $self->get_color_degree($b) }
          @vertices_by_color_degree;
    }
}

=item uncolor_vertices

The C<uncolor_vertices> method "uncolors" every vertex in the graph by
setting its color attribute to C<undef>.

=cut

sub uncolor_vertices {
    my ($self) = @_;
    for my $vertex ($self->get_vertices) {
        $self->set_vertex_color($vertex, undef);
    }
}

=item vertex_colors

The C<vertex_colors> method returns a list containing each vertex and its color.

=cut

sub vertex_colors {
    ## -> Array[Hashref]
    my ($self) = @_;
    my @colors;
    for my $vertex ($self->get_vertices) {
        push @colors, [$vertex, {color => $self->get_vertex_color($vertex)}];
    }
    return @colors;
}

=item chromatic_number

The C<chromatic_number> method does not actually return the chromatic
number.  It returns the number of colors that were used to color the
vertices of the graph using the C<color_vertices> method.

=back

=cut

sub chromatic_number {
    ## -> Integer OR Undef
    my ($self) = @_;
    my $n      = 0;
    my @colors = $self->vertex_colors;
    return unless @colors;

    my %colors;
    for my $elem (@colors) {
        my $color = $elem->[1]->{color};
        $colors{$color}++;
    }
    my @keys = keys %colors;
    if (@keys) {
        $n = scalar @keys;
    }
    return $n;
}

1;

=pod

=head1 REFERENCES

=over

=item Even, Shimon. I<Graph Algorithms>.

=item Skiena, Steven. I<Implementing Discrete Mathematics>.

=back

=head1 SEE ALSO

=over

=item * L<Graph>, by Jarkko Hietaniemi

=item * L<Graph::Fast> by Lars Stoltenow

=item * L<Boost::Graph> by David Burdick

=back

=head1 AUTHOR

Richard Loveland <r@rmloveland.com>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2019, 2020, 2021 by Rich Loveland

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut
