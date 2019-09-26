package TinyGraph;

use strict;
use warnings;
use experimentals;
use Smart::Match;
use Text::CSV;
use Hash::PriorityQueue;

our $attrs = {};

sub new {
    my $self  = shift;
    my $graph = [];

    bless $graph, $self;
    return $graph;
}

sub write_csv {
    ## Filename -> State! IO!
    my ( $self, $f ) = @_;

    open my $fh, '>:encoding(utf8)', $f or die "Can't open file '$f': $!\n";

    say $fh qq[node,neighbor,weight];

    my @vertices = $self->get_vertices;

    for my $vertex (@vertices) {
        next unless defined $vertex;
        my $neighbors = $self->get_neighbors($vertex);
        for my $neighbor (@$neighbors) {
            next unless defined $neighbor;
            my $weight =
              $self->get_edge_attribute( $vertex, $neighbor, 'weight' ) || 0;
            my @cols = ( $vertex, $neighbor, $weight );
            my $line = join ',', @cols;
            say $fh $line;
        }
    }
    close $fh;
}

sub read_csv {
    ## Filename -> State! IO!
    my ( $self, $f ) = @_;

    my $csv = Text::CSV->new( { binary => 1 } );

    open my $fh, "<:encoding(utf8)", $f or die "Can't open file '$f': $!\n";

    while ( my $line = $csv->getline($fh) ) {
        my @cols     = @$line;
        my $vertex   = $cols[0];
        my $neighbor = $cols[1];
        my $weight   = $cols[2];

        next unless defined $weight;
        next if $vertex eq 'node';

        $self->_add_neighbor( $vertex, [$neighbor] );

        if ($attrs) {
            $self->set_edge_attribute( $vertex, $neighbor,
                { weight => $weight } );
        }
    }
}

sub _find_index {
    ## Int -> Int OR undef
    my ( $self, $wanted ) = @_;

    return unless defined $wanted;

    # Naive linear search, for now.
    my $i = 0;
    for my $elem (@$self) {

        # Definedness check here is necessary because when we delete a
        # vertex from the graph, we do so by setting the vertex's
        # array index to undef.
        if ( defined $elem->[0] && $elem->[0] eq $wanted ) {
            return $i;
        }
        $i++;
    }
    return;
}

sub get_neighbors {
    ## String -> ArrayRef
    my ( $self, $vertex ) = @_;

    my $index = $self->_find_index($vertex);

    if ( defined $index ) {
        return $self->[$index]->[1];
    }
    else {
        return;
    }
}

sub is_empty {
    ## -> Boolean
    my $self     = shift;
    my $count    = 0;
    my @vertices = $self->get_vertices;

    my $return = 1;

    for my $vertex (@vertices) {
        if ( defined $vertex ) {
            undef $return;
        }
    }
    return $return;
}

sub has_vertex {
    ## String -> Boolean
    my ( $self, $vertex ) = @_;

    return 1 if defined $self->_find_index($vertex);
    return;
}

sub remove_vertex {
    ## String -> State!
    my ( $self, $vertex ) = @_;

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
        $self->_remove_neighbor( $neighbor, $vertex );
        $self->delete_edge_attributes( $vertex, $neighbor );
    }

    # Then, we delete the "root" reference to the vertex by setting it
    # to undef.
    my $index = $self->_find_index($vertex);
    if ( defined $index ) {
        $self->[$index] = undef;
    }
}

sub get_vertices {
    ## -> Array
    my $self = shift;
    my @vertices;
    for my $vertex (@$self) {
        push @vertices, $vertex->[0];
    }
    return @vertices;
}

sub get_edge {
    ## String String -> ArrayRef
    my ( $self, $a, $b ) = @_;

    return unless $self->edge_between( $a, $b );

    my $attrs = $self->get_edge_attributes( $a, $b );

    return [ $a, $b, $attrs ];
}

sub get_edges {
    ## -> Array
    my ($self) = @_;

    my @vertices = $self->get_vertices;

    my @answer;
    my %seen;

    for my $vertex (@vertices) {
        next unless defined $vertex;
        my $neighbors = $self->get_neighbors($vertex);

        for my $neighbor (@$neighbors) {
            next unless defined $neighbor;
            next if $seen{ $vertex . $neighbor };
            push @answer, $self->get_edge( $vertex, $neighbor );
            $seen{ $vertex . $neighbor }++;
            $seen{ $neighbor . $vertex }++;
        }
    }

    return @answer;
}

sub to_graphviz {
    ## ArrayRef -> String
    my ( $self, $path ) = @_;

    my @buffer;
    my %seen;

    push @buffer, qq[graph { ];

    for my $vertex (@$self) {
        next unless defined $vertex->[0];
        push @buffer, $vertex->[0];
        push @buffer, qq[ -- ];
        push @buffer, qq[ { ];
        my $neighbors = $vertex->[1];
        for my $vertex (@$neighbors) {
            if ( $vertex ~~ @$path ) {
                push @buffer, qq{$vertex [style=filled fillcolor=red]};
            }
            else {
                push @buffer, $vertex;
            }
        }
        push @buffer, qq[ } ];
    }

    push @buffer, qq[ } ];

    return join ' ', @buffer;
}

sub to_weighted_graphviz {
    ## ArrayRef -> String
    my ( $self, $path ) = @_;

    my @buffer;
    my %seen;
    my @path;

    @path = map { $_->{vertex} } @$path;

    push @buffer, qq[graph {\n];

    for my $vertex (@$self) {
        next unless defined $vertex->[0];
        my $v         = $vertex->[0];
        my $neighbors = $vertex->[1];
        for my $neighbor (@$neighbors) {
            my $edge_weight =
              $self->get_edge_attribute( $v, $neighbor, 'weight' );

            if ( $neighbor ~~ @path ) {
                push @buffer,
qq{$v -- $neighbor [label="$edge_weight"] $neighbor [style=filled, color=red];\n};
            }
            else {
                push @buffer, qq{$v -- $neighbor [label="$edge_weight"];\n};
            }
        }
    }

    push @buffer, qq[ \n} ];

    return join ' ', @buffer;
}

sub _add_neighbor {
    ## String ArrayRef HashRef -> State!
    my ( $self, $vertex, $neighbor, $data ) = @_;

    my $index = $self->_find_index($vertex);

    unless ( ref($neighbor) eq 'ARRAY' ) {
        my ( $package, $filename, $line ) = caller();
        die <<"EOF";
on line $line of file $filename:
  $package\:\:_add_neighbor('$vertex', '$neighbor', '$data'):
    expected arrayref, got '$neighbor'
EOF
    }

    if ( defined $index ) {
        my $neighbors = $self->[$index]->[1];
        for my $value (@$neighbor) {
            push @$neighbors, $value;
        }
        $self->[$index]->[1] = $neighbors;
    }
    else {
        push @$self, [ $vertex, $neighbor ];
    }

    if ($data) {
        $self->set_edge_attribute( $vertex, $neighbor->[0], $data );
    }
}

sub edge_between {
    ## String String -> Boolean
    my ( $self, $a, $b ) = @_;

    return unless ( defined $a && defined $b );
    return 1 if $a eq $b;

    my $neighbors = $self->get_neighbors($a);
    if ( $b ~~ @$neighbors ) {
        return 1;
    }
    else { return; }
}

sub dijkstra {
    ## String String -> Array
    my ( $self, $start, $end ) = @_;

    return () unless defined $start && defined $end;

    my @path;
    my @queue;
    my %seen;
    my $heap = Hash::PriorityQueue->new;
    my $st   = {};

    $st->{$start}->{distance} = 0;
    $st->{$start}->{prev}     = undef;

    for my $vertex ( $self->get_vertices ) {
        next if $vertex eq $start;
        $st->{$vertex}->{distance} = 1_000_000;
        $st->{$vertex}->{prev}     = undef;
    }

    $heap->insert( $start, $st->{$start}->{distance} );

    while ( my $v = $heap->pop() ) {
        my $neighbors = $self->get_neighbors($v);

        for my $neighbor (@$neighbors) {
            next if $seen{$neighbor};
            $seen{$neighbor}++;

            # In this block, we are setting up the information we will
            # need to answer the question "Have we found a new
            # shortest path (by distance)?"
            my $distance_to_self         = $st->{$v}->{distance};
            my $old_distance_to_neighbor = $st->{$neighbor}->{distance};
            my $edge_weight_to_neighbor =
              $self->get_edge_attribute( $v, $neighbor, 'weight' );
            my $new_distance_to_neighbor =
              $distance_to_self + $edge_weight_to_neighbor;

            # This is the core of Dijkstra's algorithm: Have we
            # discovered a path whose distance to the neighbor is
            # shorter than the previously discovered path's distance?
            # If yes, we update the spanning tree with this new path
            # information.
            if ( $new_distance_to_neighbor < $old_distance_to_neighbor ) {
                $st->{$neighbor}->{distance} = $new_distance_to_neighbor;
                $st->{$neighbor}->{prev}     = $v;
            }

            if ( $neighbor eq $end ) {
                @path = $self->_st_weighted_walk( $start, $end, $st );
                return @path;
            }
            else {
                $heap->insert( $neighbor, $st->{$neighbor}->{distance} );
            }
        }

        $seen{$v}++;
    }
    return ();
}

sub _st_weighted_walk {
    ## String String HashRef -> Array
    my ( $self, $start, $end, $st ) = @_;

    my @path;
    push @path, { vertex => $end, distance => $st->{$end}->{distance} };

    my $prev = $st->{$end}->{prev};
    while (1) {
        if ( $prev eq $start ) {

            push @path,
              { vertex => $prev, distance => $st->{$prev}->{distance} };
            last;
        }

        push @path, { vertex => $prev, distance => $st->{$prev}->{distance} };
        $prev = $st->{$prev}->{prev};
        next;
    }
    return reverse @path;
}

sub find_path_between {
    ## String String -> Array
    my ( $self, $start, $end ) = @_;

    return () unless defined $start && defined $end;

    my @path;     # Path so far
    my @queue;    # Vertices still to visit.
    my %seen;     # Vertices already seen.
    my $found;    # Whether we have found the wanted vertex.
    my $st = {};  # Spanning tree, used to find paths.

    if ( $start eq $end ) {
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
            $self->_st_add( $v, $neighbor, $st );
            if ( $neighbor eq $end ) {
                $found++;
                @path = $self->_st_walk( $start, $end, $st );
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

sub _st_walk {
    ## String String HashRef -> Array
    my ( $self, $start, $end, $st ) = @_;

    my @path;

    push @path, $end;
    my $prev = $st->{$end}->{prev};
    while (1) {
        if ( $prev eq $start ) {
            push @path, $start;
            last;
        }
        push @path, $prev;
        $prev = $st->{$prev}->{prev};
        next;
    }
    return reverse @path;
}

sub _st_add {
    ## String String HashRef -> State!
    my ( $self, $vertex, $neighbor, $st ) = @_;
    $st->{$vertex}->{$neighbor} = 1;      # Possibly unnecessary.
    $st->{$neighbor}->{prev} = $vertex;
}

sub get_edge_attributes {
    ## String String -> HashRef OR undef
    my ( $self, $start, $end ) = @_;
    my $pairkey = $start . $end;
    return $attrs->{$pairkey};
}

sub get_edge_attribute {
    ## String String String -> Value OR undef
    my ( $self, $start, $end, $attribute ) = @_;

    my $pairkey = $start . $end;
    return $attrs->{$pairkey}->{$attribute};
}

sub set_edge_attribute {
    ## String String HashRef -> State!
    # set_edge_attribute('s', 'a', { weight => 12 });
    my ( $self, $start, $end, $new_attrs ) = @_;

    my $pairkey1 = $start . $end;
    my $pairkey2 = $end . $start;

    # Attributes hashref already exists, so we add to it.  NOTE:
    # this is a hash so the update is destructive.
    for ( my ( $k, $v ) = each %$new_attrs ) {
        next unless defined $k;
        next if $k eq '';
        $attrs->{$pairkey1}->{$k} = $v;
        $attrs->{$pairkey2}->{$k} = $v;
    }
}

sub delete_edge_attributes {
    ## String String -> Undefined OR State!
    my ( $self, $start, $end ) = @_;
    return unless defined $start && defined $end;

    my $pairkey1 = $start . $end;
    my $pairkey2 = $end . $start;
    return unless ( exists $attrs->{$pairkey1} && exists $attrs->{$pairkey2} );
    delete $attrs->{$pairkey1};
    delete $attrs->{$pairkey2};
}

sub is_complete {
    my $self = shift;

    my @vertices = $self->get_vertices;
    my $v        = pop @vertices;

    my $neighbors = $self->get_neighbors($v);

    @vertices = sort { ( $a || '' ) cmp( $b || '' ) } @vertices;
    my @neighbors = sort { ( $a || '' ) cmp( $b || '' ) } @$neighbors;

    return 1 if @vertices ~~ @neighbors;

    return;
}

sub add_vertex {
    ## String -> State!
    my ( $self, $vertex ) = @_;
    push @$self, [ $vertex, [] ];
}

sub add_vertices {
    my ( $self, @vertices ) = @_;
    $self->add_vertex($_) for @vertices;
}

sub add_edge {
    ## String String -> State!
    my ( $self, $v1, $v2, $attrs ) = @_;
    $self->_add_neighbor( $v1, [$v2], $attrs );
    $self->_add_neighbor( $v2, [$v1], $attrs );
}

sub add_edges {
    my ( $self, @edges ) = @_;

    for my $elem (@edges) {
        ## ['a', 'b', { weight => 123 }]
        my ( $a, $b, $attrs ) = @$elem;
        $self->add_edge( $a, $b, $attrs );
    }
}

sub remove_edge {
    ## String String -> Boolean State! OR Undef
    my ( $self, $a, $b ) = @_;

    return unless $self->edge_between( $a, $b );

    # We delete A from B's list of neighbors, and delete B from A's
    # list of neighbors.  Then, we delete any edge attributes, since
    # said edge no longer exists.

    $self->_remove_neighbor( $a, $b );
    $self->_remove_neighbor( $b, $a );
    $self->delete_edge_attributes( $a, $b );

    return 1;
}

sub _remove_neighbor {
    ## String String -> State! OR Undef
    my ( $self, $vertex, $neighbor ) = @_;

    return unless $self->edge_between( $vertex, $neighbor );

    # Graphs are represented as an array of arrays that look like the
    # following:
    #
    # my $example = [
    #     [ 's', [ 'a', 'd' ] ],
    #     [ 'a', [ 's', 'b', 'd' ] ],
    #     [ 'b', [ 'a', 'c', 'e' ] ],
    #     [ 'c', ['b'] ],
    #     [ 'd', [ 's', 'a', 'e' ] ],
    #     [ 'e', [ 'b', 'd', 'f' ] ]
    # ];
    #
    # To delete a specific neighbor, we have to walk this vertex's
    # list of neighbors (skipping any already deleted neighbors) and
    # set the neighbor's value to undef.

    my $vertex_index = $self->_find_index($vertex);
    return unless defined $vertex_index;
    my $neighbors = $self->get_neighbors($vertex);

    for ( my $i = 0 ; $i <= @$neighbors ; $i++ ) {
        my $this = $self->[$vertex_index]->[1]->[$i];
        next unless defined $this;
        if ( $this eq $neighbor ) {
            $self->[$vertex_index]->[1]->[$i] = undef;
        }
    }
}

sub generate_random_vertices {
    ## HashRef -> State!
    my ( $self, $args ) = @_;

    my $n = $args->{n};

    # As we loop through the nodes, for each node A, this is the
    # probability that another randomly selected node B is NOT
    # connected to A.  In other words, this is the probability that
    # there is NOT an edge A-B.
    my $p          = $args->{p};
    my $max_weight = $args->{max_weight};

    my %seen;

    for my $node ( 1 .. $n ) {
        my $name = $self->_make_vertex_name;
        redo if $seen{$name};
        $seen{$name}++;
    }

    my @nodes = keys %seen;

    my @pairs;

    for my $node (@nodes) {
        my $maybe_neighbor = $nodes[ rand $#nodes ];
        next if $maybe_neighbor eq $node;
        my $connection_prob = rand 1;
        my $dist            = int rand $max_weight;
        if ( $connection_prob > $p ) {
            push @pairs, [ $node,           $maybe_neighbor, $dist ];
            push @pairs, [ $maybe_neighbor, $node,           $dist ];
        }
        redo
          if rand 1 > 0.8;    # Sometimes, add more neighbors to this node.
    }

    for my $pair (@pairs) {
        $self->_add_neighbor(
            $pair->[0],
            [ $pair->[1] ],
            { weight => $pair->[2] }
        );
    }
}

sub _make_vertex_name {
    ## -> String
    my $n     = int rand 10000;
    my $chars = qq[a b c d e f g h i j k l m n o p q r s t u v w x y z];
    my @chars = split / /, $chars;

    my $i  = rand scalar @chars;
    my $c1 = $chars[$i];
    my $c2 = $chars[ rand scalar @chars ];

    return qq[$c1$c2$n];
}

sub get_degree {
    my ( $self, $vertex ) = @_;
    if ( $self->has_vertex($vertex) ) {
        my $neighbors = $self->get_neighbors($vertex);
        if ( defined $neighbors ) {
            return scalar @$neighbors;
        }
    }
    return;
}

sub clone {
    my ($self) = @_;
    my $class  = ref($self);
    my $new    = [];

    for my $vertex ( $self->get_vertices ) {
        my $neighbors = $self->get_neighbors($vertex);
        push @$new, [ $vertex, [@$neighbors] ];
    }

    bless $new, $class;
}

sub equals {
    my ( $self, $other ) = @_;

    return
      unless $self->isa('TinyGraph')
      && $other->isa('TinyGraph');

    my @xs = sort { ( $a || '' ) lt( $b || '' ) } $self->get_vertices;
    my @ys = sort { ( $a || '' ) lt( $b || '' ) } $other->get_vertices;

    return unless @xs ~~ @ys;

    my @es = sort { $a->[0] lt $b->[0] } $self->get_edges;
    my @fs = sort { $a->[0] lt $b->[0] } $other->get_edges;

    return unless @es ~~ @fs;

    return 1;
}

1;
