#!perl

use strict;
use warnings;
use autodie;
use experimentals;
use Data::Dumper;
use SVG::Plot;

use constant X => 0;
use constant Y => 1;

use constant DEBUG => undef;

sub make_point {
    my ( $x, $y ) = @_;
    return [ $x, $y ];
}

sub make_random_point {
    my ( $max_x, $max_y ) = @_;
    return [ int rand $max_x, int rand $max_y ];
}

sub make_random_points {
    ## Int Int -> List
    my ( $width, $height, $n ) = @_;
    my @points = map { make_random_point( $width, $height ) } 0 .. $n;
}

sub theta {
    ## ArrayRef ArrayRef -> Num
    my ( $p1, $p2 ) = @_;
    my $t;

    my $dx = $p2->[X] - $p1->[X];
    my $ax = abs $dx;
    my $dy = $p2->[Y] - $p1->[Y];
    my $ay = abs $dy;

    if ( $dx == 0 && $dy == 0 ) {
        $t = 0;
    }
    else {
        $t = $dy / ( $ax + $ay );
    }

    if ( $dx < 0 ) {
        $t = 2 - $t;
    }
    elsif ( $dy < 0 ) {
        $t = 4 + $t;
    }

    my $answer = $t * 90;
    return $answer;
}

my @k = qw/a b c d e f g h i j k l m n o p/;

sub theta2 {
    my ( $anchor, $p2 ) = @_;
    my $t;

    my $dx = $p2->[X] - $anchor->[X];
    my $dy = $p2->[Y] - $anchor->[Y];

    if ( $dx == 0 && $dy == 0 ) {
        $t = 0;
    }
    else {
        $t = atan2( $p2->[Y] - $anchor->[Y], $p2->[X] - $anchor->[X] );
    }

    $t *= 90;
    return $t;
}

sub distance {
    my ( $p1, $p2 ) = @_;
    return
      sqrt( abs( ( $p2->[X] - $p1->[X] )**2 - ( $p2->[Y] - $p1->[Y] )**2 ) );
}

sub get_anchor {
    my @points = @_;

    my $min_x = 1_000_000;
    my $min_y = 1_000_000;
    my $anchor;

    for my $p (@points) {
        if ( $p->[Y] < $min_y ) {
            $min_y  = $p->[Y];
            $anchor = $p;
        }
        if ( $p->[X] < $min_x ) {
            $min_x = $p->[X];
        }
    }
    return $anchor;
}

sub clockwise {
    my ( $p1, $p2, $p3 ) = @_;

    # Return POSITIVE if turning right (clockwise)
    # Return NEGATIVE if turning left (counterclockwise)
    # Return ZERO if on the same vector

    return ( $p3->[X] - $p1->[X] ) * ( $p2->[Y] - $p1->[Y] ) -
      ( $p2->[X] - $p1->[X] ) * ( $p3->[Y] - $p1->[Y] );
}

sub convex_hull {
    ## Array -> Array
    my @polygon = @_;
    my @hull;

    # Input: The ordered list of points that make up the simple polygon.
    #
    # Output: The ordered list of points that makes up the convex hull;

    my $min_x = 1_000_000_000;
    my $leftmost;
    for my $p (@polygon) {
        if ( $p->[X] < $min_x ) {
            $min_x    = $p->[X];
            $leftmost = $p;
        }
    }

    push @hull, $leftmost;

    # Start at index 1 to skip the anchor.
    my $leftmost_index = 0;
    my $max_theta      = 0;
  LOOP: for ( my $i = 1 ; $i < @polygon ; $i++ ) {
        my $theta = theta( $hull[$#hull], $polygon[$i] );

        say qq[THETA: $theta] if DEBUG;

        if ( $theta > $max_theta ) {
            $max_theta = $theta;
            say qq[NEW MAX THETA: $max_theta] if DEBUG;
            $leftmost_index = $i;
        }
    }

    if ( @polygon > 0 && $polygon[$leftmost_index] != $hull[0] ) {
        say qq[pushing @{$polygon[$leftmost_index]} onto hull] if DEBUG;
        push @hull, $polygon[$leftmost_index];
        say qq[POLYGON BEFORE:], Dumper \@polygon if DEBUG;
        splice( @polygon, $leftmost_index, 1 );
        say qq[POLYGON AFTER:], Dumper \@polygon if DEBUG;
        $max_theta      = 0;
        $leftmost_index = 0;
        goto LOOP;
    }

    push @hull, $hull[0] unless $hull[0] eq $hull[$#hull];
    return @hull;
}

sub simple_closed_path {
    ## Array -> Array
    ## [[1,2],[3,4],...] => [[1,2],[3,4],...]
    my @points = @_;
    my @path;

    my $anchor = get_anchor(@points);
    @path = sort { theta( $anchor, $a ) <=> theta( $anchor, $b ) } @points;

    push @path, $path[0];
    return @path;
}

# Graphical output routines

sub plot_points {
    ## Array -> String
    # Input:
    # [[12, 10], [3, 12], ... ]
    my @points = @_;

    my $plot = SVG::Plot->new(
        points     => \@points,
        max_width  => 600,
        max_height => 600,
        point_size => 3,
        line       => 'follow',
        margin     => 6,
    );

    return $plot->plot;
}

sub main {
    my @points = (
        [ 3,  9 ],  [ 11, 1 ],  [ 6,  8 ],  [ 4,  3 ],
        [ 5,  15 ], [ 8,  11 ], [ 1,  6 ],  [ 7,  4 ],
        [ 9,  7 ],  [ 14, 5 ],  [ 10, 13 ], [ 16, 14 ],
        [ 15, 2 ],  [ 13, 16 ], [ 3,  12 ], [ 12, 10 ],
    );

    # Scale the point sizes so they are appropriate in a 600x600 SVG image.
    my @scaled = map { [ $_->[X] * 40, $_->[Y] * 40 ] } @points;

    my @path = simple_closed_path(@scaled);

    open my $fh, '>', 'closed-path.svg';
    say $fh plot_points(@path);
    close $fh;

    my @hull = convex_hull(@path);

    # Hull from Sedgewick 2nd ed.: B M L N E O G D
    # B: [11,  1]
    # M: [15,  2]
    # L: [16, 14]
    # N: [13, 16]
    # E: [5,  15]
    # O: [3,  12]
    # G: [1,   6]
    # D: [4,   3]

    open my $fh2, '>', 'convex-hull.svg';
    say $fh2 plot_points(@hull);
    close $fh2;
}

main();
