#!perl

use strict;
use warnings;
use autodie;
use experimentals;
use Data::Dumper;
use SVG::Plot;

use constant X => 0;
use constant Y => 1;

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

    return $t * 90;
}

my @k = qw/a b c d e f g h i j k l m n o p/;

my @points = (
    [ 3, 9 ], [ 11, 1 ], [ 6, 8 ], [ 4,  3 ], [ 5,  15 ], [ 8,  11 ],
    [ 1, 6 ], [ 7,  4 ], [ 9, 7 ], [ 14, 5 ], [ 10, 13 ], [ 16, 14 ],
    [ 15, 2 ], [ 13, 16 ], [ 3, 12 ], [ 12, 10 ],
);

# Scale the point sizes so they are appropriate in a 600x600 SVG image.
@points = map { [ $_->[X] * 40, $_->[Y] * 40 ] } @points;

# @points = make_random_points( 600, 600, 8 );

my %points;
for ( my $i = 0 ; $i < @k ; $i++ ) {
    $points{ $k[$i] } = $points[$i];
}

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

    # Input: The ordered list of points that make up the simple polygon.
    #
    # Output: The ordered list of points that makes up the convex hull;

    my @hull;

    # while ( my ( $p1, $p2, $p3 ) = splice @polygon, 0, 3 ) {
    #     if ( clockwise( $p1, $p2, $p3 ) >= 0 ) {
    #         push @hull, ( $p1, $p2, $p3 );
    #     }
    # }

    my $anchor    = get_anchor(@polygon);
    my $prev_dist = 0;
  LOOP: while ( my $curr = pop @polygon ) {

        my $dist_from_anchor = distance( $anchor, $curr );

        push @hull, $curr;

        if ( $dist_from_anchor >= $prev_dist ) {

            # If the current point is further from the anchor than the
            # previous point, we pop the previous point from the hull
            # and add the current one.
            $prev_dist = $dist_from_anchor;
            pop @hull;
            push @hull, $curr;
        }
    }

    push @hull, $hull[0];
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
    my @path = simple_closed_path(@points);

    my $plot = plot_points(@path);

    open my $fh, '>', 'convex-hull.svg';
    say $fh $plot;
    close $fh;
}

main();
