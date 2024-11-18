#!perl

use strict;
use warnings;
use autodie;
use feature qw/ say /;
use lib 'lib';
use YAGL;
use Cwd;
use Test::More tests => 1;

my $cwd = getcwd;

my $g = YAGL->new;
$g->read_csv(qq[$cwd/data/three-triangles.csv]);

chomp(my $got = $g->to_graphviz);

my $expected = <<"EOF";
graph { 
 a  --   {  b [ label=b  ]  } [label=13]
 a  --   {  c [ label=c  ]  } [label=7]
 b  --   {   } [label=13]
 b  --   {  c [ label=c  ]  } [label=9]
 b  --   {  d [ label=d  ]  } [label=16]
 c  --   {   } [label=7]
 c  --   {   } [label=9]
 c  --   {  g [ label=g  ]  } [label=23]
 d  --   {   } [label=16]
 d  --   {  e [ label=e  ]  } [label=3]
 d  --   {  f [ label=f  ]  } [label=3]
 e  --   {   } [label=3]
 e  --   {  f [ label=f  ]  } [label=12]
 f  --   {   } [label=3]
 f  --   {   } [label=12]
 f  --   {  h [ label=h  ]  } [label=18]
 g  --   {   } [label=23]
 g  --   {  h [ label=h  ]  } [label=1]
 g  --   {  i [ label=i  ]  } [label=6]
 h  --   {   } [label=18]
 h  --   {   } [label=1]
 h  --   {  i [ label=i  ]  } [label=9]
 i  --   {   } [label=6]
 i  --   {   } [label=9]
  } 
EOF

chomp($expected);

ok( $got eq $expected, "Graphviz data format is as expected" );

$g->draw('tsp');

__END__

#        A
#       / \
#      /   \
#      B - C
#     /     \
#    /       \
#   /         \
#   D          G
# /  \         / \
# E - F ----- H - I

# Local Variables:
# compile-command: "cd .. && perl t/33-to-graphviz.t"
# End:
