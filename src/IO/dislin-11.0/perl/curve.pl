#!/usr/bin/perl
use Dislin;

$n = 101;
$pi = 3.1415926;
$f = $pi / 180.;
$step = 360. / ($n - 1);
for ($i = 0; $i < $n; $i++) {
    $xray[$i] = $i * $step;
    $x = $xray[$i] * $f;
    $y1ray[$i] = sin ($x);
    $y2ray[$i] = cos ($x);
}

Dislin::metafl ('xwin');
Dislin::disini ();
Dislin::complx ();
Dislin::pagera ();

Dislin::axspos (450, 1800);
Dislin::axslen (2200, 1200);

Dislin::name   ('X-axis', 'X');
Dislin::name   ('Y-axis', 'Y');

Dislin::labdig (-1, 'X');
Dislin::ticks  (10, 'XY');

Dislin::titlin ('Demonstration of CURVE', 1);
Dislin::titlin ('SIN (X), COS (X)', 3);
 
Dislin::graf   (0., 360., 0., 90., -1., 1., -1., 0.5);
Dislin::title  ();

Dislin::color  ('red');
Dislin::curve  (\@xray, \@y1ray, $n);
Dislin::color  ('green');
Dislin::curve  (\@xray, \@y2ray, $n);

Dislin::color  ('foreground');
Dislin::dash   ();
Dislin::xaxgit ();
Dislin::disfin ();


