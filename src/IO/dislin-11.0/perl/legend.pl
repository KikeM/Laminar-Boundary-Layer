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
Dislin::titlin ('Legend', 3);
 
Dislin::graf   (0., 360., 0., 90., -1., 1., -1., 0.5);
Dislin::title  ();

Dislin::chncrv ('LINE');
Dislin::curve  (\@xray, \@y1ray, $n);
Dislin::curve  (\@xray, \@y2ray, $n);

Dislin::legini ($cbuf, 2, 7);
$nx = Dislin::nxposn (190.);
$ny = Dislin::nyposn (0.75);
Dislin::leglin ($cbuf, 'sin (x)', 1);
Dislin::leglin ($cbuf, 'cos (x)', 2);
Dislin::legpos ($nx, $ny);
Dislin::legtit ('Legend');
Dislin::legend ($cbuf, 3);
Dislin::disfin ();


