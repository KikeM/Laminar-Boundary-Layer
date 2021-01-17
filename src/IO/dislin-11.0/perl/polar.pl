#!/usr/bin/perl
use Dislin;

$n = 300;
$m = 10;
$pi = 3.1415926;
$f = $pi / 180.;
$step = 360. / ($n - 1);
for ($i = 0; $i < $n; $i++) {
    $a = ($i * $step) * $f; 
    $xray[$i] = $i * $step;
    $x = $xray[$i] * $f;
    $y1[$i] = $a;
    $x1[$i] = sin (5 * $a);
}

for ($i = 0; $i < $m; $i++) {
    $x2[$i] = $i + 1;
    $y2[$i] = $i + 1;
}

Dislin::setpag ('da4p');
Dislin::metafl ('cons');
Dislin::disini ();
Dislin::complx ();
Dislin::pagera ();

Dislin::titlin ('Polar Plots', 2);
Dislin::ticks  (3, 'Y');
Dislin::axends ('NOENDS', 'X');
Dislin::labdig (-1, 'Y');
Dislin::axslen (1000, 1000);
Dislin::axsorg (1050, 900);

Dislin::polar  (1.,0., 0.2, 0., 30.);
Dislin::curve  (\@x1, \@y1, $n);
Dislin::htitle (50);
Dislin::title  ();
Dislin::endgrf ();

Dislin::labdig (-1, 'X');
Dislin::axsorg (1050, 2250);
Dislin::labtyp ('VERT', 'Y');
Dislin::polar  (10.,0.,2.,0.,30.);
Dislin::barwth (-5.);
Dislin::polcrv ('FBARS');
Dislin::curve  (\@x2, \@y2, $m);

Dislin::disfin ();


