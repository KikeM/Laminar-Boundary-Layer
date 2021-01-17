#!/usr/bin/perl
use Dislin;

$ctit1 = '3-D Colour Plot of the Function';
$ctit2 = 'F(X,Y) = 2 * SIN(X) * SIN (Y)';

$n = 50;
$m = 50;

$fpi  = 3.1415927 / 180.;
$stepx = 360. / ($n - 1);
$stepy = 360. / ($m - 1);

for ($i = 0; $i < $n; $i++) {
    $x[$i] = $i * $stepx;
    for ($j = 0; $j < $m; $j++) {
	$y[$j] = $j * $stepy;
        $zmat[$i][$j] = 2 * sin($x[$i] * $fpi) * sin($y[$j] * $fpi);
    }
}

Dislin::metafl ('cons');
Dislin::disini ();
Dislin::pagera ();
Dislin::complx ();

Dislin::titlin ($ctit1, 2);
Dislin::titlin ($ctit2, 4);

Dislin::name   ('X-axis', 'X');
Dislin::name   ('Y-axis', 'Y');
Dislin::name   ('Z-axis', 'Z');

Dislin::intax  ();
Dislin::autres ($n, $m);
Dislin::axspos (300, 1850);
Dislin::ax3len (2200, 1400, 1400);

Dislin::graf3  (0., 360., 0., 90., 0., 360., 0., 90.,
		 -2., 2., -2., 1.);
Dislin::crvmat (\@zmat, $n, $m, 1, 1);
Dislin::height (50);
Dislin::title  ();
Dislin::disfin ();





