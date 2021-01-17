#!/usr/bin/perl
use Dislin;

$ctit1 = 'Surface Plot of the Function';
$ctit2 = 'F(X,Y) = 2 * SIN(X) * SIN (Y)';

$n = 60;
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
Dislin::setpag ('da4p');
Dislin::disini ();
Dislin::pagera ();
Dislin::complx ();

Dislin::titlin ($ctit1, 2);
Dislin::titlin ($ctit2, 4);

Dislin::axspos (200, 2600);
Dislin::axslen (1800, 1800);

Dislin::name   ('X-axis', 'X');
Dislin::name   ('Y-axis', 'Y');
Dislin::name   ('Z-axis', 'Z');

Dislin::view3d (-5., -5., 4., 'ABS');
Dislin::graf3d  (0., 360., 0., 90., 0., 360., 0., 90.,
		 -3., 3., -3., 1.);
Dislin::height (50);
Dislin::title  ();

Dislin::color  ('green');
Dislin::surmat (\@zmat, $n, $m, 1, 1);
Dislin::disfin ();





