#!/usr/bin/perl
use Dislin;

$ctit1 = 'Contour Plot of the Function';
$ctit2 = 'F(X,Y) = 2 * SIN(X) * SIN (Y)';

$n = 60;
$m = 50;

$fpi  = 3.1415927 / 180.;
$stepx = 360. / ($n - 1);
$stepy = 360. / ($m - 1);

for ($i = 0; $i < $n; $i++) {
    $xray[$i] = $i * $stepx;
    for ($j = 0; $j < $m; $j++) {
	$yray[$j] = $j * $stepy;
        $zmat[$i][$j] = 2 * sin($xray[$i] * $fpi) * sin($yray[$j] * $fpi);
    }
}

Dislin::metafl ('cons');
Dislin::setpag ('da4p');
Dislin::disini ();
Dislin::pagera ();
Dislin::complx ();

Dislin::titlin ($ctit1, 2);
Dislin::titlin ($ctit2, 4);

Dislin::axspos (450, 2670);

Dislin::name   ('X-axis', 'X');
Dislin::name   ('Y-axis', 'Y');

Dislin::graf  (0., 360., 0., 90., 0., 360., 0., 90.);

Dislin::height (30);
for ($i = 0; $i < 9; $i++) {
    $zlev = -2. + $i * 0.5; 
    if ($i == 4) {
	Dislin::labels ('NONE', 'CONTUR');
    }
    else {
      Dislin::labels ('FLOAT', 'CONTUR');
    }
    Dislin::setclr (($i+1) * 28);
    Dislin::contur (\@xray, $n, \@yray, $m, \@zmat, $zlev);
}

Dislin::color  ('FORE');
Dislin::height (50);
Dislin::title  ();
Dislin::disfin ();





