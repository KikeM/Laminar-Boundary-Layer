#!/usr/bin/perl
use Dislin;

$ctit1 = 'Shaded Contour Plot';
$ctit2 = 'F(X,Y) = (X[2$ - 1)[2$ + (Y[2$ - 1)[2$';

$n = 50;
$m = 80;

$stepx = 1.6 / ($n - 1);
$stepy = 1.6 / ($m - 1);

for ($i = 0; $i < $n; $i++) {
  $xray[$i] = $i * $stepx;
  $x = $xray[$i] * $xray[$i] - 1.;
  $x2 = $x * $x;
  for ($j = 0; $j < $m; $j++) {
    $yray[$j] = $j * $stepy;
    $y = $yray[$j] * $yray[$j] - 1.;
    $y2 = $y * $y;
    $zmat[$i][$j] = $x2 + $y2;
  }
}

Dislin::metafl ('cons');
Dislin::setpag ('da4p');

Dislin::disini ();
Dislin::pagera ();
Dislin::complx ();
Dislin::mixalf ();

Dislin::titlin ($ctit1, 1);
Dislin::titlin ($ctit2, 3);

Dislin::name   ('X-axis', 'X');
Dislin::name   ('Y-axis', 'Y');

Dislin::axspos (450, 2670);
Dislin::shdmod ('poly', 'contur');
Dislin::graf   (0., 1.6, 0., 0.2, 0., 1.6, 0., 0.2);

for ($i = 0; $i < 12; $i++) {
  $zlev[11-$i] = 0.1 + $i * 0.1;
}

Dislin::conshd (\@xray, $n, \@yray, $m, \@zmat, \@zlev, 12);

Dislin::height (50);
Dislin::title  ();
Dislin::disfin ();





