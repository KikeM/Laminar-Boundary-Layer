#!/usr/bin/perl
use Dislin;

$ctit = 'Logarithmic Scaling';
@clab = ('LOG', 'FLOAT', 'ELOG');

Dislin::setpag ('da4p');
Dislin::metafl ('cons');

Dislin::disini ();
Dislin::pagera ();
Dislin::complx ();
Dislin::axslen (1400, 500);

Dislin::name   ('X-axis', 'X');
Dislin::name   ('Y-axis', 'Y');
Dislin::axsscl ('LOG', 'XY');

Dislin::titlin ($ctit, 2);

for ($i = 0; $i < 3; $i++) {
  $nya = 2650 - $i * 800;
  Dislin::labdig (-1, 'XY');
  if (i == 1) {
    Dislin::labdig (1, 'Y');
    Dislin::name   (' ', 'X');
  }

  Dislin::axspos (500, $nya);
  $cstr = 'Labels: ' . $clab[$i];
  Dislin::messag ($cstr, 600, $nya - 400);
  Dislin::labels ($clab[$i], 'XY');
  Dislin::graf   (0., 3., 0., 1., -1., 2., -1., 1.);
  if ($i == 2) {
    Dislin::height (50);
    Dislin::title ();
  }
  Dislin::endgrf ();
}
Dislin::disfin ();


