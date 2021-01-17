#!/usr/bin/perl
use Dislin;

$ctit = 'Interpolation Methods';

@xray = (0., 1., 3., 4.5, 6., 8., 9., 11., 12., 12.5, 
	 13., 15., 16., 17., 19., 20.);
@yray = (2., 4., 4.5, 3., 1., 7., 2., 3., 5., 2., 2.5,
	 2., 4., 6., 5.5, 4.);
@cpol = ('SPLINE', 'BARS', 'STEP', 'LINEAR');

Dislin::setpag ('da4p');
Dislin::metafl ('xwin');

Dislin::disini ();
Dislin::pagera ();
Dislin::complx ();

Dislin::incmrk (1);
Dislin::hsymbl (25);
Dislin::titlin ($ctit, 1);
Dislin::axslen (1500, 500);
Dislin::setgrf ('LINE', 'LINE', 'LINE', 'LINE');

$nya = 2700;
for ($i = 0; $i < 4; $i++) {
  Dislin::axspos (350, $nya - $i * 500);
  Dislin::polcrv ($cpol[$i]);
  Dislin::marker (0);
  Dislin::graf   (0., 20., 0., 5., 0., 10., 0., 5.);
  $nx = Dislin::nxposn (1.);
  $ny = Dislin::nyposn (8.);
  Dislin::messag ($cpol[$i], $nx, $ny);
  Dislin::curve  (\@xray, \@yray, 16);

  if ($i == 3) {
    Dislin::height (50);
    Dislin::title ();
  }
  Dislin::endgrf ();
}
Dislin::disfin ();


