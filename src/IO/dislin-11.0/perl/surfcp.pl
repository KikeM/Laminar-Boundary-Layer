#!/usr/bin/perl
use Dislin;

$ctit1 = 'Surface Plot of the Parametric Function';
$ctit2 = '[COS(t)*(3+COS(u)), SIN(t)*(3+COS(u)), SIN(u)]';

$pi  = 3.1415927;

Dislin::scrmod ('revers');
Dislin::metafl ('cons');
Dislin::setpag ('da4p');
Dislin::disini ();
Dislin::pagera ();
Dislin::complx ();

Dislin::titlin ($ctit1, 2);
Dislin::titlin ($ctit2, 4);

Dislin::axspos (200, 2400);
Dislin::axslen (1800, 1800);

Dislin::name   ('X-axis', 'X');
Dislin::name   ('Y-axis', 'Y');
Dislin::name   ('Z-axis', 'Z');
Dislin::intax  ();

Dislin::vkytit (-300);
Dislin::zscale (-1.,1.);
Dislin::surmsh ('on');

Dislin::graf3d  (-4.,4.,-4.,1.,-4.,4.,-4.,1.,-3., 3., -3., 1);
Dislin::height (40);
Dislin::title  ();

$step = 2 * $pi / 30.;

Dislin::surfcp (myfunc, 0., 2*$pi, $step, 0., 2*$pi, $step);
Dislin::disfin ();

sub myfunc 
{ 
  my ($x, $y, $iopt) = @_ ;
  if ($iopt == 1) {
    $xv = cos($x)*(3+cos($y));
  }
  elsif ($iopt == 2) {
    $xv = sin($x)*(3+cos($y));
  }
  else {
    $xv = sin($y);
  }
  return $xv;
}


