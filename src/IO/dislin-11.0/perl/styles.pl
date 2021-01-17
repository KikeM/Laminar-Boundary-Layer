#!/usr/bin/perl
use Dislin;

$ctit1 = 'Demonstration of CURVE';
$ctit2 = 'Line Styles';

@ctyp = ('SOLID', 'DOT', 'DASH', 'CHNDSH', 
	 'CHNDOT', 'DASHM', 'DOTL', 'DASHL');
@x = (3., 9.);
@y = (0., 0.);

Dislin::metafl ('cons');
Dislin::setpag ('da4p');

Dislin::disini ();
Dislin::pagera ();
Dislin::complx ();
Dislin::center ();

Dislin::chncrv ('BOTH');
Dislin::name   ('X-axis', 'X');
Dislin::name   ('Y-axis', 'Y');

Dislin::titlin ($ctit1, 1);
Dislin::titlin ($ctit2, 3);

Dislin::graf   (0., 10., 0., 2., 0., 10., 0., 2.);
Dislin::title  ();

for ($i = 0; $i < 8; $i++) {
    $y[0] = 8.5 - $i;
    $y[1] = 8.5 - $i;
    $nx = Dislin::nxposn (1.0);
    $ny = Dislin::nyposn ($y[0]);
    Dislin::messag ($ctyp[$i], $nx, $ny - 20);
    Dislin::curve  (\@x, \@y, 2);
}
Dislin::disfin ();


