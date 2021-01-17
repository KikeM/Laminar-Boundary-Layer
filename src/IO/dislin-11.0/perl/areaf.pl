#!/usr/bin/perl
use Dislin;

@ix  = (0, 300, 300,   0);
@iy  = (0,   0, 400, 400);
@ixp = (0, 0, 0, 0);
@iyp = (0, 0, 0, 0);

$ctit = "Shading patterns (AREAF)";

Dislin::metafl ("cons");
Dislin::disini ();
Dislin::setvlt ("small");
Dislin::pagera ();
Dislin::complx ();

Dislin::height (50);
$nl = Dislin::nlmess ($ctit);
Dislin::messag ($ctit, (2970 - $nl)/2, 200);

$nx0 = 335;
$ny0 = 350;

$iclr = 0;
for ($i = 0; $i < 3; $i++) {
    $ny = $ny0 + $i * 600;

    for ($j = 0; $j < 6; $j++) {
        $nx = $nx0 + $j * 400;
        $ii = $i * 6 + $j;
        Dislin::shdpat ($ii);
        $iclr = $iclr + 1;
        Dislin::setclr ($iclr);
        for ($k = 0; $k < 4; $k++) {
	    $ixp[$k] = $ix[$k] + $nx;
	    $iyp[$k] = $iy[$k] + $ny;
        }

        Dislin::areaf (\@ixp, \@iyp, 4);
        $nl = Dislin::nlnumb ($ii, -1);
        $nx = $nx + (300 - $nl) / 2;
        Dislin::color ("foreground");
        Dislin::number ($ii, -1, $nx, $ny + 460); 
    }
}
Dislin::disfin ();

