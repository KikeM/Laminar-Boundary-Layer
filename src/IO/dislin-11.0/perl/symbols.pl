#!/usr/bin/perl
use Dislin;

$ctit = 'Symbols';

Dislin::setpag ('da4p');
Dislin::metafl ('cons');

Dislin::disini ();
Dislin::pagera ();
Dislin::complx ();
Dislin::paghdr ('H. Michels  (', ')', 2, 0);

Dislin::height (60);
$nl = Dislin::nlmess ($ctit);
Dislin::messag ($ctit, (2100 - $nl)/2, 200);

Dislin::height (50);
Dislin::hsymbl (120);

$ny = 150;
for ($i = 0; $i < 22; $i++) {
    if (($i % 4) == 0) {
        $ny = $ny + 400;
        $nxp = 550;
    }
    else {
	$nxp = $nxp + 350;
    }
    $nl = Dislin::nlnumb ($i, -1);
    Dislin::number ($i, -1, $nxp - $nl/2, $ny + 150);
    Dislin::symbol ($i, $nxp, $ny);
}
Dislin::disfin ();

