#!/usr/bin/perl
use Dislin;

$nproj = 14;
@cl1  = ("Cylindrical Equidistant", 
         "Mercator",
         "Cylindrical Equal-Area",
         "Hammer (Elliptical)",
         "Aitoff (Elliptical)",
         "Winkel (Elliptical)",
         "Sanson (Elliptical)",
         "Conical Equidistant",
         "Conical Equal-Area",
         "Conical Conformal",
         "Azimuthal Equidistant",
         "Azimuthal Equal-Area",
         "Azimuthal Stereographic",
         "Azimuthal Orthgraphic");

@cl2  = ("CYLI", "MERC", "EQUA", "HAMM", "AITO", "WINK",
         "SANS", "CONI", "ALBE", "CONF", "AZIM", "LAMB",
         "STER", "ORTH");

$clis = join ("|", @cl1);

Dislin::swgtit ("DISLIN Map Plot");
$ip  = Dislin::wgini ("hori");
Dislin::swgwth (-15);
$ip1 = wgbas ($ip, "vert");
Dislin::swgwth (-50);
$ip2 = wgbas ($ip, "vert");

Dislin::swgdrw (2100./2970.);
$id = Dislin::wglab ($ip1, "Projection:");
$id_lis = Dislin::wglis ($ip1, $clis, 1);

$id_but = Dislin::wgpbut ($ip1, "Plot");
Dislin::swgcbk ($id_but, "myplot");

$id_quit = Dislin::wgquit ($ip1);
$id = Dislin::wglab ($ip2, "DISLIN Draw Widget:");
$id_draw = Dislin::wgdraw ($ip2);
wgfin ();

sub myplot
{ 
  $xa   = -180.; 
  $xe   =  180.;
  $xor  = -180.;
  $xstp =   60.;

  $ya   = -90.; 
  $ye   =  90.;
  $yor  = -90.;
  $ystp =  30.;

  $isel = Dislin::gwglis ($id_lis);
  Dislin::setxid ($id_draw, "widget");
  Dislin::metafl ("xwin");
  Dislin::disini ();
  Dislin::erase ();
  Dislin::complx ();

  if ($isel >=4 && $isel <= 7) { 
    Dislin::noclip ();
  }
  elsif ($isel == 2) {
    $ya = -85;
    $ye = 85;
    $yor = -60;
  }
  elsif ($isel >= 8 && $isel <= 10) {
    $ya = 0;
    $ye = 90;
    $yor = 0;
  }

  Dislin::labdig (-1, "xy");
  Dislin::name ("Longitude", "x");
  Dislin::name ("Latitude", "y");

  Dislin::projct ($cl2[$isel-1]);
  $ctitle = $cl1[$isel-1] . " Projection";
  Dislin.titlin ($ctitle, 3);
  Dislin::htitle (50);

  Dislin::grafmp ($xa, $xe, $xor, $xstp, $ya, $ye, $yor, $ystp);
  Dislin::title ();
  Dislin::gridmp (1,1);
  Dislin::color ("green");
  Dislin::world();
  Dislin::unit (0);
  Dislin::disfin ();
}



