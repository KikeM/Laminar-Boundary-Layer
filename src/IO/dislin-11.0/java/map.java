import de.dislin.Dislin;

public class map {
  private static int id_draw, id_lis;

  private static String  cl1[] = {"Cylindrical Equidistant",
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
                      "Azimuthal Orthgraphic"};

  public static void myplot (int id) {
     String cl2[] = {"CYLI", "MERC", "EQUA", "HAMM", "AITO", "WINK",
                           "SANS", "CONI", "ALBE", "CONF", "AZIM", "LAMB",
                           "STER", "ORTH"};
     int isel;
     float xa = -180.f, xe = 180.f, xor = -180.f, xstp = 60.f,
           ya =  -90.f, ye =  90.f, yor =  -90.f, ystp = 30.f;

     isel = Dislin.gwglis (id_lis);
     Dislin.setxid (id_draw, "widget");
     Dislin.metafl ("xwin");
     Dislin.disini ();
     Dislin.erase  ();
     Dislin.complx ();

     if (isel >=4 && isel <= 7) {
       Dislin.noclip ();
     }
     else if (isel == 2) {
       ya = -85.f;
       ye = 85.f;
       yor = -60.f;
     }
     else if (isel >= 8 && isel <= 10) {
       ya = 0.f;
       ye = 90.f;
       yor = 0.f;
     }

     Dislin.labdig (-1, "xy");
     Dislin.name ("Longitude", "x");
     Dislin.name ("Latitude", "y");

     Dislin.projct (cl2[isel-1]);
     Dislin.htitle (50);
     Dislin.titlin (cl1[isel-1] + " Projection", 3);
     Dislin.grafmp (xa, xe, xor, xstp, ya, ye, yor, ystp);
     Dislin.title  ();
     Dislin.gridmp (1,1);
     Dislin.color  ("green");
     Dislin.world  ();
     Dislin.unit   (0);
     Dislin.disfin ();
  }

  public static void main (String args []) {
     int i, ip, id, ip1, ip2, id_but, id_quit;

     String clis;

     clis = cl1[0]; 
     for (i = 1; i < 14; i++) {
        clis = clis + '|' + cl1[i];
     }
          
     Dislin.swgtit ("DISLIN Map Plot"); 
     ip = Dislin.wgini ("hori");
     Dislin.swgwth (-15);
     ip1 = Dislin.wgbas (ip, "vert");
     Dislin.swgwth (-50);
     ip2 = Dislin.wgbas (ip, "vert");

     Dislin.swgdrw (2100.f/2970.f);
     id = Dislin.wglab (ip1, "Projection:");
     id_lis = Dislin.wglis (ip1, clis, 1);

     id_but = Dislin.wgpbut (ip1, "Plot");
     Dislin.swgcbk (id_but, "map.myplot");
 
     id_quit = Dislin.wgquit (ip1);

     id = Dislin.wglab (ip2, "DISLIN Draw Widget:");
     id_draw = Dislin.wgdraw (ip2);
     Dislin.wgfin ();
  }
}
