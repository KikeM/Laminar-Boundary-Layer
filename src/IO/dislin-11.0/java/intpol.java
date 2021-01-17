import de.dislin.Dislin;

public class intpol {
  public static void main (String args []) {
     int i, nya = 2700, nx, ny;
     String ctit = "Interpolation Methods";
     float xray [] = {0.f, 1.f, 3.f, 4.5f, 6.f, 8.f, 9.f, 11.f, 12.f,
                      12.5f, 13.f, 15.f, 16.f, 17.f, 19.f, 20.f};
     float yray [] = {2.f, 4.f, 4.5f, 3.f, 1.f, 7.f, 2.f, 3.f, 5.f, 
                      2.f, 2.5f, 2.f, 4.f, 6.f, 5.5f, 4.f};
     String cpol [] = {"SPLINE", "STEM", "BARS", "STAIRS", 
                       "STEP", "LINEAR"};

     Dislin.setpag ("da4p");
     Dislin.metafl ("cons");

     Dislin.disini ();
     Dislin.pagera ();
     Dislin.complx ();

     Dislin.incmrk (1);
     Dislin.hsymbl (25);
     Dislin.titlin (ctit, 1);
     Dislin.axslen (1500, 350);
     Dislin.setgrf ("LINE", "LINE", "LINE", "LINE");

     for (i = 0; i < 6; i++) {
       Dislin.axspos (350, nya - i * 350);
       Dislin.polcrv (cpol[i]);
       Dislin.marker (0);
       Dislin.graf   (0.f, 20.f, 0.f, 5.f, 0.f, 10.f, 0.f, 5.f);
       nx = Dislin.nxposn (1.f);
       ny = Dislin.nyposn (8.f);
       Dislin.messag (cpol[i], nx, ny);
       Dislin.curve  (xray, yray, 16);

       if (i == 5) {
         Dislin.height (50);
         Dislin.title ();
       }
       Dislin.endgrf ();
     }
     Dislin.disfin ();
  }
}
