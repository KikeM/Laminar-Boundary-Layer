import de.dislin.Dislin;

public class styles {
  public static void main (String args []) {
     int i, nx, ny;

     String ctit1 = "Demonstration of CURVE";
     String ctit2 = "Line Styles";

     String ctyp [] = {"SOLID", "DOT", "DASH", "CHNDSH",
                       "CHNDOT", "DASHM", "DOTL", "DASHL"};

     float x []  = {3.f, 9.f};
     float y []  = {0.f, 0.f};

     Dislin.metafl ("cons");
     Dislin.setpag ("da4p");

     Dislin.disini ();
     Dislin.pagera ();
     Dislin.complx ();
     Dislin.center ();

     Dislin.chncrv ("both");         
     Dislin.name   ("X-axis", "x");
     Dislin.name   ("Y-axis",  "y");

     Dislin.titlin (ctit1, 1);
     Dislin.titlin (ctit2, 3);

     Dislin.graf   (0.f, 10.f, 0.f, 2.f, 0.f, 10.f, 0.f, 2.f);
     Dislin.title  ();

     for (i = 0; i < 8; i++) {
       y[0] = 8.5f - i; 
       y[1] = 8.5f - i;
       nx = Dislin.nxposn (1.0f);
       ny = Dislin.nyposn (y[0]);
       Dislin.messag (ctyp[i], nx, ny - 20);
       Dislin.curve (x, y, 2);
     }

     Dislin.disfin  ();
  }
}
