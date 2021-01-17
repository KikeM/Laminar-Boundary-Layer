import de.dislin.Dislin;

public class color3d {
  public static void main (String args []) {
     int n = 50, m = 50, i, j;
     float zmat []  = new float [n*m];

     String ctit1 = "3-D Colour Plot of the Function";
     String ctit2 = "F(X,Y) = 2*SIN(X)*SIN(Y)";

     double x, y;
     double fpi = 3.1415926/180.;
     double stepx = 360. / (n-1);
     double stepy = 360. / (m-1);
     for (i = 0; i < n; i++) {
       x = i * stepx;
       for (j = 0; j < m; j++) {
         y = j * stepy;
         zmat[i*m+j] = (float) (2 * Math.sin(x*fpi)* Math.sin(y*fpi));
       }
     }
       
     Dislin.metafl ("cons");
     Dislin.disini ();
     Dislin.pagera ();
     Dislin.complx ();

     Dislin.titlin (ctit1, 1);
     Dislin.titlin (ctit2, 3);
         
     Dislin.name   ("X-axis", "x");
     Dislin.name   ("Y-axis", "y");
     Dislin.name   ("Z-axis", "z");

     Dislin.intax  ();
     Dislin.autres (n, m);
     Dislin.axspos (300, 1850);
     Dislin.ax3len (2200, 1400, 1400);

     Dislin.graf3  (0.f, 360.f, 0.f, 90.f,
                    0.f, 360.f, 0.f, 90.f,
                    -2.f, 2.f, -2.f, 1.f);
     Dislin.crvmat (zmat, n, m, 1, 1);
     Dislin.height (50);
     Dislin.title  ();
     Dislin.disfin ();
  }
}

