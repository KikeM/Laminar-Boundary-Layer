import de.dislin.Dislin;

public class contour {
  public static void main (String args []) {
     int n = 50, m = 50, i, j;
     float zlev;
     float zmat []  = new float [n*m];
     float xray []  = new float [n];
     float yray []  = new float [m];

     String ctit1 = "Contour Plot";
     String ctit2 = "F(X,Y) = 2*SIN(X)*SIN(Y)";

     double x, y;
     double fpi = 3.1415926/180.;
     double stepx = 360. / (n-1);
     double stepy = 360. / (m-1);

     for (i = 0; i < n; i++) {
        xray[i] = (float) (i * stepx);
     }

     for (j = 0; j < m; j++) {
        yray[j] = (float) (j * stepy);
     }

     for (i = 0; i < n; i++) {
       x = xray[i] * fpi;
       for (j = 0; j < m; j++) {
         y = yray[j] * fpi;
         zmat[i*m+j] = (float) (2 * Math.sin(x)* Math.sin(y));
       }
     }
       
     Dislin.metafl ("cons");
     Dislin.setpag ("da4p");
     Dislin.disini ();
     Dislin.pagera ();
     Dislin.complx ();

     Dislin.titlin (ctit1, 1);
     Dislin.titlin (ctit2, 3);
     Dislin.intax  ();
     Dislin.axspos (450, 2650);
         
     Dislin.name   ("X-axis", "x");
     Dislin.name   ("Y-axis", "y");

     Dislin.graf   (0.f, 360.f, 0.f, 90.f,
                    0.f, 360.f, 0.f, 90.f);
     Dislin.height (50);
     Dislin.title  ();

     Dislin.height (30);
     for (i = 0; i < 9; i++) {
       zlev = -2.f + i * 0.5f;
       if (i == 4) {
         Dislin.labels ("none", "contur");
       }
       else {
         Dislin.labels ("float", "contur");
       }
       Dislin.setclr ((i+1) * 28);
       Dislin.contur (xray, n, yray, m, zmat, zlev);
     }
     Dislin.disfin ();
  }
}

