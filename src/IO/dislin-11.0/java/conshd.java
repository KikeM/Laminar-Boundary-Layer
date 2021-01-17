import de.dislin.Dislin;

public class conshd {
  public static void main (String args []) {
     int n = 50, m = 50, i, j;
     float zmat []  = new float [n*m];
     float xray []  = new float [n];
     float yray []  = new float [m];
     float zlev []  = new float [12];

     String ctit1 = "Shaded Contour Plot";
     String ctit2 = "F(X,Y) = (X[2$ - 1)[2$ + (Y[2$ - 1)[2$";

     double x, y;
     double stepx = 1.6 / (n-1);
     double stepy = 1.6 / (m-1);

     for (i = 0; i < n; i++) {
        xray[i] = (float) (i * stepx);
     }

     for (j = 0; j < m; j++) {
        yray[j] = (float) (j * stepy);
     }

     for (i = 0; i < n; i++) {
       x = xray[i] * xray[i] - 1.;
       x *= x;
       for (j = 0; j < m; j++) {
         y = yray[j] * yray[j] - 1;
         zmat[i*m+j] = (float) (x + y * y);
       }
     }
       
     Dislin.metafl ("cons");
     Dislin.setpag ("da4p");
     Dislin.disini ();
     Dislin.pagera ();
     Dislin.complx ();
     Dislin.mixalf ();

     Dislin.titlin (ctit1, 1);
     Dislin.titlin (ctit2, 3);
     Dislin.name   ("X-axis", "x");
     Dislin.name   ("Y-axis", "y");
     Dislin.axspos (450, 2670);
     Dislin.shdmod ("poly", "contur");         

     Dislin.graf   (0.f, 1.6f, 0.f, 0.2f,
                    0.f, 1.6f, 0.f, 0.2f);
     for (i = 0; i < 12; i++) {
       zlev[11-i] = 0.1f + i * 0.1f;
     }

     Dislin.conshd (xray, n, yray, m, zmat, zlev, 12);

     Dislin.height (50);
     Dislin.title  ();
     Dislin.disfin ();
  }
}

