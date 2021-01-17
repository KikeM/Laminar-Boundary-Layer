import de.dislin.Dislin;

public class polar {
  public static void main (String args []) {
     int n = 300, m = 10, i;
     double a, f = 3.1415926/180., step = 360. / (n-1);

     float x1 [] = new float [n];
     float y1 [] = new float [n];
     float x2 [] = new float [m];
     float y2 [] = new float [m];

     for (i = 0; i < n; i++) {
        a = (i * step) * f;
        y1[i] = (float) a;
        x1[i] = (float) Math.sin (5 * a);
     }

     for (i = 0; i < m; i++) {
        x2[i] = i + 1;
        y2[i] = i + 1;
     }    
    
     Dislin.setpag ("da4p");
     Dislin.metafl ("cons");
     Dislin.disini ();
     Dislin.pagera ();
     Dislin.complx ();

     Dislin.titlin ("Polar Plots", 2);
     Dislin.ticks  (3, "Y");
     Dislin.axends ("NOENDS", "X");
     Dislin.labdig (-1, "Y");
     Dislin.axslen (1000, 1000);
     Dislin.axsorg (1050, 900);

     Dislin.polar  (1.f, 0.f, 0.2f, 0.f, 30.f);
     Dislin.curve  (x1, y1, n);
     Dislin.htitle (50);
     Dislin.title  ();
     Dislin.endgrf ();

     Dislin.labdig (-1, "X");
     Dislin.axsorg (1050, 2250);
     Dislin.labtyp ("VERT", "Y");
     Dislin.polar  (10.f, 0.f, 2.f, 0.f, 30.f);
     Dislin.barwth (-5.f);
     Dislin.polcrv ("FBARS");
     Dislin.curve  (x2, y2, m);

     Dislin.disfin ();
  }
}
