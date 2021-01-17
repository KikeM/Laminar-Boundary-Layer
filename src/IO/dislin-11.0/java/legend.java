import de.dislin.Dislin;

public class legend {
  public static void main (String args []) {
     int n = 100, i, nx, ny;
     float xray  [] = new float [n];
     float y1ray [] = new float [n];
     float y2ray [] = new float [n];

     String cbuf = "";

     double x;
     double fpi = 3.1415926/180.;
     double step = 360. / (n-1);
     for (i = 0; i < n; i++) {
        xray[i] = (float) (i * step);
        x = xray[i] * fpi;
        y1ray[i] = (float) Math.sin (x);
        y2ray[i] = (float) Math.cos (x);
     }
       
     Dislin.metafl ("cons");
     Dislin.disini ();
     Dislin.pagera ();
     Dislin.complx ();

     Dislin.axspos (450, 1800);
     Dislin.axslen (2200, 1200);
         
     Dislin.name   ("X-axis", "x");
     Dislin.name   ("Y-axis",  "y");

     Dislin.labdig (-1, "x");
     Dislin.ticks  (10, "xy");

     Dislin.titlin ("Demonstration of CURVE", 1);
     Dislin.titlin ("Legend", 3);

     Dislin.graf   (0.f, 360.f, 0.f, 90.f,
                     -1.f, 1.f, -1.f, 0.5f);
     Dislin.title  ();
     Dislin.xaxgit ();

     Dislin.chncrv ("both"); 
     Dislin.curve  (xray, y1ray, n);
     Dislin.curve  (xray, y2ray, n);

     Dislin.legini (cbuf, 2, 7);
     nx = Dislin.nxposn (190.f);
     ny = Dislin.nyposn (0.75f);
     Dislin.legpos (nx, ny);
     Dislin.leglin (cbuf, "sin (x)", 1);
     Dislin.leglin (cbuf, "cos (x)", 2);
     Dislin.legtit ("Legend");
     Dislin.legend (cbuf, 3);

     Dislin.disfin ();
  }
}







