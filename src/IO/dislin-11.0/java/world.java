import de.dislin.Dislin;

public class world {
  public static void main (String args []) {
     Dislin.metafl ("cons");
     Dislin.disini ();
     Dislin.pagera ();
     Dislin.complx ();

     Dislin.frame  (3);
     Dislin.axspos (400, 1850);
     Dislin.axslen (2400, 1400);
         
     Dislin.name   ("Longitude", "x");
     Dislin.name   ("Latitude",  "y");
     Dislin.titlin ("World Coastlines and Lakes",3);

     Dislin.labels ("map","xy");
     Dislin.grafmp (-180.f, 180.f, -180.f, 90.f,
                     -90.f,  90.f,  -90.f, 30.f);

     Dislin.gridmp (1, 1);
     Dislin.color  ("green");
     Dislin.world  ();
   
     Dislin.color  ("fore");
     Dislin.height (50);
     Dislin.title  (); 
     Dislin.disfin ();
  }
}
