import de.dislin.Dislin;

public class vectors {
  public static void main (String args []) {

     int ivec [] = {0, 1111, 1311, 1421, 1531, 1701, 1911,
         3111, 3311, 3421, 3531, 3703, 4221, 4302,
	 4413, 4522, 4701, 5312, 5502, 5703};
     int nl, nx = 300, ny = 400, i;
     String ctit = "Vectors";
       
     Dislin.metafl ("cons");
     Dislin.disini ();
     Dislin.pagera ();
     Dislin.complx ();

     Dislin.height (60);
     nl = Dislin.nlmess (ctit);
     Dislin.messag (ctit, (2970 - nl)/2, 200);

     Dislin.height (50);
     for (i = 0; i < 20; i++) {
       if (i == 10) {
         nx += 2970 / 2;
         ny = 400;
       }

       nl = Dislin.nlnumb (ivec[i], -1);
       Dislin.number (ivec[i], -1, nx -nl, ny - 25);
       Dislin.vector (nx + 100, ny, nx + 1000, ny, ivec[i]);
       ny += 160;
     }

     Dislin.disfin ();
  }
}
