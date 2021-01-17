import de.dislin.Dislin;

public class logscl {
  public static void main (String args []) {
     int i, nya;
     String ctit = "Logarithmic Scaling";
     String clab [] = {"LOG", "FLOAT", "ELOG"};

     Dislin.setpag ("da4p");
     Dislin.metafl ("cons");

     Dislin.disini ();
     Dislin.pagera ();
     Dislin.complx ();
     Dislin.axslen (1400, 500);

     Dislin.name   ("X-axis", "X");
     Dislin.name   ("Y-axis", "Y");
     Dislin.axsscl ("LOG", "XY");

     Dislin.titlin (ctit, 2);

     for (i = 0; i < 3; i++) {
       nya = 2650 - i * 800;
       Dislin.labdig (-1, "XY");
       if (i == 1) {
         Dislin.labdig (1, "Y");
         Dislin.name   (" ", "X");
       }
       Dislin.axspos (500, nya);
       Dislin.messag ("Labels: " + clab[i], 600, nya - 400);
       Dislin.labels (clab[i], "XY");
       Dislin.graf   (0.f, 3.f, 0.f, 1.f, -1.f, 2.f, -1.f, 1.f);

       if (i == 2) {
         Dislin.height (50);
         Dislin.title ();
       }
       Dislin.endgrf ();
     }
     Dislin.disfin ();
  }
}
