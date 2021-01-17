import de.dislin.Dislin;

public class symbols {
  public static void main (String args []) {
     int ny = 150, nxp = 0, nl, i;
     String ctit = "Symbols", cstr = "  ";
  
     Dislin.metafl ("cons");
     Dislin.setpag ("da4p");
     Dislin.disini ();
     Dislin.color  ("yellow");
     Dislin.pagera ();
     Dislin.complx ();
     Dislin.paghdr ("H. Michels  (", ")", 2, 0);

     Dislin.height (60);
     nl = Dislin.nlmess (ctit);
     Dislin.messag (ctit, (2100 - nl) / 2, 200);

     Dislin.height (50);
     Dislin.hsymbl (120);

     for (i = 0; i < 22; i++) { 
       if ((i % 4) == 0) {
         ny  += 400;
         nxp  = 550;
       }
       else {
         nxp += 350;
       }

       cstr = "" + i;
       nl = Dislin.nlmess (cstr) / 2;
       Dislin.messag (cstr, nxp - nl, ny + 150);
       Dislin.symbol (i, nxp, ny);
     }
     Dislin.disfin ();
  }
}
