#!/usr/bin/env python
import dislin

ctit1 = 'Demonstration of CURVE'
ctit2 = 'Line Styles'

ctyp = ['SOLID', 'DOT', 'DASH', 'CHNDSH', 
        'CHNDOT', 'DASHM', 'DOTL', 'DASHL']
x = [3., 9.]
y = [0., 0.]

dislin.metafl ('cons')
dislin.setpag ('da4p')

dislin.disini ()
dislin.pagera ()
dislin.complx ()
dislin.center ()

dislin.chncrv ('BOTH')
dislin.name   ('X-axis', 'X')
dislin.name   ('Y-axis', 'Y')

dislin.titlin (ctit1, 1)
dislin.titlin (ctit2, 3)

dislin.graf   (0., 10., 0., 2., 0., 10., 0., 2.)
dislin.title  ()

for i in range (0, 8):
  y[0] = 8.5 - i
  y[1] = 8.5 - i
  nx = dislin.nxposn (1.0)
  ny = dislin.nyposn (y[0])
  dislin.messag (ctyp[i], nx, ny - 20)
  dislin.curve  (x, y, 2)

dislin.disfin ()


