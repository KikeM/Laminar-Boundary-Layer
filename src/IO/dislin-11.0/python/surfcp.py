#! /usr/bin/env python
import math
import dislin

def myfunc (x, y, iopt):
  if iopt == 1:
     xv = math.cos(x)*(3+math.cos(y))
  elif iopt == 2:
     xv = math.sin(x)*(3+math.cos(y))
  else:
     xv = math.sin(y)
  return xv

ctit1 = 'Surface Plot of the Parametric Function'
ctit2 = '[COS(t)*(3+COS(u)), SIN(t)*(3+COS(u)), SIN(u)]'

pi  = 3.1415927

dislin.scrmod ('revers')
dislin.metafl ('cons')
dislin.setpag ('da4p')
dislin.disini ()
dislin.pagera ()
dislin.complx ()

dislin.titlin (ctit1, 2)
dislin.titlin (ctit2, 4)

dislin.axspos (200, 2400)
dislin.axslen (1800, 1800)

dislin.name   ('X-axis', 'X')
dislin.name   ('Y-axis', 'Y')
dislin.name   ('Z-axis', 'Z')
dislin.intax  ()

dislin.vkytit (-300)
dislin.zscale (-1.,1.)
dislin.surmsh ('on')

dislin.graf3d  (-4.,4.,-4.,1.,-4.,4.,-4.,1.,-3., 3., -3., 1)
dislin.height (40)
dislin.title  ()

step = 2 * pi / 30.

dislin.surfcp (myfunc, 0., 2*pi, step, 0., 2*pi, step)
dislin.disfin ()





