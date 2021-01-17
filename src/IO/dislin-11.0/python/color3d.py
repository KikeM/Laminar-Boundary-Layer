#! /usr/bin/env python
import math
import dislin

ctit1 = '3-D  Colour Plot of the Function'
ctit2 = 'F(X,Y) = 2 * SIN(X) * SIN (Y)'

n = 50
m = 50
zmat = range(n*m)

fpi  = 3.1415927 / 180.
stepx = 360. / (n - 1)
stepy = 360. / (m - 1)

for i in range (0, n):
  x = i * stepx
  for j in range (0, m):
    y = j * stepy
    zmat[i*m+j] = 2 * math.sin(x * fpi) * math.sin(y * fpi)

dislin.metafl ('xwin')
dislin.disini ()
dislin.pagera ()
dislin.complx ()

dislin.titlin (ctit1, 1)
dislin.titlin (ctit2, 3)

dislin.name   ('X-axis', 'X')
dislin.name   ('Y-axis', 'Y')
dislin.name   ('Z-axis', 'Z')

dislin.intax  ()
dislin.autres (n, m)
dislin.axspos (300, 1850)
dislin.ax3len (2200, 1400, 1400)

dislin.graf3   (0., 360., 0., 90., 0., 360., 0., 90.,
                -2., 2., -2., 1.)
dislin.crvmat (zmat, n, m, 1, 1)
dislin.height (50)
dislin.title  ()
dislin.disfin ()





