#! /usr/bin/env python
import math
import dislin

ctit1 = 'Surface Plot of the Function'
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

dislin.metafl ('cons')
dislin.setpag ('da4p')
dislin.disini ()
dislin.pagera ()
dislin.complx ()

dislin.titlin (ctit1, 2)
dislin.titlin (ctit2, 4)

dislin.axspos (200, 2600)
dislin.axslen (1800, 1800)

dislin.name   ('X-axis', 'X')
dislin.name   ('Y-axis', 'Y')
dislin.name   ('Z-axis', 'Z')

dislin.view3d (-5., -5., 4., 'ABS')
dislin.graf3d  (0., 360., 0., 90., 0., 360., 0., 90.,
                -3., 3., -3., 1.)
dislin.height (50)
dislin.title  ()

dislin.color  ('green')
dislin.surmat (zmat, n, m, 1, 1)
dislin.disfin ()





