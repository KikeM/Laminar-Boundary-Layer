#! /usr/bin/env python
import math
import dislin

ctit1 = 'Shaded Contour Plot'
ctit2 = 'F(X,Y) = (X[2$ - 1)[2$ + (Y[2$ - 1)[2$'

n = 50
m = 50
xray = range (n)
yray = range (m)
zlev = range (12)
zmat = range (n*m)

stepx = 1.6 / (n - 1)
stepy = 1.6 / (m - 1)

for i in range (0, n):
  xray[i] = xray[i] * stepx

for i in range (0, m):
  yray[i] = yray[i] * stepy

for i in range (0, n):
  x = xray[i] * xray[i] - 1.
  x = x * x
  for j in range (0, m):
    y = yray[j] * yray[j] - 1.
    zmat[i*m+j] = x + y * y

dislin.metafl ('cons')
dislin.setpag ('da4p')

dislin.disini ()
dislin.pagera ()
dislin.complx ()
dislin.mixalf ()

dislin.titlin (ctit1, 1)
dislin.titlin (ctit2, 3)

dislin.name   ('X-axis', 'X')
dislin.name   ('Y-axis', 'Y')

dislin.axspos (450, 2670)
dislin.shdmod ('poly', 'contur')
dislin.graf   (0., 1.6, 0., 0.2, 0., 1.6, 0., 0.2)

for i in range (0, 12):
  zlev[11-i] = 0.1 + i * 0.1

dislin.conshd (xray, n, yray, m, zmat, zlev, 12)

dislin.height (50)
dislin.title  ()

dislin.disfin ()





