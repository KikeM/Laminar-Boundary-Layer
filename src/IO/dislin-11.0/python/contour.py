#! /usr/bin/env python
import math
import dislin

ctit1 = 'Contour Plot'
ctit2 = 'F(X,Y) = 2 * SIN(X) * SIN (Y)'

n = 50
m = 50

xray = range (n)
yray = range (m)
zmat = range (n*m)

fpi  = 3.1415927 / 180.
stepx = 360. / (n - 1)
stepy = 360. / (m - 1)

for i in range (0, n):
  xray[i] = xray[i] * stepx

for i in range (0, m):
  yray[i] = yray[i] * stepy

for i in range (0, n):
  x = xray[i] * fpi
  for j in range (0, m):
    y = yray[j] * fpi
    zmat[i*m+j] = 2 * math.sin(x) * math.sin(y)

dislin.metafl ('cons')
dislin.setpag ('da4p')
dislin.disini ()
dislin.pagera ()
dislin.complx ()

dislin.titlin (ctit1, 1)
dislin.titlin (ctit2, 3)

dislin.intax  ()
dislin.axspos (450, 2650)

dislin.name   ('X-axis', 'X')
dislin.name   ('Y-axis', 'Y')

dislin.graf   (0., 360., 0., 90., 0., 360., 0., 90.)
dislin.height (50)
dislin.title  ()

dislin.height (30)
for i in range (0, 9):
  zlev = -2. + i * 0.5
  if i == 4:
    dislin.labels ('NONE', 'CONTUR')
  else:
    dislin.labels ('FLOAT', 'CONTUR')

  dislin.setclr ((i+1) * 28)
  dislin.contur (xray, n, yray, m, zmat, zlev)

dislin.disfin ()





