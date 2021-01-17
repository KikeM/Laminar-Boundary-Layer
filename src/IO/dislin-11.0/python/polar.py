#! /usr/bin/env python
import math
import dislin

n = 300
m = 10
f = 3.1415927/180.
x1 = range (n)
y1 = range (n)
x2 = range (m) 
y2 = range (m)
step = 360./(n-1)
for i in range (0,n):
  a = (i * step) * f
  y1[i] = a
  x1[i] = math.sin (5*a)

for i in range (0,m):
  x2[i] = i + 1
  y2[i] = i + 1

dislin.setpag ('da4p')
dislin.metafl ('cons')
dislin.disini ()
dislin.complx ()
dislin.pagera ()

dislin.titlin ('Polar Plots', 2)
dislin.ticks  (3, 'Y')
dislin.axends ('NOENDS', 'X')
dislin.labdig (-1, 'Y')
dislin.axslen (1000, 1000)
dislin.axsorg (1050, 900)

dislin.polar  (1.,0., 0.2, 0., 30.)
dislin.curve  (x1, y1, n)
dislin.htitle (50)
dislin.title  ()
dislin.endgrf ()

dislin.labdig (-1, 'X')
dislin.axsorg (1050, 2250)
dislin.labtyp ('VERT', 'Y')
dislin.polar  (10.,0.,2.,0.,30.)
dislin.barwth (-5.)
dislin.polcrv ('FBARS')
dislin.curve  (x2, y2, m)

dislin.disfin ()


