#! /usr/bin/env python
import math
import dislin

n = 101
f = 3.1415926 / 180.
x = range (n)
y1 = range (n)
y2 = range (n)
for i in range (0,n):
  x[i] = i * 3.6
  v = i * 3.6 * f
  y1[i] = math.sin (v)
  y2[i] = math.cos (v)

dislin.metafl ('xwin')
dislin.disini ()
dislin.complx ()
dislin.pagera ()

dislin.axspos (450, 1800)
dislin.axslen (2200, 1200)

dislin.name   ('X-axis', 'X')
dislin.name   ('Y-axis', 'Y')

dislin.labdig (-1, 'X')
dislin.ticks  (10, 'XY')

dislin.titlin ('Demonstration of CURVE', 1)
dislin.titlin ('SIN (X), COS (X)', 3)
 
dislin.graf   (0., 360., 0., 90., -1., 1., -1., 0.5)
dislin.title  ()

dislin.color  ('red')
dislin.curve  (x, y1, n)
dislin.color  ('green')
dislin.curve  (x, y2, n)

dislin.color  ('foreground')
dislin.dash   ()
dislin.xaxgit ()
dislin.disfin ()


