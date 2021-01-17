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

dislin.axspos (450, 1800)
dislin.axslen (2200, 1200)

dislin.name   ('X-axis', 'X')
dislin.name   ('Y-axis', 'Y')

dislin.labdig (-1, 'X')
dislin.ticks  (10, 'XY')

dislin.titlin ('Demonstration of CURVE', 1)
dislin.titlin ('Legend', 3)
 
dislin.graf   (0., 360., 0., 90., -1., 1., -1., 0.5)
dislin.title  ()

dislin.chncrv ('LINE')
dislin.curve  (x, y1, n)
dislin.curve  (x, y2, n)

cbuf = ' '                   
dislin.legini (cbuf, 2, 7)     # cbuf is a dummy parameter for python
nx = dislin.nxposn (190.)
ny = dislin.nyposn (0.75)
dislin.leglin (cbuf, 'sin (x)', 1)
dislin.leglin (cbuf, 'cos (x)', 2)
dislin.legpos (nx, ny)
dislin.legtit ('Legend')
dislin.legend (cbuf, 3)
dislin.disfin ()


