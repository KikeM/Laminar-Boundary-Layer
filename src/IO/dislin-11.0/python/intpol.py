#!/usr/bin/env python
import dislin

ctit = 'Interpolation Methods'

xray = [0., 1., 3., 4.5, 6., 8., 9., 11., 12., 12.5, 
        13., 15., 16., 17., 19., 20.]
yray = [2., 4., 4.5, 3., 1., 7., 2., 3., 5., 2., 2.5,
        2., 4., 6., 5.5, 4.]
cpol = ['SPLINE', 'BARS', 'STEP', 'LINEAR']

dislin.setpag ('da4p')
dislin.metafl ('cons')

dislin.disini ()
dislin.pagera ()
dislin.complx ()

dislin.incmrk (1)
dislin.hsymbl (25)
dislin.titlin (ctit, 1)
dislin.axslen (1500, 500)
dislin.setgrf ('LINE', 'LINE', 'LINE', 'LINE')

nya = 2700
for i in range (0, 4):
  dislin.axspos (350, nya - i * 500)
  dislin.polcrv (cpol[i])
  dislin.marker (0)
  dislin.graf   (0., 20., 0., 5., 0., 10., 0., 5.)
  nx = dislin.nxposn (1.)
  ny = dislin.nyposn (8.)
  dislin.messag (cpol[i], nx, ny)
  dislin.curve  (xray, yray, 16)

  if i == 3:
    dislin.height (50)
    dislin.title ()

  dislin.endgrf ()

dislin.disfin ()


