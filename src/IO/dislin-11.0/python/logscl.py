#! /usr/bin/env python
import dislin

ctit = 'Logarithmic Scaling'
clab = ['LOG', 'FLOAT', 'ELOG']

dislin.setpag ('da4p')
dislin.metafl ('cons')

dislin.disini ()
dislin.pagera ()
dislin.complx ()
dislin.axslen (1400, 500)

dislin.name   ('X-axis', 'X')
dislin.name   ('Y-axis', 'Y')
dislin.axsscl ('LOG', 'XY')

dislin.titlin (ctit, 2)

for i in range (0, 3):
  nya = 2650 - i * 800
  dislin.labdig (-1, 'XY')
  if i == 1:
    dislin.labdig (1, 'Y')
    dislin.name   (' ', 'X')

  dislin.axspos (500, nya)
  dislin.messag ('Labels: ' + clab[i], 600, nya - 400)
  dislin.labels (clab[i], 'XY')
  dislin.graf   (0., 3., 0., 1., -1., 2., -1., 1.)
  if i == 2:
    dislin.height (50)
    dislin.title ()

  dislin.endgrf ()

dislin.disfin ()


