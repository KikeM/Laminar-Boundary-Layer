#!/usr/bin/env python
import dislin

ix  = [0, 300, 300,   0]
iy  = [0,   0, 400, 400]
ixp = [0, 0, 0, 0]
iyp = [0, 0, 0, 0]

ctit = 'Shading Patterns (AREAF)'

dislin.metafl ('cons')
dislin.disini ()
dislin.setvlt ('small')
dislin.pagera ()
dislin.complx ()

dislin.height (50)
nl = dislin.nlmess (ctit)
dislin.messag (ctit, (2970 - nl)/2, 200)

nx0 = 335
ny0 = 350

iclr = 0
for i in range (0, 3):
  ny = ny0 + i * 600
  for j in range (0, 6):
    nx = nx0 + j * 400
    ii = i * 6 + j
    dislin.shdpat (ii)
    iclr = iclr + 1
    dislin.setclr (iclr)
    for k in range (0, 4):
      ixp[k] = ix[k] + nx
      iyp[k] = iy[k] + ny

    dislin.areaf (ixp, iyp, 4)
    nl = dislin.nlnumb (ii, -1)
    nx = nx + (300 - nl) / 2
    dislin.color ('foreground')
    dislin.number (ii, -1, nx, ny + 460) 

dislin.disfin ()

