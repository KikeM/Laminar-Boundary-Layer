#!/usr/bin/env python
import dislin

ivec  = [0, 1111, 1311, 1421, 1531, 1701, 1911,
         3111, 3311, 3421, 3531, 3703, 4221, 4302,
         4413, 4522, 4701, 5312, 5502, 5703]

ctit = 'Vectors'

dislin.metafl ('cons')
dislin.disini ()
dislin.pagera ()
dislin.complx ()

dislin.height (60)
nl = dislin.nlmess (ctit)
dislin.messag (ctit, (2970 - nl)/2, 200)

dislin.height (50)
nx = 300
ny = 400

for i in range (0, 20):
  if i == 10:
    nx = nx + 2970 / 2
    ny = 400

  nl = dislin.nlnumb (ivec[i], -1)
  dislin.number (ivec[i], -1, nx - nl, ny - 25)

  dislin.vector (nx + 100, ny, nx + 1000, ny, ivec[i])
  ny = ny + 160

dislin.disfin ()

