#!/usr/bin/env python
import dislin

ctit = 'Symbols'

dislin.setpag ('da4p')
dislin.metafl ('cons')

dislin.disini ()
dislin.pagera ()
dislin.complx ()
dislin.paghdr ('H. Michels  (', ')', 2, 0)

dislin.height (60)
nl = dislin.nlmess (ctit)
dislin.messag (ctit, (2100 - nl)/2, 200)

dislin.height (50)
dislin.hsymbl (120)

ny = 150

for i in range (0, 22):
  if (i % 4) == 0:
    ny = ny + 400
    nxp = 550
  else:
    nxp = nxp + 350

  nl = dislin.nlnumb (i, -1)
  dislin.number (i, -1, nxp - nl/2, ny + 150)
  dislin.symbol (i, nxp, ny)

dislin.disfin ()

