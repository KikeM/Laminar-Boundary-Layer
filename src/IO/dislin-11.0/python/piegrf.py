#!/usr/bin/env python
import dislin

xray = [1., 2.5, 2., 2.7, 1.8]

ctit = 'Pie Charts (PIEGRF)'

dislin.setpag ('da4p')
dislin.metafl ('cons')
dislin.disini ()
dislin.pagera ()
dislin.complx ()
dislin.chnpie ('BOTH')

dislin.axslen (1600, 1000)
dislin.titlin (ctit, 2)

cbuf = ' '
dislin.legini (cbuf, 5, 8)
dislin.leglin (cbuf, 'FIRST',  1)
dislin.leglin (cbuf, 'SECOND', 2)
dislin.leglin (cbuf, 'THIRD',  3)
dislin.leglin (cbuf, 'FOURTH', 4)
dislin.leglin (cbuf, 'FIFTH',  5)

# Selecting shading patterns
dislin.patcyc (1, 7)
dislin.patcyc (2, 4)
dislin.patcyc (3, 13)
dislin.patcyc (4, 3)
dislin.patcyc (5, 5)

dislin.axspos (250, 2800)
dislin.piegrf (cbuf, 1, xray, 5)
dislin.endgrf ()

dislin.axspos (250, 1600)
dislin.labels ('DATA', 'PIE')
dislin.labpos ('EXTERNAL', 'PIE')
dislin.piegrf (cbuf, 1, xray, 5)

dislin.height (50)
dislin.title  ()
dislin.disfin ()
