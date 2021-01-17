#! /usr/bin/env python
import dislin

dislin.metafl ('xwin')
dislin.disini ()
dislin.pagera ()
dislin.complx ()

dislin.axspos (400, 1850)
dislin.axslen (2400, 1400)

dislin.name   ('Longitude', 'X')
dislin.name   ('Latitude',  'Y')
dislin.titlin ('World Coastlines and Lakes', 3)

dislin.labels ('MAP', 'XY')
dislin.labdig (-1, 'XY')
dislin.grafmp (-180., 180., -180., 90., -90., 90., -90., 30.)

dislin.gridmp (1, 1)
dislin.color  ('green')
dislin.world  ()

dislin.color  ('foreground')
dislin.height (50)
dislin.title  ()
dislin.disfin ()
