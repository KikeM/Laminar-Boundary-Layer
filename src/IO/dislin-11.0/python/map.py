#! /usr/bin/env python
import dislin

nproj = 14

cl1  = ['Cylindrical Equidistant', 
         'Mercator',
         'Cylindrical Equal-Area',
         'Hammer (Elliptical)',
         'Aitoff (Elliptical)',
         'Winkel (Elliptical)',
         'Sanson (Elliptical)',
         'Conical Equidistant',
         'Conical Equal-Area',
         'Conical Conformal',
         'Azimuthal Equidistant',
         'Azimuthal Equal-Area',
         'Azimuthal Stereographic',
         'Azimuthal Orthgraphic']

cl2  =  ['CYLI', 'MERC', 'EQUA', 'HAMM', 'AITO', 'WINK',
         'SANS', 'CONI', 'ALBE', 'CONF', 'AZIM', 'LAMB',
         'STER', 'ORTH']

id_lis = 0
id_draw = 0

def myplot (id):
  xa   = -180. 
  xe   =  180.
  xor  = -180.
  xstp =   60.

  ya   = -90. 
  ye   =  90.
  yor  = -90.
  ystp =  30.

  isel = dislin.gwglis (id_lis)
  dislin.setxid (id_draw, 'widget')
  dislin.metafl ('xwin')
  dislin.disini ()
  dislin.erase  ()
  dislin.complx ()

  if (isel >=4 and isel <= 7): 
    dislin.noclip ()
  elif (isel == 2):
    ya = -85
    ye = 85
    yor = -60
  elif (isel >= 8 and isel <= 10):
    ya = 0
    ye = 90
    yor = 0

  dislin.labdig (-1, 'xy')
  dislin.name   ('Longitude', 'x')
  dislin.name   ('Latitude', 'y')
  dislin.projct (cl2[isel-1])
  dislin.titlin (cl1[isel-1] + 'Projection', 3)
  dislin.htitle (50)
  dislin.grafmp (xa, xe, xor, xstp, ya, ye, yor, ystp)
  dislin.title  ()
  dislin.gridmp (1, 1)
  dislin.color  ('green')
  dislin.world  ()
  dislin.unit   (0)
  dislin.disfin ()


clis = cl1[0]
for i in range (1, nproj):
  clis = dislin.itmcat (clis, cl1[i])

dislin.swgtit ('DISLIN Map Plot')
ip  = dislin.wgini ('hori')
dislin.swgwth (-15)
ip1 = dislin.wgbas (ip, 'vert')
dislin.swgwth (-50)
ip2 = dislin.wgbas (ip, 'vert')

dislin.swgdrw (2100./2970.)
id = dislin.wglab (ip1, 'Projection:')
id_lis = dislin.wglis (ip1, clis, 1)

id_but = dislin.wgpbut (ip1, 'Plot')
dislin.swgcbk (id_but, myplot)

id_quit = dislin.wgquit (ip1)

id = dislin.wglab (ip2, 'DISLIN Draw Widget:')
id_draw = dislin.wgdraw (ip2)
dislin.wgfin ()





