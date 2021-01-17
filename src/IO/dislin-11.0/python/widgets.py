#! /usr/bin/env python
import dislin

cl1='Item1|Item2|Item3|Item4|Item5'
cfil = ' '

dislin.swgtit ('Widgets Example')
ip = dislin.wgini  ('VERT')

id = dislin.wglab  (ip, 'File Widget:')
id_fil = dislin.wgfil (ip, 'Open File', cfil, '*.c')

id = dislin.wglab  (ip, 'List Widget:')
id_lis = dislin.wglis (ip, cl1, 1)

id = dislin.wglab  (ip, 'Button Widgets:')
id_but1 = dislin.wgbut (ip, 'This is Button 1', 0) 
id_but2 = dislin.wgbut (ip, 'This is Button 2', 1)

id = dislin.wglab  (ip, 'Scale Widget:')
id_scl = dislin.wgscl (ip, ' ', 0., 10., 5., 1) 
dislin.wgfin ()

cfil = dislin.gwgfil (id_fil)
ilis = dislin.gwglis (id_lis)
ib1  = dislin.gwgbut (id_but1)
ib2  = dislin.gwgbut (id_but2)
xscl = dislin.gwgscl (id_scl)

print 'File Widget   : ', cfil
print 'List Widget   : ', ilis
print 'Button Widgets: ', ib1, ', ', ib2
print 'Scale Widget  : ', xscl
