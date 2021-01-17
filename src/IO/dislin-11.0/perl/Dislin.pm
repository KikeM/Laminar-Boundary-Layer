package Dislin;

use strict;
use vars qw($VERSION @ISA @EXPORT);

require Exporter;
require DynaLoader;

@ISA = qw(Exporter DynaLoader);

@EXPORT = qw(
     abs3pt  angle   arcell  areaf   autres  ax2grf  ax3len  axclrs  
     axends  axgit   axis3d  axslen  axsorg  axspos  axstyp  bargrp  
     barpos  bars    bartyp  barwth  basalf  bezier  bitsi2  bitsi4  
     box2d   box3d   center  chaang  chaspc  chawth  chnatt  chncrv  
     chndot  chndsh  chnpie  circle  circsp  clip3d  closfl  clpbor
     clpmod  clpwin  clrcyc  clrmod  clswin  color   colran  colray
     complx  concrv  congap  conlab  conmat  conmod  conn3d  connpt
     conshd  contur  cross   crvmat  curv3d  curve   curve3  curvmp
     curvx3  curvy3  dash    dashl   dashm           digits  disalf
     disfin  disini  dot     dotl    duplx   dwgbut  dwgfil  dwglis
     dwgmsg  dwgtxt  ellips  endgrf  erase   errbar  eushft  expzlb
     fcha    field    
     filbox  filclr  filmod  fixspc  flab3d  flen    frame   frmess  
     gapcrv  getang  getbpp  getclr  getclp  getdig  getdsp  getfil  
     getgrf  gethgt  getind  getlab  getlen  getlev  getlin  getmfl  
     getor   getpag  getpat  getplv  getpos  getran  getres  getrgb
     getscl  
     getsp1  getsp2  getsym  gettcl  gettic  gettyp  getuni  getver  
     getvk   getvlt  getwid  getwin  getxid  grace   graf    graf3   
     graf3d  grafmp  grffin  grfini  grdpol  grid    grid3d  gridmp  
     gwgbox  gwgbut  gwgfil  gwglis  gwgscl  gwgtxt  height  helve   
     helves  histog  hname   hsvrgb  hsymbl  htitle  hwfont  hworig  
     hwpage  imgfin  imgini  inccrv  incfil  incmrk  intax   intcha  
     intlen  itmcat  itmcnt  itmstr  labclr  labdis  labels  labjus  
     labpos  labtyp  legclr  legend  legini  leglin  legopt  legpat  
     legpos  legtit  lfttit  lincyc  line    linesp  lintyp  linwid  
     lncap   lnjoin  lnmlt   logtic  mapmod  mappol  mapref  mapsph  
     marker  mdfmat  messag  metafl  mixalf  mixleg          mpaepl  
     mplang  mplclr  mplpos  mplsiz  msgbox  mylab   myline  mypat   
     myvlt   namdis  name    namjus  neglog  newmix  newpag  nlmess  
     nlnumb  noarln  nobar   nobgd   nochek  noclip  nofill  nograf  
     nohide  noline  number  numfmt  numode  nxlegn  nxposn  nylegn  
     nyposn  nzposn  openfl  opnwin  origin  page    pagera  paghdr  
     pagmod  patcyc  penwid  pie     pieexp  piegrf  pielab  pievec  
     point   polcrv  pos2pt  pos3pt  posifl  projct  psfont  recfll  
     rectan  rel3pt  resatt  reset   revscr  rgbhsv  rgtlab  rimage  
     rlarc   rlarea  rlcirc  rlell   rline   rlmess  rlnumb  rlpie   
     rlpoin  rlrec   rlrnd   rlsec   rlsymb  rlvec   rndrec  rpixel  
     rpixls  rpxrow  rtiff   rvynam  scale   sclfac  sclmod  scrmod  
     sector  selwin  sendbf  serif   setbas  setclr  setexp  setfil  
     setgrf  setind  setmix  setpag  setres  setrgb  setscl  setvlt 
     shdcha  
     shdcrv  shdeur  shdmap  shdmod  shdpat  shield  shlcir  shldel  
     shlell  shlind  shlpie  shlpol  shlrct  shlrec  shlres  shlsur  
     shlvis  simplx  skipfl  smxalf  solid   sortr1  sortr2  spline  
     splmod  strt3d  strtpt  surclr  surfce  surmat  surmsh  surshd
     sursze  
     survis  swapi2  swapi4  swgbox  swgbut  swgfil  swgfnt  swghlp  
     swgjus  swglis  swgmix          swgmrg  swgoff  swgpop  swgpos  
     swgscl  swgsiz  swgtit  swgtxt  swgtyp  swgwin  swgwth  symbol  
     symfil  symrot  tellfl  thkcrv  ticks   ticlen  ticpos  tiforg  
     tifwin  timopt  titjus  title   titlin  titpos  trfco1  trfco2  
     trfco3  trfrel  trfres  trfrot  trfscl  trfshf  triplx  trmlen  
     txtjus  unit    upstr   vang3d  vclp3d  vector  vectr3  vfoc3d
     view3d  vkxbar  vkybar  vkytit  vup3d   wgapp   wgbas   wgbox
     wgbut   wgcmd   wgfil   wgfin   
     wgini   wglab   wglis   wgok    wgpop   wgquit  wgltxt  wgpbut  
     wgscl   wgtxt   widbar  wimage  winmod  window  winid   winsiz  
     wintit  world   wpixel  wpixls  wpxrow  writfl  wtiff   x11mod  
     x2dpos  x3dabs  x3dpos  x3drel  xaxgit  xaxis   xaxlg   xaxmap  
     xcross  xdraw   xmove   xposn   y2dpos  y3dabs  y3dpos  
     y3drel  yaxgit  yaxis   yaxlg   yaxmap  ycross  yposn   z3dpos  
     zaxis   zaxlg   zbffin  zbfini  zbflin  zbftri  zscale
     axsscl  errdev  errfil  labdig  mapbas  maplev  rlconn  rlstrt 
     axsbgd  cgmbgd  cgmpic  cgmver  getalf  getmix  getshf  gmxalf
     pagfll  labmod  incdat  basdat  nwkday  trfdat  ticmod  csrpt1
     csrpts  csrmov  csruni  qplot   qplsca  qplbar  qplpie  qplclr
     qplcon  qplsur  barbor  barclr  baropt  piebor  pieclr  pieopt
     pietyp  suropt  winkey  rpng    getmat  swgcbk  winfnt  x11fnt
     sendmb  sendok  wgdraw  wgdlis  swgopt  swgatt  swgdrw  swgspc
     gwgxid  gwgatt  rbfpng  surfcp  surfun  barmod  xinvrs  yinvrs
     reawgt  getlit  light   litmod  litopt  litpos  matopt  rppm
     suriso  readfl  texmod  texopt  mshclr  sphe3d  getscr  swgstp
     pdfmod  circ3p  conclr  contri  crvtri  surtri  triang  winopt 
     pdfbuf  windbr  rlwind  imgfmt  imgmod  frmclr  chnbar  chacod
     rbmp    wmfmod  imgbox  imgsiz  confll  winapp  errmod  mapfil
     mysymb  pagorg  polar   swgclr  units   shdusa  trfmat  tifmod
     setcbk  addlab  bars3d  labl3d  pngmod  polmod  conpts  tripts
     csrtyp  trifll  wgstxt  bmpfnt  mpslogo rgif    gifmod  gothic  
     pdfmrk  shdafr  csrpos  setcsr  swgfoc  indrgb  gethnm  vltfil
     csrmod  intrgb  litop3  matop3  imgclp  psmode  texval  delglb
     thrini  thrfin  polclp  shdasi  shdaus  mapopt  nxpixl  nypixl
     rot3d   dbfini  dbffin  zbfers  zbfres  tr3res  tr3rot  tr3scl
     tr3shf  cone3d  cylid3  disk3d  hsym3d  plat3d  pyra3d  quad3d
     symb3d  torus3d tube3d  vtx3d   vtxc3d  vtxn3d  isopts  setfce
     tria3d  wgpbar  swgval  swgint  swgflt  gwgint  gwgflt  legval
     zbfscl  hwscal  tprini  tprfin  tprmod  tprval  gaxpar  bmpmod
     csrkey  csrrec  pagwin  wgtbl   swgtbf  swgtbi  swgtbl  swgtbs
     swgray  swgcb2  gwgtbf  gwgtbi  gwgtbl  gwgtbs  hpgmod  maplab
     filopt  vecfld  vecclr  vecopt  pike3d  vecf3d  field3d shdnor
     shdsou  grafp   intutf  utfint  doevnt  mshcrv  thkc3d  pieval
     piecbk  pierot  hwmode  bufmod  stream  stream3d vecmat vecmat3d
     stmpts stmpts3d stmmod  stmopt  stmval  licpts  licmod  dbfmod
     zbfmod  imgtpr  txture  crvqdr  crvt3d  mrkclr  stmtri  disenv
     dwgerr  fbars   qplcrv  qplscl  mapimg  wgsep  conshd3d swgiop 
     wincbk  curv4d  tr3axs  triflc  legtyp  gwgsiz  linmod  csrlin
     legbgd  frmbar  jusbar  posbar  spcbar  ttfont  gapsiz  txtbgd
     linclr  plyini  plyfin  surshc  wgappb  wgpopb  pt2pos  expimg
     shdfac  proj3d  vscl3d  swgcb3  filwin  legsel  filsiz  wgicon
     wgpicon wgimg   wgpimg  nancrv  ldimg   swgbgd  swgfgd  itmncat
     filtyp  gwggui  freeptr getscm  axsers  wintyp  winjus  winico
     hidwin  fitscls fitsflt fitsimg fitsopn fitsstr fitstyp fitsval
     fitshdu helvet
);
$VERSION = '11.0';

bootstrap Dislin $VERSION;
1;
__END__
# Below is the stub of documentation for your module. You better edit it!

=head1 NAME

Dislin - Perl extension for data plotting

=head1 SYNOPSIS

  use Dislin;

=head1 DESCRIPTION

  DISLIN is a high-level and easy to use graphics library for
  displaying data as curves, bar graphs, pie charts, 3D-colour plots,
  surfaces, contours and maps. Several output formats are supported
  such as X11, VGA, PostScript, PDF, CGM, WMF, HPGL, TIFF, GIF and PNG.
  The library contains about 700 plotting and parameter setting routines
  and is available for several C, Fortran 77 and Fortran 90/95 compilers.

  DISLIN Homepage:

    http://www.dislin.de

=head1 AUTHOR

  Helmut Michels
  Max-Planck-Institut fuer 
  Sonnensystemforschung               Phone: +49 551 384 979-334
  Justus-von-Liebig-Weg 3             Fax  : +49 551 384 979-240
  D-37077 Goettingen                  Mail : michels@mps.mpg.de

=cut

