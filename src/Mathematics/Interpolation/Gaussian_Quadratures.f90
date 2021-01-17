!-----------------------------------------------------------------------!
!                         Gaussian_Quadratures                          !
!                                                                       !
!                                                                       !
!--------------------------------------------Miguel Hermanns-2011.04.07-!
module Gaussian_Quadratures
implicit none

private
public  :: Gauss_Legendre_Quadrature

interface LegendreP
  module procedure LegendreP_Polynomial, LegendreP_Associated
end interface LegendreP

real :: pi = 3.1415926535897932d0

contains
!-----------------------------------------------------------------------!
!
! This module is empty because it contained private code belonging to 
! M. Hermanns and shared with me because of past work, but it is not 
! intended for public use for the moment.
!
! Enrique Millan Valbuena-2021-01-17
!
!-----------------------------------------------------------------------!
end module Gaussian_Quadratures
