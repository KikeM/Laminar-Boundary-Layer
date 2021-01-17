!-----------------------------------------------------------------------------!
!                      Piecewise_Polynomial_Interpolation                     !
!                                                                             !
! The present module implements the subroutines required to use the piecewise !
! polynomial interpolation defined in the following reference:                !
!                                                                             !
!  Hermanns M. & Hernandez J.A., 2008: "Stable high-order finite-difference   !
!  methods based on non-uniform grid point distributions", International      !
!  Journal for Numerical Methods in Fluids, Volume 56, pages 233-255.         !
!                                                                             !
!--------------------------------------------------Miguel Hermanns-2013.06.12-!
module Piecewise_Polynomial_Interpolation
use Lagrange_Interpolation_Method
implicit none

private
public :: Interpolate, Interpolation_Matrix, Piecewise_Error_Polynomial, &
          Centered_Polynomials

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
!-----------------------------------------------------------------------------!
end module Piecewise_Polynomial_Interpolation
