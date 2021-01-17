!========================================================================================!
! 								Ordinary_Differential_Operators
!
!		Implements the High-order derivatives for a one-dimensional array.
!
! Enrique Millan Valbuena ==================================================== 11.10.2017!

module Ordinary_Differential_Operators

use Finite_Difference_Formulation

implicit none

	integer, public :: ODE_Degree

contains

!========================================================================================!
! 										 Derivative
!
!	Inputs:
!		- Derivative index   (INTEGER) : alpha = k
!		- Function values    (REAL)    : f     = {f0, f1, f2, ..., fi, ..., fN}
!		- Collocation points (REAL)    : x     = {x0, x1, x2, ..., xi, ..., xN}
!
!	Outputs:
!		- Derivative values  (REAL)    : df    = d^k {f0, f1, f2, ..., fi, ..., fN} / dx^k
!
!========================================================================================!
subroutine Derivative(f, x, alpha, df)
	integer, intent(in   ) :: alpha
	real,    intent(in   ) :: f(0:)
	real,    intent(in   ) :: x(0:)
	real,    intent(inout) :: df(0:)

	!Local variables
	integer :: i
	integer :: N, s

	integer, allocatable :: Seeds(:)

	real, allocatable :: Weights(:,:,:)

	!Extract number of intervals
	N = size(x) - 1


	!Obtains the derivation seeds/weights
	allocate(Seeds(0:N), Weights(1, 0:ODE_Degree, 0:N))

	call Finite_Difference_Formulas(Order       = ODE_Degree,  &
									Nodes       = x,           &
									Derivatives = (/ alpha /), &
									Seeds       = Seeds,       &
									Weights     = Weights      )

	!Computes the spatial derivatives
	do i = 0, N
	  s = Seeds(i)
	  df(i) = sum(Weights(1,:,i) * f(s:s + ODE_Degree))
	enddo

	deallocate(Seeds, Weights)

end subroutine Derivative

!========================================================================================!
! 									Derivative weights
!
!	Inputs:
!		- Derivative order   (INTEGER) : alpha = k
!		- Collocation points (REAL)    : x     = {x0, x1, x2, ..., xi, ..., xN}
!
!	Outputs:									 {w0, w1, w2, ..., wq, ..., 0 } | 0
!		- Weights  (REAL)              : w     = {0 , w0, w1, ..., wq, ..., 0 } } i
!												 {0 ,  0, w1, ..., wi, ..., wq} | N
!
!========================================================================================!
subroutine Derivative_Weights_Seeds(x, alpha, Weights, Seeds)
	integer, intent(in   ) 	            :: alpha
	integer, intent(inout), allocatable :: Seeds(:)
	real,    intent(in   )              :: x(0:)
	real,    intent(inout), allocatable :: Weights(:,:,:)

	!Local variables
	integer :: N

	!Extract number of intervals
	N = size(x) - 1

	!Obtains the derivation seeds/weights
	allocate(Seeds(0:N), Weights(1, 0:ODE_Degree, 0:N))

	Seeds   = 0
	Weights = 0.0

	call Finite_Difference_Formulas(Order       = ODE_Degree,  &
									Nodes       = x,           &
									Derivatives = (/ alpha /), &
									Seeds       = Seeds,       &
									Weights     = Weights      )

end subroutine Derivative_Weights_Seeds

!========================================================================================!
! 										Apply_Weights
!
!	Inputs:
!		- Derivative index   (INTEGER) : alpha = k
!		- Function values    (REAL)    : f     = {f0, f1, f2, ..., fi, ..., fN}
!
!	Outputs:
!		- Derivative values  (REAL)    : df    = d^k {f0, f1, f2, ..., fi, ..., fN} / dx^k
!
!========================================================================================!
subroutine ODE_Apply_Weights(f, Weights, Seeds, df)
	integer, intent(in ) :: Seeds(0:)
	real,    intent(in ) :: f(0:)
	real,    intent(in ) :: Weights(:,0:,0:)
	real,    intent(out) :: df(0:)

	!Local variables
	integer :: i, N, s

	!Extract number of intervals
	N = size(f) - 1

	!Computes the spatial derivatives
	do i = 0, N
	  s = Seeds(i)
	  df(i) = sum(Weights(1,:,i) * f(s:s + ODE_Degree))
	enddo

end subroutine ODE_Apply_Weights

!========================================================================================!
! 										End of module
!========================================================================================!

end module Ordinary_Differential_Operators
