!========================================================================================!
!
!                                 Compatibility conditions Laplace
!
! Enrique Millan Valbuena ==================================================== 17.10.2017!
module Laplace_Boundary_Conditions

use Linear_Algebra
use PDE_Properties

implicit none

private
public :: Compatible_Boundary_Conditions, Compatible_Boundary_Conditions_Start, Boundary_Integrals

! Mathematical constants
real, parameter :: pi = 4.0 * atan(1.0)

contains
!========================================================================================!
!
!                                 Compute the compatibility conditions
!
!========================================================================================!
subroutine Compatible_Boundary_Conditions(phi, phi_n, F1, F2)
    real,   intent(inout) :: phi(1:), phi_n(1:)
    real,   intent(in   ) :: F1(1:, 1:), F2(1:, 1:)

    !Local variables
    integer                    :: N0, i, j, N

    real, allocatable          :: Row_Scaling(:), delta(:,:)
    real, allocatable, target  :: Constants(:), Solution(:)
    real, pointer              :: psi(:), psi_n(:), Equation_u(:,:), u(:,:)


    !Extract number of intervals of the problem
    N0  = ubound(F1,1) / 4
    N   = 4 * N0

    !Allocates the linear algebra vectors and initializes vectors
    allocate(Constants  (1: 2 * N), &
             Solution   (1: 2 * N), &
             Row_Scaling(1: 2 * N)  )

    allocate(delta(1:N, 1:N))

    delta = 0.0
    forall(i=1:N) delta(i,i) = 1.0

	! Direct pointers
    u    (1:N, 1:2) => Solution
	psi  (1:N     ) => u(1:N, 1)
	psi_n(1:N     ) => u(1:N, 2)

	Equation_u(1:N, 1:2) => Constants

    Constants = 0.0
    Solution  = 0.0

!========================================================================================!
!                                       Matrix scaling
!========================================================================================!

    Row_Scaling = 1.0

	call Matrix_Row_Scaling(Row_Scaling)

!========================================================================================!
!                                   RHS of the equations
!========================================================================================!

	Constants = 0.0

	do i = 1, N0

		Equation_u(i         , 1) = phi  (i)
		Equation_u(i +      0, 2) = phi_n(i)
		Equation_u(i +     N0, 2) = phi_n(i + N0)
		Equation_u(i + 2 * N0, 1) = phi  (i + 2 * N0)
		Equation_u(i + 3 * N0, 1) = phi  (i + 3 * N0)

	enddo

	open(10, file = 'constants.dat', action = 'write', status = 'replace')

	do i = 1, 2 * N
		write(10, '(f10.5)') Constants(i)
	enddo

    do i = 1, 2 * N
        Constants(i) = Constants(i) / Row_Scaling(i)
    enddo

!========================================================================================!
!                                       Initial guess
!========================================================================================!

	Solution = 0.0

!========================================================================================!
!                                     Solve linear system
!========================================================================================!

    call Linear_Algebra_Solver(Matrix_Product = Linear_Equations,  &
                               Constants      = Constants,         &
                               Solution       = Solution,          &
                               Preconditioner = Linear_Preconditioner )

!========================================================================================!
!                                     Recover solution
!========================================================================================!

	do i = 1, N
		phi(i)   = psi(i)
		phi_n(i) = psi_n(i)
	enddo

    deallocate(Constants, Solution)

contains
!========================================================================================!
!
!                                       PDE Equations
!
!========================================================================================!
subroutine Linear_Equations(Vector, Result)
    real, intent(in),  target :: Vector(1:)
    real, intent(out), target :: Result(1:)

    !Local variables
    integer           :: m, k, i, j

    real, pointer     :: u(:,:), Equation_u(:,:), psi(:), psi_n(:) ! To write the equations

    ! Direct pointers
    u    (1:N, 1:2) => Vector
    Equation_u(1:N, 1:2) => Result

	psi  (1:N     ) => u(1:N, 1)
	psi_n(1:N     ) => u(1:N, 2)

	Result = 0.0
	Equation_u = 0.0

	do m = 1, N
		do k = 1, N
			Equation_u(m,:) = Equation_u(m,:) + psi(k) * (F2(m,k) - 0.5 * delta(m,k)) - psi_n(k) *  F1(m,k)
		enddo
	enddo

	!Known conditions
	do i = 1, N0

		Equation_u(i,          1) = psi  (i         )
		Equation_u(i +      0, 2) = psi_n(i +      0)
		Equation_u(i +     N0, 2) = psi_n(i +     N0)
		Equation_u(i + 2 * N0, 1) = psi  (i + 2 * N0)
		Equation_u(i + 3 * N0, 1) = psi  (i + 3 * N0)

	enddo

	do i = 1, 2 * N
        Result(i) = Result(i) / Row_Scaling(i)
    enddo

end subroutine Linear_Equations
!========================================================================================!
!
!                                       PDE Preconditioner
!
!========================================================================================!
subroutine Linear_Preconditioner(Vector, Result)
    real, intent(inout), target :: Vector(:)
    real, intent(inout), target :: Result(:)

    Result = Vector

end subroutine Linear_Preconditioner
!========================================================================================!
!
!                                    Matrix_Row_Scaling
!
!========================================================================================!
subroutine Matrix_Row_Scaling(Row_Scaling)
    real, intent(out) :: Row_Scaling(1:)

    !Local variables
    integer :: i, j

    real, allocatable, target  :: Vector(:), Result(:), A(:,:)

    !Allocation of vectors
    allocate(Vector(1: 2 * N), &
             Result(1: 2 * N), &
             A(1:2*N, 1:2*N)  )

    !Init
    Vector = 0.0
    Result = 0.0
    A = 0.0

    do i = 1, 2 * N

        Vector    = 0.0
        Vector(i) = 1.0

        call Linear_Equations(Vector, Result)
		A(:,i) = Result
        do j = 1, 2 * N

        	if(abs(Result(j)) > Row_Scaling(j)) then
				Row_Scaling(j) = abs(Result(j))
			else
				cycle
			endif

        enddo
    enddo

    open(20, file = 'matrix.dat')

    do i = 1, 2 * N
    write(20,'(1000f10.5)') A(i,:)
    enddo

   close(20)

    deallocate(Vector, Result)

end subroutine Matrix_Row_Scaling
end subroutine Compatible_Boundary_Conditions
!========================================================================================!
!
!                             End of Compatible_Boundary_Conditions
!
!========================================================================================!


!========================================================================================!
!                                  Boundary integrals
!========================================================================================!
subroutine Boundary_Integrals(xm, ym, xb, yb, nx, ny, lk, F1, F2)
	real, intent(in)  :: xm(1:), ym(1:), xb(1:), yb(1:), nx(1:), ny(1:), lk(1:)
	real, intent(out) :: F1(1:,1:), F2(1:,1:)

	integer :: i, j, N

	real            :: PF1, PF2
	real, parameter :: pi = 4.0 * atan(1.0)

	!Size of the problem
	N = ubound(F1,1)

	!Compute the integrals
	do i = 1, N
		do j = 1, N

			call CPF(xm(i),ym(i),xb(j),yb(j),nx(j),ny(j),lk(j),PF1,PF2)

			F1(i,j) = PF1 / pi
			F2(i,j) = PF2 / pi

		enddo
	enddo

end subroutine Boundary_Integrals
!========================================================================================!
!                                  Logarithmic integrals
!========================================================================================!
subroutine CPF(xi,eta,xk,yk,nkx,nky,L,PF1,PF2)
      real, intent(in)  :: xi,eta,xk,yk,nkx,nky,L
      real, intent(out) :: PF1, PF2

      real :: A,B,E,D,BA,EA

      A  = L**2d0
      B  = 2d0*L*(-nky*(xk-xi)+nkx*(yk-eta))
      E  = (xk-xi)**2d0+(yk-eta)**2d0
      D  = dsqrt(dabs(4d0*A*E-B**2d0))
      BA = B/A
      EA = E/A

      if (D.lt.0.0000000001d0) then

		  PF1=0.5d0*L*(dlog(L) &
		  +(1d0+0.5d0*BA)*dlog(dabs(1d0+0.5d0*BA)) &
		  -0.5d0*BA*dlog(dabs(0.5d0*BA))-1d0)

		  PF2=0d0

      else

		  PF1=0.25d0*L*(2d0*(dlog(L)-1d0)-0.5d0*BA*dlog(dabs(EA)) &
		  +(1d0+0.5d0*BA)*dlog(dabs(1d0+BA+EA)) &
		  +(D/A)*(datan((2d0*A+B)/D)-datan(B/D)))

		  PF2=L*(nkx*(xk-xi)+nky*(yk-eta))/D &
		  *(datan((2d0*A+B)/D)-datan(B/D))

      endif

end subroutine CPF
!========================================================================================!
!                                  Compatible_Boundary_Conditions_Start
!========================================================================================!
subroutine Compatible_Boundary_Conditions_Start(N0, xm, ym, xb, yb, nx, ny, lk, phi, phi_n, F1, F2)
	implicit none

	integer, intent(in)    :: N0
	real, allocatable, intent(inout) :: xm(:), ym(:), xb(:), yb(:), nx(:), ny(:), lk(:), &
						      phi(:), phi_n(:), F1(:,:), F2(:,:)

	!Local variables
	integer :: i, j
	integer :: N

	!Number of nodes per element
	!N0 = 100

	!Number of equations
	N = 4 * N0

	!Allocate all
	allocate(xm(1:N), ym(1:N), xb(1:N+1), yb(1:N+1), nx(1:N), ny(1:N), lk(1:N), phi(1:N), phi_n(1:N))
	allocate(F1(1:N, 1:N), F2(1:N, 1:N))

	xm = 0.0
	ym = 0.0

	xb = 0.0
	yb = 0.0

	nx = 0.0
	ny = 0.0
	lk = 0.0

	phi   = 0.0
	phi_n = 0.0

	F1 = 0.0
	F2 = 0.0

	!TODO: change loop so it admits different number of nodes on each direction
	!TODO: change loop so it admits domain length different than 1.0

	!Create Chebyshev mesh
	forall(i=1:N0) xb(i) = 0.5 * (1.0 - cos(pi * real(i-1) / real(N0)))
	forall(i=1:N0) yb(i) = 0.0

	forall(i=1:N0) xb(i + N0) = 1.0
	forall(i=1:N0) yb(i + N0) = xb(i)

	forall(i=1:N0) xb(i + 2 * N0) = 1.0 - xb(i)
	forall(i=1:N0) yb(i + 2 * N0) = 1.0

	forall(i=1:N0) xb(i + 3 * N0) = 0.0
	forall(i=1:N0) yb(i + 3 * N0) = 1.0 - xb(i)

	!Allocate final vertex to close the boundary
	xb(N+1) = xb(1)
	yb(N+1) = yb(1)

	!Compute mid points, normals and length
	do i = 1, N
		xm(i) = (xb(i) + xb(i+1)) * 0.5
		ym(i) = (yb(i) + yb(i+1)) * 0.5

		lk(i) = sqrt((xb(i) - xb(i+1)) ** 2.0 + (yb(i) - yb(i+1)) ** 2.0)

		nx(i) = (yb(i+1) - yb(i  )) / lk(i)
		ny(i) = (xb(i  ) - xb(i+1)) / lk(i)
	enddo

	!Compute boundaries
	call Boundary_Integrals(xm, ym, xb, yb, nx, ny, lk, F1, F2)

end subroutine Compatible_Boundary_Conditions_Start
!========================================================================================!
!                                  Delta function
!========================================================================================!
! real function delta(i,j)
! 	integer :: i, j
!
! 	if (i == j) then
! 		delta = 1.0
! 	else
! 		delta = 0.0
! 	endif
!
! end function delta
!========================================================================================!
!
!                                       End of module
!
!========================================================================================!
end module Laplace_Boundary_Conditions
