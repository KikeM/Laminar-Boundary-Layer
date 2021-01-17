!========================================================================================!
!
!                                   Poisson equation
!
! Enrique Millan Valbuena ==================================================== 17.10.2017!
module Test_1D

use Linear_Algebra
use Ordinary_Differential_Operators
use PDE_Properties

implicit none

private
public :: Coupled_System_Solution

! Mathematical constants
real, parameter :: pi = 4.0 * atan(1.0)

contains
!========================================================================================!
!
!                                   PDE_Problem_Solution
!
!========================================================================================!
subroutine Coupled_System_Solution(y, ue, previous_psi, updated_psi, previous_w, updated_w)
    real,                      intent(in ) :: y(0:), ue
    real,                      intent(in ) :: previous_psi(0:), previous_w(0:)
    real,                      intent(out) :: updated_psi(0:), updated_w(0:)

    !Local variables
    integer                    :: N, i, j, Number_Equations
    integer, allocatable       :: Seeds_y(:)

	!Derivation weights
    real, allocatable          :: W_y_II(:,:,:), W_y_I(:,:,:)

	!Laplacians
    real, allocatable          :: nabla_psi(:), nabla_w(:)

	!Separate differential operators
	real, allocatable          :: psi_y(:)

	!Linear system ingredients
    real, allocatable          :: Row_Scaling(:)
    real, allocatable, target  :: Constants(:), Solution(:)
    real, pointer              :: Equations(:,:), u(:,:)


    !Extract number of intervals of the problem
    N = ubound(y,1)

    Number_Equations = 2 * (N + 1)

    !Allocates the linear algebra vectors and initializes vectors
    allocate(Constants  (1:Number_Equations), &
             Solution   (1:Number_Equations), &
             Row_Scaling(1:Number_Equations)  )

	allocate(nabla_psi(0:N), nabla_w(0:N))

	allocate(psi_y (0:N))

	nabla_psi = 0.0
	nabla_w   = 0.0

	psi_y  = 0.0

    Constants = 0.0
    Solution  = 0.0

    ! Direct pointers
    u        (1:2, 0:N) => Solution
    Equations(1:2, 0:N) => Constants

!========================================================================================!
!                                       Prepare Seeds and Weights
!========================================================================================!

    call Derivative_Weights_Seeds(y, 2, W_y_II, Seeds_y)
    deallocate(Seeds_y)
    call Derivative_Weights_Seeds(y, 1, W_y_I, Seeds_y)

!========================================================================================!
!                                       Matrix scaling
!========================================================================================!

    Row_Scaling = 1.0

    call Matrix_Row_Scaling(Row_Scaling)

!========================================================================================!
!                                   RHS of the equations
!========================================================================================!

	call Derivative(previous_psi, y, 1, psi_y)

	call Derivative(previous_psi, y, 2, nabla_psi)
	call Derivative(previous_w, y, 2, nabla_w)

    !Impose equation everywhere
    Equations = 0.0

    Equations(1,:) = - (nabla_psi + previous_w)
    Equations(2,:) = -  nabla_w

    !Boundary conditions
    !-------------------

    !Streamfunction
    Equations(1, 0) = 0.d0            !Wall
    Equations(1, N) = ue - psi_y(N) !Far-field

    !Vorticity
    Equations(2, 0) = - psi_y(0)      !Wall
    Equations(2, N) = 0.d0              !Far-field

    do i = 1, Number_Equations
        Constants(i) = Constants(i) / Row_Scaling(i)
    enddo

!========================================================================================!
!                                       Initial guess
!========================================================================================!

    u(1,:) = 0.d0
    u(2,:) = 0.d0

!========================================================================================!
!                                     Solve linear system
!========================================================================================!
    call Linear_Algebra_Solver(Matrix_Product = PDE_Equations,     &
                               Constants      = Constants,         &
                               Solution       = Solution,          &
                               Preconditioner = PDE_Preconditioner )

	! Recover solution
    updated_psi = u(1,:)
    updated_w   = u(2,:)

    deallocate(Constants, Solution)

contains
!========================================================================================!
!
!                                       PDE Equations
!
!========================================================================================!
subroutine PDE_Equations(Vector, Result)
	implicit none

    real, intent(in),  target :: Vector(1:)
    real, intent(out), target :: Result(1:)

    !Local variables
    integer           :: i, j, s

    real, pointer     :: Equations(:,:), u(:,:) ! To write the equations

	real, allocatable :: delta_psi   (:), delta_w(:)
    real, allocatable :: delta_w_yy  (:)
    real, allocatable :: delta_psi_y (:)
    real, allocatable :: delta_psi_yy(:)

    ! Direct pointers
    u        (1:2, 0:N) => Vector
    Equations(1:2, 0:N) => Result

	allocate(delta_psi(0:N), &
	         delta_w  (0:N)  )

	allocate(delta_psi_y(0:N))

    allocate(delta_w_yy  (0:N))
	allocate(delta_psi_yy(0:N))

	delta_psi = 0.d0
	delta_w   = 0.d0

	delta_psi_y = 0.d0

    delta_psi_yy = 0.d0


	!Necessary for vector-product
    delta_psi = u(1, 0:N)
    delta_w   = u(2, 0:N)

    !Compute derivatives field
    !-------------------------

    !Computes the spatial derivatives - (y) direction
    do i = 0, N
        s = Seeds_y(i)

        delta_psi_y (i) = sum(W_y_I(1,:,i)  * delta_psi(s:s+ODE_Degree))
        delta_psi_yy(i) = sum(W_y_II(1,:,i) * delta_psi(s:s+ODE_Degree))

        delta_w_yy  (i) = sum(W_y_II(1,:,i) * delta_w  (s:s+ODE_Degree))

    enddo

    !Impose equation everywhere
    Equations = 0.d0

    Equations(1,:) = delta_psi_yy + delta_w
    Equations(2,:) = delta_w_yy

    !Boundary conditions
    !-------------------

	!Streamfunction
	Equations(1, 0) = delta_psi(0)       !Wall
    Equations(1, N) = delta_psi_y(N)     !Far-field

    !Vorticity
    Equations(2, 0) = delta_psi_y(0)     !Wall
    Equations(2, N) = delta_w(N)         !Far-field

    !Scaling of the equations
    do i = 1, Number_Equations
        Result(i) = Result(i) / Row_Scaling(i)
    enddo

    deallocate(delta_w_yy)
    deallocate(delta_psi_yy, delta_psi_y)
    deallocate(delta_psi, delta_w)

end subroutine PDE_Equations
!========================================================================================!
!
!                                       PDE Preconditioner
!
!========================================================================================!
subroutine PDE_Preconditioner(Vector, Result)
    real, intent(inout), target :: Vector(:)
    real, intent(inout), target :: Result(:)

    Result = Vector

end subroutine PDE_Preconditioner
!========================================================================================!
!
!                                    Matrix_Row_Scaling
!
!========================================================================================!
subroutine Matrix_Row_Scaling(Row_Scaling)
    real, intent(out) :: Row_Scaling(1:)

    !Local variables
    integer :: i, j

    real, allocatable, target  :: Vector(:), Result(:)

    !Allocation of vectors
    allocate(Vector(1:Number_Equations), &
             Result(1:Number_Equations)  )

    !Init
    Vector = 0.0
    Result = 0.0

    do i = 1, Number_Equations

        Vector    = 0.0
        Vector(i) = 1.0

        call PDE_Equations(Vector, Result)

        do j = 1, Number_Equations

        	if(abs(Result(j)) > Row_Scaling(j)) then
				Row_Scaling(j) = abs(Result(j))
			else
				cycle
			endif

        enddo
    enddo

    deallocate(Vector, Result)

end subroutine Matrix_Row_Scaling
end subroutine Coupled_System_Solution
!========================================================================================!
!
!                                       End of module
!
!========================================================================================!
end module Test_1D
