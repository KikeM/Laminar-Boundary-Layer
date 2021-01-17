!========================================================================================!
!
!                                   Poisson equation
!
! Enrique Millan Valbuena ==================================================== 17.10.2017!
module Driven_Cavity

use Linear_Algebra
use Partial_Differential_Operators
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
subroutine Coupled_System_Solution(x, y, dt, Re, streamfunction_BC, vorticity_BC, previous_psi, updated_psi, previous_w, updated_w)
    real,                      intent(in ) :: x(0:), y(0:), dt, Re
    type(Boundary_Conditions), intent(in)  :: streamfunction_BC, vorticity_BC
    real,                      intent(in ) :: previous_psi(0:,0:), previous_w(0:,0:)
    real,                      intent(out) :: updated_psi(0:,0:), updated_w(0:,0:)

    !Local variables
    integer                    :: Nx, Ny, i, j, Number_Equations
    integer, allocatable       :: Seeds_x(:), Seeds_y(:)

	!Derivation weights
    real, allocatable          :: W_x_II(:,:,:), W_x_I(:,:,:)
    real, allocatable          :: W_y_II(:,:,:), W_y_I(:,:,:)

	!Laplacians
    real, allocatable          :: nabla_psi(:,:), nabla_w(:,:)

	!Separate differential operators
	real, allocatable          :: w_x(:,:), w_y(:,:), psi_x(:,:), psi_y(:,:)
    real, allocatable          :: w_xx(:,:), psi_yy(:,:)
    real, allocatable          :: Row_Scaling(:)
    real, allocatable, target  :: Constants(:), Solution(:)
    real, pointer              :: Equations(:,:,:), u(:,:,:)


    !Extract number of intervals of the problem
    Nx = ubound(x,1)
    Ny = ubound(y,1)

    Number_Equations = 2 * (Nx + 1) * (Ny + 1)

    !Allocates the linear algebra vectors and initializes vectors
    allocate(Constants  (1:Number_Equations), &
             Solution   (1:Number_Equations), &
             Row_Scaling(1:Number_Equations)  )

	allocate(nabla_psi(0:Nx, 0:Ny), nabla_w(0:Nx, 0:Ny))

	allocate(w_x   (0:Nx, 0:Ny), w_y   (0:Nx, 0:Ny))
	allocate(psi_x (0:Nx, 0:Ny), psi_y (0:Nx, 0:Ny))
	allocate(w_xx  (0:Nx, 0:Ny), psi_yy(0:Nx, 0:Ny))


	nabla_psi = 0.0
	nabla_w   = 0.0

	psi_x  = 0.0
	psi_y  = 0.0

	psi_yy = 0.0

	w_xx   = 0.0
	w_x    = 0.0
	w_y    = 0.0

    Constants = 0.0
    Solution  = 0.0

    ! Direct pointers
    u        (1:2, 0:Nx, 0:Ny) => Solution
    Equations(1:2, 0:Nx, 0:Ny) => Constants

!========================================================================================!
!                                       Prepare Seeds and Weights
!========================================================================================!

    call Partial_Derivative_Weights_Seeds(x, 2, W_x_II, Seeds_x)
	deallocate(Seeds_x)
    call Partial_Derivative_Weights_Seeds(x, 1, W_x_I, Seeds_x)

    call Partial_Derivative_Weights_Seeds(y, 2, W_y_II, Seeds_y)
    deallocate(Seeds_y)
    call Partial_Derivative_Weights_Seeds(y, 1, W_y_I, Seeds_y)

!========================================================================================!
!                                       Matrix scaling
!========================================================================================!

    Row_Scaling = 1.0

    call Matrix_Row_Scaling(Row_Scaling)

!========================================================================================!
!                                   RHS of the equations
!========================================================================================!

	call Partial_Derivative(previous_w,   x, y, (/1, 0/), w_x  )
	call Partial_Derivative(previous_w,   x, y, (/0, 1/), w_y  )

	call Partial_Derivative(previous_psi, x, y, (/1, 0/), psi_x)
	call Partial_Derivative(previous_psi, x, y, (/0, 1/), psi_y)

	call Partial_Derivative(previous_w,   x, y, (/2, 0/), w_xx )
	call Partial_Derivative(previous_psi, x, y, (/0, 2/), psi_yy)

    call Laplacian(previous_psi, x, y, nabla_psi)
    call Laplacian(previous_w  , x, y, nabla_w  )

    !Impose equation everywhere
    Equations = 0.0

    Equations(1,:,:) = - (nabla_psi + previous_w)
    Equations(2,:,:) = -  nabla_w   + Re * (psi_y * w_x - psi_x * w_y)

    !Boundary conditions
    !-------------------

    !Streamfunction
    Equations(1, 0, :)  =      - psi_x(0, :)      !Inlet
    Equations(1, Nx,:)  =      - psi_x(Nx,:)      !Outlet
    Equations(1, :, 0)  = 0.d0                    !Wall
    Equations(1, :, Ny) = 0.d0                    !Far-field

    !Vorticity
    Equations(2, :, 0)  =      - psi_y(:,0)      !Wall
    Equations(2, :, Ny) = 1.d0 - psi_y(:,Ny)     !Far-field
    Equations(2, 0, :)  = 0.d0                   !Inlet
    Equations(2, Nx,:)  = 0.d0                   !Outlet

    do i = 1, Number_Equations
        Constants(i) = Constants(i) / Row_Scaling(i)
    enddo

!========================================================================================!
!                                       Initial guess
!========================================================================================!

    u(1,:,:) = 0.d0
    u(2,:,:) = 0.d0

!========================================================================================!
!                                     Solve linear system
!========================================================================================!
    call Linear_Algebra_Solver(Matrix_Product = PDE_Equations,     &
                               Constants      = Constants,         &
                               Solution       = Solution,          &
                               Preconditioner = PDE_Preconditioner )

	! Recover solution
    updated_psi = u(1,:,:)
    updated_w   = u(2,:,:)

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

    real, pointer     :: Equations(:,:,:), u(:,:,:) ! To write the equations

	real, allocatable :: delta_psi   (:,:), delta_w     (:,:)
    real, allocatable :: delta_w_xx  (:,:), delta_w_yy  (:,:)
    real, allocatable :: delta_psi_x (:,:), delta_psi_y (:,:)
    real, allocatable :: delta_w_x   (:,:), delta_w_y   (:,:)
    real, allocatable :: delta_psi_xx(:,:), delta_psi_yy(:,:)

    ! Direct pointers
    u        (1:2, 0:Nx, 0:Ny) => Vector
    Equations(1:2, 0:Nx, 0:Ny) => Result

	allocate(delta_psi(0:Nx, 0:Ny), &
	         delta_w  (0:Nx, 0:Ny)  )

	allocate(delta_psi_x(0:Nx, 0:Ny), delta_psi_y(0:Nx, 0:Ny))
	allocate(delta_w_x  (0:Nx, 0:Ny), delta_w_y  (0:Nx, 0:Ny))

    allocate(delta_w_xx  (0:Nx, 0:Ny), delta_w_yy  (0:Nx, 0:Ny))
	allocate(delta_psi_xx(0:Nx, 0:Ny), delta_psi_yy(0:Nx, 0:Ny))

	delta_psi = 0.d0
	delta_w   = 0.d0

	delta_psi_y = 0.d0
	delta_psi_x = 0.d0

	delta_w_y = 0.d0
	delta_w_x = 0.d0

	delta_psi_xx = 0.d0
    delta_psi_yy = 0.d0

    delta_w_xx = 0.d0
    delta_w_yy = 0.d0

	!Necessary for vector-product
    delta_psi = u(1, 0:Nx, 0:Ny)
    delta_w   = u(2, 0:Nx, 0:Ny)

    !Compute derivatives field
    !-------------------------

    !Computes the spatial derivatives - (x) direction
    do i = 0, Nx
        s = Seeds_x(i)
        forall(j = 0:Ny) delta_w_x   (i,j) = sum(W_x_I (1,:,i) * delta_w  (s:s+Degree, j))
        forall(j = 0:Ny) delta_w_xx  (i,j) = sum(W_x_II(1,:,i) * delta_w  (s:s+Degree, j))

		forall(j = 0:Ny) delta_psi_x (i,j) = sum(W_x_I (1,:,i) * delta_psi(s:s+Degree, j))
        forall(j = 0:Ny) delta_psi_xx(i,j) = sum(W_x_II(1,:,i) * delta_psi(s:s+Degree, j))
    enddo

    !Computes the spatial derivatives - (y) direction
    do i = 0, Ny
        s = Seeds_y(i)
        forall(j = 0:Nx) delta_psi_y (j,i) = sum(W_y_I(1,:,i)  * delta_psi(j, s:s+Degree))
        forall(j = 0:Nx) delta_psi_yy(j,i) = sum(W_y_II(1,:,i) * delta_psi(j, s:s+Degree))

        forall(j = 0:Nx) delta_w_y   (j,i) = sum(W_y_I(1,:,i)  * delta_w  (j, s:s+Degree))
        forall(j = 0:Nx) delta_w_yy  (j,i) = sum(W_y_II(1,:,i) * delta_w  (j, s:s+Degree))
    enddo

    !Impose equation everywhere
    Equations = 0.d0

    Equations(1,:,:) = (delta_psi_xx + delta_psi_yy) + delta_w
    Equations(2,:,:) = (delta_w_xx   + delta_w_yy  ) - Re * (delta_psi_y *       w_x - delta_psi_x *       w_y &
    												         +     psi_y * delta_w_x -       psi_x * delta_w_y )

    !Boundary conditions
    !-------------------

	!Streamfunction
	Equations(1,  0,  :) = delta_psi_x(0,  :)          !Inlet
    Equations(1, Nx,  :) = delta_psi_x(Nx, :)          !Outlet
	Equations(1,  :,  0) = delta_psi  (:,  0)          !Wall
    Equations(1,  :, Ny) = delta_psi  (:, Ny)          !Far-field

    !Vorticity
    Equations(2, :, 0)  = delta_psi_y (:, 0)      !Wall
    Equations(2, :, Ny) = delta_psi_y (:, Ny)     !Far-field
    Equations(2, 0, :)  = delta_psi   (0, :)      !Inlet
    Equations(2, Nx,:)  = delta_psi   (Nx,:)      !Outlet


    !Scaling of the equations
    do i = 1, Number_Equations
        Result(i) = Result(i) / Row_Scaling(i)
    enddo

    deallocate(delta_w_xx, delta_w_yy)
    deallocate(delta_psi_xx, delta_psi_yy, delta_psi_y)
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
end module Driven_Cavity
