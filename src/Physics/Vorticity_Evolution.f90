!========================================================================================!
!
!                           Vorticity convective evolution
!
! Enrique Millan Valbuena ==================================================== 17.10.2017!
module Vorticity_Evolution

use Linear_Algebra
use Partial_Differential_Operators
use PDE_Properties
! imports integer :: Degree

implicit none

private
public :: Vorticity_Evolution_Step

! Mathematical constants
real, parameter :: pi = 4.0 * atan(1.0)

contains

!========================================================================================!
!
!                                   Vorticity Time Evolution
!                                           (First Step)
!
!========================================================================================!
subroutine Vorticity_Evolution_Step(x, y, dt, Re, BC, convective_field, previous_w, updated_w)
    implicit none

    !In
    real, intent(in)                               :: dt, Re
    real, intent(in)                               :: x(0:), y(0:), previous_w(0:, 0:)

    type(Boundary_Conditions), intent(in)          :: BC
    type(Bidimensional_Velocity_Field), intent(in) :: convective_field

    !Out
    real, intent(out)                              :: updated_w(0:, 0:)


    !Local variables
    integer                    :: Nx, Ny, i, j, s, Number_Equations
    integer, allocatable       :: Seeds_x(:), Seeds_y(:)

    real, allocatable          :: W_x_I(:,:,:), W_x_II(:,:,:), w_x(:,:), w_xx(:,:)
    real, allocatable          :: W_y_I(:,:,:), W_y_II(:,:,:), w_y(:,:), w_yy(:,:)

    real, allocatable          :: Row_Scaling(:)
    real, allocatable, target  :: Constants(:), Solution(:)
    real, pointer              :: w(:,:), Equation_w(:,:)


    !Extract number of intervals of the problem
    Nx = ubound(x,1)
    Ny = ubound(y,1)

    Number_Equations = (Nx + 1) * (Ny + 1)

    !Allocates the linear algebra vectors and initializes vectors
    allocate(Constants  (1:Number_Equations), &
             Solution   (1:Number_Equations), &
             Row_Scaling(1:Number_Equations)  )

    Constants = 0.0
    Solution  = 0.0

    ! Direct pointers
    w         (0:Nx, 0:Ny) => Solution
    Equation_w(0:Nx, 0:Ny) => Constants

!========================================================================================!
!                                       Prepare the Weights
!
!   Note: With the actual implementation, the Seeds need to be computed every time.
!========================================================================================!
    ! x - derivatives
    call Partial_Derivative_Weights_Seeds(x, 2, W_x_II, Seeds_x)
    deallocate(Seeds_x)
    call Partial_Derivative_Weights_Seeds(x, 1, W_x_I, Seeds_x)

    ! y - derivatives
    call Partial_Derivative_Weights_Seeds(y, 2, W_y_II, Seeds_y)
    deallocate(Seeds_y)
    call Partial_Derivative_Weights_Seeds(y, 1, W_y_I, Seeds_y)

    allocate(w_x(0:Nx, 0:Ny), w_xx(0:Nx, 0:Ny))
    allocate(w_y(0:Nx, 0:Ny), w_yy(0:Nx, 0:Ny))

    w_x  = 0.0
    w_xx = 0.0

    w_y  = 0.0
    w_yy = 0.0

!========================================================================================!
!                                       Matrix scaling
!========================================================================================!

    Row_Scaling = 1.0

    call Matrix_Row_Scaling(Row_Scaling)

!========================================================================================!
!                                   RHS of the equations
!========================================================================================!

    !Impose equation everywhere
    Equation_w = previous_w

    !Boundary conditions
    !-------------------

    Equation_w(0, :)        = BC%Left        ! Inlet
    Equation_w(1:, 0)       = BC%Bottom(1:)  ! Wall
    Equation_w(1:, Ny)      = 0.d0           ! Far-field

    Equation_w(Nx, 1:Ny-1)  = 0.d0           ! Outlet

    do i = 1, Number_Equations
        Constants(i) = Constants(i) / Row_Scaling(i)
    enddo

!========================================================================================!
!                                       Initial guess
!========================================================================================!

    w = previous_w

!========================================================================================!
!                                     Solve linear system
!========================================================================================!
    call Linear_Algebra_Solver(Matrix_Product = PDE_Equations,     &
                               Constants      = Constants,         &
                               Solution       = Solution,          &
                               Preconditioner = PDE_Preconditioner )

    updated_w = w ! Recover solution

    deallocate(Constants, Solution)

    deallocate(w_x, w_xx)
    deallocate(w_y, w_yy)


contains
!========================================================================================!
!
!                                       PDE Equations
!
!========================================================================================!
subroutine PDE_Equations(Vector, Result)

    real, intent(in),  target :: Vector(1:)
    real, intent(out), target :: Result(1:)

    !Local variables
    integer           :: i, j, s

    real, pointer     :: w(:,:), Equation_w(:,:) ! To write the equations

    real, allocatable :: w_x(:,:), w_xx(:,:)
    real, allocatable :: w_y(:,:), w_yy(:,:)
    real, allocatable :: PDE_Operator(:,:)

    ! Direct pointers
    w         (0:Nx, 0:Ny) => Vector
    Equation_w(0:Nx, 0:Ny) => Result

    allocate(w_x(0:Nx, 0:Ny), w_xx(0:Nx, 0:Ny))
    allocate(w_y(0:Nx, 0:Ny), w_yy(0:Nx, 0:Ny))

    allocate(PDE_Operator(0:Nx, 0:Ny))

    !Init
    w_x  = 0.0
    w_xx = 0.0

    w_y  = 0.0
    w_yy = 0.0

    PDE_Operator = 0.0

    !Compute derivatives field
    !-------------------------

    !Computes the spatial derivatives - (x) direction
    do i = 0, Nx
        s = Seeds_x(i)
        forall(j = 0:Ny) w_x (i,j) = sum(W_x_I(1,:,i)  * w(s:s+Degree, j))
        forall(j = 0:Ny) w_xx(i,j) = sum(W_x_II(1,:,i) * w(s:s+Degree, j))
    enddo

    !Computes the spatial derivatives - (y) direction
    do i = 0, Ny
        s = Seeds_y(i)
        forall(j = 0:Nx) w_y (j,i) = sum(W_y_I(1,:,i)  * w(j, s:s+Degree))
        forall(j = 0:Nx) w_yy(j,i) = sum(W_y_II(1,:,i) * w(j, s:s+Degree))
    enddo

    !Impose governing equations
    !--------------------------

    PDE_Operator = (w_yy + w_xx) / Re - (convective_field%U * w_x + convective_field%V * w_y)

    Equation_w = w - dt * (PDE_Operator)

    !Boundary conditions
    !-------------------

    Equation_w(0,  : )     = w    (0,    :)               ! Inlet
    Equation_w(1:Nx,   0)  = w_y  (1:Nx, 0)               ! Wall
    Equation_w(1:Nx,  Ny)  = w    (1:Nx, Ny)              ! Far-field
	Equation_w(Nx, 1:Ny-1) = w_xx (Nx, 1:Ny-1)            ! Outlet


    !Scaling of the equations
    do i = 1, Number_Equations
        Result(i) = Result(i) / Row_Scaling(i)
    enddo

    deallocate(w_x, w_xx)
    deallocate(w_y, w_yy)

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
end subroutine Vorticity_Evolution_Step
!========================================================================================!
!
!                                       End of module
!
!========================================================================================!
end module Vorticity_Evolution
