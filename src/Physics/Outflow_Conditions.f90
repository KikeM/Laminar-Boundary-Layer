!========================================================================================!
!
!                                   Poisson equation
!
! Enrique Millan Valbuena ==================================================== 17.10.2017!
module Outflow_Conditions

use Linear_Algebra
use Partial_Differential_Operators
use PDE_Properties
use High_Order_Integration

implicit none

private
public :: Outflow_Conditions_Streamfunction

! Mathematical constants
real, parameter :: pi = 4.0 * atan(1.0)

contains
!========================================================================================!
!
!                                   Parabolic outflow
!
!========================================================================================!
subroutine Outflow_Conditions_Streamfunction(y, f, BC, previous_psi, updated_psi)
    real,                      intent(in ) :: y(0:)
    real,                      intent(in ) :: f(0:)
    type(Boundary_Conditions), intent(in)  :: BC
    real,                      intent(in ) :: previous_psi(0:)
    real,                      intent(out) :: updated_psi(0:)

    !Local variables
    integer                    :: Ny, i, j, Number_Equations
    integer, allocatable       :: Seeds_y(:)

    real, allocatable          :: W_y_II(:,:,:), W_y_I(:,:,:), Integral_Weights(:)
    real, allocatable          :: Row_Scaling(:)
    real, allocatable, target  :: Constants(:), Solution(:)
    real, pointer              :: psi(:), Equation_psi(:)


    !Extract number of intervals of the problem
    Ny = ubound(y, 1)

    Number_Equations = Ny + 1

    !Allocates the linear algebra vectors and initializes vectors
    allocate(Constants  (1:Number_Equations), &
             Solution   (1:Number_Equations), &
             Row_Scaling(1:Number_Equations)  )

    allocate(Integral_Weights(0:Ny))

    Constants = 0.0
    Solution  = 0.0

    Integral_Weights = 0.0

    ! Direct pointers
    psi         (0:Ny) => Solution
    Equation_psi(0:Ny) => Constants

!========================================================================================!
!                                       Prepare Seeds and Weights
!========================================================================================!

    call Partial_Derivative_Weights_Seeds(y, 2, W_y_II, Seeds_y)
    deallocate(Seeds_y)
    call Partial_Derivative_Weights_Seeds(y, 1, W_y_I, Seeds_y)

!========================================================================================!
!                                       Matrix scaling
!========================================================================================!

    Row_Scaling = 1.0

    call Matrix_Row_Scaling(Row_Scaling)

!========================================================================================!
!                                Compatible boundary conditions
!========================================================================================!

	call Integration_Weights(Degree, y, 0.5 * (y(0:Ny-1) + y(1:Ny)), y(0), y(Ny), Integral_Weights)

!========================================================================================!
!                                   RHS of the equations
!========================================================================================!

    !Impose equation everywhere
    Equation_psi = -f

    !Boundary conditions
    !-------------------

     Equation_psi(0)  = 0.0                  !Wall
     Equation_psi(Ny) = -sum(f * Integral_Weights) !Far-field

     write(*,*) 'Integral = ', Equation_psi(Ny)

    do i = 1, Number_Equations
        Constants(i) = Constants(i) / Row_Scaling(i)
    enddo

!========================================================================================!
!                                       Initial guess
!========================================================================================!

    psi = previous_psi

!========================================================================================!
!                                     Solve linear system
!========================================================================================!
    call Linear_Algebra_Solver(Matrix_Product = PDE_Equations,     &
                               Constants      = Constants,         &
                               Solution       = Solution,          &
                               Preconditioner = PDE_Preconditioner )

    updated_psi = psi ! Recover solution

    deallocate(Constants, Solution)

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

    real, pointer     :: psi(:), Equation_psi(:) ! To write the equations

    real, allocatable :: psi_yy(:), psi_y(:)

    ! Direct pointers
    psi         (0:Ny) => Vector
    Equation_psi(0:Ny) => Result

    allocate(psi_yy(0:Ny), psi_y(0:Ny))

	psi_y  = 0.0
    psi_yy = 0.0

    !Compute derivatives field
    !-------------------------

    !Computes the spatial derivatives - (x) direction
    do i = 0, Ny
        s = Seeds_y(i)
		psi_yy(i) = sum(W_y_II(1,:,i)  * psi(s:s+Degree))
        psi_y (i) = sum(W_y_I (1,:,i)  * psi(s:s+Degree))
    enddo

    !Impose governing equations
    !--------------------------

    Equation_psi = psi_yy

    !Boundary conditions
    !-------------------

    Equation_psi(0)  = psi  (0)  !Wall
    Equation_psi(Ny) = psi_y(Ny) !Far-field

    !Scaling of the equations
    do i = 1, Number_Equations
        Result(i) = Result(i) / Row_Scaling(i)
    enddo

    deallocate(psi_yy, psi_y)

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
end subroutine Outflow_Conditions_Streamfunction
!========================================================================================!
!
!                                       End of module
!
!========================================================================================!
end module Outflow_Conditions
