module Nonlinear_Algebra
use Linear_Algebra
implicit none

contains
!-----------------------------------------------------------------------!
subroutine Newton_Raphson_Method(Nonlinear_System, Variables)
real, intent(inout) :: Variables(:)

interface
  subroutine Nonlinear_System(Variables, Equations)
  real, intent(in)  :: Variables(0:)
  real, intent(out) :: Equations(0:)
  end subroutine Nonlinear_System
end interface

integer :: Number_Variables, v
real    :: Equations(size(Variables),-1:1), Perturbation(size(Variables)), &
           Iteration_Error, Jacobi_Matrix(size(Variables),size(Variables))

!Extracts the information of the shapes
Number_Variables = size(Variables)

!Iterates with the Newton-Raphson Method till the error is small
Iteration_Error = 1d0

do while (Iteration_Error > 1d-10)

  !Evaluates the nonlinear system with the actual solution
  call Nonlinear_System(Variables, Equations(:,0))
  
  !Constructs the Jacobi matrix
  do v = 1, Number_Variables
    
    Perturbation    = 0d0
    Perturbation(v) = 1d-8
  
    call Nonlinear_System(Variables + Perturbation, Equations(:, 1))
    call Nonlinear_System(Variables - Perturbation, Equations(:,-1))

    Jacobi_Matrix(:,v) = (Equations(:,1) - Equations(:,-1)) / (2d0 * 1d-8)
  enddo

  !Substitutes zero equations by zero perturbation conditions
  do v = 1, Number_Variables
    if(maxval(abs(Jacobi_Matrix(v,:))) /= 0d0) cycle
    
    Jacobi_Matrix(v,:) = 0d0
    Jacobi_Matrix(v,v) = 1d0
  enddo

  !Determines the variation of the variables
  call Linear_Algebra_Solver(Jacobi_Matrix, Equations(:,0), Perturbation)

  !Updates the variables and computes the iteration error
  Variables       = Variables - Perturbation
  Iteration_Error = maxval(abs(Perturbation))
  
enddo

end subroutine Newton_Raphson_Method
!-----------------------------------------------------------------------!
end module Nonlinear_Algebra
