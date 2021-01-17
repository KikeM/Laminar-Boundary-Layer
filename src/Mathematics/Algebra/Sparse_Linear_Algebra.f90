!-----------------------------------------------------------------------------!
!                            Sparse_Linear_Algebra                            !
!                                                                             !
!---------------------------------------------------------------- 2016.03.01 -!
module Sparse_Linear_Algebra
use Dense_Linear_Algebra
implicit none

interface Sparse_Linear_Algebra_Solver
  module procedure Sparse_Linear_Algebra_FGMRES, &
                   Block_Tridiagonal_System, Tridiagonal_System
end interface Sparse_Linear_Algebra_Solver

!=============================================================================!
!                           Banded matrices types
!=============================================================================!

type Tridiagonal_Matrix
  real, allocatable :: Lower_Diagonal(:)
  real, allocatable ::       Diagonal(:)
  real, allocatable :: Upper_Diagonal(:)
end type Tridiagonal_Matrix

type Block_Tridiagonal_Matrix
  real, allocatable :: Sub_Diagonal(:,:,:)
  real, allocatable ::     Diagonal(:,:,:)
  real, allocatable :: Sup_Diagonal(:,:,:)
end type Block_Tridiagonal_Matrix

!=============================================================================!
!                           Public FGMRES information
!=============================================================================!

integer, public :: FGMRES_Iterations, FGMRES_Restart
real, public    :: FGMRES_norm2_r0, FGMRES_norm2_rn, FGMRES_norm2_stop, FGMRES_norm2_x

contains
!-----------------------------------------------------------------------------!
!                         Sparse_Linear_Algebra_FGMRES                        !
!                                                                             !
!                                                                             !
!---------------------------------------------------------------- 2016.03.01 -!
subroutine Sparse_Linear_Algebra_FGMRES(Matrix_Product, Constants, Solution, Preconditioner)
real, intent(in)    :: Constants(:)
real, intent(inout) :: Solution(:)

interface
  subroutine Matrix_Product(Vector, Result)
  real, intent(in ), target :: Vector(:)
  real, intent(out), target :: Result(:)
  end subroutine Matrix_Product

  subroutine Preconditioner(Vector, Result)
  real, intent(inout), target :: Vector(:)
  real, intent(inout), target :: Result(:)
  end subroutine Preconditioner
end interface

integer           :: Matrix_Dimension,   Iterations,     Restart_Iterations,    &
                     Maximum_Iterations, Workspace_Size, RCI_Request
integer           :: Parameters(128)
real              :: Tolerances(128)
real, allocatable :: Workspace(:)

!Opens file for residual recuperation
open(40, file = 'residuals.dat', action = 'write', status = 'replace')


!Extracts the information of the shapes
Matrix_Dimension   = size(Solution)

!Specifies the iteration strategy, allocates the workspace, and initializes FGMRES
Maximum_Iterations = 3 * Matrix_Dimension
Restart_Iterations = Matrix_Dimension

FGMRES_Restart = Restart_Iterations

Workspace_Size     = (2 * Restart_Iterations + 1) * Matrix_Dimension + Restart_Iterations * (Restart_Iterations + 9) / 2 + 1

allocate(Workspace(Workspace_Size))

call dfgmres_init(Matrix_Dimension, Solution, Constants, RCI_Request, Parameters, Tolerances, Workspace)

!Specifies the FGMRES parameters and tolerances

! Warning messages
Parameters( 2) = 6
Parameters( 6) = 1
Parameters( 7) = 1

!Maximum iterations
Parameters( 5) = Maximum_Iterations
!Test
!0 = no
!1 = yes
Parameters( 8) = 1

!Residual test
! Tol(5) <= Tol(4)
!0 = no
!1 = yes
Parameters( 9) = 1

!User-defined stopping test
!RCI_Request = 2
!0 = no
!1 = yes
Parameters(10) = 0

!Preconditioner
!0 = no
!1 = yes
Parameters(11) = 0

!Automatic test for zero norm of generated vector
! Tol(7) <= Tol(8)
! 0 = no
! 1 = yes
Parameters(12) = 1

!Location of returned solution
!0 = Solution vector
!1 = Constants vector
Parameters(13) = 0

!Number of non-restarted iterations
Parameters(15) = Restart_Iterations

! Tolerances
Tolerances( 1) = 10.0 ** (-10.0)  ! Relative tolerance
Tolerances( 2) = 10.0 ** (- 7.0)  ! Absolute tolerance
!Tolerances( 3) =                 ! Euclidean norm of the initial residual
!Tolerances( 4) =                 ! Tol(1) * Tol(3) + Tol(2)
!Tolerances( 5) =                 ! Euclidean norm of the current residual
!Tolerances( 6) =                 ! Euclidean norm of the previous residual
!Tolerances( 7) =                 ! Norm of the current generated vector
!Tolerances( 8) =                 ! Tolerance for the zero norm of the currently generated vector


call dfgmres_check(Matrix_Dimension, Solution, Constants, RCI_Request, Parameters, Tolerances, Workspace)

!Calls the solver and acts in each iteration as requested by the Reverse Communication Interface (RCI)
RCI_Request = 1

do while (RCI_Request > 0)

  call dfgmres(Matrix_Dimension, Solution, Constants, RCI_Request, Parameters, Tolerances, Workspace)

  if(RCI_Request == 1) then
    call Matrix_Product(Workspace(Parameters(22):Parameters(22) + Matrix_Dimension - 1), &
                        Workspace(Parameters(23):Parameters(23) + Matrix_Dimension - 1))

    !Update residuals
    FGMRES_norm2_r0   = Tolerances(3)
    FGMRES_norm2_stop = Tolerances(4)
    FGMRES_norm2_rn   = Tolerances(5)
    FGMRES_norm2_x    = Tolerances(7)

    write(40, '(4e20.10)') FGMRES_norm2_r0, FGMRES_norm2_stop, FGMRES_norm2_rn, FGMRES_norm2_x

  endif

  if(RCI_Request == 3) call Preconditioner(Workspace(Parameters(22):Parameters(22) + Matrix_Dimension - 1), &
                                           Workspace(Parameters(23):Parameters(23) + Matrix_Dimension - 1))

end do

!Recovers the converged solution and frees the FGMRES buffers
call dfgmres_get(Matrix_Dimension, Solution, Constants, RCI_Request, Parameters, Tolerances, Workspace, FGMRES_Iterations)

!write(*,*) 'Iterations = ', FGMRES_Iterations

call mkl_free_buffers

close(40)

end subroutine Sparse_Linear_Algebra_FGMRES
!-----------------------------------------------------------------------------!
!                             Thomas Algorithm                                !
!                                                                             !
!                      Solution for tridiagonal systems                       !
!                                                                             !
!                                                                             !
!---------------------------------------------------------------- 2017.11.03 -!
pure subroutine Tridiagonal_System(Matrix, Constants, Solution)
    type(Tridiagonal_Matrix), intent(in)  :: Matrix
    real,                     intent(in)  :: Constants(:)
    real,                     intent(out) :: Solution(:)

    !Local variables
    integer           :: start, final, i

    real, allocatable :: lower_diagonal(:), diagonal(:), &
                         upper_diagonal(:), RHS(:)

    start = lbound(Constants, 1)
    final = ubound(Constants, 1)

    allocate(lower_diagonal(start:final), &
             diagonal      (start:final), &
             upper_diagonal(start:final), &
             RHS           (start:final)  )

    lower_diagonal = 0.0
    diagonal       = 0.0
    upper_diagonal = 0.0
    RHS            = 0.0

    !Copy data
    lower_diagonal = Matrix%Lower_Diagonal
          diagonal = Matrix%Diagonal
    upper_diagonal = Matrix%Upper_Diagonal
               RHS = Constants

    !Forward elimination phase
    do i = start + 1, final
        diagonal(i) = diagonal(i) - lower_diagonal(i) / diagonal(i-1) * upper_diagonal(i-1)
        RHS(i) = RHS(i) - lower_diagonal(i) / diagonal(i-1) * RHS(i-1)
    enddo

    !Backward substitution phase
    Solution(final) = RHS(final) / diagonal(final)

    do i = final - 1, start, -1
        Solution(i) = (RHS(i) - upper_diagonal(i) * Solution(i+1)) / Diagonal(i)
    enddo

    deallocate(lower_diagonal, diagonal, upper_diagonal, RHS)

end subroutine Tridiagonal_System
!-----------------------------------------------------------------------------!
!                           Block_Tridiagonal_System                          !
!                                                                             !
!                   Matrix%Sub_Diagonal(1:Nv,1:Nv,Nb0+1:Nb  )                 !
!                   Matrix%Diagonal(    1:Nv,1:Nv,Nb0  :Nb  )                 !
!                   Matrix%Sup_Diagonal(1:Nv,1:Nv,Nb0  :Nb-1)                 !
!                                                                             !
!                            Constants(1:Nv,Nb0:Nb)                           !
!                            Variables(1:Nv,Nb0:Nb)                           !
!                                                                             !
!---------------------------------------------------------------- 2016.03.01 -!
pure subroutine Block_Tridiagonal_System(Matrix, Constants, Variables)
type(Block_Tridiagonal_Matrix), intent(in)  :: Matrix
real,                           intent(in)  :: Constants(:,lbound(Matrix%Diagonal,3):)
real,                           intent(out) :: Variables(:,lbound(Matrix%Diagonal,3):)

integer           :: Nb0, Nb, Nv, n
real, allocatable :: A(:,:), B(:,:), Bm1(:,:), C(:,:), k(:), g(:,:), H(:,:,:), x(:)

!Extracts the information of the shapes and allocates memory
Nb0 = lbound(Matrix%Diagonal, 3)
Nb  = ubound(Matrix%Diagonal, 3)
Nv  = size(  Matrix%Diagonal, 1)

allocate(A(Nv,Nv), B(Nv,Nv), Bm1(Nv,Nv), C(Nv,Nv), k(Nv), g(Nv,Nb0:Nb), H(Nv,Nv,Nb0:Nb), x(Nv))

!Performs first the forward elimination
B = Matrix%Diagonal(    :,:,Nb0)
C = Matrix%Sup_Diagonal(:,:,Nb0)
k = Constants(            :,Nb0)

call Matrix_Inverse(B, Bm1)

g(  :,Nb0) = matmul(Bm1, k)
H(:,:,Nb0) = matmul(Bm1, C)

do n = Nb0+1, Nb-1

  A = Matrix%Sub_Diagonal(:,:,n)
  B = Matrix%Diagonal(    :,:,n)
  C = Matrix%Sup_Diagonal(:,:,n)
  k = Constants(            :,n)

  call Matrix_Inverse(B - matmul(A, H(:,:,n-1)), Bm1)

  g(  :,n) = matmul(Bm1, k - matmul(A, g(:,n-1)))
  H(:,:,n) = matmul(Bm1, C)

enddo

A = Matrix%Sub_Diagonal(:,:,Nb)
B = Matrix%Diagonal(    :,:,Nb)
k = Constants(            :,Nb)

call Matrix_Inverse(B - matmul(A, H(:,:,Nb-1)), Bm1)

g(:,Nb) =  matmul(Bm1, k - matmul(A, g(:,Nb-1)))

!Then performs the backward substitution
x               = g(:,Nb)
Variables(:,Nb) = x

do n = Nb-1, Nb0, -1
  x              = g(:,n) - matmul(H(:,:,n), x)
  Variables(:,n) = x
enddo

end subroutine Block_Tridiagonal_System
!-----------------------------------------------------------------------------!
!                   Sparse_Linear_Algebra_FGMRES_Auxiliary                    !
!                                                                             !
!                                                                             !
!---------------------------------------------------------------- 2016.03.01 -!
subroutine Sparse_Linear_Algebra_FGMRES_Auxiliary(Matrix_Product, Constants, Solution, Preconditioner)
real, intent(in)    :: Constants(:)
real, intent(inout) :: Solution(:)

interface
    subroutine Matrix_Product(Vector, Result)
        real, intent(in ), target :: Vector(:)
        real, intent(out), target :: Result(:)
    end subroutine Matrix_Product

    subroutine Preconditioner(Vector, Result)
        real, intent(inout), target :: Vector(:)
        real, intent(inout), target :: Result(:)
    end subroutine Preconditioner
end interface

integer           :: Matrix_Dimension,   Iterations,     Restart_Iterations,    &
                     Maximum_Iterations, Workspace_Size, RCI_Request
integer           :: Parameters(128)
real              :: Tolerances(128)
real, allocatable :: Workspace(:)

!Opens file for residual recuperation
!open(40, file = 'residuals.dat', action = 'write', status = 'replace')


!Extracts the information of the shapes
Matrix_Dimension   = size(Solution)

!Specifies the iteration strategy, allocates the workspace, and initializes FGMRES
Maximum_Iterations = 3 * Matrix_Dimension
Restart_Iterations = Matrix_Dimension

Workspace_Size     = (2 * Restart_Iterations + 1) * Matrix_Dimension + Restart_Iterations * (Restart_Iterations + 9) / 2 + 1

allocate(Workspace(Workspace_Size))

call dfgmres_init(Matrix_Dimension, Solution, Constants, RCI_Request, Parameters, Tolerances, Workspace)

!Specifies the FGMRES parameters and tolerances

! Warning messages
Parameters( 2) = 6
Parameters( 6) = 1
Parameters( 7) = 1

!Maximum iterations
Parameters( 5) = Maximum_Iterations
!Test
!0 = no
!1 = yes
Parameters( 8) = 1

!Residual test
! Tol(5) <= Tol(4)
!0 = no
!1 = yes
Parameters( 9) = 1

!User-defined stopping test
!RCI_Request = 2
!0 = no
!1 = yes
Parameters(10) = 0

!Preconditioner
!0 = no
!1 = yes
Parameters(11) = 0

!Automatic test for zero norm of generated vector
! Tol(7) <= Tol(8)
! 0 = no
! 1 = yes
Parameters(12) = 1

!Location of returned solution
!0 = Solution vector
!1 = Constants vector
Parameters(13) = 0

!Number of non-restarted iterations
Parameters(15) = Restart_Iterations

! Tolerances
Tolerances( 1) = 10.0 ** (-12.0)  ! Relative tolerance
Tolerances( 2) = 10.0 ** (- 8.0)   ! Absolute tolerance
!Tolerances( 3) =                 ! Euclidean norm of the initial residual
!Tolerances( 4) =                 ! Tol(1) * Tol(3) + Tol(2)
!Tolerances( 5) =                 ! Euclidean norm of the current residual
!Tolerances( 6) =                 ! Euclidean norm of the previous residual
!Tolerances( 7) =                 ! Norm of the current generated vector
!Tolerances( 8) =                 ! Tolerance for the zero norm of the currently generated vector


call dfgmres_check(Matrix_Dimension, Solution, Constants, RCI_Request, Parameters, Tolerances, Workspace)

!Calls the solver and acts in each iteration as requested by the Reverse Communication Interface (RCI)
RCI_Request = 1

do while (RCI_Request > 0)

  call dfgmres(Matrix_Dimension, Solution, Constants, RCI_Request, Parameters, Tolerances, Workspace)

  if(RCI_Request == 1) then
    call Matrix_Product(Workspace(Parameters(22):Parameters(22) + Matrix_Dimension - 1), &
                        Workspace(Parameters(23):Parameters(23) + Matrix_Dimension - 1))

    !Update residuals
    !norm2_r0   = Tolerances(3)
    !norm2_rn   = Tolerances(5)
    !norm2_stop = Tolerances(4)
    !write(40, '(4e15.5)') Tolerances(3), Tolerances(4), Tolerances(5), Tolerances(7)

  endif

  if(RCI_Request == 3) call Preconditioner(Workspace(Parameters(22):Parameters(22) + Matrix_Dimension - 1), &
                                           Workspace(Parameters(23):Parameters(23) + Matrix_Dimension - 1))



end do

!Recovers the converged solution and frees the FGMRES buffers
call dfgmres_get(Matrix_Dimension, Solution, Constants, RCI_Request, Parameters, Tolerances, Workspace, FGMRES_Iterations)

!write(*,*) 'Iterations = ', FGMRES_Iterations

call mkl_free_buffers

!close(40)

end subroutine Sparse_Linear_Algebra_FGMRES_Auxiliary
!-----------------------------------------------------------------------------!
end module Sparse_Linear_Algebra
