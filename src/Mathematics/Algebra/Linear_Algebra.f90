!-----------------------------------------------------------------------------!
!                                 Linear_Algebra                              !
!                                                                             !
!                                                                             !
!---------------------------------------------------------------- 17.10.2017 -!
module Linear_Algebra
use Dense_Linear_Algebra
use Sparse_Linear_Algebra
implicit none

private
public :: Linear_Algebra_Solver, Matrix_Inverse, &
          Block_Tridiagonal_Matrix

! - Direct Linear Gaussian Solver:
!	Linear_Algebra_Solver(Matrix    = , &
!						  Constants = , &
!						  Solution  =    )

! - Iterative Linear FGMRES Solver:
!	Linear_Algebra_Solver(Matrix_Product = , &
!						  Constants 	 = , &
!						  Solution       = , &
!						  Preconditioner =   )

interface Linear_Algebra_Solver
  module procedure Dense_Linear_Algebra_Gauss, Sparse_Linear_Algebra_FGMRES
  module procedure Block_Tridiagonal_System
end interface Linear_Algebra_Solver

end module Linear_Algebra
