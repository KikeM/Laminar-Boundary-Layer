!-----------------------------------------------------------------------------!
!                              Dense_Linear_Algebra                           !
!                                                                             !
!                                                                             !
!---------------------------------------------------------------- 2016.03.01 -!
module Dense_Linear_Algebra
implicit none

interface Dense_Linear_Algebra_Solver
  module procedure Dense_Linear_Algebra_Gauss
end interface Dense_Linear_Algebra_Solver

contains
!-----------------------------------------------------------------------------!
!                           Dense_Linear_Algebra_Gauss                        !
!                                                                             !
!                                                                             !
!---------------------------------------------------------------- 2014.03.06 -!
pure subroutine Dense_Linear_Algebra_Gauss(Matrix, Constants, Solution)
	real, intent(in)  :: Matrix(:,:), Constants(:)
	real, intent(out) :: Solution(:)

	integer :: r(ubound(Matrix,1)), N, i, j, max_loc(1)
	real    :: C(ubound(Matrix,1),ubound(Matrix,2)+1)

	!Extracts the information of the shapes
	N = ubound(Matrix, 1)

	!Constructs the augmented matrix C
	C(:,1:N) = Matrix
	C(:,N+1) = Constants

	forall(i=1:N) r(i) = i

	!Solves the system of equations by Gauss elimination with pivoting
	do j = 1, N

	  !Finds the row with the largest element in column j
	  max_loc = maxloc(abs(C(r(j:N),j))) + j - 1

	  i             = r(j)
	  r(j)          = r(max_loc(1))
	  r(max_loc(1)) = i

	  !Performs the elimination and backward substitution all at once
	  C(r(j),:) = C(r(j),:) / C(r(j),j)

	  do i = 1, N
		if(r(i) /= r(j)) C(r(i),:) = C(r(i),:) - C(r(i),j) * C(r(j),:)
	  enddo
	enddo

	Solution = C(r,N+1)

end subroutine Dense_Linear_Algebra_Gauss
!-----------------------------------------------------------------------------!
!                               Matrix_Inverse                                !
!                                                                             !
!                                                                             !
!---------------------------------------------------------------- 2015.11.03 -!
pure subroutine Matrix_Inverse(Matrix, Inverse_Matrix)
	real, intent(in)  :: Matrix(:,:)
	real, intent(out) :: Inverse_Matrix(:,:)

	integer :: r(ubound(Matrix,1)), N, i, j, max_loc(1)
	real    :: C(ubound(Matrix,1),2*ubound(Matrix,2))

	!Extracts the information of the shapes
	N = ubound(Matrix, 1)

	!Constructs the augmented matrix C
	C        = 0d0
	C(:,1:N) = Matrix

	forall(i=1:N) C(i,N+i) = 1d0
	forall(i=1:N) r(i    ) = i

	!Solves the system of equations by Gauss elimination with pivoting
	do j = 1, N

	  !Finds the row with the largest element in column j
	  max_loc = maxloc(abs(C(r(j:N),j))) + j - 1

	  i             = r(j)
	  r(j)          = r(max_loc(1))
	  r(max_loc(1)) = i

	  !Continues with the solving process
	  C(r(j),:) = C(r(j),:) / C(r(j),j)

	  do i = 1, N
		if(r(i) /= r(j)) C(r(i),:) = C(r(i),:) - C(r(i),j) * C(r(j),:)
	  enddo
	enddo

	Inverse_Matrix = C(r,N+1:)

end subroutine Matrix_Inverse


end module Dense_Linear_Algebra
