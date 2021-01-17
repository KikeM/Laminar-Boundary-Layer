program compact_differences

use Sparse_Linear_Algebra

implicit none

	integer                        :: i, N
	real, parameter                :: pi = 4.0 * atan(1.0)
	real                           :: h
	real, allocatable              :: x(:), f(:), df(:), df_exact(:)
	type(Tridiagonal_Matrix) :: A

	!Mesh size
	N = 5000

	allocate(x(0:N), f(0:N), df(0:N), df_exact(0:N))

	x  = 0.0

	f  = 0.0

	df       = 0.0
	df_exact = 0.0

	!Differentiation matrix
	allocate(A%Upper_Diagonal(1:N), &
			 A%Diagonal(1:N-1),       &
			 A%Lower_Diagonal(1:N)  )

	!Load bands for Padé compact differences
	A%Upper_Diagonal = 1.0
	A%Diagonal       = 4.0
	A%Lower_Diagonal = 1.0

	!Load function images
	do i = 0, N
		x(i)        = real(i) / real(N)
		f(i)        = sin(pi * x(i))
		df_exact(i) = pi * cos(pi * x(i))
	enddo

	h = 1.0 / real(N)

	write(*, '(A, e15.5)') 'h = ', h

	!Compute derivatives
	call Sparse_Linear_Algebra_Solver(Matrix    = A,                             &
		    				          Constants = 3.0 * (f(2:N) - f(0:N-2)) / h, &
						              Solution  = df(1:N-1)        	             )

	!Compare with analytical solution
	open(10, file = 'derivatives.dat', action = 'write', status = 'replace')

	do i = 1, N-1
		write(10, '(2f15.10, e15.5)') df_exact(i), df(i), abs(df_exact(i) - df(i))
	enddo

	close(10)

end program
