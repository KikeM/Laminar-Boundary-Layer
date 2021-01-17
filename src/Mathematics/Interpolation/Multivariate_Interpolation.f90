!========================================================================================!
!
! 								Multivariate interpolation
!
! Enrique Millan Valbuena ==================================================== 17.10.2017!
module Multivariate_Interpolation
use Piecewise_Polynomial_Interpolation

implicit none

contains

!========================================================================================!
!
! 									Interpolation
!
!========================================================================================!
subroutine Multivariate_Interpolation_From_File(x, y, f, Degree)
	integer, intent(in)  :: Degree
	real,    intent(in)  :: x(0:), y(0:)
	real,    intent(out) :: f(0:, 0:)

	! Local variables
	integer      	  :: Nx,     Ny,    i, j, io
	integer           :: Nx_old, Ny_old

	real, allocatable :: x_old(:), y_old(:), f_old(:,:), f_aux(:,:)
	real, allocatable :: interpolated_values(:,:)


	! Extract number of intervals of the problem
	Nx = size(x) - 1
	Ny = size(y) - 1

!========================================================================================!
! 							Read number of points the previous grids
!========================================================================================!

	open(1, file = 'old_x.dat', action = 'read', status = 'old')
	open(2, file = 'old_y.dat', action = 'read', status = 'old')

	Nx_old = 0
	Ny_old = 0

	! x - direction
	do
		read(1, *, iostat=io)

	  	if (io/=0) then
	  		exit
	  	else
	  		Nx_old = Nx_old + 1
	  	endif

	enddo

	! y - direction
	do
		read(2, *, iostat=io)

	  	if (io/=0) then
	  		exit
	  	else
	  		Ny_old = Ny_old + 1
	  	endif

	enddo

	!Rewind to load the mesh
	rewind(1)
	rewind(2)

	!Compute number of intervals
	Nx_old = Nx_old - 1
	Ny_old = Ny_old - 1

!========================================================================================!
! 								Load the previous mesh
!========================================================================================!

	allocate(x_old(0:Nx_old), y_old(0:Ny_old))

	x_old = 0.0
	y_old = 0.0

	!Read x - mesh
	do i = 0, Nx_old
		read(1, *) x_old(i)
	enddo

	!Read y - mesh
	do i = 0, Ny_old
		read(2, *) y_old(i)
	enddo

	!Close the mesh documents
	close(1)
	close(2)

!========================================================================================!
! 								Load the previous solution
!========================================================================================!

	open(1, file = 'old_solution.dat', action = 'read', status = 'old')

	allocate(f_old(0:Ny_old, 0:Nx_old))

	f_old = 0.0

	do i = 0, Ny_old
		read(1, *) f_old(Ny_old - i,:)
	enddo

	close(1)

!========================================================================================!
! 									Interpolate in x
!========================================================================================!

	allocate(f_aux(0:Ny_old, 0:Nx))
	allocate(interpolated_values(0:Nx, 1))

	interpolated_values = 0.0

	f_aux = 0.0

	do j = 0, Ny_old

		call Interpolate(Polynomial_Degree    = Degree,        &
						 Grid_Nodes           = x_old,         &
						 Nodal_Values         = f_old(j, :),   &
						 Interpolation_Points = x,             &
				    	 Derivatives          = (/ 0 /),       &
					     Interpolated_Values  = interpolated_values)

		f_aux(j, :) = interpolated_values(:,1)

    enddo

    deallocate(interpolated_values)

!========================================================================================!
! 									Interpolate in y
!========================================================================================!

	allocate(interpolated_values(0:Ny, 1))

	interpolated_values = 0.0

	do j = 0, Nx

		call Interpolate(Polynomial_Degree    = Degree,      &
						 Grid_Nodes           = y_old,       &
					     Nodal_Values         = f_aux(:, j), &
						 Interpolation_Points = y,           &
						 Derivatives          = (/ 0 /),     &
					     Interpolated_Values  = interpolated_values)

		f(:,j) = interpolated_values(:,1)

    enddo

	deallocate(interpolated_values)
	deallocate(f_aux, f_old)
	deallocate(x_old, y_old)

end subroutine Multivariate_Interpolation_From_File
end module Multivariate_Interpolation
