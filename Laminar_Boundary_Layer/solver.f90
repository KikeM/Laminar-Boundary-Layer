program Navier_Stokes_2D_Boundary_Layer

use Partial_Differential_Operators
use Ordinary_Differential_Operators
use Fields
use Optimum_Grid_Methods
use Coupled_System
use Blasius_Boundary_Layer
use PDE_Properties

implicit none

!========================================================================================!
!                                Declaration of variables
!========================================================================================!

    character(30) :: output_format, filename

    !Integers
    integer, parameter   :: residual_output = 10

    integer              :: Nx, Ny, Nt, s, Number_Nodes
    integer              :: i, j, u

    !Finite differences
    integer, allocatable :: Seeds_x(:), Seeds_y(:)
	real,    allocatable :: W_x_I (:,0:,0:), W_y_I (:,0:,0:)
    real,    allocatable :: W_x_II(:,0:,0:), W_y_II(:,0:,0:)

    !Reals
    real, parameter   :: pi        = 4.0 * atan(1.0), &
                         Tolerance = 1.d-7

	!Problem parameters
    real              :: cpu_t0, cpu_t1
    real              :: L, ye, gamma
    real              :: Re, blasius_external_Ue, u_bl, xc

	!Fluid variables
    real, allocatable :: delta_streamfunction(:,:), delta_streamfunction_0(:,:), streamfunction_0(:,:)
    real, allocatable :: delta_vorticity(:,:),      delta_vorticity_0(:,:), vorticity_0(:,:)
    real, allocatable :: x(:), y(:)
    real, allocatable :: dp_dx(:), ue(:), due_dx(:)

    !Derived types
    type(Boundary_Conditions)           :: vorticity_BC, streamfunction_BC
    type(Bidimensional_Velocity_Field)  :: velocity
    type(Unidimensional_Velocity_Field) :: blasius

!========================================================================================!
!                        Set-up size problem and allocate memory
!========================================================================================!

    !Start counting execution time
    call cpu_time(cpu_t0)

    !Set problem size
    write(*,*) 'Define problem size: Nx, Ny, Nt'
    read(*,*) Nx
    read(*,*) Ny
    read(*,*) Nt

    Number_Nodes = (Nx + 1) * (Ny + 1)

    !Initial vorticity field
    allocate(dp_dx(0:Nx), ue(0:Nx), due_dx(0:Nx))

    !Fluid variables
    allocate(delta_streamfunction  (0:Nx, 0:Ny), &
    		 delta_streamfunction_0(0:Nx, 0:Ny), &
             streamfunction_0      (0:Nx, 0:Ny), &
             delta_vorticity       (0:Nx, 0:Ny), &
             delta_vorticity_0     (0:Nx, 0:Ny), &
             vorticity_0           (0:Nx, 0:Ny)  )

    allocate(velocity%U(0:Nx, 0:Ny), velocity%V(0:Nx, 0:Ny))

    !Boundary conditions
    allocate(vorticity_BC%Left  (0:Ny), &
             vorticity_BC%Right (0:Ny), &
             vorticity_BC%Upper (0:Nx), &
             vorticity_BC%Bottom(0:Nx)  )

    allocate(streamfunction_BC%Left  (0:Ny), &
             streamfunction_BC%Right (0:Ny), &
             streamfunction_BC%Upper (0:Nx), &
             streamfunction_BC%Bottom(0:Nx)  )

    !Blasius profile
    allocate(blasius%U(0:Ny),            &
             blasius%V(0:Ny),            &
             blasius%Streamfunction(0:Ny))


!========================================================================================!
!                                   Arrays initialization
!========================================================================================!

	!Blasius inlet
    blasius%U              = 0.0
    blasius%V              = 0.0
    blasius%streamfunction = 0.0

	!Wall pressure gradient
    dp_dx = 0.0

    !Far-field velocity field
    ue     = 0.0
    due_dx = 0.0

    !Vorticity
    vorticity_0         = 0.0
    delta_vorticity     = 0.0
    delta_vorticity_0   = 0.0

    vorticity_BC%Left   = 0.0
    vorticity_BC%Right  = 0.0
    vorticity_BC%Upper  = 0.0
    vorticity_BC%Bottom = 0.0

    !Streamfunction
    delta_streamfunction   = 0.0
    delta_streamfunction_0 = 0.0
    streamfunction_0       = 0.0

    streamfunction_BC%Left   = 0.0
    streamfunction_BC%Right  = 0.0
    streamfunction_BC%Upper  = 0.0
    streamfunction_BC%Bottom = 0.0

!========================================================================================!
!                                  Simulation parameters
!========================================================================================!

    !Reynolds number
    write(*,*) 'Define Reynolds number'
    read(*,*) Re

    Re = real(Re)

    !Relaxation factor
    write(*,*) 'Define relaxation factor: x(n+1) = x(n) + gamma * dx(n)'
    read(*,*) gamma

    gamma = real(gamma)

!========================================================================================!
!                                           Mesh
!========================================================================================!

    ! Interpolation degree
    Degree = 6
    ODE_Degree = Degree

    allocate(x(0:Nx), y(0:Ny))

    ! Mesh
    x = 0.0
    y = 0.0

    call Optimum_Grid_Nodes(Degree, x) ! x in [-1, 1]
    call Optimum_Grid_Nodes(Degree, y) ! y in [-1, 1]

	!L
    write(*,*) 'Define L:'
    read(*,*) L

	!ye
    write(*,*) 'Define ye:'
    read(*,*) ye

     L = real(L)
    ye = real(ye)

    x = (x + 1.0) / 2.0 ! x in  [0,  L  ]
    y = (y + 1.0) / 2.0 ! y in  [0,  ye ]

    !TODO: create a subroutine to write down the mesh
    ! x - mesh
    open(newunit = u, file = 'x.dat', action = 'write', status = 'replace')
    do i = 0, Nx
        write(u, *) x(i)
    enddo
    close(u)

    ! y - mesh
    open(newunit = u, file = 'y.dat', action = 'write', status = 'replace')
    do i = Ny, 0, -1
        write(u, *) y(i)
    enddo
    close(u)

!========================================================================================!
!                                       Blasius Inlet
!                          Also computes the initial vorticity field
!========================================================================================!

    blasius_external_Ue = 1.0

    call Blasius_Boundary_Layer_Solution(y, blasius_external_Ue, 1000.d0, &
                                         blasius%streamfunction, blasius%U, blasius%V)

	!Output far-field streamwise velocity
	open(newunit = u, file = 'inlet_conditions.dat', action = 'write', status = 'replace')
	do i = Ny, 0, -1
		write(u, '(3f20.10)') blasius%streamfunction(i), blasius%U(i), blasius%V(i)
	enddo
	close(u)

!========================================================================================!
!                                    Initial streamfunction
!========================================================================================!

	!call External_Velocity(x, blasius%U(Ny), ue, xc)

	ue = blasius%U(Ny)

	call Derivative(ue, x, 1, due_dx)

	!Output far-field streamwise velocity
	open(newunit = u, file = 'far_field_streamwise_velocity.dat', action = 'write', status = 'replace')
	do i = 0, Nx
		write(u, '(3f20.10)') ue(i), due_dx(i), - 0.5d0 * ue(i) * due_dx(i)
	enddo
	close(u)

    !Boundary conditions
    streamfunction_BC%Left   = blasius%streamfunction ! Inlet
    streamfunction_BC%Right  = 0.0 					  ! Outlet
    streamfunction_BC%Upper  = ue                     ! Far-field
    streamfunction_BC%Bottom = 0.0                    ! Wall

    !Scaling of the inlet profile to match the velocity at the far-field
	!do j = 0, Ny
	!	do i = 1, Nx
	!	    velocity%U(i,j)       = blasius%U(j) * ue(i) / ue(0)
	!        streamfunction_0(i,j) = blasius%streamfunction(j) * ue(i) / ue(0)
    !    enddo
    !enddo

!========================================================================================!
!                                   Initial vorticity field
!========================================================================================!

    !Computes the spatial derivatives - (y) direction
    call Partial_Derivative(-velocity%U / ye, x, y, (/ 0, 1 /), vorticity_0)

	!Show the initial vorticity
	call Vorticity_Output_Solution(vorticity_0, 111)

    !Boundary conditions
    vorticity_BC%Left   = vorticity_0(0,:)
    vorticity_BC%Right  = 0.0
    vorticity_BC%Upper  = 0.0

    !Pressure gradient
    vorticity_BC%Bottom = 0.d0

!========================================================================================!
!                                   Output parameters file
!========================================================================================!

    open(newunit = u, file = 'parameters.dat', action = 'write', status = 'replace')

		write(u, '(A, f10.4)') 'Re = ', Re
		write(u, '(A, e15.4)') 'Tolerance = ', Tolerance

		write(u, '(A, f15.4)') 'L = ',   L
		write(u, '(A, f15.4)') 'ye = ', ye

		write(u, '(A, I4)') 'Nx = ', Nx
		write(u, '(A, I4)') 'Ny = ', Ny

		write(u, '(A, f20.5)') 'u_bl = ', blasius%U(Ny)
		write(u, '(A, f20.5)') 'x_c = ', xc

    close(u)

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
!                                     Time integration
!========================================================================================!

    open(residual_output, file = 'time_residual.dat', action = 'write', status = 'replace')

    !Advance in time
    do i = 0, Nt

		write(*,'(A, I4)') 'Iteration = ', i

        !Compute coupled system
        call Coupled_System_Solution(x                 = x,                      &
									 y                 = y,                      &
									 L                 = L,                      &
									 ye                = ye,                     &
									 Re                = Re,                     &
									 streamfunction_BC = streamfunction_BC,      &
									 vorticity_BC      = vorticity_BC,           &
									 W_x_I             = W_x_I,                  &
									 W_y_I             = W_y_I,                  &
									 W_x_II            = W_x_II,                 &
									 W_y_II            = W_y_II,                 &
									 Seeds_x           = Seeds_x,                &
									 Seeds_y           = Seeds_y,                &
									 psi_0             = streamfunction_0,       &
									 delta_psi_0       = delta_streamfunction_0, &
									 w_0               = vorticity_0,            &
									 delta_w_0         = delta_vorticity_0,      &
									 delta_psi         = delta_streamfunction,   &
									 delta_w           = delta_vorticity         )

        !------------ Write out convergence ------------
        write(*, '(/, A, e15.5)'   ) 'norm2(r_vorticity)      = ', norm2((delta_vorticity     ) / Number_Nodes)
        write(*, '(   A, e15.5, /)') 'norm2(r_streamfunction) = ', norm2((delta_streamfunction) / Number_Nodes)

        write(residual_output, '(2f30.15)') norm2((delta_vorticity     ) / Number_Nodes), &
                                            norm2((delta_streamfunction) / Number_Nodes)

		!------------ Check for convergence ------------
		if(norm2(delta_streamfunction / Number_Nodes) < Tolerance .AND. &
		   norm2(delta_vorticity      / Number_Nodes) < Tolerance)      then

            write(*, '(/, A, I4, /)') 'Convergence in L2 was reached at the step = ', i
            exit

        endif

        !------------ Update for next iteration ------------
        vorticity_0      = vorticity_0      + gamma * delta_vorticity
        streamfunction_0 = streamfunction_0 + gamma * delta_streamfunction

        delta_streamfunction_0 = delta_streamfunction
        delta_vorticity_0      = delta_vorticity

        !------------ Print out the solution ------------
        if (i == 0 .OR. mod(i,10) == 0) then

        	!Compute velocity field
	        call Velocity_Field(x, y, streamfunction_0, velocity%U, velocity%V)

    		!Output solutions
        	call Streamlines_Output_Solution(streamfunction_0, i)
        	call Vorticity_Output_Solution(vorticity_0, i)
        	call Output_Solution(velocity%U, i, 'u.dat')
   			call Output_Solution(velocity%V, i, 'v.dat')

   			!Compute velocity field
	        call Velocity_Field(x, y, delta_streamfunction, velocity%U, velocity%V)

			!Output solutions
        	call Streamlines_Output_Solution(delta_streamfunction, 111)
        	call Vorticity_Output_Solution(delta_vorticity, 222)
        	call Output_Solution(velocity%U, 333, 'u.dat')
   			call Output_Solution(velocity%V, 444, 'v.dat')

   		endif

    enddo

    close(residual_output)

!========================================================================================!
!                                      Computer time
!========================================================================================!

    !Compute velocity field
	call Velocity_Field(x, y, streamfunction_0, velocity%U, velocity%V)

	!Output converged solution
	call Streamlines_Output_Solution(streamfunction_0, i)
	call Vorticity_Output_Solution(vorticity_0, i)
	call Output_Solution(velocity%U, i, 'u.dat')
	call Output_Solution(velocity%V, i, 'v.dat')

!========================================================================================!
!                                      Computer time
!========================================================================================!

    call cpu_time(cpu_t1)

    write(*,'(A, f10.5, A)') 'Computed time = ',  cpu_t1 - cpu_t0,       ' (s) '
    write(*,'(A, f10.5, A)') '                ', (cpu_t1 - cpu_t0) / 60, '(min)'

contains
!========================================================================================!
!                               SUBROUTINE: Output solution
!========================================================================================!
subroutine Streamlines_Output_Solution(scalar_field, time_step)
    implicit none
    integer, intent(in) :: time_step
    real,    intent(in) :: scalar_field(0:,0:)

    integer       :: i, Ny, Nx, u
    character(20) :: filename

    Nx = size(scalar_field, 1) - 1
    Ny = size(scalar_field, 2) - 1

    !Create filename
    write(filename, '(I4, A)') time_step , '_psi.dat'
    filename = trim(filename)

    !Open file
    open(newunit = u, file = filename, action = 'write', status = 'replace')

    !Write output format
    write(output_format, '(A, I4, A)') '(', Nx + 1, 'f30.15)'
    output_format = trim(output_format)

    !Write output
    do i = Ny, 0, -1
        write(u, output_format) scalar_field(:, i)
    enddo

    !Close file
    close(u)

end subroutine Streamlines_Output_Solution
!========================================================================================!
!                               SUBROUTINE: Output solution
!========================================================================================!
subroutine Vorticity_Output_Solution(scalar_field, time_step)
    implicit none
    integer, intent(in) :: time_step
    real,    intent(in) :: scalar_field(0:,0:)

    integer       :: i, Ny, Nx, u
    character(20) :: filename

    Nx = size(scalar_field, 1) - 1
    Ny = size(scalar_field, 2) - 1

    !Create filename
    write(filename, '(I4, A)') time_step , '_w.dat'
    filename = trim(filename)

    !Open file
    open(newunit = u, file = filename, action = 'write', status = 'replace')

    !Write output format
    write(output_format, '(A, I4, A)') '(', Nx + 1, 'f30.15)'
    output_format = trim(output_format)

    !Write output
    do i = Ny, 0, -1
        write(u, output_format) scalar_field(:, i)
    enddo

    !Close file
    close(u)

end subroutine Vorticity_Output_Solution
!========================================================================================!
!                               SUBROUTINE: Output solution
!========================================================================================!
subroutine Output_Solution(scalar_field, time_step, name)
    implicit none
    character(5), intent(in) :: name
    integer, intent(in)   :: time_step
    real,    intent(in)   :: scalar_field(0:,0:)

    integer       :: i, Ny, Nx, u
    character(20) :: filename

    Nx = size(scalar_field, 1) - 1
    Ny = size(scalar_field, 2) - 1

    !Create filename
    write(filename, '(I4, A)') name, time_step
    filename = trim(filename)

    !Open file
    open(newunit = u, file = filename, action = 'write', status = 'replace')

    !Write output format
    write(output_format, '(A, I4, A)') '(', Nx + 1, 'f30.15)'
    output_format = trim(output_format)

    !Write output
    do i = Ny, 0, -1
        write(u, output_format) scalar_field(:, i)
    enddo

    !Close file
    close(u)

end subroutine Output_Solution
!========================================================================================!
!                               SUBROUTINE: External velocity
!========================================================================================!
subroutine External_Velocity(x, u_bl, ue, xc)
    implicit none
    real,    intent(in)    :: x(0:), u_bl
    real,    intent(inout) :: ue(0:), xc

	!Local variableso
    integer       :: Nx, i, j
	real          :: u_out

    Nx = ubound(x, 1)

    write(*,*) 'Insert break point xc'
    read(*,*) xc

    write(*,*) 'Insert outlet velocity u_out'
    read(*,*) u_out

    xc = real(xc)

	!Closest point interpolation
	do i = 0, Nx
		if(xc < x(i)) then
			xc = x(i)
			exit
		endif
	enddo

	write(*,'(A, f20.4)') 'Closest point in the mesh to xc:', xc

	!Parabola external velocity
	ue = (u_out - u_bl) * (x**2.d0 - 2.d0 * xc * x) / (1.d0 - 2.d0 * xc) + u_bl


end subroutine External_Velocity
!========================================================================================!
!                                      End of program
!========================================================================================!
end program Navier_Stokes_2D_Boundary_Layer
