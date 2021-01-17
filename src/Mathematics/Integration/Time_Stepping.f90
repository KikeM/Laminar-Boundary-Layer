!========================================================================================!
!
! 									Time Stepping Methods
!
! Enrique Millan Valbuena ==================================================== 17.10.2017!
module Time_Stepping

implicit none

contains
!========================================================================================!
!
! 										Euler
!
!========================================================================================!
subroutine Euler(t, h, y, RHS)
	real, intent(in   ) :: t, h
	real, intent(inout) :: y(:)

	interface
		subroutine RHS(t, y, Equations)
			real, intent(in ) :: t, y(:)
			real, intent(out) :: Equations(:)
		end subroutine
	end interface

	!Local variables
	integer :: number_of_equations
	real, allocatable :: ODE_System(:)

	number_of_equations = size(y)

	allocate(ODE_System(1:number_of_equations))

	ODE_System = 0.0

	!First step
	call RHS(t         = t,         &
	         y         = y,         &
	         Equations = ODE_System )

	!Reconstruct solution
    y = y + h * ODE_System

end subroutine Euler
!========================================================================================!
!
! 										Runge Kutta 4
!
!========================================================================================!
subroutine Runge_Kutta_4(t, h, y, RHS)
	real, intent(in   ) :: t, h
	real, intent(inout) :: y(:)

	interface
		subroutine RHS(t, y, Equations)
			real, intent(in ) :: t, y(:)
			real, intent(out) :: Equations(:)
		end subroutine
	end interface

	!Local variables
	integer :: number_of_equations
	real, allocatable :: ODE_System(:), k(:)

	number_of_equations = size(y)

	allocate(k1(1:4), k2(1:4), k3(1:4), k4(1:4))
	allocate(ODE_System(1:number_of_equations))

	k1 = 0.0
	k2 = 0.0
	k3 = 0.0
	k4 = 0.0

	ODE_System = 0.0

	!First step
	call RHS(t         = t,         &
	         y         = y,         &
	         Equations = ODE_System )

	k1 = h * ODE_System

	!Second step
	call RHS(t         = t + h  / 2.0, &
	         y         = y + k1 / 2.0, &
	         Equations = ODE_System    )

	k2 = h * ODE_System

	!Third step
	call RHS(t         = t + h  / 2.0, &
	         y         = y + k2 / 2.0, &
	         Equations = ODE_System    )

	k3 = h * ODE_System

	!Fourth step
	call RHS(t         = t +  h,    &
	         y         = y + k3,    &
	         Equations = ODE_System )

	k4 = h * ODE_System

	!Reconstruct solution
    y = y + (k1 + 2.0 * (k2 + k3) + k4) / 6.0

end subroutine Runge_Kutta_4
!========================================================================================!
!
! 										End of module
!
!========================================================================================!
end module Time_Stepping
