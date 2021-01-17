!========================================================================================!
!
!                                     Blasius Boundary Layer
!
!========================================================================================!
module Blasius_Boundary_Layer

use Piecewise_Polynomial_Interpolation


private
public :: Blasius_Boundary_Layer_Solution

contains
!========================================================================================!
!
!                               Blasius Boundary Layer Solution
!
!========================================================================================!
subroutine Blasius_Boundary_Layer_Solution(y, Ue, Re, psi, u, v)
    real, intent(in ) :: y(0:), Ue, Re
    real, intent(out) :: psi(0:), u(0:), v(0:)

    !Local variables
    integer           :: Ny, num_steps

    real              :: x0, eta_e, x_probe
    real, allocatable :: f(:)
    real, allocatable :: eta(:), eta_y(:), f_eta(:), df_eta(:), f_y(:,:), df_y(:,:)

    !Solve f''' + f*f' = 0, for f(eta), eta in [0, laaaarge]

    !Array definitions:
    !f(1) = f''
    !f(2) = f'
    !f(3) = f

    !========================================================================================!
    !                                     Compute domain values
    !========================================================================================!

    !Extract sizes
    Ny = size(y) - 1

    !Init
    allocate(eta_y (0:Ny),  &
             f_y   (0:Ny, 1), &
             df_y  (0:Ny, 1))

    eta_y = 0.0
    f_y   = 0.0
    df_y  = 0.0

    !Self-similar variable at infinity
    x0 = 10.0**(-2.0) / Re

    eta_e = maxval(y) * sqrt(2.0 * Re * Ue * x0) / (2.0 * Re * x0)

    eta_y = y * sqrt(2.0 * Re * Ue * x0) / (2.0 * Re * x0)

    write(*, '(/, A, /)') 'BLASIUS BOUNDARY LAYER SOLVER ...'
    write(*, '(A, e20.5)') 'x0 = ', x0
    write(*, '(A, e20.5)') 'eta_e = ', eta_e
    write(*, '(A, e20.5, /)') 'sqrt(Ue/2Rex0) = ', sqrt(2.0 * Re * Ue * x0) / (2.0 * Re * x0)

    !Integration variables
    ne = 3 !number of equations

    allocate(f(ne))

    !Init
    f = 0.0
    num_steps = 2000

    allocate(eta   (0:num_steps), &
             f_eta (0:num_steps), &
             df_eta(0:num_steps))

    eta    = 0.0
    f_eta  = 0.0
    df_eta = 0.0

    !Load integration interval
    do j = 0, num_steps
        eta(j) = real(j) * (eta_e / real(num_steps))
    enddo

    !Integrate boundary layer
    call Secant_Method(eta, f)

    write(*, *) 'Blasius boundary layer solved. Check convergence'

    !Time integration with RK4 scheme + output

    f_eta(0)  = f(3)
    df_eta(0) = f(2)

    do j = 1, num_steps

        call rk4(ne, eta(j), eta_e / real(num_steps), f)

        f_eta(j)  = f(3)
        df_eta(j) = f(2)

    enddo

    !========================================================================================!
    !                               Interpolate solution to mesh
    !========================================================================================!


    !Linear interpolation

    call Interpolate(Polynomial_Degree    = 1,       &
                     Grid_Nodes           = eta,     &
                     Nodal_Values         = f_eta,   &
                     Interpolation_Points = eta_y,   &
                     Derivatives          = (/ 0 /), &
                     Interpolated_Values  = f_y)

    call Interpolate(Polynomial_Degree    = 1,  &
                     Grid_Nodes           = eta,     &
                     Nodal_Values         = df_eta,  &
                     Interpolation_Points = eta_y,   &
                     Derivatives          = (/ 0 /), &
                     Interpolated_Values  = df_y)

    !========================================================================================!
    !                               Physical variables
    !========================================================================================!

    x_probe = 10.0 * Re

    psi = sqrt(2.0 * Re * Ue * x0)        *           f_y(:,1)
    u   =                              Ue *          df_y(:,1)
    v   = sqrt(Re * Ue / (2.0 * x_probe)) * (eta_y * df_y(:,1) - f_y(:, 1))

    write(*, '(A, e10.2, A)') 'Distance x = ', x_probe, ' for v velocity.'

    deallocate(eta,   f_eta, df_eta, &
               eta_y, f_y,   df_y)

end subroutine Blasius_Boundary_Layer_Solution
!========================================================================================!
!
!                               Blasius Boundary Layer Solution
!
!========================================================================================!
subroutine Secant_Method(eta, f)
    real, intent(in)    :: eta(:)
    real, intent(inout) :: f(:)

    !Local variables
    integer :: i, j, num_steps, ne
    integer :: maximum_iterations

    real    :: h, a, b, y2a, y3a, y2b
    real    :: A1, A2, B1, B2, guess
    real    :: tolerance

    !Init
    f = 0.0

    ne = size(f)

    !Geometry definition
    num_steps = size(eta)      ! Number of steps

    a = 0.0                    ! Domain start
    b = maxval(eta)            ! Domain end

    h = (b-a) / real(num_steps) ! Step size

    !Tolerance
    maximum_iterations = 1000

    tolerance = 10.0**(-8.0)

    write(*, '(A, e10.3)') 'Step size = ', h
    write(*, '(A, e10.3)') 'Tolerance = ', tolerance

    !Boundary conditions
    y2a = 0.0
    y3a = 0.0
    y2b = 1.0

    !Initial two guesses for y(1) at x=0
    A1 = 1.0
    A2 = 0.5

    !========================================================================================!
    !                                     First guess
    !========================================================================================!
    !Initial condition
    f(1) = A1  !guess
    f(2) = y2a !bc
    f(3) = y3a !bc

    !Time integration with RK4 scheme to find B
    do j = 1, num_steps
        call rk4(ne, eta(j), h, f)
    enddo

    !assign estimated value for y(2) at x=10
    B1 = f(2)

    !========================================================================================!
    !                                     Second guess
    !========================================================================================!
    !Initial condition
    f(1) = A2  !Guess
    f(2) = y2a !BC
    f(3) = y3a !BC

    !Time integration with RK4 scheme
    do j = 1, num_steps
        call rk4(ne, eta(j), h, f)
    enddo

    !Assign estimated value for y(2) at x=10
    B2 = f(2)

    !========================================================================================!
    !                                     Secant iterations
    !========================================================================================!
    !It will converge quickly
    do i = 1, maximum_iterations

        !Initial condition
        guess = A2 + (y2b - B2) / ((B2 - B1) / (A2 - A1)) !Secant method

        f(1) = guess !guess
        f(2) = y2a   !bc
        f(3) = y3a   !bc

        !Time integration with RK4 scheme
        do j = 1, num_steps
            call rk4(ne, eta(j), h, f)
        end do

        !Update for the next iteration
        B1 = B2
        B2 = f(2)
        A1 = A2
        A2 = guess

        !Remove comment if you want to see the iterations
        !write(*,'(A, I3, A, f20.16)') 'i = ', i, ' f''(infinity) = ', B2

        !Check for the final point
        if(abs(B2 - y2b) .le. tolerance) then

            f(1) = guess !guess
            f(2) = y2a   !bc
            f(3) = y3a   !bc

            exit

        endif

    enddo

    write(*, '(A, f10.5)') 'f   (0) = ', f(3)
    write(*, '(A, f10.5)') 'f_x (0) = ', f(2)
    write(*, '(A, f10.5)') 'f_xx(0) = ', f(1)


end subroutine Secant_Method

!-----------------------------------------------------------------------------!
!RK4 scheme
!-----------------------------------------------------------------------------!
subroutine rk4(ne,x,h,y)
    integer, intent(in   ) :: ne
    real,    intent(in   ) :: x, h
    real,    intent(inout) :: y(ne)

    !Local variables
    integer :: i
    real    :: r(ne)
    real    :: k1(ne), k2(ne), k3(ne), k4(ne)

    call RHS(ne = ne, &
             y  = y , &
             x  = x , &
             r  = r   )

    !Update coefficients
    k1 = h * r

    call RHS(ne = ne,            &
             y  = y + k1 / 2.0,  &
             x  = x +  h / 2.0,  &
             r  = r               )

    !Update coefficients
    k2 = h * r

    call RHS(ne = ne,            &
             y  = y + k2 / 2.0,  &
             x  = x +  h / 2.0,  &
             r  = r               )

    !Update coefficients
    k3 = h * r

    call RHS(ne = ne,  &
         y  = y + k3,  &
         x  = x +  h,  &
         r  = r        )

    !Update coefficients
    k4 = h * r

    y = y + (k1 + 2.0 * (k2 + k3) + k4) / 6.0


end subroutine rk4

!-----------------------------------------------------------------------------!
!Right Hand Side (RHS) : Blasius problem
!-----------------------------------------------------------------------------!
subroutine RHS(ne, y, x, r)
    integer, intent(in ) :: ne
    real,    intent(in ) :: x, y(ne)
    real,    intent(out) :: r(ne)

    !Solve y''' + y*y' = 0, for y(x), x in [0,10]

    !Array definitions:
    !y(1) = y''
    !y(2) = y'
    !y(3) = y

    r(1) = -y(1)*y(3)
    r(2) =  y(1)
    r(3) =  y(2)

end subroutine RHS
end module Blasius_Boundary_Layer
