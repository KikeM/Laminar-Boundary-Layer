!========================================================================================!
!
!                                   Physical Post-Processing
!
! Enrique Millan Valbuena ==================================================== 18.10.2017!
module Fields

use Partial_Differential_Operators

implicit none

contains

!========================================================================================!
!
!                                       Vorticity
!
!   Computes the vorticity field from the streamlines field.
!
!========================================================================================!
subroutine Vorticity_Field(x, y, psi, vorticity)
    real, intent(in)  :: x(0:), y(0:), psi(0:, 0:)
    real, intent(out) :: vorticity(0:, 0:)

    call Laplacian(psi, x, y, vorticity)

    vorticity = -vorticity

end subroutine Vorticity_Field

!========================================================================================!
!
!                                     Velocity field
!
!   Computes the velocity field from the streamlines field.
!
!========================================================================================!
subroutine Velocity_Field(x, y, psi, u, v)
    real, intent(in)  :: x(0:), y(0:), psi(0:, 0:)
    real, intent(out) :: u(0:, 0:), v(0:, 0:)

    ! u - velocity
    call Partial_Derivative(psi, x, y, (/ 0 , 1 /), u)
    u = u

    ! v - velocity
    call Partial_Derivative(psi, x, y, (/ 1 , 0 /), v)
    v = -v

end subroutine Velocity_Field
!========================================================================================!
!
!                                      End of module
!
!========================================================================================!

end module Fields
