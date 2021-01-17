!========================================================================================!
!                               Partial_Differential_Operators
! Enrique Millan Valbuena ==================================================== 02.10.2017!

module Partial_Differential_Operators

use Finite_Difference_Formulation

implicit none

    integer, public :: Degree

contains

!========================================================================================!
!                                       Partial Derivative
!========================================================================================!
subroutine Partial_Derivative(f, x, y, alpha, df)
    integer, intent(in)    :: alpha(1:)
    real,    intent(in)    :: f(0:, 0:)
    real,    intent(in)    :: x(0:), y(0:)
    real,    intent(inout) :: df(0:, 0:)

    !Local variables
    integer :: i, j
    integer :: Nx, Ny
    integer :: s
    integer, allocatable :: Seeds(:)

    real, allocatable :: Weights(:,:,:)
    real, allocatable :: df_aux(:,:)

    !Extract sizes
    !Nx: number of intervals in the x-direction
    !Ny: number of intervals in the y-direction
    Nx = size(x) - 1
    Ny = size(y) - 1
    !Note: realize how all the vectors are allocated from 0 to a given number.
    !      This is the reason why we have substracted one before-hand.

!============================= If alpha == 0 in one index ===============================!

    !y - direction
    if (alpha(1) == 0) then

        !Obtains the derivation seeds/weights
        allocate(Seeds(0:Ny), Weights(1, 0:Degree, 0:Ny))

        call Finite_Difference_Formulas(Order       = Degree,  &
                                        Nodes       = y,       &
                                        Derivatives = (/ alpha(2) /), &
                                        Seeds       = Seeds,   &
                                        Weights     = Weights  )

        !Computes the spatial derivatives
        do i = 0, Ny
          s = Seeds(i)
          forall(j = 0:Nx) df(j,i) = sum(Weights(1,:,i) * f(j, s:s + Degree))
        enddo

        deallocate(Seeds, Weights)

        return

    end if

    !y - direction
    if (alpha(2) == 0) then

        !Obtains the derivation seeds/weights
        allocate(Seeds(0:Nx), Weights(1, 0:Degree, 0:Nx))

        call Finite_Difference_Formulas(Order       = Degree,  &
                                        Nodes       = x,       &
                                        Derivatives = (/ alpha(1) /), &
                                        Seeds       = Seeds,   &
                                        Weights     = Weights  )

        !Computes the spatial derivatives
        do i = 0, Nx
          s = Seeds(i)
          forall(j = 0:Ny) df(i, j) = sum(Weights(1,:,i) * f(s:s+Degree, j))
        enddo

        deallocate(Seeds, Weights)

        return

    end if

!============================= If alpha != 0 in any index ===============================!

    allocate(df_aux(0:Nx, 0:Ny))
    df_aux = 0.0

!=============================== x - direction ==========================================!
    !Obtains the derivation seeds/weights
    allocate(Seeds(0:Nx), Weights(1, 0:Degree, 0:Nx))

    call Finite_Difference_Formulas(Order       = Degree,  &
                                    Nodes       = x,       &
                                    Derivatives = (/ alpha(1) /), &
                                    Seeds       = Seeds,   &
                                    Weights     = Weights  )

    !Computes the spatial derivatives
    do i = 0, Nx
      s = Seeds(i)
      forall(j = 0:Ny) df_aux(i,j) = sum(Weights(1,:,i) * f(s:s+Degree, j))
    enddo

    deallocate(Seeds, Weights)

!=============================== y - direction ==========================================!
    !Obtains the derivation seeds/weights
    allocate(Seeds(0:Ny), Weights(1,0:Degree,0:Ny))

    call Finite_Difference_Formulas(Order       = Degree,  &
                                    Nodes       = y,       &
                                    Derivatives = (/ alpha(2) /), &
                                    Seeds       = Seeds,   &
                                    Weights     = Weights  )

    !Computes the spatial derivatives
    do i = 0, Ny
      s = Seeds(i)
      forall(j = 0:Nx) df(j,i) = sum(Weights(1,:,i) * df_aux(j, s:s + Degree))
    enddo

    deallocate(Seeds, Weights)

end subroutine Partial_Derivative

!========================================================================================!
!                                           Laplacian
!========================================================================================!
subroutine Laplacian(f, x, y, nabla)
    real, intent(in)    :: f(0:, 0:)
    real, intent(in)    :: x(0:), y(0:)
    real, intent(inout) :: nabla(0:, 0:)

    !Local variables
    integer           :: Nx, Ny
    real, allocatable :: df(:, :)

    !Extract sizes
    Nx = size(x) - 1
    Ny = size(y) - 1

    allocate(df(0:Nx, 0:Ny))

    ! Init
    df    = 0.0
    nabla = 0.0

    ! Compute f_xx
    call Partial_Derivative(f, x, y, (/ 2, 0 /), df)
    nabla = nabla + df

    df = 0.0

    ! Compute f_yy
    call Partial_Derivative(f, x, y, (/ 0, 2 /), df)

    ! Update nabla operator
    nabla = nabla + df

    deallocate(df)

end subroutine Laplacian

!========================================================================================!
!                                           Biharmonic
!========================================================================================!
subroutine Biharmonic(f, x, y, nabla2)
    real, intent(in)    :: f(0:, 0:)
    real, intent(in)    :: x(0:), y(0:)
    real, intent(inout) :: nabla2(0:, 0:)

    !Local variables
    integer           :: Nx, Ny
    real, allocatable :: df(:, :)

    !Extract sizes
    Nx = size(x) - 1
    Ny = size(y) - 1

    allocate(df(0:Nx, 0:Ny))

    df     = 0.0
    nabla2 = 0.0

    call Partial_Derivative(f, x, y, (/4, 0/), df)
        nabla2 = nabla2 + df

    call Partial_Derivative(f, x, y, (/0, 4/), df)
        nabla2 = nabla2 + df

    call Partial_Derivative(f, x, y, (/2, 2/), df)
        nabla2 = nabla2 + 2.0 * df

    deallocate(df)

end subroutine Biharmonic
!
! !========================================================================================!
! !                                         Convective term
! !========================================================================================!
! subroutine Convection(f, v, x, y, convective_term)
!
!   convective_term = 0.0
!
!   call Partial_Derivative(f, x, y, (/1, 0/), df)
!       convective_term = convective_term + v(1) * df
!
!   call Partial_Derivative(f, x, y, (/0, 1/), df)
!       convective_term = convective_term + v(2) * df
!
! end subroutine Convective_Term

!========================================================================================!
!                                   Derivative weights
!
!   Inputs:
!       - Derivative order   (INTEGER) : alpha = k
!       - Collocation points (REAL)    : x     = {x0, x1, x2, ..., xi, ..., xN}
!
!   Outputs:                                     {w0, w1, w2, ..., wq, ..., 0 } | 0
!       - Weights  (REAL)              : w     = {0 , w0, w1, ..., wq, ..., 0 } } i
!                                                {0 ,  0, w1, ..., wi, ..., wq} | N
!
!========================================================================================!
subroutine Partial_Derivative_Weights_Seeds(x, alpha, Weights, Seeds)
    integer, intent(in   )              :: alpha
    integer, intent(inout), allocatable :: Seeds(:)
    real,    intent(in   )              :: x(0:)
    real,    intent(inout), allocatable :: Weights(:,:,:)

    !Local variables
    integer :: N

    !Extract number of intervals
    N = size(x) - 1

    !Obtains the derivation seeds/weights
    allocate(Seeds(0:N), Weights(1, 0:Degree, 0:N))

    Seeds   = 0
    Weights = 0.0

    call Finite_Difference_Formulas(Order       = Degree,      &
                                    Nodes       = x,           &
                                    Derivatives = (/ alpha /), &
                                    Seeds       = Seeds,       &
                                    Weights     = Weights      )

end subroutine Partial_Derivative_Weights_Seeds

!========================================================================================!
!                                       Apply_Weights
!
!   Inputs:
!       - Derivative index   (INTEGER) : alpha = k
!       - Function values    (REAL)    : f     = {f0, f1, f2, ..., fi, ..., fN}
!       - Collocation points (REAL)    : x     = {x0, x1, x2, ..., xi, ..., xN}
!
!   Outputs:
!       - Derivative values  (REAL)    : df    = d^k {f0, f1, f2, ..., fi, ..., fN} / dx^k
!
!========================================================================================!
subroutine Apply_Weights_Direction_y(f, Weights, Seeds, df)
    integer, intent(in   ) :: Seeds(0:)
    real,    intent(in   ) :: f(0:, 0:)
    real,    intent(in   ) :: Weights(:,0:,0:)
    real,    intent(inout) :: df(0:,0:)

    !Local variables
    integer :: i, j
    integer :: Nx, Ny, s

    !Extract number of intervals
    Nx = size(f,2) - 1
    Ny = size(f,1) - 1

    df = 0.0

    !Computes the spatial derivatives
    do i = 0, Ny
        s = Seeds(i)
        forall(j = 0:Nx) df(i,j) = sum(Weights(1,:,i) * f(s:s + Degree, j))
    enddo

end subroutine Apply_Weights_Direction_y

subroutine Apply_Weights_Direction_x(f, Weights, Seeds, df)
    integer, intent(in   ) :: Seeds(0:)
    real,    intent(in   ) :: f(0:, 0:)
    real,    intent(in   ) :: Weights(:,0:,0:)
    real,    intent(inout) :: df(0:,0:)

    !Local variables
    integer :: i, j
    integer :: Nx, Ny, s

    !Extract number of intervals
    Nx = size(f,2) - 1
    Ny = size(f,1) - 1

    df = 0.0

    !Computes the spatial derivatives
    do i = 0, Nx
      s = Seeds(i)
      forall(j = 0:Ny) df(j,i) = sum(Weights(1,:,i) * f(j, s:s+Degree))
    enddo

end subroutine Apply_Weights_Direction_x

!========================================================================================!
!                                       End of module
!========================================================================================!

end module Partial_Differential_Operators
