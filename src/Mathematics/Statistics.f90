module Statistics

implicit none

contains

!========================================================================================!
!                                       Linear fit
!========================================================================================!
! From:          Dr. David G. Simpson
!                Department of Physical Science
!                Prince George's Community College
!                Largo, Maryland  20774
!
!  Date:         January 21, 2002
!
! Adapted by:    Enrique Millan Valbuena
!                Aerospace Engineering Department,
!                Monash University
!
!  Date:         September 26, 2017
!


subroutine Statistics_Linear_Fit(x, y, slope, y_intercept, r)

    real, intent(in)  :: x(1:), y(1:)         ! Data points to correlate
    real, intent(out) :: slope                ! Slope of least-squares best fit line
    real, intent(out) :: y_intercept          ! y-intercept of least-squares best fit line
    real, intent(out) :: r                    ! Squared correlation coefficient

    ! Local variables
    integer :: i
    integer :: N                              ! Number of data points

    real ::  sumx  = 0.0d0                    ! Sum of x
    real ::  sumx2 = 0.0d0                    ! Sum of x**2
    real ::  sumxy = 0.0d0                    ! Sum of x * y
    real ::  sumy  = 0.0d0                    ! Sum of y
    real ::  sumy2 = 0.0d0                    ! Sum of y**2

    ! Extract size
    N = size(x)
    
    ! Init
    sumx  = sum(x)
    sumx2 = sum(x**2)
    sumxy = sum(x * y)
    sumy  = sum(y)
    sumy2 = sum(y**2)

    ! Compute slope
    slope = (N * sumxy  -  sumx * sumy) / (N * sumx2 - sumx**2)
    
    ! Compute y-intercept  
    y_intercept = (sumy * sumx2  -  sumx * sumxy) / (N * sumx2  -  sumx**2)
    
    ! Compute regression coefficient
    r = (sumxy - sumx * sumy / N) / sqrt((sumx2 - sumx**2/N) * (sumy2 - sumy**2 / N))

end subroutine Statistics_Linear_Fit

end module Statistics