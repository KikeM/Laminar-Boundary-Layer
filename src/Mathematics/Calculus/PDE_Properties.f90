module PDE_Properties

implicit none

type Boundary_Conditions
    real, allocatable :: Left  (:)
    real, allocatable :: Upper (:)
    real, allocatable :: Right (:)
    real, allocatable :: Bottom(:)
end type Boundary_Conditions

type Unidimensional_Velocity_Field
    real, allocatable :: U(:)
    real, allocatable :: V(:)
    real, allocatable :: Streamfunction(:)
end type Unidimensional_Velocity_Field

type Bidimensional_Velocity_Field
    real, allocatable :: U(:,:)
    real, allocatable :: V(:,:)
end type Bidimensional_Velocity_Field

type Point
	real :: x
	real :: y
end type Point

type Boundary_Element
	real :: x
	real :: y
	real :: nx
	real :: ny
	real :: length
	real :: psi, psi_analytical
	real :: psi_n, psi_analytical_n
end type Boundary_Element


end module PDE_Properties
