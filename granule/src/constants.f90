! Created by  on 21.10.22.

module constants
    use, intrinsic:: iso_c_binding
    implicit none
    public
    integer(c_int) :: jdim = 42
    integer(c_int) :: idim = 24
    real(c_double), parameter ::  zero = 0.0
    real(c_double), parameter :: zero_int = 0
    integer(c_int), parameter :: mod_my_granule = 102

end module constants