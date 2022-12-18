module rtweekend
    use, intrinsic :: iso_fortran_env, only : real64
    implicit none
    real(kind=real64), parameter :: pi = acos(-1.0d0), infinity=huge(infinity)
    
    contains
        pure function degrees_to_radians(deg)
            real(kind=real64), intent(IN) :: deg
            real(kind=real64) :: degrees_to_radians
            degrees_to_radians = deg * (pi/180.0d0)
        end function degrees_to_radians
end module rtweekend