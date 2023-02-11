module rays
    use, intrinsic :: iso_fortran_env, only : real64
    use vectors
    implicit none
    
    type ray
        type(vec3), private :: orig,dir
    contains
        procedure, public :: origin, direction, at
    end type ray
    
    interface ray
        module procedure ray_init
    end interface ray
    
    contains
        pure function ray_init(origin,direction)
            type(vec3), optional, intent(IN) :: origin
            type(vec3), optional, intent(IN) :: direction
            type(ray) :: ray_init
            
            if(present(origin)) then
                ray_init%orig = origin
            else
                ray_init%orig = vec3()
            endif
            
            if(present(direction)) then
                ray_init%dir = direction
            else
                ray_init%dir = vec3()
            endif
        end function ray_init
        
        pure function origin(this)
            class(ray), intent(IN) :: this
            type(vec3) :: origin
            origin = this%orig
        end function origin
        
        pure function direction(this)
            class(ray), intent(IN) :: this
            type(vec3) :: direction
            direction = this%dir
        end function direction
        
        pure function at(this,t)
            class(ray), intent(IN) :: this
            real(real64), intent(IN) :: t
            type(vec3) :: at
            at = this%orig + (t*this%dir)
        end function at
end module rays