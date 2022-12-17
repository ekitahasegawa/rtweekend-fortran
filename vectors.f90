module vectors
    use, intrinsic :: iso_fortran_env, only : real64
    implicit none
    
    type vec3
        private
        real(kind=real64), dimension(3) :: e
    contains
        procedure, public :: x,y,z
    end type vec3
    
    interface vec3
        module procedure vec_init_default,vec_init_r8
    end interface vec3
    
    interface operator(+)
        pure module elemental function vector_plus_vector(v1,v2) result(v3)
            type(vec3), intent(IN) :: v1,v2
            type(vec3) :: v3
        end function vector_plus_vector
    
        pure module elemental function vector_plus_scalar(v1,t) result(v2)
            type(vec3), intent(IN) :: v1
            real(kind=real64), intent(IN) :: t
            type(vec3) :: v2
        end function vector_plus_scalar
        end interface
    
        interface operator(-)
        pure module elemental function vector_minus_vector(v1,v2) result(v3)
            type(vec3), intent(IN) :: v1,v2
            type(vec3) :: v3
        end function vector_minus_vector
    
        pure module elemental function vector_minus_scalar(v1,t) result(v2)
            type(vec3), intent(IN) :: v1
            real(kind=real64), intent(IN) :: t
            type(vec3) :: v2
        end function vector_minus_scalar
    
        pure module elemental function vector_negative(v1) result(v2)
            type(vec3), intent(IN) :: v1
            type(vec3) :: v2
        end function vector_negative
    end interface
    
    interface operator(*)
        pure module elemental function vector_times_scalar(v1,t) result(v2)
            type(vec3), intent(IN) :: v1
            real(kind=real64), intent(IN) :: t
            type(vec3) :: v2
        end function vector_times_scalar
    end interface
    
    interface operator(/)
        pure module elemental function vector_div_scalar(v1,t) result(v2)
            type(vec3), intent(IN) :: v1
            real(kind=real64), intent(IN) :: t
            type(vec3) :: v2
        end function vector_div_scalar
    end interface
    
    interface assignment(=)
        pure module elemental subroutine vector_assign_vector(v1,v2)
            type(vec3), intent(OUT) :: v1
            type(vec3), intent(IN) :: v2
        end subroutine vector_assign_vector
    
        pure module elemental subroutine vector_assign_real(v1,t)
            type(vec3), intent(OUT) :: v1
            real(kind=real64), intent(IN) :: t
        end subroutine vector_assign_real
    end interface
        
    contains
        pure function x(this)
            class(vec3), intent(IN) :: this
            real(kind=real64) :: x
            x = this%e(1)
        end function x
        
        pure function y(this)
            class(vec3), intent(IN) :: this
            real(kind=real64) :: y
            y = this%e(2)
        end function y
        
        pure function z(this)
            class(vec3), intent(IN) :: this
            real(kind=real64) :: z
            z = this%e(3)
        end function z
        
        pure function vec_init_default() result(v)
            type(vec3) :: v
            v%e = [0d0,0d0,0d0]
        end function vec_init_default
        
        pure function vec_init_r8(e1,e2,e3) result(v)
            real(kind=real64), intent(IN) :: e1,e2,e3
            type(vec3) :: v
            v%e = [e1,e2,e3]
        end function vec_init_r8
        
        pure function length(this)
            type(vec3), intent(IN) :: this
            real(kind=real64) :: length
            length=norm2(this%e)
        end function length
        
        pure function length_squared(this)
            type(vec3), intent(IN) :: this
            real(kind=real64) :: length_squared
            length_squared = dot_product(this%e,this%e)
        end function length_squared
        
        subroutine write_color(color,lun)
            use, intrinsic :: iso_fortran_env, only : output_unit
            type(vec3), intent(IN) :: color
            integer, optional, intent(IN) :: lun
        
            integer :: outlun
        
            if(present(lun)) then
                outlun=lun
            else
                outlun=output_unit
            endif
        
            write(outlun,*) int(255.999*color%x()),int(255.999*color%y()),int(255.999*color%z())
        end subroutine write_color
end module vectors