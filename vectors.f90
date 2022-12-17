module vectors
    use, intrinsic :: iso_fortran_env, only : real64
    implicit none
    
    type vec3
        private
        real(kind=real64), dimension(3) :: e
    contains
        private
        procedure, public :: x,y,z
    end type vec3
    
    interface vec3
        module procedure vec_init_default,vec_init_r8
    end interface vec3
    
    interface write(formatted)
        module procedure writevec_form
    end interface
    
    interface operator(.cross.)
        module procedure vec_cross_vec
    end interface
    
    interface operator(.unit.)
        module procedure unit_vector
    end interface
    
    interface operator(.dot.)
        module procedure vec_dot_vec
    end interface
    
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
          pure module elemental function scalar_plus_vector(t,v1) result(v2)
            type(vec3), intent(IN) :: v1
            real(kind=real64), intent(IN) :: t
            type(vec3) :: v2
        end function scalar_plus_vector
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
    
        pure module elemental function scalar_minus_vector(t,v1) result(v2)
            type(vec3), intent(IN) :: v1
            real(kind=real64), intent(IN) :: t
            type(vec3) :: v2
        end function scalar_minus_vector
    
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
    
        pure module elemental function scalar_times_vector(t,v1) result(v2)
            type(vec3), intent(IN) :: v1
            real(kind=real64), intent(IN) :: t
            type(vec3) :: v2
        end function scalar_times_vector
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
        
        subroutine writevec_form(vec,unit,iotype,v_list,iostat,iomsg)
            class(vec3), intent(IN) :: vec
            integer, intent(IN) :: unit
            character(len=*), intent(IN) :: iotype
            integer, intent(IN) :: v_list(:)
            integer, intent(OUT) :: iostat
            character(len=*), intent(INOUT) :: iomsg
            
            if(.not.(iotype.eq."LISTDIRECTED")) then
                stop "VECTOR OUTPUT NOT DEFINED FOR I/O TYPE OTHER THAN LIST-DIRECTED"
            endif
            write(unit,*,IOSTAT=iostat) vec%e
        end subroutine writevec_form
        
        pure function vec_cross_vec(v1,v2) result(v3)
            type(vec3), intent(IN) :: v1,v2
            type(vec3) :: v3
            
            v3%e(1) = v1%e(2)*v2%e(3) - v1%e(3)*v2%e(2)
            v3%e(2) = v1%e(1)*v2%e(3) - v1%e(3)*v2%e(1)
            v3%e(3) = v1%e(1)*v2%e(2) - v1%e(2)*v2%e(1)
        end function vec_cross_vec
        
        pure function vec_dot_vec(v1,v2) result(r)
            type(vec3), intent(IN) :: v1,v2
            real(kind=real64) :: r
            r = dot_product(v1%e,v2%e)
        end function vec_dot_vec
        
        pure function unit_vector(v1) result(v2)
            type(vec3), intent(IN) :: v1
            type(vec3) :: v2
            v2%e = v1%e/norm2(v1%e)
        end function unit_vector
end module vectors