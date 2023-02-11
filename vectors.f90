module vectors
    use, intrinsic :: iso_fortran_env, only : real64
    implicit none
    
    type vec3
        private
        real(real64), dimension(3) :: e
    contains
        private
        procedure, public :: x,y,z,val,length_squared
    end type vec3
    
    interface vec3
        module procedure vec_init_default,vec_init_r8,vec_init_vec
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
            real(real64), intent(IN) :: t
            type(vec3) :: v2
        end function vector_plus_scalar
          pure module elemental function scalar_plus_vector(t,v1) result(v2)
            type(vec3), intent(IN) :: v1
            real(real64), intent(IN) :: t
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
            real(real64), intent(IN) :: t
            type(vec3) :: v2
        end function vector_minus_scalar
    
        pure module elemental function scalar_minus_vector(t,v1) result(v2)
            type(vec3), intent(IN) :: v1
            real(real64), intent(IN) :: t
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
            real(real64), intent(IN) :: t
            type(vec3) :: v2
        end function vector_times_scalar
    
        pure module elemental function scalar_times_vector(t,v1) result(v2)
            type(vec3), intent(IN) :: v1
            real(real64), intent(IN) :: t
            type(vec3) :: v2
        end function scalar_times_vector
    end interface
    
    interface operator(/)
        pure module elemental function vector_div_scalar(v1,t) result(v2)
            type(vec3), intent(IN) :: v1
            real(real64), intent(IN) :: t
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
            real(real64), intent(IN) :: t
        end subroutine vector_assign_real
    end interface
        
    contains
        pure function x(this)
            class(vec3), intent(IN) :: this
            real(real64) :: x
            x = this%e(1)
        end function x
        
        pure function y(this)
            class(vec3), intent(IN) :: this
            real(real64) :: y
            y = this%e(2)
        end function y
        
        pure function z(this)
            class(vec3), intent(IN) :: this
            real(real64) :: z
            z = this%e(3)
        end function z
        
        pure function val(this)
            class(vec3), intent(IN) :: this
            real(real64), dimension(3) :: val
            val = this%e
        end function val
        
        pure function vec_init_default() result(v)
            type(vec3) :: v
            v%e = [0d0,0d0,0d0]
        end function vec_init_default
        
        pure function vec_init_r8(e1,e2,e3) result(v)
            real(real64), intent(IN) :: e1,e2,e3
            type(vec3) :: v
            v%e = [e1,e2,e3]
        end function vec_init_r8
        
        pure function vec_init_vec(w) result(v)
            real(real64), dimension(3), intent(IN) :: w
            type(vec3) :: v
            v%e = w
        end function vec_init_vec
        
        pure function length(this)
            type(vec3), intent(IN) :: this
            real(real64) :: length
            length=norm2(this%e)
        end function length
        
        pure function length_squared(this)
            class(vec3), intent(IN) :: this
            real(real64) :: length_squared
            length_squared = dot_product(this%e,this%e)
        end function length_squared
        
        subroutine write_color(color,samples_per_pixel,lun)
            use, intrinsic :: iso_fortran_env, only : output_unit
            use rtweekend, only : clamp
            type(vec3), intent(IN) :: color
            integer, intent(IN) :: samples_per_pixel
            integer, optional, intent(IN) :: lun
        
            integer :: outlun
            real(real64) :: r,g,b,scale
            real(real64), parameter :: maxclamp = nearest(1.0d0,-1.0d0)
        
            if(present(lun)) then
                outlun=lun
            else
                outlun=output_unit
            endif
            
            scale = 1.0d0/real(samples_per_pixel,kind=real64)
            r = sqrt(color%x()*scale)
            g = sqrt(color%y()*scale)
            b = sqrt(color%z()*scale)
        
            write(outlun,*) int(256*clamp(r,0.0d0,maxclamp)),int(256*clamp(g,0.0d0,maxclamp)),int(256*clamp(b,0.0d0,maxclamp))
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
            real(real64) :: r
            r = dot_product(v1%e,v2%e)
        end function vec_dot_vec
        
        pure function unit_vector(v1) result(v2)
            type(vec3), intent(IN) :: v1
            type(vec3) :: v2
            v2%e = v1%e/norm2(v1%e)
        end function unit_vector
        
        function random_in_unit_sphere()
            type(vec3) :: random_in_unit_sphere
            random_in_unit_sphere = vec3(1.0d0,1.0d0,1.0d0)
            
            do while((random_in_unit_sphere.dot.random_in_unit_sphere).gt.1.0d0)
                call random_number(random_in_unit_sphere%e)
            enddo
        end function random_in_unit_sphere
        
        function random_unit_vector()
            type(vec3) :: random_unit_vector
            random_unit_vector = .unit.(random_in_unit_sphere())
        end function random_unit_vector
        
        function random_in_hemisphere(nhat)
            type(vec3), intent(IN) :: nhat
            type(vec3) :: random_in_hemisphere
            
            random_in_hemisphere = random_in_unit_sphere()
            
            if((random_in_hemisphere.dot.nhat).lt.0.0d0) then
                random_in_hemisphere = -1.0d0 * random_in_hemisphere
            endif
        end function random_in_hemisphere
        
        function near_zero(this)
            class(vec3), intent(IN) :: this
            real(real64), parameter :: smallnum = 1d-8
            logical :: near_zero
            near_zero = all(abs(this%e).lt.smallnum)
        end function near_zero
end module vectors