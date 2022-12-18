submodule (vectors) vector_operators
    use, intrinsic :: iso_fortran_env, only : real64
    implicit none
    contains
    
    !First the addition functions
    pure module elemental function vector_plus_vector(v1,v2) result(v3)
        type(vec3), intent(IN) :: v1,v2
        type(vec3) :: v3
        v3%e = v1%e + v2%e
    end function vector_plus_vector
    
    pure module elemental function vector_plus_scalar(v1,t) result(v2)
        type(vec3), intent(IN) :: v1
        real(kind=real64), intent(IN) :: t
        type(vec3) :: v2
        v2%e = v1%e + t
    end function vector_plus_scalar
    
    pure module elemental function scalar_plus_vector(t,v1) result(v2)
        type(vec3), intent(IN) :: v1
        real(kind=real64), intent(IN) :: t
        type(vec3) :: v2
        v2%e = v1%e + t
    end function scalar_plus_vector
    
    !Next the subtraction functions
    pure module elemental function vector_minus_vector(v1,v2) result(v3)
        type(vec3), intent(IN) :: v1,v2
        type(vec3) :: v3
        v3%e = v1%e - v2%e
    end function vector_minus_vector
    
    pure module elemental function vector_minus_scalar(v1,t) result(v2)
        type(vec3), intent(IN) :: v1
        real(kind=real64), intent(IN) :: t
        type(vec3) :: v2
        v2%e = v1%e - t
    end function vector_minus_scalar
    
    pure module elemental function scalar_minus_vector(t,v1) result(v2)
        type(vec3), intent(IN) :: v1
        real(kind=real64), intent(IN) :: t
        type(vec3) :: v2
        v2%e = v1%e - t
    end function scalar_minus_vector
    
    pure module elemental function vector_negative(v1) result(v2)
        type(vec3), intent(IN) :: v1
        type(vec3) :: v2
        v2%e = -v1%e
    end function vector_negative
    
    !Next multiplication
    pure module elemental function vector_times_scalar(v1,t) result(v2)
        type(vec3), intent(IN) :: v1
        real(kind=real64), intent(IN) :: t
        type(vec3) :: v2
        v2%e = v1%e * t
    end function vector_times_scalar
    
    pure module elemental function scalar_times_vector(t,v1) result(v2)
        type(vec3), intent(IN) :: v1
        real(kind=real64), intent(IN) :: t
        type(vec3) :: v2
        v2%e = v1%e * t
    end function scalar_times_vector
    
    !Next division
    pure module elemental function vector_div_scalar(v1,t) result(v2)
        type(vec3), intent(IN) :: v1
        real(kind=real64), intent(IN) :: t
        type(vec3) :: v2
        v2%e = v1%e/t
    end function vector_div_scalar
    
    !Next the assignment functions
    pure module elemental subroutine vector_assign_vector(v1,v2)
        type(vec3), intent(OUT) :: v1
        type(vec3), intent(IN) :: v2
        v1%e = v2%e
    end subroutine vector_assign_vector
    
    pure module elemental subroutine vector_assign_real(v1,t)
        type(vec3), intent(OUT) :: v1
        real(kind=real64), intent(IN) :: t
        v1%e = t
    end subroutine vector_assign_real
end submodule vector_operators