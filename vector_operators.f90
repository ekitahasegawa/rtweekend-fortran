submodule (vectors) vector_operators
    use, intrinsic :: iso_fortran_env, only : real64
    implicit none
    contains
    
    !First the addition functions
    module procedure vector_plus_vector
        v3%e = v1%e + v2%e
    end procedure vector_plus_vector
    
    module procedure vector_plus_scalar
        v2%e = v1%e + t
    end procedure vector_plus_scalar
    
    module procedure scalar_plus_vector
        v2%e = v1%e + t
    end procedure scalar_plus_vector
    
    !Next the subtraction functions
    module procedure vector_minus_vector
        v3%e = v1%e - v2%e
    end procedure vector_minus_vector
    
    module procedure vector_minus_scalar
        v2%e = v1%e - t
    end procedure vector_minus_scalar
    
    module procedure scalar_minus_vector
        v2%e = v1%e - t
    end procedure scalar_minus_vector
    
    module procedure vector_negative
        v2%e = -v1%e
    end procedure vector_negative
    
    !Next multiplication
    module procedure vector_times_scalar
        v2%e = v1%e * t
    end procedure vector_times_scalar
    
    module procedure scalar_times_vector
        v2%e = v1%e * t
    end procedure scalar_times_vector
    
    !Next division
    module procedure vector_div_scalar
        v2%e = v1%e/t
    end procedure vector_div_scalar
    
    !Next the assignment functions
    module procedure vector_assign_vector
        v1%e = v2%e
    end procedure vector_assign_vector
    
    module procedure vector_assign_real
        v1%e = t
    end procedure vector_assign_real
end submodule vector_operators