module vec3_mod
   !use rtweekend, only : rk
   use iso_fortran_env, only : real32, real64
   implicit none
   integer, parameter :: rk=real32

   type vec3
      real(real64), dimension(3) :: e
   end type vec3

   interface operator(+)
      pure module function vec_plus_vec(u,v)
         type(vec3), intent(IN) :: u,v
         type(vec3) :: vec_plus_vec
      end function vec_plus_vec

      pure module function vec_plus_real(u,t)
         type(vec3), intent(IN) :: u
         real, value, intent(IN) :: t
         type(vec3) :: vec_plus_real
      end function vec_plus_real
   end interface

   interface operator(*)
      pure module function vec_times_vec(u,v)
         type(vec3), intent(IN) :: u,v
         type(vec3) :: vec_times_vec
      end function vec_times_vec

      pure module function vec_times_real(u,t)
         type(vec3), intent(IN) :: u
         real, value, intent(IN) :: t
         type(vec3) :: vec_times_real
      end function vec_times_real
   end interface

   interface operator(/)
      pure module function vec_div_real(u,t)
         type(vec3), intent(IN) :: u
         real, value, intent(IN) :: t
         type(vec3) :: vec_div_real
      end function vec_div_real
   end interface

   interface operator(.cross.)
      pure module function cross_product(z,w)
         type(vec3), intent(IN) :: z,w
         type(vec3) :: cross_product
      end function cross_product
   end interface

   interface operator(.unit.)
      pure module function unit_vector(v)
         type(vec3), intent(IN) :: v
         type(vec3) :: unit_vector
      end function unit_vector
   end interface

end module vec3_mod