module vec3_mod
   use rtweekend_mod, only : rk
   implicit none

   type vec3
      real(rk), dimension(3) :: e
   end type vec3

   interface operator(+)
      pure module function vec_plus_vec(u,v)
         type(vec3), intent(IN) :: u,v
         type(vec3) :: vec_plus_vec
      end function vec_plus_vec

      pure module function vec_plus_real(u,t)
         type(vec3), intent(IN) :: u
         real(rk), value, intent(IN) :: t
         type(vec3) :: vec_plus_real
      end function vec_plus_real

      pure module function real_plus_vec(t,u)
         type(vec3), intent(IN) :: u
         real(rk), value, intent(IN) :: t
         type(vec3) :: real_plus_vec
      end function real_plus_vec
   end interface

   interface operator(-)
      pure module function vec_minus_vec(u,v)
         type(vec3), intent(IN) :: u,v
         type(vec3) :: vec_minus_vec
      end function vec_minus_vec

      pure module function vec_minus_real(u,t)
         type(vec3), intent(IN) :: u
         real(rk), value, intent(IN) :: t
         type(vec3) :: vec_minus_real
      end function vec_minus_real

      pure module function real_minus_vec(t,u)
         type(vec3), intent(IN) :: u
         real(rk), value, intent(IN) :: t
         type(vec3) :: real_minus_vec
      end function real_minus_vec
   end interface

   interface operator(*)
      pure module function vec_times_vec(u,v)
         type(vec3), intent(IN) :: u,v
         type(vec3) :: vec_times_vec
      end function vec_times_vec

      pure module function vec_times_real(u,t)
         type(vec3), intent(IN) :: u
         real(rk), value, intent(IN) :: t
         type(vec3) :: vec_times_real
      end function vec_times_real

      pure module function real_times_vec(t,u)
         type(vec3), intent(IN) :: u
         real(rk), value, intent(IN) :: t
         type(vec3) :: real_times_vec
      end function real_times_vec
   end interface

   interface operator(/)
      pure module function vec_div_real(u,t)
         type(vec3), intent(IN) :: u
         real(rk), value, intent(IN) :: t
         type(vec3) :: vec_div_real
      end function vec_div_real
   end interface

   interface operator(.dot.)
      pure module function vec_dot_vec(u,v)
         type(vec3), intent(IN) :: u,v
         real(rk) :: vec_dot_vec
      end function vec_dot_vec
   end interface

   interface operator(.cross.)
      pure module function vec_cross_vec(z,w)
         type(vec3), intent(IN) :: z,w
         type(vec3) :: vec_cross_vec
      end function vec_cross_vec
   end interface

   interface operator(.unit.)
      pure module function unit_vector(v)
         type(vec3), intent(IN) :: v
         type(vec3) :: unit_vector
      end function unit_vector
   end interface

   interface operator(.reverse.)
      pure elemental module function reverse_vector(v_in)
         type(vec3), intent(IN) :: v_in
         type(vec3) :: reverse_vector
      end function reverse_vector
   end interface

end module vec3_mod