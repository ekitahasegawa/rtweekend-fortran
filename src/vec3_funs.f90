submodule(vec3_mod) vec3_funs
   use rtweekend_mod, only : rk
   implicit none

   contains

   pure module function unit_vector(v)
      type(vec3), intent(IN) :: v
      type(vec3) :: unit_vector

      unit_vector = vec3(v%e / norm2(v%e))
   end function unit_vector

   pure module function vec_dot_vec(u,v)
      type(vec3), intent(IN) :: u,v
      real(rk) :: vec_dot_vec

      vec_dot_vec = dot_product(u%e,v%e)
   end function vec_dot_vec

   pure module function vec_cross_vec(z,w)
      type(vec3), intent(IN) :: z,w
      type(vec3) :: vec_cross_vec

      associate(u=>z%e, v=>w%e)
         vec_cross_vec = vec3([ &
            u(2)*v(3) - u(3)*v(2),  &
            u(3)*v(1) - u(1)*v(3),  &
            u(1)*v(2) - u(2)*v(1)   &
         ])
      end associate
   end function vec_cross_vec

   pure module function vec_plus_vec(u,v)
      type(vec3), intent(IN) :: u,v
      type(vec3) :: vec_plus_vec

      vec_plus_vec = vec3(u%e + v%e)
   end function vec_plus_vec

   pure module function vec_plus_real(u,t)
      type(vec3), intent(IN) :: u
      real(rk), value, intent(IN) :: t
      type(vec3) :: vec_plus_real

      vec_plus_real = vec3(u%e + t)
   end function vec_plus_real

   pure module function real_plus_vec(t,u)
      type(vec3), intent(IN) :: u
      real(rk), value, intent(IN) :: t
      type(vec3) :: real_plus_vec

      real_plus_vec = u+t
   end function real_plus_vec

   pure module function vec_minus_vec(u,v)
      type(vec3), intent(IN) :: u,v
      type(vec3) :: vec_minus_vec

      vec_minus_vec = vec3(u%e - v%e)
   end function vec_minus_vec

   pure module function vec_minus_real(u,t)
      type(vec3), intent(IN) :: u
      real(rk), value, intent(IN) :: t
      type(vec3) :: vec_minus_real

      vec_minus_real = vec3(u%e - t)
   end function vec_minus_real

   pure module function real_minus_vec(t,u)
      type(vec3), intent(IN) :: u
      real(rk), value, intent(IN) :: t
      type(vec3) :: real_minus_vec

      real_minus_vec = u-t
   end function real_minus_vec

   pure module function negate_vec(v)
      type(vec3), intent(IN) :: v
      type(vec3) :: negate_vec

      negate_vec = vec3(-v%e)
   end function negate_vec

   pure module function vec_times_vec(u,v)
      type(vec3), intent(IN) :: u,v
      type(vec3) :: vec_times_vec

      vec_times_vec = vec3(u%e * v%e)
   end function vec_times_vec

   pure module function vec_times_real(u,t)
      type(vec3), intent(IN) :: u
      real(rk), value, intent(IN) :: t
      type(vec3) :: vec_times_real

      vec_times_real = vec3(u%e * t)
   end function vec_times_real

   pure module function real_times_vec(t,u)
      type(vec3), intent(IN) :: u
      real(rk), value, intent(IN) :: t
      type(vec3) :: real_times_vec

      real_times_vec = u*t
   end function real_times_vec

   pure module function vec_div_real(u,t)
      type(vec3), intent(IN) :: u
      real(rk), value, intent(IN) :: t
      type(vec3) :: vec_div_real

      vec_div_real = vec3(u%e / t)
   end function vec_div_real

   pure elemental module function reverse_vector(v_in)
      type(vec3), intent(IN) :: v_in
      type(vec3) :: reverse_vector

      associate(v => v_in%e)
         reverse_vector = vec3(v(3:1:-1))
      end associate
   end function reverse_vector
end submodule vec3_funs