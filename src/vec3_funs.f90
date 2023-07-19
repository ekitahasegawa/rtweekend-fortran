submodule(vec3_mod) vec3_funs
   implicit none

   contains

   pure module function unit_vector(v)
      type(vec3), intent(IN) :: v
      type(vec3) :: unit_vector

      unit_vector = vec3(v%e / norm2(v%e))
   end function unit_vector

   pure module function cross_product(z,w)
      type(vec3), intent(IN) :: z,w
      type(vec3) :: cross_product

      associate(u=>z%e, v=>w%e)
         cross_product = vec3([ &
            u(2)*v(3) - u(3)*v(2),  &
            u(3)*v(1) - u(1)*v(3),  &
            u(1)*v(2) - u(2)*v(1)   &
         ])
      end associate
   end function cross_product

   pure module function vec_plus_vec(u,v)
      type(vec3), intent(IN) :: u,v
      type(vec3) :: vec_plus_vec

      vec_plus_vec = vec3(u%e + v%e)
   end function vec_plus_vec

   pure module function vec_plus_real(u,t)
      type(vec3), intent(IN) :: u
      real, value, intent(IN) :: t
      type(vec3) :: vec_plus_real

      vec_plus_real = vec3(u%e + t)
   end function vec_plus_real

   pure module function vec_times_vec(u,v)
      type(vec3), intent(IN) :: u,v
      type(vec3) :: vec_times_vec

      vec_times_vec = vec3(u%e * v%e)
   end function vec_times_vec

   pure module function vec_times_real(u,t)
      type(vec3), intent(IN) :: u
      real, value, intent(IN) :: t
      type(vec3) :: vec_times_real

      vec_times_real = vec3(u%e * t)
   end function vec_times_real

   pure module function vec_div_real(u,t)
      type(vec3), intent(IN) :: u
      real, value, intent(IN) :: t
      type(vec3) :: vec_div_real

      vec_div_real = vec3(u%e * t)
   end function vec_div_real
end submodule vec3_funs