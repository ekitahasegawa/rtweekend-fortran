module ray_mod
   use rtweekend_mod, only : rk
   use vec3_mod, only : vec3
   implicit none

   type ray
      type(vec3) :: origin, direction
   end type ray

   interface operator(.at.)
      pure module function ray_at_t(r,t)
         type(ray), intent(IN) :: r
         real(rk), value, intent(IN) :: t
      end function ray_at_t
   end interface
end module ray_mod