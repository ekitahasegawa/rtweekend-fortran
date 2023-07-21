submodule (ray_mod) ray_funs
   implicit none

   contains 

   pure module function ray_at_t(r,t)
      type(ray), intent(IN) :: r
      real(rk), value, intent(IN) :: t
      type(vec3) :: ray_at_t

      ray_at_t = r%origin + (r%direction)*t
   end function ray_at_t
end submodule ray_funs