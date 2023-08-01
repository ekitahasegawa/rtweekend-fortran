module sphere_mod
   use hittable_mod
   use vec3_mod
   implicit none

   type, extends(hittable) :: sphere
      type(vec3) :: center
      real(rk) :: radius
   contains
      procedure, pass :: hit => hit_sphere
   end type sphere

   interface
      module function hit_sphere(this,r_in,tmin,tmax,rec) result(was_hit)
         use ray_mod, only : ray
         class(sphere), intent(IN) :: this
         type(ray), intent(IN) :: r_in
         real(rk), intent(INOUT) :: tmin,tmax
         type(hit_record), intent(INOUT) :: rec
         logical :: was_hit
      end function hit_sphere
   end interface
end module sphere_mod