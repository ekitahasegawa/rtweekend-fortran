module hittable_mod
   use rtweekend_mod, only : rk
   use vec3_mod
   implicit none

   type hit_record
      type(vec3) :: p,normal
      real(rk) :: t
      logical :: front_face

      contains
      procedure, pass :: set_face_normal
   end type hit_record

   type, abstract :: hittable

      contains
      procedure (hit_query), deferred :: hit
   end type hittable

   abstract interface
      logical function hit_query(this,r_in, tmin, tmax, rec)
         use ray_mod, only : ray
         import hittable
         import hit_record, rk
         class(hittable), intent(IN) :: this
         type(ray), intent(IN) :: r_in
         real(rk), intent(INOUT) :: tmin,tmax
         type(hit_record), intent(INOUT) :: rec
      end function hit_query
   end interface

   interface
      module subroutine set_face_normal(this, r_in, outward_normal)
         use ray_mod
         class(hit_record), intent(INOUT) :: this
         type(ray), intent(IN) :: r_in
         type(vec3), intent(IN) :: outward_normal
      end subroutine set_face_normal
   end interface
end module hittable_mod