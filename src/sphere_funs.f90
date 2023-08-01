submodule(sphere_mod) sphere_funs
   implicit none

   contains

   module function hit_sphere(this,r_in,tmin,tmax,rec) result(was_hit)
      use ray_mod
      class(sphere), intent(IN) :: this
      type(ray), intent(IN) :: r_in
      real(rk), intent(INOUT) :: tmin,tmax
      type(hit_record), intent(INOUT) :: rec
      logical :: was_hit

      type(vec3) :: oc, outward_normal
      real(rk) :: a, half_b, c, discriminant, root

      was_hit = .false.

      oc = r_in%origin - this%center
      a = r_in%direction.dot.r_in%direction
      half_b = oc.dot.r_in%direction
      c = (oc.dot.oc) - (this%radius**2)
      discriminant = (half_b**2) - (a*c)

      if (discriminant.lt.0) return

      root = (-half_b - sqrt(discriminant)) / a

      if ((root.lt.tmin).or.(root.gt.tmax)) then
         root = (-half_b + sqrt(discriminant)) / a
         if ((root.lt.tmin).or.(root.gt.tmax)) then
            return
         end if
      end if

      rec%t = root
      rec%p = r_in.at.(root)
      outward_normal = (rec%p - this%center) / this%radius

      call rec%set_face_normal(r_in, outward_normal)

      was_hit = .true.
      return
   end function hit_sphere
end submodule sphere_funs