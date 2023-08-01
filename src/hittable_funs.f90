submodule (hittable_mod) hittable_funs
   implicit none

   contains

   module subroutine set_face_normal(this, r_in, outward_normal)
      use ray_mod
      use vec3_mod
      class(hit_record), intent(INOUT) :: this
      type(ray), intent(IN) :: r_in
      type(vec3), intent(IN) :: outward_normal

      this%front_face = (r_in%direction.dot.outward_normal).lt.0
      
      if(this%front_face) then
         this%normal = outward_normal
      else
         this%normal = -outward_normal
      end if
   end subroutine set_face_normal
end submodule hittable_funs