program main
   use rtweekend_mod, only : rk, convert_to_unsigned
   use iso_fortran_env, only : int8,int32,int64,real32,real64
   use vec3_mod
   use color_mod, only : write_ppm_binary, write_ppm_ascii
   use ray_mod, only : ray
   implicit none

   integer, parameter :: default_image_width = 256, default_image_height = 256, max_pixel_val = 255
   real(rk), parameter :: default_aspect_ratio = 16.0_rk/9.0_rk
   character(len=*), parameter :: filename="image.ppm"

   integer :: ii, jj, image_width, image_height, ir, ig, ib, arg_count
   integer(int32), dimension(:,:,:), allocatable :: pixel_field
   type(vec3), dimension(:,:), allocatable :: vectors
   real(rk) :: r,g,b
   character(len=256) :: arg

   real(rk) :: start_time, stop_time

   !Camera variables
   real(rk) :: viewport_height, viewport_width, focal_length
   type(vec3) :: origin, horizontal, vertical, lower_left_corner
   real(rk) :: s,t

   type(ray) :: camera_ray
   type(vec3) :: pixel_color

   arg_count = command_argument_count()

   if(arg_count.eq.0) then
      image_width = default_image_width
      image_height = default_image_height
   else
      call get_command_argument(1,arg)
      read(arg,*) image_width
      if(arg_count.gt.1) then
         call get_command_argument(2,arg)
         read(arg,*) image_height
      else
         image_height = nint(image_width / default_aspect_ratio)
      end if
   end if

   !Setting default viewport parameters
   viewport_height = 2.0_rk
   viewport_width = default_aspect_ratio * viewport_height
   focal_length = 1.0_rk
   origin = vec3([0.0_rk, 0.0_rk, 0.0_rk])
   horizontal = vec3([viewport_width, 0.0_rk, 0.0_rk])
   vertical = vec3([0.0_rk, viewport_height, 0.0_rk])
   lower_left_corner = origin - (horizontal/2.0_rk) - (vertical/2.0_rk) - vec3([0.0_rk, 0.0_rk, focal_length])


   print"(A,I0,A,I0)", "Resolution: ", image_width, "x", image_height

   allocate(pixel_field(3,image_width,image_height))
   allocate(vectors(image_width,image_height))

   call cpu_time(start_time)

   do jj=1,image_height
      do ii=1,image_width
         s = real(ii-1,kind=rk) / (image_width - 1)
         t = real(jj-1,kind=rk) / (image_height - 1)

         camera_ray = ray(origin, &
            lower_left_corner + (s*horizontal) + (1.0_rk-t)*vertical - origin)

         pixel_color = ray_color(camera_ray)
         ! pixel_color = ray_color_default(s,t)

         vectors(ii,jj) = 255.999_rk * pixel_color
      end do
   end do

   call cpu_time(stop_time)

   print*, "Iterations Done. Total Time: ", stop_time - start_time

   ! call write_ppm_ascii("ascii_image.ppm",pixel_field)
   ! call write_ppm_binary("binary_image.ppm",pixel_field)
   call write_ppm_binary("binary_image.ppm",vectors)
   !call write_ppm_ascii("ascii_image.ppm",vectors)

   contains

   pure function hit_sphere(center, radius, ray_in)
      type(vec3), intent(IN) :: center
      real(rk), intent(IN) :: radius
      type(ray), intent(IN) :: ray_in
      logical :: hit_sphere

      type(vec3) :: oc
      real(rk) :: a,b,c,discriminant

      oc = ray_in%origin - center
      a = ray_in%direction.dot.ray_in%direction
      b = 2.0_rk * oc.dot.ray_in%direction
      c = (oc.dot.oc) - (radius**2.0_rk)
      discriminant = (b**2.0_rk) - (4.0_rk*a*c)
      hit_sphere = (discriminant.gt.0)
   end function hit_sphere

   pure function ray_color(r_in)
      use ray_mod, only : ray
      type(ray), intent(IN) :: r_in
      type(vec3) :: ray_color

      type(vec3) :: unit_direction
      real(rk) :: a

      if(hit_sphere(vec3([0.0_rk, 0.0_rk, -1.0_rk]),0.5_rk,r_in)) then
         ray_color = vec3([1.0_rk, 0.0_rk, 0.0_rk])
         return
      end if

      unit_direction = .unit.(r_in%direction)
      a = 0.5_rk*(unit_direction%e(2) + 1.0_rk)

      ray_color = (1.0_rk - a)*vec3([1.0_rk, 1.0_rk, 1.0_rk]) + &
         a*vec3([0.5_rk, 0.7_rk, 1.0_rk])
   end function ray_color

   pure function ray_color_default(s_in,t_in)
      real(rk), intent(IN) :: s_in,t_in
      type(vec3) :: ray_color_default

      ray_color_default = vec3([s_in,t_in,0.25_rk])

   end function ray_color_default
end program main