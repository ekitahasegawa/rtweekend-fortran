program main
   use rtweekend, only : rk, convert_to_unsigned, &
      write_ppm_ascii, write_ppm_binary
   use iso_fortran_env, only : int8,int32,int64,real32,real64
   implicit none

   integer, parameter :: default_image_width = 256, default_image_height = 256, max_pixel_val = 255
   real(rk), parameter :: default_aspect_ratio = 16.0_rk/9.0_rk
   character(len=*), parameter :: filename="image.ppm"

   integer :: ii, jj, image_width, image_height, ir, ig, ib, arg_count
   integer(int32), dimension(:,:,:), allocatable :: pixel_field
   real(rk) :: r,g,b
   character(len=256) :: arg

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

   allocate(pixel_field(3,image_width,image_height))

   do jj=1,image_height
      do ii=1,image_width
         r = real((ii-1),kind=rk)/(image_width)
         g = real((jj-1),kind=rk)/(image_height)
         b = 0.25_rk

         ir = int(256.0_rk * r)
         ig = int(256.0_rk * g)
         ib = int(256.0_rk * b)

         pixel_field(:,ii,jj) = [ir,ig,ib]
      end do
   end do

   call write_ppm_ascii("ascii_image.ppm",pixel_field)
   call write_ppm_binary("binary_image.ppm",pixel_field)
end program main