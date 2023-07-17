program main
   use rtweekend, only : rk, convert_to_unsigned, newln
   use iso_fortran_env, only : int8,int64
   implicit none

   integer, parameter :: image_width = 256, image_height=256, max_pixel_val = 255
   character(len=*), parameter :: filename="image.ppm"
   integer :: lun, ii, jj
   integer(int8), dimension(:,:,:), allocatable :: pixel_field

   integer :: ir,ig,ib
   real(rk) :: r,g,b

   allocate(pixel_field(3,image_width,image_height))

   do jj=1,image_height
      do ii=1,image_width
         r = real((ii-1),kind=rk)/(image_width)
         g = real((jj-1),kind=rk)/(image_height)
         b = 0.5_rk

         ir = int(256.0_rk * r)
         ig = int(256.0_rk * g)
         ib = int(256.0_rk * b)

         pixel_field(:,ii,jj) = convert_to_unsigned([ir,ig,ib])
      end do
   end do

   open(newunit=lun,file=filename,status="replace",action="write",&
   form="formatted")

   write(lun,"(A)") "P6"
   write(lun,"(I0,X,I0)") image_width, image_height
   write(lun,"(A)") "255"
   close(lun)

   open(newunit=lun,file=filename,status="old",action="write",&
   form="unformatted",position="append")

   write(lun) pixel_field
   close(lun)
end program main