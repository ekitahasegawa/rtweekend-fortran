program main
   implicit none

   integer, parameter :: rk = selected_real_kind(15)

   integer, parameter :: image_width = 256, image_height=256
   character(len=*), parameter :: filename="image.ppm"
   integer :: lun, ii, jj

   integer :: ir,ig,ib
   real :: r,g,b
   
   open(newunit=lun,file=filename)
   write(lun,"(A)") "P3"
   write(lun,"(I0,X,I0)") image_width, image_height
   write(lun,"(A)") "255"

   do ii=1,image_height
      do jj=1,image_width
         r = real(ii,kind=kind(r))/(image_width-1)
         g = real(jj,kind=kind(g))/(image_height-1)
         b = 0.25_rk

         ir = int(256.0_rk * r)
         ig = int(256.0_rk * g)
         ib = int(256.0_rk * b)

         write(lun,"(I0,X,I0,X,I0)") ir, ig, ib
      end do
   end do


   close(lun)
end program main