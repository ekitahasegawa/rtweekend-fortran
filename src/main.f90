program main
   use rtweekend, only : rk, convert_to_unsigned, newln
   use iso_fortran_env, only : int8,int32,int64,real32,real64
   implicit none

   integer, parameter :: image_width = 256, image_height=256, max_pixel_val = 255
   character(len=*), parameter :: filename="image.ppm"
   integer :: ii, jj
   integer(int32), dimension(:,:,:), allocatable :: pixel_field

   integer :: ir,ig,ib
   real(rk) :: r,g,b

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

   contains

   subroutine write_ppm_ascii(filename,data,pixel_max)
      character(len=*), intent(IN) :: filename
      integer, dimension(:,:,:), intent(IN) :: data
      integer, intent(IN), optional :: pixel_max

      integer :: lun, image_height, image_width, pmax, ii, jj

      pmax = 255
      if(present(pixel_max)) pmax = pixel_max

      open(newunit=lun,file=filename,status="replace",action="write")

      image_width = size(data,2)
      image_height = size(data,3)

      write(lun,"(A)") "P3"
      write(lun,"(I0,X,I0)") image_width, image_height
      write(lun,"(I0)") pmax

      do jj=1,image_height
         do ii=1,image_width
            write(lun,"(I0,X,I0,X,I0)") data(:,ii,jj)
         end do
      end do

      close(lun)
   end subroutine write_ppm_ascii

   subroutine write_ppm_binary(filename,data,pixel_max)
      character(len=*), intent(IN) :: filename
      integer(int32), dimension(:,:,:), intent(IN) :: data
      integer, intent(IN), optional :: pixel_max

      integer :: lun, image_height, image_width, pmax
      integer(int8), dimension(:,:,:), allocatable :: binary_data

      pmax = 255
      if(present(pixel_max)) pmax = pixel_max

      image_width = size(data,2)
      image_height = size(data,3)

      open(newunit=lun,file=filename,status="replace",action="write",&
      form="formatted")

      write(lun,"(A)") "P6"
      write(lun,"(I0,X,I0)") image_width, image_height
      write(lun,"(I0)") pmax
      close(lun)

      open(newunit=lun,file=filename,status="old",action="write",&
      form="unformatted",position="append")

      write(lun) convert_to_unsigned(data)
      close(lun)
   end subroutine write_ppm_binary
end program main