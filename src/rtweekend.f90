module rtweekend
   use iso_fortran_env, only : int8,int32,int64
   implicit none
   integer, parameter :: rk = selected_real_kind(15)
   character, parameter :: newln = new_line("A")

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
   
   elemental function convert_to_unsigned(n) result(n_u)
      integer, value :: n
      integer(int8) :: n_u
      integer :: n_set_upper_bits

      integer :: bit_mask
      character(len=32) :: err

      bit_mask = ibits(n,bit_size(n_u),bit_size(n)-bit_size(n_u))

      n_set_upper_bits = popcnt(bit_mask)

      if(n_set_upper_bits.gt.0) then
         write(err,*) n
         error stop "ERROR: BITWISE VALUE "//trim(adjustl(err))//" CANNOT FIT IN 8 BIT UNSIGNED INT"
      end if

      n_u = iand(n,z'FF')
   end function convert_to_unsigned
end module rtweekend