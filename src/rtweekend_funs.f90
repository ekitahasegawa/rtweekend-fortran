submodule(rtweekend) rtweekend_funs
   implicit none

   contains

   module subroutine write_ppm_ascii_array(filename,data,pixel_max)
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
   end subroutine write_ppm_ascii_array

   module subroutine write_ppm_binary_array(filename,data,pixel_max)
      character(len=*), intent(IN) :: filename
      integer(int32), dimension(:,:,:), intent(IN) :: data
      integer, intent(IN), optional :: pixel_max

      integer :: lun, image_height, image_width, pmax

      integer(int8), dimension(:,:,:), allocatable :: ints

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

      allocate(ints(3,image_width,image_height))
      ints = convert_to_unsigned(data)

      !write(lun) convert_to_unsigned(data)
      write(lun) ints
      close(lun)
   end subroutine write_ppm_binary_array

   module subroutine write_ppm_binary_vec(filename,data,pixel_max)
      use vec3_mod, only : vec3
      character(len=*), intent(IN) :: filename
      type(vec3), dimension(:,:), intent(IN) :: data
      integer, intent(IN), optional :: pixel_max
      type(vec3) :: v

      integer :: lun, image_height, image_width, pmax,ii,jj

      pmax = 255
      if(present(pixel_max)) pmax = pixel_max

      image_width = size(data,1)
      image_height = size(data,2)

      open(newunit=lun,file=filename,status="replace",action="write",&
      form="formatted")

      write(lun,"(A)") "P6"
      write(lun,"(I0,X,I0)") image_width, image_height
      write(lun,"(I0)") pmax
      close(lun)

      open(newunit=lun,file=filename,status="old",action="write",&
      form="unformatted",position="append")

      do jj=1,image_height
         do ii=1,image_width
            v = data(ii,jj)
            write(lun) convert_to_unsigned(int(v%e))
         end do
      end do

      close(lun)
   end subroutine write_ppm_binary_vec

   module elemental function convert_to_unsigned(n) result(n_u)
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
end submodule rtweekend_funs