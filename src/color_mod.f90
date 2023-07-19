module color_mod
   use iso_fortran_env, only : int32
   use vec3_mod, only : vec3
   implicit none

   private

   public :: write_ppm_binary, write_ppm_ascii

   interface write_ppm_binary
      module subroutine write_ppm_binary_array(filename,data,pixel_max)
         character(len=*), intent(IN) :: filename
         integer(int32), dimension(:,:,:), intent(IN) :: data
         integer, intent(IN), optional :: pixel_max
      end subroutine write_ppm_binary_array

      module subroutine write_ppm_binary_vec(filename,data,pixel_max)
         character(len=*), intent(IN) :: filename
         type(vec3), dimension(:,:), intent(IN) :: data
         integer, intent(IN), optional :: pixel_max
      end subroutine write_ppm_binary_vec
   end interface write_ppm_binary

   interface write_ppm_ascii
      module subroutine write_ppm_ascii_array(filename,data,pixel_max)
         character(len=*), intent(IN) :: filename
         integer, dimension(:,:,:), intent(IN) :: data
         integer, intent(IN), optional :: pixel_max
      end subroutine write_ppm_ascii_array
   end interface write_ppm_ascii

end module color_mod