module rtweekend
   use iso_fortran_env, only : int8,int32,int64
   use vec3_mod, only : vec3
   implicit none
   integer, parameter :: rk = selected_real_kind(15)
   character, parameter :: newln = new_line("A")

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

   interface
      module elemental function convert_to_unsigned(n) result(n_u)
         integer, value :: n
         integer(int8) :: n_u
      end function convert_to_unsigned
   end interface 

end module rtweekend