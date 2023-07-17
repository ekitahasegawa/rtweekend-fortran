module rtweekend
   use iso_fortran_env, only : int8,int32,int64
   implicit none
   integer, parameter :: rk = selected_real_kind(15)
   character, parameter :: newln = new_line("A")

   contains

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