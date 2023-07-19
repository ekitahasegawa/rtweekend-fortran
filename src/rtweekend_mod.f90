module rtweekend_mod
   use iso_fortran_env, only : int8,int32,real64
   implicit none
   integer, parameter :: rk = real64
   character, parameter :: newln = new_line("A")

   interface
      module elemental function convert_to_unsigned(n) result(n_u)
         integer, value :: n
         integer(int8) :: n_u
      end function convert_to_unsigned
   end interface 

end module rtweekend_mod