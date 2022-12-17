!****************************************************************************

    program rtweekendfortran
    use vectors
    use,intrinsic :: iso_fortran_env, only : real64

    implicit none
    
    ! Variables
    ! Constant variables
    integer, parameter :: image_width = 256, image_height = 256
    character(len=*), parameter :: filename = "image.ppm", newln = new_line('A')
    
    !Other variables
    integer :: i,j,filelun,ir,ig,ib
    real(kind=real64) :: r,g,b
    
    !Vector Testing
    type(vec3) :: color,v1,v2

    ! Body of rtweekendfortran
    open(newunit=filelun,file=filename)
    write(filelun,*) "P3"//newln, image_width, " ", image_height, newln//"255"//newln
        
    do j=0,image_height-1
        do i=0,image_width-1
            color = vec3(real(i,kind=real64) / real(image_width-1,kind=real64),&
                    real(j,kind=real64) / real(image_height-1,kind=real64),&
                    0.25d0)
            call write_color(color,lun=filelun)
        enddo
    enddo
    close(filelun)
    
    end program rtweekendfortran
