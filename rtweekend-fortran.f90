!****************************************************************************

    program rtweekendfortran

    implicit none
    
    ! Variables
    ! Constant variables
    integer, parameter :: image_width = 256, image_height = 256, realkind=8
    character(len=*), parameter :: filename = "image.ppm", newln = new_line('A')
    
    !Other variables
    integer :: i,j,lun,ir,ig,ib
    real(realkind) :: r,g,b

    ! Body of rtweekendfortran
    open(newunit=lun,file=filename)
    write(lun,*) "P3"//newln, image_width, " ", image_height, newln//"255"//newln
        
    do j=0,image_height-1
        do i=0,image_width-1
            r = real(i,kind=realkind) / real(image_width-1,kind=realkind)
            g = real(j,kind=realkind) / real(image_height-1,kind=realkind)
            b = 0.25
            
            ir = int(255.999*r)
            ig = int(255.999*g)
            ib = int(255.999*b)
            
            write(lun,*) ir,ig,ib
        enddo
    enddo
    
    close(lun)
    end program rtweekendfortran

