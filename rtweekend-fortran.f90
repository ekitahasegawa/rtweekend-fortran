!****************************************************************************

    program rtweekendfortran
    use vectors
    use rays
    use hittables
    use rtweekend
    use,intrinsic :: iso_fortran_env, only : real64,output_unit

    implicit none
    
    ! Variables
    ! Constant variables
    real(kind=real64), parameter :: aspect_ratio = 16.0d0/9.0d0
    integer, parameter :: image_width = 3840, image_height = int(image_width/aspect_ratio)
    character(len=*), parameter :: filename = "image.ppm", newln = new_line('A')
    
    !World Variables
    type(hittable_list) :: world
    
    !Camera Variables
    real(kind=real64), parameter :: viewport_height = 2.0d0, viewport_width = aspect_ratio*viewport_height, focal_length=1.0d0
    type(vec3) :: world_origin,horizontal,vertical,lower_left_corner
    
    !Other variables
    integer :: i,j,filelun
    real(kind=real64) :: s,t,start_time,stop_time
    type(vec3) :: color
    type(ray) :: r

    ! Body of rtweekendfortran
    
    call world%add(sphere(vec3(0.0d0,0.0d0,-1.0d0),0.5d0))
    call world%add(sphere(vec3(0.0d0,-100.5d0,-1.0d0),100.0d0))
        
    world_origin = vec3(0.0d0,0.0d0,0.0d0)
    horizontal = vec3(viewport_width,0.0d0,0.0d0)
    vertical = vec3(0.0d0,viewport_height,0.0d0)
    lower_left_corner = world_origin - (horizontal/2.0d0) - (vertical/2.0d0) - vec3(0.0d0,0.0d0,focal_length)
    
    open(newunit=filelun,file=filename)
    write(filelun,*) "P3"//newln, image_width, " ", image_height, newln//"255"//newln
        
    do j=0,image_height-1
        write(output_unit,fmt='(A,I5,A)',advance='no') "Scanlines remaining:",image_height-j,char(13)
        call flush(output_unit)
        do i=0,image_width-1
            s = real(i,kind=real64)/(image_width-1)
            t = real(j,kind=real64)/(image_height-1)
            
            r = ray(origin=world_origin,&
                    direction=lower_left_corner + s*horizontal + (1.0d0-t)*vertical - world_origin)
            
            color = ray_color(r,world)
            call write_color(color,lun=filelun)
        enddo
    enddo
    close(filelun)
    
    contains
        function ray_color(r, world)
            use rtweekend, only : infinity
            type(ray), intent(IN) :: r
            type(hittable_list), intent(IN) :: world
            type(vec3) :: ray_color
            
            type(hit_record) :: rec
            real(kind=real64) :: t
            type(vec3) :: unit_direction
            
            if(world%hit(r,0.0d0,infinity,rec)) then
                ray_color = 0.5d0 * (rec%normal + vec3(1.0d0,1.0d0,1.0d0))
                return
            endif
            
            unit_direction = .unit.(r%direction())
            t = 0.5d0 * (unit_direction%y() + 1.0d0)
            ray_color = (1.0d0-t)*vec3(1.0d0,1.0d0,1.0d0) + t*vec3(0.5d0,0.7d0,1.0d0)
        end function ray_color
    end program rtweekendfortran
