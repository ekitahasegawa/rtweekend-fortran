!****************************************************************************

    program rtweekendfortran
    use vectors
    use rays
    use hittables
    use rtweekend
    use cameras
    use,intrinsic :: iso_fortran_env, only : real64,output_unit

    implicit none
    
    ! Variables
    ! Constant variables
    real(real64), parameter :: aspect_ratio = 16.0d0/9.0d0
    integer, parameter :: image_width = 1920, image_height = int(image_width/aspect_ratio),samples_per_pixel=100,max_depth=50
    character(len=*), parameter :: filename = "image.ppm", newln = new_line('A')
    
    !World Variables
    type(hittable_list) :: world
    
    !Camera Variables
    real(real64), parameter :: viewport_height = 2.0d0, viewport_width = aspect_ratio*viewport_height,focal_length=1.0d0
    type(vec3) :: world_origin,horizontal,vertical,lower_left_corner
    type(camera) :: cam
    
    !Other variables
    integer :: i,j,n,filelun
    real(real64) :: u,v,start_time,stop_time,rnums(2,samples_per_pixel)
    type(vec3) :: pixel_color
    type(ray) :: r

    ! Body of rtweekendfortran
    
    cam = camera()
    call random_init(.true.,.false.)    
    
    !call world%add(sphere(vec3(0.0d0,0.0d0,-1.0d0),0.5d0))
    !call world%add(sphere(vec3(0.0d0,-100.5d0,-1.0d0),100.0d0))
        
    world_origin = vec3(0.0d0,0.0d0,0.0d0)
    horizontal = vec3(viewport_width,0.0d0,0.0d0)
    vertical = vec3(0.0d0,viewport_height,0.0d0)
    lower_left_corner = world_origin - (horizontal/2.0d0) - (vertical/2.0d0) - vec3(0.0d0,0.0d0,focal_length)
    
    open(newunit=filelun,file=filename)
    write(filelun,*) "P3"//newln, image_width, " ", image_height, newln//"255"//newln
        
    do j=image_height-1,0,-1
        write(output_unit,fmt='(A,I5,A)',advance='no') "Scanlines remaining:",j,char(13)
        flush output_unit
        do i=0,image_width-1
            call random_number(rnums)
            pixel_color = vec3(0.0d0,0.0d0,0.0d0)
            do n=1,samples_per_pixel
                u = (real(i,kind=real64)+rnums(1,n))/(image_width-1)
                v = (real(j,kind=real64)+rnums(2,n))/(image_height-1)
                
                r = cam%get_ray(u,v)
            
                pixel_color = pixel_color + ray_color(r,world,max_depth)
            enddo
            call write_color(pixel_color,samples_per_pixel,lun=filelun)
        enddo
    enddo
    close(filelun)
    
    contains
        recursive function ray_color(r, world,depth)
            use rtweekend, only : infinity
            type(ray), intent(IN) :: r
            type(hittable_list), intent(IN) :: world
            integer, intent(IN) :: depth
            type(vec3) :: ray_color
            
            type(hit_record) :: rec
            real(real64) :: t
            real(real64), parameter :: smallnum=1d-4
            type(vec3) :: unit_direction,targetvec
            
            if(depth.le.0) then
                ray_color = vec3(0.0d0,0.0d0,0.0d0)
                return
            endif
            
            if(world%hit(r,smallnum,infinity,rec)) then
                targetvec = rec%p + rec%normal + random_in_hemisphere(rec%normal)
                ray_color = 0.5d0 * ray_color(ray(rec%p,targetvec - rec%p),world,depth-1)
                return
            endif
            
            unit_direction = .unit.(r%direction())
            t = 0.5d0 * (unit_direction%y() + 1.0d0)
            ray_color = (1.0d0-t)*vec3(1.0d0,1.0d0,1.0d0) + t*vec3(0.5d0,0.7d0,1.0d0)
        end function ray_color
    end program rtweekendfortran
