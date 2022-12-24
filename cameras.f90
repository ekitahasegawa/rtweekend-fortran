module cameras
    use, intrinsic :: iso_fortran_env, only : real64
    use vectors
    implicit none
    
    type camera
        private
        type(vec3) :: origin,lower_left_corner,horizontal,vertical
    contains
        procedure, public :: get_ray
    end type camera
    
    interface camera
        module procedure init_camera_default
    end interface camera
    
    contains
    
    pure function init_camera_default() result(cam)
        type(camera) :: cam
        real(kind=real64), parameter :: aspect_ratio=16.0d0/9.0d0,viewport_height=2.0d0,viewport_width=aspect_ratio*viewport_height,focal_length=1.0d0
        
        cam%origin = vec3(0.0d0,0.0d0,0.0d0)
        cam%horizontal = vec3(viewport_width,0.0d0,0.0d0)
        cam%vertical = vec3(0.0d0,viewport_height,0.0d0)
        cam%lower_left_corner = cam%origin - (cam%horizontal/2.0d0) - (cam%vertical/2.0d0) - vec3(0.0d0,0.0d0,focal_length)
    end function init_camera_default
    
    pure function get_ray(this,u,v)
        use rays, only : ray
        class(camera), intent(IN) :: this
        real(kind=real64), intent(IN) :: u,v
        type(ray) :: get_ray
        get_ray = ray(this%origin, this%lower_left_corner + (u*this%horizontal) + (v*this%vertical) - this%origin)
    end function get_ray
end module cameras