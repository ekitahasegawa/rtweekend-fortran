module hittables
    use vectors
    use rays
    use iso_fortran_env, only : real64
    implicit none
!------------------------------------------------------------------------------------------------------------------------------------------------------------------------------!
    type hit_record
        type(vec3) :: p,normal
        real(real64) :: t
        logical :: front_face
        class(material), pointer :: mat_ptr => NULL()
    contains
        procedure :: set_face_normal
    end type hit_record
!------------------------------------------------------------------------------------------------------------------------------------------------------------------------------!
    type, abstract :: hittable
    contains
        procedure (hitobj), deferred :: hit
    end type hittable
!------------------------------------------------------------------------------------------------------------------------------------------------------------------------------!
    abstract interface
        logical function hitobj(this,r,t_min,t_max,rec)
            import ray,hit_record,real64,hittable
            class(hittable), intent(IN) :: this
            type(ray), intent(IN) :: r
            real(real64), intent(IN) :: t_min,t_max
            type(hit_record), intent(INOUT) :: rec
        end function hitobj
    end interface
!------------------------------------------------------------------------------------------------------------------------------------------------------------------------------!
    type, abstract :: material
    contains
        procedure (scatter_ray), public, deferred :: scatter
    end type material
!------------------------------------------------------------------------------------------------------------------------------------------------------------------------------!
    abstract interface
        function scatter_ray(this,r_in,rec,attenuation,scattered)
            use rays, only : ray
            use vectors, only : vec3
            import material
            import hit_record
            class(material), intent(IN) :: this
            type(ray), intent(IN) :: r_in
            type(hit_record), intent(IN) :: rec
            type(vec3), intent(OUT) :: attenuation
            type(ray), intent(OUT) :: scattered
            logical :: scatter_ray
        end function scatter_ray
    end interface
!------------------------------------------------------------------------------------------------------------------------------------------------------------------------------!
    type, extends(material) :: lambertian
        type(vec3), public :: albedo
    contains
        procedure, public :: scatter=>lambertian_scatter
    end type lambertian
!------------------------------------------------------------------------------------------------------------------------------------------------------------------------------!
    interface lambertian
        module procedure :: init_lambertian_default,init_lambertian
    end interface lambertian
    
    interface
        module function lambertian_scatter(this,r_in,rec,attenuation,scattered)
            import lambertian
            class(lambertian), intent(IN) :: this
            type(ray), intent(IN) :: r_in
            type(hit_record), intent(IN) :: rec
            type(vec3), intent(OUT) :: attenuation
            type(ray), intent(OUT) :: scattered
            logical :: lambertian_scatter
        end function lambertian_scatter
    
        pure module function init_lambertian_default()
            type(lambertian) :: init_lambertian_default
        end function init_lambertian_default
        
        module function init_lambertian(c)
            type(vec3), intent(IN) :: c
            type(lambertian) init_lambertian
        end function init_lambertian
    end interface
!------------------------------------------------------------------------------------------------------------------------------------------------------------------------------!
    type, extends(material) :: metal
        type(vec3), public :: albedo
        real(real64), public :: fuzz=0.0d0
    contains
        procedure, public :: scatter=>metal_scatter
    end type metal
!------------------------------------------------------------------------------------------------------------------------------------------------------------------------------!
    interface metal
        module procedure :: init_metal_default,init_metal
    end interface metal
    
    interface
        module function metal_scatter(this,r_in,rec,attenuation,scattered)
            import metal
            class(metal), intent(IN) :: this
            type(ray), intent(IN) :: r_in
            type(hit_record), intent(IN) :: rec
            type(vec3), intent(OUT) :: attenuation
            type(ray), intent(OUT) :: scattered
            logical :: metal_scatter
        end function metal_scatter
    
        pure module function init_metal_default() result(init_metal)
            type(metal) :: init_metal
        end function init_metal_default
        
        module function init_metal(c,fuzz)
            type(vec3), intent(IN) :: c
            real(real64), optional, intent(IN) :: fuzz
            type(metal) init_metal
        end function init_metal
    end interface
!------------------------------------------------------------------------------------------------------------------------------------------------------------------------------!
    type, extends(hittable) :: sphere
        private
        type(vec3) :: c
        real(real64) :: r
        class(material), pointer :: mat_ptr => NULL()
    contains
        procedure, public :: hit=>hit_sphere
    end type sphere
!------------------------------------------------------------------------------------------------------------------------------------------------------------------------------!
    !Interfaces for porcedures in the sphere submodule
    !Constructor interface
    interface sphere
        module procedure :: init_sphere_default,init_sphere_ref
    end interface sphere
    
    !Other
    interface
        module function hit_sphere(this,r,t_min,t_max,rec)
            import sphere
            class(sphere), intent(IN) :: this
            type(ray), intent(IN) :: r
            real(real64), intent(IN) :: t_min,t_max
            type(hit_record), intent(INOUT) :: rec
            logical :: hit_sphere
        end function hit_sphere
        
        pure module function init_sphere_default() result(init_sphere)
            type(sphere) :: init_sphere
        end function init_sphere_default
    
        module function init_sphere_ptr(cen,r,mat_ptr) result(init_sphere)
            import material
            type(vec3), intent(IN) :: cen
            real(real64), intent(IN) :: r
            class(material), pointer, intent(IN) :: mat_ptr
            type(sphere) :: init_sphere
        end function init_sphere_ptr
    
        module function init_sphere_ref(cen,r,mat) result(init_sphere)
            import material
            type(vec3), intent(IN) :: cen
            real(real64), intent(IN) :: r
            class(material), target, intent(IN) :: mat
            type(sphere) :: init_sphere
        end function init_sphere_ref
    
        pure module function radius(this)
            class(sphere), intent(IN) :: this
            real(real64) :: radius
        end function radius
    
        pure module function center(this)
            class(sphere), intent(IN) :: this
            type(vec3) :: center
        end function center
    end interface
!------------------------------------------------------------------------------------------------------------------------------------------------------------------------------!
    type, private :: hittable_object
        class(hittable), allocatable :: object
    contains
        procedure, private :: assign_object
        generic, public :: assignment(=) => assign_object
    end type hittable_object
!------------------------------------------------------------------------------------------------------------------------------------------------------------------------------!
    type, extends(hittable) :: hittable_list
        private
        type(hittable_object), dimension(:), allocatable :: objects
    contains
        procedure, public :: hit=>hit_objects
        procedure, public :: add=>add_hittable
        procedure, public :: list_size
    end type hittable_list
!------------------------------------------------------------------------------------------------------------------------------------------------------------------------------!
    interface
        module function hit_objects(this,r,t_min,t_max,rec)
            import hittable_list
            class(hittable_list), intent(IN) :: this
            type(ray), intent(IN) :: r
            real(real64), intent(IN) :: t_min,t_max
            type(hit_record), intent(INOUT) :: rec
            logical :: hit_objects
        end function hit_objects
    
        pure module subroutine add_hittable(this,newobj)
            class(hittable_list), intent(INOUT) :: this
            class(hittable), intent(IN) :: newobj
        end subroutine add_hittable
    
        pure module function list_size(this)
            class(hittable_list), intent(IN) :: this
            integer :: list_size
        end function list_size
    end interface

!------------------------------------------------------------------------------------------------------------------------------------------------------------------------------!
    contains
    
    subroutine set_face_normal(this,r,outward_normal)
        class(hit_record), intent(INOUT) :: this
        type(ray), intent(IN) :: r
        type(vec3), intent(IN) :: outward_normal
        this%front_face = (r%direction().dot.outward_normal).lt.0d0
        if(this%front_face) then
            this%normal = outward_normal
        else
            this%normal = -1.0d0 * outward_normal
        endif
    end subroutine set_face_normal
    
    pure subroutine assign_object(this,obj)
        class(hittable_object), intent(INOUT) :: this
        class(hittable), intent(IN) :: obj
        
        allocate(this%object,source=obj)
    end subroutine assign_object
end module hittables