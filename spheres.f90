submodule (hittables) spheres
    use, intrinsic :: iso_fortran_env, only : real64
    implicit none
    
    contains
    
    module function hit_sphere(this,r,t_min,t_max,rec)
        class(sphere), intent(IN) :: this
        type(ray), intent(IN) :: r
        real(kind=real64), intent(IN) :: t_min,t_max
        type(hit_record), intent(INOUT) :: rec
        logical :: hit_sphere
            
        type(vec3) :: oc,outward_normal
        real(kind=real64) :: a,half_b,c,discriminant,root,sqrtd
            
        oc = r%origin() - this%c
        a = r%direction().dot.r%direction()
        half_b = oc.dot.r%direction()
        c = oc%length_squared() - this%r**2
        discriminant = half_b**2 - a*c
            
        if(discriminant.lt.0) then
            hit_sphere = .false.
            return
        endif
        
        sqrtd = sqrt(discriminant)
        
        root = (-half_b - sqrtd) / a
        
        if((root.lt.t_min).or.(root.gt.t_max)) then
            root = (-half_b + sqrtd) / a
            if((root.lt.t_min).or.(root.gt.t_max)) then
                hit_sphere = .false.
                return
            endif
        endif
        
        rec%t = root
        rec%p = r%at(rec%t)
        outward_normal = (rec%p - this%c) / this%r
        call rec%set_face_normal(r,outward_normal)
        hit_sphere = .true.
    end function hit_sphere
    
    pure module function init_sphere_default()
        type(sphere) :: init_sphere_default
        init_sphere_default%r = 0.0d0
        init_sphere_default%c = vec3(0.0d0,0.0d0,0.0d0)
    end function init_sphere_default
    
    pure module function init_sphere(cen,r)
        type(vec3), intent(IN) :: cen
        real(kind=real64), intent(IN) :: r
        type(sphere) :: init_sphere
        init_sphere%c = cen
        init_sphere%r = r
    end function init_sphere
    
    pure module function radius(this)
        class(sphere), intent(IN) :: this
        real(kind=real64) :: radius
        radius = this%r
    end function radius
    
    pure module function center(this)
        class(sphere), intent(IN) :: this
        type(vec3) :: center
        center = this%c
    end function center
end submodule spheres