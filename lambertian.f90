submodule (hittables) lambertian
    implicit none
    contains
    
    module procedure lambertian_scatter
        type(vec3) :: scatter_direction
        
        scatter_direction = rec%normal + random_unit_vector()
        
        if(scatter_direction%near_zero()) then
            scatter_direction = rec%normal
        end if
        
        scattered = ray(rec%p, scatter_direction)
        attenuation = this%albedo
        lambertian_scatter = .true.
    end procedure lambertian_scatter
    
    module procedure init_lambertian_default
        init_lambertian_default%albedo = vec3()
    end procedure init_lambertian_default
    
    module procedure init_lambertian
        init_lambertian%albedo = c
    end procedure init_lambertian
end submodule lambertian