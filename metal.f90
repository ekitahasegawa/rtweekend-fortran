submodule (hittables) metal
    implicit none
    contains
    
    module procedure metal_scatter
        type(vec3) :: reflected
        
        reflected = reflect(.unit.r_in.direction(),rec%normal)
        scattered = ray(rec%p, reflected)
        attenuation = this%albedo
        metal_scatter = (scattered%direction().dot.rec%normal).gt.0.0d0
    end procedure metal_scatter
    
    module procedure init_metal_default
        init_metal%albedo = vec3()
    end procedure init_metal_default
    
    module procedure init_metal
        init_metal%albedo = c
    end procedure init_metal
end submodule metal