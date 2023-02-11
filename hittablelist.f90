submodule (hittables) hittablelist
    use, intrinsic :: iso_fortran_env, only : real64
    implicit none
    contains
    
    module procedure hit_objects        
        type(hit_record) :: tmp_rec
        real(real64) :: closest_so_far
        integer :: i
        
        hit_objects = .false.
        closest_so_far = t_max
        do i=1,this%list_size()
            if(this%objects(i)%object%hit(r,t_min,closest_so_far,tmp_rec)) then
                hit_objects = .true.
                closest_so_far = tmp_rec%t
                rec = tmp_rec
            endif
        enddo
    end procedure hit_objects
    
    module procedure add_hittable     
        integer :: n
        type(hittable_object), dimension(:), allocatable :: tmp
        
        if(.not.allocated(this%objects)) then
            allocate(this%objects(1))
            this%objects(1) = newobj
            return
        endif
        
        n = size(this%objects)
        allocate(tmp(n+1))
        tmp(1:n) = this%objects
        tmp(n+1) = newobj
        call move_alloc(from=tmp,to=this%objects)
    end procedure add_hittable
    
    module procedure list_size
        list_size=size(this%objects)
    end procedure list_size
end submodule hittablelist