program po_int                        
implicit none
integer, parameter :: N= 13                        
real(8) x(1:N)/1d0,1d0,1d0,1d0,1d0,1d0,1d0,1d0,-3d0,-7d0,-2d0,0d0,10d0/
real(8) y(1:N)/1d0,1d0,1d0,1d0,1d0,1d0,1d0,1d0,-4d0,0d0,-4d0,-5d0,-4d0/
real(8) r(1:N), mv 
integer i, mvi

    do i= 1,N
        if(x(i) .GE. 0) then 
            x(i)= 0
            y(i)= 0
        end if
        if(y(i) .GE. 0) then
            x(i)= 0
            y(i)= 0
        end if
    end do
    
    do i= 1,N   
        r(i)= sqrt(x(i)**2 + y(i)**2)
    end do
    
    mv= 0
    do i=1,N
        if (mv .LE. r(i)) then 
            mv= r(i)
            mvi= i
        end if
    end do
    
    write(*,*) 'point P with index', mvi 
    write(*,*) 'is from the O(0,0) at a distance of', mv
    
end program po_int