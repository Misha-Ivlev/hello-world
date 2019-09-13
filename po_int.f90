program po_int                        !rvl = radius vector length
implicit none                         !mv  = max value
real(8) x(1:6), y(1:6), rvl(1:6), mv  !mvi = max value index
integer i, mvi

    i= 1                              !unit fill the array
    do while(i .LE. 6)
        write(*,*) 'add X of point P with index', i
        read(*,*) x(i)
        write(*,*) 'add Y of point P with index', i
        read(*,*) y(i)
        i= i + 1
    end do
    
    i= 1                              !remove the points in the 1, 2 and 4 quadrants
    do while(i .LE. 6)
        if(x(i) .GT. 0) x(i)= 0
        if(y(i) .GT. 0) y(i)= 0
        i= i + 1
    end do
    
    i= 1                              !find the length radius vectors of the points
    do while(i .LE. 6)
    rvl(i)= sqrt(x(i)**2 + y(i)**2)
    i= i + 1
    end do
    
    mv= MAXVAL(rvl)                   !find the farthest from the O(0,0) point
    
    i=1                               !find the index of the farthest point                             
    do while(i .LE. 6)
        if(rvl(i) .EQ. mv) mvi= i
        i= i + 1
    end do
    
    write(*,*) 'point P with index', mvi 
    write(*,*) 'is from the O(0,0) at a distance of', mv
    
end program po_int