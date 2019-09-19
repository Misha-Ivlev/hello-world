program p96ex33
implicit none 
real(16) g, gx0, xm, xm1, e0, x0, e 
integer m 
logical(1) L
    
    e= 0.0000001
    write(*,*) e
    x0= 4.7
    gx0= sin(x0) - (x0 * cos(x0))
    
    xm= x0
    L= .true.
    m= 1
    do while(L)
        g= sin(xm) - (xm * cos(xm))
        xm1= xm - (g / gx0)
        e0= abs(xm1 - xm)
        
        write(*,*) e0, m
        
        L= e0 .GT. e
        
        if(.NOT. L) write(*,*) 'e0', e0
        if(.NOT. L) write(*,*) 'm', m
        if(.NOT. L) write(*,*) 'xm1', xm1
        if(.NOT. L) g= sin(xm1) - (xm1 * cos(xm1))
        if(.NOT. L) write(*,*) 'g', g
        
        xm= xm1
        m= m + 1
        !if(m .GT. 40) L= .false.
    end do
end program p96ex33
    