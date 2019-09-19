program p96ex33
implicit none 
real(16) g, g1x0, xm, xm1, e0, x0, e 
integer m 
logical(1) L
    
    e= 0.0000001
    x0= 4.7
    g1x0= sin(x0)
    xm= x0
    L= .true.
    m= 1
    do while(L)
        g= sin(xm) - (xm * cos(xm))
        xm1= xm - (g / g1x0)
        e0= abs(xm1 - xm)
        
        write(*,*) e0, m
        
        L= e0 .GT. e
        
        if(.NOT. L) then
            write(*,*) 'e0', e0
            write(*,*) 'm', m
            write(*,*) 'xm1', xm1
            g= sin(xm1) - (xm1 * cos(xm1))
            write(*,*) 'g', g
        end if
        xm= xm1
        m= m + 1
    end do
end program p96ex33
    