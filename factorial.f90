program factorial
implicit none
real(16) f, pi, e, u_f
integer n, i
    
    e= 2.718281828459045
    pi= 3.141592653589793
    write(*,*) 'Add N'
    read(*,*) n
    f= sqrt(2*pi*n)*((n/e)**n)*(1+(1/(12*n))+(1/(288*n*n))-(139/(51840*n*n*n))-(571/(2488320*n*n*n*n)))
    write(*,*) 'n!~', f
    
    u_f= 1
    i= 1
    do while(i .LE. n)
        u_f= u_f * i
        i= i + 1
    end do
    write(*,*) 'n!=', u_f
    
end program factorial