program pr_cor
implicit none
real(8) x,y,r,fi,pi

    pi= 3.141592653589793
    write(*,*) 'Add variable X'
    read(*,*) x
    write(*,*) 'Add variable Y'
    read(*,*) y
    r= sqrt(x**2 + y**2)
    
    if ((x .EQ. 0) .AND. (y .EQ. 0)) fi= 0
    if ((x .EQ. 0) .AND. (y .GT. 0)) fi= pi/2
    if ((x .EQ. 0) .AND. (y .LT. 0)) fi= (3*pi)/2
    if ((x .GT. 0) .AND. (y .GE. 0)) fi= ATAN(y/x)
    if ((x .GT. 0) .AND. (y .LT. 0)) fi= 2*pi + ATAN(y/x)
    if  (x .LT. 0)                   fi= pi + ATAN(y/x)
    
    write(*,*) 'r=', r, 'fi=', fi
end program pr_cor