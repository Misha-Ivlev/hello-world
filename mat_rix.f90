program mat_rix
implicit none
integer(4), parameter :: N= 5
real(4)               :: r1, r2
integer(4)            :: A(1:N,1:N), i, j, min_val, min_val_i, min_val_j
    
    !i= string number
    !j= column number
    !unit fill array A with random numbers
    i= 1
    do while(i .LE. N)
        j= 1
        do while(j .LE. N)
            CALL RANDOM_NUMBER(r1)
            r1= r1 * 100
            CALL RANDOM_NUMBER(r2)
            r2= r2 * 100
            A(i,j)= nint(r1 - r2)
            j= j + 1
        end do
        i= i + 1
    end do
    
    !unit print the matrix A
    write(*,*) 'matrix A'
    i= 1
    do while(i .LE. N)
        write(*,*) (A(i,j), j= 1,N)
        i= i + 1
    end do
    
    !just indent
    write(*,*) 
    
    !block remove all numbers besides left triangle numbers
    i= 1
    do while(i .LE. N)
        j= 1
        do while(j .LE. N)
            if((i .LE. j) .OR. ((i+j) .GE. N + 1)) A(i, j)= 0
            j= j + 1
        end do
        i= i + 1
    end do
    
    !unit print the changed matrix A
    write(*,*) 'matrix A with left triangle and nothing else :-)'
    i= 1
    do while(i .LE. N)
        write(*,*) (A(i,j), j= 1,N)
        i= i + 1
    end do
    
    !find minimal value of changed matrix A
    min_val= MINVAL(A)
    
    !find indexes of minimal value of changed matrix A
    i= 1
    do while(i .LE. N)
        j= 1
        do while(j .LE. N)
            if(A(i,j) .EQ. min_val) min_val_i= i
            if(A(i,j) .EQ. min_val) min_val_j= j
            j= j + 1
        end do
        i= i + 1
    end do
    
    !just indent
    write(*,*) 
    
    !culmination!
    write(*,*) 'the minimal number of left triangle matrix A that', min_val, 'with indexes i=', min_val_i, ', j=', min_val_j
    
end program mat_rix
    
    