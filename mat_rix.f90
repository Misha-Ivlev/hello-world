program mat_rix
implicit none
real(16) A(1:5,1:5), min_val 
integer i, j, min_val_i, min_val_j
    
    i= 1
    do while(i .LE. 5)
        j= 1
        write(*,*) 'complete line', i, 'of the matrix A'
        do while(j .LE. 5)
            read(*,*) A(i,j)
            j= j + 1
        end do
        i= i + 1
    end do
    
   i= 1
    do while(i .LE. 5)
        j= 1
        do while(j .LE. 5)
            if((i .LE. j) .OR. ((i+j) .GE. 6)) A(i,j)= 0
            j= j + 1
           write(*,*) A(i,j)
        end do
       i= i + 1
    end do
    
    min_val= minval(A)
    
    i= 1
    do while(i .LE. 5)
        j= 1
        do while(j .LE. 5)
            if(A(i,j) .EQ. min_val) min_val_i= i
            if(A(i,j) .EQ. min_val) min_val_j= j
            j= j + 1
        end do
        i= i + 1
    end do
    write(*,*) 'element', min_val, 'with indexes (', min_val_i, ',', min_val_j, ')'
    
end program mat_rix
    
    