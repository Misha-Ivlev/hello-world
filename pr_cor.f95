program pr_cor
implicit none
real x,y,r,fi,pi
    pi= 3.141592653589793
    write(*,*) 'Add variable X'
    read(*,*) x
    write(*,*) 'Add variable Y'
    read(*,*) y
    r= sqrt(x**2 + y**2)
    if (x < 0) then
        fi= pi + atan(y/x)
    end if
    if (x == 0) then
        if (y < 0) then
            fi= (3*pi)/2
        else
            if (y == 0) then
                fi= 0
            else
                if (y > 0) then
                    fi= pi/2
                end if
            end if
        end if
    end if
    if (y >= 0) then
        fi= atan(y/x)
    else
        fi= 2*pi + atan(y/x)
    end if
    write(*,*) 'r=', r, 'fi=', fi
end program pr_cor