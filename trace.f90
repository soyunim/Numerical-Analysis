program trace
    implicit none
    integer :: j,k,temp,m,n, a(1000,1000)
    read(*,*) m, n
    do j=1,m
        do k=1,n
            read(*,*) temp
            a(j,k) = temp
        end do
        print *, "--------------"
    end do

    print *, sum_is(a)
    
    contains
    integer function sum_is(dim)
        integer :: sum = 0, dim(1000, 1000), i
        if(m<n) then
            do i=1, m
                sum = sum + dim(i, i)
            end do
        else
            do i=1, n
                sum = sum + dim(i,i)
            end do
        end if
        sum_is = sum
    end function sum_is
END PROGRAM trace
