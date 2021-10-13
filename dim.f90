program dim
    implicit none
    integer :: j,k,temp,r,c, arr(1000,1000),temp2
    read(*,*) r, c
    do j=1,r
        do k=1,c
            read(*,*) temp
            arr(j,k) = temp
        end do
        print *, "--------------"
    end do
    temp2=trace(arr,r,c)
    print *, temp2
    
    contains
    integer function trace(a,m,n)
        integer :: sum = 0, a(1000, 1000), i, m, n
        if(m<n) then
            do i=1, m
                sum = sum + a(i, i)
            end do
        else
            do i=1, n
                sum = sum + a(i,i)
            end do
        end if
        trace = sum
    end function trace
END PROGRAM dim
