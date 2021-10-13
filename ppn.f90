program ppn
    implicit none
    integer :: sum, temp1, r, n, num, cnt, temp2
    num = 1
    cnt = 0
    do
        sum = 0
        temp1 = num
        do while(temp1>0)
            r = mod(temp1,10)
            sum = sum*10+r
            temp1 = temp1/10
        end do
        if(sum .eq. num) then
            temp2 = prime(num)
            if(temp2 == 1) then
                cnt = cnt+1
                if(cnt == 100) then
                    exit
                end if
            end if
        end if
        num = num+1
    end do
    print *, "the 100th palindrome prime is", num

contains
integer function prime(n)
    integer :: i,n
    i = 1
    do while(i < n)
        if((mod(n,i) .eq. 0) .and. i/=1) then
            prime = 0
            exit
        else
            prime = 1
        end if
        i = i+1
    end do

end function

end program