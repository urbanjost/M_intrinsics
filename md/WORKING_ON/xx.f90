        program change
        i=1
        if(increment(i).gt.10)then
           write(*,*)'IF',i
        elseif(increment(i).gt.20)then
           write(*,*)'ELSEIF A',i
        elseif(increment(i).gt.20)then
           write(*,*)'ELSEIF B',i
        elseif(increment(i).gt.20)then
           write(*,*)'ELSEIF C',i
        elseif(increment(i).gt.20)then
           write(*,*)'ELSEIF D',i
        else
           write(*,*)'ELSE',i
        endif
        contains
        function increment(i)
           write(*,*)'INCREMENT',I
           increment=i
           i=i+10
        end function increment
        end program change
