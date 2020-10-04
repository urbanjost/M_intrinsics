     program memoryloss
     implicit none
     real, pointer, dimension(:,:) :: p1
     character(len=80), pointer    :: c1
     character(len=1)              :: paws
        write(*,*)'look at memory use before allocation'; read(*,*)paws
        allocate ( p1(5000,5000), c1)
	p1=1.0
        write(*,*)'look at memory use after allocation'; read(*,*)paws
	write(*,*)sum(p1)
        c1 = 'example   '
        nullify(p1,c1) ! NO!: last name to reference memory is being lost
        write(*,*)'look at memory use after nullify'; read(*,*)paws
     end program memoryloss
