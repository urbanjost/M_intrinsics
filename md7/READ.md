## read

### **Name**
   read(7f) - [FORTRAN:IO] read data
   

### **Synopsis**
```fortran
```
### **Description**
### **Options**
### **Examples**
  Sample:
```fortran
   program testit
   use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
   implicit none
   character(len=:),allocatable :: line
   character(len=*),parameter   :: gen='(*(g0))'
   integer                      :: ichars=0, ilines=0, ilength=0, ios
      open(unit=stdin,pad='no')
      READFILE: do
         call getl(line,ios)
         if(ios.ne.0)exit READFILE
         ilines=ilines+1
         ilength=len(line)
         ichars=ichars+ilength
         write(*,'(i9,i9,i9,"[",a,"]")')ichars,ilines,ilength,line
      enddo READFILE
      write(*,gen)'CHARS+LINES=',ichars+ilines
   contains
   subroutine getl(line,ios)
   use,intrinsic :: iso_fortran_env, only : iostat_eor, iostat_end, &
    & stderr=>error_unit
   character(len=:),intent(out),allocatable :: line
   integer,intent(out) :: ios
   character :: a*1,msg*256
      line=''
      READLINE: do
         read(stdin,advance='no',iostat=ios,fmt='(a)',iomsg=msg) a
         select case(ios)
         case(IOSTAT_END);              exit READLINE
         case(IOSTAT_EOR); ios=0;       exit READLINE
         case(0);          line=line//a
         case default
            write(stderr,gen)'LINE ',ilines,' ERROR:',trim(msg)
            exit READLINE
         end select
      enddo READLINE
   end subroutine getl
   end program testit
```
### **See Also**

[**backspace**(3)](#backspace),
[**close**(3)](#close),
[**endfile**(3)](#endfile),
[**flush**(3)](#flush),
[**inquire**(3)](#inquire),
[**open**(3)](#open),
[**print**(3)](#print),
[**read**(3)](#read),
[**rewind**(3)](#rewind),
[**wait**(3)](#wait),
[**write**(3)](#write)
