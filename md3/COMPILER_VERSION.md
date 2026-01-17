## compiler_version

### **Name**

**compiler_version**(3) - \[COMPILER:INQUIRY\] Compiler version string

### **Synopsis**
```fortran
    result = compiler_version()
```
```fortran
     character(len=:) function compiler_version()
```
### **Characteristics**

- The return value is a default-kind scalar _character_  with
  system-dependent length.

### **Description**

  **compiler_version**(3) returns a string containing the name and
  version of the compiler.

### **Options**

  None.

### **Result**

  The return value contains the name of the compiler and its version
  number used to compile the file containing the **compiler_version**(3)
  call.

### **Examples**

Sample program:
```fortran
program demo_compiler_version
use, intrinsic :: iso_fortran_env, only : compiler_version
implicit none
      print '(4a)', 'This file was compiled by ', compiler_version()
end program demo_compiler_version
```
Results (plain):
```text
 > This file was compiled by GCC version 10.3.0

 > This file was compiled by Intel(R) Fortran Intel(R) 64 Compiler Classic for 
 > applications running on Intel(R) 64, Version 2021.3.0 Build 20210609_000000

 > This file was compiled by nvfortran 21.5-0 LLVM
```
An extended version that wraps the version to a width of 80 columns
and attempts to show the options used one per line:
```fortran
program extended_compiler_version
implicit none
   call platform()
contains

subroutine platform()
use, intrinsic :: iso_fortran_env, only : compiler_version
use, intrinsic :: iso_fortran_env, only : compiler_options
implicit none
character(len=:),allocatable :: version, options
character(len=*),parameter   :: nl=new_line('a')
integer                      :: where, start, break, i, last, col
   version=compiler_version()//' '
   options=' '//compiler_options()
   start=1
   do 
      where=index(options(start:),' -')
      if(where.eq.0)exit
      break=where+start-1
      options(break:break)=nl
      start=where
   enddo
   if(start.eq.1)then
      do 
         where=index(options(start:),' /')
         if(where.eq.0)exit
         break=where+start-1
         options(break:break)=nl
         start=where
      enddo
   endif
   last=len_trim(version)+1
   col=0
   do i=1,len_trim(version)
    col=col+1
    if(version(i:i).eq.' ')last=i
    if(col.gt.76)then
       version(last:last)=nl
       col=0
    endif
   enddo
   print '(a,/,3x,*(a))', 'This file was compiled by :', inset(version)
   if(options.ne.'')then
      print '(*(a))', 'using the options :', inset(options)
   endif
end subroutine platform

function inset(string) result(longer)
character(len=*),intent(in)  :: string
character(len=:),allocatable :: longer
character(len=*),parameter   :: nl=new_line('a')
integer                      :: i
   longer=''
   do i=1,len(string)
      longer=longer//string(i:i)
      if(string(i:i).eq.nl)then
         longer=longer//'   '
      endif
   enddo
end function inset

end program extended_compiler_version
```
Results (fancy):
```text
 > This file was compiled by :
 >    GCC version 16.0.0 20250727 (experimental) 
 > using the options :
 >    -mtune=generic
 >    -march=x86-64
```
### **Standard**

Fortran 2008

### **See Also**

[**compiler_options**(3)](#compiler_options),
**iso_fortran_env**(7)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
