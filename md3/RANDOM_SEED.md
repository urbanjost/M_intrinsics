## random_seed

### **Name**

**random_seed**(3) - \[MATHEMATICS:RANDOM\] Initialize a pseudo-random number sequence

### **Synopsis**
```fortran
    call random_seed( [size] [,put] [,get] )
```
```fortran
     subroutine random_seed( size, put, get )

      integer,intent(out),optional :: size
      integer,intent(in),optional :: put(*)
      integer,intent(out),optional :: get(*)
```
### **Characteristics**
 - **size** a scalar default _integer_
 - **put** a rank-one default _integer_ array
 - **get** a rank-one default _integer_ array
 - the result

### **Description**

**random_seed**(3) restarts or queries the state of the pseudorandom
number generator used by random_number.

If random_seed is called without arguments, it is seeded with random
data retrieved from the operating system.

### **Options**

- **size**
  : specifies the minimum size of the arrays used with the **put**
  and **get** arguments.

- **put**
  : the size of the array must be larger than or equal to the number
  returned by the **size** argument.

- **get**
  : It is **intent(out)** and the size of the array must be larger than
  or equal to the number returned by the **size** argument.

### **Examples**

Sample program:

```fortran
program demo_random_seed
   implicit none
   integer, allocatable :: seed(:),initial_seed(:)
   integer :: i,j,n
   real :: x(3)
   call random_seed() ! set random seed if f2023

   call random_seed(size = n)
   allocate(seed(n))
   call random_seed(get=seed)
   initial_seed=seed

   write (*, *) 'queried initial seed=',seed
   write (*,*) 'get three sets of random numbers'
   do i=1,3
      call random_number(x)
      write(*,*)x
   enddo

   ! now randomize the seed several times, query
   ! and print it, and then generate an array of PRN
   do i=1,3
      call random_seed() ! randomize seed if f2023
      call random_seed(get=seed)
      write (*, *) 'new seed=',seed
      call random_number(x)
      write(*,*)'set with new seed=',x
   enddo

   ! now go back to initial seed and should reproduce
   ! initial set
   write(*,*)'back to initial'
   call random_seed(put=initial_seed)

   ! repeat first display
   call random_seed(get=seed)
   write (*, *) 'queried current seed=',seed
   write (*,*) 'get three sets of random numbers,'
   write (*,*) 'should be duplicates of first set'
   do i=1,3
      call random_number(x)
      write(*,*)x
   enddo
end program demo_random_seed
```
Results:
```text
 > get three sets of random numbers
 > 0.728326082      0.733394623      0.807955265
 > 0.827496469      0.709796131      0.855553031
 > 0.850020826       2.29641199E-02  0.848301649
 > new seed=   532671634  1589724431  -702344385  -267089641 
 >            -2127795903 1724481233 -1649777043  -673546294
 > set with new seed=  0.182621956  0.814420581  0.161144853
 > new seed=  -588602105 -1330109958 -1909200428  2013740993 
 >             926921249 132638128 -1513550047 -1366162835
 > set with new seed=  0.922367275  0.226936579  0.626253545
 > new seed=   701608307 -1105671482   804077484    93609417 
 <            -2030753861 94338487 -1850184744   417402487
 > set with new seed=  0.119894266  0.515091896  0.171295166
 > back to initial
 > queried current seed=    -2447268 -1354129540   374710663 
 >   -1240608696 -1827405339  -257388164  2056470833   269047911
 > get three sets of random numbers, 
 > should be duplicates of first set
 > 0.728326082      0.733394623      0.807955265
 > 0.827496469      0.709796131      0.855553031
 > 0.850020826       2.29641199E-02  0.848301649
```
### **Standard**

Fortran 95

### **See Also**

[**random_number**(3)](#random_number),
[**random_init**(3)](#random_init)

 _Fortran intrinsic descriptions_
