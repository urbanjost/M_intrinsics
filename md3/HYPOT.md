## hypot

### **Name**

**hypot**(3) - \[MATHEMATICS\] Returns the Euclidean distance - the distance between a point and the origin.

### **Synopsis**
```fortran
    result = hypot(x, y)
```
```fortran
     elemental real(kind=KIND) function hypot(x,y)

      real(kind=KIND),intent(in) :: x
      real(kind=KIND),intent(in) :: y
```
### **Characteristics**

 - **x,y** and the result shall all be _real_ and of the same **kind**.

### **Description**

In mathematics, the _Euclidean distance_ between two points in Euclidean
space is the length of a line segment between two points.

**hypot(x,y)** returns the special case of the Euclidean distance between
the point **<x,y>** and the origin. It is equal to
```fortran
sqrt(x**2+y**2)
```
without undue underflow or overflow.

### **Options**

- **x**
: the x value of the point of interest

- **y**
: the y value of the point of interest

### **Result**

The result is the positive magnitude of the distance of the point
**<x,y>** from the origin **<0.0,0.0>** .

### **Examples**

Sample program:
```text
   program demo_hypot
   use, intrinsic :: iso_fortran_env, only : real32, real64, real128
   implicit none
   real(kind=real32)             :: x, y
   real(kind=real32),allocatable :: xs(:), ys(:)
   integer                       :: i
   character(len=*),parameter    :: f='(a,/,SP,*(3x,g0,1x,g0:,/))'

     ! basics
      write(*,*)hypot(3.0,4.0)
      write(*,*)hypot(1.0,0.25)
      write(*,*)hypot(1.0,0.5)

      x=3.0
      y=4.0
      ! all equivalent
      write(*,*)sqrt(x**2+y**2), hypot(x,y), abs(cmplx(x,y))

      ! a common use is to determine the distance of a point
      ! from the origin
      x = 1.e0_real32
      y = 0.5e0_real32

      write(*,*)
      write(*,'(*(g0))')'point <',x,',',y,'> is ',hypot(x,y)
      write(*,'(*(g0))')'units away from the origin'
      write(*,*)

     ! elemental
      xs=[  x,  x**2,  x*10.0,  x*15.0, -x**2  ]
      ys=[  y,  y**2, -y*20.0,  y**2,   -y**2  ]

      write(*,f)"the points",(xs(i),ys(i),i=1,size(xs))
      write(*,f)"have distances from the origin of ",hypot(xs,ys)
      write(*,f)"the closest is",minval(hypot(xs,ys))

   ! Finding primitive Euclidean triple sets, which are pairs of whole
   ! numbers that form the sides of a right triangle with a hypotenuse
   ! whose length is also a whole number (like [3,4,5]).
   !
   EUCLIDEAN: block
   ! Euclid's formula is a fundamental formula for generating Pythagorean
   ! triples given an arbitrary pair of integers m and n with m > n > 0.
   ! The formula states that the integers
   !
   !    a = m**2 − n**2
   !    b = 2*m*n
   !    c = m**2 + n**2
   !
   ! form a Pythagorean triple.
      integer :: i,j
      real    :: m,n, a,b,c
      integer,parameter :: maxside=100
      ! find all primitive Euclidean triplets with sides a and b <= maxside
      do i=1,maxside
         do j=1,maxside
            m=i
            n=j
            ! skip values unless m > 2
            if(m.le.n)cycle
            a=m**2-n**2
            b=2*m*n
            c=m**2+n**2
            ! skip writing it if it is not a primitive Euclidean triplet
            if (gcd_vector(nint([a,b,c])) > 1)cycle
            if(a>maxside.or.b>maxside)cycle
            ! c should be hypot(a,b) or equivalently abs(cmplx(a,b))
            write(*,*) a, b, c, hypot(a,b), c==hypot(a,b)
         enddo
      enddo
   endblock EUCLIDEAN
   contains
   function gcd(m,n) result(answer) ! greatest common denominator
   integer,intent(in) :: m, n
   integer            :: answer
   integer            :: irest
   intrinsic          :: mod,abs
   integer            :: ifirst
      ifirst=abs(m)
      answer=abs(n)
      if(answer.eq.0)then
         answer=ifirst
      else
         do
            irest = mod(ifirst,answer)
            if(irest == 0)  exit
            ifirst = answer
            answer = irest
         enddo
         answer= iabs(answer)
      endif
   end function gcd
   integer function gcd_vector(m)
   integer,intent(in) :: m(:)
   integer            :: vsize
   integer            :: i
      vsize=size(m)
      if(vsize.gt.0)then
         gcd_vector = m(1)
         TILLONE: do i=1,vsize
            gcd_vector = gcd(gcd_vector,iabs(m(i)))
            if (gcd_vector.eq.1) exit TILLONE
         enddo TILLONE
      else
         gcd_vector=0
      endif
   end function gcd_vector

   end program demo_hypot
```
Results:
```text
    >    5.00000000
    >    1.03077638
    >    1.11803401
    >    5.00000000       5.00000000       5.00000000
    >
    > point <1.00000000,0.500000000> is 1.11803401
    > units away from the origin
    >
    > the points
    >    +1.00000000 +0.500000000
    >    +1.00000000 +0.250000000
    >    +10.0000000 -10.0000000
    >    +15.0000000 +0.250000000
    >    -1.00000000 -0.250000000
    > have distances from the origin of
    >    +1.11803401 +1.03077638
    >    +14.1421356 +15.0020828
    >    +1.03077638
    > the closest is
    >    +1.03077638
    >    3.00000000     4.00000000     5.00000000     5.00000000 T
    >    5.00000000     12.0000000     13.0000000     13.0000000 T
    >    15.0000000     8.00000000     17.0000000     17.0000000 T
    >    7.00000000     24.0000000     25.0000000     25.0000000 T
    >    21.0000000     20.0000000     29.0000000     29.0000000 T
    >    9.00000000     40.0000000     41.0000000     41.0000000 T
    >    35.0000000     12.0000000     37.0000000     37.0000000 T
    >    11.0000000     60.0000000     61.0000000     61.0000000 T
    >    45.0000000     28.0000000     53.0000000     53.0000000 T
    >    33.0000000     56.0000000     65.0000000     65.0000000 T
    >    13.0000000     84.0000000     85.0000000     85.0000000 T
    >    63.0000000     16.0000000     65.0000000     65.0000000 T
    >    55.0000000     48.0000000     73.0000000     73.0000000 T
    >    39.0000000     80.0000000     89.0000000     89.0000000 T
    >    77.0000000     36.0000000     85.0000000     85.0000000 T
    >    65.0000000     72.0000000     97.0000000     97.0000000 T
    >    99.0000000     20.0000000     101.000000     101.000000 T
    >    91.0000000     60.0000000     109.000000     109.000000 T
```
### **Standard**

Fortran 2008

### **See also**

 - [exp(3)](#exp)     -  Base-e exponential function
 - [gamma(3)](#gamma) -  Gamma function, which yields factorials for positive whole numbers
 - [log(3)](#log)     -  Natural logarithm
 - [log10(3)](#log10) -  Base 10 or common logarithm
 - [log_gamma(3)](#log_gamma) -  Logarithm of the absolute value of the Gamma function

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
