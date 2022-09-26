## num_images

### **Name**

**num_images**(3) - \[COLLECTIVE\] Number of images

### **Synopsis**
```fortran
    result = num_images(distance, failed)
```
```fortran
     integer function num_images (distance, failed)

      integer(kind=KIND),intent(in),optional :: distance
      integer(kind=KIND),intent(in),optional :: failed
```
### **Description**

Returns the number of images.

### **Options**

- **distance**
  : Nonnegative scalar integer

- **failed**
  : Scalar logical expression

### **Result**

  The number of images.

  If **distance** is not present or has value 0, the number of images
  in the current team is returned.

  For values smaller or equal distance to the initial team, it returns
  the number of images index on the ancestor team which has a distance
  of **distance** from the invoking team.

  If **distance** is larger than the distance to the initial team,
  the number of images of the initial team is returned.

  If **failed** is not present the total number of images is returned;
  if it has the value _.true._, the number of failed images is returned,
  otherwise, the number of images which do have not the failed status.

### **Examples**

Sample program:

```fortran
program demo_num_images
implicit none
integer :: value[*]
integer :: i

   value = this_image()
   sync all
   if (this_image() == 1) then
     do i = 1, num_images()
       write(*,'(2(a,i0))') 'value[', i, '] is ', value[i]
     end do
   endif

end program demo_num_images
```

### **Standard**

Fortran 2008 and later. With DISTANCE or FAILED argument, TS 18508 or later

### **See Also**

[**this_image**(3)](#this_image),
[**image_index**(3)](#this_index)

 _fortran-lang intrinsic descriptions_
