## event_query

### **Name**

**event_query**(3) - \[COLLECTIVE\] Query whether a coarray event has occurred

### **Synopsis**
```fortran
    call event_query(event, count [,stat] )
```
```fortran
```
### **Characteristics**

### **Description**

**event_query**(3) assigns the number of events to **count** which have been
posted to the **event** variable and not yet been removed by calling
**event_wait**. When **stat** is present and the invocation was successful, it
is assigned the value **0**. If it is present and the invocation has failed,
it is assigned a positive value and **count** is assigned the value **-1**.

### **Options**

- **event**
  : (intent(in)) Scalar of type event_type, defined in
  iso_fortran_env; shall not be coindexed.

- **count**
  : (intent(out))Scalar integer with at least the precision of default
  _integer_.

- **stat**
  : (OPTIONAL) Scalar default-kind _integer_ variable.

### **Examples**

Sample program:

```fortran
program demo_event_query
use iso_fortran_env
implicit none
type(event_type) :: event_value_has_been_set[*]
integer :: cnt
   if (this_image() == 1) then
      call event_query(event_value_has_been_set, cnt)
      if (cnt > 0) write(*,*) "Value has been set"
   elseif (this_image() == 2) then
      event post(event_value_has_been_set[1])
   endif
end program demo_event_query
```
### **Standard**

TS 18508

### **See also**

 - [co_broadcast(3)](#co_broadcast) - Copy a value to all images the current set of images
 - [co_lbound(3)](#co_lbound) - Lower codimension bounds of an array
 - [co_max(3)](#co_max) - Maximal value on the current set of images
 - [co_min(3)](#co_min) - Minimal value on the current set of images
 - [co_reduce(3)](#co_reduce) - Reduction of values on the current set of images
 - [co_sum(3)](#co_sum) - Sum of values on the current set of images
 - [co_ubound(3)](#co_ubound) - Upper codimension bounds of an array
 - [event_query(3)](#event_query) - Query whether a coarray event has occurred
 - [image_index(3)](#image_index) - Cosubscript to image index conversion
 - [lcobound(3)](#lcobound) - Lower codimension bounds of an array
 - [num_images(3)](#num_images) - Number of images
 - [this_image(3)](#this_image) - Cosubscript index of this image
 - [ucobound(3)](#ucobound) - Upper codimension bounds of an array

 _Fortran intrinsic descriptions_
