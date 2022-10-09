## scan

### **Name**

**scan**(3) - \[CHARACTER:SEARCH\] Scan a string for the presence of a set of characters

### **Synopsis**
```fortran
    result = scan( string, set, [,back] [,kind] )
```
```fortran
     elemental integer(kind=KIND) function scan(string,set,back,kind)

      character(len=*,kind=**),intent(in) :: string
      character(len=*,kind=**),intent(in) :: set
      logical,intent(in),optional :: back
      integer,intent(in),optional :: kind
```
### **Characteristics**

**string** and **set**  must have the same kind type parameter.

the kind of the returned value is the same as **kind** if
present. Otherwise a default _integer_ kind is returned.

### **Description**

**scan**(3) scans a **string** for any of the characters in a **set**
of characters.

If **back** is either absent or equals _\.false._, this function
returns the position of the leftmost character of **STRING** that is
in **set**. If **back** equals _\.true._, the rightmost position is
returned. If no character of **set** is found in **string**, the result
is zero.

### **Options**

- **string**
  : Shall be of type _character_.

- **set**
  : Shall be of type _character_.

- **back**
  : (Optional) shall be of type _logical_.

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Result**

The return value is of type _integer_ and of kind **kind**. If **kind** is absent,
the return value is of default integer kind.

### **Examples**

Sample program:

```fortran
program demo_scan
implicit none
   write(*,*) scan("fortran", "ao")          ! 2, found 'o'
   write(*,*) scan("fortran", "ao", .true.)  ! 6, found 'a'
   write(*,*) scan("fortran", "c++")         ! 0, found none
end program demo_scan
```

Results:

```text
              2
              6
              0
```

### **Standard**

Fortran 95 , with KIND argument - Fortran 2003

### **See Also**

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](#adjustl), [**adjustr**(3)](#adjustr), [**index**(3)](#index),
  [**scan**(3)](#scan), [**verify**(3)](#verify)

- **Nonelemental:**
  [**len_trim**(3)](#len_trim),
  [**len**(3)](#len),
  [**repeat**(3)](#repeat), [**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions_
