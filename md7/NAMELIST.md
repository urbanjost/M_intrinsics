## namelist

### **Name**
    NAMELIST(7f) - [FORTRAN:STATEMENT] specify a group of data to be referred to by a single name in data input/output
    
### **Synopsis**
```fortran
   NAMELIST /namelist-group-name/ namelist-group-object-list [[,] /namelist-group-name/
            namelist-group-object-list ] ...

            namelist-group-object is variable-name
```
### **Description**

   A NAMELIST statement specifies a group of named data objects, which
   may be referred to by a single name for the purpose of data transfer.

   The order in which the variables are specified in the NAMELIST
   statement determines the order in which the values appear on output.

    o The namelist-group-name shall not be a name accessed by use association.
    o A namelist-group-object shall not be an assumed-size array.
    o A namelist-group-object shall not have the PRIVATE attribute if
      the namelist-group-name has the PUBLIC attribute.

   Any namelist-group-name may occur more than once in the NAMELIST
   statements in a scoping unit. The namelist-group-object-list following
   each successive appearance of the same namelist-group-name in a
   scoping unit is treated as a continuation of the list for that
   namelist-group-name.

   A namelist group object may be a member of more than one namelist
   group.

   A namelist group object shall either be accessed by use or host
   association or shall have its type, type parameters, and shape
   specified by previous specification statements or the procedure heading
   in the same scoping unit or by the implicit typing rules in effect for
   the scoping unit. If a namelist group object is typed by the implicit
   typing rules, its appearance in any subsequent type declaration
   statement shall confirm the implied type and type parameters.

   The specification-part of a BLOCK construct shall not contain a
   NAMELIST statement.

   Why is NAMELIST not allowed in a BLOCK unit?
   Would be handy for quick writes, like list-directed output

```fortran
    block
       namelist /nlist/ a,b,c,d
       write(*,nlist)
    endblock
```

   Input for a namelist input statement consists of

      1) optional blanks and namelist comments,
      2) the character & followed immediately by the namelist-group-name as specified in the NAMELIST
         statement,
      3) one or more blanks,
      4) a sequence of zero or more name-value subsequences separated by value separators, and
      5) a slash to terminate the namelist input.

         A slash encountered in a namelist input record causes the input statement to terminate. A slash cannot be
         used to separate two values in a namelist input statement.

   A group name or object name is without regard to case.

### **Example**
   
   An example of a NAMELIST statement is:

```fortran
     NAMELIST /NLIST/ A, B, C
```

   or a group may be defined by multiple statements using the same group name in a scoping unit:

```fortran
     NAMELIST /NLIST/ A, B
     NAMELIST /NLIST/ C

     ! READ/WRITE EXAMPLES: [ NML = ] namelist-group-name
     READ(*,NML=NLIST)
     WRITE(*,NLIST)
     WRITE(*,NML=NLIST)
```

```fortran
      program sample_namelist
      implicit none
      logical           :: l=.true.
      character(len=10) :: c='XXXXXXXXXX'
      real              :: r=12.3456
      integer           :: i=789
      complex           :: x=(12345.6789,9876.54321)
      doubleprecision   :: d= 123456789.123456789d0
      namelist /nlist/ l,c,r,i,x,d
         write(*,nlist)
      end program sample_namelist
```

   Results:

```code
      &NLIST
       L=T,
       C="XXXXXXXXXX",
       R=  12.3456001    ,
       I=        789,
       X=(  12345.6787    ,  9876.54297    ),
       D=  123456789.12345679     ,
       /
```

   Longer example:

```fortran
      program demo_namelist
      implicit none
      integer           :: lun

      ! create a namelist and initialize the values
      logical           :: l=.true.
      character(len=10) :: c='XXXXXXXXXX'
      real              :: r=12.3456
      integer           :: i=789
      complex           :: x=(12345.6789,9876.54321)
      doubleprecision   :: d= 123456789.123456789d0
      integer           :: a(5)=[1,2,3,4,5]
      type point
       integer           :: x=0
       integer           :: y=0
       character(len=10) :: color='red'
      endtype point
      type(point) :: dot
      namelist /nlist/ l,c,r,i,x,d,a,dot

      open(file='_tmp_',newunit=lun)

         write(*,*)'initial nlist'
         write(*,nlist)
         write(lun,nlist)

         write(*,*)'change values and print nlist again'
         a=[10,20,30,40,50]
         dot%color='orange'
         write(lun,nlist)

         write(*,*)'read back values. Can have multiple sets in a file'
         rewind(lun)
         read(lun,nlist)
         read(lun,nlist)
         write(*,nlist)

      end program demo_namelist
```

  Results:

```code
    initial nlist
   &NLIST
    L=T,
    C="XXXXXXXXXX",
    R=  12.3456001    ,
    I=        789,
    X=(12345.6787,9876.54297),
    D=  123456789.12345679     ,
    A=          1,          2,          3,          4,          5,

    DOT%X=          0,
    DOT%Y=          0,
    DOT%COLOR="red       ",
    /
    change values and print nlist again
    read back values. Can have multiple sets in a file
   &NLIST
    L=T,
    C="XXXXXXXXXX",
    R=  12.3456001    ,
    I=        789,
    X=(12345.6787,9876.54297),
    D=  123456789.12345679     ,
    A=         10,         20,         30,         40,         50,

    DOT%X=          0,
    DOT%Y=          0,
    DOT%COLOR="orange    ",
    /

   o Scanning on input till group name is found
   o reading partial lists
   o string quoting
   o NAMELIST in internal read and write. See

    ./arguments/namelist
```

OTHER
### **Other**

  C915    (R913) A namelist-group-name shall be the name of a namelist group.

  C916    (R913) A namelist-group-name shall not appear if a REC= specifier, format, input-item-list, or an
          output-item-list appears in the data transfer statement.
  C917    (R913) An io-control-spec-list shall not contain both a format and a namelist-group-name.
  C919    (R913) If namelist-group-name appears without a preceding NML=, it shall be the second item in the
          io-control-spec-list and the first item shall be io-unit.
  C928    (R913) If a DECIMAL=, BLANK=, PAD=, SIGN=, or ROUND= specifier appears, a format or
          namelist-group-name shall also appear.
  C929    (R913) If a DELIM= specifier appears, either format shall be an asterisk or namelist-group-name shall
          appear.
3. If the data transfer statement contains a format or namelist-group-name, the statement is a formatted
   input/output statement; otherwise, it is an unformatted input/output statement.

1. The NML= specifier supplies the namelist-group-name (5.6). This name identifies a particular collection of data
   objects on which transfer is to be performed.

2. If a namelist-group-name appears, the statement is a namelist input/output statement.

4. All values following the name= part of the namelist entity (10.11) within the input records are transmitted to
   the matching entity specified in the namelist-group-object-list prior to processing any succeeding entity within
   the input record for namelist input statements. If an entity is specified more than once within the input record
   during a namelist formatted data transfer input statement, the last occurrence of the entity specifies the value or
   values to be used for that entity.

9.6.4.6     Namelist formatting

 1. If namelist formatting has been established, editing is performed as described in 10.11.

 2. Every allocatable namelist-group-object in the namelist group shall be allocated and every namelist-group-object
    that is a pointer shall be associated with a target. If a namelist-group-object is polymorphic or has an ultimate
    component that is allocatable or a pointer, that object shall be processed by a defined input/output procedure
    (9.6.4.7).

9.6.5       Termination of data transfer statements

1. Termination of an input/output data transfer statement occurs when

        format processing encounters a colon or data edit descriptor and there are no remaining elements in the
        input-item-list or output-item-list,
        unformatted or list-directed data transfer exhausts the input-item-list or output-item-list,
        namelist output exhausts the namelist-group-object-list,
        an error condition occurs,
        an end-of-file condition occurs,

        a slash (/) is encountered as a value separator (10.10, 10.11) in the record being read during list-directed
        or namelist input, or
        an end-of-record condition occurs during execution of a nonadvancing input statement (9.11).
2. If an error condition occurs during execution of an input/output statement that contains neither an ERR= nor
   IOSTAT= specifier, error termination of the program is initiated. If an error condition occurs during execution
   of an input/output statement that contains either an ERR= specifier or an IOSTAT= specifier then:
        1.    processing of the input/output list, if any, terminates;

        2.    if the statement is a data transfer statement or the error occurs during a wait operation, all
              do-variables in the statement that initiated the transfer become undefined;
        3.    if an IOSTAT= specifier appears, the scalar-int-variable in the IOSTAT= specifier becomes defined
              as specified in 9.11.5;
        4.    if an IOMSG= specifier appears, the iomsg-variable becomes defined as specified in 9.11.6;
        5.    if the statement is a READ statement and it contains a SIZE= specifier, the scalar-int-variable in
              the SIZE= specifier becomes defined as specified in 9.6.2.15;
        6.    if the statement is a READ statement or the error condition occurs in a wait operation for a transfer
              initiated by a READ statement, all input items or namelist group objects in the statement that
              initiated the transfer become undefined;
        7.    if an ERR= specifier appears, a branch to the statement labeled by the label in the ERR= specifier
               occurs.
 8. In a data transfer statement, the variable specified in an IOSTAT=, IOMSG=, or SIZE= specifier, if any, shall
    not be associated with any entity in the data transfer input/output list (9.6.3) or namelist-group-object-list, nor
    with a do-variable of an io-implied-do in the data transfer input/output list.
10.11        Namelist formatting
10.11.1      General
 1. Namelist input/output allows data editing with NAME=value subsequences. This facilitates documentation of
    input and output files and more flexibility on input.

10.11.2      Name-value subsequences
 1. The characters in one or more namelist records constitute a sequence of name-value subsequences, each of
    which consists of an object designator followed by an equals and followed by one or more values and value
    separators. The equals may optionally be preceded or followed by one or more contiguous blanks. The end of a
    record has the same effect as a blank character, unless it is within a character constant. Any sequence of two or
    more consecutive blanks is treated as a single blank, unless it is within a character constant.

 2. The name may be any name in the namelist-group-object-list (5.6).

 3. A value separator for namelist formatting is the same as for list-directed formatting (10.10).

10.11.3      Namelist input
10.11.3.1    Overall syntax

 2. In each name-value subsequence, the name shall be the name of a namelist group object list item with an optional
    qualification and the name with the optional qualification shall not be a zero-sized array, a zero-sized array section,
    or a zero-length character string. The optional qualification, if any, shall not contain a vector subscript.

10.11.3.2    Namelist group object names

1. Within the input data, each name shall correspond to a particular namelist group object name. Subscripts,
   strides, and substring range expressions used to qualify group object names shall be optionally signed integer
   literal constants with no kind type parameters specified. If a namelist group object is an array, the input record
   corresponding to it may contain either the array name or the designator of a subobject of that array, using the
   syntax of object designators (R601). If the namelist group object name is the name of a variable of derived type,
   the name in the input record may be either the name of the variable or the designator of one of its components,
   indicated by qualifying the variable name with the appropriate component name. Successive qualifications may
   be applied as appropriate to the shape and type of the variable represented.

2. The order of names in the input records need not match the order of the namelist group object items. The input
   records need not contain all the names of the namelist group object items. The definition status of any names
   from the namelist-group-object-list that do not occur in the input record remains unchanged. In the input record,
   each object name or subobject designator may be preceded and followed by one or more optional blanks but shall
   not contain embedded blanks.

10.11.3.3    Namelist group object list items

1. The name-value subsequences are evaluated serially, in left-to-right order. A namelist group object designator
   may appear in more than one name-value sequence.

2. When the name in the input record represents an array variable or a variable of derived type, the effect is as
   if the variable represented were expanded into a sequence of scalar list items, in the same way that formatted
   input/output list items are expanded (9.6.3). Each input value following the equals shall then be acceptable to
   format specifications for the type of the list item in the corresponding position in the expanded sequence, except
   as noted in this subclause. The number of values following the equals shall not exceed the number of list items
   in the expanded sequence, but may be less; in the latter case, the effect is as if sufficient null values had been
   appended to match any remaining list items in the expanded sequence.

        NOTE 10.35
        For example, if the name in the input record is the name of an integer array of size 100, at most 100 values,
        each of which is either a digit string or a null value, may follow the equals; these values would then be
        assigned to the elements of the array in array element order.

3. A slash encountered as a value separator during the execution of a namelist input statement causes termination
   of execution of that input statement after transference of the previous value. If there are additional items in the
   namelist group object being transferred, the effect is as if null values had been supplied for them.

4. A namelist comment may appear after any value separator except a slash. A namelist comment is also permitted
   to start in the first nonblank position of an input record except within a character literal constant.

5. Successive namelist records are read by namelist input until a slash is encountered; the remainder of the record
   is ignored and need not follow the rules for namelist input values.

   10.11.3.4    Namelist input values

1. Each value is either a null value (10.11.3.5), c, r *c, or r *, where c is a literal constant, optionally signed if integer
   or real, and r is an unsigned, nonzero, integer literal constant. A kind type parameter shall not be specified for c
   or r. The constant c is interpreted as though it had the same kind type parameter as the corresponding effective
   item. The r *c form is equivalent to r successive appearances of the constant c, and the r * form is equivalent to
   r successive null values. Neither of these forms may contain embedded blanks, except where permitted within
   the constant c.

2. The datum c (10.11) is any input value acceptable to format specifications for a given type, except for a restriction
   on the form of input values corresponding to list items of types logical, integer, and character as specified in this
   subclause. The form of a real or complex value is dependent on the decimal edit mode in effect (10.6). The form

   of an input value shall be acceptable for the type of the namelist group object list item. The number and forms
   of the input values that may follow the equals in a name-value subsequence depend on the shape and type of
   the object represented by the name in the input record. When the name in the input record is that of a scalar
   variable of an intrinsic type, the equals shall not be followed by more than one value. Blanks are never used
   as zeros, and embedded blanks are not permitted in constants except within character constants and complex
   constants as specified in this subclause.

3. When the next effective item is of type real, the input form of the input value is that of a numeric input field. A
   numeric input field is a field suitable for F editing (10.7.2.3.2) that is assumed to have no fractional digits unless
   a decimal symbol appears within the field.

4. When the next effective item is of type complex, the input form of the input value consists of a left parenthesis
   followed by an ordered pair of numeric input fields separated by a comma (if the decimal edit mode is POINT) or
   a semicolon (if the decimal edit mode is COMMA), and followed by a right parenthesis. The first numeric input
   field is the real part of the complex constant and the second part is the imaginary part. Each of the numeric
   input fields may be preceded or followed by any number of blanks and ends of records. The end of a record may
   occur between the real part and the comma or semicolon, or between the comma or semicolon and the imaginary
   part.

5. When the next effective item is of type logical, the input form of the input value shall not include equals or value
   separators among the optional characters permitted for L editing (10.7.3).

6. When the next effective item is of type integer, the value in the input record is interpreted as if an Iw edit
   descriptor with a suitable value of w were used.

7. When the next effective item is of type character, the input form consists of a delimited sequence of zero or more
   rep-char s whose kind type parameter is implied by the kind of the corresponding list item. Such a sequence
   may be continued from the end of one record to the beginning of the next record, but the end of record shall
   not occur between a doubled apostrophe in an apostrophe-delimited sequence, nor between a doubled quote in a
   quote-delimited sequence. The end of the record does not cause a blank or any other character to become part
   of the sequence. The sequence may be continued on as many records as needed. The characters blank, comma,
   semicolon, and slash may appear in such character sequences.

          NOTE 10.36
          A character sequence corresponding to a namelist input item of character type shall be delimited either with
          apostrophes or with quotes. The delimiter is required to avoid ambiguity between undelimited character
          sequences and object names. The value of the DELIM= specifier, if any, in the OPEN statement for an
          external file is ignored during namelist input (9.5.6.8).

8. Let len be the length of the next effective item, and let w be the length of the character sequence. If len is less
   than or equal to w, the leftmost len characters of the sequence are transmitted to the next effective item. If len
   is greater than w, the constant is transmitted to the leftmost w characters of the next effective item and the
   remaining len-w characters of the next effective item are filled with blanks. The effect is as though the sequence
   were assigned to the next effective item in an intrinsic assignment statement (7.2.1.3).

10.11.3.5      Null values

1. A null value is specified by

          the r * form,
          blanks between two consecutive nonblank value separators following an equals,
          zero or more blanks preceding the first value separator and following an equals, or
          two consecutive nonblank value separators.

2. A null value has no effect on the definition status of the corresponding input list item. If the namelist group
   object list item is defined, it retains its previous value; if it is undefined, it remains undefined. A null value shall

   not be used as either the real or imaginary part of a complex constant, but a single null value may represent an
   entire complex constant.

   NOTE 10.37
   The end of a record following a value separator, with or without intervening blanks, does not specify a null
   value in namelist input.

10.11.3.6    Blanks

1.  All blanks in a namelist input record are considered to be part of some value separator except for

      o blanks embedded in a character constant,
      o embedded blanks surrounding the real or imaginary part of a complex constant,
      o leading blanks following the equals unless followed immediately by a slash or comma, or a semicolon if the
      o decimal edit mode is comma, and
      o blanks between a name and the following equals.

10.11.3.7    Namelist Comments

1. Except within a character literal constant, a "!" character after a value separator or in the first nonblank position
   of a namelist input record initiates a comment. The comment extends to the end of the current input record and
   may contain any graphic character in the processor-dependent character set. The comment is ignored. A slash
   within the namelist comment does not terminate execution of the namelist input statement. Namelist comments
   are not allowed in stream input because comments depend on record structure.

        NOTE 10.38
        Namelist input example:

```fortran
        INTEGER I; REAL X (8); CHARACTER (11) P; COMPLEX Z;
        LOGICAL :: G
        NAMELIST / TODAY / G, I, P, Z, X
        READ (*, NML = TODAY)
```

        The input data records are:

```code
        &TODAY I = 12345, X(1) = 12345, X(3:4) = 2*1.5, I=6, ! This is a comment.
        P = ''ISN'T_BOB'S'', Z = (123,0)/
```

        The results stored are:

```code
                Variable                         Value
                I                                6
                X (1)                            12345.0
                X (2)                            unchanged
                X (3)                            1.5
                X (4)                            1.5
                X (5)   X (8)                    unchanged
                P                                ISN'T BOB'S
                Z                                (123.0,0.0)
                G                                unchanged
```

  10.11.4      Namelist output
  10.11.4.1    Form of namelist output

1. The form of the output produced is the same as that required for input, except for the forms of real, character,
   and logical values. The name in the output is in upper case. With the exception of adjacent undelimited character
   values, the values are separated by one or more blanks or by a comma, or a semicolon if the decimal edit mode
   is COMMA, optionally preceded by one or more blanks and optionally followed by one or more blanks.

2. Namelist output shall not include namelist comments.

3. The processor may begin new records as necessary. However, except for complex constants and character values,
   the end of a record shall not occur within a constant, character value, or name, and blanks shall not appear
   within a constant, character value, or name.

        NOTE 10.39
        The length of the output records is not specified exactly and may be processor dependent.

10.11.4.2    Namelist output editing

1. Values in namelist output records are edited as for list-directed output (10.10.4).

        NOTE 10.40
        Namelist output records produced with a DELIM= specifier with a value of NONE and which contain a
        character sequence might not be acceptable as namelist input records.

10.11.4.3    Namelist output records

1. If two or more successive values for the same namelist group item in an output record produced have identical
   values, the processor has the option of producing a repeated constant of the form r *c instead of the sequence of
   identical values.

2. The name of each namelist group object list item is placed in the output record followed by an equals and a list
   of values of the namelist group object list item.

3. An ampersand character followed immediately by a namelist-group-name will be produced by namelist formatting
   at the start of the first output record to indicate which particular group of data objects is being output. A slash
   is produced by namelist formatting to indicate the end of the namelist formatting.

4. A null value is not produced by namelist formatting.

5. Except for new records created by explicit formatting within a defined output procedure or by continuation of
   delimited character sequences, each output record begins with a blank character.
