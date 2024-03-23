program maxmin
!! Fortran standard does NOT specify how to handle NaN vis-a-vis max and min
!! https://gcc.gnu.org/onlinedocs/gfortran/MAX-and-MIN-intrinsics-with-REAL-NaN-arguments.html

use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_is_nan, ieee_quiet_nan, ieee_positive_inf
use, intrinsic :: iso_fortran_env, stderr=>error_unit
implicit none


real :: A(4), x, y, nan, inf

nan = ieee_value(0., ieee_quiet_nan)
inf = ieee_value(0., ieee_positive_inf)

if(.not.ieee_is_nan(nan)) then
  write(stderr,*) 'NaN: ',nan
endif

A = [0., 1., 2., nan]
print '(4F10.7,A,F10.7)', A, ' maximum is', maxval(A)

x = nan
y = 1.

print '(F10.7,F10.7,A,F10.7)', x,y, ' maximum is ',max(x,y)

! Gfortran, Flang 7 say inf; Intel says NaN
print *,'max of inf() and NaN is', max(inf, nan)

end program
!
!url = https://github.com/scivision/fortran2018-examples
!
!    16    17.8   Exceptional values
!    17    ISO/IEC 60559:2020 speci   es the following exceptional    oating-point values.
!    18          Subnormal values have very small absolute values and reduced precision.
!    19          Infinite values (+infinity and    infinity) are created by over   ow or division by zero.
!    20          Not-a-Number ( NaN) values are undefined values or values created by an invalid operation.
!    21    A value that does not fall into the above classes is called a normal number.
!    22    The functions IEEE_IS_FINITE, IEEE_IS_NAN, IEEE_IS_NEGATIVE, and IEEE_IS_NORMAL are
!    23    provided to test whether a value is finite, NaN, negative, or normal. The function IEEE_VALUE is provided to
!    24    generate an IEEE number of any class, including an infinity or a NaN. The inquiry functions IEEE_SUPPORT_-
!    25    SUBNORMAL,IEEE_SUPPORT_INF,andIEEE_SUPPORT_NANareprovidedtodeterminewhether these
!    26    facilities are available for a particular kind of real.

!    8     17.11.66 IEEE_VALUE (X, CLASS)
!    9     Description. Return number in a class.
!    10    Class. Elemental function.
!    11    Arguments.
!    12    X         shall be of type real.
!    13    CLASS     shall be of type IEEE_CLASS_TYPE. The value is permitted to be: IEEE_SIGNALING_NAN or
!    14              IEEE_QUIET_NANifIEEE_SUPPORT_NAN(X)hasthevaluetrue,IEEE_NEGATIVE_INF
!    15              or IEEE_POSITIVE_INFifIEEE_SUPPORT_INF(X)hasthevaluetrue,IEEE_NEGATIVE_-
!    16              SUBNORMALorIEEE_POSITIVE_SUBNORMALifIEEE_SUPPORT_SUBNORMAL(X)has
!    17              the value true, IEEE_NEGATIVE_NORMAL, IEEE_NEGATIVE_ZERO, IEEE_POSITIVE_-
!    18              ZEROorIEEE_POSITIVE_NORMAL.
!    19    Restriction. IEEE_VALUE (X, CLASS) shall not be invoked if IEEE_SUPPORT_DATATYPE (X) has the
!    20    value false.
!    21    Result Characteristics. Same as X.
!    22    Result Value. The result value is an IEEE value as speci   ed by CLASS. Although in most cases the value is
!    23    processor dependent, the value shall not vary between invocations for any particular X kind type parameter and
!    24    CLASS value.
!    25    Example. IEEE_VALUE (1.0, IEEE_NEGATIVE_INF) has the value    in   nity.
!    26    Whenever IEEE_VALUE returns a signaling NaN, it is processor dependent whether or not invalid is raised and
!    27    processor dependent whether or not the signaling NaN is converted into a quiet NaN.
!          NOTE
!          If the expr in an assignment statement is a reference to the IEEE_VALUE function that returns a signaling
!          NaNandthe variable is of the same type and kind as the function result, it is recommended that the signaling
!          NaNbepreserved.
!    28    17.12   Examples
!          NOTE1
!            MODULE DOT
!               ! Module for dot product of two real arrays of rank 1.
!               ! The caller needs to ensure that exceptions do not cause halting.
!              USE, INTRINSIC :: IEEE_EXCEPTIONS
!              LOGICAL :: MATRIX_ERROR = .FALSE.
!              INTERFACE OPERATOR(.dot.)
!                 MODULE PROCEDURE MULT
!              END INTERFACE
!            CONTAINS
!              REAL FUNCTION MULT (A, B)
!                REAL, INTENT (IN) :: A(:), B(:)
!                INTEGER I
!                LOGICAL OVERFLOW
!                IF (SIZE(A) /= SIZE(B)) THEN
!                  MATRIX_ERROR = .TRUE.
!                  RETURN
!                END IF
!                ! The processor ensures that IEEE_OVERFLOW is quiet.
!                MULT = 0.0
!                DO I = 1, SIZE (A)
!                  MULT = MULT + A(I)*B(I)
!                END DO
!                CALL IEEE_GET_FLAG (IEEE_OVERFLOW, OVERFLOW)
!                IF (OVERFLOW) MATRIX_ERROR = .TRUE.
!              END FUNCTION MULT
!            END MODULE DOT
!        This module provides a function that computes the dot product of two real arrays of rank 1. If the sizes of the
!        arrays are di   erent, an immediate return occurs with MATRIX_ERROR true. If over   ow occurs during the
!        actual calculation, the IEEE_OVERFLOW    ag will signal and MATRIX_ERROR will be true.
!       NOTE2
!            USE, INTRINSIC :: IEEE_EXCEPTIONS
!            USE, INTRINSIC :: IEEE_FEATURES, ONLY: IEEE_INVALID_FLAG
!            ! The other exceptions of IEEE_USUAL (IEEE_OVERFLOW and
!            ! IEEE_DIVIDE_BY_ZERO) are always available with IEEE_EXCEPTIONS
!            TYPE (IEEE_STATUS_TYPE) STATUS_VALUE
!            LOGICAL, DIMENSION(3) :: FLAG_VALUE
!            . . .
!            CALL IEEE_GET_STATUS (STATUS_VALUE)
!            CALL IEEE_SET_HALTING_MODE (IEEE_USUAL, .FALSE.) ! Needed in case the
!            !          default on the processor is to halt on exceptions
!            CALL IEEE_SET_FLAG (IEEE_USUAL, .FALSE.)
!            ! First try the "fast" algorithm for inverting a matrix:
!            MATRIX1 = FAST_INV (MATRIX) ! This shall not alter MATRIX.
!            CALL IEEE_GET_FLAG (IEEE_USUAL, FLAG_VALUE)
!              ANY(FLAG_VALUE)) THEN
!            IF (
!              ! "Fast" algorithm failed; try "slow" one:
!              CALL IEEE_SET_FLAG (IEEE_USUAL, .FALSE.)
!              MATRIX1 = SLOW_INV (MATRIX)
!                                J3/23-007r1                    499
!       J3/23-007r1              WD1539-1                  2023-06-13
!       NOTE2 (cont.)
!              CALL IEEE_GET_FLAG (IEEE_USUAL, FLAG_VALUE)
!              IF (ANY (FLAG_VALUE)) THEN
!                WRITE (*, *)    Cannot invert matrix
!                STOP
!              END IF
!            END IF
!            CALL IEEE_SET_STATUS (STATUS_VALUE)
!        In this example, the function FAST_INV might cause a condition to signal. If it does, another try is made with
!        SLOW_INV. If this still fails, a message is printed and the program stops. Note, also, that it is important to
!        set the    ags quiet before the second try. The state of all the    ags is stored and restored.
!       NOTE3
!            USE, INTRINSIC :: IEEE_EXCEPTIONS
!            LOGICAL FLAG_VALUE
!            . . .
!            CALL IEEE_SET_HALTING_MODE (IEEE_OVERFLOW, .FALSE.)
!            ! First try a fast algorithm for inverting a matrix.
!            CALL IEEE_SET_FLAG (IEEE_OVERFLOW, .FALSE.)
!            DO K = 1, N
!              . . .
!              CALL IEEE_GET_FLAG (IEEE_OVERFLOW, FLAG_VALUE)
!              IF (FLAG_VALUE) EXIT
!            END DO
!            IF (FLAG_VALUE) THEN
!            ! Alternative code which knows that K-1 steps have executed normally.
!            . . .
!            END IF
!        Here the code for matrix inversion is in line and the transfer is made more precise by adding extra tests of the
!           ag.
