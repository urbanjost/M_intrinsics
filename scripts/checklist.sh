for NAME in  \
ALL                  ALLOCATED         ANINT                  ANY                 ASIN                  ASINH \
ASSOCIATED           ATAN              ATAN2                  ATANH               ATOMIC_ADD            ATOMIC_AND \
ATOMIC_CAS           ATOMIC_DEFINE     ATOMIC_FETCH_ADD       ATOMIC_FETCH_AND    ATOMIC_FETCH_OR       ATOMIC_FETCH_XOR \
XOR                  ATOMIC_OR         ATOMIC_REF             ATOMIC_XOR          BESSEL_J0             BESSEL_J1 \
BESSEL_JN            BESSEL_JN         BESSEL_Y0              BESSEL_Y1           BESSEL_YN             BESSEL_YN \
BGE                  BGT               BIT_SIZE               BLE                 BLT                   BTEST \
CEILING              CHAR              CMPLX                  CO_BROADCAST        CO_MAX                CO_MIN \
CO_REDUCE            CO_SUM            COMMAND_ARGUMENT_COUNT CONJG               COS                   COSH \
COSHAPE              COUNT             CPU_TIME               CSHIFT              DATE_AND_TIME         DBLE \
DIGITS               DIM               DOT_PRODUCT            DPROD               DSHIFTL               DSHIFTR \
EOSHIFT              EPSILON           ERF                    ERFC                ERFC_SCALED           EVENT_QUERY \
EXECUTE_COMMAND_LINE EXP               EXPONENT               EXTENDS_TYPE_OF     FAILED_IMAGES         FINDLOC \
FLOOR                FRACTION          GAMMA                  GET_COMMAND         GET_COMMAND_ARGUMENT  GET_ENVIRONMENT_VARIABLE \
GET_TEAM             HUGE              HYPOT                  IACHAR              IALL                  IAND \
IANY                 IBCLR             IBITS                  IBSET               ICHAR                 IEOR \
IMAGE_INDEX          IMAGE_STATUS      INDEX                  INT                 IOR                   IPARITY \
ISHFT                ISHFTC            IS_CONTIGUOUS          IS_IOSTAT_END       IS_IOSTAT_EOR         KIND \
LBOUND               LCOBOUND          LEADZ                  LEN                 LEN_TRIM              LGE \
LGT                  LLE               LLT                    LOG                 LOG_GAMMA             LOG10 \
LOGICAL              MASKL             MASKR                  MATMUL              MAX                   MAXEXPONENT \
MAXLOC               MAXVAL            MERGE                  MERGE_BITS          MIN                   MINEXPONENT \
MINLOC               MINVAL            MOD                    MODULO              MOVE_ALLOC            MVBITS \
NEAREST              NEW_LINE          NINT                   NORM2               NOT                   NULL \
NUM_IMAGES           OUT_OF_RANGE      PACK                   PARITY              POPCNT                POPPAR \
PRECISION            PRESENT           PRODUCT                RADIX               RANDOM_INIT           RANDOM_NUMBER \
RANDOM_SEED          RANGE             RANK                   REAL                REDUCE                REPEAT \
RESHAPE              RRSPACING         SAME_TYPE_AS           SCALE               SCAN                  SELECTED_CHAR_KIND \
KIND                 SELECTED_INT_KIND KIND                   SELECTED_REAL_KIND  KIND                  SET_EXPONENT \
SHAPE                SHIFTA            SHIFTL                 SHIFTR              SIGN                  SIN \
SINH                 SIZE              SPACING                SPREAD              SQRT                  STOPPED_IMAGES \
STORAGE_SIZE         SUM               SYSTEM_CLOCK           TAN                 TANH                  TEAM_NUMBER \
THIS_IMAGE           THIS_IMAGE        TINY                   TRAILZ              TRANSFER              TRANSPOSE \
TRIM                 UBOUND            UCOBOUND               UNPACK              VERIFY \
ACHAR                ABS               ACOS                   ACOSH               ADJUSTL               ADJUSTR \
AINT                 AIMAG \
 \
COMPILER_VERSION     COMPILER_OPTIONS  C_SIZEOF               CO_UBOUND           CO_LBOUND  C_LOC \
C_FUNLOC             C_F_PROCPOINTER   C_F_POINTER            C_ASSOCIATED \
$NULL
do
   if [ -f $NAME.md ] 
   then
      echo found $NAME
      touch $NAME.md
   else
      echo missing $NAME
   fi
done
