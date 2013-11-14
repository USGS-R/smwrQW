************************************************************************
*
*     Function D1MACH                             Called by: 
*
*     Return a double precision machine-dependent parameter for the
*     local architecture.  One of 5 machine dependent parameters is
*     returned:
*
*     D1MACH(1) = B**(EMIN-1), the smallest positive magnitude.
*     D1MACH(2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
*     D1MACH(3) = B**(-T), the smallest relative spacing.
*     D1MACH(4) = B**(1-T), the largest relative spacing.
*     D1MACH(5) = LOG10(B)
*
*     The function is based on the CMLIB version of D1MACH, the function
*     originally developed for the PORT library (see Fox et al., 1978)
*     The dimensions for the integer values set to 2 to avoid compiler 
*     issues
*
************************************************************************
      DOUBLE PRECISION FUNCTION D1MACH(I)
*
*     function argument
*
      INTEGER*4 I
*
*     local vars
*
      INTEGER*4 SMALL(2),LARGE(2),RIGHT(2),DIVER(2),LOG10(2)
      DOUBLE PRECISION DMACH(5)

      EQUIVALENCE (DMACH(1),SMALL(1))
      EQUIVALENCE (DMACH(2),LARGE(1))
      EQUIVALENCE (DMACH(3),RIGHT(1))
      EQUIVALENCE (DMACH(4),DIVER(1))
      EQUIVALENCE (DMACH(5),LOG10(1))
*
*     Machine constants for SUN SPARC
*
*     Machine constants for IEEE arithmetic machines, such as the AT&T
*     3b series and Motorola 68000 based machines (e.g. SUN 3 and AT&T
*     PC 7300), in which the most significant byte is stored first.
*
*       DATA SMALL(1),SMALL(2) /    1048576,          0 /
*       DATA LARGE(1),LARGE(2) / 2146435071,         -1 /
*       DATA RIGHT(1),RIGHT(2) / 1017118720,          0 /
*       DATA DIVER(1),DIVER(2) / 1018167296,          0 /
*       DATA LOG10(1),LOG10(2) / 1070810131, 1352628735 /
*
*
*     Machine constants for Windows & Linux on PC architecture
*     
*
*     Machine constants for IEEE arithmetic machines and 8087-based
*     micros, such as the IBM PC and AT&T 6300, in which the least
*     significant byte is stored first.
*
      DATA SMALL(1),SMALL(2) /          0,    1048576 /
      DATA LARGE(1),LARGE(2) /         -1, 2146435071 /
      DATA RIGHT(1),RIGHT(2) /          0, 1017118720 /
      DATA DIVER(1),DIVER(2) /          0, 1018167296 /
      DATA LOG10(1),LOG10(2) / 1352628735, 1070810131 /

      D1MACH = DMACH(I)

      RETURN
      END
