************************************************************************
*
*     Subroutine DVDFLT                              Called by: TACIT_R
*
*     set default values for V 
*
*     ***SOL (VERSION 2.3)
*     GENERAL UNCONSTRAINED OPTIMIZATION CONSTANTS.
*
************************************************************************
      SUBROUTINE DVDFLT(V)
*
*     subroutine arguments
*
      DOUBLE PRECISION V(*)
*
*     local vars
*
      DOUBLE PRECISION MACHEP
*
*     function declaration
*
      DOUBLE PRECISION D1MACH
*
*     set V
*
      MACHEP = D1MACH(4)
      V(19) = 0.1D0
      V(20) = -0.1D0
      V(21) = 0.1D0
      V(22) = 0.5D0
      V(23) = 2.D0
      V(24) = 0.1D0
      V(25) = 4.D0
      V(26) = 0.1D0
      V(27) = 1.D-4
      V(28) = 0.75D0
      V(29) = 0.5D0
      V(30) = 0.75D0
      V(31) = 1.D-20
      IF (MACHEP .GT. 1.D-10) V(31) = MACHEP**2
      V(32) = DMAX1(1.D-10,(MACHEP**(1.D0/3.D0))**2)
      V(33) = DSQRT(MACHEP)
      V(34) = 1.D2 * MACHEP
      V(35) = 1.D0
      V(36) = 1.D0
      V(37) = V(32)
      V(39) = 1.D-6
      V(40) = 1.D0
      V(41) = 0.6D0
      V(44) = DSQRT(MACHEP)
*
*     general optimization values
*
      V(38) = -1.D0
      V(42) = 1.D3 * MACHEP
      V(43) = 0.8D0

      RETURN
      END
