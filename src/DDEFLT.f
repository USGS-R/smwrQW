************************************************************************
*
*     Subroutine DDEFLT                              Called by: TACIT_R
*
*     set default values for IV 
*
*     ***SOL (VERSION 2.3)
*     GENERAL UNCONSTRAINED OPTIMIZATION CONSTANTS.
*
************************************************************************
      SUBROUTINE DDEFLT(IV)
*
*     subroutine arguments
*
      INTEGER*4 IV(*)
*
*     local vars
*
      INTEGER*4 I
*
*     misc. default values
*
      IV(1) = 12
      IV(3) = 0
      IV(4) = 0
      IV(17) = 200
      IV(18) = 150
      IV(42) = 72
      IV(44) = 59
      IV(45) = 71
      IV(51) = 2
      IV(58) = 60
*
*     general optimization values
*
      IV(16) = 0
      IV(25) = 1
      IV(49) = 47
      IV(50) = 25
      IV(52) = 0
      IV(53) = 0
*
*     turn off print options
*
      DO 10 I=19,24
         IV(I) = 0
 10   CONTINUE

      RETURN
      END

