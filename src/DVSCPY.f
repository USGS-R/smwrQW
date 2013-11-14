************************************************************************
*
*     Subroutine DVSCPY                               Called by: DHUMIT
*
*     set P-vector Y to scalar S
*
************************************************************************
      SUBROUTINE DVSCPY(NP,Y,S)
*
*     subroutine arguments
*
      INTEGER*4 NP
      DOUBLE PRECISION S,Y(*)
*
*     local var
*
      INTEGER*4 I

      DO 10 I = 1, NP+1
         Y(I) = S
 10   CONTINUE

      RETURN
      END
