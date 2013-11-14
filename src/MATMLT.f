************************************************************************
*
*     Subroutine MATMLT                     Called by: MLELOAD2, MLEVAR
*
*     matrix multiplication for XO * XTXINV * XT (for mean) or
*     XO * XTXINV * XJT (for variance), where:
*
*     XT       explanatory variables for a single population value,
*              transposed
*     XJT      Jth set of explanatory variables for which a predicted
*              value is desired, transposed
*
************************************************************************
      SUBROUTINE MATMLT(V,XO,XTXINV,NPAR,I,JJ)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 NPAR,I,JJ
      DOUBLE PRECISION V,XO(MAXOBSE,*),XTXINV(MAXPARMS,*)
*
*     local vars
*
      INTEGER*4 J,K
      DOUBLE PRECISION B(MAXPARMS)
*
*     multiply XO and XTXINV, storing intermediate result in B, then
*     multiply B and XO.  (XO in the second of these two operations
*     is equal to XT or XJT (?)).
*     
      V = 0.D0
      DO 20 J=1,NPAR
         B(J) = 0.D0
         DO 10 K=1,NPAR
            B(J) = B(J) + XO(I,K)*XTXINV(K,J)
 10      CONTINUE
         V = V + B(J)*XO(JJ,J)
 20   CONTINUE

      RETURN
      END


