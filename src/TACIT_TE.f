************************************************************************
*
*     Subroutine TACIT_TEST                          Called by: AMLPVAL
*
*     computes likelihood ratio tests for AMLE parameter estimates
*
*     local vars
*     ----------
*     B_MLE    vector of MLE parameter estimates, where NPAR+1 element
*              contains S**2-HAT
*     X
*     X2
*
************************************************************************
      SUBROUTINE TACIT_TEST(CENSFLAG,ITEST,NOBSC,NPAR,XLCAL,XLIKEP,YD,
     &                      YLCAL)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      LOGICAL CENSFLAG(*)
      INTEGER*4 NOBSC,NPAR,ITEST(*)
      DOUBLE PRECISION XLIKEP,YD(*),YLCAL(*),XLCAL(MAXOBSC,*)
*
*     local vars
*
      INTEGER*4 I,K,J2
      DOUBLE PRECISION B_MLE(MAXPARMS),X(MAXOBSC,MAXPARMS)
      DOUBLE PRECISION X2(MAXOBSC,MAXPARMS)
*
*     if ITEST(K) = 0, fit BETA(K)
*
      J2 = 0
      DO 20 K=1,NPAR
         IF(ITEST(K) .EQ. 0) THEN
            J2 = J2 + 1
            DO 10 I=1,NOBSC
               X(I,J2) = XLCAL(I,K)
 10         CONTINUE
         ENDIF
 20   CONTINUE
*
*     
*
      DO 40 I=1,NOBSC
         X2(I,J2+1) = 1.D0
         DO 30 K =1,J2
            X2(I,K) = X(I,K)
 30      CONTINUE
 40   CONTINUE

      CALL TACIT_R(NOBSC,J2,YLCAL,YD,X2,B_MLE,XLIKEP,CENSFLAG)

      RETURN
      END
