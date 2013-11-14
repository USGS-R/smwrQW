************************************************************************
*
*     Subroutine TACIT_R                  Called by: AMLREG, TACIT_TEST
*
*     perform censored regression
*
************************************************************************
      SUBROUTINE TACIT_R(NOBSC,NPAR,YLCAL,YD,X,PARMLE,XLIKE,CENSFLAG)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      LOGICAL CENSFLAG(*)
      INTEGER*4 NOBSC,NPAR
      DOUBLE PRECISION XLIKE,YLCAL(*),YD(*),PARMLE(*),X(MAXOBSC,*)
*
*     local vars
*
      INTEGER*4 I
      DOUBLE PRECISION WSS,D(MAXPARMS+1)
*
*     dimensioning of local vars IV and V.  LIV should be set to 59;
*     if for some reason LIV is set to <59, code in DDEFLT and DPARCK
*     that checks the value of LIV needs to be restored (its been 
*     deleted as its not needed when LIV=59).  If for some reason
*     LV is set to < 71, code in DDEFLT needs to be restored.
*
      INTEGER*4 LIV,LV
      PARAMETER (LV=1500,LIV=59)
      INTEGER*4 IV(LIV)
      DOUBLE PRECISION V(LV)
*
*     compute WSS
*
      WSS = 0.D0
      DO 10 I=1,NOBSC
         WSS = WSS + MAX(YLCAL(I),YD(I))**2
 10   CONTINUE
*
*     initialize PARMLE and D
*
      DO 20 I=1,NPAR
         PARMLE(I) = 0.D0
         D(I) = 1.D0
 20   CONTINUE
      PARMLE(NPAR+1) = 0.5*LOG(WSS/NOBSC)
      D(NPAR+1) = 1.D0
*
*     set default values for V (DVDFLT) and IV (DDEFLT)
*
      CALL DVDFLT(V)
      CALL DDEFLT(IV)
*
*     
*
      CALL DHUMSL(D,PARMLE,IV,LV,V,NOBSC,NPAR,X,YLCAL,YD,CENSFLAG)
      CALL TACIT_L(PARMLE,XLIKE,NOBSC,NPAR,X,YLCAL,YD,CENSFLAG)

      XLIKE = -XLIKE
      PARMLE(NPAR+1) = EXP(PARMLE(NPAR+1))

      RETURN
      END
