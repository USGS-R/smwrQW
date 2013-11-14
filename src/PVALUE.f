************************************************************************
*
*     Function PVALUE                                Called by: AMLPVAL
*
*     computes likelihood ratio test p-value (PVALUE) for AMLE
*     parameter estimates
*
*     XLIKE_S  Log-Likelihood associated with simple model (S)
*     XLIKE_C  Log-Likelihood associated with complex model (C)
*
************************************************************************
      DOUBLE PRECISION FUNCTION PVALUE(XLIKE_S,XLIKE_C,IERR)
*
*     dimensional parameters and logical devices
*
      INCLUDE 'fmodules.inc'
*
*     function arguments
*
      DOUBLE PRECISION XLIKE_S,XLIKE_C
      INTEGER*4 IERR
*
*     local vars
*
      INTEGER*4 I
      DOUBLE PRECISION A0,A1,AN,ANA,AP,B0,B1,DEL,FAC,G,GOLD,SUM,X
*
*     0.57236... is gammln(0.5d0)
*
      IERR = 0
      IF(XLIKE_C .LE. XLIKE_S) THEN
         PVALUE = 1.D0
      ELSE
         X = XLIKE_C-XLIKE_S
         IF (X.LT.1.5D0) THEN
            AP = 0.5D0
            SUM = 2.D0
            DEL = 2.D0
            DO 10 I=1,10000
               AP = AP+1.D0
               DEL = DEL*X/AP
               SUM = SUM+DEL
               IF(ABS(DEL).LT.ABS(SUM)*3.D-12) THEN
                  PVALUE = 1.D0-SUM
     &                   * EXP(-X+0.5D0*LOG(X)-0.5723649429133818D0)
                  RETURN
               ENDIF
 10         CONTINUE
            IERR = -5
            PVALUE = 9999999.99
         ELSE
            GOLD = 0.D0
            A0 = 1.D0
            A1 = X
            B0 = 0.D0
            B1 = 1.D0
            FAC = 1.D0
            DO 20 I=1,10000
               AN = I
               ANA = AN-0.5D0
               A0 = (A1+A0*ANA)*FAC
               B0 = (B1+B0*ANA)*FAC
               A1 = X*A0+AN*FAC*A1
               B1 = X*B0+AN*FAC*B1
               IF(A1.NE.0.D0)THEN
                  FAC = 1.D0/A1
                  G = B1*FAC
                  IF(ABS((G-GOLD)/G).LT.3.D-12) THEN
                     PVALUE = EXP(-X+0.5D0*LOG(X)-0.5723649429133818D0)
     &                        * G
                     RETURN
                  ENDIF
                  GOLD = G
               ENDIF
 20         CONTINUE
            IERR = -6
            PVALUE = 9999999.99
         ENDIF
      ENDIF

      RETURN
      END
