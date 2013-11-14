************************************************************************
*
*     Function PRED            Called by: evalaml,
*                                         AMLREG,AMLSTAT,LADREG,LOGLK,
*                                         MLELOAD2,MLESTAT,TACIT_G,
*                                         TACIT_H,TACIT_L
*
*     calculate predicted value of response variable for a linear
*     model with (NPAR) explanatory variables
*
************************************************************************
      DOUBLE PRECISION FUNCTION PRED(NPAR,NXDIM,I,X,PARAM)
*
*     function arguments
*
      INTEGER*4 NPAR,NXDIM,I
      DOUBLE PRECISION PARAM(*),X(NXDIM,*)
*
*     local var
*
      INTEGER*4 J
*
*     sum the quantities associated with the (NPAR) explanatory
*     variables.  The first column vector of X(I,J) is a vector of
*     ones associated with the intercept, PARAM(1).
*
      PRED = 0.D0
      DO 10 J=1,NPAR
         PRED = PRED + PARAM(J)*X(I,J)
 10   CONTINUE

      RETURN
      END
