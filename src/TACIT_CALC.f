************************************************************************
*
*     Subroutine TACIT_CALC         Called by: AMLSTAT, TACIT_G, TACIT_H
*
*     compute gaussian functions and their derivatives
*
************************************************************************
      SUBROUTINE TACIT_CALC(XSI,A)
*
*     subroutine arguments
*
      DOUBLE PRECISION XSI,A
*
*     function declaration
*
      DOUBLE PRECISION ERFC
*
*   
*
      IF (XSI .LT. -6.D0) THEN
         A = -XSI
      ELSEIF (XSI .GT. 6.D0) THEN
         A = 0.D0
      ELSE
         A =  0.3989422D0*EXP(-0.5*XSI**2)/
     &        (1.D0 - ERFC(XSI*0.7071068)/2.D0)
      ENDIF

      RETURN
      END

