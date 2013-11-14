************************************************************************
*
*     Subroutine TACIT_D                              Called by: AMLREG
*
*     compute partial derivatives of the likelihood function (L.F.)
*     for a single observation; results are standardized (divided by
*     sigma), which leads to more precise estimates.
*
*     See Cohn (1988) or Shenton and Bowman (1977) for details.
*
*     Arguments (all are dimensionless):
*
*     XSI     censoring threshold (input)
*     K2      hessian of L.F. w/ respect to SIGMA (output)
*     K3      3rd derivatives of L.F. w/ respect to SIGMA (output)
*     K3S     EXP. V. of L.F. w/ respect to SIGMA (output)
*     S2K2    Hessian of L.F. w/ respect to SIGMA**2 (output)
*     S2K3    3rd derivative w/ respect to SIGMA**2 (output)
*     S2K3S   EXP. V. of L.F. w/ respect to SIGMA**2 (output)
*
************************************************************************
      SUBROUTINE TACIT_D(XSI,K2,K3,K3S,S2K2,S2K3,S2K3S)
*
*     subroutine arguments
*
      DOUBLE PRECISION XSI
      DOUBLE PRECISION K2(2,*),S2K2(2,*)
      DOUBLE PRECISION K3(2,2,*),K3S(2,2,*),S2K3(2,2,*),S2K3S(2,2,*)
*
*     local vars
*
      DOUBLE PRECISION F,PHI,A,G,B,E,OP,H
*
*     function declaration
*
      DOUBLE PRECISION ERFC
*
*     compute gaussian functions and their derivatives
*
      IF (XSI .LT. -6.D0) THEN
         PHI = 0.D0
         F = 0.D0
         A = -XSI
         G = 0.D0
      ELSEIF (XSI .GT. 6.D0) THEN
         PHI = 1.0
         F = 0.D0
         A = 0.D0
         G = XSI
      ELSE
         PHI = 1.D0 - ERFC(XSI*0.7071068)/2.D0
         F = 0.3989422D0*EXP(-0.5*XSI**2)
         A =  F/PHI
         G =  F/(1.D0-PHI)
      ENDIF
*
*     
*
      OP = 1.-PHI

      B = -A*(XSI+A)
      E = -(B*(XSI+A)+(1.+B)*A)
      H = 1.0 + XSI*G

      K2(1,1) = -OP+PHI*B
      K2(1,2) = -2.*F+PHI*(XSI*B+A)
      K2(2,1) = K2(1,2)
      K2(2,2) = OP*(1.-3.*H)+PHI*(XSI**2*B+2.*XSI*A)
      
      K3(1,1,1) = -PHI*E
      K3(1,1,2) = OP*2.-PHI*(XSI*E+2.*B)
      K3(1,2,1) = K3(1,1,2)
      K3(2,1,1) = K3(1,1,2)
      K3(1,2,2) = 6.*F-PHI*(XSI**2*E+4.*XSI*B+2.*A)
      K3(2,1,2) = K3(1,2,2)
      K3(2,2,1) = K3(1,2,2)
      K3(2,2,2) = OP*(-2.+12.*H)-PHI*(XSI**3*E+6.*XSI**2*B+6.*XSI*A)
      
      K3S(1,1,1) = -(F+PHI*(A*B+E))
      K3S(1,1,2) = XSI*K3S(1,1,1)-2.*K2(1,1)
      K3S(1,2,1) = -(2.*XSI*F+PHI*(A*(XSI*B+A)+2.*B+XSI*E))
      K3S(2,1,1) = K3S(1,2,1)
      K3S(1,2,2) = XSI*K3S(1,2,1)-2.*K2(1,2)
      K3S(2,1,2) = K3S(1,2,2)
      K3S(2,2,1) = -(-F+3.*XSI**2*F+PHI*(2.*XSI*B+
     1     (A*B+E)*XSI**2+2.*(A+XSI*(A*A+B))))
      K3S(2,2,2) = XSI*K3S(2,2,1)-2.*K2(2,2)

*
*     calculate biases for SIGMA^2 instead of SIGMA
*
      S2K3(1,1,1) = K3(1,1,1)
      S2K3(1,1,2) = K3(1,1,2)/2.
      S2K3(1,2,1) = S2K3(1,1,2)
      S2K3(1,2,2) = (K3(1,2,2)-K2(1,2))/4.
      S2K3(2,1,1) = S2K3(1,1,2)
      S2K3(2,1,2) = S2K3(1,2,2)
      S2K3(2,2,1) = S2K3(1,2,2)
      S2K3(2,2,2) = (K3(2,2,2)-3.*K2(2,2))/8.0
      
      S2K3S(1,1,1) = K3S(1,1,1)
      S2K3S(1,1,2) = K3S(1,1,2)/2.
      S2K3S(1,2,1) = K3S(1,2,1)/2.
      S2K3S(1,2,2) = (K3S(1,2,2)-K2(1,2))/4.0
      S2K3S(2,1,1) = S2K3S(1,2,1)
      S2K3S(2,1,2) = S2K3S(1,2,2)
      S2K3S(2,2,1) = K3S(2,2,1)/4.
      S2K3S(2,2,2) = (K3S(2,2,2)-2.*K2(2,2))/8.0
      
      S2K2(1,1) = K2(1,1)
      S2K2(1,2) = K2(1,2)/2.
      S2K2(2,1) = S2K2(1,2)
      S2K2(2,2) = K2(2,2)/4.
      
      RETURN
      END
