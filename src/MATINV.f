************************************************************************
*
*     Subroutine MATINV                               Called by: AMLREG
*
*     invert matrix X producing XINV
*
************************************************************************
      SUBROUTINE MATINV(NPAR,X,XINV,ierr)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 NPAR
      DOUBLE PRECISION X(MAXPARMS+1,*),XINV(MAXPARMS+1,*)
*
*     local vars
*
      INTEGER*4 I,J
*
*     copy X to XINV; on return from FACINV, XINV is the inverse of X
*
      DO 20 I=1,NPAR
         DO 10 J=1,NPAR
            XINV(I,J) = X(I,J)
 10      CONTINUE
 20   CONTINUE
*
*     factor and invert the matrix
*
      CALL FACINV(XINV,MAXPARMS+1,NPAR, ierr)

      RETURN
      END
