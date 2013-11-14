************************************************************************
*
*     Function INITDS                                   Called by: ERFC
*
*     Determine the number of terms needed in an orthogonal polynomial
*     series so that it meets a specified accuracy.
*
*     Routine is a modified version of the INITDS code retrieved from
*     NETLIB (www.netlib.org). 
*
*     Original author: W. Fullerton of Los Alamos National Lab
*
*     Initialize the orthogonal series, represented by the array OS, so
*     that INITDS is the number of terms needed to insure the error is
*     no larger than ETA.  Ordinarily, ETA will be chosen to be
*     one-tenth machine precision.
*
*     Arguments:
*
*     OS      array of NOS coefficients in an orthogonal series
*     NOS     number of coefficients in OS.
*     ETA     scalar containing requested accuracy of series
*
************************************************************************
      INTEGER*4 FUNCTION INITDS(OS,NOS,ETA)
*
*     function arguments
*
      INTEGER*4 NOS
      REAL ETA
      DOUBLE PRECISION OS(*)
*
*     local vars
*
      INTEGER*4 I,II
      REAL ERR

      ierr = 0
      ERR = 0.
      DO 10 II=1,NOS
        I = NOS + 1 - II
        ERR = ERR + ABS(REAL(OS(I)))
        IF (ERR.GT.ETA) GOTO 20
   10 CONTINUE

C     Original from LOADEST 20 IF (I .EQ. NOS) CALL ERROR7(4)
 20   CONTINUE
      INITDS = I

      RETURN
      END
