************************************************************************
*
*     Subroutine DDUPDU                               Called by: DHUMIT
*
*     update scale vector D for DHUMSL
*
************************************************************************
      SUBROUTINE DDUPDU(D,HDIAG,IV,NP,V)
*
*     subroutine arguments
*
      INTEGER*4 NP,IV(*)
      DOUBLE PRECISION D(*),HDIAG(*),V(*)
*
*     local vars
*
      INTEGER*4 DTOLI,D0I,I
      DOUBLE PRECISION T
*
*     
*
      IF (IV(16).EQ.1 .OR. IV(31).LE.0) THEN
         DTOLI = IV(59)
         D0I = DTOLI + NP + 1
         DO 20 I = 1, NP+1
            T = DMAX1(DSQRT(DABS(HDIAG(I))),V(41)*D(I))
            IF (T .LT. V(DTOLI)) T = DMAX1(V(DTOLI), V(D0I))
            D(I) = T
            DTOLI = DTOLI + 1
            D0I = D0I + 1
 20      CONTINUE
      ENDIF

      RETURN
      END
