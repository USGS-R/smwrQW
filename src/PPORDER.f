************************************************************************
*
*     Subroutine PPORDER                     Called by: PPCCTEST
*
*     Return the index (IX) to put the censored data in sequential order.
*
************************************************************************
      SUBROUTINE PPORDER(CENSFLAG,X,N,IX)

      LOGICAL CENSFLAG(*)
      REAL*8 X(*)
      INTEGER*4 IX(*)
      INTEGER N
C
C     LOCAL VARS
C
      INTEGER INDX,I,ICT
      INTEGER, dimension(0:N) :: L, R, P
      
C
C    FIRST CHECK TO SEE IF WE HAVE ORDERED DATA TO BEGIN WITH
C
      DO 50 I2=2,N
         IX(I2)    =  I2
         IF(X(I2) .LT. X(I2-1)) GOTO 1
         IF(X(I2) .EQ. X(I2-1) .AND. (CENSFLAG(I2) .AND. .NOT.
     $        CENSFLAG(I2-1))) GOTO 1
 50   CONTINUE
      IX(1)     =  1
      RETURN
C
C     DATA NOT SORTED
C
 1    CONTINUE
      L(1) =  0
      R(1) =  0
      P(1) =  0
      DO 10 I=2,N
         INDX =  1
         L(I)   =  0
         R(I)   =  0
 20      CONTINUE
         IF(X(I) .GT. X(INDX) .OR. ((X(I) .EQ. X(INDX)) .AND.
     $        (CENSFLAG(INDX) .OR. (.NOT. CENSFLAG(I) .AND. 
     $        .NOT. CENSFLAG(INDX))))) THEN
            IF(R(INDX) .EQ. 0) THEN
               R(INDX)   =  I
               P(I)      =  INDX
               GOTO 10
            ELSE
               INDX =  R(INDX)
               GOTO 20
            ENDIF
         ELSE
            IF(L(INDX) .EQ. 0) THEN
               L(INDX)   =  I
               P(I)      =  INDX
               GOTO 10
            ELSE
               INDX =  L(INDX)
               GOTO 20
            ENDIF
         ENDIF
 10   CONTINUE

      INDX =  1
      DO 40 ICT=1,N
         
 30      CONTINUE
         IF(L(INDX) .EQ. 0) THEN
            IX(ICT)     =  INDX
            P(R(INDX))   =  P(INDX)
            L(P(INDX))   =  R(INDX)
            INDX         =  P(INDX)
         ELSE
            INDX =  L(INDX)
            GOTO 30
         ENDIF
 40   CONTINUE
      RETURN
      END
