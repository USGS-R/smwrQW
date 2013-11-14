************************************************************************
*
*     Subroutine PPARRANGE                   Called by: PPCCTEST
*
*     Arrange the censoring levels so that the plotting positions can be 
*     computed.
*
************************************************************************
      SUBROUTINE PPARRANGE(X,XC,NOBSC,AJ,BJ,CJ,NDL,Y,NUNCEN)
*
*     SUBROUTINE ARGUMENTS
*
      INTEGER NOBSC,AJ(*), BJ(*),CJ(*), NDL, NUNCEN
      DOUBLE PRECISION X(*),Y(*)
      LOGICAL XC(*)
*
*     LOCAL VARS
*
      INTEGER I,J
      DOUBLE PRECISION, DIMENSION(NDL + 2) :: DL
      DOUBLE PRECISION PLUSINF, CDL
      DATA PLUSINF/1.D300/
*
*     AGGREGATE THE DL'S AND CONSTRUCT THE SORTED Y VECTOR
*
      NUNCEN = 0
      DL(1) = -PLUSINF
      CDL = DL(1)
      J = 1
      DO I=1,NOBSC
         IF(.NOT. XC(I)) THEN
            NUNCEN = NUNCEN + 1
            Y(NUNCEN) = X(I)
         ELSE
            IF(X(I) .GT. CDL) THEN
               J = J + 1
               DL(J) = X(I)
               CDL = X(I)
            ENDIF
         ENDIF
      ENDDO
      DL(J+1) = PLUSINF
*
*     ACCUMULATE THE INDEXES
*
      DO J=1,(NDL + 1)
         AJ(J) = 0
         BJ(J) = 0
         CJ(J) = 0
         DO I=1, NOBSC
            IF(XC(I)) THEN
               IF(X(I) .LE. DL(J)) BJ(J) = BJ(J) + 1
               IF(X(I) .EQ. DL(J)) CJ(J) = CJ(J) + 1
            ELSE
               IF(DL(J) .LE. X(I) .AND. X(I) .LT. DL(J+1)) 
     $              AJ(J) = AJ(J) + 1
               IF(X(I) .LT. DL(J)) BJ(J) = BJ(J) + 1
            ENDIF
         ENDDO
      ENDDO
      AJ(NDL + 2) = 0
      BJ(NDL + 2) = NOBSC
      CJ(NDL + 2) = 0
      RETURN
      END
