************************************************************************
*
*     Subroutine DPARCK                               Called by: DHUMIT
*
*     check validity of IV and V values 
C
C     CHECK **SOL (VERSION 2.3) PARAMETERS
C
C     GENERAL UNCONSTRAINED OPT.
*
*
************************************************************************
      SUBROUTINE DPARCK(D,IV,LV,NP,V)
*
*     subroutine arguments
*
      INTEGER*4 LV,NP,IV(*)
      DOUBLE PRECISION D(*),V(*)
*
*     local vars
*
      INTEGER*4 I,IV1,K,L,M
      DOUBLE PRECISION BIG, MACHEP, TINY, VK, VM(34), VX(34)

      DATA BIG/0.D0/, MACHEP/-1.D0/, TINY/1.D0/

      DATA VM(1)/1.0D-3/, VM(2)/-0.99D0/, VM(3)/1.0D-3/, VM(4)/1.0D-2/,
     1     VM(5)/1.2D0/, VM(6)/1.D-2/, VM(7)/1.2D0/, VM(8)/0.D0/,
     2     VM(9)/0.D0/, VM(10)/1.D-3/, VM(11)/-1.D0/, VM(15)/0.D0/,
     3     VM(16)/0.D0/, VM(19)/0.D0/, VM(20)/-10.D0/, VM(21)/0.D0/,
     4     VM(22)/0.D0/, VM(23)/0.D0/, VM(27)/1.01D0/,
     5     VM(28)/1.D10/, VM(30)/0.D0/, VM(31)/0.D0/, VM(32)/0.D0/,
     6     VM(34)/0.D0/
      DATA VX(1)/0.9D0/, VX(2)/-1.D-3/, VX(3)/1.D1/, VX(4)/0.8D0/,
     1     VX(5)/1.D2/, VX(6)/0.8D0/, VX(7)/1.D2/, VX(8)/0.5D0/,
     2     VX(9)/0.5D0/, VX(10)/1.D0/, VX(11)/1.D0/, VX(14)/0.1D0/,
     3     VX(15)/1.D0/, VX(16)/1.D0/, VX(19)/1.D0/, VX(23)/1.D0/,
     4     VX(24)/1.D0/, VX(25)/1.D0/, VX(26)/1.D0/, VX(27)/1.D10/,
     5     VX(29)/1.D0/, VX(31)/1.D0/, VX(32)/1.D0/, VX(33)/1.D0/,
     6     VX(34)/1.D0/
*
*     function declaration
*
      DOUBLE PRECISION D1MACH
*
*     do whatever this mess does
*
      IV(44) = 59

      IV(3) = 0
      IV(45) = MAX0(IV(4), 0) + IV(42) - 1

      IF (LV .LT. IV(45)) THEN
         IV(1) = 16
         RETURN
      ENDIF
      IV(4) = 0

      IF (IV(51) .NE. 2) THEN
         IV(1) = 82
         RETURN
      ENDIF

      IV1 = IV(1)

      IF (IV1 .GE. 12 .AND. IV1 .LE. 14) THEN
         IF (NP+1 .LT. 1) THEN
            IV(1) = 81
            RETURN
         ENDIF

         IF (IV1 .NE. 14) THEN
            IV(46) = IV(58)
            IV(47) = IV(42)
            IF (IV1 .EQ. 13) RETURN
         ENDIF

         K = IV(49) - 19
         CALL DVDFLT(V(K+1))
         IV(54) = 0
         IV(38) = NP + 1
      ELSE
         IF (NP+1 .NE. IV(38)) THEN
            IV(1) = 17
            RETURN
         ENDIF
         IF (IV1 .GT. 11 .OR. IV1 .LT. 1) THEN
            IV(1) = 80
            RETURN
         ENDIF
      ENDIF      

      IF (IV1 .EQ. 14) IV1 = 12
      IF (BIG .LE. TINY) THEN
         TINY = D1MACH(1)
         MACHEP = D1MACH(4)
         BIG = D1MACH(2)
         VM(12) = MACHEP
         VX(12) = BIG
         VM(13) = TINY
         VX(13) = BIG
         VM(14) = MACHEP
         VM(17) = TINY
         VX(17) = BIG
         VM(18) = TINY
         VX(18) = BIG
         VX(20) = BIG
         VX(21) = BIG
         VX(22) = BIG
         VM(24) = MACHEP
         VM(25) = MACHEP
         VM(26) = MACHEP
         VX(28) = DSQRT(D1MACH(2))*16.
         VM(29) = MACHEP
         VX(30) = BIG
         VM(33) = MACHEP
      ENDIF
      M = 0
      I = 1
      K = 19
      DO 10 L = 1,25
         VK = V(K)
         IF (VK .LT. VM(I) .OR. VK .GT. VX(I)) M = K
         K = K + 1
         I = I + 1
         IF (I .EQ. 24) I = 33
 10   CONTINUE
         
      IF (IV(50) .NE.  25) THEN
         IV(1) = 51
         RETURN
      ENDIF

      IF ((IV(16).GT.0.OR.V(38).GT.0.D0).AND. IV1.EQ.12) THEN
         IF (M .NE. 0) IV(1) = M
         RETURN
      ENDIF
      DO 20 I=1,NP+1
         IF (D(I) .LE. 0.D0) M = 18
 20   CONTINUE

      IF (M .NE. 0) IV(1) = M

      RETURN
      END


