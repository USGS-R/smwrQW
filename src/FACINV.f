************************************************************************
*
*     Subroutine FACINV                       Called by: MATCALC, MATINV
*
*     factor and invert matrix A by gaussian elimination.
*
*     Based on Linpack routines DGECO and DGEDI (versions dated
*     08/14/78; Cleve Moler, U. of New Mexico, Argonne National Lab);
*     modified to eliminate computation of determinant.
*
*     local vars
*     ----------
*     IPVT    pivot indices
*     WORK    work space whose contents are usually unimportant. If A
*             is close to a singular matrix, then WORK is an approximate
*             null vector in the sense that norm(A*WORK) =
*             rcond*norm(A)*norm(WORK)
*
*     subroutine arguments
*     --------------------
*     A       matrix to be factored and inverted
*     LDA     leading dimension of A
*     NPAR    the order A
*
************************************************************************
      SUBROUTINE FACINV(A,LDA,NPAR, ierr)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 LDA,NPAR, ierr
      DOUBLE PRECISION A(LDA,*)
*
*     local vars
*
      INTEGER*4 I,J,K,KB,IPVT(MAXPARMS+1)
      DOUBLE PRECISION ANORM,EK,S,SM,T,WK,WKM,YNORM
      DOUBLE PRECISION WORK(MAXPARMS+1)
*
*     function declarations
*
      DOUBLE PRECISION DASUM,DDOT
*
*     compute 1-norm of A
*
      ANORM = 0.D0
      DO 10 I=1,NPAR
         ANORM = DMAX1(ANORM,DASUM(NPAR,A(1,I)))
   10 CONTINUE
*
*     factor
*
      CALL DGEFA(A,LDA,NPAR,IPVT)
*
*     rcond = 1/(norm(A)*(estimate of norm(inverse(A)))) .
*     estimate = norm(work)/norm(y) where a*work=y and trans(a)*y=e.
*     trans(a) is the transpose of A .  The components of e are chosen
*     to cause maximum local growth in the elements of w where
*     trans(u)*w = e .  The vectors are frequently rescaled to avoid
*     overflow.
*
*     solve trans(u)*w = e
*
      EK = 1.D0
      DO 20 I=1,NPAR
         WORK(I) = 0.D0
 20   CONTINUE

      DO 50 K=1,NPAR
         IF (WORK(K) .NE. 0.D0) EK = DSIGN(EK,-WORK(K))
         IF (DABS(EK-WORK(K)) .GT. DABS(A(K,K))) THEN
            S = DABS(A(K,K))/DABS(EK-WORK(K))
            CALL DSCAL(NPAR,S,WORK)
            EK = S*EK
         ENDIF
         WK = EK - WORK(K)
         WKM = -EK - WORK(K)
         S = DABS(WK)
         SM = DABS(WKM)
         IF (A(K,K) .NE. 0.D0) THEN
            WK = WK/A(K,K)
            WKM = WKM/A(K,K)
         ELSE
            WK = 1.D0
            WKM = 1.D0
         ENDIF
         
         DO 30 I=K+1,NPAR
            SM = SM + DABS(WORK(I)+WKM*A(K,I))
            WORK(I) = WORK(I) + WK*A(K,I)
            S = S + DABS(WORK(I))
 30      CONTINUE
         IF (S .LT. SM) THEN
            T = WKM - WK
            WK = WKM
            DO 40 I=K+1,NPAR
               WORK(I) = WORK(I) + T*A(K,I)
 40         CONTINUE
         ENDIF

         WORK(K) = WK
 50   CONTINUE

      S = 1.D0/DASUM(NPAR,WORK)
      CALL DSCAL(NPAR,S,WORK)
*
*     solve trans(l)*y = w
*
      DO 60 KB=1,NPAR
         K = NPAR + 1 - KB
         IF (K .LT. NPAR)
     &      WORK(K) = WORK(K) + DDOT(NPAR-K,A(K+1,K),WORK(K+1))
         IF (DABS(WORK(K)) .GT. 1.D0) THEN
            S = 1.D0/DABS(WORK(K))
            CALL DSCAL(NPAR,S,WORK)
         ENDIF
         T = WORK(IPVT(K))
         WORK(IPVT(K)) = WORK(K)
         WORK(K) = T
 60   CONTINUE
      S = 1.D0/DASUM(NPAR,WORK)
      CALL DSCAL(NPAR,S,WORK)
      YNORM = 1.D0
*
*     solve l*v = y
*
      DO 70 K=1,NPAR
         T = WORK(IPVT(K))
         WORK(IPVT(K)) = WORK(K)
         WORK(K) = T
         IF (K .LT. NPAR) CALL DAXPY(NPAR-K,T,A(K+1,K),WORK(K+1))
         IF (DABS(WORK(K)) .GT. 1.D0) THEN
            S = 1.D0/DABS(WORK(K))
            CALL DSCAL(NPAR,S,WORK)
            YNORM = S*YNORM
         ENDIF
 70   CONTINUE
      S = 1.D0/DASUM(NPAR,WORK)
      CALL DSCAL(NPAR,S,WORK)
      YNORM = S*YNORM
*
*     solve  u*work = v
*
      DO 80 KB=1,NPAR
         K = NPAR + 1 - KB
         IF (DABS(WORK(K)) .GT. DABS(A(K,K))) THEN
            S = DABS(A(K,K))/DABS(WORK(K))
            CALL DSCAL(NPAR,S,WORK)
            YNORM = S*YNORM
         ENDIF
         IF (A(K,K) .NE. 0.D0) THEN
            WORK(K) = WORK(K)/A(K,K)
         ELSE
            WORK(K) = 1.D0
         ENDIF
         T = -WORK(K)
         CALL DAXPY(K-1,T,A(1,K),WORK(1))
 80   CONTINUE
*
*     make znorm = 1.0
*
      S = 1.D0/DASUM(NPAR,WORK)
      CALL DSCAL(NPAR,S,WORK)
      YNORM = S*YNORM
*
*     rcond (=YNORM/ANORM) is an estimate of the reciprocal condition of
*     A.  For the system a*x = b , relative perturbations in a and b of
*     size epsilon may cause relative perturbations in x of size
*     epsilon/rcond.
*
*     check to see if matrix is singular. If rcond is
*     effectively zero, then XTX may be singular to working precision.
*     rcond is zero if exact singularity is detected or underflow
*     occurs.
*
      ierr=0
      IF (ANORM .EQ. 0.D0 .OR. (1.D0+YNORM/ANORM.EQ.1.D0))
     &     ierr=713
      if(ierr .gt. 0) return
*
*     compute matrix inverse using the factors computed above
*
*     at this point, A is an upper triangular matrix that includes the
*     multipliers which were used to obtain it.  The factorization can
*     be written A = l*u  where l  is a product of permutation and unit
*     lower triangular matrices and u is upper triangular.
*
*     error condition: division by zero will occur if the input
*     factor contains a zero on the diagonal and the inverse is
*     requested. It will not occur if the subroutines are called
*     correctly and if DGECO has set rcond .gt. 0.0.
*
*     compute inverse(u)     
*
      DO 100 I=1,NPAR
         A(I,I) = 1.D0/A(I,I)
         T = -A(I,I)
         CALL DSCAL(I-1,T,A(1,I))
         DO 90 J=I+1,NPAR
            T = A(I,J)
            A(I,J) = 0.D0
            CALL DAXPY(I,T,A(1,I),A(1,J))
 90      CONTINUE
 100  CONTINUE
*     
*     form inverse(u)*inverse(l)
*
      DO 130 I=1,NPAR-1
         K = NPAR - I
         DO 110 J=K+1,NPAR
            WORK(J) = A(J,K)
            A(J,K) = 0.D0
 110     CONTINUE
         DO 120 J=K+1,NPAR
            T = WORK(J)
            CALL DAXPY(NPAR,T,A(1,J),A(1,K))
 120     CONTINUE
         IF (IPVT(K) .NE. K) CALL DSWAP(NPAR,A(1,K),A(1,IPVT(K)))
 130   CONTINUE

      RETURN
      END
