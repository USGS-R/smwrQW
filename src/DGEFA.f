************************************************************************
*
*     Subroutine DGEFA                                 Called by: FACINV
* 
c     factors a matrix by gaussian elimination.
c
c     dgefa is usually called by dgeco, but it can be called
c     directly with a saving in time if  rcond  is not needed.
c     (time for dgeco) = (1 + 9/n)*(time for dgefa) .
c
c     on entry
c
c        a       double precision(lda, n)
c                the matrix to be factored.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       an upper triangular matrix and the multipliers
c                which were used to obtain it.
c                the factorization can be written  a = l*u  where
c                l  is a product of permutation and unit lower
c                triangular matrices and  u  is upper triangular.
c
c        ipvt    integer(n)
c                an integer vector of pivot indices.
c
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas daxpy,dscal,idamax
*
************************************************************************
      SUBROUTINE DGEFA(A,LDA,N,IPVT)
*
*     subroutine args
*
      INTEGER*4 LDA,N,IPVT(*)
      DOUBLE PRECISION A(LDA,*)
*
*     local vars
*
      INTEGER*4 IDAMAX,J,K,KP1,L
      DOUBLE PRECISION T
*
*     gaussian elimination with partial pivoting     
*
      DO 60 K=1,N-1
         KP1 = K + 1
*
*        find pivot index, L
*
         L = IDAMAX(N-K+1,A(K,K)) + K - 1
         IPVT(K) = l
*
*        zero pivot implies this column already triangularized
*
         IF (A(L,K) .EQ. 0.D0) GOTO 60
*
*        interchange if necessary
*
         IF (L .NE. K) THEN
            T = A(L,K)
            A(L,K) = A(K,K)
            A(K,K) = T
         ENDIF
*
*        compute multipliers
*
         T = -1.D0/A(K,K)
         CALL DSCAL(N-K,T,A(K+1,K))
*
*        row elimination with column indexing
*
         DO 30 J=KP1,N
            T = A(L,J)
            IF (L .NE. K) THEN
               A(L,J) = A(K,J)
               A(K,J) = T
            ENDIF
            CALL DAXPY(N-K,T,A(K+1,K),A(K+1,J))
 30      CONTINUE
 60   CONTINUE

      IPVT(N) = N

      RETURN
      END
