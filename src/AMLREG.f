************************************************************************
*
*     Subroutine AMLREG                     Called by: automdl, evalaml (R) 
*                                           LOADEST:   AMLLOAD2, CALIBR
*
*     compute nearly-unbiased AMLE parameter estimates
*
*     First-order bias is eliminated for S^2, not S.  Results are
*     equivalent to OLS estimates if there is no censoring (second-
*     order adjustment is made to eliminate first-order bias to
*     guarantee this result).
*
*     Parameter results are for the first NPAR parameters, followed by
*     S**2, the estimated variance of the residuals.  The standard
*     covariances wrt S**2 are given in CV, wrt S in SCV.
*
*     local vars
*     ----------
*     CV_A     Adj. Max. Likelihood Est. of the covariance matrix
*              w.r.t. S**2
*
************************************************************************
      SUBROUTINE AMLREG(NOBSC,NPAR,XLCAL,YLCAL,YD,CENSFLAG,XLIKE,
     &                  STDDEV,PARMLE,PARAML,BIAS,CV,SBIAS,SCV,CV_M,
     &     ierr)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      LOGICAL CENSFLAG(*)
      INTEGER*4 NOBSC,NPAR, ierr
      DOUBLE PRECISION XLIKE
      DOUBLE PRECISION BIAS(*),PARAML(*),PARMLE(*),SBIAS(*),STDDEV(*),
     &                 YD(*),YLCAL(*)
      DOUBLE PRECISION CV(MAXPARMS+1,*),CV_M(MAXPARMS+1,*),
     &                 SCV(MAXPARMS+1,*),XLCAL(MAXOBSC,*)
*
*     local vars
*
      INTEGER*4 K1,K2,K3,I,K,IK1,IK2,IK3,J,L
      DOUBLE PRECISION S,S2_M,S3_M,S3_A
      DOUBLE PRECISION CV_A(MAXPARMS+1,MAXPARMS+1),
     &                 H2(MAXPARMS+1,MAXPARMS+1),HK2(2,2),
     &                 SH2(MAXPARMS+1,MAXPARMS+1),SHK2(2,2),
     &                 X(MAXOBSC,MAXPARMS+1)
      DOUBLE PRECISION H3(MAXPARMS+1,MAXPARMS+1,MAXPARMS+1),
     &                 H3S(MAXPARMS+1,MAXPARMS+1,MAXPARMS+1),
     &                 HK3(2,2,2),HK3S(2,2,2),
     &                 SH3(MAXPARMS+1,MAXPARMS+1,MAXPARMS+1),
     &                 SH3S(MAXPARMS+1,MAXPARMS+1,MAXPARMS+1),
     &                 SHK3(2,2,2),SHK3S(2,2,2)
*
*     function declaration
*
      DOUBLE PRECISION PRED
*
*     
*
      DO 20 K1=1,NOBSC
         X(K1,NPAR+1) = 1.D0
         DO 10 K2 =1,NPAR
            X(K1,K2) = XLCAL(K1,K2)
 10      CONTINUE
 20   CONTINUE

      CALL TACIT_R(NOBSC,NPAR,YLCAL,YD,X,PARMLE,XLIKE,CENSFLAG)

      S = PARMLE(NPAR+1)
      PARMLE(NPAR+1) = S**2

      DO 50 K1=1,NPAR+1
         DO 40 K2=1,NPAR+1
            H2(K1,K2) = 0.D0
            SH2(K1,K2) = 0.D0
            DO 30 K3=1,NPAR+1
               H3(K1,K2,K3) = 0.D0
               SH3(K1,K2,K3) = 0.D0
               H3S(K1,K2,K3) = 0.D0
               SH3S(K1,K2,K3) = 0.D0
 30         CONTINUE
 40      CONTINUE
 50   CONTINUE

      DO 100 I=1,NOBSC
         CALL TACIT_D((YD(I)-PRED(NPAR,MAXOBSC,I,X,PARMLE))
     &                / S,SHK2,SHK3,SHK3S,HK2,HK3,HK3S)
*
*        H vars identify CV's corresponding to S**1; S vars identify
*        terms corresponding to S**2
*
         DO 90 K1=1,NPAR+1
            IK1 = 1 + K1/(NPAR+1)
            DO 80 K2=1,NPAR+1
               IK2 = 1 + K2/(NPAR+1)
               H2(K1,K2) = H2(K1,K2) - HK2(IK1,IK2)*X(I,K1)*X(I,K2)
               SH2(K1,K2) = SH2(K1,K2) - SHK2(IK1,IK2)*X(I,K1)*X(I,K2)
               DO 70 K3=1,NPAR+1
                  IK3 = 1 + K3/(NPAR+1)
                  H3(K1,K2,K3) = H3(K1,K2,K3)
     &                    + HK3(IK1,IK2,IK3)*X(I,K1)*X(I,K2)*X(I,K3)
                  H3S(K1,K2,K3) = H3S(K1,K2,K3)
     &                    + HK3S(IK1,IK2,IK3)*X(I,K1)*X(I,K2)*X(I,K3)
                  SH3(K1,K2,K3) = SH3(K1,K2,K3)
     &                    + SHK3(IK1,IK2,IK3)*X(I,K1)*X(I,K2)*X(I,K3)
                  SH3S(K1,K2,K3) = SH3S(K1,K2,K3)
     &                    + SHK3S(IK1,IK2,IK3)*X(I,K1)*X(I,K2)*X(I,K3)
 70            CONTINUE
 80         CONTINUE
 90      CONTINUE
 100  CONTINUE
*
*     invert matrices
*
      CALL MATINV(NPAR+1,H2,CV, ierr)
      if(ierr .gt. 0) return
      CALL MATINV(NPAR+1,SH2,SCV, ierr)
      if(ierr .gt. 0) return

      DO 140 I=1,NPAR+1
         BIAS(I) = 0.D0
         SBIAS(I) = 0.D0
         DO 130 J=1,NPAR+1
            DO 120 K=1,NPAR+1
               DO 110 L=1,NPAR+1
                  BIAS(I) = BIAS(I) + CV(I,J)*CV(K,L)
     &                               *(H3S(J,K,L)-H3(J,K,L)/2.D0)
                  SBIAS(I) = SBIAS(I) + SCV(I,J)*SCV(K,L)
     &                               *(SH3S(J,K,L)-SH3(J,K,L)/2.D0)
 110           CONTINUE
 120        CONTINUE
 130     CONTINUE
 140  CONTINUE

      DO 150 I=1,NPAR
         PARAML(I) = PARMLE(I) - S*BIAS(I)
 150   CONTINUE

      PARAML(NPAR+1) = S**2/(1.D0+BIAS(NPAR+1))
      S2_M = S**2
      S3_M = S**3
*
*     to get unbiased estimates of the powers of PARAML(NPAR+1), we
*     need to consider the variance of PARAML(NPAR+1)
*
      S3_A = PARAML(NPAR+1)**1.5D0/(1.D0+0.375D0*CV(NPAR+1,NPAR+1))
      CV_M(NPAR+1,NPAR+1) = S2_M*S2_M*CV(NPAR+1,NPAR+1)
      CV_A(NPAR+1,NPAR+1) = (PARAML(NPAR+1)**2
     &                  / (1.D0+CV(NPAR+1,NPAR+1)))
     &                  *CV(NPAR+1,NPAR+1)
      DO 170 I=1,NPAR
         CV_M(I,NPAR+1) = S3_M*CV(I,NPAR+1)
         CV_M(NPAR+1,I) = CV_M(I,NPAR+1)
         CV_A(I,NPAR+1) = S3_A*CV(I,NPAR+1)
         CV_A(NPAR+1,I) = CV_A(I,NPAR+1)
         DO 160 J=1,NPAR
            CV_M(I,J) = S2_M*CV(I,J)
            CV_A(I,J) = PARAML(NPAR+1)*CV(I,J)
 160     CONTINUE
         STDDEV(I) = SQRT(CV_A(I,I))
 170  CONTINUE

      STDDEV(NPAR+1) = SQRT(CV_A(NPAR+1,NPAR+1))      

      RETURN
      END
