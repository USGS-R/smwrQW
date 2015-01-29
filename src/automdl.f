      SUBROUTINE automdl(obs, Censin, detlim, nobs, zmu, zsig, nlt, 
     $     uselogin, ierr)
C
C       AUTOMDL:   Estimate of Summary Statistics for Data With
c               Multiple Detection Limits
C     _________________________________________________________
C MDL ESTIMATES MEAN,SD,PERCENTILES USING 
C MLE  (TIM'S AM METHOD).
c
c     Developed by Dennis Helsel
c     Version  5/19/88
c     Revised 7/12/94 by Tim Cohn
c     Revised 9/18/02 by Dave Lorenz to work with S-PLUS
c     Revised 9/18/02 by Dave Lorenz to allow user to set the value of alpha
c     Revised 3/28/12 by Dave Lorenz for R and only AMLE as MLE is easily
c             coded in R
c
C     ARGUMENTS:
C     INPUT:
C     OBS    double   The values of the data.
C     Censflag logical  Censoring flag T is censored
C     detlim double   Detection limit for each observation.
C     NOBS   integer  The number of observations.
C     OUTPUT:
C     ZMU    double   The mean of the data computed by each method.
C     ZSIG   double   The std dev of the data computed by each method.
C     NLT    integer  The number of less than values.
C     NOTES:
C     zmu and zsig return the estimates for the mean and standard deviation
C        for AMLE of back-transformed and of the log-transformed data.
c
C     local variables:

      INCLUDE 'fmodules.inc'
C
C     the dimensions in the following statement must be modified for the 
c     subroutine, to nobs and nqtile
      double precision obs(*),  detlim(*)
      double precision zmu(2), zsig(2)
      integer Censin(*), uselogin
      integer nobs, nlt, nqtile, ierr
c
c     Local variables
c
      logical CENSFLAG(MAXOBSC), USELOG
      REAL*8 X(MAXOBSC)/MAXOBSC*0.0/, XLT(MAXOBSC)/MAXOBSC*0.0/
      INTEGER M,I
      REAL*8 XX(MAXOBSC),FX(MAXOBSC,2)
      REAL*8 Y(MAXOBSC),XN,XNC,XLIKE,STDDEV(2)
      REAL*8 COV(2,2),BETA(2),B_AMLE(2),BIAS(2),CV(MAXPARMS+1,2)
      REAL*8 SBIAS(2),SCV(MAXPARMS+1,2),CV_M(MAXPARMS+1,2),
     $     MU,SIGMA,ZQ
      REAL*8 DL2(MAXOBSC),XSS(2)
      real test
C
C     convert data to format needed by the called routines
C     Includes converting integer 0/1 to logical FALSE/TRUE
C
      IERR=0
      M=0
      NLT=0
      if(uselogin .eq. 1) then
         USELOG = .TRUE.
      else
         USELOG = .FALSE.
      endif
      DO I=1,NOBS
         if(Censin(I) .eq. 1) then
           CENSFLAG(I) = .TRUE.
         else
           CENSFLAG(I) = .FALSE.
         endif
         IF(.NOT. CENSFLAG(I)) THEN
            M=M+1
            X(M) = OBS(I)
         ELSE
            NLT=NLT+1
            XLT(NLT) = OBS(I)
         ENDIF
      ENDDO
      test = float(NLT)/float(NOBS)
      IF (test .gt. 0.8) THEN
         IERR = -201
         IF (test .gt. 0.9) THEN
            IERR = 201
            RETURN
         ENDIF
      ENDIF
      IF (M .LE. 2) THEN
         IERR=202
         RETURN
      ENDIF
      CALL MOM2(X,M,XSS)
      IF (XSS(2) .LE. 1.0D-5) THEN
         IERR=203
         RETURN
      ENDIF
      if(uselog) then
         DO I=1,NOBS
            Y(I) = DLOG(OBS(I))
            DETLIM(I) = DLOG(DETLIM(I))
            FX(I,1) = 1.D0
         ENDDO
         DO 100 I=1,M
            XX(I)=X(I)
            DL2(I)=0.D0
 100     CONTINUE
         N=M+NLT
         DO 200 I=M+1,NOBS
            XX(I)=0.D0
            IM=I-M
            DL2(I)=XLT(IM)
 200     CONTINUE
      else
         DO I=1,NOBS
            Y(I) = OBS(I)
            FX(I,1) = 1.D0
         ENDDO
      endif
      CALL AMLREG(NOBS,1,FX,Y,DETLIM,CENSFLAG,XLIKE,STDDEV,BETA,B_AMLE,
     $     BIAS,CV,SBIAS,SCV,CV_M,IERR)
      IF(IERR .GT. 0) RETURN
      ZMU(1)=BETA(1)
      ZSIG(1)=DSQRT(BETA(2))
      if(uselog)
     $     CALL RBMOMNT(NOBS,XX,DL2,ZMU(1),ZSIG(1),ZMU(2),ZSIG(2))

      RETURN
      END

      subroutine mom4(x,n,s)
      real*8 x(n),s(*)
      double precision w,sum,ssq,scu,sfo,fn,std
      logical three,four
      three=.true.
      four=.true.
   10 continue
      s(1)=x(1)
      s(2)=0.0
      if (three) s(3)=0.0
      if (four) s(4)=0.0
      if (n.lt.2) return
      fn=dble(float(n))
      sum=0.0
      ssq=0.0
      if (three) scu=0.0
      if (four) sfo=0.0
      do 20 i=1,n
      w=dble(x(i))
      sum=sum+w
      ssq=ssq+w**2
      if (three) scu=scu+w**3
      if (four) sfo=sfo+w**4
   20 continue
      sum=sum/fn
      s(1)=sngl(sum)
      ssq=ssq/fn-sum**2
      if (ssq.le.0.0d0) return
      std=dsqrt(ssq)
      s(2)=sngl(std)
      if (.not.three.or.s(2).le.0.0) return
      scu=scu/fn-3.0d0*sum*ssq-sum**3
      s(3)=sngl(scu/(std**3))
      if (four)
     1   s(4)=sngl((sfo/fn-4.0d0*sum*scu-6.0d0*ssq*sum**2-sum**4)/(ssq**
     12))
      return
      entry mom3(x,n,s)
      three=.true.
      four=.false.
      go to 10
      entry mom2(x,n,s)
      three=.false.
      four=.false.
      go to 10
      end
