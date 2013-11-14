c
c*****************************************************************************
c
c     Subroutine ktau
c
c      Program to perform simple linear regression when both response and
c        predictor are subject to censoring.  A biscection search is
c        used to find the slope that puts a modified version of the
c        Kendall's tau correlation to zero.
c
c        Modified 7/30 to correct the direction of the indicators (Indi).
c
c*****************************************************************************
c 

      subroutine ktau(x, y, ixcens, iycens, num, betahat,
     +                lbound, ubound, dev, iter, bbs)
      integer num,i,j,l,iter,k
      integer ixcens(num),iycens(num),Indi,ind1,
     +        ind2,ind3,ind4
      double precision, dimension(num) :: tot
      double precision x(num),y(num),b(3),convrg,betahat,residi,
     +      residj,lbound,ubound,lb(3),ub(3),slope,chat,delta,
     +      theta,dev
      double precision tau(3),itot,ltau(3),utau(3)
      parameter(convrg = .000000001)

      delta = 0.01

c
c     Now the upper and lower bounds on the slope are read in from the
c     variables ubound and lbound.
c

      b(1) = lbound
      b(3) = ubound

c
c     Kendall's tau is evaluated at both bounds
c

      do 100 k = 1, 2
         
         itot = 0.0
         
         do 90 i = 1, num
            residi = y(i) - b(2*k-1)*x(i)
            do 80 j = 1, i
               residj = y(j) - b(2*k-1)*x(j)
               ind1 = Indi(x(i),x(j))
               ind2 = Indi(x(j),x(i))
               ind3 = Indi(residi,residj)
               ind4 = Indi(residj,residi)
               if(ixcens(i)*ixcens(j) .eq. 1) then
                  itot = itot + dble(ind1-ind2)*dble(iycens(i)*ind3-
     +                 iycens(j)*ind4)
               endif
 80         continue
 90      continue
         tau(2*k-1) = itot
 100  continue
      
c     
c     Now the bisection search is begun.  The midpoint of the two bounds
c     is found and Kendall's tau is evaluated there.  Then the midpoint
c     is matched up with the bound that has opposite sign, and these two
c     slopes are used as the bounds for the next iteration.
c

      l = 0
      
 120  l = l + 1
      b(2) = .5*(b(1) + b(3))
      itot = 0
      
      do 190 i = 1, num
         residi = y(i) - b(2)*x(i)
         do 180 j = 1, num
            residj = y(j) - b(2)*x(j)
            ind1 = Indi(x(i),x(j))
            ind2 = Indi(x(j),x(i))
            ind3 = Indi(residi,residj)
            ind4 = Indi(residj,residi)
            if(ixcens(i)*ixcens(j) .eq. 1) then
               itot = itot + dble(ind1-ind2)*dble(iycens(i)*ind3-
     +              iycens(j)*ind4)
            endif
 180     continue
 190  continue
      
      tau(2) = itot
      
      if(tau(1)*tau(2) .le. 0D0) then
         b(3) = b(2)
         tau(3) = tau(2)
      else
         b(1) = b(2)
         tau(1) = tau(2)
      endif

c
c     The loop is terminated when a Kendall's tau value of 0 is achieved,
c     the bounds become closer than the convergence criterion, or the
c     number of iterations exceeds the fixed limit on iterations.
c     
      
      if((b(3)-b(1)) .gt. convrg .and. l .lt. iter .and. tau(2)
     +     .ne. 0) then
         goto 120
      else if(tau(2) .eq. 0) then
         
         ub(1) = b(2)
         ub(3) = b(3)
         utau(1) = tau(2)
         utau(3) = tau(3)
         
 200     ub(2) = .5*(ub(1)+ub(3))
         
         itot = 0
         
         do 220 i = 1, num
            residi = y(i) - ub(2)*x(i)
            do 210 j = 1, i
               residj = y(j) - ub(2)*x(j)
               ind1 = Indi(x(i),x(j))
               ind2 = Indi(x(j),x(i))
               ind3 = Indi(residi,residj)
               ind4 = Indi(residj,residi)
               if(ixcens(i)*ixcens(j) .eq. 1) then
                  itot = itot + (ind1-ind2)*(iycens(i)*ind3-
     +                 iycens(j)*ind4)
               endif
 210        continue
 220     continue
         
         utau(2) = itot
         
         if (utau(2) .eq. 0) then
            ub(1) = ub(2)
            utau(1) = utau(2)
         else
            ub(3) = ub(2)
            utau(3) = utau(2)
         endif

         if(ub(3)-ub(1) .gt. convrg) goto 200

         lb(1) = b(1)
         lb(3) = b(2)
         ltau(1) = tau(1)
         ltau(3) = tau(2)

 300     lb(2) = .5*(lb(1) + lb(3))

         itot = 0

         do 320 i = 1, num
            residi = y(i) - lb(2)*x(i)
            do 310 j = 1, i
               residj = y(j) - lb(2)*x(j)
               ind1 = Indi(x(i),x(j))
               ind2 = Indi(x(j),x(i))
               ind3 = Indi(residi,residj)
               ind4 = Indi(residj,residi)
               if(ixcens(i)*ixcens(j) .eq. 1) then
                  itot = itot + (ind1-ind2)*(iycens(i)*ind3-
     +                iycens(j)*ind4)
               endif
 310        continue
 320     continue

         ltau(2) = itot

         if(ltau(2) .eq. 0) then
            lb(3) = lb(2)
            ltau(3) = ltau(2)
         else
            lb(1) = lb(2)
            ltau(1) = ltau(2)
         endif

         if(lb(3)-lb(1) .gt. convrg) goto 300

         betahat = .5*(ub(2) + lb(2))
      elseif(l .ge. iter) then
         betahat = b(2)
      else
         betahat = b(2)
      endif
      iter = l

c
c     Now the standard deviation is computed.  First the numerator is computed
c     and then the denominator.  The denominator can take on a range of values
c     depending on the value of delta.
c

      chat = 0.0

      do 340 i = 1, num
         tot(i) = 0.0
         residi = y(i) - betahat*x(i)
         do 330 j = 1, num
            residj = y(j) - betahat*x(j)
            ind1 = Indi(x(i),x(j))
            ind2 = Indi(x(j),x(i))
            ind3 = Indi(residi,residj)
            ind4 = Indi(residj,residi)
            if(ixcens(i)*ixcens(j) .eq. 1 .and. i .ne. j) then
               tot(i) = tot(i) + dble((ind1-ind2))*
     +              dble((iycens(i)*ind3-iycens(j)*ind4))
            endif
 330     continue
         chat = chat + (tot(i)/dble(num-1))**(2.0)
 340  continue
      chat = chat/(dble(num-1))

         itot = 0
         slope = betahat + (dble(num))**(delta - .5)
         do 360 i = 1, num
            residi = y(i) - slope*x(i)
            do 350 j = 1, i
               residj = y(j)-slope*x(j)
               ind1 = Indi(x(i),x(j))
               ind2 = Indi(x(j),x(i))
               ind3 = Indi(residi,residj)
               ind4 = Indi(residj,residi)
               if(ixcens(i)*ixcens(j) .eq. 1) then
                  itot = itot + (ind1-ind2)*(iycens(i)*ind3-
     +                   iycens(j)*ind4)
               endif
 350        continue
 360     continue
         theta = ((dble(num))**(.5 - delta))*(2.0/(dble(num*
     +              (num-1))))*dble(itot)
         dev = dsqrt((4.0 * chat /(theta**2.0))/(dble(num)))
      end

c***************************************************************************
c
c     Indi function subprogram
c         Given two double precision arguments it returns the integer 1
c         if the first argument is smaller, and the integer 0 if the
c         second argument is smaller.
c
c***************************************************************************

      function Indi(x,y)

      integer Indi
      double precision x,y

      if(x .lt. y) then
         Indi = 1
      else
         Indi = 0
      endif
      return
      end
