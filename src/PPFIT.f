************************************************************************
*
*     Subroutine PPFIT                    Called by: PPCCTEST
*
*     Compute the linear regression between the smoothed Zs
*     and the actual Z scores for the 0.9, .95, and .99 pvalues.
*     This is directly accomplished by application of the QR decomp
*     of the matrix of 1s and the Z scores.
*
************************************************************************
      SUBROUTINE PPFIT(ZSMOOTH,MU,SIGMA)
*
*     SUBROUTINE ARGUMENTS
*
      DOUBLE PRECISION ZSMOOTH(3),MU,SIGMA
*
*     LOCAL VARS
*
      INTEGER I
      DOUBLE PRECISION QTB1, QTB2
*
*     Just plain ol' brute force multiplication:
*
      QTB1 = 0.D0
      DO I = 1,3
         QTB1 = QTB1 - 0.577350269189626D0 * ZSMOOTH(I)
      ENDDO
      QTB2 = 0.625724703684127D0 * ZSMOOTH(1)
      QTB2 = QTB2 + 0.141396876352102D0 * ZSMOOTH(2)
      QTB2 = QTB2 - 0.767121580036229D0 * ZSMOOTH(3)
      MU = -0.577350269189626D0 * QTB1
      MU = MU + 2.33419583910949D0 * QTB2
      SIGMA = -1.33312711041740D0 * QTB2
      RETURN
      END
