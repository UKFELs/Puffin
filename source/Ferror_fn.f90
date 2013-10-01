MODULE error_fn

USE typesAndConstants
USE nrutil

IMPLICIT NONE
CONTAINS

!********************************************************
!*+*Return the error function*+*!
FUNCTION erf(x)

  IMPLICIT NONE

  REAL(KIND=WP),INTENT(IN) :: x
  REAL(KIND=WP) :: erf

  erf=gammp(0.5_WP,x**2)

  IF (x<0.0) erf=-erf

END FUNCTION erf
!********************************************************
!*+*Return the complementary error function*+*!
FUNCTION erfc(x)

  IMPLICIT NONE

  REAL(KIND=WP),INTENT(IN)   :: x
  REAL(KIND=WP)		   :: erfc

  erfc=merge(1.0_WP+gammp(0.5_WP,x**2),gammq(0.5_WP,x**2),x<0.0)

END FUNCTION erfc
!********************************************************
FUNCTION erfi(Y)			
!-----------------------
! Inverse error function
!-----------------------
  REAL(KIND=WP),INTENT(IN)   :: Y
  REAL(KIND=WP)		   :: erfi
 
!---
! constants and parameters
!---
  INTEGER(KIND=IP)    :: maxit, iter
  REAL(KIND=WP)       :: eps
  REAL(KIND=WP)       :: const
  REAL(KIND=WP)       :: Y0, Y1, DY0, DY1
  REAL(KIND=WP)       :: Derfi

  maxit = 20_IP
  eps   = 0.0000001_WP
  const = sqrt(pi)/2.0_WP     

!---
! very small value
!---

  If(abs(Y).LE.eps) then
     erfi = Const*Y
     iter = 1_IP
     Return
  End If

!---
! Newton iterations
!---

  If(abs(Y).LT.1.0_WP) then

     erfi  = const*abs(Y)
     Y0    = ERF(0.9_WP*ERFI)
     Derfi = 0.1_WP*erfi

     Do iter=1,maxit
        Y1  = 1.0_WP - erfc(erfi)
        DY1 = abs(Y) - Y1
        If (abs(DY1).LT.eps) Go to 99
        DY0   = Y1 - Y0
        Derfi = Derfi*DY1/DY0
        Y0    = Y1
        erfi  = erfi + Derfi
        If(abs(Derfi/erfi).LT.eps) Go to 99
     End Do

  End If

!-----------
! unfeasible
!-----------

  Iter = 0_IP     ! Did not converge
  erfi = 0.0_WP

  Return

  99  Continue

  If(Y.LT.0.0_WP) erfi = -erfi

!-----
! Done
!-----
  Return
End Function erfi
!********************************************************
FUNCTION gammp(a,x)

  IMPLICIT NONE

  REAL(KIND=WP),INTENT(IN)			:: a,x
  REAL(KIND=WP)					:: gammp

  CALL assert(x>=0.0, a>0.0,'gammp args')

  IF (x<a+1.0_WP) THEN
     gammp=gser_s(a,x)
  ELSE
     gammp=1.0_WP-gcf(a,x)
  ENDIF

END FUNCTION gammp
!********************************************************
FUNCTION gammq(a,x)
  
  IMPLICIT NONE

  REAL(KIND=WP),INTENT(IN) :: a,x
  REAL(KIND=WP)	:: gammq

  CALL assert(x>=0.0, a>0.0,'gammq args')

  IF (x<a+1.0_WP) THEN
     gammq=1.0_WP-gser_s(a,x)
  ELSE
     gammq=gcf(a,x)
  ENDIF
  
END FUNCTION gammq
!********************************************************
FUNCTION gser_s(A,X,GLN)

  REAL(KIND=WP),INTENT(IN) :: A,X
  REAL(KIND=WP),OPTIONAL,INTENT(OUT) :: GLN

  INTEGER(KIND=IP) :: ITMAX,N
  REAL(KIND=WP)	:: gser_s,EPS,AP,SUMM,DEL

  ITMAX=100_IP
  EPS=epsilon(X)
  
  IF (X == 0.0) THEN
     gser_s=0.0_WP
     RETURN
  ENDIF

  AP=A
  SUMM=1.0_WP/A
  DEL=SUMM

  DO N=1,ITMAX
     AP=AP+1.0_WP
     DEL=DEL*X/AP
     SUMM=SUMM+DEL
     
     IF (ABS(DEL) < ABS(SUMM)*EPS) EXIT
  ENDDO

  IF (N> ITMAX) CALL nrerror('A too large,ITMAX too small in GSER')

  IF (present(GLN)) THEN

     GLN=gammln(A)
     gser_s=SUMM*EXP(-X+A*LOG(X)-GLN)
  ELSE
     gser_s=SUMM*EXP(-X+A*LOG(X)-gammln(A))
  ENDIF

END FUNCTION gser_s
!********************************************************
FUNCTION gcf(a,x,gln)

  REAL(KIND=WP),INTENT(IN)			:: a,x
  REAL(KIND=WP),OPTIONAL,INTENT(OUT)		:: gln
  REAL(KIND=WP)					:: gcf,EPS,FPMIN
  REAL(KIND=WP)					:: an,b,c,d,del,h
  INTEGER(KIND=IP)				:: ITMAX,i

  ITMAX=100_IP
  EPS=epsilon(x)
  FPMIN=tiny(x)/EPS

  IF (x==0.0_WP) THEN
     gcf=1.0_WP
     RETURN
  ENDIF

  b=x+1.0_WP-a
  c=1.0_WP/FPMIN
  d=1.0_WP/b
  h=d

  DO i=1,ITMAX
     an=-i*(i-a)
     b=b+2.0_WP
     d=an*d+b
     IF (ABS(d)<FPMIN) d=FPMIN
     c=b+an/c
     IF (ABS(c)<FPMIN) c=FPMIN
     d=1.0_WP/d
     del=d*c
     h=h*del
     IF (ABS(del-1.0_WP) <=EPS) EXIT
  ENDDO

  IF (i> ITMAX) CALL nrerror ('a too large, ITMAX too small in gcf')
  IF (present(gln)) THEN
     gln=gammln(a)
     gcf=EXP(-x+a*log(x)-gln)*h
  ELSE
     gcf=EXP(-x+a*log(x)-gammln(a))*h
  ENDIF
  
END FUNCTION gcf
!********************************************************
FUNCTION gammln(xx)

  USE paratype

  IMPLICIT NONE

  REAL(KIND=WP),INTENT(IN) :: xx
  REAL(KIND=WP) :: gammln,tmp,ser,x,y,stp
  REAL(KIND=WP),DIMENSION(6) :: coef
  
  INTEGER(KIND=IP) :: i

  stp=2.5066282746310005_WP
  coef=(/76.18009172947146_WP,-86.50532032941677_WP,24.01409824083091_WP, &
       -1.231739572450155_WP,0.1208650973866179E-2_WP,-0.5395239384953E-5_WP /)

  IF (xx==0) RETURN

  CALL assert(xx > 0.0,'gammln_s arg')

  x=xx
  tmp=x+5.5_WP
  tmp=(x+0.5_WP)*log(tmp)-tmp
  ser=1.000000000190015_WP
  y=x
  DO i=1,size(coef)
     y=y+1.0_WP
     ser=ser+coef(i)/y
  ENDDO
  gammln=tmp+log(stp*ser/x)

END FUNCTION gammln

END MODULE error_fn
