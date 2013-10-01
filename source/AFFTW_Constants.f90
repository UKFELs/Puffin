MODULE FFTW_CONSTANTS            		

!---------------------------------------------------------------------
! Module contains the constants needed for FFTW transforms.
!
! Written by:- Lawrence Campbell
!	       University of Strathclyde
!	       January '09
! Values for constants as defined in FFTW manual.
!---------------------------------------------------------------------
	    
	    INTEGER FFTW_FORWARD,FFTW_BACKWARD
            PARAMETER (FFTW_FORWARD=-1,FFTW_BACKWARD=1)
      
            INTEGER FFTW_REAL_TO_COMPLEX,FFTW_COMPLEX_TO_REAL
            PARAMETER (FFTW_REAL_TO_COMPLEX=-1,FFTW_COMPLEX_TO_REAL=1)
      
            INTEGER FFTW_ESTIMATE,FFTW_MEASURE
            PARAMETER (FFTW_ESTIMATE=0,FFTW_MEASURE=1)
      
            INTEGER FFTW_OUT_OF_PLACE,FFTW_IN_PLACE,FFTW_USE_WISDOM
            PARAMETER (FFTW_OUT_OF_PLACE=0)
            PARAMETER (FFTW_IN_PLACE=8,FFTW_USE_WISDOM=16)
      
            INTEGER FFTW_THREADSAFE
            PARAMETER (FFTW_THREADSAFE=128)  
	    
	    INTEGER FFTW_TRANSPOSED_ORDER,FFTW_NORMAL_ORDER
	    PARAMETER(FFTW_TRANSPOSED_ORDER = 1,FFTW_NORMAL_ORDER = 0)
	    
	    INTEGER USE_WORK
	    PARAMETER(USE_WORK=1)
	   
END MODULE FFTW_CONSTANTS
