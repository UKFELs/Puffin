! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

MODULE MacrosGenNew

USE paratype
USE typempicomm
USE sddsPuffin
USE randomGauss
USE ParallelSetUp
USE macrosGen
!USE error_fn
!USE Functions
!USE particleFunctions
!USE typesAndConstants
!USE FileType
!USE IO

IMPLICIT NONE

CONTAINS


  SUBROUTINE genMacrosNew(i_total_electrons, &
       q_noise, & 
       x_1_grid, x_1_integral, &
       x_2_grid, x_2_integral, &
       x_3_grid, x_3_integral, &
       p_1_grid, p_1_integral, &
       p_2_grid, p_2_integral, &
       p_3_grid, p_3_integral, &
       s_number_macro, s_vol_element, max_av, &
       x_1_coord, x_2_coord,   &
       x_3_coord, p_1_vector,  &
       p_2_vector, p_3_vector)
!
! Calculate the electrons' position and Chi-Weight
! factor.
! Poisson Distribution is used for the random
! generation positioning when noise is present.
!
! i_total_electrons       -INPUT   - the total number
!                                    of electrons in
!                                    beam slice
! q_noise                 -INPUT   - noise?
! x_1_grids               -INPUT   - the grid in x
! x_2_grids (OPTIONAL)    -INPUT   - Grid points along the electron pulse in 2nd direction
! x_3_grids (OPTIONAL)    -INPUT   - Grid points along the electron pulse in 3rd directions
! p_1_grids (OPTIONAL)    -INPUT   - Grid points of the electron pulse in the 1st momentum space
! p_2_grids (OPTIONAL)    -INPUT   - Grid points of the electron pulse in the 2nd momentum space
! p_3_grids (OPTIONAL)    -INPUT   - Grid points of the electron pulse in the 3rd momentum space
! x_1_integral            -INPUT   - Integral under the density distribution in 1st direction
! x_2_integral (OPTIONAL) -INPUT   - Integral under the density distribution in 2nd direction
! x_2_integral (OPTIONAL) -INPUT   - Integral under the density distribution in 3rd direction
! p_1_integral (OPTIONAL) -INPUT   - Integral under the density distribution in 1st momentum space
! p_2_integral (OPTIONAL) -INPUT   - Integral under the density distribution in 2nd momentum space
! p_2_integral (OPTIONAL) -INPUT   - Integral under the density distribution in 3rd momentum space
! chi_bar                 -OUTPUT  - scaled weighting factor (=chichi*volume)
! s_number_macro          -OUTPUT  - number of real electrons per macro particles
! chichi                  -OUTPUT  - the weighting of macroparticles
! x_1_coord               -OUTPUT  - hold the final macroparticle position!
!--------------------------------------------------------

! If gamma is input as x_1, it will still have the correct number of electrons per macroparticle.
! It just need to be used with the supplied peak number in the data file.
! Probably don't want the parallel comm to get the peak number....????


    REAL(KIND=WP), INTENT(IN) ::  i_total_electrons
    LOGICAL ::  q_noise
    REAL(KIND=WP),INTENT(IN) ::  x_1_grid(:),x_1_integral(:)
    REAL(KIND=WP),INTENT(IN),OPTIONAL :: x_2_grid(:), x_3_grid(:)
    REAL(KIND=WP),INTENT(IN),OPTIONAL :: x_2_integral(:),&
         x_3_integral(:)
    REAL(KIND=WP),INTENT(IN),OPTIONAL :: p_1_grid(:),&
         p_2_grid(:), p_3_grid(:)
    REAL(KIND=WP),INTENT(IN),OPTIONAL :: p_1_integral(:), &
     p_2_integral(:), p_3_integral(:)

    
    !REAL(KIND=WP),DIMENSION(:), INTENT(IN)  ::  sigma
    !REAL(KIND=WP), INTENT(IN)               ::  pxstart,pxend,pxmean
    !REAL(KIND=WP), INTENT(IN)               ::  pystart,pyend,pymean
    REAL(KIND=WP), INTENT(OUT)  :: max_av
    REAL(KIND=WP),DIMENSION(:), INTENT(OUT)  ::  x_1_coord
    REAL(KIND=WP),DIMENSION(:), INTENT(OUT), OPTIONAL  ::  x_2_coord, x_3_coord
    REAL(KIND=WP),DIMENSION(:), INTENT(OUT), OPTIONAL  ::  p_1_vector, p_2_vector, p_3_vector
    REAL(KIND=WP),DIMENSION(:), INTENT(OUT) ::  s_number_macro , s_vol_element
!
! LOCAL VARIABLES
! i_total_number_macro        - Total number of macro particels
! np1                         - size of arrary for p_1
! np2                         - size of arrary for p_2
! np3                         - size of arrary for p_3
! nx1                         - size of arrary for x_1
! nx2                         - size of arrary for x_2
! nx3                         - size of arrary for x_3
! a,b,c                       - Loop counters
! i,j,k                       - Loop counters
! index,icount                - Loop counters
! s_mean                      - Mean number of real electrons
! s_macro                     - Number of real electrons using
!                               Poisson distribution if shot-noise
!                               is present else =s_mean
! s_spatial_mean              - Mean number of real electrons in volume space
! u                           - A number used in generate random numbers
! s_mean_number_macro         - Array of all s_mean
! s_spatial_macro             - Array for all s_spatial_mean
! s_vol_element               - Volume of each marco particle contains in
! qOKL                        - Local error flag
! zDataFileName               - Data file name
!******************************************************************
! Electrons co-ordinates locally in all 6 directions
!******************************************************************
!
! x_1_position     x_2_position    x_3_position
! p_1_position     p_2_position    p_3_position
!
!******************************************************************
! Space between two macro electrons
!******************************************************************
!
! x_1_del          x_2_del         x_3_del
! p_1_del          p_2_del         p_3_del
!
!******************************************************************
! Random value of all marco electrons in all 6 directions
!******************************************************************
!
! x_1_random          x_2_random         x_3_random
! p_1_random          p_2_random         p_3_random
!
    INTEGER(KIND=IP) :: i_total_number_macro
    INTEGER(KIND=IP) :: np1,np2,np3,nx1,nx2,nx3
    INTEGER(KIND=IP) :: a,b,c,i,j,k
    INTEGER(KIND=IPL) :: index,icount
    REAL(KIND=WP) :: s_mean,s_macro,s_spatial_mean,u
    REAL(KIND=WP) :: local_max_av
    REAL(KIND=WP),ALLOCATABLE,DIMENSION(:) :: s_mean_number_macro
    REAL(KIND=WP),ALLOCATABLE,DIMENSION(:) :: s_spatial_macro
    REAL(KIND=WP),ALLOCATABLE,DIMENSION(:) :: x_1_position,&
         x_1_del, x_1_random,x_2_position, x_2_del,&
         x_2_random, x_3_position, x_3_del, x_3_random
    REAL(KIND=WP),ALLOCATABLE,DIMENSION(:) ::  p_1_position, p_1_del,&
         p_1_random, p_2_position, p_2_del, p_2_random, & 
         p_3_random
	REAL(KIND=WP),ALLOCATABLE,DIMENSION(:) :: p_3_position, p_3_del
    INTEGER(KIND=IP) :: np3full(3)
    !REAL(KIND=WP) ::  radius,sLOne,sLTwo
    INTEGER(KIND=IP) :: error,nprocs,proc
    LOGICAL :: qOKL  	
    CHARACTER(1024_IP) :: zDataFileName

!     Determine the number of macroparticles in each dimension
!     If the dimension is not present, then the number of macroparticles
!     is set to one

    nx1=SIZE(x_1_integral)
    nx2=1
    nx3=1
    np1=1
    np2=1
    np3=1

!     Test parameters for input

    IF(PRESENT(x_2_integral)) nx2=SIZE(x_2_integral)
    IF(PRESENT(x_3_integral)) nx3=SIZE(x_3_integral)
    IF(PRESENT(p_1_integral)) np1=SIZE(p_1_integral)
    IF(PRESENT(p_2_integral)) np2=SIZE(p_2_integral)
    IF(PRESENT(p_3_integral)) np3=SIZE(p_3_integral)
	  IF(PRESENT(p_3_integral)) np3full=SIZE(p_3_integral)

!     Total number of macroparticles 

    i_total_number_macro=nx1*nx2*nx3*np1*np2*np3

!     Allocate the arrays

    ALLOCATE(x_1_position(nx1),x_1_del(nx1),x_1_random(i_total_number_macro))

    IF(PRESENT(x_2_grid)) ALLOCATE(x_2_position(nx2),x_2_del(nx2),x_2_random(i_total_number_macro))
    IF(PRESENT(x_3_grid)) ALLOCATE(x_3_position(nx3),x_3_del(nx3),x_3_random(i_total_number_macro))
    IF(PRESENT(p_1_grid)) ALLOCATE(p_1_position(np1),p_1_del(np1),p_1_random(i_total_number_macro))
    IF(PRESENT(p_2_grid)) ALLOCATE(p_2_position(np2),p_2_del(np2),p_2_random(i_total_number_macro))
    IF(PRESENT(p_3_grid)) ALLOCATE(p_3_position(np3),p_3_del(np3),p_3_random(i_total_number_macro))

!     If shot-noise present then generate random numbers (0<x<1) else 
!     x_1_random set to 0.5.

    IF (q_noise) THEN
       CALL init_random_seed()
       DO i=1,i_total_number_macro
          x_1_random(i)=RandomNoGenerator(u)
          IF(PRESENT(x_2_grid)) x_2_random(i)=RandomNoGenerator(u)
          IF(PRESENT(x_3_grid)) x_3_random(i)=RandomNoGenerator(u)
          IF(PRESENT(p_1_grid)) p_1_random(i)=RandomNoGenerator(u)
          IF(PRESENT(p_2_grid)) p_2_random(i)=RandomNoGenerator(u)
          IF(PRESENT(p_3_grid)) p_3_random(i)=RandomNoGenerator(u)
       ENDDO
    ELSE
       x_1_random=0.5_WP

       IF(PRESENT(x_2_grid)) x_2_random = 0.5_WP
       IF(PRESENT(x_3_grid)) x_3_random = 0.5_WP
       
       IF(PRESENT(p_1_grid)) p_1_random = 0.5_WP
       IF(PRESENT(p_2_grid)) p_2_random = 0.5_WP
       IF(PRESENT(p_3_grid)) p_3_random = 0.5_WP   
    ENDIF

!     Following loops sets up macroparticle mean positions and intervals 
!     based on grid-pts 

    DO i=1,nx1
       x_1_del(i)=x_1_grid(i+1)-x_1_grid(i)
       x_1_position(i)=x_1_grid(i)+x_1_del(i)/2.0_WP
    END DO

    IF(PRESENT(x_2_grid)) THEN
       DO i=1,nx2
          x_2_del(i)=x_2_grid(i+1)-x_2_grid(i)
          x_2_position(i)=x_2_grid(i)+x_2_del(i)/2.0_WP
       END DO
    END IF

    IF(PRESENT(x_3_grid)) THEN
       DO i=1,nx3
          x_3_del(i)=x_3_grid(i+1)-x_3_grid(i)
          x_3_position(i)=x_3_grid(i)+x_3_del(i)/2.0_WP
       END DO
    END IF

    IF(PRESENT(p_1_grid)) THEN
       DO i=1,np1
          p_1_del(i)=p_1_grid(i+1)-p_1_grid(i)
          p_1_position(i)=p_1_grid(i)+p_1_del(i)/2.0_WP
       END DO
    END IF

    IF(PRESENT(p_2_grid)) THEN
       DO i=1,np2
          p_2_del(i)=p_2_grid(i+1)-p_2_grid(i)
          p_2_position(i)=p_2_grid(i)+p_2_del(i)/2.0_WP
       END DO
    END IF

    IF(PRESENT(p_3_grid)) THEN
      DO i=1,np3
        p_3_del(i)=p_3_grid(i+1)-p_3_grid(i)
        p_3_position(i)=p_3_grid(i)+p_3_del(i)/2.0_WP
      END DO
    END IF

!     Allocate arrays

    ALLOCATE(s_mean_number_macro(i_total_number_macro))
    ALLOCATE(s_spatial_macro(i_total_number_macro))

!     Loop sets up macroparticles overall position (with random deviate)
!     and number of electrons

    nprocs=tProcInfo_G%size
    index=0_IPL
    icount=0_IPL
!   If (ttypempicomm_G%qROOT) PRINT *, 'noise',q_noise
    s_number_macro=0.0_WP
!radius=(x_1_grid(size(x_1_grid))-x_1_grid(1))/2.0_WP

!     Loop round processors to ensure different random numbers
!     Previously there was a problem when the process times
!     were all the same, and all processors were generating
!     identical random numbers....NO LONGER USED

!    DO proc = 0,nprocs-1
!    IF (tProcInfo_G%rank==proc) THEN
!If (ttypempicomm_G%qROOT) PRINT *, 'Radius= ', radius

  DO k=1,nx3
    DO j=1,nx2
       DO i=1,nx1
          DO c=1,np3
             DO b=1,np2
                DO a=1,np1

                  index=index+1_IPL	

                  s_mean= i_total_electrons*x_1_integral(i)
  
                  IF(PRESENT(x_2_grid)) THEN
                     s_mean=s_mean*x_2_integral(j)
                  END IF

                  IF(PRESENT(x_3_grid)) THEN 
                     s_mean=s_mean*x_3_integral(k)
                  END IF

                  s_spatial_mean=s_mean


                  IF(PRESENT(p_1_grid)) THEN 
                     s_mean=s_mean*p_1_integral(a)
                  END IF
      
                  IF(PRESENT(p_2_grid)) THEN 
                     s_mean=s_mean*p_2_integral(b)
                  END IF
      
                  IF(PRESENT(p_3_grid)) THEN 
                     s_mean=s_mean*p_3_integral(c)
                  END IF

!*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
! q_noise is an input, if it is TRUE then randomness will be added, else "number=m_number"
! Turns mean number weight of macroparticle into actual number weight (Poisson Distribution)
! see random.f90 on line 678
!*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
                                !========================================!
                                !     IMPORTATN NOTE -- MUST READ        !
                                !========================================!
                                !If s_macro=0 then the macro electron's  !
                                !position will remain unchanged.         !
                                !s_macro>0, the position will change     !
                                !according to the randomness             !
                                !generated previously.                   !
                                !If any macro particles' s_mean are      !
                                !small compare to the rest, this particle!
                                !will be remove, see CelectronGrid.f90   !
                                !Removal process starts on line 544      !
                                !========================================!

                  IF(q_noise) THEN
                     s_macro =random_Poisson(s_mean, .TRUE.)
                  ELSE
                     s_macro =s_mean  		       
                  ENDIF
 
                  IF (s_macro > 0 ) THEN
		       
                     icount=icount+1_IPL
				
                     x_1_coord(index)=x_1_position(i)+(x_1_random(index)-0.5_WP)*x_1_del(i)/SQRT(s_macro)
		      	      

                     IF(PRESENT(x_2_grid)) THEN
                        x_2_coord(index)=x_2_position(j)+(x_2_random(index)- 0.5_WP)*x_2_del(j)/SQRT(s_macro)
                     END IF
     
                     IF(PRESENT(x_3_grid)) THEN
                        x_3_coord(index)=x_3_position(k)+(x_3_random(index)- 0.5_WP)*x_3_del(k)/SQRT(s_macro)
                     END IF
     
                     IF(PRESENT(p_1_grid)) THEN
                        p_1_vector(index)=p_1_position(a)+(p_1_random(index)- 0.5_WP)*p_1_del(a)/SQRT(s_macro)
                     END IF
      
                     IF(PRESENT(p_2_grid)) THEN
                        p_2_vector(index)=p_2_position(b)+(p_2_random(index)- 0.5_WP)*p_2_del(b)/SQRT(s_macro)
                     END IF
      
                     IF(PRESENT(p_3_grid)) THEN
                        p_3_vector(index)=p_3_position(c)+(p_3_random(index)- 0.5_WP)*p_3_del(c)/SQRT(s_macro)
                     END IF

                  ELSE 
                     x_1_coord(index)=x_1_position(i)
		      
                     IF (PRESENT(x_2_grid)) THEN
                        x_2_coord(index)=x_2_position(j)
                     END IF

                     IF (PRESENT(x_3_grid)) THEN
                        x_3_coord(index)=x_3_position(k)
                     END IF

                     IF (PRESENT(p_1_grid)) THEN
                        p_1_vector(index)=p_1_position(a)
                     END IF
		      
                     IF (PRESENT(p_2_grid)) THEN
                        p_2_vector(index)=p_2_position(b)
                     END IF
		      
                     IF (PRESENT(p_3_grid)) THEN
                        p_3_vector(index)=p_3_position(c)
                     END IF
		      
                     s_macro = 0.0_WP
                  ENDIF
		     
                  s_number_macro(index)=s_macro
                  s_mean_number_macro(index)=s_mean
                  s_spatial_macro(index)=s_spatial_mean

!*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
! Calculate the element volume of each macro particle		
!*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+	 
    
                  s_vol_element(index)=x_1_del(i)
                  
                  IF (PRESENT(x_2_grid)) THEN
                     s_vol_element(index)=s_vol_element(index)*x_2_del(j)
                  END IF

                  IF (PRESENT(x_3_grid)) THEN
                     s_vol_element(index)=s_vol_element(index)*x_3_del(k)
                  END IF
                  
                END DO
             END DO
          END DO
       END DO
    END DO
  END DO

    IF (icount==0) THEN
       IF (tProcInfo_G%qROOT)  STOP "Error in GenMacros.f90, no macroparticles exist\!"
    ENDIF

! Calculate the scaled weighting (chi_bar) and weighting(chichi) of all macro particles

!     Local max...

  local_max_av = MAXVAL(s_spatial_macro/s_vol_element)

  max_av = local_max_av  ! FOR NOW!!!

!     Sum to global to get max across all processes

!  CALL MPI_ALLREDUCE(local_max_av, max_av, 1, MPI_DOUBLE_PRECISION, &
!       MPI_MAX, MPI_COMM_WORLD, error)

    DEALLOCATE(s_mean_number_macro,s_spatial_macro)
    DEALLOCATE(x_1_position,x_1_del,x_1_random)
    
    IF(PRESENT(x_2_grid)) DEALLOCATE(x_2_position,x_2_del,x_2_random)
    IF(PRESENT(x_3_grid)) DEALLOCATE(x_3_position,x_3_del,x_3_random)
!
    IF(PRESENT(p_1_grid)) DEALLOCATE(p_1_position,p_1_del,p_1_random)
    IF(PRESENT(p_2_grid)) DEALLOCATE(p_2_position,p_2_del,p_2_random)
    IF(PRESENT(p_3_grid)) DEALLOCATE(p_3_position,p_3_del,p_3_random)

  END SUBROUTINE genMacrosNew


END MODULE MacrosGenNew
