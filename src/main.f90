!-------------------------------------------------------
!
! main.f90
!
! Description:
! ============
!	Simple test program to inspect the CRTM CloudCoeff 
!	binary coefficient files that contain the scattering
! properties for water cloud particles.
! For a definition of the CloudCoeff_type TYPE, please
! look at CloudCoeff_Define.f90.
!
! SIDE-EFFECTS:
! =============
! Overwrites all existing ASCII files in the folder output. 
!
!	Record of Revisions:
! ====================
! 
!	Date: 	    Author:        Description:
! =====       =======        ============
! 2018-09-04  P. Stegmann    Original code 
! 2018-09-06  P. Stegmann    Added ASCII file output
!                            Added particle type selection
! 2018-09-10  P. Stegmann    Completed ASCII file output 
!                            Added solid hydrometeor output
!                            Added IR SSP output 
! 2018-09-11  P. Stegmann    Endianness check of CloudCoeff.bin 
!
!
! Copyright © 2018 Patrick Stegmann
!
! This file is part of INSPECT_CloudCoeff.
!
! INSPECT_CloudCoeff is free software:
! you can redistribute it and/or modify it under 
! the terms of the Apache License as published by
! the Apache Software Foundation, either version 2.0
! of the License, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful,
! but WITHOUT ANY WARRANTY; without even the implied 
! warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
! PURPOSE.  See the Apache License for more details.
!
! You should have received a copy of the Apache 2.0 
! License along with this program. If not, 
! see <https://www.apache.org/licenses/LICENSE-2.0>.
!
!-------------------------------------------------------


!-------------------------------------------------------

PROGRAM inspectTauCoeff
  

    ! ============================================================================
    ! **** ENVIRONMENT SETUP FOR RTM USAGE ****
    !
    ! Module usage
    USE CRTM_Module
    USE CloudCoeff_Binary_IO, ONLY: CloudCoeff_Binary_InquireFile, &
                                  CloudCoeff_Binary_ReadFile, &
                                  CloudCoeff_Binary_WriteFile

    USE CloudCoeff_Define,    ONLY: CloudCoeff_type, &
                                  CloudCoeff_Associated, &
                                  CloudCoeff_Destroy, &
                                  CloudCoeff_Inspect

    ! Disable all implicit typing
    IMPLICIT NONE
    ! ============================================================================

    ! String lengths
    INTEGER, PARAMETER :: ML = 256   ! Error message length
    INTEGER, PARAMETER :: SL = 500  ! Maximum length for path+filenames

    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CloudCoeff test read'

    ! Arguments
    INTEGER       :: Process_ID
    INTEGER       :: Output_Process_ID  

    INTEGER :: err_stat
    INTEGER :: Destroy_Status
    LOGICAL :: noisy
    LOGICAL :: Quiet
    LOGICAL :: selection
    INTEGER :: particle_selector, &
               temp_selector, &
               freq_selector, &
               rad_selector

    ! Local variables
    CHARACTER(ML)   :: msg, pid_msg
    CHARACTER(SL)   :: Default_CloudCoeff_File, CloudCoeff_File
    CHARACTER(SL)   :: Filename
    CHARACTER(SL)   :: File_Path
    CHARACTER(SL)   :: FP_output



    INTEGER ::   n_MW_Frequencies, &
                 n_MW_Radii      , &
                 n_IR_Frequencies, &
                 n_IR_Radii      , &
                 n_Temperatures  , &
                 n_Densities     , &
                 n_Legendre_Terms, &
                 n_Phase_Elements, &
                 Release         , &
                 Version         

    INTEGER :: ii, jj ! Iterators


    REAL, DIMENSION(31,10) :: w_S_MW, g_S_MW, ke_S_MW
    REAL, DIMENSION(31,10,38) :: pcoeff_S_MW
    CHARACTER(ML) :: strde, strfre
    INTEGER, DIMENSION(10) :: r

    !data r/2, 3, 4, 5, 8, 10, 15, 20, 50, 100/  !! effective radius for IR
    DATA r/5, 15, 30, 50, 100, 300, 500, 800, 1000, 1500/  !! effective radius for MW


  ! ---------------------------------
  ! The shared cloud coefficient data
  ! ---------------------------------
  TYPE(CloudCoeff_type), TARGET, SAVE :: CloudC

    ! Set up
    err_stat = SUCCESS

    Default_CloudCoeff_File    = 'CloudCoeff_orig.bin'


    Filename  = Default_CloudCoeff_File
    File_Path  = '../fix/'
    FP_output = './output/'

    ! ...Add the file path
    CloudCoeff_File = TRIM(ADJUSTL(File_Path))//TRIM(Filename)
   
    ! Inquire the content of the CloudCoeff data file
    err_stat = CloudCoeff_Binary_InquireFile(&
                 CloudCoeff_File, &
                 n_MW_Frequencies = n_MW_Frequencies, &
                 n_MW_Radii       = n_MW_Radii      , &
                 n_IR_Frequencies = n_IR_Frequencies, &
                 n_IR_Radii       = n_IR_Radii      , &
                 n_Temperatures   = n_Temperatures  , &
                 n_Densities      = n_Densities     , &
                 n_Legendre_Terms = n_Legendre_Terms, &
                 n_Phase_Elements = n_Phase_Elements, &
                 Release          = Release         , &
                 Version          = Version           )

    ! Write General Info about the CloudCoeff.bin to ASCII file.
    OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//TRIM('General_Info.txt'))
    WRITE(1,*)   "n_MW_Frequencies = ", n_MW_Frequencies, NEW_LINE('A'),&
                 "n_MW_Radii = ", n_MW_Radii, NEW_LINE('A'),&
                 "n_IR_Frequencies = ", n_IR_Frequencies, NEW_LINE('A'),&
                 "n_IR_Radii = ", n_IR_Radii, NEW_LINE('A'),&
                 "n_Temperatures = ", n_Temperatures, NEW_LINE('A'),&
                 "n_Densities = ", n_Densities, NEW_LINE('A'),&
                 "n_Legendre_Terms = ", n_Legendre_Terms, NEW_LINE('A'),&
                 "n_Phase_Elements = ", n_Phase_Elements, NEW_LINE('A'),&
                 "Release: ", Release, NEW_LINE('A'),&
                 "Version: ", Version
    CLOSE(1)


    ! Read the CloudCoeff data file
    err_stat = CloudCoeff_Binary_ReadFile( &
                CloudCoeff_File, &
                 CloudC, &
                 Quiet = .NOT. noisy )
    IF ( err_stat /= SUCCESS ) THEN
      WRITE( msg,'("Error reading CloudCoeff file ",a)') TRIM(CloudCoeff_File)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
    ELSE
      WRITE( msg,'("Success reading CloudCoeff file ",a)') TRIM(CloudCoeff_File)
    END IF

    !-------------------------------------------------------
    !
    ! Output CloudCoeff data to file:
    !
    !-------------------------------------------------------
    
    !-------------------------------------------------------
    ! Frequencies
    !-------------------------------------------------------

    ! MW Frequencies:
    OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Frequencies.txt')
    WRITE(1,*) 'CRTM MW scattering property frequencies in units of [Giga-Hertz].'
    DO ii = 1,n_MW_Frequencies
        WRITE(1,*) "MW Frequency: ", CloudC%Frequency_MW(ii)
    END DO
    CLOSE(1)

    ! IR Frequencies:
    OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Frequencies.txt')
    WRITE(1,*) 'CRTM IR scattering property frequencies in units of [Giga-Hertz].'
    DO ii = 1,n_IR_Frequencies
        WRITE(1,*) "IR Frequency: ", CloudC%Frequency_IR(ii)
    END DO
    CLOSE(1)

    !-------------------------------------------------------
    ! Scattering Particle Effective Radii 
    !-------------------------------------------------------

    ! MW Radii:
    OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Radii.txt')
    WRITE(1,*) 'CRTM MW scattering property particle effective radii in units of [micrometer].'
    DO ii = 1,n_MW_Radii
        WRITE(1,*) "MW Radius: ", CloudC%Reff_MW(ii)
    END DO
    CLOSE(1)

    ! IR Radii:
    OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Radii.txt')
    WRITE(1,*) 'CRTM IR scattering property particle effective radii in units of [micrometer].'
    DO ii = 1,n_IR_Radii
        WRITE(1,*) "MW Radius: ", CloudC%Reff_IR(ii)
    END DO
    CLOSE(1)

    !-------------------------------------------------------
    ! Temperatures:
    !-------------------------------------------------------

    OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'Water_Temperatures.txt')
    WRITE(1,*) 'CRTM liquid water particle temperatures in units of [Kelvin].'
    DO ii = 1,n_Temperatures
        WRITE(1,*) "Temperature: ", CloudC%Temperature(ii)
    END DO
    CLOSE(1)

    !-------------------------------------------------------
    ! Particle Densities:
    !-------------------------------------------------------

    OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'Particle_Densities.txt')
    WRITE(1,*) 'CRTM scattering particle density in units of [?]'
    DO ii = 1,n_Densities
        WRITE(1,*) "Density: ", CloudC%Density(ii)
    END DO
    CLOSE(1)

    !-------------------------------------------------------
    !
    ! CRTM Bulk Scattering Properties:
    !
    ! Abreviations: 
    !   MW:   Microwave
    !   IR:   Infrared
    !   Reff: Effective radius
    !   ke:   Extinction coefficient
    !   w:    Single scatter albedo
    !   g:    Asymmetry parameter
    !   L:    Liquid phase
    !   S:    Solid phase
    !
    !-------------------------------------------------------
    
    !-------------------------------------------------------
    ! Query Particle type as defined by CRTM
    !-------------------------------------------------------

    WRITE(*,*) "Please select the particle type by number:"
    WRITE(*,*) "1: WATER"
    WRITE(*,*) "2: RAIN"
    WRITE(*,*) "3: ICE"
    WRITE(*,*) "4: GRAUPEL"
    WRITE(*,*) "5: SNOW"
    WRITE(*,*) "6: HAIL"
    selection = .FALSE.
    particle_selector = 0

    select_particle: DO WHILE (selection .EQV. .FALSE.)
      READ(*,*) particle_selector
      SELECT CASE (particle_selector)

    !-------------------------------------------------------
    ! WATER
    !-------------------------------------------------------

      CASE(1) ! WATER
        selection = .TRUE.
        
        WRITE(*,*) "Please select a temperature by number:"
        WRITE(*,*) "1:    263.16"     
        WRITE(*,*) "2:    273.16"     
        WRITE(*,*) "3:    282.00"     
        WRITE(*,*) "4:    290.00"     
        WRITE(*,*) "5:    300.00"
        READ(*,*) temp_selector
        IF (temp_selector < 1 .OR. temp_selector > 5) THEN
          STOP "Wrong temperature selection"
        END IF 

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Extinction_Coefficients.txt')
          WRITE(1,*) CloudC%ke_L_MW(:,:,temp_selector)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Albedo.txt')  
          WRITE(1,*) CloudC%w_L_MW(:,:,temp_selector)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Asymmery_factor.txt')
          WRITE(1,*) CloudC%g_L_MW(:,:,temp_selector)
        CLOSE(1)

        WRITE(*,*) "To retrieve the corresponding MW Legendre coefficients,"
        WRITE(*,*) "Please select the frequency as an integer between 1 and ", n_MW_Frequencies
        READ(*,*) freq_selector
        WRITE(*,*) "and the effective radius as an integer between 1 and ", n_MW_Radii
        READ(*,*) rad_selector
        ! Write MW Legendre coefficients to file.
        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//"Legendre_terms_MW.txt")
        DO ii = 0,n_Legendre_Terms
          WRITE(1,*) CloudC%pcoeff_L_MW(freq_selector,rad_selector,temp_selector,ii,1)
        END DO
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Extinction_Coefficients.txt')
          WRITE(1,*) CloudC%ke_IR(:,:,0)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Albedo.txt')  
          WRITE(1,*) CloudC%w_IR(:,:,0)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Asymmery_factor.txt')
          WRITE(1,*) CloudC%g_IR(:,:,0)
        CLOSE(1)

        WRITE(*,*) "To retrieve the corresponding IR Legendre coefficients,"
        WRITE(*,*) "Please select the frequency as an integer between 1 and ", n_IR_Frequencies
        READ(*,*) freq_selector
        WRITE(*,*) "and the effective radius as an integer between 1 and ", n_IR_Radii
        READ(*,*) rad_selector
        ! Write IR Legendre coefficients to file.
        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//"Legendre_terms_IR.txt")
        DO ii = 0,n_Legendre_Terms
          WRITE(1,*) CloudC%pcoeff_IR(freq_selector,rad_selector,0,ii)
        END DO
        CLOSE(1)

    !-------------------------------------------------------
    ! RAIN
    !-------------------------------------------------------

      CASE(2) ! RAIN
        selection = .TRUE.

        WRITE(*,*) "Please select a temperature by number:"
        WRITE(*,*) "1:    263.16"     
        WRITE(*,*) "2:    273.16"     
        WRITE(*,*) "3:    282.00"     
        WRITE(*,*) "4:    290.00"     
        WRITE(*,*) "5:    300.00"
        READ(*,*) temp_selector
        IF (temp_selector < 1 .OR. temp_selector > 5) THEN
          STOP "Wrong temperature selection"
        END IF 

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Extinction_Coefficients.txt')
          WRITE(1,*) CloudC%ke_L_MW(:,:,temp_selector)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Albedo.txt')  
          WRITE(1,*) CloudC%w_L_MW(:,:,temp_selector)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Asymmery_factor.txt')
          WRITE(1,*) CloudC%g_L_MW(:,:,temp_selector)
        CLOSE(1)

        WRITE(*,*) "To retrieve the corresponding MW Legendre coefficients,"
        WRITE(*,*) "Please select the frequency as an integer between 1 and ", n_MW_Frequencies
        READ(*,*) freq_selector
        WRITE(*,*) "and the effective radius as an integer between 1 and ", n_MW_Radii
        READ(*,*) rad_selector
        ! Write MW Legendre coefficients to file.
        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//"Legendre_terms_MW.txt")
        DO ii = 0,n_Legendre_Terms
          WRITE(1,*) CloudC%pcoeff_L_MW(freq_selector,rad_selector,temp_selector,ii,1)
        END DO
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Extinction_Coefficients.txt')
          WRITE(1,*) CloudC%ke_IR(:,:,0)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Albedo.txt')  
          WRITE(1,*) CloudC%w_IR(:,:,0)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Asymmery_factor.txt')
          WRITE(1,*) CloudC%g_IR(:,:,0)
        CLOSE(1)

        WRITE(*,*) "To retrieve the corresponding IR Legendre coefficients,"
        WRITE(*,*) "Please select the frequency as an integer between 1 and ", n_IR_Frequencies
        READ(*,*) freq_selector
        WRITE(*,*) "and the effective radius as an integer between 1 and ", n_IR_Radii
        READ(*,*) rad_selector
        ! Write IR Legendre coefficients to file.
        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//"Legendre_terms_IR.txt")
        DO ii = 0,n_Legendre_Terms
          WRITE(1,*) CloudC%pcoeff_IR(freq_selector,rad_selector,0,ii)
        END DO
        CLOSE(1)

    !-------------------------------------------------------
    ! ICE
    !-------------------------------------------------------

      CASE(3) ! ICE
        selection = .TRUE.

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Extinction_Coefficients.txt')
          WRITE(1,*) CloudC%ke_S_MW(:,:,3)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Albedo.txt')  
          WRITE(1,*) CloudC%w_S_MW(:,:,3)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Asymmery_factor.txt')
          WRITE(1,*) CloudC%g_S_MW(:,:,3)
        CLOSE(1)

        WRITE(*,*) "To retrieve the corresponding MW Legendre coefficients,"
        WRITE(*,*) "Please select the frequency as an integer between 1 and ", n_MW_Frequencies
        READ(*,*) freq_selector
        WRITE(*,*) "and the effective radius as an integer between 1 and ", n_MW_Radii
        READ(*,*) rad_selector
        ! Write MW Legendre coefficients to file.
        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//"Legendre_terms_MW.txt")
        DO ii = 0,n_Legendre_Terms
          WRITE(1,*) CloudC%pcoeff_S_MW(freq_selector,rad_selector,3,ii,1)
        END DO
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Extinction_Coefficients.txt')
          WRITE(1,*) CloudC%ke_IR(:,:,3)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Albedo.txt')  
          WRITE(1,*) CloudC%w_IR(:,:,3)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Asymmery_factor.txt')
          WRITE(1,*) CloudC%g_IR(:,:,3)
        CLOSE(1)

        WRITE(*,*) "To retrieve the corresponding IR Legendre coefficients,"
        WRITE(*,*) "Please select the frequency as an integer between 1 and ", n_IR_Frequencies
        READ(*,*) freq_selector
        WRITE(*,*) "and the effective radius as an integer between 1 and ", n_IR_Radii
        READ(*,*) rad_selector
        ! Write IR Legendre coefficients to file.
        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//"Legendre_terms_IR.txt")
        DO ii = 0,n_Legendre_Terms
          WRITE(1,*) CloudC%pcoeff_IR(freq_selector,rad_selector,3,ii)
        END DO
        CLOSE(1)

    !-------------------------------------------------------
    ! GRAUPEL
    !-------------------------------------------------------

      CASE(4) ! GRAUPEL
        selection = .TRUE.

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Extinction_Coefficients.txt')
          WRITE(1,*) CloudC%ke_S_MW(:,:,2)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Albedo.txt')  
          WRITE(1,*) CloudC%w_S_MW(:,:,2)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Asymmery_factor.txt')
          WRITE(1,*) CloudC%g_S_MW(:,:,2)
        CLOSE(1)

        WRITE(*,*) "To retrieve the corresponding MW Legendre coefficients,"
        WRITE(*,*) "Please select the frequency as an integer between 1 and ", n_MW_Frequencies
        READ(*,*) freq_selector
        WRITE(*,*) "and the effective radius as an integer between 1 and ", n_MW_Radii
        READ(*,*) rad_selector
        ! Write MW Legendre coefficients to file.
        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//"Legendre_terms_MW.txt")
        DO ii = 0,n_Legendre_Terms
          WRITE(1,*) CloudC%pcoeff_S_MW(freq_selector,rad_selector,2,ii,1)
        END DO
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Extinction_Coefficients.txt')
          WRITE(1,*) CloudC%ke_IR(:,:,2)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Albedo.txt')  
          WRITE(1,*) CloudC%w_IR(:,:,2)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Asymmery_factor.txt')
          WRITE(1,*) CloudC%g_IR(:,:,2)
        CLOSE(1)

        WRITE(*,*) "To retrieve the corresponding IR Legendre coefficients,"
        WRITE(*,*) "Please select the frequency as an integer between 1 and ", n_IR_Frequencies
        READ(*,*) freq_selector
        WRITE(*,*) "and the effective radius as an integer between 1 and ", n_IR_Radii
        READ(*,*) rad_selector
        ! Write IR Legendre coefficients to file.
        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//"Legendre_terms_IR.txt")
        DO ii = 0,n_Legendre_Terms
          WRITE(1,*) CloudC%pcoeff_IR(freq_selector,rad_selector,2,ii)
        END DO
        CLOSE(1)

    !-------------------------------------------------------
    ! SNOW
    !-------------------------------------------------------

      CASE(5) ! SNOW
        selection = .TRUE.

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Extinction_Coefficients.txt')
          WRITE(1,*) CloudC%ke_S_MW(:,:,1)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Albedo.txt')  
          WRITE(1,*) CloudC%w_S_MW(:,:,1)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Asymmery_factor.txt')
          WRITE(1,*) CloudC%g_S_MW(:,:,1)
        CLOSE(1)

        WRITE(*,*) "To retrieve the corresponding MW Legendre coefficients,"
        WRITE(*,*) "Please select the frequency as an integer between 1 and ", n_MW_Frequencies
        READ(*,*) freq_selector
        WRITE(*,*) "and the effective radius as an integer between 1 and ", n_MW_Radii
        READ(*,*) rad_selector
        ! Write MW Legendre coefficients to file.
        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//"Legendre_terms_MW.txt")
        DO ii = 0,n_Legendre_Terms
          WRITE(1,*) CloudC%pcoeff_S_MW(freq_selector,rad_selector,1,ii,1)
        END DO
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Extinction_Coefficients.txt')
          WRITE(1,*) CloudC%ke_IR(:,:,1)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Albedo.txt')  
          WRITE(1,*) CloudC%w_IR(:,:,1)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Asymmery_factor.txt')
          WRITE(1,*) CloudC%g_IR(:,:,1)
        CLOSE(1)

        WRITE(*,*) "To retrieve the corresponding IR Legendre coefficients,"
        WRITE(*,*) "Please select the frequency as an integer between 1 and ", n_IR_Frequencies
        READ(*,*) freq_selector
        WRITE(*,*) "and the effective radius as an integer between 1 and ", n_IR_Radii
        READ(*,*) rad_selector
        ! Write IR Legendre coefficients to file.
        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//"Legendre_terms_IR.txt")
        DO ii = 0,n_Legendre_Terms
          WRITE(1,*) CloudC%pcoeff_IR(freq_selector,rad_selector,1,ii)
        END DO
        CLOSE(1)

    !-------------------------------------------------------
    ! HAIL
    !-------------------------------------------------------

      CASE(6) ! HAIL
        selection = .TRUE.

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Extinction_Coefficients.txt')
          WRITE(1,*) CloudC%ke_S_MW(:,:,2)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Albedo.txt')  
          WRITE(1,*) CloudC%w_S_MW(:,:,2)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'MW_Asymmery_factor.txt')
          WRITE(1,*) CloudC%g_S_MW(:,:,2)
        CLOSE(1)

        WRITE(*,*) "To retrieve the corresponding MW Legendre coefficients,"
        WRITE(*,*) "Please select the frequency as an integer between 1 and ", n_MW_Frequencies
        READ(*,*) freq_selector
        WRITE(*,*) "and the effective radius as an integer between 1 and ", n_MW_Radii
        READ(*,*) rad_selector
        ! Write MW Legendre coefficients to file.
        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//"Legendre_terms_MW.txt")
        DO ii = 0,n_Legendre_Terms
          WRITE(1,*) CloudC%pcoeff_S_MW(freq_selector,rad_selector,3,ii,1)
        END DO
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Extinction_Coefficients.txt')
          WRITE(1,*) CloudC%ke_IR(:,:,2)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Albedo.txt')  
          WRITE(1,*) CloudC%w_IR(:,:,2)
        CLOSE(1)

        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//'IR_Asymmery_factor.txt')
          WRITE(1,*) CloudC%g_IR(:,:,2)
        CLOSE(1)

        WRITE(*,*) "To retrieve the corresponding IR Legendre coefficients,"
        WRITE(*,*) "Please select the frequency as an integer between 1 and ", n_IR_Frequencies
        READ(*,*) freq_selector
        WRITE(*,*) "and the effective radius as an integer between 1 and ", n_IR_Radii
        READ(*,*) rad_selector
        ! Write IR Legendre coefficients to file.
        OPEN(UNIT=1,FILE=TRIM(ADJUSTL(FP_output))//"Legendre_terms_IR.txt")
        DO ii = 0,n_Legendre_Terms
          WRITE(1,*) CloudC%pcoeff_IR(freq_selector,rad_selector,3,ii)
        END DO
        CLOSE(1)


      CASE DEFAULT 
        WRITE(*,*) "Please select a valid particle type."
      END SELECT
    END DO select_particle

    WRITE(*,*) "Output complete. The program has finished successfully."

END PROGRAM inspectTauCoeff