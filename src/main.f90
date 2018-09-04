!-------------------------------------------------------
!
! main.f90
!
! Description:
! ============
!	Simple test program to inspect the CRTM CloudCoeff 
!	binary coefficient files that contain the scattering
! properties for water cloud particles.
!
!	Record of Revisions:
! ====================
! 
!	Date: 	    Author:        Description:
! =====       =======        ============
! 2018-09-04  P. Stegmann    Original code 
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

    ! Local variables
    CHARACTER(ML)   :: msg, pid_msg
    CHARACTER(SL)   :: Default_CloudCoeff_File, CloudCoeff_File, new_CloudCoeff_File, CloudCoeff_File_ybq
    CHARACTER(SL)   :: Filename
    CHARACTER(SL)   :: File_Path

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

    INTEGER :: I, J, K, ii


    real,dimension(31,10) :: w_S_MW_ybq, g_S_MW_ybq, ke_S_MW_ybq
    real,dimension(31,10,38) :: pcoeff_S_MW_ybq
    character(ML) :: strde, strfre
    integer, dimension(10) :: r

    !data r/2, 3, 4, 5, 8, 10, 15, 20, 50, 100/  !! effective radius for IR
    data r/5, 15, 30, 50, 100, 300, 500, 800, 1000, 1500/  !! effective radius for MW


  ! ---------------------------------
  ! The shared cloud coefficient data
  ! ---------------------------------
  TYPE(CloudCoeff_type), TARGET, SAVE :: CloudC



    ! Set up
    err_stat = SUCCESS

    Default_CloudCoeff_File    = 'CloudCoeff_orig.bin'
    !Default_CloudCoeff_File    = 'CloudCoeff_pst_270K_le_test.bin'
    !Default_CloudCoeff_File     = 'CloudCoeff_out.bin'

    !new_CloudCoeff_File   = 'CloudCoeff_ybq_test.bin'

    Filename  = Default_CloudCoeff_File
    File_Path  = './'

    ! ...Add the file path
    CloudCoeff_File = TRIM(ADJUSTL(File_Path))//TRIM(Filename)
    
    CloudCoeff_File_ybq = TRIM(ADJUSTL(File_Path))//TRIM(new_CloudCoeff_File)
   
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

    print *, n_MW_Frequencies, &
                 n_MW_Radii      , &
                 n_IR_Frequencies, &
                 n_IR_Radii      , &
                 n_Temperatures  , &
                 n_Densities     , &
                 n_Legendre_Terms, &
                 n_Phase_Elements, &
                 Release         , &
                 Version


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
      ! CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
    END IF


    ! Replace the default CloudCoeff SNOW MW properties with the ICE MW properties:
    !CloudC%w_S_MW(:,:,3)            = CloudC%
    !CloudC%ke_S_MW(:,:,3)           = CloudC%
    !CloudC%g_S_MW(:,:,3)            = CloudC%
    !CloudC%pcoeff_S_MW(:,:3,0:37,1) = CloudC%

    ! Output CloudCoeff data to prompt
    DO ii = 1,31
        WRITE(*,*) "MW Frequency: ", CloudC%Frequency_MW(ii)
    END DO
    DO ii = 1,10
        WRITE(*,*) "MW Radii: ", CloudC%Reff_MW(ii)
    END DO
    DO ii = 1,3
        WRITE(*,*) "Temperatures: ", CloudC%Temperature(ii)
    END DO
    DO ii = 1,3
        WRITE(*,*) "Densities: ", CloudC%Density(ii)
    END DO

    DO ii = 1,31
        WRITE(*,*) CloudC%ke_S_MW(ii,8,2)
    END DO
    !WRITE(*,*) CloudC%g_S_MW(14,10,1)
    OPEN(UNIT=1,FILE="Legendre_terms.txt")
    DO ii = 1,38
        WRITE(1,*) CloudC%pcoeff_S_MW(1,1,3,ii,1)
    END DO
    CLOSE(1)

END PROGRAM inspectTauCoeff