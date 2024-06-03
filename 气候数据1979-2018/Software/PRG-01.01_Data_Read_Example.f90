!===================================================================================================
! File:		PRG-01.01_Data_Read_Example.f90
! Category:	Fortran 90 program
! Version:	01.02.0032
! Author(s):	Jie He (hejie@mail.iap.ac.cn)
! Copyright:	(C) 2010-2011 Jie He
! License:	GNU General Public License version 3 (GPLv3)
! Date Created:	2010-04-30 by Jie He
! Last Updated:	2011-05-03 by Jie He
!---------------------------------------------------------------------------------------------------
! Function:	A sample Fortran 90 program for reading ITP LSM forcing data.
!---------------------------------------------------------------------------------------------------
! Arguments:	None
!---------------------------------------------------------------------------------------------------
! Parameters:	Number:	12
!		IFNAME:		Input file name.
!		OFNAME:		Output file name.
!		OMVALUE:	Missing value of output data file.
!		NX_SRT:		X-dimensional grid coordinate of the grid on the western boundary 
!				of the field you want to get (default value is 1).
!		NX_END:		X-dimensional grid coordinate of the grid on the eastern boundary 
!				of the field you want to get (default value is 700).
!		NX:		Total x-dimensional grid number of the field you want to get 
!				(NX = NX_END - NX_SRT + 1).
!		NY_SRT:		Y-dimensional grid coordinate of the grid on the southern boundary 
!				of the field you want to get (default value is 1).
!		NY_END:		Y-dimensional grid coordinate of the grid on the northern boundary 
!				of the field you want to get (default value is 400).
!		NY:		Total y-dimensional grid number of the field you want to get 
!				(NY = NY_END - NY_SRT + 1).
!		NT_SRT:		T-dimensional grid coordinate of the beginning time step of the 
!				data you want to get (default value is 1).
!		NT_END:		T-dimensional grid coordinate of the last time step of the data you 
!				want to get (default value is 31 * 8 = 248).
!		NT:		Total t-dimensional grid number of the data you want to get 
!				(NT = NT_END - NT_SRT + 1).
!---------------------------------------------------------------------------------------------------
! Variables:	Number:	13
!		ncid:		NetCDF file ID.
!		stat:		Return value of NetCDF functions.
!		dmid:		NetCDF variable's dimension ID.
!		start(3):	Storing NX_SRT, NY_SRT, and NT_SRT, needed by NetCDF functions.
!		count(3):	Storing NX, NY, and NT, needed by NetCDF functions.
!		nlon:		Same to NX, but for storing automatically fetched value.
!		nlat:		Same to NY, but for storing automatically fetched value.
!		ntime:		Same to NT, but for storing automatically fetched value.
!		imvalue:	Missing value of input data file.
!		sfactor:	Scale factor, obtained from NetCDF data file.
!		aoffset:	Add offset, obtained from NetCDF data file.
!		ivar(NX,NY,NT):	Working array (integer).
!		fvar(NX,NY,NT):	Working array (float or real).
!---------------------------------------------------------------------------------------------------
! Description:	This simple program is only a sample to show how to read ITP LSM forcing data, 
!		users might have to modify it to satisfy special needs.
!		This program can automatically get the lengths of the dimensions from NetCDF files, 
!		but this function is disabled by default, uncomment the corresponding lines to 
!		enable this function.
!===================================================================================================


program main


  use netcdf


  implicit none


  ! --  Variable declaration.  ---------------------------------------------------------------------

  ! **  Parameters.  **
  character*200, parameter :: IFNAME = "temp_ITPCAS-CMFD_V0106_B-01_03hr_010deg_197901.nc", OFNAME = "output.bin"

  integer, parameter :: NX_SRT =   1, NX_END = 700, NX = NX_END - NX_SRT + 1
  integer, parameter :: NY_SRT =   1, NY_END = 400, NY = NY_END - NY_SRT + 1
  integer, parameter :: NT_SRT =   1, NT_END = 248, NT = NT_END - NT_SRT + 1

  real, parameter :: OMVALUE = 1.0E+36

  ! **  Loop variables.  **
  integer :: i, j, t

  ! **  Variables needed NetCDF functions.  **
  integer*4 :: ncid, stat
  integer*4, dimension(3) :: dmid
  integer*4, dimension(3) :: start, count

  ! **  Variables for storing NetCDF variable properties.  **
  integer*4 :: nlon, nlat, ntime
  integer*2 :: imvalue
  real*4 :: sfactor, aoffset

  ! **  Working array.  **
  integer*2, dimension(NX, NY, NT) :: ivar
  real*4, dimension(NX, NY, NT) :: fvar


  ! --  Variable initialization.  ------------------------------------------------------------------

  start(1) = NX_SRT; count(1) = NX
  start(2) = NY_SRT; count(2) = NY
  start(3) = NT_SRT; count(3) = NT


  ! --  Data input.  -------------------------------------------------------------------------------

  stat = nf90_open (IFNAME, nf90_nowrite, ncid)
  call check (stat)

  ! **  Automatically get the lengths of the dimensions.  **
  ! Caution:	Uncomment the following 12 lines to enable this function. Once it is enabled, user 
  !		denfined NX_SRT, NY_SRT, NT_SRT, NX_END, NY_END, NT_END will be omitted.
  !stat = nf90_inquire_variable (ncid, 4, dimids = dmid)
  !call check (stat)
  !
  !stat = nf90_inquire_dimension (ncid, dmid(1), len = nlon)
  !call check (stat)
  !
  !stat = nf90_inquire_dimension (ncid, dmid(2), len = nlat)
  !call check (stat)
  !
  !stat = nf90_inquire_dimension (ncid, dmid(3), len = ntime)
  !call check (stat)
  !
  !write (*, '(3(A5, I4, 4X))') "NX = ", nlon, "NY = ", nlat, "NT = ", ntime
  !start(1) = 1; count(1) = nlon
  !start(2) = 1; count(2) = nlat
  !start(3) = 1; count(3) = ntime
  ! ----------------

  stat = nf90_get_var (ncid, 4, ivar, start, count)
  call check (stat)

  stat = nf90_get_att (ncid, 4, "scale_factor", sfactor)
  call check (stat)

  stat = nf90_get_att (ncid, 4, "add_offset", aoffset)
  call check (stat)

  stat = nf90_get_att (ncid, 4, "missing_value", imvalue)
  call check (stat)

  stat = nf90_close (ncid)
  call check (stat)


  ! --  Data processing.  --------------------------------------------------------------------------

  do t = 1, NT
    do j = 1, NY
      do i = 1, NX
        if (ivar(i, j, t) /= imvalue) then
          fvar(i, j, t) = ivar(i, j, t) * sfactor + aoffset
        else
          fvar(i, j, t) = OMVALUE
        end if
      end do
    end do
  end do


  ! --  Data output.  ------------------------------------------------------------------------------

  open (21, file = OFNAME, form = 'unformatted', access = 'direct', recl = NX * NY * 4)

  do t = 1, NT
    write (21, rec = t) ((fvar(i, j, t), i = 1, NX), j = 1, NY)
  end do

  close (21)


contains


  ! --  Subordinate subroutines and functions.  ----------------------------------------------------

  subroutine check (status)

    integer, intent(in) :: status

    if (status /= nf90_noerr) then
      write (*, *) trim (nf90_strerror (status))
      stop "Stopped"
    end if

  end subroutine check

end program main

