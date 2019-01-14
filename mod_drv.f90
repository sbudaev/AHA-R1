!> @file mod_drv.f90
!! The main "driver" file for the AHA Model.
!! @author Sergey Budaev <sergey.budaev@uib.no>
!! @author Jarl Giske <jarl.giske@uib.no>
!! @date 2016-2017

!-------------------------------------------------------------------------------
! $Id$
!-------------------------------------------------------------------------------

!> @brief   Main driver component for the AHA Model.
!! @details This is the actual Fortran program that calls all the
!!          computations for the AHA Model. It calls highest level
!!          procedures that are implemented in the modules:
!!          - @ref commondata
!!          - @ref the_environment
!!          - @ref the_genome
!!          - @ref the_hormones
!!          - @ref the_body
!!          - @ref the_neurobio
!!          - @ref the_individual
!!          - @ref the_population
!!          - @ref the_evolution
!!          .
!! File version:
!! @verbatim
!! $Id: mod_drv.f90 8a93c01ba9c1 2017/12/17 07:18:19 sergey $
!! @endverbatim
program AHA_MODEL_DRIVER

  use COMMONDATA
  use THE_EVOLUTION, only: generations_loop_ga

  implicit none

  !> - Initialise the system and logger calling commondata::system_init().
  call system_init

  !> - Finally, call the the_evolution::generations_loop_ga(). This starts
  !!   the evolution process.
  !! .
  call generations_loop_ga

end program AHA_MODEL_DRIVER
