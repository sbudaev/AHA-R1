!> @file m_evolut.f90
!! THE_EVOLUTION Module implements the Genetic Algorithm for the AHA Model.
!! @author Sergey Budaev <sergey.budaev@uib.no>
!! @author Jarl Giske <jarl.giske@uib.no>
!! @date 2016-2017

!-------------------------------------------------------------------------------
! $Id$
!-------------------------------------------------------------------------------

!> @brief Implementation of the genetic algorithm.
!> @section the_evolution_module THE_EVOLUTION module
!> The Genetic Algorithm is implemented here
module THE_EVOLUTION

  use COMMONDATA      ! Global definitions of model objects
  use THE_GENOME      ! This mod defines our individual fish object
  use THE_NEUROBIO
  use THE_INDIVIDUAL
  use THE_POPULATION
  use THE_ENVIRONMENT

  use BASE_UTILS  ! Modelling tools
  use BASE_RANDOM
  use CSV_IO
  use LOGGER

  implicit none
  private
  public  generations_loop_ga,              & ! Only global GA loop is exposed,
          preevol_steps_adaptive,           & ! other objects are public for
          preevol_steps_adaptive_save_csv     ! external tests.

  ! PROCNAME is the procedure name for logging and debugging
  character (len=*), parameter, private :: MODNAME = "(THE_EVOLUTION)"

  !> Model-global stopwatch objects.
  !! @note Use the keyword `TIMER:` (LTAG_TIMER) for logging, e.g.
  !!       `call LOG_MSG( LTAG_TIMER // stopwatch_op_current%show() )`
  type(TIMER_CPU), public ::  stopwatch_global,           & !> global stopwatch
                              stopwatch_generation,       & !> generation-wise
                              stopwatch_op_current          !> single operation

  !> We have an environment composed of two habitats, safe and a dangerous.
  type(HABITAT), public :: habitat_safe, habitat_dangerous

  !> Here we create instances for two populations which will then serve as
  !! parents and offspring. And then we declare pointers that will point to
  !! parents and offspring.
  type(POPULATION), public, target :: generation_one ! new populations
  type(POPULATION), public, target :: generation_two !  as objects
  type(POPULATION), public, pointer :: proto_parents
  type(POPULATION), public, pointer :: proto_offspring

contains ! ........ implementation of procedures for this level ................

!-----------------------------------------------------------------------------
  !> Initialise the environmental objects. Most of the environmental objects,
  !! such as the environment, habitats etc. are kept static throughout the
  !! model running. There are, however, patterned and stochastic changes in
  !! the environment, such as diurnal variation of the illumination level.
  subroutine init_environment_objects()

    character(len=*), parameter :: PROCNAME = "(init_environment_objects)"

    integer :: i ! counter

    !> ### Build the environmental objects ###
    !> Build the overall environment "universe". It can be used for the
    !! whole-environment placement of objects, e.g. random walks of an agent
    !! crossing the borders between the habitats.
    call LOG_DBG("Initialisation of the environment and the habitat(s)")

    ! Start stopwatch for timing the environment init process.
    call stopwatch_op_current%start("Initialisation of the environment")

    !> Build the habitats.
    call habitat_safe%make(                                                   &
                            coord_min=SPATIAL( HABITAT_SAFE_MIN_COORD(1),     &
                                               HABITAT_SAFE_MIN_COORD(2),     &
                                               HABITAT_SAFE_MIN_COORD(3) ),   &
                            coord_max=SPATIAL( HABITAT_SAFE_MAX_COORD(1),     &
                                               HABITAT_SAFE_MAX_COORD(2),     &
                                               HABITAT_SAFE_MAX_COORD(3) ),   &
                            label="Safe",                                     &
                            predators_number=PREDATORS_NUM_HABITAT_SAFE,      &
                            otherrisks=OTHER_RISKS_HABITAT_SAFE,              &
                            food_abundance=FOOD_ABUNDANCE_HABITAT_SAFE )

    call habitat_dangerous%make(                                              &
                            coord_min=SPATIAL( HABITAT_DANGER_MIN_COORD(1),   &
                                               HABITAT_DANGER_MIN_COORD(2),   &
                                               HABITAT_DANGER_MIN_COORD(3) ), &
                            coord_max=SPATIAL( HABITAT_DANGER_MAX_COORD(1),   &
                                               HABITAT_DANGER_MAX_COORD(2),   &
                                               HABITAT_DANGER_MAX_COORD(3) ), &
                            label="Dangerous",                                &
                            predators_number=PREDATORS_NUM_HABITAT_DANGER,    &
                            otherrisks=OTHER_RISKS_HABITAT_DANGER,            &
                            food_abundance=FOOD_ABUNDANCE_HABITAT_DANGER )

    call LOG_MSG( LTAG_TIMER // stopwatch_op_current%show() )

    !> Define and allocate the global array of all habitats available to the
    !! agents. See the_environment::global_habitats_available for details of
    !! this global array. This is now made using the the_environment::assemble()
    !! procedure.
    ! It is analogous to such a code:
    ! @code
    !   allocate(Global_Habitats_Available(2))
    !   Global_Habitats_Available = [ habitat_safe, habitat_dangerous ]
    ! @endcode
    call assemble ( habitat_safe, habitat_dangerous )
    !> Allocation of the the_environment::global_habitats_available  is
    !! checked. If it turns out not allocated, a critical error is signalled
    !! in the logger and the program calls commondata::system_halt().
    if (.not. allocated(Global_Habitats_Available) ) then
      call LOG_MSG( LTAG_CRIT // "Global_Habitats_Available array "   //      &
                    "cannot be allocated in " // PROCNAME // "!" )
      call system_halt(is_error=.TRUE., message=ERROR_ALLOCATION_FAIL)
    end if

    !> ### Save initial diagnostic data ###
    !> Output the number of the habitats in the global array
    !! the_environment::global_habitats_available and their labels into
    !! the logger.
    call LOG_MSG( LTAG_INFO // "Allocated 'Global_Habitats_Available' to " // &
                  TOSTR(size(Global_Habitats_Available)) // " elements:" )
    call LOG_MSG( LTAG_INFO // "  " // TOSTR(                                 &
                    [( Global_Habitats_Available(i)%get_label(),              &
                        i=1, size(Global_Habitats_Available) )] ) )

    !> Certain data are also saved. Their names start from the `init_` prefix.
    !> - Save initial food data (uniform distribution as built at init). Note
    !!   that the distribution of the food items can change at each time step
    !!   due to vertical migration of the food items and their local random
    !!   Gaussian movements.
    call LOG_MSG( LTAG_INFO //                                                &
                  "Saving initial uniform food resources to CSV files.")
    call habitat_safe%food%save_csv(                                          &
                          csv_file_name = "init_food_safe_habitat" // csv )
    call habitat_dangerous%food%save_csv(                                     &
                          csv_file_name = "init_food_dangerous_habitat" // csv )

    !> - Save predators' data.
    call LOG_MSG(LTAG_INFO // "Saving predators from habitats into CSV files.")
    call habitat_safe%save_predators_csv(                                     &
                  csv_file_name = "init_predators_safe_habitat" // csv )
    call habitat_dangerous%save_predators_csv(                                &
                  csv_file_name = "init_predators_dangerous_habitat" // csv )

    !> - Save the basic data on the dynamics of illumination, food items and
    !!   visibility across the life span of the agents.
    !! .
    call save_dynamics( csv_file_name = "init_dynamics" // csv )

    !> #### Save plots ####
    !> If the plotting is enabled (see commondata::is_plotting), some plots
    !! of the initialisation data are also saved.
    DO_PLOT: if (IS_PLOTTING) then
      !> - Save debug scatterplots of food items distribution within in
      !!   the habitats.
      call debug_scatterplot_save(x_data=habitat_safe%food%food%x,            &
                  y_data=habitat_safe%food%food%y,                            &
                  csv_out_file="debug_plot_food_safe_"// MMDD // "_g" //      &
                      TOSTR(Global_Generation_Number_Current) // csv,         &
                  delete_csv=.FALSE., enable_non_debug=.TRUE. )
      call debug_scatterplot_save(x_data=habitat_dangerous%food%food%x,       &
                  y_data=habitat_dangerous%food%food%y,                       &
                  csv_out_file="debug_plot_food_danger_" // MMDD // "_g" //   &
                      TOSTR(Global_Generation_Number_Current) // csv,         &
                  delete_csv=.FALSE., enable_non_debug=.TRUE. )
       !> - Save debug scatterplots of predators distribution in the habitats.
      call debug_scatterplot_save(x_data=habitat_safe%predators%x,            &
                  y_data=habitat_safe%predators%y,                            &
                  csv_out_file="debug_plot_predat_safe_" // MMDD // "_g" //   &
                      TOSTR(Global_Generation_Number_Current) // csv,         &
                  delete_csv=.FALSE., enable_non_debug=.TRUE. )
      call debug_scatterplot_save(x_data=habitat_dangerous%predators%x,       &
                  y_data=habitat_dangerous%predators%y,                       &
                  csv_out_file="debug_plot_predat_danger_" // MMDD // "_g"//  &
                      TOSTR(Global_Generation_Number_Current) // csv,         &
                  delete_csv=.FALSE., enable_non_debug=.TRUE. )
      !> - Save histograms of food item sizes.
      !! .
      call debug_histogram_save(x_data=habitat_safe%food%food%size,           &
                  csv_out_file="debug_hist_food_safe_size_" // MMDD // "_g" //&
                      TOSTR(Global_Generation_Number_Current) // csv,         &
                  delete_csv=.FALSE., enable_non_debug=.TRUE. )
      call debug_histogram_save(x_data=habitat_dangerous%food%food%size,      &
                  csv_out_file="debug_hist_food_dang_size_" // MMDD // "_g" //&
                    TOSTR(Global_Generation_Number_Current) // csv,           &
                  delete_csv=.FALSE., enable_non_debug=.TRUE. )

    end if DO_PLOT

  end subroutine init_environment_objects

  !-----------------------------------------------------------------------------
  !> Calculate the adaptive number of time steps for the fixed fitness
  !! preevolution stage of the genetic algorithm.
  !!
  !! The number of time steps in the fixed-fitness pre-evolution genetic
  !! algorithm is calculated using an adaptive algorithm. Briefly, the number
  !! of time steps (total lifespan) at the early stages of evolution (the first
  !! generations) is very short and increases as the evolution proceeds towards
  !! the maximum set by commondata::preevol_tsteps.
  !! @note The time steps data generated by this function for each GA
  !!       generation are saved in CSV file by
  !!       the_evolution::preevol_steps_adaptive_save_csv().
  function preevol_steps_adaptive( generation ) result(steps)
    !> @param[in] generation optional current generation number, if not
    !!            provided, set to commondata::global_generation_number_current.
    integer, optional, intent(in) :: generation
    !> @return The number of lifecycle time steps at the specific generation.
    integer :: steps

    ! Local copies of optionals
    real(SRP) :: generation_number

    ! The duration of a single diel cycle
    integer, parameter :: ONE_CYCLE = LIFESPAN / DIELCYCLES

    ! The number of diel cycles in the pre-evolution stage.
    integer, parameter :: PREEVOL_CYCLES = PREEVOL_TSTEPS / ONE_CYCLE

    !> ### Implementation notes ###
    !> The number of time steps in this fixed fitness pre-evol adaptive GA
    !! algorithm is calculated based on a linear interpolation from a grid
    !! defined by the two arrays:
    !! - `STEPS_ABSCISSA` -- grid abscissa, from the first generation to
    !!    the total number of generations commondata::generations.
    real(SRP), dimension(*), parameter :: STEPS_ABSCISSA =                    &
        [ real(SRP) ::  1.0_SRP,                                              &
                        GENERATIONS / 2,                                      &
                        GENERATIONS * 3 / 4,                                  &
                        GENERATIONS + 1 ]

    !> - `STEPS_ORDINATE` -- grid ordinate, ranging from the number of time
    !!    steps in one diel cycle to the total number of time steps in the
    !!    fixed fitness pre-evolution stage commondata::preevol_tsteps.
    !     `htintrpl.exe [1 50 75 100] [0 0.3 0.6 1] [1] [nonlinear]`
    !     `htintrpl.exe [1 50 75 101] [0 0.8 0.95 1] [1] [nonlinear]`
    !     `htintrpl.exe [1 50 75 101] [0.5 0.8 0.95 1] [1] [nonlinear]`
    !> .
    real(SRP), dimension(*), parameter :: STEPS_ORDINATE =                    &
        [ real(SRP) ::  ONE_CYCLE * PREEVOL_CYCLES * 0.30_SRP,                &
                        ONE_CYCLE * PREEVOL_CYCLES * 0.80_SRP,                &
                        ONE_CYCLE * PREEVOL_CYCLES * 0.95_SRP,                &
                        PREEVOL_TSTEPS ]

    !> However, for debugging purposes, evolution time steps can be set to a
    !! specific fixed value. This value is set by
    !! commondata::preevol_tsteps_force_debug integer parameter and for this
    !! fixed value to be forced, commondata::preevol_tsteps_force_debug_enabled
    !! must be TRUE.
    if ( PREEVOL_TSTEPS_FORCE_DEBUG_ENABLED ) then
      steps = PREEVOL_TSTEPS_FORCE_DEBUG
      return
    end if

    ! Check optional parameter.
    if (present(generation)) then
      generation_number = real( generation, SRP )
    else
      generation_number = real( Global_Generation_Number_Current, SRP )
    end if

    !> Then, the total (adaptive) number of time steps is determined by the
    !! integer lower limit (floor) of the linear interpolation DDPINTERPOL()
    !! procedure, with further limitation that its result value must be
    !! within the range of [*t,T*], where *t* is the length of a single
    !! diel cycle, *T* is the number of time steps in the pre-evolution stage.
    !!
    !! @note Plotting commands:
    !!         - `htintrpl.exe [1 50 75 101] [0 0.8 0.95 1] [1] [nonlinear]`
    !!         .
    steps = floor( within( DDPINTERPOL( STEPS_ABSCISSA,                       &
                                        STEPS_ORDINATE,                       &
                                        generation_number ),                  &
                           real( ONE_CYCLE, SRP ),                            &
                           real( PREEVOL_TSTEPS, SRP ) ) )

  end function preevol_steps_adaptive

  !-----------------------------------------------------------------------------
  !> This is a diagnostic subroutine to save the number of time steps for the
  !! adaptive GA.
  subroutine preevol_steps_adaptive_save_csv(csv_file_name, is_success)
    !> @param[in] csv_file_name the name of the CSV file to save the arrays.
    character(len=*), intent(in) :: csv_file_name
    !> @param[out] is_success Flag showing that data save was successful
    !!             (if TRUE).
    logical, optional, intent(out) :: is_success

    logical :: csv_file_status

    integer, dimension(GENERATIONS) :: generation         ! generation
    integer, dimension(GENERATIONS) :: time_steps         ! n of time steps
    integer :: i                                          ! counter

    generation = [( i, i=1, GENERATIONS )]
    time_steps = [(preevol_steps_adaptive(i), i=1, GENERATIONS)]

    call CSV_MATRIX_WRITE ( reshape( [ generation,                            &
                                       time_steps ],                          &
                                     [ GENERATIONS, 2 ] ),                    &
                            csv_file_name,                                    &
                            [ "GENERATION","TIME_STEP " ],                    &
                            csv_file_status  )

    if (present(is_success)) is_success = csv_file_status

  end subroutine preevol_steps_adaptive_save_csv

  !-----------------------------------------------------------------------------
  !> Swap generation pointers between parents and offspring.
  subroutine generations_swap()

    if (associated(proto_parents, target=generation_one)) then
      proto_parents => generation_two
      proto_offspring => generation_one
    else
      proto_parents => generation_one
      proto_offspring => generation_two
    end if

  end subroutine generations_swap

  !-----------------------------------------------------------------------------
  !> Select reproducing agents, the best commondata::ga_reproduce_pr
  !! portion of agents.
  subroutine selection()

    ! Local counter
    integer :: i

    ! Number of the best reproducing agents.
    integer :: ga_reproduce

    !> The best (sorted) parents are copied to the offspring population object.
    !! Note that the number of such reproducing parents is determined by the
    !! the_population::population::ga_reproduce_max() method.
    ga_reproduce = proto_parents%ga_reproduce_max()

    !> Old fixed proportion implementation:
    !! @code
    !!   proto_offspring(:GA_REPRODUCE_N) = proto_parents(:GA_REPRODUCE_N)
    !! @endcode
    proto_offspring%individual(:ga_reproduce) =                               &
                                      proto_parents%individual(:ga_reproduce)

    !> The best parents (elite group) are then re-initialised from the genome
    !! for the next generation using the_individual::individual_agent::init()
    !! method.
    do i=1, ga_reproduce
      call proto_offspring%individual(i)%init(exclude_genome=.TRUE.)
    end do

  end subroutine selection

  !-----------------------------------------------------------------------------
  !> Mate, reproduce and mutate.
  subroutine mate_reproduce()

    integer :: i, i1, i2

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(mate_reproduce)"

    real(SRP) :: adapt_mut_point, adapt_mut_batch

    !> Calculate adaptive mutation rate
    adapt_mut_point = proto_parents%ga_mutat_adaptive(MUTATIONRATE_POINT,     &
                                                      GA_MUTATIONRATE_POINT_MAX)
    adapt_mut_batch = proto_parents%ga_mutat_adaptive(MUTATIONRATE_BATCH,     &
                                                      GA_MUTATIONRATE_BATCH_MAX)
    call LOG_MSG( LTAG_STAGE // "Mutation rates: " //                         &
                  TOSTR(adapt_mut_point) // ", " // TOSTR(adapt_mut_batch) // &
                  " for population size " // TOSTR(proto_parents%get_size()) )

    !> Loop through all the non-elite population members. These individuals
    !! are created from the genomes of the elite group. The non-elite
    !! individuals are from commondata::ga_reproduce_n+1 to commondata::popsize.
    do i = proto_parents%ga_reproduce_max() + 1, POPSIZE

      !> - If chromosomes are not allocated, this means it is a new individual.
      !!   We have to initialise it -- now as random. The same is true for all
      !!   individuals that the_genome::individual::genome::is_dead().
      if ( .not. allocated(proto_offspring%individual(i)%chromosome) ) then
        call proto_offspring%individual(i)%init()
        !call proto_offspring%individual(i)%sex_init()
        call proto_offspring%individual(i)%place_uniform(habitat_safe)
        call LOG_DBG( LTAG_INFO // "Initialised individual " //               &
                      TOSTR(i) // " (" //                                     &
                      TOSTR(proto_offspring%individual(i)%get_id()) // ")",   &
                      PROCNAME, MODNAME  )
      end if

      !> - Two agents are randomly chosen from the population. They become the
      !!   mother and the father of new `proto_offspring` agents. The mother
      !!   and the father exchange their genetic material using the
      !!   the_genome::individual_genome::recombine_random() method. Note that
      !!   the mother must be the_genome::individual_genome::is_female()
      !!   and the father, the_genome::individual_genome::is_male().
      i1 = RAND_I( 1, GA_REPRODUCE_N * 2 )  ! the **mother** must be female.
      do while (proto_parents%individual(i1)%is_male())
        i1 = RAND_I( 1, GA_REPRODUCE_N * 2 )
      end do
      i2 = RAND_I( 1, GA_REPRODUCE_N * 2 )  ! the **father** must be male.
      do while (proto_parents%individual(i2)%is_female())
        i2 = RAND_I( 1, GA_REPRODUCE_N * 2 )
      end do
      call proto_offspring%individual(i)%recombine_random(                    &
                                      mother = proto_parents%individual(i1),  &
                                      father = proto_parents%individual(i2)  )

      !> - Once the genome of the offspring is created from recombination data,
      !!   the offspring are subjected to random mutation using the
      !!   the_genome::individual_genome::mutate() backend.
      call proto_offspring%individual(i)%mutate(                              &
                          p_point = adapt_mut_point, p_set = adapt_mut_batch )

      !> - After this, the whole agent is initialised using he constructor
      !!   the_genome::individual_agent::init(), but without random
      !!   initialisation of the genome, the latter is based on the
      !!   recombination data from the parents.
      !! .
      call proto_offspring%individual(i)%init(exclude_genome=.TRUE.)

    end do

    !> Finally, loop through the elite group and introduce random mutations
    !! there too with the_genome::individual_genome::mutate().
    !! @note This is disabled (elitism).
    !do i = 1, GA_REPRODUCE_N
    !  call proto_offspring%individual(i)%mutate()
    !end do

  end subroutine mate_reproduce

  !-----------------------------------------------------------------------------
  !> This procedure implements the main **Genetic Algorithm** for evolving the
  !! agents.
  subroutine generations_loop_ga()
    use FILE_IO
    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(generations_loop_ga)"

    !> #### Objects for the GA ####
    !> - `energy_mean_gen1_birth_mort` -- average value of the birth energy
    !!    reserves, for forced selective birth mortality. See
    !!    the_population::population::mortality_birth().
    real(SRP) :: energy_mean_gen1_birth_mort
    !> - `energy_sd_gen1_birth_mort` -- standard deviationof the birth energy
    !!    reserves, for forced selective birth mortality. See
    !!    the_population::population::mortality_birth().
    !! .
    real(SRP) :: energy_sd_gen1_birth_mort

    !> #### Objects for generation-wise statistics file ####
    !> The definitions below are for the objects that are used to write
    !! generation-wise statistics in the ::generation_stats_record_write()
    !! sub-procedure.
    !! - `file_stats_gener`: File handle object for generation-wise statistics.
    type(FILE_HANDLE) :: file_stats_gener
    !> - `file_stats_gener_record`: Record for the generation-wise statistics
    !!    file.
    character(len=:), allocatable :: file_stats_gener_record
    !> - `FILE_STATS_GENER_COLS`: an array of column names for the
    !!    generation-wise statistics file.
    character(len=LABEL_LENGTH), dimension(*), parameter ::                   &
                    FILE_STATS_GENER_COLS = [ character(len=LABEL_LENGTH) ::  &
                                                    "GENERATION",      &  !  1
                                                    "PREEVOL_STEPS",   &  !  2
                                                    "MUTAT_POINT",     &  !  3
                                                    "MUTAT_BATCH",     &  !  4
                                                    "ELITE_GROUP",     &  !  5
                                                    "N_ALIVE",         &  !  6
                                                    "N_GROWN",         &  !  7
                                                    "N_MALES_L",       &  !  8
                                                    "N_FEMALES_L",     &  !  9
                                                    "N_EATEN_PRED",    &  ! 10
                                                    "BODY_MASS",       &  ! 11
                                                    "BODY_LEN",        &  ! 12
                                                    "BIRTH_MASS",      &  ! 13
                                                    "BIRTH_LENGTH",    &  ! 14
                                                    "BIRTH_ENERGY",    &  ! 15
                                                    "ENERGY",          &  ! 16
                                                    "STOMACH",         &  ! 17
                                                    "SMR",             &  ! 18
                                                    "CTRL_RND",        &  ! 19
                                                    "REPRFACT",        &  ! 20
                                                    "P_REPR",          &  ! 21
                                                    "N_REPROD",        &  ! 22
                                                    "N_OFFSPRING",     &  ! 23
                                                    "GOS_AROUSAL",     &  ! 24
                                                    "FOODS_TRY",       &  ! 25
                                                    "FOODS_EATEN",     &  ! 26
                                                    "FMASS_EATEN",     &  ! 27
                                                    "PERC_FOOD",       &  ! 28
                                                    "PERC_CONS",       &  ! 29
                                                    "PERC_PRED",       &  ! 30
                                                    "DEPTH",           &  ! 31
                                                    "N_SAFE_HABITAT",  &  ! 32
                                                    "N_DANG_HABITAT",  &  ! 33
                                                    "PERC_FOOD_SAFE",  &  ! 34
                                                    "PRC_FDIST_SAFE",  &  ! 35
                                                    "PERC_CONS_SAFE",  &  ! 36
                                                    "PERC_PRED_SAFE",  &  ! 37
                                                    "PERC_FOOD_DANG",  &  ! 38
                                                    "PRC_FDIST_DANG",  &  ! 39
                                                    "PERC_CONS_DANG",  &  ! 40
                                                    "PERC_PRED_DANG",  &  ! 41
                                                    "FDIST_SAFE",      &  ! 42
                                                    "FDIST_DANGER",    &  ! 43
                                                    "FITNESS_MIN",     &  ! 44
                                                    "FITNESS_MEAN",    &  ! 45
                                                    "N_FOODS_SAFE",    &  ! 46
                                                    "N_FOODS_DANG",    &  ! 47
                                                    "BODY_MASS_L",     &  ! 48
                                                    "BODY_LENGTH_L",   &  ! 49
                                                    "ENERGY_L",        &  ! 50
                                                    "SMR_L",           &  ! 51
                                                    "CONTROL_L",       &  ! 52
                                                    "REPRFACT_L",      &  ! 53
                                                    "P_REPROD_L",      &  ! 54
                                                    "FOODS_TRY_L",     &  ! 55
                                                    "FOODS_EATEN_L",   &  ! 56
                                                    "FMASS_EATEN_L",   &  ! 57
                                                    "N_SAFE_HAB_L",    &  ! 58
                                                    "N_DANG_HAB_L",    &  ! 59
                                                    "FITNESS_MEAN_L"   ]  ! 60

    !> - `FILE_STATS_RECORD_LEN`: The maximum length of the CSV record assuming
    !!    the maximum length of a single field is commondata::label_length; the
    !!    number of fields is equal to the size of the columns array
    !!    `FILE_STATS_GENER_COLS`.
    !! .
    integer, parameter :: FILE_STATS_RECORD_LEN =                             &
                                size(FILE_STATS_GENER_COLS) * LABEL_LENGTH +  &
                                size(FILE_STATS_GENER_COLS) * 3

    !> #### Parameters for the GA stopping rule ####
    !> Parameters determining the **stopping rule** for the fixed fitness
    !! genetic algorithm. These are based on the values obtained in the first
    !! generation. If in any succeeding generation, they fall below the first
    !! generation values, evolution is considered unsuccessful and the main GA
    !! loop stops.
    !> The number of alive agents at the first random generation.
    !! - Evolution should stop with unsuccessful status if the number of alive
    !!   agents falls below this value.
    integer :: ga_alive_generation_1
    !> The number of agents that have increased their body mass at the first
    !! random generation.
    !> - Evolution should stop with unsuccessful status if the number of alive
    !!   agents falls below this value.
    !! .
    integer :: ga_growing_generation_1

    ! Total N of alive and N of agents that have grown
    integer :: n_alive, n_growing

    !> # Preliminary steps #
    !> `Global_Generation_Number_Current` is the global generation number.
    !! It is first initialised to **1**.
    Global_Generation_Number_Current = 1

    !> commondata::Global_Rescale_Maximum_Motivation is the global maximum
    !! motivation value, it is fixed at the start of the simulation to an
    !! arbitrary high value but is automatically updated from the maximum
    !! motivation value across all agents after each time step.
    Global_Rescale_Maximum_Motivation = 6.0_SRP

    !> The stopping rule parameters based on the first generation values are
    !! initialised to some values allowing the first generation to occur
    !! safely, i.e. with sufficiently large number of randomly created
    !! pre-optimal agents.
    !! - If the number of alive agents is smaller than this minimum number,
    !!   GA stops: some parameters must be tweaked.
    ga_alive_generation_1 = ceiling( POPSIZE * 0.005_SRP )
    !> - The number of agents growing is set to a large negative value
    !!   commondata::unknown,so initial zero is always larger, so evolution
    !!   is allowed to start.
    !! .
    ga_growing_generation_1 = UNKNOWN

    call LOG_DBG( LTAG_MAJOR // "GLOBAL STARTUP " // PROCNAME )

    ! Start global stopwatch
    call stopwatch_global%start("Global time whole simulation")

    !> ## Initialise the environment ##
    !> All environmental objects are initialised with
    !! ::init_environment_objects().
    call init_environment_objects()

    !> ## Initialise base agent population objects ##
    !> New populations of agents are now built and initialised:
    !! (a) `generation_one`, (b) `generation_two`
    !! These population objects serve as targets for two pointer objects:
    !! (a) `proto_parents`, (b) `proto_offspring`.
    call LOG_MSG("INFO: Initialising generation one objects.")

    call stopwatch_op_current%start("Initialising agents: generations 1 and 2")

    !> - Initialise the whole `generation_one` of the agents,
    !!   commondata::popsize is the size of the population.
    call generation_one%init(POPSIZE)

    !> - Also initialise the `generation_two`, that will then take parents'
    !!   values.
    !! .
    call generation_two%init(POPSIZE)

    call LOG_MSG( stopwatch_op_current%log())

    !> Calculate initial fitness of the agents in the `generation_one` for the
    !! pre-evolution phase. At this stage fitness is equal to the maximum
    !! value (note that fitness is actually a reverse of fitness) and is not
    !! very interesting.
    call generation_one%fitness_calc()

    !> Place all the agents that have been initialised to random
    !! spatial positions in the safe habitat (`habitat_safe`), they
    !! have just the uniformly distributed spatial positions at
    !! start.
    !! @note Note that the initial vertical position and distribution of
    !!       the agents depends on these parameters:
    !!       - commondata::init_agents_depth_is_fixed
    !!       - commondata::init_agents_depth_is_gauss
    !!       .
    !!       See the_population::individ_posit_in_environ_uniform() for details.
    call generation_one%scatter_uniform(habitat_safe)
    call generation_two%scatter_uniform(habitat_safe)

    call LOG_MSG(LTAG_INFO // "Initialisation of generation one completed" )
    call LOG_DBG("Population with numeric ID " //                             &
                  TOSTR(generation_one%get_num_id()) //                       &
                  " and name '" //  trim(generation_one%get_name()) //        &
                  "' allocated to " // TOSTR(generation_one%get_size()) //    &
                  " objects.",  MODNAME, PROCNAME)

    !> ## Transfer pointers: parents and offspring populations ##
    !> Allocate the first `proto_parents` and `proto_offspring`
    !! population objects, they are pointers to `generation_one` and
    !! `generation_two` target objects.
    proto_parents => generation_one
    proto_offspring => generation_two

    !> Calculate statistical parameters of the initial generation for
    !! selective birth mortality. See
    !! the_population::population::mortality_birth().
    associate (AGENTS => proto_parents%individual )
      energy_mean_gen1_birth_mort = average( AGENTS%get_energ_birth() )
      energy_sd_gen1_birth_mort = std_dev( AGENTS%get_energ_birth() )
    end associate

    !> These values are then logged.
    call LOG_MSG(LTAG_STAGE //"Birth mortality values:" )
    call LOG_MSG(LTAG_STAGE //"  mean: " // TOSTR(energy_mean_gen1_birth_mort))
    call LOG_MSG(LTAG_STAGE //"  std.dev.:" // TOSTR(energy_sd_gen1_birth_mort))
    ! Also, a table showing the confidence limits is logged, it is useful
    ! for assessing possible limit on the birth energy evolution.
    call LOG_MSG(LTAG_INFO // "Limits of std.dev. for birth mortality:" )
    call LOG_MSG(LTAG_INFO // "  [ MEAN,  1 SD,  2 SD,  3 SD]")
    ! Template for aligned vals: [0.200, 0.409, 0.617, 0.825]
    call LOG_MSG(LTAG_INFO // "  [" //                                        &
          TOSTR(energy_mean_gen1_birth_mort,"(f5.3)") // ", " //              &
          TOSTR(energy_mean_gen1_birth_mort+                                  &
                energy_sd_gen1_birth_mort*1.0_SRP,"(f5.3)") // ", " //        &
          TOSTR(energy_mean_gen1_birth_mort+                                  &
                energy_sd_gen1_birth_mort*2.0_SRP,"(f5.3)") // ", " //        &
          TOSTR(energy_mean_gen1_birth_mort+                                  &
                energy_sd_gen1_birth_mort*3.0_SRP,"(f5.3)") // "]"    )

    !> ## Save diagnostics data ##
    !> Save initialisation data in the debug mode.
    call LOG_DBG(LTAG_INFO // "Sizes of populations after init:: " //         &
            "parents: " // TOSTR(size(proto_parents%individual)) //           &
            ", offspring: " // TOSTR( size(proto_offspring%individual) ) )

    !> - Saving histograms of agents' body length.
    call debug_histogram_save(x_data=proto_parents%individual%body_length,    &
                  csv_out_file="debug_hist_agent_body_len_birth_"// MMDD //   &
                      "_rev_" // SVN_Version //                               &
                      "_g" // TOSTR(Global_Generation_Number_Current) // csv, &
                  delete_csv=.FALSE., enable_non_debug=.TRUE. )
    !> - Saving histograms of agents' body mass.
    call debug_histogram_save(x_data=proto_parents%individual%body_mass,      &
                  csv_out_file="debug_hist_agent_body_mass_birth_"// MMDD //  &
                      "_rev_" // SVN_Version //                               &
                      "_g" // TOSTR(Global_Generation_Number_Current) // csv, &
                  delete_csv=.FALSE., enable_non_debug=.TRUE. )
    !> - Saving histograms of agents' energy.
    call debug_histogram_save(x_data=proto_parents%individual%energy_current, &
                  csv_out_file="debug_hist_agent_energy_birth_"// MMDD //     &
                      "_rev_" // SVN_Version //                               &
                      "_g" // TOSTR(Global_Generation_Number_Current) // csv, &
                  delete_csv=.FALSE., enable_non_debug=.TRUE. )
    !> - Saving histograms of agents' smr.
    !! .
    call debug_histogram_save(x_data=proto_parents%individual%smr,            &
                  csv_out_file="debug_hist_agent_smr_birth_"// MMDD //        &
                      "_rev_" // SVN_Version //                               &
                      "_g" // TOSTR(Global_Generation_Number_Current) // csv, &
                  delete_csv=.FALSE., enable_non_debug=.TRUE. )

    !> **SAVE_DATA_INIT block**: The random initialisation individual data
    !! for the whole parent population are saved to csv files:
    SAVE_DATA_INIT: block
      ! A temporary variable to keep the file names at initialising files.
      ! @note This variable is used only to set the names of the files once,
      !       the names afterwards are kept internally in the file handle
      !       objects
      character(len=:), allocatable :: output_data_file
      !> - Individual agent's data, file `init_agents_`;
      output_data_file = "init_agents_" // MODEL_NAME // "_" // MMDD //       &
                          "_rev_" // SVN_Version //                           &
                          "_gen_" // TOSTR(Global_Generation_Number_Current,  &
                          GENERATIONS) // csv
      call proto_parents%save_csv(output_data_file, is_logging=.TRUE.)
      !> - Initial genome data, file `init_genome_`.
      output_data_file = "init_genome_" // MODEL_NAME // "_" // MMDD //       &
                          "_rev_" // SVN_Version //                           &
                          "_gen_" // TOSTR(Global_Generation_Number_Current,  &
                          GENERATIONS) // csv
      call proto_parents%save_genomes_csv(output_data_file)
      !> - The number of time steps in the adaptive GA
      !! .
      output_data_file = "init_tsteps_" // MODEL_NAME // "_" // MMDD //       &
                          "_rev_" // SVN_Version //                           &
                          "_gen_" // TOSTR(Global_Generation_Number_Current,  &
                          GENERATIONS) // csv
      call preevol_steps_adaptive_save_csv(output_data_file)
      !> The generation wise statistics file `generations_` is opened for
      !! writing ...
      output_data_file="generations_" // MODEL_NAME // "_" // MMDD //         &
                                "_rev_" // SVN_Version // csv
      call file_stats_gener%open_write( output_data_file, FORMAT_CSV )
      !> ... and the first row of column names `FILE_STATS_GENER_COLS` is
      !! written.
      !! @note Note that the main body of the statistical data is processed
      !!       in the sub-procedure ::generation_stats_record_write().
      file_stats_gener_record = repeat(" ", FILE_STATS_RECORD_LEN )
      call CSV_RECORD_APPEND( file_stats_gener_record, FILE_STATS_GENER_COLS )
      call file_stats_gener%record_write( file_stats_gener_record )
    end block SAVE_DATA_INIT

    !> The average distance between the food items is reported to the log.
    !! The average distance between the food items is good to know, e.g. to
    !! compare it with the agent's random walk step size.
    call LOG_DBG(LTAG_INFO // "Average distance between food items in the " //&
                      habitat_dangerous%habitat_name // " habitat: "//        &
                      TOSTR(habitat_dangerous%food%distance_average(100)),    &
                      PROCNAME, MODNAME )
    call LOG_DBG(LTAG_INFO // "Average distance between food items in the " //&
                      habitat_safe%habitat_name // " habitat: "//             &
                      TOSTR(habitat_safe%food%distance_average(100)),         &
                      PROCNAME, MODNAME )

    !> # Pre-evolution stage #
    !> Pre-evolution stage involves the Genetic Algorithm that is based on
    !! selection of agents based on an explicit global fitness. It aims to
    !! produce a population of agents that can stably sustain for the whole
    !! commondata::lifespan
    !> ## GENERATIONS_PREEVOL: The main loop of (pre-) evolution ##
    !> At this stage the main loop of generations evolving is started.
    !! The conditions for **continuing** the main evolution loop are as
    !! follows:
    GENERATIONS_PREEVOL: do while (                                           &
              !> - Global generation number does not exceed the maximum
              !!   commondata::generations.
              Global_Generation_Number_Current <= GENERATIONS                 &
              .and.                                                           &
              !> - Average (anti-) fitness still exceeds the target value.
              !! .
              average(proto_parents%individual%fitness) > 500 )

      call LOG_DELIMITER(LOG_LEVEL_VOLUME)
      call LOG_MSG( LTAG_STAGE // LTAG_MAJOR // " Starting GENERATION: " //   &
                      TOSTR(Global_Generation_Number_Current) )
      call LOG_DELIMITER(LOG_LEVEL_VOLUME)

      !> #### Diagnostics ####
      !> Stopwatch object for calculating time since generation start is
      !! initialised.
      call stopwatch_generation%start("Generation "//                         &
                                      TOSTR(Global_Generation_Number_Current))

      !> Initially, place all the agents in the `proto_parents` population
      !! randomly uniformly in the safe habitat (`habitat_safe`).
      !! However, note that the initial vertical position and distribution of
      !! the agents depends on these parameters:
      !! - commondata::init_agents_depth_is_fixed
      !! - commondata::init_agents_depth_is_gauss
      !! .
      !! See the_population::individ_posit_in_environ_uniform() method for
      !! details.
      call proto_parents%scatter_uniform(habitat_safe)

      !> Initialise the global generation-wise counter of the number of
      !! agents that die as a consequence of predation
      !! the_population::global_ind_n_eaten_by_predators, as opposed to
      !! starvation.
      Global_Ind_N_Eaten_by_Predators = 0

      !> If it is **not** the first generation, replenish all food items (i.e.
      !! for all habitats), they are restored to the "available" (non-eaten)
      !! state. Two methods can be used here:
      !! - the_environment::food_resource::make() -- re-initialise food items
      !!   from scratch.
      !! - the_environment::food_resource::replenish() -- reuse food items as
      !!   initialised in ::init_environment_objects()
      !! .
      REPLENISH_FOOD: if ( Global_Generation_Number_Current > 1 ) then
        call habitat_safe%food%replenish()
        call habitat_dangerous%food%replenish()
        !> The global habitat array the_environment::global_habitats_available
        !! is then updated by the_environment::assemble() procedure.
        call assemble( habitat_safe, habitat_dangerous )
      end if REPLENISH_FOOD
      !> If it is the first generation, it does not make sense doing this as
      !! the environment has been already fully initialised in the
      !! ::init_environment_objects() procedure.

      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !> ### lifecycle_preevol for proto_parents ###
      !> Start the loop of the life cycle of all agents of the `proto_parents`.
      !! It includes commondata::preevol_tsteps time steps. (Note that
      !! commondata::preevol_tsteps is less than commondata::lifespan).
      !! This is implemented in the ::lifecycle_preevol() procedure.
      call LOG_MSG( LTAG_STAGE // "Life cycle parents." )
      call lifecycle_preevol( proto_parents )
      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      !> Calculate the number of agents alive and agents growing. These values
      !! are used later, including as a criterion of GA deterioration.
      n_alive = count( proto_parents%individual%is_alive() )
      n_growing = count( proto_parents%individual%get_mass() >                &
                         proto_parents%individual%get_mass_birth() )

      !> Report these values in the logger.
      call LOG_MSG( LTAG_INFO // "N alive: " // TOSTR(n_alive) //             &
                                 ", N grown: " // TOSTR(n_growing) )

      !> After the agents went through their life cycle, their fitness
      !! is processed.
      !> - Fitness of all `proto_parents` agents is recalculated following
      !!   their performance in the full lifecycle.
      call LOG_MSG( LTAG_STAGE // "Fitness calculate in parents." )
      call proto_parents%fitness_calc()

      !> - The agents `proto_parents` are sorted by fitness.
      call LOG_MSG( LTAG_STAGE // "Sort parents by fitness." )
      call proto_parents%sort_by_fitness()

      ! Output the best parent to logger.
      call LOG_MSG( LTAG_INFO // "Best parent (1), fitness: " //              &
                    TOSTR(proto_parents%individual(1)%fitness) )

      !> If this is the first generation, determine the GA deterioration
      !! stopping parameters, evolution "failure"
      PARAMS_GEN_1: if (Global_Generation_Number_Current == 1) then
        ga_alive_generation_1 = n_alive
        ga_growing_generation_1 = n_growing
        !> These Generation one parameters are also reported to the logger.
        call LOG_MSG( LTAG_INFO // "This is the first generation" )
        call LOG_MSG( LTAG_INFO // "Survival parameters that determine " //   &
                      "the stopping rule: N Alive=" //                        &
                      TOSTR(ga_alive_generation_1) //                         &
                      ", N Growing=" //                                       &
                      TOSTR(ga_growing_generation_1)  )
      end if PARAMS_GEN_1

      !> - **SAVE_DATA_INDS_GENERATION block**: The individual statistical
      !!   data for the whole `proto_parents` population are saved using the
      !!   the_population::population class bound `save_` methods:
      SAVE_DATA_INDS_GENERATION: block
        character(len=:), allocatable :: output_data_file
        call LOG_MSG( LTAG_STAGE // "Saving parents." )
        !>   - Individual agent's data, file `agents_`
        output_data_file = "agents_" // MODEL_NAME // "_" // MMDD //          &
                            "_rev_" // SVN_Version //                         &
                            "_gen_" // TOSTR(Global_Generation_Number_Current,&
                            GENERATIONS) // "_p1_parents" // csv
        call proto_parents%save_csv(output_data_file, is_logging=.TRUE.)
        !>   - The genome data, file `genome_`
        output_data_file = "genomes_" // MODEL_NAME // "_" // MMDD  //        &
                            "_rev_" // SVN_Version //                         &
                            "_gen_" // TOSTR(Global_Generation_Number_Current,&
                            GENERATIONS) // csv
        call proto_parents%save_genomes_csv(output_data_file)
        !>   - Memory stacks data, file `memory_`.
        output_data_file = "memory_" // MODEL_NAME // "_" // MMDD //          &
                            "_rev_" // SVN_Version //                         &
                            "_gen_" // TOSTR(Global_Generation_Number_Current,&
                            GENERATIONS) // csv
        call proto_parents%save_memory_csv(output_data_file)
        !>   - Movement history data, file `movements_`.
        output_data_file = "movements_" // MODEL_NAME // "_" // MMDD //       &
                            "_rev_" // SVN_Version //                         &
                            "_gen_" // TOSTR(Global_Generation_Number_Current,&
                            GENERATIONS) // csv
        call proto_parents%save_movements_csv(output_data_file)
        !>   - Behaviour history data, file `behaviours_`.
        output_data_file = "behaviours_" // MODEL_NAME // "_" // MMDD //      &
                            "_rev_" // SVN_Version //                         &
                            "_gen_" // TOSTR(Global_Generation_Number_Current,&
                            GENERATIONS) // csv
        call proto_parents%save_behaviour_csv(output_data_file)
      end block SAVE_DATA_INDS_GENERATION

      !> - **SAVE_DATA_FOOD_POST**: The food resources data for all the
      !!   habitats are saved using the
      !!   the_environment::food_resource::save_csv() method.
      SAVE_DATA_FOOD_POST: block
        character(len=:), allocatable :: output_data_file
        call LOG_MSG( LTAG_STAGE // "Saving food resources." )
        output_data_file = "food_habitat_safe_gen_" //                        &
                            MODEL_NAME // "_" // MMDD //                      &
                            "_rev_" // SVN_Version //                         &
                            "_gen_" // TOSTR(Global_Generation_Number_Current,&
                            GENERATIONS) // csv
        call habitat_safe%food%save_csv( output_data_file )
        output_data_file = "food_habitat_dang_gen_" //                        &
                            MODEL_NAME // "_" // MMDD //                      &
                            "_rev_" // SVN_Version //                         &
                            "_gen_" // TOSTR(Global_Generation_Number_Current,&
                            GENERATIONS) // csv
        call habitat_dangerous%food%save_csv( output_data_file )
      end block SAVE_DATA_FOOD_POST

      !> - Generation-wise statistics are calculated and saved in the CSV
      !!   file. This is implemented in the ::generation_stats_record_write()
      !!   subprocedure.
      call generation_stats_record_write()

      !> - Check if the unsuccessful evolution criterion is met. If yes,
      !!   terminate the GA.
      !>    - The number of agents that are alive exceeds that in the
      !!      first generation: there must be improvement (at least in
      !!      debug).
      !>    - The number of agents that have grown exceeds that in the
      !!      first generation.
      !!    .
      !! .
      CHECK_DETERIORATE: if ( Global_Generation_Number_Current > 1 ) then
        if ( n_alive < ga_alive_generation_1     / 10  .or.                   &
             n_growing < ga_growing_generation_1 / 10 ) then
          call LOG_MSG( LTAG_MAJOR // "GA deterioration detected! " //        &
                        "N alive=" // TOSTR(n_alive) //                       &
                        " (<" // TOSTR(ga_alive_generation_1) //              &
                        "); N grown=" // TOSTR(n_growing) //                  &
                        " (<" // TOSTR(ga_growing_generation_1) // ")." )
          call LOG_MSG( LTAG_CRIT //                                          &
                        "Exiting GA due to deterioration in CHECK_DETERIORATE.")
          exit GENERATIONS_PREEVOL
        end if
      !> - If this is the first generation, terminate GA if the number of
      !!   agents alive < 1/100 of the commpndata::popsize or if no agents are
      !!   growing.
      !! .
      else CHECK_DETERIORATE
        if ( n_alive < POPSIZE / 100 ) then
          call LOG_MSG( LTAG_CRIT // "Insufficient number of alive agents: "  &
                        // TOSTR(n_alive) )
          call system_halt(message="INSUFFICIENT ALIVE AGENTS IN GEN. 1")
        elseif ( n_growing < 1 ) then
          call LOG_MSG( LTAG_WARN // "LESS THAN ONE AGENT GROWN IN GEN. 1" )
          ! call system_halt(message="LESS THAN ONE AGENT GROWN IN GEN. 1")
        end if
      end if CHECK_DETERIORATE

      !> ### Selection ###
      !> Select reproducing minority: the_evolution::selection()
      call LOG_MSG( LTAG_STAGE // "Selection (elitism)." )
      call selection()

      !> ### Exchange of the genetic material ###
      !> A minority of the best parents produces the proto_offspring population
      !! object: the_evolution::mate_reproduce().
      call LOG_MSG( LTAG_STAGE // "Mate and reproduce." )
      call mate_reproduce()

      !> Reset individual IDs of proto_offspring
      call proto_offspring%reset_id()

      !> ### Finalise the generation cycle: swap pointers ###
      !> Swap populations: `proto_offspring` are now `proto_parents`:
      !! the_evolution::generations_swap().
      call LOG_MSG( LTAG_STAGE // "Swap generations." )
      call generations_swap()

      ! Log generation timing
      call LOG_DELIMITER(LOG_LEVEL_VOLUME)
      call LOG_MSG ( LTAG_MAJOR // stopwatch_generation%show() )
      call LOG_DELIMITER(LOG_LEVEL_VOLUME)

      !> ### End of the generations loop ###
      !> Finally, the global generation counter
      !! commondata::global_generation_number_current is incremented by one.
      Global_Generation_Number_Current = Global_Generation_Number_Current + 1

    end do GENERATIONS_PREEVOL

    !> After all generations were done, the CSV file `file_stats_gener` that
    !! saves generation-wise statistics is closed.
    call file_stats_gener%close()

    ! > # Evolution stage #
    ! > This version of the model stops at the pre-evolution stage. Therefore,
    ! ! The evolution (GA) mechanism based on "elitism" is simplistic and not
    ! ! fairly realistic.
    call LOG_MSG( LTAG_INFO // "Best fitness evolved: " //                    &
                  TOSTR( proto_parents%individual(1)%fitness ) )

    call LOG_DELIMITER(LOG_LEVEL_VOLUME)
    call LOG_MSG( LTAG_MAJOR // "Simulation completed." )
    call LOG_MSG( LTAG_MAJOR // LTAG_TIMER // stopwatch_global%show() )
    call LOG_DELIMITER(LOG_LEVEL_VOLUME)

    !> # System terminates #
    !> Finally, the concluding procedure commondata::system_halt() is called
    !! for the normal termination of the model.
    call system_halt( is_error = .FALSE.,                                     &
                      message = "Normal termination of the pre-evolution" )

    contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !> This subroutine implements the full life cycle in a whole population of
    !! agents. It is built around the main loop `LIFECYCLE_PREEVOL_LOOP`.
    subroutine lifecycle_preevol( active_population )
      class(POPULATION), intent(inout) :: active_population

      ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
      character(len=*), parameter :: PROCNAME = "(lifecycle_preevol)"

      ! This is a counter for time steps of the model in the second loop of
      ! pre-evolution stage. It is separate from the
      ! commondata::global_time_step_model_current.
      integer :: time_step

      ! Counters
      integer :: i

      ! Adaptive number of time steps for the GA.
      integer :: steps_gen_current

      ! ### Notable variables ###
      ! #### Time-step-wise statistics data ####
      ! Objects for producing time-steps statistics for each generation
      ! - `filename_time_steps` -- the name of the csv data file;
      character(len=FILENAME_LENGTH) :: filename_time_steps

      ! - `dpos_mean_agents`, `dpos_mean_foods_hab_safe` ... - arrays to save
      !    mean depth and std.dev. of depth of agents and food items at each
      !    time step.
      real(SRP), allocatable, dimension(:) :: dpos_mean_agents,               &
                                              dpos_min_agents,                &
                                              dpos_max_agents,                &
                                              dpos_mean_foods_hab_safe,       &
                                              dpos_min_foods_hab_safe,        &
                                              dpos_max_foods_hab_safe,        &
                                              dpos_mean_foods_hab_dang,       &
                                              dpos_min_foods_hab_dang,        &
                                              dpos_max_foods_hab_dang,        &
                                              dpos_sd_agents,                 &
                                              dpos_sd_foods_hab_safe,         &
                                              dpos_sd_foods_hab_dang
      ! - `n_agents_alive` ... - integer arrays to save the counts of alive
      !    agents and available (not eaten) food items in each habitat;
      integer, allocatable, dimension(:) ::   n_agents_alive,                 &
                                              n_agents_eaten,                 &
                                              n_foods_available_safe,         &
                                              n_foods_available_dang

      ! - `temp_val_select_alive` -- temporary array to keep the agent (depth)
      !    data for only 'alive' agents;
      real(SRP), dimension(POPSIZE) :: temp_val_select_alive ! select 'alive'

      ! - `temp_val_select_avail_safe` -- temporary arrays to keep the food
      !    item's (depth) data for only those food items that are 'available'.
      ! .
      real(SRP), dimension(FOOD_ABUNDANCE_HABITAT_SAFE) ::                    &
                                          temp_val_select_avail_safe
      real(SRP), dimension(FOOD_ABUNDANCE_HABITAT_DANGER) ::                  &
                                          temp_val_select_avail_dang

      !> ### Implementation details ###
      !> First, the ages of all agents are reset to 0 before the cycle of their
      !! life (time steps of the model).
      call active_population%individual%age_reset()

      !> Second, each generation is subjected to selective birth mortality
      !! by the_population::population::mortality_birth() at birth, before
      !! the first time step.
      !! @note Forced mean and sd values from generation 1 data. Normally
      !!       must be obtained from the first generation data, with global
      !!       parameters added.
      call active_population%mortality_birth(                                 &
                                  energy_mean = energy_mean_gen1_birth_mort,  &
                                  energy_sd = energy_sd_gen1_birth_mort )

      !> Then, calculate the number of time steps for the current generation.
      !! The number of time steps is based on the adaptive algorithm
      !! implemented in the the_evolution::preevol_steps_adaptive() function.
      steps_gen_current = preevol_steps_adaptive()

      !> The arrays that keep time-step wise statistics for the current
      !! generation are allocated with the above number of time steps.
      allocate( dpos_mean_agents( steps_gen_current)          )
      allocate( dpos_min_agents( steps_gen_current)           )
      allocate( dpos_max_agents( steps_gen_current)           )
      allocate( dpos_mean_foods_hab_safe( steps_gen_current ) )
      allocate( dpos_min_foods_hab_safe( steps_gen_current )  )
      allocate( dpos_max_foods_hab_safe( steps_gen_current )  )
      allocate( dpos_mean_foods_hab_dang( steps_gen_current ) )
      allocate( dpos_min_foods_hab_dang( steps_gen_current )  )
      allocate( dpos_max_foods_hab_dang( steps_gen_current )  )
      allocate( dpos_sd_agents( steps_gen_current )           )
      allocate( dpos_sd_foods_hab_safe( steps_gen_current )   )
      allocate( dpos_sd_foods_hab_dang( steps_gen_current )   )
      !> Some of these arrays are integer counts.
      allocate( n_agents_alive( steps_gen_current )           )
      allocate( n_agents_eaten( steps_gen_current )           )
      allocate( n_foods_available_safe( steps_gen_current )   )
      allocate( n_foods_available_dang( steps_gen_current )   )

      !> ... and they also initialised to commondata::missing value
      !! (integer arrays to commondata::unknown).
      dpos_mean_agents         = MISSING
      dpos_min_agents          = MISSING
      dpos_max_agents          = MISSING
      dpos_mean_foods_hab_safe = MISSING
      dpos_min_foods_hab_safe  = MISSING
      dpos_max_foods_hab_safe  = MISSING
      dpos_mean_foods_hab_dang = MISSING
      dpos_min_foods_hab_dang  = MISSING
      dpos_max_foods_hab_dang  = MISSING
      dpos_sd_agents           = MISSING
      dpos_sd_foods_hab_safe   = MISSING
      dpos_sd_foods_hab_dang   = MISSING
      n_agents_alive           = UNKNOWN
      n_agents_eaten           = UNKNOWN
      n_foods_available_safe   = UNKNOWN
      n_foods_available_dang   = UNKNOWN

      !> Start the main life cycle loop `LIFECYCLE_PREEVOL_LOOP` over all the
      !! time steps (limited by the adaptive algorithm).
      LIFECYCLE_PREEVOL_LOOP: do time_step = 1, steps_gen_current

        !> Reset/update the global commondata::global_time_step_model_current.
        Global_Time_Step_Model_Current = time_step

        if (IS_DEBUG) call LOG_DELIMITER(LOG_LEVEL_CHAPTER)
        call LOG_MSG( LTAG_STAGE // "Time step: " //                          &
                      TOSTR(Global_Time_Step_Model_Current) //                &
                      " of Generation " //                                    &
                      TOSTR(Global_Generation_Number_Current) //              &
                      " (adaptive maximum: " // TOSTR(steps_gen_current)      &
                      // ")." )

        !> #### Prepare the environment ####
        !> Perform the sinusoidal vertical migration of the food items,
        !! they are relocating to the depth appropriate for specific
        !! time step of the model. Food migration is done here with the
        !! the_environment::migrate_food_vertical() directly on the global
        !! array of habitats the_environment::global_habitats_available to
        !! avoid the need to synchronise the array with the habitat objects.
        call migrate_food_vertical( Global_Habitats_Available )

        !> The average distance between the food items is reported to the log.
        !! The average distance between the food items is good to know, e.g. to
        !! compare it with the agent's random walk step size.
        call LOG_DBG("Average distance between food items in the " //         &
                          habitat_dangerous%habitat_name // " habitat: "//    &
                          TOSTR(habitat_dangerous%food%distance_average(100)))
        call LOG_DBG("Average distance between food items in the " //         &
                          habitat_safe%habitat_name // " habitat: "//         &
                          TOSTR(habitat_safe%food%distance_average(100)))

        !> #### Habitat-specific mortality ####
        !> Agents are subjected to random habitat-specific mortality by
        !! calling the_population::population::mortality_habitat().
        !! @warning Mortality is so far disabled.
        !call active_population%mortality_habitat()

        !> #### Agents do a single time step of life ####
        !> Perform a single step of the life cycle of the whole population of
        !! agents. The agents do this step of their life cycle in a random
        !! (or non-random) order.
        !! See the_population::population::lifecycle_step() for details.
        call active_population%lifecycle_step()
        ! Eat only lifecycle step, for debugging and testing.
        !call active_population%lifecycle_eatonly()

        !> Immediately after the time step is done, time step-wise statistics
        !! are calculated.
        where ( active_population%individual%is_alive() )
          temp_val_select_alive = active_population%individual%dpos()
        elsewhere
          temp_val_select_alive = MISSING
        end where
        dpos_mean_agents(time_step) = average( temp_val_select_alive )
        dpos_min_agents(time_step)  = minval( temp_val_select_alive,          &
                                              temp_val_select_alive/=MISSING )
        dpos_max_agents(time_step)  = maxval( temp_val_select_alive,          &
                                              temp_val_select_alive/=MISSING )
        dpos_sd_agents(time_step)   = std_dev( temp_val_select_alive )
        n_agents_alive(time_step)   = count(                                  &
                              active_population%individual%is_alive() )
        n_agents_eaten(time_step)   = Global_Ind_N_Eaten_by_Predators

        !> The habitat and food resource data are disassembled back into the
        !! original static habitat objects out of the global array
        !! the_environment::global_habitats_available. This transfers the
        !! changes in the food resources (e.g. the agents consume the food)
        !! from the global array back to the original static habitat objects.
        !! See the_environment::disassemble() procedure.
        call disassemble( habitat_safe, habitat_dangerous )

        !> Now, the time-step-wise habitat statistics can be computed for the
        !! current time step.
        where ( habitat_safe%food%food%is_available() )
          temp_val_select_avail_safe = habitat_safe%food%food%dpos()
        elsewhere
          temp_val_select_avail_safe = MISSING
        end where
        dpos_mean_foods_hab_safe(time_step)=average(temp_val_select_avail_safe)
        dpos_min_foods_hab_safe(time_step)=minval(temp_val_select_avail_safe, &
                                           temp_val_select_avail_safe/=MISSING)
        dpos_max_foods_hab_safe(time_step)=maxval(temp_val_select_avail_safe, &
                                           temp_val_select_avail_safe/=MISSING)
        dpos_sd_foods_hab_safe(time_step)=std_dev(temp_val_select_avail_safe)
        n_foods_available_safe(time_step)=count(                              &
                                    habitat_safe%food%food%is_available())

        where ( habitat_dangerous%food%food%is_available() )
          temp_val_select_avail_dang = habitat_dangerous%food%food%dpos()
        elsewhere
          temp_val_select_avail_dang = MISSING
        end where
        dpos_mean_foods_hab_dang(time_step)=average(temp_val_select_avail_dang)
        dpos_min_foods_hab_dang(time_step)=minval(temp_val_select_avail_dang, &
                                           temp_val_select_avail_dang/=MISSING)
        dpos_max_foods_hab_dang(time_step)=maxval(temp_val_select_avail_dang, &
                                           temp_val_select_avail_dang/=MISSING)
        dpos_sd_foods_hab_dang(time_step)=std_dev(temp_val_select_avail_dang)
        n_foods_available_dang(time_step)=count(                              &
                                    habitat_dangerous%food%food%is_available())

        !> #### Maximum rescale motivation updated ####
        !> The population-wise maximum motivation parameter
        !! commondata::global_rescale_maximum_motivation is updated based on
        !! the global maximum value.
        Global_Rescale_Maximum_Motivation =                                   &
            maxval( active_population%individual%motivations%max_perception() )

        !> #### The agents are subjected to predation ####
        !> It is implemented by cycling over all predators within the safe
        !! and dangerous habitat and calling the
        !! the_population::population::attacked() method for each predator.
        !> - Safe habitat: `PREDATION_HAB_SAFE` block;
        if (LIFECYCLE_PREDATION_DISABLED_DEBUG .eqv. .FALSE.) then
          PREDATION_HAB_SAFE: do i = 1, habitat_safe%predators_number
            call active_population%attacked( habitat_safe%predators(i) )
          end do PREDATION_HAB_SAFE
          !> - Dangerous habitat: `PREDATION_HAB_DANGER` block.
          !! .
          PREDATION_HAB_DANGER: do i = 1, habitat_dangerous%predators_number
            call active_population%attacked( habitat_dangerous%predators(i) )
          end do PREDATION_HAB_DANGER
        end if

        ! > Recalculate fitness in the debug mode: Fitness of all agents in the
        ! ! population is recalculated at each step of the life cycle.
        if (IS_DEBUG) then
          call active_population%fitness_calc()
          ! > The minimum fitness is reported to the logger for each time step
          ! ! in the debug mode.
          call LOG_DBG( LTAG_INFO // "Best fitness is "  //                   &
                        TOSTR(minval(active_population%individual%fitness)),  &
                        PROCNAME, MODNAME  )
          call LOG_DBG( LTAG_INFO // "Global maximum motivation: " //         &
                        TOSTR(Global_Rescale_Maximum_Motivation),             &
                        PROCNAME, MODNAME  )
        end if

      end do LIFECYCLE_PREEVOL_LOOP

      !> #### Save time-step-wise data ####
      !> After the life cycle loop is completed, time-step-wise statistics are
      !! saved into CSV data file for the current generation.
      filename_time_steps="timesteps_" // MODEL_NAME // "_" // MMDD //        &
                      "_rev_" // SVN_Version //                               &
                      "_gen_" // TOSTR(Global_Generation_Number_Current,      &
                        GENERATIONS) // csv

      call CSV_MATRIX_WRITE (                                                 &
                          reshape( [ dpos_mean_agents,                    & !  1
                                     dpos_min_agents,                     & !  2
                                     dpos_max_agents,                     & !  3
                                     dpos_sd_agents,                      & !  4
                                     real(n_agents_alive,SRP),            & !  5
                                     real(n_agents_eaten,SRP),            & !  6
                                     dpos_mean_foods_hab_safe,            & !  7
                                     dpos_min_foods_hab_safe,             & !  8
                                     dpos_max_foods_hab_safe,             & !  9
                                     dpos_sd_foods_hab_safe,              & ! 10
                                     real(n_foods_available_safe,SRP),    & ! 11
                                     dpos_mean_foods_hab_dang,            & ! 12
                                     dpos_min_foods_hab_dang,             & ! 13
                                     dpos_max_foods_hab_dang,             & ! 14
                                     dpos_sd_foods_hab_dang,              & ! 15
                                     real(n_foods_available_dang,SRP) ],  & ! 16
                                   [ steps_gen_current, 16 ] ),           &
                          filename_time_steps,                            &
                          [ "DEP_AGENTS_MEAN",                            & !  1
                            "DEP_AGENTS_MIN ",                            & !  2
                            "DEP_AGENTS_MAX ",                            & !  3
                            "DEP_AGENTS_SD  ",                            & !  4
                            "AGENTS_ALIVE   ",                            & !  5
                            "N_EATEN_PRED   ",                            & !  6
                            "DEP_F_MEAN_SAFE",                            & !  7
                            "DEP_F_MIN_SAFE ",                            & !  8
                            "DEP_F_MAX_SAFE ",                            & !  9
                            "DEP_F_SD_SAFE  ",                            & ! 10
                            "FOOD_AVAIL_SAFE",                            & ! 11
                            "DEP_F_MEAN_DANG",                            & ! 12
                            "DEP_F_MIN_DANG ",                            & ! 13
                            "DEP_F_MAX_DANG ",                            & ! 14
                            "DEP_F_SD_DANG  ",                            & ! 15
                            "FOOD_AVAIL_DANG" ] )                           ! 16

      !> The CSV output data file can be optionally compressed with the
      !! commondata::cmd_zip_output command if commondata::is_zip_outputs is set
      !! to TRUE.
      if ( IS_ZIP_OUTPUTS ) then
        call call_external(command=CMD_ZIP_OUTPUT//" "//filename_time_steps,  &
                          suppress_output=.TRUE.,                             &
                          is_background_task=ZIP_OUTPUTS_BACKGROUND )
      end if

    end subroutine lifecycle_preevol

    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !> Save generation-wise statistics. This procedure only writes a single
    !! record of data after each generation. Opening the file, definition of
    !! the file handling objects that are used here etc. are done in the
    !! upstream procedure the_evolution::generations_loop_ga().
    !! @warning This subroutine neither opens nor closes the output CSV file,
    !!          only writes a single record of statistical data from the
    !!          current generation commondata::global_generation_number_current
    !!          into it.
    subroutine generation_stats_record_write()

      ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
      character(len=*), parameter ::                                          &
                              PROCNAME = "(generation_stats_record_write)"

      integer :: i
      real(SRP), dimension(proto_parents%population_size) :: perc_food
      real(SRP), dimension(proto_parents%population_size) :: perc_f_dist
      real(SRP), dimension(proto_parents%population_size) :: perc_consp
      real(SRP), dimension(proto_parents%population_size) :: perc_pred

      ! - Arrays to calculate means for alive agents
      real(SRP), dimension(proto_parents%population_size) :: body_mass_l
      real(SRP), dimension(proto_parents%population_size) :: body_leng_l
      real(SRP), dimension(proto_parents%population_size) :: energy_l
      real(SRP), dimension(proto_parents%population_size) :: stomach_l
      real(SRP), dimension(proto_parents%population_size) :: smr_l
      real(SRP), dimension(proto_parents%population_size) :: control_l
      real(SRP), dimension(proto_parents%population_size) :: reprfac_l
      real(SRP), dimension(proto_parents%population_size) :: p_repr_l
      integer,   dimension(proto_parents%population_size) :: foods_try_l
      integer,   dimension(proto_parents%population_size) :: foods_eaten_l
      real(SRP), dimension(proto_parents%population_size) :: fmass_eaten_l
      integer,   dimension(proto_parents%population_size) :: fitness_l

      !> ### Implementation notes ###
      !! First, initialise an empty record for CSV data.
      file_stats_gener_record = repeat(" ", FILE_STATS_RECORD_LEN )

      !> Then calculate and append each of the statistical data fields to
      !! build the complete record of the CSV output file. Note that the
      !! fields must agree with the columns defined by the
      !! `FILE_STATS_GENER_COLS` parameter array.
      !! - "GENERATION" -- generation number;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & !  1
                            Global_Generation_Number_Current )
      !> - "PREEVOL_STEPS" -- lifespan, number of time steps;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & !  2
                      preevol_steps_adaptive(Global_Generation_Number_Current) )
      !> - "MUTAT_POINT" -- adaptive rate of point mutations;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & !  3
                    proto_parents%ga_mutat_adaptive(MUTATIONRATE_POINT,       &
                                                    GA_MUTATIONRATE_POINT_MAX))
      !> - "MUTAT_BATCH" -- adaptive rate of batch mutations;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & !  4
                    proto_parents%ga_mutat_adaptive(MUTATIONRATE_BATCH,       &
                                                    GA_MUTATIONRATE_BATCH_MAX))
      !> - "ELITE_GROUP" -- the number of reproducing agents;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & !  5
                                      proto_parents%ga_reproduce_max() )
      !> - "N_ALIVE" -- number of agents alive at the end;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & !  6
                            count( proto_parents%individual%is_alive() ) )
      !> - "N_GROWN" -- number of agents that had grown;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & !  7
                count( proto_parents%individual%get_mass() >                  &
                       proto_parents%individual%get_mass_birth() ) )
      !> - "N_MALES_L" -- number of males alive;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & !  8
                      count( proto_parents%individual%is_male() .and.         &
                              proto_parents%individual%is_alive() ) )
      !> - "N_FEMALES_L" number of females alive;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & !  9
                      count( proto_parents%individual%is_female() .and.       &
                              proto_parents%individual%is_alive() ) )
      !> - "N_EATEN_PRED" number of agents that are eaten by predators;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 10
                      Global_Ind_N_Eaten_by_Predators )
      !> - "BODY_MASS" -- average body mass;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 11
                            average(proto_parents%individual%body_mass,       &
                                    MISSING, .FALSE.) )
      !> - "BODY_LEN" -- average body length;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 12
                            average(proto_parents%individual%body_length,     &
                                    MISSING, .FALSE.) )
      !> - "BIRTH_MASS" -- body mass at birth;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 13
                        average(proto_parents%individual%body_mass_birth,     &
                                    MISSING, .FALSE.) )
      !> - "BIRTH_LENGTH" -- body length at birth;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 14
                        average(proto_parents%individual%body_length_birth,   &
                                    MISSING, .FALSE.) )
      !> - "BIRTH_ENERGY" -- energy reserves at birth;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 15
                        average(proto_parents%individual%energy_birth,        &
                                    MISSING, .FALSE.) )
      !> - "ENERGY" -- energy reserve;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 16
                            average(proto_parents%individual%energy_current,  &
                                    MISSING, .FALSE.) )
      !> - "STOMACH" -- stomach contents, mass;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 17
                      average(proto_parents%individual%stomach_content_mass,  &
                              MISSING, .FALSE.) )
      !> - "SMR" -- average SMR;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 18
                            average(proto_parents%individual%smr,             &
                                    MISSING, .FALSE.) )
      !> - "CTRL_RND" -- average control trait;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 19
                        average(proto_parents%individual%control_unselected,  &
                                    MISSING, .FALSE.) )
      !> - "REPRFACT" -- average reproductive factor;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 20
                      average(proto_parents%individual%reproductive_factor(), &
                              MISSING, .FALSE.) )
      !> - "P_REPR" -- probability of reproduction;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 21
        average( [( proto_parents%individual(i)%probability_reproduction(),   &
                    i=1, proto_parents%population_size )] )  )
      !> - "N_REPROD" -- total nuber or reproductions;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 22
                          average(proto_parents%individual%n_reproductions,   &
                                  UNKNOWN, .FALSE.) )
      !> - "N_OFFSPRING" -- number of offspring;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 23
                            average(proto_parents%individual%n_offspring ,    &
                                    UNKNOWN, .FALSE.) )
      !> - "GOS_AROUSAL" -- GOS arousal;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 24
                            average(proto_parents%individual%gos_arousal,     &
                                    MISSING, .FALSE.) )
      !> - "FOODS_TRY" -- average number of attempts to catch food items;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 25
                      average(proto_parents%individual%n_eats_all_indicator,  &
                              UNKNOWN, .FALSE.) )
      !> - "FOODS_EATEN" -- average number of food items eaten;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 26
                      average(proto_parents%individual%n_eaten_indicator,     &
                              UNKNOWN, .FALSE.) )
      !> - "FMASS_EATEN" -- average number of food items eaten;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 27
                      average(proto_parents%individual%mass_eaten_indicator,  &
                              MISSING, .FALSE.) )
      !> - "PERC_FOOD" -- food perception, average;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 28
          average(proto_parents%individual%memory_stack%get_food_mean_n(),    &
                  MISSING, .FALSE.) )
      !> - "PERC_CONS" -- conspecific perception, average;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 29
          average(proto_parents%individual%memory_stack%get_consp_mean_n(),   &
                  MISSING, .FALSE.) )
      !> - "PERC_PRED" -- predator perception, average;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  &  !30
          average(proto_parents%individual%memory_stack%get_pred_mean(),      &
                  MISSING, .FALSE.) )
      !> - "DEPTH" -- location depth at the end,
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 31
          average(proto_parents%individual%depth, MISSING, .FALSE.) )
      !> - "N_SAFE_HABITAT" -- number of agents in the "safe" habitat;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 32
                count( proto_parents%individual%is_within( habitat_safe ) ) )
      !> - "N_DANG_HABITAT" -- number of agents in the "dangerous" habitat.
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 33
                count(proto_parents%individual%is_within( habitat_dangerous )))
      !> - Calculate perception averages in the safe habitat:
      perc_food = MISSING; perc_consp = MISSING; perc_pred = MISSING
      !    - `IN_SAFE` block calculates statistics arrays in the safe
      !       habitat.
      IN_SAFE: where( proto_parents%individual%is_within( habitat_safe ) )
        perc_food = proto_parents%individual%memory_stack%get_food_mean_n()
        perc_f_dist = proto_parents%individual%memory_stack%                  &
                                  get_food_mean_dist(undef_ret_null=.FALSE.)
        perc_consp = proto_parents%individual%memory_stack%get_consp_mean_n()
        perc_pred = proto_parents%individual%memory_stack%get_pred_mean()
      end where IN_SAFE
      !>    - "PERC_FOOD_SAFE" -- food perception in "safe" habitat;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 34
                              average(perc_food, MISSING, .FALSE.) )
      !>    - "PRC_FDIST_SAFE" -- food perception in "safe" habitat;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 35
                              average(perc_f_dist, MISSING, .FALSE.) )
      !>    - "PERC_CONS_SAFE" -- conspecific perception in "dangerous" habitat;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 36
                              average(perc_consp, MISSING, .FALSE.) )
      !>    - "PERC_PRED_SAFE" -- predator perception in "safe" habitat;
      !!    .
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 37
                              average(perc_pred, MISSING, .FALSE.) )
      !> - Calculate perception averages in the dangerous habitat:
      perc_food = MISSING; perc_consp = MISSING; perc_pred = MISSING
      !    - `IN_DANG` block calculates statistics arrays in the dangerous
      !       habitat.
      IN_DANG: where( proto_parents%individual%is_within( habitat_dangerous ) )
        perc_food = proto_parents%individual%memory_stack%get_food_mean_n()
        perc_f_dist = proto_parents%individual%memory_stack%                  &
                                  get_food_mean_dist(undef_ret_null=.FALSE.)
        perc_consp = proto_parents%individual%memory_stack%get_consp_mean_n()
        perc_pred = proto_parents%individual%memory_stack%get_pred_mean()
      end where IN_DANG
      !>    - "PERC_FOOD_DANG" -- food perception in "dangerous" habitat;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 38
                              average(perc_food, MISSING, .FALSE.) )
      !>    - "PRC_FDIST_DANG" -- food perception in "dangerous" habitat;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 39
                              average(perc_f_dist, MISSING, .FALSE.) )
      !>    - "PERC_CONS_DANG" -- conspecific perception in "dangerous" habitat;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 40
                              average(perc_consp, MISSING, .FALSE.) )
      !>    - "PERC_PRED_DANG" -- predator perception in "dangerous" habitat;
      !!    .
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 41
                              average(perc_pred, MISSING, .FALSE.) )
      !> - "FDIST_SAFE" -- average distance between food items in the
      !!   safe habitat;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 42
                              habitat_safe%food%distance_average(100) )
      !> - "FDIST_DANGER" - average distance between food items in the
      !!   dangerous habitat;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 43
                              habitat_dangerous%food%distance_average(100) )
      !> - "FITNESS_MIN" -- minimum fitness value.
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 44
                              minval(proto_parents%individual%fitness) )
      !> - "FITNESS_MEAN" -- average fitness.
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 45
                  average(proto_parents%individual%fitness, UNKNOWN, .FALSE.) )
      !> - "N_FOODS_SAFE" -- number of food items available in "safe" habitat;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 46
                count( habitat_safe%food%food%eaten .eqv. .FALSE. ) )
      !> - "N_FOODS_DANG" -- number of food items available in "dangerous"
      !!    habitat.
      !! .
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 47
                count( habitat_dangerous%food%food%eaten .eqv. .FALSE. ) )

      !> The following characteristics are calculated for **alive** agents.
      body_mass_l = MISSING
      body_leng_l = MISSING
      energy_l = MISSING
      stomach_l = MISSING
      smr_l = MISSING
      control_l = MISSING
      reprfac_l = MISSING
      p_repr_l = MISSING
      foods_try_l = UNKNOWN
      foods_eaten_l = UNKNOWN
      fmass_eaten_l = MISSING
      fitness_l = UNKNOWN

      !> Here the `ALIVE` block implements sorting out the individuals that are
      !! the_genome:individual_genome::is_alive().
      ALIVE: where( proto_parents%individual%is_alive() )
        body_mass_l = proto_parents%individual%body_mass
        body_leng_l =  proto_parents%individual%body_length
        energy_l =  proto_parents%individual%energy_current
        smr_l =  proto_parents%individual%smr
        control_l =  proto_parents%individual%control_unselected
        ! reprfac_l and p_repr_l calculated separately
        foods_try_l = proto_parents%individual%n_eats_all_indicator
        foods_eaten_l = proto_parents%individual%n_eaten_indicator
        fmass_eaten_l = proto_parents%individual%mass_eaten_indicator
        fitness_l = proto_parents%individual%fitness
      end where ALIVE

      ALIVE_MALES: where(   proto_parents%individual%is_alive() .and.         &
                            proto_parents%individual%is_male() )
        reprfac_l = proto_parents%individual%testosterone_level
      end where ALIVE_MALES

      ALIVE_FEMALES: where( proto_parents%individual%is_alive() .and.         &
                            proto_parents%individual%is_female() )
        reprfac_l = proto_parents%individual%estrogen_level
      end where ALIVE_FEMALES

      ! Probability of reproduction is computed in explicit loop because
      ! it is not elemental function.
      do i = 1, proto_parents%population_size
        if ( proto_parents%individual(i)%is_alive() ) then
          p_repr_l = proto_parents%individual(i)%probability_reproduction()
        else
          p_repr_l = MISSING
        end if
      end do

      !> - "BODY_MASS_L" -- average body mass of alive agents;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 48
                              average(body_mass_l, MISSING, .FALSE.) )
      !> - "BODY_LENGTH_L" -- average body length  of alive agents;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 49
                              average(body_leng_l, MISSING, .FALSE.) )
      !> - "ENERGY_L" -- average energy reserves of alive agents;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 50
                              average(energy_l, MISSING, .FALSE.) )
      !> - "SMR_L" -- average SMR of alive agents;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 51
                              average(smr_l, MISSING, .FALSE.) )
      !> - "CONTROL_L" -- control trait of alive agents;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 52
                              average(control_l, MISSING, .FALSE.) )
      !> - "REPRFACT_L" -- reproductive factor of alive agents;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 53
                              average(reprfac_l, MISSING, .FALSE.) )
      !> - "P_REPROD_L" -- probability of reproduction of alive agents;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 54
                              average(p_repr_l, MISSING, .FALSE.) )
      !> - "FOODS_TRY_L" -- average rate of attempts to catch food items by
      !!   alive agents;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 55
                              average( foods_try_l, UNKNOWN, .FALSE.) )
      !> - "FOODS_EATEN_L" -- average rate of successful food captures;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 56
                              average( foods_eaten_l, UNKNOWN, .FALSE.) )
      !> - "FMASS_EATEN_L" -- average rate of successful food captures;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 57
                              average( fmass_eaten_l, MISSING, .FALSE.) )
      !> - "N_SAFE_HAB_L" -- number of alive agents in "safe" habitats;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 58
                count( proto_parents%individual%is_alive() .and.        &
                       proto_parents%individual%is_within( habitat_safe ) ) )
      !> - "N_DANG_HAB_L" -- number of alive agents in "dangerous" habitat;
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 59
                count( proto_parents%individual%is_alive() .and.        &
                       proto_parents%individual%is_within( habitat_dangerous )))
      !> - "FITNESS_MEAN_L" -- mean fitness of alive agents.
      !! .
      call CSV_RECORD_APPEND( file_stats_gener_record,                  & ! 60
                              average(fitness_l, UNKNOWN, .FALSE.) )

      !> Once the record is fully built, it is written to the file using the
      !! file_io::record_write() method.
      call file_stats_gener%record_write( file_stats_gener_record )

    end subroutine generation_stats_record_write

  end subroutine generations_loop_ga

end module THE_EVOLUTION
