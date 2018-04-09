!> @file m_popul.f90
!! The Population objects for the AHA Model.
!! @author Sergey Budaev <sergey.budaev@uib.no>
!! @author Jarl Giske <jarl.giske@uib.no>
!! @date 2016-2017

!-------------------------------------------------------------------------------
! $Id$
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief   Define the population of agents object, its properties and
!!          functions.
!> @section the_population_module THE_POPULATION module
!! @details The population is in the simplest case an array of individual
!!          agent objects. Individual properties of the population members can
!!          be referred as, e.g. `proto_parents%individual(i)%fitness` .
module THE_POPULATION

  use COMMONDATA      ! Global definitions of the model objects
  use THE_INDIVIDUAL  ! The individual - properties of the individuals

  implicit none

  character (len=*), parameter, private :: MODNAME = "(THE_POPULATION)"

  !> @brief   Definition of individual member of a population.
  !! @details Add an additional object, a member of the population.
  !!          It is the`INDIVIDUAL_AGENT` adding an (unique) integer
  !!          ID: `person_number`. Here we also have two functions for
  !!          setting and retrieving the IDs. We do not init members
  !!          of the population using a single init function, Instead,
  !!          init normally the INDIVIDUAL_AGENT object and for the
  !!          MEMBER_POPULATION just set_id. This is because we cannot
  !!          set id for isolated subject, only as a member of population.
  type, public, extends(INDIVIDUAL_AGENT) :: MEMBER_POPULATION
    integer :: person_number
    contains
      private
      !> Set integer ID number to individual member of the population object.
      !! See `the_population::set_individual_id()`.
      procedure, private :: set_id => set_individual_id
      !> Get integer ID number to individual member of the population object.
      !! See `the_population::get_individual_id()`.
      procedure, public  :: get_id => get_individual_id
      !> Places the individual agent, a member of the population, within a specific
      !! environment at random with a **uniform** distribution.
      !! See `the_population::individ_posit_in_environ_uniform()`.
      procedure, public  :: place_uniform => individ_posit_in_environ_uniform
      !> Set the individual to be **dead**.
      !! See `the_population::genome_individual_set_dead_non_pure()`.
      procedure, public  :: dies_debug => genome_individual_set_dead_non_pure
  end type MEMBER_POPULATION

  !> @brief   Definition of the population object.
  !! @details This is basically an array of individuals (of the type
  !!          `MEMBER_POPULATION`) plus population or generation descriptors.
  type, public :: POPULATION
    !> The size of the population.
    integer :: population_size
    !> @brief   `POPULATION` is represented by an array of objects
    !!          of the type `MEMBER_POPULATION`
    !! @details `POPULATION` is an array of objects of the type
    !!          `MEMBER_POPULATION`. It is represented as the \%individual
    !!          component of the type. The `i`-th individual of the population
    !!          `this` is accessed as `this%individual(i)`. the population also
    !!          has two descriptors: integer `pop_number` and string `pop_name`.
    type(MEMBER_POPULATION), allocatable, dimension(:) :: individual
    !> The numeric ID of the population (if we have several populations))
    integer :: pop_number
    !> The descriptive name of the population
    character (len=LABEL_LENGTH) :: pop_name
    contains
      private
      !> Initialise the population object.
      !! See `the_population::init_population_random()`.
      procedure, public :: init => init_population_random
      !> Impose selective mortality at birth of the agents.
      !! See `the_population::population_birth_mortality_init()`.
      procedure, public :: mortality_birth => population_birth_mortality_init
      !> Destroys this population and deallocates the array of individual
      !! member  objects.
      !! See `the_population::population_destroy_deallocate_objects()`.
      procedure, public :: destroy => population_destroy_deallocate_objects
      !> Get the size of this population.
      !! See `the_population::population_get_popsize()`.
      procedure, public :: get_size => population_get_popsize
      !> Get the population number ID.
      !! See `the_population::population_get_pop_number()`.
      procedure, public :: get_num_id => population_get_pop_number
      !> Get the population character label ID.
      !! See `the_population::population_get_pop_name()`.
      procedure, public :: get_name => population_get_pop_name
      !> Reset individual IDs of the population members.
      !! See `the_population::reset_population_id_random()`.
      procedure, public :: reset_id => reset_population_id_random
      !> Determine the sex for each member of the population.
      !! See `the_population::sex_initialise_from_genome()`.
      procedure, public :: sex => sex_initialise_from_genome
      !> Position each member of the population randomly within a
      !! bounding environment with the **uniform** distribution.
      !! See `the_population::position_individuals_uniform()`.
      procedure, public :: scatter_uniform => position_individuals_uniform
      !> Perform a single step of random walk by all agents, in 3D.
      !! See `the_population::population_rwalk3d_all_agents_step()`.
      procedure, public :: rwalk3d => population_rwalk3d_all_agents_step
      !> Perform a single step of random walk by all agents, in 2.5D.
      !! See `the_population::population_rwalk25d_all_agents_step()`.
      procedure, public :: rwalk25d => population_rwalk25d_all_agents_step
      !> This subroutine sorts the population `individual` object by
      !! their \%fitness components.
      !! See `the_population::sort_population_by_fitness()`.
      procedure, public :: sort_by_fitness => sort_population_by_fitness
      !> Save data for all agents within the population into a csv file.
      !! See `the_population::population_save_data_all_agents_csv()`.
      procedure, public :: save_csv => population_save_data_all_agents_csv
      !> Save the genome data of all agents in this population to a CSV file.
      !! See `the_population::population_save_data_all_genomes()`.
      procedure, public :: save_genomes_csv => population_save_data_all_genomes
      !> Save the perceptual and emotional memory stack data of all agents
      !! in this population to a CSV file.
      !! See `the_population::population_save_data_memory()`.
      procedure, public :: save_memory_csv => population_save_data_memory
      !> Save the latest movement history of all agents.
      !! See `the_population::population_save_data_movements()`.
      procedure, public :: save_movements_csv => population_save_data_movements
      !> Save the behaviours history the_neurobio::behaviour::history_behave
      !! for all agents.
      !! See `the_population::population_save_data_behaviours()`.
      procedure, public :: save_behaviour_csv => population_save_data_behaviours
      !> Subject the population to an attack by a specific predator.
      !! See `the_population::population_subject_predator_attack()`.
      procedure, public :: attacked => population_subject_predator_attack
      !> Subject the population to mortality caused by habitat-specific
      !! mortality risk. Each agent is affected by the risk associated with
      !! the habitat it is currently in.
      !! See `the_population::population_subject_other_risks()`.
      procedure, public :: mortality_habitat => population_subject_other_risks
      !> Subject all members of this population to their individual mortality
      !! risks.
      !!  See `the_population::population_subject_individual_risk_mortality()`.
      procedure, public :: mortality_individ =>                               &
                                  population_subject_individual_risk_mortality
      !> Calculate fitness for the pre-evolution phase of the genetic algorithm.
      !! **Pre-evolution** is based on selection for a simple criterion without
      !! explicit reproduction etc. The criterion for selection at this phase
      !! is set by the integer the_individual::individual_agent::fitness
      !! component. This procedure provides a whole-population wrapper for the
      !! the_individual::fitness_calc() function.
      !! See `the_population::population_preevol_fitness_calc()`.
      procedure, public :: fitness_calc => population_preevol_fitness_calc
      !> Determine the number of parents that have fitness higher than the
      !! minimum acceptable value.
      !! See `the_population::population_ga_reproduce_max()`.
      procedure, public :: ga_reproduce_max => population_ga_reproduce_max
      !> This function implements adaptive mutation rate that increases
      !! as the population size reduces.
      !! See `the_population::population_ga_mutation_rate_adaptive()`.
      procedure, public :: ga_mutat_adaptive =>                               &
                                          population_ga_mutation_rate_adaptive
      !> Perform a single step of the life cycle of the population.
      !! See `the_population::population_lifecycle_step_preevol()`.
      procedure, public :: lifecycle_step => population_lifecycle_step_preevol
      !> Perform a single step of the life cycle of the population. This
      !! version includes only optimal food selection and eating without
      !! the full fledged behaviour selection cascade of procedures
      !! ::do_behave().
      !! See `the_population::population_lifecycle_step_eatonly_preevol()`.
      procedure, public :: lifecycle_eatonly =>                               &
                                      population_lifecycle_step_eatonly_preevol

  end type POPULATION

  !> Global indicator variable that keeps the number of agents that have died
  !! as a consequence of predatory attacks. All other dies are therefore caused
  !! by starvation.
  !! @note Note that this variable is initialised to zero at the start of each
  !!       generation in `GENERATIONS_PREEVOL` named loop in
  !!       the_evolution::generations_loop_ga().
  integer, public :: Global_Ind_N_Eaten_by_Predators

contains ! ........ implementation of procedures for this level ................

  !-----------------------------------------------------------------------------
  !> Set integer ID number to individual member of the population object.
  !! @note Note that this subroutine is private as setting individual IDs
  !!       makes sense only during the initialisation phase of the population.
  subroutine set_individual_id (this, idnumber)
    !> @param class, member of population.
    class(MEMBER_POPULATION), intent(inout) :: this
    !> @param id, integer id number assigned.
    integer, intent(in) :: idnumber

    this%person_number = idnumber    !> `person_number` is assigned the idnumber

  end subroutine set_individual_id

  !-----------------------------------------------------------------------------
  !> Get integer ID number to individual member of the population object.
  !! @note Note that this is public, so we can retrieve individual IDs but not
  !!       set them (id's are always set at initialisation)
  function get_individual_id (this) result (idnumber)
    !> @param class, member of population.
    class(MEMBER_POPULATION), intent(in) :: this
    !> @param id, integer id number retreived.
    integer :: idnumber

    idnumber = this%person_number    !> `person_number` is assigned the idnumber

  end function get_individual_id

  !-----------------------------------------------------------------------------
  !> Places the individual agent, a member of the population, within a specific
  !! environment at random with a **uniform** distribution.
  !! The agents can be positioned with respect to their initial depth
  !! - fixed depth, see commondata::init_agents_depth_is_fixed
  !! - Gaussian depth, see commondata::init_agents_depth_is_gauss
  !! - fully uniform distribution.
  !! .
  subroutine individ_posit_in_environ_uniform(this, environ)
    class(MEMBER_POPULATION), intent(inout) :: this
    !> @param environ sets the environment object where the agent is
    !!        placed randomly
    class(ENVIRONMENT), intent(in) :: environ

    if (INIT_AGENTS_DEPTH_IS_FIXED) then
      call this%position( environ%uniform( within( INIT_AGENTS_DEPTH,         &
                                                   environ%depth_min(),       &
                                                   environ%depth_max()  ) ) )
    else if (INIT_AGENTS_DEPTH_IS_GAUSS) then
      call this%position(                                                     &
          environ%uniform( within( RNORM( INIT_AGENTS_DEPTH,                  &
                                         cv2variance(INIT_AGENTS_DEPTH_CV,    &
                                             INIT_AGENTS_DEPTH)),             &
                                   environ%depth_min(),                       &
                                   environ%depth_max()                  ) ) )
    else
      call this%position( environ%uniform() )
    end if

  end subroutine individ_posit_in_environ_uniform

  !-----------------------------------------------------------------------------
  !> Set the individual to be **dead**. Note that this function does not
  !! deallocate the individual agent object, this may be a separate destructor
  !! function.
  !! @note    This is a non-pure function logging some extended diagnostics
  !!          about the dying agent. See `dies` procedure from @ref the_genome
  !!          and all its overrides:
  !!            - the_genome::individual_genome::dies();
  !!            - the_neurobio::appraisal::dies();
  !!            - the_neurobio::gos_global::dies();
  !!            - the_individual::individual_agent::dies().
  !!            .
  !! @warning This function should be normally used only while **debugging**.
  !!          In normal runs use the pure subroutine `dies` from
  !!          `ÌNDIVIDUAL_GENOME`.
  subroutine genome_individual_set_dead_non_pure(this, non_debug_log)
    class(MEMBER_POPULATION), intent(inout) :: this
    logical, optional, intent(in) :: non_debug_log

    !> Local flag to do show log.
    logical :: do_show_log

    !> Local parameter showing how many latest memory values to log out.
    integer, parameter :: HIST_N=10

    do_show_log = .FALSE. !> Don't show log by default.

    call this%dies() !> This is a pure function from `THE_GENOME` level.

    !> Turn on output logging only if in the DEBUG mode or if `non_debug_log`
    !! is explicitly set to TRUE.
    if (IS_DEBUG) then
      do_show_log = .TRUE.
    else
      if (present(non_debug_log)) then
        if (non_debug_log) do_show_log = .TRUE.
      end if
    end if

    if (.not. do_show_log) return   !> Just exit back if no logging.

    call LOG_MSG( "Agent # " //  TOSTR(this%get_id()) // " with name " //     &
                  trim(this%individ_label()) // " dies." )
    call LOG_MSG( "Agent properties: " )
    call LOG_MSG( "  body mass: " // TOSTR(this%get_mass()) //                &
                  ", body mass at birth: " // TOSTR(this%body_mass_birth) //  &
                  ", max body mass: " // TOSTR(this%body_mass_maximum) //     &
                  ", body length: " // TOSTR(this%get_length()) //            &
                  ", energy reserves: " // TOSTR(this%get_energy()) //        &
                  ", SMR: " // TOSTR(this%get_smr()) //                       &
                  ", stomach content: " // TOSTR(this%get_stom_content()) )
    call LOG_MSG( "  Latest body mass (" // TOSTR(HIST_N) // ") history: " // &
                  TOSTR(this%body_mass_history(                               &
                    HISTORY_SIZE_AGENT_PROP-HIST_N+1:HISTORY_SIZE_AGENT_PROP)) )
    call LOG_MSG( "  Latest body length (" // TOSTR(HIST_N) // ") history: "  &
                  // TOSTR(this%body_length_history(                          &
                    HISTORY_SIZE_AGENT_PROP-HIST_N+1:HISTORY_SIZE_AGENT_PROP)) )
    call LOG_MSG( "Agent's GOS is " // this%gos_label() //                    &
                  ", arousal: " // TOSTR(this%arousal()) // "." )
    !> @note Note that `TOSTR` accepts arrays including (concatenated)
    !!       character arrays.
    call LOG_MSG( "  Latest GOS states: " //                                  &
                  TOSTR(this%memory_motivations%gos_main(                     &
                    HISTORY_SIZE_MOTIVATION-HIST_N+1:HISTORY_SIZE_MOTIVATION)) )

  end subroutine genome_individual_set_dead_non_pure

  !-----------------------------------------------------------------------------
  !> @brief   Initialise the population object.
  !! @details Initialise the population object, init it with random individuals
  !!          (function init on individuals), and assign sequential
  !!          person_number`s.
  subroutine init_population_random(this, pop_size, pop_number_here,          &
                                                                  pop_name_here)
    ! Parameters for this subroutine:
    !> @param class, This -- member of population class (this)).
    class(POPULATION), intent(inout) :: this
    !> @param pop_size, The size of the population.
    integer, intent(in) :: pop_size
    !> @param id, Optional numeric population ID.
    integer, optional, intent(in) :: pop_number_here
    !> @param descriptor, Optional population string descriptor.
    character (len=*), optional, intent(in) :: pop_name_here

    ! Local variables:
    integer :: i                !> local variable `i` is counter

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME = "(init_population_random)"

    !> Set population size from input parameter.
    this%population_size = pop_size

    !>  Allocate the population
    if (.not. allocated(this%individual))                                     &
                                allocate(this%individual(this%population_size))

    !> Initialise all individuals of the population
    do i=1, this%population_size
      !> first, call `init to object individual(i)
      call this%individual(i)%init()
      !! second, ! call `set_id(i)` to identify agents within the population.
      call this%individual(i)%set_id(i)
    end do

    !> Set optional descriptors for the whole population. Numeric ID and
    !! a short text description.
    if (present(pop_number_here)) then
      this%pop_number = pop_number_here
    else
      !> If optional ID is absent, ID is set to a random integer value from 1
      !! to the maximum integer allowed for the pop_number type (minus 1).
      this%pop_number = RAND_I(1,huge(this%pop_number-1))
    end if
    if (present(pop_name_here)) then
      this%pop_name = pop_name_here
    else
      !> If optional text description string of the population is absent,
      !! it is set to the string representation of its numeric ID.
      this%pop_name = TOSTR(this%pop_number)
    end if

    call LOG_MSG( LTAG_INFO // "Initialised population " // this%pop_name //  &
                  " # " //  TOSTR(this%pop_number) // " with " //             &
                  TOSTR(this%population_size) // " agents." )

    !> Log the initial location of the agents.
    !! @warning The logic of the logger constructs here must coincide with
    !!          that in the population::individ_posit_in_environ_uniform().
    if (INIT_AGENTS_DEPTH_IS_FIXED) then
      call LOG_MSG( LTAG_INFO // "Initial location is FIXED at " //           &
                    TOSTR(INIT_AGENTS_DEPTH) )
    else if (INIT_AGENTS_DEPTH_IS_GAUSS) then
      call LOG_MSG( LTAG_INFO // "Initial location is GAUSSIAN at " //        &
                    TOSTR(INIT_AGENTS_DEPTH) //  ", with CV " //              &
                    TOSTR(INIT_AGENTS_DEPTH_CV) )
    else
      call LOG_MSG( LTAG_INFO // "Initial location is fully uniform." )
    end if

  end subroutine init_population_random

  !-----------------------------------------------------------------------------
  !> Destroys this population and deallocates the array of individual member
  !!  objects.
  subroutine population_destroy_deallocate_objects(this)
    class(POPULATION), intent(inout) :: this

    this%population_size = 0
    this%pop_number = UNKNOWN
    this%pop_name = ""
    if (allocated(this%individual)) deallocate(this%individual)

  end subroutine population_destroy_deallocate_objects

  !-----------------------------------------------------------------------------
  !> Impose selective mortality at birth on the agents. Selective mortality
  !! sets a fixed limit on uncontrolled evolution of the *energy reserves*
  !! in newborn agents. If some newborn has too high energy at birth
  !! (genetically fixed), such a deviating agent is killed at once.
  !!
  !! The values of the risk are chosen such that it is zero at the
  !! fixed population mean (agent does not deviate) and reaches 1.0 if
  !! the agent's energy reserves are 3.0 standard deviations higher than
  !! the fixed mean value (agent strongly deviates).
  subroutine population_birth_mortality_init(this, energy_mean, energy_sd)
    class(POPULATION), intent(inout) :: this
    !> @param[in] energy_mean optional mean energy at birth, if absent, is
    !!            calculated from the population data.
    real(SRP), optional, intent(in) :: energy_mean
    !> @param[in] energy_sd optional mean energy at birth, if absent, is
    !!            calculated from the population data.
    real(SRP), optional, intent(in) :: energy_sd

    ! Local copies of optionals
    real(SRP) :: energy_mean_loc, energy_sd_loc

    real(SRP) :: mortality
    integer :: ind

    !> - `MORTALITY_BIRTH_INIT_ENERG_ABSCISSA` is the baseline abscissa for the
    !!    nonparametric interpolation function grid, in units of the standard
    !!    deviation (sigma) of the energy reserves in the newborn population,
    !!    i.e. the_body::condition::energy_birth.
    ! htintrpl.exe [1.4 4 5] [0 0.5 1]
    ! htintrpl.exe [0.7 1 2 3] [0 0.005 0.1 1]
    real(SRP), parameter, dimension(*) :: MORTALITY_BIRTH_INIT_ENERG_ABSCISSA &
              = [ 0.7_SRP,   1.0_SRP,  1.5_SRP, 2.0_SRP, 3.0_SRP ]

    !> - `MORTALITY_BIRTH_INIT_ENERG_ORDINATE` is the ordinate for the
    !!    nonparametric interpolation function grid: sets the probability of
    !!    the agent's death given its energy reserves deviate from the fixed
    !!    mean by the number of standard deviations set by
    !!    `MORTALITY_BIRTH_INIT_ENERG_ABSCISSA`.
    !! .
    real(SRP), parameter, dimension(*) :: MORTALITY_BIRTH_INIT_ENERG_ORDINATE &
              = [ 0.0_SRP, 0.002_SRP, 0.01_SRP, 0.1_SRP, 1.0_SRP ]

    if (present(energy_mean)) then
      energy_mean_loc = energy_mean
    else
      energy_mean_loc = average( this%individual%get_energ_birth() )
    end if

    if (present(energy_sd)) then
      energy_sd_loc = energy_sd
    else
      energy_sd_loc = std_dev( this%individual%get_energ_birth() )
    end if

    INDS: do ind=1, this%population_size

      !> Selective mortality sets a fixed limit on uncontrolled evolution of
      !! the energy reserves in newborn agents. All newborn agents that have
      !! the energy reserves exceeding some point may die with the probability
      !! determined by the grid arrays:
      !! - `MORTALITY_BIRTH_INIT_ENERG_ABSCISSA`;
      !! - `MORTALITY_BIRTH_INIT_ENERG_ORDINATE`.
      !! .
      !!
      !! The probability of death (mortality risk) is determined by the
      !! nonparametric nonlinear function defined by `DDPINTERPOL`, where
      !! the actual grid abscissa is the energy reserve of the agent in
      !! and grid ordinate is the mortality risk.
      !!
      !! The values of the risk are chosen such that it is zero at the
      !! fixed population mean (agent does not deviate) and reaches 1.0 if
      !! the agent's energy reserves are 3.0 standard deviations higher than
      !! the mean value.
      mortality = within(DDPINTERPOL(energy_mean_loc +                        &
                                       MORTALITY_BIRTH_INIT_ENERG_ABSCISSA *  &
                                       energy_sd_loc,                         &
                                     MORTALITY_BIRTH_INIT_ENERG_ORDINATE,     &
                                     this%individual(ind)%get_energ_birth()), &
                         0.0_SRP, 1.0_SRP )

      !> Interpolation plots can be saved in the @ref intro_debug_mode
      !! "debug mode" using this plotting command:
      !! `commondata::debug_interpolate_plot_save()`.
      !! @warning Involves **huge** number of plots, should normally be
      !!          disabled.
      call debug_interpolate_plot_save(                                       &
            grid_xx=energy_mean_loc + MORTALITY_BIRTH_INIT_ENERG_ABSCISSA *   &
                          energy_sd_loc,                                      &
            grid_yy=MORTALITY_BIRTH_INIT_ENERG_ORDINATE,                      &
            ipol_value=this%individual(ind)%get_energ_birth(),                &
            algstr="DDPINTERPOL",                                             &
            output_file="plot_debug_mortality_birth_" //                      &
                        "gen_" // TOSTR(Global_Generation_Number_Current) //  &
                        "_step_"// TOSTR(Global_Time_Step_Model_Current) //   &
                        MMDD // "_a_"//                                       &
                        trim(this%individual(ind)%individ_label()) //         &
                        "_" // RAND_STRING(LABEL_LENGTH, LABEL_CST,LABEL_CEN) &
                        // PS )

      if ( RAND_R4() < mortality ) then
        call this%individual(ind)%dies()
      end if
    end do INDS

  end subroutine population_birth_mortality_init

  !-----------------------------------------------------------------------------
  !> Accessor get-function for the size of this population.
  function population_get_popsize(this) result (pop_size_output)
    class(POPULATION), intent(in) :: this
    integer :: pop_size_output

    pop_size_output = this%population_size

  end function population_get_popsize

  !-----------------------------------------------------------------------------
  !> Accessor get-function for the population number ID.
  function population_get_pop_number(this) result(pop_number_output)
    class(POPULATION), intent(in) :: this
    integer :: pop_number_output

    pop_number_output = this%pop_number

  end function population_get_pop_number

  !-----------------------------------------------------------------------------
  !> Accessor get-function for the population character label ID.
  function population_get_pop_name(this) result (pop_name_string_out)
    class(POPULATION), intent(in) :: this
    character(len=LABEL_LENGTH) :: pop_name_string_out

    pop_name_string_out = this%pop_name

  end function population_get_pop_name

  !-----------------------------------------------------------------------------
  !> @brief   Reset individual IDs of the population members.
  !! @details Makes new random individual IDs for the population members.
  subroutine reset_population_id_random(this, pop_number_here, pop_name_here)
  ! Parameters for this subroutine:
    !> @param class, This -- member of population class (this)).
    class(POPULATION), intent(inout) :: this
    !> @param id, Optional numeric population ID.
    integer, optional, intent(in) :: pop_number_here
    !> @param descriptor, Optional population string descriptor.
    character (len=*), optional, intent(in) :: pop_name_here

    ! Local variables:
    integer :: i                !> local variable `i` is counter

    !> Reset all individual IDs of the population members
    do i=1, this%population_size
      call this%individual(i)%set_id(i) ! call set_id(i) to the same object
    end do

    !> Set optional descriptors for the whole population. Numeric ID and
    !! a short text description.
    if (present(pop_number_here)) then
      this%pop_number = pop_number_here
    else
      !> If optional ID is absent, ID is set to a random integer value from 1
      !! to the maximum integer allowed for the pop_number type (minus 1).
      this%pop_number = RAND_I(1,huge(this%pop_number-1))
    end if
    if (present(pop_name_here)) then
      this%pop_name = pop_name_here
    else
      !> If optional text description string of the population is absent,
      !! it is set to the string representation of its numeric ID.
      this%pop_name = TOSTR(this%pop_number)
    end if

  end subroutine reset_population_id_random

  !-----------------------------------------------------------------------------
  !> @brief Determine the sex for each member of the population.
  subroutine sex_initialise_from_genome(this)
    class(POPULATION), intent(inout) :: this

    integer :: i

    do i=1, this%population_size
      call this%individual(i)%sex_init()
    end do

  end subroutine sex_initialise_from_genome

  !-----------------------------------------------------------------------------
  !> @brief Position each member of the population randomly within a
  !!        bounding environment.
  !! @note  Moved the positioning procedure into a separate procedure as
  !!        initialising population may involve different spatial positioning,
  !!        e.g. uniform within the bounding environment or gaussian localised.
  subroutine position_individuals_uniform(this, environ)
    class(POPULATION), intent(inout) :: this

    !> @param   environ the environment where we place the population
    !! @warning Even though this parameter is optional, the bounding
    !!          environment should in most cases (almost?) be fixed, and
    !!          provided as a parameter. In most cases, unlimited environment
    !!          is useful for debugging only.
    !! TODO: convert to class
    class(ENVIRONMENT), optional, intent(in) :: environ

    !> Local object representing the bounding environment for this population
    type(ENVIRONMENT) :: environ_here

    !> Local counter
    integer :: i

    !> Check if the bounding environment is provided, if not, place agents
    !! without limits
    !! @warning The bounding environment should in most cases be fixed, and
    !!          provided as a parameter.
    if (present(environ)) then
      call environ_here%build( environ%lim_min(), environ%lim_max() )
    else
      call environ_here%build_unlimited()
    end if

    !> Position agents randomly (uniform distribution) within the
    !! bounding environment.
    do i=1, this%population_size
      call this%individual(i)%place_uniform(environ_here)
    end do

  end subroutine position_individuals_uniform

  !-----------------------------------------------------------------------------
  !> @brief   This subroutine sorts the population `individual` object by
  !!          their \%fitness components.
  !! @details The two subroutines `qsort` and `qs_partition_fitness` are a
  !! variant of the recursive quick sort algorithm adapted for
  !! `MEMBER_POPULATION` integer fitness component
  elemental subroutine sort_population_by_fitness(this)
    !> @param class, This -- member of population class (this)).
    class(POPULATION), intent(inout) :: this

    call qsort(this%individual) !> This is the array component we sort.

    contains

    !...........................................................................
    !> `qsort` is a recursive frontend for `MEMBER_POPULATION` objects
    recursive pure subroutine qsort(A)

      !> @param `A` has the same type as the individual component objects
      !!         of the array-object that we are going to sort.
      type(MEMBER_POPULATION), intent(in out), dimension(:) :: A
      integer :: iq

      if(size(A) > 1) then
        call qs_partition_fitness(A, iq) ! partition
        call qsort(A(:iq-1))
        call qsort(A(iq:))
      endif

    end subroutine qsort

    !...........................................................................
    !> partition is a pivot backend for `fitness`
    pure subroutine qs_partition_fitness(A, marker)

      type(MEMBER_POPULATION), intent(in out), dimension(:) :: A
      integer, intent(out) :: marker
      integer :: i, j
      type(MEMBER_POPULATION) :: temp
      !> @note Pivot point `x`, has the same type **as
      !!       the sorted object component**.
      integer :: x

      !> Fitness is hardwired in this partition subroutine, but it can be used
      !! as a model for similar othert sorting functions. Note that here integer
      !! array is sorted.
      x = A(1)%fitness
      i= 0
      j= size(A) + 1

      do
        j = j-1
        do
            if (A(j)%fitness <= x) exit
            j = j-1
        end do
        i = i+1
        do
            if (A(i)%fitness >= x) exit
            i = i+1
        end do
        if (i < j) then
            ! exchange A(i) and A(j)
            temp = A(i)
            A(i) = A(j)
            A(j) = temp
        elseif (i == j) then
            marker = i+1
            return
        else
            marker = i
            return
        endif
      end do

    end subroutine qs_partition_fitness

  end subroutine sort_population_by_fitness

  !-----------------------------------------------------------------------------
  !> Perform one or several steps of random walk by all agents.
  !! @note This procedure was used for debugging.
  subroutine population_rwalk3d_all_agents_step( this, dist_array, cv_array,  &
                                                 dist_all, cv_all,            &
                                                 environment_limits, n_walks )
    class(POPULATION), intent(inout) :: this

    !> @param[in] step_size_array an array of step sizes for each individual.
    real(SRP), optional, dimension(:), intent(in) :: dist_array
    !> @param[in]cv_array Coefficients of variation for the walk.
    real(SRP), optional, dimension(:), intent(in) :: cv_array
    !> @param[in] dist_all the value of the walk step size that is identical in
    !!            all agents within the population.
    real(SRP), optional, intent(in) :: dist_all
    !> @param[in] cv_all the value of the walk coefficient of variation that is
    !!            identical in all agents within the population.
    real(SRP), optional, intent(in) :: cv_all
    !> @param environment_limits Limits of the environment area available for
    !!        the random walk. The moving object cannot get beyond this limit.
    !!        If this parameter is not provided, the environmental limits are
    !!        obtained automatically from the global array
    !!        the_environment::global_habitats_available.
    class(ENVIRONMENT), intent(in), optional :: environment_limits

    !> @param[in] n_walk optional number of walk steps that should be
    !!            performed, default just one.
    integer, optional, intent(in) :: n_walks

    ! Local variables, copies of optionals.
    real(SRP), dimension(this%population_size) :: dist_array_here, cv_array_here
    integer :: n_walks_here

    ! Local params.
    real(SRP), dimension(this%population_size) :: step_size_walk
    integer :: j, i, ind, pop_n
    integer, dimension(this%population_size) :: pop_permutation

    ! Default walk step CV.
    real(SRP), parameter ::  CV_DEFAULT = 0.5_SRP

    !> ### Implementation details ###
    !> - Calculate the distance array size.
    pop_n = this%population_size

    if (present(dist_array)) then
      dist_array_here = dist_array
    else
      dist_array_here = this%individual%get_length()
    end if

    if (present(cv_array)) then
      cv_array_here = cv_array
    else
      cv_array_here = CV_DEFAULT
    end if

    if (present(dist_all)) then
      dist_array_here = dist_all
    else
      dist_array_here = this%individual%get_length()
    end if

    if (present(cv_all)) then
      cv_array_here = cv_all
    else
      cv_array_here = CV_DEFAULT
    end if

    if (present(n_walks)) then
      n_walks_here = n_walks
    else
      n_walks_here = 1
    end if

    !> - calculate the step size along the axes from the distance array.
    step_size_walk = dist2step(dist_array_here)

    !> - Calculate the random permutation of individual indices.
    !!   @warning Random order here is a prototype for testing for use in
    !!          behaviour selection by population members.
    pop_permutation = PERMUTE_RANDOM(pop_n)

    !> - Perform Gaussian random walks for each of the individuals in a random
    !!   order that is set by the `pop_permutation` array.
    !! .
    ENVIRON_RESTRICT: if (present(environment_limits)) then
      do j=1, n_walks_here
        do i=1, pop_n
          ind = pop_permutation(i)
          if (this%individual(ind)%is_alive())                                &
                    call this%individual(ind)%rwalk( step_size_walk(ind),     &
                                                     cv_array_here(ind),      &
                                                     environment_limits  )
        end do
      end do
    else ENVIRON_RESTRICT
      do j=1, n_walks_here
        do i=1, pop_n
          ind = pop_permutation(i)
          if (this%individual(ind)%is_alive())                                &
                    call this%individual(ind)%rwalk(                          &
                                step_size_walk(ind),                          &
                                cv_array_here(ind),                           &
                                Global_Habitats_Available(                    &
                                    this%individual(ind)%find_environment(    &
                                              Global_Habitats_Available) ) )
        end do
      end do
    end if ENVIRON_RESTRICT

  end subroutine population_rwalk3d_all_agents_step

  !-----------------------------------------------------------------------------
  !> Perform one or several steps of random walk by all agents.
  !! @note This procedure was used for debugging.
  subroutine population_rwalk25d_all_agents_step ( this,                      &
                                            dist_array_xy, cv_array_xy,       &
                                            dist_array_depth, cv_array_depth, &
                                            dist_all_xy, cv_all_xy,           &
                                            dist_all_depth, cv_all_depth,     &
                                            environment_limits, n_walks )
    class(POPULATION), intent(inout) :: this

    !> @param[in] dist_array_xy an array of step sizes for each individual.
    real(SRP), optional, dimension(:), intent(in) :: dist_array_xy
    !> @param[in]cv_array_xy Coefficients of variation for the walk.
    real(SRP), optional, dimension(:), intent(in) :: cv_array_xy

    !> @param[in] dist_array_depth an array of step sizes for each individual.
    real(SRP), optional, dimension(:), intent(in) :: dist_array_depth
    !> @param[in]cv_array_depth Coefficients of variation for the walk.
    real(SRP), optional, dimension(:), intent(in) :: cv_array_depth

    !> @param[in] dist_all_xy the value of the walk step size for horizontal
    !!            plane that is identical in all agents within the population.
    real(SRP), optional, intent(in) :: dist_all_xy
    !> @param[in] cv_all_xy the value of the walk coefficient of variation in
    !!            the horizontal plane that is identical in all agents within
    !!            the population.
    real(SRP), optional, intent(in) :: cv_all_xy

    !> @param[in] dist_all_depth the value of the walk step size for the depth
    !!            plane that is identical in all agents within the population.
    real(SRP), optional, intent(in) :: dist_all_depth
    !> @param[in] cv_all_depth the value of the walk coefficient of variation
    !!            in the depth plane that is identical in all agents within
    !!            the population.
    real(SRP), optional, intent(in) :: cv_all_depth
    !> @param environment_limits Limits of the environment area available for
    !!        the random walk. The moving object cannot get beyond this limit.
    !!        If this parameter is not provided, the environmental limits are
    !!        obtained automatically from the global array
    !!        the_environment::global_habitats_available.
    class(ENVIRONMENT), intent(in), optional :: environment_limits
    !> @param[in] n_walk optional number of walk steps that should be
    !!            performed, default just one.
    integer, optional, intent(in) :: n_walks

    ! Local variables, copies of optionals.
    real(SRP), dimension(this%population_size) ::                             &
                                      dist_array_xy_here, cv_array_xy_here
    real(SRP), dimension(this%population_size) ::                             &
                                      dist_array_depth_here, cv_array_depth_here
    integer :: n_walks_here

    ! Local params.
    real(SRP), dimension(this%population_size) :: step_size_walk_xy,          &
                                                  step_size_walk_depth
    integer :: j, i, ind, pop_n
    integer, dimension(this%population_size) :: pop_permutation

    ! Default walk step CV.
    real(SRP), parameter ::  CV_DEFAULT = 0.5_SRP

    !> ### Implementation details ###
    !> - Calculate the distance array size.
    pop_n = this%population_size

    if (present(dist_array_xy)) then
      dist_array_xy_here = dist_array_xy
    else
      dist_array_xy_here = this%individual%get_length()
    end if

    if (present(cv_array_xy)) then
      cv_array_xy_here = cv_array_xy
    else
      cv_array_xy_here = CV_DEFAULT
    end if

    !> - If the depth walk step distance is not provided as a parameter,
    !!   1/2 of the agent body size is used as the default value. Thus,
    !!   it is assumed that the extent of random movements of the agents
    !!   in the horizontal plane is greater than vertical movements.
    if (present(dist_array_depth)) then
      dist_array_depth_here = dist_array_depth
    else
      dist_array_depth_here = this%individual%get_length() / 2.0_SRP
    end if

    if (present(cv_array_depth)) then
      cv_array_depth_here = cv_array_depth
    else
      cv_array_depth_here = CV_DEFAULT
    end if

    if (present(dist_all_xy)) then
      dist_array_xy_here = dist_all_xy
    else
      dist_array_xy_here = this%individual%get_length()
    end if

    if (present(cv_all_xy)) then
      cv_array_xy_here = cv_all_xy
    else
      cv_array_xy_here = CV_DEFAULT
    end if

    if (present(dist_all_depth)) then
      dist_array_depth_here = dist_all_depth
    else
      dist_array_depth_here = this%individual%get_length() / 2.0_SRP
    end if

    if (present(cv_all_depth)) then
      cv_array_depth_here = cv_all_depth
    else
      cv_array_depth_here = CV_DEFAULT
    end if

    if (present(n_walks)) then
      n_walks_here = n_walks
    else
      n_walks_here = 1
    end if

    !> - Calculate the step size along the axes from the distance array.
    step_size_walk_xy = dist2step(dist_array_xy_here)
    step_size_walk_depth = dist2step(dist_array_depth_here)

    !> - Calculate the random permutation of individual indices.
    !!   @warning Random order here is a prototype for testing for use in
    !!          behaviour selection by population members.
    pop_permutation = PERMUTE_RANDOM(pop_n)

    !> - Perform Gaussian random walks for each of the individuals in a random
    !!   order that is set by the `pop_permutation` array.
    !! .
    ENVIRON_RESTRICT: if (present(environment_limits)) then
      do j=1, n_walks_here
        do i=1, pop_n
          ind = pop_permutation(i)
          if (this%individual(ind)%is_alive())                                &
              call this%individual(ind)%rwalk25d                              &
                    ( meanshift_xy = step_size_walk_xy(ind),                  &
                      cv_shift_xy = cv_array_xy_here(ind),                    &
                      meanshift_depth = step_size_walk_depth(ind),            &
                      cv_shift_depth = cv_array_depth_here(ind),              &
                      environment_limits = environment_limits  )
        end do
      end do
    else ENVIRON_RESTRICT
      do j=1, n_walks_here
        do i=1, pop_n
          ind = pop_permutation(i)
          if (this%individual(ind)%is_alive())                                &
              call this%individual(ind)%rwalk25d                              &
                    ( meanshift_xy = step_size_walk_xy(ind),                  &
                      cv_shift_xy = cv_array_xy_here(ind),                    &
                      meanshift_depth = step_size_walk_depth(ind),            &
                      cv_shift_depth = cv_array_depth_here(ind),              &
                      environment_limits=Global_Habitats_Available(           &
                                      this%individual(ind)%find_environment(  &
                                              Global_Habitats_Available) ) )
        end do
      end do
    end if ENVIRON_RESTRICT

  end subroutine population_rwalk25d_all_agents_step

  !-----------------------------------------------------------------------------
  !> Subject the population to an attack by a specific predator. The predator
  !! acts on agents in its proximity and takes account of the predation
  !! confusion and dilution effects (see
  !! the_environment::predator::risk_fish_group()).
  subroutine population_subject_predator_attack(this, this_predator,          &
                                                                time_step_model)
    class(POPULATION), intent(inout) :: this
    class(PREDATOR), intent(in) :: this_predator
    integer, optional, intent(in) :: time_step_model

    ! Local copie sof optionals
    integer :: time_step_model_here

    !> ### Notable variables ###
    !> - p_risk is an array with the size equal to the agent population size,
    !!   that keeps all the attack probabilities calculated by the
    !!   the_environment::predator::risk_fish_group() function.
    real(SRP), dimension(size(this%individual)) :: p_risk

    !> - prey_index is the partial index of the prey agents that are in proximity of
    !!   this predator.
    !! .
    integer, dimension(size(this%individual)) :: prey_index

    ! This is a temporary array of the the_environment::spatial type to keep
    ! the location of the prey agent data.
    ! COMPILER BUG REPORT:
    ! @warning It is necessary as a work around the bug in GNU gfortran
    !          where the array is passed into the (neighbours) subroutine
    !          and suddenly loses the standard Fortran array bounds starting
    !          from 1 and gets C-specific bounds starting from 0, keeping
    !          the overall array size intact.
    type(SPATIAL), dimension(size(this%individual)) :: tmp_location

    ! Local counter
    integer :: i

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME =                                 &
                                        "(population_subject_predator_attack)"

    !> ### Implementation notes ###
    !> **Preparations:** Check optional time step parameter. If not provided,
    !! use global commondata::global_time_step_model_current parameter value.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> **First,** calculate the risk of predation from this specific predator
    !! to each of the agents in the population using the
    !! the_environment::predator::risk_fish_group() method. The "raw" indexed
    !! output scheme is used here to avoid multiple cycling over the whole
    !! large population of agents, only the agents that are closest to the
    !! predator are processed and attacked (maximum number is equal to the
    !! index limit commondata::predator_risk_group_select_index_partial).
    tmp_location = this%individual%location()
    call this_predator%risk_fish_group(                                       &
                          prey_spatial = tmp_location,                        &
                          prey_length = this%individual%get_length(),         &
                          is_freezing = this%individual%freeze%is_executed(), &
                          time_step_model = time_step_model_here,             &
                          risk_indexed = p_risk,                              &
                          index_dist = prey_index )

    !> **Second,** cycle through all the agents in close proximity of the
    !! predator, up to the maximum size of partial indexing parameter
    !! commondata::predator_risk_group_select_index_partial. The predator then
    !! stochastically attacks each of these agents with the probability equal
    !! to the risk of predation. If the attack is successful, the agent
    !! the_genome::individual_genome::dies() and loop is exited because the
    !! predator is assumed to catch only one agent at a time.
    do i=1, PREDATOR_RISK_GROUP_SELECT_INDEX_PARTIAL
      if ( this%individual(prey_index(i))%is_alive() ) then
        if ( RAND_R4() < p_risk(i) ) then
          ! Increment global counter of agents that are eaten by predators
          Global_Ind_N_Eaten_by_Predators = Global_Ind_N_Eaten_by_Predators + 1
          call this%individual(prey_index(i))%dies()
          call LOG_DBG( LTAG_INFO // "The agent # " // TOSTR(prey_index(i)) //&
                        " (" // TOSTR(i) // ")" //                            &
                        " is caught by the predator and dies.",               &
                        PROCNAME, MODNAME)
          call LOG_DBG( LTAG_INFO // "Agent status is: " //                   &
                        TOSTR(this%individual(prey_index(i))%is_alive()) )
          exit ! the predator catches only one agent at a time
        end if
      end if
    end do

  end subroutine population_subject_predator_attack

  !-----------------------------------------------------------------------------
  !> Subject the population to mortality caused by habitat-specific
  !! mortality risk. Each agent is affected by the risk associated with
  !! the habitat it is currently in.
  !> @note Note that there is no such a function for a single agent as it does
  !!       not seem to be necessary.
  subroutine population_subject_other_risks(this)
    class(POPULATION), intent(inout) :: this

    ! Local variable that sets the habitat (number) the agent is currently in
    ! within the global array the_environment::global_habitats_available.
    integer :: agent_in

    ! Local counter.
    integer :: i

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME = "(population_subject_other_risks)"

    !> ### Implementation notes ###
    !> All agents in the population are randomly subjected to the mortality
    !! risk  the_environment::habitat::risk_mortality that is linked to the
    !! habitat object the agent is currently in in a loop.
    do i=1, this%population_size
      agent_in = this%individual(i)%find_environment()
      if (RAND_R4() < Global_Habitats_Available(agent_in)%get_mortality()) then
        !> If the agent is unhappy and is subjected to mortality event, it
        !! immediately the_genome::individual_genome::dies().
        call this%individual(i)%dies()
        call LOG_DBG( LTAG_INFO // "Agent " //                                &
                  TOSTR(this%individual(i)%get_id()) // " dies due to " //    &
                  "habitat linked mortality risk " //                         &
                  TOSTR(Global_Habitats_Available(agent_in)%get_mortality()), &
                  PROCNAME, MODNAME  )
      end if
    end do

  end subroutine population_subject_other_risks

  !-----------------------------------------------------------------------------
  !> Subject all members of this population to their individual mortality
  !! risks.
  subroutine population_subject_individual_risk_mortality(this)
    class(POPULATION), intent(inout) :: this

    ! Local counter.
    integer :: i

    !> ### Implementation notes ###
    !> The procedure is simple, loop over all individual agents in the
    !! population and stochastically call the_genome::individual_genome::dies()
    !! method with probability equal to individual mortality of the agent.
    do i=1, this%population_size
      if ( RAND_R4() < this%individual(i)%get_mortality() )                   &
                                                call this%individual(i)%dies()
    end do

  end subroutine population_subject_individual_risk_mortality

  !-----------------------------------------------------------------------------
  !> This procedure performs a **single step** of the life cycle of the whole
  !! population, the agents for the step are selected in a random order.
  subroutine population_lifecycle_step_preevol(this)
    class(POPULATION), intent(inout) :: this

    !> ### Notable variables ###
    !> - `inds_order` is an array that sets the order in which the agents are
    !!   being drawn out of the population to perform the step.
    integer, dimension(this%population_size) :: inds_order

    !> - `ind_seq` is the sequential number of the agent as drawn from the
    !!   population; it is the loop control counter variable.
    integer :: ind_seq

    !> - `ind_real` is the real sequential number of the agent in the
    !!   population; this real id is obtained from the order array
    !!   `inds_order`.
    !! .
    integer :: ind_real

    ! Local variable that sets the habitat (number) the agent is currently in
    ! within the global array the_environment::global_habitats_available.
    integer :: agent_in

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME =                                 &
                                          "(population_lifecycle_step_preevol)"

    !> ### Implementation notes ###
    !> First, an ordering array `inds_order` is calculated that sets the order
    !! in which the agents are drawn from the population. In the simplest case,
    !! the order of the agents is random, so this array is actually an array of
    !! random integers. The procedure
    !! [PERMUTE_RANDOM](http://ahamodel.uib.no/doc/ar01s09.html#_random_permutation_permute_random_function)
    !! from HEDTOOLS is used here then.
    inds_order = PERMUTE_RANDOM(this%population_size)

    !> The agents can also be processed in any **non-random** order. This would
    !! require invoking an array indexing procedure
    !! [ARRAY_INDEX](http://ahamodel.uib.no/doc/ar01s07.html#_subroutines_array_index_and_array_rank)
    !! instead of `PERMUTE_RANDOM`.
    !!
    !! For example, to process the agents in the order of the body mass,
    !! the ordering array can be obtained obtained as below:
    !! @code
    !! call ARRAY_INDEX(this%individual%get_mass(), inds_order)
    !! @endcode
    !> But this code would rank the agents in an *increasing* order of their
    !! body mass. If this is not what is expected, e.g. if the
    !! non-random selection is used to mimic a competitive advantage for bigger
    !! and heavier agents, a reverse of the ordering is obtained like this:
    !! @code
    !! inds_order = inds_order(this%population_size:1:-1)
    !! @endcode

    !> Second, the agents are drawn from the population, one by one, in the
    !! named loop construct `INDIVIDUALS`.
    INDIVIDUALS: do ind_seq = 1, POPSIZE

      !> - One individual is then drawn from the `inds_order` ordering array.
      ind_real = inds_order(ind_seq)

      associate ( AGENT => this%individual(ind_real) )

        !> #### Initialisations ####
        !> - First, a check is done if the agent is dead or starved to death;
        !!   if yes, no further processing is done. Also, in the former case
        !!   the individual_genome::dies() method is called.
        if (AGENT%is_dead()) then
          cycle INDIVIDUALS
        end if
        if (AGENT%starved_death()) then
          call AGENT%dies()
          cycle INDIVIDUALS
        end if

        !> - `agent_in` index calculates the habitat number where the selected
        !!   agent is currently in, calling the
        !!   the_environment::spatial::find_environment() method.
        agent_in = AGENT%find_environment()
        if (agent_in<1 .or. agent_in>size(Global_Habitats_Available) ) then
          call LOG_MSG( LTAG_ERROR // "agent_in zero in " // PROCNAME //      &
                        TOSTR([AGENT%xpos(),AGENT%ypos(),AGENT%dpos()]) )
          call LOG_MSG( LTAG_ERROR // "Agent: " // TOSTR(ind_real) //         &
                        " in " // TOSTR(ind_seq) )
        end if

        !> #### Get perceptions ####
        !! - Inner perceptions are obtained from the agent's "organism":
        !!   stomach contents, bodymass, energy, age, reproductive factor:
        !!   the_neurobio::perception::perceptions_inner().
        call AGENT%perceptions_inner()

        !> - Simple environmental perceptions are obtained: light, depth:
        !!   the_neurobio::perception::perceptions_environ().
        call AGENT%perceptions_environ()

        !> - Spatial perceptions are obtained for food items, conspecifics and
        !!   predators in proximity of this agent:
        !!   - the_neurobio::perception::see_food()
        !!   - the_neurobio::perception::see_consp()
        !!   - the_neurobio::perception::see_pred()
        !!   .
        call AGENT%see_food( Global_Habitats_Available( agent_in )%food )
        call AGENT%see_consp( this%individual )
        call AGENT%see_pred( Global_Habitats_Available( agent_in )%predators )

        !> - The above perceptions are added into the memory stack calling
        !!   the_neurobio::perception::perception_to_memory().
        !! .
        call AGENT%perception_to_memory()

        !> #### Appraisal: Produce motivations ####
        !> - Perception components are calculated for each of the motivational
        !!   states of the agent via the neuronal response function(s):
        !!   the_neurobio::appraisal::motivations_percept_components().
        call AGENT%motivations_percept_components()
        !> - Then, primary motivation values are calculated from the perception
        !!   components: the_neurobio::appraisal::motivations_primary_calc().
        call AGENT%motivations_primary_calc()
        !> - The values of the primary motivations are subjected to modulation
        !!   the_neurobio::appraisal::modulation().
        call AGENT%modulation()

        !> - Final motivations of the agent are saved into the emotional
        !!   memory stack: the_neurobio::appraisal::motivations_to_memory().
        !! .
        call AGENT%motivations_to_memory()

        !> #### Determine the Global Organismic State ####
        !> - The Global Organismic State of the agent is calculated using the
        !!   the_neurobio::gos_global::gos_find() method.
        call AGENT%gos_find()
        call LOG_DBG( LTAG_INFO //  "Agent " // TOSTR(AGENT%get_id()) //      &
                      ", GOS is: " // AGENT%gos_label() //                    &
                      ", GOS arousal :" // TOSTR(AGENT%arousal()),            &
                      PROCNAME, MODNAME )

        !> - The population-wise maximum motivation value that is used for
        !!   rescaling is calculated now.
        Global_Rescale_Maximum_Motivation =                                   &
                maxval( this%individual%motivations%max_perception() )

        !> #### Determine and execute the optimal behaviour ####
        !> - Once the GOS is determined for this agent, it selects the
        !!   individually optimal behaviour that minimises its expected
        !!   arousal; this behaviour is then executed. Both steps are
        !!   implemented in the the_neurobio::behaviour::do_behave()
        !!   method.
        !! .
        call AGENT%do_behave(                                                 &
                      rescale_max_motivation=Global_Rescale_Maximum_Motivation )
        call LOG_DBG( LTAG_INFO // "Agent " // TOSTR(AGENT%get_id()) //       &
                      " executed behaviour: " // AGENT%behaviour_is() //      &
                      " (at global time_step " //                             &
                      TOSTR(Global_Time_Step_Model_Current) //                &
                      ")", PROCNAME, MODNAME )

        !> #### Update characteristics of the agent ####
        !> - Finally, characteristics of this agent are updated depending on
        !!   the consequences of the behaviour. For example, hormone levels
        !!   are updated the_body::condition::sex_steroids_update() and the
        !!   living cost of the agent is subtracted
        !!   (the_body::condition::subtract_living_cost()).
        !!   Digestion also occurs by emptying the stomach by a fixed
        !!   fraction (the_body::condition::stomach_empify(). Also, the
        !!   energy reserves of the agent are updated based on the current
        !!   mass and length (the_body::condition::energy_update().
        !!   Note that the agent characteristics that directly follow from
        !!   the behaviour unit that has been executed (e.g. food gain if the
        !!   agent  decided to eat a food item or travel cost if the agent
        !!   migrated) are processed and updated in the respective behaviour
        !!   execution method.
        call AGENT%sex_steroids_update()
        call AGENT%subtract_living_cost()
        call AGENT%stomach_empify()
        call AGENT%energy_update()

        !> - Finally, the age of the agent is incremented to one time step.
        call AGENT%age_increment()

        !> - Finally, another check is done if the agent is starved to death,
        !!   if yes, the agent individual_genome::dies().
        !! .
        if (AGENT%starved_death()) then
          call AGENT%dies()
        end if

      end associate

    end do INDIVIDUALS

  end subroutine population_lifecycle_step_preevol

  !-----------------------------------------------------------------------------
  !> This procedure performs a **single step** of the life cycle of the whole
  !! population, the agents for the step are selected in a random order.
  !! @warning This version of the life cycle step includes only optimal food
  !!          selection and eating and does not include the full fledged
  !!          behaviour selection cascade of procedures ::do_behave().
  subroutine population_lifecycle_step_eatonly_preevol(this)
    class(POPULATION), intent(inout) :: this

    !> ### Notable variables ###
    !> - `inds_order` is an array that sets the order in which the agents are
    !!   being drawn out of the population to perform the step.
    integer, dimension(this%population_size) :: inds_order

    !> - `ind_seq` is the sequential number of the agent as drawn from the
    !!   population; it is the loop control counter variable.
    integer :: ind_seq

    !> - `ind_real` is the real sequential number of the agent in the
    !!   population; this real id is obtained from the order array
    !!   `inds_order`.
    !! .
    integer :: ind_real

    ! Local variable that sets the habitat (number) the agent is currently in
    ! within the global array the_environment::global_habitats_available.
    integer :: agent_in

    ! Local variable that keeps the optimal food item
    integer :: food_item_selected

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME =                                 &
                                  "(population_lifecycle_step_eatonly_preevol)"

    !> ### Implementation notes ###
    !> First, an ordering array `inds_order` is calculated that sets the order
    !! in which the agents are drawn from the population. In the simplest case,
    !! the order of the agents is random, so this array is actually an array of
    !! random integers. The procedure
    !! [PERMUTE_RANDOM](http://ahamodel.uib.no/doc/ar01s09.html#_random_permutation_permute_random_function)
    !! from HEDTOOLS is used here then.
    inds_order = PERMUTE_RANDOM(this%population_size)

    !> The agents can also be processed in any **non-random** order. This would
    !! require invoking an array indexing procedure
    !! [ARRAY_INDEX](http://ahamodel.uib.no/doc/ar01s07.html#_subroutines_array_index_and_array_rank)
    !! instead of `PERMUTE_RANDOM`.
    !!
    !! For example, to process the agents in the order of the body mass,
    !! the ordering array can be obtained obtained as below:
    !! @code
    !! call ARRAY_INDEX(this%individual%get_mass(), inds_order)
    !! @endcode
    !> But this code would rank the agents in an *increasing *order of their
    !! of their body mass. If this is not what is expected, e.g. if the
    !! non-random selection is used to mimic a competitive advantage for bigger
    !! and heavier agents, a reverse of the ordering is obtained like this:
    !! @code
    !! inds_order = inds_order(this%population_size:1:-1)
    !! @endcode

    !> Second, the agents are drawn from the population, one by one, in the
    !! named loop construct `INDIVIDUALS`.
    INDIVIDUALS: do ind_seq = 1, POPSIZE

      !> - One individual is then drawn from the `inds_order` ordering array.
      ind_real = inds_order(ind_seq)

      associate ( AGENT => this%individual(ind_real) )

        !> #### Initialisations ####
        !> - First, a check is done if the agent is dead or starved to death;
        !!   if yes, no further processing is done. Also, in the former case
        !!   the individual_genome::dies() method is called.
        if (AGENT%is_dead()) then
          cycle INDIVIDUALS
        end if
        if (AGENT%starved_death()) then
          call AGENT%dies()
          cycle INDIVIDUALS
        end if

        !> - `agent_in` index calculates the habitat number where the selected
        !!   agent is currently in, calling the
        !!   the_environment::spatial::find_environment() method.
        agent_in = AGENT%find_environment()
        if (agent_in<1 .or. agent_in>size(Global_Habitats_Available) ) then
          call LOG_MSG( LTAG_ERROR // "agent_in zero in " // PROCNAME //      &
                        TOSTR([AGENT%xpos(),AGENT%ypos(),AGENT%dpos()]) )
          call LOG_MSG( LTAG_ERROR // "Agent: " // TOSTR(ind_real) //         &
                        " in " // TOSTR(ind_seq) )
        end if

        !> #### Get perceptions ####
        !! - Inner perceptions are obtained from the agent's "organism":
        !!   stomach contents, bodymass, energy, age, reproductive factor:
        !!   the_neurobio::perception::perceptions_inner().
        call AGENT%perceptions_inner()

        !> - Simple environmental perceptions are obtained: light, depth:
        !!   the_neurobio::perception::perceptions_environ().
        call AGENT%perceptions_environ()

        !> - Spatial perceptions are obtained for food items, conspecifics and
        !!   predators in proximity of this agent:
        !!   - the_neurobio::perception::see_food()
        !!   - the_neurobio::perception::see_consp()
        !!   - the_neurobio::perception::see_pred()
        !!   .
        call AGENT%see_food( Global_Habitats_Available( agent_in )%food )
        call AGENT%see_consp( this%individual )
        call AGENT%see_pred( Global_Habitats_Available( agent_in )%predators )

        !> - The above perceptions are added into the memory stack calling
        !!   the_neurobio::perception::perception_to_memory().
        !! .
        call AGENT%perception_to_memory()

        !> #### Appraisal: Produce motivations ####
        !> - Perception components are calculated for each of the motivational
        !!   states of the agent via the neuronal response function(s):
        !!   the_neurobio::appraisal::motivations_percept_components().
        call AGENT%motivations_percept_components()
        !> - Then, primary motivation values are calculated from the perception
        !!   components: the_neurobio::appraisal::motivations_primary_calc().
        call AGENT%motivations_primary_calc()
        !> - The values of the primary motivations are subjected to modulation
        !!   the_neurobio::appraisal::modulation().
        call AGENT%modulation()

        !> - Final motivations of the agent are saved into the emotional
        !!   memory stack: the_neurobio::appraisal::motivations_to_memory().
        !! .
        call AGENT%motivations_to_memory()

        !> #### Determine the Global Organismic State ####
        !> - The Global Organismic State of the agent is calculated using the
        !!   the_neurobio::gos_global::gos_find() method.
        call AGENT%gos_find()
        call LOG_DBG( LTAG_INFO //  "Agent " // TOSTR(AGENT%get_id()) //      &
                      ", GOS is: " // AGENT%gos_label() //                    &
                      ", GOS arousal :" // TOSTR(AGENT%arousal()),            &
                      PROCNAME, MODNAME )

        !> - The population-wise maximum motivation value that is used for
        !!   rescaling is calculated now.
        Global_Rescale_Maximum_Motivation =                                   &
                maxval( this%individual%motivations%max_perception() )

        !> #### Determine and execute the optimal behaviour ####
        !> - Once the GOS is determined for this agent, it selects the optimal
        !!   food item that minimises its expected arousal and then tries to
        !!   eat this food item. However, if the agent has no food in its
        !!   perception a default random walk is executed.
        !! .
        DO_BEHAVE: if ( AGENT%has_food() ) then
          food_item_selected = AGENT%food_item_select(                        &
                      rescale_max_motivation=Global_Rescale_Maximum_Motivation)
          ! ++++ EAT FOOD ITEM
          call AGENT%do_eat_food_item(                                        &
                food_item_selected, Global_Habitats_Available( agent_in )%food )
        else DO_BEHAVE
          call AGENT%do_walk()
        end if DO_BEHAVE

        call LOG_DBG( LTAG_INFO // "Agent " // TOSTR(AGENT%get_id()) //       &
                      " executed behaviour: " // AGENT%behaviour_is() //      &
                      " (at global time_step " //                             &
                      TOSTR(Global_Time_Step_Model_Current) //                &
                      ")", PROCNAME, MODNAME )

        !> #### Update characteristics of the agent ####
        !> - Finally, characteristics of this agent are updated depending on
        !!   the consequences of the behaviour. For example, hormone levels
        !!   are updated and the living cost of the agent is subtracted.
        !!   Note that the agent characteristics that directly follow from
        !!   the behaviour unit that has been executed (e.g. food gain if the
        !!   agent  decided to eat a food item or travel cost if the agent
        !!   migrated) are processed and updated in the respective behaviour
        !!   execution method.
        call AGENT%sex_steroids_update()
        call AGENT%subtract_living_cost()
        call AGENT%energy_update()

        !> - Finally, the age of the agent is incremented to one time step.
        call AGENT%age_increment()

        !> - Finally, another check is done if the agent is starved to death,
        !!   if yes, the agent individual_genome::dies().
        !! .
        if (AGENT%starved_death()) then
          call AGENT%dies()
        end if

      end associate

    end do INDIVIDUALS

  end subroutine population_lifecycle_step_eatonly_preevol

  !-----------------------------------------------------------------------------
  !> Save data for all agents within the population into a CSV file.
  !> @note Note that this procedure is not using the @ref file_io wrappers.
  subroutine population_save_data_all_agents_csv(this, csv_file_name,         &
                                        save_header, is_logging, is_success)
    use CSV_IO
    class(POPULATION), intent(in) :: this
    !> @param[in] csv_file_name is the name of the CSV output file.
    character(len=*), intent(in) :: csv_file_name
    !> @param[in] save_header turn ON/OFF of the descriptive file header.
    !!            Header is saved into the first row of the CSV output file
    !!            If not present, default is FALSE.
    logical, optional, intent(in) :: save_header
    !> @param[in] is_logging turn ON/OFF writing the file name and data into
    !!            the logger. If not present, default is TRUE if it is the
    !!            debug mode.
    logical, optional, intent(in) :: is_logging
    !> @param[out] is_success Flag showing that data save was successful
    !!             (if TRUE).
    logical, optional, intent(out) :: is_success

    ! Local copies of optionals.
    logical :: logging_enabled

    ! Counter
    integer :: ind

    !> ### Implementation notes ###
    !> #### Local variables for CSV backend ####
    !> - `handle_csv` is the CSV file handle object defining the file name,
    !!   Fortran unit and error descriptor, see HEDTOOLS manual for details.
    type(CSV_FILE) :: handle_csv
    !> - `csv_record_tmp` is the temporary character string that keeps the
    !!   whole record of the file, i.e. the whole row of the spreadsheet table.
    character(len=:), allocatable :: csv_record_tmp
    !> - `COLUMNS` is a parameter array that keeps all column headers; its
    !!   size is equal to the total number of variables (columns) in the data
    !!   spreadsheet file.
    !! .
    character(len=LABEL_LENGTH), dimension(*),                       &
      parameter :: COLUMNS =  [ character(len=LABEL_LENGTH) ::       &  ! COLS
                                                      "ID_NUM   ",   &  !  1
                                                      "PERS_NAME",   &  !  2
                                                      "ALIVE    ",   &  !  3
                                                      "SEX_MALE ",   &  !  4
                                                      "BODY_LEN ",   &  !  5
                                                      "BIRTH_LEN",   &  !  6
                                                      "CTRL_RND ",   &  !  7
                                                      "BODY_MASS",   &  !  8
                                                      "BIRTHMASS",   &  !  9
                                                      "ENERGY   ",   &  ! 10
                                                      "BIRT_ENER",   &  ! 11
                                                      "STOMACH  ",   &  ! 12
                                                      "MAXSTOMCP",   &  ! 13
                                                      "SMR      ",   &  ! 14
                                                      "S_COST_SW",   &  ! 15
                                                      "LIV_COST ",   &  ! 16
                                                      "MORTALITY",   &  ! 17
                                                      "HORM_GROW",   &  ! 18
                                                      "HORM_THYR",   &  ! 19
                                                      "HORM_ADRE",   &  ! 20
                                                      "HORM_CORT",   &  ! 21
                                                      "HORM_TEST",   &  ! 22
                                                      "HORM_ESTR",   &  ! 23
                                                      "HTEST_BAS",   &  ! 24
                                                      "HESTR_BAS",   &  ! 25
                                                      "REPR_FAC ",   &  ! 26
                                                      "P_REPROD ",   &  ! 27
                                                      "N_REPROD ",   &  ! 28
                                                      "N_OFFSPNG",   &  ! 29
                                                      "AGE      ",   &  ! 30
                                                      "GOS_MAIN ",   &  ! 31
                                                      "GOS_ARUSL",   &  ! 32
                                                      "GOS_REPET",   &  ! 33
                                                      "EAT_ATMPT",   &  ! 34
                                                      "N_EATEN  ",   &  ! 35
                                                      "MASSEATEN",   &  ! 36
                                                      "PERC_FOOD",   &  ! 37
                                                      "PERC_CONS",   &  ! 38
                                                      "PERC_PRED",   &  ! 39
                                                      "POS_X    ",   &  ! 40
                                                      "POS_Y    ",   &  ! 41
                                                      "POS_DEPTH",   &  ! 42
                                                      "HABITAT  ",   &  ! 43
                                                      "FITNNESS "  ]    ! 44

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter ::                                            &
                            PROCNAME = "(population_save_data_all_agents_csv)"

    ! Local, the name of the habitat the agent is currently in, out of the
    ! commondata::global_habitats_available array.
    character(len=LABEL_LENGTH) :: habitat_in

    ! Local timer object for file write.
    type(TIMER_CPU) :: file_write_timing

    if (present(is_logging)) then
      logging_enabled = is_logging
    else
      logging_enabled = IS_DEBUG
    end if

    if (logging_enabled) then
      call LOG_MSG (LTAG_INFO // "Saving all individuals in population # " // &
              TOSTR(this%pop_number) // " (name '" // trim(this%pop_name) //  &
              "'), " //                                                       &
              "generation # " // TOSTR(Global_Generation_Number_Current) //   &
              ", time step " // TOSTR(Global_Time_Step_Model_Current) //      &
              " to file: " // csv_file_name )
      call file_write_timing%start(                                           &
                  "Writing population data for population '" //               &
                  trim(this%pop_name) // "' " // TOSTR(size(this%individual)) &
                  // " individuals" )
    end if

    !> #### Save data in CSV file ####
    !> @note Note that this subroutine does not use the object oriented
    !!       wrappers from the @ref file_io module.
    !!
    !> - Define the file name \%name component of the CSV file handle. The
    !!   file handle object `handle_csv` is now used as the file identifier.
    handle_csv%name = csv_file_name
    !> - Open the output file defined by the `handle_csv` handle object for
    !!   writing.
    call CSV_OPEN_WRITE( handle_csv )

    !> - Possible error status of the latest file operation is obtained by the
    !!   \%status component of the file handle. Check if there were any errors
    !!   opening the file and report in the logger with the error tag.
    if ( .not. handle_csv%status ) then
        call LOG_MSG( LTAG_ERROR // "Opening output CSV file FAILED: " //     &
                      csv_file_name // ", in " // PROCNAME )
        call LOG_MSG( LTAG_ERROR // "Data file " // csv_file_name //          &
                      " is not written in " // PROCNAME )
        if (present(is_success)) is_success = handle_csv%status
        return
    end if

    !> - If the `save_header` flag is set to TRUE, save the CSV file header.
    if (present(save_header)) then
      if (save_header) then
        call CSV_HEADER_WRITE( "Population: " // this%pop_name, handle_csv )
        if ( .not. handle_csv%status ) then    ! treat possible write error.
          if (present(is_success)) is_success = .FALSE.
          call CSV_CLOSE( handle_csv )
          return
        end if
      end if
    end if

    !> - Prepare the character string variable `csv_record_tmp` that keeps the
    !!   whole record (row) of data in the output CSV data file. The length of
    !!   this string should be enough to fit all the record data, otherwise
    !!   the record is truncated.
    csv_record_tmp = repeat(" ", size(COLUMNS) * len(COLUMNS(1)) )

    !> - Produce the first record containing the column headers (variable
    !!   names). Note that
    !!   [CSV_RECORD_APPEND()](http://ahamodel.uib.no/doc/ar01s08.html#_subroutine_csv_record_append)
    !!   accepts both arrays and scalar values for appending. Also, write the
    !!   first record physically to the file.
    call CSV_RECORD_APPEND( csv_record_tmp, COLUMNS )
    call CSV_RECORD_WRITE ( csv_record_tmp, handle_csv )
    if ( .not. handle_csv%status ) then    ! treat possible write error.
      if (present(is_success)) is_success = .FALSE.
      call CSV_CLOSE( handle_csv )
      return
    end if


    !> - The actual data are written to the CSV file in a loop over all the
    !!   individual members of the population. One record (row) of the data
    !!   file then represents a single individual.
    do ind = 1, size(this%individual)
      !>   - the `csv_record_tmp` character string variable is produced such
      !!     that it can fit the whole record;
      csv_record_tmp = repeat(" ",                                            &
                      max( CSV_GUESS_RECORD_LENGTH(size(COLUMNS) + 1,0.0_SRP),&
                            len(this%individual(ind)%genome_label) ) )

      !>   - the actual data for the individual is appended to the current
      !!     record one by one. Note that logical values are converted to
      !!     integers using commondata::conv_l2r() function.
      associate ( AGENT => this%individual(ind) )                          !COLS
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%person_number        ) ! 1
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%genome_label         ) ! 2
        call CSV_RECORD_APPEND(csv_record_tmp,conv_l2r(AGENT%alive)      ) ! 3*
        call CSV_RECORD_APPEND(csv_record_tmp,conv_l2r(AGENT%sex_is_male)) ! 4*
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%body_length          ) ! 5
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%body_length_birth    ) ! 6
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%control_unselected   ) ! 7
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%body_mass            ) ! 8
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%body_mass_birth      ) ! 9
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%energy_current       ) ! 10
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%energy_birth         ) ! 11
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%stomach_content_mass ) ! 12
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%maxstomcap           ) ! 13
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%smr                  ) ! 14
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%cost_swim_std()      ) ! 15
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%living_cost()        ) ! 16
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%ind_mortality        ) ! 17
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%growhorm_level       ) ! 18
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%thyroid_level        ) ! 19
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%adrenaline_level     ) ! 20
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%cortisol_level       ) ! 21
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%testosterone_level   ) ! 22
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%estrogen_level       ) ! 23
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%testosterone_baseline) ! 24
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%estrogen_baseline    ) ! 25
        if ( AGENT%is_male() ) then  ! repr. factor in males and females
          call CSV_RECORD_APPEND(csv_record_tmp,AGENT%testosterone_level )
        else
          call CSV_RECORD_APPEND(csv_record_tmp,AGENT%estrogen_baseline  ) ! 26
        endif
        call CSV_RECORD_APPEND(csv_record_tmp,                           &
                                        AGENT%probability_reproduction() ) ! 27
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%n_reproductions      ) ! 28
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%n_offspring          ) ! 29
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%age                  ) ! 30
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%gos_main             ) ! 31
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%gos_arousal          ) ! 32
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%gos_repeated         ) ! 33
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%n_eats_all_indicator ) ! 34
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%n_eaten_indicator    ) ! 35
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%mass_eaten_indicator ) ! 36
        call CSV_RECORD_APPEND(csv_record_tmp,                           &
                                  AGENT%memory_stack%get_food_mean_n()   ) ! 37
        call CSV_RECORD_APPEND(csv_record_tmp,                           &
                                  AGENT%memory_stack%get_consp_mean_n()  ) ! 38
        call CSV_RECORD_APPEND(csv_record_tmp,                           &
                                  AGENT%memory_stack%get_pred_mean()     ) ! 39
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%x                    ) ! 40
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%y                    ) ! 41
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%depth                ) ! 42
        if ( AGENT%is_alive() ) then
          habitat_in = Global_Habitats_Available(                        &
                                    AGENT%find_environment())%get_label()
        else
          ! If the agent is dead and nullified, its habitat cannot be
          ! determined.
          habitat_in = "agent_dead"
        end if
        call CSV_RECORD_APPEND(csv_record_tmp,habitat_in                 ) ! 43
        call CSV_RECORD_APPEND(csv_record_tmp,AGENT%fitness              ) ! 44
      end associate

      !>   - after all data are appended to the record, this record is
      !!     physically written to the disk using
      !!     [CSV_RECORD_WRITE()](http://ahamodel.uib.no/doc/ar01s08.html#_subroutine_csv_record_write).
      !!   .
      call CSV_RECORD_WRITE( csv_record_tmp, handle_csv )
      if ( .not. handle_csv%status ) then    ! treat possible write error.
        if (present(is_success)) is_success = .FALSE.
        call CSV_CLOSE( handle_csv )
        return
      end if

    end do

    !> - When all the records are saved, the CSV file is closed with
    !!   [CSV_CLOSE()](http://ahamodel.uib.no/doc/ar01s08.html#_subroutine_csv_close).
    call CSV_CLOSE( handle_csv )

    !> - This is finally sent to the logger (if `logging_enabled` is TRUE).
    !! .
    if (logging_enabled) then
      call LOG_MSG (LTAG_INFO // "Individual data saved, population size " // &
              TOSTR(this%population_size) //                                  &
              ", number of columns " // TOSTR(size(COLUMNS)) )
      if ( .not. handle_csv%status ) call LOG_MSG( LTAG_ERROR //              &
                                              "File write operation FAILED." )
      call LOG_MSG( file_write_timing%log() )
    end if

    if (present(is_success)) is_success = handle_csv%status

    !> The CSV output data file can be optionally compressed with the
    !! commondata::cmd_zip_output command if commondata::is_zip_outputs is set
    !! to TRUE.
    if ( IS_ZIP_OUTPUTS ) then
      call call_external(command=CMD_ZIP_OUTPUT // " " // csv_file_name,      &
                         suppress_output=.TRUE.,                              &
                         is_background_task=ZIP_OUTPUTS_BACKGROUND )
    end if

  end subroutine population_save_data_all_agents_csv

  !-----------------------------------------------------------------------------
  !> Save the genome data of all agents in this population to a CSV file.
  subroutine population_save_data_all_genomes(this, csv_file_name, is_success)
    use FILE_IO
    use CSV_IO
    class(POPULATION), intent(in) :: this
    !> @param[in] csv_file_name is the name of the CSV output file.
    character(len=*), intent(in) :: csv_file_name
    !> @param[out] is_success Flag showing that data save was successful
    !!             (if TRUE).
    logical, optional, intent(out) :: is_success

    ! Local counters.
    integer :: i, j, k, l, m

    ! File handle object.
    type(FILE_HANDLE) :: genome_file

    ! **Column names**
    ! The column names are built from the components below with the
    ! consecutive digits indicating the order of each component.
    character(len=*), parameter :: TAG_CRO = "CRO_"   ! Chromosome
    character(len=*), parameter :: TAG_GAP = "_"      ! a single gap
    character(len=*), parameter :: TAG_ALE = "_ALE_"  ! Allele
    character(len=*), parameter :: TAG_ACO = "_AC_"   ! Allele component

    ! The length of the digit part of the column name, assumed two for the
    ! normal range 1:99
    integer, parameter :: DIG_LEN = 2

    ! Column length hard limit
    integer, parameter :: COL_LEN = len(TAG_CRO) + DIG_LEN +                  &
                                    len(TAG_GAP) + DIG_LEN +                  &
                                    len(TAG_ALE) + DIG_LEN +                  &
                                    len(TAG_ACO) + DIG_LEN

    ! Column (variable) headers.
    character(len=COL_LEN), allocatable, dimension(:) :: colname

    ! The number of columns in the output data file.
    integer :: n_columns

    ! *Record* that keeps the whole row of data. See @ref file_io.
    character(len=:), allocatable :: record_csv

    ! Separate variables were necessary to work around ifort 17 compiler bug:
    ! it crashed when everything was placed inline into the CSV_RECORD_APPEND():
    ! HEDG2_04.f90(45689): internal error: Please visit
    ! 'http://www.intel.com/software/products/support' for assistance.
    !       record_csv = repeat(" ", len(TOSTR(ALLELERANGE_MAX)) * n_columns +
    ! ^
    ! [ Aborting due to internal error. ]
    integer :: record_csv_max_length
    integer :: allele_val_append

    !> ### Implementation notes ###
    !> #### Build column names ####
    !> First, determine the number of columns in the data file. The number of
    !! columns is calculated by unwinding the whole data structure
    !! - `chromosome(j,k)%allele(l)%allele_value(m)`
    !! .
    !! See @ref the_genome for more details on the data structure.
    n_columns=0
    do j=1, N_CHROMOSOMES
      do k=1, CHROMOSOME_PLOIDY
        do l=1, LEN_CHROMOSOMES(j)
          do m=1, ADDITIVE_COMPS
            n_columns = n_columns + 1
          end do
        end do
      end do
    end do

    !> The array of the column names is then allocated to the above number.
    allocate(colname(n_columns))

    !> The column names are built again by unwinding the whole genome data
    !! structure into a linear sequence. The column names are like this:
    !!
    !! `CRO_1_1_ALE_01_AC_1, CRO_1_1_ALE_01_AC_2, CRO_1_1_ALE_01_AC_3, ... `
    i=1
    do j=1, N_CHROMOSOMES
      do k=1, CHROMOSOME_PLOIDY
        do l=1, LEN_CHROMOSOMES(j)
          do m=1, ADDITIVE_COMPS
            ! CRO_1_1_ALE_01_AC_1, CRO_1_1_ALE_01_AC_2, CRO_1_1_ALE_01_AC_3, ...
            colname(i) = TAG_CRO // TOSTR(j) //                               &
                          TAG_GAP // TOSTR(k) //                              &
                            TAG_ALE // TOSTR(l,10) //                         &
                              TAG_ACO // TOSTR(m)
            i=i+1
          end do
        end do
      end do
    end do

    !> #### Write data to the file ####
    !> First, the file is opened for writing.
    call genome_file%open_write( csv_file_name, FORMAT_CSV )
    if ( .not. genome_file%is_success() ) then
      if (present(is_success)) is_success = .FALSE.
      call genome_file%close()
      return
    end if

    !> The first record of the data that contains the column names is then
    !! "appended" into the complete record and written to the file.
    !! The length of this record is calculated based on the length of the
    !! columns and their number.
    record_csv = repeat( " ", LABEL_LENGTH * 2 + 2 * 3 +                      &
                              COL_LEN * n_columns + n_columns * 3  )
    !! - The first two columns contain the identifiers for each agent:
    !!   - numeric ID of the agent
    !!   - test string label ("name") of the agent
    !!   .
    call CSV_RECORD_APPEND( record_csv,                                       &
                    [ character(len=LABEL_LENGTH) :: "ID_NUM", "AGENT_NAME"] )

    !> - The remaining columns contain the chromosome and gene labels
    !!   (see above);
    !! .
    call CSV_RECORD_APPEND( record_csv, colname )

    !> This first line consisting of column names is then written to the
    !! output file.
    call genome_file%record_write( record_csv )
    if ( .not. genome_file%is_success() ) then
      if (present(is_success)) is_success = .FALSE.
      call genome_file%close()
      return
    end if

    !> The maximum length of the data record is calculated as the maximum
    !! string length of a single data value multiplied by the number of
    !! columns. Because the record also adds separators, the number of columns
    !! multiplied by three is added to this value.
    record_csv_max_length = LABEL_LENGTH * 2 + 2 * 3 +                        &
                            len(TOSTR(ALLELERANGE_MAX)) * n_columns +         &
                                                                n_columns * 3

    !> Finally, cycle over all individuals in this population and
    !! save the genome data. The first two columns are
    !! the_genome::individual_genome::person_number and
    !! the_genome::individual_genome::genome_label. The other columns
    !! "unwind" the genome data structure over the inner loops for
    !! chromosomes, homologues, alleles and allele components.
    !! - - `chromosome(j,k)%allele(l)%allele_value(m)`
    !! .
    !! See @ref the_genome for more details on the data structure.
    INDS: do i = 1, this%population_size

      record_csv = repeat(" ", record_csv_max_length )

      call CSV_RECORD_APPEND( record_csv, this%individual(i)%person_number )
      call CSV_RECORD_APPEND( record_csv, this%individual(i)%genome_label )

      GENOME: do j=1, N_CHROMOSOMES
                do k=1, CHROMOSOME_PLOIDY
                  do l=1, LEN_CHROMOSOMES(j)
                    do m=1, ADDITIVE_COMPS
                      allele_val_append = this%individual(i)%                 &
                                  chromosome(j,k)%allele(l)%allele_value(m)
                      call CSV_RECORD_APPEND( record_csv, allele_val_append )
                    end do
                  end do
                end do
              end do GENOME
      call genome_file%record_write( record_csv )
      if ( .not. genome_file%is_success() ) then
        if (present(is_success)) is_success = .FALSE.
        call genome_file%close()
        return
      end if

    end do INDS

    !> Once all individuals are saved, the file is closed.
    call genome_file%close()
    if ( .not. genome_file%is_success() ) then
      if (present(is_success)) is_success = .FALSE.
    else
      if (present(is_success)) is_success = .TRUE.
    end if

    !> The CSV output data file can be optionally compressed with the
    !! commondata::cmd_zip_output command if commondata::is_zip_outputs is set
    !! to TRUE.
    if ( IS_ZIP_OUTPUTS ) then
      call call_external(command=CMD_ZIP_OUTPUT // " " // csv_file_name,      &
                         suppress_output=.TRUE.,                              &
                         is_background_task=ZIP_OUTPUTS_BACKGROUND )
    end if

  end subroutine population_save_data_all_genomes

  !-----------------------------------------------------------------------------
  !> Save the perceptual and emotional memory stack data of all agents in this
  !! population to a CSV file.
  subroutine population_save_data_memory(this, csv_file_name, is_success)
    use FILE_IO
    use CSV_IO
    class(POPULATION), intent(in) :: this
    !> @param[in] csv_file_name is the name of the CSV output file.
    character(len=*), intent(in) :: csv_file_name
    !> @param[out] is_success Flag showing that data save was successful
    !!             (if TRUE).
    logical, optional, intent(out) :: is_success

    ! Local counters.
    integer :: i, j, agent

    ! File handle object.
    type(FILE_HANDLE) :: memory_file

    ! *Record* that keeps the whole row of data. See @ref file_io.
    character(len=:), allocatable :: record_csv

    ! The length of a single record of data.
    integer :: record_csv_max_length

    !> ### Implementation details ###
    !> #### Notable variables ####
    !> - **COLUMNS_PERC** defines the column names for all components of the
    !!   perceptual memory stack. They must agree with the components of
    !!   perceptual memory: the_neurobio::memory_perceptual
    character(len=LABEL_LENGTH), dimension(*), parameter ::                   &
                  COLUMNS_PERC =  [ character(len=LABEL_LENGTH) ::            &
                                      ! Perception memory:
                                          "PERC_LIGHT",               & !  1
                                          "PERC_DEPTH",               & !  2
                                          "PERC_FOOD",                & !  3
                                          "PERC_FOODSIZ",             & !  4
                                          "PERC_FOODIST",             & !  5
                                          "PERC_CONSP",               & !  6
                                          "PERC_PRED",                & !  7
                                          "PERC_STOM",                & !  8
                                          "PERC_BDMASS",              & !  9
                                          "PERC_ENERG",               & ! 10
                                          "PERC_REPRFAC"  ]             ! 11

    !> - **COLUMNS_EMOT** defines the column name for all components of the
    !!   emotional memory stack. They must agree with the components of
    !!   emotional memory: the_neurobio::memory_emotional.
    ! .
    character(len=LABEL_LENGTH), dimension(*), parameter ::                   &
                  COLUMNS_EMOT = [ character(len=LABEL_LENGTH) ::             &
                                      ! Emotional memory:
                                          "MOTIV_HUNGER",             & ! 1
                                          "MOTIV_AVOIDACT",           & ! 2
                                          "MOTIV_REPROD",             & ! 3
                                      ! GOS memory:
                                          "GOS_MAIN",                 & ! 4
                                          "GOS_AROUSAL",              & ! 5
                                          "GOS_REPEATED"  ]             ! 6

    !> #### Preliminary ####
    !> First, the file `csv_file_name` is opened for writing.
    call memory_file%open_write( csv_file_name, FORMAT_CSV )
    if ( .not. memory_file%is_success() ) then
      if (present(is_success)) is_success = .FALSE.
      call memory_file%close()
      return
    end if

    !> #### Build column names ####
    !> The maximum length of the data record is calculated from three components
    !! - individual IDs: numeric ID and label;
    !! - perceptual memory components from `COLUMNS_PERC` array for
    !!   commondata::history_size_perception steps;
    !! - emotional memory components from `COLUMNS_EMOT` array for
    !!   commondata::history_size_motivation steps.
    !! .
    !! Note that, because the record also adds separators, such as comma and
    !! possibly double quotes, the number of columns multiplied by three is
    !! added to this value.
    record_csv_max_length =                                                   &
          LABEL_LENGTH * 2 + 2 * 3 +                                          &
          (LABEL_LENGTH * size(COLUMNS_PERC) + size(COLUMNS_PERC) * 3) *      &
                  HISTORY_SIZE_PERCEPTION +                                   &
          (LABEL_LENGTH * size(COLUMNS_EMOT) + size(COLUMNS_EMOT) * 3) *      &
                  HISTORY_SIZE_MOTIVATION

    !> The first record of the data that contains the column names is now being
    !! "appended" into the complete record and written to the file.
    !! The maximum length of this record is calculated above.
    record_csv = repeat( " ", record_csv_max_length )

    !! The first two columns contain the identifiers for each agent:
    !! - numeric ID of the agent
    !! - test string label ("name") of the agent
    !! .
    call CSV_RECORD_APPEND( record_csv,                                       &
                    [ character(len=LABEL_LENGTH) :: "ID_NUM", "AGENT_NAME"] )

    !> The next portion is composed of the perceptual memory columns from
    !! `COLUMNS_PERC` array for each of the commondata::history_size_perception
    !! steps in the memory.
    !! @note Note that the order of data is: (1) perception component,
    !!       (2) history steps:
    !!       `PERC_LIGHT01, PERC_LIGHT02, PERC_LIGHT03, ...
    !!        PERC_DEPTH01, PERC_DEPTH02, PERC_DEPTH03, ...`
    do j = 1, size(COLUMNS_PERC)
      do i = 1, HISTORY_SIZE_PERCEPTION
        call CSV_RECORD_APPEND( record_csv,                                   &
                                trim(COLUMNS_PERC(j)) //                      &
                                    TOSTR(i, HISTORY_SIZE_PERCEPTION) )
      end do
    end do

    !> The third portion is composed of the emotional memory columns from
    !! `COLUMNS_EMOT` array for each of the commondata::history_size_motivation
    !! steps in the memory.
    !! @note Note that the order of data is: (1) motivation component,
    !!       (2) history steps:
    !!       `MOTIV_HUNGER01, MOTIV_HUNGER02, MOTIV_HUNGER03, ...
    !!        MOTIV_AVOIDPAS01, MOTIV_AVOIDPAS02, MOTIV_AVOIDPAS03, ...`.
    do j = 1, size(COLUMNS_EMOT)
      do i = 1, HISTORY_SIZE_MOTIVATION
        call CSV_RECORD_APPEND( record_csv,                                   &
                                trim(COLUMNS_EMOT(j)) //                      &
                                    TOSTR(i, HISTORY_SIZE_MOTIVATION) )
      end do
    end do

    !> After this step, the first record of column names is ready to be
    !! written to the file.
    call memory_file%record_write( record_csv )
    if ( .not. memory_file%is_success() ) then
      if (present(is_success)) is_success = .FALSE.
      call memory_file%close()
      return
    end if

    !> #### Write the numerical data ####
    !> The actual data are written in the same order as above, looping over
    !! all individual agents in this population.
    !!
    !! The maximum record length `record_csv_max_length` is here the same as
    !! for writing the column headers, it is assumed that any numeric value
    !! in the data matrix occupies less than commondata::label_length
    !! characters.
    !!
    !! So, for each agent, the following data are written with full history:
    INDS: do agent=1, this%population_size
      record_csv = repeat(" ", record_csv_max_length )
      associate ( AGENT => this%individual(agent) )
        !> - ID data: numeric ID and the string label;
        call CSV_RECORD_APPEND(record_csv,AGENT%person_number)
        call CSV_RECORD_APPEND(record_csv,AGENT%genome_label )
        !> - Perceptual memory components;
        call CSV_RECORD_APPEND(record_csv,AGENT%memory_stack%memory_light  ) !1
        call CSV_RECORD_APPEND(record_csv,AGENT%memory_stack%memory_depth  ) !2
        call CSV_RECORD_APPEND(record_csv,AGENT%memory_stack%memory_food   ) !3
        call CSV_RECORD_APPEND(record_csv,AGENT%memory_stack%memory_foodsiz) !4
        call CSV_RECORD_APPEND(record_csv,AGENT%memory_stack%memory_foodist) !5
        call CSV_RECORD_APPEND(record_csv,AGENT%memory_stack%memory_consp  ) !6
        call CSV_RECORD_APPEND(record_csv,AGENT%memory_stack%memory_pred   ) !7
        call CSV_RECORD_APPEND(record_csv,AGENT%memory_stack%memory_stom   ) !8
        call CSV_RECORD_APPEND(record_csv,AGENT%memory_stack%memory_bdmass ) !9
        call CSV_RECORD_APPEND(record_csv,AGENT%memory_stack%memory_energ  ) !10
        call CSV_RECORD_APPEND(record_csv,AGENT%memory_stack%memory_reprfac) !11
        !> - Emotional memory components;
        call CSV_RECORD_APPEND(record_csv,                                 & !1
                                  AGENT%memory_motivations%hunger)
        call CSV_RECORD_APPEND(record_csv,                                 & !2
                                  AGENT%memory_motivations%defence_fear)
        call CSV_RECORD_APPEND(record_csv,                                 & !3
                                  AGENT%memory_motivations%reproduction)
        !> - GOS memory components, note here that `gos_main` is a text value
        !!   that is undefined (empty string) at the initialisation stage;
        !! .
        call CSV_RECORD_APPEND(record_csv,                                 & !4
                                  AGENT%memory_motivations%gos_main)
        call CSV_RECORD_APPEND(record_csv,                                 & !5
                                  AGENT%memory_motivations%gos_arousal)
        call CSV_RECORD_APPEND(record_csv,                                 & !6
                                  AGENT%memory_motivations%gos_repeated)
      end associate

      !> Each complete record is written to the file as it is built.
      call memory_file%record_write( record_csv )
      if ( .not. memory_file%is_success() ) then
        if (present(is_success)) is_success = .FALSE.
        call memory_file%close()
        return
      end if

    end do INDS

    !> Once all individuals are saved, the file is closed.
    call memory_file%close()
    if ( .not. memory_file%is_success() ) then
      if (present(is_success)) is_success = .FALSE.
    else
      if (present(is_success)) is_success = .TRUE.
    end if

    !> The CSV output data file can be optionally compressed with the
    !! commondata::cmd_zip_output command if commondata::is_zip_outputs is set
    !! to TRUE.
    if ( IS_ZIP_OUTPUTS ) then
      call call_external(command=CMD_ZIP_OUTPUT // " " // csv_file_name,      &
                         suppress_output=.TRUE.,                              &
                         is_background_task=ZIP_OUTPUTS_BACKGROUND )
    end if

  end subroutine population_save_data_memory

  !-----------------------------------------------------------------------------
  !> Save the latest movement history of all agents.
  !! This method makes use of the the_environment::spatial_moving::history
  !! structure that saves latest movements of each agent.
  subroutine population_save_data_movements(this, csv_file_name, is_success)
    use FILE_IO
    use CSV_IO
    class(POPULATION), intent(in) :: this
    !> @param[in] csv_file_name is the name of the CSV output file.
    character(len=*), intent(in) :: csv_file_name
    !> @param[out] is_success Flag showing that data save was successful
    !!             (if TRUE).
    logical, optional, intent(out) :: is_success

    ! Local counters.
    integer :: i, agent

    ! File handle object.
    type(FILE_HANDLE) :: history_file

    ! *Record* that keeps the whole row of data. See @ref file_io.
    character(len=:), allocatable :: record_csv

    ! The length of a single record of data.
    integer :: record_csv_max_length

    !> ### Implementation notes ###
    !> The maximum length of the data record is calculated from three
    !! components: (1) commondata::history_size_spatial * 3 columns of X, Y,
    !! and depth coordinates plus (2) the same number of separators for these
    !! data columns (assuming 3 characters) plus (3) two additional columns
    !! that contain the numeric ID of the agent and its string label.
    record_csv_max_length =  HISTORY_SIZE_SPATIAL * LABEL_LENGTH * 3 +        &
                             HISTORY_SIZE_SPATIAL * 3 * 3  +                  &
                             LABEL_LENGTH * 2 + 2 * 3

    !> First, the file `csv_file_name` is opened for writing.
    call history_file%open_write( csv_file_name, FORMAT_CSV )
    if ( .not. history_file%is_success() ) then
      if (present(is_success)) is_success = .FALSE.
      call history_file%close()
      return
    end if

    !> #### Build column names ####
    !> The first record of the data that contains the column names is now being
    !! built. The maximum length of this record is calculated above.
    !! First thing to do is to cleanup the record string.
    record_csv = repeat( " ", record_csv_max_length )

    !> After this, the first record is built by appending components.
    !!
    !! The first two columns contain the identifiers for each agent:
    !! - numeric ID of the agent
    !! - test string label ("name") of the agent
    !! .
    call CSV_RECORD_APPEND( record_csv,                                       &
                    [ character(len=LABEL_LENGTH) :: "ID_NUM", "AGENT_NAME"] )

    !> All remaining columns are built in a loop, by triplets: "X", "Y",
    !! "Depth" for each step of the history, up to
    !! commondata::history_size_spatial triplets.
    do i=1, HISTORY_SIZE_SPATIAL
      call CSV_RECORD_APPEND( record_csv,                                     &
                              "X_" // TOSTR(i, HISTORY_SIZE_SPATIAL),         &
                              "Y_" // TOSTR(i, HISTORY_SIZE_SPATIAL),         &
                              "D_" // TOSTR(i, HISTORY_SIZE_SPATIAL)  )
    end do

    !> Now the first record of column names is ready to be written to the file.
    call history_file%record_write( record_csv )
    if ( .not. history_file%is_success() ) then
      if (present(is_success)) is_success = .FALSE.
      call history_file%close()
      return
    end if

    !> #### Write the numerical data ####
    !> The actual data are written in the same order as above, looping over
    !! all individual agents in this population.
    INDS: do agent=1, this%population_size

      !> - The record string is cleaned;
      record_csv = repeat(" ", record_csv_max_length )

      associate ( AGENT => this%individual(agent) )
        !> - ID data appended: numeric ID and the string label;
        call CSV_RECORD_APPEND( record_csv, AGENT%person_number )
        call CSV_RECORD_APPEND( record_csv, AGENT%genome_label  )
        !> - Movement history triplets (x, y, depth) for all
        !!   commondata::history_size_spatial are appended to the
        !!   record string in a loop.
        do i = 1, HISTORY_SIZE_SPATIAL
          call CSV_RECORD_APPEND( record_csv, AGENT%history(i)%x ,            &
                                              AGENT%history(i)%y ,            &
                                              AGENT%history(i)%depth  )
        end do
      end associate

      !> Each complete record is written to the file as it is built.
      call history_file%record_write( record_csv )
      if ( .not. history_file%is_success() ) then
        if (present(is_success)) is_success = .FALSE.
        call history_file%close()
        return
      end if

    end do INDS

    !> Once all individuals are saved, the file is closed.
    call history_file%close()
    if ( .not. history_file%is_success() ) then
      if (present(is_success)) is_success = .FALSE.
    else
      if (present(is_success)) is_success = .TRUE.
    end if

    !> The CSV output data file can be optionally compressed with the
    !! commondata::cmd_zip_output command if commondata::is_zip_outputs is set
    !! to TRUE.
    if ( IS_ZIP_OUTPUTS ) then
      call call_external(command=CMD_ZIP_OUTPUT // " " // csv_file_name,      &
                         suppress_output=.TRUE.,                              &
                         is_background_task=ZIP_OUTPUTS_BACKGROUND )
    end if

  end subroutine population_save_data_movements

  !-----------------------------------------------------------------------------
  !> Save the behaviours history stack the_neurobio::behaviour::history_behave
  !! for all agents.
  subroutine population_save_data_behaviours(this, csv_file_name, is_success)
    use FILE_IO
    use CSV_IO
    class(POPULATION), intent(in) :: this
    !> @param[in] csv_file_name is the name of the CSV output file.
    character(len=*), intent(in) :: csv_file_name
    !> @param[out] is_success Flag showing that data save was successful
    !!             (if TRUE).
    logical, optional, intent(out) :: is_success

    ! Local counters.
    integer :: i, agent

    ! File handle object.
    type(FILE_HANDLE) :: history_file

    ! *Record* that keeps the whole row of data. See @ref file_io.
    character(len=:), allocatable :: record_csv

    ! The length of a single record of data.
    integer :: record_csv_max_length

    !> ### Implementation notes ###
    !> The maximum length of the data record is calculated from three
    !! components: (1) commondata::history_size_behaviours labels of behaviours
    !! plus (2) the same number of separators for these data columns (assuming
    !! 3 characters) plus (3) two additional columns that contain the numeric
    !! ID of the agent and its string label.
    record_csv_max_length =  HISTORY_SIZE_BEHAVIOURS * LABEL_LENGTH +        &
                             HISTORY_SIZE_BEHAVIOURS * 3  +                  &
                             LABEL_LENGTH * 2 + 2 * 3

    !> First, the file `csv_file_name` is opened for writing.
    call history_file%open_write( csv_file_name, FORMAT_CSV )
    if ( .not. history_file%is_success() ) then
      if (present(is_success)) is_success = .FALSE.
      call history_file%close()
      return
    end if

    !> #### Build column names ####
    !> The first record of the data that contains the column names is now being
    !! built. The maximum length of this record is calculated above.
    !! First thing to do is to cleanup the record string.
    record_csv = repeat( " ", record_csv_max_length )

    !> After this, the first record is built by appending components.
    !!
    !! The first two columns contain the identifiers for each agent:
    !! - numeric ID of the agent
    !! - test string label ("name") of the agent
    !! .
    call CSV_RECORD_APPEND( record_csv,                                       &
                    [ character(len=LABEL_LENGTH) :: "ID_NUM", "AGENT_NAME"] )

    !> All remaining columns are built in a loop for each step of the history,
    !! up to commondata::history_size_behaviours steps back in history.
    do i=1, HISTORY_SIZE_BEHAVIOURS
      call CSV_RECORD_APPEND( record_csv,                                     &
                              "BEHAV_" // TOSTR(i, HISTORY_SIZE_BEHAVIOURS) )
    end do

    !> Now the first record of column names is ready to be written to the file.
    call history_file%record_write( record_csv )
    if ( .not. history_file%is_success() ) then
      if (present(is_success)) is_success = .FALSE.
      call history_file%close()
      return
    end if

    !> #### Write the numerical data ####
    !> The actual data are written in the same order as above, looping over
    !! all individual agents in this population.
    INDS: do agent=1, this%population_size

      !> - The record string is cleaned;
      record_csv = repeat(" ", record_csv_max_length )

      associate ( AGENT => this%individual(agent) )
        !> - ID data appended: numeric ID and the string label;
        call CSV_RECORD_APPEND( record_csv, AGENT%person_number )
        call CSV_RECORD_APPEND( record_csv, AGENT%genome_label  )
        !> - Behaviour history for all commondata::history_size_behaviours
        !!   steps is appended to the record string in a loop.
        do i = 1, HISTORY_SIZE_BEHAVIOURS
          call CSV_RECORD_APPEND( record_csv, AGENT%history_behave(i) )
        end do
      end associate

      !> Each complete record is written to the file as it is built.
      call history_file%record_write( record_csv )
      if ( .not. history_file%is_success() ) then
        if (present(is_success)) is_success = .FALSE.
        call history_file%close()
        return
      end if

    end do INDS

    !> Once all individuals are saved, the file is closed.
    call history_file%close()
    if ( .not. history_file%is_success() ) then
      if (present(is_success)) is_success = .FALSE.
    else
      if (present(is_success)) is_success = .TRUE.
    end if

    !> The CSV output data file can be optionally compressed with the
    !! commondata::cmd_zip_output command if commondata::is_zip_outputs is set
    !! to TRUE.
    if ( IS_ZIP_OUTPUTS ) then
      call call_external(command=CMD_ZIP_OUTPUT // " " // csv_file_name,      &
                         suppress_output=.TRUE.,                              &
                         is_background_task=ZIP_OUTPUTS_BACKGROUND )
    end if

  end subroutine population_save_data_behaviours

  !-----------------------------------------------------------------------------
  !> Calculate fitness for the pre-evolution phase of the genetic algorithm.
  !! **Pre-evolution** is based on selection for a simple criterion without
  !! explicit reproduction etc. The criterion for selection at this phase
  !! is set by the integer the_individual::individual_agent::fitness component.
  !! This procedure provides a whole-population wrapper for the
  !! the_individual::individual_agent::fitness_calc() function.
  !! @warning Note that fitness here is actually an inverse of the fitness: the
  !!          higher its value, the worse fitting is the agent.
  pure subroutine population_preevol_fitness_calc(this)
    class(POPULATION), intent(inout) :: this

    call this%individual%fitness_calc()

  end subroutine population_preevol_fitness_calc

  !-----------------------------------------------------------------------------
  !> Determine the number of parents that have fitness higher than the
  !! minimum acceptable value.
  !! @note This procedure is used in the fixed explicit fitness genetic
  !!       algorithm, see the_evolution::generations_loop_ga().
  pure function population_ga_reproduce_max (this) result (val_out)
    class(POPULATION), intent(in) :: this
    integer :: val_out

    !> - `MIN_FITNESS` is the normal limit of the fitness value for inclusion
    !!    into the reproducing elite group. See also
    !!    the_individual::individual_agent::individual_preevol_fitness_calc().
    integer, parameter :: MIN_FITNESS = GA_FITNESS_SELECT

    !> - `MIN_GA_REPRODUCE` is the minimum GA_REPRODUCE, the final value
    !!    cannot be smaller than. It is set as the minimum proportion
    !!    commondata::ga_reproduce_min_prop of the commondata::popsize.
    !!    However, it cannot be smaller than the absolute minimum
    !!    commondata::ga_reproduce_n_min.
    !! .
    integer, parameter :: MIN_GA_REPRODUCE =                                  &
                      max( GA_REPRODUCE_N_MIN, GA_REPRODUCE_MIN_PROP*POPSIZE )

    val_out = within( count( this%individual%fitness < MIN_FITNESS            &
                               .and. this%individual%fitness >= 0  ),         &
                      MIN_GA_REPRODUCE, GA_REPRODUCE_N  )

  end function population_ga_reproduce_max

  !-----------------------------------------------------------------------------
  !> This function implements adaptive mutation rate that *increases*
  !! as the population size *reduces*.
  function population_ga_mutation_rate_adaptive(this, baseline, maxvalue)     &
                                                        result (mutat_rate_out)
    class(POPULATION), intent(in) :: this
    !> @param[in] baseline baseline mutation rate
    real(SRP), intent(in) :: baseline
    !> @param[in] maxvalue maximum mutation rate
    real(SRP), optional, intent(in) :: maxvalue
    !> @return The adjusted adaptive mutation rate.
    real(SRP) :: mutat_rate_out

    !> ### Implementation notes ###
    !> #### Notable variables and parameters ####
    !> - `mutationrate_max` -- the maximum limit to the mutation rate. Can be
    !!    obtained from the optional parameter `maxvalue`. The function
    !!    returns its value at the lowest population size.
    !!     @remark It is actually local copy of optional `maxvalue` parameter.
    real(SRP) :: mutationrate_max

    !> - `MUTATIONRATE_MAX_DEF` -- the default maximum limit to the mutation
    !!    rate `mutationrate_max` if `maxvalue` is not provided.
    real(SRP), parameter :: MUTATIONRATE_MAX_DEF = 0.4_SRP

    real(SRP) :: step

    !> - `n_base_point` -- this is the base value for calculation of the
    !!    adaptive mutation rate. It can be the number of agents alive or
    !!    the number of agents that have grown.
    integer :: n_base_point

    !> - `mutation_grid_abscissa` and `mutation_grid_ordinate` are the arrays
    !!    that define the interpolation grid for the adaptive mutation rate.
    real(SRP), dimension(3) :: mutation_grid_abscissa, mutation_grid_ordinate

    !> - `MIN_GROWING` a parameter setting the minimum number of growing
    !!    agents in the population. If their actual number is smaller,
    !!    the mutation rate further incremented by a factor set by the
    !!    parameter
    integer, parameter :: MIN_GROWING = 4

    !> - `NON_GROW_INCREMENT` is an increment factor for the mutation rate
    !!    in case the number of growing agents is below the lower limit
    !!    `MIN_GROWING`.
    !! .
    real(SRP), parameter :: NON_GROW_INCREMENT = 1.3_SRP

    if (present(maxvalue)) then
      mutationrate_max = maxvalue
    else
      mutationrate_max = MUTATIONRATE_MAX_DEF
    end if

    !> #### Procedure ####
    !> First, calculate the base point `n_base_point`. This value is the
    !! base for the calculation of the adaptive mutation rate.
    !! - If this number reduces, mutation rate increases up to
    !!  `mutationrate_max`
    !! .
    !! The `n_base_point` can be:
    !! - Number of agents that are **alive**:
    !!       n_base_point = count( this%individual%is_alive() )
    !! - Number of agents that have **grown**:
    !!       n_base_point = count( this%individual%get_mass() >               &
    !!                             this%individual%get_mass_birth() )
    !! .
    n_base_point = count( this%individual%is_alive() )
    !n_base_point = count( this%individual%get_mass() >                       &
    !                      this%individual%get_mass_birth() )

    step = ( mutationrate_max - baseline ) / 4.0_SRP

    !> - If the mutation rate is based on the number of alive agents, the grid
    !!   abscissa `mutation_grid_abscissa` is an array with three
    !!   elements:
    !!   - minimum full population size
    !!   - 1/2 of the full population size commondata::popsize
    !!   - full commondata::popsize
    !!   .
    mutation_grid_abscissa = [ 0.0_SRP, POPSIZE/2.0_SRP, real(POPSIZE, SRP) ]

    !> - If, on the other hand, adaptive mutation rate is based on the number of
    !!   agents that have grown, abscissa is set to specific arbitrary numbers
    !!   that seem more or less optimal for the model performance.
    !! .
    !mutation_grid_abscissa = [ 0.0_SRP, 50.0_SRP, 100.0_SRP ]

    !> The grid ordinate is defined as
    !! - the maximum mutation rate limit `mutationrate_max`
    !! - a small middle value that is calculated as 1/4 of the range between
    !!   the maximum (`MUTATIOlNRATE_MAX`) and the minimum (`baseline`)
    !!   mutation values; the latter value increments the minimum `baseline`.
    !! - the lowest baseline value that is set by the `baseline` dummy
    !!   parameter.
    !! .
    mutation_grid_ordinate = [ mutationrate_max, baseline + step, baseline  ]

    !> Adaptive mutation rate is calculated based on the DDPINTERPOL()
    !! procedure with the grid array set by `mutation_grid_abscissa` and
    !! `mutation_grid_ordinate` and the interpolation value set by the
    !! number of agents that are the_genome::individual_genome::is_alive()
    !! in the population.
    !! An example pattern of the adaptive mutation rate function is plotted
    !! below. Here @f$ P_{max} @f$ is the maximum mutation rate defined by
    !! `mutationrate_max` and @f$ P_{b} @f$ is the baseline (normal, low)
    !! mutation rate defined by the `baseline` parameter.
    !! @image html img_doxygen_adapt_mutation.svg  "Adaptive mutation rate"
    !! @image latex img_doxygen_adapt_mutation.eps "Adaptive mutation rate" width=14cm
    ! plotting commands:
    ! htintrpl.exe [0 500 1000] [0.5 0.275 0.2]
    ! htintrpl.exe [0 500 1000] [0.5   0.2 0.1]
    mutat_rate_out = within(  DDPINTERPOL(                                    &
                                        mutation_grid_abscissa,               &
                                        mutation_grid_ordinate,               &
                                        real(n_base_point, SRP) ),            &
                              baseline,                                       &
                              mutationrate_max )

    !> If the number of agents that had grown from their birth mass is less
    !! than the minimum number (`MIN_GROWING`), mutation rate is incremented
    !! by a fixed factor `NON_GROW_INCREMENT`. However, it is still forced to
    !! be within the range `[ baseline, mutationrate_max ]`.
    if ( count( this%individual%get_mass() >                                  &
                this%individual%get_mass_birth() ) < MIN_GROWING ) then
      mutat_rate_out = within( mutat_rate_out * NON_GROW_INCREMENT,           &
                               baseline, mutationrate_max  )
    end if

    !> Interpolation plots can be saved in the @ref intro_debug_mode
    !! "debug mode" using this plotting command:
    !! commondata::debug_interpolate_plot_save().
    !! @warning Involves **huge** number of plots, should normally be
    !!          disabled.
    call debug_interpolate_plot_save(                                         &
            grid_xx=mutation_grid_abscissa, grid_yy=mutation_grid_ordinate,   &
            ipol_value=real(n_base_point, SRP), algstr="DDPINTERPOL",         &
            output_file="plot_debug_adaptive_mutation_rate_" // MMDD // "_g_" &
                        // TOSTR(Global_Generation_Number_Current) // PS )

  end function population_ga_mutation_rate_adaptive

end module THE_POPULATION
