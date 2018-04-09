!> @file m_indiv.f90
!! Definition of the individual agent in the AHA Model.
!! @author Sergey Budaev <sergey.budaev@uib.no>
!! @author Jarl Giske <jarl.giske@uib.no>
!! @date 2016-2017

!-------------------------------------------------------------------------------
! $Id$
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief   An umbrella module that collects all the components of the
!!          individual  agent.
!> @section the_individual_module THE_INDIVIDUAL module
!> Define the individual fish object and its properties and methods/functions
module THE_INDIVIDUAL

  use COMMONDATA      ! Global definitions of the model objects
  use THE_GENOME      ! ... and genome
  use THE_NEUROBIO
  use THE_BEHAVIOUR

  implicit none

  character (len=*), parameter, private :: MODNAME = "(THE_INDIVIDUAL)"

  !> @brief  This type describes parameters of the individual agent
  !! @details `INDIVIDUAL_AGENT` extends the neurobiological architecture type
  !!          by adding the alive  flag and explicit fitness.
  !!          Fitness can be either set or calculated or assessed
  !!          approximately. In models, it can be used in calculations
  !!          or used just for data output.
  !!          NOTE: procedure, final may not be implemented in all
  !!          compilers. E.g. gfortran issues this error prior to 4.9:
  !!          Error: Finalization at (1) is not yet implemented
  !! @note    `INDIVIDUAL_AGENT` is an umbrella object for the individual
  type, public, extends(ARCHITECTURE_NEURO) :: INDIVIDUAL_AGENT
    !> fitness is a fixed criterion for the evolution at the starting
    !! 'pre-evolution' phase of the genetic algorithm.
    !! @warning Note that fitness here is actually an "antifitness", the
    !!          higher its value, the **worse** fitting is the agent.
    integer :: fitness
    !> Individually specific mortality risk for various undefined or defined
    !! reasons, such as immune status. Note that this risk is not linked to
    !! the predation or habitat-specific factors. The latter two causes are
    !! treated separately.
    real(SRP) :: ind_mortality
    contains
      private
      !> Generate a random agent from the genotype.
      !! See `the_individual::individual_init_random()`.
      procedure, public :: init => individual_init_random
      !> Generate a new *empty* agent.
      !! See `the_individual::individual_create_zero()`.
      procedure, public :: create_ind => individual_create_zero
      !> Set the individual to be **dead**. This method overrides the
      !! the_genome::individual_genome::dies() method, nullifying all
      !! reproductive and neurobiological and behavioural objects.
      !! However, this function does not deallocate the individual
      !! agent object, this may be a separate destructor function.
      !! The `dies` method is implemented at the following levels
      !! of the agent object hierarchy (upper overrides the lower level):
      !! - the_genome::individual_genome::dies();
      !! - the_neurobio::appraisal::dies();
      !! - the_neurobio::gos_global::dies();
      !! - the_individual::individual_agent::dies().
      !! .
      !! See `the_individual::individual_agent_set_dead()`.
      procedure, public ::  dies => individual_agent_set_dead
      !> Get the individually-specific mortality risk for the agent.
      !! See `the_individual::individual_get_risk_mortality_individual()`.
      procedure, public :: get_mortality =>                                   &
                                    individual_get_risk_mortality_individual
      !> Calculate fitness for the pre-evolution phase of the genetic algorithm.
      !! Pre-evolution is based on selection for a simple criterion without
      !! explicit reproduction etc. The criterion for selection at this phase
      !! is set by the integer the_individual::individual_agent::fitness
      !! component. See `the_individual::individual_preevol_fitness_calc()`.
      procedure, public :: fitness_calc => individual_preevol_fitness_calc
  end type INDIVIDUAL_AGENT

  ! we never use the actual proc names, only object-bound
  private :: individual_init_random

contains ! ........ implementation of procedures for this level ................

  !-----------------------------------------------------------------------------
  !> @brief   Generate a random agent from the genotype.
  !! @details This subroutine is used to initialise the individual agent with
  !!          random data from the genotype. Its use is the most natural for
  !!          the initialisation of a population of agents. The init procedure
  !!          calls the init functions for the lower order layers of the class
  !!          hierarchy (genome, hormones, neurobio), and sets values.
  subroutine individual_init_random(this, exclude_genome)
    class(INDIVIDUAL_AGENT), intent(inout) :: this
    !> @param[in] exclude_genome is a logical flag to exclude initialisation
    !!            of random genome. If absent, assumed FALSE.
    !!              @note Only the first generation is initialised with random
    !!                    genome (exclude_genome = FALSE), all subsequent
    !!                    generations use pre-existing genomes from the
    !!                    ancestors (exclude_genome = TRUE) randomised by
    !!                    crossover in the genetic algorithm.
    logical, optional, intent(in) :: exclude_genome

    !> We first initialise all the components of the agent down the class
    !! hierarchy: from individual genome to the neurobiological architecture.
    if (present(exclude_genome)) then
      if (exclude_genome .eqv. .FALSE.) then
        call this%init_genome()   ! initialise **random** genome
      end if
    else
      call this%init_genome()     ! initialise **random** genome
    end if

    !> Clean the spatial location history stack of the agent.
    call this%spatial_history_clean()

    call this%init_hormones()     !> initialise hormone objects **from genome**
    call this%init_condition()    !> initialise condition **from genome**
    call this%init_reproduction() !> initialise empty reproduction objects
    call this%init_neurobio()     !> initialise empty neuro objects

    !> Finally, we bring the agent to life by setting alive boolean flag
    call this%lives()

    !> Set the individually specific mortality risk initially as a Gaussian
    !! variable with mean commondata::individual_mortality_risk_def and CV
    !! commondata::individual_mortality_risk_cv. There is also a restriction
    !! that the risk of mortality should never be smaller than
    !! commondata::zero.
    this%ind_mortality =                                                      &
              max( ZERO, RNORM( INDIVIDUAL_MORTALITY_RISK_DEF,                &
                                cv2variance( INDIVIDUAL_MORTALITY_RISK_CV,    &
                                             INDIVIDUAL_MORTALITY_RISK_DEF) ) )

    !> Calculate the initial value of fitness for pre-evolution stage.
    call this%fitness_calc()

  end subroutine individual_init_random

  !-----------------------------------------------------------------------------
  !> @brief   Generate a new empty agent.
  !! @details This subroutine is used to create a new empty individual agent.
  !!          It should be used to make newborn agents that inherit traits
  !!          from their parents.
  !! @warning This procedure is not used so far and is candidate for being
  !!          deprecated. Not clear if necessary.
  subroutine individual_create_zero(this)
    class(INDIVIDUAL_AGENT), intent(inout) :: this

    !> Clean the spatial location history stack of the agent.
    call this%spatial_history_clean()

    call this%create_genome()               !> create **empty** genome

    !> Create empty hormone objects. No genome-based initialisation is done.
    call this%hormone_history_clean()
    call this%growhorm_set(MISSING)
    call this%thyroid_set(MISSING)
    call this%adrenaline_set(MISSING)
    call this%cortisol_set(MISSING)
    call this%testosterone_set(MISSING, .FALSE.)
    call this%estrogen_set(MISSING, .FALSE.)

    !> Empty condition objects. Clean history stack.
    !  @warning: energy_current, control_unselected and smr that are set
    !            from the genome are not initialised so far due to absence of
    !            "setter" methods: it is not clear if this procedure is ever
    !            necessary at all (may be obsolete).
    call this%body_history_clean()
    call this%set_mass(MISSING, .FALSE.)
    call this%set_length(MISSING, .FALSE.)

    !> Initialise reproduction objects and neurobiology to a zero state.
    call this%init_reproduction()
    call this%init_neurobio()

    !> Finally, we bring the agent to life by setting alive boolean flag
    call this%lives()

    this%ind_mortality = MISSING

    !> Calculate the initial value of fitness for pre-evolution stage.
    this%fitness = MISSING

  end subroutine individual_create_zero

  !-----------------------------------------------------------------------------
  !> Set the individual to be **dead**. Note that this function does not
  !! deallocate the individual agent object, this may be a separate destructor
  !! function.
  !!
  !! The `dies` method is implemented at the following levels
  !! of the agent object hierarchy (upper overrides the lower level):
  !! - the_genome::individual_genome::dies();
  !! - the_neurobio::appraisal::dies();
  !! - the_neurobio::gos_global::dies();
  !! - the_individual::individual_agent::dies().
  !! .
  !! @note This method overrides the the_genome::individual_genome::dies()
  !!       method, nullifying all reproductive and neurobiological and
  !!       behavioural objects.
  elemental subroutine individual_agent_set_dead(this)
    class(INDIVIDUAL_AGENT), intent(inout) :: this

    call this%set_dead()          !> - Set the agent "dead";
    call this%init_reproduction() !> - emptify reproduction objects;
    call this%init_neurobio()     !> - emptify neurobiological objects.
                                  !> .
  end subroutine individual_agent_set_dead

  !-----------------------------------------------------------------------------
  !> Finalisation procedure. Note that finalisation of objects may
  !! not yet be implemented in the compiler. Therefore this subroutine
  !! is not used so far, just a stub.
  subroutine kill_destroy(this)
    class(INDIVIDUAL_AGENT), intent(inout) :: this

    call this%dies()

    ! call deallocation etc when final procedures are implemented
    ! so far it is only a stub.

  end subroutine kill_destroy

  !-----------------------------------------------------------------------------
  !> Get the individually-specific mortality risk for the agent.
  elemental function individual_get_risk_mortality_individual(this)           &
                                                              result (get_val)
    class(INDIVIDUAL_AGENT), intent(in) :: this
    !> @return Individually-specific mortality risk for the agent.
    real(SRP) :: get_val

    get_val = this%ind_mortality

  end function individual_get_risk_mortality_individual

  !-----------------------------------------------------------------------------
  !> Calculate fitness for the pre-evolution phase of the genetic algorithm.
  !! Pre-evolution is based on selection for a simple criterion without
  !! explicit reproduction etc. The criterion for selection at this phase
  !! is set by the integer the_individual::individual_agent::fitness component.
  !! @warning Note that fitness here is actually an "antifitness", the
  !!          higher its value, the **worse** fitting is the agent.
  elemental subroutine individual_preevol_fitness_calc(this)
    class(INDIVIDUAL_AGENT), intent(inout) :: this

    !> `INT_FITNESS_DEAD` is the fitness ascribed to the dead agent, it should
    !!  be a fairly large value greater than for any alive.
    integer, parameter :: INT_FITNESS_DEAD = GA_FITNESS_DEAD

    !> First check if the agent is dead. If so, give very high value that
    !! is never selected.
    if ( this%is_dead() ) then
      this%fitness = INT_FITNESS_DEAD
      return
    end if

    !> Now, the reverse of fitness can be calculated by various methods.
    !! Specific calculation functions are implemented within this function.
    !! So far the following routines were implemented:
    !! - ::fitness_birth_mass_ratio()
    !! - ::fitness_stomach_mass_ratio()
    !! - ::fitness_stomach_mass_abs()
    !! - ::fitness_food_mass_sum()
    !! - ::fitness_mass_incr_ratio()
    !! - ::fitness_mass_incr_abs()
    !! - ::fitness_reprod_factor()
    !! .
    this%fitness = fitness_mass_incr_abs()

    contains !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !> Fitness is calculated as the ratio of the birth mass to the current
      !! mass. This value is weighted by the multiplier (1000) to get a fairly
      !! large integer (so decimals are unimportant) and also weighted by the
      !! number of food items eaten and the number of offspring produced.
      !! @note This procedure is internal to
      !!       the_individual::individual_agent::fitness_calc().
      elemental function fitness_birth_mass_ratio() result (fitness_out)
        !> @return Fitness value.
        integer :: fitness_out
        !> `INT_MULTIPLIER_FITNESS` is a multiplier to set the appropriate
        !! scaling for the initial the_individual::individual_agent::fitness.
        real, parameter :: INT_MULTIPLIER_FITNESS = 1000.0_SRP
        !> `INT_WEIGHT_FEEDING` is an integer weight given to the any non-zero
        !! successful feeding.
        integer, parameter :: INT_WEIGHT_FEEDING = 80
        !> `INT_WEIGHT_OFFSPRING` is an integer weight given to the any
        !! non-zero successful reproductions
        integer, parameter :: INT_WEIGHT_OFFSPRING = 400
        !> Initial fitness is the ratio of the birth mass to the current mass
        !! weighted by the `INT_MULTIPLIER_FITNESS`.
        fitness_out = nint( INT_MULTIPLIER_FITNESS *                         &
                                ( this%get_mass_birth() / this%get_mass() ) )
        !> If the agent successfully caught and eaten any number of food items,
        !! its fitness is divided by the number of food items eaten weighted by
        !! the `INT_WEIGHT_FEEDING` parameter.
        if (this%n_eaten_indicator>0) fitness_out = fitness_out /            &
                                ( this%n_eaten_indicator * INT_WEIGHT_FEEDING )
        !> If the agent has successfully done any reproduction, its fitness is
        !! divided by the number of offspring weighted by `INT_WEIGHT_OFFSPRING`.
        !  @note wxMaxima quick plot:
        !        `wxplot3d ( (40/m*1000)/(o*10)  , [m,30,100], [o,1,10]);`
        !        Gnuplot code:
        !        @verbatim
        !          set xrange [30:100]
        !          set yrange [1:100]
        !          set xlabel "mass"
        !          set xlabel "offspring"
        !          splot (40/x*1000)/(y*10) with lines
        !        @endverbatim
        if (this%get_offspring()>0) fitness_out = fitness_out /               &
                                ( this%get_offspring() * INT_WEIGHT_OFFSPRING )
      end function fitness_birth_mass_ratio

      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !> Fitness is calculated as the ratio of the body mass to the stomach
      !! content
      !! @note This procedure is internal to
      !!       the_individual::individual_agent::fitness_calc().
      elemental function fitness_stomach_mass_ratio() result (fitness_out)
        !> @return Fitness value.
        integer :: fitness_out

        !> `INT_MULTIPLIER_FITNESS` is a multiplier to set the appropriate
        !! scaling for the initial the_individual::individual_agent::fitness.
        real, parameter :: INT_MULTIPLIER_FITNESS = 10.0_SRP

        if (is_near_zero(this%get_stom_content(), TOLERANCE_LOW_DEF_SRP)) then
          fitness_out = 2000
          return
        end if

        fitness_out = nint( INT_MULTIPLIER_FITNESS *                          &
                                    this%get_mass() / this%get_stom_content() )

      end function fitness_stomach_mass_ratio

      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !> Fitness is calculated as the absolute stomach content.
      !! @note This procedure is internal to
      !!       the_individual::individual_agent::fitness_calc().
      elemental function fitness_stomach_mass_abs() result (fitness_out)
        !> @return Fitness value.
        integer :: fitness_out

        !> `INT_MULTIPLIER_FITNESS` is a multiplier to set the appropriate
        !! scaling for the initial the_individual::individual_agent::fitness.
        real, parameter :: INT_MULTIPLIER_FITNESS = 100.0_SRP

        if (is_near_zero(this%get_stom_content(), TOLERANCE_LOW_DEF_SRP)) then
          fitness_out = 2000
          return
        end if

        fitness_out = nint( INT_MULTIPLIER_FITNESS *                          &
                                          1.0_SRP / this%get_stom_content() )

      end function fitness_stomach_mass_abs

      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !> Fitness is calculated as the cumulative mass of all food objects eaten.
      !! @note This procedure is internal to
      !!       the_individual::individual_agent::fitness_calc().
      elemental function fitness_food_mass_sum() result (fitness_out)
        !> @return Fitness value.
        integer :: fitness_out

        !> `INT_MULTIPLIER_FITNESS` is a multiplier to set the appropriate
        !! scaling for the initial the_individual::individual_agent::fitness.
        real, parameter :: INT_MULTIPLIER_FITNESS = 10000.0_SRP

        if (is_near_zero(this%mass_eaten_indicator, TOLERANCE_LOW_DEF_SRP)) then
          fitness_out = 1000
          return
        end if

        fitness_out = nint( INT_MULTIPLIER_FITNESS *                          &
                                        1.0_SRP / this%mass_eaten_indicator )

      end function fitness_food_mass_sum

      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !> Fitness is calculated as the mass increment in units of birth mass.
      !! @note This procedure is internal to
      !!       the_individual::individual_agent::fitness_calc().
      elemental function fitness_mass_incr_ratio() result (fitness_out)
        !> @return Fitness value.
        integer :: fitness_out

        !> `INT_MULTIPLIER_FITNESS` is a multiplier to set the appropriate
        !! scaling for the initial the_individual::individual_agent::fitness.
        real, parameter :: INT_MULTIPLIER_FITNESS = 10.0_SRP

        if (this%get_mass() <= this%get_mass_birth()) then
          fitness_out = 1000 - this%n_eaten_indicator
          return
        end if

        fitness_out = nint( INT_MULTIPLIER_FITNESS *                          &
            this%get_mass_birth() / (this%get_mass() - this%get_mass_birth()) )

      end function fitness_mass_incr_ratio

      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !> Fitness is calculated as absolute mass increment from the birth mass.
      !! @note This procedure is internal to
      !!       the_individual::individual_agent::fitness_calc().
      elemental function fitness_mass_incr_abs() result (fitness_out)
        !> @return Fitness value.
        integer :: fitness_out

        !> `INT_MULTIPLIER_FITNESS` is a multiplier to set the appropriate
        !! scaling for the initial the_individual::individual_agent::fitness.
        real, parameter :: INT_MULTIPLIER_FITNESS = 10.0_SRP

        if (this%get_mass() <= this%get_mass_birth()) then
          fitness_out = 1000 - this%n_eaten_indicator
          return
        end if

        fitness_out = nint( INT_MULTIPLIER_FITNESS *                          &
                        1.0_SRP / (this%get_mass() - this%get_mass_birth()) )

      end function fitness_mass_incr_abs

      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !> Fitness as the reproductive factor.
      !! @note This procedure is internal to
      !!       the_individual::individual_agent::fitness_calc().
      elemental function fitness_reprod_factor() result (fitness_out)
        !> @return Fitness value.
        integer :: fitness_out

        !> `INT_MULTIPLIER_FITNESS` is a multiplier to set the appropriate
        !! scaling for the initial the_individual::individual_agent::fitness.
        real, parameter :: INT_MULTIPLIER_FITNESS = 100.0_SRP

        if (this%get_mass() <= this%get_mass_birth()) then
          fitness_out = 1000 - this%n_eaten_indicator
          return
        end if

        fitness_out =nint( INT_MULTIPLIER_FITNESS *                          &
                                        1.0_SRP / this%reproductive_factor() )

      end function fitness_reprod_factor


  end subroutine individual_preevol_fitness_calc

end module THE_INDIVIDUAL
