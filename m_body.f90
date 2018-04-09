!> @file m_body.f90
!! The Body condition and architecture of the AHA Model.
!! @author Sergey Budaev <sergey.budaev@uib.no>
!! @author Jarl Giske <jarl.giske@uib.no>
!! @date 2016-2017

!-------------------------------------------------------------------------------
! $Id$
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief Definition the physical properties and condition of the agent
!> @section the_genome_module THE_BODY module
!> This module defines various physical properties of the agent, such as the
!! body size, body mass etc, as well as the condition and basic physiological
!! variables.
!! @note Note that the agent has the size property but is nonetheless
!!       represented as a single commondata::spatial point for simplicity.
module THE_BODY
  use COMMONDATA
  use THE_ENVIRONMENT
  use THE_HORMONES

  implicit none

  character (len=*), parameter, private :: MODNAME = "(THE_CONDITION)"

  !> `CONDITION` defines the physical condition of the agent
  type, public, extends(HORMONES) :: CONDITION
    !> The age of the agent in units of the integer time steps.
    integer :: age
    !> Current energy reserves, initialised non-genetically, Gaussian.
    real(SRP) :: energy_current
    !> Maximum historical energy reserves.
    real(SRP) :: energy_maximum
    !> Energy reserves at birth, non-genetic, Gaussian.
    real(SRP) :: energy_birth
    !> current body length, initialised non-genetically, Gaussian, will grow.
    real(SRP) :: body_length
    !> History stack for the body length.
    real(SRP), dimension(HISTORY_SIZE_AGENT_PROP) :: body_length_history
    !> Body length at birth (genetically fixed), it is not used so far in the
    !! calculations but is recorded and can be output.
    real(SRP) :: body_length_birth
    !> This is a **control unselected** (and unused) trait that is set from
    !! the genome as normal but is not used in any calculations. It can be
    !! used as a control marker for random genetic drift.
    real(SRP) :: control_unselected
    !> Current body mass, initialised calculated from length and energy
    !! reserves.
    real(SRP) :: body_mass
    !> History stack for body mass.
    real(SRP), dimension(HISTORY_SIZE_AGENT_PROP) :: body_mass_history
    !> Body mass at birth, will keep record of it.
    real(SRP) :: body_mass_birth
    !> Maximum historically body, will keep record of it.
    real(SRP) :: body_mass_maximum
    !> Standard metabolitic rate, can change depending on hormones and
    !! psychological state (GOS)), at birth initialised from the genome.
    real(SRP) :: smr
    !> Maximum stomach capacity, max. fraction of body mass available for food
    !! here set from default value. But can change in future versions of the
    !! model depending on the body length and the physiological state (so
    !! specifically set for each agent rather than defined from global
    !! parameter commondata::max_stomach_capacity_def).
    !! @note  In the old model the stomach content cannot surpass
    !!        maxstomcap=15% of agent's body mass.
    real(SRP) :: maxstomcap = MAX_STOMACH_CAPACITY_DEF
    !> Stomach content mass.
    real(SRP) :: stomach_content_mass
    ! @warning The functions defining the growth is a quick and dirty
    !          solution.
    contains
      private
      !> Initialise the individual body condition object based on the
      !! genome values.
      !! See `the_body::condition_init_genotype()`
      procedure, public :: init_condition => condition_init_genotype
      !> This procedure enforces selective mortality of agents at birth.
      !! See `the_body::birth_mortality_enforce_init_fixed_debug()`.
      procedure, public :: mortality_birth =>                                 &
                                    birth_mortality_enforce_init_fixed_debug
      !> Cleanup the history stack of the body length and mass.
      !! See `the_body::condition_clean_history()`
      procedure, public :: body_history_clean => condition_clean_history

      !> Get current age.
      !! See `the_body::condition_age_get()`
      procedure, public :: get_age => condition_age_get
      !> Reset the age of the agent to zero.
      !! See `the_body::condition_age_reset_zero()`.
      procedure, public :: age_reset => condition_age_reset_zero
      !> Get current energy reserves.
      !! See `the_body::condition_energy_current_get()`
      procedure, public :: get_energy => condition_energy_current_get
      !> Get historical maximum of energy reserves.
      !! See `the_body::condition_energy_maximum_get()`
      procedure, public :: get_energy_max => condition_energy_maximum_get
      !> Get current body length.
      !! See `the_body::condition_body_length_get()`
      procedure, public :: get_length => condition_body_length_get
        !> Generic interface (alias) for `get_length`.
        generic, public :: length => get_length
      !> Get current value of the control unselected trait.
      !! See `the_body:condition_control_unsel_get:()`
      procedure, public :: get_control_unselected => condition_control_unsel_get
      !> Get current body mass.
      !! See `the_body::condition_body_mass_get()`
      procedure, public :: get_mass => condition_body_mass_get
        !> Generic interface to get_mass.
        generic, public :: mass => get_mass
      !> Get historical record of energy reserves at birth.
      !! See `the_body::condition_energy_birth_get()`.
      procedure, public :: get_energ_birth => condition_energy_birth_get
      !> Get historical record of body length at birth.
      !! See `the_body::condition_body_length_birth_get()`
      procedure, public :: get_length_birth => condition_body_length_birth_get
      !> Get historical record of body mass at birth.
      !! See `the_body::condition_body_mass_birth_get()`
      procedure, public :: get_mass_birth => condition_body_mass_birth_get
      !> Get historcal maximum for body mass.
      !! See `the_body::condition_body_mass_max_get()`
      procedure, public :: get_mass_max => condition_body_mass_max_get
      !> Get current smr.
      !! See `the_body::condition_smr_get()`
      procedure, public :: get_smr => condition_smr_get
      !> Get current stomach content.
      !! See `the_body::condition_stomach_content_get()`
      procedure, public :: get_stom_content => condition_stomach_content_get

      !> Increment the age of the agent by one.
      !! See `the_body::condition_age_increment()`.
      procedure, public :: age_increment => condition_age_increment
      !> Set body mass optionally updating the history stack.
      !! See `the_body::condition_body_mass_set_update_hist()`
      procedure, public :: set_mass => condition_body_mass_set_update_hist
      !> Set body length optionally updating the history stack.
      !! See `the_body::condition_body_length_set_update_hist()`
      procedure, public :: set_length => condition_body_length_set_update_hist
      !> Calculate the visibility range of this agent. Visibility depends on
      !! the size of the agent, ambient illumination and agent contrast.
      !! Visibility is the distance from which this agent can be seen by a
      !! visual object (e.g. predator or conspecific).
      !! See `the_body::condition_agent_visibility_visual_range`.
      procedure, public :: visibility => condition_agent_visibility_visual_range

      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ! @note `food_process_cost` can be calculated several times per time step.
      !> Calculate the basic processing cost of catching a food item
      !!          with the mass `food_gain`. Vector-based procedure.
      !! See `the_body::body_mass_processing_cost_calc_v()`
      procedure, public :: food_proc_cost_v => body_mass_processing_cost_calc_v
      !> Calculate the basic processing cost of catching a food item
      !! with the mass `food_gain`. Object-based procedure.
      !! See `the_body::body_mass_processing_cost_calc_o()`
      procedure, public :: food_proc_cost_o => body_mass_processing_cost_calc_o
        !> Generic interface to  the procedures calculating the basic
        !! processing cost of catching a food item.
        generic, public :: food_process_cost => food_proc_cost_v,             &
                                                food_proc_cost_o

      !> Calculate the value of possible food gain as fitting into the agent's
      !! stomach (or full gain if the food item fits wholly). Vector-based.
      !! See `the_body::stomach_content_food_gain_fitting_v()`
      procedure, public :: food_fitt_v => stomach_content_food_gain_fitting_v
      !> Calculate the value of possible food gain as fitting into the agent's
      !! stomach (or full gain if the food item fits wholly). Object-based.
      !! See `the_body::stomach_content_food_gain_fitting_o()`
      procedure, public :: food_fitt_o => stomach_content_food_gain_fitting_o
        !> Generic interface to procedures that calculate the value of
        !! possible food gain as fitting into the agent's stomach (or
        !! full gain if the food item fits wholly).
        !! See `the_body::stomach_content_food_gain_fitting_v()` and
        !! `the_body::stomach_content_food_gain_fitting_o()`.
        generic, public :: food_fitting => food_fitt_v, food_fitt_o

      !> Calculate extra food surplus mass non fitting into the stomach of the
      !! agent. Vector-based.
      !! See `the_body::stomach_content_food_gain_non_fit_v()`
      procedure, public :: food_surpl_v => stomach_content_food_gain_non_fit_v
      !> Calculate extra food surplus mass non fitting into the stomach of the
      !! agent. Object-based.
      !! See `the_body::stomach_content_food_gain_non_fit_o()`
      procedure, public :: food_surpl_o => stomach_content_food_gain_non_fit_o
        !> Generic interface to procedures that calculate extra food surplus
        !! mass non fitting into the stomach of the agent.
        generic, public :: food_surplus => food_surpl_v, food_surpl_o

      !> Do grow body mass based on food gain from a single food item adjusted
      !! for cost etc.
      !! See `the_body::body_mass_grow_do_calculate()`
      procedure, public :: mass_grow => body_mass_grow_do_calculate
      !> Do increment stomach contents with adjusted (fitted) value.
      !! See `the_body::stomach_content_get_increment()`
      procedure, public :: stomach_increment => stomach_content_get_increment

      !> The fraction of the cost of the processing of the food item(s)
      !! depending on the agent SMR. It is scaled in terms of the ratio of
      !! the food item mass to the agent mass.
      !! See `the_body::body_mass_food_processing_cost_factor_smr()`
      procedure, public :: cost_factor_food_smr =>                            &
                                      body_mass_food_processing_cost_factor_smr
      !> The cost of swimming of a specific distance in terms of body mass loss.
      !! See `the_body::condition_cost_swimming_burst()`
      procedure, public :: cost_swim => condition_cost_swimming_burst
      !> The standard cost of swimming is a diagnostic function that shows
      !! the cost, in units of the body mass, incurred if the agent passes a
      !! distance equal to commondata::lifespan units of its body length.
      !! See `the_body::cost_swimming_standard()`.
      procedure, public :: cost_swim_std => cost_swimming_standard

      !> Update the energy reserves of the agent based on its current mass and
      !! length.
      !! See `the_body::condition_energy_update_after_growth()`
      procedure, public :: energy_update => condition_energy_update_after_growth
      !> Check if the body mass is smaller than the birth body mass or
      !! structural body mass.
      !! See `the_body::body_mass_is_starvation_check()`
      procedure, public :: starved_death => body_mass_is_starvation_check

      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ! @note Procedures that are calculated at the end of the time step
      !       of the model.
      !> Calculate the cost of living for a single model step.
      !! See `the_body::body_mass_calculate_cost_living_step()`
      procedure, public :: living_cost => body_mass_calculate_cost_living_step
      !> Adjust the body mass at the end of the model step against the
      !! cost of living.
      !! See `the_body::body_mass_adjust_living_cost_step()`
      procedure, public :: subtract_living_cost => body_mass_adjust_living_cost_step
      !> Calculate body length increment.
      !! See `the_body::body_len_grow_calculate_increment_step()`
      procedure, public :: len_incr => body_len_grow_calculate_increment_step
      !> Do linear growth for one model step based on the above
      !! increment function.
      !! See `the_body::body_len_grow_do_calculate_step()`
      procedure, public :: len_grow => body_len_grow_do_calculate_step
      !> Do digestion. Stomach contents S(t) is emptied by a constant fraction
      !! each time step. See details in the `stomach_content_mass_emptify_step`
      !! function call.
      !! See `the_body::stomach_content_mass_emptify_step()`
      procedure, public :: stomach_empify => stomach_content_mass_emptify_step
      !> Update the level of the sex steroids. Sex steroids are incremented
      !! each time step of the model.
      !! See `the_body::sex_steroids_update_increment()`
      procedure, public :: sex_steroids_update => sex_steroids_update_increment

  end type CONDITION

  !> `REPRODUCTION` type defines parameters of the reproduction system.
  type, public, extends(CONDITION) :: REPRODUCTION
    !> Total number of reproductions during the lifespan.
    integer :: n_reproductions
    !> Total number of offspring reproduced during the lifespan.
    integer :: n_offspring
    contains
      !> Init reproduction class.
      !! See `the_body::reproduction_init_zero()`.
      procedure, public :: init_reproduction => reproduction_init_zero

      !> Determine if the agent's hormonal system is ready for reproduction
      !! See `the_body::reproduction_ready_steroid_hormones_exceed()`.
      procedure, public :: is_ready_reproduce =>                              &
                                      reproduction_ready_steroid_hormones_exceed

      !> Get the number of reproductions for this agent.
      !! See `the_body::reproduction_n_reproductions_get()`.
      procedure, public :: get_reproductions => reproduction_n_reproductions_get
      !> Set the number of reproductions for the agent.
      !! See `the_body::reproduction_n_reproductions_set()`.
      procedure, public :: reproductions_set => reproduction_n_reproductions_set
      !> Get the number of offspring for this agent for its lifespan.
      !! See `the_body::reproduction_n_offspring_get()`.
      procedure, public :: get_offspring => reproduction_n_offspring_get
      !> Set the number of offspring this agent had during its lifespan.
      !! See `the_body::reproduction_n_offspring_set()`.
      procedure, public :: offspring_set => reproduction_n_offspring_set
      !> Increment the number of reproductions and offspring for the agent.
      !! See `the_body::reproduction_n_increment()`.
      procedure, public :: reproductions_increment => reproduction_n_increment
      !> Calculate the number of offspring per a single reproduction.
      !! See `the_body::reproduction_n_offspring_calc()`.
      procedure, public :: offspring_number => reproduction_n_offspring_calc
      !> Calculate the total mass of all offspring per single reproduction.
      !! See `the_body::reproduction_mass_offspring_calc()`.
      procedure, public :: offspring_mass => reproduction_mass_offspring_calc
      !> Calculate the energetic cost of reproduction.
      !! @note Two versions are implemented:
      !!       - `the_body::reproduction_cost_energy_fix()`
      !!       - `the_body::reproduction_cost_energy_dynamic()`
      !!       .
      procedure, public :: reproduction_cost => reproduction_cost_energy_dynamic
      !> Calculate the costs of unsuccessful reproduction. This is calculated
      !! as a fraction of the normal cost of reproduction returned by the
      !! function `reproduction::reproduction_cost()`.
      !! See `the_body::reproduction_cost_unsuccessful_calc()`.
      procedure, public :: reproduction_cost_unsuccess =>                     &
                                            reproduction_cost_unsuccessful_calc
  end type REPRODUCTION

contains ! ........ implementation of procedures for this level ................

  !> This is the function to calculate the body weight from the length and
  !! the Fulton condition factor (energy reserves).
  !! @param[in] k, l condition and body length.
  !! @returns Body mass.
  elemental function length2mass(k, l) result (body_mass)
    real(SRP), intent(in)  :: k, l
    real(SRP)              :: body_mass

    !> ### Implementation details ###
    !! Body mass is non-genetic, length and initial condition factor are
    !! genetically determined. Body mass is calculated initially from
    !! the Fulton'scondition factor formula @f[ K=\frac{M}{L^{3}} , @f]
    !! i.e. @f[ M=K L^{3} . @f] The exponent can be non-cube for
    !! non-isometric growth.
    !! @note The "cube law" exponent (3.0 normally), might be redefined here
    !!       as the LINEAR_GROWTH_EXPONENT parameter constant.
    real(SRP), parameter   :: B = LINEAR_GROWTH_EXPONENT

    body_mass = k * l**B

  end function length2mass

  !-----------------------------------------------------------------------------
  !> Calculate the current energy reserves (Fulton condition factor) from body
  !! mass and length.
  !! @param[in] m, l body mass and body length.
  !! @returns energy reserve available.
  elemental function energy_reserve (m, l) result (k)

    real(SRP), intent(in) :: m, l
    real(SRP) :: k

    !> @note The "cube law" exponent (3.0 normally), might be redefined here
    !!       as the LINEAR_GROWTH_EXPONENT parameter constant.
    real(SRP), parameter   :: B = LINEAR_GROWTH_EXPONENT

    k = m / (l**B)

  end function energy_reserve

  !-----------------------------------------------------------------------------
  !> Initialise the individual body condition object based on the genome values.
  !! Two alleles are selected at random and input into the `gamma2gene`
  !! function to get the initial hormone values rescaled to 0:1. Note that
  !! the `gamma2gene` alleles defining the **shape** of the gamma function
  !! and the **half-max effect** are selected randomly in this version.
  !! Also, polyploid organisms are possible, in such case, two parameters
  !! are also randomly defined from a larger set (e.g. from four chromosomes
  !! in case of tetraploids). See implementation details and comments for
  !! each of the hormones.
  subroutine condition_init_genotype(this)
    class(CONDITION), intent(inout) :: this

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(condition_init_genotype)"

    !> ### Implementation details ###
    !> First, initialise all the physical condition components of the
    !! agent, starting from **age**: age=0 initially.
    this%age = 0

    !> The **energy reserves** are set as Gaussian with the mean
    !! commondata::energy_init and CV commondata::energy_gerror_cv.
    this%energy_current = RNORM( ENERGY_INIT,                                 &
                                 cv2variance( ENERGY_GERROR_CV,               &
                                           ENERGY_INIT ) )

    !> Set the birth energy reserves from the initial current value.
    this%energy_birth = this%energy_current

    !> Additionally, update the historical maximum energy value.
    this%energy_maximum = this%energy_current

    !> The **body length** is initialised as Gaussian with the mean
    !! commondata::body_length_init and cv commondata::body_length_gerror_cv.
    this%body_length = RNORM( BODY_LENGTH_INIT,                               &
                              cv2variance( BODY_LENGTH_GERROR_CV,             &
                                           BODY_LENGTH_INIT ) )

    !> @note Body length cannot be zero or less than the minimum possible
    !!       size that is defined by `BODY_LENGTH_MIN`.
    if (this%body_length < BODY_LENGTH_MIN) then
      call LOG_DBG (                                                          &
            "WARNING: Initialised body length " // TOSTR(this%body_length) // &
            " is smaller than the BODY_LENGTH_MIN in " // PROCNAME )
      this%body_length = BODY_LENGTH_MIN
    end if

    !> Also, body length at birth cannot reach the maximum value
    !! `BODY_LENGTH_MAX`, if it does occurs,  erroneous parameter value
    !! was set. This aberrant agent then the_genome::individual_genome::dies().
    if (this%body_length >= BODY_LENGTH_MAX) then
      call LOG_MSG(                                                           &
            "WARNING: Initialised body length " // TOSTR(this%body_length) // &
            " exceeds 1/10 BODY_LENGTH_MAX in " // PROCNAME )
      this%body_length = BODY_LENGTH_MAX / 10.0_SRP
      call this%dies()
    end if

    !> The historical body length at birth is saved as
    !! the_body::condition::body_length_birth.
    this%body_length_birth = this%body_length

    !> A **control unselected** trait is also set from the genome. This trait
    !! is not used in any calculations but serves as a control for random or
    !! nonrandom genetic drift.
    call this%trait_init(this%control_unselected,                             &
                    CONTROL_UNSELECTED_GENOTYPE_PHENOTYPE,                    &
                    CONTROL_UNSELECTED_INIT,                                  &
                    CONTROL_UNSELECTED_GERROR_CV, "CONTROL_UNSEL")

    !> The **body mass** is determined by the genetically determined energy
    !! reserves and the body length (using `length2mass` function). Thus,
    !! the body mass is **non-genetic**.
    this%body_mass = length2mass(this%energy_current, this%body_length)
    !> The historical body mass at birth and the maximum body mass ever
    !! achieved are saved.
    this%body_mass_birth = this%body_mass
    this%body_mass_maximum = this%body_mass

    !> **SMR** is set from the genome.
    call this%trait_init(this%smr,                                            &
                         SMR_GENOTYPE_PHENOTYPE,                              &
                         SMR_INIT, SMR_GERROR_CV, "SMR")

    !> However, it must never be lower than commondata::smr_min. Very low
    !! values are unrealistic and might crash model.
    if ( this%smr < SMR_MIN ) this%smr = SMR_MIN

    !> **Stomach contents** is initialised as a random Gaussian value, average,
    !! units of the body mass with `STOMACH_CONTENT_INIT` and coefficient
    !! of variation `STOMACH_CONTENT_INIT_CV`. Stomach contents also must
    !! always be above zero and never exceed the `maxstomcap` factor.
    this%stomach_content_mass                                                 &
          = min(                                                              &
                  max( ZERO, RNORM(this%body_mass*STOMACH_CONTENT_INIT,       &
                       (this%body_mass*STOMACH_CONTENT_INIT*                  &
                                            STOMACH_CONTENT_INIT_CV)**2)  ),  &
                  this%body_mass*this%maxstomcap  )

    !> Finally, the procedure initialises the history stacks for the body mass
    !! and length.
    call this%body_history_clean()

    !> And put the initial birth values of body length and mass into the
    !! history stack.
    !! @note The body length and mass history stack keeps the latest historical
    !!       values.
    call add_to_history(this%body_length_history, this%body_length)
    call add_to_history(this%body_mass_history, this%body_mass)

  end subroutine condition_init_genotype

  !-----------------------------------------------------------------------------
  !> This procedure enforces selective mortality of agents at birth to avoid
  !! strong selection for energy and length.
  !! @warning This is a debug version of the mortality procedure with fixed
  !!          mortality pattern, final should depend on the statistical
  !!          properties of the first generation, mean and sd.
  subroutine birth_mortality_enforce_init_fixed_debug(this)
    class(CONDITION), intent(inout) :: this

    ! htintrpl.exe [0.2 0.3 0.6 1.0] [0 0.006 0.1 1.0]
    ! htintrpl.exe [0.2 0.3 0.5 0.8] [0 0.006 0.1 1.0]
    real(SRP), dimension(*), parameter :: BIRTH_MORTALITY_ENERGY_ABSCISSA =   &
              [ 0.2_SRP, 0.3_SRP, 0.5_SRP, 0.8_SRP ]

    real(SRP), dimension(*), parameter :: BIRTH_MORTALITY_ENERGY_ORDINATE =   &
              [ 0.0_SRP, 0.006_SRP, 0.1_SRP, 1.0_SRP ]

    real(SRP) :: mortality

    mortality = within( DDPINTERPOL( BIRTH_MORTALITY_ENERGY_ABSCISSA,         &
                              BIRTH_MORTALITY_ENERGY_ORDINATE,                &
                              this%energy_birth ),                            &
                        0.0_SRP, 1.0_SRP )

    if ( RAND_R4() < mortality ) then
      call this%dies()
    end if

  end subroutine birth_mortality_enforce_init_fixed_debug

  !-----------------------------------------------------------------------------
  !> Cleanup the history stack of the body length and mass.
  elemental subroutine condition_clean_history(this)
    class(CONDITION), intent(inout) :: this

    this%body_length_history = MISSING
    this%body_mass_history = MISSING

  end subroutine condition_clean_history

  !=============================================================================
  ! Accessors for the CONDITION object parameters.

  !-----------------------------------------------------------------------------
  !> Get current age. *Standard GET-function.*
  elemental function condition_age_get(this) result(age)
    class(CONDITION), intent(in) :: this
    !> @return Return the agent's age
    integer :: age

    age = this%age

  end function condition_age_get

  !-----------------------------------------------------------------------------
  !> Reset the age of the agent to zero.
  elemental subroutine condition_age_reset_zero(this)
    class(CONDITION), intent(inout) :: this

    this%age = 0

  end subroutine condition_age_reset_zero

  !-----------------------------------------------------------------------------
  !> Increment the age of the agent by one.
  elemental subroutine condition_age_increment(this, increment)
    class(CONDITION), intent(inout) :: this
    !> @param[in] increment optional increment for increasing the age of the
    !!            agent, the default value is 1.
    integer, optional, intent(in) :: increment

    if (present(increment)) then
      this%age = this%age + increment
    else
      this%age = this%age + 1
    end if

  end subroutine condition_age_increment

  !-----------------------------------------------------------------------------
  !> Get current energy reserves. *Standard GET-function.*
  elemental function condition_energy_current_get(this) result(energy)
    class(CONDITION), intent(in) :: this
    !> @return Return the agent's energy reserves.
    real(SRP) :: energy

    energy = this%energy_current

  end function condition_energy_current_get

  !-----------------------------------------------------------------------------
  !> Get historical maximum of energy reserves. *Standard GET-function.*
  elemental function condition_energy_maximum_get(this) result(energy)
    class(CONDITION), intent(in) :: this
    !> @return Return the agent's maximum energy reserves.
    real(SRP) :: energy

    energy = this%energy_maximum

  end function condition_energy_maximum_get

  !-----------------------------------------------------------------------------
  !> Get current body length. *Standard GET-function.*
  elemental function condition_body_length_get(this) result(length)
    class(CONDITION), intent(in) :: this
    !> @return Return the agent's body length.
    real(SRP) :: length

    length = this%body_length

  end function condition_body_length_get

  !-----------------------------------------------------------------------------
  !> Get current value of the control unselected trait. Standard GET-function.
  elemental function condition_control_unsel_get(this) result(value_out)
    class(CONDITION), intent(in) :: this
    !> @return Return the agent's control unselected trait value.
    real(SRP) :: value_out

    value_out = this%control_unselected

  end function condition_control_unsel_get

  !-----------------------------------------------------------------------------
  !> Get current body mass. *Standard GET-function.*
  elemental function condition_body_mass_get(this) result(mass)
    class(CONDITION), intent(in) :: this
    !> @return Return the agent's body mass.
    real(SRP) :: mass

    mass = this%body_mass

  end function condition_body_mass_get

  !-----------------------------------------------------------------------------
  !> Calculate the visibility range of this agent. Visibility depends on the
  !! size of the agent, ambient illumination and agent contrast. Visibility is
  !! the distance from which this agent can be seen by a visual object (e.g.
  !! predator or conspecific). This function  is a wrapper to the
  !! the_environment::visual_range() function.
  !! @warning The `visual_range` procedures use meter for units, this
  !!          auto-converts to cm.
  !! @warning Cannot implement a generic function accepting also vectors of
  !!          this objects as only elemental object-bound array functions are
  !!          allowed by the standard. This function cannot be elemental, so
  !!          passed-object dummy argument must always be scalar.
  function condition_agent_visibility_visual_range(this, object_area,         &
                                  contrast, time_step_model) result (visrange)
    class(CONDITION), intent(in) :: this
    !> @param[in] object_area optional area of this agent, m. If not provided
    !!            (normally), is obtained from the body length attribute of
    !!            the agent (the_body::condition::body_length).
    real(SRP), optional, intent(in) :: object_area
    !> @param[in] contrast is the inherent visual contrast of the agent.
    !!            the default contrast of all objects is defined by the
    !!            commondata::preycontrast_default parameter.
    real(SRP), optional, intent(in) :: contrast
    !> @param[in] optional time step of the model, if absent gets the current
    !!            time step as defined by the value of
    !!            `commondata::global_time_step_model_current`.
    integer, optional, intent(in) :: time_step_model
    !> @return The maximum distance from which this agent can be seen.
    real(SRP) :: visrange

    ! Local copies of optionals
    real(SRP) :: object_area_here, contrast_here
    integer :: time_step_model_here

    ! Local variables
    real(SRP) :: irradiance_agent_depth

    !> ### Implementation details ###
    !> **Checks.** Check optional object area, the default value, if this
    !> parameter is absent, the body side area is calculated from the
    !! the_body::condition::body_length attribute of the agent with inline
    !! conversion to m. Note that the body side area of a fish object is
    !! calculated from the body length using the
    !! commondata::length2sidearea_fish() function.
    if (present(object_area)) then
      object_area_here = object_area
    else
      object_area_here = length2sidearea_fish( cm2m( this%body_length ) )
    end if

    !> Check optional `contrast` parameter. If unset, use global
    !! `commondata::preycontrast_default`.
    if (present(contrast)) then
      contrast_here = contrast
    else
      contrast_here = PREYCONTRAST_DEFAULT
    end if

    !> Check optional time step parameter. If unset, use global
    !! `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> Calculate ambient illumination / irradiance at the depth of
    !! this agent at the given time step using the
    !! the_environment::spatial::illumination() method.
    irradiance_agent_depth =  this%illumination(time_step_model_here)

    !> Return visual range to see this spatial object: its visibility range by
    !! calling the the_environment::visual_range() function.
    visrange =  m2cm( visual_range ( irradiance = irradiance_agent_depth,     &
                                     prey_area = object_area_here,            &
                                     prey_contrast = contrast_here )  )

  end function condition_agent_visibility_visual_range

  !-----------------------------------------------------------------------------
  !> Set body mass optionally updating the history stack.
  subroutine condition_body_mass_set_update_hist(this, value_set,             &
                                                            update_history)
    class(CONDITION), intent(inout) :: this
    !> @param value_set, Set the new (overwrite) value of the **body mass**.
    real(SRP), intent(in) :: value_set
    !> @param update_history is an optional logical flag to update the body
    !!        mass history stack, the default is **not to update**.
    logical, optional, intent(in) :: update_history

    !> ### Implementation details ###
    !> If the `value_set` is smaller that the minimum body mass parameter
    !! `BODY_MASS_MIN`, the body mass is set to this minimum value. This avoids
    !! getting the body mass too small or negative.
    !> This "set"-procedure, however, does not check if the new value is below
    !! the structure mass or any other minimum value that leads to the death of
    !! the agent. To check for starvation death, the method
    !! `condition::starved_death()` =>
    !! `the_body::body_mass_is_starvation_check()` should be explicitly
    !! executed.
    if ( value_set < BODY_MASS_MIN ) then
      this%body_mass = BODY_MASS_MIN
    else
      this%body_mass = value_set
    end if

    !> Update the body mass history stack if the `update_history` is
    !! explicitly set to TRUE. The default not to update is used because
    !! body mass should normally be updated in parallel with the length, if
    !! this is not the case, they will be dis-synchronised within the
    !! history stack arrays.
    if (present(update_history)) then
      if (update_history)                                                     &
                      call add_to_history(this%body_mass_history, value_set)
    end if

  end subroutine condition_body_mass_set_update_hist

  !-----------------------------------------------------------------------------
  !> Set body length optionally updating the history stack.
  subroutine condition_body_length_set_update_hist(this, value_set,           &
                                                                update_history)
    class(CONDITION), intent(inout) :: this
    !> @param value_set, Set the new (overwrite) value of the **body length**.
    real(SRP), intent(in) :: value_set
    !> @param update_history is an optional logical flag to update the body
    !!        length history stack, the default is **not to update**.
    logical, optional, intent(in) :: update_history

    !> ### Implementation details ###
    !> If the `value_set` is smaller that the minimum body length parameter
    !! `BODY_LENGTH_MIN` or the maximum `BODY_LENGTH_MAX`, the length is set
    !! to this minimum or maximum value respectively. This avoids setting
    !! the body length outside of the normal limits. The function
    !! `commondata::within()` is called to set the new value.
    this%body_length = within(value_set, BODY_LENGTH_MIN, BODY_LENGTH_MAX)

    !> Update the body length history stack if the `update_history` is
    !! explicitly set to TRUE. The default not to update is used because
    !! body length should normally be updated in parallel with the mass, if
    !! this is not the case, they will be dis-synchronised within the
    !! history stack arrays.
    if (present(update_history)) then
      if (update_history)                                                     &
                      call add_to_history(this%body_length_history, value_set)
    end if

  end subroutine condition_body_length_set_update_hist

  !-----------------------------------------------------------------------------
  !> Get historical record of energy reserves at birth. *Standard GET-function.*
  elemental function condition_energy_birth_get(this) result(energy)
    class(CONDITION), intent(in) :: this
    !> @return Return the agent's body length at birth.
    real(SRP) :: energy

    energy = this%energy_birth

  end function condition_energy_birth_get

  !-----------------------------------------------------------------------------
  !> Get historical record of body length at birth. *Standard GET-function.*
  elemental function condition_body_length_birth_get(this) result(length)
    class(CONDITION), intent(in) :: this
    !> @return Return the agent's body length at birth.
    real(SRP) :: length

    length = this%body_length_birth

  end function condition_body_length_birth_get

  !-----------------------------------------------------------------------------
  !> Get historical record of body mass at birth. *Standard GET-function.*
  elemental function condition_body_mass_birth_get(this) result(mass)
    class(CONDITION), intent(in) :: this
    !> @return Return the agent's body mass at birth.
    real(SRP) :: mass

    mass = this%body_mass_birth

  end function condition_body_mass_birth_get

  !-----------------------------------------------------------------------------
  !> Get historcal maximum for body mass. Standard *GET-function.*
  elemental function condition_body_mass_max_get(this) result(mass)
    class(CONDITION), intent(in) :: this
    !> @return Return the agent's maximum body mass.
    real(SRP) :: mass

    mass = this%body_mass_maximum

  end function condition_body_mass_max_get

  !-----------------------------------------------------------------------------
  !> Get current smr. Standard *GET-function.*
  elemental function condition_smr_get(this) result(smr)
    class(CONDITION), intent(in) :: this
    !> @return Return the agent's SMR.
    real(SRP) :: smr

    smr = this%smr

  end function condition_smr_get

  !-----------------------------------------------------------------------------
  !> Get current stomach content. *Standard GET-function.*
  elemental function condition_stomach_content_get(this) result(stom)
    class(CONDITION), intent(in) :: this
    !> @return Return the agent's stomach content.
    real(SRP) :: stom

    stom = this%stomach_content_mass

  end function condition_stomach_content_get

  !=============================================================================

  !> @brief   Calculate the basic processing cost of catching a food item
  !!          with the mass `food_gain`.
  !! @details There is a small cost of the food item catching, in terms of the
  !!          **food item mass** (proportional cost). So, if the agent does
  !!          an unsuccessful attempt to catch a food item, the cost still
  !!          applies. So we subtract it before testing if the agent actually
  !!          got this food item. Also, there is a fixed minimum capture cost
  !!          (in terms of the **agent body mass**), so if the food item is
  !!          very small, the actual gain can be negative (capture cost exceeds
  !!          the value of the item).
  !! @note    Note that this version accepts the the raw food mass (real value).
  elemental function body_mass_processing_cost_calc_v(this,                   &
                                                    food_gain, distance_food) &
                                                                  result (cost)
    class(CONDITION), intent(in) :: this          !> @param[in] this object.
    real(SRP), optional, intent(in) :: food_gain  !> @param[in] food gain.
    !> @param[in] distance_food distance to the food item.
    real(SRP), optional, intent(in) :: distance_food
    reaL(SRP) :: cost                             !> @return processing cost.

    ! Local copy of optionals.
    real(SRP) :: food_gain_here, distance_food_here

    ! Check optional parameter, set default values.
    if(present(food_gain)) then
      food_gain_here = food_gain
    else
      food_gain_here = FOOD_ITEM_SIZE_DEFAULT
    end if

    !> ### Implementation details ###
    !> First, check the optional distance towards the food item. It is used to
    !! calculate the energetic cost of swimming towards the food item.
    if (present(distance_food)) then
      distance_food_here = distance_food
    else
      !> If the distance to the food item is not provided, we assume it is
      !! equal to the *agent size* (so the relative distance = 1 body size).
      distance_food_here = this%body_length
    end if

    !> The cost of the processing of the food item is a sum of two components:
    !! 1. some small processing cost depending on the food item mass and
    !! 2. the cost of swimming towards the food item depending on the relative
    !!     distance (distance in terms of the agent body length.
    !! .
    !! @f[ C_{p} = max(\mu \cdot \beta_{fp}, \mu \cdot C_{smr}) + C_{s} , @f]
    !! where @f$ \mu @f$ is the food gain, @f$ \beta_{fp} @f$ is a factor
    !! proportional to the food item mass, and @f$ C_{smr} @f$ is a food
    !! processing cost factor that is proportional to the agent's SMR.
    cost = max( food_gain_here * FOOD_ITEM_CAPTURE_PROP_COST,                 &
                food_gain_here * this%cost_factor_food_smr(food_gain_here) )  &
           + this%cost_swim(distance=distance_food_here)

  end function body_mass_processing_cost_calc_v

  !-----------------------------------------------------------------------------
  !> The cost of swimming of a specific distance in terms of the actor's
  !! body mass.
  !! @note Note that power needed to swim is proportional to the body
  !!       mass with the exponent 0.6 assuming turbulent flow (see
  !!       doi:10.1242/jeb.01484).
  !! @param[in] distance the optional distance traversed (absolute distance
  !!            in real units, cm). If distance is not provided, it is
  !!            calculated from the latest spatial displacement of the agent
  !!            using the the_environment::spatial_moving::way() function.
  !! @param[in] exponent an optional cost exponent parameter. Can be 0.5
  !!            (commondata::swimming_cost_exponent_laminar,  laminar flow) or
  !!            0.6 (commondata::swimming_cost_exponent_turbulent, turbulent
  !!            flow), the default is set to 0.6.
  !! @returns The cost of swimming in terms of the body mass lost.
  elemental function condition_cost_swimming_burst(this,                      &
                                     distance, exponent) result (cost_swimming)
    class(CONDITION), intent(in) :: this             ! This object.
    real(SRP), optional, intent(in) :: distance      ! Distance traversed.
    real(SRP), optional, intent(in) :: exponent      ! Cost exponent.
    real(SRP) :: cost_swimming                       ! Return value.

    ! Local copies of optionals.
    real(SRP) :: dist_loc, exponent_here

    !> ### Notable parameters ###
    !! **SWIM_COST_EXP** is the default swimming cost body mass exponent
    !! parameter for turbulent flow
    !! commondata::swimming_cost_exponent_turbulent = 0.6. For laminar flow,
    !! equal to commondata::swimming_cost_exponent_laminar = 0.5.
    !! See doi:10.1242/jeb.01484 (https://dx.doi.org/10.1242/jeb.01484).
    real(SRP), parameter :: SWIM_COST_EXP = SWIMMING_COST_EXPONENT_TURBULENT

    ! Check optional distance
    if (present(distance)) then
      dist_loc = distance
    else
      dist_loc = this%way()
    end if

    ! Check optional exponent parameter.
    if (present(exponent)) then
      exponent_here = exponent
    else
      exponent_here = SWIM_COST_EXP
    end if

    !> ### Implementation details ###
    !> The cost of swimming (for turbulent flow) is calculated as:
    !! @f[ C_{s} = M^{0.6} \cdot \beta \cdot d / L , @f] where
    !! @f$ M @f$ is the body mass, @f$ \beta @f$ is a parameter factor
    !! defined as `commondata::swimming_speed_cost_burst`, @f$ d / L @f$ is
    !! the distance in units of the agent's body length. For laminar flow,
    !! the exponent should be 0.5.
    !! @note An arbitrary value for the exponent can be provided as the second
    !!       dummy parameter to this function `exponent`.
    !! @note The function the_body::cost_swimming_standard() calculates a
    !!       diagnostic function, the "standard" cost of swimming.
    cost_swimming = this%body_mass**exponent_here * SWIMMING_SPEED_COST_BURST &
                        * dist_loc / this%body_length

  end function condition_cost_swimming_burst

  !-----------------------------------------------------------------------------
  !> @brief  Calculate the basic processing cost of catching a food item
  !!         with the mass `food_gain`.
  !! @note   Note that this version accepts the food object not its raw mass.
  !! @param[in] food_obj food item object, of class `FOOD_ITEM`.
  !! @param[in] distance_food distance to the food item.
  !! @return Food processing cost.
  elemental function body_mass_processing_cost_calc_o(this,                   &
                                                    food_obj, distance_food)  &
                                                                  result (cost)
    class(CONDITION), intent(in) :: this        ! @param[in] this object.
    class(FOOD_ITEM), intent(in) :: food_obj    ! @param[in] food item object.
    ! @param[in] distance_food distance to the food item.
    real(SRP), optional, intent(in) :: distance_food
    reaL(SRP) :: cost                           ! @returns processing cost.

    ! Local copy of optionals.
    real(SRP) :: distance_food_here

    !> ### Implementation details ###
    ! The swimming cost body mass exponent parameter for turbulent flow is
    ! equal to 0.6 (see doi:10.1242/jeb.01484).
    ! @note **Disabled** here as this procedure now uses the above scalar
    !       `food_proc_cost_v` function for calculations.
    !real(SRP), parameter :: SWIM_COST_EXP = 0.6_SRP

    !> First, check the optional distance towards the food item. We use it to
    !! calculate the energetic cost of swimming towards the food item.
    if (present(distance_food)) then
      distance_food_here = distance_food
    else
      !> If the distance to the food item is not provided, we assume it is
      !! equal to the agent body size (so the relative distance = 1 body size).
      distance_food_here = this%body_length
    end if

    !> The cost of the processing of the food item is a sum of two components:
    !! 1. some small processing cost depending on the food item mass and
    !! 2. the cost of swimming towards the food item depending on the relative
    !!     distance (distance in terms of the agent body length.
    !! .
    !! @f[ C_{p} = max(\mu \cdot \beta_{fp}, \mu \cdot C_{smr}) + C_{s} , @f]
    !! where @f$ \mu @f$ is the food gain, @f$ \beta_{fp} @f$ is a factor
    !! proportional to the food item mass, and @f$ C_{smr} @f$ is a food
    !! processing cost factor that is proportional to the agent's SMR.
    !!
    !! @note The calculations are done by the scalar procedure
    !!       body_mass_processing_cost_calc_v().
    cost = this%food_proc_cost_v(food_obj%get_mass(), distance_food_here)

  end function body_mass_processing_cost_calc_o

  !-----------------------------------------------------------------------------
  !> Calculate the value of possible food gain as fitting into the agent's
  !! stomach, or the full gain if the food item wholly fits in.
  !! @param[in] food_gain food gain.
  !! @param[in] food_dist distance to food.
  !! @returns processing cost.
  !! @note   Note that this version accepts the the raw food mass (real value).
  !! @note   The food fitting is adjusted for the food item processing cost
  !!         body_mass_processing_cost_calc_v() call.
  elemental function stomach_content_food_gain_fitting_v(this,                &
                                                        food_gain, food_dist) &
                                                        result (food_adjusted)
    class(CONDITION), intent(in)    :: this
    real(SRP), optional, intent(in) :: food_gain ! @param[in] food gain.
    real(SRP), optional, intent(in) :: food_dist ! @param[in] distance to food.
    reaL(SRP) :: food_adjusted                   ! @returns processing cost.

    real(SRP) :: food_gain_here

    !> ### Implementation details ###
    !> Check optional `food_gain` parameter, set default values. If food
    !! gain is not provided, an average/default food item is assumed, defined
    !! by `FOOD_ITEM_SIZE_DEFAULT`.
    if(present(food_gain)) then
      food_gain_here = food_gain
    else
      food_gain_here = FOOD_ITEM_SIZE_DEFAULT
    end if

    if (present(food_dist)) then
      food_adjusted = food_gain_here - this%food_surplus(food_gain_here) -    &
                              this%food_process_cost(food_gain_here, food_dist)
    else
      food_adjusted = food_gain_here - this%food_surplus(food_gain_here) -    &
                              this%food_process_cost(food_gain_here)
    end if

  end function stomach_content_food_gain_fitting_v

  !-----------------------------------------------------------------------------
  !> Calculate the value of possible food gain as fitting into the agent's
  !! stomach (or full gain if the food item fits wholly).
  !! @note   Note that this version accepts the food object not its raw mass.
  !! @note   Note that the food fitting is adjusted for the food item
  !!         processing cost via `food_process_cost` call.
  elemental function stomach_content_food_gain_fitting_o(this,                &
                                                         food_obj, food_dist) &
                                                          result (food_adjusted)
    class(CONDITION), intent(in)    :: this      !> @param[in] this object.
    class(FOOD_ITEM), intent(in)    :: food_obj  !> @param[in] food gain.
    real(SRP), optional, intent(in) :: food_dist !> @param[in] distance to food.
    real(SRP) :: food_adjusted                   !> @returns processing cost.

    !> This code is **not using** the `food_fitt_v` scalar-based function
    !! above:
    !! `if (present(food_dist)) then
    !!    food_adjusted = food_obj%get_mass() - this%food_surplus(food_obj) - &
    !!                              this%food_process_cost(food_obj, food_dist)
    !!  else
    !!    food_adjusted = food_obj%get_mass() - this%food_surplus(food_obj) - &
    !!                              this%food_process_cost(food_obj)
    !!  end if`

    !> This code **uses** the `food_fitt_v` scalar-based function above
    !! to avoid code duplication.
    if (present(food_dist)) then
      food_adjusted = this%food_fitt_v(food_obj%get_mass(), food_dist)
    else
      food_adjusted = this%food_fitt_v(food_obj%get_mass())
    end if

  end function stomach_content_food_gain_fitting_o

  !-----------------------------------------------------------------------------
  !> Calculate extra food surplus mass non fitting into the stomach of the
  !! agent.
  !! @note   Note that this version accepts the the raw food mass (real value).
  elemental function stomach_content_food_gain_non_fit_v(this, food_gain)     &
                                                            result (extra_food)
    class(CONDITION), intent(in) :: this         !> @param[in] this object self.
    real(SRP), optional, intent(in) :: food_gain !> @param[in] food gain.
    reaL(SRP) :: extra_food                      !> @returns food surplus.

    !> Maximum  stomach capacity is determined by the factor `maxstomcap` in
    !! proportion of the body mass. The stomach content cannot surpass
    !! `maxstomcap`=15% of agent's body mass.
    real(SRP) :: stomach_content_ceiling

    real(SRP) :: food_gain_here

    !> Check optional parameter, set default values.
    if(present(food_gain)) then
      food_gain_here = food_gain
    else
      food_gain_here = FOOD_ITEM_SIZE_DEFAULT
    end if

    stomach_content_ceiling = this%body_mass * this%maxstomcap

    !> Get the possible food surplus, the part of the food gain that does
    !! not fit into the agent's stomach. If happily fits, takes zero.
    extra_food = max(0.0_SRP, this%stomach_content_mass + food_gain_here -    &
                                                    stomach_content_ceiling)

  end function stomach_content_food_gain_non_fit_v

  !-----------------------------------------------------------------------------
  !> Calculate extra food surplus mass non fitting into the stomach of the
  !! agent.
  !! @note   Note that this version accepts the food object not its raw mass.
  elemental function stomach_content_food_gain_non_fit_o(this, food_obj)      &
                                                            result (extra_food)
    class(CONDITION), intent(in) :: this      !> @param[in] this object self.
    class(FOOD_ITEM), intent(in) :: food_obj  !> @param[in] food gain.
    reaL(SRP) :: extra_food                   !> @returns food surplus.

    !> Maximum  stomach capacity is determined by the factor `maxstomcap` in
    !! proportion of the body mass. The stomach content cannot surpass
    !! `maxstomcap`=15% of agent's body mass.
    real(SRP) :: stomach_content_ceiling

    stomach_content_ceiling = this%body_mass * this%maxstomcap

    !> Get the possible food surplus, the part of the food gain that does
    !! not fit into the agent's stomach. If happily fits, takes zero.
    extra_food = max(0.0_SRP, this%stomach_content_mass +                     &
                                food_obj%get_mass() - stomach_content_ceiling)

  end function stomach_content_food_gain_non_fit_o













  !-----------------------------------------------------------------------------
  !> Calculate the cost of living for a single model step.
  !! So the agent mass increment per a single model step should subtract this
  !! cost.
  !! @note Should be calculated at the end of model time step.
  elemental function body_mass_calculate_cost_living_step(this) result(stepcost)
    class(CONDITION), intent(in) :: this
    real(SRP) :: stepcost        !> energetic cost of bodymass growth per time step.

    !> `SIGMA` parameter defining the shape of the living cost weight component.
    !! It is the value of *x* when *y* becomes 0.5.
    !! @note Use this formula for wxMaxima plot (sigma 10):
    !! @code
    !!       wxplot2d( (smr/10)**(4/3) , [smr,0,10] );
    !! @endcode
    real(SRP), parameter :: SIGMA = 10

    ! Check erroneous value of body mass, this results in a
    ! commondata::missing standard cost.
    if (this%body_mass < ZERO) then
      stepcost = MISSING
      return
    end if

    !> The energetic costs is a fraction of body weight and scales to
    !! number of time steps : @f[ M_{c}= \frac{C B}{\Omega} @f] (eq. 1)
    !! now it also depends on the SMR:
    !! @f[ M_{c}= \frac{C B}{\Omega} +
    !!       \left ({\frac{s}{\sigma}} \right)^{4/3} \frac{C B}{\Omega} @f]
    !! where @f$ \sigma @f$ is is a coefficient defining the increment of the
    !! function (value at which 1.0 is reached) and *s* is the SMR (standard
    !! metabolic rate). When SMR is small, the additional part of the living
    !! cost is close to zero and the value set in the equation 1 will hold
    !! (same as the old model value as in JG), but the cost increase weakly
    !! exponentially when smr gets large. The cost equation is inspired by the
    !! Kleiber's law: `metabolic rate = mass**3/4`
    !! TODO: Get rid of the old model cost, do dependent on SMR mainly.
    stepcost = this%body_mass * LIVING_COST / LIFESPAN +                      &
               (this%smr/SIGMA)**4/3 * this%body_mass * LIVING_COST / LIFESPAN

  end function body_mass_calculate_cost_living_step

  !-----------------------------------------------------------------------------
  !> Adjust the body mass at the end of the model step against the cost of
  !! living. We do not adjust the cost of living at each food gain as several
  !! food items can be consumed by the agent at a single time step of the model.
  !! Cost of living is now calculated at the end of the time step of the model.
  !! @note Should be calculated at the end of model time step.
  elemental subroutine body_mass_adjust_living_cost_step(this)
    class(CONDITION), intent(inout) :: this

    this%body_mass = this%body_mass - this%living_cost()

  end subroutine body_mass_adjust_living_cost_step

  !-----------------------------------------------------------------------------
  !> Do grow body mass based on food gain from a single food item adjusted
  !! for cost etc.
  !! @note Can be calculated after consumption of each food item, many times
  !!       within a single time step of the model.
  elemental subroutine body_mass_grow_do_calculate(this, food_gain, update_history)
    class(CONDITION), intent(inout) :: this   !> @param[inout] self.
    real(SRP), intent(in) :: food_gain        !> @param[in] food gain.
    !> @param[in] update_history optional logical flag to enable saving
    !!            the body mass value to the body mass history stack.
    !! @warning   History update is disabled by default because the length and
    !!            mass histories can be updated separately, so could get not
    !!            synchronous.
    logical, optional, intent(in) :: update_history

    !> Add the food increment to the current body mass.
    this%body_mass = max( this%body_mass + food_gain , BODY_MASS_MIN )

    !> And also update the historical maximum value, if the current exceeds.
    if (this%body_mass > this%body_mass_maximum)                              &
                                    this%body_mass_maximum = this%body_mass

    !> Add the current updated body mass to the history stack.
    if (present(update_history)) then
      if (update_history)                                                     &
                  call add_to_history(this%body_mass_history, this%body_mass)
    end if

  end subroutine body_mass_grow_do_calculate

  !-----------------------------------------------------------------------------
  !> The fraction of the cost of the processing of the food item(s) depending
  !! on the agent SMR. It is scaled in terms of the ratio of the food item mass
  !! to the agent mass.
  elemental function body_mass_food_processing_cost_factor_smr(this,food_gain) &
                                                            result(cost_factor)
    class(CONDITION), intent(in) :: this      !> @param[inout] self.
    real(SRP), intent(in) :: food_gain        !> @param[in] food gain.
    real(SRP) :: cost_factor

    cost_factor = max( food_gain / this%body_mass * COST_FACTOR_FORAGING_SMR  &
                                                          * this%smr, 0.0_SRP )

  end function body_mass_food_processing_cost_factor_smr

  !-----------------------------------------------------------------------------
  !> Do increment stomach contents with adjusted (fitted) value.
  !! @note Note that the `stomach_increment` should be adjusted
  !!       for fitting size. This is the actual increment.
  elemental subroutine stomach_content_get_increment(this, stomach_increment)
    class(CONDITION), intent(inout) :: this
    real(SRP), intent(in) :: stomach_increment

    !> ### Implementation details ###
    !> Stomach content mass is incremented by the `stomach_increment` value.
    ! Negative stomach increment is not processed as we cannot have negative
    ! stomach content mass.
    if (stomach_increment > 0.0_SRP) this%stomach_content_mass =              &
                              this%stomach_content_mass + stomach_increment

  end subroutine stomach_content_get_increment

  !-----------------------------------------------------------------------------
  !> @brief   Calculate body length increment for a time step of the model.
  !! @details This function describes linear growth of the agent resulting from
  !!          food intake. Linear growth increment scales with **growth
  !!          hormone** level. The increment in length is weighted by the growth
  !!          hormone factor that is obtained via interpolation. So, if the
  !!          agent's growth hormone level is very low, the growth increment
  !!          factor is near-zero, if growth hormone level is high, the growth
  !!          increment approaches the maximum value that is proportional to the
  !!          body mass increment in units of the agent's body mass (growth
  !!          hormone weighting factor approaches 1.0).
  function body_len_grow_calculate_increment_step(this, mass_increment)       &
                                                      result (length_increment)
    class(CONDITION), intent(in) :: this
    real(SRP), intent(in) :: mass_increment !> @param increment of the weight

    real(SRP) :: length_increment           !> @returns Body length increment

    !> ### Notable local parameters ###
    !> `increment_factor_ipoint` is a local parameter showing the linear
    !! growth increment factor interpolation point, @f$ \vartheta @f$ in the
    !! formula below.
    real(SRP) :: increment_factor_ipoint

    !> ### Implementation details ###
    !> If the body mass increment is positive and exceeds fixed threshold
    !! `MASS_GROWTH_THRESHOLD`, the agent can grow body length. Otherwise,
    !! if the mass threshold is not exceeded in MASS_THRESHOLD, the agent
    !! does not increase in length. This check is done in the main named
    !! if condition block `MASS_THRESHOLD`.
    !> #### MASS_THRESHOLD block ####
    MASS_THRESHOLD: if (mass_increment >                                      &
                                this%body_mass * MASS_GROWTH_THRESHOLD) then

      !> - **First,** we get the interpolation-based growth increment factor
      !!   `increment_factor_ipoint` (@f$ \vartheta @f$) depending on the
      !!   agent's current **growth hormone** level. The function linking
      !!   growth hormone level and the linear growth increment factor
      !!   (@f$ \vartheta @f$) is represented by this relationship:
      !!   @image html img_doxygen_growth_linear_factor.svg
      !!   @image latex img_doxygen_growth_linear_factor.eps "Linear growth increment factor" width=14cm
      !!   @note Note that the linear interpolation `LINTERPOL` engine is used
      !!         here instead of non-linear `DDPINTERPOL`. This is done because
      !!         of not well predictable raw values in the grid abscissa;
      !!         `DDPINTERPOL` tends to produce sigmoidal waves here and needs
      !!         precise tuning of the interpolation grid parameter arrays
      !!         - `LINEAR_GROWTH_HORMONE_INCREMENT_FACTOR_CURVE_ABSCISSA`
      !!         - `LINEAR_GROWTH_HORMONE_INCREMENT_FACTOR_CURVE_ORDINATE`.
      !!         .
      increment_factor_ipoint = LINTERPOL(                                    &
          gamma2gene(LINEAR_GROWTH_HORMONE_INCREMENT_FACTOR_CURVE_ABSCISSA),  &
          LINEAR_GROWTH_HORMONE_INCREMENT_FACTOR_CURVE_ORDINATE,              &
          this%growhorm_get() )

      !> - Save the interpolation plot in the @ref intro_debug_mode
      !!   "debug mode" using external command.
      !!   @warning Involves **huge** number of plots, should normally be
      !!            disabled.
      call debug_interpolate_plot_save(                                       &
            grid_xx=LINEAR_GROWTH_HORMONE_INCREMENT_FACTOR_CURVE_ABSCISSA,    &
            grid_yy=LINEAR_GROWTH_HORMONE_INCREMENT_FACTOR_CURVE_ORDINATE,    &
            ipol_value=this%growhorm_get(), algstr="LINTERPOL",               &
            output_file="plot_debug_growth_linear_factor_" //                 &
                      TOSTR(Global_Time_Step_Model_Current) //                &
                      MMDD // "_a_"// trim(this%individ_label()) // "_" //    &
                      RAND_STRING(LABEL_LENGTH, LABEL_CST,LABEL_CEN) // PS )

      !> - **Second,** The body length increment in units of length
      !!   @f$ \frac{\Delta L}{L} @f$ is proportional to the body mass
      !!   increment in units of mass: @f$ \frac{\Delta M}{M} @f$, however
      !!   weighted by the linear growth increment factor @f$ \vartheta @f$
      !!   that depends on the growth hormone and is obtained via interpolation
      !!   (see above). If the agent's growth hormone level is very low, the
      !!   growth increment factor is near-zero and linear growth increment is
      !!   also near-zero. However, if growth hormone level is high, the growth
      !!   increment weighting factor @f$ \vartheta @f$ approaches the maximum
      !!   value that is proportional to the body mass increment in units of
      !!   the agent's body mass:
      !!   @f[ \Delta L = L \cdot  \vartheta \cdot \frac{\Delta M}{M} , @f]
      !!   where @f$ \Delta L @f$ is the body length increment, @f$ L @f$ is
      !!   the body length, @f$ \vartheta @f$ is the growth-hormone-dependent
      !!   linear growth increment factor, @f$ \frac{\Delta M}{M} @f$ is the
      !!   body mass increment in units of body mass. So the relative body
      !!   length increment is proportional to the relative body mass increment.
      length_increment = this%body_length * increment_factor_ipoint *         &
                         mass_increment / this%body_mass

    else MASS_THRESHOLD

      !> - If the mass threshold is not exceeded in MASS_THRESHOLD, the agent
      !!   does not increase in length, the length increment is zero.
      !!
      length_increment = 0.0_SRP

    end if MASS_THRESHOLD

  end function body_len_grow_calculate_increment_step

  !-----------------------------------------------------------------------------
  !> Do linear growth for one model step based on the increment function
  !! the_body::condition::len_incr().
  !! @note Should be calculated at the end of model time step.
  subroutine body_len_grow_do_calculate_step(this, mass_increment, update_history)
    class(CONDITION), intent(inout) :: this    !> @param[inout] self
    real(SRP), intent(in) :: mass_increment    !> @param increment of the weight
    !> @param[in] update_history optional logical flag to enable saving
    !!            the body mass value to the body mass history stack.
    !! @warning   History update is disabled by default because the length and
    !!            mass histories can be updated separately, so could get not
    !!            synchronous.
    logical, optional, intent(in) :: update_history

    !> ### Implementation details ###
    !> Body length is incremented by a value of the
    !! the_body::condition::len_incr() function.
    this%body_length = this%body_length + this%len_incr(mass_increment)

    !> Also, the current updated body length is added to the history stack.
    if (present(update_history)) then
      if (update_history)                                                     &
                call add_to_history(this%body_length_history, this%body_length)
    end if

  end subroutine body_len_grow_do_calculate_step

  !-----------------------------------------------------------------------------
  !> @brief   Update the level of the sex steroids.
  !! @details Sex steroids are incremented each model time step. Testosteron is
  !!          incremented in males and estrogen, in females. However, such an
  !!          increment occurs only if there has recently been any body length
  !!          growth (which occurs only if the food gain exceeds a specific
  !!          threshold value). The growth increment is calculated as the
  !!          difference between the current body length and the maximum body
  !!          length in `n` latest historical entries from the length history
  !!          stack. The `n` value is set by the parameter
  !!          commondata::sex_steroids_check_history.
  subroutine sex_steroids_update_increment(this)
    class(CONDITION), intent(inout) :: this

    ! Local variables.
    real(SRP) :: length_increment, steroid_increment_factor

    !> ### Implementation details ###
    !> First, the sex steroid **increment factor** is calculated using
    !! a nonparametric relationship. Calculations can be based either
    !! on its link with the **age** or the **body length**:
    !! - ::steroid_factor_age() or
    !! - ::steroid_factor_len().
    !! .
    !! These are the two alternative procedures implemented here.
    !! @note Here implementation is based on ::steroid_factor_age().
    steroid_increment_factor = steroid_factor_age()

    !> Next, calculate the **past increments of the body length** across the
    !! body length history stack. If there has been any increment in the body
    !! length during the commondata::sex_steroids_check_history latest steps
    !! in the history stack **and** the current length, sex steroids are
    !! incremented. If such an increment is zero, sex steroids are not
    !> incremented. The length increment over the latest history is calculated
    !! as follows:
    !! @code
    !!   length_increment =                                                 &
    !!     maxval( [history_array, current_value] ) -                       &
    !!     minval( [history_array, current_value] )
    !! @endcode
    length_increment =                                                        &
          maxval( [this%body_length_history(                                  &
                      HISTORY_SIZE_AGENT_PROP-SEX_STEROIDS_CHECK_HISTORY+1:   &
                      HISTORY_SIZE_AGENT_PROP), this%body_length],            &
                  [this%body_length_history(                                  &
                      HISTORY_SIZE_AGENT_PROP-SEX_STEROIDS_CHECK_HISTORY+1:   &
                      HISTORY_SIZE_AGENT_PROP), this%body_length] /= MISSING  &
                ) -  & ! @note minus sign here
          minval( [this%body_length_history(                                  &
                      HISTORY_SIZE_AGENT_PROP-SEX_STEROIDS_CHECK_HISTORY+1:   &
                      HISTORY_SIZE_AGENT_PROP), this%body_length],            &
                  [this%body_length_history(                                  &
                      HISTORY_SIZE_AGENT_PROP-SEX_STEROIDS_CHECK_HISTORY+1:   &
                      HISTORY_SIZE_AGENT_PROP), this%body_length] /= MISSING  &
                )
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! Previous implementation:
    !  @note Note that only non-MISSING values count: `maxval`
    !        uses a **mask**.
    !  @note  **This version** checks the current body length against the
    !         maximum historical value. It is disabled now.
    !length_increment = this%body_length -                                    &
    !      maxval( this%body_length_history(                                  &
    !                  HISTORY_SIZE_AGENT_PROP-SEX_STEROIDS_CHECK_HISTORY+1:  &
    !                  HISTORY_SIZE_AGENT_PROP),                              &
    !              this%body_length_history(                                  &
    !                  HISTORY_SIZE_AGENT_PROP-SEX_STEROIDS_CHECK_HISTORY+1:  &
    !                  HISTORY_SIZE_AGENT_PROP) /= MISSING                    &
    !            )
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    !> Finally, do increment the sex steroids depending on the body length
    !! (`length_increment`) value.  Sex steroids get non-zero increment only
    !! if there has been some growth of the body length
    !! (`length_increment>0.0`). Otherwise a previous value is retained.
    !  TODO: replace if condition to the start of the procedure with return
    !        to save on steroid_increment_factor dry calculation overhead.
    if (length_increment > TOLERANCE_LOW_DEF_SRP) then
      if (this%is_male()) then
        !> - If the agent is **male**, **testosterone** is incremented.
        call this%testosterone_set(                                           &
                  this%testosterone_get() +                                   &
                  this%testosterone_get() * steroid_increment_factor )
      else
        !> - If the agent is **female**, **estrogen** is incremented.
        !! .
        call this%estrogen_set(                                               &
                  this%estrogen_get() +                                       &
                  this%estrogen_get() * steroid_increment_factor )
      end if
    !> If there was no growth and the gonadal steroids are not incremented,
    !! the current values are still saved in the history stack by calling
    !! the_hormones::hormones::hormones_to_history().
    else
      call this%hormones_to_history()
    end if

  contains
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !> Function calculating the value of the sex steroid increment factor that
    !! depends on the agent's **age**.
    !! Calculate the steroid increment factor. It is set as a nonparametric
    !! relationship that is set by a linear interpolation LINTERPOL (or
    !! DDPINTERPOL) of a parameter grid values. The increment of the sex
    !! steroid hormones depends on the **age** of the agent: it is very slight
    !! at the early stage of the ontogeny, i.e. small age, but increase to the
    !! end of the agent's lifespan.
    function steroid_factor_age() result (value_out)
      real(SRP) :: value_out

    !> Calculate the steroid increment factor. It is set as a nonparametric
    !! relationship that is set by a linear interpolation LINTERPOL (or
    !! DDPINTERPOL) of a parameter grid values. The increment of the sex
    !! steroid hormones depends on the **age** of the agent: it is very slight
    !! at the early stage of the ontogeny, i.e. small age, but increase to the
    !! end of the agent's lifespan.
    value_out = DDPINTERPOL(SEX_STEROIDS_INCREMENT_FACTOR_AGE_CURVE_ABSCISSA, &
                            SEX_STEROIDS_INCREMENT_FACTOR_AGE_CURVE_ORDINATE, &
                            real(this%get_age(), SRP))

    !> Interpolation plots can be saved in the @ref intro_debug_mode
    !! "debug mode" using this plotting command:
    !! commondata::debug_interpolate_plot_save().
    !! @warning Involves **huge** number of plots, should normally be
    !!          disabled.
    call debug_interpolate_plot_save(                                         &
            grid_xx=SEX_STEROIDS_INCREMENT_FACTOR_AGE_CURVE_ABSCISSA,         &
            grid_yy=SEX_STEROIDS_INCREMENT_FACTOR_AGE_CURVE_ORDINATE,         &
            ipol_value=real(this%get_age(), SRP),                             &
            algstr="DDPINTERPOL",                                             &
            output_file="plot_debug_sex_steroid_incr_factor_" //              &
                        TOSTR(Global_Time_Step_Model_Current) //              &
                        MMDD // "_a_"// trim(this%individ_label()) //         &
                        "_" // RAND_STRING(LABEL_LENGTH, LABEL_CST,LABEL_CEN) &
                        // PS )

    end function steroid_factor_age

    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !> Function calculating the value of the sex steroid increment factor that
    !! depends on the agent's **body length**.
    !! Calculate the steroid increment factor. It is set as a nonparametric
    !! relationship that is set by a linear interpolation LINTERPOL (or
    !! DDPINTERPOL) of a parameter grid values. The increment of the sex
    !! steroid hormones depends on the **length** of the agent: it is very
    !! slight in small agents, e.g. at the early stage of the ontogeny, but
    !! increases in larger agents up to `BODY_LENGTH_MAX`.
    function steroid_factor_len() result (value_out)
      real(SRP) :: value_out

    !> Calculate the steroid increment factor. It is set as a nonparametric
    !! relationship that is set by a linear interpolation LINTERPOL (or
    !! DDPINTERPOL) of a parameter grid values. The increment of the sex
    !! steroid hormones depends on the **length** of the agent: it is very
    !! slight in small agents, e.g. at the early stage of the ontogeny, but
    !! increases in larger agents up to `BODY_LENGTH_MAX`.
    !! @image html img_doxy_steroid_fact_length.svg
    !! @image latex img_doxy_steroid_fact_length.eps "Steroid increment factor by body length" width=14cm
    value_out = DDPINTERPOL(SEX_STEROIDS_INCREMENT_FACTOR_LEN_CURVE_ABSCISSA, &
                            SEX_STEROIDS_INCREMENT_FACTOR_LEN_CURVE_ORDINATE, &
                            real(this%get_length(), SRP))

    !> Interpolation plots can be saved in the @ref intro_debug_mode
    !! "debug mode" using this plotting command:
    !! commondata::debug_interpolate_plot_save().
    !! @warning Involves **huge** number of plots, should normally be
    !!          disabled.
    call debug_interpolate_plot_save(                                         &
            grid_xx=SEX_STEROIDS_INCREMENT_FACTOR_LEN_CURVE_ABSCISSA,         &
            grid_yy=SEX_STEROIDS_INCREMENT_FACTOR_LEN_CURVE_ORDINATE,         &
            ipol_value=real(this%get_length(), SRP),                          &
            algstr="DDPINTERPOL",                                             &
            output_file="plot_debug_sex_steroid_incr_factor_" //              &
                        TOSTR(Global_Time_Step_Model_Current) //              &
                        MMDD // "_a_"// trim(this%individ_label()) //         &
                        "_" // RAND_STRING(LABEL_LENGTH, LABEL_CST,LABEL_CEN) &
                        // PS )

    end function steroid_factor_len

  end subroutine sex_steroids_update_increment

  !-----------------------------------------------------------------------------
  !> Check if the body mass is smaller than the birth body mass or structural
  !! body mass.
  !! An agent dies of starvation if either of these conditions is met:
  !!  - its body mass falls below half the birth mass;
  !!  - below the structural mass, which is defined as half the
  !!    historic maximum body mass of the individual;
  !!  - energy reserves fall below 1/4 of historical maximum value;
  !!  - body mass is below the hard limit BODY_MASS_MIN.
  !  @warning We use *internal object-properties* here instead of get-functions
  !           for brevity. Also, all raw components used here are within-object:
  !           `body_mass`, `stomach_content_mass`, `body_mass_birth`
  !           `body_mass_maximum`, `energy_current`, energy_maximum`.
  elemental function body_mass_is_starvation_check (this)                     &
                                                    result (is_starved_to_death)
    class(CONDITION), intent(in) :: this
    !> @return Returns a logical flag: TRUE if starved, FALSE otherwise.
    logical :: is_starved_to_death

    !> The `the_body::is_starved()` backend function (non-OO) is called to
    !! check the starvation condition.
    if ( is_starved( this%body_mass, this%stomach_content_mass,               &
                     this%body_mass_birth, this%body_mass_maximum,            &
                     this%energy_current, this%energy_maximum) ) then
      is_starved_to_death = .TRUE.
    else
      is_starved_to_death = .FALSE.
    end if

  end function body_mass_is_starvation_check

  !-----------------------------------------------------------------------------
  !> This is the backend logical function that checks if the agent is starved.
  !! It is called by the `condition::starved_death()` =>
  !! `the_body::body_mass_is_starvation_check()` procedure.
  !! @note Note that this function is not type-bound (non-OO).
  elemental function is_starved ( body_mass, stomach_content_mass,            &
                                  body_mass_birth, body_mass_maximum,         &
                                  energy_current, energy_maximum)             &
          result (starved)
    !> @param[in] body_mass the current body mass of the agent.
    real(SRP), intent(in) :: body_mass
    !> @param[in] stomach_content_mass the mass of the stomach content of the
    !!            agent.
    real(SRP), intent(in) :: stomach_content_mass
    !> @param[in] body_mass_birth body mass of the agent at birth.
    real(SRP), intent(in) :: body_mass_birth
    !> @param[in] body_mass_maximum the historical maximum body mass of
    !!            the agent.
    real(SRP), intent(in) :: body_mass_maximum
    !> @param[in] energy_current the current level of energy.
    real(SRP), intent(in) :: energy_current
    !> @param[in] energy_maximum the historical maximum level of energy.
    real(SRP), intent(in) :: energy_maximum
    !> @returns TRUE if the input conditions make the agent starved to death.
    !!          Otherwise returns FALSE.
    logical :: starved

    !> ### Conditions of severe starvation ###
    !> An agent is considered starving to death if either of these conditions
    !! is met:
    !!  - its body mass falls below half the birth mass;
    !!  - below the structural mass, which is defined as half the
    !!    historic maximum body mass of the individual;
    !!  - energy reserves fall below 1/4 of historical maximum value;
    !!  - body mass is below the hard limit BODY_MASS_MIN.
    if (  body_mass - stomach_content_mass < body_mass_birth/2.0_SRP .or.     &
          body_mass - stomach_content_mass < body_mass_maximum/2.0_SRP .or.   &
          energy_current < energy_maximum/4.0_SRP  .or.                       &
          body_mass < BODY_MASS_MIN  ) then
      starved = .TRUE.
    else
      starved = .FALSE.
    end if

  end function is_starved

  !-----------------------------------------------------------------------------
  !> Digestion. Stomach contents S(t) is emptied by a constant fraction each
  !! time step @f[ S_{t+1} = S_{t} - S_{t} \frac{ K }{\Omega } , @f]
  !! where @f$ K @f$ is the *stomach content emptify factor* parameter
  !! (`commondata::stomach_content_emptify_factor`) and @f$ \Omega @f$ is the
  !! lifespan (`commondata::lifespan` parameter).
  !! The calculation calls the backend function for
  !! @f$ \Delta S = S_{t} \frac{ K }{\Omega } @f$:
  !! `the_body::stomach_emptify_backend()`.
  !> @note Should be calculated at the end of model time step.
  !  @warning We use internal object-properties here instead of get-functions
  !           for brevity. Also, all raw components used here are within-object:
  !           `stomach_content_mass`.
  elemental subroutine stomach_content_mass_emptify_step(this)
    class(CONDITION), intent(inout) :: this

    this%stomach_content_mass =                                               &
                 max( 0.0_SRP,                                                &
                      this%stomach_content_mass -                             &
                          stomach_emptify_backend(this%stomach_content_mass) )

  end subroutine stomach_content_mass_emptify_step

  !-----------------------------------------------------------------------------
  !> The backend engine for calculating the stomach content mass decrement as
  !! a consequence of *digestion*. Stomach contents S(t) is emptied by a
  !! constant fraction each time step @f$ \Delta S @f$:
  !! @f[ \Delta S = S_{t} \frac{ K }{\Omega } , @f] where @f$ K @f$
  !! is the *stomach content emptify factor* parameter
  !! (`commondata::stomach_content_emptify_factor`) and @f$ \Omega @f$ is the
  !! lifespan (`commondata::lifespan` parameter).
  elemental function stomach_emptify_backend(stomach_content_mass)            &
                                            result(stomach_decrement_digestion)
    !> @param[in] stomach_content_mass Current mass of the stomach contents.
    real(SRP), intent(in) :: stomach_content_mass
    !> @return The decrement value
    real(SRP) :: stomach_decrement_digestion

    stomach_decrement_digestion =                                             &
              stomach_content_mass * STOMACH_CONTENT_EMPTIFY_FACTOR / LIFESPAN

  end function stomach_emptify_backend

  !-----------------------------------------------------------------------------
  !> Update the energy reserves of the agent based on its current mass and
  !! length. This subroutine should be called after any event that can change
  !! the mass or/and length of the agent, e.g. food consumption.
  elemental subroutine condition_energy_update_after_growth(this)
    class(CONDITION), intent(inout) :: this

    !> Update the energy reserves. This is done by calling the standard
    !! function energy_reserve()
    this%energy_current = energy_reserve( this%body_mass, this%body_length )

    !> And also update the historical maximum value, if the current
    !! energy reserves value exceeds the maximum.
    if (this%energy_current > this%energy_maximum)                            &
                                    this%energy_maximum = this%energy_current

  end subroutine condition_energy_update_after_growth

  !-----------------------------------------------------------------------------
  !> The standard cost of swimming is a diagnostic function that shows the cost,
  !! in units of the body mass, incurred if the agent passes a distance equal
  !! to commondata::lifespan units of its body length.
  elemental function cost_swimming_standard(this, steps) result (cost_out)
    class(CONDITION), intent(in) :: this
    !> @param[in] steps is the optional number of steps of the agent length
    !!            the agent walks. Default value is commondata::lifespan (i.e.
    !!            the whole lifespan).
    integer, optional, intent(in) :: steps
    !> @return The cost of swimming in terms of the agent's current body mass.
    real(SRP) :: cost_out

    ! Local copies of optionals
    integer :: steps_loc

    ! Check erroneous value of body mass, this results in a
    ! commondata::missing standard cost.
    if (this%body_mass < ZERO ) then
      cost_out = MISSING
      return
    end if

    if (present(steps)) then
      steps_loc = steps
    else
      steps_loc = LIFESPAN
    end if

    cost_out =  this%cost_swim( distance=this%get_length(),                   &
                                exponent=SWIMMING_COST_EXPONENT_LAMINAR ) *   &
                real(steps_loc, SRP) / this%get_mass()

  end function cost_swimming_standard

  !-----------------------------------------------------------------------------
  !> Calculate the energetic cost of reproduction in terms of the **body mass**
  !! of the this agent. The energetic cost of reproduction is obtained as a
  !! specific fixed fraction of the current body mass of the agent defined by
  !! the `commondata::reproduction_cost_body_mass` parameter.
  elemental function reproduction_cost_energy_fix(this) result(repr_cost_mass)
    class(REPRODUCTION), intent(in) :: this
    !> @returns Returns the energetic cost of reproduction for the this agent.
    real(SRP) :: repr_cost_mass

    repr_cost_mass = this%get_mass() * REPRODUCTION_COST_BODY_MASS_FIX

  end function reproduction_cost_energy_fix

  !-----------------------------------------------------------------------------
  !> Calculate the energetic cost of reproduction in terms of the **body mass**
  !! of the this agent. The energetic cost of reproduction is different in
  !! males and females.
  function reproduction_cost_energy_dynamic(this) result(repr_cost_mass)
    class(REPRODUCTION), intent(in) :: this
    !> @returns Returns the energetic cost of reproduction for the this agent.
    real(SRP) :: repr_cost_mass

    ! Local variables
    real(SRP) :: offspring_mass

    !> ### Implementation details ###
    !> **First,** calculate the overall mass of the offspring that are
    !! produced as a result of this reproduction event @f$ \mu @f$. This
    !! is done using the procedure `reproduction::offspring_mass`
    !! (=> `the_body::reproduction_mass_offspring_calc`). The total
    !! mass of the offspring serves as a baseline for calculating the
    !! overall cost of reproduction.
    offspring_mass = this%offspring_mass()

    !> **Second,** calculate the cost of reproduction as a sum of
    !! two components:
    !!   - component that scales with the total mass of the offspring
    !!     @f$ \mu @f$;
    !!   - component that scales with the body mass of the agent @f$ M @f$.
    !!   .
    !> There are two versions of this function implemented:
    !!   - `::cost_full()` where the cost component that scales with the
    !!     agent's body mass is calculated from the full agent's mass *not
    !!     subtracting* the total mass of the offspring:
    !!     @f[ C = \mu \cdot \phi + M \cdot \varphi , @f]
    !!   - `::cost_residual()` where the cost component that scales with
    !!     the agent's body mass is calculated from the agent's *residual*
    !!     body mass *after subtracting* the total mass of the offspring:
    !!     @f[ C = \mu \cdot \phi + (M - \mu \cdot \phi ) \cdot \varphi , @f]
    !!   .
    !! where @f$ \phi @f$ and @f$ \varphi @f$ are the scaling factors
    !! that are set by the following sex-specific parameter values:
    !! Scaling factor of the offspring mass component @f$ \phi @f$:
    !!   - `commondata::reproduction_cost_offspring_fract_male`;
    !!   - `commondata::reproduction_cost_offspring_fract_female`.
    !!   .
    !! Scaling factor of the agent's body mass component @f$ \varphi @f$:
    !!   - `commondata::reproduction_cost_body_mass_factor_male`;
    !!   - `commondata::reproduction_cost_body_mass_factor_female`.
    !!   .
    !> ### Notes ###
    !> This allows setting the cost of reproduction in a sex-specific
    !! way. For males, for example, the component proportional to the
    !! total offspring mass is set to some small value (@f$ \phi \leq 1.0 @f$,
    !! whereas in females, who carry the eggs, this cost is at least equal to
    !! the full offspring mass (@f$ \phi \geq 1.0 @f$). On the other hand,
    !! the cost component that is proportional to the agent body mass can
    !! be higher in males that in females due to competition for mates
    !! (@f$ \varphi_{males} \geq \varphi_{females} @f$). Various patterns
    !! can be implemented by varying the sex-specific scaling parameters and
    !! the two versions of the backend procedure (`::cost_full()`,
    !! `::cost_residual()`).
    if ( this%is_male() ) then
      repr_cost_mass = cost_full( REPRODUCTION_COST_OFFSPRING_FRACT_MALE,     &
                                  REPRODUCTION_COST_BODY_MASS_FACTOR_MALE)
    else
      repr_cost_mass = cost_full( REPRODUCTION_COST_OFFSPRING_FRACT_FEMALE,   &
                                  REPRODUCTION_COST_BODY_MASS_FACTOR_FEMALE)
    end if

    contains
      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !> Backend function that is called in
      !! `the_body::reproduction_cost_energy_dynamic()`.
      !> Calculate the cost of reproduction as a sum of
      !! two components: (a) component that scales with the total mass
      !! of the offspring @f$ \mu @f$; (b) component that scales with the
      !! body mass of the agent @f$ M @f$.
      !! @f[ C = \mu \cdot \phi + M \cdot \varphi , @f]
      !! where @f$ \phi @f$ and @f$ \varphi @f$ are the scaling factors
      !! that are set by the following sex-specific parameter values:
      !! Scaling factor of the offspring mass component @f$ \phi @f$:
      !!   - `commondata::reproduction_cost_offspring_fract_male`;
      !!   - `commondata::reproduction_cost_offspring_fract_female`.
      !!   .
      !! Scaling factor of the agent's body mass component @f$ \varphi @f$:
      !!   - `commondata::reproduction_cost_body_mass_factor_male`;
      !!   - `commondata::reproduction_cost_body_mass_factor_female`.
      !!   .
      !! @note In this version, the cost component that scales with the agent's
      !!       body mass is calculated from the agent's mass not subtracting
      !!       the total mass of the offspring:@f$ M \cdot \varphi @f$.
      pure function cost_full (offspring_mass_fact, agent_mass_fact) result (cost)
        !> @param[in] offspring_mass_fact scaling factor of the offspring mass
        !!            component @f$ \phi @f$:
        !!             - `commondata::reproduction_cost_offspring_fract_male`;
        !!             - `commondata::reproduction_cost_offspring_fract_female`.
        !!             .
        real(SRP), intent(in) :: offspring_mass_fact
        !> @param[in] agent_mass_fact scaling factor of the agent's body mass
        !!            component @f$ \varphi @f$:
        !!             - `commondata::reproduction_cost_body_mass_factor_male`;
        !!             - `commondata::reproduction_cost_body_mass_factor_female`.
        !!             .
        real(SRP), intent(in) :: agent_mass_fact
        !> @return The cost of reproduction.
        real(SRP) :: cost

        ! Using **full** body mass of the agent.
        cost = offspring_mass * offspring_mass_fact +                         &
                                      this%get_mass() * agent_mass_fact

      end function cost_full

      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !> Backend function that is called in
      !! `the_body::reproduction_cost_energy_dynamic()`.
      !> Calculate the cost of reproduction as a sum of
      !! two components: (a) component that scales with the total mass
      !! of the offspring @f$ \mu @f$; (b) component that scales with the
      !! body mass of the agent @f$ M @f$.
      !! @f[ C = \mu \cdot \phi + (M - \mu \cdot \phi ) \cdot \varphi , @f]
      !! where @f$ \phi @f$ and @f$ \varphi @f$ are the scaling factors
      !! that are set by the following sex-specific parameter values:
      !! Scaling factor of the offspring mass component @f$ \phi @f$:
      !!   - `commondata::reproduction_cost_offspring_fract_male`;
      !!   - `commondata::reproduction_cost_offspring_fract_female`.
      !!   .
      !! Scaling factor of the agent's body mass component @f$ \varphi @f$:
      !!   - `commondata::reproduction_cost_body_mass_factor_male`;
      !!   - `commondata::reproduction_cost_body_mass_factor_female`.
      !!   .
      !! @note In this version, the cost component that scales with the agent's
      !!       *residual* body mass is calculated from the agent's mass after
      !!       subtracting the total mass of the offspring:
      !!       @f$ (M - \mu \cdot \phi ) \cdot \varphi @f$.
      pure function cost_residual (offspring_mass_fact, agent_mass_fact) result (cost)
        !> @param[in] offspring_mass_fact scaling factor of the offspring mass
        !!            component @f$ \phi @f$:
        !!             - `commondata::reproduction_cost_offspring_fract_male`;
        !!             - `commondata::reproduction_cost_offspring_fract_female`.
        !!             .
        real(SRP), intent(in) :: offspring_mass_fact
        !> @param[in] agent_mass_fact scaling factor of the agent's body mass
        !!            component @f$ \varphi @f$:
        !!             - `commondata::reproduction_cost_body_mass_factor_male`;
        !!             - `commondata::reproduction_cost_body_mass_factor_female`.
        !!             .
        real(SRP), intent(in) :: agent_mass_fact
        !> @return The cost of reproduction.
        real(SRP) :: cost

        ! Using **residual body mass** of the agent after subtracting
        ! the fraction that scales with the offspring mass.
        cost =  offspring_mass * offspring_mass_fact +                        &
                  (this%get_mass() - offspring_mass * offspring_mass_fact)    &
                    * agent_mass_fact

      end function cost_residual

  end function reproduction_cost_energy_dynamic

  !-----------------------------------------------------------------------------
  !> Calculate the costs of unsuccessful reproduction. This is calculated as
  !! a fraction of the normal cost of reproduction returned by the function
  !! `reproduction::reproduction_cost()`. Reproduction can
  !! be unsuccessful for various reasons: insufficient reserves
  !! (reproduction results in starvation death) or stochastic no success.
  function reproduction_cost_unsuccessful_calc (this, cost_factor)            &
                                                      result (repr_cost_mass)
    class(REPRODUCTION), intent(in) :: this
    !> @param[in] cost_factor Optional cost factor multiplier the normal
    !!            cost of reproduction is applied to. If absent, the default
    !!            value set by the `commondata::reproduction_cost_unsuccess`
    !!            parameter is used.
    real(SRP), optional, intent(in) :: cost_factor
    !> @returns Returns the energetic cost of unsuccessful reproduction for
    !!          the 'this' agent.
    real(SRP) :: repr_cost_mass

    !> Unsuccessful reproduction attempt results in a cost,
    !! in terms of the body mass, that is a fraction of the normal cost
    !! of reproduction: the fraction is defined by the parameter
    !! `commondata::reproduction_cost_unsuccess` in `COMMONDATA`.
    !> The body mass of the agent is then reduced to take this fraction of
    !! the full cost of reproduction. This updated value is saved into
    !! the body mass history stack (`update_history` parameter is `TRUE`).
    if(present(cost_factor)) then
      repr_cost_mass = this%reproduction_cost() * cost_factor
    else
      repr_cost_mass = this%reproduction_cost() * REPRODUCTION_COST_UNSUCCESS
    end if

  end function reproduction_cost_unsuccessful_calc

  !-----------------------------------------------------------------------------
  !> Initialise the reproduction object for the agent. Everything is set to
  !! zero.
  elemental subroutine reproduction_init_zero(this)
    class(REPRODUCTION), intent(inout) :: this

    !> Set the total number of reproductions and offspring to zero.
    this%n_reproductions = 0
    this%n_offspring = 0

  end subroutine reproduction_init_zero

  !-----------------------------------------------------------------------------
  !> Get the number of reproductions for this agent.
  elemental function reproduction_n_reproductions_get(this) result (n_repr)
    class(REPRODUCTION), intent(in) :: this
    !> @return The number of reproductions the agent had.
    integer :: n_repr

    n_repr = this%n_reproductions

  end function reproduction_n_reproductions_get

  !-----------------------------------------------------------------------------
  !> Set the number of reproductions for the agent.
  elemental subroutine reproduction_n_reproductions_set(this, n_repr)
    class(REPRODUCTION), intent(inout) :: this
    !> @param[in] n_repr The total number of reproductions for this agent.
    integer, intent(in) :: n_repr

    this%n_reproductions = n_repr

  end subroutine reproduction_n_reproductions_set

  !-----------------------------------------------------------------------------
  !> Get the number of offspring
  elemental function reproduction_n_offspring_get(this) result (n_offspr)
    class(REPRODUCTION), intent(in) :: this
    !> @return The number of offspring the agent had during its lifespan.
    integer :: n_offspr

    n_offspr = this%n_offspring

  end function reproduction_n_offspring_get

  !-----------------------------------------------------------------------------
  !> Set the number of offspring for the agent.
  elemental subroutine reproduction_n_offspring_set(this, n_offspr)
    class(REPRODUCTION), intent(inout) :: this
    !> @param[in] n_offspr The number of offspring to set for this agent.
    integer, intent(in) :: n_offspr

    this%n_offspring = n_offspr

  end subroutine reproduction_n_offspring_set

  !-----------------------------------------------------------------------------
  !> Increment the number of reproductions and offspring for this agent.
  subroutine reproduction_n_increment(this, add_repr, average_mass_offspring)
    class(REPRODUCTION), intent(inout) :: this
    !> @param[in] add_repr Increment the total number of reproductions for
    !!            this agent by this number; if not provided as a dummy
    !!            parameter assume increment by one.
    !!            **Note:** Varying the number of reproductions allows
    !!            implementation of multiple fertilisations by a male,
    !!            resulting in `add_repr`>1 during a single reproduction
    !!            event, provided several females are present in the male's
    !!            perception object. This allows modelling sexual asymmetries.
    integer, optional, intent(in) :: add_repr
    !> @param[in] average_mass_offspring optional average body mass of the
    !!    `       offspring. If not provided, back calculated from the
    !!            Fulton's  condition factor and the body length of the agents
    !!            at init (birth) using the `the_body::length2mass()` function.
    real(SRP), optional, intent(in) :: average_mass_offspring

    ! Local variables
    integer :: add_repr_here

    !> ### Implementation details ###
    !> **First,** check if the `add_repr`  increment parameter is provided.
    !! If not, set it to the default value = 1.
    if (present(add_repr)) then
      add_repr_here = add_repr
    else
      add_repr_here = 1
    end if

    !> **Second,** increment the number of reproductions for this agent (data
    !! component, `reproduction::n_reproductions`) by the increment parameter.
    this%n_reproductions = this%n_reproductions + add_repr_here

    !> **Third,** calculate the number of the offspring
    !! that result from this/these reproduction occurrence(s) and
    !! increment the lifetime number `reproduction::n_offspring` for the
    !! agent respectively. The number of offspring is calculated
    !! using the function `reproduction::offspring_number()`
    !! (`the_body::reproduction_n_offspring_calc()`).
    !! The average mass of the offspring (`average_mass_offspring`), if
    !! provided, transfers into the above backend function.
    if (present(average_mass_offspring)) then
      this%n_offspring = this%n_offspring +                                   &
                         this%offspring_number(average_mass_offspring)        &
                         * add_repr_here
    else
      this%n_offspring = this%n_offspring +                                   &
                         this%offspring_number() * add_repr_here
    end if

  end subroutine reproduction_n_increment

  !-----------------------------------------------------------------------------
  !> Calculate the number of offspring per a single reproduction as a
  !! function of the agent's body mass.
  function reproduction_n_offspring_calc(this, average_mass_offspr)          &
                                                          result(n_offspr)
    class(REPRODUCTION), intent(inout) :: this
    !> @param[in] average_mass_offspr Optional average body mass of the
    !!            offspring (@f$ \mu_{o} @f$, see below).
    real(SRP), optional, intent(in) :: average_mass_offspr
    !> @return Returns the number of offspring per single
    !!         reproduction.
    integer :: n_offspr

    ! Local, total body mass of all the offspring.
    real(SRP) :: total_mass_offspring

    ! Local copies of optionals
    real(SRP) :: average_mass_offspr_here

    !> ### Implementation details ###
    !> Initially check if the average mass of newborn agents @f$ \mu_{o} @f$
    !! is provided as a dummy parameter to this function call. If not,
    !! calculate a guess of the average mass from the Fulton's condition
    !! factor and the body length parameters of the agents at init (birth)
    !! using the `the_body::length2mass()` function.
    if (present(average_mass_offspr)) then
      average_mass_offspr_here = average_mass_offspr
    else
      average_mass_offspr_here = length2mass(ENERGY_INIT, BODY_LENGTH_INIT)
    end if

    !> The number of offspring produced at a single reproduction
    !! scales with the body mass of the agent.
    !> **First,** all the offspring comprise the maximum combined fraction
    !! of the agent's body mass @f$ \phi \cdot M_{agent} @f$, this fraction
    !! @f$ \phi @f$ is obtained by a nonparametric relationship defined by
    !! the the interpolation grid:
    !!   - `commondata::reproduct_body_mass_offspr_abscissa`
    !!   - `commondata::reproduct_body_mass_offspr_ordinate`
    !!   .
    !! The total mass of the offspring (@f$ \phi \cdot M_{agent} @f$) is
    !! calculated in the procedure `reproduction::offspring_mass()` (=>
    !! `the_body::reproduction_mass_offspring_calc()`).
    total_mass_offspring = this%offspring_mass()

    !> **Second,** the number of the offspring with the overall mass
    !! @f$ \phi \cdot M_{agent} @f$ is calculated as the fraction:
    !! @f[ \frac{ \phi \cdot M_{agent} }{ \mu_{o} } , @f] where
    !! @f$ \mu_{o} @f$ is the average mass of a single offspring
    !! at init (birth). The `floor` Fortran intrinsic function is
    !! used to calculate the integer value from this ratio. This
    !! guarantees that the number of offspring returned is the
    !! *lowest*  integer value resulting from the above ratio.
    n_offspr = floor (total_mass_offspring / average_mass_offspr_here)

  end function reproduction_n_offspring_calc

  !-----------------------------------------------------------------------------
  !> Determine if the agent's hormonal system is ready for reproduction.
  elemental function reproduction_ready_steroid_hormones_exceed(this)         &
                                                              result (is_ready)
    class(REPRODUCTION), intent(in) :: this
    !> @return TRUE if the agent is ready to reproduce by sufficiently high
    !!         level of gonadal steroids or FALSE otherwise.
    logical :: is_ready

    !> ### Implementation notes ###
    !> Determine if the agent's hormonal system is ready for reproduction, that
    !! is, its current level of sex steroids @f$ \sigma_{i} @f$ exceeds the
    !! baseline (initially determined by the genome) @f$ \sigma_{0} @f$ by a
    !! factor @f$ \nu @f$ determined by the parameter
    !! commondata::sex_steroids_reproduction_threshold:
    !! @f[ \sigma_{i} > \nu \sigma_{0} . @f]
    !! If the level of sex steroids is insufficient, reproduction is
    !! impossible and FALSE is returned.
    is_ready = .FALSE.
    if (  this%is_male() .and.                                                &
          this%testosterone_get() > this%testosterone_base_get()              &
              * SEX_STEROIDS_REPRODUCTION_THRESHOLD ) then
      is_ready = .TRUE.
    else if (                                                                 &
          this%is_female() .and.                                              &
          this%estrogen_get() > this%estrogen_base_get()                      &
              * SEX_STEROIDS_REPRODUCTION_THRESHOLD ) then
      is_ready = .TRUE.
    end if

  end function reproduction_ready_steroid_hormones_exceed

  !-----------------------------------------------------------------------------
  !> Calculate the total mass of all offspring produced by this agent
  !! during a single reproduction event.
  function reproduction_mass_offspring_calc (this) result (offspr_mass)
    class(REPRODUCTION), intent(in) :: this
    !> @return Total mass of all offspring produced by the agent during a
    !!         specific reproduction event.
    real(SRP) :: offspr_mass

    !> ### Implementation details ###
    !> The total mass of all offspring produced at a single reproduction
    !! scales with the body mass of the agent and is obtained by
    !! a non-parametric relationship involving non-linear interpolation.
    !> The combined offspring mass is calculated as a fraction of the agent's
    !! body mass @f$ M_{agent} @f$ using this equation:
    !! @f[ \phi \cdot M_{agent} , @f] where @f$ \phi @f$ is the fraction
    !! coefficient obtained by a nonparametric relationship defined by
    !! the the interpolation grid
    !!   - `commondata::reproduct_body_mass_offspr_abscissa`
    !!   - `commondata::reproduct_body_mass_offspr_ordinate`
    !!   .
    !! @image html img_doxy_aha-reprod_mass_fract.svg
    !! @image latex img_doxy_aha-reprod_mass_fract.eps "Offspring fraction of body mass" width=14cm
    offspr_mass = this%body_mass *                                            &
                  DDPINTERPOL( REPRODUCT_BODY_MASS_OFFSPR_ABSCISSA,           &
                               REPRODUCT_BODY_MASS_OFFSPR_ORDINATE,           &
                               this%body_mass )

    !> Interpolation plots can be saved in the @ref intro_debug_mode
    !! "debug mode" using this plotting command:
    !! `commondata::debug_interpolate_plot_save()`.
    !! @warning Involves **huge** number of plots, should normally be
    !!          disabled.
    call debug_interpolate_plot_save(                                         &
          grid_xx=REPRODUCT_BODY_MASS_OFFSPR_ABSCISSA,                        &
          grid_yy=REPRODUCT_BODY_MASS_OFFSPR_ORDINATE,                        &
          ipol_value=this%body_mass, algstr="DDPINTERPOL",                    &
          output_file="plot_debug_reprod_mass_fract_" //                      &
                      TOSTR(Global_Time_Step_Model_Current) //                &
                      MMDD // "_a_"// trim(this%individ_label()) //           &
                      "_" // RAND_STRING(LABEL_LENGTH, LABEL_CST,LABEL_CEN)   &
                      // PS )

  end function reproduction_mass_offspring_calc

end module THE_BODY
