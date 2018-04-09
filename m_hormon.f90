!> @file m_hormon.f90
!! The Hormone architecture of the AHA Model.
!! @author Sergey Budaev <sergey.budaev@uib.no>
!! @author Jarl Giske <jarl.giske@uib.no>
!! @date 2016-2017

!-------------------------------------------------------------------------------
! $Id$
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief Definition the hormonal architecture of the agent.
!> @section the_hormones_module THE_HORMONES module
!> Define the hormonal architecture objects. Hormones affect implemented in
!! such a way as to affect decision making and behaviour, but change relatively
!! slowly across the lifespan of the agent. Their initial state is also
!! genetically determined.
module THE_HORMONES

  use COMMONDATA
  use THE_GENOME

  implicit none

  character (len=*), parameter, private :: MODNAME = "(THE_HORMONES)"

  !> This type adds hormonal architecture  extending the genome object
  type, public, extends(INDIVIDUAL_GENOME) :: HORMONES

    !> **growth** hormone increases metabolic rate and growth, has costs
    !!   changes/effects relatively slow and long-term.
    real(SRP) :: growhorm_level
    !> **thyroid** hormone limits growth hormone, has costs
    !!   changes/effects very slow, level very stable, genetically determined.
    real(SRP) :: thyroid_level
    !> **adrenaline** increases general arousal, increases escape
    !!   speed/performance, primes active fear response, primes aggression,
    !!   increases cognitive performance, focus attention, suppresses immune
    !!   system, changes/effects relatively short-term.
    real(SRP) :: adrenaline_level
    !> **cortisol** (HPI axis) linked with higher stress and fear, reduces
    !!   hunger, suppresses immune system, increases blood pressure, reduce
    !!   cognitive performance, changes/effects long-term.
    real(SRP) :: cortisol_level

    !> Gonadal steroids - Sex hormones of males and females
    !!   - we implement them and their effects differently i males and females
    !> **testosterone** - development of male sex characteristics
    !!   increases boldness and aggression, reduces immunity, changes/effects
    !!   relatively short-term
    !! @note use *testosterone* (with *e*), not *testosteron* in the code!
    !! @note single-hormone initialisation function is private, we don't need to
    !!       init single hormones anywhere outside of this module.
    real(SRP) :: testosterone_level
    !> **estrogen** - development of female sex characteristics, suppresses
    !!   immunity, changes/effects relatively short-term.
    real(SRP) :: estrogen_level
    !> The *testosterone* baseline genetically determined level.
    real(SRP) :: testosterone_baseline
    !> The *estrogen* baseline genetically determined level.
    real(SRP) :: estrogen_baseline
    !> History stacks for the gonadal steroids.
    real(SRP), dimension(HISTORY_SIZE_AGENT_PROP) :: testosterone_history
    real(SRP), dimension(HISTORY_SIZE_AGENT_PROP) :: estrogen_history

    contains
      private
      !> Initialise hormone levels based on the genome value.
      !! See `the_hormones::hormones_init_genotype()`
      procedure, public :: init_hormones => hormones_init_genotype
      !> Clean the history stack of hormones.
      !! See `the_hormones::hormones_clean_history_stack()`
      procedure, public :: hormone_history_clean => hormones_clean_history_stack

      !> Get the value of **thyroid**.
      !! See `the_hormones::growhorm_get_level()`
      procedure, public :: growhorm_get => growhorm_get_level
      !> Set the value of **thyroid**.
      !! See `the_hormones::growhorm_set_level()`
      procedure, public :: growhorm_set => growhorm_set_level

      !> Get the value of **thyroid**.
      !! See `the_hormones::thyroid_get_level()`
      procedure, public :: thyroid_get => thyroid_get_level
      !> Set the value of **thyroid**.
      !! See `the_hormones::thyroid_set_level()`
      procedure, public :: thyroid_set => thyroid_set_level

      !> Get the value of **adrenaline**.
      !! See `the_hormones::adrenaline_get_level()`
      procedure, public :: adrenaline_get => adrenaline_get_level
      !> Set the value of **adrenaline**.
      !! See `the_hormones::adrenaline_set_level()`
      procedure, public :: adrenaline_set => adrenaline_set_level

      !> Get the value of **cortisol**.
      !! See `the_hormones::cortisol_get_level()`
      procedure, public :: cortisol_get => cortisol_get_level
      !> Set the value of **cortisol**.
      !! See `the_hormones::cortisol_set_level()`
      procedure, public :: cortisol_set => cortisol_set_level

      !> Get the value of **testosterone**.
      !! See `the_hormones::testosterone_get_level()`
      procedure, public :: testosterone_get => testosterone_get_level
      !> Set the value of **testosterone**.
      !! See `the_hormones::testosterone_set_level()`
      procedure, public :: testosterone_set => testosterone_set_level

      !> Get the value of **estrogen**.
      !! See `the_hormones::estrogen_get_level()`
      procedure, public :: estrogen_get => estrogen_get_level
      !> Set the value of **estrogen**.
      !! See `the_hormones::estrogen_set_level()`
      procedure, public :: estrogen_set => estrogen_set_level

      !> Get the value of testosterone baseline.
      !! See `the_hormones::testosteron_baseline_get_level()`
      procedure, public :: testosterone_base_get =>                           &
                                            testosteron_baseline_get_level
      !> Get the value of estrogen baseline.
      !! See `the_hormones::estrogen_baseline_get_level()`
      procedure, public :: estrogen_base_get => estrogen_baseline_get_level

      !> Calculate the reproductive factor. Reproductive factor is defined as
      !! the current level of the_hormones::testosterone_level in males and
      !! the_hormones::estrogen_level in females.
      !! See `the_hormones::hormones_reproductive_factor_calc()`.
      procedure, public :: reproductive_factor =>                             &
                                            hormones_reproductive_factor_calc

      !> Update the sex steroid hormones history stack from the current level
      !! See `the_hormones::hormones_update_history()`.
      procedure, public :: hormones_to_history => hormones_update_history

  end type HORMONES

contains ! ........ implementation of procedures for this level ................

  !> Initialise hormone levels based on the genome value. Two alleles are
  !! selected at random and input into the `gamma2gene` function to get the
  !! initial hormone values rescaled to 0:1. Note that the `gamma2gene`
  !! alleles defining the **shape** of the gamma function and the **half-max
  !! effect** are selected randomly in this version. Also, polyploid organisms
  !! are possible, in such case, two parameters are also randomly defined from
  !! a larger set (e.g. from four chromosomes in case of tetraploids).
  !! See implementation details and comments for each of the hormones.
  subroutine hormones_init_genotype(this)
    class(HORMONES), intent(inout) :: this

      !> ### Implementation details ###
      !> First, get all the initial hormone level values from the genotype.
      call this%trait_init(this%growhorm_level,                               &
                           GROWHORM_GENOTYPE_PHENOTYPE,                       &
                           GROWHORM_INIT, GROWHORM_GERROR_CV, "GROWHORM")

      call this%trait_init(this%thyroid_level,                                &
                           THYROID_GENOTYPE_PHENOTYPE,                        &
                           THYROID_INIT, THYROID_GERROR_CV, "THYROID")

      call this%trait_init(this%adrenaline_level,                             &
                           ADRENALINE_GENOTYPE_PHENOTYPE,                     &
                           ADRENALINE_INIT, ADRENALINE_GERROR_CV, "ADRENALINE")

      call this%trait_init(this%cortisol_level,                               &
                           CORTISOL_GENOTYPE_PHENOTYPE,                       &
                           CORTISOL_INIT, CORTISOL_GERROR_CV, "CORTISOL")

      call this%trait_init(this%testosterone_level,                           &
                           TESTOSTERONE_GENOTYPE_PHENOTYPE,                   &
                           TESTOSTERONE_INIT, TESTOSTERONE_GERROR_CV,         &
                           "TESTOSTERONE")

      call this%trait_init(this%estrogen_level,                               &
                           ESTROGEN_GENOTYPE_PHENOTYPE,                       &
                           ESTROGEN_INIT, ESTROGEN_GERROR_CV, "ESTROGEN")

      !> Then, initialise the baseline levels of sex steroids from the starting
      !! genetically determined hormone levels.
      this%testosterone_baseline = this%testosterone_level
      this%estrogen_baseline = this%estrogen_level

      !> Clean history stack of all the hormones upon init.
      call this%hormone_history_clean()

      !> Finally, update the hormone history stack with the first init values.
      call add_to_history(this%testosterone_history, this%testosterone_level)
      call add_to_history(this%estrogen_history, this%estrogen_level)

  end subroutine hormones_init_genotype

  !-----------------------------------------------------------------------------
  !> Clean the history stack of hormones: testosterone and estrogen histories
  !! are set to `MISSING`.
  elemental subroutine hormones_clean_history_stack(this)
    class(HORMONES), intent(inout) :: this

    this%testosterone_history = MISSING
    this%estrogen_history = MISSING

  end subroutine hormones_clean_history_stack

  !-----------------------------------------------------------------------------
  !> Update the sex steroid hormones history stack from the current level.
  elemental subroutine hormones_update_history(this)
    class(HORMONES), intent(inout) :: this

    !> Update the hormone history stack with the first init values.
    call add_to_history(this%testosterone_history, this%testosterone_level)
    call add_to_history(this%estrogen_history, this%estrogen_level)

  end subroutine hormones_update_history

  !-----------------------------------------------------------------------------
  !> Calculate the reproductive factor. Reproductive factor is defined as
  !! the current level of the_hormones::testosterone_level in males and
  !! the_hormones::estrogen_level in females.
  !! @note Because the reproductive factor is obtained by sex-specific
  !!       operations directly on the hormones, the use of this function
  !!       is mostly limited to diagnostic outputs.
  elemental function hormones_reproductive_factor_calc(this) result (reprfact)
    class(HORMONES), intent(in) :: this
    !> @return Reproductive factor.
    real(SRP) :: reprfact

    if ( this%is_male() ) then
      reprfact = this%testosterone_level
    else
      reprfact = this%estrogen_level
    end if

  end function hormones_reproductive_factor_calc

  !-----------------------------------------------------------------------------

  !> @name Accessor functions for all the hormones.
  !! Get and set functions for each hormone follow. We left them as
  !! individual  hormone-specific functions duplicating code. Not ideal,
  !! but easy to use provided hormones do not change too often.
  !! Tiny atomic hormone get/set functions are easy to code.
  !! @{

  !-----------------------------------------------------------------------------
  !> Get the value of **growth hormone**.
  elemental function growhorm_get_level (this) result (value_get)
    class(HORMONES), intent(in) :: this

    !> @return value, Returns the value of the **growth hormone**.
    real(SRP) :: value_get

    value_get = this%growhorm_level

  end function growhorm_get_level

  !-----------------------------------------------------------------------------
  !> Set the value of **growth hormone**.
  elemental subroutine growhorm_set_level (this, value_set)
    class(HORMONES), intent(inout) :: this

    !> @param value, Set the value of the **growth hormone**.
    real(SRP), intent(in) :: value_set

    this%growhorm_level = value_set

  end subroutine growhorm_set_level

  !-----------------------------------------------------------------------------
  !> Get the value of **thyroid**.
  elemental function thyroid_get_level (this) result (value_get)
    class(HORMONES), intent(in) :: this

    !> @return value, Returns the value of the **thyroid hormone**.
    real(SRP) :: value_get

    value_get = this%thyroid_level

  end function thyroid_get_level

  !-----------------------------------------------------------------------------
  !> Set the value of **thyroid**.
  elemental subroutine thyroid_set_level (this, value_set)
    class(HORMONES), intent(inout) :: this

    !> @param value, Set the value of the **thyroid hormone**.
    real(SRP), intent(in) :: value_set

    this%thyroid_level = value_set

  end subroutine thyroid_set_level

  !-----------------------------------------------------------------------------
  !> Get the value of **adrenaline**.
  elemental function adrenaline_get_level (this) result (value_get)
    class(HORMONES), intent(in) :: this

    !> @return value, Returns the value of **adrenaline**.
    real(SRP) :: value_get

    value_get = this%adrenaline_level

  end function adrenaline_get_level

  !-----------------------------------------------------------------------------
  !> Set the value of **adrenaline**.
  elemental subroutine adrenaline_set_level (this, value_set)
    class(HORMONES), intent(inout) :: this

    !> @param value, Set the value of the **adrenaline**.
    real(SRP), intent(in) :: value_set

    this%adrenaline_level = value_set

  end subroutine adrenaline_set_level

  !-----------------------------------------------------------------------------
  !> Get the value of **cortisol**.
  elemental function cortisol_get_level (this) result (value_get)
    class(HORMONES), intent(in) :: this

    !> @return value, Returns the value of **cortisol**
    real(SRP) :: value_get

    value_get = this%cortisol_level

  end function cortisol_get_level

  !-----------------------------------------------------------------------------
  !> Set the value of **cortisol**.
  elemental subroutine cortisol_set_level (this, value_set)
    class(HORMONES), intent(inout) :: this

    !> @param value, Set the value of the **cortisol**.
    real(SRP), intent(in) :: value_set

    this%cortisol_level = value_set

  end subroutine cortisol_set_level

  !-----------------------------------------------------------------------------
  !> Get the value of **testosterone**.
  elemental function testosterone_get_level (this) result (value_get)
    class(HORMONES), intent(in) :: this

    !> @return value, Returns the value of **testosterone**
    real(SRP) :: value_get

    value_get = this%testosterone_level

  end function testosterone_get_level

  !-----------------------------------------------------------------------------
  !> Set the value of **testosterone**.
  elemental subroutine testosterone_set_level (this, value_set, update_history)
    class(HORMONES), intent(inout) :: this

    !> @param value_set, Set the value of the **testosterone**.
    real(SRP), intent(in) :: value_set

    !> @param update_history is an optional logical flag to update the hormone
    !!        history stack, the default is to do update, no update only if
    !!        explicitly set to FALSE.
    logical, optional, intent(in) :: update_history

    this%testosterone_level = value_set

    if (present(update_history)) then
      if (.NOT. update_history) return ! Return non-updating history if FALSE.
    end if
    call add_to_history(this%testosterone_history, value_set)

  end subroutine testosterone_set_level

  !-----------------------------------------------------------------------------
  !> Get the value of **estrogen**.
  elemental function estrogen_get_level (this) result (value_get)
    class(HORMONES), intent(in) :: this

    !> @return value, Returns the value of **estrogen**
    real(SRP) :: value_get

    value_get = this%estrogen_level

  end function estrogen_get_level

  !-----------------------------------------------------------------------------
  !> Set the value of **estrogen**.
  elemental subroutine estrogen_set_level (this, value_set, update_history)
    class(HORMONES), intent(inout) :: this

    !> @param value, Set the value of the **estrogen**.
    real(SRP), intent(in) :: value_set

    !> @param update_history is an optional logical flag to update the hormone
    !!        history stack, the default is to do update, no update only if
    !!        explicitly set to FALSE.
    logical, optional, intent(in) :: update_history

    this%estrogen_level = value_set

    if (present(update_history)) then
      if (.NOT. update_history) return ! Return non-updating history if FALSE.
    end if
    call add_to_history(this%estrogen_history, value_set)

  end subroutine estrogen_set_level
  !> @}

  !-----------------------------------------------------------------------------
  !> Get the value of testosterone baseline.
  elemental function testosteron_baseline_get_level (this) result (value_get)
    class(HORMONES), intent(in) :: this

    !> @return value, Returns the value of **testosterone** baseline.
    real(SRP) :: value_get

    value_get = this%testosterone_baseline

  end function testosteron_baseline_get_level

  !-----------------------------------------------------------------------------
  !> Get the value of estrogen baseline.
  elemental function estrogen_baseline_get_level (this) result (value_get)
    class(HORMONES), intent(in) :: this

    !> @return value, Returns the value of **testosterone** baseline.
    real(SRP) :: value_get

    value_get = this%estrogen_baseline

  end function estrogen_baseline_get_level

end module THE_HORMONES
