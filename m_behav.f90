!> @file m_behav.f90
!! The behaviour architecture of the AHA Model.
!! @author Sergey Budaev <sergey.budaev@uib.no>
!! @author Jarl Giske <jarl.giske@uib.no>
!! @date 2016-2017

!-------------------------------------------------------------------------------
! $Id$
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief Definition of high level behavioural architecture
!> @section the_neurobio_module THE_BEHAVIOUR module
!> This module defines the behavioural architecture of the agent, extending
!! the starting neutobiology defined in @ref the_neurobio. Various behavioural
!! actions are implemented that form the behavioural repertoire of the agent.
module THE_BEHAVIOUR

  use COMMONDATA
  use THE_ENVIRONMENT
  use THE_BODY
  use THE_NEUROBIO

  implicit none

  character (len=*), parameter, private :: MODNAME = "(THE_BEHAVIOUR)"

  !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! Define the components of the **behavioural repertoire** of the agent.
  !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  !> Root behaviour abstract type. Several different discrete behaviours
  !! encompass the @ref aha_buildblocks_behaviour "behavioural repertoire" of
  !! the agent. This is the base root type from which all other behaviours
  !! are obtained by inheritance/extension.
  type, abstract, public :: BEHAVIOUR_BASE
    !> Label for the behaviour type.
    character(len=LABEL_LENGTH), private :: label
    !> Logical flag indicating that this behaviour is activated (executed).
    !! @warning Only one behaviour unit can be executed at a time.
    logical :: is_active
    !> Each behavioural type within the whole repertoire has
    !! **expectancies** that set how each of the GOS motivational components
    !! is affected by its execution.
    type(MOTIVATION) :: expectancy
    !> An expectation of the arousal level. It is the maximum weighted
    !! value among all motivation components. This value is actually
    !! minimised -- those behaviour which would result in the lowest
    !! `arousal_expected` is finally executed.
    real(SRP) :: arousal_expected
    contains
      !> Abstract **init** function that has to be overriden by each object
      !! that extends the root behaviour component class.
      procedure(behaviour_init_root), public, deferred :: init
      !> Get the execution status of the behaviour unit.
      !! See `the_behaviour::behaviour_root_get_is_executed()`.
      procedure, public :: is_executed => behaviour_root_get_is_executed
      !> `gos_expected` is an accessor get-function that returns the final GOS
      !! expectation from `expectancies_calculate`. Once we get this value for
      !! all the possible behaviours, we choose what behaviour to execute by
      !! minimising `gos_expected`.
      !! See `the_behaviour::behaviour_root_gos_expectation()`.
      procedure, public :: gos_expected => behaviour_root_gos_expectation
      !> `attention_transfer` transfers attention weights from the actor
      !! agent to this behaviour expectancy objects.
      !! See `the_behaviour::behaviour_root_attention_weights_transfer()`.
      procedure, public :: attention_transfer =>                              &
                                      behaviour_root_attention_weights_transfer

  end type BEHAVIOUR_BASE

  !> Abstract interface for the deferred **init** function that
  !! has to be overriden by each object that extends the basic behavioural
  !! component class.
  abstract interface
    elemental subroutine behaviour_init_root(this)
      import :: BEHAVIOUR_BASE
      class(BEHAVIOUR_BASE), intent(inout) :: this
    end subroutine behaviour_init_root
  end interface

  !> Movement is an umbrella abstract type linked with spatial movement.
  type, abstract, public, extends(BEHAVIOUR_BASE) :: MOVE
    !> Movement is described by its absolute distance.
    !> @note Note that the expected cost of movement is implemented
    !!       separately in each derived class because calculations
    !!       of the movement cost are specific to each (derived)
    !!       behavioural component (e.g. the_behaviour::freeze and
    !!       the_behaviour::go_down_depth.
    real(SRP) :: distance
    contains
      !> The the_behaviour::move::init() is a deferred function that is
      !! overriden by each extension object `init` method.
      procedure(move_init_root), public, deferred :: init
  end type MOVE

  !> Abstract interface for the deferred **init** function that
  !! has to be overriden by each object that extends the basic behavioural
  !! component class.
  abstract interface
    elemental subroutine move_init_root(this)
      import :: MOVE
      class(MOVE), intent(inout) :: this
    end subroutine move_init_root
  end interface

  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! Specific behavioural units that are part of the behavioural repertoire
  ! follow...

  !> **Eat food** is consuming food item(s) perceived.
  type, public, extends(BEHAVIOUR_BASE) :: EAT_FOOD
  !> Increment of the agent's stomach content that results from eating a single
  !! specific food item.
  real(SRP) :: stomach_increment_from_food
  !> Increment of the agent's body mass that results from eating a single
  !! specific food item.
  real(SRP) :: mass_increment_from_food
  contains
    !> `init` inits the behaviour element object.
    !! See `the_behaviour::eat_food_item_init_zero()`.
    procedure, public :: init => eat_food_item_init_zero
    !> `do_this` performs the agent's action without changing the agent or the
    !! environment.
    !! @warning `do_this` is **not** intended to be called directly, only
    !!          from within `expectancies_calculate` and `execute`.
    !! See `the_behaviour::eat_food_item_do_this()`.
    procedure, public :: do_this => eat_food_item_do_this
    !> `expectancies_calculate` is a subroutine (re)calculating motivations
    !! from fake expected perceptions following from `do_this`.
    !! @note Note that this is the computational engine to assess the
    !!       expected GOS of the behaviour, it is called from within
    !!      the base root behaviour class-bound polymorphic `gos_expect`.
    !! See `the_behaviour::eat_food_item_motivations_expect()`.
    procedure, public :: expectancies_calculate =>                            &
                                        eat_food_item_motivations_expect
    !> `execute`performs the action fully, **changing the state** of the agent
    !! and the environment.
    !! See `the_behaviour::eat_food_item_do_execute()`.
    procedure, public :: execute => eat_food_item_do_execute
  end type EAT_FOOD

  !> *Reproduce* is do a single reproduction.
  type, public, extends(BEHAVIOUR_BASE) :: REPRODUCE
    !> Decrement of the agent's current reproductive factor (sex-specific sex
    !! steroids level): testosterone
    real(SRP) :: reprfact_decrement_testosterone
    !> Decrement of the agent's current reproductive factor (sex-specific sex
    !! steroids level): estrogen
    real(SRP) :: reprfact_decrement_estrogen
    !> Decrement of the agent's body mass resulting from this reproduction
    !! event object. The objective value is calculated via the
    !! `appraisal::probability_reproduction()` method.
    real(SRP) :: decrement_mass
    contains
      !> Initialise reproduce behaviour object.
      !! See `the_behaviour::reproduce_init_zero()`.
      procedure, public :: init => reproduce_init_zero
      !> Do reproduce by `this_agent` (the actor agent) given the specific
      !! probability of successful reproduction.
      !! See `the_behaviour::reproduce_do_this()`.
      procedure, public :: do_this => reproduce_do_this
      !> `expectancies_calculate` is a subroutine (re)calculating
      !! motivations from fake expected perceptions following from
      !! `reproduce::do_this()` =>  `the_behaviour::reproduce_do_this()`
      !! procedure. See implementation in
      !! `the_behaviour::reproduce_motivations_expect()`.
      procedure, public :: expectancies_calculate =>                          &
                                                reproduce_motivations_expect
      !> Execute this behaviour component "reproduce" by the `this_agent` agent.
      !! See `the_behaviour::reproduce_do_execute()`.
      procedure, public :: execute => reproduce_do_execute
  end type REPRODUCE

  !> **Walk_random** is a single step of a Gaussian random walk.
  type, public, extends(MOVE) :: WALK_RANDOM
    !> Coefficient of variation for the Gaussian random walk.
    real(SRP) :: distance_cv
    !> The body mass cost of movement; depends on the distance.
    real(SRP) :: expected_cost_moving
    !> The expected food gain (for body mass increment) is determined from
    !! the past history for the random walk.
    real(SRP) :: expected_food_gain
    !> The expected direct food perception in the novel target habitat.
    real(SRP) :: expected_food_dir
    !> The expected direct predation risk is zero for random walk.
    real(SRP) :: expected_pred_dir_risk
    !> The expected general predation risk, i.e. the risk depending on the
    !! current number of predators in both the perception and memory stack.
    real(SRP) :: expected_predation_risk
    contains
      !> Initialise the **walk_random** behaviour component to a zero state.
      !! See `the_behaviour::walk_random_init_zero()`.
      procedure, public :: init => walk_random_init_zero
      !> The "do" procedure component of the behaviour element performs the
      !! behaviour without affecting the actor agent (the_agent) and the world
      !! (here food_item_eaten) which have intent(in), so it only can change
      !! the internal representation of the behaviour (the type to which this
      !! procedure is bound to, here `the_behaviour::walk_random`).
      !! See `the_behaviour::walk_random_do_this()`.
      procedure, public :: do_this => walk_random_do_this
      !> `the_behaviour::walk_random::motivations_expect()` (re)calculates
      !! motivations from fake expected perceptions following from the procedure
      !! `walk_random::do_this()` => `the_behaviour::walk_random_do_this()`.
      !! See `the_behaviour::walk_random_motivations_expect()`.
      procedure, public :: expectancies_calculate =>                          &
                                                walk_random_motivations_expect
      !> Execute this behaviour component "random walk" by `this_agent` agent.
      !! See `the_behaviour::walk_random_do_execute()`.
      procedure, public :: execute => walk_random_do_execute
  end type WALK_RANDOM

  !> **Freeze** is stop any locomotion completely.
  type, public, extends(MOVE) :: FREEZE
    !> The expected food gain (body mass increment) is always zero for
    !! freezing. Although energy costs are also zero.
    real(SRP) :: expected_food_gain
    !> The expected direct predation risk is small and near-zero due to
    !! the function the_environment::predator::risk_fish() getting low
    !! values with is_freezing=TRUE.
    real(SRP) :: expected_pred_dir_risk
    !> The expected general predation risk, i.e. the risk depending on the
    !! current number of predators in both the perception and memory stack.
    !! The expected risk assumes that a freezing predator is not easily
    !! noticed by the roaming predators. So the subjective number of
    !! predators in the perception is zero in the predation_risk_backend()
    !! function.
    real(SRP) :: expected_predation_risk
    contains
    !> Initialise the **freeze** behaviour component to a zero state.
    !! Freeze is a special type of move to a zero distance.
    !! See `the_behaviour::freeze_init_zero()`.
    procedure, public :: init => freeze_init_zero
    !> Do freeze by `this_agent` (the actor agent). Subjective assessment
    !! of the motivational value for this is based on the number of food
    !! items, conspecifics and predators in the perception object.
    !! See `the_behaviour::freeze_do_this()`.
    procedure, public :: do_this => freeze_do_this
    !> `the_behaviour::freeze::motivations_expect()` is a subroutine
    !! (re)calculating motivations from fake expected perceptions from
    !! the procedure `freeze::do_this()` => `the_behaviour::freeze_do_this()`.
    !! See `the_behaviour:: freeze_motivations_expect()`.
    procedure, public ::expectancies_calculate => freeze_motivations_expect
    !> Execute this behaviour component "freeze" by `this_agent` agent.
    !! See `the_behaviour::freeze_do_execute()`.
    procedure, public :: execute => freeze_do_execute
  end type FREEZE

  !> **Escape dart** is a very fast long distance movement, normally in
  !! response to a direct predation threat.
  type, public, extends(MOVE) :: ESCAPE_DART
    !> The expected food gain (body mass increment) is always **null** for
    !! active escape.
    real(SRP) :: expected_food_gain
    !> Expected body mass cost of movement; depends on the distance.
    !! Distance, in turn, should be calculated based on the visual range
    !! detectability of the predator for the agent, in the do_this procedure.
    real(SRP) :: expected_cost_moving
    !> The expected direct predation risk is zero for active escape.
    real(SRP) :: expected_pred_dir_risk
    !> The expected general predation risk, i.e. the risk depending on the
    !! current number of predators in both the perception and memory stack.
    !! The expected risk assumes that a escape moves the agent fully out of
    !! reach of any direct predation risk.
    real(SRP) :: expected_predation_risk
    contains
      !> Initialise the **escape dart** behaviour component to a zero state.
      !! Dart is a quick high speed active escape.
      !! See `the_behaviour::escape_dart_init_zero()`.
      procedure, public :: init => escape_dart_init_zero
      !> Do active escape dart by `this_agent` (the actor agent). Subjective
      !! assessment of the motivational value for this is based on the
      !! distance of escape (in turn, dependent on the visibility of the
      !! predator).
      !! See `the_behaviour::escape_dart_do_this()`.
      procedure, public :: do_this => escape_dart_do_this
      !> `escape_dart::motivations_expect()` is a subroutine (re)calculating
      !! motivations from fake expected perceptions following from
      !! `escape_dart::do_this()` => `the_behaviour::escape_dart_do_this()`.
      !! See `the_behaviour::escape_dart_motivations_expect()
      procedure, public :: expectancies_calculate =>                          &
                                           escape_dart_motivations_expect
      !> Execute this behaviour component "escape" by `this_agent` agent.
      !! See `the_behaviour::escape_dart_do_execute()`.
      procedure, public :: execute => escape_dart_do_execute
  end type ESCAPE_DART

  !> **Approach an arbitrary spatial object** is a directed movement to an
  !! arbitrary the_environment::spatial class target object.
  type, public, extends(MOVE) :: APPROACH
    !> The body mass cost of movement; depends on the distance.
    !> @note Note that such class attributes as `expected_food_gain`,
    !!       `expected_food_gain` `expected_pred_dir_risk`
    !!       `expected_predation_risk` should be implemented in specific
    !!       derived subclasses of the_behaviour::approach, e.g.
    !!       the_behaviour::approach_conspec.
    real(SRP) :: expected_cost_moving
    contains
      !> Initialise the **approach** behaviour component to a zero state.
      !! Approach is a generic type but not abstract.
      !! See `the_behaviour::approach_spatial_object_init_zero()`.
      procedure, public :: init => approach_spatial_object_init_zero
      !> The "do" procedure component of the behaviour element performs the
      !! behaviour without affecting the actor agent (the_agent) and the world
      !! which have intent(in), so it only can change the internal
      !! representation of the behaviour (the type to which this procedure is
      !! bound to, here the_environment::approach).
      !! See `the_behaviour::approach_do_this()`.
      procedure, public :: do_this => approach_do_this
      !> `the_behaviour::approach::expectancies_calculate()` (re)calculates
      !! motivations from fake expected perceptions following from the
      !! procedure `approach::do_this()` => `the_behaviour::approach_do_this()`.
      !! See `the_behaviour::approach_motivations_expect()`.
      procedure, public :: expectancies_calculate => approach_motivations_expect
      !> Execute this behaviour component "approach" by `this_agent` agent.
      !! See `the_behaviour::approach_do_execute()`.
      procedure, public :: execute => approach_do_execute
  end type APPROACH

  !> **Approach conspecifics** is directed movement towards a conspecific.
  !> @note The `execute` method for the_behaviour::approach_conspec uses
  !!       the base class the_behaviour::approach::execute() method.
  type, public, extends(APPROACH) :: APPROACH_CONSPEC
    !> The expected food gain (body mass increment) is always **null** for
    !! active escape.
    real(SRP) :: expected_food_gain
    !> The expected direct predation risk, from the nearest predator.
    real(SRP) :: expected_pred_dir_risk
    !> The expected general predation risk, i.e. the risk depending on the
    !! current number of predators in both the perception and memory stack.
    real(SRP) :: expected_predation_risk
  contains
    !> Initialise the **approach conspecific** behaviour to a zero state.
    !! Approach conspecific is a special extension of the generic
    !! `the_behaviour::approach` behaviour.
    !! See `the_behaviour::approach_conspecifics_init_zero()`.
    procedure, public :: init => approach_conspecifics_init_zero
    !> The "do" procedure component of the behaviour element performs the
    !! behaviour without affecting the actor agent (the_agent) and the world
    !! which have intent(in), so it only can change the internal
    !! representation of the behaviour (the type to which this
    !! procedure is bound to, here `APPROACH_CONSPES`).
    !! See `the_behaviour::approach_conspecifics_do_this()`.
    procedure, public :: do_this => approach_conspecifics_do_this
    !> `the_behaviour::approach_conspec::expectancies_calculate()` (re)calculates
    !! motivations from fake expected perceptions following from the procedure
    !! approach_conspec::do_this().
    !! See `the_behaviour::approach_conspecifics_motivations_expect()`.
    procedure, public :: expectancies_calculate =>                            &
                                  approach_conspecifics_motivations_expect

  end type APPROACH_CONSPEC

  !> **Migrate** is move quickly directing to the other habitat
  type, public, extends(MOVE) :: MIGRATE
    !> Target point (with offset) for migration into the target environment.
    type(SPATIAL) :: target_point
    !> The body mass cost of movement; depends on the distance.
    real(SRP) :: expected_cost_moving
    !> The expected food gain (for body mass increment).
    real(SRP) :: expected_food_gain
    !> The expected direct food perception in the novel target habitat.
    real(SRP) :: expected_food_dir
    !> The expected number of conspecifics at the layer below. This value is
    !! based on the number of conspecifics below the agent's current horizon
    integer :: expected_consp_number
    !> The expected direct predation risk is zero for random walk.
    real(SRP) :: expected_pred_dir_risk
    !> The expected general predation risk, i.e. the risk depending on the
    !! current number of predators in both the perception and memory stack.
    real(SRP) :: expected_predation_risk
  contains
    !> Initialise the **migrate** behaviour component to a zero state.
    !! See `the_behaviour::migrate_init_zero()`.
    procedure, public :: init => migrate_init_zero
    !> The "do" procedure component of the behaviour element performs the
    !! behaviour without affecting the actor agent (the_agent) and the world
    !! (here food_item_eaten) which have intent(in), so it only can change
    !! the internal representation of the behaviour (the type to which this
    !! procedure is bound to, here `MIGRATE`).
    !! See `the_behaviour::migrate_do_this()`.
    procedure, public :: do_this => migrate_do_this
    !> `the_behaviour::migrate::expectancies_calculate()` (re)calculates
    !! motivations from fake expected perceptions following from the procedure
    !! `migrate::do_this()` => `the_behaviour::migrate_do_this()`.
    !! See `the_behaviour::migrate_motivations_expect()`.
    procedure, public :: expectancies_calculate => migrate_motivations_expect
    !> Execute this behaviour component "migrate" by `this_agent` agent.
    !! See `the_behaviour::migrate_do_execute()`.
    procedure, public :: execute => migrate_do_execute
  end type MIGRATE

  !> *Go down* dive deeper.
  type, public, extends(MOVE) :: GO_DOWN_DEPTH
    !> The cost of the swimming downwards. Should be relatively low, much
    !! smaller than the cost of active locomotion to the same distance (in
    !! terms of the body length as set by condition_cost_swimming_burst()).
    !! This is because it is assumed to be based on the hydrodynamic
    !! (swimbladder) volume  manipulation rather than active propulsion.
    real(SRP) :: decrement_mass_cost
    !> The expected food gain (body mass increment) from the food items deeper
    !! than the actor agent. This value is based on the number and average
    !! mass of food items below the agent's current horizon.
    real(SRP) :: expected_food_gain
    !> The expected number of conspecifics at the layer below. This value is
    !! based on the number of conspecifics below the agent's current horizon
    integer :: expected_consp_number
    !> The expected predation risk at the layer below. This value is based on
    !! the number of predators below the agent's current horizon.
    real(SRP) :: expected_predation_risk
    contains
      !> Initialise the **go down to a deeper spatial layer** behaviour
      !! component to a zero state.
      !! See `the_behaviour::go_down_depth_init_zero()`.
      procedure, public :: init => go_down_depth_init_zero
      !> `do_this` performs the agent's action without changing the agent
      !! or the environment.
      procedure, public :: do_this => go_down_do_this
      !> `expectancies_calculate` is a subroutine (re)calculating motivations
      !! from fake expected perceptions following from `do_this`.
      !! @note Note that this is the computational engine to assess the
      !!       expected GOS of the behaviour, it is called from within
      !!      the base root behaviour class-bound polymorphic `gos_expect`.
      !! See `the_behaviour::go_down_motivations_expect()`.
      procedure, public :: expectancies_calculate => go_down_motivations_expect
      !> Execute this behaviour component "go down" by `this_agent` agent.
      !! See `the_behaviour::go_down_do_execute()`.
      procedure, public :: execute => go_down_do_execute
  end type GO_DOWN_DEPTH

  !> *Go up* raise to a smaller depth.
  !! TODO: abstract type linking both Up and Down.
  type, public, extends(MOVE) :: GO_UP_DEPTH
    !> The cost of the swimming downwards. Should be relatively low, much
    !! smaller than the cost of active locomotion to the same distance (in
    !! terms of the body length as set by condition_cost_swimming_burst()).
    !! This is because it is assumed to be based on the hydrodynamic
    !! (swimbladder) volume  manipulation rather than active propulsion.
    real(SRP) :: decrement_mass_cost
    !> The expected food gain (body mass increment) from the food items deeper
    !! than the actor agent. This value is based on the number and average
    !! mass of food items below the agent's current horizon.
    real(SRP) :: expected_food_gain
    !> The expected number of conspecifics at the layer below. This value is
    !! based on the number of conspecifics below the agent's current horizon
    integer :: expected_consp_number
    !> The expected predation risk at the layer below. This value is based on
    !! the number of predators below the agent's current horizon.
    real(SRP) :: expected_predation_risk
    contains
      !> Initialise the **go up to a shallower spatial layer** behaviour
      !! component to a zero state.
      !! See `the_behaviour::go_up_depth_init_zero()`.
      procedure, public :: init => go_up_depth_init_zero
      !> `do_this` performs the agent's action without changing the agent
      !! or the environment.
      !! See `the_behaviour::go_up_do_this()`.
      procedure, public :: do_this => go_up_do_this
      !> `expectancies_calculate` is a subroutine (re)calculating motivations
      !! from fake expected perceptions following from `do_this`.
      !! @note Note that this is the computational engine to assess the
      !!       expected GOS of the behaviour, it is called from within
      !!      the base root behaviour class-bound polymorphic `gos_expect`.
      !! See `the_behaviour::go_up_motivations_expect()`.
      procedure, public :: expectancies_calculate => go_up_motivations_expect
      !> Execute this behaviour component "go up" by `this_agent` agent.
      !! See `the_behaviour::go_up_do_execute()`.
      procedure, public :: execute => go_up_do_execute
  end type GO_UP_DEPTH

  !> This is a test fake behaviour unit that is used only for debugging.
  !! It cannot be "execute"'d, but the expectancy can be calculated (normally
  !! in the @ref intro_debug_mode "debug mode").
  type, private, extends(BEHAVIOUR_BASE) :: DEBUG_BASE
    contains
      procedure, public :: init => debug_base_init_zero
      procedure, public ::expectancies_calculate =>                           &
                                              debug_base_motivations_expect
  end type DEBUG_BASE

  !-----------------------------------------------------------------------------
  !> The behaviour of the agent is defined by the the_behaviour::behaviour
  !! class. This class defines the *behavioural repertoire* of the agent.
  !! Each of the components of the behavioural repertoire (behaviour object)
  !! is defined as a separate independent class with its own *self* parameter.
  !! However, the agent which performs the behaviour (the *actor agent*) is
  !! included as the first non-self parameter into the behaviour component
  !! methods.
  !!
  !! For example, there is a behaviour component the_behaviour::eat_food that
  !! defines the feeding behaviour of the agent. The method that calculates
  !! the basic (and expected) outputs from this behaviour (i.e. "does" it)
  !! the_behaviour::eat_food::do_this() includes the actor agent as the first
  !! non-self (non-`this`) parameter. The same is true for all other methods
  !! of the the_behaviour::eat_food class:
  !! the_behaviour::eat_food::expectancies_calculate() and
  !! the_behaviour::execute().
  !!
  !! Thus, the many individual classes define the behavioural repertoire:
  !! - the_behaviour::eat_food;
  !! - the_behaviour::reproduce;
  !! - the_behaviour::walk_random;
  !! - the_behaviour::freeze;
  !! - the_behaviour::escape_dart;
  !! - the_behaviour::approach;
  !! - the_behaviour::approach_conspec;
  !! - the_behaviour::migrate;
  !! - the_behaviour::go_down_depth;
  !! - the_behaviour::go_up_depth.
  !! .
  !! However the_behaviour::behaviour unites all these classes together and
  !! plugs them into the agent class hierarchy. An overview of the behavioural
  !! repertoire is found @ref aha_buildblocks_behaviour "here".
  type, public, extends(GOS_GLOBAL) :: BEHAVIOUR
    !> Parameters that set the parameters of the behaviours and
    !! their **expectancies** (perceived consequences).
    type(EAT_FOOD)          :: eat
    type(REPRODUCE)         :: reproduce
    type(WALK_RANDOM)       :: walk_random
    type(FREEZE)            :: freeze
    type(ESCAPE_DART)       :: escape_dart
    type(APPROACH)          :: approach_spatial
    type(APPROACH_CONSPEC)  :: approach_conspec
    type(MIGRATE)           :: migrate
    type(GO_DOWN_DEPTH)     :: depth_down
    type(GO_UP_DEPTH)       :: depth_up
    type(DEBUG_BASE)        :: debug_base
    !> Overall label of the behaviour being executed. It is used
    !! only for outputs.
    character(len=LABEL_LENGTH) :: behaviour_label
    !> @name Indicator and debugging variables.
    !! @anchor behav_debug_indicators
    !> @{
    !> A history stack of behaviours (labels) that have been executed.
    character(len=LABEL_LENGTH), dimension(HISTORY_SIZE_BEHAVIOURS) ::        &
                                                                history_behave
    !> An indicator showing the cumulative count of the_behaviour::eat_food
    !! attempts (notwithstanding successful or failures).
    integer :: n_eats_all_indicator
    !> An indicator showing the cumulative count of the food items eaten.
    integer :: n_eaten_indicator
    !> An indicator showing the cumulative mass of the food items eaten.
    real(SRP) :: mass_eaten_indicator
    !> @}
    !> The subroutines contained define what the agent really does, i.e.
    !! implements the actual behavioural repertoire components.
    contains
      !> Initialise the behaviour components of the agent, the
      !! the_behaviour::behaviour class. See
      !! `the_behaviour::behaviour_whole_agent_init()`.
      procedure, public :: init_behaviour => behaviour_whole_agent_init
      !> Cleanup the behaviour history stack for the agent.
      !! See `the_behaviour::behaviour_cleanup_history()`.
      procedure, public :: cleanup_behav_history => behaviour_cleanup_history
      !> Deactivate all behaviour units that compose the behaviour repertoire
      !! of the agent. See `the_behaviour::behaviour_whole_agent_deactivate()`.
      procedure, public :: deactivate => behaviour_whole_agent_deactivate
      !> Obtain the label of the currently executing behaviour for the `this`
      !! agent. See `the_behaviour::behaviour_get_behaviour_label_executing()`.
      procedure, public :: behaviour_is =>                                    &
                                      behaviour_get_behaviour_label_executing
      !> Select the optimal food item among (possibly) several ones that are
      !! available in the **perception object** of the agent.
      !! See `the_behaviour::behaviour_select_food_item()`.
      procedure, public :: food_item_select => behaviour_select_food_item
      !> Select the optimal conspecific among (possibly) several ones that are
      !! available in the **perception object** of the agent.
      !! See `the_behaviour::behaviour_select_conspecific()`.
      procedure, public :: consp_select => behaviour_select_conspecific
      !> Select the nearest food item among (possibly) several ones that are
      !! available in the perception object.
      !! See `the_behaviour::behaviour_select_food_item_nearest()`.
      procedure, public :: food_item_select_nearest =>                        &
                                            behaviour_select_food_item_nearest
      !> Select the nearest conspecific among (possibly) several ones that are
      !! available in the perception object.
      !! See `the_behaviour::behaviour_select_conspecific_nearest()`.
      procedure, public :: consp_select_nearest =>                            &
                                          behaviour_select_conspecific_nearest

      !> Eat a food item(s) that are found in the perception object.
      !! See `the_behaviour::behaviour_do_eat_food_item()`.
      procedure, public :: do_eat_food_item => behaviour_do_eat_food_item
      !> Reproduce based on the `this` agent's current state.
      !! See `the_behaviour::behaviour_do_reproduce()`.
      procedure, public :: do_reproduce => behaviour_do_reproduce
      !> Perform a random Gaussian walk to a specific average distance with
      !! certain variance (set by the CV).
      !! See `the_behaviour::behaviour_do_walk()`.
      procedure, public :: do_walk => behaviour_do_walk
      !> Perform (execute) the the_behaviour::freeze behaviour.
      !! See `the_behaviour::behaviour_do_freeze()`.
      procedure, public :: do_freeze => behaviour_do_freeze
      !> Perform (execute) the the_behaviour::escape_dart behaviour.
      !! See `the_behaviour::behaviour_do_escape_dart()`.
      procedure, public :: do_escape => behaviour_do_escape_dart
      !> Approach a specific the_environment::spatial class target, i.e. execute
      !! the the_behaviour::approach behaviour.
      !! See `the_behaviour::behaviour_do_approach()`.
      procedure, public :: do_approach => behaviour_do_approach
      !> Perform (execute) the the_behaviour::migrate (migration) behaviour.
      !! See `the_behaviour::behaviour_do_migrate()`.
      procedure, public :: do_migrate => behaviour_do_migrate
      !> Perform a simplistic random migration. If the agent is within a specific
      !! distance to the target environment, it emigrates there with a specific
      !! fixed probability.
      !! See `the_behaviour::behaviour_try_migrate_random()`.
      procedure, public :: migrate_random => behaviour_try_migrate_random
      !> Perform (execute) the the_behaviour::go_down_depth (go down) behaviour.
      !! See `the_behaviour::behaviour_do_go_down()`.
      procedure, public :: do_go_down => behaviour_do_go_down
      !> Perform (execute) the the_behaviour::go_up_depth (go up) behaviour.
      !! See `the_behaviour::behaviour_do_go_up()`.
      procedure, public :: do_go_up => behaviour_do_go_up
      !> Select and execute the optimal behaviour, i.e. the behaviour which
      !! minimizes the expected GOS arousal.
      !! See `the_behaviour::behaviour_select_optimal()`.
      !! - There is a different behaviour selection backend procedure that
      !!   depends on the current GOS:
      !!   `the_behaviour::behaviour_select_fixed_from_gos`. This procedure
      !!   selects a specific fixed behaviour unit at specific GOS.
      !! .
      procedure, public :: do_behave => behaviour_select_optimal
  end type BEHAVIOUR

  !> This type is an "umbrella" for all the lower-level classes.
  type, public, extends(BEHAVIOUR) :: ARCHITECTURE_NEURO
    contains
      private
      !> Initialise neuro-biological architecture.
      !! See `the_behaviour::neurobio_init_components()` for implementation.
      procedure, public :: init_neurobio => neurobio_init_components
  end type ARCHITECTURE_NEURO

  ! Implementation procedures for all "init" methods are private.
  private :: behaviour_whole_agent_init, neurobio_init_components

contains ! ........ implementation of procedures for this level ................

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with BEHAVIOURAL COMPONENTS and their expectancies.
  ! base root behavioural (abstract) class.
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Transfer attention weights from the actor agent to the behaviour's
  !! GOS expectancy object. At this stage, attention weights for **this**
  !! behaviour's expectancy motivational state components are copied from
  !! the actor agent's (`this_agent`) main motivational components' attention
  !! weights.
  !! @note The `associate` construct makes it easier to write all possible
  !!       combinations, so there is little need to implement motivation-state
  !!       specific attention transfer functions separately. Here in the below
  !!       `associate` constructs `EX` is the this **expectancy** class root
  !!       and `AG` is the **actor agent** class root.
  !! @note Attention transfer routine cannot be conveniently placed into the
  !!       `STATE_MOTIVATION_BASE` because specific motivation states
  !!       (hunger,...) are still unavailable at this level, but
  !!       we are intended to get access to specific motivational state
  !!       of the actor agent. The `state_motivation_attention_weights_transfer`
  !!       procedure in `STATE_MOTIVATION_BASE` class just implements attention
  !!       weights transfer across two **motivation state** root class objects.
  !!       Even so, we still would need this function here calling specific
  !!       motivation object-bound versions. However, this is more complicated
  !!       than just a single subroutine as implemented here for the
  !!       `BEHAVIOUR_BASE`. Anyway, we only really copy attention weights for
  !!       all motivation states in a single batch here and never need it
  !!       elsewhere.
  pure subroutine behaviour_root_attention_weights_transfer (this, this_agent)
    class(BEHAVIOUR_BASE), intent(inout) :: this
    class(APPRAISAL), intent(in) :: this_agent

    !> @note We have to include all the motivation state components that
    !!       are found in the `MOTIVATION` class, hunger, fear_defence etc.

    !> Transfer attention weights for **hunger**.
    !! @note The `STATE_MOTIVATION_BASE` bound procedure that implements this
    !!       attention transfer is:
    !!       `call this%expectancy%hunger%attention_copy(                     &
    !!                                          this_agent%motivations%hunger)`
    HUNGER: associate(  EX=>this%expectancy%hunger%attention_weight,          &
                 AG=>this_agent%motivations%hunger%attention_weight  )
      EX%light =    AG%light
      EX%depth =    AG%depth
      EX%food_dir = AG%food_dir
      EX%food_mem = AG%food_mem
      EX%conspec =  AG%conspec
      EX%pred_dir = AG%pred_dir
      EX%predator = AG%predator
      EX%stomach =  AG%stomach
      EX%bodymass = AG%bodymass
      EX%energy =   AG%energy
      EX%age =      AG%age
      EX%reprfac =  AG%reprfac
    end associate HUNGER

    !> Transfer attention weights for **fear_defence**.
    !! @note The `STATE_MOTIVATION_BASE` bound procedure that implements this
    !!       attention transfer is:
    !!       `call this%expectancy%fear_defence%attention_copy(               &
    !!                                    this_agent%motivations%fear_defence)`
    A_ACTIVE: associate(  EX=>this%expectancy%fear_defence%attention_weight,  &
                   AG=>this_agent%motivations%fear_defence%attention_weight  )
      EX%light =    AG%light
      EX%depth =    AG%depth
      EX%food_dir = AG%food_dir
      EX%food_mem = AG%food_mem
      EX%conspec =  AG%conspec
      EX%pred_dir = AG%pred_dir
      EX%predator = AG%predator
      EX%stomach =  AG%stomach
      EX%bodymass = AG%bodymass
      EX%energy =   AG%energy
      EX%age =      AG%age
      EX%reprfac =  AG%reprfac
    end associate A_ACTIVE

    !> Transfer attention weights for **reproduction**.
    !! @note The `STATE_MOTIVATION_BASE` bound procedure that implements this
    !!       attention transfer is:
    !!       `call this%expectancy%reproduction%attention_copy(               &
    !!                                    this_agent%motivations%reproduction)`
    REPROD: associate(  EX=>this%expectancy%reproduction%attention_weight,  &
                 AG=>this_agent%motivations%reproduction%attention_weight  )
      EX%light =    AG%light
      EX%depth =    AG%depth
      EX%food_dir = AG%food_dir
      EX%food_mem = AG%food_mem
      EX%conspec =  AG%conspec
      EX%pred_dir = AG%pred_dir
      EX%predator = AG%predator
      EX%stomach =  AG%stomach
      EX%bodymass = AG%bodymass
      EX%energy =   AG%energy
      EX%age =      AG%age
      EX%reprfac =  AG%reprfac
    end associate REPROD

  end subroutine behaviour_root_attention_weights_transfer

  !-----------------------------------------------------------------------------
  !> Accessor get-function for the final expected GOS arousal from this
  !! behaviour. All calculations for are done in `expectancies_calculate` for
  !! the specific behaviour unit.
  elemental function behaviour_root_gos_expectation(this) result (gos_expected)
    class(BEHAVIOUR_BASE), intent(in)  :: this  !< @param this self.
    !<  @returns Expected GOS arousal level if **this** behaviour is executed.
    real(SRP) :: gos_expected

    gos_expected = this%arousal_expected

  end function behaviour_root_gos_expectation

  !-----------------------------------------------------------------------------
  !> Get the execution status of the behaviour unit. If TRUE, the unit is
  !! currently active and is being executed. This is the "getter" for
  !! the_behaviour::behaviour_base::is_active
  elemental function behaviour_root_get_is_executed(this) result (is_exec)
    class(BEHAVIOUR_BASE), intent(in) :: this
    !> @return TRUE, the behaviour unit is currently active and is being
    !!         executed; FALSE otherwise.
    logical :: is_exec

    is_exec = this%is_active

  end function behaviour_root_get_is_executed

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with SPECIFIC BEHAVIOURAL COMPONENTS.
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Initialise the **eat food item** behaviour component to a zero state.
  elemental subroutine eat_food_item_init_zero(this)
    class(EAT_FOOD), intent(inout) :: this

    !> First init components from the base root class
    !! `the_behaviour::behaviour_base`:
    !! Mandatory label component that should be read-only.
    this%label = "EAT_FOOD"
    !> The execution status is always FALSE, can be reset to TRUE only when
    !! the behaviour unit is called to execution.
    this%is_active = .FALSE.

    !> And the *expectancy* type components.
    call this%expectancy%init()
    !> And init the expected arousal data component.
    this%arousal_expected = 0.0_SRP

    !> Second, init components of this specific behaviour (`EAT_FOOD`)
    !! component extended class.
    !! @note Note that we initialise increments to 0.0, not MISSING as
    !!       increments will be later added. And several items can be added
    !!       consecutively.
    this%stomach_increment_from_food = 0.0_SRP
    this%mass_increment_from_food = 0.0_SRP

  end subroutine eat_food_item_init_zero

  !-----------------------------------------------------------------------------
  !> Initialise the **walk_random** behaviour component to a zero state.
  elemental subroutine walk_random_init_zero(this)
    class(WALK_RANDOM), intent(inout) :: this

    !> First, initialise components from the base root class
    !! `the_behaviour::behaviour_base`.
    !> Mandatory label component that should be read-only.
    this%label = "WALK_RANDOM"
    !> The execution status is always FALSE, can be reset to TRUE only when
    !! the behaviour unit is called to execution.
    this%is_active = .FALSE.

    !> And the *expectancy* components.
    call this%expectancy%init()
    this%arousal_expected = 0.0_SRP

    !> Abstract `MOVE` component.
    this%distance = MISSING

    !> Second, init components of this specific behaviour (`WALK_RANDOM`).
    this%distance_cv = MISSING
    this%expected_cost_moving = MISSING
    this%expected_food_gain = MISSING
    this%expected_food_dir = MISSING
    this%expected_pred_dir_risk = MISSING
    this%expected_predation_risk = MISSING

  end subroutine walk_random_init_zero

  !-----------------------------------------------------------------------------
  !> Initialise the **freeze** behaviour component to a zero state.
  !! Freeze is a special type of move to a zero distance / zero speed.
  elemental subroutine freeze_init_zero(this)
    class(FREEZE), intent(inout) :: this

    !> First init components from the base root class
    !! `the_behaviour::behaviour_base`.
    !> Mandatory label component that should be read-only.
    this%label = "FREEZE"
    !> The execution status is always FALSE, can be reset to TRUE only when
    !! the behaviour unit is called to execution.
    this%is_active = .FALSE.

    !> And the *expectancy* components.
    call this%expectancy%init()
    this%arousal_expected = 0.0_SRP

    !> Abstract `MOVE` component.
    this%distance = 0.0_SRP

    !> Second, init components of this specific behaviour (`FREEZE`).
    this%expected_food_gain = 0.0_SRP
    this%expected_pred_dir_risk = MISSING
    this%expected_predation_risk = MISSING

  end subroutine freeze_init_zero

  !-----------------------------------------------------------------------------
  !> Do freeze by `this_agent` (the actor agent). Subjective assessment of the
  !! motivational value for this is based on the number of food items,
  !! conspecifics and predators in the perception object.
  subroutine freeze_do_this(this, this_agent)
    !> @param[inout] this the object itself.
    class(FREEZE), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which goes down.
    class(APPRAISAL), intent(in) :: this_agent

    ! **WEIGHT_DIRECT**  is the relative weight  given to the immediate
    ! perception of predators over the predators counts in the memory stack.
    ! Obtained from global parameters
    ! (`commondata::predation_risk_weight_immediate`).
    real(SRP), parameter :: WEIGHT_DIRECT = PREDATION_RISK_WEIGHT_IMMEDIATE

    ! **MEM_WIND** is the size of the memory window when assessing the
    ! predator risk, only this number of the latest elements from the memory
    ! stack is taken into account. So we further weight the direct threat
    ! over the background risk when making the decision.
    ! @note  Note that we take into account the whole memory size
    !        (commondata::history_size_perception).
    integer, parameter :: MEM_WIND = HISTORY_SIZE_PERCEPTION

    !> ### Implementation details ###
    !> The expected food gain for freezing is zero as immobile agent
    !! does not eat.
    this%expected_food_gain = 0.0_SRP

    !> Calculate the expected direct risk of predation that is based on the
    !! distance to the nearest predator. However, a version of the
    !! the_neurobio::perception::risk_pred() procedure for freezing/immobile
    !! agent is used here.
    this%expected_pred_dir_risk = this_agent%risk_pred( is_freezing=.TRUE. )

    !> Calculate the expected predation risk for the immobile agent. It is
    !! assumed that predators that are roaming nearby cannot easily detect an
    !! immobile/freezing agent as long as it does not move (freezing here has
    !! significant similarity with sheltering). Therefore, the expectancy is
    !! based on a (subjective) **zero** count of the number of predators in
    !! the agent's perception object and normal risk component based on the
    !! predators in the memory stack. The calculation is done by the standard
    !! `the_neurobio::predation_risk_backend()` function.
    !! Thus, the resulting general risk is calculated as:
    !! @f[ R = 0 + r_{id} \cdot (1 - \omega) , @f]
    !! where @f$ r_{id} @f$) is the average number of predators in the latest
    !! memory stack and @f$ \omega @f$ is the weighting factor for the actual
    !! number of predators (that is zero in this case).
    this%expected_predation_risk =                                            &
        predation_risk_backend(                                               &
            pred_count=0,                                                     &
            pred_memory_mean=this_agent%memory_stack%get_pred_mean(MEM_WIND), &
            weight_direct=WEIGHT_DIRECT )

  end subroutine freeze_do_this

  !-----------------------------------------------------------------------------
  !> `the_behaviour::freeze::motivations_expect()` (re)calculates
  !! motivations from fake expected perceptions following from the procedure
  !! `freeze::do_this()` => `the_behaviour::freeze_do_this()`.
  subroutine freeze_motivations_expect(this, this_agent,                      &
                                    time_step_model, rescale_max_motivation)
    !> @param[inout] this the object itself.
    class(FREEZE), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which does freezing.
    class(APPRAISAL), intent(in)    :: this_agent
    !> @param [in] time_step_model optional time step of the model,
    !!             **overrides** the value calculated from the spatial data.
    integer, optional, intent(in)   :: time_step_model
    !> @param[in] rescale_max_motivation optional maximum motivation value for
    !!            rescaling all motivational components for comparison
    !!            across all motivation and perceptual components and behaviour
    !!            units.
    real(SRP), optional, intent(in) :: rescale_max_motivation

    ! Local copy of optional model time step
    integer :: time_step_model_here

    ! Local variables
    real(SRP) :: max_motivation ! Local max. over all motivation components.

    ! The actor agent's current stomach contents.
    real(SRP) :: agent_stomach

    !> ### Notable local variables ###
    !> #### Perception overrides ####
    !> - **expect_pred_dir** is the expected direct predation risk; it is zero.
    real(SRP) :: expect_pred_dir
    !> - **expect_predator** is the expected general predation risk, that is
    !!   based on a weighting of the current predation and predation risk
    !!   from the memory stack.
    real(SRP) :: expect_predator
    !> - **expect_stomach** is the expected stomach contents as a consequence
    !!   of freezing. Note that there is no food consumption while freezing.
    real(SRP) :: expect_stomach
    !> - **expect_bodymass** is the expected body mass as a consequence of
    !!   freezing. Notably, it subtracts a small living cost component.
    real(SRP) :: expect_bodymass
    !> - **expect_energy** is the expected energy reserves as a consequence
    !!   of the freezing. Calculated from the body mass and weight.
    !! .
    real(SRP) :: expect_energy

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME =                                 &
                                          "(freeze_motivations_expect)"

    !> #### Checks and preparations ####
    !> Check optional time step parameter. If not provided, use global
    !! parameter value from commondata::global_time_step_model_current.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> #### Call do_this ####
    !> As the first step, we use the **do**-procedure `freeze::do_this()`
    !! => `the_behaviour::freeze_do_this()` to perform the behaviour desired
    !! without changing either the agent or its environment, obtaining  the
    !! **subjective** values of the `this` behaviour components that later feed
    !! into the motivation **expectancy** functions:
    !!  - `perception_override_pred_dir`
    !!  - `perception_override_predator`
    !!  - `perception_override_stomach`
    !!  - `perception_override_bodymass`
    !!  - `perception_override_energy`
    !!  .
    call this%do_this(this_agent = this_agent)

    !> #### Calculate expected (fake) perceptions ####
    !> First, calculate the expected stomach contents, body mass and energy
    !! reserves out of the fixed zero food gain that is returned from the
    !! `do_this` procedure.
    !> - Obtain the agent's current stomach contents.
    agent_stomach = this_agent%get_stom_content()

    !> - Calculate the expected stomach content, which is decremented by the
    !!   expected digestion value (the_body::stomach_emptify_backend()).
    expect_stomach = max( ZERO,                                               &
                          agent_stomach -                                     &
                                stomach_emptify_backend(agent_stomach) )

    !> - Calculate the expected body mass of the agent as a consequence of
    !!   freezing. The body mass is decremented by a small value of the
    !!   living cost (the_body::body_mass_calculate_cost_living_step()).
    expect_bodymass = max( ZERO,                                              &
                           this_agent%get_mass() - this_agent%living_cost() )

    !> - The expected energy reserves are calculated from the fake
    !!   perceptions of the body mass and the current length (it does not
    !!   change as food intake is zero in case of freezing) using
    !!   the_body::energy_reserve() function.
    !! .
    expect_energy = energy_reserve( expect_bodymass, this_agent%length() )

    !> Second, transfer the predation risk expectancies from the freezing
    !! class object to the dedicated override perception variables (their
    !! final values are calculated in `do_this`).
    expect_pred_dir = this%expected_pred_dir_risk
    expect_predator = this%expected_predation_risk

    !> #### Calculate motivation expectancies ####
    !> The next step is to calculate the motivational expectancies using the
    !! fake perceptions to override the default (actual agent's) values.
    !> At this stage, first, calculate motivation values resulting from the
    !! behaviour done (`freeze::do_this()` ) at the previous steps: what
    !! would be the motivation values *if* the agent does perform
    !! FREEZE? Technically, this is done by calling the **neuronal
    !! response function**, `percept_components_motiv::motivation_components()`
    !! method, for each of the motivational states with `perception_override_`
    !! dummy parameters overriding the default values.
    !! Here is the list of the fake overriding perceptions for the
    !! FREEZE behaviour:
    !!  - `perception_override_pred_dir`
    !!  - `perception_override_predator`
    !!  - `perception_override_stomach`
    !!  - `perception_override_bodymass`
    !!  - `perception_override_energy`
    !!  .
    !  @note  **Expectancy** assessment for **hunger** motivation, using
    !         `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in] for
    !         `this_agent` now.
    call this%expectancy%hunger%percept_component%motivation_components       &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL,         &
      param_gp_matrix_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL,     &
      param_gp_matrix_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL,     &
      param_gp_matrix_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL,               &
      param_gp_matrix_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL,             &
      param_gp_matrix_age      = AGE_HUNGER_GENOTYPE_NEURONAL,                &
      param_gp_matrix_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL,            &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,     &
      param_gerror_cv_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_age      = AGE_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,      &
      param_gerror_cv_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,  &
      !  Real agent perception components are now substituted by the *fake*
      !  values resulting from executing this behaviour (`do_this` method).
      !  This is repeated for all the motivations: *hunger*,
      !  *fear state* etc.
      perception_override_pred_dir = expect_pred_dir,                         &
      perception_override_predator = expect_predator,                         &
      perception_override_stomach = expect_stomach,                           &
      perception_override_bodymass = expect_bodymass,                         &
      perception_override_energy = expect_energy                              &
                                                                              )
      !> Real agent perception components are now substituted by the *fake*
      !! values resulting from executing this behaviour (`reproduce::do_this()`
      !! => `the_behaviour::reproduce_do_this()` method). This is repeated for
      !! all the motivations: *hunger*, *fear state* etc. These optional
      !! **override parameters** are substituted by the "fake" values.

    !  @note  **Expectancy** assessment for **fear_defence** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%fear_defence%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL, &
      param_gp_matrix_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_pred_dir = expect_pred_dir,                         &
      perception_override_predator = expect_predator,                         &
      perception_override_stomach = expect_stomach,                           &
      perception_override_bodymass = expect_bodymass,                         &
      perception_override_energy = expect_energy                              &
                                                                              )

    !  @note  **Expectancy** assessment for **reproduction** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%reproduction%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL,    &
      param_gp_matrix_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_pred_dir = expect_pred_dir,                         &
      perception_override_predator = expect_predator,                         &
      perception_override_stomach = expect_stomach,                           &
      perception_override_bodymass = expect_bodymass,                         &
      perception_override_energy = expect_energy                              &
                                                                              )

    !> #### Calculate primary and final motivations ####
    !> Next, from the perceptual components calculated at the previous
    !! step we can obtain the **primary** and **final motivation** values by
    !! weighed summing.
    if (present(rescale_max_motivation)) then
      !> Here we can use global maximum motivation across all behaviours and
      !! perceptual components if it is provided, for rescaling.
      max_motivation = rescale_max_motivation
    else
      !> Or can rescale using local maximum value for this behaviour only.
      max_motivation =  this%expectancy%max_perception()
    end if

    !> Transfer attention weights from the actor agent `this_agent` to the
    !! `this` behaviour component. So, we will now use the updated modulated
    !! attention weights of the agent rather than their default parameter
    !! values.
    call this%attention_transfer(this_agent)

    !> So the primary motivation values are calculated.
    call this%expectancy%motivation_primary_calc(max_motivation)

    !> Primary motivations are logged in the @ref intro_debug_mode "debug mode".
    call LOG_DBG( LTAG_INFO // "Primary motivations: " //                   &
                  "hunger: " //                                             &
                    TOSTR(this%expectancy%hunger%motivation_prim)  //       &
                  ", fear_defence: " //                                     &
                    TOSTR(this%expectancy%fear_defence%motivation_prim) //  &
                  ", reproduce: " //                                        &
                    TOSTR(this%expectancy%reproduction%motivation_prim),    &
                  PROCNAME, MODNAME )

    !> There is **no modulation** at this stage, so the final motivation
    !! values are the same as primary motivations.
    call this%expectancy%modulation_none()

    !> #### Calculate motivation expectancies ####
    !> Finally, calculate the finally **expected arousal level for this
    !! behaviour**. As in the GOS, the overall arousal is the maximum value
    !! among all motivation components.
    this%arousal_expected = this%expectancy%max_final()

    !> Log also the final expectancy value in the @ref intro_debug_mode
    !! "debug mode".
    call LOG_DBG( LTAG_INFO // "Expectancy: " // TOSTR(this%arousal_expected),&
                  PROCNAME, MODNAME )

    !> Now as we know the expected arousal, we can choose the behaviour which
    !! would minimise this arousal level.

  end subroutine freeze_motivations_expect

  !-----------------------------------------------------------------------------
  !> Execute this behaviour component "freeze" by `this_agent` agent.
  subroutine freeze_do_execute(this, this_agent)
    class(FREEZE), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which goes down.
    class(APPRAISAL), intent(inout)    :: this_agent

    !> ### Implementation details ###
    !> #### Step 1: do_this ####
    !> As the first step, we use the **do**-procedure `freeze::do_this()`
    !! to perform the behaviour desired. As a result, the following values
    !! are obtained:
    !! - expected zero food gain
    !! - expected zero direct predation risk
    !! - expected general predation risk, assuming no direct threat.
    !! .
    !!
    !> However, because freezing does not incur any specific behavioural costs
    !! and does not change any environmental objects, calling `do_this()` is
    !! really **unnecessary**. It is therefore only called in the DEBUG mode to
    !! log and check the resulting perception values.
    if (IS_DEBUG) then
      call this%do_this(this_agent = this_agent)
      call LOG_DBG(LTAG_INFO // "Executed FREEZING; Perception values: " //   &
                   "Food gain: " // TOSTR(this%expected_food_gain) //         &
                   ", Direct risk: " // TOSTR(this%expected_pred_dir_risk) // &
                   ", Indirect risk: " // TOSTR(this%expected_predation_risk) )
    end if

    !> #### Step 2: Change the agent ####
    !> Freezing results in some small cost, equal to a single piece of the the
    !! cost of living. However, it is much smaller than the cost of locomotion.
    !! Also, no food can be obtained while freezing but digestion still occurs,
    !! so the value of the stomach contents is reduced by a fixed fraction.
    !! However, freezing, unlike other behaviour components, does not incur any
    !! specific cost or change of the agent. Cost of living and digestion
    !! subtractions are updated for every time step for every other
    !! behaviours anyway. Therefore, it is **not** done here.

    !> #### Step 3: Change the environment ####
    !> Freezing does not affect the environmental objects.

  end subroutine freeze_do_execute

  !-----------------------------------------------------------------------------
  !> Initialise the **escape dart** behaviour component to a zero state.
  !! Dart is a quick high speed active escape.
  elemental subroutine escape_dart_init_zero(this)
    class(ESCAPE_DART), intent(inout) :: this

    !> First init components from the base root class
    !! `the_behaviour::behaviour_base`.
    !> Mandatory label component that should be read-only.
    this%label = "ESCAPE_DART"
    !> The execution status is always FALSE, can be reset to TRUE only when
    !! the behaviour unit is called to execution.
    this%is_active = .FALSE.

    !> And the *expectancy* components.
    call this%expectancy%init()
    this%arousal_expected = 0.0_SRP

    !> Abstract `MOVE` component.
    this%distance = MISSING

    !> Second, init components of this specific behaviour (`ESCAPE_DART`).
    this%expected_food_gain = 0.0_SRP
    this%expected_cost_moving = MISSING
    this%expected_pred_dir_risk = MISSING
    this%expected_predation_risk = MISSING

  end subroutine escape_dart_init_zero

  !-----------------------------------------------------------------------------
  !> Do active escape dart by `this_agent` (the actor agent). Subjective
  !! assessment of the motivational value for this is based on the distance of
  !! escape (in turn, dependent on the visibility of the predator).
   subroutine escape_dart_do_this(this, this_agent, predator_object,          &
                                        dist_is_stochastic, time_step_model)
    class(ESCAPE_DART), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which goes down.
    class(APPRAISAL), intent(in) :: this_agent
    !> @param[in] predator_object optional predator object, if present, it is
    !!            assumed the actor agent tries to actively escape from this
    !!            specific predator.
    class(SPATIAL), optional, intent(in) :: predator_object
    !> @param[in] dist_is_stochastic Logical flag, if set to TRUE, the escape
    !!            distance is stochastic in the expectancy engine; this can
    !!            define an internal expectation uncertainty.
    logical, optional, intent(in) :: dist_is_stochastic
    !> @param[in] time_step_model optional time step of the model, overrides
    !!            the value calculated from the spatial data.
    integer, optional, intent(in)   :: time_step_model

    ! **WEIGHT_DIRECT**  is the relative weight  given to the immediate
    ! perception of predators over the predators counts in the memory stack.
    ! Obtained from global parameters
    ! (`commondata::predation_risk_weight_immediate`).
    real(SRP), parameter :: WEIGHT_DIRECT = PREDATION_RISK_WEIGHT_IMMEDIATE

    ! **MEM_WIND** is the size of the memory window when assessing the
    ! predator risk, only this number of the latest elements from the memory
    ! stack is taken into account. So we further weight the direct threat
    ! over the background risk when making the decision.
    ! @note  Note that we take into account the whole memory size
    !        (commondata::history_size_perception).
    integer, parameter :: MEM_WIND = HISTORY_SIZE_PERCEPTION

    ! Local copy of the time step parameter.
    integer :: time_step_model_here

    ! Local maximum visibility distance (visual range) to the predator.
    real(SRP) :: visibility_range_predator

    !> ### Implementation details ###
    !> #### Checks and preparations ####
    !> Check optional time step parameter. If unset, use global
    !! `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> #### Calculate expected food gain ####
    !> The expected food gain for active escape is zero as the agent cannot
    !! eat at this time.
    this%expected_food_gain = 0.0_SRP

    !> #### Calculate cost of fast escape movement ####
    !! First, calculate the distance of escape. The escape distance, in turn,
    !! depends on the visibility distance of the predator object: it should
    !! exceed this distance, so the actor agent could not see the predator
    !! any more.
    !> ##### Visibility range of the predator #####
    if (present(predator_object)) then
      !> First, check if the predator object is provided. If the predator
      !! object is provided as a dummy parameter, visibility range can be
      !! assessed using its size. However, the calculations depend on the
      !! exact type of the predator object because it can be
      !! the_environment::predator or the_neurobio::spatialobj_percept_comp
      !! (in predator perception: the_neurobio::percept_predator) or perhaps
      !! even just any extension of the the_environment::spatial class.
      !! Fortran `select type` construct is used here.
      select type (predator_object)
        !> - If the type of the object is the_environment::predator, then
        !!   visibility benefits from the object-bound function
        !!   the_environment::predator::visibility().
        type is (PREDATOR)
          visibility_range_predator =                                         &
                              predator_object%visibility(                     &
                                        time_step_model = time_step_model_here)
        !> - If the object type is the_neurobio::spatialobj_percept_comp
        !!   (as in perception objects), the visibility is calculated
        !!   using the object bound function
        !!   the_neurobio::spatialobj_percept_comp::visibility() with the
        !!   default object type (`object_area` parameter is not provided),
        !!   so the object area is calculated for *fish* (see
        !!   the_neurobio::spatialobj_percept_visibility_visual_range()).
        type is (SPATIALOBJ_PERCEPT_COMP)
          ! @warning The named parameter `time_step_model` is mandatory here
          !          because `visibility()` function first non-self parameter
          !          is `object_area`, not `time_step_model`.
          visibility_range_predator =                                         &
                              predator_object%visibility(                     &
                                        time_step_model = time_step_model_here)
        !> - If the object type is the default class the_environment::spatial,
        !!   e.g. the_environment::spatial_moving, its size may not be
        !!   available; the visibility is calculated manually
        !!   using the_environment::visual_range() function assuming
        !!   default predator size set by the commondata::predator_body_size
        !!   parameter.
        !! .
        class default
          visibility_range_predator =                                         &
              m2cm(                                                           &
                visual_range( irradiance=                                     &
                                predator_object%illumination(                 &
                                                       time_step_model_here), &
                              prey_area=                                      &
                                length2sidearea_fish(                         &
                                    cm2m( PREDATOR_BODY_SIZE ) ),             &
                              prey_contrast=PREYCONTRAST_DEFAULT )  )

      end select
    else
      !> If the predator object is not provided as a dummy parameter,
      !! visibility range is assessed using the default size of the predator
      !! commondata::predator_body_size and the ambient illumination at the
      !! actor agent's depth.
      visibility_range_predator =                                             &
              m2cm(                                                           &
                visual_range( irradiance=                                     &
                                this_agent%illumination(time_step_model_here),&
                              prey_area=                                      &
                                length2sidearea_fish(                         &
                                    cm2m( PREDATOR_BODY_SIZE ) ),             &
                              prey_contrast=PREYCONTRAST_DEFAULT )  )
    end if

    !> ##### Exact escape distance #####
    !> Knowing the visibility range of the predator, one can calculate the
    !! escape distance. Namely, the escape distance is obtained by multiplying
    !! the visibility range by the
    !! commondata::escape_dart_distance_default_factor parameter constant.
    !!
    !! This constant should normally exceed 1.0. In such a case, the escape
    !! distance exceeds the visibility of the predator. However, it should not
    !! be too long to avoid extra energetic cost.
    !!
    !! If the `dist_is_stochastic` optional parameter is TRUE, the escape
    !! distance is stochastic with the mean as above and the coefficient of
    !! variation set by the  commondata::escape_dart_distance_default_stoch_cv
    !! parameter. Stochastic distance can define *uncertainty* in the escape
    !! behaviour expectancy.
    if (present(dist_is_stochastic)) then
      if (dist_is_stochastic) then
        this%distance =                                                       &
          RNORM(visibility_range_predator*ESCAPE_DART_DISTANCE_DEFAULT_FACTOR,&
                cv2variance(ESCAPE_DART_DISTANCE_DEFAULT_STOCH_CV,            &
                            visibility_range_predator*                        &
                              ESCAPE_DART_DISTANCE_DEFAULT_FACTOR))
      else
        ! Non-stochastic escape distance.
        this%distance = visibility_range_predator *                           &
                                            ESCAPE_DART_DISTANCE_DEFAULT_FACTOR
      end if
    else
      ! Non-stochastic escape distance.
      this%distance = visibility_range_predator *                             &
                                            ESCAPE_DART_DISTANCE_DEFAULT_FACTOR
    end if

    !> ##### Cost of movement #####
    !> Knowing the movement distance, it is possible to calculate the cost
    !! of movement to this distance using the
    !! the_body::condition_cost_swimming_burst() method assuming the
    !! swimming is turbulent (so the exponent parameter takes the
    !! commondata::swimming_cost_exponent_turbulent value).
    this%expected_cost_moving =                                               &
          this_agent%cost_swim( distance=this%distance,                       &
                                exponent=SWIMMING_COST_EXPONENT_TURBULENT)

    !> #### Calculate the direct and general risk of predation ####
    !> The expected direct risk of predation is assumed to be commondata::zero.
    this%expected_pred_dir_risk = ZERO

    !> Accordingly, the general risk of predation taking account both the
    !! number of predators in the perception object and the average number
    !! of predators in the memory stack is calculated using the
    !! the_neurobio::predation_risk_backend() method, assuming there are no
    !! predators in perception.
    this%expected_predation_risk =                                            &
        predation_risk_backend(                                               &
                        pred_count = 0,                                       &
                        pred_memory_mean =                                    &
                            this_agent%memory_stack%get_pred_mean(MEM_WIND),  &
                        weight_direct = WEIGHT_DIRECT )

  end subroutine escape_dart_do_this

  !-----------------------------------------------------------------------------
  !> `escape_dart::motivations_expect()` is a subroutine (re)calculating
  !! motivations from fake expected perceptions following from the procedure
  !! `escape_dart::do_this()` => `the_behaviour::escape_dart_do_this()`.
  subroutine escape_dart_motivations_expect(this, this_agent, predator_object,&
                                      time_step_model, rescale_max_motivation )
    class(ESCAPE_DART), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which goes down.
    class(APPRAISAL), intent(in) :: this_agent
    !> @param[in] predator_object optional predator object, if present, it is
    !!            assumed the actor agent tries to actively escape from this
    !!            specific predator.
    class(SPATIAL), optional, intent(in) :: predator_object
    !> @param[in] time_step_model optional time step of the model, overrides
    !!            the value calculated from the spatial data.
    integer, optional, intent(in)   :: time_step_model
    !> @param[in] rescale_max_motivation maximum motivation value for
    !!            rescaling all motivational components for comparison
    !!            across all motivation and perceptual components and behaviour
    !!            units.
    real(SRP), optional, intent(in) :: rescale_max_motivation

    ! Local copy of optional model time step
    integer :: time_step_model_here

    ! Local variables
    real(SRP) :: max_motivation ! Local max. over all motivation components.

    !> ### Notable local variables ###
    !> #### Perception overrides ####
    !> - **expect_pred_dir** is the expected direct predation risk; it is zero.
    real(SRP) :: expect_pred_dir
    !> - **expect_predator** is the expected general predation risk, that is
    !!   based on a weighting of the current predation and predation risk
    !!   from the memory stack.
    real(SRP) :: expect_predator
    !> - **expect_stomach** is the expected stomach contents as a consequence
    !!   of escape movement. Note that there is no food consumption during
    !!   escape.
    real(SRP) :: expect_stomach
    !> - **expect_bodymass** is the expected body mass as a consequence of
    !!   the escape movement. Notably, it subtracts the cost of the escape
    !!   movement.
    real(SRP) :: expect_bodymass
    !> - **expect_energy** is the expected energy reserves as a consequence
    !!   of the escape movement. Calculated from the body mass and weight.
    !! .
    real(SRP) :: expect_energy

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME =                                 &
                                          "(escape_dart_motivations_expect)"

    !> ### Implementation details ###
    !> #### Checks and preparations ####
    !> Check optional time step parameter. If not provided, use global
    !! parameter value from commondata::global_time_step_model_current.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> #### Call do_this ####
    !> As the first step, we use the **do**-procedure `go_down_depth::do_this()`
    !! => `the_behaviour::go_down_do_this()` to perform the behaviour desired
    !! without changing either the agent or its environment, obtaining  the
    !! **subjective** values of the `this` behaviour components that later feed
    !! into the motivation **expectancy** functions:
    !!  - `perception_override_pred_dir`
    !!  - `perception_override_predator`
    !!  - `perception_override_stomach`
    !!  - `perception_override_bodymass`
    !!  - `perception_override_energy`
    !!  .
    if (present(predator_object)) then
      call this%do_this( this_agent = this_agent,                             &
                         predator_object = predator_object,                   &
                         dist_is_stochastic = .FALSE.,                        &
                         time_step_model = time_step_model_here )
    else
      call this%do_this( this_agent = this_agent,                             &
                         dist_is_stochastic = .FALSE.,                        &
                         time_step_model = time_step_model_here )
    end if

    !> #### Calculate expected (fake) perceptions ####
    !> First, calculate the expected **stomach content**, which is decremented
    !! by the expected digestion value (the_body::stomach_emptify_backend()).
    expect_stomach = max( ZERO,                                               &
                          this_agent%get_stom_content() -                     &
                                stomach_emptify_backend(                      &
                                  this_agent%get_stom_content() ) )

    !> Second, calculate the expected **body mass** of the agent as a consequence
    !! of the escape movement. The body mass is decremented by the cost of
    !! movement to the this\%distance and the cost of living
    !! (the_body::condition::living_cost()).
    expect_bodymass = max( ZERO,                                              &
                           this_agent%get_mass() -                            &
                             this%expected_cost_moving -                      &
                             this_agent%living_cost() )

    !> The expected **energy reserves** are calculated from the fake
    !! perceptions of the body mass and the current length (length does not
    !! change as food intake is zero in case of escape) using the
    !! the_body::energy_reserve() function.
    expect_energy = energy_reserve( expect_bodymass, this_agent%length() )

    !> The expected **direct predation risk** is transferred from the this
    !! object (the_behaviour::escape_dart).
    expect_pred_dir = this%expected_pred_dir_risk

    !> The expected **general predation risk** is also transferred from the
    !! this object (the_behaviour::escape_dart).
    expect_predator = this%expected_predation_risk

    !> #### Calculate motivation expectancies ####
    !> The next step is to calculate the motivational expectancies using the
    !! fake perceptions to override the default (actual agent's) values.
    !> At this stage, first, calculate motivation values resulting from the
    !! behaviour done (`the_behaviour::escape_dart::do_this()`) at the previous
    !! steps: what would be the motivation values *if* the agent does perform
    !! escape? Technically, this is done by calling the **neuronal
    !! response function**, `percept_components_motiv::motivation_components()`
    !! method, for each of the motivational states with `perception_override_`
    !! dummy parameters overriding the default values.
    !! Here is the list of the fake overriding perceptions for the
    !! `ESCAPE_DART` behaviour:
    !! - `perception_override_pred_dir`
    !! - `perception_override_predator`
    !! - `perception_override_stomach`
    !! - `perception_override_bodymass`
    !! - `perception_override_energy`
    !! .
    !  @note  **Expectancy** assessment for **hunger** motivation, using
    !         `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in] for
    !         `this_agent` now.
    call this%expectancy%hunger%percept_component%motivation_components       &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL,         &
      param_gp_matrix_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL,     &
      param_gp_matrix_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL,     &
      param_gp_matrix_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL,               &
      param_gp_matrix_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL,             &
      param_gp_matrix_age      = AGE_HUNGER_GENOTYPE_NEURONAL,                &
      param_gp_matrix_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL,            &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,     &
      param_gerror_cv_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_age      = AGE_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,      &
      param_gerror_cv_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,  &
      !  Real agent perception components are now substituted by the *fake*
      !  values resulting from executing this behaviour (`do_this` method).
      !  This is repeated for all the motivations: *hunger*,
      !  *passive avoidance,* *fear state* etc.
      perception_override_pred_dir = expect_pred_dir,                         &
      perception_override_predator = expect_predator,                         &
      perception_override_stomach = expect_stomach,                           &
      perception_override_bodymass = expect_bodymass,                         &
      perception_override_energy = expect_energy                              &
                                                                              )
      !> Real agent perception components are now substituted by the *fake*
      !! values resulting from executing this behaviour (`reproduce::do_this()`
      !! => `the_behaviour::reproduce_do_this()` method). This is repeated for
      !! all the motivations: *hunger*, *passive avoidance,* *active
      !! avoidance* etc. These optional **override parameters** are
      !! substituted by the "fake" values.

    !  @note  **Expectancy** assessment for **fear_defence** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%fear_defence%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL, &
      param_gp_matrix_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_pred_dir = expect_pred_dir,                         &
      perception_override_predator = expect_predator,                         &
      perception_override_stomach = expect_stomach,                           &
      perception_override_bodymass = expect_bodymass,                         &
      perception_override_energy = expect_energy                              &
                                                                              )

    !  @note  **Expectancy** assessment for **reproduction** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%reproduction%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL,    &
      param_gp_matrix_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_pred_dir = expect_pred_dir,                         &
      perception_override_predator = expect_predator,                         &
      perception_override_stomach = expect_stomach,                           &
      perception_override_bodymass = expect_bodymass,                         &
      perception_override_energy = expect_energy                              &
                                                                              )

    !> #### Calculate primary and final motivations ####
    !> Next, from the perceptual components calculated at the previous
    !! step we can obtain the **primary** and **final motivation** values by
    !! weighed summing.
    if (present(rescale_max_motivation)) then
      !> Here we can use global maximum motivation across all behaviours and
      !! perceptual components if it is provided, for rescaling.
      max_motivation = rescale_max_motivation
    else
      !> Or can rescale using local maximum value for this behaviour only.
      max_motivation =  this%expectancy%max_perception()
    end if

    !> Transfer attention weights from the actor agent `this_agent` to the
    !! `this` behaviour component. So, we will now use the updated modulated
    !! attention weights of the agent rather than their default parameter
    !! values.
    call this%attention_transfer(this_agent)

    !> So the primary motivation values are calculated.
    call this%expectancy%motivation_primary_calc(max_motivation)

    !> Primary motivations are logged in the @ref intro_debug_mode "debug mode".
    call LOG_DBG( LTAG_INFO // "Primary motivations: " //                   &
                  "hunger: " //                                             &
                    TOSTR(this%expectancy%hunger%motivation_prim)  //       &
                  ", fear_defence: " //                                     &
                    TOSTR(this%expectancy%fear_defence%motivation_prim) //  &
                  ", reproduce: " //                                        &
                    TOSTR(this%expectancy%reproduction%motivation_prim),    &
                  PROCNAME, MODNAME )

    !> There is **no modulation** at this stage, so the final motivation
    !! values are the same as primary motivations.
    call this%expectancy%modulation_none()

    !> #### Calculate motivation expectancies ####
    !> Finally, calculate the finally **expected arousal level for this
    !! behaviour**. As in the GOS, the overall arousal is the maximum value
    !! among all motivation components.
    this%arousal_expected = this%expectancy%max_final()

    !> Log also the final expectancy value in the @ref intro_debug_mode
    !! "debug mode".
    call LOG_DBG( LTAG_INFO // "Expectancy: " // TOSTR(this%arousal_expected),&
                  PROCNAME, MODNAME )

    !> Now as we know the expected arousal, we can choose the behaviour which
    !! would minimise this arousal level.

  end subroutine escape_dart_motivations_expect

  !-----------------------------------------------------------------------------
  !> Execute this behaviour component "escape" by `this_agent` agent.
  subroutine escape_dart_do_execute(this, this_agent, predator_object,        &
                                                            environment_limits)
    class(ESCAPE_DART), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which goes down.
    class(APPRAISAL), intent(inout)    :: this_agent
    !> @param[in] predator_object optional predator object, if present, it is
    !!            assumed the actor agent tries to actively escape from this
    !!            specific predator.
    class(SPATIAL), optional, intent(in) :: predator_object
    !> @param environment_limits Limits of the environment area available for
    !!        the random walk. The moving object cannot get beyond this limit.
    !!        If this parameter is not provided, the environmental limits are
    !!        obtained automatically from the global array
    !!        the_environment::global_habitats_available.
    class(ENVIRONMENT), intent(in), optional :: environment_limits

    ! Number of iterations in Gaussian correlated random walk, mainly for debug.
    integer :: iter_debug

    !> ### Implementation details ###
    !> #### Step 1: do_this ####
    !> As the first step, we use the **do**-procedure
    !! `the_behaviour::escape_dart::do_this()` to perform the behaviour
    !! desired. As a result, the following values are obtained:
    !! - **escape distance**;
    !! - expected (zero) food gain;
    !! - expected **stomach contents, body mass and energy reserves**, assuming
    !!   nonzero cost of movement and lack of feeding while escaping (i.e.
    !!   zero food gain).
    !! - the estimates of the predation risk are not used here, they only
    !!   are used in the subjective evaluation phase, when the agent computes
    !!   expectancies.
    !!  .
    if (present(predator_object)) then
      call this%do_this( this_agent = this_agent,                             &
                         predator_object = predator_object,                   &
                         dist_is_stochastic = .FALSE. )
    else
      call this%do_this( this_agent = this_agent,                             &
                         dist_is_stochastic = .FALSE. )
    end if

    !> In the @ref intro_debug_mode "debug mode", checking and logging the
    !! perception values.
    call LOG_DBG(LTAG_INFO // "Executed ESCAPE; Perception values: " //       &
                 "Escape distance: " // TOSTR(this%distance) //               &
                 ", Food gain: " // TOSTR(this%expected_food_gain) //         &
                 ", Direct risk: " // TOSTR(this%expected_pred_dir_risk) //   &
                 ", Indirect risk: " // TOSTR(this%expected_predation_risk) )

    !> #### Step 2: Change the agent ####
    !> Escape involves a random walk. Thus, the first thing is the agent
    !! *displacement*:
    if (present(predator_object)) then
      !> - If the predator is present, the agent does a correlated Gaussian
      !!   random walk the_environment::spatial_moving::corwalk() in a
      !!   direction roughly opposite to the predator position.
      ! TODO: DISPLACEMENT NON-IMPLEMENTED SO FAR -- NEED IMPLEMENT
      ! MOVE OPPOSITE FOR SPATIAL_MOVING
      if (present(environment_limits)) then
        call this_agent%corwalk(                                              &
                        target=predator_object,                               &
                        meanshift=this%distance,                              &
                        cv_shift=ESCAPE_DART_DISTANCE_DEFAULT_STOCH_CV,       &
                        is_away=.TRUE.,                                       &
                        environment_limits=environment_limits,                &
                        debug_reps = iter_debug  )
      else
        call this_agent%corwalk(                                              &
                        target=predator_object,                               &
                        meanshift=this%distance,                              &
                        cv_shift=ESCAPE_DART_DISTANCE_DEFAULT_STOCH_CV,       &
                        is_away=.TRUE.,                                       &
                        environment_limits=Global_Habitats_Available(         &
                                            this_agent%find_environment(      &
                                              Global_Habitats_Available) ),   &
                        debug_reps = iter_debug  )
      end if
    else
      !> - If the predator is not present, the agent performs a Gaussian
      !!   walk, to a distance equal to the this\%distance data component
      !!   and the CV set by the parameter
      !!   commondata::escape_dart_distance_default_stoch_cv.
      !!   @note Note that the escape involves a full 3D walk
      !!         with a single set of distance and CV parameters
      !!         (i.e. no separate depth walk parameters).
      !! .
      if (present(environment_limits)) then
        call this_agent%rwalk(                                                &
                        meanshift=this%distance,                              &
                        cv_shift=ESCAPE_DART_DISTANCE_DEFAULT_STOCH_CV,       &
                        environment_limits=environment_limits )
      else
        call this_agent%rwalk(                                                &
                        meanshift=this%distance,                              &
                        cv_shift=ESCAPE_DART_DISTANCE_DEFAULT_STOCH_CV,       &
                        environment_limits=Global_Habitats_Available(         &
                                     this_agent%find_environment(             &
                                       Global_Habitats_Available) )  )
      end if
    end if

    !> Escape movement results a *cost* that is defined by the actual distance
    !! travelled, the_environment::spatial_moving::way() which is subtracted
    !! here. Call `the_body::condition::set_mass()` for this.
    !! @note Note that the_body::condition::cost_swim() calculates the cost
    !!       of the latest way passed (the_environment::spatial_moving::way()
    !!       if the distance parameter is not provided.
    call this_agent%set_mass( value_set = this_agent%get_mass() -             &
                                this_agent%cost_swim(exponent=                &
                                        SWIMMING_COST_EXPONENT_TURBULENT),    &
                              update_history = .TRUE. )

    !> Additionally, also call the `the_body::condition::set_length()` method
    !! to update the body length history stack. However, the value_set
    !! parameter here is just the current value. This fake re-setting of the
    !! body length is done to keep both mass and length synchronised in their
    !! history stack arrays (there is no procedure for only updating history).
    call this_agent%set_length( value_set = this_agent%get_length(),          &
                                update_history = .TRUE. )

    !> After resetting the body mass, update energy reserves of the agent, that
    !! depend on both the length and the mass.
    call this_agent%energy_update()

    !> Check if the agent is starved to death. If yes, the agent can
    !! die without going any further.
    if (this_agent%starved_death()) call this_agent%dies()

    !> #### Step 3: Change the environment ####
    !> Escape movement itself does not affect the environmental objects.

  end subroutine escape_dart_do_execute

  !-----------------------------------------------------------------------------
  !> Initialise the **approach** behaviour component to a zero state.
  !! Approach is a generic type but not abstract.
  elemental subroutine approach_spatial_object_init_zero(this)
    class(APPROACH), intent(inout) :: this

    !> First init components from the base root class
    !! `the_behaviour::behaviour_base`.
    !> Mandatory label component that should be read-only.
    this%label = "APPROACH"
    !> The execution status is always FALSE, can be reset to TRUE only when
    !! the behaviour unit is called to execution.
    this%is_active = .FALSE.

    !> And the *expectancy* components.
    call this%expectancy%init()
    this%arousal_expected = 0.0_SRP

    !> Abstract `MOVE` component.
    this%distance = MISSING

    !> Then init components of this specific behaviour component extended class.
    this%expected_cost_moving = MISSING

  end subroutine approach_spatial_object_init_zero

  !-----------------------------------------------------------------------------
  !> The "do" procedure component of the behaviour element performs the
  !! behaviour without affecting the actor agent (the_agent) and the world
  !! (here food_item_eaten) which have intent(in), so it only can change
  !! the internal representation of the behaviour (the type to which this
  !! procedure is bound to, here `APPROACH`).
  subroutine approach_do_this(this, this_agent, target_object, target_offset, &
                                          predict_window_food, time_step_model )
    class(APPROACH), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which eats the food item.
    class(APPRAISAL), intent(in) :: this_agent
    !> @param[in] target_object is the spatial target  object the actor agent
    !!            is going to approach.
    class(SPATIAL), intent(in) :: target_object
    !> @param[in] target_offset is an optional offset for the target, so that
    !!            the target position of the approaching agent does not
    !!            coincide with the target object. If absent, a default value
    !!            set by the commondata::approach_offset_default is used.
    real(SRP), optional, intent(in) :: target_offset
    !> @param[in] predict_window_food the size of the prediction window, i.e.
    !!            how many steps back in memory are used to calculate the
    !!            predicted food gain. This parameter is limited by the maximum
    !!            commondata::history_size_perception value of the perception
    !!            memory history size.
    !! @note      This parameter is not used here and is placed only to make
    !!            derived class subroutine make the same argument list.
    integer, optional, intent(in) :: predict_window_food
    !> @param[in] time_step_model optional time step of the model, overrides
    !!            the value calculated from the spatial data.
    !!            This parameter is not used for this class, it is here only
    !!            to allow placement of this parameter for higher-order derived
    !!            classes.
    integer, optional, intent(in)   :: time_step_model


    ! Local copy of the body length of the agent
    real(SRP) :: agent_length

    ! Local copy of optional target offset
    real(SRP) :: target_offset_here

    ! PROCNAME is the procedure name for logging and debugging.
    character(len=*), parameter :: PROCNAME = "(approach_do_this)"

    !> ### Implementation details ###
    !> Check the optional parameter for the target offset and set the default
    !! one if offset is not provided.
    if (present(target_offset)) then
      target_offset_here = target_offset
    else
      target_offset_here = APPROACH_OFFSET_DEFAULT
    end if

    ! Agent length is local variable to avoid multiple calls to get_length().
    agent_length = this_agent%get_length()

    !> #### Proximity check ####
    !> The agent approaches the conspecific but to a nonzero distance equal
    !! to the target offset value (`target_offset`). A check is done if the
    !! distance between the agent and the conspecific target object is
    !! actually smaller than the target offset.
    !! - If so, the agent is already in close proximity to the target and
    !!   there is no need to do an approach movement.
    IN_PROXIMITY: if ( this_agent%distance( target_object ) <=                &
                                                target_offset_here ) then
      !>   - The approach distance is set to zero.
      this%distance = 0.0_SRP
      !>   - The expected cost of approach movement is also zero.
      !!   .
      this%expected_cost_moving = 0.0_SRP

    !> - If the agent is currently at a distance exceeding the target
    !!   offset, the approach distance towards the target position of
    !!   the actor agent is calculated as the true distance towards the
    !!   target conspecific minus the offset value `target_offset`.
    !!   (Note that whenever the default target offset is set, i.e. an average
    !!   of the agent and target body sizes, the approach distance depends
    !!   on the body sizes of both parties; it is also symmetric, i.e. the
    !!   same if a large agent approaches a small target conspecific or
    !!   *vice versa*.)
    else IN_PROXIMITY
      this%distance = this_agent%distance( target_object ) - target_offset_here
      !>   - Check if the distance to the target object exceeds the
      !!     migration travel maximum value, set as
      !!     commondata::migrate_dist_max_step body sizes of the agent. This
      !!     case should never occur if the maximum distance is sufficiently
      !!     large so that the target object is beyond the agent's visual range.
      !!     So, nothing is done here except logging a possible error.
      if (this%distance > agent_length * MIGRATE_DIST_MAX_STEP ) then
        call LOG_MSG( LTAG_WARN // "Approach travel distance exceeds big " // &
                      "threshold in " // PROCNAME // " for the agent " //     &
                      this_agent%individ_label() // ". Agent length: " //     &
                      TOSTR(agent_length) // ", migration distance: " //      &
                      TOSTR(this%distance)   )
      end if
      !>   - Calculate expected cost of the swimming. The expected cost of
      !!     swimming in the approach walk step depends on the above approach
      !!     distance and is calculated using the_body::condition::cost_swim()
      !!     method assuming *laminar* flow (laminar flow is due to normal
      !!     relatively slow swimming pattern).
      !!   .
      !! .
      this%expected_cost_moving =                                             &
                  this_agent%cost_swim( distance=this%distance,               &
                                        exponent=SWIMMING_COST_EXPONENT_LAMINAR)
    end if IN_PROXIMITY

  end subroutine approach_do_this

  !-----------------------------------------------------------------------------
  !> `the_behaviour::approach::expectancies_calculate()` (re)calculates
  !! motivations from fake expected perceptions following from the procedure
  !! `approach::do_this()` => `the_behaviour::approach_do_this()`.
  subroutine approach_motivations_expect( this, this_agent, target_object,    &
                                          target_offset, time_step_model,     &
                                          rescale_max_motivation )
    class(APPROACH), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which does approach.
    class(APPRAISAL), intent(in)    :: this_agent
    !> @param[in] target_object is the spatial target object the actor agent
    !!            is going to approach.
    class(SPATIAL), optional, intent(in) :: target_object
    !> @param[in] target_offset is an optional offset for the target, so that
    !!            the target position of the approaching agent does not
    !!            coincide with the target object. If absent, a default value
    !!            set by the commondata::approach_offset_default is used.
    real(SRP), optional, intent(in) :: target_offset
    !> @param[in] time_step_model optional time step of the model, overrides
    !!            the value calculated from the spatial data.
    !!            This parameter is not used for this class, it is here only
    !!            to allow placement of this parameter for higher-order derived
    !!            classes.
    integer, optional, intent(in)   :: time_step_model
    !> @param[in] rescale_max_motivation optional maximum motivation value for
    !!            rescaling all motivational components for comparison
    !!            across all motivation and perceptual components and behaviour
    !!            units.
    real(SRP), optional, intent(in) :: rescale_max_motivation

    ! Local variables
    real(SRP) :: max_motivation ! Local max. over all motivation components.

    ! Local copy of optional target offset
    real(SRP) :: target_offset_here

    !> ### Notable local variables ###
    !> #### Perception overrides ####
    !> - **perception_override_bodymass** is the expected body mass as a
    !!   consequence of the approach movement.
    real(SRP) :: perception_override_bodymass
    !> - **perception_override_energy** is the expected energy reserves
    !!   as a consequence of the escape movement. Calculated from the body
    !!   mass and weight.
    !! .
    real(SRP) :: perception_override_energy

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME =                                 &
                                          "(approach_motivations_expect)"

    ! Check the optional parameter for the target offset and set the default
    ! one if offset is not provided.
    if (present(target_offset)) then
      target_offset_here = target_offset
    else
      target_offset_here = APPROACH_OFFSET_DEFAULT
    end if

    !> ### Implementation details ###
    !> #### Call do_this ####
    !> As the first step, we use the **do**-procedure `walk_random::do_this()`
    !! => the_behaviour::walk_random_do_this() to perform the behaviour desired
    !! without changing either the agent or its environment, obtaining  the
    !! **subjective** values of the `this` behaviour components that later feed
    !! into the motivation **expectancy** functions:
    !!  - `perception_override_bodymass`
    !!  - `perception_override_energy`
    !!  .
    call this%do_this(  this_agent = this_agent,                              &
                        target_object = target_object,                        &
                        target_offset = target_offset_here )

    !> #### Calculate expected (fake) perceptions ####
    !> **Body mass**: the **body mass** perception override is obtained by
    !! subtracting the approach movement cost and the
    !! the_body::condition::living_cost() from the current mass.
    perception_override_bodymass = max( this_agent%get_mass() -               &
                                            this%expected_cost_moving -       &
                                            this_agent%living_cost(),         &
                                        ZERO )

    !> **Energy**: The fake perception values for the energy reserves
    !! (`perception_override_energy`) using the `the_body::energy_reserve()`
    !! procedure.
    perception_override_energy = energy_reserve( perception_override_bodymass,&
                                                 this_agent%length() )

    !> #### Calculate motivation expectancies ####
    !> The next step is to calculate the motivational expectancies using the
    !! fake perceptions to override the default (actual agent's) values.
    !> At this stage, first, calculate motivation values resulting from the
    !! behaviour done (`walk_random::do_this()` ) at the previous steps: what
    !! would be the motivation values *if* the agent does perform
    !! APPROACH? Technically, this is done by calling the **neuronal
    !! response function**, `percept_components_motiv::motivation_components()`
    !! method, for each of the motivational states with `perception_override_`
    !! dummy parameters overriding the default values.
    !! Here is the list of the fake overriding perceptions for the
    !! `APPROACH` behaviour:
    !!  - `perception_override_bodymass`
    !!  - `perception_override_energy`
    !!  .
    !  @note  **Expectancy** assessment for **hunger** motivation, using
    !         `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in] for
    !         `this_agent` now.
    call this%expectancy%hunger%percept_component%motivation_components       &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL,         &
      param_gp_matrix_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL,     &
      param_gp_matrix_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL,     &
      param_gp_matrix_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL,               &
      param_gp_matrix_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL,             &
      param_gp_matrix_age      = AGE_HUNGER_GENOTYPE_NEURONAL,                &
      param_gp_matrix_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL,            &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,     &
      param_gerror_cv_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_age      = AGE_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,      &
      param_gerror_cv_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,  &
      !  Real agent perception components are now substituted by the *fake*
      !  values resulting from executing this behaviour (`do_this` method).
      !  This is repeated for all the motivations: *hunger*,
      !  *passive avoidance,* *fear state* etc.
      perception_override_bodymass = perception_override_bodymass,            &
      perception_override_energy = perception_override_energy                 &
                                                                              )
      !> Real agent perception components are now substituted by the *fake*
      !! values resulting from executing this behaviour (`approach::do_this()`
      !! method). This is repeated for all the motivations: *hunger*,
      !! *passive avoidance,* *fear state* etc. These optional **override
      !! parameters** are substituted by the "fake" values.

    !  @note  **Expectancy** assessment for **fear_defence** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%fear_defence%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL, &
      param_gp_matrix_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_bodymass = perception_override_bodymass,            &
      perception_override_energy = perception_override_energy                 &
                                                                              )

    !  @note  **Expectancy** assessment for **reproduction** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%reproduction%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL,    &
      param_gp_matrix_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_bodymass = perception_override_bodymass,            &
      perception_override_energy = perception_override_energy                 &
                                                                              )

    !> #### Calculate primary and final motivations ####
    !> Next, from the perceptual components calculated at the previous
    !! step we can obtain the **primary** and **final motivation** values by
    !! weighed summing.
    if (present(rescale_max_motivation)) then
      !> Here we can use global maximum motivation across all behaviours and
      !! perceptual components if it is provided, for rescaling.
      max_motivation = rescale_max_motivation
    else
      !> Or can rescale using local maximum value for this behaviour only.
      max_motivation =  this%expectancy%max_perception()
    end if

    !> Transfer attention weights from the actor agent `this_agent` to the
    !! `this` behaviour component. So, we will now use the updated modulated
    !! attention weights of the agent rather than their default parameter
    !! values.
    call this%attention_transfer(this_agent)

    !> So the primary motivation values are calculated.
    call this%expectancy%motivation_primary_calc(max_motivation)

    !> Primary motivations are logged in the @ref intro_debug_mode "debug mode".
    call LOG_DBG( LTAG_INFO // "Primary motivations: " //                     &
                  "hunger: " //                                               &
                    TOSTR(this%expectancy%hunger%motivation_prim)  //         &
                  ", fear_defence: " //                                       &
                    TOSTR(this%expectancy%fear_defence%motivation_prim) //    &
                  ", reproduce: " //                                          &
                    TOSTR(this%expectancy%reproduction%motivation_prim),      &
                  PROCNAME, MODNAME )

    !> There is **no modulation** at this stage, so the final motivation
    !! values are the same as primary motivations.
    call this%expectancy%modulation_none()

    !> #### Calculate motivation expectancies ####
    !> Finally, calculate the finally **expected arousal level for this
    !! behaviour**. As in the GOS, the overall arousal is the maximum value
    !! among all motivation components.
    this%arousal_expected = this%expectancy%max_final()

    !> Log also the final expectancy value in the @ref intro_debug_mode
    !! "debug mode".
    call LOG_DBG( LTAG_INFO // "Expectancy: " // TOSTR(this%arousal_expected),&
                  PROCNAME, MODNAME )

    !> Now as we know the expected arousal, we can choose the behaviour which
    !! would minimise this arousal level.

  end subroutine approach_motivations_expect

  !-----------------------------------------------------------------------------
  !> Execute this behaviour component "approach" by `this_agent` agent.
  subroutine approach_do_execute( this, this_agent, target_object, is_random, &
                                          target_offset, environment_limits )
    class(APPROACH), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which eats the food item.
    class(APPRAISAL), intent(inout) :: this_agent
    !> @param[in] target_object is the spatial target object the actor agent
    !!            is going to approach.
    class(SPATIAL), intent(in) :: target_object
    !> @param[in] is_random indicator flag for random correlated walk. If
    !!            present and is TRUE, the agent approaches to the
    !!            `target_object` in form of random correlated walk (see
    !!            the_environment::spatial_moving::corwalk()), otherwise
    !!            directly.
    logical, optional, intent(in) :: is_random
    !> @param[in] target_offset is an optional offset for the target, so that
    !!            the target position of the approaching agent does not
    !!            coincide with the target object. If absent, a default value
    !!            set by the commondata::approach_offset_default is used.
    !!            For the the_behaviour::approach_conspec, the default value
    !!            is as an average of the agent and target conspecific body
    !!            lengths.
    real(SRP), optional, intent(in) :: target_offset
    !> @param environment_limits Limits of the environment area available for
    !!        the random walk. The moving object cannot get beyond this limit.
    class(ENVIRONMENT), intent(in), optional :: environment_limits

    ! Temporary local spatial object for keeping the location of the
    ! target object.
    type(SPATIAL) :: target_object_tmp, target_object_offset

    ! Local copy of optional random flag
    logical :: is_random_walk

    ! Local copy of optional target offset
    real(SRP) :: target_offset_here

    ! Local copy of the body length of the agent
    real(SRP) :: agent_length

    ! Mean shift for random walk
    real(SRP) :: rwalk_meanshift_xy

    ! Debugging indicators for correlated random walk.
    logical :: is_converged_debug
    integer :: iter_debug

    ! PROCNAME is the procedure name for logging and debugging.
    character(len=*), parameter :: PROCNAME = "(approach_do_execute)"

    !> ### Implementation details ###
    !> #### Checks and preparations ####
    !> First, check the optional parameters
    !! - random walk flag: `is_random`; if the parameter is not provided,
    !!   the default value FALSE is set so that the agent does a direct
    !!   approach towards the target object leaving the target offset
    !!   distance.
    if (present(is_random)) then
      is_random_walk = is_random
    else
      is_random_walk = .FALSE.
    end if

    !> - target offset: `target_offset`. Note that setting the default
    !!   value for the target offset involves calling the `select type`
    !!   construct. Therefore, the default offset for a simple
    !!   the_behaviour::approach behaviour is equal to the fixed
    !!   commondata::approach_offset_default value whereas for the
    !!   the_behaviour::approach_conspec, it is set as an average of the
    !!   agent and target conspecific body lengths.
    !! .
    if (present(target_offset)) then
      target_offset_here = target_offset
    else
      select type (this)
        class is (APPROACH)
          target_offset_here = APPROACH_OFFSET_DEFAULT
        class is (APPROACH_CONSPEC)
          target_offset_here = ( this_agent%get_length() +                    &
                             get_prop_size(target_object) ) / 2.0_SRP
        class default
          target_offset_here = APPROACH_OFFSET_DEFAULT
      end select
    end if

    !> Second, copy the spatial location of the target `target_object` to
    !! a temporary spatial object `target_object_tmp` to avoid multiple
    !! calling the the_environment::spatial::position() method.
    !! @note This is needed because the `target_object` is **class** and
    !!       getting location can be only done through the `location` method.
    call target_object_tmp%position( target_object%location() )

    !> #### Step 1: do_this ####
    !> First, we use the intent-in **do**-procedure
    !! the_behaviour::approach::do_this() to perform the behaviour desired.
    !! Here it calculates the distance towards the target object also taking
    !! account of the offset parameter.
    call this%do_this(  this_agent = this_agent,                              &
                        target_object = target_object_tmp,                    &
                        target_offset = target_offset_here )

    !> Also check here if the approach distance exceeds the limit set by the
    !! commondata::migrate_dist_max_step parameter. If it does exceed, the
    !! agent will move towards the target object, but the distance is reduced
    !! according to the limit.
    agent_length = this_agent%get_length()
    if (this%distance - target_offset_here >                                  &
                                  agent_length * MIGRATE_DIST_MAX_STEP) then
      call LOG_MSG( LTAG_WARN // "Approach travel distance exceeds big " //   &
                    "threshold in " // PROCNAME // " for the agent " //       &
                    this_agent%individ_label() // ". Agent length: " //       &
                    TOSTR(agent_length) // ", migration distance: " //        &
                    TOSTR(this%distance)   )
      this%distance = agent_length * MIGRATE_DIST_MAX_STEP + target_offset_here
    end if

    !> #### Step 2: Change the agent ####
    !> ##### Relocate towards the target object #####
    !> Relocate to the target object can be either a correlated random walk
    !! in the target direction or direct movement to the target.
    !! - In the former case, the environmental limits can be either provided
    !!   by the `environment_limits` parameter or obtained automatically
    !!   from the global array the_environment::global_habitats_available.
    !!   - If the approach distance is less then commondata::zero (i.e. the
    !!     target object is already at a distance smaller than target offset),
    !!     the correlated random walk step is set to the target offset.
    !!   .
    DO_WALK_RANDOM: if (is_random_walk) then
      if (present(environment_limits)) then
        if (this%distance > ZERO) then
          rwalk_meanshift_xy = this%distance - target_offset_here
        else
          rwalk_meanshift_xy = target_offset_here
        end if
        call this_agent%corwalk(                                              &
                target = target_object_tmp,                                   &
                meanshift_xy = rwalk_meanshift_xy,                            &
                cv_shift_xy = WALK_RANDOM_DISTANCE_STOCHASTIC_CV,             &
                meanshift_depth = (this%distance - target_offset_here) *      &
                              WALK_RANDOM_VERTICAL_SHIFT_RATIO,               &
                cv_shift_depth = WALK_RANDOM_DISTANCE_STOCHASTIC_CV *         &
                      WALK_RANDOM_VERTICAL_SHIFT_CV_RATIO,                    &
                is_away = .FALSE.,                                            &
                environment_limits = environment_limits,                      &
                is_converged = is_converged_debug,                            &
                debug_reps = iter_debug  )
      else
        if (this%distance > ZERO) then
          rwalk_meanshift_xy = this%distance - target_offset_here
        else
          rwalk_meanshift_xy = target_offset_here
        end if
        call this_agent%corwalk(                                              &
                target = target_object_tmp,                                   &
                meanshift_xy = rwalk_meanshift_xy,                            &
                cv_shift_xy = WALK_RANDOM_DISTANCE_STOCHASTIC_CV,             &
                meanshift_depth = (this%distance - target_offset_here) *      &
                              WALK_RANDOM_VERTICAL_SHIFT_RATIO,               &
                cv_shift_depth = WALK_RANDOM_DISTANCE_STOCHASTIC_CV *         &
                      WALK_RANDOM_VERTICAL_SHIFT_CV_RATIO,                    &
                is_away = .FALSE.,                                            &
                environment_limits = Global_Habitats_Available(               &
                                            this_agent%find_environment(      &
                                              Global_Habitats_Available) ),   &
                is_converged = is_converged_debug,                            &
                debug_reps = iter_debug  )
      end if
      call LOG_DBG( LTAG_INFO // "Correlated random walk: converged " //      &
                      TOSTR(is_converged_debug) // ", iterations: " //        &
                      TOSTR(iter_debug), PROCNAME, MODNAME )
    !> - If correlated random walk is not enabled (`is_random` parameter is
    !!   FALSE), the agent goes *directly* towards the target. It actually
    !!   relocates to a spatial position with the the target offset. The new
    !!   position of the agent is defined by the the_environment::offset_dist()
    !!   function subtracting the value of the offset.
    !!   - However, if the approach distance is less than commondata::zero,
    !!     (i.e. the agent is already in proximity of the target object, at a
    !!     distance smaller than the target offset), the agent "moves" to its
    !!     *current* position, i.e. no real relocation is done. This situation
    !!     is logged in the DEBUG mode.
    !!   .
    !! .
    else DO_WALK_RANDOM
      if (this%distance > ZERO) then
        target_object_offset = offset_dist( this_agent, target_object_tmp,    &
                                            target_offset_here)
        call this_agent%position( target_object_offset )
        call LOG_DBG(LTAG_INFO // "Agent approached the target, distance to " &
                    // " the target: " //                                     &
                    TOSTR(this_agent%distance(target_object_tmp)) //          &
                    "; distance offset: " //  TOSTR(target_offset_here) //    &
                    ", updated target distance to the target: " //            &
                    TOSTR(this_agent%distance(target_object_offset)) //       &
                    "; original target: " // TOSTR([target_object_tmp%xpos(), &
                    target_object_tmp%ypos(), target_object_tmp%dpos()]) //   &
                    ", new agent position: " // TOSTR([this_agent%xpos(),     &
                    this_agent%ypos(),this_agent%dpos()]), PROCNAME, MODNAME )
      else
        call this_agent%position( this_agent%location() )
        call LOG_DBG(LTAG_INFO // "Agent has not relocated because it is " // &
                     "in proximity of the target; distance to target: " //    &
                     TOSTR(this_agent%distance(target_object)) )
      end if

    end if DO_WALK_RANDOM

    !> ##### Process the cost of movement #####
    !> - Reset the body mass of the actor agent subtracting the actual cost of
    !!   moving that is automatically calculated in the call to
    !!   the_body::condition::cost_swim(). The the_body::condition::set_mass()
    !!   method is used here to adjust the mass.
    call this_agent%set_mass(                                                 &
                      value_set = this_agent%get_mass() -                     &
                        this_agent%cost_swim(exponent=                        &
                                        SWIMMING_COST_EXPONENT_LAMINAR),      &
                      update_history = .TRUE. )

    !> - Additionally, also call the `the_body::condition::set_length()` method
    !!   to update the body length history stack. However, the value_set
    !!   parameter here is just the current value. This fake re-setting of the
    !!   body length is done to keep both mass and length synchronised in their
    !!   history stack arrays (there is no procedure for only updating history).
    call this_agent%set_length( value_set = this_agent%get_length(),          &
                                update_history = .TRUE. )

    !> - After resetting the body mass, update energy reserves of the agent,
    !!   that depend on both the length and the mass.
    !! .
    call this_agent%energy_update()

    !> Finally, check if the agent is starved to death. If yes, the agent can
    !! die without going any further.
    if (this_agent%starved_death()) call this_agent%dies()

    !> #### Step 3: Change the environment ####
    !> Approach does not affect the environmental objects.

  end subroutine approach_do_execute

  !-----------------------------------------------------------------------------
  !> Initialise the **approach conspecific** behaviour to a zero state.
  !! Approach conspecific is a special extension of the generic `APPROACH`
  !! behaviour.
  elemental subroutine approach_conspecifics_init_zero(this)
    class(APPROACH_CONSPEC), intent(inout) :: this

    !> First init components from the base root class
    !! `the_behaviour::behaviour_base`.
    !> Mandatory label component that should be read-only.
    this%label = "APPR_CONSPEC"
    !> The execution status is always FALSE, can be reset to TRUE only when
    !! the behaviour unit is called to execution.
    this%is_active = .FALSE.

    !> And the *expectancy* components.
    call this%expectancy%init()
    this%arousal_expected = 0.0_SRP

    !> Abstract `MOVE` component.
    this%distance = MISSING

    !> Component of `APPROACH` class.
    !> Then init components of this specific behaviour component extended class.
    this%expected_cost_moving = MISSING

    !> This class, APPROACH_CONSPEC, initialisations.
    this%expected_food_gain = MISSING
    this%expected_predation_risk = MISSING
    this%expected_pred_dir_risk = MISSING

  end subroutine approach_conspecifics_init_zero

  !-----------------------------------------------------------------------------
  !> The "do" procedure component of the behaviour element performs the
  !! behaviour without affecting the actor agent (the_agent) and the world
  !! (here food_item_eaten) which have intent(in), so it only can change
  !! the internal representation of the behaviour (the type to which this
  !! procedure is bound to, here `APPROACH_CONSPEC`).
  subroutine approach_conspecifics_do_this( this, this_agent, target_object,  &
                                                              target_offset,  &
                                                         predict_window_food, &
                                                              time_step_model )
    class(APPROACH_CONSPEC), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which approaches.
    class(APPRAISAL), intent(in) :: this_agent
    !> @param[in] target_object is the target conspecific the actor agent
    !!            is going to approach.
    class(SPATIAL), intent(in) :: target_object
    !> @param[in] target_offset is an optional offset for the target, so that
    !!            the target position of the approaching agent does not
    !!            coincide with the target object. If absent, a default value
    !!            set by the commondata::approach_offset_default is used.
    real(SRP), optional, intent(in) :: target_offset
    !> @param[in] predict_window_food the size of the prediction window, i.e.
    !!            how many steps back in memory are used to calculate the
    !!            predicted food gain. This parameter is limited by the maximum
    !!            commondata::history_size_perception value of the perception
    !!            memory history size.
    integer, optional, intent(in) :: predict_window_food
    !> @param[in] time_step_model optional time step of the model, overrides
    !!            the value calculated from the spatial data.
    integer, optional, intent(in)   :: time_step_model

    ! Local copy of the body length of the agent
    real(SRP) :: agent_length

    ! Local copy of optional target offset
    real(SRP) :: target_offset_here

    ! Local copies of optionals.
    integer :: predict_window_food_here, time_step_model_here

    ! PROCNAME is the procedure name for logging and debugging.
    character(len=*), parameter :: PROCNAME = "(approach_conspecifics_do_this)"

    ! File name for debug plot.
    character(FILENAME_LENGTH) :: debug_plot_file_sufx

    ! WEIGHT_DIRECT is the relative weight  given to the immediate
    ! perception of predators over the predators counts in the memory stack.
    ! Obtained from global parameters
    ! (`commondata::predation_risk_weight_immediate`).
    real(SRP), parameter :: WEIGHT_DIRECT = PREDATION_RISK_WEIGHT_IMMEDIATE

    ! MEM_WIND is the size of the memory window when assessing the
    ! predator risk, only this number of the latest elements from the memory
    ! stack is taken into account. So we further weight the direct threat
    ! over the background risk when making the decision.
    ! @note  Note that we take into account the whole memory size
    !        (commondata::history_size_perception).
    integer, parameter :: MEM_WIND = HISTORY_SIZE_PERCEPTION

    !> ### Notable local variables ###
    !! - consp_size - the size of the target conspecific,
    !! - consp_mass - body mass of the target conspecific
    !! - consp_dist - the distance to the target conspecific
    real(SRP) :: consp_size, consp_mass, consp_dist

    !> - target_position_agent - the target position of the agent, it does
    !!   not coincide with the position of the target conspecific and is
    !!   smaller by the value of the target offset.
    type(SPATIAL) :: target_position_agent

    !> - tmp_predator - temporary predator object, a subjective representation
    !!   of the first nearest predator from the perception object of the actor
    !!   agent.
    type(PREDATOR) :: tmp_predator

    !> - risk_pred_expect - an array keeping the expectancy of the predation
    !!   risk for each predator in the perception object.
    real(SRP), allocatable, dimension(:) :: risk_pred_expect

    !> - n_pred_now - current number of predators in the perception object
    !!   of the actor agent.
    integer :: n_pred_now, i

    !> - body_mass_ratio - the ratio of the body mass of the actor agent
    !!   to the target conspecific @f$ \frac{M}{M_{TC}} @f$.
    real(SRP) :: body_mass_ratio

    !> - food_gain_expect_baseline is a baseline expected food gain, not
    !!   taking account of competition with the target conspecific.
    real(SRP) :: food_gain_expect_baseline

    !> - agent_length - agent length by condition::get_length() method.
    !! .
    agent_length = this_agent%get_length()

    !> #### Checks and preparations ####
    !> Check optional parameter for the food perception memory window. If
    !! the `predict_window_food` dummy parameter is not provided, its default
    !! value is the proportion of the whole perceptual memory window defined
    !! by commondata::history_perception_window_food. Thus, only the
    !! latest part of the memory is used for the prediction of the future
    !! food gain.
    if (present(predict_window_food)) then
      predict_window_food_here = predict_window_food
    else
      predict_window_food_here = floor( HISTORY_SIZE_PERCEPTION *             &
                                        HISTORY_PERCEPTION_WINDOW_FOOD )
    end if
    !> Check optional time step parameter. If unset, use global
    !! `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> Set the debug plot file name that will be passed to the
    !! predator-class-bound function the_environment::predator::risk_fish().
    debug_plot_file_sufx = TOSTR(Global_Time_Step_Model_Current) // "_" //    &
                      MMDD // "_a_" // trim(this_agent%individ_label())       &
                      // "_"  //                                              &
                      RAND_STRING(LABEL_LENGTH, LABEL_CST,LABEL_CEN) // PS

    !> ### Implementation details ###
    !> #### Get the properties of the target conspecific ####
    !> Get the properties of the conspecific from the perception object
    !! or real physical conspecific data. This is done by determining the
    !! `target_object` data type with "`select type`" construct (named
    !! construct `GET_TARGET`).
    !!
    !! The distance to the target conspecific is determined from the target
    !! object with the_neurobio::conspec_percept_comp::get_dist() for
    !! perception object or the_environment::spatial::distance() for real
    !! conspecific.
    GET_TARGET: select type (target_object)
      !> - if the `target_object` is a conspecific from the perception object,
      !!   its body length and mass are obtained from the respective
      !!   data components of the_neurobio::conspec_percept_comp.
      class is (CONSPEC_PERCEPT_COMP)                               GET_TARGET
        consp_size = target_object%get_size()
        consp_mass = target_object%get_mass()
        consp_dist = target_object%get_dist()
        call LOG_DBG( LTAG_INFO // "Perception of target conspecific in " //  &
                      PROCNAME // ", size: " // TOSTR(consp_size) //          &
                      ", mass: " // TOSTR(consp_mass) //                      &
                      ", distance (from perception): " // TOSTR(consp_dist) //&
                      " and (from object): " //                               &
                      TOSTR( this_agent%distance( target_object ) ) // "." )
      !> - if the `target_object` is real conspecific (the_neurobio::appraisal
      !!   class), its body length and mass are obtained from lower order
      !!   class component the_body::condition::get_length() and
      !!   the_body::condition::get_mass() methods.
      class is (APPRAISAL)                                          GET_TARGET
        consp_size = target_object%get_length()
        consp_mass = target_object%get_mass()
        consp_dist = this_agent%distance( target_object )
        call LOG_DBG( LTAG_INFO // "Explicit target conspecific in " //       &
                      PROCNAME // ", size: " // TOSTR(consp_size) //          &
                      ", mass: " // TOSTR(consp_mass) //                      &
                      ", distance: " // TOSTR(consp_dist) // "." )
      !> - in the case construct "default" case, if the `target_object` is
      !!   neither a perception object nor real conspecific, get the
      !!   location from the commondata::spatial class position data and other
      !!   properties of the conspecific from the actor agent itself.
      !!   Such a situation of **undefined target** type is unexpected and is
      !!   likely to point to a bug. Therefore, an error is issued into the
      !!   logger.
      !> .
      class default                                                 GET_TARGET
        consp_size = agent_length
        consp_mass = this_agent%get_mass()
        consp_dist = this_agent%distance( target_object )
        call LOG_DBG( LTAG_WARN // "Target conspecific in " // PROCNAME //    &
                      " is undefined, get properties from the agent. " //     &
                      "Length: " // TOSTR(consp_size) // ", " //              &
                      "mass: " // TOSTR(consp_mass) // ", " //                &
                      "distance: " // TOSTR(consp_dist) // ". ",              &
                      PROCNAME, MODNAME )
        call LOG_DBG( LTAG_WARN // "Position of the target object: " //       &
                        TOSTR([ target_object%xpos(),                         &
                                target_object%ypos(),                         &
                                target_object%dpos() ]) // " in " // PROCNAME,&
                                PROCNAME, MODNAME )
    end select GET_TARGET

    !> #### Determine the target offset ####
    !> Target offset `target_offset` can be provided as an optional dummy
    !! parameter to this procedure. However, if it is not provided explicitly,
    !! a default value is set as an average of the actor agent body length
    !! and the target conspecific body length.
    if (present(target_offset)) then
      target_offset_here = target_offset
    else
      target_offset_here = (agent_length + consp_size) / 2.0_SRP
    end if

    !> #### Proximity check and target distance ####
    !> The agent approaches the conspecific but to a nonzero distance equal
    !! to the target offset value (`target_offset`). A check is done if the
    !! distance between the agent and the conspecific target object is
    !! actually smaller than the target offset.
    !! - If so, the agent is already in close proximity to the target and
    !!   there is no need to do an approach movement.
    IN_PROXIMITY: if ( this_agent%distance( target_object ) <=                &
                                                target_offset_here ) then
      !>   - The approach distance is set to zero.
      this%distance = 0.0_SRP
      !>   - The target position of the agent (`target_position_agent`) after
      !!     such a zero approach actually coincides with the current position
      !!     of the agent: it does not plan to swim.
      target_position_agent = this_agent%location()
      !>   - The expected cost of approach movement is also zero.
      !!   .
      this%expected_cost_moving = 0.0_SRP

    !> - If the agent is currently at a distance exceeding the target
    !!   offset, the approach distance towards the target position of
    !!   the actor agent is calculated as the true distance towards the
    !!   target conspecific minus the offset value `target_offset`.
    !!   (Note that whenever the default target offset is set, i.e. an average
    !!   of the agent and target body sizes, the approach distance depends
    !!   on the body sizes of both parties; it is also symmetric, i.e. the
    !!   same if a large agent approaches a small target conspecific or
    !!   *vice versa*.)
    else IN_PROXIMITY
      this%distance = this_agent%distance( target_object ) - target_offset_here
      !>   - Check if the distance to the target object exceeds the
      !!     migration travel maximum value, set as
      !!     commondata::migrate_dist_max_step body sizes of the agent. This
      !!     case should not normally occur if the maximum distance is
      !!     sufficiently large so that the target object is beyond the
      !!     agent's visual range. So, nothing is done here except logging a
      !!     warning.
      if (this%distance > agent_length * MIGRATE_DIST_MAX_STEP ) then
        call LOG_MSG( LTAG_WARN // "Target conspecific travel distance " //   &
                      "exceeds big threshold in " // PROCNAME //              &
                      " for the agent " // this_agent%individ_label() //      &
                      ". Agent length: " //  TOSTR(agent_length) //           &
                      ", target distance: " // TOSTR(this%distance)  )
      end if
      !>   - Calculate the prospective target position of the agent in
      !!     proximity of the target conspecific `target_position_agent` with
      !!     the offset, using the the_environment::offset_dist() procedure.
      target_position_agent = offset_dist( this_agent, target_object,         &
                                           target_offset_here )
      !>   - Calculate expected cost of the swimming. The expected cost of
      !!     swimming in the approach walk step depends on the above approach
      !!     distance and is calculated using the the_body::condition::cost_swim()
      !!     method assuming *laminar* flow (laminar flow is due to normal
      !!     relatively slow swimming pattern).
      !!   .
      !! .
      this%expected_cost_moving =                                             &
                   this_agent%cost_swim(distance=this%distance,               &
                                        exponent=SWIMMING_COST_EXPONENT_LAMINAR)
    end if IN_PROXIMITY

    !> #### Calculate expected risk of predation ####
    !> The expected risk of predation is assumed to **reduce** due to predator
    !! dilution or confusion effects if the agent approaches a conspecific.
    !! Furthermore, the risk values depend on the relative positions and
    !! distances between the predator and the actor agent and predator and
    !! the target conspecific.
    !!
    !> Calculation of the expected risks of predation depends on the current
    !! perception of the agent. The simplest case is when the agent has
    !! currently **no predators** in its predator perception object:
    NO_PREDATORS: if ( .not. this_agent%has_pred() ) then

      !> - If there are no predators in the perception object, the expected
      !!   general risk is calculated using the
      !!   the_neurobio::predation_risk_backend() method assuming the current
      !!   perception of predators is null.
      this%expected_predation_risk =                                          &
        predation_risk_backend(                                               &
                        pred_count = 0,                                       &
                        pred_memory_mean =                                    &
                            this_agent%memory_stack%get_pred_mean(MEM_WIND),  &
                        weight_direct = WEIGHT_DIRECT )
      !> - The expected direct risk of predation is zero if there are no
      !!   predators in the current perception.
      !! .
      this%expected_pred_dir_risk = 0.0_SRP

    !> If there is a **non-zero number of predators** in the current predator
    !! perception, calculations of the expected risks are more complex.
    else NO_PREDATORS
      !> ##### General risk #####
      !> First, get the number of predators in the current perception object
      !! using the the_neurobio::percept_predator::get_count().
      n_pred_now = this_agent%perceive_predator%get_count()

      !> Accordingly, the **general risk** of predation taking account both the
      !! number of predators in the perception object and the average number
      !! of predators in the memory stack is calculated using the
      !! the_neurobio::predation_risk_backend() method. However, the expected
      !! number of predators is reduced by a factor defined by the parameter
      !! commondata::approach_conspecfic_dilute_general_risk (the integer
      !! expected number of predators is actually obtained by the `floor`
      !! intrinsic giving the lower integer value). (Therefore, the reduced
      !! expectancy is based on reduction of the expected number of predators
      !! while keeping memory part of the expectation fixed).
      this%expected_predation_risk =                                          &
          predation_risk_backend(                                             &
                          pred_count = floor( n_pred_now *                    &
                              APPROACH_CONSPECFIC_DILUTE_GENERAL_RISK ),      &
                          pred_memory_mean =                                  &
                              this_agent%memory_stack%get_pred_mean(MEM_WIND),&
                          weight_direct = WEIGHT_DIRECT )

      !> ##### Direct risk #####
      !> Expectation of the direct risk of predation depends on the target
      !! position of the actor agent @f$ P_T @f$ (with the target offset
      !! @f$ \Delta @f$) and relative distances between the actor agent, target
      !! conspecific @f$ P_{TC} @f$ and all the predators @f$ P_i @f$ in the
      !! current perception object of the actor agent following the predicted
      !! agent movement.
      !> @image html img_doxygen_approach_consp.svg
      !! @image latex img_doxygen_approach_consp.eps "Calculation of the predicted direct risk of predation" width=14cm
      !!
      !>  First, allocate the array `risk_pred_expect` that keeps the values
      !! of risk for each of the predators in the perception object.
      allocate( risk_pred_expect(n_pred_now) )

      !> Then, cycle over all the predators @f$ P_i @f$ in the current
      !! perception object of the actor agent @f$ P_a @f$ and check if the
      !! prospective movement towards the target conspecific @f$ P_{TC} @f$
      !! would place the agent *further* from the predator (a) than the target
      !! conspecific: @f$ D_{AP} > D_{CP} @f$.
      !! If yes, direct risk of predation for this
      !! predator is equal to the risk of predation @f$ r @f$ unadjusted for
      !! the dilution or confusion effects multiplied by the
      !! commondata::approach_conspecfic_adjust_pair_behind factor (normally
      !! 1/2 as diluted in a half by the target conspecific, @f$ 0.5 r_i @f$).
      !! If the movement is likely to place the actor agent *closer* to the
      !! predator than the target conspecific @f$ D_{AP} < D_{CP} @f$, the
      !! expected risk for the actor agent is calculated as unadjusted value
      !! @f$ r_i @f$.
      !!
      !! Thus, the predator dilution effect is introduced only if the actor
      !! agent is moving to the backward position further away from the predator
      !! (a) than the target conspecific (the target conspecific then is closer
      !! to the predator and suffers higher risk). If the actor agent moves to
      !! the forward position with respect to the predator (b), it suffers full
      !! unadjusted risk instead. This is the classical "selfish herd" effect.
      !!
      !! Finally, the **maximum** value of the predation risks across all the
      !! predators @f$ max (r_i) @f$ in the perception object of the actor agent
      !! constitutes the "final" expectation of the direct risk of predation:
      !! the_behaviour::approach_conspec::expected_pred_dir_risk.
      PRED_PERCEPT: do i=1, n_pred_now
        !> - At each (*i*-th) step of the loop, create a temporary
        !!   the_environment::predator type object `tmp_predator` using
        !!   the_environment::predator::make(). This predator's body size and
        !!   the spatial position are obtained directly from the *i*-th predator
        !!   1/2 the agent's current perception object. But note that the agent
        !!   is unable to determine the individually specific attack rate of
        !!   the predator and uses the default value.
        call tmp_predator%make(                                               &
            body_size =                                                       &
                  this_agent%perceive_predator%predators_seen(i)%get_size(),  &
            attack_rate = PREDATOR_ATTACK_RATE_DEFAULT,                       &
            position =                                                        &
                  this_agent%perceive_predator%predators_seen(i)%location(),  &
            label="tmp_object" )

        !> - If the distance between the agent and the *i*-th predator in the
        !!   perception object (the temporary predator object `tmp_predator`)
        !!   would become **shorter** than the distance between
        !!   the target conspecific and the predator (i.e. the agent would go
        !!   closer to the *i*-th predator than the target conspecific
        !!   @f$ D_{AP} < D_{CP} @f$), the direct risk of predation is
        !!   calculated as unadjusted risk of predation computed using the
        !!   the_environment::predator::risk_fish() method, assuming the
        !!   actor agent is in the target approach position
        !!   `target_position_agent`.
        GO_CLOSER: if ( target_position_agent%distance(                       &
                          this_agent%perceive_predator%predators_seen(i) ) <  &
                        target_object%distance(                               &
                          this_agent%perceive_predator%predators_seen(i) ) )  &
          then
          risk_pred_expect(i) =                                               &
            tmp_predator%risk_fish( prey_spatial=target_position_agent,       &
                                    prey_length=this_agent%get_length(),      &
                                    prey_distance=this%distance,              &
                                    is_freezing=.FALSE.,                      &
                                    time_step_model=time_step_model_here,     &
                                    debug_plot_file=                          &
                                          "plot_debug_exp_predation_risk_" // &
                                          debug_plot_file_sufx )
        !> - Otherwise, if the agent is going to relocate to a more remote
        !!   location from the *i*-th predator (@f$ D_{AP} > D_{CP} @f$), the
        !!   baseline predation risk the_environment::predator::risk_fish() is
        !!   diluted by a factor constant that is defined by the parameter
        !!   commondata::approach_conspecfic_dilute_adjust_pair_behind
        !!   (normally 1/2, i.e. diluted halfway by the target conspecific that
        !!   is going to be closer to this predator).
        else GO_CLOSER
          risk_pred_expect(i) =                                               &
            APPROACH_CONSPECFIC_DILUTE_ADJUST_PAIR_BEHIND *                   &
            tmp_predator%risk_fish( prey_spatial=target_position_agent,       &
                                    prey_length=this_agent%get_length(),      &
                                    prey_distance=this%distance,              &
                                    is_freezing=.FALSE.,                      &
                                    time_step_model=time_step_model_here,     &
                                    debug_plot_file=                          &
                                          "plot_debug_exp_predation_risk_" // &
                                          debug_plot_file_sufx )
        end if GO_CLOSER

      end do PRED_PERCEPT

      !> - Finally, the value of the overall direct predation risk expected if
      !!   the agent approaches the target conspecific is calculated as the
      !!   maximum value of the expected risks across all predators in the
      !!   perception object.
      !! .
      this%expected_pred_dir_risk = maxval(risk_pred_expect)

      !> - The array of the expected direct risks from each of the predators
      !!   in perception is logged out in the DEBUG mode.
      call LOG_DBG( LTAG_INFO // "Saved direct predation risks for " //       &
                    TOSTR(n_pred_now) // " predators in perception; " //      &
                    " the maximum value is: " //                              &
                    TOSTR(this%expected_pred_dir_risk) //                     &
                    "; full array: " // TOSTR(risk_pred_expect),              &
                    PROCNAME, MODNAME )

    end if NO_PREDATORS

    !> #### Calculate the expected food gain ####
    !> The expected food gain is assumed to be **reduced** due to possible
    !! competition if the agent approaches a conspecific. Furthermore, the
    !! competition effect should depend on the relative body masses of the
    !! actor agent and the target conspecific.
    !!
    !> First, a baseline assessment of the food gain @f$ f_0 @f$ is calculated
    !! that does not take into account any effects of competition with the
    !! target conspecific. It is equal to the average mass of all food items in
    !! the current food perception object weighted by the subjective
    !! probability of food item capture that is calculated based on the memory
    !! the_neurobio::perception::food_probability_capture_subjective().
    !! (The mass is zero if there are no food items perceived).
    food_gain_expect_baseline = this_agent%perceive_food%get_meanmass() *     &
                      this_agent%food_probability_capture_subjective(         &
                              predict_window_food_here, time_step_model_here )

    !> The expected value of the food gain when the agent is about to approach
    !! the target conspecific is calculated as the baseline expected food gain
    !! @f$ f_0 @f$ multiplied by a nonparametric weighting function that
    !! depends on the ratio of the body mass of the actor agent
    !! @f$ M @f$ and the target conspecific @f$ M_{TC} @f$:
    !! @f[ f = f_0 \Phi ( \frac{M}{M_{TC}} ) . @f]
    !! The function @f$ \Phi @f$ is defined by the grid set by the arrays
    !! commondata::approach_food_gain_compet_factor_abscissa and
    !! commondata::approach_food_gain_compet_factor_ordinate.
    body_mass_ratio = this_agent%get_mass() / consp_mass
    !> @image html img_doxygen_approach_consp_food.svg
    !! @image latex img_doxygen_approach_consp_food.eps "Food competition factor for expected food gain" width=14cm
    !! @note The maximum value of the grid abscissa defines the body mass ratio
    !!       that guarantees 100% expectancy of winning of competition for food
    !!       against the target conspecific. For example, the value of 1.5
    !!       means that an agent is guaranteed to get the whole baseline
    !!       expected food gain if its body weight is 1.5 of the target
    !!       conspecific. The grid ordinate corresponding to the abscissa 1.0
    !!       determines the food gain weighting when the body sizes of the
    !!       agent and the target conspecifics are equal, e.g. 0.5 points to
    !!       equal share by equal competitive ability.
    this%expected_food_gain =                                                 &
                    food_gain_expect_baseline *                               &
                    DDPINTERPOL( APPROACH_FOOD_GAIN_COMPET_FACTOR_ABSCISSA,   &
                                 APPROACH_FOOD_GAIN_COMPET_FACTOR_ORDINATE,   &
                                 body_mass_ratio )

    !> Interpolation plots can be saved in the @ref intro_debug_mode
    !! "debug mode" using this plotting command:
    !! `commondata::debug_interpolate_plot_save()`.
    !! @warning Involves **huge** number of plots, should normally be
    !!          disabled.
    call debug_interpolate_plot_save(                                         &
                    grid_xx=APPROACH_FOOD_GAIN_COMPET_FACTOR_ABSCISSA,        &
                    grid_yy=APPROACH_FOOD_GAIN_COMPET_FACTOR_ORDINATE,        &
                    ipol_value=body_mass_ratio, algstr="DDPINTERPOL",         &
                    output_file="plot_debug_expect_food_gain_" //             &
                                debug_plot_file_sufx )

  end subroutine approach_conspecifics_do_this

  !-----------------------------------------------------------------------------
  !> `the_behaviour::approach_conspec::expectancies_calculate()` (re)calculates
  !! motivations from fake expected perceptions following from the procedure
  !! the_behaviour::approach_conspec::do_this().
  subroutine approach_conspecifics_motivations_expect( this, this_agent,      &
         target_object, target_offset, time_step_model, rescale_max_motivation)
    class(APPROACH_CONSPEC), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which approaches a target
    !!            conspecific.
    class(APPRAISAL), intent(in)    :: this_agent
    !> @param[in] target_object is the spatial target object the actor agent
    !!            is going to approach.
    class(SPATIAL), optional, intent(in) :: target_object
    !> @param[in] target_offset is an optional offset for the target, so that
    !!            the target position of the approaching agent does not
    !!            coincide with the target object. If absent, a default value
    !!            set by the commondata::approach_offset_default is used.
    real(SRP), optional, intent(in) :: target_offset
    !> @param[in] time_step_model optional time step of the model, overrides
    !!            the value calculated from the spatial data.
    integer, optional, intent(in)   :: time_step_model
    !> @param[in] rescale_max_motivation optional maximum motivation value for
    !!            rescaling all motivational components for comparison
    !!            across all motivation and perceptual components and behaviour
    !!            units.
    real(SRP), optional, intent(in) :: rescale_max_motivation

    ! Local variables
    real(SRP) :: max_motivation ! Local max. over all motivation components.

    ! Local copy of optional target offset
    real(SRP) :: target_offset_here

    ! Local copy of optional model time step
    integer :: time_step_model_here

    ! Expected food item that is used in the calculations, its properties are
    ! based on the average food items that the agent perceives below.
    type(FOOD_ITEM) :: expected_food_item

    real(SRP) :: expected_food_item_distance

    !> The probability of capture of the expected food object.
    real(SRP) :: expected_food_item_prob_capture

    !> Expected food gain that is fitting into the stomach of the agent.
    real(SRP) :: expected_food_item_gain_fits

    ! Current stomach contents of the agent.
    real(SRP) :: agent_stomach

    !> ### Notable local variables ###
    !> A full list of @ref percept_overrides_lst "all perception overrides"
    !! is available in the description of the
    !! the_neurobio::percept_components_motiv::motivation_components()
    !! procedure.
    !> #### Perception overrides ####
    !> - **perception_override_pred_dir** is the expected direct predation risk.
    real(SRP) :: perception_override_pred_dir
    !> - **perception_override_predator** is the expected general predation
    !!   risk, that is based on a weighting of the current predation and
    !!   predation risk from the memory stack.
    real(SRP) :: perception_override_predator

    !> - **perception_override_food_dir** is the expected number of food items
    !!   in perception general predation.
    real(SRP) :: perception_override_food_dir

    !> - **perception_override_stomach** is the expected stomach contents as a
    !!   consequence of approach movement. Note that there is no food
    !!   consumption during approach.
    real(SRP) :: perception_override_stomach
    !> - **perception_override_bodymass** is the expected body mass as a
    !!   consequence of the approaching the target conspecific.
    real(SRP) :: perception_override_bodymass
    !> - **perception_override_energy** is the expected energy reserves
    !!   as a consequence of the escape movement. Calculated from the body
    !!   mass and weight.
    !! .
    real(SRP) :: perception_override_energy

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME =                                 &
                                  "(approach_conspecifics_motivations_expect)"

    !> ### Implementation details ###
    !> #### Checks and preparations ####
    !> Check optional time step parameter. If not provided, use global
    !! parameter value from commondata::global_time_step_model_current.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> Determine the target offset. Target offset `target_offset` can be
    !! provided as an optional dummy parameter to this procedure. However, if
    !! it is not provided explicitly, a default value is set as an average of
    !! the actor agent body length and the target conspecific body length.
    !! The the_neurobio::get_prop_size() method for polymorphic object gets
    !! the size of the target conspecific.
    if (present(target_offset)) then
      target_offset_here = target_offset
    else
      target_offset_here = ( this_agent%get_length() +                        &
                             get_prop_size(target_object) ) / 2.0_SRP
    end if

    !> #### Call do_this ####
    !> As the first step, we use the **do**-procedure
    !! approach_conspec::do_this() to perform the behaviour desired
    !! without changing either the agent or its environment, obtaining  the
    !! **subjective** values of the `this` behaviour components that later
    !! feed into the motivation **expectancy** functions:
    !!  - `perception_override_food_dir`
    !!  - `perception_override_pred_dir`
    !!  - `perception_override_predator`
    !!  - `perception_override_stomach`
    !!  - `perception_override_bodymass`
    !!  - `perception_override_energy`
    !!  .
    call this%do_this(  this_agent = this_agent,                              &
                        target_object = target_object,                        &
                        target_offset = target_offset_here,                   &
                        time_step_model = time_step_model_here )

    !> #### Calculate expected (fake) perceptions ####
    !> ##### Fake perception of stomach content #####
    !> First, create a fake food item with the spatial position identical to
    !! that of the agent. The position is used only to calculate the
    !! illumination and therefore visual range. The cost(s) are calculated
    !! providing explicit separate distance parameter, so the zero distance
    !! from the agent is inconsequential. The size of the
    !! food item is obtained from the expected food gain by the reverse
    !! calculation function the_environment::mass2size_food().
    !! Standard `make` method for the food item class is used.
    call expected_food_item%make(location=this_agent%location(),              &
                                 size=mass2size_food(this%expected_food_gain),&
                                 iid=UNKNOWN )

    !> Second, calculate the **probability of capture** of this expected food
    !! item. The probability of capture of the fake food item is calculated
    !! using the the_environment::food_item::capture_probability() backend
    !! assuming the distance to the food item is equal to the average distance
    !! of all food items in the **current perception** object. However, if the
    !! agent does not see any food items currently, the distance to the fake
    !! food item is assumed to be equal to the visibility range weighted by
    !! the (fractional) commondata::walk_random_dist_expect_food_uncertain_fact
    !! parameter. Thus, the expected *raw* food gain (in the `do`-function) is
    !! based on the past memory whereas the probability of capture is based
    !! on the latest perception experience.
    if ( this_agent%has_food() ) then
      expected_food_item_distance = this_agent%perceive_food%get_meandist()
    else
      ! TODO: add average food distances to perception memory
      expected_food_item_distance = expected_food_item%visibility() *         &
                                    DIST_EXPECT_FOOD_UNCERTAIN_FACT
    end if

    expected_food_item_prob_capture =                                         &
        expected_food_item%capture_probability(                               &
                                        distance=expected_food_item_distance )

    !> Third, the expected food gain corrected for fitting into the agent's
    !! current stomach (and subtracting capture cost) is obtained by
    !! the_body::condition::food_fitting(). It is then weighted by the
    !! expected capture probability. Note that the probability of capture
    !! (weighting factor) is calculated based on the current perception
    !! (see above), but the travel cost is based on the actual expected
    !! \%distance (see the_behaviour::walk_random::expectancies_calculate()
    !! for a similar procedure).
    expected_food_item_gain_fits =                                            &
        this_agent%food_fitting( this%expected_food_gain, this%distance )     &
        * expected_food_item_prob_capture

    !> **Stomach content**: the perception override value for the stomach
    !! content is obtained incrementing the current stomach contents by
    !! the nonzero expected food gain, adjusting also for the digestion
    !! decrement (the_body::stomach_emptify_backend()).
    agent_stomach = this_agent%get_stom_content()
    perception_override_stomach =                                             &
                max( ZERO,                                                    &
                     agent_stomach - stomach_emptify_backend(agent_stomach) + &
                        expected_food_item_gain_fits )

    !> **Body mass**: the **body mass** perception override is obtained by
    !! incrementing (or decrementing if the expected food gain is negative)
    !! the current body mass by the expected food gain and also subtracting
    !! the cost of living component.
    perception_override_bodymass =                                            &
                max( ZERO,                                                    &
                     this_agent%get_mass() -                                  &
                        this_agent%living_cost() +                            &
                        expected_food_item_gain_fits )

    !> **Energy**: The fake perception values for the energy reserves
    !! (`energy_override_perc`) using the `the_body::energy_reserve()`
    !! procedure.
    perception_override_energy = energy_reserve( perception_override_bodymass,&
                                                 this_agent%length() )

    !> **Direct food perception**: override is based on the current count
    !! of the food items in the perception object.
    !! @note Thus, the prediction of the food gain and stomach contents
    !!       (see above) are based on a lower value that results from
    !!       competition with the target conspecific. However, predicted
    !!       perception of the general food availability is based on the
    !!       current unmodified "objective" value.
    perception_override_food_dir = real(                                      &
                                    this_agent%perceive_food%get_count(), SRP)

    !> ##### Fake perception of predation risk #####
    !> **Predation risk**: finally, fake perceptions of predation risk are
    !! obtained from the values calculated in the `do` procedure:
    !! the_behaviour::approach_conspec::expected_pred_dir_risk and
    !! the_behaviour::approach_conspec::expected_predation_risk.
    perception_override_pred_dir = this%expected_pred_dir_risk
    perception_override_predator = this%expected_predation_risk

    !> #### Calculate motivation expectancies ####
    !> The next step is to calculate the motivational expectancies using the
    !! fake perceptions to override the default (actual agent's) values.
    !> At this stage, first, calculate motivation values resulting from the
    !! behaviour done (the_behaviour::approach_conspec::do_this()) at the
    !! previous steps:
    !! - what would be the motivation values *if* the agent does perform
    !!   the_behaviour::approach_conspec?
    !! .
    !! Technically, this is done by calling the **neuronal
    !! response function**, `percept_components_motiv::motivation_components()`
    !! method, for each of the motivational states with `perception_override_`
    !! dummy parameters overriding the default values.
    !! Here is the list of the fake overriding perceptions for the
    !! `the_behaviour::approach_conspec` behaviour:
    !!  - `perception_override_food_dir`
    !!  - `perception_override_pred_dir`
    !!  - `perception_override_predator`
    !!  - `perception_override_stomach`
    !!  - `perception_override_bodymass`
    !!  - `perception_override_energy`
    !!  .
    !  @note  **Expectancy** assessment for **hunger** motivation, using
    !         `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in] for
    !         `this_agent` now.
    call this%expectancy%hunger%percept_component%motivation_components       &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL,         &
      param_gp_matrix_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL,     &
      param_gp_matrix_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL,     &
      param_gp_matrix_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL,               &
      param_gp_matrix_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL,             &
      param_gp_matrix_age      = AGE_HUNGER_GENOTYPE_NEURONAL,                &
      param_gp_matrix_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL,            &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,     &
      param_gerror_cv_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_age      = AGE_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,      &
      param_gerror_cv_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,  &
      !  Real agent perception components are now substituted by the *fake*
      !  values resulting from executing this behaviour (`do_this` method).
      !  This is repeated for all the motivations: *hunger*,
      !  *passive avoidance,* *fear state* etc.
      perception_override_food_dir = perception_override_food_dir,            &
      perception_override_pred_dir = perception_override_pred_dir,            &
      perception_override_predator = perception_override_predator,            &
      perception_override_stomach = perception_override_stomach,              &
      perception_override_bodymass = perception_override_bodymass,            &
      perception_override_energy = perception_override_energy                 &
                                                                              )
      !> Real agent perception components are now substituted by the *fake*
      !! values resulting from executing this behaviour (`approach::do_this()`
      !! method). This is repeated for all the motivations: *hunger*,
      !! *passive avoidance,* *fear state* etc. These optional **override
      !! parameters** are substituted by the "fake" values.

    !  @note  **Expectancy** assessment for **fear_defence** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%fear_defence%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL, &
      param_gp_matrix_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_food_dir = perception_override_food_dir,            &
      perception_override_pred_dir = perception_override_pred_dir,            &
      perception_override_predator = perception_override_predator,            &
      perception_override_stomach = perception_override_stomach,              &
      perception_override_bodymass = perception_override_bodymass,            &
      perception_override_energy = perception_override_energy                 &
                                                                              )

    !  @note  **Expectancy** assessment for **reproduction** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%reproduction%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL,    &
      param_gp_matrix_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_food_dir = perception_override_food_dir,            &
      perception_override_pred_dir = perception_override_pred_dir,            &
      perception_override_predator = perception_override_predator,            &
      perception_override_stomach = perception_override_stomach,              &
      perception_override_bodymass = perception_override_bodymass,            &
      perception_override_energy = perception_override_energy                 &
                                                                              )

    !> #### Calculate primary and final motivations ####
    !> Next, from the perceptual components calculated at the previous
    !! step we can obtain the **primary** and **final motivation** values by
    !! weighed summing.
    if (present(rescale_max_motivation)) then
      !> Here we can use global maximum motivation across all behaviours and
      !! perceptual components if it is provided, for rescaling.
      max_motivation = rescale_max_motivation
    else
      !> Or can rescale using local maximum value for this behaviour only.
      max_motivation =  this%expectancy%max_perception()
    end if

    !> Transfer attention weights from the actor agent `this_agent` to the
    !! `this` behaviour component. So, we will now use the updated modulated
    !! attention weights of the agent rather than their default parameter
    !! values.
    call this%attention_transfer(this_agent)

    !> So the primary motivation values are calculated.
    call this%expectancy%motivation_primary_calc(max_motivation)

    !> Primary motivations are logged in the @ref intro_debug_mode "debug mode".
    call LOG_DBG( LTAG_INFO // "Primary motivations: " //                     &
                  "hunger: " //                                               &
                    TOSTR(this%expectancy%hunger%motivation_prim)  //         &
                  ", fear_defence: " //                                       &
                    TOSTR(this%expectancy%fear_defence%motivation_prim) //    &
                  ", reproduce: " //                                          &
                    TOSTR(this%expectancy%reproduction%motivation_prim),      &
                  PROCNAME, MODNAME )

    !> There is **no modulation** at this stage, so the final motivation
    !! values are the same as primary motivations.
    call this%expectancy%modulation_none()

    !> #### Calculate motivation expectancies ####
    !> Finally, calculate the finally **expected arousal level for this
    !! behaviour**. As in the GOS, the overall arousal is the maximum value
    !! among all motivation components.
    this%arousal_expected = this%expectancy%max_final()

    !> Log also the final expectancy value in the @ref intro_debug_mode
    !! "debug mode".
    call LOG_DBG( LTAG_INFO // "Expectancy: " // TOSTR(this%arousal_expected),&
                  PROCNAME, MODNAME )

    !> Now as we know the expected arousal, we can choose the behaviour which
    !! would minimise this arousal level.

  end subroutine approach_conspecifics_motivations_expect

  !-----------------------------------------------------------------------------
  !> Initialise the **migrate** behaviour component to a zero state.
  elemental subroutine migrate_init_zero(this)
    class(MIGRATE), intent(inout) :: this

    !> First init components from the base root class
    !! `the_behaviour::behaviour_base`.
    !> Mandatory label component that should be read-only.
    this%label = "MIGRATE"
    !> The execution status is always FALSE, can be reset to TRUE only when
    !! the behaviour unit is called to execution.
    this%is_active = .FALSE.

    !> And the *expectancy* components.
    call this%expectancy%init()
    this%arousal_expected = 0.0_SRP

    !> Abstract `MOVE` component.
    this%distance = MISSING

    !> Then init components of this specific behaviour component extended class.
    this%target_point = SPATIAL(x=MISSING, y=MISSING, depth=MISSING)
    this%expected_cost_moving = MISSING
    this%expected_food_gain = MISSING
    this%expected_food_dir = MISSING
    this%expected_consp_number = UNKNOWN
    this%expected_pred_dir_risk = MISSING
    this%expected_predation_risk = MISSING

  end subroutine migrate_init_zero

  !-----------------------------------------------------------------------------
  !> The "do" procedure component of the behaviour element performs the
  !! behaviour without affecting the actor agent (the_agent) and the world
  !! (here food_item_eaten) which have intent(in), so it only can change
  !! the internal representation of the behaviour (the type to which this
  !! procedure is bound to, here `MIGRATE`).
  subroutine migrate_do_this( this, this_agent, target_env,                   &
              predict_window_food, predict_window_consp, predict_window_pred, &
                              time_step_model )
    class(MIGRATE), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which eats the food item.
    class(APPRAISAL), intent(in) :: this_agent
    !> @param[in] target_env the target environment the actor agent is going
    !!            to (e)migrate into.
    class(ENVIRONMENT), intent(in) :: target_env
    !> @param[in] predict_window_food optional size of the *food* prediction
    !!            window, i.e. how many steps back in memory are used to
    !!            calculate the predicted food gain. This parameter is limited
    !!            by the maximum commondata::history_size_perception value of
    !!            the perception memory history size.
    integer, optional, intent(in) :: predict_window_food
    !> @param[in] predict_window_consp optional size of the *conspecifics*
    !!            prediction window, i.e. how many steps back in memory are
    !!            used to calculate the predicted food gain. This parameter
    !!            is limited by the maximum commondata::history_size_perception
    !!            value of the perception memory history size.
    integer, optional, intent(in) :: predict_window_consp
    !> @param[in] predict_window_pred optional size of the *predator*
    !!            prediction window, i.e. how many steps back in memory are
    !!            used to calculate the predicted food gain. This parameter
    !!            is limited by the maximum commondata::history_size_perception
    !!            value of the perception memory history size.
    integer, optional, intent(in) :: predict_window_pred
    !> @param[in] time_step_model optional time step of the model, overrides
    !!            the value calculated from the spatial data.
    integer, optional, intent(in)   :: time_step_model

    ! Local copies of optionals.
    integer   :: predict_window_food_here, predict_window_consp_here,         &
                 predict_window_pred_here, time_step_model_here

    ! Local copy of the body length of the agent
    real(SRP) :: agent_length

    ! - **WEIGHT_DIRECT**  is the relative weight  given to the immediate
    !   perception of predators over the predators counts in the memory stack.
    !   Obtained from global parameters
    !   (`commondata::predation_risk_weight_immediate`).
    real(SRP), parameter :: WEIGHT_DIRECT = PREDATION_RISK_WEIGHT_IMMEDIATE

    !> ### Notable variables ###
    !> - **point_target_env** is the target point inside the target
    !!   environment to which this agent is going to relocate.
    type(SPATIAL) :: point_target_env
    !> - **distance_target** is the distance to the target environment
    real(SRP) :: distance_target

    !> - **mean_n_food_memory_old, mean_n_food_memory_new** are the average
    !!   numbers of food items in the past memory window, the "older" and
    !!   "newer" parts that are used to calculate the "older"
    !!   @f$ \overline{f_1} @f$ and "newer" @f$ \overline{f_2} @f$
    !!   values of food availability retrieved from the perception memory.
    !!   Used in calculation of the the_behaviour::hope function.
    real(SRP) :: mean_n_food_memory_old, mean_n_food_memory_new

    !> - **mean_size_food_memory_old, mean_size_food_memory_new** are the
    !!   average sizes of food items in the past memory window, the "older"
    !!   and "newer" parts that are used to calculate the "older"
    !!   @f$ \overline{f_1} @f$ and "newer" @f$ \overline{f_2} @f$
    !!   values of food availability retrieved from the perception memory.
    !!   Used in calculation of the the_behaviour::hope function.
    real(SRP) :: mean_size_food_memory_old, mean_size_food_memory_new

    !> - **food_gain_memory_old, food_gain_memory_new** are the "older"
    !!   @f$ \overline{f_1} @f$ and "newer" @f$ \overline{f_2} @f$
    !!   values of food availability retrieved from the perception memory.
    !!   Used in calculation of the the_behaviour::hope function.
    real(SRP) :: food_gain_memory_old, food_gain_memory_new

    !> - **food_gain_memory_baseline** is the baseline value of the food gain
    !!   retrieved from the memory, that is used to calculate the actual
    !!   food gain expectancy value calculated from the hope function.
    real(SRP) :: food_gain_memory_baseline

    !> - **mean_n_pred_memory_old, mean_n_pred_memory_new** are the average
    !!   numbers of predators in the past perception memory window.
    real(SRP) :: mean_n_pred_memory_old, mean_n_pred_memory_new

    !> - **pred_current** is the current estimate of the general predation
    !!   risk.
    !! .
    real(SRP) :: pred_current

    ! PROCNAME is the procedure name for logging and debugging.
    character(len=*), parameter :: PROCNAME = "(migrate_do_this)"

    !> ### Implementation details ###
    !> #### Checks and preparations ####
    !> Check optional parameter for the food perception memory window. If
    !! the `predict_window_food` dummy parameter is not provided, its default
    !! value is its default value is the whole memory stack
    !! commondata::history_size_perception.
    if (present(predict_window_food)) then
      predict_window_food_here = predict_window_food
    else
      predict_window_food_here = HISTORY_SIZE_PERCEPTION
    end if

    !> Check optional parameter for the conspecifics perception
    !! memory window. If the `predict_window_consp` dummy parameter is not
    !! provided, its default value is the whole memory stack
    !! commondata::history_size_perception.
    if (present(predict_window_consp)) then
      predict_window_consp_here= predict_window_consp
    else
      predict_window_consp_here = HISTORY_SIZE_PERCEPTION
    end if

    !> Check optional parameter for the general predation risk perception
    !! memory window. If the `predict_window_pred` dummy parameter is not
    !! provided, its default value is the whole memory stack
    !! commondata::history_size_perception.
    if (present(predict_window_pred)) then
      predict_window_pred_here= predict_window_pred
    else
      predict_window_pred_here = HISTORY_SIZE_PERCEPTION
    end if

    !> Check optional time step parameter. If unset, use global
    !! `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    ! Agent length is local variable to avoid multiple calls to get_length().
    agent_length = this_agent%get_length()

    !> #### Calculate the distance towards the target environment ####
    !> The distance towards the target environment (and the target point in
    !! this environment) is defined as the minimum  distance towards
    !! all segments limiting this environment in the 2D X x Y projection
    !! @warning This is valid only for the simple box environment
    !!          implementation. Generally, it equals to the minimum
    !!          distance across all the polyhedrons limiting the target
    !!          environment).
    !!
    !! The target point for the migrating agent within the target
    !! environment is then not just the edge of the target environment, but
    !! some point penetrating inside to some distance defined by the parameter
    !! commondata::migrate_dist_penetrate_offset (in units of the agent's
    !! body length). The the_environment::environment::nearest_target()
    !! method is used to find the closest point in the target environment and
    !! the (smallest) distance towards this environment, these values are
    !! adjusted automatically for the offset parameter in the procedure call.
    call target_env%nearest_target( outside_object=this_agent,                &
                                    offset_into=agent_length *                &
                                            MIGRATE_DIST_PENETRATE_OFFSET,    &
                                    point_spatial = point_target_env,         &
                                    point_dist = distance_target  )

    !> The distance value returned from the
    !! the_environment::environment::nearest_target() is saved into the
    !! this\%distance data component and the target point (of class
    !! the_environment::spatial) is saved into the this\%target_point
    !! data component.
    this%distance = distance_target
    this%target_point = point_target_env

    !> Check if the distance to the target environment exceeds the
    !! migration travel maximum value, set as commondata::migrate_dist_max_step
    !! body sizes of the agent.
    if (this%distance > agent_length * MIGRATE_DIST_MAX_STEP ) then
      !> - So far nothing is done in such a case except logging a warning.
      !!   Note that in the_behaviour::migrate::migrate_do_execute() method,
      !!   agents that had the distance exceeding this threshold do a random
      !!   correlated walk towards the target environment, but do not enter
      !!   it.
      !! .
      call LOG_DBG( LTAG_WARN // "Migration travel distance exceeds big " //  &
                    "threshold in " // PROCNAME // " for the agent " //       &
                    this_agent%individ_label() // ". Agent length: " //       &
                    TOSTR(agent_length) // ", migration distance: " //        &
                    TOSTR(this%distance), PROCNAME, MODNAME   )
    end if

    !> #### Calculate expected cost of the swimming ####
    !> The expected cost of swimming in the random walk depends on the walk
    !! distance and is calculated using the the_body::condition::cost_swim()
    !! assuming *laminar* flow (laminar flow is due to normal relatively slow
    !! swimming pattern).
    this%expected_cost_moving =                                               &
            this_agent%cost_swim( distance=this%distance,                     &
                                  exponent=SWIMMING_COST_EXPONENT_LAMINAR )

    !> #### Calculate expected food gain ####
    !> The expected food gain resulting from emigrating into a completely
    !! different novel habitat cannot be assessed based only on current
    !! perception because the agent has virtually no information (i.e. no
    !! perception) about this habitat yet. The target habitat is a novel
    !! environment about which the agent has absolutely no local knowledge.
    !! A mechanism based on the **hope function** (the_behaviour::hope())
    !! is used here. Specifically, the hope function calculates the expected
    !! food gain in the target novel habitat based on the ratio of the
    !! "newer" to "older" food gains in the perceptual memory of the agent.
    !!
    !> Calculation of the "older" and "newer" average food gain values from
    !! the memory involves several steps. First, average *number* of food
    !! items and the average *size* of the food items in the above two halves
    !! of the memory stack is calculated using the
    !! the_neurobio::memory_perceptual::get_food_mean_n_split() and
    !! the_neurobio::memory_perceptual::get_food_mean_size_split() procedures.
    !! (Note that the `split_val` parameter to this procedure is not
    !! provided so the default 1/2 split is used.)
    call this_agent%memory_stack%get_food_mean_n_split(                       &
                                        window = predict_window_food_here,    &
                                        older = mean_n_food_memory_old,       &
                                        newer = mean_n_food_memory_new )

    call this_agent%memory_stack%get_food_mean_size_split(                    &
                                        window = predict_window_food_here,    &
                                        older = mean_size_food_memory_old,    &
                                        newer = mean_size_food_memory_new )

    !> Second, the values of the "old" and "new" *food gain* used to calculate
    !! the expectations are obtained by weighting the respective average mass
    !! of the food item by the average number of food items if this number is
    !! less than 1 or 1 (i.e. unweighted) if their average number is higher.
    !  Latex formulas don't render correctly in Doxygen
    !    @f[
    !      \left\{\begin{matrix}
    !          f_{1}=\overline{m_1} \cdot \overline{n_1}, & \overline{n_1}<1; \\
    !          f_{1}=\overline{m_1}, & \overline{n_1} \geq 1
    !      \end{matrix}\right
    !    @f]
    !    @f[
    !      \left\{\begin{matrix}
    !          f_{2}=\overline{m_2} \cdot \overline{n_2}, & \overline{n_2}<1; \\
    !          f_{2}=\overline{m_2}, & \overline{n_2} \geq 1
    !      \end{matrix}\right.
    !    @f]
    !> @image html img_doxygen_migrate_formula_1.svg
    !! @image latex img_doxygen_migrate_formula_1.eps "" width=14cm
    !> where @f$ \overline{m_1} @f$ is the average mass of the food items
    !! and @f$ \overline{n_1} @f$ is the average number of food items
    !! in the "older" half of the perceptual memory stack and
    !! @f$ \overline{m_2} @f$ is the average mass of the food items
    !! and @f$ \overline{n_2} @f$ is the average number of food items
    !! in the "newer" half of the memory stack.
    !!
    !! Thus, if the agent had some relatively poor perceptual history of
    !! encountering food items, so that the average *number* of  food items
    !! is fractional < 1 (e.g. average number 0.5, meaning that it has seen a
    !! single food item approximately every other time step), the food gain is
    !! weighted by this fraction (0.5). If, on the other hand, the agent had
    !! more than one food items at each time step previously, the average food
    !! item size is unweighted (weight=1.0). This conditional weighting
    !! reflects the fact that it is not possible to eat more than
    !! one food item at a time in this model version.
    !! @note A similar expectancy assessment mechanism is used in the
    !!       assessment of the food gain expectancy for the
    !!       the_behaviour::walk_random behaviour component
    !!       the_behaviour::walk_random_do_this().
    food_gain_memory_old = size2mass_food(mean_size_food_memory_old) *        &
                            within( mean_n_food_memory_old, 0.0_SRP, 1.0_SRP )
    food_gain_memory_new = size2mass_food(mean_size_food_memory_new) *        &
                            within( mean_n_food_memory_new, 0.0_SRP, 1.0_SRP )

    ! Produce diagnostic logger message in the @ref intro_debug_mode DEBUG mode.
    call LOG_DBG( LTAG_INFO // "Mean number of old and new " //               &
                  "food items in memory: " //                                 &
                  TOSTR(mean_n_food_memory_old)  //                           &
                  ":" // TOSTR(mean_n_food_memory_new),                       &
                  PROCNAME, MODNAME )
    call LOG_DBG( LTAG_INFO // "Mean size of old and new " //                 &
                  "food items in memory: " //                                 &
                  TOSTR(mean_size_food_memory_old)                            &
                  // ":" // TOSTR(mean_size_food_memory_new),                 &
                  PROCNAME, MODNAME )
    call LOG_DBG( LTAG_INFO // "Food gain old and new " //                    &
                  "food items in memory: " // TOSTR(food_gain_memory_old) //  &
                  ":" // TOSTR(food_gain_memory_new),                         &
                  PROCNAME, MODNAME )
    LOG_RATIO_CHECK: if (IS_DEBUG) then
      block
        real(SRP) :: debug_ratio
        ! @warning The logic of the `ìf` condition should be the same as
        !          in the the_behaviour::hope() function.
        if (food_gain_memory_old < ZERO) then
          debug_ratio = MIGRATE_FOOD_GAIN_RATIO_ZERO_HOPE
        elseif (food_gain_memory_old < ZERO .and.                             &
                food_gain_memory_new < ZERO) then
          debug_ratio = 1.0_SRP
        else
          debug_ratio = food_gain_memory_new / food_gain_memory_old
        end if
        call LOG_DBG( LTAG_INFO // "Food gain hope ratio (new/old): " //      &
                      TOSTR(debug_ratio), PROCNAME, MODNAME )
      end block
    end if LOG_RATIO_CHECK

    !> The next step is to calculate the baseline food gain @f$ f_0 @f$,
    !! against which the expectancy based on the the_behaviour::hope() function
    !! is evaluated. This baseline value is obtained by weighting the average
    !! mass of the food items in the whole memory stack @f$ \overline{m} @f$
    !! by their average number @f$ \overline{n} @f$ provided this number
    !! is *n<1* as above:
    !  Latex formula below doesn't render well:
    !    @f[
    !      \left\{\begin{matrix}
    !          f_0=\overline{m} \cdot \overline{n}, & \overline{n}<1; \\
    !          f_0=\overline{m}, & \overline{n} \geq 1
    !      \end{matrix}\right.
    !    @f]
    !> @image html img_doxygen_migrate_formula_2.svg
    !! @image latex img_doxygen_migrate_formula_2.eps "" width=14cm
    !! This baseline value is then weighted by the subjective probability
    !! of food item capture that is calculated based on the memory
    !! the_neurobio::perception::food_probability_capture_subjective().
    food_gain_memory_baseline =                                               &
        size2mass_food(                                                       &
            this_agent%memory_stack%get_food_mean_size(                       &
                predict_window_food_here)) *                                  &
        within(                                                               &
            this_agent%memory_stack%get_food_mean_n(predict_window_food_here),&
            0.0_SRP, 1.0_SRP ) *                                              &
        this_agent%food_probability_capture_subjective(                       &
                              predict_window_food_here, time_step_model_here )

    !> Finally, the the_behaviour::hope() function is called with the above
    !! estimates for the baseline food gain, its "older" and "newer" values.
    !! The *zero hope ratio* and the *maximum hope* parameters are obtained from
    !! commondata::migrate_food_gain_ratio_zero_hope and
    !! commondata::migrate_food_gain_maximum_hope parameter constants.
    !! @image html img_doxygen_migrate_hope_food_nonpar.svg  "The hope function"
    !! @image latex img_doxygen_migrate_hope_food_nonpar.eps "The hope function" width=14cm
    this%expected_food_gain = hope( food_gain_memory_baseline,                &
                                    food_gain_memory_old,                     &
                                    food_gain_memory_new,                     &
                                    MIGRATE_FOOD_GAIN_RATIO_ZERO_HOPE,        &
                                    MIGRATE_FOOD_GAIN_MAXIMUM_HOPE  )

    ! Produce diagnostic logger message in the @ref intro_debug_mode DEBUG mode.
    call LOG_DBG( LTAG_INFO // "Expected food gain from hope function: " //   &
                  TOSTR(this%expected_food_gain) // ", with baseline " //     &
                  "value: " // TOSTR(food_gain_memory_baseline),              &
                  PROCNAME, MODNAME  )

    !> #### Calculate expected food items perception ####
    !> A similar, although simpler, procedure based on the the_behaviour::hope
    !! function as above is used to calculate the expected *number* of food
    !! items perceived in the target novel habitat.
    !!
    !! Here, the baseline value @f$ f_0 @f$ is the current number of food
    !! items in the food perception object, and the historical ratio
    !! @f$ \varrho @f$ is calculated as the mean number of food items in the
    !! old to new memory parts:
    !! @f[ \varrho = \frac{\overline{n_2}}{\overline{n_1}} . @f]
    !! The *zero hope ratio* and the *maximum hope* parameters are also
    !! obtained from commondata::migrate_food_gain_ratio_zero_hope and
    !! commondata::migrate_food_gain_maximum_hope parameter constants.
    this%expected_food_dir =                                                  &
                      hope( real(this_agent%perceive_food%get_count(),SRP),   &
                            mean_n_food_memory_old,                           &
                            mean_n_food_memory_new,                           &
                            MIGRATE_FOOD_GAIN_RATIO_ZERO_HOPE,                &
                            MIGRATE_FOOD_GAIN_MAXIMUM_HOPE  )

    ! Produce diagnostic logger message in the @ref intro_debug_mode DEBUG mode.
    call LOG_DBG( LTAG_INFO // "Expected food perception from hope " //       &
                  "function: " // TOSTR(this%expected_food_dir ),             &
                  PROCNAME, MODNAME  )

    !> #### Calculate expected predation risks ####
    !> **Direct predation** risk is assumed to be zero for migration.
    this%expected_pred_dir_risk = 0.0_SRP

    !> **General predation** risk expectancy is not possible to determine
    !! because there is no local perception of the target novel environment
    !! yet. Therefore, its assessment is based on the the_behaviour::hope()
    !! function, just as the expected food gain.
    !! - First, calculate the older and newer predation averages from the
    !!   memory stack;
    call this_agent%memory_stack%get_pred_mean_split(                         &
                                        window = predict_window_pred_here,    &
                                        older = mean_n_pred_memory_old,       &
                                        newer = mean_n_pred_memory_new )

    !> - Second, calculate the *current* general risk of predation, based
    !!   on the local perception. This is done calling the
    !!   the_neurobio::predation_risk_backend() function. This current risk
    !!   serves as a baseline value (@f$ f_0 @f$) for calculation of the
    !!   general risk in the target novel environment.
    pred_current =                                                            &
        predation_risk_backend(                                               &
                    pred_count=this_agent%perceive_predator%get_count(),      &
                    pred_memory_mean=this_agent%memory_stack%get_pred_mean(   &
                                                    predict_window_pred_here),&
                    weight_direct=WEIGHT_DIRECT )

    !> - Third, the expectancy value of general predation risk in the target
    !!   novel environment is obtained via the the_behaviour::hope() function.
    !!   If the general predation risk is increasing in the local environment,
    !!   its expectancy in the novel environment diminishes, if the risk is
    !!   reducing over time in the local environment, the novel environment
    !!   expectancy increases. The hope grid values for the general predation
    !!   hope function are defined by the commondata::migrate_predator_zero_hope
    !!   and commondata::migrate_predator_maximum_hope parameter constants.
    this%expected_predation_risk = hope( pred_current,                        &
                                         mean_n_pred_memory_old,              &
                                         mean_n_pred_memory_new,              &
                                         MIGRATE_PREDATOR_ZERO_HOPE,          &
                                         MIGRATE_PREDATOR_MAXIMUM_HOPE  )

    !> #### Calculate expected conspecifics ####
    !> The expected number of conspecifics in the target environment is
    !! calculated as an average retrieved from the memory stack with the
    !! memory window defined by `predict_window_consp_here`.
    this%expected_consp_number = nint(                                        &
          this_agent%memory_stack%get_consp_mean_n(predict_window_consp_here) )

    call LOG_DBG( LTAG_INFO // "Expected N of conspecifics: " //              &
                  TOSTR(this%expected_consp_number), PROCNAME, MODNAME )

  end subroutine migrate_do_this

  !-----------------------------------------------------------------------------
  !> `the_behaviour::migrate::expectancies_calculate()` (re)calculates
  !! motivations from fake expected perceptions following from the procedure
  !! `migrate::do_this()`.
  subroutine migrate_motivations_expect(this, this_agent, target_env,         &
            predict_window_food, predict_window_consp, predict_window_pred,   &
                                    time_step_model, rescale_max_motivation)
    class(MIGRATE), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which is going to migrate.
    class(APPRAISAL), intent(in)    :: this_agent
    !> @param[in] target_env the target environment the actor agent is going
    !!            to (e)migrate into.
    class(ENVIRONMENT), intent(in) :: target_env
    !> @param[in] predict_window_food optional size of the *food* prediction
    !!            window, i.e. how many steps back in memory are used to
    !!            calculate the predicted food gain. This parameter is limited
    !!            by the maximum commondata::history_size_perception value of
    !!            the perception memory history size.
    integer, optional, intent(in) :: predict_window_food
    !> @param[in] predict_window_consp optional size of the *conspecifics*
    !!            prediction window, i.e. how many steps back in memory are
    !!            used to calculate the predicted food gain. This parameter
    !!            is limited by the maximum commondata::history_size_perception
    !!            value of the perception memory history size.
    integer, optional, intent(in) :: predict_window_consp
    !> @param[in] predict_window_pred optional size of the *predator*
    !!            prediction window, i.e. how many steps back in memory are
    !!            used to calculate the predicted food gain. This parameter
    !!            is limited by the maximum commondata::history_size_perception
    !!            value of the perception memory history size.
    integer, optional, intent(in) :: predict_window_pred
    !> @param[in] time_step_model optional time step of the model, overrides
    !!            the value calculated from the spatial data.
    integer, optional, intent(in)   :: time_step_model
    !> @param[in] rescale_max_motivation optional maximum motivation value for
    !!            rescaling all motivational components for comparison
    !!            across all motivation and perceptual components and behaviour
    !!            units.
    real(SRP), optional, intent(in) :: rescale_max_motivation

    ! PROCNAME is the procedure name for logging and debugging.
    character(len=*), parameter :: PROCNAME = "(migrate_motivations_expect)"

    ! Local copies of optionals.
    integer   :: predict_window_food_here, predict_window_consp_here,         &
                 predict_window_pred_here, time_step_model_here

    ! Local variables
    real(SRP) :: max_motivation ! Local max. over all motivation components.

    ! Current stomach contents of the agent.
    real(SRP) :: agent_stomach

    ! Expected food item that is used in the calculations, its properties are
    ! based on the average food items that the agent perceives below.
    type(FOOD_ITEM) :: expected_food_item

    real(SRP) :: expected_food_item_distance

    !> Expected food gain that is fitting into the stomach of the agent.
    real(SRP) :: expected_food_item_gain_fits

        !> The probability of capture of the expected food object.
    real(SRP) :: expected_food_item_prob_capture

    !> ### Notable local variables ###
    !> #### Perception overrides ####
    !> - **perception_override_conspec** is the expected number of
    !!   conspecifics.
    real(SRP) :: perception_override_conspec

    !> - **perception_override_pred_dir** is the expected direct
    !!   predation risk.
    real(SRP) :: perception_override_pred_dir
    !> - **perception_override_predator** is the expected general predation
    !!   risk, that is based on a weighting of the current predation and
    !!   predation risk from the memory stack.
    real(SRP) :: perception_override_predator
    !> - **perception_override_food_dir** is the expected number of food items
    !!   in perception.
    real(SRP) :: perception_override_food_dir
    !> - **perception_override_stomach** is the expected stomach contents
    !!   as a consequence of random walk.
    real(SRP) :: perception_override_stomach
    !> - **perception_override_bodymass** is the expected body mass as a
    !!   consequence of the random walk.
    real(SRP) :: perception_override_bodymass
    !> - **perception_override_energy** is the expected energy reserves
    !!   as a consequence of the escape movement. Calculated from the body
    !!   mass and weight.
    !! .
    real(SRP) :: perception_override_energy

    !> ### Implementation details ###
    !> #### Checks and preparations ####
    !> Check optional parameter for the food perception memory window. If
    !! the `predict_window_food` dummy parameter is not provided, its default
    !! value is its default value is the whole memory stack
    !! commondata::history_size_perception.
    if (present(predict_window_food)) then
      predict_window_food_here = predict_window_food
    else
      predict_window_food_here = HISTORY_SIZE_PERCEPTION
    end if

    !> Check optional parameter for the conspecifics perception
    !! memory window. If the `predict_window_consp` dummy parameter is not
    !! provided, its default value is the whole memory stack
    !! commondata::history_size_perception.
    if (present(predict_window_consp)) then
      predict_window_consp_here= predict_window_consp
    else
      predict_window_consp_here = HISTORY_SIZE_PERCEPTION
    end if

    !> Check optional parameter for the general predation risk perception
    !! memory window. If the `predict_window_pred` dummy parameter is not
    !! provided, its default value is the whole memory stack
    !! commondata::history_size_perception.
    if (present(predict_window_pred)) then
      predict_window_pred_here= predict_window_pred
    else
      predict_window_pred_here = HISTORY_SIZE_PERCEPTION
    end if

    !> Check optional time step parameter. If unset, use global
    !! commondata::global_time_step_model_current.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> #### Call do_this ####
    !> As the first step, we use the **do**-procedure `migrate::do_this()`
    !! => `the_behaviour::walk_random_do_this()` to perform the behaviour desired
    !! without changing either the agent or its environment, obtaining  the
    !! **subjective** values of the `this` behaviour components that later feed
    !! into the motivation **expectancy** functions:
    !!  - `perception_override_food_dir`
    !!  - `perception_override_conspec`
    !!  - `perception_override_pred_dir`
    !!  - `perception_override_predator`
    !!  - `perception_override_stomach`
    !!  - `perception_override_bodymass`
    !!  - `perception_override_energy`
    !!  .
    call this%do_this(  this_agent=this_agent,                                &
                        target_env=target_env,                                &
                        predict_window_food=predict_window_food_here,         &
                        predict_window_consp=predict_window_consp_here,       &
                        predict_window_pred=predict_window_pred_here,         &
                        time_step_model=time_step_model_here     )

    !> #### Calculate expected (fake) perceptions ####
    !> First, create a fake food item with the spatial position identical to
    !! that of the agent. The position is used to calculate the current
    !! illumination and therefore visual range. The cost(s) are calculated
    !! providing explicit separate distance parameter. The size of the
    !! food item is obtained from the expected food gain by the reverse
    !! calculation function the_environment::mass2size_food().
    !! Standard `make` method for the food item class is used.
    call expected_food_item%make(location=this_agent%location(),              &
                                 size=mass2size_food(this%expected_food_gain),&
                                 iid=UNKNOWN )

    !> Second, calculate the **probability of capture** of this expected food
    !! item. The probability of capture of the fake food item is calculated
    !! using the the_environment::food_item::capture_probability() backend
    !! assuming the distance to the food item is equal to the average distance
    !! of all food items in the **current perception** object. However, if the
    !! agent does not see any food items currently, the distance to the fake
    !! food item is assumed to be equal to the visibility range weighted by
    !! the (fractional) commondata::dist_expect_food_uncertain_fact
    !! parameter.
    ! TODO: add average distance to food items into perception memory and use
    ! it here.
    if ( this_agent%has_food() ) then
      expected_food_item_distance = this_agent%perceive_food%get_meandist()
    else
      ! TODO: add average food distances to perception memory
      expected_food_item_distance = expected_food_item%visibility() *         &
                                    DIST_EXPECT_FOOD_UNCERTAIN_FACT
    end if

    expected_food_item_prob_capture =                                         &
        expected_food_item%capture_probability(                               &
                                        distance=expected_food_item_distance )

    ! Produce diagnostic logger message in the @ref intro_debug_mode DEBUG mode.
    call LOG_DBG( LTAG_INFO // "Distance to the expected food item: "     //  &
                  TOSTR(expected_food_item_distance)                      //  &
                  ", capture prpbability of the expected food item: "     //  &
                  TOSTR(expected_food_item_prob_capture) // ".",              &
                  PROCNAME, MODNAME )

    !> Third, the expected food gain corrected for fitting into the agent's
    !! stomach and capture cost is obtained by
    !! the_body::condition::food_fitting(). It is then weighted by the
    !! expected capture probability.
    expected_food_item_gain_fits =                                            &
                            this_agent%food_fitting( this%expected_food_gain, &
                                expected_food_item_distance ) *               &
                            expected_food_item_prob_capture

    ! Produce diagnostic logger message in the @ref intro_debug_mode DEBUG mode.
    call LOG_DBG( LTAG_INFO // "Raw food gain: "                          //  &
                  TOSTR(this_agent%food_fitting( this%expected_food_gain,     &
                                expected_food_item_distance ))            //  &
                  ", subjective food gain weighted by capture prob.: "    //  &
                  TOSTR(expected_food_item_gain_fits) // ".",                 &
                  PROCNAME, MODNAME )

    !> **Stomach content**: the perception override value for the stomach
    !! content is obtained incrementing the current stomach contents by
    !! the nonzero expected food gain, adjusting also for the digestion
    !! decrement (the_body::stomach_emptify_backend()).
    agent_stomach = this_agent%get_stom_content()
    perception_override_stomach =                                             &
                max( ZERO,                                                    &
                     agent_stomach - stomach_emptify_backend(agent_stomach) + &
                        expected_food_item_gain_fits )

    !> **Body mass**: the **body mass** perception override @f$ \pi_m @f$ is
    !!  obtained by incrementing (or decrementing if the expected food gain
    !! is negative) the current body mass @f$ M @f$ by the expected food gain
    !! @f$ \phi @f$ and also subtracting the cost of living @f$ M_c @f$
    !! (the_body::condition::living_cost()) and the expected cost of movement
    !! into the target novel habitat @f$ \mu @f$:
    !! @f[ \pi_m = M + \phi - M_c - \mu @f]
    !! Thus, probability of capture and costs of food processing in
    !! calculating the stomach content increment depend on the distance to
    !! the expected food item and do not take into account the travel cost
    !! to the novel environment (it can be quite large, beyond the visibility
    !! of the expected food item). However, expectancy of the body mass
    !! (the fake perception value) takes into account the cost of migration
    !! movement to the novel target habitat.
    perception_override_bodymass =                                            &
                max( ZERO,                                                    &
                     this_agent%get_mass() -                                  &
                        this_agent%living_cost() +                            &
                        expected_food_item_gain_fits -                        &
                        this%expected_cost_moving )

    !> **Energy**: The fake perception values for the energy reserves
    !! (`energy_override_perc`) using the the_body::energy_reserve()
    !! procedure.
    perception_override_energy = energy_reserve( perception_override_bodymass,&
                                                 this_agent%length() )


    !> **Direct food perception**: The fake perception of the number of food
    !! items expected for the perception in the target novel environment
    !! is calculated from the this\%expected_food_dir component (obtained in
    !! the `do_this` procedure).
    perception_override_food_dir = this%expected_food_dir

    !> **Predation risk**: fake perceptions of predation risk are
    !! obtained from the values calculated in the `do` procedure:
    !! the_behaviour::migrate::expected_pred_dir_risk and
    !! the_behaviour::migrate::expected_predation_risk.
    perception_override_pred_dir = this%expected_pred_dir_risk
    perception_override_predator = this%expected_predation_risk

    !> **Number of conspecifics**: finally, the fake perception of the
    !! number of conspecifics is calculated from the values calculated in the
    !! `do` procedure: the_behaviour::migrate::expected_consp_number.
    perception_override_conspec = this%expected_consp_number

    !> #### Calculate motivation expectancies ####
    !> The next step is to calculate the motivational expectancies using the
    !! fake perceptions to override the default (actual agent's) values.
    !> At this stage, first, calculate motivation values resulting from the
    !! behaviour done (`migrate::do_this()` ) at the previous steps: what
    !! would be the motivation values *if* the agent does perform
    !! MIGRATE? Technically, this is done by calling the **neuronal
    !! response function**, `percept_components_motiv::motivation_components()`
    !! method, for each of the motivational states with `perception_override_`
    !! dummy parameters overriding the default values.
    !! Here is the list of the fake overriding perceptions for the
    !! MIGRATE behaviour:
    !!  - `perception_override_food_dir`
    !!  - `perception_override_conspec`
    !!  - `perception_override_pred_dir`
    !!  - `perception_override_predator`
    !!  - `perception_override_stomach`
    !!  - `perception_override_bodymass`
    !!  - `perception_override_energy`
    !!  .
    !  @note  **Expectancy** assessment for **hunger** motivation, using
    !         `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in] for
    !         `this_agent` now.
    call this%expectancy%hunger%percept_component%motivation_components       &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL,         &
      param_gp_matrix_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL,     &
      param_gp_matrix_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL,     &
      param_gp_matrix_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL,               &
      param_gp_matrix_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL,             &
      param_gp_matrix_age      = AGE_HUNGER_GENOTYPE_NEURONAL,                &
      param_gp_matrix_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL,            &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,     &
      param_gerror_cv_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_age      = AGE_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,      &
      param_gerror_cv_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,  &
      !  Real agent perception components are now substituted by the *fake*
      !  values resulting from executing this behaviour (`do_this` method).
      !  This is repeated for all the motivations: *hunger*,
      !  *passive avoidance,* *fear state* etc.
      perception_override_food_dir = perception_override_food_dir,            &
      perception_override_conspec = perception_override_conspec,              &
      perception_override_pred_dir = perception_override_pred_dir,            &
      perception_override_predator = perception_override_predator,            &
      perception_override_stomach = perception_override_stomach,              &
      perception_override_bodymass = perception_override_bodymass,            &
      perception_override_energy = perception_override_energy                 &
                                                                              )
      !> Real agent perception components are now substituted by the *fake*
      !! values resulting from executing this behaviour (`reproduce::do_this()`
      !! => `the_behaviour::reproduce_do_this()` method). This is repeated for
      !! all the motivations: *hunger*, *passive avoidance,* *active
      !! avoidance* etc. These optional **override parameters** are
      !! substituted by the "fake" values.

    !  @note  **Expectancy** assessment for **fear_defence** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%fear_defence%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL, &
      param_gp_matrix_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_food_dir = perception_override_food_dir,            &
      perception_override_conspec = perception_override_conspec,              &
      perception_override_pred_dir = perception_override_pred_dir,            &
      perception_override_predator = perception_override_predator,            &
      perception_override_stomach = perception_override_stomach,              &
      perception_override_bodymass = perception_override_bodymass,            &
      perception_override_energy = perception_override_energy                 &
                                                                              )

    !  @note  **Expectancy** assessment for **reproduction** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%reproduction%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL,    &
      param_gp_matrix_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_food_dir = perception_override_food_dir,            &
      perception_override_conspec = perception_override_conspec,              &
      perception_override_pred_dir = perception_override_pred_dir,            &
      perception_override_predator = perception_override_predator,            &
      perception_override_stomach = perception_override_stomach,              &
      perception_override_bodymass = perception_override_bodymass,            &
      perception_override_energy = perception_override_energy                 &
                                                                              )

    !> #### Calculate primary and final motivations ####
    !> Next, from the perceptual components calculated at the previous
    !! step we can obtain the **primary** and **final motivation** values by
    !! weighed summing.
    if (present(rescale_max_motivation)) then
      !> Here we can use global maximum motivation across all behaviours and
      !! perceptual components if it is provided, for rescaling.
      max_motivation = rescale_max_motivation
    else
      !> Or can rescale using local maximum value for this behaviour only.
      max_motivation =  this%expectancy%max_perception()
    end if

    !> Transfer attention weights from the actor agent `this_agent` to the
    !! `this` behaviour component. So, we will now use the updated modulated
    !! attention weights of the agent rather than their default parameter
    !! values.
    call this%attention_transfer(this_agent)

    !> So the primary motivation values are calculated.
    call this%expectancy%motivation_primary_calc(max_motivation)

    !> Primary motivations are logged in the @ref intro_debug_mode "debug mode".
    call LOG_DBG( LTAG_INFO // "Primary motivations: " //                     &
                  "hunger: " //                                               &
                    TOSTR(this%expectancy%hunger%motivation_prim)  //         &
                  ", fear_defence: " //                                       &
                    TOSTR(this%expectancy%fear_defence%motivation_prim) //    &
                  ", reproduce: " //                                          &
                    TOSTR(this%expectancy%reproduction%motivation_prim),      &
                  PROCNAME, MODNAME )

    !> There is **no modulation** at this stage, so the final motivation
    !! values are the same as primary motivations.
    call this%expectancy%modulation_none()

    !> #### Calculate motivation expectancies ####
    !> Finally, calculate the finally **expected arousal level for this
    !! behaviour**. As in the GOS, the overall arousal is the maximum value
    !! among all motivation components.
    this%arousal_expected = this%expectancy%max_final()

    !> Log also the final expectancy value in the @ref intro_debug_mode
    !! "debug mode".
    call LOG_DBG( LTAG_INFO // "Expectancy: " // TOSTR(this%arousal_expected),&
                  PROCNAME, MODNAME )

    !> Now as we know the expected arousal, we can choose the behaviour which
    !! would minimise this arousal level.

  end subroutine migrate_motivations_expect

  !-----------------------------------------------------------------------------
  !> Execute this behaviour component "migrate" by `this_agent` agent.
  subroutine migrate_do_execute(this, this_agent, target_env)
    class(MIGRATE), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which goes down.
    class(APPRAISAL), intent(inout)    :: this_agent
    !> @param[in] target_env the target environment the actor agent is going
    !!            to (e)migrate into.
    class(ENVIRONMENT), intent(in) :: target_env

    ! PROCNAME is the procedure name for logging and debugging.
    character(len=*), parameter :: PROCNAME = "(migrate_do_execute)"

    ! Local copy of the body length of the agent
    real(SRP) :: agent_length

    ! Debugging indicators for correlated random walk.
    logical :: is_converged_debug
    integer :: iter_debug

    ! 95% confidence interval of Gaussian distribution, sets the upper
    ! limit on maximum migration distance.
    real(SRP), parameter :: CIDIF = 1.95996_SRP

    ! Agent length is local variable to avoid multiple calls to get_length().
    agent_length = this_agent%get_length()

    !> ### Implementation details ###
    !> #### Step 1: do_this ####
    !> First, we use the intent-in **do**-procedure
    !! the_behaviour::migrate::do_this() to perform the behaviour desired.
    !! However, instead of expectations, get the target point in the novel
    !! habitat.(Expectancies for food gain, predator risk etc. are not used
    !! at this stage, memory windows are absent from the parameter list.)
    call this%do_this( this_agent=this_agent, target_env=target_env )

    !> #### Step 2: Change the agent ####
    !> ##### Try to relocate to the target novel habitat #####
    !> The agent does a directional walk at this\%distance towards the
    !! this\%target_point in the novel target environment. However, it is
    !! possible only if the walk distance does not exceed the maximum value
    !! defined by the commondata::migrate_dist_max_step body sizes of the
    !! agent.
    DO_WALK: if (this%distance > agent_length * MIGRATE_DIST_MAX_STEP ) then
      !> - If this is the case, a warning is logged.
      call LOG_DBG( LTAG_WARN // "Migration travel distance exceeds big " //  &
                    "threshold in " // PROCNAME // " for the agent " //       &
                    this_agent%individ_label() // ". Agent length: " //       &
                    TOSTR(agent_length) // ", migration distance: " //        &
                    TOSTR(this%distance), PROCNAME, MODNAME   )
      !> - the agent is executing a Gaussian correlated random walk towards the
      !!   target point. The average walk length is the above maximum distance
      !!   minus 95% confidence limit and the CV is the default for random
      !!   walks (thus, there is almost a guarantee that the actual walk is
      !!   the maximum commondata::migrate_dist_max_step distance and unlikely
      !!   to exceed it. This walk is, additionally, limited to the present
      !!   environment (i.e. no migration is performed by the agent).
      !! .
      call this_agent%corwalk(                                                &
                        target = this%target_point,                           &
                        meanshift_xy = agent_length * MIGRATE_DIST_MAX_STEP - &
                              agent_length * MIGRATE_DIST_MAX_STEP * CIDIF *  &
                              (agent_length * MIGRATE_DIST_MAX_STEP) *        &
                              WALK_RANDOM_DISTANCE_STOCHASTIC_CV,             &
                        cv_shift_xy = WALK_RANDOM_DISTANCE_STOCHASTIC_CV,     &
                        meanshift_depth = agent_length *                      &
                                      WALK_RANDOM_VERTICAL_SHIFT_RATIO,       &
                        cv_shift_depth = WALK_RANDOM_DISTANCE_STOCHASTIC_CV * &
                              WALK_RANDOM_VERTICAL_SHIFT_CV_RATIO,            &
                        is_away = .FALSE.,                                    &
                        environment_limits = Global_Habitats_Available(       &
                                               this_agent%find_environment(   &
                                                 Global_Habitats_Available) ),&
                        is_converged = is_converged_debug,                    &
                        debug_reps = iter_debug  )
      call LOG_DBG( LTAG_INFO // "Correlated random walk: converged " //      &
                    TOSTR(is_converged_debug) // ", iterations: " //          &
                    TOSTR(iter_debug), PROCNAME, MODNAME )
    !> If the above limit on the length of a single walk is not
    !! exceeded, the agent relocates to the target point in the novel
    !! target environment. It is now in the target environment.
    else DO_WALK
      call LOG_DBG( LTAG_INFO // "Agent is about to migrate to " //           &
                    TOSTR([ this%target_point%xpos(),                         &
                            this%target_point%ypos(),                         &
                            this%target_point%dpos()]), PROCNAME, MODNAME )
      call this_agent%position( this%target_point%location() )
    end if DO_WALK

    !> In the @ref intro_debug_mode "DEBUG Mode", print diagnostic information
    !! to the logger.
    call LOG_DBG( LTAG_INFO // "Migration displacement position:" //          &
                  TOSTR([ this_agent%xpos(),                                  &
                          this_agent%ypos(),                                  &
                          this_agent%dpos()] ) // ", distance (way) " //      &
                  "traversed: " // TOSTR(this_agent%way()) //                 &
                  " (distance expected " // TOSTR(this%distance) // ")",      &
                  PROCNAME, MODNAME )
    call LOG_DBG( LTAG_INFO // "Cost of this movement: " //                   &
                  TOSTR(this_agent%cost_swim(exponent =                       &
                        SWIMMING_COST_EXPONENT_LAMINAR)), PROCNAME, MODNAME )
    call LOG_DBG( LTAG_INFO // "The agent is now in [" //                     &
                  TOSTR(this_agent%find_environment()) // "] environment: "// &
                  trim(Global_Habitats_Available(                             &
                          this_agent%find_environment())%get_label()),        &
                  PROCNAME, MODNAME )

    !> ##### Process the cost of movement #####
    !> - Reset the body mass of the actor agent subtracting the actual cost of
    !!   the migration moving that is automatically calculated in the call to
    !!   the_body::condition::cost_swim(). The the_body::condition::set_mass()
    !!   method is used here to adjust the mass.
    call this_agent%set_mass(                                                 &
                      value_set = this_agent%get_mass() -                     &
                        this_agent%cost_swim(exponent=                        &
                                        SWIMMING_COST_EXPONENT_LAMINAR),      &
                      update_history = .TRUE. )

    !> - Additionally, also call the `the_body::condition::set_length()` method
    !!   to update the body length history stack. However, the value_set
    !!   parameter here is just the current value. This fake re-setting of the
    !!   body length is done to keep both mass and length synchronised in their
    !!   history stack arrays (there is no procedure for only updating history).
    call this_agent%set_length( value_set = this_agent%get_length(),          &
                                update_history = .TRUE. )

    !> - After resetting the body mass, update energy reserves of the agent,
    !!   that depend on both the length and the mass.
    !! .
    call this_agent%energy_update()

    call LOG_DBG( LTAG_INFO // "Updated mass: "                            // &
                                          TOSTR(this_agent%get_mass())     // &
                  ", body length: " // TOSTR(this_agent%get_length())      // &
                  ", energy: " // TOSTR(this_agent%get_energy()),             &
                  PROCNAME, MODNAME )

    !> Finally, check if the agent is starved to death. If yes, the agent can
    !! die without going any further.
    if (this_agent%starved_death()) call this_agent%dies()

    !> #### Step 3: Change the environment ####
    !> Random walk does not affect the environmental objects.

  end subroutine migrate_do_execute

  !-----------------------------------------------------------------------------
  !> The hope function for the assessment of expectancy for a completely
  !! novel stimulus or environment for which local information is absent.
  !!
  !> Calculation of the expectancy and therefore fake perceptions is not
  !! possible for completely novel environment or stimuli (e.g. for
  !! emigrating into a completely different novel habitat) based on the
  !! current perception because the agent has absolutely no local information
  !! (i.e. no perception of this habitat yet).
  !!
  !! A mechanism based on the **hope function** should be used in such a case.
  !! @image html img_doxygen_hope_function_rule.svg  "The hope function mechanism"
  !! @image latex img_doxygen_hope_function_rule.eps "The hope function mechanism" width=14cm
  !!
  !! - A baseline expectancy @f$ f_0 @f$ based on the locally available
  !!   information (e.g. local expectation of the food gain) is selected.
  !! - Then, a trend of the baseline expectancy characteristic (e.g. average
  !!   food gain) in the past memory stack is determined by
  !!   - splitting a food memory stack *window* into two halves: older
  !!     @f$ \mathbf{M_1} @f$ and newer @f$ \mathbf{M_2} @f$,
  !!   - calculating the average local expectancies for the older
  !!     @f$ \overline{f_1} @f$ and newer @f$ \overline{f_2} @f$ parts,
  !!   - calculating the ratio
  !!     @f[ \varrho = \frac{\overline{f_2}}{\overline{f_1}} . @f]
  !!   .
  !! .
  !! Following this, the expectancy (e.g. expected food gain) for the
  !! novel stimuli or situation is calculated as:
  !! @f[ F_{exp}= f_0 \cdot \Xi(\varrho) , @f] where @f$ f_0 @f$ is the
  !! baseline food gain against which the expectancy is evaluated, and
  !! @f$ \Xi(\varrho) @f$ is the "hope" function that is obtained as a
  !! nonparametric relationship (see the right panel plots above):
  !! nonlinear interpolation based on the grid vectors
  !! @f$ \mathbf{V} @f$ and @f$ \mathbf{W} @f$:
  !  Note: Simplified Latex formula:
  !    @f[ \mathbf{V} = \left( 0.0; 1.0; \varrho_0 \right ),
  !       \mathbf{W} = \left( \Xi_{max}; 1.0; \to 0.0 \right) , @f]
  !    The vectors are now as graphics in svg and eps formats because Latex
  !    matrices  may not render correctly by Doxygen.
  !> @n
  !> @image html img_doxygen_hope_function_formula_1.svg
  !! @image latex img_doxygen_hope_function_formula_1.eps "" width=14cm
  !! @n
  !! where @f$ \varrho_0 @f$ is the *zero hope ratio* parameter and
  !! @f$ \Xi_{max} @f$ is the *maximum hope* parameter.
  pure function hope(  baseline, memory_old, memory_new,                      &
                       zero_hope, maximum_hope, raw_grid_x, raw_grid_y)       &
                                                        result (expected_value)
    !> @param[in] baseline is the baseline stimulus expectancy @f$ f_0 @f$
    !!            that is based on the locally available information.
    real(SRP), intent(in) :: baseline
    !> @param[in]  memory_old is the older part (half) of the memory stack
    !!             @f$ \overline{f_1} @f$ for the baseline perception.
    real(SRP), intent(in) :: memory_old
    !> @param[in] memory_new is the newer part (half) of the memory stack
    !!             @f$ \overline{f_2} @f$ for the baseline perception.
    real(SRP), intent(in) :: memory_new
    !> @param[in] zero_hope is the zero hope ratio @f$ \varrho_0 @f$  parameter
    !!            of the hope function grid abscissa vector.
    real(SRP), optional, intent(in) :: zero_hope
    !> @param[in] maximum_hope is the maximum hope @f$ \Xi_{max} @f$ parameter
    !!            of the hope function grid ordinate vector.
    real(SRP), optional, intent(in) :: maximum_hope
    !> @param[in] raw_grid_x a raw interpolation grid array that can be
    !!            provided (along with raw_grid_y) instead of the normal
    !!            `zero_hope` and `maximum_hope` parameters.
    real(SRP), dimension(:), optional, intent(in) :: raw_grid_x
    !> @param[in] raw_grid_y a raw interpolation grid array that can be
    !!            provided (along with raw_grid_x) instead of the normal
    !!            `zero_hope` and `maximum_hope` parameters.
    real(SRP), dimension(:), optional, intent(in) :: raw_grid_y

    !> @return The expected value for the wholly novel stimulus or environment.
    !> @note Note that the scalar parameters `zero_hope`
    !!       and `maximum_hope` represent the normal standard
    !!       way to provide the interpolation grid for the hope
    !!       function. However, these grids can also be accepted
    !!       as raw grid arrays (see `raw_grid_x` and `raw_grid_y`
    !!       parameters below).
    !> @note Raw grid arrays have priority if both raw grid arrays
    !!       and  normal scalar parameters `zero_hope` and
    !!       `maximum_hope` are simultaneously provided.
    !> @warning The grid vectors `raw_grid_x` and `raw_grid_y`
    !!       must have the same length.
    real(SRP) :: expected_value

    !> ### Notable variables ###
    !> - **memory_ratio** is the ratio of the newer to older memory values;
    real(SRP) :: memory_ratio

    !> - **hope_func_grid_abscissa** and **hope_func_grid_ordinate** are the
    !!   hope function grid arrays. They define the nonparametric hope function
    !!   that is obtained by nonlinear interpolation. These arrays can be also
    !!   provided as raw `raw_grid_x` `raw_grid_y` parameters.
    !! .
    integer, parameter :: HOPE_FUNC_GRID_DIM = 3
    real(SRP), dimension(HOPE_FUNC_GRID_DIM) :: hope_func_grid_abscissa,      &
                                                hope_func_grid_ordinate

    !> ##### Implementation details #####
    !> First, calculate the memory-based ratio
    !! @f[ \varrho = \frac{\overline{f_2}}{\overline{f_1}} . @f]
    !> - The calculation also checks for possible division by zero, if
    !!   the older memory value @f$ \overline{f_2} = 0.0 @f$; in such
    !!   a case, the ratio is set to the maximum abscissa grid value
    !!   resulting in zero hope function.
    if (memory_old < ZERO) then
      if (present(raw_grid_x) .and. present(raw_grid_y)) then
        memory_ratio = maxval(raw_grid_x)
      else if (present(zero_hope) .and. present(maximum_hope)) then
        memory_ratio = zero_hope
      else
        expected_value = MISSING
        return
      end if
    !> - An additional case of both @f$ \overline{f_1} = 0.0 @f$ and
    !!   @f$ \overline{f_2} = 0.0 @f$ is also checked, the ratio in such
    !!   a case is set to 1.0, bringing about a unity hope function value
    !!   (i.e. baseline expectancy is unchanged).
    !! .
    elseif (memory_old < ZERO .and. memory_new < ZERO) then
      memory_ratio = 1.0_SRP
    else
      memory_ratio = memory_new / memory_old
    end if

    PROVIDE_RAW_GRID: if (present(raw_grid_x) .and. present(raw_grid_y)) then
      expected_value = baseline * DDPINTERPOL( raw_grid_x,                    &
                                               raw_grid_y,                    &
                                               memory_ratio  )
      ! Grid arrays have priority if both arrays and normal scalar
      ! parameters are simultaneously provided, so exit.
      return
    end if PROVIDE_RAW_GRID

    !> Second, get the hope function grid vectors @f$ \mathbf{V} @f$ and
    !! @f$ \mathbf{W} @f$ as:
    !! @verbatim
    !!   V = [ 0.0_SRP,      1.00_SRP,  zero_hope ]
    !!   W = [ maximum_hope, 1.00_SRP,       ZERO ]
    !! @endverbatim
    PROVIDE_SCALARS: if (present(zero_hope) .and. present(maximum_hope)) then
      hope_func_grid_abscissa = [ 0.0_SRP,      1.00_SRP, zero_hope ]
      hope_func_grid_ordinate = [ maximum_hope, 1.00_SRP,      ZERO ]

      !> Finally, the hope function value is obtained from a nonlinear
      !! interpolation based on `DDPINTERPOL` (see HEDTOOLS) with the
      !! interpolation grid defined by the @f$ \mathbf{V} @f$ (abscissa)
      !! and @f$ \mathbf{W} @f$ (ordinate) vectors.
      ! htintrpl.exe [0 1 2] [4 1 0]
      expected_value = baseline * DDPINTERPOL( hope_func_grid_abscissa,       &
                                               hope_func_grid_ordinate,       &
                                               memory_ratio  )
      ! Exit, everything below this point is error, wrong parameter pair.
      return
    end if PROVIDE_SCALARS

    !> If neither a pair of the scalar parameters `zero_hope` and
    !! `maximum_hope` nor the raw grid arrays `raw_grid_x` and `raw_grid_y`
    !! are provided, return commondata::missing value as an indicator of
    !! error.
    expected_value = MISSING

  end function hope

  !-----------------------------------------------------------------------------
  !> Calculate the default upward and downward walk step size. This function is
  !! called from the_behaviour::go_down_do_this() and
  !! the_behaviour::go_down_motivations_expect() if the upwards or downwards
  !! walk size is not provided explicitly.
  elemental function depth_walk_default (length, walk_factor)                 &
                                                        result (depth_walk_out)
    !> @param[in] length The body length of the agent.
    real(SRP), intent(in) :: length
    !> @param[in] walk_factor The multiplocation factor for the walk step.
    !!            The fdefault value is defined by the parameter
    !!            commondata::up_down_walk_step_stdlength_factor.
    real(SRP), intent(in), optional :: walk_factor
    !> @return The default up/down walk step size.
    real(SRP) :: depth_walk_out

    !> ### Details ###
    !> If the walk size is not provided, it is set equal to the agent's body
    !! length multiplied by the commondata::up_down_walk_step_stdlength_factor
    !! factor parameter.
    if (present(walk_factor)) then
        depth_walk_out = length * walk_factor
    else
        depth_walk_out = length * UP_DOWN_WALK_STEP_STDLENGTH_FACTOR
    end if

  end function depth_walk_default

  !-----------------------------------------------------------------------------
  !> Initialise the **go down to a deeper spatial layer** behaviour component
  !! to a zero state.
  elemental subroutine go_down_depth_init_zero(this)
    class(GO_DOWN_DEPTH), intent(inout) :: this

    !> First init components from the base root class
    !! `the_behaviour::behaviour_base`.
    !> Mandatory label component that should be read-only.
    this%label = "GO_DOWN_DEPTH"
    !> The execution status is always FALSE, can be reset to TRUE only when
    !! the behaviour unit is called to execution.
    this%is_active = .FALSE.

    !> And the *expectancy* components.
    call this%expectancy%init()
    this%arousal_expected = 0.0_SRP

    !> Abstract `MOVE` component.
    this%distance = MISSING

    !> Then init components of this specific behaviour component extended class.
    this%decrement_mass_cost = MISSING
    this%expected_food_gain = MISSING
    this%expected_consp_number = UNKNOWN
    this%expected_predation_risk = MISSING

  end subroutine go_down_depth_init_zero

  !-----------------------------------------------------------------------------
  !> Do go down by `this_agent` (the actor agent). Subjective assessment of the
  !! motivational value for this is based on the number of food items,
  !! conspecifics and predators at the layers below the `this_agent` actor
  !! agent.
  subroutine go_down_do_this(this, this_agent, max_depth, depth_walk,         &
                                          predict_window_food, time_step_model)
    !> @param[inout] this the object itself.
    class(GO_DOWN_DEPTH), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which goes down.
    class(APPRAISAL), intent(in)    :: this_agent
    !> @param[in] max_depth is the maximum limit on the depth.
    real(SRP), intent(in) :: max_depth
    !> @param[in] depth_walk Optional downward walk size, by how deep
    !!            the agent goes down.
    real(SRP), intent(in), optional :: depth_walk
    !> @param[in] predict_window_food the size of the prediction window, i.e.
    !!            how many steps back in memory are used to calculate the
    !!            predicted food gain. This parameter is limited by the maximum
    !!            commondata::history_size_perception value of the perception
    !!            memory history size.
    integer, optional, intent(in) :: predict_window_food
    !> @param[in] time_step_model optional time step of the model, overrides
    !!            the value calculated from the spatial data.
    integer, optional, intent(in)   :: time_step_model

    ! Local copies of optionals
    real(SRP) :: depth_walk_here

    ! Local copies of optionals.
    integer :: predict_window_food_here, time_step_model_here

    ! **WEIGHT_DIRECT**  is the relative weight  given to the immediate
    ! perception of predators over the predators counts in the memory stack.
    ! Obtained from global parameters
    ! (`commondata::predation_risk_weight_immediate`).
    real(SRP), parameter :: WEIGHT_DIRECT = PREDATION_RISK_WEIGHT_IMMEDIATE

    ! **MEM_WIND** is the size of the memory window when assessing the
    ! predator risk, only this number of the latest elements from the memory
    ! stack is taken into account. So we further weight the direct threat
    ! over the background risk when making the decision.
    ! @note  Note that we take into account the whole memory size
    !        (commondata::history_size_perception).
    integer, parameter :: MEM_WIND = HISTORY_SIZE_PERCEPTION

    !> ### Implementation details ###
    !> First, check if the size of the downward walk `depth_walk` dummy
    !! parameter is provided.
    if (present(depth_walk)) then
      depth_walk_here = depth_walk
    else
      !> If it is not provided, it is set equal to the agent's body length
      !! multiplied by the commondata::up_down_walk_step_stdlength_factor
      !! factor parameter. Calculated by `the_behaviour::depth_walk_default()`.
      depth_walk_here = depth_walk_default ( this_agent%get_length() )
    end if

    !> Check optional parameter for the food perception memory window. If
    !! the `predict_window_food` dummy parameter is not provided, its default
    !! value is the proportion of the whole perceptual memory window defined
    !! by commondata::history_perception_window_food. Thus, only the
    !! latest part of the memory is used for the prediction of the future
    !! food gain.
    if (present(predict_window_food)) then
      predict_window_food_here = predict_window_food
    else
      predict_window_food_here = floor( HISTORY_SIZE_PERCEPTION *             &
                                        HISTORY_PERCEPTION_WINDOW_FOOD )
    end if

    !> Check optional time step parameter. If unset, use global
    !! `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> #### Downward step size ####
    !> Here, first, check if the target depth is likely to go beyond the
    !! environment depth limits and reduce the downward walk step size
    !! accordingly. Namely, if the depth coordinate of the actor agent
    !! plus the depth step exceeds the maximum depth, the step is reduced
    !! to be within the available environment:
    !! @f$ D_{max} - d_{a} - \varepsilon @f$, where @f$ D_{max} @f$ is the
    !! maximum depth, @f$ d_{a} @f$ is the agent's current depth and
    !! @f$ \varepsilon @f$ is a very small constant defined by the parameter
    !! commondata::zero.
    if (this_agent%dpos() + depth_walk_here >= max_depth )                    &
                depth_walk_here = max( 0.0_SRP,                               &
                                       max_depth - this_agent%dpos() - ZERO )

    !> The down step size component of the class is then equal to the
    !! `depth_walk`.
    this%distance = depth_walk_here

    !> #### The cost of swimming down ####
    !> The expected cost of the swimming down by the buoyancy is much smaller
    !! than active propulsion. It is set as a fraction, defined by the
    !! parameter commondata::swimming_cost_factor_buoyancy_down, of active
    !! laminar propulsion calculated by function
    !! the_body::condition_cost_swimming_burst().
    this%decrement_mass_cost = SWIMMING_COST_FACTOR_BUOYANCY_DOWN *           &
              this_agent%cost_swim( distance=depth_walk_here,                 &
                                    exponent=SWIMMING_COST_EXPONENT_LAMINAR )

    !> #### Calculate expected perceptions ####
    !> Calculate the number of conspecifics at the down of the agent using the
    !! function perception::consp_below().
    this%expected_consp_number = this_agent%consp_below()

    !> Calculate the expected predation risk at the down of the agent using
    !! the `the_neurobio::predation_risk_backend()` function. This is a general
    !! predation risk (the_neurobio::percept_components_motiv::predator), not
    !! direct risk based on the distance to the nearest predator (see
    !! the_neurobio::percept_components_motiv::pred_dir).
    this%expected_predation_risk =                                            &
        predation_risk_backend(                                               &
                              this_agent%pred_below(),                        &
                              this_agent%memory_stack%get_pred_mean(MEM_WIND),&
                              WEIGHT_DIRECT )

    !> Calculate the expected food gain as an average mass of the food items
    !! down the agent. It is used by calling perception::food_mass_below()
    !! function.
    !! This expected food gain is then weighted by the subjective probability
    !! of food item capture that is calculated based on the memory
    !! the_neurobio::perception::food_probability_capture_subjective().
    this%expected_food_gain = this_agent%food_mass_below() *                  &
                              this_agent%food_probability_capture_subjective( &
                              predict_window_food_here, time_step_model_here )

  end subroutine go_down_do_this

  !-----------------------------------------------------------------------------
  !> `go_down_depth::motivations_expect()` is a subroutine (re)calculating
  !! motivations from fake expected perceptions following from the procedure
  !! `go_down_depth::do_this()` => `the_behaviour::go_down_do_this()`.
  subroutine go_down_motivations_expect(this, this_agent, depth_walk,         &
                                    max_depth, environments,                  &
                                    time_step_model, rescale_max_motivation )
    !> @param[inout] this the object itself.
    class(GO_DOWN_DEPTH), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which goes down.
    class(APPRAISAL), intent(in)    :: this_agent
    !> @param[in] depth_walk The downward walk size, by how deep the agent
    !!            goes down.
    real(SRP), intent(in), optional :: depth_walk
    !> @param[in] max_depth is the optional maximum limit on the depth.
    real(SRP), intent(in), optional :: max_depth
    !> @param[in] environments optional array of the all available
    !!            environments where the this agent can be in, needed for the
    !!            calculation of the depth limits. If such an array of the
    !!            environments is provided, `max_depth` has precedence.
    class(ENVIRONMENT), dimension(:), optional, intent(in) :: environments
    !> @param [in] time_step_model optional time step of the model,
    !!             **overrides** the value calculated from the spatial data.
    integer, optional, intent(in)   :: time_step_model
    !> @param[in] rescale_max_motivation maximum motivation value for
    !!            rescaling all motivational components for comparison
    !!            across all motivation and perceptual components and behaviour
    !!            units.
    real(SRP), optional, intent(in) :: rescale_max_motivation

    ! Local copy of optional depth_walk
    real(SRP) :: depth_walk_here, max_depth_here

    ! Local copy of optional model time step
    integer :: time_step_model_here

    !> Target depth, i.e. the absolute depth of the agent after it moves down.
    real(SRP) :: target_depth

    ! Expected food item that is used in the calculations, its properties are
    ! based on the average food items that the agent perceives below.
    type(FOOD_ITEM) :: expected_food_item

    ! the coordinates of the expected food item.
    type(SPATIAL) :: expected_food_item_xyz

    ! The expected distance to the food item at the target downward horizon.
    real(SRP) :: expect_distance_food

    ! Expected mass increment from food at the target depth.
    real(SRP) :: expect_mass_increment_from_food

    ! Expected stomach increment from food at the target depth.
    real(SRP) :: expect_stomach_increment_from_food

    ! The number of food items under the agent, obtained from the current
    ! perception object.
    integer :: n_food_items_below

    ! Local variable
    real(SRP) :: max_motivation ! Local max. over all motivation components.

    !> ### Notable local variables ###
    !> #### Perception overrides ####
    !> - **expect_food_perc_override** is the fake perception for the food items
    !!   at the target depth.
    integer :: expect_food_perc_override

    !> - **expect_depth_perc_override** is the fake perception of the depth,
    !!   identical to the target depth.
    real(SRP) :: expect_depth_perc_override

    !> - **expect_light_perc_override** is the fake perception of the
    !!   illumination level at the target depth.
    real(SRP) :: expect_light_perc_override

    !> - **expect_mass_perc_override** is the fake perception value for the
    !!   mass from the expected food.
    real(SRP) :: expect_mass_perc_override

    !> - **expect_stomach_perc_override** is the fake perception value for the
    !!   stomach increment from the expected food.
    real(SRP) :: expect_stomach_perc_override

    ! Current stomach contents mass of the actor agent.
    real(SRP) :: agent_stomach

    !> - **expect_energy_perc_override** is the fake perception for the energy
    !!   reserves from the expected food at the target depth.
    real(SRP) :: expect_energy_perc_override

    !> - **expected_probability_capture** is the expected probability of capture
    !!   of the expected food item at the target depth.
    real(SRP) :: expected_probability_capture

    !> - **expect_conspecicifc_perc_override** is the fake perception value for
    !!   the number of conspecifics at the target depth.
    integer :: expect_conspecicifc_perc_override

    !> - **expect_predator_perc_override** is fake perception value for the
    !!   predation risk at the target depth.
    !! .
    real(SRP) :: expect_predator_perc_override

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME =                                 &
                                          "(go_down_motivations_expect)"

    !> ### Implementation details ###
    !> #### Sanity checks and preparations ####
    !> Initially, check if the size of the downward walk `depth_walk` dummy
    !! parameter is provided.
    if (present(depth_walk)) then
      depth_walk_here = depth_walk
    else
      !> If it is not provided, it is set equal to the agent's body length
      !! multiplied by the commondata::up_down_walk_step_stdlength_factor
      !! factor parameter. Calculated by `the_behaviour::depth_walk_default()`.
      depth_walk_here = depth_walk_default ( this_agent%get_length() )

    end if

    ! @note The `GET_MAXDEPTH` block is used unchanged in several places,
    !       however, it cannot be isolated into a single procedure because
    !       its code heavily uses optional parameters checks using `present`
    !       intrinsic function that should apply to the called procedure.
    GET_MAXDEPTH: block
      !> Check downward step size. Here, first, check if the target depth is
      !! likely to go beyond the environment depth limits and reduce the
      !! downward walk step size accordingly. Either the explicitly provided
      !! maximum depth dummy parameter `max_depth` or an array of possible
      !! environment objects where the `this_agent` actor agent can be located
      !! is used to get the depth limit.
      max_depth_here = MISSING

      if (present(environments)) then
        !> If the array of possible environment objects that can contain the
        !! actor agent is provided, the check involves the
        !! `the_environment::spatial::find_environment()` function to find the
        !! specific environment object the agent is currently in followed by
        !! `the_environment::environment::depth_max()` to find the minimum
        !! depth in this environment object.
        max_depth_here =                                                     &
            environments(this_agent%find_environment(environments))%depth_max()
      else
        !> If the array of possible environment objects that can contain the
        !! actor agent is not provided, the current environment is obtained
        !! from the global array the_environment::global_habitats_available.
        !! In this case, the environment that actor agent is within is
        !! determined  using the the_environment::spatial::find_environment()
        !! method, which is in followed by
        !! the_environment::environment::depth_max()` to find the minimum
        !! depth in this environment object.
        max_depth_here = Global_Habitats_Available(                           &
                            this_agent%find_environment(                      &
                                              Global_Habitats_Available)      &
                            )%depth_max()
      end if

      !> If `max_depth` is provided, it has precedence over the depth
      !! detected explicitly or implicitly from the environment objects.
      if (present(max_depth)) max_depth_here = max_depth

      !> In the case the maximum depth cannot be determined,it is set as
      !! the depth of the actor agent (with an additional condition that it
      !! should exceed zero), so movement down would be **impossible**.
      if (max_depth_here .feq. MISSING)                                       &
                          max_depth_here = max( 0.0_SRP, this_agent%dpos() )
    end block GET_MAXDEPTH

    !> If the depth coordinate of the actor agent plus the depth step
    !! exceeds the maximum depth, the step is reduced to be strictly within
    !! the available environment. However, it should also never be below zero.
    if (this_agent%dpos() + depth_walk_here >= max_depth_here )               &
              depth_walk_here = max( 0.0_SRP,                                 &
                                     max_depth_here - this_agent%dpos() - ZERO )

    !> Check optional time step parameter. If not provided, use global
    !! parameter value from commondata::global_time_step_model_current.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> Assess the number of food items below using the
    !! perception::food_items_below() method.
    n_food_items_below = this_agent%food_items_below()

    !> Calculate the expected distance to the food item. It is equal to the
    !! average distance to the food items perceived below in case there are
    !! any such items perceived below. Calculated using the
    !! perception::food_dist_below() method.
    if ( n_food_items_below > 0 ) then
      expect_distance_food = this_agent%food_dist_below()
    else
      !> However, if there are no food items below (resulting a
      !! commondata::missing distance, see perception::food_dist_below()),
      !! the expected distance is set the downward walk distance `depth_walk`,
      !! that should be sufficiently long to assure the probability of food
      !! item capture is very small or zero.
      expect_distance_food = depth_walk_here
    end if

    !> #### Call do_this ####
    !> As the first step, we use the **do**-procedure `go_down_depth::do_this()`
    !! => `the_behaviour::go_down_do_this()` to perform the behaviour desired
    !! without changing either the agent or its environment, obtaining  the
    !! **subjective** values of the `this` behaviour components that later feed
    !! into the motivation **expectancy** functions:
    !!  - `perception_override_light`
    !!  - `perception_override_depth`
    !!  - `perception_override_food_dir`
    !!  - `perception_override_predator`
    !!  - `perception_override_stomach`
    !!  - `perception_override_bodymass`
    !!  - `perception_override_energy`
    !!  .
    call this%do_this( this_agent = this_agent,                               &
                       max_depth = max_depth_here,                            &
                       depth_walk = depth_walk_here,                          &
                       time_step_model = time_step_model_here )

    !> The absolute value of the target depth is equal to the agent's current
    !! depth **plus** the depth step class data component this\%distance
    !! because the agent is intended to deepen down.
    target_depth = this_agent%dpos() + this%distance

    !> #### Calculate expected food increments at the target depth ####
    !> ##### Create a virtual expected food item #####
    !> First, create a subjective representation of the expected food item that
    !! is used as a major reference for calculating fake override perceptions.
    !! First, calculate the fake coordinates for the expected food item, a
    !! spatial object of the class the_environment::spatial. They are equal
    !! to those of the actor agent, with the depth coordinate equal to the
    !! target depth.
    call expected_food_item_xyz%position(                                     &
                                          SPATIAL(this_agent%xpos(),          &
                                                  this_agent%ypos(),          &
                                                  target_depth)     )

    !> Make an expected food item using the food_item standard method
    !! `make` (the_environment::food_item::make()) with the following
    !! parameters: the above spatial location, the size equal to the expected
    !! food gain from `do_this`, iid is set to  commondata::unknown.
    !! Note that the size of the food item is reverse-calculated using the
    !! the_environment::mass2size_food() function.
    call expected_food_item%make(                                             &
                                 location=expected_food_item_xyz,             &
                                 size=mass2size_food(this%expected_food_gain),&
                                 iid=UNKNOWN )

    !> Calculate the expected probability of capture (normally using the
    !! average distance to the food items under the agent
    !! perception::food_dist_below(), see above).
    !! Note that the illumination level in the calculation backend is set
    !! from the food item's current depth, i.e. the target depth of the agent.
    !! This means that the subjective illumination level used in the
    !! calculation of the capture probability is reduced automatically
    !! according to the agent's target depth.
    expected_probability_capture =                                            &
      expected_food_item%capture_probability(                                 &
                                      distance=expect_distance_food,          &
                                      time_step_model=time_step_model_here )

    !> ##### Calculate food increments #####
    !> Build the expected food gain perception.
    !> The mass increment that this_agent gets from consuming this
    !! food item is defined by `the_body::condition::food_fitting`.
    !! @note Note that `the_body::condition::food_fitting` already subtracts
    !!       processing cost automatically. Note that the expected food
    !!       increment is weighted by the expected probability of capture of
    !!       the expected food item.
    expect_mass_increment_from_food =                                         &
            this_agent%food_fitting(expected_food_item, expect_distance_food) &
                                      * expected_probability_capture

    !> Stomach increment from food is equal to the above value of the expected
    !! mass increment. However, stomach increment can only be zero or a
    !! positive value.
    expect_stomach_increment_from_food =                                      &
                                  max(0.0_SRP, expect_mass_increment_from_food)

    !> #### Build the fake perceptions ####
    !> ##### Body mass and stomach contents #####
    !> Finally, the fake perceptions for the body mass and stomach content
    !! are calculated as the current body mass minus the cost of moving to
    !! the target depth plus the expected food increment.
    expect_mass_perc_override = max( ZERO,                                    &
                                     this_agent%get_mass() -                  &
                                       this_agent%living_cost() -             &
                                       this%decrement_mass_cost +             &
                                       expect_mass_increment_from_food )

    !> The expected fake perception value for the stomach content at the
    !! target depth is obtained similarly by adding the expected stomach
    !! increment to the current stomach content of the agent.
    agent_stomach = this_agent%get_stom_content()
    expect_stomach_perc_override =                                            &
            max( ZERO,                                                        &
                 agent_stomach - stomach_emptify_backend(agent_stomach)  +    &
                   expect_stomach_increment_from_food )

    !> The expected energy reserves perceived are calculated from the fake
    !! perceptions of the mass and length using the_body::energy_reserve()
    !! function.
    expect_energy_perc_override =                                             &
        energy_reserve( expect_mass_perc_override, this_agent%length() +      &
                        this_agent%len_incr(expect_mass_increment_from_food) )

    !> ##### Conspecifics #####
    !> The fake perception value for the conspecifics at the target depth is
    !! calculated directly from the `this` class data component
    !! this\%expected_consp_number.
    expect_conspecicifc_perc_override = this%expected_consp_number

    !> ##### Predators #####
    !> The fake perception value for the predation risk at the target depth is
    !! calculated directly from the `this` class data component
    expect_predator_perc_override = this%expected_predation_risk

    !> ##### Environmental perceptions #####
    !> The number of food items (direct food perception) is equal to the
    !! number of food items currently under the agent.
    expect_food_perc_override = n_food_items_below

    !> Depth perception is according to the absolute target depth value.
    expect_depth_perc_override = target_depth

    !> Light perception is according to the new depth.
    expect_light_perc_override =                                              &
              light_depth(depth=expect_depth_perc_override,                   &
                          surface_light =                                     &
                              light_surface(tstep=time_step_model_here,       &
                                            is_stochastic=DAYLIGHT_STOCHASTIC) )

    !> #### Calculate motivation expectancies ####
    !> The next step is to calculate the motivational expectancies using the
    !! fake perceptions to override the default (actual agent's) values.
    !> At this stage, first, calculate motivation values resulting from the
    !! behaviour done (`go_down_depth::do_this()`) at the previous steps: what
    !! would be the motivation values *if* the agent does perform
    !! GO_DOWN_DEPTH? Technically, this is done by calling the **neuronal
    !! response function**, `percept_components_motiv::motivation_components()`
    !! method, for each of the motivational states with `perception_override_`
    !! dummy parameters overriding the default values.
    !! Here is the list of the fake overriding perceptions for the
    !! GO_DOWN_DEPTH behaviour:
    !!  - `perception_override_light`
    !!  - `perception_override_depth`
    !!  - `perception_override_food_dir`
    !!  - `perception_override_predator`
    !!  - `perception_override_stomach`
    !!  - `perception_override_bodymass`
    !!  - `perception_override_energy`
    !!  .
    !  @note  **Expectancy** assessment for **hunger** motivation, using
    !         `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in] for
    !         `this_agent` now.
    call this%expectancy%hunger%percept_component%motivation_components       &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL,         &
      param_gp_matrix_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL,     &
      param_gp_matrix_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL,     &
      param_gp_matrix_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL,               &
      param_gp_matrix_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL,             &
      param_gp_matrix_age      = AGE_HUNGER_GENOTYPE_NEURONAL,                &
      param_gp_matrix_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL,            &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,     &
      param_gerror_cv_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_age      = AGE_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,      &
      param_gerror_cv_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,  &
      !  Real agent perception components are now substituted by the *fake*
      !  values resulting from executing this behaviour (`do_this` method).
      !  This is repeated for all the motivations: *hunger*,
      !  *passive avoidance,* *fear state* etc.
      perception_override_light = expect_light_perc_override,                 &
      perception_override_depth = expect_depth_perc_override,                 &
      perception_override_food_dir = real(expect_food_perc_override, SRP),    &
      perception_override_predator = expect_predator_perc_override,           &
      perception_override_stomach = expect_stomach_perc_override,             &
      perception_override_bodymass = expect_mass_perc_override,               &
      perception_override_energy = expect_energy_perc_override                &
                                                                              )
      !> Real agent perception components are now substituted by the *fake*
      !! values resulting from executing this behaviour (`reproduce::do_this()`
      !! => `the_behaviour::reproduce_do_this()` method). This is repeated for
      !! all the motivations: *hunger*, *passive avoidance,* *active
      !! avoidance* etc. These optional **override parameters** are
      !! substituted by the "fake" values.

    !  @note  **Expectancy** assessment for **fear_defence** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%fear_defence%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL, &
      param_gp_matrix_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_light = expect_light_perc_override,                 &
      perception_override_depth = expect_depth_perc_override,                 &
      perception_override_food_dir = real(expect_food_perc_override, SRP),    &
      perception_override_predator = expect_predator_perc_override,           &
      perception_override_stomach = expect_stomach_perc_override,             &
      perception_override_bodymass = expect_mass_perc_override,               &
      perception_override_energy = expect_energy_perc_override                &
                                                                              )

    !  @note  **Expectancy** assessment for **reproduction** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%reproduction%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL,    &
      param_gp_matrix_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_light = expect_light_perc_override,                 &
      perception_override_depth = expect_depth_perc_override,                 &
      perception_override_food_dir = real(expect_food_perc_override, SRP),    &
      perception_override_predator = expect_predator_perc_override,           &
      perception_override_stomach = expect_stomach_perc_override,             &
      perception_override_bodymass = expect_mass_perc_override,               &
      perception_override_energy = expect_energy_perc_override                &
                                                                              )

    !> Next, from the perceptual components calculated at the previous
    !! step we can obtain the **primary** and **final motivation** values by
    !! weighed summing.
    if (present(rescale_max_motivation)) then
      !> Here we can use global maximum motivation across all behaviours and
      !! perceptual components if it is provided, for rescaling.
      max_motivation = rescale_max_motivation
    else
      !> Or can rescale using local maximum value for this behaviour only.
      max_motivation =  this%expectancy%max_perception()
    end if

    !> Transfer attention weights from the actor agent `this_agent` to the
    !! `this` behaviour component. So, we will now use the updated modulated
    !! attention weights of the agent rather than their default parameter
    !! values.
    call this%attention_transfer(this_agent)

    !> So the primary motivation values are calculated.
    call this%expectancy%motivation_primary_calc(max_motivation)

    !> Primary motivations are logged in the @ref intro_debug_mode "debug mode".
    call LOG_DBG( LTAG_INFO // "Primary motivations: " //                     &
                  "hunger: " //                                               &
                    TOSTR(this%expectancy%hunger%motivation_prim)  //         &
                  ", fear_defence: " //                                       &
                    TOSTR(this%expectancy%fear_defence%motivation_prim) //    &
                  ", reproduce: " //                                          &
                    TOSTR(this%expectancy%reproduction%motivation_prim),      &
                  PROCNAME, MODNAME )

    !> There is **no modulation** at this stage, so the final motivation
    !! values are the same as primary motivations.
    call this%expectancy%modulation_none()

    !> #### Calculate motivation expectancies ####
    !> Finally, calculate the finally **expected arousal level for this
    !! behaviour**. As in the GOS, the overall arousal is the maximum value
    !! among all motivation components.
    this%arousal_expected = this%expectancy%max_final()

    !> Log also the final expectancy value in the @ref intro_debug_mode
    !! "debug mode".
    call LOG_DBG( LTAG_INFO // "Expectancy: " // TOSTR(this%arousal_expected),&
                  PROCNAME, MODNAME )

    !> Now as we know the expected arousal, we can choose the behaviour which
    !! would minimise this arousal level.

  end subroutine go_down_motivations_expect

  !-----------------------------------------------------------------------------
  !> Execute this behaviour component "go down" by `this_agent` agent.
  !! @note The "do"-function does not change the state of the this_agent
  !!       or the the environment (the food item), the "execute" function
  !!       **does**.
  subroutine go_down_do_execute( this, this_agent,                            &
                                 max_depth, environments, depth_walk )
        !> @param[inout] this the object itself.
    class(GO_DOWN_DEPTH), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which goes down.
    class(APPRAISAL), intent(inout)    :: this_agent
    !> @param[in] max_depth is the optional maximum limit on the depth.
    real(SRP), optional, intent(in) :: max_depth
    !> @param[in] environments optional array of the all available
    !!            environments where the this agent can be in, needed for the
    !!            calculation of the depth limits. If such an array of the
    !!            environments is provided, `max_depth` has precedence.
    class(ENVIRONMENT), dimension(:), optional, intent(in) :: environments
    !> @param[in] depth_walk Optional downward walk size, by how deep
    !!            the agent goes down.
    real(SRP), intent(in), optional :: depth_walk

    ! Local copy of optionals
    real(SRP) :: depth_walk_here, max_depth_here

    !> ### Implementation details ###
    !> #### Initial checks ####
    !> First, check if the size of the downward walk `depth_walk` dummy
    !! parameter is provided.
    if (present(depth_walk)) then
      depth_walk_here = depth_walk
    else
      !> If it is not provided, it is set equal to the agent's body length
      !! multiplied by the commondata::up_down_walk_step_stdlength_factor
      !! factor parameter. Calculated by `the_behaviour::depth_walk_default()`.
      depth_walk_here = depth_walk_default ( this_agent%get_length() )
    end if

    ! @note The `GET_MAXDEPTH` block is used unchanged in several places,
    !       however, it cannot be isolated into a single procedure because
    !       its code heavily uses optional parameters checks using `present`
    !       intrinsic function that should apply to the called procedure.
    GET_MAXDEPTH: block
      !> Check downward step size. Here, first, check if the target
      !! depth is likely to go beyond the environment depth limits and reduce
      !! the downward walk step size accordingly. Either the explicitly provided
      !! maximum depth dummy parameter `max_depth` or an array of possible
      !! environment objects where the `this_agent` actor agent can be located
      !! is used to get the depth limit.
      max_depth_here = MISSING

      if (present(environments)) then
        !> If the array of possible environment objects that can contain
        !! the max_depth actor agent is provided, the check involves the
        !! `the_environment::spatial::find_environment()` function to find the
        !! specific environment object the agent is currently in followed by
        !! `the_environment::environment::depth_max()` to find the minimum
        !! depth in this environment object.
        max_depth_here =                                                     &
            environments(this_agent%find_environment(environments))%depth_max()
      else
        !> If the array of possible environment objects that can contain the
        !! actor agent is not provided, the current environment is obtained
        !! from the global array the_environment::global_habitats_available.
        !! In this case, the environment that actor agent is within is
        !! determined  using the the_environment::spatial::find_environment()
        !! method, which is in followed by
        !! the_environment::environment::depth_max()` to find the minimum
        !! depth in this environment object.
        max_depth_here = Global_Habitats_Available(                           &
                            this_agent%find_environment(                      &
                                              Global_Habitats_Available)      &
                            )%depth_max()
      end if

      !> If `max_depth` is provided, it has precedence over the depth
      !! detected explicitly or implicitly from the environment objects.
      if (present(max_depth)) max_depth_here = max_depth

      !> In the case neither of the above optional parameters are provided,
      !! the maximum depth is set as the depth of the actor agent (with an
      !! additional condition that it should exceed zero), so movement
      !! down would be **impossible**.
      if (max_depth_here .feq. MISSING)                                       &
                          max_depth_here = max( 0.0_SRP, this_agent%dpos() )
    end block GET_MAXDEPTH

    !> #### Step 1: do_this ####
    !> First, we use the intent-in **do**-procedure `go_down_depth::do_this()`
    !! to perform the behaviour desired and get the **expectations of fake
    !! perceptions** for GOS. As a result, we now get this\%decrement_mass_cost
    !! that defines the cost of buoyancy-based movement downwards.
    !! @note At this stage, the state of the actor agent is not changed.
    call this%do_this(this_agent = this_agent ,                               &
                      max_depth = max_depth_here, depth_walk = depth_walk_here )


    !> #### Step 2: Change the agent ####
    !> Change the location of the actor agent, moving it down to the distance
    !! this\%distance.
    call this_agent%position( SPATIAL ( this_agent%xpos(),                    &
                                        this_agent%ypos(),                    &
                                        this_agent%dpos() + this%distance) )

    !> Decrement the body mass as a consequence of transfer down. This body
    !! mass decrement constitutes the (small) energetic cost of locomotion.
    !! Call `the_body::condition::set_mass()` for this.
    call this_agent%set_mass( value_set = this_agent%get_mass() -             &
                                                this%decrement_mass_cost,     &
                              update_history = .TRUE. )
    !> Additionally, also call the `the_body::condition::set_length()` method
    !! to update the body length history stack. However, the value_set
    !! parameter here is just the current value. This fake re-setting of the
    !! body length is done to keep both mass and length synchronised in their
    !! history stack arrays (there is no procedure for only updating history).
    call this_agent%set_length( value_set = this_agent%get_length(),          &
                                update_history = .TRUE. )

    !> After resetting the body mass, update energy reserves of the agent, that
    !! depend on both the length and the mass.
    call this_agent%energy_update()

    !> Check if the agent is starved to death. If yes, the agent can
    !! die without going any further.
    if (this_agent%starved_death()) call this_agent%dies()

    !> #### Step 3: Change the environment ####
    !> Moving down by the agent does not affect the environmental objects.

  end subroutine go_down_do_execute

  !-----------------------------------------------------------------------------
  !> Initialise the **go up to a shallower spatial layer** behaviour component
  !! to a zero state.
  elemental subroutine go_up_depth_init_zero(this)
    class(GO_UP_DEPTH), intent(inout) :: this

    !> First init components from the base root class
    !! `the_behaviour::behaviour_base`.
    !> Mandatory label component that should be read-only.
    this%label = "GO_UP_DEPTH"
    !> The execution status is always FALSE, can be reset to TRUE only when
    !! the behaviour unit is called to execution.
    this%is_active = .FALSE.

    !> And the *expectancy* components.
    call this%expectancy%init()
    this%arousal_expected = 0.0_SRP

    !> Abstract `MOVE` component.
    this%distance = MISSING

    !> Then init components of this specific behaviour component extended class.
    this%decrement_mass_cost = MISSING
    this%expected_food_gain = MISSING
    this%expected_consp_number = UNKNOWN
    this%expected_predation_risk = MISSING

  end subroutine go_up_depth_init_zero

  !-----------------------------------------------------------------------------
  !> Do go up by `this_agent` (the actor agent). Subjective assessment of the
  !! motivational value for this is based on the number of food items,
  !! conspecifics and predators at the layers below the `this_agent` actor
  !! agent.
  subroutine go_up_do_this(this, this_agent, min_depth, depth_walk,           &
                                          predict_window_food, time_step_model)
    !> @param[inout] this the object itself.
    class(GO_UP_DEPTH), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which goes up.
    class(APPRAISAL), intent(in)    :: this_agent
    !> @param[in] min_depth is the maximum limit on the depth.
    real(SRP), intent(in) :: min_depth
    !> @param[in] depth_walk Optional downward walk size, by how deep
    !!            the agent goes down.
    real(SRP), intent(in), optional :: depth_walk
    !> @param[in] predict_window_food the size of the prediction window, i.e.
    !!            how many steps back in memory are used to calculate the
    !!            predicted food gain. This parameter is limited by the maximum
    !!            commondata::history_size_perception value of the perception
    !!            memory history size.
    integer, optional, intent(in) :: predict_window_food
    !> @param[in] time_step_model optional time step of the model, overrides
    !!            the value calculated from the spatial data.
    integer, optional, intent(in)   :: time_step_model

    ! Local copies of optionals
    real(SRP) :: depth_walk_here

    ! Local copies of optionals.
    integer :: predict_window_food_here, time_step_model_here

    ! **WEIGHT_DIRECT**  is the relative weight  given to the immediate
    ! perception of predators over the predators counts in the memory stack.
    ! Obtained from global parameters
    ! (`commondata::predation_risk_weight_immediate`).
    real(SRP), parameter :: WEIGHT_DIRECT = PREDATION_RISK_WEIGHT_IMMEDIATE

    ! **MEM_WIND** is the size of the memory window when assessing the
    ! predator risk, only this number of the latest elements from the memory
    ! stack is taken into account. So we further weight the direct threat
    ! over the background risk when making the decision.
    ! @note  Note that we take into account the whole memory size
    !        (commondata::history_size_perception).
    integer, parameter :: MEM_WIND = HISTORY_SIZE_PERCEPTION

    !> ### Implementation details ###
    !> First, check if the size of the upward walk `depth_walk` dummy
    !! parameter is provided.
    if (present(depth_walk)) then
      depth_walk_here = depth_walk
    else
      !> If it is not provided, it is set equal to the agent's body length
      !! multiplied by the commondata::up_down_walk_step_stdlength_factor
      !! factor parameter.  Calculated by `the_behaviour::depth_walk_default()`.
      depth_walk_here = depth_walk_default ( this_agent%get_length() )
    end if

    !> Check optional parameter for the food perception memory window. If
    !! the `predict_window_food` dummy parameter is not provided, its default
    !! value is the proportion of the whole perceptual memory window defined
    !! by commondata::history_perception_window_food. Thus, only the
    !! latest part of the memory is used for the prediction of the future
    !! food gain.
    if (present(predict_window_food)) then
      predict_window_food_here = predict_window_food
    else
      predict_window_food_here = floor( HISTORY_SIZE_PERCEPTION *             &
                                        HISTORY_PERCEPTION_WINDOW_FOOD )
    end if

    !> Check optional time step parameter. If unset, use global
    !! `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> #### Upward step size ####
    !> Here, first, check if the target depth is likely to go beyond the
    !! environment depth limits and reduce the upwnward walk step size
    !! accordingly. Namely, if the depth coordinate of the actor agent
    !! minus the depth step exceeds the minimum depth, the step is reduced
    !! to be within the available environment:
    !! @f$ d_{a} - D_{min} - \varepsilon @f$, where @f$ D_{min} @f$ is the
    !! maximum depth, @f$ d_{a} @f$ is the agent's current depth and
    !! @f$ \varepsilon @f$ is a very small constant defined by the parameter
    !! commondata::zero.
    if (this_agent%dpos() - depth_walk_here <= min_depth )                    &
                depth_walk_here = max( 0.0_SRP,                               &
                                       this_agent%dpos() - min_depth - ZERO )

    !> The upward step size component of the class is then equal to the
    !! `depth_walk`.
    this%distance = depth_walk_here

    !> #### The cost of swimming up ####
    !> The expected cost of the swimming up by the buoyancy is much smaller
    !! than active propulsion. It is set as a fraction, defined by the
    !! parameter commondata::swimming_cost_factor_buoyancy_down, of active
    !! laminar propulsion calculated by function
    !! the_body::condition_cost_swimming_burst().
    this%decrement_mass_cost = SWIMMING_COST_FACTOR_BUOYANCY_UP *             &
              this_agent%cost_swim( distance=depth_walk_here,                 &
                                    exponent=SWIMMING_COST_EXPONENT_LAMINAR )

    !> #### Calculate expected perceptions ####
    !> Calculate the number of conspecifics upwards of the agent using the
    !! function perception::consp_below().
    this%expected_consp_number = this_agent%consp_above()

    !> Calculate the expected predation risk above the agent.
    this%expected_predation_risk =                                            &
        predation_risk_backend(                                               &
                              this_agent%pred_above(),                        &
                              this_agent%memory_stack%get_pred_mean(MEM_WIND),&
                              WEIGHT_DIRECT )

    !> Calculate the expected food gain as an average mass of the food items
    !! above the agent. It is used by calling perception::food_mass_below()
    !! function.
    !! This expected food gain is then weighted by the subjective probability
    !! of food item capture that is calculated based on the memory
    !! the_neurobio::perception::food_probability_capture_subjective().
    this%expected_food_gain = this_agent%food_mass_above() *                  &
                              this_agent%food_probability_capture_subjective( &
                              predict_window_food_here, time_step_model_here )

  end subroutine go_up_do_this

  !-----------------------------------------------------------------------------
  !> `go_up_depth::motivations_expect()` is a subroutine (re)calculating
  !! motivations from fake expected perceptions following from the procedure
  !! `go_up_depth::do_this()` => `the_behaviour::go_up_do_this()`.
  subroutine go_up_motivations_expect(this, this_agent, depth_walk,           &
                                        min_depth, environments,              &
                                        time_step_model, rescale_max_motivation)
    !> @param[inout] this the object itself.
    class(GO_UP_DEPTH), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which goes up.
    class(APPRAISAL), intent(in)    :: this_agent
    !> @param[in] depth_walk The upward walk size, by how deep the agent
    !!            goes up.
    real(SRP), intent(in), optional :: depth_walk
    !> @param[in] min_depth is the optional maximum limit on the depth.
    real(SRP), intent(in), optional :: min_depth
    !> @param[in] environments optional array of the all available
    !!            environments where the this agent can be in, needed for the
    !!            calculation of the depth limits. If such an array of the
    !!            environments is provided, min_depth` has precedence.
    class(ENVIRONMENT), dimension(:), optional, intent(in) :: environments
    !> @param [in] time_step_model optional time step of the model,
    !!             **overrides** the value calculated from the spatial data.
    integer, optional, intent(in)   :: time_step_model
    !> @param[in] rescale_max_motivation maximum motivation value for
    !!            rescaling all motivational components for comparison
    !!            across all motivation and perceptual components and behaviour
    !!            units.
    real(SRP), optional, intent(in) :: rescale_max_motivation

    ! Local copy of optional depth_walk
    real(SRP) :: depth_walk_here, min_depth_here

    ! Local copy of optional model time step
    integer :: time_step_model_here

    !> Target depth, i.e. the absolute depth of the agent after it moves up.
    real(SRP) :: target_depth

    ! Expected food item that is used in the calculations, its properties are
    ! based on the average food items that the agent perceives above.
    type(FOOD_ITEM) :: expected_food_item

    ! the coordinates of the expected food item.
    type(SPATIAL) :: expected_food_item_xyz

    ! The expected distance to the food item at the target upward horizon.
    real(SRP) :: expect_distance_food

    ! Expected mass increment from food at the target depth.
    real(SRP) :: expect_mass_increment_from_food

    ! Expected stomach increment from food at the target depth.
    real(SRP) :: expect_stomach_increment_from_food

    ! The number of food items over the agent, obtained from the current
    ! perception object.
    integer :: n_food_items_above

    ! Local variable
    real(SRP) :: max_motivation ! Local max. over all motivation components.

    !> ### Notable local variables ###
    !> #### Perception overrides ####
    !> - **expect_food_perc_override** is the fake perception for the food items
    !!   at the target depth.
    integer :: expect_food_perc_override

    !> - **expect_depth_perc_override** is the fake perception of the depth,
    !!   identical to the target depth.
    real(SRP) :: expect_depth_perc_override

    !> - **expect_light_perc_override** is the fake perception of the
    !!   illumination level at the target depth.
    real(SRP) :: expect_light_perc_override

    !> - **expect_mass_perc_override** is the fake perception value for the
    !!   mass from the expected food.
    real(SRP) :: expect_mass_perc_override

    !> - **expect_stomach_perc_override** is the fake perception value for the
    !!   stomach increment from the expected food.
    real(SRP) :: expect_stomach_perc_override

    ! Current stomach contents mass of the actor agent.
    real(SRP) :: agent_stomach

    !> - **expect_energy_perc_override** is the fake perception for the energy
    !!   reserves from the expected food at the target depth.
    real(SRP) :: expect_energy_perc_override

    !> - **expected_probability_capture** is the expected probability of capture
    !!   of the expected food item at the target depth.
    real(SRP) :: expected_probability_capture

    !> - **expect_conspecicifc_perc_override** is the fake perception value for
    !!   the number of conspecifics at the target depth.
    integer :: expect_conspecicifc_perc_override

    !> - **expect_predator_perc_override** is fake perception value for the
    !!   predation risk at the target depth.
    !! .
    real(SRP) :: expect_predator_perc_override

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME =                                 &
                                          "(go_up_motivations_expect)"

    !> ### Implementation details ###
    !> #### Sanity checks and preparations ####
    !> Initially, check if the size of the upward walk `depth_walk` dummy
    !! parameter is provided.
    if (present(depth_walk)) then
      depth_walk_here = depth_walk
    else
      !> If it is not provided, it is set equal to the agent's body length
      !! multiplied by the commondata::up_down_walk_step_stdlength_factor
      !! factor parameter.Calculated by `the_behaviour::depth_walk_default()`.
      depth_walk_here = depth_walk_default ( this_agent%get_length() )
    end if

    ! @note The `GET_MAXDEPTH` block is used unchanged in several places,
    !       however, it cannot be isolated into a single procedure because
    !       its code heavily uses optional parameters checks using `present`
    !       intrinsic function that should apply to the called procedure.
    GET_MAXDEPTH: block
      !> Check upward step size. Here, first, check if the target depth is
      !! likely to go beyond the environment depth limits and reduce the upward
      !! walk step size accordingly. Either the explicitly provided minimum
      !! depth dummy parameter `min_depth` or an array of possible environment
      !! objects where the `this_agent` actor agent can be located is used to
      !! get the depth limit.
      min_depth_here = MISSING

      if (present(environments)) then
        !> If the array of possible environment objects that can contain the
        !! actor agent is provided, the check involves the
        !! `the_environment::spatial::find_environment()` function to find the
        !! specific environment object the agent is currently in followed by
        !! `the_environment::environment::depth_min()` to find the minimum
        !! depth in this environment object.
        min_depth_here =                                                     &
            environments(this_agent%find_environment(environments))%depth_min()
      else
        !> If the array of possible environment objects that can contain the
        !! actor agent is not provided, the current environment is obtained
        !! from the global array the_environment::global_habitats_available.
        !! In this case, the environment that actor agent is within is
        !! determined  using the the_environment::spatial::find_environment()
        !! method, which is in followed by
        !! the_environment::environment::depth_max()` to find the minimum
        !! depth in this environment object.
        min_depth_here = Global_Habitats_Available(                           &
                            this_agent%find_environment(                      &
                                              Global_Habitats_Available)      &
                            )%depth_min()
      end if

      !> If `min_depth` is provided, it has precedence over the depth
      !! detected from environment objects.
      if (present(min_depth)) min_depth_here = min_depth

      !> In the case the minimum depth cannot be determined,it is set as
      !! the depth of the actor agent (with an additional condition that it
      !! should exceed zero), so movement up would be **impossible**.
      !! Notably, it is not set to zero, a logical
      !! choice, to avoid possible asymmetric effects as the counterpart
      !! "move down" procedures use the agent's current depth as a last resort
      !! in the analogous case of no depth parameters.
      if (min_depth_here .feq. MISSING)                                       &
                          min_depth_here = max( 0.0_SRP, this_agent%dpos() )
    end block GET_MAXDEPTH

    !> If the depth coordinate of the actor agent minus the depth step is
    !! smaller than the minimum depth, the step is reduced to be strictly within
    !! the available environment. However, it should also never be below zero.
    if (this_agent%dpos() - depth_walk_here <= min_depth_here )               &
              depth_walk_here = max( 0.0_SRP,                                 &
                                     this_agent%dpos() - min_depth_here - ZERO )

    !> Check optional time step parameter. If not provided, use global
    !! parameter value from commondata::global_time_step_model_current.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> Assess the number of food items above using the
    !! perception::food_items_above() method.
    n_food_items_above = this_agent%food_items_above()

    !> Calculate the expected distance to the food item. It is equal to the
    !! average distance to the food items perceived above in case there are
    !! any such items perceived above. Calculated using the
    !! perception::food_dist_above() method.
    if ( n_food_items_above > 0 ) then
      expect_distance_food = this_agent%food_dist_above()
    else
      !> However, if there are no food items above (resulting a
      !! commondata::missing distance, see perception::food_dist_below()),
      !! the expected distance is set the upward walk distance `depth_walk`,
      !! that should be sufficiently long to assure the probability of food
      !! item capture is very small or zero.min_depth
      expect_distance_food = depth_walk_here
    end if

    !> #### Call do_this ####
    !> As the first step, we use the **do**-procedure `go_up_depth::do_this()`
    !! => `the_behaviour::go_up_do_this()` to perform the behaviour desired
    !! without changing either the agent or its environment, obtaining  the
    !! **subjective** values of the `this` behaviour components that later feed
    !! into the motivation **expectancy** functions:
    !!  - `perception_override_light`
    !!  - `perception_override_depth`
    !!  - `perception_override_food_dir`
    !!  - `perception_override_predator`
    !!  - `perception_override_stomach`
    !!  - `perception_override_bodymass`
    !!  - `perception_override_energy`
    !!  .
    call this%do_this(this_agent = this_agent,                               &
                      min_depth = min_depth_here, depth_walk = depth_walk_here )

    !> The absolute value of the target depth is equal to the agent's current
    !! depth **minus** the depth step class data component this\%distance
    !! because the agent is intended to lift up.
    target_depth = this_agent%dpos() - this%distance

    !> #### Calculate expected food increments at the target depth ####
    !> ##### Create a virtual expected food item #####
    !> First, create a subjective representation of the expected food item that
    !! is used as a major reference for calculating fake override perceptions.
    !! First, calculate the fake coordinates for the expected food item, a
    !! spatial object of the class the_environment::spatial. They are equal
    !! to those of the actor agent, with the depth coordinate equal to the
    !! target depth.
    call expected_food_item_xyz%position(                                     &
                                          SPATIAL(this_agent%xpos(),          &
                                                  this_agent%ypos(),          &
                                                  target_depth)     )

    !> Make an expected food item using the food_item standard method
    !! `make` (the_environment::food_item::make()) with the following
    !! parameters: the above spatial location, the size equal to the expected
    !! food gain from `do_this`, iid is set to  commondata::unknown.
    !! Note that the size of the food item is reverse-calculated using the
    !! the_environment::mass2size_food() function.
    call expected_food_item%make(                                             &
                                 location=expected_food_item_xyz,             &
                                 size=mass2size_food(this%expected_food_gain),&
                                 iid=UNKNOWN )

    !> Calculate the expected probability of capture (normally using the
    !! average distance to the food items above the agent
    !! perception::food_dist_above()).
    !! Note that the illumination level in the calculation backend is set
    !! from the food item's current depth, i.e. the target depth of the agent.
    !! This means that the subjective illumination level used in the
    !! calculation of the capture probability is increased automatically
    !! according to the agent's target depth.
    expected_probability_capture =                                            &
      expected_food_item%capture_probability(                                 &
                                      distance=expect_distance_food,          &
                                      time_step_model=time_step_model_here )

    !> ##### Calculate food increments #####
    !> Build the expected food gain perception.
    !> The mass increment that this_agent gets from consuming this
    !! food item is defined by `the_body::condition::food_fitting`.
    !! @note Note that `the_body::condition::food_fitting` already subtracts
    !!       processing cost automatically. Note that the expected food
    !!       increment is weighted by the expected probability of capture of
    !!       the expected food item.
    expect_mass_increment_from_food =                                         &
            this_agent%food_fitting(expected_food_item, expect_distance_food) &
                                      * expected_probability_capture

    !> Stomach increment from food is equal to the above value of the expected
    !! mass increment. However, stomach increment can only be zero or a
    !! positive value.
    expect_stomach_increment_from_food =                                      &
                                  max(0.0_SRP, expect_mass_increment_from_food)

    !> #### Build the fake perceptions ####
    !> ##### Body mass and stomach contents #####
    !> Finally, the fake perceptions for the body mass and stomach content
    !! are calculated as the current body mass minus the cost of moving to
    !! the target depth plus the expected food increment.
    expect_mass_perc_override = max( ZERO,                                    &
                                     this_agent%get_mass() -                  &
                                       this_agent%living_cost() -             &
                                       this%decrement_mass_cost +             &
                                       expect_mass_increment_from_food )

    !> The expected fake perception value for the stomach content at the
    !! target depth is obtained similarly by adding the expected stomach
    !! increment to the current stomach content of the agent.
    agent_stomach = this_agent%get_stom_content()
    expect_stomach_perc_override =                                            &
            max( ZERO,                                                        &
                 agent_stomach - stomach_emptify_backend(agent_stomach)  +    &
                   expect_stomach_increment_from_food )

    !> The expected energy reserves perceived are calculated from the fake
    !! perceptions of the mass and length using the_body::energy_reserve()
    !! function.
    expect_energy_perc_override =                                             &
        energy_reserve( expect_mass_perc_override, this_agent%length() +      &
                        this_agent%len_incr(expect_mass_increment_from_food) )

    !> ##### Conspecifics #####
    !> The fake perception value for the conspecifics at the target depth is
    !! calculated directly from the `this` class data component
    !! this\%expected_consp_number.
    expect_conspecicifc_perc_override = this%expected_consp_number

    !> ##### Predators #####
    !> The fake perception value for the predation risk at the target depth is
    !! calculated directly from the `this` class data component
    expect_predator_perc_override = this%expected_predation_risk

    !> ##### Environmental perceptions #####
    !> The number of food items (direct food perception) is equal to the
    !! number of food items currently above the agent.
    expect_food_perc_override = n_food_items_above

    !> Depth perception is according to the absolute target depth value.
    expect_depth_perc_override = target_depth

    !> Light perception is according to the new depth.
    expect_light_perc_override =                                              &
              light_depth(depth=expect_depth_perc_override,                   &
                          surface_light =                                     &
                              light_surface(tstep=time_step_model_here,       &
                                            is_stochastic=DAYLIGHT_STOCHASTIC) )

    !> #### Calculate motivation expectancies ####
    !> The next step is to calculate the motivational expectancies using the
    !! fake perceptions to override the default (actual agent's) values.
    !> At this stage, first, calculate motivation values resulting from the
    !! behaviour done (`go_up_depth::do_this()`) at the previous steps: what
    !! would be the motivation values *if* the agent does perform
    !! GO_UP_DEPTH? Technically, this is done by calling the **neuronal
    !! response function**, `percept_components_motiv::motivation_components()`
    !! method, for each of the motivational states with `perception_override_`
    !! dummy parameters overriding the default values.
    !! Here is the list of the fake overriding perceptions for the
    !! GO_UP_DEPTH behaviour:
    !!  - `perception_override_light`
    !!  - `perception_override_depth`
    !!  - `perception_override_food_dir`
    !!  - `perception_override_predator`
    !!  - `perception_override_stomach`
    !!  - `perception_override_bodymass`
    !!  - `perception_override_energy`
    !!  .
    !  @note  **Expectancy** assessment for **hunger** motivation, using
    !         `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in] for
    !         `this_agent` now.
    call this%expectancy%hunger%percept_component%motivation_components       &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL,         &
      param_gp_matrix_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL,        &
      param_gp_matrix_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL,     &
      param_gp_matrix_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL,               &
      param_gp_matrix_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL,             &
      param_gp_matrix_age      = AGE_HUNGER_GENOTYPE_NEURONAL,                &
      param_gp_matrix_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL,            &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,     &
      param_gerror_cv_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_age      = AGE_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,      &
      param_gerror_cv_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,  &
      !  Real agent perception components are now substituted by the *fake*
      !  values resulting from executing this behaviour (`do_this` method).
      !  This is repeated for all the motivations: *hunger*,
      !  *passive avoidance,* *fear state* etc.
      perception_override_light = expect_light_perc_override,                 &
      perception_override_depth = expect_depth_perc_override,                 &
      perception_override_food_dir = real(expect_food_perc_override, SRP),    &
      perception_override_predator = expect_predator_perc_override,           &
      perception_override_stomach = expect_stomach_perc_override,             &
      perception_override_bodymass = expect_mass_perc_override,               &
      perception_override_energy = expect_energy_perc_override                &
                                                                              )
      !> Real agent perception components are now substituted by the *fake*
      !! values resulting from executing this behaviour (`reproduce::do_this()`
      !! => `the_behaviour::reproduce_do_this()` method). This is repeated for
      !! all the motivations: *hunger*, *passive avoidance,* *active
      !! avoidance* etc. These optional **override parameters** are
      !! substituted by the "fake" values.

    !  @note  **Expectancy** assessment for **fear_defence** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%fear_defence%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL,    &
      param_gp_matrix_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_light = expect_light_perc_override,                 &
      perception_override_depth = expect_depth_perc_override,                 &
      perception_override_food_dir = real(expect_food_perc_override, SRP),    &
      perception_override_predator = expect_predator_perc_override,           &
      perception_override_stomach = expect_stomach_perc_override,             &
      perception_override_bodymass = expect_mass_perc_override,               &
      perception_override_energy = expect_energy_perc_override                &
                                                                              )

    !  @note  **Expectancy** assessment for **reproduction** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%reproduction%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL,    &
      param_gp_matrix_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_light = expect_light_perc_override,                 &
      perception_override_depth = expect_depth_perc_override,                 &
      perception_override_food_dir = real(expect_food_perc_override, SRP),    &
      perception_override_predator = expect_predator_perc_override,           &
      perception_override_stomach = expect_stomach_perc_override,             &
      perception_override_bodymass = expect_mass_perc_override,               &
      perception_override_energy = expect_energy_perc_override                &
                                                                              )

    !> Next, from the perceptual components calculated at the previous
    !! step we can obtain the **primary** and **final motivation** values by
    !! weighed summing.
    if (present(rescale_max_motivation)) then
      !> Here we can use global maximum motivation across all behaviours and
      !! perceptual components if it is provided, for rescaling.
      max_motivation = rescale_max_motivation
    else
      !> Or can rescale using local maximum value for this behaviour only.
      max_motivation =  this%expectancy%max_perception()
    end if

    !> Transfer attention weights from the actor agent `this_agent` to the
    !! `this` behaviour component. So, we will now use the updated modulated
    !! attention weights of the agent rather than their default parameter
    !! values.
    call this%attention_transfer(this_agent)

    !> So the primary motivation values are calculated.
    call this%expectancy%motivation_primary_calc(max_motivation)

    !> Primary motivations are logged in the @ref intro_debug_mode "debug mode".
    call LOG_DBG( LTAG_INFO // "Primary motivations: " //                   &
                  "hunger: " //                                             &
                    TOSTR(this%expectancy%hunger%motivation_prim)  //       &
                  ", fear_defence: " //                                     &
                    TOSTR(this%expectancy%fear_defence%motivation_prim) //  &
                  ", reproduce: " //                                        &
                    TOSTR(this%expectancy%reproduction%motivation_prim),    &
                  PROCNAME, MODNAME )

    !> There is **no modulation** at this stage, so the final motivation
    !! values are the same as primary motivations.
    call this%expectancy%modulation_none()

    !> #### Calculate motivation expectancies ####
    !> Finally, calculate the finally **expected arousal level for this
    !! behaviour**. As in the GOS, the overall arousal is the maximum value
    !! among all motivation components.
    this%arousal_expected = this%expectancy%max_final()

    !> Log also the final expectancy value in the @ref intro_debug_mode
    !! "debug mode".
    call LOG_DBG( LTAG_INFO // "Expectancy: " // TOSTR(this%arousal_expected),&
                  PROCNAME, MODNAME )

    !> Now as we know the expected arousal, we can choose the behaviour which
    !! would minimise this arousal level.

  end subroutine go_up_motivations_expect

  !-----------------------------------------------------------------------------
  !> Execute this behaviour component "go up" by `this_agent` agent towards.
  !! @note The "do"-function does not change the state of the this_agent
  !!       or the the environment (the food item), the "execute" function
  !!       **does**.
  subroutine go_up_do_execute( this, this_agent,                            &
                                 min_depth, environments, depth_walk )
        !> @param[inout] this the object itself.
    class(GO_UP_DEPTH), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which goes up.
    class(APPRAISAL), intent(inout)    :: this_agent
    !> @param[in] min_depth is the optional minimum limit on the depth.
    real(SRP), optional, intent(in) :: min_depth
    !> @param[in] environments optional array of the all available
    !!            environments where the this agent can be in, needed for the
    !!            calculation of the depth limits. If such an array of the
    !!            environments is provided, `min_depth` has precedence.
    class(ENVIRONMENT), dimension(:), optional, intent(in) :: environments
    !> @param[in] depth_walk Optional upward walk size, by how deep
    !!            the agent goes up.
    real(SRP), intent(in), optional :: depth_walk

    ! Local copies of optionals
    real(SRP) :: depth_walk_here, min_depth_here

    !> ### Implementation details ###
    !> #### Initial checks ####
    !> First, check if the size of the upward walk `depth_walk` dummy
    !! parameter is provided.
    if (present(depth_walk)) then
      depth_walk_here = depth_walk
    else
      !> If it is not provided, it is set equal to the agent's body length
      !! multiplied by the commondata::up_down_walk_step_stdlength_factor
      !! factor parameter. Calculated by `the_behaviour::depth_walk_default()`.
      depth_walk_here = depth_walk_default ( this_agent%get_length() )
    end if

    ! @note The `GET_MAXDEPTH` block is used unchanged in several places,
    !       however, it cannot be isolated into a single procedure because
    !       its code heavily uses optional parameters checks using `present`
    !       intrinsic function that should apply to the called procedure.
    GET_MAXDEPTH: block
      !> Check upward step size. Here, first, check if the target depth is
      !! likely to go beyond the environment depth limits and reduce the upward
      !! walk step size accordingly. Either the explicitly provided minimum
      !! depth dummy parameter `min_depth` or an array of possible environment
      !! objects where the `this_agent` actor agent can be located is used to
      !! get the depth limit.
      min_depth_here = MISSING

      if (present(environments)) then
        !> If the array of possible environment objects that can contain the
        !! actor agent is provided, the check involves the
        !! `the_environment::spatial::find_environment()` function to find the
        !! specific environment object the agent is currently in followed by
        !! in this `the_environment::environment::depth_min()` to find the
        !! minimum depth in this environment object.
        min_depth_here =                                                     &
            environments(this_agent%find_environment(environments))%depth_min()
      else
        !> If the array of possible environment objects that can contain the
        !! actor agent is not provided, the current environment is obtained
        !! from the global array the_environment::global_habitats_available.
        !! In this case, the environment that actor agent is within is
        !! determined  using the the_environment::spatial::find_environment()
        !! method, which is in followed by
        !! the_environment::environment::depth_max()` to find the minimum
        !! depth in this environment object.
        min_depth_here = Global_Habitats_Available(                           &
                            this_agent%find_environment(                      &
                                              Global_Habitats_Available)      &
                            )%depth_min()
      end if

      !> If `min_depth` is provided, it has precedence over the depth
      !! detected explicitly or implicitly from the environment objects.
      if (present(min_depth)) min_depth_here = min_depth

      !> In the case neither of the above optional parameters are provided,
      !! the minimum depth is set as the depth of the actor agent (with an
      !! additional condition that it should exceed zero), so movement
      !! up would be **impossible**. Notably, it is not set to zero, a logical
      !! choice, to avoid possible asymmetric effects as the counterpart
      !! "move down" procedures use the agent's current depth as a last resort
      !! in the analogous case of no depth parameters.
      if (min_depth_here .feq. MISSING)                                       &
                          min_depth_here = max( 0.0_SRP, this_agent%dpos() )
    end block GET_MAXDEPTH

    !> #### Step 1: do_this ####
    !> First, we use the intent-in **do**-procedure `go_up_depth::do_this()`
    !! to perform the behaviour desired and get the **expectations of fake
    !! perceptions** for GOS. As a result, we now get this\%decrement_mass_cost
    !! that defines the cost of buoyancy-based movement upwards.
    !! @note At this stage, the state of the actor agent is not changed.
    call this%do_this(this_agent = this_agent ,                               &
                      min_depth = min_depth_here, depth_walk = depth_walk_here )


    !> #### Step 2: Change the agent ####
    !> Change the location of the actor agent, moving it up to the distance
    !! this\%distance.
    call this_agent%position( SPATIAL ( this_agent%xpos(),                    &
                                        this_agent%ypos(),                    &
                                        this_agent%dpos() - this%distance) )

    !> Decrement the body mass as a consequence of transfer upwards. This body
    !! mass decrement constitutes the (small) energetic cost of locomotion.
    !! Call `the_body::condition::set_mass()` for this.
    call this_agent%set_mass( value_set = this_agent%get_mass() -             &
                                                this%decrement_mass_cost,     &
                              update_history = .TRUE. )
    !> Additionally, also call the `the_body::condition::set_length()` method
    !! to update the body length history stack. However, the value_set
    !! parameter here is just the current value. This fake re-setting of the
    !! body length is done to keep both mass and length synchronised in their
    !! history stack arrays (there is no procedure for only updating history).
    call this_agent%set_length( value_set = this_agent%get_length(),          &
                                update_history = .TRUE. )

    !> After resetting the body mass, update energy reserves of the agent, that
    !! depend on both the length and the mass.
    call this_agent%energy_update()

    !> Check if the agent is starved to death. If yes, the agent can
    !! die without going any further.
    if (this_agent%starved_death()) call this_agent%dies()

    !> #### Step 3: Change the environment ####
    !> Moving down by the agent does not affect the environmental objects.

  end subroutine go_up_do_execute

  !-----------------------------------------------------------------------------
  !> Initialise the **fake debug behaviour** behaviour component
  !! to a zero state.
  elemental subroutine debug_base_init_zero(this)
    class(DEBUG_BASE), intent(inout) :: this

    !> First init components from the base root class
    !! `the_neurobio::behaviour_base`.
    !> Mandatory label component that should be read-only.
    this%label = "DEBUG_BASE"
    !> The execution status is always FALSE, can be reset to TRUE only when
    !! the behaviour unit is called to execution.
    !! @note Note that this behaviour unit is never executed.
    this%is_active = .FALSE.

    !> And the *expectancy* components.
    call this%expectancy%init()
    this%arousal_expected = 0.0_SRP

  end subroutine debug_base_init_zero

  !-----------------------------------------------------------------------------
  !> `the_behaviour::debug_base::motivations_expect()` is a subroutine
  !! (re)calculating motivations from fake expected perceptions for the
  !! **fake debug behaviour**.
  subroutine debug_base_motivations_expect(this, this_agent, time_step_model, &
                           rescale_max_motivation)
    !> @param [inout] this the self object.
    class(DEBUG_BASE), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which does reproduce.
    class(APPRAISAL), intent(in)    :: this_agent
    !> @param [in] time_step_model optional time step of the model,
    !!             **overrides** the value calculated from the spatial data.
    integer, optional, intent(in)   :: time_step_model
    !> @param[in] rescale_max_motivation maximum motivation value for
    !!            rescaling all motivational components for comparison
    !!            across all motivation and perceptual components and behaviour
    !!            units.
    real(SRP), optional, intent(in) :: rescale_max_motivation

    ! Local copy of optional model time step
    integer :: time_step_model_here

    ! Local variable
    real(SRP) :: max_motivation ! Local max. over all motivation components.

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME = "(debug_base_motivations_expect)"

    !> ### Implementation notes ###
    !> #### Check optional parameters ####
    !> Check optional time step parameter. If not provided, use global
    !! parameter value from `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> #### Main processing steps ####
    !> This is the **fake debug behaviour**, for which the **do**-procedure
    !! is absent.
    !!
    !> The motivation values resulting from the behaviour are calculated
    !! for unchanged perceptions. That is, no fake perceptions are placed
    !! into the percept_components_motiv::motivation_components() procedures.
    call this%expectancy%hunger%percept_component%motivation_components       &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL,         &
      param_gp_matrix_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL,        &
      param_gp_matrix_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL,     &
      param_gp_matrix_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL,               &
      param_gp_matrix_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL,             &
      param_gp_matrix_age      = AGE_HUNGER_GENOTYPE_NEURONAL,                &
      param_gp_matrix_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL,            &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,     &
      param_gerror_cv_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_age      = AGE_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,      &
      param_gerror_cv_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL_GERROR_CV   &
                                                                              )

    !  The motivation values resulting from the behaviour are calculated
    !  for unchanged perceptions. That is, no fake perceptions are placed
    !  into the percept_components_motiv::motivation_components() procedures.
    call this%expectancy%fear_defence%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL,    &
      param_gp_matrix_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV&
                                                                              )

    !  The motivation values resulting from the behaviour are calculated
    !  for unchanged perceptions. That is, no fake perceptions are placed
    !  into the percept_components_motiv::motivation_components() procedures.
    call this%expectancy%reproduction%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL,    &
      param_gp_matrix_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV &
                                                                              )

    !> From the perceptual components calculated at the previous
    !! step we can obtain the **primary** and **final motivation** values by
    !! weighed summing.
    if (present(rescale_max_motivation)) then
      !> Here we can use global maximum motivation across all behaviours and
      !! perceptual components if it is provided, for rescaling.
      max_motivation = rescale_max_motivation
    else
      !> Or can rescale using local maximum value for this behaviour only.
      max_motivation =  this%expectancy%max_perception()
    end if

    !> Transfer attention weights from the actor agent `this_agent` to the
    !! `this` behaviour component. So, we will now use the updated modulated
    !! attention weights of the agent rather than their default parameter
    !! values.
    call this%attention_transfer(this_agent)

    !> So the primary motivation values are calculated.
    call this%expectancy%motivation_primary_calc(max_motivation)

    !> Primary motivations are logged in the @ref intro_debug_mode "debug mode".
    call LOG_DBG( LTAG_INFO // "Primary motivations: " //                     &
                  "hunger: " //                                               &
                    TOSTR(this%expectancy%hunger%motivation_prim)  //         &
                  ", fear_defence: " //                                       &
                    TOSTR(this%expectancy%fear_defence%motivation_prim) //    &
                  ", reproduce: " //                                          &
                    TOSTR(this%expectancy%reproduction%motivation_prim),      &
                  PROCNAME, MODNAME )

    !> There is **no modulation** at this stage, so the final motivation
    !! values are the same as primary motivations.
    !! TODO: Should include developmental or other modulation? If yes, need to
    !!       separate genetic modulation component from
    !!       `motivation_modulation_genetic` into a procedure bound to
    !!       `MOTIVATIONS` with `this_agent` as actor.
    call this%expectancy%modulation_none()

    !> **Fourth,** Calculate the finally **expected arousal level for this
    !! behaviour**. As in the GOS, the overall arousal is the maximum value
    !! among all motivation components.
    this%arousal_expected = this%expectancy%max_final()

    !> Log also the final expectancy value in the @ref intro_debug_mode
    !! "debug mode".
    call LOG_DBG( LTAG_INFO // "Expectancy: " // TOSTR(this%arousal_expected),&
                  PROCNAME, MODNAME )

    !> Now as we know the expected arousal, we can choose the behaviour which
    !! would minimise this arousal level.

  end subroutine debug_base_motivations_expect

  !-----------------------------------------------------------------------------
  !> Eat a food item defined by the object `food_item_eaten`.
  !! The "do" procedure component of the behaviour element performs the
  !! behaviour without affecting the actor agent (the_agent) and the world
  !! (here food_item_eaten) which have intent(in), so it only can change
  !! the internal representation of the behaviour (the type to which this
  !! procedure is bound to, here `the_behaviour::eat_food`). So, here the
  !! result of this procedure is assessment of the stomach content increment
  !! and body mass increment that would result from eating the **this** food
  !! item by the **this_agent**.
  !> The **main output** from this **do** procedure is the `this` behavioural
  !! unit, namely two of its internal data components:
  !! - this\%mass_increment_from_food
  !! - this\%stomach_increment_from_food
  !! .
  !! @note The "do"-function does not change the state of the this_agent
  !!       or the the environment (the food item), the "execute" function
  !!       does change them.
  !! @note Use subroutine rather than function as the "do"-action can
  !!       potentially have several results / outputs, affect several
  !!       components of the behaviour object.
  !! @note There are three optional parameters which can be used as "fake"
  !!       parameters in calculating fake values for subjective expectancy:
  !!       `distance_food_item`, `capture_prob`, `time_step_model`.
  !!       If they are not set, true objective values are calculated or used,
  !!       e.g. time step of the model is taken from
  !!       `commondata::global_time_step_model_current` and the distance
  !!       between the agent and the food item `distance_food_item` is
  !!       calculated from their spatial data.
  subroutine eat_food_item_do_this(this, this_agent, food_item_eaten,         &
                time_step_model, distance_food_item, capture_prob, is_captured)
    !> @param[inout] this the object itself.
    class(EAT_FOOD), intent(inout)  :: this
    !> @param[in] this_agent is the actor agent which eats the food item.
    class(APPRAISAL), intent(in)    :: this_agent
    !> @param[in] food_item_eaten is the food object that is eaten.
    class(FOOD_ITEM), intent(in)    :: food_item_eaten

    !> @param[in] time_step_model optional time step of the model, overrides
    !!            the value calculated from the spatial data.
    integer, optional, intent(in)   :: time_step_model
    !> @param[in] distance_food_item is the optional distance to the food item.
    real(SRP), optional, intent(in) :: distance_food_item
    !> @param[in] capture_prob is optional probability of capture of this
    !!            food item, overrides the value calculated from the
    !!            spatial data.
    real(SRP), optional, intent(in) :: capture_prob

    !> @param[out] is_captured optional capture flag, TRUE if the food item is
    !!            captured by the agent.
    logical, optional, intent(out)  :: is_captured

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME="(eat_food_item_do_this)"

    ! Local copy of optional food item capture probability.
    real(SRP) :: capture_prob_here, distance_food_item_here

    ! Local copy of the time step parameter.
    integer :: time_step_model_here

    !> ### Implementation details ###
    !> #### Preliminary checks ####
    !> This food item, if found in the perception object, should be available.
    !! If not, something wrong has occurred. We cannot process an food item
    !! that has been already eaten, so no increments are done and error is
    !! reported into the log.
    if (food_item_eaten%is_unavailable()) then
      call LOG_DBG( LTAG_WARN // PROCNAME // ", Cannot capture food item " // &
                "as it is not available (has been already eaten?). Check code.")
      return
    end if

    !> Check optional time step parameter.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> Check distance to the food item. If provided, use the override value,
    !! if not, calculate from the the agent and the food item spatial data.
    if (present(distance_food_item)) then
      distance_food_item_here = distance_food_item
    else
      distance_food_item_here = this_agent%distance(food_item_eaten)
    end if

    !> Check if food item capture probability is supplied.
    !! @note  If capture probability is supplied as a dummy parameter to
    !!        this procedure, it will override the intrinsic capture
    !!        probability that is based on the distance between the predator
    !!        agent and the food item it is about to eat.  This may be for
    !!        example necessary when a subjective expected motivational
    !!        expectancy is calculated, it can assume 100% probability and/or
    !!        weightings of the resulting motivation value(s).
    if (present(capture_prob)) then
      capture_prob_here = capture_prob
    else
      !> If the food item capture probability is not supplied, **calculate**
      !! it based on the current distance between the predator agent and this
      !! food item. (`commondata::food_item_capture_probability` is a baseline
      !! value at near-zero distance).
      capture_prob_here =                                                     &
        food_item_eaten%capture_probability(                                  &
                            distance = distance_food_item_here,               &
                            time_step_model = time_step_model_here           )
    end if

    !> #### Processing ####
    !> The probability that the food item is captured is stochastic and
    !! is normally below 100%. However while calculating the behaviour
    !! expectancies, the capture probability is set to 1.0 to make
    !! the internal subjective processing deterministic.
    !! Stochastic capture success is now determined by the
    !! `the_environment::food_item::capture_success()` function.
    !! @note  The distance to the food item `distance_food_item_here` is used
    !!        here not only to calculate the probability of food item capture
    !!        (above), but also the fast burst swimming cost of approaching
    !!        the food item that is about to be eaten.
    CAPTURED: if (food_item_eaten%capture_success(capture_prob_here)) then
      !> ##### Food item is captured #####
      !> The food item **is captured**, set the optional logical flag first.
      if (present(is_captured)) is_captured = .TRUE.
      !> The mass increment that this_agent gets from consuming this
      !! food item is defined by `the_body::condition::food_fitting`.
      !! @note Note that `the_body::condition::food_fitting` already subtracts
      !!       processing cost.
      this%mass_increment_from_food = this%mass_increment_from_food +         &
              this_agent%food_fitting(food_item_eaten, distance_food_item_here)
      this%stomach_increment_from_food = this%mass_increment_from_food
    else CAPTURED
      !> ##### Food item is not captured #####
      !> The food item is **not** captured, set the optional logical flag first.
      if (present(is_captured)) is_captured = .FALSE.
      !> If the food item is **not captured**, the agent has only to
      !! pay the energetic **processing cost** without food gain.
      !! The cost (mass decrement) is defined by
      !! `the_body::condition::food_process_cost()`. The stomach contents
      !! mass does not change in this case.
      this%mass_increment_from_food = this%mass_increment_from_food -       &
          this_agent%food_process_cost(food_item_eaten, distance_food_item_here)
      this%stomach_increment_from_food = 0.0_SRP
    end if CAPTURED

  end subroutine eat_food_item_do_this

  !-----------------------------------------------------------------------------
  !> `eat_food::motivations_expect()` is a subroutine (re)calculating
  !! motivations from fake expected perceptions following from the procedure
  !! `eat_food::do_this()` => `the_behaviour::eat_food_item_do_this()`.
  subroutine eat_food_item_motivations_expect(this,this_agent,food_item_eaten,&
                          time_step_model, distance_food_item, capture_prob,  &
                          rescale_max_motivation )

    !> @param [inout] this the self object.
    class(EAT_FOOD), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which does eat.
    class(APPRAISAL), intent(in)    :: this_agent
    !> @param[in] food_item_eaten is the food item object that is eaten.
    class(FOOD_ITEM), intent(in)    :: food_item_eaten

    !> @param [in] time_step_model optional time step of the model,
    !!             **overrides** the value calculated from the spatial data.
    integer, optional, intent(in)   :: time_step_model
    !> @param[in] distance_food_item optional distance to the food item,
    !!            **overrides** the value calculated from the spatial data.
    real(SRP), optional, intent(in) :: distance_food_item
    !> @param[in] capture_prob is optional probability of capture of this
    !!            food item, **overrides** the value calculated from the
    !!            spatial data.
    real(SRP), optional, intent(in) :: capture_prob

    !> @param[in] rescale_max_motivation maximum motivation value for
    !!            rescaling all motivational components for comparison
    !!            across all motivation and perceptual components and behaviour
    !!            units.
    real(SRP), optional, intent(in) :: rescale_max_motivation

    ! Local copy of optionals, capture probability, override distance.
    real(SRP) :: capture_prob_here, distance_food_item_here

    ! Local copy of optional model time step
    integer :: time_step_model_here

    ! Local variables
    real(SRP) :: max_motivation ! Local max. over all motivation components.

    !> ### Notable local parameters ###
    !> #### food_capture_prob ####
    !> `FOOD_CAPTURE_PROB` is the expected (subjective) food item capture
    !! probability parameter. We assume that the agent assumes 100% probability
    !! of capture of the food item.
    !! @note    The probability is here > 1.0 to make sure the procedure
    !!          is never stochastic (subjective prob=1) and the food item
    !!          is always caught (the stochastic function it is based on
    !!          random_value[0..1] < P ).
    real(SRP), parameter :: FOOD_CAPTURE_PROB = 1.1_SRP

    !> #### Stomach contents ####
    !> `stomach_increment_from_food_perc` is expected increment of the stomach
    !! contents that is used in the fake perception value in the neuronal
    !! response function.
    real(SRP) :: stomach_increment_from_food_perc

    !>  `stomach_overrride_perc` is the fake perception value for the
    !! stomach contents that goes into the neuronal response function.
    real(SRP) :: stomach_overrride_perc

    !> #### Body mass ####
    !> `mass_increment_from_food_perc` is the expected increment of the agent's
    !! body  mass that is used in the fake perception value in the neuronal
    !! response function.
    real(SRP) :: mass_increment_from_food_perc

    !>  `bodymass_override_perc` is the fake perception value for the body mass
    !! that goes into the neuronal response function.
    real(SRP) :: bodymass_override_perc

    !> #### energy_override_perc ####
    !>  `energy_override_perc` is the fake perception value that goes into
    !! the neuronal response function.
    real(SRP) :: energy_override_perc

    !> #### capture_prob_intrinsic ####
    !> `capture_prob_intrinsic` is the intrinsic probability of capture of the
    !! this food item. It is calculated using the
    !! `food_item::capture_probability()` method.
    real(SRP) :: capture_prob_intrinsic

    ! Local value of the current agent stomach contents.
    real(SRP) :: agent_stomach

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME =                                 &
                                          "(eat_food_item_motivations_expect)"

    !> ### Implementation details ###
    !> #### Preliminary steps and checks ####
    !> Check optional time step parameter. If not provided, use global
    !! parameter value from `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> Check distance to the food item. If provided, use the override value,
    !! if not, calculate from the the agent and the food item spatial data.
    if (present(distance_food_item)) then
      distance_food_item_here = distance_food_item
    else
      distance_food_item_here = this_agent%distance(food_item_eaten)
    end if

    !> Check if food item capture probability is supplied.
    !! If capture probability is supplied as a dummy parameter to
    !! this procedure, it will override the intrinsic capture
    !! probability that is based on the distance between the predator
    !! agent and the food item it is about to eat.  This may be for
    !! example necessary when a subjective expected motivational
    !! expectancy is calculated, it can assume 100% probability and/or
    !! weightings of the resulting motivation value(s).
    if (present(capture_prob)) then
      capture_prob_here = capture_prob
    else
      !> If the food item capture probability is not supplied, expectancy is
      !! based on a 100% capture probability.
      !! @warning Unlike the `eat_food::do_this()` procedure where the capture
      !!          probability is calculated from the true objective values,
      !!          the subjective expectancies are based by default on **100%
      !!          expected probability** of this food item capture.
      capture_prob_here = FOOD_CAPTURE_PROB
    end if

    !> The intrinsic (objective) probability of capture of this food item
    !! `capture_prob_intrinsic` is calculated using the
    !! `food_item::capture_probability()` method.
    capture_prob_intrinsic =                                                  &
      food_item_eaten%capture_probability(distance=distance_food_item_here,   &
                                          time_step_model=time_step_model_here)

    ! Produce diagnostic logger message in the @ref intro_debug_mode DEBUG mode.
    call LOG_DBG( LTAG_INFO // "Distance to the food item: "              //  &
                  TOSTR(distance_food_item_here)                          //  &
                  ", intrinsic capture probability: "                     //  &
                  TOSTR(capture_prob_intrinsic) // ".", PROCNAME, MODNAME )

    !> #### Main processing steps ####
    !> **First,** we use the **do**-procedure `eat_food::do_this()` =>
    !! `the_behaviour::eat_food_item_do_this()` to perform the behaviour desired
    !! without changing either the agent or its environment and here find
    !! **representation** values that later feed into the motivation
    !! **expectancy** functions.
    !! @note Note that the optional capture success flag is not used here
    !!       as what is important for expectancy calculation is the agent's
    !!       weight and stomach increments only.
    !! @note The dummy parameter `time_step_model` is not used here for
    !!       calculating the capture probability because a fixed fake value
    !!       of the later `FOOD_CAPTURE_PROB` is used.
    call this%do_this( this_agent = this_agent,                               &
                       food_item_eaten = food_item_eaten,                     &
                       distance_food_item = distance_food_item_here,          &
                       capture_prob = capture_prob_here )

    !> We then weight the subjective increments of the body mass and
    !! stomach content that are expected from eating this food item by the
    !! **intrinsic objective capture probability** `capture_prob_intrinsic`
    !! calculated for the current time step on the basis of the distance
    !! between the agent and the food item.
    stomach_increment_from_food_perc = this%stomach_increment_from_food *     &
                                                        capture_prob_intrinsic

    mass_increment_from_food_perc = this%mass_increment_from_food *           &
                                                        capture_prob_intrinsic

    ! Produce diagnostic logger message in the @ref intro_debug_mode DEBUG mode.
    call LOG_DBG( LTAG_INFO // "Raw stomach increment: "                  //  &
                  TOSTR(this%stomach_increment_from_food)                 //  &
                  ", raw mass increment: "                                //  &
                  TOSTR(this%mass_increment_from_food) // ".",                &
                  PROCNAME, MODNAME )
    call LOG_DBG( LTAG_INFO // "Subjective stomach increment (weighted "  //  &
                  "by intrinsic probability): "                           //  &
                  TOSTR(stomach_increment_from_food_perc)                 //  &
                  ", subjective mass increment (weighted by intrinsic "   //  &
                  "probability): " // TOSTR(mass_increment_from_food_perc),   &
                  PROCNAME, MODNAME )

    !> After this, it is possible to calculate the fake perceptions for the
    !! stomach contents (`stomach_overrride_perc`), body mass
    !! (`bodymass_override_perc`) and the energy reserves
    !! (`energy_override_perc`). These values are ready to be passed
    !! to the neuronal response function.
    agent_stomach = this_agent%get_stom_content()
    stomach_overrride_perc =                                                  &
                    max(  ZERO,                                               &
                          agent_stomach -                                     &
                              stomach_emptify_backend(agent_stomach) +        &
                              stomach_increment_from_food_perc )

    bodymass_override_perc =                                                  &
                    max(  ZERO,                                               &
                          this_agent%mass() -                                 &
                              this_agent%living_cost() +                      &
                              mass_increment_from_food_perc )

    energy_override_perc =                                                    &
          energy_reserve( bodymass_override_perc, this_agent%length() +       &
                            this_agent%len_incr(mass_increment_from_food_perc)&
                        )

    !> **Second,** we calculate motivation values resulting from the behaviour
    !! done (`eat_food::do_this()`) at the previous step: what would be the
    !! motivation values *if* the agent eats this food item? This is
    !! done by calling the **neuronal response function**,
    !! `percept_components_motiv::motivation_components()`
    !! method, for each of the motivational states with `perception_override_`
    !! dummy parameters overriding the default values:
    !!  - `perception_override_stomach`;
    !!  - `perception_override_bodymass`;
    !!  - `perception_override_energy`.
    !!  .
    !  @note  **Expectancy** assessment for **hunger** motivation, using
    !         `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in] for
    !         `this_agent` now.
    call this%expectancy%hunger%percept_component%motivation_components       &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL,         &
      param_gp_matrix_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL,        &
      param_gp_matrix_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL,     &
      param_gp_matrix_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL,               &
      param_gp_matrix_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL,             &
      param_gp_matrix_age      = AGE_HUNGER_GENOTYPE_NEURONAL,                &
      param_gp_matrix_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL,            &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,     &
      param_gerror_cv_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_age      = AGE_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,      &
      param_gerror_cv_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,  &
      !  Real agent perception components are now substituted by
      !  the *fake* values resulting from executing this
      !  behaviour (`do_this` method). This is repeated for all the motivatios:
      !  *hunger*, *passive avoidance,* *fear state* etc.
      perception_override_stomach = stomach_overrride_perc,                   &
      perception_override_bodymass = bodymass_override_perc,                  &
      perception_override_energy = energy_override_perc                       &
                                                                              )
      !> Real agent perception components are now substituted by
      !! the *fake* values resulting from executing this
      !! behaviour (`eat_food::do_this()` =>
      !! `the_behaviour::eat_food_item_do_this()` method). This is repeated
      !! for all the motivatios: *hunger*, *passive  avoidance,* *active
      !! avoidance* etc. These optional **override parameters** are
      !! substituted by the "fake" values:
      !!  - `perception_override_stomach`;
      !!  - `perception_override_bodymass`;
      !!  - `perception_override_energy`.
      !!  .

    !  @note  **Expectancy** assessment for **fear_defence** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%fear_defence%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL,    &
      param_gp_matrix_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_stomach = stomach_overrride_perc,                   &
      perception_override_bodymass = bodymass_override_perc,                  &
      perception_override_energy = energy_override_perc                       &
                                                                              )

    !  @note  **Expectancy** assessment for **reproduction** motivation, using
    !         `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in] for
    !         `this_agent` now.
    call this%expectancy%reproduction%percept_component%motivation_components  &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL,           &
      param_gp_matrix_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL,           &
      param_gp_matrix_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL,        &
      param_gp_matrix_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL,      &
      param_gp_matrix_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL,     &
      param_gp_matrix_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL,  &
      param_gp_matrix_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL,            &
      param_gp_matrix_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL,        &
      param_gp_matrix_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL,             &
      param_gp_matrix_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL,         &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !       by the **fake** values resulting from executing this
      !       behaviour (`do_this` method).
      perception_override_stomach = stomach_overrride_perc,                   &
      perception_override_bodymass = bodymass_override_perc,                  &
      perception_override_energy = energy_override_perc                       &
                                                                              )

    !> **Third,** From the perceptual components calculated at the previous
    !! step we can obtain the **primary** and **final motivation** values by
    !! weighed summing.
    if (present(rescale_max_motivation)) then
      !> Here we can use global maximum motivation across all behaviours and
      !! perceptual components if it is provided, for rescaling.
      max_motivation = rescale_max_motivation
    else
      !> Or can rescale using local maximum value for this behaviour only.
      max_motivation =  this%expectancy%max_perception()
    end if

    !> Transfer attention weights from the actor agent `this_agent` to the
    !! `this` behaviour component. So, we will now use the updated modulated
    !! attention weights of the agent rather than their default parameter
    !! values.
    call this%attention_transfer(this_agent)

    !> So the primary motivation values are calculated.
    call this%expectancy%motivation_primary_calc(max_motivation)

    !> Primary motivations are logged in the @ref intro_debug_mode "debug mode".
    call LOG_DBG( LTAG_INFO // "Primary motivations: " //                     &
                  "hunger: " //                                               &
                    TOSTR(this%expectancy%hunger%motivation_prim)  //         &
                  ", fear_defence: " //                                       &
                    TOSTR(this%expectancy%fear_defence%motivation_prim) //    &
                  ", reproduce: " //                                          &
                    TOSTR(this%expectancy%reproduction%motivation_prim),      &
                  PROCNAME, MODNAME )

    !> There is **no modulation** at this stage, so the final motivation
    !! values are the same as primary motivations.
    !! TODO: Should include developmental or other modulation? If yes, need to
    !!       separate genetic modulation component from
    !!       `motivation_modulation_genetic` into a procedure bound to
    !!       `MOTIVATIONS` with `this_agent` as actor.
    call this%expectancy%modulation_none()

    !> **Fourth,** Calculate the finally **expected arousal level for this
    !! behaviour**. As in the GOS, the overall arousal is the maximum value
    !! among all motivation components.
    this%arousal_expected = this%expectancy%max_final()

    !> Log also the final expectancy value in the @ref intro_debug_mode
    !! "debug mode".
    call LOG_DBG( LTAG_INFO // "Expectancy: " // TOSTR(this%arousal_expected),&
                  PROCNAME, MODNAME )

    !> Now as we know the expected arousal, we can choose the behaviour which
    !! would minimise this arousal level.

  end subroutine eat_food_item_motivations_expect

  !-----------------------------------------------------------------------------
  !> Execute this behaviour component "eat food item" by `this_agent` agent
  !! towards the `food_item_eaten`.
  !! @note The "do"-function does not change the state of the this_agent
  !!       or the the environment (the food item), the "execute" function
  !!       **does** change them.
  subroutine eat_food_item_do_execute(this, this_agent, food_item_eaten,      &
                                            food_resource_real, eat_is_success)
    !> @param [inout] this the self object.
    class(EAT_FOOD), intent(inout)  :: this
    !> @param[inout] this_agent is the actor agent which eats the food item.
    class(APPRAISAL), intent(inout) :: this_agent
    !> @param[inout] food_item_eaten is the food item object that is eaten.
    class(FOOD_ITEM), intent(inout) :: food_item_eaten
    !> @param[inout] food_resource_real The food resource we are eating the
    !!               food item in.
    !! @note We need to provide the food resource that the agent has perceived
    !!       the food items (using the `see_food` method) because the
    !!       food perception object contains **copies** of food items from the
    !!       physical resource. So we have to change the availability status of
    !!       the real physical resource items, not just items in the perception
    !!       object of the agent.
    class(FOOD_RESOURCE), intent(inout) :: food_resource_real
    !> @param[out] eat_is_success logical indicator showing if the food item
    !!             has actually been eaten (TRUE) or failed (FALSE).
    logical, optional, intent(out) :: eat_is_success

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(eat_food_item_do_execute)"

    ! Local logical flag setting stochastic food item capture success.
    logical :: is_captured

    ! Real id number of the food item iid
    integer :: food_item_real_iid

    !> ### Implementation details ###
    !> First, check if this food item is **not eaten** and this agent is
    !! **not dead**. It should normally the case. If not, may point to a bug.
    !  TODO: probably it is better to remove the logging code out here and only
    !        execute it once at start of behaviour procedure. Then can use pure
    !        elemental procedure.
    ERROR_NOFOOD_CHECK: if (food_item_eaten%is_unavailable()) then
      call LOG_DBG( LTAG_WARN // PROCNAME // ", Cannot capture food " //      &
           "item as it is not available (has been already eaten?). Check code.")
      return
    end if ERROR_NOFOOD_CHECK
    ERROR_DEAD_CHECK: if (this_agent%is_dead()) then
      call LOG_DBG( LTAG_WARN // PROCNAME // "Agent is dead, cannot " //      &
           "enter this subroutine. Check code.")
      return
    end if ERROR_DEAD_CHECK

    !> Now process the food item by `this_agent`.
    !> #### Step 1: do_this ####
    !> First, we use the intent-in do-procedure `eat_food::do_this()` =>
    !! `the_behaviour::eat_food_item_do_this()` to perform the behaviour
    !! desired and get the **expectations of fake perceptions** for GOS.
    !! As a result, we get `mass_increment_from_food` and
    !! `stomach_increment_from_food`.
    !! @note At this stage, the state of the food item is not changed.
    !!       Only the state of `this` behaviour changes, and it will
    !!       be later passed to modify the agent.
    !! @note `capture_prob` is not set here, so it is set to the true
    !!       objective value that depends on the distance between the
    !!       predator agent and the food item, see `capture_probability`
    !!       function bound to the `FOOD_ITEM` class.
    call this%do_this( this_agent = this_agent,                               &
                       food_item_eaten = food_item_eaten,                     &
                       is_captured = is_captured)

    !> Also, here set the optional output argument `eat_is_success` from the
    !! stochastic result (success/failed) of the foor item capture.
    if (present(eat_is_success)) eat_is_success = is_captured

    !> Also log the fake perceptions along with the agent's sex if running in
    !! the DEBUG mode.
    call LOG_DBG( "INFO: Body mass increment from food item: " //             &
                  TOSTR(this%mass_increment_from_food) //                     &
                  " with raw stomach content increment: " //                  &
                  TOSTR(this%stomach_increment_from_food) //                  &
                  "; food item processed has size: " //                       &
                  TOSTR(food_item_eaten%get_size()) //                        &
                  " and mass: " // TOSTR(food_item_eaten%get_mass()),         &
                  PROCNAME, MODNAME )

    !> #### Step 2: Change the agent ####
    !> Second, **change the agent's state** as a consequence of eating.
    !! (1) Grow the **body length** of the agent based on the mass increment
    !! from food.
    !! @warning Note that we increment the body length first, before
    !!          incrementing/growing the body **mass**. This is because the
    !!          body length increment uses the ratio of the food gain mass
    !!          to the agent's body mass. So incrementing the body mass itself
    !!          with the food gain should be done after the length is
    !!          processed, otherwise a wrong (mass+gain) value is used.
    call this_agent%len_grow(this%mass_increment_from_food)

    !> (2). Grow the **body mass** of the agent.
    !! @note  Note that `mass_increment_from_food` already has
    !!   the processing cost subtracted. Specifically, the
    !!   mass increment can be negative if the agent did not
    !!   catch the food item.
    !!   @note Note that even if `is_captured` is False, we do call
    !!         the mass and stomach increment procedures as in such a
    !!         case there is a mass cost that is still subtracted
    !!         (increment negative), and stomach increment is zero.
    call this_agent%mass_grow(this%mass_increment_from_food)

    !> (3). And increment the **stomach contents** of the agent using
    !! `condition::stomach_increment()`.
    call this_agent%stomach_increment(this%stomach_increment_from_food)

    !> (4). Update the energy reserves using the new currently updated mass
    !! and length by calling `condition::energy_update()`.
    call this_agent%energy_update()

    !> (5). Check if the agent is starved to death. If yes, the agent can
    !! die without going any further.
    !  TODO: decide should it be also checked here, after each behaviour
    !        that can have cost or only after executing the behaviour...
    !        the latter should avoid the overhead of multiple checking but
    !        gets unrealistic as a starved to death zombie agent could
    !        execute behaviours.
    if (this_agent%starved_death()) then
      call this_agent%dies()
      call LOG_DBG( LTAG_INFO // "The agent dies of starvation.",             &
                    PROCNAME, MODNAME )
      return
    end if

    !> #### Step 3: Change the environment ####
    !> Third, **change the state of the environment**. Disable the food
    !! item if it is eaten and not available any more. If the capture
    !! success if False, the item is not affected.
    CAPTURED: if (is_captured) then
      !> Set the eaten status to the food item in the perception object.
      call food_item_eaten%disappear()
      !> We also have to set the food item in the real food resource the
      !! same eaten/absent status because the perception object may operate
      !! on a **copy of the real food objects**.
      !! So we here first get the ID number of the food item.
      ! ERROR: Sometimes an out of bound id is produced for unknown reason:
      !        @verbatim
      !          Fortran runtime error: Index '88986' of dimension 1 of array
      !          'food_resource_real' above upper bound of 60000
      !        @endverbatim
      food_item_real_iid = food_item_eaten%get_iid()
      !> Second, set the food item in the food resource with the same iid
      !! the absent/eaten status.
      OUT_BOUND: if ( food_item_real_iid>food_resource_real%abundance() ) then
        call LOG_MSG( LTAG_ERROR // "ID of the food item " //                 &
                      TOSTR(food_item_real_iid) //                            &
                      " is outside of the valid range " //                    &
                      TOSTR(food_resource_real%abundance()) //                &
                      ", array size is " //                                   &
                      TOSTR( size(food_resource_real%food) ) // " in " //     &
                      PROCNAME // ". Cannot call (disappear) on this item!" )
      else OUT_BOUND
        call LOG_DBG( LTAG_INFO // "The food item " //                        &
                      TOSTR(food_item_real_iid) //                            &
                      " from the resource " //                                &
                      trim( food_resource_real%get_label() ) //               &
                      " (array size " //                                      &
                      TOSTR( size(food_resource_real%food) ) //               &
                      " = " //                                                &
                      TOSTR(food_resource_real%abundance()) //                &
                      " ) is marked eaten: (disappear) method called",        &
                      PROCNAME, MODNAME )
        call food_resource_real%food(food_item_real_iid)%disappear()

        !> Log the food item eaten in the @ref intro_debug_mode "debug mode".
        !! @warning This would result in huge amount of log writing that
        !!          significantly slows down execution!
        call LOG_DBG(LTAG_INFO // "Food item capture SUCCESS.",               &
                     PROCNAME, MODNAME)
        call LOG_DBG(LTAG_INFO //                                             &
              "Food item " // TOSTR(food_item_eaten%get_iid()) //             &
              " in the physical resource " //                                 &
              trim(food_resource_real%food_label) // " (real iid=" //         &
              TOSTR(food_resource_real%food(food_item_real_iid)%get_iid()) // &
              ") is now eaten and unavailable; size: " //                     &
              TOSTR(food_item_eaten%get_size()) //                            &
              ", mass: " // TOSTR(food_item_eaten%get_mass()) //              &
              " (physical resource size: " //                                 &
              TOSTR(food_resource_real%food(food_item_real_iid)%get_size()) //&
              ", mass: " //                                                   &
              TOSTR(food_resource_real%food(food_item_real_iid)%get_mass()) //&
              ")", PROCNAME, MODNAME)
        call LOG_DBG(LTAG_INFO //  "Check food item final status is " //      &
              TOSTR(food_item_eaten%is_available()) //                        &
              ", in the physical resource: " //                               &
              TOSTR(food_resource_real%food(food_item_real_iid)%              &
                                                              is_available()),&
              PROCNAME, MODNAME)
              food_item_real_iid = 0
      end if OUT_BOUND
    else CAPTURED
      call LOG_DBG( LTAG_INFO // "Food item capture FAILED for item " //      &
                    TOSTR(food_item_eaten%get_iid()), PROCNAME, MODNAME )
    end if CAPTURED

  end subroutine eat_food_item_do_execute

  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  !-----------------------------------------------------------------------------
  !> Initialise reproduce behaviour object.
  elemental subroutine reproduce_init_zero(this)
    class(REPRODUCE), intent(inout) :: this

    ! Local parameter for zero = 0.0
    real(SRP), parameter :: NULL_SRP = 0.0_SRP

    !> First init components from the base root class
    !! `the_behaviour::behaviour_base`:
    !! Mandatory label component that should be read-only.
    this%label = "REPRODUCE"
    !> The execution status is always FALSE, can be reset to TRUE only when
    !! the behaviour unit is called to execution.
    this%is_active = .FALSE.

    !> And the *expectancy* type components.
    call this%expectancy%init()
    !> And init the expected arousal data component.
    this%arousal_expected = NULL_SRP

    !> Second, init components of this specific behaviour (`REPRODUCE`)
    !! component extended class.
    !! @note Note that we initialise increments to 0.0, not MISSING as
    !!       increments will be later added. And several items can be added
    !!       consecutively.
    this%reprfact_decrement_testosterone = NULL_SRP
    this%reprfact_decrement_estrogen = NULL_SRP
    this%decrement_mass = NULL_SRP

  end subroutine reproduce_init_zero

  !-----------------------------------------------------------------------------
  !> Calculate the maximum number of possible reproductions for this agent.
  !! It is assumed that a male can potentially fertilise several females
  !! that are within its perception object (in proximity) during a single
  !! reproduction event. For females, this number if always one.
  function maximum_n_reproductions(this) result (max_num)
    class(APPRAISAL), intent(in) :: this
    !> @returns The maximum number of reproductions (successful fertilisations)
    !!          within the same reproduction event.
    integer :: max_num

    ! Local variables: number of conspecifics in the perception object.
    integer :: n_conspecifics_perception
    integer :: n_males_high_t_perception, n_females_perception

    ! Local counter
    integer :: i

    n_conspecifics_perception = this%perceive_consp%get_count()
    !> Initialise the number of same- and opposite-sex conspecifics
    !! (integer counters) to zero.
    n_males_high_t_perception = 0
    n_females_perception = 0

    !> ### Implementation details ###
    !> **First,** determine if there are any conspecifics in the perception, if
    !! there are no, reproduction is impossible. Return straight away with zero
    !! result in such a case.
    CHECK_IS_ALONE: if ( .NOT. this%has_consp() ) then
      max_num = 0
      return
    end if CHECK_IS_ALONE

    !> **Second,** check if this agent is **female**. If yes, only one
    !! fertilisation is possible, so return `max_num=1`.
    if (this%is_female()) then
      max_num = 1
      !> Exit from the procedure afterwards.
      return
    end if

    !> From now on, it is assumed the agent is male.
    !> **Third,** determine how many conspecific male agents in the perception
    !! object have testosterone level **higher** than this actor agent. These
    !! conspecific male agents can take part in the fertilisation. However, all
    !! male conspecifics with testosterone **lower** than in this agent are
    !! out-competed by this agent and the other high-testosterone males and
    !! would not be involved in reproduction.
    do concurrent (i=1:n_conspecifics_perception)
      if ( this%perceive_consp%conspecifics_seen(i)%is_male() ) then
        if ( this%testosterone_get() < 1. ) & !this%perceive_consp%conspecifics_seen(i)%testosterone  )
          n_males_high_t_perception = n_males_high_t_perception + 1
      else
        n_females_perception = n_females_perception + 1
      end if
    end do

    !> **Finally,** calculate the expected number of fertilised females, i.e.
    !! the number of reproductions for this agent assuming only this agent and
    !! all other male agents with the testosterone levels exceeding that in
    !! this agent can reproduce.
    max_num = floor( real(n_females_perception, SRP) /                        &
                     (1.0_SRP + real(n_males_high_t_perception, SRP)) )
    ! TODO: calculate on the basis of average testosterone in perception
    !       where only those > average can fertilise. Could result in zero
    !       for the this agent.

  end function maximum_n_reproductions

  !-----------------------------------------------------------------------------
  !> Do reproduce by `this_agent` (the actor agent) given the specific
  !! probability of successful reproduction. The probability of reproduction
  !! depends on the number of agents of the same and of the opposite sex
  !! within the visual range of the this agent weighted by the difference in
  !! the body mass between the actor agent and the average body mass of the
  !! other same-sex agents.
  !> The **main output** from this **do** procedure is the `this` behavioural
  !! unit object, namely its two components:
  !!  - this\%reprfact_decrement_testosterone
  !!  - this\%reprfact_decrement_estrogen
  !!  .
  subroutine reproduce_do_this(this, this_agent, p_reproduction, is_reproduce)
    !> @param[inout] this the object itself.
    class(REPRODUCE), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which does/does not reproduce.
    class(APPRAISAL), intent(in)    :: this_agent
    !> @param[in] p_reproduction optional probability of reproduction,
    !!            overrides the value calculated from `this_agent` data.
    real(SRP), optional, intent(in) :: p_reproduction
    !> @param[out] is_reproduce optional reproduction success flag, TRUE if
    !!             the reproduction is successfully done by the agent.
    logical, optional, intent(out) :: is_reproduce

    ! Local copies of optionals
    real(SRP) :: p_reproduction_here

    !> ### Implementation details ###
    !> Determine if the agent's hormonal system is ready for reproduction, that
    !! its current level of sex steroids @f$ \sigma_{i} @f$ exceeds the
    !! baseline (initially determined by the genome) @f$ \sigma_{0} @f$ by a
    !! factor @f$ \nu @f$ determined by the parameter
    !! commondata::sex_steroids_reproduction_threshold:
    !! @f[ \sigma_{i} > \nu \sigma_{0} . @f]
    !! This check is done by the the_body::is_ready_reproduce() function.
    !! - If the level of sex steroids is insufficient, reproduction is
    !!   impossible and the values of gonadal steroid decrements get are
    !!   zero. The  reproduction indicator `is_reproduce` if present, is
    !!   also set to FALSE and no further processing is then performed.
    !! .
    CHECK_MATURE: if ( .not. this_agent%is_ready_reproduce() ) then
      this%reprfact_decrement_testosterone = 0.0_SRP
      this%reprfact_decrement_estrogen = 0.0_SRP
      if (present(is_reproduce)) is_reproduce = .FALSE.
      return
    end if CHECK_MATURE

    !> Determine if there are any conspecifics in the perception, if there
    !! are no, reproduction is impossible. Return straight away with zero
    !! values of gonadal steroid decrements, as in the case of unsuccessful
    !! reproduction. The reproduction indicator `is_reproduce` if present,
    !! is also set to FALSE.
    CHECK_IS_ALONE: if ( .NOT. this_agent%has_consp() ) then
      this%reprfact_decrement_testosterone = 0.0_SRP
      this%reprfact_decrement_estrogen = 0.0_SRP
      if (present(is_reproduce)) is_reproduce = .FALSE.
      return
    end if CHECK_IS_ALONE

    !> Check optional probability of reproduction dummy parameter. If it is
    !! absent, use the value calculated from the `this_agent` agent's
    !! perception data calling `probability_reproduction()` method. This is
    !! the **upper limit** on the reproduction probability provided the actor
    !! agent has sufficient motivation and resources.
    if (present(p_reproduction)) then
      p_reproduction_here = p_reproduction
    else
      p_reproduction_here = this_agent%probability_reproduction()
    end if

    !> Then we call stochastic logical function `reproduction_success()`
    !! to determine the **actual outcome of reproduction**.
    if ( this_agent%reproduction_success() ) then
      !> If reproduction is **successful**, the reproductive factor gonadal
      !! steroid (hormonal) components
      !! reproduce::reprfact_decrement_testosterone` and
      !! reproduce::reprfact_decrement_estrogen data component are determined
      !! in sex  specific manner:
      !!  - in males testosterone is decreased,
      !!  - in females, estrogen is decreased.
      !!  .
      !! An additional condition is that the level of the gonadal hormones
      !! should not fall below the baseline level.
      !! Additionally, the cost of reproduction, body mass decrement
      !! `reproduce::decrement_mass`, is calculated and set using the
      !! `reproduction::reproduction_cost()` method.
      ! @note TODO: The other steroid (i.e. estrogen in males) can also
      !       be changed, or not?
      if ( this_agent%is_male() ) then
        this%reprfact_decrement_testosterone =                                &
                        this_agent%testosterone_get()*decrement_factor_fixed()
        if ( this_agent%testosterone_get() -                                  &
            this%reprfact_decrement_testosterone <                            &
            this_agent%testosterone_base_get() )                              &
                this%reprfact_decrement_testosterone =                        &
                          this_agent%testosterone_get() -                     &
                          this_agent%testosterone_base_get()
        this%decrement_mass = this_agent%reproduction_cost()
      else
        this%reprfact_decrement_estrogen =                                    &
                        this_agent%estrogen_get() * decrement_factor_fixed()
        if ( this_agent%estrogen_get() -                                      &
            this%reprfact_decrement_estrogen <                                &
            this_agent%estrogen_base_get() )                                  &
                this%reprfact_decrement_estrogen =                            &
                          this_agent%estrogen_get() -                         &
                          this_agent%estrogen_base_get()
        this%decrement_mass = this_agent%reproduction_cost()
      end if
      !> Also, if `is_reproduce` optional parameter is provided, set it to TRUE.
      if (present(is_reproduce)) is_reproduce = .TRUE.
    else
      !> If reproduction is **not successful**, reproduction factor decrements
      !! equal to zero are returned. The body mass decrement is equivalent to
      !! the reproduction cost of unsuccessful reproduction
      !! (`reproduction::reproduction_cost_unsuccess()`).
      this%reprfact_decrement_testosterone = 0.0_SRP
      this%reprfact_decrement_estrogen = 0.0_SRP
      this%decrement_mass = this_agent%reproduction_cost_unsuccess()
      !> Additionally, set `is_reproduce` to FALSE if it is provided.
      if (present(is_reproduce)) is_reproduce = .FALSE.
    end if

    contains
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !> Calculate the decrement factor for the gonadal steroids based
    !! reproductive factor.
    !! @note  This is based on fixed parameter value, trivial. A more complex
    !!        pattern can also be implemented.
    function decrement_factor_fixed() result (decrement)
      real(SRP) :: decrement
      !> `REPRFAC_DECREMENT_FACTOR_REPRODUCTION` is a fixed decrement factor
      !! for the gonadal steroid hormone based reproductive factor (reprfact).
      real(SRP), parameter :: REPRFAC_DECREMENT_FACTOR_REPRODUCTION = 0.3_SRP
      decrement = REPRFAC_DECREMENT_FACTOR_REPRODUCTION
    end function decrement_factor_fixed

  end subroutine reproduce_do_this

  !-----------------------------------------------------------------------------
  !> `reproduce::motivations_expect()` is a subroutine (re)calculating
  !! motivations from fake expected perceptions following from
  !! `reproduce::do_this()` =>  `the_behaviour::reproduce_do_this()` procedure.
  subroutine reproduce_motivations_expect(this, this_agent, time_step_model,  &
                           reprod_prob, non_stochastic, rescale_max_motivation)
    !> @param [inout] this the self object.
    class(REPRODUCE), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which does reproduce.
    class(APPRAISAL), intent(in)    :: this_agent
    !> @param [in] time_step_model optional time step of the model,
    !!             **overrides** the value calculated from the spatial data.
    integer, optional, intent(in)   :: time_step_model
    !> @param[in] reprod_prob is optional probability of reproduction for the
    !!            this actor agent, **overrides** the value calculated from the
    !!            agent data using the probability_reproduction() function.
    real(SRP), optional, intent(in) :: reprod_prob
    !> @param[in] non_stochastic is a logical flag that sets 100% probability
    !!            of reproduction. This parameter has precedence over the
    !!            `reprod_prob`.
    logical, optional, intent(in) :: non_stochastic
    !> @param[in] rescale_max_motivation maximum motivation value for
    !!            rescaling all motivational components for comparison
    !!            across all motivation and perceptual components and behaviour
    !!            units.
    real(SRP), optional, intent(in) :: rescale_max_motivation

    ! Local copy of optional model time step
    integer :: time_step_model_here

    ! Local copy of optionals, capture probability, override distance.
    real(SRP) :: reprod_prob_here

    ! Local variable
    real(SRP) :: max_motivation ! Local max. over all motivation components.

    !> ### Notable local parameters ###
    !> #### probability_reproduction_base_def ####
    !> `PROBABILITY_REPRODUCTION_BASE_DEF` is the expected (subjective)
    !! probability of reproduction; set as a parameter.
    !! @details We assume that the agent assumes 100% probability of
    !!          reproduction.
    !! @note    The probability is here > 1.0 to make sure the procedure
    !!          is never stochastic (subjective prob=1) and reproduction
    !!          always performed (it is based on random_value[0..1] < P ).
    !  TODO: we may also make the agent expectation
    !        of the probability the default fixed parameter
    !        value from `COMMONDATA` or genetically-selected value or
    !        calculated depending on the agent's current state.
    real(SRP), parameter :: PROBABILITY_REPRODUCTION_BASE_DEF = 1.1_SRP

    !> #### reproduction_prob_intrinsic ####
    !> `reproduction_prob_intrinsic` is the probability of reproduction that
    !! is intrinsic for the agent at the given conditions, calculated using the
    !! probability_reproduction() function.
    real(SRP) :: reproduction_prob_intrinsic

    !> #### reprfactor_percept ####
    !> `reprfactor_percept` is the value of the reproductive factor that
    !! goes as a fake perception value into the neuronal response function.
    !! This reproductive factor is determined in a sex specific way:
    !!  - `reprfact_decrement_testosterone` in males;
    !!  - `reprfact_decrement_estrogen` in females.
    !!  .
    real(SRP) :: reprfactor_percept

    !> #### body_mass_percept ####
    !> `body_mass_percept` is the "subjective" value of the energetic
    !! cost of reproduction that goes as a fake perception value into the
    !! neuronal response function. It is calculated via the
    !! `reproduction::reproduction_cost()` method.
    real(SRP) :: body_mass_percept

    !> #### energy_override_perc ####
    !>  `energy_override_perc` is the fake perception value for the energy
    !! reserves that goes into the neuronal response function.
    real(SRP) :: energy_override_perc

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME = "(reproduce_motivations_expect)"

    !> ### Implementation details ###
    !! First, calculate the intrinsic probability of reproduction for this
    !! actor agent using the probability_reproduction() method.
    reproduction_prob_intrinsic = this_agent%probability_reproduction()

    !> #### Check optional parameters ####
    !> Check optional time step parameter. If not provided, use global
    !! parameter value from `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> Check if the probability of reproduction is supplied.
    !! If the probability of reproduction is supplied as a dummy
    !! parameter to this procedure, it will override the intrinsic
    !! probability of reproduction for this actor agent that is
    !! calculated using the probability_reproduction() method.
    !! This may be for example necessary when a subjective motivational
    !! expectancy is calculated, it can assume 100% probability and/or
    !! weightings of the resulting motivation value(s).
    if (present(reprod_prob)) then
      reprod_prob_here = reprod_prob
    else
      !> If the probability of reproduction is not supplied, expectancy is
      !! based on the intrinsic probability_reproduction() value.
      reprod_prob_here = reproduction_prob_intrinsic
    end if

    !> If the `non_stochastic` dummy parameter is set to TRUE, the probability
    !! of reproduction is obtained from the `PROBABILITY_REPRODUCTION_BASE_DEF`
    !! local parameter that is 1.1. In such a case, it guarantees that the
    !! agent will always reproduce.
    !! @note Unlike the `reproduce::do_this()` procedure where the reproduction
    !!       probability is calculated from the true objective values, the
    !!       subjective expectancies are based by default on **100% expected
    !!       probability** of this agent reproduction.
    if (present(non_stochastic)) then
      if(non_stochastic) reprod_prob_here = PROBABILITY_REPRODUCTION_BASE_DEF
    end if

    call LOG_DBG( LTAG_INFO // "Probability of peprodiuction: " //            &
                  TOSTR(reprod_prob_here) // ", P intrinsic: " //             &
                  TOSTR(reproduction_prob_intrinsic), PROCNAME, MODNAME  )

    !> #### Main processing steps ####
    !> **First,** we use the **do**-procedure `reproduce::do_this()` =>
    !! `the_behaviour::reproduce_do_this()` to perform the behaviour desired
    !! without changing either the agent or its environment, obtaining  the
    !! **subjective** values of the `this` behaviour components that later feed
    !! into the motivation **expectancy** functions:
    !!  - `reprfact_decrement_testosterone`
    !!  - `reprfact_decrement_estrogen`
    !!  .
    call this%do_this( this_agent = this_agent,                               &
                       p_reproduction = reprod_prob_here )

    call LOG_DBG( LTAG_INFO // "Repfactor decrements: " //                    &
                  TOSTR(this%reprfact_decrement_testosterone) // "," //       &
                  TOSTR(this%reprfact_decrement_estrogen) //                  &
                  ", mass decrement: " // TOSTR(this%decrement_mass),         &
                  PROCNAME, MODNAME  )

    !> We then weight the expected subjective decrements of the reproductive
    !! factor components of he_neurobio::reproduce class, testosterone or
    !! estrogen, that are intrinsically expected for the actor agent by the
    !! **objective probability of reproduction** `reproduction_prob_intrinsic`
    !! (calculated for the current time step using the **intrinsic**
    !! the_neurobio::probability_reproduction() method).
    !> The reproductive factor `reprfactor_percept` that goes into the neuronal
    !! response function as a fake perception is based on gonadal steroid
    !! (hormonal) components: `reprfact_decrement_testosterone` and
    !! `reprfact_decrement_estrogen` in a sex specific manner:
    !!  - in males testosterone is weighted by `reproduction_prob_intrinsic`,
    !!  - in females, estrogen is weighted by `reproduction_prob_intrinsic`.
    !!  .
    !  TODO: The other steroid (i.e. estrogen in males) can also
    !        be changed, or not?
    if ( this_agent%is_male() ) then
      reprfactor_percept = this_agent%testosterone_get()                      &
                                  - this%reprfact_decrement_testosterone *    &
                                    reproduction_prob_intrinsic
    else
      reprfactor_percept = this_agent%estrogen_get()                          &
                                  - this%reprfact_decrement_estrogen *        &
                                                  reproduction_prob_intrinsic
    end if

    call LOG_DBG( LTAG_INFO // "Reproductive factor fake perception:" //      &
                  TOSTR(reprfactor_percept), PROCNAME, MODNAME )

    !> The same is done for the subjective assessment of the body mass cost
    !! of reproduction (`body_mass_percept`): it is weighted by the
    !! intrinsic probability of reproduction (`reproduction_prob_intrinsic`).
    body_mass_percept =                                                       &
                max( ZERO,                                                    &
                     this_agent%get_mass() - this_agent%living_cost() -       &
                        this%decrement_mass * reproduction_prob_intrinsic )

    !> At this point, therefore, the fake perception values for the
    !! reproductive factor (`reprfactor_percept`) and body mass
    !! (`body_mass_percept`) are known. Finally, calculate also the fake
    !! perception for the energy reserves (`energy_override_perc`) using the
    !! `the_body::energy_reserve()` procedure.
    energy_override_perc = energy_reserve( body_mass_percept,                 &
                                           this_agent%length() )

    !> **Second,** we calculate motivation values resulting from the behaviour
    !! done (`reproduce::do_this()` => `the_behaviour::reproduce_do_this()`) at
    !! the previous step: what would be the motivation values *if* the
    !! agent doe perform reproduction? Technically, this is done by
    !! calling the **neuronal response function**,
    !! `percept_components_motiv::motivation_components()`
    !! method, for each of the motivational states with `perception_override_`
    !! dummy parameters overriding the default values:
    !! `perception_override_reprfac` and also `perception_override_energy`.
    !  @note  **Expectancy** assessment for **hunger** motivation, using
    !         `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in] for
    !         `this_agent` now.
    call this%expectancy%hunger%percept_component%motivation_components       &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL,         &
      param_gp_matrix_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL,        &
      param_gp_matrix_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL,     &
      param_gp_matrix_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL,               &
      param_gp_matrix_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL,             &
      param_gp_matrix_age      = AGE_HUNGER_GENOTYPE_NEURONAL,                &
      param_gp_matrix_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL,            &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,     &
      param_gerror_cv_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_age      = AGE_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,      &
      param_gerror_cv_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,  &
      !  Real agent perception components are now substituted by the *fake*
      !  values resulting from executing this behaviour (`do_this` method).
      !  This is repeated for all the motivations: *hunger*,
      !  *passive avoidance,* *fear state* etc.
      perception_override_reprfac = reprfactor_percept,                       &
      perception_override_bodymass = body_mass_percept,                       &
      perception_override_energy = energy_override_perc                       &
                                                                              )
      !> Real agent perception components are now substituted by the *fake*
      !! values resulting from executing this behaviour (`reproduce::do_this()`
      !! => `the_behaviour::reproduce_do_this()` method). This is repeated for
      !! all the motivations: *hunger*, *passive avoidance,* *active
      !! avoidance* etc. These optional **override parameters** are
      !! substituted by the "fake" values:
      !!  - `perception_override_reprfac`;
      !!  - `perception_override_bodymass`.
      !!  .

    !  @note  **Expectancy** assessment for **fear_defence** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%fear_defence%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL,    &
      param_gp_matrix_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_reprfac = reprfactor_percept,                       &
      perception_override_bodymass = body_mass_percept,                       &
      perception_override_energy = energy_override_perc                       &
                                                                              )

    !  @note  **Expectancy** assessment for **reproduction** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%reproduction%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL,    &
      param_gp_matrix_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_reprfac = reprfactor_percept,                       &
      perception_override_bodymass = body_mass_percept,                       &
      perception_override_energy = energy_override_perc                       &
                                                                              )

    !> **Third,** From the perceptual components calculated at the previous
    !! step we can obtain the **primary** and **final motivation** values by
    !! weighed summing.
    if (present(rescale_max_motivation)) then
      !> Here we can use global maximum motivation across all behaviours and
      !! perceptual components if it is provided, for rescaling.
      max_motivation = rescale_max_motivation
    else
      !> Or can rescale using local maximum value for this behaviour only.
      max_motivation =  this%expectancy%max_perception()
    end if

    !> Transfer attention weights from the actor agent `this_agent` to the
    !! `this` behaviour component. So, we will now use the updated modulated
    !! attention weights of the agent rather than their default parameter
    !! values.
    call this%attention_transfer(this_agent)

    !> So the primary motivation values are calculated.
    call this%expectancy%motivation_primary_calc(max_motivation)

    !> Primary motivations are logged in the @ref intro_debug_mode "debug mode".
    call LOG_DBG( LTAG_INFO // "Primary motivations: " //                     &
                  "hunger: " //                                               &
                    TOSTR(this%expectancy%hunger%motivation_prim)  //         &
                  ", fear_defence: " //                                       &
                    TOSTR(this%expectancy%fear_defence%motivation_prim) //    &
                  ", reproduce: " //                                          &
                    TOSTR(this%expectancy%reproduction%motivation_prim),      &
                  PROCNAME, MODNAME )

    !> There is **no modulation** at this stage, so the final motivation
    !! values are the same as primary motivations.
    !! TODO: Should include developmental or other modulation? If yes, need to
    !!       separate genetic modulation component from
    !!       `motivation_modulation_genetic` into a procedure bound to
    !!       `MOTIVATIONS` with `this_agent` as actor.
    call this%expectancy%modulation_none()

    !> **Fourth,** Calculate the finally **expected arousal level for this
    !! behaviour**. As in the GOS, the overall arousal is the maximum value
    !! among all motivation components.
    this%arousal_expected = this%expectancy%max_final()

    !> Log also the final expectancy value in the @ref intro_debug_mode
    !! "debug mode".
    call LOG_DBG( LTAG_INFO // "Expectancy: " // TOSTR(this%arousal_expected),&
                  PROCNAME, MODNAME )

    !> Now as we know the expected arousal, we can choose the behaviour which
    !! would minimise this arousal level.

  end subroutine reproduce_motivations_expect

  !-----------------------------------------------------------------------------
  !> Execute this behaviour component "reproduce" by the `this_agent` agent.
  subroutine reproduce_do_execute(this, this_agent)
    class(REPRODUCE), intent(inout) :: this
    !> @param[inout] this_agent is the actor agent which reproduces.
    class(APPRAISAL), intent(inout) :: this_agent

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(reproduce_do_execute)"

    ! Local logical flag setting actual stochastic reproduction success.
    logical :: is_reproduce

    !> #### Notable local variables ####
    !> *body_mass_after* is the updated body mass of the agent excluding the
    !! cost of reproduction.
    real(SRP) :: body_mass_after

    !> ### Implementation details ###
    !> #### Basic checks ####
    !> First, check if there are **any conspecifics** in the perception object
    !! of the agent and this agent is **not dead**. It should normally the
    !! case. If not, may point to a bug.
    if (.not. this_agent%has_consp()) then
      call LOG_DBG( LTAG_WARN // PROCNAME // " Cannot reproduce as " //       &
                    "there are no conspecifics in perception. Check code.")
      return
    end if
    if (this_agent%is_dead()) then
      call LOG_DBG( LTAG_WARN // PROCNAME // " Agent is dead, cannot " //     &
           "enter this subroutine. Check code.")
      return
    end if

    !> #### Check the agent condition ####
    !> Calculate the updated body mass of the agent after reproduction
    !! `body_mass_after`. It is obtained by subtracting the cost of
    !! reproduction from the current body mass of the agent. The cost of
    !! reproduction is calculated using the function
    !! `reproduction::reproduction_cost()`
    !! (=> `the_body::reproduction_cost_energy()`). Therefore it does not
    !! necessarily coincide with the subjective cost of reproduction that is
    !! kept in the `the_behaviour::reproduce` class.
    body_mass_after = this_agent%get_mass() - this_agent%reproduction_cost()

    !> Additionally, check if the energy reserves of the agent and the body
    !! mass are enough for reproduction. That is, if the agent survives
    !! following the reproduction and does not get starved to death. The check
    !! is done using the `the_body::is_starved()` function in the named if
    !! block `CHECK_STARVED_AFTER`.
    CHECK_STARVED_AFTER: if ( is_starved( body_mass_after,                    &
                     this_agent%stomach_content_mass,                         &
                     this_agent%body_mass_birth,                              &
                     this_agent%body_mass_maximum,                            &
                     this_agent%energy_current,                               &
                     this_agent%energy_maximum) ) then
      !> - If the condition of the agent is insufficient for reproduction, it
      !!   is assumed that the agent **has attempted** reproduction but was not
      !!   successful. Then, the `::reproduction_unsuccessful_cost_subtract()`
      !!   procedure is called to subtract some small cost if unsuccessful
      !!   reproduction.
      call reproduction_unsuccessful_cost_subtract()
      !> - Following this, exit and **return** back from this procedure.
      !! .
      return
    end if CHECK_STARVED_AFTER

    !> #### Step 1: do_this ####
    !> First, we use the intent-in do-procedure `reproduce::do_this()` =>
    !! `the_behaviour::reproduce_do_this()` to perform the behaviour
    !! desired and get the **expectations of fake perceptions** for GOS:
    !!  - this\%reprfact_decrement_testosterone
    !!  - this\%reprfact_decrement_estrogen.
    !!  .
    !! At this stage, the state of the agent is not changed. Only the state of
    !! `this` behaviour changes, and it will be later passed to modify the
    !! agent. The `do_this` procedure also returns the stochastic status
    !! of the reproduction event `is_reproduce` is TRUE if the reproduction
    !! event was successful.
    call this%do_this( this_agent = this_agent,                               &
                       p_reproduction=this_agent%probability_reproduction(),  &
                       is_reproduce = is_reproduce)

    !> Also log the fake perceptions along with the agent's sex if running in
    !! the DEBUG mode.
    call LOG_DBG( LTAG_INFO // "Reproduction attempted, success is: " //      &
                  TOSTR(is_reproduce) //                                      &
                  " for agent " // this_agent%individ_label() )
    call LOG_DBG( LTAG_INFO // "Agent sex is: " // this_agent%label_sex() //  &
                  "(is male: " // TOSTR(this_agent%is_male()) // "); " //     &
                  "testosterone decrement: " //                               &
                  TOSTR(this%reprfact_decrement_testosterone) //              &
                  "; estrogen decrement: " //                                 &
                  TOSTR(this%reprfact_decrement_estrogen), PROCNAME, MODNAME )

    !> #### Step 2: Change the agent ####
    !> ##### Check reproduction success #####
    !> Second, **change the agent's state** as a consequence of reproduction.
    !> Check if reproduction event was stochastically successful. If the
    !! reproduction event was not successful (`is_reproduce` is FALSE), the
    !! `::reproduction_unsuccessful_cost_subtract()` procedure is called to
    !! subtract some small cost of unsuccessful reproduction.
    if ( .not. is_reproduce ) then
      call reproduction_unsuccessful_cost_subtract()
      !> Following this, exit and **return** back from this procedure.
      return
    end if

    !> ##### Process reproducing agent #####
    !> From now on it is assumed that the reproduction event was stochastically
    !! **successful**.
    !> (A) Update the number of successful reproductions and the number of
    !! offspring that result from this reproduction, for this agent, by the
    !! default number (=1) calling `reproduction::reproductions_increment()`.
    call this_agent%reproductions_increment(add_repr=1)

    !> (B) Decrease the sex steroids level following the reproduction. This
    !! is different in males and females: testosterone is decreased in males
    !! and estrogen, in females. An additional condition is that the
    !! level of gonadal steroids could not fall to less than the baseline.
    if ( this_agent%is_male() ) then
      call this_agent%testosterone_set(                                       &
              value_set =  max( this_agent%testosterone_base_get(),           &
                                this_agent%testosterone_get() -               &
                                    this%reprfact_decrement_testosterone ),   &
              update_history=.TRUE. )
    else
      call this_agent%estrogen_set(                                           &
              value_set = max(  this_agent%estrogen_base_get(),               &
                                this_agent%estrogen_get() -                   &
                                    this%reprfact_decrement_estrogen ),       &
              update_history=.TRUE. )
    end if

    !> (C) Decrement the body mass as a consequence of reproduction. This
    !! body mass decrement constitutes the energetic cost of reproduction.
    !! The updated body mass (after subtraction of the cost) has already been
    !! calculated as `body_mass_after`.
    call this_agent%set_mass( value_set = body_mass_after,                    &
                              update_history = .TRUE. )
    !> Additionally, also call the `the_body::condition::set_length()` method
    !! to update the body length history stack. However, the value_set
    !! parameter here is just the current value. This fake re-setting of the
    !! body length is done to keep both mass and length synchronised in their
    !! history stack arrays (there is no procedure for only updating history).
    call this_agent%set_length( value_set = this_agent%get_length(),          &
                                update_history = .TRUE. )

    !> After resetting the body mass, update energy reserves of the agent, that
    !! depend on both the length and the mass.
    call this_agent%energy_update()

    !> (D). Check if the agent is starved to death. If yes, the agent can
    !! die without going any further.
    if (this_agent%starved_death()) call this_agent%dies()

    !> #### Step 3: Change the environment ####
    !> Reproduction of the agent does not affect the environmental objects.
    !! TODO: add method to do actual reproduction crossover mate choice and
    !! produce eggs

    contains
      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !> Process the costs of unsuccessful reproduction. Reproduction can
      !! be unsuccessful for various reasons: insufficient reserves
      !! (reproduction results in starvation death) or stochastic no success.
      subroutine reproduction_unsuccessful_cost_subtract()

        !> Unsuccessful reproduction attempt results in a cost,
        !! in terms of the body mass, that is a fraction of the normal cost
        !! of reproduction: the fraction is defined by the parameter
        !! `commondata::reproduction_cost_unsuccess` in `COMMONDATA`.
        !! The cost of unsuccessful reproduction is calculated by the
        !! function `reproduction::reproduction_cost_unsuccess()`.
        !> The body mass of the agent is then reduced to take this fraction of
        !! the full cost of reproduction. This updated value is saved into
        !! the body mass history stack (`update_history` parameter is `TRUE`).
        call this_agent%set_mass( value_set =  this_agent%get_mass() -        &
                                    this_agent%reproduction_cost_unsuccess(), &
                                  update_history = .TRUE. )
        !> Body length is also saved to history to make the mass and length
        !! history stack arrays synchronised.
        call this_agent%set_length( value_set = this_agent%get_length(),      &
                                    update_history = .TRUE. )
        !> The energy reserve of the agent, depending on both the length
        !! and the mass, is updated.
        call this_agent%energy_update()

      end subroutine reproduction_unsuccessful_cost_subtract

  end subroutine reproduce_do_execute

  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  !-----------------------------------------------------------------------------
  !> The "do" procedure component of the behaviour element performs the
  !! behaviour without affecting the actor agent (the_agent) and the world
  !! (here food_item_eaten) which have intent(in), so it only can change
  !! the internal representation of the behaviour (the type to which this
  !! procedure is bound to, here `WALK_RANDOM`).
  subroutine walk_random_do_this(this, this_agent, distance, distance_cv,     &
                    predict_window_pred, predict_window_food, time_step_model)
    class(WALK_RANDOM), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which eats the food item.
    class(APPRAISAL), intent(in)    :: this_agent
    !> @param[in] distance is an optional walk distance. If stochastic Gaussian
    !!            walk is set, this value defines the average distance.
    !!            @note Even though the walk distance is internally defined in
    !!                  terms of the agent's body length, this parameter
    !!                  defines the **absolute distance** in cm.
    real(SRP), optional, intent(in) :: distance
    !> @param[in] distance_cv is an optional coefficient of variation for the
    !!            random walk distance. If absent, non-stochastic walk step
    !!            size is used.
    real(SRP), optional, intent(in) :: distance_cv
    !> @param[in] predict_window_pred the size of the prediction window, i.e.
    !!            how many steps back in memory are used to calculate the
    !!            predicted general predation risk. This parameter is limited
    !!            by the maximum
    !!            commondata::history_size_perception value of the perception
    !!            memory history size.
    integer, optional, intent(in) :: predict_window_pred
    !> @param[in] predict_window_food the size of the prediction window, i.e.
    !!            how many steps back in memory are used to calculate the
    !!            predicted food gain. This parameter is limited by the maximum
    !!            commondata::history_size_perception value of the perception
    !!            memory history size.
    integer, optional, intent(in) :: predict_window_food
    !> @param[in] time_step_model optional time step of the model, overrides
    !!            the value calculated from the spatial data.
    integer, optional, intent(in)   :: time_step_model

    ! Local copies of optionals.
    integer :: time_step_model_here, predict_window_food_here,                &
               predict_window_pred_here

    ! mean_n_food_memory_old, mean_n_food_memory_new** are the average
    ! numbers of food items in the past memory window, the "older" and
    ! "newer" parts that are used to calculate the "older"
    ! and "newer" values of food availability retrieved from the
    ! perception memory. Used in calculation of the the_behaviour::hope
    ! function.
    real(SRP) :: mean_n_food_memory_old, mean_n_food_memory_new

    ! **mean_size_food_memory_old, mean_size_food_memory_new** are the
    ! average sizes of food items in the past memory window, the "older"
    ! and "newer" parts that are used to calculate the "older"
    ! @f$ \overline{f_1} @f$ and "newer" @f$ \overline{f_2} @f$
    ! values of food availability retrieved from the perception memory.
    ! Used in calculation of the the_behaviour::hope function.
    real(SRP) :: mean_size_food_memory_old, mean_size_food_memory_new

    ! **food_gain_memory_old, food_gain_memory_new** are the "older"
    ! @f$ \overline{f_1} @f$ and "newer" @f$ \overline{f_2} @f$
    ! values of food gain retrieved from the perception memory.
    ! Used in calculation of the the_behaviour::hope function.
    real(SRP) :: food_gain_memory_old, food_gain_memory_new

    ! **food_gain_memory_baseline** is the baseline value of the food gain
    ! retrieved from the memory, that is used to calculate the actual
    ! food gain expectancy value calculated from the hope function.
    real(SRP) :: food_gain_memory_baseline

    ! Baseline distance for random walk.
    real(SRP) :: distance_baseline

    ! **WEIGHT_DIRECT**  is the relative weight  given to the immediate
    ! perception of predators over the predators counts in the memory stack.
    ! Obtained from global parameters
    ! (`commondata::predation_risk_weight_immediate`).
    real(SRP), parameter :: WEIGHT_DIRECT = PREDATION_RISK_WEIGHT_IMMEDIATE

    ! **mean_n_pred_memory_old, mean_n_pred_memory_new** are the average
    ! numbers of predators in the past perception memory window.
    real(SRP) :: mean_n_pred_memory_old, mean_n_pred_memory_new

    ! **pred_dir_current** and **pred_current** are the current estimates
    ! of the direct and general predation risk.
    real(SRP) :: pred_dir_current, pred_current

    !> ### Implementation details ###
    !> #### Checks and preparations ####
    !> Check optional time step parameter. If unset, use global
    !! `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> Check optional parameter for the general predation risk perception
    !! memory window. If the `predict_window_pred` dummy parameter is not
    !! provided, its default value is the proportion of the whole perceptual
    !! memory window defined by commondata::history_perception_window_pred.
    !! Thus, only the latest part of the memory is used for the prediction
    !! of the future predation risk.
    if (present(predict_window_pred)) then
      predict_window_pred_here= predict_window_pred
    else
      predict_window_pred_here = floor( HISTORY_SIZE_PERCEPTION *             &
                                        HISTORY_PERCEPTION_WINDOW_PRED )
    end if

    !> Check optional parameter for the food perception memory window. If
    !! the `predict_window_food` dummy parameter is not provided, its default
    !! value is the proportion of the whole perceptual memory window defined
    !! by commondata::history_perception_window_food. Thus, only the
    !! latest part of the memory is used for the prediction of the future
    !! food gain.
    if (present(predict_window_food)) then
      predict_window_food_here = predict_window_food
    else
      predict_window_food_here = floor( HISTORY_SIZE_PERCEPTION *             &
                                        HISTORY_PERCEPTION_WINDOW_FOOD )
    end if

    !> #### Calculate the distance of swimming ####
    !> The normal locomotion distance is fixed to the fraction of the agent
    !! current body length set by the parameter
    !! commondata::walk_random_distance_default_factor. This is a baseline
    !! value that can serve as the mean in case of stochastic walks
    !! (the_environment::spatial_moving::rwalk()) or as the actual value in
    !! case of deterministic walks.
    !!
    !! The walk distance @f$ D_{rw} = L \varrho @f$ where @f$ L @f$ is the
    !! agent's body length and  @f$ \varrho @f$ is the parameter factor
    !! commondata::walk_random_distance_default_factor.
    !!
    !! However, if the walk distance is provided as an optional parameter
    !! `distance` to this procedure, this provided value is used as the
    !! baseline distance instead. This allows to easily implement several
    !! types of walks, e.g. "long" (migration-like) and short (local).
    if (present(distance)) then
      distance_baseline = distance
    else
      distance_baseline = this_agent%get_length() *                           &
                                      WALK_RANDOM_DISTANCE_DEFAULT_FACTOR
    end if

    !> This baseline distance value @f$ D_{rw} @f$ is saved into the `this`
    !! behaviour data component \%distance.
    this%distance = distance_baseline

    !> - If the `distance_cv` optional dummy parameter is set to a
    !!   non-zero value (> commondata::tolerance_high_def_srp), the the walk
    !!   distance is *stochastic* with the mean equal to the above baseline
    !!   value and the coefficient of variation set by the \%distance_cv
    !!   data component of the `this` walk object, that is in turn equal to
    !!   the `distance_cv` parameter.
    if (present(distance_cv)) then
      if ( distance_cv > TOLERANCE_HIGH_DEF_SRP ) then
        this%distance_cv = distance_cv
        !> The \%distance is then reset to a Gaussian value, creating an
        !! error/uncertainty in the expectancy.
        this%distance = RNORM( distance_baseline,                             &
                               cv2variance(distance_cv, distance_baseline) )
      else
        this%distance_cv = 0.0_SRP
      end if
    !> - If `distance_cv` parameter is absent or is explicitly set to zero, the
    !!   walk distance is deterministic with the value equal to the baseline.
    !!   Also, the \%distance_c data component is 0.0 for non-stochastic
    !!   distances.
    !! .
    !! This allows to implement uncertainty in the walk distance depending on
    !! different factors, such as the arousal or hormone level.
    else
      this%distance_cv = 0.0_SRP
    end if

    !> #### Calculate expected cost of the swimming ####
    !> The expected cost of swimming in the random walk depends on the walk
    !! distance and is calculated using the the_body::condition::cost_swim()
    !! assuming *laminar* flow (laminar flow is due to normal relatively slow
    !! swimming pattern).
    this%expected_cost_moving =                                               &
              this_agent%cost_swim( distance=this%distance,                   &
                                    exponent=SWIMMING_COST_EXPONENT_LAMINAR )

    !> #### Calculate expected food item perception ####
    !> *Food item* perception expected after a random walk is calculated
    !! using the the_behaviour::hope() function mechanism.
    !!
    !! First, average number of food items in the "older" and "newer" parts of
    !! the memory is calculated using the
    !! the_neurobio::memory_perceptual::get_food_mean_n_split() procedure.
    !! (Note that the `split_val` parameter to this procedure is not
    !! provided so the default 1/2 split is used.)
    call this_agent%memory_stack%get_food_mean_n_split(                       &
                                        window = predict_window_food_here,    &
                                        older = mean_n_food_memory_old,       &
                                        newer = mean_n_food_memory_new )

    !> Second, the expected number of food items following the walk
    !! (\%expected_food_dir) is calculated based on the the_behaviour::hope()
    !! function mechanism. Here, the baseline value @f$ f_0 @f$ is
    !! the current number of food items in the food perception object of the
    !! actor agent, and the historical ratio @f$ \varrho @f$ is calculated
    !! as the mean number of food items in the "older" to "newer" memory
    !! parts: @f[ \varrho = \frac{\overline{n_2}}{\overline{n_1}} . @f]
    !! The grid arrays for the hope function are defined by the
    !! obtained from commondata::walk_random_food_hope_abscissa and
    !! commondata::walk_random_food_hope_ordinate  parameter arrays.
    this%expected_food_dir =                                                  &
                      hope( baseline = real(                                  &
                                this_agent%perceive_food%get_count(),SRP ),   &
                            memory_old = mean_n_food_memory_old,              &
                            memory_new = mean_n_food_memory_new,              &
                            raw_grid_x = WALK_RANDOM_FOOD_HOPE_ABSCISSA,      &
                            raw_grid_y = WALK_RANDOM_FOOD_HOPE_ORDINATE  )

    !> #### Calculate expected food gain ####
    !> The expected food gain is calculated differently depending on the
    !! mean distance of the random walk.
    !! - If the agent currently has any food items in perception, the short
    !!   walk is defined as a walk with the distance not exceeding
    !!   commondata::walk_random_food_gain_hope units of the average
    !!   to the food items in perception.
    !! - If there is no food in the perception object, a walk is "short" if
    !!   it does not exceed commondata::walk_random_food_gain_hope_agentl
    !!   units of the agent body length.
    !! .
    !> ##### Short walks #####
    !> For relatively short walks, the expected food gain is based on the
    !! currently available value.
    SELECT_DIST_FOOD: if ( ( this_agent%has_food() .and.                      &
                          this%distance < WALK_RANDOM_FOOD_GAIN_HOPE *        &
                          this_agent%perceive_food%get_meandist() )           &
                      .or.                                                    &
                      ( .not. this_agent%has_food() .and.                     &
                          this%distance < WALK_RANDOM_FOOD_GAIN_HOPE_AGENTL * &
                          this_agent%get_length() )  ) then
      !> The expected food gain is equal to the average mass of the food item
      !! in the latest `predict_window_food_here` steps of the memory stack,
      !! weighted by the average number of food items in the same width latest
      !! memory if this number is less than 1 or 1 (i.e. unweighted) if their
      !! number is higher.
      !  @f[
      !   \left\{\begin{matrix}
      !   F_{exp}=\overline{f(m)} \cdot \overline{n(m)}, & \overline{n(m)}<1; \\
      !   F_{exp}=\overline{f(m)}, & \overline{n(m)} \geq 1
      !   \end{matrix}\right.
      !  @f]
      !> @image html img_doxygen_walk_rand_formula_1.svg
      !> @image latex img_doxygen_walk_rand_formula_1.eps "" width=14cm
      !! where @f$ \overline{f(m)} @f$ is the average mass of the food items
      !! and @f$ \overline{n(m)} @f$ is the average number of food items
      !! in the @f$ m @f$ latest steps of the perceptual memory stack.
      !!
      !! The averages are calculated with
      !! the_neurobio::memory_perceptual::get_food_mean_size() and
      !! the_neurobio::memory_perceptual::get_food_mean_n(). The average mass of
      !! the food items is calculated from their average size using the
      !! the_environment::size2mass_food() function.
      !!
      !! Thus, if the agent had previously some relatively poor perceptual
      !! history of encountering food items, so that the average number of food
      !! items is fractional < 1 (e.g. average number 0.5, meaning that it has
      !! seen a single food item approximately every other time step), the
      !! expected food is weighted by this fraction (0.5). If, on the other
      !! hand, the agent had several food items at each time step previously,
      !! the average food item size is unweighted (weight=1.0). This conditional
      !! weighting reflects the fact that it is not possible to eat more than
      !! one food item at a time in this model version.
      !!
      !! This expected food gain is then weighted by the subjective probability
      !! of food item capture that is calculated based on the memory
      !! the_neurobio::perception::food_probability_capture_subjective().
      this%expected_food_gain =                                               &
          size2mass_food(                                                     &
              this_agent%memory_stack%get_food_mean_size(                     &
                  predict_window_food_here)) *                                &
          within(                                                             &
              this_agent%memory_stack%get_food_mean_n(                        &
                                                    predict_window_food_here),&
              0.0_SRP, 1.0_SRP ) *                                            &
          this_agent%food_probability_capture_subjective(                     &
                              predict_window_food_here, time_step_model_here )

    !> ##### Long walks #####
    !> For relatively long walks, the expected food gain is based on the
    !! the_behaviour::hope() function.
    else SELECT_DIST_FOOD

      !> First, average size of food items in the "older" and "newer" parts of
      !! the memory is calculated using the
      !! the_neurobio::memory_perceptual::get_food_mean_size_split() procedure.
      !! (Note that the `split_val` parameter to this procedure is not
      !! provided so the default 1/2 split is used.)
      call this_agent%memory_stack%get_food_mean_size_split(                  &
                                        window = predict_window_food_here,    &
                                        older = mean_size_food_memory_old,    &
                                        newer = mean_size_food_memory_new )

      !> Second, the values of the "old" and "new" *food gain* used to calculate
      !! the expectations are obtained by weighting the respective average mass
      !! of the food item by the average number of food items if this number is
      !! less than 1 or 1 (i.e. unweighted) if their average number is higher.
      !> @image html img_doxygen_migrate_formula_1.svg
      !! @image latex img_doxygen_migrate_formula_1.eps "" width=14cm
      !> where @f$ \overline{m_1} @f$ is the average mass of the food items
      !! and @f$ \overline{n_1} @f$ is the average number of food items
      !! in the "older" half of the perceptual memory stack and
      !! @f$ \overline{m_2} @f$ is the average mass of the food items
      !! and @f$ \overline{n_2} @f$ is the average number of food items
      !! in the "newer" half of the memory stack.
      !!
      !! Thus, if the agent had some relatively poor perceptual history of
      !! encountering food items, so that the average *number* of  food items
      !! is fractional < 1 (e.g. average number 0.5, meaning that it has seen a
      !! single food item approximately every other time step), the food gain is
      !! weighted by this fraction (0.5). If, on the other hand, the agent had
      !! more than one food items at each time step previously, the average food
      !! item size is unweighted (weight=1.0). This conditional weighting
      !! reflects the fact that it is not possible to eat more than
      !! one food item at a time in this model version.
      !! @note A similar expectancy assessment mechanism is used in the
      !!       assessment of the food gain expectancy for the
      !!       the_behaviour::migrate behaviour component
      !!       the_behaviour::migrate_do_this().
      food_gain_memory_old = size2mass_food(mean_size_food_memory_old) *      &
                              within( mean_n_food_memory_old, 0.0_SRP, 1.0_SRP )
      food_gain_memory_new = size2mass_food(mean_size_food_memory_new) *      &
                              within( mean_n_food_memory_new, 0.0_SRP, 1.0_SRP )

      !> The next step is to calculate the baseline food gain @f$ f_0 @f$,
      !! against which the expectancy based on the the_behaviour::hope() function
      !! is evaluated. This baseline value is obtained by weighting the average
      !! mass of the food items in the whole memory stack @f$ \overline{m} @f$
      !! by their average number @f$ \overline{n} @f$ provided this number
      !! is *n<1* as above:
      !> @image html img_doxygen_migrate_formula_2.svg
      !! @image latex img_doxygen_migrate_formula_2.eps "" width=14cm
      !!
      !! This baseline food gain is then weighted by the subjective probability
      !! of food item capture that is calculated based on values from the the
      !! memory the_neurobio::perception::food_probability_capture_subjective().
      food_gain_memory_baseline =                                             &
          size2mass_food(                                                     &
              this_agent%memory_stack%get_food_mean_size(                     &
                  predict_window_food_here)) *                                &
          within(                                                             &
              this_agent%memory_stack%get_food_mean_n(                        &
                                                  predict_window_food_here),  &
              0.0_SRP, 1.0_SRP ) *                                            &
          this_agent%food_probability_capture_subjective(                     &
                              predict_window_food_here, time_step_model_here )

      !> Finally, the the_behaviour::hope() function is called with the above
      !! estimates for the baseline food gain, its "older" and "newer" values.
      !! The *zero hope ratio* and the *maximum hope* parameters are obtained
      !! from commondata::migrate_food_gain_ratio_zero_hope and
      !! commondata::migrate_food_gain_maximum_hope parameter constants.
      this%expected_food_gain =                                               &
                      hope( baseline = food_gain_memory_baseline,             &
                            memory_old = food_gain_memory_old,                &
                            memory_new = food_gain_memory_new,                &
                            raw_grid_x = WALK_RANDOM_FOOD_HOPE_ABSCISSA,      &
                            raw_grid_y = WALK_RANDOM_FOOD_HOPE_ORDINATE  )

    end if SELECT_DIST_FOOD

    !> #### Calculate expected predation risk ####
    !> The expected risk of predation (as the food gain above) is calculated
    !! differently for relatively short and long walks. The walk is considered
    !! *short* if the distance does not exceed
    !! commondata::walk_random_pred_risk_hope_agentl units of the agent
    !! body lengths and *long* otherwise.
    !!
    !> First, the current level of the direct risk of predation is calculated
    !! using the_neurobio::perception::risk_pred() based on the perception of
    !! the `this_agent` agent assuming the agent is not freezing (because it
    !! is going to move a random walk).
    pred_dir_current = this_agent%risk_pred( is_freezing=.FALSE. )
    !!
    !> Second, calculate the current value of the general predation risk using
    !! the the_neurobio::predation_risk_backend() procedure. The size of this
    !! limited memory window is set by the `predict_window_pred` dummy
    !! parameter.
    !! @note In contrast to the above limited prediction memory window,
    !!       calculation of the predation risk in  the "objective" procedure
    !!       the_neurobio::perception_predation_risk_objective() uses
    !!       the same backend but the *whole* memory window
    !!       (commondata::history_size_perception).
    pred_current =                                                            &
        predation_risk_backend(                                               &
            pred_count = this_agent%perceive_predator%get_count(),            &
            pred_memory_mean = this_agent%memory_stack%get_pred_mean(         &
                                                predict_window_pred_here),    &
            weight_direct = WEIGHT_DIRECT )

    !> ##### Short walk #####
    !! In short walks, the expected values are just equal to the above current
    !! direct extimates.
    SELECT_DIST_PRED: if ( this%distance < WALK_RANDOM_PRED_RISK_HOPE_AGENTL  &
                           * this_agent%get_length() ) then

      !> - **Direct** risk of predation is equal to the current value as
      !!   calculated above using the_neurobio::perception::risk_pred().
      this%expected_pred_dir_risk = pred_dir_current

      !> - **General** risk, the expected *general* risk of predation in
      !!   random walk is equal to the current value of direct predation risk
      !!   as above.
      !! .
      this%expected_predation_risk =  pred_current

    !> ##### Long walk #####
    !! On the other hand, for long walks, the expected values of the risks
    !! are based on the the_behaviour::hope() function mechanism.
    else SELECT_DIST_PRED

      !> - First, calculate the older and newer predation averages from the
      !!   memory stack using the
      !!   the_neurobio::memory_perceptual::get_pred_mean_split() method.
      !!   These averages serve as the base point for calculating the
      !!   new to old ratio in the the_behaviour::hope() function.
      call this_agent%memory_stack%get_pred_mean_split(                       &
                                          window = predict_window_pred_here,  &
                                          older = mean_n_pred_memory_old,     &
                                          newer = mean_n_pred_memory_new )

      !> - The **direct risk** of predation is based on the_behaviour::hope()
      !!   function. If the number of predators in the latest memory
      !!   (predation risk) is increasing in the local environment, its
      !!   expectancy in the unknown environment at a long distance
      !!   diminishes, if the risk is reducing over time in the agent's
      !!   perception, the  expectancy increases. The hope grid values for
      !!   the general predation hope function are defined by the
      !!   commondata::migrate_predator_zero_hope and
      !!   commondata::migrate_predator_maximum_hope parameter constants.
      !!   @note Note that the hope function constants used here are the same
      !!        as for the_behaviour::migrate.
      this%expected_pred_dir_risk = hope(  pred_dir_current,                  &
                                           mean_n_pred_memory_old,            &
                                           mean_n_pred_memory_new,            &
                                           MIGRATE_PREDATOR_ZERO_HOPE,        &
                                           MIGRATE_PREDATOR_MAXIMUM_HOPE  )

      !> - The expectancy value of **general predation risk** after long walk
      !!   is obtained via the the_behaviour::hope() function. If the number of
      !!   predators (risk) is increasing in the latest perception memory,
      !!   its expectancy after long walk diminishes, if the perceived risk is
      !!   reducing over time, the expectancy increases. The hope grid values
      !!   for the general predation hope function are defined by the
      !!   commondata::migrate_predator_zero_hope and
      !!   commondata::migrate_predator_maximum_hope parameter constants.
      !!   @note Note that the hope function constants used here are the same
      !!         as for the_behaviour::migrate.
      !! .
      this%expected_predation_risk = hope( pred_current,                      &
                                           mean_n_pred_memory_old,            &
                                           mean_n_pred_memory_new,            &
                                           MIGRATE_PREDATOR_ZERO_HOPE,        &
                                           MIGRATE_PREDATOR_MAXIMUM_HOPE  )

    end if SELECT_DIST_PRED

  end subroutine walk_random_do_this

  !-----------------------------------------------------------------------------
  !> `the_behaviour::walk_random::expectancies_calculate()` (re)calculates
  !! motivations from fake expected perceptions following from the procedure
  !! `walk_random::do_this()` => `the_behaviour::walk_random_do_this()`.
  subroutine walk_random_motivations_expect(this, this_agent,                 &
                                    distance, distance_cv,                    &
                                    predict_window_pred, predict_window_food, &
                                    time_step_model, rescale_max_motivation)
    class(WALK_RANDOM), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which does random walk.
    class(APPRAISAL), intent(in)    :: this_agent
    !> @param[in] distance is an optional walk distance. If stochastic Gaussian
    !!            walk is set, this value defines the average distance.
    real(SRP), optional, intent(in) :: distance
    !> @param[in] distance_cv is an optional coefficient of variation for the
    !!            random walk distance. If absent, non-stochastic walk step
    !!            size is used.
    real(SRP), optional, intent(in) :: distance_cv
    !> @param[in] predict_window_pred the size of the prediction window, i.e.
    !!            how many steps back in memory are used to calculate the
    !!            predicted general predation risk. This parameter is limited
    !!            by the maximum
    !!            commondata::history_size_perception value of the perception
    !!            memory history size.
    integer, optional, intent(in) :: predict_window_pred
    !> @param[in] predict_window_food the size of the prediction window, i.e.
    !!            how many steps back in memory are used to calculate the
    !!            predicted food gain. This parameter is limited by the maximum
    !!            commondata::history_size_perception value of the perception
    !!            memory history size.
    integer, optional, intent(in) :: predict_window_food
    !> @param [in] time_step_model optional time step of the model,
    !!             **overrides** the value calculated from the spatial data.
    integer, optional, intent(in)   :: time_step_model
    !> @param[in] rescale_max_motivation optional maximum motivation value for
    !!            rescaling all motivational components for comparison
    !!            across all motivation and perceptual components and behaviour
    !!            units.
    real(SRP), optional, intent(in) :: rescale_max_motivation

    ! Local copies of optionals
    real(SRP) :: dist_loc

    ! Local copy of optional model time step
    integer :: time_step_model_here, predict_window_food_here,                &
               predict_window_pred_here

    ! Local variables
    real(SRP) :: max_motivation ! Local max. over all motivation components.

    ! Expected food item that is used in the calculations, its properties are
    ! based on the average food items that the agent perceives below.
    type(FOOD_ITEM) :: expected_food_item

    real(SRP) :: expected_food_item_distance

    !> The probability of capture of the expected food object.
    real(SRP) :: expected_food_item_prob_capture

    !> Expected food gain that is fitting into the stomach of the agent.
    real(SRP) :: expected_food_item_gain_fits

    ! Current stomach contents of the agent.
    real(SRP) :: agent_stomach

    !> ### Notable local variables ###
    !> #### Perception overrides ####
    !> - **perception_override_food_dir** is the expected number of food items
    !!   in perception.
    real(SRP) :: perception_override_food_dir
    !> - **perception_override_pred_dir** is the expected direct
    !!   predation risk.
    real(SRP) :: perception_override_pred_dir
    !> - **perception_override_predator** is the expected general predation
    !!   risk, that is based on a weighting of the current predation and
    !!   predation risk from the memory stack.
    real(SRP) :: perception_override_predator
    !> - **perception_override_stomach** is the expected stomach contents
    !!   as a consequence of random walk.
    real(SRP) :: perception_override_stomach
    !> - **perception_override_bodymass** is the expected body mass as a
    !!   consequence of the random walk.
    real(SRP) :: perception_override_bodymass
    !> - **perception_override_energy** is the expected energy reserves
    !!   as a consequence of the escape movement. Calculated from the body
    !!   mass and weight.
    !! .
    real(SRP) :: perception_override_energy

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME =                                 &
                                          "(walk_random_motivations_expect)"

    !> ### Implementation details ###
    !> #### Checks and preparations ####
    !> Check optional distance of walk. If it is absent, defined as
    !! commondata::walk_random_distance_default_factor times of the agent
    !! body length.
    if (present(distance)) then
      dist_loc = distance
    else
      dist_loc = this_agent%get_length() * WALK_RANDOM_DISTANCE_DEFAULT_FACTOR
    end if

    !> Check optional time step parameter. If not provided, use global
    !! parameter value from commondata::global_time_step_model_current.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> Check optional parameter for the general predation risk perception
    !! memory window. If the `predict_window_pred` dummy parameter is not
    !! provided, its default value is the proportion of the whole perceptual
    !! memory window defined by commondata::history_perception_window_pred.
    !! Thus, only the latest part of the memory is used for the prediction
    !! of the future predation risk.
    if (present(predict_window_pred)) then
      predict_window_pred_here= predict_window_pred
    else
      predict_window_pred_here = floor( HISTORY_SIZE_PERCEPTION *             &
                                        HISTORY_PERCEPTION_WINDOW_PRED )
    end if

    !> Check optional parameter for the food perception memory window. If
    !! the `predict_window_food` dummy parameter is not provided, its default
    !! value is the proportion of the whole perceptual memory window defined
    !! by commondata::history_perception_window_food. Thus, only the
    !! latest part of the memory is used for the prediction of the future
    !! food gain.
    if (present(predict_window_food)) then
      predict_window_food_here = predict_window_food
    else
      predict_window_food_here = floor( HISTORY_SIZE_PERCEPTION *             &
                                        HISTORY_PERCEPTION_WINDOW_FOOD )
    end if

    !> #### Call do_this ####
    !> As the first step, we use the **do**-procedure walk_random::do_this()
    !! => the_behaviour::walk_random_do_this() to perform the behaviour desired
    !! without changing either the agent or its environment, obtaining  the
    !! **subjective** values of the `this` behaviour components that later feed
    !! into the motivation **expectancy** functions:
    !!  - `perception_override_food_dir`
    !!  - `perception_override_pred_dir`
    !!  - `perception_override_predator`
    !!  - `perception_override_stomach`
    !!  - `perception_override_bodymass`
    !!  - `perception_override_energy`
    !!  .
    if (present(distance_cv)) then
      ! Stochastic walk expectancy.
      call this%do_this( this_agent=this_agent,                               &
                         distance=dist_loc,                                   &
                         distance_cv=distance_cv,                             &
                         predict_window_pred=predict_window_pred_here,        &
                         predict_window_food=predict_window_food_here,        &
                         time_step_model=time_step_model_here   )
    else
      ! Normal deterministic walk expectancy.
      call this%do_this( this_agent=this_agent,                               &
                         distance=dist_loc,                                   &
                         predict_window_pred=predict_window_pred_here,        &
                         predict_window_food=predict_window_food_here,        &
                         time_step_model=time_step_model_here   )
    end if

    !> #### Calculate expected (fake) perceptions ####
    !> ##### Fake perception for the food items #####
    !> The expected perception of the number of food items that the agent
    !! is going to see following the walk is calculated in the `do_this`
    !! procedure. Here it is obtained from the \%expected_food_dir data
    !! component of the class.
    perception_override_food_dir = this%expected_food_dir

    !> ##### Fake perception of stomach content #####
    !> First, create a fake food item with the spatial position identical to
    !! that of the agent. The position is used only to calculate the
    !! illumination and therefore visual range. The cost(s) are calculated
    !! providing explicit separate distance parameter, so the zero distance
    !! from the agent is inconsequential. The size of the
    !! food item is obtained from the expected food gain by the reverse
    !! calculation function the_environment::mass2size_food().
    !! Standard `make` method for the food item class is used.
    call expected_food_item%make(location=this_agent%location(),              &
                                 size=mass2size_food(this%expected_food_gain),&
                                 iid=UNKNOWN )

    !> Second, calculate the **probability of capture** of this expected food
    !! item. The probability of capture of the fake food item is calculated
    !! using the the_environment::food_item::capture_probability() backend
    !! assuming the distance to the food item is equal to the average distance
    !! of all food items in the **current perception** object. However, if the
    !! agent does not see any food items currently, the distance to the fake
    !! food item is assumed to be equal to the visibility range weighted by
    !! the (fractional) commondata::walk_random_dist_expect_food_uncertain_fact
    !! parameter. Thus, the expected *raw* food gain (in the `do`-function) is
    !! based on the past memory whereas the probability of capture is based
    !! on the latest perception experience.
    if ( this_agent%has_food() ) then
      expected_food_item_distance = this_agent%perceive_food%get_meandist()
    else
      ! TODO: add average food distances to perception memory
      expected_food_item_distance = expected_food_item%visibility() *         &
                                    DIST_EXPECT_FOOD_UNCERTAIN_FACT
    end if

    expected_food_item_prob_capture =                                         &
        expected_food_item%capture_probability(                               &
                                        distance=expected_food_item_distance )

    !> Third, the expected food gain corrected for fitting into the agent's
    !! current stomach and capture cost is obtained by
    !! the_body::condition::food_fitting(). It is then weighted by the
    !! expected capture probability. Note that the probability of capture
    !! (weighting factor) is calculated based on the current perception
    !! (see above), but the travel cost is based on the actual expected
    !! \%distance.
    expected_food_item_gain_fits =                                            &
        this_agent%food_fitting( this%expected_food_gain, this%distance )     &
        * expected_food_item_prob_capture

    !> **Stomach content**: the perception override value for the stomach
    !! content is obtained incrementing the current stomach contents by
    !! the nonzero expected food gain, adjusting also for the digestion
    !! decrement (the_body::stomach_emptify_backend()).
    agent_stomach = this_agent%get_stom_content()
    perception_override_stomach =                                             &
                max( ZERO,                                                    &
                     agent_stomach - stomach_emptify_backend(agent_stomach) + &
                        expected_food_item_gain_fits )

    !> **Body mass**: the **body mass** perception override is obtained by
    !! incrementing (or decrementing if the expected food gain is negative)
    !! the current body mass by the expected food gain and also subtracting
    !! the cost of living component.
    perception_override_bodymass =                                            &
                max( ZERO,                                                    &
                     this_agent%get_mass() -                                  &
                        this_agent%living_cost() +                            &
                        expected_food_item_gain_fits )

    !> **Energy**: The fake perception values for the energy reserves
    !! (`energy_override_perc`) using the `the_body::energy_reserve()`
    !! procedure.
    perception_override_energy = energy_reserve( perception_override_bodymass,&
                                                 this_agent%length() )

    !> **Predation risk**: finally, fake perceptions of predation risk are
    !! obtained from the values calculated in the `do` procedure:
    !! the_behaviour::walk_random::expected_pred_dir_risk and
    !! the_behaviour::walk_random::expected_predation_risk.
    perception_override_pred_dir = this%expected_pred_dir_risk
    perception_override_predator = this%expected_predation_risk

    !> #### Calculate motivation expectancies ####
    !> The next step is to calculate the motivational expectancies using the
    !! fake perceptions to override the default (actual agent's) values.
    !> At this stage, first, calculate motivation values resulting from the
    !! behaviour done (`walk_random::do_this()` ) at the previous steps: what
    !! would be the motivation values *if* the agent does perform
    !! WALK_RANDOM? Technically, this is done by calling the **neuronal
    !! response function**, `percept_components_motiv::motivation_components()`
    !! method, for each of the motivational states with `perception_override_`
    !! dummy parameters overriding the default values.
    !! Here is the list of the fake overriding perceptions for the
    !! WALK_RANDOM behaviour:
    !!  - `perception_override_food_dir`
    !!  - `perception_override_pred_dir`
    !!  - `perception_override_predator`
    !!  - `perception_override_stomach`
    !!  - `perception_override_bodymass`
    !!  - `perception_override_energy`
    !!  .
    !  @note  **Expectancy** assessment for **hunger** motivation, using
    !         `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in] for
    !         `this_agent` now.
    call this%expectancy%hunger%percept_component%motivation_components       &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL,              &
      param_gp_matrix_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL,         &
      param_gp_matrix_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL,     &
      param_gp_matrix_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL,     &
      param_gp_matrix_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL,               &
      param_gp_matrix_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL,           &
      param_gp_matrix_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL,             &
      param_gp_matrix_age      = AGE_HUNGER_GENOTYPE_NEURONAL,                &
      param_gp_matrix_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL,            &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_depth    = DEPTH_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,    &
      param_gerror_cv_food_dir = FOODCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,     &
      param_gerror_cv_bodymass = BODYMASS_HUNGER_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_energy   = ENERGY_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_age      = AGE_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,      &
      param_gerror_cv_reprfac  = REPRFAC_HUNGER_GENOTYPE_NEURONAL_GERROR_CV,  &
      !  Real agent perception components are now substituted by the *fake*
      !  values resulting from executing this behaviour (`do_this` method).
      !  This is repeated for all the motivations: *hunger*,
      !  *passive avoidance,* *fear state* etc.
      perception_override_food_dir = perception_override_food_dir,            &
      perception_override_pred_dir = perception_override_pred_dir,            &
      perception_override_predator = perception_override_predator,            &
      perception_override_stomach = perception_override_stomach,              &
      perception_override_bodymass = perception_override_bodymass,            &
      perception_override_energy = perception_override_energy                 &
                                                                              )
      !> Real agent perception components are now substituted by the *fake*
      !! values resulting from executing this behaviour (`reproduce::do_this()`
      !! => `the_behaviour::reproduce_do_this()` method). This is repeated for
      !! all the motivations: *hunger*, *passive avoidance,* *active
      !! avoidance* etc. These optional **override parameters** are
      !! substituted by the "fake" values.

    !  @note  **Expectancy** assessment for **fear_defence** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%fear_defence%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL, &
      param_gp_matrix_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_food_dir = perception_override_food_dir,            &
      perception_override_pred_dir = perception_override_pred_dir,            &
      perception_override_predator = perception_override_predator,            &
      perception_override_stomach = perception_override_stomach,              &
      perception_override_bodymass = perception_override_bodymass,            &
      perception_override_energy = perception_override_energy                 &
                                                                              )

    !  @note  **Expectancy** assessment for **reproduction** motivation,
    !         using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[in]
    !         for `this_agent` now.
    call this%expectancy%reproduction%percept_component%motivation_components &
      (this_agent,                                                            &
      ! Parameters:: Boolean G x P matrices:
      param_gp_matrix_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL,          &
      param_gp_matrix_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL,      &
      param_gp_matrix_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL,     &
      param_gp_matrix_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL,    &
      param_gp_matrix_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL, &
      param_gp_matrix_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL,           &
      param_gp_matrix_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL,       &
      param_gp_matrix_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL,         &
      param_gp_matrix_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL,            &
      param_gp_matrix_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL,        &
      ! Parameters :: G x P variances:
      param_gerror_cv_light    = LIGHT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_depth    = DEPTH_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_food_dir = FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_food_mem = FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV, &
      param_gerror_cv_conspec  = CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_pred_dir = PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_predator = PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_stomach  = STOM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,  &
      param_gerror_cv_bodymass = BODYMASS_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_energy   = ENERGY_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      param_gerror_cv_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      !  @note Real agent perception components are now **substituted**
      !        by the **fake** values resulting from executing this
      !        behaviour (`do_this` method).
      perception_override_food_dir = perception_override_food_dir,            &
      perception_override_pred_dir = perception_override_pred_dir,            &
      perception_override_predator = perception_override_predator,            &
      perception_override_stomach = perception_override_stomach,              &
      perception_override_bodymass = perception_override_bodymass,            &
      perception_override_energy = perception_override_energy                 &
                                                                              )

    !> #### Calculate primary and final motivations ####
    !> Next, from the perceptual components calculated at the previous
    !! step we can obtain the **primary** and **final motivation** values by
    !! weighed summing.
    if (present(rescale_max_motivation)) then
      !> Here we can use global maximum motivation across all behaviours and
      !! perceptual components if it is provided, for rescaling.
      max_motivation = rescale_max_motivation
    else
      !> Or can rescale using local maximum value for this behaviour only.
      max_motivation =  this%expectancy%max_perception()
    end if

    !> Transfer attention weights from the actor agent `this_agent` to the
    !! `this` behaviour component. So, we will now use the updated modulated
    !! attention weights of the agent rather than their default parameter
    !! values.
    call this%attention_transfer(this_agent)

    !> So the primary motivation values are calculated.
    call this%expectancy%motivation_primary_calc(max_motivation)

    !> Primary motivations are logged in the @ref intro_debug_mode "debug mode".
    call LOG_DBG( LTAG_INFO // "Primary motivations: " //                     &
                  "hunger: " //                                               &
                    TOSTR(this%expectancy%hunger%motivation_prim)  //         &
                  ", fear_defence: " //                                       &
                    TOSTR(this%expectancy%fear_defence%motivation_prim) //    &
                  ", reproduce: " //                                          &
                    TOSTR(this%expectancy%reproduction%motivation_prim),      &
                  PROCNAME, MODNAME )

    !> There is **no modulation** at this stage, so the final motivation
    !! values are the same as primary motivations.
    call this%expectancy%modulation_none()

    !> #### Calculate motivation expectancies ####
    !> Finally, calculate the finally **expected arousal level for this
    !! behaviour**. As in the GOS, the overall arousal is the maximum value
    !! among all motivation components.
    this%arousal_expected = this%expectancy%max_final()

    !> Log also the final expectancy value in the @ref intro_debug_mode
    !! "debug mode".
    call LOG_DBG( LTAG_INFO // "Expectancy: " // TOSTR(this%arousal_expected),&
                  PROCNAME, MODNAME )

    !> Now as we know the expected arousal, we can choose the behaviour which
    !! would minimise this arousal level.

  end subroutine walk_random_motivations_expect

  !-----------------------------------------------------------------------------
  !> Execute this behaviour component "random walk" by `this_agent` agent.
  subroutine walk_random_do_execute(this, this_agent, step_dist, step_cv,     &
                                                            environment_limits)
    class(WALK_RANDOM), intent(inout) :: this
    !> @param[in] this_agent is the actor agent which goes down.
    class(APPRAISAL), intent(inout)    :: this_agent
    !> @param[in] step_dist optional fixed distance of the walk. In case the
    !!            coefficient of variation (next optional parameter) is
    !!            provided, the walk distance is stochastic with the later
    !!            coefficient of variation.
    real(SRP), optional, intent(in) :: step_dist
    !> @param[in] step_cv Optional coefficient of variation for the walk step,
    !!            if not provided, the step CV set by the parameter
    !!            commondata::walk_random_distance_stochastic_cv.
    real(SRP), optional, intent(in) :: step_cv
    !> @param environment_limits Limits of the environment area available for
    !!        the random walk. The moving object cannot get beyond this limit.
    !!        If this parameter is not provided, the environmental limits are
    !!        obtained automatically from the global array
    !!        the_environment::global_habitats_available.
    class(ENVIRONMENT), intent(in), optional :: environment_limits

    ! Local copies of optionals
    real(SRP) :: step_cv_here

    !> ### Implementation details ###
    !> #### Checks and preparations ####
    !> Check if the optional coefficient of variation for the step size. If the
    !! parameter is not provided, the CV is set from the parameter
    !! commondata::walk_random_distance_stochastic_cv.
    !! @warning To set deterministic walk, the coefficient of variation should
    !!          be explicitly set to 0.0. This is different from the expectancy
    !!          procedures , which assume deterministic default walk (CV=0.0).
    if (present(step_cv)) then
      step_cv_here = step_cv
    else
      step_cv_here = WALK_RANDOM_DISTANCE_STOCHASTIC_CV
    end if

    !> #### Step 1: do_this ####
    !> First, we use the intent-in **do**-procedure
    !! the_behaviour::walk_random::do_this() to perform the behaviour desired.
    !! However, Expectancies for food gain and predator risk that are not used
    !! at this stage.
    if (present(step_dist)) then
      call this%do_this( this_agent=this_agent, distance=step_dist,           &
                                                distance_cv=step_cv_here )
    else
      call this%do_this( this_agent=this_agent, distance_cv=step_cv_here )
    end if

    !> #### Step 2: Change the agent ####
    !> ##### Perform walk #####
    !> The agent does the random walk with the step size this\%distance.
    !! Therefore, it is now possible to change the state of the agent.
    !!
    !! Random walk is done in the "2.5D" mode, i.e. with separate parameters
    !! for the horizontal distance (and CV) and vertical depth distance (and
    !! its CV). This is done to avoid potentially a too large vertical
    !! displacement of the agent (vertical migration involves separate
    !! behaviours). Thus, the vertical shift distance should normally be
    !! smaller than the horizontal shift. The difference between the main
    !! horizontal and smaller vertical shifts is defined by the parameter
    !! commondata::walk_random_vertical_shift_ratio. Note that the coefficient
    !! of variation for the vertical walk component is set separately
    !! using the ratio commondata::walk_random_vertical_shift_cv_ratio.
    !!
    !! The agent performs the random walk using the main
    !! the_environment::spatial_moving::rwalk() procedure. If the limiting
    !! environment is known (`environment_limits` optional parameter), the
    !! `rwalk` call also includes it. If environmental limits are not provided
    !! as a dummy parameter, they are obtained automatically from the global
    !! array the_environment::global_habitats_available.
    if (present(environment_limits)) then
      call this_agent%rwalk(                                                  &
                        this%distance, this%distance_cv,                      &
                        this%distance * WALK_RANDOM_VERTICAL_SHIFT_RATIO,     &
                        this%distance_cv*WALK_RANDOM_VERTICAL_SHIFT_CV_RATIO, &
                        environment_limits )
    else
      call this_agent%rwalk(                                                  &
                        this%distance, this%distance_cv,                      &
                        this%distance * WALK_RANDOM_VERTICAL_SHIFT_RATIO,     &
                        this%distance_cv*WALK_RANDOM_VERTICAL_SHIFT_CV_RATIO, &
                        Global_Habitats_Available(                            &
                                            this_agent%find_environment(      &
                                              Global_Habitats_Available) )  )
    end if

    !> ##### Process the cost of movement #####
    !> - Reset the body mass of the actor agent subtracting the actual cost of
    !!   moving that is automatically calculated in the call to
    !!   the_body::condition::cost_swim(). The the_body::condition::set_mass()
    !!   method is used here to adjust the mass.
    call this_agent%set_mass(                                                 &
                      value_set = this_agent%get_mass() -                     &
                        this_agent%cost_swim(exponent=                        &
                                        SWIMMING_COST_EXPONENT_LAMINAR),      &
                      update_history = .TRUE. )

    !> - Additionally, also call the `the_body::condition::set_length()` method
    !!   to update the body length history stack. However, the value_set
    !!   parameter here is just the current value. This fake re-setting of the
    !!   body length is done to keep both mass and length synchronised in their
    !!   history stack arrays (there is no procedure for only updating history).
    call this_agent%set_length( value_set = this_agent%get_length(),          &
                                update_history = .TRUE. )

    !> - After resetting the body mass, update energy reserves of the agent,
    !!   that depend on both the length and the mass.
    !! .
    call this_agent%energy_update()

    !> Finally, check if the agent is starved to death. If yes, the agent can
    !! die without going any further.
    if (this_agent%starved_death()) call this_agent%dies()

    !> #### Step 3: Change the environment ####
    !> Random walk does not affect the environmental objects.

  end subroutine walk_random_do_execute

  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  !-----------------------------------------------------------------------------
  !> Initialise the behaviour components of the agent, the
  !! the_behaviour::behaviour class.
  elemental subroutine behaviour_whole_agent_init(this)
    class(BEHAVIOUR), intent(inout) :: this

    !> ### Implementation notes ###
    !> Initialise the label for the currently executed behaviour to an easily
    !! discernible value (e.g. by `gre`).
    this%behaviour_label = "init_unknown"

    !> Initialise the execution status of each of the behaviour units that
    !! compose this class to FALSE (these behavioural units are not executing).
    call this%deactivate()

    !> Cleanup the history stack of behaviour labels
    !! the_behaviour::behaviour::history_behave.
    call this%cleanup_behav_history()

    !> Initialise @ref anchor behav_debug_indicators "debugging indicators"
    !! for the_behaviour::behaviour class.
    this%n_eats_all_indicator = 0
    this%n_eaten_indicator = 0
    this%mass_eaten_indicator = 0.0_SRP

  end subroutine behaviour_whole_agent_init

  !-----------------------------------------------------------------------------
  !> Deactivate all behaviour units that compose the behaviour repertoire of
  !! the agent.
  elemental subroutine behaviour_whole_agent_deactivate(this)
    class(BEHAVIOUR), intent(inout) :: this

    this%eat%is_active = .FALSE.
    this%reproduce%is_active = .FALSE.
    this%walk_random%is_active = .FALSE.
    this%freeze%is_active = .FALSE.
    this%escape_dart%is_active = .FALSE.
    this%approach_spatial%is_active = .FALSE.
    this%approach_conspec%is_active = .FALSE.
    this%migrate%is_active = .FALSE.
    this%depth_down%is_active = .FALSE.
    this%depth_up%is_active = .FALSE.
    this%debug_base%is_active = .FALSE.

  end subroutine behaviour_whole_agent_deactivate

  !-----------------------------------------------------------------------------
  !> Obtain the label of the currently executing behaviour for the `this` agent.
  elemental function behaviour_get_behaviour_label_executing(this)            &
                                                              result (label_is)
    class(BEHAVIOUR), intent(in) :: this
    character(len=LABEL_LENGTH) :: label_is

    label_is = this%behaviour_label

  end function behaviour_get_behaviour_label_executing

  !-----------------------------------------------------------------------------
  !> Select the optimal conspecific among (possibly) several ones that are
  !! available in the **perception object** of the agent.
  function behaviour_select_conspecific(this, rescale_max_motivation)         &
                                                        result (number_in_seen)
    class(BEHAVIOUR), intent(inout) :: this    !> @param[in] self object.
    !> @param[in] rescale_max_motivation maximum motivation value for
    !!            rescaling all motivational components for comparison
    !!            across all motivation and perceptual components and behaviour
    !!            units.
    real(SRP), optional, intent(in) :: rescale_max_motivation
    !> @return The function returns the index of the food item that is chosen
    !!         for eating (if there are any food items within the perception
    !!         object of the agent) or 0 otherwise.
    integer :: number_in_seen

    ! Local variables.
    !> ### Notable local variables ###
    !> - **n_seen_percepis** is the total number of food items found in the
    !!   perception object.
    integer :: iconsp, n_seen_percep
    real(SRP) :: rescale_max_motivation_here

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(behaviour_select_conspecific)"

    !> - **expected_gos_consp** is an *array* of motivational GOS expectancies
    !!   from each of the food items within the perception object.
    real(SRP), dimension(this%perceive_consp%conspecifics_seen_count) ::      &
                                                            expected_gos_consp

    !> ### Implementation details ###
    !> #### Preparation steps ####
    !> First, check if the agent has any conspecific(s) within its perception
    !! objects using `perception::has_consp()` method. Return zero straight
    !! away if no conspecifics are seen. Therefore, from now on it is assumed
    !! that the agent has at least one conspecific in its perception object.
    if (.not. this%has_consp()) then
      number_in_seen = 0
      return
    end if

    !> The local variable `n_seen_percep` is the total number of conspecifics
    !! found in the perception object.
    !  @note Can also be obtained using class function `...%get_count()`.
    n_seen_percep = this%perceive_consp%conspecifics_seen_count

    !> If there is only one conspecific, get its number (1) and exit. There
    !! is no choice if only a single conspecific is here.
    if (n_seen_percep==1) then
      number_in_seen = 1
      return
    end if

    !> Check if the maximum motivation value for rescale is provided as
    !! a parameter.
    if (present(rescale_max_motivation)) then
      !> If provided, use global maximum motivation across all behaviours and
      !! perceptual components for rescaling.
      rescale_max_motivation_here = rescale_max_motivation
    else
      !> If not provided, rescale using local maximum motivation value for
      !! this agent.
      rescale_max_motivation_here =  this%motivations%max_final()
    end if

    !> #### Calculate GOS expectancies ####
    !> Calculate GOS expectancies from approaching each of the conspecifics
    !! in the perception object. This is implemented in the `CONSP_EXPECT`
    !! loop.
    !> ##### CONSP_EXPECT loop #####
    CONSP_EXPECT: do iconsp = 1, n_seen_percep

      !> - First, initialise the behavioural state. Specifically, the
      !!   `approach_conspec::init()` method initialises the attention weights.
      call this%approach_conspec%init()

      !> - Second, calculate the motivation GOS expectancies that would result
      !!   if the agent approaches to each of the conspecifics that are
      !!   in its perception object. The method
      !!   approach_conspec::expectancies_calculate()` does the job.
      !!   @note Note that the target offset parameter is absent, which means
      !!         that the default value, average body size of the agent and its
      !!         target, is used. TODO: or set explicitly?
      call this%approach_conspec%expectancies_calculate(                      &
                this_agent = this,                                            &
                target_object = this%perceive_consp%conspecifics_seen(iconsp),&
                rescale_max_motivation = rescale_max_motivation_here )

      !> - Now we can get an array of motivational GOS expectancies from
      !!   approaching each of the conspecifics within the perception object:
      !!   `expected_gos_consp`.
      expected_gos_consp(iconsp) = this%approach_conspec%arousal_expected    ! %gos_expected()

    end do CONSP_EXPECT

    !> ####  Select minimum arousal items ####
    !> Once we calculated GOS motivational expectancies for all the food items,
    !! we can determine which of the food items results in the **minimum**
    !! arousal.
    number_in_seen = minloc(expected_gos_consp, 1)

    call LOG_DBG(LTAG_INFO // "arousal expectancies for all conspecifics" //  &
                 "in the perception object: " // TOSTR(expected_gos_consp),   &
                 PROCNAME,MODNAME)

    call LOG_DBG(LTAG_INFO // "minimum arousal # " // TOSTR(number_in_seen) //&
                 " = " // TOSTR(expected_gos_consp(number_in_seen)),          &
                 PROCNAME, MODNAME)

  end function behaviour_select_conspecific

  !-----------------------------------------------------------------------------
  !> Select the nearest conspecific among (possibly) several ones that are
  !! available in the perception object. Note that conspecifics are sorted
  !! by distance within the perception object. Thus, this procedure just
  !! selects the first conspecific.
  function behaviour_select_conspecific_nearest(this) result(number_in_seen)
    class(BEHAVIOUR), intent(in) :: this    !> @param[in] self object.
    !> @return The function returns the index of the first conspecific
    !!         if there are any within the perception object of the agent,
    !!         0 otherwise.
    integer :: number_in_seen

    if(this%has_consp()) then
      number_in_seen = 1
    else
      number_in_seen = 0
    end if

  end function behaviour_select_conspecific_nearest

  !-----------------------------------------------------------------------------
  !> Select the optimal food item among (possibly) several ones that are
  !! available in the **perception object** of the agent.
  !! @details Choosing the optimal food item to catch may be a non-trivial task
  !!          and different decision rules could be implemented for this.
  function behaviour_select_food_item(this, rescale_max_motivation)           &
                                                        result(number_in_seen)
    class(BEHAVIOUR), intent(inout) :: this    !> @param[in] self object.
    !> @param[in] rescale_max_motivation maximum motivation value for
    !!            rescaling all motivational components for comparison
    !!            across all motivation and perceptual components and behaviour
    !!            units.
    real(SRP), optional, intent(in) :: rescale_max_motivation
    !> @return The function returns the index of the food item that is chosen
    !!         for eating (if there are any food items within the perception
    !!         object of the agent) or 0 otherwise.
    integer :: number_in_seen

    ! Local variables.
    !> ### Notable local variables ###
    !> - **n_seen_percepis** is the total number of food items found in the
    !!   perception object.
    integer :: fitem, n_seen_percep
    real(SRP) :: rescale_max_motivation_here

    !> - **expected_gos_fitem** is an *array* of motivational GOS expectancies
    !!   from each of the food items within the perception object.
    real(SRP), dimension(this%perceive_food%food_seen_count) ::               &
                                                            expected_gos_fitem

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(behaviour_select_food_item)"

    !> ### Implementation details ###
    !> #### Preparation steps ####
    !> First, check if the agent has any food item(s) within its perception
    !! objects using `perception::has_food()` method. Return zero straight away
    !! if no food seen. Therefore, from now on it is assumed that the agent
    !! has at least one food item in its perception object.
    if (.not. this%has_food()) then
      number_in_seen = 0
      return
    end if

    !> The local variable `n_seen_percep` is the total number of food items
    !! found in the perception object.
    !  @note Can also be obtained using class function `...%get_count()`.
    n_seen_percep = this%perceive_food%food_seen_count

    !> If there is only one food item, get its number (1) and exit.
    !  @warning The condition can be disabled if we check the
    !           possible case when eating the food actually reduces
    !           fitness (GOS arousal increases), in such case
    !           do **not** eat the food item. But for just selecting
    !           which of the available items to eat, this case is
    !           degenerate.
    if (n_seen_percep==1) then
      number_in_seen = 1
      return
    end if

    !> Check if the maximum motivation value for rescale is provided as
    !! a parameter.
    if (present(rescale_max_motivation)) then
      !> If provided, use global maximum motivation across all behaviours and
      !! perceptual components for rescaling.
      rescale_max_motivation_here = rescale_max_motivation
    else
      !> If not provided, rescale using local maximum motivation value for
      !! this agent.
      rescale_max_motivation_here =  this%motivations%max_final()
    end if

    !> #### Calculate GOS expectancies ####
    !> Calculate GOS expectancies from each of the food items in the perception
    !! object. This is implemented in the `ITEMS_EXPECT` loop.
    !> ##### ITEMS_EXPECT loop #####
    ITEMS_EXPECT: do fitem = 1, n_seen_percep

      !> - First, initialise the behavioural state. Specifically, the
      !!   `eat_food::init()` method initialises the attention weights.
      call this%eat%init()

      !> - Second, calculate the motivation GOS expectancies from each of the
      !!   food item in the perception object of the **this** agent. The
      !!   `eat_food::expectancies_calculate()` does the job.
      call this%eat%expectancies_calculate(                                   &
                      this_agent = this,                                      &
                      food_item_eaten = this%perceive_food%foods_seen(fitem), &
                      rescale_max_motivation = rescale_max_motivation_here )

      !> - Now we can get an array of motivational GOS expectancies from
      !!   each of the food items within the perception object:
      !!   `expected_gos_fitem`.
      expected_gos_fitem(fitem) = this%eat%arousal_expected   ! %gos_expected()

      !  - And finally **weight** the final GOS expectancy value for this food
      !    item by a subjective assessment of the capture probability of this
      !    food item (weighting is reverse). The subjective capture probability
      !    is calculated by the sub-function `::subjective_capture_prob()`.
      !  .
      !expected_gos_fitem(fitem) = expected_gos_fitem(fitem) *                &
      !                              (1.0_SRP - subjective_capture_prob(fitem))

    end do ITEMS_EXPECT

    !> ####  Select minimum arousal items ####
    !> Once we calculated GOS motivational expectancies for all the food items,
    !! we can determine which of the food items results in the **minimum**
    !! arousal.
    !  @details The final value of the GOS expectancy is the the minimum
    !           @f[ min( \gamma_{i} \cdot (1-\rho_{i} \varepsilon ) ) , @f]
    !           where @f$ \gamma_{i} @f$ is the expected GOS arousal value
    !           for the i-th food item and @f$ \rho_{i} @f$ is subjective
    !           probability of the capture for this item, while
    !           @f$ \varepsilon @f$ is a Gaussian subjective probability
    !           assessment error.
    number_in_seen = minloc(expected_gos_fitem, 1)

    call LOG_DBG(LTAG_INFO // "Arousal expectancies for all food items " //   &
                 "in the perception object: " // TOSTR(expected_gos_fitem),   &
                 PROCNAME,MODNAME)

    call LOG_DBG(LTAG_INFO // "Minimum arousal # " //                         &
          TOSTR(number_in_seen) // " = " //                                   &
          TOSTR(expected_gos_fitem(number_in_seen)) //" item mass="           &
          // TOSTR(this%perceive_food%foods_seen(number_in_seen)%get_mass()), &
          PROCNAME, MODNAME)

    contains !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    !> Calculate subjective probability of food item capture, as objective
    !! capture probability and random assessment error.
    !! @note Note that this function is contained (ower order) in
    !!       the_behaviour::behaviour_select_food_item().
    function subjective_capture_prob(fitem) result (capture_prob_subjective)
      real(SRP) :: capture_prob_subjective
      integer   :: fitem
      !! For this we first calculate the probability of capture for this
      !! specific food item.
      capture_prob_subjective =                                               &
          this%perceive_food%foods_seen(fitem)%capture_probability(           &
                          distance=this%perceive_food%foods_distances(fitem) )
      call LOG_DBG( LTAG_INFO //                                              &
                    "Subjective capture probability, true value: " //         &
          TOSTR(capture_prob_subjective), PROCNAME, MODNAME )
      !> Then we add a random Gaussian error to the above objective value.
      !! Now we have obtained the stochastic subjective value of the capture
      !! probability for this food item including a Gaussian error. There is
      !! also a strong limitation for the subjective probability to be within
      !! the range [0.0, 1.0].
      !! See the_neurobio::food_perception_probability_capture_memory_object()
      !! for a similar Gaussian error in subjective probability.
      capture_prob_subjective = within( RNORM( capture_prob_subjective,       &
                        cv2variance(                                          &
                          FOOD_ITEM_CAPTURE_PROBABILITY_SUBJECTIVE_ERRORR_CV, &
                          capture_prob_subjective) ),       0.0_SRP, 1.0_SRP )

      call LOG_DBG( LTAG_INFO //                                              &
                    "Subjective capture probability, final value " //         &
                    "with Gaussian error: " // TOSTR(capture_prob_subjective),&
                    PROCNAME, MODNAME )

    end  function subjective_capture_prob

  end function behaviour_select_food_item

  !-----------------------------------------------------------------------------
  !> Select the nearest food item among (possibly) several ones that are
  !! available in the perception object. This is a specific and **most
  !! simplistic** version of the `behaviour_select_food_item` function: select
  !! the nearest food item available in the agent's perception object. Because
  !! the food items are sorted within the perception object just select the
  !! first item.
  function behaviour_select_food_item_nearest(this) result(number_in_seen)
    class(BEHAVIOUR), intent(in) :: this    !> @param[in] self object.
    !> @return The function returns the index of the first food item if there
    !!         are any food items within the perception object of the agent or
    !!         0 otherwise.
    integer :: number_in_seen

    if(this%has_food()) then
      number_in_seen = 1
    else
      number_in_seen = 0
    end if

  end function behaviour_select_food_item_nearest

  !-----------------------------------------------------------------------------
  !> Eat a specific food item that are found in the perception object.
  subroutine behaviour_do_eat_food_item(this,                                 &
                                            number_in_seen, food_resource_real)
    class(BEHAVIOUR), intent(inout) :: this   !> @param[in] self object.
    !> @param[in] The index of the first food item (if there are any food items)
    !!            within the perception object of the agent. If not set,
    !!            default is the first (nearest) food item in the perception
    !!            object.
    integer, optional, intent(in) :: number_in_seen

    !> @param[inout] food_resource_real The food resource the agent is eating
    !!               the food item in. Note that it could be a joined food
    !!               resource composed with the_environment::join() procedure
    !!               for assembling several habitats into the
    !!               the_environment::global_habitats_available array or
    !!               resources collapsed using the
    !!               the_environment::food_resource::join() method.
    class(FOOD_RESOURCE), intent(inout) :: food_resource_real

    ! Local copies of optionals.
    integer :: number_in_seen_here

    ! Local indicator of the eat success
    logical :: eat_food_item_is_success

    !> ### Implementation details ###
    !> First, check if the agent has any food items in its perception object
    !! using the perception::has_food() method. Return straight away if no
    !! food perceived.
    if (.not. this%has_food()) return

    if (present(number_in_seen)) then
      !> If there are no food items in the perception object or nothing is
      !! chosen, exit without any further processing. Normally this should
      !! not occur as the `perception::has_food()` check method guarantees
      !! that there are some food items in the perception object.
      if ( number_in_seen == 0 ) return
      !> If this check is passed set the id of the food perception object.
      number_in_seen_here = number_in_seen
    else
      number_in_seen_here = 1
    end if

    !> Finally, init the behaviour object `eat` before "execute". Calls the
    !! eat_food::init() method.
    call this%eat%init()

    !> Set the currently executed behaviour label. It is from the
    !! the_behaviour::behaviour_base::label data component of the base class.
    this%behaviour_label = this%eat%label

    !> Set the execution status for all behaviours to FALSE and then for this
    !! specific behaviour to TRUE. Only one behaviour unit can be executed at
    !! a time.
    call this%deactivate()
    this%eat%is_active = .TRUE.

    !> Do eat the food item chosen using the `execute` method of the
    !! `EAT_FOOD` class: eat_food::execute().
    call this%eat%execute( this_agent = this,                                 &
                           food_item_eaten =                                  &
                              this%perceive_food%foods_seen(                  &
                                                    number_in_seen_here),     &
                           food_resource_real = food_resource_real,           &
                           eat_is_success = eat_food_item_is_success  )

    !> Update (add to stack) the agent's history of behaviours
    !! the_behaviour::behaviour::history_behave: string labels of the
    !! behaviours are are saved.
    call add_to_history( this%history_behave, this%eat%label )

    !> Update (increment) the agent's debugging indicators from
    !! @ref behav_debug_indicators "indicators":
    !! - individual count of the_behaviour::eat_food occasions;
    this%n_eats_all_indicator = this%n_eats_all_indicator +1
    !> - individual count of successful food items \%n_eaten_indicator.
    !! .
    if (eat_food_item_is_success) then
      this%n_eaten_indicator = this%n_eaten_indicator + 1
      this%mass_eaten_indicator = this%mass_eaten_indicator +                 &
            this%perceive_food%foods_seen(number_in_seen_here)%get_mass()
    end if

    !> Shift the position of the agent to the position of the food item eaten.
    !! That is, the agent itself moves to the spatial position that has been
    !! occupied by the food item that has just been consumed.
    call this%position(                                                       &
                this%perceive_food%foods_seen(number_in_seen_here)%location() )

  end subroutine behaviour_do_eat_food_item

  !-----------------------------------------------------------------------------
  !> Reproduce based on the `this` agent's current state.
  subroutine behaviour_do_reproduce(this)
    class(BEHAVIOUR), intent(inout) :: this   !> @param[in] self object.

    !> ### Implementation details ###
    !! First, check if there are any conspecifics in the perception object.
    !! Return straight away if no conspecifics seen. No cost of reproduction
    !! is subtracted in such a case.
    if (.not. this%has_consp()) return

    !> Then, Init the behaviour (`reproduce::init()` =>
    !! `the_behaviour::reproduce_init_zero`) before "execute".
    call this%reproduce%init()

    !> Set the currently executed behaviour label. It is from the
    !! the_behaviour::behaviour_base::label data component of the base class.
    this%behaviour_label = this%reproduce%label

    !> Set the execution status for all behaviours to FALSE and then for this
    !! specific behaviour to TRUE. Only one behaviour unit can be executed at
    !! a time.
    call this%deactivate()
    this%reproduce%is_active = .TRUE.

    !> Finally, do the reproduction using the `reproduce::execute()` =>
    !! `the_behaviour::reproduce_do_execute()` method from the
    !! `the_behaviour::reproduce` class.
    call this%reproduce%execute( this )

    !> Update (add to stack) the agent's history of behaviours
    !! the_behaviour::behaviour::history_behave: string labels of the
    !! behaviours are are saved.
    call add_to_history( this%history_behave, this%reproduce%label )

    !> Finally, update the positional history stack
    !! the_environment::spatial_moving::history: the current spatial
    !! position of the agent is re-saved in the history stack using
    !! the_environment::spatial_moving::repeat_position().
    !! @note Re-saving the current position is necessary to keep the
    !!       full positional history even for the behaviours that do not
    !!       involve spatial displacement (movement).
    call this%repeat_position()

  end subroutine behaviour_do_reproduce

  !-----------------------------------------------------------------------------
  !> Perform a random Gaussian walk to a specific average distance with
  !! certain variance (set by the CV).
  subroutine behaviour_do_walk(this, distance, distance_cv)
    class(BEHAVIOUR), intent(inout) :: this
    !> @param[in] distance is an optional walk distance. If stochastic Gaussian
    !!            walk is set, this value defines the average distance.
    !!            @note Even though the walk distance is internally defined in
    !!                  terms of the agent's body length, this parameter
    !!                  defines the **absolute distance** in cm.
    real(SRP), optional, intent(in) :: distance
    !> @param[in] distance_cv is an optional coefficient of variation for the
    !!            random walk distance. If absent, non-stochastic walk step
    !!            size is used.
    real(SRP), optional, intent(in) :: distance_cv

    ! Local copies of optionals.
    real(SRP) :: distance_loc, distance_cv_loc

    !> ### Implementation notes ###
    !> - First, check if the walk distance is provided as a dummy parameter,
    !!   and if not, the default value is set by the
    !!   commondata::walk_random_distance_default_factor times of the agent
    !!   body length.
    if (present(distance)) then
      distance_loc = distance
    else
      distance_loc = this%get_length() * WALK_RANDOM_DISTANCE_DEFAULT_FACTOR
    end if

    !> - Then check if the Coefficient of Variation of the distance parameter
    !!   is also provided. If no, the default If the `distance_cv` optional
    !!   dummy parameter is set to the value defined by the
    !!   commondata::walk_random_distance_stochastic_cv parameter.
    distance_cv_loc = WALK_RANDOM_DISTANCE_STOCHASTIC_CV
    if (present(distance_cv)) then
      if ( distance_cv > TOLERANCE_HIGH_DEF_SRP ) then
        distance_cv_loc = distance_cv
      else
        distance_cv_loc = 0.0_SRP
      end if
    end if

    !> - Initialise the behavioural component for this behaviour,
    !!   the_behaviour::walk_random::init().
    call this%walk_random%init()

    !> Set the currently executed behaviour label. It is from the
    !! the_behaviour::behaviour_base::label data component of the base class.
    this%behaviour_label = this%walk_random%label

    !> Set the execution status for all behaviours to FALSE and then for this
    !! specific behaviour to TRUE. Only one behaviour unit can be executed at
    !! a time.
    call this%deactivate()
    this%walk_random%is_active = .TRUE.

    !> - Finally, call the `execute` method for this behaviour:
    !!   the_behaviour::walk_random::execute().
    !! .
    call this%walk_random%execute( this, step_dist=distance_loc,              &
                                         step_cv=distance_cv       )

    !> Update (add to stack) the agent's history of behaviours
    !! the_behaviour::behaviour::history_behave: string labels of the
    !! behaviours are are saved.
    call add_to_history( this%history_behave, this%walk_random%label )

  end subroutine behaviour_do_walk

  !-----------------------------------------------------------------------------
  !> Perform (execute) the the_behaviour::freeze behaviour.
  subroutine behaviour_do_freeze(this)
    class(BEHAVIOUR), intent(inout) :: this

    !> ### Implementation notes ###
    !> This behaviour has no parameters (e.g. target) and is rather trivial
    !! to execute:
    !! - initialise the behaviour using the the_behaviour::freeze::init()
    !!   method.
    call this%freeze%init()

    !> Set the currently executed behaviour label. It is from the
    !! the_behaviour::behaviour_base::label data component of the base class.
    this%behaviour_label = this%freeze%label

    !> Set the execution status for all behaviours to FALSE and then for this
    !! specific behaviour to TRUE. Only one behaviour unit can be executed at
    !! a time.
    call this%deactivate()
    this%freeze%is_active = .TRUE.

    !> - execute the behaviour with the_behaviour::freeze::execute() method.
    !! .
    call this%freeze%execute( this )

    !> Update (add to stack) the agent's history of behaviours
    !! the_behaviour::behaviour::history_behave: string labels of the
    !! behaviours are are saved.
    call add_to_history( this%history_behave, this%freeze%label )

    !> Finally, update the positional history stack
    !! the_environment::spatial_moving::history: the current spatial
    !! position of the agent is re-saved in the history stack using
    !! the_environment::spatial_moving::repeat_position() method.
    !! @note Re-saving the current position is necessary to keep the
    !!       full positional history even for the behaviours that do not
    !!       involve spatial displacement (movement).
    call this%repeat_position()

  end subroutine behaviour_do_freeze

  !-----------------------------------------------------------------------------
  !> Perform (execute) the the_behaviour::escape_dart behaviour.
  subroutine behaviour_do_escape_dart(this, predator_object)
    class(BEHAVIOUR), intent(inout) :: this
    !> @param[in] predator_object optional predator object, if present, it is
    !!            assumed the actor agent tries to actively escape from this
    !!            specific predator.
    class(SPATIAL), optional, intent(in) :: predator_object

    !> ### Implementation notes ###
    !! - Initialise the behaviour using the the_behaviour::escape_dart::init()
    !!   method.
    call this%escape_dart%init()

    !> Set the currently executed behaviour label. It is from the
    !! the_behaviour::behaviour_base::label data component of the base class.
    this%behaviour_label = this%escape_dart%label

    !> Set the execution status for all behaviours to FALSE and then for this
    !! specific behaviour to TRUE. Only one behaviour unit can be executed at
    !! a time.
    call this%deactivate()
    this%escape_dart%is_active = .TRUE.

    !> - Execute the behaviour with the_behaviour::escape_dart::execute()
    !!   method. Note that if the target predator object is not provided, a
    !!   default predator with the size commondata::predator_body_size is
    !!   assumed. See the_behaviour::escape_dart_do_this() for details.
    !! .
    if (present(predator_object)) then
      call this%escape_dart%execute( this, predator_object )
    else
      call this%escape_dart%execute( this )
    end if

    !> Update (add to stack) the agent's history of behaviours
    !! the_behaviour::behaviour::history_behave: string labels of the
    !! behaviours are are saved.
    call add_to_history( this%history_behave, this%escape_dart%label )

  end subroutine behaviour_do_escape_dart

  !-----------------------------------------------------------------------------
  !> Approach a specific the_environment::spatial class target, i.e. execute
  !! the the_behaviour::approach behaviour. The target is either a
  !! conspecific from the perception (the_neurobio::conspec_percept_comp class)
  !! or any arbitrary the_environment::spatial class object.
  subroutine behaviour_do_approach(this, target_object, is_random, target_offset)
    class(BEHAVIOUR), intent(inout) :: this
    !> @param[in] target_object is the spatial target object the actor agent
    !!            is going to approach.
    class(SPATIAL), intent(in) :: target_object
    !> @param[in] is_random indicator flag for random correlated walk. If
    !!            present and is TRUE, the agent approaches to the
    !!            `target_object` in form of random correlated walk (see
    !!            the_environment::spatial_moving::corwalk()), otherwise
    !!            directly.
    logical, optional, intent(in) :: is_random
    !> @param[in] target_offset is an optional offset for the target, so that
    !!            the target position of the approaching agent does not
    !!            coincide with the target object. If absent, a default value
    !!            set by the commondata::approach_offset_default is used.
    !!            For the the_behaviour::approach_conspec, the default value
    !!            is as an average of the agent and target conspecific body
    !!            lengths.
    real(SRP), optional, intent(in) :: target_offset

    ! Local copies of optionals
    logical :: is_random_walk
    real(SRP) :: target_offset_here

    !> ### Implementation details ###
    !> - Check the optional parameter flag: `is_random`: if the parameter is
    !!   set to TRUE, a random Gaussian walk towards the target object is done,
    !!   otherwise a direct direct approach towards the target object leaving
    !!   the target offset distance is performed.
    !! .
    if (present(is_random)) then
      is_random_walk = is_random
    else
      is_random_walk = .FALSE.
    end if

    !> Check the type of the target object. Different targets are processed
    !! differently for approach.
    CONSPEC_OTHER: select type (target_object)

      !> - If it is of the class  the_neurobio::conspec_percept_comp (i.e.
      !!   conspecific perception object):
      !!   - the default target offset is set to the average body sizes of
      !!     the agent and its target conspecific;
      !!   - The the_behaviour::approach_conspec behaviour class is initialised
      !!     with the the_behaviour::approach_conspec::init() method;
      !!   - Finally, approach to the conspecific is executed with the
      !!     the_behaviour::approach_conspec::execute() method.
      !!   .
      class is (CONSPEC_PERCEPT_COMP)                           CONSPEC_OTHER

        if (present(target_offset)) then
          target_offset_here = target_offset
        else
          target_offset_here = ( this%get_length() +                          &
                                   get_prop_size(target_object) ) / 2.0_SRP
        end if
        call this%approach_conspec%init()
        this%behaviour_label = this%approach_conspec%label
        call this%deactivate()
        this%approach_conspec%is_active = .TRUE.
        call this%approach_conspec%execute(this, target_object,               &
                                           is_random_walk, target_offset_here)

        !> Update (add to stack) the agent's history of behaviours
        !! the_behaviour::behaviour::history_behave: string labels of the
        !! behaviours are are saved.
        call add_to_history( this%history_behave, this%approach_conspec%label )

      !> - If, on the other hand,  the target object is of the any other
      !!   class (i.e. it is an arbitrary object):
      !!   - The default target offset is set by the the parameter constant
      !!     commondata::approach_offset_default;
      !!   - The the_behaviour::approach_spatial behaviour class is initialised
      !!     with the the_behaviour::approach_spatial::init() method;
      !!   - Finally, approach to the conspecific is executed with the
      !!     the_behaviour::approach_spatial::execute() method.
      !!   .
      class default                                             CONSPEC_OTHER

        if (present(target_offset)) then
          target_offset_here = target_offset
        else
          target_offset_here = APPROACH_OFFSET_DEFAULT
        end if
        call this%approach_spatial%init()
        this%behaviour_label = this%approach_spatial%label
        call this%deactivate()
        this%approach_spatial%is_active = .TRUE.
        call this%approach_spatial%execute(this, target_object,               &
                                           is_random_walk, target_offset_here)

        !> Update (add to stack) the agent's history of behaviours
        !! the_behaviour::behaviour::history_behave: string labels of the
        !! behaviours are are saved.
        call add_to_history( this%history_behave, this%approach_spatial %label )

    end select CONSPEC_OTHER

  end subroutine behaviour_do_approach

  !-----------------------------------------------------------------------------
  !> Perform (execute) the the_behaviour::migrate (migration) behaviour.
  subroutine behaviour_do_migrate(this, target_env)
    class(BEHAVIOUR), intent(inout) :: this
    !> @param[in] target_env the target environment the actor agent is going
    !!            to (e)migrate into.
    class(ENVIRONMENT), intent(in) :: target_env

    !> ### Implementation notes ###
    !> - Initialise the the_behaviour::migrate behaviour component
    !!   using the the_behaviour::migrate::init() method.
    call this%migrate%init()

    !> Set the currently executed behaviour label. It is from the
    !! the_behaviour::behaviour_base::label data component of the base class.
    this%behaviour_label = this%migrate%label

    !> Set the execution status for all behaviours to FALSE and then for this
    !! specific behaviour to TRUE. Only one behaviour unit can be executed at
    !! a time.
    call this%deactivate()
    this%migrate%is_active = .TRUE.

    !> - The "migrate" behaviour is executed by the
    !!   the_behaviour::migrate::execute() method.
    !! .
    call this%migrate%execute( this, target_env )

    !> Update (add to stack) the agent's history of behaviours
    !! the_behaviour::behaviour::history_behave: string labels of the
    !! behaviours are are saved.
    call add_to_history( this%history_behave, this%migrate%label )

  end subroutine behaviour_do_migrate

  !-----------------------------------------------------------------------------
  !> Perform a simplistic random migration. If the agent is within a specific
  !! distance to the target environment, it emigrates there with a specific
  !! fixed probability.
  function behaviour_try_migrate_random( this, target_env, max_dist, prob )    &
                                                           result (is_migrated)
    class(BEHAVIOUR), intent(inout) :: this
    !> @param[in] target_env the target environment the actor agent is going
    !!            to (e)migrate into.
    class(ENVIRONMENT), intent(in) :: target_env
    !> @param[in] max_dist Optional maximum distance, in units of the agent's
    !!            body size, towards the target environment when the agent
    !!            can (probabilistically) emigrate into it.
    real(SRP), optional, intent(in) :: max_dist
    !> @param[in] prob Probability of migration
    real(SRP), optional, intent(in) :: prob
    !> @returns Logical flag that shows if the agent has actually emigrated
    !!          (TRUE) or not (FALSE).
    logical ::  is_migrated

    ! Local copies of optionals.
    real(SRP) :: max_dist_loc, prob_loc

    !> ### Notable variables ###
    !> - **point_target_env** is the target point inside the target
    !!   environment to which this agent is going to relocate.
    type(SPATIAL) :: point_target_env
    !> - **distance_target** is the distance to the target environment.
    real(SRP) :: distance_target
    !> - **MAX_DIST_DEFAULT** is the default maximum distance towards the
    !!   target environment (units of the agent's body size) when the agent
    !!   can emigrate into it. This default distance is set by the parameter
    !!   commondata::migrate_random_max_dist_target. However, note that the
    !!   migration is probabilistic and occurs with the probability `prob`.
    !! .
    real(SRP), parameter :: MAX_DIST_DEFAULT = MIGRATE_RANDOM_MAX_DIST_TARGET

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME = "(behaviour_do_migrate_random)"

    !> ### Implementation notes ###
    !> The function returns FALSE whenever the agent has not actually migrated
    !! into the target environment.
    is_migrated = .FALSE.

    !> #### Optional parameters ####
    !! Optional parameters `max_dist` and `prob` are checked and the default
    !! values are set in case any of them is absent.
    !! - `max_dist` = `MAX_DIST_DEFAULT`
    !!    (commondata::migrate_random_max_dist_target)
    !! - `prob` = 0.5.
    !! .
    if (present(max_dist)) then
      max_dist_loc = max_dist
    else
      max_dist_loc = MAX_DIST_DEFAULT
    end if
    if (present(prob)) then
      prob_loc = prob
    else
      prob_loc = 0.5_SRP
    end if

    !> #### Calculate the distance towards the target environment ####
    !> First, determine the nearest target point within the target environment
    !! and calculate the distance to the target point.
    !!
    !> The distance towards the target environment (and the target point in
    !! this environment) is defined as the minimum  distance towards
    !! all segments limiting this environment in the 2D X x Y projection
    !! @warning This is valid only for the simple box environment
    !!          implementation. Generally, it equals to the minimum
    !!          distance across all the polyhedrons limiting the target
    !!          environment).
    !!
    !! The target point for the migrating agent within the target
    !! environment is then not just the edge of the target environment, but
    !! some point penetrating inside to some distance defined by the parameter
    !! commondata::migrate_dist_penetrate_offset (in units of the agent's
    !! body length). The the_environment::environment::nearest_target()
    !! method is used to find the closest point in the target environment and
    !! the (smallest) distance towards this environment, these values are
    !! adjusted automatically for the offset parameter in the procedure call.
    call target_env%nearest_target( outside_object=this,                      &
                                    offset_into=this%get_length() *           &
                                            MIGRATE_DIST_PENETRATE_OFFSET,    &
                                    point_spatial = point_target_env,         &
                                    point_dist = distance_target  )

    !> #### Move to the target environment with probability "prob" ####
    !> If the distance towards the target environment does not exceed
    !! `max_dist` body lengths of the agent, the agent can move into
    !! this target environment, exactly to the target point `point_target_env`
    !! with the probability `prob`.
    if ( distance_target < max_dist_loc * this%get_length() ) then
      if ( RAND() < prob_loc ) then
        call LOG_DBG( LTAG_INFO // "Agent is about to migrate to " //         &
                      TOSTR([ point_target_env%xpos(),                        &
                              point_target_env%ypos(),                        &
                              point_target_env%dpos()]), PROCNAME, MODNAME )
        call this%position( point_target_env%location() )
        !> - If the agent has emigrated into the target environment, the
        !! output logical flag `is_migrated` is set to TRUE. (Otherwise, it is
        !! always FALSE.)
        is_migrated = .TRUE.
      else
        return
      end if
    else
      return
    end if

    !> ##### Process the cost of movement #####
    !> This only concerns the cases when the agent had migrated into the
    !! target environment `target_env`.
    !> - Reset the body mass of the agent subtracting the actual cost of
    !!   the migration moving that is automatically calculated in the call to
    !!   the_body::condition::cost_swim(). The the_body::condition::set_mass()
    !!   method is used here to adjust the mass.
    call this%set_mass( value_set = this%get_mass() -                         &
                           this%cost_swim(exponent=                           &
                                            SWIMMING_COST_EXPONENT_LAMINAR),  &
                        update_history = .TRUE. )

    !> - Additionally, also call the `the_body::condition::set_length()` method
    !!   to update the body length history stack. However, the value_set
    !!   parameter here is just the current value. This fake re-setting of the
    !!   body length is done to keep both mass and length synchronised in their
    !!   history stack arrays (there is no procedure for only updating history).
    call this%set_length(value_set = this%get_length(),update_history = .TRUE.)

    !> - After resetting the body mass, update energy reserves of the agent,
    !!   that depend on both the length and the mass.
    !! .
    call this%energy_update()

    !> Finally, check if the agent is starved to death. If yes, the agent can
    !! die without going any further.
    if (this%starved_death()) call this%dies()

  end function behaviour_try_migrate_random

  !-----------------------------------------------------------------------------
  !> Perform (execute) the the_behaviour::go_down_depth (go down) behaviour.
  subroutine behaviour_do_go_down(this, depth_walk)
    class(BEHAVIOUR), intent(inout) :: this
    !> @param[in] depth_walk Optional downward walk size, by how deep
    !!            the agent goes down.
    real(SRP), intent(in), optional :: depth_walk

    !> ### Implementation notes ###
    !> - Initialise the the_behaviour::go_down_depth behaviour component
    !!   using the the_behaviour::go_down_depth::init() method.
    call this%depth_down%init()

    !> Set the currently executed behaviour label. It is from the
    !! the_behaviour::behaviour_base::label data component of the base class.
    this%behaviour_label = this%depth_down%label

    !> Set the execution status for all behaviours to FALSE and then for this
    !! specific behaviour to TRUE. Only one behaviour unit can be executed at
    !! a time.
    call this%deactivate()
    this%depth_down%is_active = .TRUE.

    !> - The "go down" behaviour is executed by calling the
    !!   the_behaviour::go_down_depth::execute() method. Note that
    !!   the walk length can be provided by dummy parameter `depth_walk`,
    !!   otherwise the default step is used that is equal to the
    !!   commondata::up_down_walk_step_stdlength_factor times of
    !!   the agent body length.
    !! .
    if (present(depth_walk)) then
      call this%depth_down%execute( this, depth_walk = depth_walk )
    else
      call this%depth_down%execute( this )
    end if

    !> Update (add to stack) the agent's history of behaviours
    !! the_behaviour::behaviour::history_behave: string labels of the
    !! behaviours are are saved.
    call add_to_history( this%history_behave, this%depth_down%label )

  end subroutine behaviour_do_go_down

  !-----------------------------------------------------------------------------
  !> Perform (execute) the the_behaviour::go_up_depth (go up) behaviour.
  subroutine behaviour_do_go_up(this, depth_walk)
    class(BEHAVIOUR), intent(inout) :: this
    !> @param[in] depth_walk Optional downward walk size, by how deep
    !!            the agent goes up.
    real(SRP), intent(in), optional :: depth_walk

    !> ### Implementation notes ###
    !> - Initialise the the_behaviour::go_up_depth behaviour component
    !!   using the the_behaviour::go_up_depth::init() method.
    call this%depth_up%init()

    !> Set the currently executed behaviour label. It is from the
    !! the_behaviour::behaviour_base::label data component of the base class.
    this%behaviour_label = this%depth_up%label

    !> Set the execution status for all behaviours to FALSE and then for this
    !! specific behaviour to TRUE. Only one behaviour unit can be executed at
    !! a time.
    call this%deactivate()
    this%depth_up%is_active = .TRUE.

    !> - The "go up" behaviour is executed by calling the
    !!   the_behaviour::go_up_depth::execute() method. Note that
    !!   the walk length can be provided by dummy parameter `depth_walk`,
    !!   otherwise the default step is used that is equal to the
    !!   commondata::up_down_walk_step_stdlength_factor times of
    !!   the agent body length.
    !! .
    if (present(depth_walk)) then
      call this%depth_up%execute( this, depth_walk = depth_walk )
    else
      call this%depth_up%execute( this )
    end if

    !> Update (add to stack) the agent's history of behaviours
    !! the_behaviour::behaviour::history_behave: string labels of the
    !! behaviours are are saved.
    call add_to_history( this%history_behave, this%depth_up%label )


  end subroutine behaviour_do_go_up

  !-----------------------------------------------------------------------------
  !> Cleanup the behaviour history stack for the agent. All values are empty.
  elemental subroutine behaviour_cleanup_history(this)
    class(BEHAVIOUR), intent(inout) :: this

    this%history_behave = ""

  end subroutine behaviour_cleanup_history

  !-----------------------------------------------------------------------------
  !> Select and **execute** the optimal behaviour, i.e. the behaviour which
  !! minimizes the expected GOS arousal.
  !! @note Note that the "select" method should be called **after** the
  !!       the_neurobio::perception, the_neurobio::appraisal and the Global
  !!       Organismic State (the_neurobio::gos_global) objects were obtained.
  subroutine behaviour_select_optimal(  this, rescale_max_motivation,         &
                                        food_resource_real )
    class(BEHAVIOUR), intent(inout) :: this
    !> @param[inout] food_resource_real The food resource the agent is eating
    !!               the food item in. Note that it could be a joined food
    !!               resource composed with the_environment::join() procedure
    !!               for assembling several habitats into the
    !!               the_environment::global_habitats_available array or
    !!               resources collapsed using the
    !!               the_environment::food_resource::join() method.
    class(FOOD_RESOURCE), optional, intent(inout) :: food_resource_real
    !> @param[in] rescale_max_motivation maximum motivation value for
    !!            rescaling all motivational components for comparison
    !!            across all motivation and perceptual components and behaviour
    !!            units.
    real(SRP), optional, intent(in) :: rescale_max_motivation

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME = "(behaviour_select_optimal)"

    ! Local copies of optionals
    real(SRP) :: rescale_max_motivation_here

    !> ### Notable local variables ###
    !> - **expected_gos_debug_base** is the GOS expectancy for the fake debug
    !!   behaviour unit the_behaviour::debug_base: it does not depend on any
    !!   fake perceptions and represents a baseline estimate. This behaviour
    !!   unit also does not participate in the procedure that selects the
    !!   minimum arousal.
    real(SRP) :: expected_gos_debug_base
    !> - **expected_gos_eat** is the GOS expectancy value predicted from
    !!   eating the optimal food item.
    real(SRP) :: expected_gos_eat
    !> - **food_item_selected** is the optimal food item selected from all
    !!   those that are currently within the perception object of the agent.
    integer :: food_item_selected

    !> - **expected_gos_reproduce** is the GOS expectancy value predicted
    !!   from reproduction.
    real(SRP) :: expected_gos_reproduce

    !> - **expected_gos_walk** is the GOS expectancy value predicted
    !!   from the Gaussian random walk of the optimal step size.
    real(SRP) :: expected_gos_walk
    !> - **walk_distance_selected** - the static step (from values in the
    !!   commondata::behav_walk_step_stdlen_static array).
    real(SRP) :: walk_distance_selected

    !> - **expected_gos_freeze** is the GOS expectancy value predicted
    !!   from freezing.
    real(SRP) :: expected_gos_freeze

    !> - **expected_gos_escape** is the GOS expectancy value predicted
    !!   from escape movement.
    real(SRP) :: expected_gos_escape
    !> - **predator_selected_n** - the predator object within the perception,
    !!   that is associated with the lowest GOS arousal of escape, i.e. the
    !!   most subjectively dangerous predator for the agent. Thus is actually
    !!   the *number* of the predator within the perception object.
    integer :: predator_selected_n

    !> - **expected_gos_approach_conspec** is the GOS expectancy value
    !!   predicted from the approach to conspecific behaviour.
    real(SRP) :: expected_gos_approach_conspec
    !> - **conspec_selected_n** - the conspecific object within the perception,
    !!   that is associated with the lowest GOS arousal of approach, i.e. the
    !!   most subjectively attractive conspecific for the agent. Thus is
    !!   actually the *number* of the conspecific within the perception object.
    integer :: conspec_selected_n

    !> - **expected_gos_migrate** is the GOS expectancy value predicted from
    !!   migration behaviour into the optimal habitat, i.e. the habitat within
    !!   the array of available habitats commondata::global_habitats_available
    !!    that minimises the linked GOS arousal.
    real(SRP) :: expected_gos_migrate
    !> - **habitat_selected_n** - the number of the habitat object within the
    !!   commondata::global_habitats_available array, that is associated with
    !!   the lowest GOS arousal of the migration behaviour, i.e. the most
    !!   subjectively attractive habitat for the agent.
    integer :: habitat_selected_n

    !> - **expected_gos_depth_down** is the GOS expectancy value predicted
    !!   from the downward vertical migration with the optimal step size.
    real(SRP) :: expected_gos_depth_down
    !> - **go_down_distance_selected** - the static step size for the downwards
    !!   vertical migration (from values in the
    !!   commondata::behav_go_up_down_step_stdlen_static array).
    real(SRP) :: go_down_distance_selected

    !> - **expected_gos_depth_up** is the GOS expectancy value predicted
    !!   from the upward vertical migration with the optimal step size.
    real(SRP) :: expected_gos_depth_up
    !> - **go_up_distance_selected** - the static step size for the upwards
    !!   vertical migration (from values in the
    !!   commondata::behav_go_up_down_step_stdlen_static array).
    real(SRP) :: go_up_distance_selected

    !> - **expected_gos_all** is the array that contains GOS arousal values
    !!   for all of the behaviours that count when calculating the minimum.
    !! .
    !> @warning Automatic allocation of the `expected_gos_all` array might not
    !!          work on all compilers and platforms. If manually allocated,
    !!          check the exact number of behaviour units.
    real(SRP), allocatable, dimension(:) :: expected_gos_all

    ! A very big positive number that is used for GOS arousal values that
    ! should never be able to win in the arousal minimisation procedure. This
    ! parameter is also used for initialisations.
    real(SRP), parameter :: BIG_NEVER_WINS = -1.0_SRP * MISSING

    ! Local number of food resource within the global array
    ! the_environment::global_habitats_available.
    integer :: fres_num

    !> ### Implementation details ###
    !> #### Checks and preparations ####
    !> Determine optional parameter `rescale_max_motivation`. If it is absent
    !! from the parameter list, the value is calculated from the current
    !! perception using the the_neurobio::motivation::max_perception() method.
    if (present(rescale_max_motivation)) then
      rescale_max_motivation_here = rescale_max_motivation
    else
      rescale_max_motivation_here = this%motivations%max_perception()
    end if

    !> #### Calculate the motivational expectancies ####
    !> First, the expectancies of the GOS arousal from each of the available
    !! behaviour units are calculated.
    !> - **Debug base fake behaviour**  (the_behaviour::debug_base) calling
    !!  ::debug_base_select(). This behaviour does not enter in the
    !!  competition of behaviour units for arousal minimisation and is useful
    !!  only in the @ref intro_debug_mode "debug mode".
    if (IS_DEBUG) call debug_base_select(expected_gos_debug_base)

    !> - **Eat food** (the_behaviour::eat_food) calling ::eat_food_select().
    call eat_food_select(expected_gos_eat, food_item_selected)

    !> - **Reproduce** (the_behaviour::reproduce) calling ::reproduce_select().
    call reproduce_select(expected_gos_reproduce)

    !> - **Random walks** (the_behaviour::walk_random) calling
    !!   ::walk_random_select().
    call walk_random_select(expected_gos_walk, walk_distance_selected)
          LOG_STEP: block
            real(SRP) :: walk_dist_fi
            if ( is_near_zero(this%memory_stack%get_food_mean_dist()) ) then
              walk_dist_fi = MISSING
            else
              walk_dist_fi = walk_distance_selected /                         &
                                        this%memory_stack%get_food_mean_dist()
            end if
            call LOG_DBG( LTAG_INFO // "Optimal walk step: " //               &
                          TOSTR(walk_distance_selected) // ", SL units: " //  &
                          TOSTR(walk_distance_selected/this%get_length()) //  &
                          ", units of average distance food items: " //       &
                          TOSTR(walk_dist_fi), PROCNAME, MODNAME )
          end block LOG_STEP

    !> - **Freezing** (the_behaviour::freeze) calling ::freeze_select().
    call freeze_select(expected_gos_freeze)

    !> - **Escape** (the_behaviour::escape_dart) calling ::escape_dart_select().
    call escape_dart_select(expected_gos_escape, predator_selected_n)

    !> - **Approach to a spatial object** (the_behaviour::apprach): Approach
    !!   to an arbitrary spatial object is not used in this version, this
    !!   behaviour is never executed.

    !> - **Approach conspecifics** (the the_behaviour::approach_conspec)
    !!   calling ::approach_consp_select().
    call approach_consp_select(expected_gos_approach_conspec,conspec_selected_n)

    !> - **Migrate** (the_behaviour::migrate) calling ::migrate_select().
    call migrate_select(expected_gos_migrate, habitat_selected_n)

    !> - **Go down** (the_behaviour::go_down_depth) calling ::go_down_select().
    call go_down_select(expected_gos_depth_down, go_down_distance_selected)
          call LOG_DBG( LTAG_INFO // "Optimal walk step down: " //            &
                        TOSTR(go_down_distance_selected) // ", sl units: " // &
                        TOSTR(go_down_distance_selected/this%get_length()),   &
                        PROCNAME, MODNAME )

    !> - **Go up** (the the_behaviour::go_up_depth) calling ::go_up_select().
    !! .
    call go_up_select(expected_gos_depth_up, go_up_distance_selected)
          call LOG_DBG( LTAG_INFO // "Optimal walk step up: " //              &
                        TOSTR(go_up_distance_selected) // ", sl units: " //   &
                        TOSTR(go_up_distance_selected/this%get_length()),     &
                        PROCNAME, MODNAME )

    !> #### Execute behaviours that minimise GOS arousal ####
    !> After the GOS arousal values for all behaviour units are calculated,
    !! the agent can determine the minimum value and what is the associated
    !! behaviour unit that minimises the GOS arousal.
    !!
    !! First, an array containing all GOS arousal values for all of the above
    !! behaviour units is constructed `expected_gos_all`.
    !! @note Note that there is no `allocate` command here as all fairly modern
    !!       Fortran compilers support automatic allocation of arrays on
    !!       intrinsic assignment. This feature should work by default in
    !!       GNU gfortran v.4.6 and Intel ifort v.17.0.1. Automatic allocation
    !!       allows to avoid a possible bug when the number of array elements
    !!       in the `allocate` statement is not updated when the `expected_gos_`
    !!       components of the array are added or removed.
    expected_gos_all = [  expected_gos_eat,                                   &
                          expected_gos_reproduce,                             &
                          expected_gos_walk,                                  &
                          expected_gos_freeze,                                &
                          expected_gos_escape,                                &
                          expected_gos_approach_conspec,                      &
                          expected_gos_migrate,                               &
                          expected_gos_depth_down,                            &
                          expected_gos_depth_up         ]

    !> Automatic array allocation is checked. If the ` expected_gos_all array
    !! turns out not allocated, a critical error is signalled in the logger.
    if (.not. allocated(expected_gos_all) ) then
      call LOG_MSG( LTAG_CRIT // "Automatic array allocation is not "   //    &
                    "enabled or supported by the compiler. Check "      //    &
                    PROCNAME // " code and insert explicit"             //    &
                    "'allocate(expected_gos_all(N))' or use compiler "  //    &
                    "switch  to enable F2003 features." )
      call system_halt(is_error=.TRUE., message=ERROR_NO_AUTOALLOC )
    end if

    !> In the @ref intro_debug_mode "DEBUG mode", the array of the GOS
    !! arousal levels is logged.
    if (IS_DEBUG) call LOG_DELIMITER(LOG_LEVEL_CHAPTER)
    call LOG_DBG( LTAG_INFO // "  +++ GOS arousal array: " //                 &
                  TOSTR(expected_gos_all) //                                  &
                  ", agent label: " // this%individ_label() //                &
                  ", minimum # " // TOSTR(minloc(expected_gos_all)) //        &
                  " is value: " // TOSTR(minval(expected_gos_all)) )
    if (IS_DEBUG) call LOG_DELIMITER(LOG_LEVEL_CHAPTER)

    !> Second, each of the behaviours is checked for being the minimum value.
    !! If true, this behaviour is executed using the `do_` method of the
    !! the_behaviour::behaviour class.
    !!
    !! Additionally, for each behaviour unit, an additional check is performed
    !! to make sure the conditions for the behaviour are satisfied. If the
    !! conditions are not satisfied, a default Gausssian random walk
    !! the_behaviour::behaviour::do_walk() is done.
    !!
    !! The correctness conditions for each of the behaviour units are:
    !> - the_behaviour::eat_food: the agent must have food items in perception,
    !!   the_neurobio::perception::has_food() is TRUE.
    if ( is_minval( expected_gos_eat, expected_gos_all ) ) then
      if (this%has_food()) then
        if (present(food_resource_real)) then
          call this%do_eat_food_item( food_item_selected, food_resource_real )
        else
          fres_num = this%find_environment(Global_Habitats_Available)
          call this%do_eat_food_item( food_item_selected,                     &
                                      Global_Habitats_Available(fres_num)%food )
        end if
      else
        call this%do_walk()
      end if
      return
    end if

    !> - the_behaviour::reproduce: the agent must be mature
    !!   (the_body::reproduction::is_ready_reproduce() is TRUE) *and* have
    !!   conspecifics in perception (the_neurobio::perception::has_consp()
    !!   is TRUE).
    if ( is_minval( expected_gos_reproduce, expected_gos_all ) ) then
      if (this%is_ready_reproduce() .and. this%has_consp()) then
        call this%do_reproduce()
      else
        call this%do_walk()
      end if
      return
    end if

    !> - the_behaviour::walk_random: the optimal distance selected
    !!   `walk_distance_selected` must be nonzero, i.e. exceed the tolerance
    !!   value commondata::tolerance_high_def_srp.
    if ( is_minval( expected_gos_walk, expected_gos_all ) ) then
      if (walk_distance_selected > TOLERANCE_HIGH_DEF_SRP ) then
        call this%do_walk( walk_distance_selected )
      else
        call this%do_walk()
      end if
      return
    end if

    !> - the_behaviour::freeze: this behaviour does not require any specific
    !!   conditions and can be executed anyway.
    if ( is_minval( expected_gos_freeze, expected_gos_all ) ) then
      call this%do_freeze()
      return
    end if

    !> - the_behaviour::escape_dart: the agent must have predators in its
    !!   perception, i.e. the_neurobio::perception::has_pred() should return
    !!   TRUE. If no predators are present, a non-targeted
    !!   the_behaviour::escape_dart instance is esecuted.
    if ( is_minval( expected_gos_escape, expected_gos_all ) ) then
      if (this%has_pred()) then
        call this%do_escape( predator_object =                                &
                this%perceive_predator%predators_seen(predator_selected_n) )
      else
        call this%do_escape( )
      end if
      return
    end if

    !> - the_behaviour::approach_conspec: the agent must have conspecifics in
    !!   perception, the_neurobio::perception::has_consp() is TRUE.
    if ( is_minval( expected_gos_approach_conspec, expected_gos_all ) ) then
      if (this%has_consp()) then
        call this%do_approach(                                                &
              target_object =                                                 &
                    this%perceive_consp%conspecifics_seen(conspec_selected_n),&
              is_random = .FALSE. )
      else
        call this%do_walk()
      end if
      return
    end if

    !> - the_behaviour::migrate: the agent must have a valid target habitat for
    !!   migration; the optimal habitat index `habitat_selected_n` must
    !!   correspond to a valid habitat in the global array
    !!   the_environment::global_habitats_available.
    if ( is_minval( expected_gos_migrate, expected_gos_all ) ) then
      if ( habitat_selected_n > 0 .and.                                       &
           habitat_selected_n <= size(Global_Habitats_Available) ) then
        call this%do_migrate(                                                 &
                  target_env = Global_Habitats_Available(habitat_selected_n) )
      else
        call this%do_walk()
      end if
      return
    end if

    !> - the_behaviour::go_down_depth: the optimal vertical migration distance
    !!   selected `go_down_distance_selected` must be nonzero, i.e. exceed the
    !!   tolerance value commondata::tolerance_high_def_srp.
    if ( is_minval( expected_gos_depth_down, expected_gos_all ) ) then
      if ( go_down_distance_selected > TOLERANCE_HIGH_DEF_SRP ) then
        call this%do_go_down( depth_walk = go_down_distance_selected  )
      else
        call this%do_walk()
      end if
      return
    end if

    !> - the_behaviour::go_up_depth: the optimal vertical migration distance
    !!   selected `go_up_distance_selected` must be nonzero, i.e. exceed the
    !!   tolerance value commondata::tolerance_high_def_srp.
    !! .
    if ( is_minval( expected_gos_depth_up, expected_gos_all ) ) then
      if ( go_up_distance_selected > TOLERANCE_HIGH_DEF_SRP ) then
        call this%do_go_up( depth_walk = go_up_distance_selected  )
      else
        call this%do_walk()
      end if
      return
    end if

    !> The control is passed back out of this procedure on execution of the
    !! optimal behaviour. However, if no behaviour was selected up to this
    !! point, the agent just does a default Gaussian walk. However, this
    !! situation is very suspicious and can point to a bug. Therefore, such
    !! situation is logged with the ERROR tag.
    call LOG_MSG( LTAG_ERROR // "Cannot select optimal behaviour unit in " // &
                  PROCNAME // ". Default random Gaussian walk is executed." )

    call this%do_walk()

  contains
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !> Calculate the expected GOS arousal that would be predicted from execution
  !! of the the_behaviour::eat_food behaviour unit. The subjectively optimal
  !! food item (that minimises GOS arousal) is also obtained in this procedure.
  !! @note This procedure is part of the_behaviour::behaviour_select_optimal()
  !!       procedure and called within.
  subroutine eat_food_select(expected_gos, selected)
    !> @param[out] expected_gos is the GOS expectancy value predicted from
    !!             eating the optimal food item.
    real(SRP), intent(out) :: expected_gos
    !> @param[out] selected optimal food item that would result in
    !!             the minimum resulting GOS arousal.
    integer, intent(out) :: selected

    !> ### Implementation details ###
    !> First, the the_behaviour::eat_food behaviour class is initialised by
    !! calling the the_behaviour::eat_food::init() method.
    call this%eat%init()

    !> Then, perception components of the food objects are processed.
    !! If the agent has any food items in perception, then
    if ( this%has_food() ) then
      !> - determine the best, optimal food item out of all the items
      !!   currently in perception object of the agent: this is the food item
      !!   that would result in the *minimum expected arousal*
      !!   the_behaviour::behaviour::food_item_select();
      selected = this%food_item_select(                                       &
                        rescale_max_motivation = rescale_max_motivation_here )
      !> - calculate the overall motivational expectancy that eating this
      !!   optimal food item would provide. This value is now the *arousal
      !!   expectancy* from eating behaviour (the_behaviour::eat_food) by call
      !!   to the the_behaviour::eat_food::expectancies_calculate() method.
      !! .
      call this%eat%expectancies_calculate( this_agent = this,                &
                      food_item_eaten = this%perceive_food%foods_seen(        &
                                                                  selected),  &
                      rescale_max_motivation = rescale_max_motivation_here )
      expected_gos = this%eat%arousal_expected            ! %gos_expected()
    !> On the other hand, if the agent has no food items in its perception
    !! object, the motivational expectancy is set to a large value that is
    !! guaranteed to not win, so that this behaviour cannot be executed.
    else
      expected_gos = BIG_NEVER_WINS
      selected = 0
    end if

    DEBUG_LOG: block
      call LOG_DBG( LTAG_INFO // "Selected optimal food item number: "     // &
                    TOSTR(selected) // ", out of total "                   // &
                    TOSTR(this%perceive_food%food_seen_count)              // &
                    " items in perception." , PROCNAME, MODNAME )
    end block DEBUG_LOG

  end subroutine eat_food_select

  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !> Calculate the expected GOS arousal that would be predicted from execution
  !! of the the_behaviour::reproduce behaviour unit.
  !! @note This procedure is part of the_behaviour::behaviour_select_optimal()
  !!       procedure and called within.
  subroutine reproduce_select(expected_gos)
    !> @param[out] expected_gos is the GOS expectancy value
    !!             predicted from reproduction.
    real(SRP), intent(out) :: expected_gos

    !> ### Implementation notes ###
    !> Calculation is rather straightforward here. It involves calling the
    !! the the_behaviour::reproduce::expectancies_calculate() method.
    if ( this%has_consp() .and. this%is_ready_reproduce() ) then
      call this%reproduce%expectancies_calculate( this_agent = this,          &
                        rescale_max_motivation = rescale_max_motivation_here )
      expected_gos = this%reproduce%arousal_expected  ! %gos_expected()
    else
      expected_gos = BIG_NEVER_WINS
    end if

  end subroutine reproduce_select

  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !> Calculate the expected GOS arousal that would be predicted from execution
  !! of the the_behaviour::walk_random behaviour unit. The best (subjectively
  !! optimal) walk step from the commondata::behav_walk_step_stdlen_static
  !! parameter array values (that minimises GOS arousal) is also obtained in
  !! this procedure.
  !! @note This procedure is part of the_behaviour::behaviour_select_optimal()
  !!       procedure and called within.
  subroutine walk_random_select(expected_gos, selected)
    !> @param[out] expected_gos is the GOS expectancy value predicted
    !!             from the Gaussian random walk of the optimal step size.
    real(SRP), intent(out) :: expected_gos
    !> @param[out] selected the static step (from values in the
    !!             commondata::behav_walk_step_stdlen_static array).
    real(SRP), intent(out) :: selected

    ! Local counter.
    integer :: walk_step
    ! Local temporary value of the walk.
    real(SRP) :: walk_current

    !> ### Implementation notes ###
    !> There are several random walks with different step sizes that are
    !! defined by the commondata::behav_walk_step_stdlen_static parameter array
    !! (i.e. a *repertoire* of walks). Therefore, selection of the arousal
    !! expectancy that would follow from the_behaviour::walk_random behaviour
    !! as a whole requires finding the *optimal walk step* among all those
    !! defined in the repertoire (commondata::behav_walk_step_stdlen_static).
    !! Such an optimal walk step size is the step size that would result in
    !! the lowest expected arousal. This is done by looping over the values of
    !! the walk step size repertoire, commondata::behav_walk_step_stdlen_static.
    !  @note `expected_gos`, the value being minimized, starts with a
    !        large number.
    expected_gos = BIG_NEVER_WINS
    selected = MISSING
    do walk_step=1, size(BEHAV_WALK_STEP_STDLEN_STATIC)
      walk_current = BEHAV_WALK_STEP_STDLEN_STATIC(walk_step)*this%get_length()
      call this%walk_random%init()
      call this%walk_random%expectancies_calculate( this_agent = this,        &
                        distance = walk_current,                              &
                        rescale_max_motivation = rescale_max_motivation_here )
      if (this%walk_random%arousal_expected < expected_gos ) then
        expected_gos = this%walk_random%arousal_expected
        selected = walk_current
      end if
    end do

  end subroutine walk_random_select

  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !> Calculate the expected GOS arousal that would be predicted from execution
  !! of the the_behaviour::freeze behaviour unit.
  !! @note This procedure is part of the_behaviour::behaviour_select_optimal()
  !!       procedure and called within.
  subroutine freeze_select(expected_gos)
    !> @param[out] expected_gos the GOS expectancy value predicted
    !!             from freezing.
    real(SRP), intent(out) :: expected_gos

    !> ### Implementation notes ###
    !> First, initialise this behaviour unit object by calling the
    !! the_behaviour::freeze::init() method.
    call this%freeze%init()

    !> The following calculations are rather straightforward here. The
    !! arousal expectancy that would follow from freezing the_behaviour::freeze
    !! is done by calling the the_behaviour::freeze::expectancies_calculate()
    !! method.
    call this%freeze%expectancies_calculate( this_agent = this,               &
                        rescale_max_motivation = rescale_max_motivation_here )
    expected_gos = this%freeze%arousal_expected        ! %gos_expected()

  end subroutine freeze_select

  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !> Calculate the expected GOS arousal that would be predicted from execution
  !! of the the_behaviour::escape_dart behaviour unit. The predator object that
  !! minimises the expected arousal (i.e. subjectively the most dangerous)
  !! is also obtained in this procedure.
  !! @note This procedure is part of the_behaviour::behaviour_select_optimal()
  !!       procedure and called within.
  subroutine escape_dart_select (expected_gos, selected)
    !> @param[out] expected_gos is the GOS expectancy value predicted
    !!             from escape movement.
    real(SRP), intent(out) :: expected_gos
    !> @param[out] selected the predator object within the
    !!             perception, that is associated with the lowest GOS arousal
    !!             of escape, i.e. the most subjectively dangerous predator
    !!             for the agent. Thus is actually the *number* of the predator
    !!             within the perception object.
    integer, intent(out) :: selected

    ! Local counter
    integer :: escape_step

    !> ### Implementation details ###
    !> There can be several different escape behaviour instances if the agent
    !! perceives several predators simultaneously: escape in response to each
    !! of these predators. Additionally, if the agent has no predator in the
    !! perception, escape behaviour is still possible to execute, but in such
    !! a case it is an undirected escape.
    !!
    !! Thus, first, a check is done if the agent has any predator in perception.
    if ( this%has_pred() ) then
      !> - If yes, a loop is constructed overall predators within perception,
      !!   the expected arousal is calculated for escape in response to each of
      !!   these predators by calling
      !!   the_behaviour::escape_dart::expectancies_calculate(). Finally,
      !!   the predator number `selected` that minimises the expected arousal
      !!   is taken as the "selected" predator and its linked (the minimum)
      !!   arousal now represents the arousal expectancy for the escape
      !!   behaviour.
      !    @note `expected_gos`, the value being minimized, starts
      !          with a large number.
      expected_gos = BIG_NEVER_WINS
      selected = UNKNOWN
      do escape_step=1, this%perceive_predator%get_count()
        call this%escape_dart%init()
        call this%escape_dart%expectancies_calculate( this_agent = this,      &
                        predator_object =                                     &
                          this%perceive_predator%predators_seen(escape_step), &
                        rescale_max_motivation = rescale_max_motivation_here )
        if ( this%escape_dart%arousal_expected < expected_gos ) then
          expected_gos = this%escape_dart%arousal_expected
          selected = escape_step
        end if
      end do
    else
      !> - If there are no predators in the operception of the agent, an
      !!   undirected escape is assumed. In such a case, the
      !!   the_behaviour::escape_dart::expectancies_calculate() method is
      !!   called omitting the optional predator object parameter.
      !!   Also, the number of the predator in the perception
      !!   (`selected`) is set to 0.
      !! .
      call this%escape_dart%expectancies_calculate( this_agent = this,        &
                        rescale_max_motivation = rescale_max_motivation_here )
      expected_gos = this%escape_dart%arousal_expected
      selected = 0
    end if

  end subroutine escape_dart_select

  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !> Calculate the expected GOS arousal that would be predicted from execution
  !! of the the_behaviour::approach_conspec behaviour unit. The conspecific that
  !! minimises the expected arousal (i.e. subjectively the most attractive)
  !! is also obtained in this procedure.
  !! @note This procedure is part of the_behaviour::behaviour_select_optimal()
  !!       procedure and called within.
  subroutine approach_consp_select(expected_gos, selected)
    !> @param[out] expected_gos is the GOS expectancy value
    !!             predicted from the approach to conspecific behaviour.
    real(SRP), intent(out) :: expected_gos
    !> @param[out] selected the conspecific object within the
    !!             perception, that is associated with the lowest GOS arousal
    !!             of approach, i.e. the most subjectively attractive
    !!             conspecific for the agent. Thus is actually the *number*
    !!             of the conspecific within the perception object.
    integer, intent(out) :: selected

    !> ### Implementation details ###
    !> First, the the_behaviour::approach_conspec behaviour class is
    !! initialised by calling the the_behaviour::approach_conspec::init()
    !! method.
    call this%approach_conspec%init()
    !> There can potentially be several different approach behaviour instances
    !! if the agent perceives several conspecifics simultaneously: separate
    !! instances of the approach behaviour are evaluated towards each of these
    !! conspecifics. However, if the agent has no conspecifics in its
    !! perception, approach has no mandatory target and is impossible. Thus,
    !! first, a check is done if the agent has any conspecifics in perception
    !! using the the_neurobio::perception::has_consp() method.
    if ( this%has_consp() ) then
      !> - If yes, determine the best, optimal conspecific to approach among
      !!   all that currently are in the perception object of the agent: this
      !!   is the conspecific that would result in the *minimum expected
      !!   arousal* the_behaviour::behaviour::consp_select();
      selected = this%consp_select( rescale_max_motivation =                  &
                                                  rescale_max_motivation_here)
      !> - calculate the overall motivational expectancy that approaching
      !!   this most attractive conspecific would provide by calling
      !!   the the_behaviour::approach_conspec::expectancies_calculate() method.
      !!   This value is now the *arousal expectancy* from the "approach
      !!   conspecifics" behaviour (the_behaviour::approach_conspec)
      !! .
      call this%approach_conspec%expectancies_calculate( this_agent = this,   &
                        target_object =                                       &
                            this%perceive_consp%conspecifics_seen(selected),  &
                        rescale_max_motivation = rescale_max_motivation_here )

      expected_gos = this%approach_conspec%arousal_expected
    else
      !> - On the other hand, if the agent has **no conspecifics** in its
      !!   perception object, the motivational expectancy is set to a large
      !!   positive value that is guaranteed to not win, so that this
      !!   behaviour cannot be executed.
      !! .
      expected_gos = BIG_NEVER_WINS
      selected = 0
    end if

  end subroutine approach_consp_select

  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !> Calculate the expected GOS arousal that would be predicted from execution
  !! of the the_behaviour::migrate behaviour unit. The habitat object that
  !! minimises the expected arousal (i.e. subjectively the most attractive)
  !! is also obtained in this procedure.
  !! @note This procedure is part of the_behaviour::behaviour_select_optimal()
  !!       procedure and called within.
  subroutine migrate_select(expected_gos,  selected)
    !> @param[out] expected_gos is the GOS expectancy value predicted
    !!             from migration behaviour into the optimal habitat, i.e.
    !!             the habitat within the array of available habitats
    !!             commondata::global_habitats_available that minimises the
    !!             linked GOS arousal.
    real(SRP), intent(out) :: expected_gos
    !> @param[out] selected the number of the habitat object within
    !!             the commondata::global_habitats_available array, that is
    !!             associated with the lowest GOS arousal of the migration
    !!             behaviour, i.e. the most subjectively attractive habitat
    !!             for the agent.
    integer, intent(out) :: selected

    ! Local counter.
    integer :: habitat_step

    !> ### Implementation details ###
    !> The migration behaviour depends on the target habitat that is different
    !! than the current habitat the agent is currently in. Therefore, there
    !! can potentially be several instances of the migration behaviour with
    !! different specific migration habitat targets. Then, a loop is
    !! constructed over all these targets (they are by default obtained from
    !! the the_environment::global_habitats_available global array) and the
    !! expected arousal is calculated for each one using
    !! the_behaviour::migrate::expectancies_calculate(). Finally, the habitat
    !! that minimises the expected arousal is taken as the "selected"habitat
    !! and its linked (the minimum) arousal now represents the arousal
    !! expectancy for the migration behaviour.
    expected_gos = BIG_NEVER_WINS
    selected = UNKNOWN
    do habitat_step=1, size(Global_Habitats_Available)
      if ( habitat_step /= this%find_environment() )  then
        call this%migrate%init()
        call this%migrate%expectancies_calculate( this_agent = this,          &
                        target_env = Global_Habitats_Available(habitat_step), &
                        rescale_max_motivation = rescale_max_motivation_here )
        if ( this%migrate%arousal_expected < expected_gos ) then
          expected_gos = this%migrate%arousal_expected
          selected = habitat_step
        end if
      end if
    end do

    DEBUG_LOG: block
      call LOG_DBG( LTAG_INFO // "Current agent's environment number: " //    &
                    TOSTR(this%find_environment()), PROCNAME, MODNAME )
      call LOG_DBG( LTAG_INFO // "Selected optimal environment number: "  //  &
                    TOSTR(selected) // ", distance to traverse: "         //  &
                    TOSTR(this%migrate%distance) // ", expected cost of " //  &
                    "moving: " // TOSTR(this%migrate%expected_cost_moving),   &
                    PROCNAME, MODNAME )
    end block DEBUG_LOG

  end subroutine migrate_select

  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !> Calculate the expected GOS arousal that would be predicted from execution
  !! of the the_behaviour::go_down_depth behaviour unit. The vertical migration
  !! walk step, from the commondata::behav_go_up_down_step_stdlen_static
  !! parameter array, that minimises the expected arousal (i.e. subjectively
  !! optimal) is also obtained in this procedure.
  !! @note This procedure is part of the_behaviour::behaviour_select_optimal()
  !!       procedure and called within.
  subroutine go_down_select(expected_gos, selected)
    !> @param[out] expected_gos is the GOS expectancy value
    !!             predicted from the downward vertical migration with the
    !!             optimal step size.
    real(SRP), intent(out) :: expected_gos
    !> @param[out] selected the static step size for the
    !!             downwards vertical migration (from values in the
    !!             commondata::behav_go_up_down_step_stdlen_static array).
    real(SRP), intent(out) :: selected

    ! Local counter
    integer :: depth_step
    ! Local value of the walk.
    real(SRP) :: walk_current

    !> ### Implementation details ###
    !> There are several Go down step sizes that are defined by the
    !! commondata::behav_go_up_down_step_stdlen_static parameter array
    !! (i.e. a *repertoire* of the vertical migration walks). Therefore,
    !! selection of the arousal expectancy that would follow from
    !! the_behaviour::go_down_depth behaviour as a whole requires finding the
    !! *optimal walk step* among all those defined in the repertoire
    !! (commondata::behav_go_up_down_step_stdlen_static). Such an optimal walk
    !! step size is the step size that would result in the lowest expected
    !! arousal (as computed by
    !! the_behaviour::go_down_depth::expectancies_calculate()).
    !! - This is done by looping over the available values of the depth
    !!   step size repertoire, commondata::behav_go_up_down_step_stdlen_static.
    !! .
    !  @note `expected_gos`, the value being minimized, starts
    !        with a large number.
    expected_gos = BIG_NEVER_WINS
    selected = MISSING
    do depth_step=1, size(BEHAV_GO_UP_DOWN_STEP_STDLEN_STATIC)
      walk_current = BEHAV_GO_UP_DOWN_STEP_STDLEN_STATIC(depth_step) *        &
                                                              this%get_length()
      call this%depth_down%init()
      call this%depth_down%expectancies_calculate( this_agent = this,         &
                        depth_walk = walk_current,                            &
                        environments = Global_Habitats_Available,             &
                        rescale_max_motivation = rescale_max_motivation_here )
      if (this%depth_down%arousal_expected < expected_gos ) then
        expected_gos = this%depth_down%arousal_expected
        selected = walk_current
      end if
    end do

  end subroutine go_down_select

  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !> Calculate the expected GOS arousal that would be predicted from execution
  !! of the the_behaviour::go_up_depth behaviour unit. The vertical migration
  !! walk step, from the commondata::behav_go_up_down_step_stdlen_static
  !! parameter array, that minimises the expected arousal (i.e. subjectively
  !! optimal) is also obtained in this procedure.
  !! @note This procedure is part of the_behaviour::behaviour_select_optimal()
  !!       procedure and called within.
  subroutine go_up_select(expected_gos, selected)
    !> @param[out] expected_gos is the GOS expectancy value predicted
    !!             from the upward vertical migration with the optimal step
    !!             size.
    real(SRP), intent(out) :: expected_gos
    !> @param[out] selected the static step size for the upwards
    !!             vertical migration (from values in the
    !!             commondata::behav_go_up_down_step_stdlen_static array).
    real(SRP), intent(out) :: selected

    ! Local counter
    integer :: depth_step
    ! Local value of the walk.
    real(SRP) :: walk_current

    !> ###Implementation details ###
    !> There are several Go up step sizes that are defined by the
    !! commondata::behav_go_up_down_step_stdlen_static parameter array
    !! (i.e. a *repertoire* of the vertical migration walks). Therefore,
    !! selection of the arousal expectancy that would follow from
    !! the_behaviour::go_up_depth behaviour as a whole requires finding the
    !! *optimal walk step* among all those defined in the repertoire
    !! (commondata::behav_go_up_down_step_stdlen_static). Such an optimal walk
    !! step size is the step size that would result in the lowest expected
    !! arousal (as computed by
    !! the_behaviour::go_up_depth::expectancies_calculate()).
    !! - This is done by looping over the available values of the depth
    !!   step size repertoire, commondata::behav_go_up_down_step_stdlen_static.
    !! .
    !  @note `expected_gos`, the value being minimized, starts
    !        with a large number.
    expected_gos = BIG_NEVER_WINS
    selected = MISSING
    do depth_step=1, size(BEHAV_GO_UP_DOWN_STEP_STDLEN_STATIC)
      walk_current = BEHAV_GO_UP_DOWN_STEP_STDLEN_STATIC(depth_step) *        &
                                                              this%get_length()
      call this%depth_up%init()
      call this%depth_up%expectancies_calculate( this_agent = this,           &
                        depth_walk = walk_current,                            &
                        environments = Global_Habitats_Available,             &
                        rescale_max_motivation = rescale_max_motivation_here )
      if (this%depth_up%arousal_expected < expected_gos ) then
        expected_gos = this%depth_up%arousal_expected
        selected = walk_current
      end if
    end do

  end subroutine go_up_select

  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !> Calculate the expected GOS arousal that would be predicted from execution
  !! of the the_behaviour::debug_base behaviour unit.
  !! @note This procedure is part of the_behaviour::behaviour_select_optimal()
  !!       procedure and called within.
  subroutine debug_base_select(expected_gos)
    !> @param[out] expected_gos the GOS expectancy value predicted
    !!             from freezing.
    real(SRP), intent(out) :: expected_gos

    !> ### Implementation notes ###
    !> First, initialise this behaviour unit object by calling the
    !! the_behaviour::debug_base::init() method.
    call this%debug_base%init()

    !> The following calculations are rather straightforward here. The
    !! arousal expectancy that would follow from the_behaviour::debug_base
    !! is done by calling the_behaviour::debug_base::expectancies_calculate().
    call this%debug_base%expectancies_calculate( this_agent = this,           &
                        rescale_max_motivation = rescale_max_motivation_here )
    expected_gos = this%debug_base%arousal_expected        ! %gos_expected()

    call LOG_DBG( LTAG_INFO // "Expected GOS arousal: " //                    &
                  TOSTR(expected_gos), PROCNAME, MODNAME )

  end subroutine debug_base_select

  end subroutine behaviour_select_optimal

  !-----------------------------------------------------------------------------
  !> Select and **execute** behaviour based on the current global organismic
  !! state. This procedure is significantly different from
  !! the_behaviour::behaviour_select_optimal() in that the behaviour that is
  !! executed is not based on optimisation of the expected GOS. Rather, the
  !! current GOS fully determines which behaviour unit is executed. Such a
  !! rigid  link necessarily limits the range of behaviours that could be
  !! executed.
  subroutine behaviour_select_fixed_from_gos(  this, rescale_max_motivation,  &
                                        food_resource_real )
    class(BEHAVIOUR), intent(inout) :: this
    !> @param[inout] food_resource_real The food resource the agent is eating
    !!               the food item in. Note that it could be a joined food
    !!               resource composed with the_environment::join() procedure
    !!               for assembling several habitats into the
    !!               the_environment::global_habitats_available array or
    !!               resources collapsed using the
    !!               the_environment::food_resource::join() method.
    class(FOOD_RESOURCE), optional, intent(inout) :: food_resource_real
    !> @param[in] rescale_max_motivation maximum motivation value for
    !!            rescaling all motivational components for comparison
    !!            across all motivation and perceptual components and behaviour
    !!            units.
    real(SRP), optional, intent(in) :: rescale_max_motivation

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter ::                                            &
                                PROCNAME = "(behaviour_select_fixed_from_gos)"

    ! Local copies of optionals
    real(SRP) :: rescale_max_motivation_here

    !> ### Notable local variables ###
    !> - **food_item_selected** is the optimal food item selected from all
    !!   the items that are currently within the perception object of the
    !!   agent. In this version of `do_behave`, the nearest food item is
    !!   selected.
    integer :: food_item_selected
    !> - **predator_selected_n** - the predator object within the perception,
    !!   that is considered the most subjectively dangerous for the agent.
    !!   (This is actually the *number* of the predator within the perception
    !!   object.) Note that in this version of `do_behave`, the nearest
    !!   predator is selected.
    integer :: predator_selected_n
    ! Local number of food resource within the global array
    ! the_environment::global_habitats_available.
    integer :: fres_num

    !> ### Implementation details ###
    !> #### Checks and preparations ####
    !> Determine optional parameter `rescale_max_motivation`. If it is absent
    !! from the parameter list, the value is calculated from the current
    !! perception using the the_neurobio::motivation::max_perception() method.
    if (present(rescale_max_motivation)) then
      rescale_max_motivation_here = rescale_max_motivation
    else
      rescale_max_motivation_here = this%motivations%max_perception()
    end if

    !> #### Try to perform random migration ####
    !! Random migration is implemented in the `TRY_MIGRATE` block.
    !! @warning This code does not work well in case the agent is within the
    !!          maximum random migration distance from more than one target
    !!          environment at once. It cycles in fixed order 1,2... over
    !!          the commondata::global_habitats_available. Ideally, should
    !!          select at random. Hopefully, such cases are very rare. TODO.
    TRY_MIGRATE: block
      logical :: is_migrated
      integer :: current_in, i
      is_migrated = .FALSE.
      !> - First, find what is the current agent's environment within the
      !!   commondata::global_habitats_available array, calling
      !!   the_environment::spatial::find_environment() method.
      current_in = this%find_environment()
      !> - Second, loop over all the habitats available in the
      !!   commondata::global_habitats_available array. If the `i`th habitat
      !!   does not coincide with the current agent's habitat (i.e. the agent
      !!   cannot emigrate to the currently occupied habitat), the agent
      !!   tries to perform random migration
      !!   the_behaviour::behaviour::migrate_random().
      do i = 1, size(Global_Habitats_Available)
        if ( current_in /= i ) then
          is_migrated = this%migrate_random( Global_Habitats_Available(i) )
        end if
        !>   Note that the loop is terminated (`exit`) if migration into
        !!   the i-th habitat was successful. The agent can perform only a
        !!   single behaviour (migration across habitats) per a single time
        !!   step.
        !! .
        if ( is_migrated ) exit
      end do
      !> If the migration was successful, no further behaviour is executed,
      !! it is assumed that the agent has executed the_behaviour::migrate
      !! behaviour unit.
      if ( is_migrated ) return
    end block TRY_MIGRATE

    !> #### Execute behaviours depending on the current GOS arousal ####
    !! Fixed behaviour selection is implemented in the `SELECT_BEHAV`construct.
    !! Each of the GOS is rigidly associated with a specific behaviour pattern.
    !> - the_neurobio::state_hunger is the GOS:
    SELECT_BEHAV: if ( this%motivations%hunger%is_dominant() ) then
      !>    - at least one food item is present within the perception object,
      !!      calls the the_behaviour::eat_food() method for the nearest
      !!      food item.
      if (this%has_food()) then
        food_item_selected = 1              ! first is nearest food item
        if (present(food_resource_real)) then
          call this%do_eat_food_item(food_item_selected, food_resource_real)
        else
          fres_num = this%find_environment(Global_Habitats_Available)
          call this%do_eat_food_item(food_item_selected,                      &
                                     Global_Habitats_Available(fres_num)%food)
        end if
      !>    - there are no food items in the perception object, calls default
      !!      random walk the_behaviour::walk_random.
      !!    .
      else
        call this%do_walk()
      end if
    !> - the_neurobio::state_fear_defence is the GOS:
    else if ( this%motivations%fear_defence%is_dominant() ) then    SELECT_BEHAV
      !>   - there is at least one predator in perception: calls
      !!     the_behaviour::escape_dart
      if (this%has_pred()) then
        predator_selected_n = 1             ! first is the nearest predator
        call this%do_escape( predator_object =                                &
                this%perceive_predator%predators_seen(predator_selected_n) )
      !>   - no predators are present in the perception object: call
      !!     the_behaviour::freeze.
      !!   .
      else
        call this%do_freeze( )
      end if

    !> - the_neurobio::state_reproduce is the GOS:
    else if ( this%motivations%reproduction%is_dominant() ) then    SELECT_BEHAV
      !>   - if the agent is ready to reproduce and there are conspecifics in
      !!     proximity, call the_behaviour::reproduce
      if (this%is_ready_reproduce() .and. this%has_consp()) then
        call this%do_reproduce()
      !>   - if the above condition is not satisfied, do default
      !!     the_behaviour::walk_random.
      !!   .
      else
        call this%do_walk()
      end if
    end if SELECT_BEHAV

  end subroutine behaviour_select_fixed_from_gos

  !-----------------------------------------------------------------------------
  !> Initialise neuro-biological architecture.
  elemental subroutine neurobio_init_components(this)
    class(ARCHITECTURE_NEURO), intent(inout) :: this

    !> Initialise neurobiological components of the agent.
    call this%init_perception()
    call this%init_appraisal()
    call this%init_gos()
    call this%init_behaviour()

  end subroutine neurobio_init_components

end module
