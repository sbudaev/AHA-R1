!> @file m_neuro.f90
!! The Neurobiological and behaviour architecture of the AHA Model.
!! @author Sergey Budaev <sergey.budaev@uib.no>
!! @author Jarl Giske <jarl.giske@uib.no>
!! @date 2016-2017

!-------------------------------------------------------------------------------
! $Id$
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief Definition of the decision making and behavioural the architecture
!> @section the_neurobio_module THE_NEUROBIO module
!> This module defines the neurobiological architecture of the agent, starting
!! from perception to representation, appraisal, motivation, emotion,
!! determination of the global organismic state, and behaviour.
module THE_NEUROBIO

  use COMMONDATA
  use THE_ENVIRONMENT
  use THE_BODY

  implicit none

  character (len=*), parameter, private :: MODNAME = "(THE_NEUROBIO)"

  !.............................................................................
  !  Lower-order perception components and objects. These describe
  !  the stimuli that the agent gets from the environment. These are of
  !  two kinds: (1) spatial/environmental perception objects which get
  !  objects from the outside world (e.g. food, conspecifics, predators)
  !  and (2) internal perception objects that get objects of the self
  !  organism (self stomach capacity available for food, energy reserve,
  !  body mass etc).

  !.............................................................................
  ! Spatial / environmental perception components: perception of objects in the
  ! outer world

  !> This type defines how the agent perceives food items.
  !! The food perception object the_neurobio::percept_food is basically an
  !! array of food objects within the visual range of the agent plus
  !! distances to the agent. This is the "objective" perception container,
  !! reflecting the "real world". We introduce a perception error when
  !! perception object is analysed by the agent's neurobiological system.
  type, public :: PERCEPT_FOOD
    !> An array of food items found within the visual range, limited by
    !! the maximum order of partial indexing
    !! `commondata::food_select_items_index_partial`.
    !! @note **Food perception** is quite complex to implement as it requires
    !!       determining individual food items within the current visual range
    !!       of the agent. There are, however, potentially thousands (or
    !!       millions) of food items in the food resource, each of the food
    !!       items is stochastic (e.g. they have different sizes), so visual
    !!       range differ for each item and each agent should determine
    !!       food items in its proximity at numerous time steps of the model.
    !!       This means repeating huge loops many times for each agent at
    !!       each time step. This is approached by array segmentation: the
    !!       perception object is obtained by *partial indexing* of a very
    !!       limited number (=`commondata::food_select_items_index_partial`) of
    !!       only the  nearest food items, the agent's visual range is then
    !!       determined for each of this nearest neighbouring food items, and
    !!       finally those food items that individually fall within the
    !!       visual range
    !!       are included into the perception object.
    !  @warning Tried to convert all perception types to allocatable
    !        (at rev. 1651), food perception worked ok, but other objects were
    !        broken. (1) pure/elemental  procedures have problems with
    !        allocatable within (allocatable is a side effect); (2) allocatable
    !        from within user types have problems with whole array passage to
    !        subroutines dummy parameters, issue error: [`Error: Component to
    !        the right of a part reference with nonzero rank must not have the
    !        ALLOCATABLE attribute at... `]. F2003 standard 6.1.2. prohibits
    !        passing whole arrays of allocatable derived types. So far returned
    !        to non-allocatable scalars and allocatable arrays, scalars would
    !        not cause small memory expenditures.
    type(FOOD_ITEM), allocatable, dimension(:) :: foods_seen
    !> An array of distances towards each of the food items.
    real(SRP), allocatable, dimension(:) :: foods_distances
    !> Total number of food items within the visual range of the agent.
    !! must not exceed the `commondata::food_select_items_index_partial`
    !! parameter.
    integer :: food_seen_count
    contains
      !> Initiate an empty **food** perception object with known number of
      !! components. See `the_neurobio::percept_food_create_init()`.
      procedure, public :: init => percept_food_create_init
      !! See `the_neurobio::percept_food_number_seen()`
      procedure, public :: number => percept_food_number_seen
      !> Set the total number of food items perceived (seen) in the food
      !! perception object. Do not reallocate the perception object components
      !! with respect to this new number yet.
      !! See `the_neurobio::percept_food_make_fill_arrays()`.
      procedure, public :: make => percept_food_make_fill_arrays
      !> Get the number (count) of food items seen.
      !! See `the_neurobio::percept_food_get_count_found()`.
      procedure, public :: get_count => percept_food_get_count_found
      !> Get the average size of food items seen.
      !! See `the_neurobio::percept_food_get_meansize_found()`.
      procedure, public :: get_meansize => percept_food_get_meansize_found
      !> Get the average mass of food items seen.
      !! See `the_neurobio::percept_food_get_meanmass_found()`.
      procedure, public :: get_meanmass => percept_food_get_meanmass_found
      !> Get the average distance tot the food items seen.
      !! See `the_neurobio::percept_food_get_meandist_found()`.
      procedure, public :: get_meandist => percept_food_get_meandist_found
      !> Deallocate and delete a **food** perception object.
      !! See `the_neurobio::percept_food_destroy_deallocate()`.
      procedure, public :: destroy => percept_food_destroy_deallocate
  end type PERCEPT_FOOD

  !> This type defines a single spatial perception component, i.e. some single
  !! elementary spatial object that can be perceived by the agent from a big
  !! array of objects of the same type which are available in the agent's
  !! environment. Different kinds of perception objects (e.g. conspecifics,
  !! predators etc.) can be produced by extending this  basic type.
  !! @note Note that the **food items** the_neurobio::percept_food are
  !!       implemented separately and  do not currently use the spatial
  !!       perception component objects. For example, individual food items
  !!       are indexed seperately witghin the food resource object by `iid`
  !!       data component.
  type, public, extends(SPATIAL) :: SPATIAL_PERCEPT_COMPONENT
    !> Spatial perception component adds an unique component id (`cid`) number
    !! to the basic `the_environment::spatial` so that individual objects
    !! within the whole perception object array can be identified. As a
    !! consequence, spatial perception component adds only two type-bound
    !! procedures that do the \%cid.
    integer :: cid
    contains
      !> Get the unique **id** of the food item object.
      !! See `the_neurobio::spatial_percept_get_cid()`.
      procedure, public :: get_cid => spatial_percept_get_cid
      !> Set unique **id** for the conspecific perception component.
      !! See `the_neurobio::spatial_percept_set_cid()`.
      procedure, public :: set_cid => spatial_percept_set_cid
  end type SPATIAL_PERCEPT_COMPONENT

  !> This type defines a **single conspecific** perception component.
  !! It is required for the `the_neurobio::percept_conspecifics` type that
  !! defines the whole conspecifics perception object (array of conspecifics).
  type, public, extends(SPATIAL_PERCEPT_COMPONENT) :: CONSPEC_PERCEPT_COMP
    !> Body size. The body size of the perception conspecific.
    !! @note We may need body size of the conspecifics as the decision
    !!       making rules may depend on the conspecific size (e.g.
    !!       approach big and repulse from small, also may affect
    !!       mating, e.g. mate with the biggest, etc.).
    real(SRP) :: consp_body_size
    !> Body mass. The body mass of the perception conspecific.
    real(SRP) :: consp_body_mass
    !> @note Any other characteristics of the perception conspecifics may
    !!       be added, e.g. sex etc. What is crucial for the decision making.
    !> The distance towards this conspecific in the visual field.
    real(SRP)  :: consp_distance
    !> The sex of the conspecific in the perception object.
    logical :: sex_is_male
    contains
      !> Create a single conspecific perception component at an undefined
      !! position with default properties.
      !! See `the_neurobio::consp_percept_comp_create()`.
      procedure, public :: create => consp_percept_comp_create
      !> Make a single conspecific perception component. This is a single
      !! conspecific located within the visual range of the agent.
      !! See `the_neurobio::consp_percept_make()`.
      procedure, public :: make => consp_percept_make
      !> Get the **conspecific** perception component body size.
      !! See `the_neurobio::consp_percept_get_size()`.
      procedure, public :: get_size => consp_percept_get_size
      !> Get the **conspecific** perception component body mass.
      !! See `the_neurobio::consp_percept_get_mass()`.
      procedure, public :: get_mass => consp_percept_get_mass
      !> Get the **conspecific** perception component distance.
      !! See `the_neurobio::consp_percept_get_dist()`.
      procedure, public :: get_dist => consp_percept_get_dist
      !> Get the **conspecific** perception component sex flag (male).
      !! See `the_neurobio::consp_percept_sex_is_male_get()`.
      procedure, public :: is_male => consp_percept_sex_is_male_get
      !> Get the **conspecific** perception component sex flag (female).
      !! See `the_neurobio::consp_percept_sex_is_female_get()`.
      procedure, public :: is_female => consp_percept_sex_is_female_get
  end type CONSPEC_PERCEPT_COMP

  !> This type defines how the agent perceives conspecifics.
  type, public :: PERCEPT_CONSPECIFICS
    !> An array of conspecifics seen in proximity, within the visual range.
    !! @note Perception of conspecifics is implemented similar to food items.
    !!       Conspecific perception object the_neurobio::percept_conspecifics
    !!       is a simple the_environment::spatial object.
    type(CONSPEC_PERCEPT_COMP), allocatable, dimension(:) :: conspecifics_seen
    !> The number of conspecifics seen.
    integer :: conspecifics_seen_count
    contains
      !> Create conspecifics perception object, it is an array of
      !! conspecific perception components.
      !! See `the_neurobio::percept_consp_create_init()`.
      procedure, public :: init => percept_consp_create_init
      !> Set the total number of conspecifics perceived (seen) in the
      !! conspecific perception object.
      !! See `the_neurobio::percept_consp_number_seen()`.
      procedure, public :: number => percept_consp_number_seen
      !> Make the conspecifics perception object, fill it with the actual
      !! arrays.
      !! See `the_neurobio::percept_consp_make_fill_arrays()`.
      procedure, public :: make => percept_consp_make_fill_arrays
      !> Get the number (count) of conspecifics seen.
      !! See `the_neurobio::percept_consp_get_count_seen()`.
      procedure, public :: get_count => percept_consp_get_count_seen
      !> Deallocate and delete a conspecific perception object.
      !! See `the_neurobio::percept_consp_destroy_deallocate()`.
      procedure, public :: destroy => percept_consp_destroy_deallocate
  end type PERCEPT_CONSPECIFICS

  !> This type defines a **single** arbitrary spatial object perception
  !! component. For example, a predator perception object is then an array of
  !! such spatial object perception components.
  type, public, extends(SPATIAL_PERCEPT_COMPONENT) :: SPATIALOBJ_PERCEPT_COMP
    !> Size. The size of the perception object.
    real(SRP) :: sobj_size
    !> @note Any other characteristics of the perception conspecifics may
    !!       be added, e.g. sex etc. What is crucial for the decision making.
    !> The distance towards this conspecific in the visual field.
    real(SRP)  :: sobj_distance
    contains
      !> Create a single arbitrary spatial object perception component at an
      !! undefined position with default properties.
      !! See `the_neurobio::spatialobj_percept_comp_create()`.
      procedure, public :: create => spatialobj_percept_comp_create
      !> Make a single arbitrary **spatial** object perception component.
      !! See `the_neurobio::spatialobj_percept_make()`.
      procedure, public :: make => spatialobj_percept_make
      !> Get an arbitrary spatial object perception component size.
      !! See `the_neurobio::spatialobj_percept_get_size()`.
      procedure, public :: get_size => spatialobj_percept_get_size
      !> Get the distance to an arbitrary spatial object perception component.
      !! See `the_neurobio::spatialobj_percept_get_dist()`.
      procedure, public :: get_dist => spatialobj_percept_get_dist
      !> Calculate the visibility range of this spatial object. Wrapper to the
      !! `visual_range` function. This function calculates the distance from
      !! which this object can be seen by a visual object (e.g. predator or
      !! prey).
      !! See `the_neurobio::spatialobj_percept_visibility_visual_range()`.
      procedure, public :: visibility =>                                      &
                                    spatialobj_percept_visibility_visual_range
  end type SPATIALOBJ_PERCEPT_COMP

  !> This type defines how the agent perceives a predator.
  type, public :: PERCEPT_PREDATOR
    !> An array of predators seen in proximity, within the visual range.
    !! @note Perception of an array of predators uses the arbitrary spatial
    !!       object components type defined by `SPATIALOBJ_PERCEPT_COMP`.
    type(SPATIALOBJ_PERCEPT_COMP), allocatable, dimension(:) :: predators_seen
    !> An array of the attack rates of the predators in the perception object.
    real(SRP), allocatable, dimension(:) :: predators_attack_rates
    !> The number of conspecifics seen.
    integer :: predators_seen_count
    contains
      !> Create **conspecifics** perception object, it is an array of
      !! conspecific perception components.
      !! See `the_neurobio::percept_predator_create_init()`.
      procedure, public :: init => percept_predator_create_init
      !> Set the total number of **predators** perceived (seen) in the predator
      !! perception object.
      !! See `the_neurobio::percept_predator_number_seen()`.
      procedure, public :: number => percept_predator_number_seen
      !> Make the **predator** perception object, fill it with the
      !! actual arrays.
      !! See `the_neurobio::percept_predator_make_fill_arrays()`.
      procedure, public :: make => percept_predator_make_fill_arrays
      !> Set an array of the attack rates for the predator perception object.
      !! See `the_neurobio::percept_predator_set_attack_rate_vector()`.
      procedure, public :: set_attack_rate_v => percept_predator_set_attack_rate_vector
      !> Set an array of the attack rates for the predator perception object.
      !! See `the_neurobio::percept_predator_set_attack_rate_scalar()`.
      procedure, public :: set_attack_rate_s => percept_predator_set_attack_rate_scalar
      !> A generic interface to set the attack rates for the predator
      !! perception object.
      !! See `the_neurobio::percept_predator_set_attack_rate_vector()` and
      !! `the_neurobio::percept_predator_set_attack_rate_scalar()`.
      generic, public :: set_attack_rate => set_attack_rate_v, set_attack_rate_s
      !> Get the number (count) of predators seen.
      !! See `the_neurobio:percept_predator_get_count_seen:()`.
      procedure, public :: get_count => percept_predator_get_count_seen
      !> Deallocate and delete a **predator** perception object.
      !! See `the_neurobio::percept_predator_destroy_deallocate()`.
      procedure, public :: destroy => percept_predator_destroy_deallocate
  end type PERCEPT_PREDATOR

  !.............................................................................
  ! Internal perception components: perception of objects within the
  ! self organism.

  !> This type defines how the agent perceives its own stomach capacity.
  type, public :: PERCEPT_STOMACH
    !> Available stomach capacity as a proportion of the full stomach. So,
    !! 0 is full stomach (no space for new food), 1 is empty stomach (full
    !! capacity available).
    real(SRP) :: capacity
    contains
      !> Initiate an empty **stomach** capacity perception object.
      !! See `the_neurobio::percept_stomach_create_init()`.
      procedure, public :: init => percept_stomach_create_init
      !> Get the currently available value of the available **stomach** volume.
      !! See `the_neurobio::percept_stomach_get_avail_capacity()`.
      procedure, public :: get_available => percept_stomach_get_avail_capacity
      !> Set and update the currently available value of the available
      !! **stomach** volume.
      !! See `the_neurobio::percept_stomach_update_avail_capacity()`.
      procedure, public :: set_available =>percept_stomach_update_avail_capacity
      !> Destroy the **stomach** perception object and deallocate.
      !! See `the_neurobio::percept_stomach_destroy_deallocate()`.
      procedure, public :: destroy => percept_stomach_destroy_deallocate
  end type PERCEPT_STOMACH

  !> This type defines how the agent perceives its own body mass
  !> it can be important for state-dependency.
  type, public :: PERCEPT_BODY_MASS
    !> The current body mass of the agent.
    real(SRP) :: body_mass
    contains
      !> Initiate an empty **body mass** perception object.
      !! See `the_neurobio::percept_bodymass_create_init()`.
      procedure, public :: init => percept_bodymass_create_init
      !> Get the current value of the **body mass** perception.
      !! See `the_neurobio::percept_bodymass_get_current()`.
      procedure, public :: get_current => percept_bodymass_get_current
      !> Set and update the current **body mass** perception value.
      !! See `the_neurobio::percept_bodymass_update_current()`.
      procedure, public :: set_current => percept_bodymass_update_current
      !> Destroy the **body mass** perception object and deallocate.
      !! See `the_neurobio::percept_bodymass_destroy_deallocate()`.
      procedure, public :: destroy => percept_bodymass_destroy_deallocate
  end type PERCEPT_BODY_MASS

  !> This type defines how the agent perceives its own energy reserves
  !> it can be important for state-dependency.
  type, public :: PERCEPT_ENERGY
    !> The current energy reserves of the agent.
    real(SRP) :: energy_reserve
    contains
      !> Initiate an empty **energy** perception object.
      !! See `the_neurobio::percept_energy_create_init()`.
      procedure, public :: init => percept_energy_create_init
      !> Get the current value of the **energy** reserves.
      !! See `the_neurobio::percept_energy_get_current()`.
      procedure, public :: get_current => percept_energy_get_current
      !> Set and update the current **energy** perception value.
      !! See `the_neurobio::percept_energy_update_current()`.
      procedure, public :: set_current =>percept_energy_update_current
      !> Destroy the **energy** perception object and deallocate.
      !! See `the_neurobio::percept_energy_destroy_deallocate()`.
      procedure, public :: destroy => percept_energy_destroy_deallocate
  end type PERCEPT_ENERGY

  !> This type defines how the agent perceives its own age in terms of
  !! the model discrete time step.
  type, public :: PERCEPT_AGE
    integer :: age
    contains
      !> Initiate an empty **age** perception object.
      !! See `the_neurobio::percept_age_create_init()`.
      procedure, public :: init => percept_age_create_init
      !> Get the current value of the **age** reserves.
      !! See `the_neurobio::percept_age_get_current()`.
      procedure, public :: get_current => percept_age_get_current
      !> Set and update the current **age** perception value.
      !! See `the_neurobio::percept_age_update_current()`.
      procedure, public :: set_current =>percept_age_update_current
      !> Destroy the **age** perception object and deallocate.
      !! See `the_neurobio::percept_age_destroy_deallocate()`.
      procedure, public :: destroy => percept_age_destroy_deallocate
  end type PERCEPT_AGE

  !> Perception of the reproductive factor, reproductive factor depends
  !! on the sex hormones differently in males and females.
  type, public :: PERCEPT_REPRFACT
    real(SRP) :: reproduct_fact
    contains
      !> Make en empty reproductive factor perception component.
      !! See `the_neurobio::percept_reprfac_create_init()`.
      procedure, public :: init => percept_reprfac_create_init
      !> Get the current perception of the **reproductive factor**.
      !! See `the_neurobio::percept_reprfac_get_current()`.
      procedure, public :: get_current => percept_reprfac_get_current
      !> Set the current **reproductive factor** level into perception component.
      !! See `the_neurobio::percept_reprfac_set_current()`.
      procedure, public :: set_current => percept_reprfac_set_current
      !> Destroy / deallocate **reproductive factor** perception component.
      !! See `the_neurobio::percept_reprfac_destroy_deallocate()`.
      procedure, public :: destroy => percept_reprfac_destroy_deallocate
  end type PERCEPT_REPRFACT

  !.............................................................................
  ! External **direct** non-spatial perception components: perception of the
  ! general environmental factors like light and depth that are not localised
  ! in the environment and can be perceived directly. The functions are almost
  ! wholly trivial, but needed here to make perception structure consistent and
  ! consisting of the same standardised units.

  !> Perception of the ambient illumination. This is a very simple
  !! perception component, singular and static.
  type, public :: PERCEPT_LIGHT
    real(SRP) :: illumination
    contains
      !> Make en empty light perception component.
      !! See `the_neurobio::percept_light_create_init()`.
      procedure, public :: init => percept_light_create_init
      !> Get the current perception of the illumination.
      !! See `the_neurobio::percept_light_get_current()`.
      procedure, public :: get_current => percept_light_get_current
      !> Set the current **light** level into the perception component.
      !! See `the_neurobio::percept_light_set_current()`.
      procedure, public :: set_current => percept_light_set_current
      !> Destroy / deallocate **light** perception component.
      !! See `the_neurobio::percept_light_destroy_deallocate()`.
      procedure, public :: destroy => percept_light_destroy_deallocate
  end type PERCEPT_LIGHT

  !> Perception of the current depth horizon.
  type, public :: PERCEPT_DEPTH
    real(SRP) :: depth
    contains
      !> Make en empty depth perception component.
      !! See `the_neurobio::percept_depth_create_init()`.
      procedure, public :: init => percept_depth_create_init
      !> Get the current perception of the **depth**.
      !! See `the_neurobio::percept_depth_get_current()`.
      procedure, public :: get_current => percept_depth_get_current
      !> Set the current **depth** level into the perception component.
      !! See `the_neurobio::percept_depth_set_current()`.
      procedure, public :: set_current => percept_depth_set_current
      !> Destroy / deallocate **depth** perception component.
      !! See `the_neurobio::percept_depth_destroy_deallocate()`.
      procedure, public :: destroy => percept_depth_destroy_deallocate
  end type PERCEPT_DEPTH

  !.............................................................................
  ! Here we collect all the above individual perception components into a
  ! unitary individual-specific perception object.

  !> Individual perception memory(history) stack, a memory component that
  !! saves perception values at previous time steps of the model. Not whole
  !! perception objects are saved for simplicity, only the most important
  !! parameters, integer and real types so commondata::add_to_history() can
  !! be used in unmodified form. Decision making can make use of this memory
  !! stack.
  !! @note Note that age perception `the_neurobio::percept_age` is **not
  !!       saved** in memory stack as it is trivial to get/predict.
  type, public :: MEMORY_PERCEPTUAL
    !> Memory for **light**.
    real(SRP), dimension(HISTORY_SIZE_PERCEPTION) :: memory_light   ! Light
    !> Memory for **depth**.
    real(SRP), dimension(HISTORY_SIZE_PERCEPTION) :: memory_depth   ! Depth
    !> Memory for **number of food items** seen (in perception).
    integer  , dimension(HISTORY_SIZE_PERCEPTION) :: memory_food    ! N foods
    !> Memory for **mean size of food items** seen (in perception).
    real(SRP), dimension(HISTORY_SIZE_PERCEPTION) :: memory_foodsiz ! mean size
    !> Memory for **mean distance to the food items** seen (in perception).
    real(SRP), dimension(HISTORY_SIZE_PERCEPTION) :: memory_foodist ! mean dist
    !> Memory for **number of conspecifics** seen.
    integer  , dimension(HISTORY_SIZE_PERCEPTION) :: memory_consp   ! N consp.
    !> Memory for **number of predators**.
    integer  , dimension(HISTORY_SIZE_PERCEPTION) :: memory_pred    ! N pred.
    !> Memory for **stomach contents**.
    real(SRP), dimension(HISTORY_SIZE_PERCEPTION) :: memory_stom    ! Stomach
    !> Memory for **body mass**.
    real(SRP), dimension(HISTORY_SIZE_PERCEPTION) :: memory_bdmass  ! Body mass
    !> Memory for **energy reserves**.
    real(SRP), dimension(HISTORY_SIZE_PERCEPTION) :: memory_energ   ! Energy
    !> Memory for **reproductive factor** values.
    real(SRP), dimension(HISTORY_SIZE_PERCEPTION) :: memory_reprfac ! Repr.fact.
    contains
      !> Add perception components into the memory stack.
      !! See `the_neurobio::percept_memory_add_to_stack()`.
      procedure, public :: add_to_memory => percept_memory_add_to_stack
      !> Cleanup and destroy the perceptual memory stack.
      !! See `the_neurobio::percept_memory_cleanup_stack()`.
      procedure, public :: memory_cleanup => percept_memory_cleanup_stack
      !> Get the total number of food items within the whole perceptual memory
      !! stack. See `the_neurobio::percept_memory_food_get_total()`.
      procedure, public :: get_food_total => percept_memory_food_get_total
      !> Get the average number of food items per single time step within the
      !! whole perceptual memory stack.
      !! See `the_neurobio::percept_memory_food_get_mean_n()`.
      procedure, public :: get_food_mean_n => percept_memory_food_get_mean_n
      !> Get the **average number** of food items per single time step within
      !! the perceptual memory stack, split to the first (older) and second
      !! (newer) parts. The whole memory stack ('sample') is split by the
      !! `split_val` parameter and two means are calculated: before the
      !! `split_val` and after it.
      !! See `the_neurobio::percept_memory_food_mean_n_split()`.
      procedure, public :: get_food_mean_n_split =>                           &
                                            percept_memory_food_mean_n_split
      !> Get the average size of food item per single time step within the
      !! whole perceptual memory stack.
      !! See `the_neurobio::percept_memory_food_get_mean_size()`.
      procedure, public :: get_food_mean_size=>percept_memory_food_get_mean_size
      !> Get the **average size** of food items per single time step within the
      !! perceptual memory stack, split to the first (older) and second(newer)
      !! parts. The whole memory stack 'sample' is split by the `split_val`
      !! parameter and two means are calculated: before the `split_val` and
      !! after it. See `the_neurobio::percept_memory_food_mean_size_split()`.
      procedure, public :: get_food_mean_size_split =>                        &
                                            percept_memory_food_mean_size_split
      !> Get the **average distance** to food item per single time step within the
      !! whole perceptual memory stack.
      !! See `the_neurobio::percept_memory_food_get_mean_dist()`.
      procedure, public :: get_food_mean_dist =>                              &
                                            percept_memory_food_get_mean_dist
      !> Get the **average distance** to food items per single time step within the
      !! perceptual memory stack, split to the first (older) and second(newer)
      !! parts. The whole memory stack 'sample' is split by the `split_val`
      !! parameter and two means are calculated: before the `split_val` and after
      !! it. See `the_neurobio::percept_memory_food_mean_dist_split()`.
      procedure, public :: get_food_mean_dist_split =>                        &
                                            percept_memory_food_mean_dist_split
      !> Get the **average number** of conspecifics per single time step
      !! within the whole perceptual memory stack.
      !! See `the_neurobio::percept_memory_consp_get_mean_n()`.
      procedure, public :: get_consp_mean_n => percept_memory_consp_get_mean_n
      !> Get the total number of predators within the whole perceptual memory
      !! stack. See `the_neurobio::percept_memory_predators_get_total()`.
      procedure, public :: get_pred_total => percept_memory_predators_get_total
      !> Get the average number of predators per single time step within the
      !! whole perceptual memory stack.
      !! See `the_neurobio::percept_memory_predators_get_mean()`.
      procedure, public :: get_pred_mean => percept_memory_predators_get_mean
      !> Get the **average number** of predators per single time step within the
      !! perceptual memory stack, split to the first (older) and second(newer)
      !! parts. The whole memory stack ('sample') is split by the `split_val`
      !! parameter and two means are calculated: before the `split_val` and after
      !! it. See `the_neurobio::percept_memory_predators_mean_split()`.
      procedure, public :: get_pred_mean_split =>                             &
                                            percept_memory_predators_mean_split
  end type MEMORY_PERCEPTUAL

  !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  !> The perception architecture of the agent. See @ref aha_buildblocks_percept
  !! "\"The perception mechanism\"" for a general overview.
  !! At this level, lower order perception objects are combined into the
  !! the_neurobio::perception class hierarchy level of the agent. The object
  !! bound functions see_ and feel_ obtain (**set**) the specific perception
  !! objects from the external or internal environments of the agent and put
  !! them into the the_neurobio::perception data structure. Also, memory
  !! component is updated with the perception data. Perception objects can
  !! then be used as input into the individual decision-making procedures.
  !! @note  **Templates for outer environmental perceptions**:
  !!        @code
  !!          call proto_parents%individual(ind)%see_food(                   &
  !!                      food_resource_available = habitat_safe%food,       &
  !!                      time_step_model = 1)
  !!
  !!          call proto_parents%individual(ind)%see_consp(                  &
  !!                      consp_agents = proto_parents%individual,           &
  !!                      time_step_model = 1 )
  !!
  !!          call proto_parents%individual(ind)%see_pred(                   &
  !!                      spatl_agents = predators,                          &
  !!                      time_step_model = 1 )
  !!          call proto_parents%individual(ind)%feel_light(timestep)`
  !!          call proto_parents%individual(ind)%feel_depth()
  !!        @endcode
  type, public, extends(REPRODUCTION) :: PERCEPTION
    type(PERCEPT_LIGHT)         :: perceive_light     !> perception of light
    type(PERCEPT_DEPTH)         :: perceive_depth     !> perception of depth
    type(PERCEPT_FOOD)          :: perceive_food      !> perception of food
    type(PERCEPT_CONSPECIFICS)  :: perceive_consp     !> conspecifics perception
    type(PERCEPT_PREDATOR)      :: perceive_predator  !> perceive predator
    type(PERCEPT_STOMACH)       :: perceive_stomach   !> perception for stomach
    type(PERCEPT_BODY_MASS)     :: perceive_body_mass !> perception for bodymass
    type(PERCEPT_ENERGY)        :: perceive_energy    !> percept. for energy
    type(PERCEPT_AGE)           :: perceive_age       !> percept. of age
    type(PERCEPT_REPRFACT)      :: perceive_reprfac   !> percept. of repr.factor
    type(MEMORY_PERCEPTUAL)     :: memory_stack       !> @note Memory object.
    contains
      !> Get **light** perception objects into the individual
      !! the_neurobio::perception object layer.
      !! See `the_neurobio::light_perception_get_object()`.
      procedure, public :: feel_light => light_perception_get_object
      !> Get **depth** perception objects into the **individual**
      !! the_neurobio::perception object layer.
      !! See `the_neurobio::depth_perception_get_object()`.
      procedure, public :: feel_depth => depth_perception_get_object
      !> Get available food items within the visual range of the agent, which
      !! the agent can perceive and therefore respond to. Food perception is
      !! packaged into the food perception object `this%perceive_food` for
      !! output. See `the_neurobio::food_perception_get_visrange_objects()`.
      procedure, public :: see_food => food_perception_get_visrange_objects
      !> Get available conspecific perception objects within the visual range
      !! of the agent, which the agent can perceive and therefore respond to.
      !! See `the_neurobio::consp_perception_get_visrange_objects()`.
      procedure, public :: see_consp => consp_perception_get_visrange_objects
      !> Get available predators perception objects within the visual range of
      !! the agent, which the agent can perceive and therefore respond to.
      !! See `the_neurobio::predator_perception_get_visrange_objects()`.
      procedure, public :: see_pred => predator_perception_get_visrange_objects
      !> Get the **stomach capacity** perception objects into the **individual**
      !! the_neurobio::perception object layer.
      !! See `the_neurobio::stomach_perception_get_object()`.
      procedure, public :: feel_stomach => stomach_perception_get_object
      !> Get the **body mass** perception objects into the **individual**
      !! the_neurobio::perception object layer.
      !! See `the_neurobio::bodymass_perception_get_object()`.
      procedure, public :: feel_bodymass => bodymass_perception_get_object
      !> Get the **energy reserves** perception objects into the **individual**
      !! the_neurobio::perception object layer.
      !! See `the_neurobio::energy_perception_get_object()`.
      procedure, public :: feel_energy => energy_perception_get_object
      !> Get the **age** perception objects into the **individual**
      !! the_neurobio::perception object layer.
      !! See `the_neurobio::age_perception_get_object()`.
      procedure, public :: feel_age => age_perception_get_object
      !> Get the **reproductive factor** perception objects into the
      !! **individual** the_neurobio::perception object layer.
      !! See `the_neurobio::repfac_perception_get_object()`.
      procedure, public :: feel_repfac => repfac_perception_get_object

      !> Calculate the risk of **predation** as being **perceived / assessed**
      !! by this agent.
      !! See `the_neurobio::perception_predation_risk_objective()`.
      procedure, public :: predation_risk => perception_predation_risk_objective
      !> A single umbrella subroutine to get all **environmental** perceptions:
      !! light, depth.
      !! See `the_neurobio::perception_objects_get_all_environmental()`.
      procedure, public :: perceptions_environ => perception_objects_get_all_environmental
      !> A single umbrella subroutine wrapper to get all **inner** perceptions:
      !! stomach, body mass, energy, age.
      !! See `the_neurobio::perception_objects_get_all_inner()`.
      procedure, public :: perceptions_inner => perception_objects_get_all_inner
      !> Add the various perception objects to the memory stack object. This
      !! procedure is called **after** all the perceptual components (light,
      !! depth food, conspecifics, predators, etc.) are collected (using `set`
      !! object-bound subroutines) into the perception bundle, so all the
      !! values are known and ready to be used.
      !! See `the_neurobio::perception_objects_add_memory_stack()`.
      procedure, public :: perception_to_memory => perception_objects_add_memory_stack

      !> Initialise all the perception objects for the current agent. Do not
      !! fill perception objects with the real data yet.
      !! See `the_neurobio::perception_objects_init_agent()`.
      procedure, public :: init_perception => perception_objects_init_agent
      !> Destroy and deallocate all perception objects.
      !! See `the_neurobio::perception_objects_destroy()`.
      procedure, public :: destroy_perception => perception_objects_destroy

      ! Accessor methods (get).
      !> Check if the agent sees any food items within its visual range.
      !! See `the_neurobio::food_perception_is_seeing_food()`.
      procedure, public :: has_food => food_perception_is_seeing_food
      !> Check if the agent sees any conspecifics within the visual range.
      !! See `the_neurobio::consp_perception_is_seeing_conspecifics()`.
      procedure, public :: has_consp => consp_perception_is_seeing_conspecifics
      !> Check if the agent sees any predators within the visual range.
      !! See `the_neurobio::predator_perception_is_seeing_predators()`.
      procedure, public :: has_pred => predator_perception_is_seeing_predators

      ! Relative location functions: Food items
      !> Calculate the number of food items in the perception object that are
      !! located **below** the actor agent.
      !! See `the_neurobio::perception_food_items_below_calculate()`
      procedure, public :: food_items_below_all =>                            &
                                        perception_food_items_below_calculate
      !> Calculate the number of food items in the perception object that are
      !! located **below** the actor agent within a specific vertical horizon
      !! [hz_lower,hz_upper]. The horizon limits are relative, in that they
      !! start from the depth position of the `this` actor agent:
      !! [z+hz_lower, z+hz_upper].
      !! See `the_neurobio::perception_food_items_below_horiz_calculate()`.
      procedure, public :: food_items_below_horiz =>                          &
                                    perception_food_items_below_horiz_calculate
      !> A generic interface for the two functions calculating the number of
      !! food items in the perception object that are located **below** the
      !! actor agent. See perception::food_items_below_all(),
      !! perception::food_items_below_horiz().
      generic, public :: food_items_below =>                                  &
                                  food_items_below_all, food_items_below_horiz
      !> Calculate the average mass of a food item from all the items in the
      !! current perception object that are **below** the actor agent.
      !! See `the_neurobio::perception_food_mass_below_calculate()`.
      procedure, public :: food_mass_below_all =>                             &
                                          perception_food_mass_below_calculate
      !> Calculate the average mass of a food item from all the items in the
      !! current perception object that are **below** the actor agent within a
      !! specific vertical horizon [hz_lower,hz_upper]. The horizon limits are
      !! relative, in that they start from the depth position of the `this`
      !! actor agent: [z+hz_lower, z+hz_upper].
      !! See `the_neurobio::perception_food_mass_below_horiz_calculate()`.
      procedure, public :: food_mass_below_horiz =>                           &
                                    perception_food_mass_below_horiz_calculate
      !> A generic interface to the two functions that calculating the
      !! average mass of food items in the perception object that are located
      !! **below** the actor agent. See perception::food_mass_below_all(),
      !! perception::food_mass_below_horiz().
      generic, public :: food_mass_below =>                                   &
                                    food_mass_below_all, food_mass_below_horiz
      !> Calculate the number of food items in the perception object that are
      !! located **above** the actor agent.
      !! See `the_neurobio::perception_food_items_above_calculate()`
      procedure, public :: food_items_above_all =>                            &
                                          perception_food_items_above_calculate
      !> Calculate the number of food items in the perception object that are
      !! located **above** the actor agent within a specific vertical horizon
      !! [hz_lower,hz_upper]. The horizon limits are relative, in that they
      !! start from the depth position of the `this` actor agent:
      !! [z-hz_upper, z-hz_upper].
      !! See `the_neurobio::perception_food_items_above_horiz_calculate()`.
      procedure, public :: food_items_above_horiz =>                          &
                                  perception_food_items_above_horiz_calculate
      !> A generic interface for the two functions calculating the number of
      !! food itemsin the perception object that are located **below** the
      !! actor agent. See perception::food_items_above_all(),
      !! perception::food_items_above_horiz().
      generic, public :: food_items_above =>                                  &
                                  food_items_above_all, food_items_above_horiz
      !> Calculate the average mass of a food item from all the items in the
      !! current perception object that are **above** the actor agent.
      !! See `the_neurobio::perception_food_mass_above_calculate()`.
      procedure, public :: food_mass_above_all =>                             &
                                          perception_food_mass_above_calculate
      !> Calculate the average mass of a food item from all the items in the
      !! current perception object that are **above** the actor agent within a
      !! specific vertical horizon [hz_lower,hz_upper]. The horizon limits are
      !! relative, in that they start from the depth position of the `this`
      !! actor agent: [z-hz_upper, z-hz_upper].
      !! See `the_neurobio::perception_food_mass_above_horiz_calculate()`.
      procedure, public :: food_mass_above_horiz =>                           &
                                    perception_food_mass_above_horiz_calculate
      !> A generic interface to the two functions that calculating the
      !! average mass of food items in the perception object that are located
      !! **above** the actor agent. See perception::food_mass_above_all(),
      !! perception::food_mass_above_horiz().
      generic, public :: food_mass_above =>                                   &
                                    food_mass_above_all, food_mass_above_horiz
      !> Calculate the average distance to all food items in the current
      !! perception object that are **below** the actor agent.
      !! See `the_neurobio::perception_food_dist_below_calculate()`.
      procedure, public :: food_dist_below =>                                 &
                                          perception_food_dist_below_calculate
      !> Calculate the average distance to all food items in the current
      !! perception object that are **above** the actor agent.
      !! See `the_neurobio::perception_food_dist_above_calculate()`.
      procedure, public :: food_dist_above =>                                 &
                                          perception_food_dist_above_calculate

      ! Relative location functions: conspecifics
      !> Calculate the number of conspecifics in the perception object that are
      !! located **below** the actor agent.
      !! See `the_neurobio::perception_conspecifics_below_calculate()`.
      procedure, public :: consp_below_all =>                                 &
                                      perception_conspecifics_below_calculate
      !> Calculate the number of conspecifics in the perception object that are
      !! located **above** the actor agent.
      !! See `the_neurobio::perception_conspecifics_above_calculate()`.
      procedure, public :: consp_above_all =>                                 &
                                      perception_conspecifics_above_calculate
      !> Calculate the number of conspecifics in the perception object that are
      !! located **below** the actor agent within a specific vertical horizon
      !! [hz_lower,hz_upper].
      !! See `the_neurobio::perception_conspecifics_below_horiz_calculate()`.
      procedure, public :: consp_below_horiz =>                               &
                                perception_conspecifics_below_horiz_calculate
      !> Calculate the number of conspecifics in the perception object that are
      !! located **above** the actor agent within a specific vertical horizon
      !! [hz_lower,hz_upper].
      !! See `the_neurobio::perception_conspecifics_above_horiz_calculate()`.
      procedure, public :: consp_above_horiz =>                               &
                                perception_conspecifics_above_horiz_calculate
      !> A generic interface to the two functions that calculating the
      !! number of conspecifics in the perception object that are located
      !! **below** the actor agent. See perception::consp_below_all(),
      !! perception::consp_below_horiz().
      generic, public :: consp_below => consp_below_all, consp_below_horiz
      !> A generic interface to the two functions that calculating the
      !! number of conspecifics in the perception object that are located
      !! **above** the actor agent. See perception::consp_above_all(),
      !! perception::consp_above_horiz().
      generic, public :: consp_above => consp_above_all, consp_above_horiz
      !> Calculate the average distance to all conspecifics in the current
      !! perception object that are **below** the actor agent.
      !! See `the_neurobio::perception_consp_dist_below_calculate()`.
      procedure, public :: consp_dist_below =>                                &
                                      perception_consp_dist_below_calculate
      !> Calculate the average distance to all conspecifics in the current
      !! perception object that are **above** the actor agent.
      !! See `the_neurobio::perception_consp_dist_above_calculate()`.
      procedure, public :: consp_dist_above =>                                &
                                      perception_consp_dist_above_calculate

      ! Relative location functions: predators
      !> Calculate the number of predators in the perception object that are
      !! located **below** the actor agent.
      !! See `the_neurobio::perception_predator_below_calculate()`.
      procedure, public :: pred_below_all => perception_predator_below_calculate
      !> Calculate the number of predators in the perception object that are
      !! located **above** the actor agent.
      !! See `the_neurobio::perception_predator_above_calculate()`.
      procedure, public :: pred_above_all => perception_predator_above_calculate
      !> Calculate the number of predators in the perception object that are
      !! located **below** the actor agent within a specific vertical horizon
      !! [hz_lower,hz_upper].
      !! See `the_neurobio::perception_predator_below_horiz_calculate`.
      procedure, public :: pred_below_horiz =>                                &
                                    perception_predator_below_horiz_calculate
      !> Calculate the number of predators in the perception object that are
      !! located **above** the actor agent within a specific vertical horizon
      !! [hz_lower,hz_upper].
      !! See `the_neurobio::perception_predator_above_horiz_calculate`.
      procedure, public :: pred_above_horiz =>                                &
                                    perception_predator_above_horiz_calculate
      !> A generic interface to the two functions that calculating the
      !! number of predators in the perception object that are located
      !! **below** the actor agent. See perception::pred_below_all(),
      !! perception::pred_below_horiz().
      generic, public :: pred_below => pred_below_all, pred_below_horiz
      !> A generic interface to the two functions that calculating the
      !! number of predators in the perception object that are located
      !! **above** the actor agent. See perception::pred_above_all(),
      !! perception::pred_above_horiz().
      generic, public :: pred_above => pred_above_all, pred_above_horiz
      !> Calculate the average distance to all predators in the current
      !! perception object that are **below** the actor agent.
      !! See `the_neurobio::perception_predator_dist_below_calculate()`.
      procedure, public :: pred_dist_below =>                                 &
                                      perception_predator_dist_below_calculate
      !> Calculate the average distance to all predators in the current
      !! perception object that are **above** the actor agent.
      !! See `the_neurobio::perception_predator_dist_above_calculate()`.
      procedure, public :: pred_dist_above =>                                 &
                                      perception_predator_dist_above_calculate
      !> Calculate the probability of attack and capture of the `this` agent by
      !! the predator `this_predator`. This probability is a function of the
      !! distance between the predator and the agent and is calculated by the
      !! predator-class-bound procedure the_environment::predator::risk_fish().
      !! @note Note that this version of the procedure accepts `this_predator`
      !!       parameter as class the_neurobio::spatialobj_percept_comp that is
      !!       used for keeping the predator representations in the **perception
      !!       object**. This representation keeps two separate array for
      !!       the_neurobio::spatialobj_percept_comp spatial objects and the
      !!       attack rate.
      !! See `the_neurobio::predator_capture_probability_calculate_spatobj()`.
      procedure, public :: risk_pred_s =>                                     &
                                predator_capture_probability_calculate_spatobj
      !> Calculate the probability of attack and capture of the `this` agent by
      !! the predator `this_predator`. This probability is a function of the
      !! distance between the predator and the agent and is calculated by the
      !! predator-class-bound procedure the_environment::predator::risk_fish().
      !! @note Note that this version of the procedure accepts `this_predator`
      !!       parameter as class the_neurobio::predator, i.e. for the
      !!       **objective predator object**.
      !! See `the_neurobio::predator_capture_probability_calculate_pred()`.
      procedure, public :: risk_pred_p =>                                     &
                                predator_capture_probability_calculate_pred
      !> Calculate the overall direct predation risk for the agent, i.e.
      !! the probability of attack and capture by the nearest predator.
      !! See `the_neurobio::predation_capture_probability_risk_wrapper()`.
      procedure, public :: risk_pred_w =>                                     &
                                predation_capture_probability_risk_wrapper
      !> A single generic interface for the calculation of the probability of
      !! attack and capture of the `this` agent by a predator.
      !! See `the_neurobio::predator_capture_probability_calculate_spatobj()`,
      !! `the_neurobio::predator_capture_probability_calculate_pred()` and
      !! `the_neurobio::predation_capture_probability_risk_wrapper()`.
      generic, public :: risk_pred => risk_pred_s, risk_pred_p, risk_pred_w

      !> Calculate the probability of capture of a subjective representation of
      !! food item based on the data from the perceptual memory stack. See
      !! `the_neurobio::food_perception_probability_capture_memory_object()`.
      procedure, public :: food_probability_capture_subjective =>             &
                            food_perception_probability_capture_memory_object

  end type PERCEPTION

  !> Perceptual components of motivational states. Plugged into all `STATE_`,
  !! attention etc. These components are linked to specific inner or outer
  !! perception objects (stimuli). Their sum result(s) in the overall
  !! value of the motivation component.
  type, public :: PERCEPT_COMPONENTS_MOTIV
    !> Light perception, direct environmental.
    real(SRP) :: light
    !> Depth perception, direct environmental.
    real(SRP) :: depth
    !> Perception of directly seen food items, spatial.
    real(SRP) :: food_dir
    !> Perception of the food items in the memory stack.
    real(SRP) :: food_mem
    !> Perception of conspecifics, spatial.
    real(SRP) :: conspec
    !> Direct perception of predators, spatial. Based on the distance
    !! to the nearest predator.
    real(SRP) :: pred_dir
    !> General perception of predation risk, spatial. Based on a sum of the
    !! number of predators in the perception object weighted by the number of
    !! predators in the memory stack.
    real(SRP) :: predator
    !> Perception of the stomach contents, direct, internal.
    real(SRP) :: stomach
    !> Perception of the body mass, direct, internal.
    real(SRP) :: bodymass
    !> Perception of the energy reserves, direct, internal.
    real(SRP) :: energy
    !> Age perception, direct internal.
    real(SRP) :: age
    !> Perception of the reproductive factor, based on the sex steroid
    !! hormones, calculated differently in males and females.
    real(SRP) :: reprfac
    contains
      !> Initialise perception components for a motivation state object.
      !! See `the_neurobio::perception_component_motivation_init_zero()`.
      procedure, public :: init => perception_component_motivation_init_zero
      !> Calculate the **maximum** value over all the perceptual components.
      !! See `the_neurobio::perception_component_maxval()`.
      procedure, public :: max_value => perception_component_maxval
      !> Calculate  individual perceptual components for **this** motivational
      !! state using the **neuronal response** function, for an agent. This
      !! agent has intent[in], so is **unchanged** in this procedure. Also
      !! `motivation_components` can take optional arbitrary (fake) perception
      !! values.
      !! @note This procedure is used for normal calculations of motivation
      !!    components. A similar method with the agent intent[inout]
      !!    the_neurobio::percept_components_motiv::motivation_components_init()
      !!    is used to initialise an agent.
      !! See
      !! `the_neurobio::perception_components_neuronal_response_calculate()`.
      procedure, public :: motivation_components =>                           &
                              perception_components_neuronal_response_calculate
      !> Calculate  individual perceptual components for **this** motivational
      !! state using the **neuronal response** function, for an agent. This
      !! agent has intent[inout], so **is changed** (gene labels reset).
      !> @warning  This procedure is used only for initialisation of an agent.
      !!       For normal calculation of the motivational components use
      !!       the_neurobio::percept_components_motiv::motivation_components()
      !!       procedure that does not change the actor agent (intent[in]).
      !! See `the_neurobio::perception_components_neuronal_response_init_set()`.
      procedure, public :: motivation_components_init =>                      &
                              perception_components_neuronal_response_init_set
      !> Initialise the attention components of the emotional state to their
      !! default parameter values. Attention sets weights to individual
      !! perceptual components when the overall weighted sum is calculated.
      !! The default weights are parameters defined in `COMMONDATA`.
      !! See `the_neurobio::perception_components_attention_weights_init()`.
      procedure, public :: attention_init =>                                  &
                                  perception_components_attention_weights_init
  end type PERCEPT_COMPONENTS_MOTIV

  !> These types describe the **neurobiological states** of the agent.
  !! (1) Each state may have several components that are related to specific
  !! inner or outer perception objects (stimuli). (2) There is also a
  !! `motivation` component that describes the global **motivation** value
  !! for this state.
  !!
  !! This is the **base type** that serves as root for all other
  !! motivation and emotion states, which are **extensions** of this
  !! the_neurobio::state_motivation_base type.
  type, abstract, public :: STATE_MOTIVATION_BASE
    !> Label for the motivation state, fixed, **cannot be changed**.
    !! @note Note that the label can be used as an **ID** for the motivational
    !!       state.
    !! @note Note that we cannot use `protected` attribute within derived type,
    !!       so make it `private` and implement the accessor function
    !!       \%label_is. The label component is then set in each derived
    !!       motivation object in its respective `clean_init` procedure.
    !! @note Note that the `clean_init` procedure is deferred (see abstract
    !!       interface) in this abstract type. Specific `clean_init` should
    !!       be implemented for each of the separate motivational/emotional
    !!       state type.
    !! @note The procedure `motivation_components` does not seem to be necessary
    !!       at this level of class hierarchy as it would duplicate that in
    !!       the_neurobio::percept_components_motiv. Therefore just call the
    !!       upper procedure \%percept_component\%motivation_components().
    character(len=LABEL_LENGTH), private :: label
    !> **Perceptual components**.
    type(PERCEPT_COMPONENTS_MOTIV) :: percept_component
    !> **Attention** sets the weights given to the individual perceptual
    !! components in the calculation of the motivation value.
    type(PERCEPT_COMPONENTS_MOTIV) :: attention_weight
    !> Overall **primary motivation values**.
    real(SRP) :: motivation_prim
    !> Overall **final** motivation value after modulation is performed.
    real(SRP) :: motivation_finl
    !> Overall GOS value, is this motivation state is dominant (TRUE/FALSE)?
    !> @note Note that only one state can be dominant at a time (Or not?
    logical   :: dominant_state
    contains
      !> Abstract **init** function that has to be overridden by each object
      !! that extends the basic motivational state type.
      !! @warning Needs abstract interface, with import of the base object
      !!          type `the_neurobio::state_motivation_base`.
      procedure(motivation_init_root), public, deferred :: clean_init
      !> These are basically the accessor `get`-functions, the `set`-functions
      !! are based on neural response from the perception object
      !! the_neurobio::appraisal.
      !> Get **light** perception component for this motivation state.
      !! See `the_neurobio::state_motivation_light_get()`.
      procedure, public :: get_light => state_motivation_light_get
      !> Get **depth** perception component for this motivation state.
      !! See `the_neurobio::state_motivation_depth_get()`.
      procedure, public :: get_depth => state_motivation_depth_get
      !> Get **directly perceived food** perception component for this
      !! motivation state. See `the_neurobio::state_motivation_food_dir_get()`.
      procedure, public :: get_food_dir => state_motivation_food_dir_get
      !> Get **food in past memory** perception component for this motivation
      !! state. See `the_neurobio::state_motivation_food_mem_get()`.
      procedure, public :: get_food_mem => state_motivation_food_mem_get
      !> Get **conspecifics** perception component for this motivation state.
      !! See `the_neurobio::state_motivation_conspec_get()`.
      procedure, public :: get_conspec => state_motivation_conspec_get
      !> Standard "get" function for the state neuronal **direct predation**
      !! effect component.
      !! See `the_neurobio::state_motivation_pred_dir_get()`.
      procedure, public :: get_pred_dir => state_motivation_pred_dir_get
      !> Get **predator** perception component for this motivation state.
      !! See `the_neurobio::state_motivation_predator_get()`.
      procedure, public :: get_predator => state_motivation_predator_get
      !> Get **stomach contents** perception component for this motivation
      !! state. See `the_neurobio::state_motivation_stomach_get()`.
      procedure, public :: get_stomach => state_motivation_stomach_get
      !> Get **body mass** perception component for this motivation state.
      !! See `the_neurobio::state_motivation_bodymass_get()`.
      procedure, public :: get_bodymass => state_motivation_bodymass_get
      !> Get **energy reserves** perception component for this motivation
      !! state. See `the_neurobio::state_motivation_energy_get()`.
      procedure, public :: get_energy => state_motivation_energy_get
      !> Get **age** perception component for this motivation state.
      !! See `the_neurobio::state_motivation_age_get()`.
      procedure, public :: get_age => state_motivation_age_get
      !> Get **reproductive factor** perception component for this motivation
      !! state. See `the_neurobio::state_motivation_reprfac_get()`.
      procedure, public :: get_reprfac => state_motivation_reprfac_get
      !> Get the overall **primary motivation value** (before modulation).
      !! See `the_neurobio::state_motivation_motivation_prim_get()`.
      procedure, public :: motivation_value_prim =>                           &
                                        state_motivation_motivation_prim_get
      !> Get the overall **final motivation value** (after modulation).
      !! See `the_neurobio::state_motivation_motivation_get()`.
      procedure, public :: motivation_value => state_motivation_motivation_get
      !> Check if the root state is the dominant state in GOS.
      !! See `the_neurobio::state_motivation_is_dominant_get()`.
      procedure, public :: is_dominant => state_motivation_is_dominant_get
      !> Get the fixed label for this motivational state. Note that the label
      !! is fixed and cannot be changed.
      !! See `the_neurobio::state_motivation_fixed_label_get()`.
      procedure, public :: label_is => state_motivation_fixed_label_get
      !> Transfer attention weights between two motivation state components.
      !! See `the_neurobio::state_motivation_attention_weights_transfer()`.
      procedure, public :: attention_copy =>                                  &
                                    state_motivation_attention_weights_transfer
      !> Calculate the maximum value over all perceptual components.
      !! See `the_neurobio::state_motivation_percept_maxval()`.
      procedure, public :: max_perception => state_motivation_percept_maxval
      !> Calculate the level of the **primary motivation**.
      !! See `the_neurobio::state_motivation_calculate_prim()`.
      procedure, public :: motivation_calculate =>                            &
                                  state_motivation_calculate_prim
  end type STATE_MOTIVATION_BASE

  !> Abstract interface for the deferred **init** function `clean_init` that
  !! has to be overridden by each object that extends the basic motivational
  !! state type.
  abstract interface
    elemental subroutine motivation_init_root(this)
      !> @warning Import base type. Without import gfortran issues this error:
      !!          `Error: Derived type 'state_motivation_base' at (1) is being
      !!          used before it is defined`.
      import :: STATE_MOTIVATION_BASE
      class(STATE_MOTIVATION_BASE), intent(inout) :: this
    end subroutine motivation_init_root
  end interface

  !> The motivational state of **hunger**. Evokes food seeking, eating, higher
  !! activity, emigrating and habitat switching.
  type, public, extends(STATE_MOTIVATION_BASE) :: STATE_HUNGER
    contains
      !> Init and cleanup **hunger** motivation object.
      !! See `the_neurobio::state_hunger_zero()`.
      procedure, public :: clean_init => state_hunger_zero
  end type STATE_HUNGER

  !> The state of **fear state**. Evokes active escape, fleeing,
  !! emigration and habitat switch.
  type, public, extends(STATE_MOTIVATION_BASE) :: STATE_FEAR_DEFENCE
    contains
      !> Init and cleanup **fear state** motivation object.
      !! See `the_neurobio::state_fear_defence_zero()`.
      procedure, public :: clean_init => state_fear_defence_zero
  end type STATE_FEAR_DEFENCE

  !> The state of **reproduction**. Evokes seeking conspecifics and
  !! mating during the reproductive phase.
  type, public, extends(STATE_MOTIVATION_BASE) :: STATE_REPRODUCE
    contains
      !> Init and cleanup **reproductive** motivation object.
      !! See `the_neurobio::state_reproduce_zero()`.
      procedure, public :: clean_init => state_reproduce_zero
  end type STATE_REPRODUCE

  !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! Define the **motivational/affective system** of the agent.
  !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  !> **Motivation** is a collection of all internal motivational states of
  !! the agent. This type is also used in defining *Expectancies* of
  !! motivations.
  type, public :: MOTIVATION
    !> - **hunger** is the state of the_neurobio::state_hunger.
    type(STATE_HUNGER)        :: hunger
    !> - **fear state** is the state of the_neurobio::state_fear_defence;
    type(STATE_FEAR_DEFENCE)  :: fear_defence
    !> - **reproduction** is the state of the_neurobio::state_reproduce;
    type(STATE_REPRODUCE)     :: reproduction
    !> - `number_of_states` is a private value indicating the total number of
    !!   motivational states. It is initialised to 4 in the
    !!   the_neurobio::motivation_init_all_zero().
    !! .
    integer, private          :: number_of_states
    contains
      !> Init the expectancy components to a zero state.
      !! See `the_neurobio::motivation_init_all_zero()`.
      procedure, public :: init => motivation_init_all_zero
      !> Calculate maximum value of the perception components across all
      !! motivations
      !! See `the_neurobio::motivation_max_perception_calc()`.
      procedure, public :: max_perception => motivation_max_perception_calc
      !> Return the vector of final motivation values for all motivational
      !! state components.
      !! See `the_neurobio::motivation_return_final_as_vector()`.
      procedure, public :: finals => motivation_return_final_as_vector
      !> Calculate the maximum value of the final motivations across all
      !! motivational state components.
      !! See `the_neurobio::motivation_maximum_value_motivation_finl()`.
      procedure, public :: max_final => motivation_maximum_value_motivation_finl
      !> Checks if the test value is the maximum **final** motivation value
      !! across all motivational state components.
      !! See `the_neurobio::motivation_val_is_maximum_value_motivation_finl()`.
      procedure, public :: is_max_final_val =>                                &
                              motivation_val_is_maximum_value_motivation_finl
      !> Checks if the test value is the maximum **final** motivation value
      !! across all motivational state components.
      !! See `the_neurobio::motivation_val_is_maximum_value_motivation_finl_o()`.
      procedure, public :: is_max_final_obj =>                                &
                              motivation_val_is_maximum_value_motivation_finl_o
        generic, public :: is_max_final => is_max_final_val, is_max_final_obj
      !> Reset all GOS indicators for this motivation object.
      !! See `the_neurobio::motivation_reset_gos_indicators()`.
      procedure, public :: gos_ind_reset => motivation_reset_gos_indicators
      !> Functions calculating the overall **motivation state values**.
      !! @important These functions calculate the **motivation state** of
      !!            the agent. This is a kind of a summator for the many
      !!            perception-specific state components into the unitary
      !!            inner **motivation state**.
      !! See `the_neurobio::motivation_primary_sum_components()`.
      procedure, public :: motivation_primary_calc =>                         &
                                         motivation_primary_sum_components
      !> Functions re-calculating the overall motivation values after
      !!            **modulation**.
      !! @note      Modulation modifies the motivation value based on other
      !!            properties of the agent with effect coefficients
      !!            depending on the genome.
      !! See `the_neurobio::motivation_modulation_absent()`.
      procedure, public :: modulation_none => motivation_modulation_absent

  end type MOTIVATION

  !> Individual motivation/emotion memory stack, a memory component that
  !! saves the values of the **final motivations** at previous time
  !! steps of the model. Not whole state (`STATE_`) objects are saved for
  !! simplicity. `add_to_history` is used in unmodified form. Decision making
  !! can make use of this emotional memory stack.
  type MEMORY_EMOTIONAL
    real(SRP), dimension(HISTORY_SIZE_MOTIVATION) :: hunger
    real(SRP), dimension(HISTORY_SIZE_MOTIVATION) :: defence_fear
    real(SRP), dimension(HISTORY_SIZE_MOTIVATION) :: reproduction
    !> Memory also includes a component for the global organismic state (GOS).
    !! @note Note that GOS cannot be determined at the `APPRAISAL` level, is
    !!       updated later, so we may need a separate add-to-memory function.
    character(len=LABEL_LENGTH), dimension(HISTORY_SIZE_MOTIVATION) :: gos_main
    !> Memory also includes the motivation level that has resulted in the
    !! current GOS. This is a memory for the arousal. Although doubles one of
    !! the basic motivations (hunger etc.), but here for convenience.
    real(SRP), dimension(HISTORY_SIZE_MOTIVATION) :: gos_arousal
    !> Memory also includes the GOS repeat counter, this is the number of
    !! times that the same GOS state is repeated.
    !! See `the_neurobio::gos_global` for implementation details.
    integer, dimension(HISTORY_SIZE_MOTIVATION) :: gos_repeated
    contains
      !> Add emotional components into the memory stack.
      !! See `the_neurobio::emotional_memory_add_to_stack()`.
      procedure, public :: add_to_memory => emotional_memory_add_to_stack
      !> Add the current GOS label or/and arousal value and/or arousal repeat
      !! count into the emotional memory stack.
      !! See `the_neurobio::emotional_memory_add_gos_to_stack()`.
      procedure, public :: gos_to_memory => emotional_memory_add_gos_to_stack
      !> Cleanup and destroy the emotional memory stack.
      !! See `the_neurobio::emotional_memory_cleanup_stack()`.
      procedure, public :: memory_cleanup => emotional_memory_cleanup_stack
      !> Get the average value of the hunger motivation state within the
      !! whole emotional memory stack.
      !! See `the_neurobio::emotional_memory_hunger_get_mean()`.
      procedure, public :: get_hunger_mean =>                                 &
                                    emotional_memory_hunger_get_mean
      !> Get the average value of the fear state motivation state within
      !! the whole emotional memory stack.
      !! See `the_neurobio::emotional_memory_actve_avoid_get_mean()`.
      procedure, public :: get_active_avoid_mean =>                           &
                                    emotional_memory_actve_avoid_get_mean
      !> Get the average value of the reproductive motivation state within the
      !! whole emotional memory stack.
      !! See `the_neurobio::emotional_memory_reproduct_get_mean()`.
      procedure, public :: get_reproduction_mean =>                           &
                                    emotional_memory_reproduct_get_mean
      !> Get the average value of the GOS arousal within the whole emotional
      !! memory stack.
      !! See `the_neurobio::emotional_memory_arousal_mean()`.
      procedure, public :: get_arousal => emotional_memory_arousal_mean
  end type MEMORY_EMOTIONAL

  !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  !> The **appraisal** level. At this level, perception objects are feed into
  !! the commondata::gamma2gene() sigmoid function and the neuronal responses
  !! are obtained at the output. Neuronal responses for different perception
  !! objects are then summed up and the promary motivation values are obtained.
  !! Following this, modulation alters some of the primary motivation values
  !! resulting in the final motivation values. See @ref aha_neurobio_flow
  !! "\"From perception to GOS\"" for an overview.
  type, public, extends(PERCEPTION) :: APPRAISAL
    !> The appraisal component plugs-in the different motivational/emotional
    !! objects.
    type(MOTIVATION)          :: motivations
    !> The emotional state memory stack object.
    type(MEMORY_EMOTIONAL)    :: memory_motivations
    contains
      !> Initialise and cleanup all appraisal object components and sub-objects.
      !! See `the_neurobio::appraisal_init_zero_cleanup_all()`.
      procedure, public :: init_appraisal => appraisal_init_zero_cleanup_all
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
      !! See `the_individual::appraisal_agent_set_dead()`.
      procedure, public ::  dies => appraisal_agent_set_dead
      !> Calculate **perception components** for each of the motivational state
      !! component.
      !! @brief      Initialise motivational states from perception objects
      !!             through the neuronal response function.
      !! @important  We initialise here all the **perception components**
      !!             (`the_neurobio::percept_components_motiv`) for every
      !!             **motivational state** component.
      !! See `the_neurobio::appraisal_perceptual_comps_motiv_neur_response_calculate()`.
      procedure, public :: motivations_percept_components =>                  &
                    appraisal_perceptual_comps_motiv_neur_response_calculate
      !> Calculate **primary motivation values** of the agent by summing up
      !! the perception components of each motivation state.
      !! @details Here it is just wrapper to the `the_neurobio::motivation`
      !!          -bound procedure `motivation_primary_calc`.
      !! See `the_neurobio::appraisal_primary_motivations_calculate()`.
      procedure, public :: motivations_primary_calc =>                        &
                                        appraisal_primary_motivations_calculate
      !> Calculate the **final motivation values** after **modulation**.
      !! @details Perform developmental and/or genetic modulation of primary
      !!          motivations that result in the final motivation values.
      !! @note    Genetic modulation backend
      !!          the_neurobio::appraisal_motivation_modulation_genetic() is
      !!          bound to the agent rather than the_neurobio::motivation.
      !! See `the_neurobio::appraisal_motivation_modulation_non_genetic()`.
      procedure, public :: modulation =>                                      &
                                    appraisal_motivation_modulation_non_genetic
      !> Add individual final emotional state components into the emotional
      !! memory stack.
      !! See `the_neurobio::appraisal_add_final_motivations_memory()`.
      procedure, public :: motivations_to_memory =>                           &
                                        appraisal_add_final_motivations_memory
      !> Calculate the probability of successful reproduction for `this` agent
      !! in its current state.
      !! @note  Note that this function is defined and bound to
      !!        `the_neurobio::appraisal` but used in `the_neurobio::reproduce`
      !!        behavioural component class.
      !! See `the_neurobio::reproduce_do_probability_reproduction_calc()`.
      procedure, public :: probability_reproduction =>                        &
                                    reproduce_do_probability_reproduction_calc
      !> Determine a stochastic outcome of **this** agent reproduction.
      !! Returns TRUE if the agent has reproduced successfully.
      !! See `the_neurobio::reproduction_success_stochast()`.
      procedure, public :: reproduction_success => reproduction_success_stochast
  end type APPRAISAL

  !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! Define the **Global Organismic State** of the agent.
  !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  !> Global organismic state (GOS) level. GOS is defined by the dominant
  !! motivational state component (`STATE_`), namely, by the logical flag
  !! \%dominant_state. If this logical flag is TRUE for a particular
  !! motivational state component, this state is the GOS. Thus, there should
  !! be is no separate data component(s) e.g. "value" for GOS. The values
  !!  the_neurobio::gos_global::gos_main and
  !! the_neurobio::gos_global::gos_arousal can be inferred from the
  !! motivations, here are doubled mainly for convenience. See @ref
  !! aha_neurobio_flow "\"From perception to GOS\"" for an overview.
  type, public, extends(APPRAISAL) :: GOS_GLOBAL
    !> Current global organismic state (GOS). Obtained from the GOS-specific
    !! emotional state \%label data component.
    character(len=LABEL_LENGTH) :: gos_main
    !> This is the current value of the dominant motivation.
    real(SRP) :: gos_arousal
    !> Integer number of the same GOS repetition, e.g. if GOS is the same the
    !! second time, gets 2 etc. Needed to asymptotically reduce GOS arousal
    !! when it is repeated, so smaller stimuli could overtake control.
    integer :: gos_repeated
    contains
      !> Initialise GOS engine components to a zero state.
      !! See `the_neurobio::gos_init_zero_state()`.
      procedure, public :: init_gos => gos_init_zero_state
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
      !! See `the_individual::gos_agent_set_dead()`.
      procedure, public ::  dies => gos_agent_set_dead
      !> Find and set the global organismic state (GOS) based on the various
      !! available motivation  values.
      !! See `the_neurobio::gos_find_global_state()`.
      procedure, public :: gos_find => gos_find_global_state
      !> Reset all motivation states as NOT dominant with respect to the GOS.
      !! See `the_neurobio::gos_reset_motivations_non_dominant()`.
      procedure, public :: gos_reset => gos_reset_motivations_non_dominant
      !> Get the current global organismic state (GOS).
      !! See `the_neurobio::gos_global_get_label()`.
      procedure, public :: gos_label => gos_global_get_label
      !> Get the overall level of arousal. Arousal is the current level
      !! of the dominant motivation that has brought about the current GOS
      !! at the previous time step.
      !! See `the_neurobio::gos_get_arousal_level()`.
      procedure, public :: arousal =>  gos_get_arousal_level
      !> Modulate the attention weights to suppress all perceptions alternative
      !! to the current GOS. This is done using the attention modulation
      !! interpolation curve.
      !! See `the_neurobio::gos_attention_modulate_weights()`.
      procedure, public :: attention_modulate => gos_attention_modulate_weights
  end type GOS_GLOBAL

  ! Implementation procedures for all "init" methods are private.
  private :: perception_objects_init_agent, appraisal_init_zero_cleanup_all,  &
             gos_init_zero_state

contains ! ........ implementation of procedures for this level ................

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with FOOD PERCEPTION
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Initiate an empty **food** perception object with known number of
  !! components.
  elemental subroutine percept_food_create_init(this, maximum_number_food_items)
    class(PERCEPT_FOOD), intent(inout) :: this

    !> @param[in] maximum_number_food_items Maximum number of food items in the
    !!            food perception object, normally equal to the partial food
    !!            resource indexing order
    !!            `commondata::food_select_items_index_partial`.
    integer, intent(in) :: maximum_number_food_items

    !  Local counter, not needed in newer vector form, TODO: delete after
    !  speed test
    !integer :: i

    if (.not. allocated(this%foods_seen))                                     &
               allocate(this%foods_seen(maximum_number_food_items))
    if (.not. allocated(this%foods_distances))                                &
               allocate(this%foods_distances(maximum_number_food_items))

    !> ### Implementation details ###
    ! Create every food item in the perception object array.
    ! do i = 1, maximum_number_food_items
    !   call this%foods_seen(i)%create()
    ! end do
    !> Create all food items in the perception array (`create` is elemental
    !! procedure).
    call this%foods_seen%create()

    !> Initialise all other components of the perception object.
    this%foods_distances = MISSING  !> array

    !> Set the initial number of food items in the perception object to the
    !! maximum number using the function `percept_food_number_seen` below.
    ! @note Call outside of this module:
    !!      `call this%number(maximum_number_food_items)`.
    this%food_seen_count = maximum_number_food_items

  end subroutine percept_food_create_init

  !-----------------------------------------------------------------------------
  !> Set the total number of food items perceived (seen) in the food
  !! perception object. Do not reallocate the perception object components
  !! with respect to this new number yet.
  subroutine percept_food_number_seen(this, number_set)
    class(PERCEPT_FOOD), intent(inout) :: this
    !> @param[in] Set the number of food items in the perception object.
    integer, intent(in) :: number_set

    this%food_seen_count = number_set

  end subroutine percept_food_number_seen

  !-----------------------------------------------------------------------------
  !> Make the food perception object, fill it with the actual data arrays.
  !! @note Note that the size and allocation is set by the `init` method.
  subroutine percept_food_make_fill_arrays(this, items, dist)
    class(PERCEPT_FOOD), intent(inout) :: this

    !> @param[in] items an array of food items that form the perception object.
    type(FOOD_ITEM), intent(in), dimension(:) ::  items

    !> @param[in] dist an array of the distances between the agent and each of
    !!            the food items in the perception object.
    real(SRP), intent(in), dimension(:) :: dist

    !  Local adjusted value of the number of food items seen
    !  @note Used in testing for non-conformant input arrays.
    integer :: n_adjusted

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(percept_food_make_fill_arrays)"

    !> ### Implementation details ###
    !> First we check for non-conforming input arrays and re-init and
    !! reallocate the perception object, if needed, to the minimum value.
    CHECK_CONFORMING: if (size(items) /= size(dist)) then
      n_adjusted = (min(size(items), size(dist)))
      call this%destroy()
      call this%init(n_adjusted)
      !> Report this issue to the log.
      call LOG_MSG("WARNING: " // PROCNAME //                                 &
                   ": Non-conforming input arrays, re-initialised to " //     &
                   TOSTR(n_adjusted) // " food items.")
    end if CHECK_CONFORMING

    !> Second, fill the dynamic food perception object with the data
    !! from the input arrays. They should have conforming sizes now.
    this%foods_seen = items
    this%foods_distances = dist

  end subroutine percept_food_make_fill_arrays

  !-----------------------------------------------------------------------------
  !> Get the number (count) of food items seen. Trivial.
  elemental function percept_food_get_count_found (this) result (count_obj)
    class(PERCEPT_FOOD), intent(in) :: this
    integer :: count_obj

    count_obj = this%food_seen_count

  end function percept_food_get_count_found

  !-----------------------------------------------------------------------------
  !> Get the average size of food items seen. Trivial.
  elemental function percept_food_get_meansize_found (this) result (size_obj)
    class(PERCEPT_FOOD), intent(in) :: this
    real(SRP) :: size_obj

    if (this%food_seen_count <1) then
      size_obj = 0.0_SRP
    else
      size_obj = average(this%foods_seen%size)
    end if

  end function percept_food_get_meansize_found

  !-----------------------------------------------------------------------------
  !> Get the average mass of food items seen. Trivial.
  elemental function percept_food_get_meanmass_found (this) result (mass_obj)
    class(PERCEPT_FOOD), intent(in) :: this
    real(SRP) :: mass_obj

    if (this%food_seen_count < 1) then
      mass_obj = 0.0_SRP
    else
      ! @note Alternatively can also use OO frontent for mass directly:
      !       mass_obj = average( this%foods_seen%get_mass() )
      mass_obj = average( size2mass_food(this%foods_seen%size) )
    end if

  end function percept_food_get_meanmass_found

  !-----------------------------------------------------------------------------
  !> Get the average distance to the food items seen. Trivial.
  elemental function percept_food_get_meandist_found (this) result (dist_obj)
    class(PERCEPT_FOOD), intent(in) :: this
    real(SRP) :: dist_obj

    if (this%food_seen_count <1) then
      dist_obj = MISSING  !> If no food items seen, we have undefined distance.
    else
      dist_obj = average(this%foods_distances)
    end if

  end function percept_food_get_meandist_found

  !-----------------------------------------------------------------------------
  !> Deallocate and delete a **food** perception object.
  elemental subroutine percept_food_destroy_deallocate(this)
    class(PERCEPT_FOOD), intent(inout) :: this

    if (allocated(this%foods_seen)) deallocate(this%foods_seen)
    if (allocated(this%foods_distances)) deallocate(this%foods_distances)
    this%food_seen_count = UNKNOWN

  end subroutine percept_food_destroy_deallocate

  !-----------------------------------------------------------------------------
  !> Get available food items within the visual range of the agent, which the
  !! agent can perceive and therefore respond to. Food perception is packaged
  !! into the food perception object this\%perceive_food for output.
  !!
  !! **Food perception** is quite complex to implement as it requires
  !! determining individual food items within the current visual range
  !! of the agent. There are, however, potentially thousands (or
  !! millions) of food items in the food resource, each of the food
  !! items is stochastic (e.g. they have different sizes), so visual
  !! range differ for each item and each agent should determine
  !! food items in its proximity at numerous time steps of the model.
  !! This means repeating huge loops many times for each agent at
  !! each time step. This is approached by array segmentation: the
  !! perception object is obtained by *partial indexing* of a very
  !! limited number (=`commondata::food_select_items_index_partial`) of
  !! only the nearest food items, the agent's visual range is then
  !! determined for each of this nearest neighbouring food items, and
  !! finally those food items that individually fall within the visual
  !! range are included into the perception object.
  !! @note Note that there are three similar procedures that detect spatial
  !!       objects within the visual range of the agent:
  !!       - the_neurobio::perception::see_food -- perception of food items;
  !!       - the_neurobio::perception::see_consp -- perception of conspecifics:
  !!       - the_neurobio::perception::see_pred -- perception of predators.
  !!       .
  !!       All these procedures were actually implemented using the first
  !!       (the_neurobio::perception::see_food) as a template. All three
  !!       implement partial indexing of the nearest spatial objects to
  !!       accelerate computation of large arrays of spatial objects.
  subroutine food_perception_get_visrange_objects (this,                      &
                                                food_resource_available,      &
                                                time_step_model)
    class(PERCEPTION), intent(inout) :: this

    !> @param[in] food_resource_available Global food resource object from
    !!            which we select neighbouring item components that are
    !!            present within the visual range of the agent.
    class(FOOD_RESOURCE), intent(in) :: food_resource_available

    !> @param[in] time_step_model The current time step of the model.
    integer, optional, intent(in) :: time_step_model

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME =                                 &
                                      "(food_perception_get_visrange_objects)"

    !> ### Notable variables and parameters ###
    !> - **dist_foods** - temporary array of food items
    !!   (`the_environment::food_item`) available to the agent.
    !    @note We cannot just place neighbours = food_resource_available\%food
    !        in the call to the this\%neighbours(). function and need
    !        a raw array of `the_environment::food_item`'s.
    !    @note This is a component of the input array of objects we search among.
    type(FOOD_ITEM), dimension(size(food_resource_available%food)) :: dist_foods

    !> - **dist_food_neighbours** - temporary array of the distances to the
    !!   neighbouring food items.
    !    @note Note that we determine the size of the array the same as the
    !        whole input food resource size. We can use the number
    !        `number_food_items`, but determining from array is safer if
    !        `number_food_items` parameter goes un-updated for some reasons.
    real(SRP), dimension(size(food_resource_available%food)) ::               &
                                                          dist_food_neighbours

    !> - **dist_food_index** - temporary partial index vector for the
    !!   distances to the neighbouring food items.
    integer, dimension(size(food_resource_available%food))  :: dist_food_index

    ! Temporary possible error status for sub-procedures
    logical :: dist_food_errflag

    !> - **irradiance_agent_depth** - local variable defining the irradiance
    !!   (illumination) at the current depth of the agent. Needed to calculate
    !!   the agent's visual range.
    real(SRP) :: irradiance_agent_depth

    !> - **food_item_area** - local variable defining the area of the food
    !!   item. It is an array, area of each item in the
    !!   `food_resource_available` and `dist_foods`. Needed to calculate
    !!   the agent's visual range.
    real(SRP), dimension(size(food_resource_available%food)) :: food_item_area

    !> - **food_item_visual_range** - local variable defining the visual range
    !!   of the agent for detecting each of the food items (with known areas)
    !!   at the agent's current depth.
    real(SRP), dimension(size(food_resource_available%food)) ::               &
                                                        food_item_visual_range

    !> - **food_items_percept_in_visrange** - local sorted array of food
    !!   objects that are within the visual range of the agent for output.
    !!   The array should normally have the size of
    !!   commondata::food_select_items_index_partial  elements, but only the
    !!   first `food_items_n_visrange` elements of it are actually within the
    !!   visual range.
    type(FOOD_ITEM), dimension(FOOD_SELECT_ITEMS_INDEX_PARTIAL) ::            &
                                              food_items_percept_in_visrange

    !> - **food_items_dist_sorted** - temporary local sorted array of
    !!   distances between the agent and each of the nearest neighbouring food
    !!   items, sorted for output.
    real(SRP), dimension(FOOD_SELECT_ITEMS_INDEX_PARTIAL) ::                  &
                                              food_items_dist_sorted

    !> - **food_items_n_visrange** - local number of elements of
    !!   `food_items_percept_in_visrange` for output that are within he
    !!   visual range of the agent.
    !! .
    integer ::  food_items_n_visrange

    ! Local copy of the time step of the model.
    integer :: time_step_model_here

    ! Local counter
    integer :: i

    !> ### Implementation details ###
    !> #### Checks and preparations ####
    !> Check optional time step parameter. If unset, use global
    !! `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

    !> Initialise index and rank values. Unititialised index arrays may result in
    !! invalid memory reference in `ARRAY_INDEX` (it is not safe by design).
    dist_food_neighbours = MISSING ; dist_food_index = UNKNOWN
    food_item_area = MISSING ; food_item_visual_range = MISSING
    food_items_dist_sorted = MISSING

    !> Copy food items array component from the `food_resource_available`
    !! class' the_environment::food_item's array.
    !! @warning Note that we cannot here call
    !!   @verbatim
    !!     call dist_foods%position( food_resource_available%food%location() )
    !!   @endverbatim
    !!   as the objects are `the_environment::food_item` higher level
    !!   than `the_environment::spatial`.
    dist_foods = food_resource_available%food

    !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    !> #### Step 1 ####
    !> First, we determine up to the maximum order (fast *partial indexing*)
    !! of `commondata::food_select_items_index_partial` neighbouring food
    !! items that are in proximity of the agent. This is done using the
    !! the_environment::spatial::neighbours() backend procedure.
    call this%neighbours( neighbours = dist_foods,                            &
                          dist = dist_food_neighbours,                        &
                          index_vector = dist_food_index,                     &
                          rank_max = FOOD_SELECT_ITEMS_INDEX_PARTIAL,         &
                          error_flag = dist_food_errflag )

    if (dist_food_errflag) call LOG_MSG ( LTAG_WARN // PROCNAME //            &
                    ": Got error flag from food object neighbours procedure.")

    !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    !> #### Step 2 ####
    !> Second, we select only those items within this set, which are within
    !! the visual range of the agent under the current conditions.
    !! To do this we, first, calculate the ambient illumination/irradiance
    !! level at the depth of the agent. Done using the
    !! the_environment::spatial::illumination() procedure.
    irradiance_agent_depth = this%illumination(time_step_model_here)

    !> Compute the array of "prey areas" for each of the food items (whole
    !! array or neighbouring food items only).
    food_item_area(dist_food_index(1:FOOD_SELECT_ITEMS_INDEX_PARTIAL)) =      &
      carea( cm2m( dist_foods(                                                &
                   dist_food_index(1:FOOD_SELECT_ITEMS_INDEX_PARTIAL) )%size )&
            )

    !> Compute the vector of the visual ranges for detecting each of
    !! the food items by the agent.
    food_item_visual_range(                                                   &
                          dist_food_index(1:FOOD_SELECT_ITEMS_INDEX_PARTIAL)  &
                          )                                                   &
              = m2cm( visual_range(                                           &
                        irradiance = irradiance_agent_depth,                  &
                        prey_area = food_item_area(                           &
                            dist_food_index(1:FOOD_SELECT_ITEMS_INDEX_PARTIAL)&
                            )                                                 &
                      )                                                       &
                    )

    !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    !> #### Step 3 ####
    !> Now we can get the pre-output array `food_items_percept_in_visrange`
    !! that contains the food objects available within the visual range of
    !! the agent. Also, we count the number of such items for the output
    !! parameter `food_items_n_visrange`.
    food_items_n_visrange = 0
    do i=1, FOOD_SELECT_ITEMS_INDEX_PARTIAL
      if ( dist_food_neighbours(dist_food_index(i)) <                         &
                              food_item_visual_range(dist_food_index(i)) ) then
        !> Also, check if the food item is available (not eaten).
        if (dist_foods(dist_food_index(i))%is_available()) then
          food_items_n_visrange = food_items_n_visrange + 1
          food_items_percept_in_visrange(food_items_n_visrange) =             &
                                                dist_foods(dist_food_index(i))
          food_items_dist_sorted(food_items_n_visrange) =                     &
                                      dist_food_neighbours(dist_food_index(i))
        else
          call LOG_DBG ( LTAG_INFO // PROCNAME // ": Food item within " //    &
                         "visual range but NOT available (eaten)." )
        end if
      end if
    end do
    !> Here we also log warning if no food items found, when debugging
    !! (see commondata::is_debug).
    if (food_items_n_visrange==0) call LOG_DBG( LTAG_WARN // PROCNAME //      &
            ": No food items found within the visual range of the agent; " // &
            "The nearest food item distance=" //                              &
            TOSTR(dist_food_neighbours(dist_food_index(1))) //                &
            ", with visual range=" //                                         &
            TOSTR(food_item_visual_range(dist_food_index(1))) )

    !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    !> #### Step 4 ####
    !> Finally, we can now create the output food perception object,
    !! including only food items that are within the current visual range
    !! of the agent.
    !! Init (create and allocate) the food perception object (at first empty)
    !! using the the_neurobio::percept_food::init().
    call this%perceive_food%init(food_items_n_visrange)

    !> Fill the output perception object with the values obtained at
    !! the step 3. This is done using the the_neurobio::percept_food::make()
    !! backend procedure.
    call this%perceive_food%make (                                            &
                    food_items_percept_in_visrange(1:food_items_n_visrange),  &
                    food_items_dist_sorted(1:food_items_n_visrange) )

  end subroutine food_perception_get_visrange_objects

  !-----------------------------------------------------------------------------
  !> Check if the agent sees any food items within its visual range.
  !! @warning Should be called after the `see_food` method as it is only an
  !!          accessor get-function.
  elemental function food_perception_is_seeing_food(this) result (sees_food)
    class(PERCEPTION), intent(in) :: this
    !> @return Returns TRUE if the agent has any food items in its perception
    !!         object and FALSE otherwise.
    logical :: sees_food

    sees_food = .FALSE.
    if (this%perceive_food%get_count() > 0 ) sees_food = .TRUE.

  end function food_perception_is_seeing_food

  !-----------------------------------------------------------------------------
  !> Calculate the probability of capture of a subjective representation of
  !! food item based on the data from the perceptual memory stack.
  function food_perception_probability_capture_memory_object(this, last,      &
                                        time_step_model ) result (capture_prob)
    class(PERCEPTION), intent(in) :: this
    !> @param last Limit to only this number of latest components in history.
    integer, optional, intent(in) :: last
    !> @param[in] time_step_model optional time step of the model, if absent,
    !!       obtained from the global variable
    !!       `commondata::global_time_step_model_current`.
    integer, optional, intent(in) :: time_step_model
    !> @return Capture probability of the "subjective" food item that has
    !!         the size equal to the size of the average memorised food items
    !!         (from the agent's perception memory stack) and located at an
    !!         average distance of food items from the memory stack.
    real(SRP) :: capture_prob

    ! Local copies of optionals
    integer :: time_step_model_here

    !> ### Implementation notes ###
    !> `subjective_food_item_average` of the type the_environment::food_item
    !! is a subjective representation of the food item object built from the
    !! memory stack data.
    type(FOOD_ITEM) :: subjective_food_item_average

    !> First, check optional time step parameter. If unset, use global
    !! commondata::global_time_step_model_current.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> Second, build the subjective food item `subjective_food_item_average`
    !! using the the_environment:: food_item::make() method. The location of
    !! this subjective food item coincides with the location of the agent.
    !! This allows to calculate the visibility (visual range) of the food items
    !! bat the depth of the agent.
    !!
    !! Then the capture probability is calculated using the type-bound
    !! method the_environment::food_item::capture_probability(). Importantly,
    !! the distance towards towards the food item is explicitly provided
    !! as the average distance from the memory stack calculated by the
    !! the_neurobio::memory_perceptual::get_food_mean_dist().
    if (present(last)) then
      call subjective_food_item_average%make(                                 &
                      location = this%location(),                             &
                      size = this%memory_stack%get_food_mean_size(last),      &
                      iid = UNKNOWN  )
      capture_prob = subjective_food_item_average%capture_probability(        &
                      distance = this%memory_stack%get_food_mean_dist(last),  &
                      time_step_model = time_step_model_here )
    else
      call subjective_food_item_average%make(                                 &
                      location = this%location(),                             &
                      size = this%memory_stack%get_food_mean_size(),          &
                      iid = UNKNOWN  )
      capture_prob = subjective_food_item_average%capture_probability(        &
                      distance = this%memory_stack%get_food_mean_dist(),      &
                      time_step_model = time_step_model_here )
    end if

    !> Finally, we add a random Gaussian error to the above objective value.
    !! Now we have obtained the stochastic subjective value of the capture
    !! probability for this food item including a Gaussian error. There is
    !! also a strong limitation for the subjective probability to be within
    !! the range [0.0, 1.0]. See also ::subjective_capture_prob() for a
    !! similar Gaussian error in subjective probability.
    capture_prob = within( RNORM( capture_prob, cv2variance(                  &
                          FOOD_ITEM_CAPTURE_PROBABILITY_SUBJECTIVE_ERRORR_CV, &
                          capture_prob) ),                   0.0_SRP, 1.0_SRP )

  end function food_perception_probability_capture_memory_object

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with STOMACH PERCEPTION
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Initiate an empty **stomach** capacity perception object.
  elemental subroutine percept_stomach_create_init(this)
    class(PERCEPT_STOMACH), intent(inout) :: this

    !> First, assign the current stomach capacity to `MISSING`.
    this%capacity = MISSING

  end subroutine percept_stomach_create_init

  !-----------------------------------------------------------------------------
  !> Get the currently available value of the available **stomach** volume.
  elemental function percept_stomach_get_avail_capacity (this) result(avail_capacity)
    class(PERCEPT_STOMACH), intent(in) :: this

    !> @returns the stomach capacity currently available for new food.
    real(SRP) :: avail_capacity

    ! Return the current capacity.
    avail_capacity = this%capacity

  end function percept_stomach_get_avail_capacity

  !-----------------------------------------------------------------------------
  !> Set and update the currently available value of the available **stomach**
  !! volume.
  subroutine percept_stomach_update_avail_capacity(this, current_volume)
    class(PERCEPT_STOMACH), intent(inout) :: this

    !> @param current_volume the new (updated) current volume of the
    !!        stomach capacity.
    real(SRP), intent(in) ::current_volume

    ! And then place the parameter value to the updated object.
    this%capacity = current_volume

  end subroutine percept_stomach_update_avail_capacity

  !-----------------------------------------------------------------------------
  !> Destroy the **stomach** perception object and deallocate it.
  elemental subroutine percept_stomach_destroy_deallocate(this)
    class(PERCEPT_STOMACH), intent(inout) :: this

    !> Set the current value to commondata::missing.
    this%capacity = MISSING

  end subroutine percept_stomach_destroy_deallocate

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with BODY MASS PERCEPTION
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Initiate an empty **body mass** perception object.
  elemental subroutine percept_bodymass_create_init(this)
    class(PERCEPT_BODY_MASS), intent(inout) :: this

    !> Assign the current body mass to `commondata::missing`.
    this%body_mass = MISSING

  end subroutine percept_bodymass_create_init

  !-----------------------------------------------------------------------------
  !> Get the current value of the **body mass** perception.
  elemental function percept_bodymass_get_current (this) result(current)
    class(PERCEPT_BODY_MASS), intent(in) :: this

    !> @returns the current body mass value.
    real(SRP) :: current

    ! Return the current mass.
    current = this%body_mass

  end function percept_bodymass_get_current

  !-----------------------------------------------------------------------------
  !> Set and update the current **body mass** perception value.
  subroutine percept_bodymass_update_current(this, current)
    class(PERCEPT_BODY_MASS), intent(inout) :: this

    !> @param current the new (updated) current volume of the
    !!        stomach capacity.
    real(SRP), intent(in) ::current

    ! And then place the parameter value to the updated object.
    this%body_mass = current

  end subroutine percept_bodymass_update_current

  !-----------------------------------------------------------------------------
  !> Destroy the **body mass** perception object and deallocate.
  elemental subroutine percept_bodymass_destroy_deallocate(this)
    class(PERCEPT_BODY_MASS), intent(inout) :: this

    !> Set the current value to commondata::missing.
    this%body_mass = MISSING

  end subroutine percept_bodymass_destroy_deallocate

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with ENERGY PERCEPTION
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Initiate an empty **energy** perception object.
  elemental subroutine percept_energy_create_init(this)
    class(PERCEPT_ENERGY), intent(inout) :: this

    !> Assign the current energy to commondata::missing.
    this%energy_reserve = MISSING

  end subroutine percept_energy_create_init

  !-----------------------------------------------------------------------------
  !> Get the current value of the **energy** reserves.
  elemental function percept_energy_get_current (this) result(current)
    class(PERCEPT_ENERGY), intent(in) :: this

    !> @returns the current energy reserve.
    real(SRP) :: current

    ! Return the current capacity.
    current = this%energy_reserve

  end function percept_energy_get_current

  !-----------------------------------------------------------------------------
  !> Set and update the current **energy** perception value.
  subroutine percept_energy_update_current(this, current)
    class(PERCEPT_ENERGY), intent(inout) :: this

    !> @param current the new (updated) current energy reserves.
    real(SRP), intent(in) ::current

    ! And then place the parameter value to the updated object.
    this%energy_reserve = current

  end subroutine percept_energy_update_current

  !-----------------------------------------------------------------------------
  !> Destroy the **energy** perception object and deallocate.
  elemental subroutine percept_energy_destroy_deallocate(this)
    class(PERCEPT_ENERGY), intent(inout) :: this

    ! Set the current value to commondata::missing.
    this%energy_reserve = MISSING

  end subroutine percept_energy_destroy_deallocate

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with AGE PERCEPTION
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Initiate an empty **age** perception object.
  elemental subroutine percept_age_create_init(this)
    class(PERCEPT_AGE), intent(inout) :: this

    !> Assign the current age to commondata::unknown.
    this%age = UNKNOWN

  end subroutine percept_age_create_init

  !-----------------------------------------------------------------------------
  !> Get the current value of the **age** reserves.
  elemental function percept_age_get_current (this) result(current)
    class(PERCEPT_AGE), intent(in) :: this

    !> @returns the current age.
    integer :: current

    ! Return the current age.
    current = this%age

  end function percept_age_get_current

  !-----------------------------------------------------------------------------
  !> Set and update the current **age** perception value.
  subroutine percept_age_update_current(this, current)
    class(PERCEPT_AGE), intent(inout) :: this

    !> @param current the new (updated) current age.
    integer, intent(in) ::current

    ! And then place the parameter value to the updated object.
    this%age = current

  end subroutine percept_age_update_current

  !-----------------------------------------------------------------------------
  !> Destroy the **age** perception object and deallocate it.
  elemental subroutine percept_age_destroy_deallocate(this)
    class(PERCEPT_AGE), intent(inout) :: this

    !> Set the current value to commondata::unknown.
    this%age = UNKNOWN

  end subroutine percept_age_destroy_deallocate

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with SPATIAL PERCEPTIONS
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Set unique **id** for the conspecific perception component.
  subroutine spatial_percept_set_cid(this, id)
    class(SPATIAL_PERCEPT_COMPONENT), intent(inout) :: this

    !> @param iid optional individual id number for the food item.
    integer, optional, intent(in) :: id

    !> ### Implementation details ###
    !> **HUGE_ID** is a local parameter, the maximum unique id ever possible.
    integer, parameter :: HUGE_ID = huge(0)

    !> Check if conspecific cid is provided and if not, set random within the
    !! huge range.
    if (present(id)) then
      this%cid = id
    else
      this%cid = RAND_I(1, HUGE_ID)
    end if

  end subroutine spatial_percept_set_cid

  !-----------------------------------------------------------------------------
  !> Get the unique **id** of the food item object.
  elemental function spatial_percept_get_cid(this) result(id)
    class(SPATIAL_PERCEPT_COMPONENT), intent(in) :: this

    !> @returns cid the individual id number of this perception component.
    integer :: id

    id = this%cid

  end function spatial_percept_get_cid

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with CONSPECIFIC PERCEPTION
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Create a single conspecific perception component at an undefined
  !! position with default properties.
  elemental subroutine consp_percept_comp_create(this)
    class(CONSPEC_PERCEPT_COMP), intent(inout) :: this

    !> ### Implementation details ###
    !> We here just set an undefined location of the food object using
    !! standard interface function `missing`.
    call this%missing()

    !> Set cid to UNKNOWN.
    !> @warning random id on create is now disabled to allow elemental function,
    !!          because random are never pure. So care to set cid's elsewhere.
    this%cid = UNKNOWN

    !> Then we set the conspecific size. Should it be MISSING or grand average?
    !! this%consp_body_size = MISSING
    this%consp_body_size = (BODY_LENGTH_MAX - BODY_LENGTH_MIN) / 2.0_SRP

    !> Set the conspecific mass. The default mass of the conspecific is
    !! twice the minimum body mass. There is no upper limit on the body mass.
    !! @note These values are not very important as they are for default init
    !!       only and will be overwritten by the actual values.
    this%consp_body_mass = BODY_MASS_MIN * 2.0_SRP

    !> Init distance towards the conspecific, now with MISSING value.
    this%consp_distance = MISSING

    !> Init the sex is male by default, it is arbitrary.
    this%sex_is_male = .TRUE.

  end subroutine consp_percept_comp_create

  !-----------------------------------------------------------------------------
  !> Make a single conspecific perception component. This is a single
  !! conspecific located within the visual range of the agent.
  subroutine consp_percept_make(this, location, size, mass, dist, cid, is_male)
    class(CONSPEC_PERCEPT_COMP), intent(inout) :: this

    !> @param Location of the conspecific perception component, as a
    !!        `SPATIAL` type container
    type(SPATIAL), intent(in) :: location

    !> @param size This is the optional conspecific body size as guessed by
    !!        the agent. May or may not reflect the "true" size of the
    !!        conspecific.
    real(SRP), optional, intent(in) :: size

    !> @param mass This is the optional conspecific body mass as guessed by
    !!        the agent. May or may not reflect the "true" mass of the
    !!        conspecific.
    real(SRP), optional, intent(in) :: mass

    !> @param dist The distance towards this conspecific.
    real(SRP), optional, intent(in) :: dist

    !> @param iid Optional cid for the conspecific perception component.
    !!        If not provided, set random.
    integer, optional, intent(in) :: cid

    !> @param is_male Optional flag that sex is male.
    logical, optional, intent(in) :: is_male

    !> ### Implementation details ###
    !> We here just set the location of the food object using
    !! standard interface function `position`.
    call this%position(location)

    !> If individual id is provided, set it. If not, set random.
    if (present(cid)) then
      call this%set_cid(cid)
    else
      call this%set_cid()
    end if

    !> Then we set the conspecific perception component body size.
    !! Check if optional size is provided and left untouched if not.
    if (present(size)) then
      this%consp_body_size = min( max(BODY_LENGTH_MIN, size), BODY_LENGTH_MAX )
    end if

    !> Then we set the conspecific perception component body mass.
    !! Check if optional size is provided and left untouched if not.
    if (present(mass)) then
      this%consp_body_mass = max(BODY_MASS_MIN, mass)
    end if

    !> Also set the distance towards the conspecific if provided. If not
    !! provided, ignore.
    if (present(dist)) then
      this%consp_distance = dist
    end if

    if (present(is_male)) then
      this%sex_is_male = is_male
    end if

  end subroutine consp_percept_make

  !-----------------------------------------------------------------------------
  !> Get the **conspecific** perception component body size.
  elemental function consp_percept_get_size (this) result (body_size)
    class(CONSPEC_PERCEPT_COMP), intent(in) :: this

    real(SRP) :: body_size

    body_size = this%consp_body_size

  end function consp_percept_get_size
 !-----------------------------------------------------------------------------
  !> Get the **conspecific** perception component body mass.
  elemental function consp_percept_get_mass (this) result (body_mass)
    class(CONSPEC_PERCEPT_COMP), intent(in) :: this

    real(SRP) :: body_mass

    body_mass = this%consp_body_mass

  end function consp_percept_get_mass

  !-----------------------------------------------------------------------------
  !> Get the **conspecific** perception component distance.
  elemental function consp_percept_get_dist (this) result (dist_consp)
    class(CONSPEC_PERCEPT_COMP), intent(in) :: this

    real(SRP) :: dist_consp

    dist_consp = this%consp_distance

  end function consp_percept_get_dist

  !-----------------------------------------------------------------------------
  !> Get the **conspecific** perception component sex flag (male).
  elemental function consp_percept_sex_is_male_get (this) result (sex_is_male)
    class(CONSPEC_PERCEPT_COMP), intent(in) :: this
    logical :: sex_is_male

    sex_is_male = this%sex_is_male

  end function consp_percept_sex_is_male_get

  !-----------------------------------------------------------------------------
  !> Get the **conspecific** perception component sex flag (female).
  elemental function consp_percept_sex_is_female_get (this) result (sex_is_female)
    class(CONSPEC_PERCEPT_COMP), intent(in) :: this
    logical :: sex_is_female

    if (this%sex_is_male) then
      sex_is_female = .FALSE.
    else
      sex_is_female = .TRUE.
    end if

  end function consp_percept_sex_is_female_get

  !-----------------------------------------------------------------------------
  !> Create conspecifics perception object, it is an array of
  !! conspecific perception components.
  elemental subroutine percept_consp_create_init (this, maximum_number_conspecifics)
    class(PERCEPT_CONSPECIFICS), intent(inout) :: this

    !> @param maximum_number_conspecifics The maximum number of conspecifics
    !!        in the conspecifics perception object. Normally equal to the
    !!        partial conspecific selection indexing order
    !!        `CONSP_SELECT_ITEMS_INDEX_PARTIAL`.
    integer, intent(in) :: maximum_number_conspecifics

    !> ### Implementation details ###
    !> Allocate the array of the conspecific perception components
    if (.not. allocated(this%conspecifics_seen))                              &
              allocate (this%conspecifics_seen(maximum_number_conspecifics))

    !> And create all perception components (create is `elemental`).
    call this%conspecifics_seen%create()

    !> Set the initial number of conspecifics within the visual range
    !! to the maximum number provided.
    call this%number(maximum_number_conspecifics)

  end subroutine percept_consp_create_init

  !-----------------------------------------------------------------------------
  !> Set the total number of conspecifics perceived (seen) in the conspecific
  !! perception object. But do **not** reallocate the conspecific perception
  !! components so far.
  elemental subroutine percept_consp_number_seen(this, number_set)
    class(PERCEPT_CONSPECIFICS), intent(inout) :: this

    !> @param[in] number_set Set the number of conspecifics in the perception
    !!            object.
    integer, intent(in) :: number_set

    this%conspecifics_seen_count = number_set

  end subroutine percept_consp_number_seen

  !-----------------------------------------------------------------------------
  !> Make the conspecifics perception object, fill it with the actual arrays.
  !! @note Note that the size and allocation is set by the `init` method.
  pure subroutine percept_consp_make_fill_arrays(this, consps)
    class(PERCEPT_CONSPECIFICS), intent(inout) :: this

    !> @param[in] consps an array of conspecific perception components that
    !!            form the perception object.
    type(CONSPEC_PERCEPT_COMP), intent(in), dimension(:) ::  consps

    !> PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(percept_consp_make_fill_arrays)"

    !> ### Implementation details ###
    !> Fill the dynamic conspecific perception object with the data from
    !! the input array.
    this%conspecifics_seen = consps

  end subroutine percept_consp_make_fill_arrays

  !-----------------------------------------------------------------------------
  !> Get the number (count) of conspecifics seen. Trivial.
  elemental function percept_consp_get_count_seen (this) result (count_obj)
    class(PERCEPT_CONSPECIFICS), intent(in) :: this
    integer :: count_obj

    count_obj = this%conspecifics_seen_count

  end function percept_consp_get_count_seen

  !-----------------------------------------------------------------------------
  !> Deallocate and delete a conspecific perception object.
  elemental subroutine percept_consp_destroy_deallocate(this)
    class(PERCEPT_CONSPECIFICS), intent(inout) :: this

    if (allocated(this%conspecifics_seen)) deallocate(this%conspecifics_seen)
    this%conspecifics_seen_count = UNKNOWN

  end subroutine percept_consp_destroy_deallocate

  !-----------------------------------------------------------------------------
  !> Get available conspecific perception objects within the visual range of
  !! the agent, which the agent can perceive and therefore respond to.
  !! @note Note that there are three similar procedures that detect spatial
  !!       objects within the visual range of the agent:
  !!       - the_neurobio::perception::see_food -- perception of food items;
  !!       - the_neurobio::perception::see_consp -- perception of conspecifics:
  !!       - the_neurobio::perception::see_pred -- perception of predators.
  !!       .
  !!       All these procedures were actually implemented using the first
  !!       (the_neurobio::perception::see_food) as a template. All three
  !!       implement partial indexing of the nearest spatial objects to
  !!       accelerate computation of large arrays of spatial objects.
  subroutine consp_perception_get_visrange_objects (this,                     &
                                                consp_agents,                 &
                                                time_step_model )
    class(PERCEPTION), intent(inout) :: this

    !> @param[in] consp_agents An array of spatial objects where we are
    !!            looking for the nearest available perception objects (array).
    class(CONDITION), dimension(:), intent(in)  :: consp_agents

    !> @param[in] time_step_model The current time step of the model.
    integer, optional, intent(in) :: time_step_model

    !> ### Notable variables and parameters ###
    !> - **consp_sizes** - local array for the body lengths of the agents.
    !    @note We now get this array from the `consp_agents` objects.
    real(SRP), dimension(size(consp_agents)) :: consp_sizes

    !> - **consp_masses** - local array for the body masses of the agents.
    !    @note We now get this array from the `consp_agents` objects.
    real(SRP), dimension(size(consp_agents)) :: consp_masses

    !> - **consp_alive** - local array of logical indicators if the agents
    !!   are alive (TRUE).
    !    @note We now get this array from the `consp_agents` objects.
    logical, dimension(size(consp_agents)) :: consp_alive

    !> - **consp_sex_is_male** - local array of the sex of the conspecifics.
    !    @note We now get this array from the `consp_agents` objects.
    logical, dimension(size(consp_agents)) :: consp_sex_is_male

    ! Local copy of the time step of the model.
    integer :: time_step_model_here

    !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME =                                 &
                                      "(consp_perception_get_visrange_objects)"

    !> - **MIN_DIST_SELF** - exclude self from the neighbours. Because we
    !!   cannot (easily) use
    !!   recursive reference to the indvidal agent class from this lower-order
    !!   perception class, we have to pass some parameters of the *this* agent
    !!   as dummy parameters to the subroutine. E.g. individual ID, if ID is
    !!   incorrect or not passed, the only way to distinguish self from other
    !!   agents in the neighbourhood is by different location, i.e. the
    !!   distance should be non-zero. This parameter sets the tolerance limit
    !!   for the difference in the distance for considering the neighbour
    !!   not-self. Possibly can be equal to the parameter `commondata::zero`,
    !!   or we can allow a higher discrepancy (this also might correct some
    !!   errors).
    real(SRP), parameter :: MIN_DIST_SELF = 2.0_SRP * ZERO ! 0.001_SRP

    !> - **agents_near** - temporary array of nearby other conspecifics
    !!   available to this agent.
    type(SPATIAL), dimension(size(consp_agents)) :: agents_near

    !> - **dist_neighbours** - temporary array of the distances to the
    !!   neighbouring food items.
    real(SRP), dimension(size(consp_agents)) ::  dist_neighbours

    !> - **dist_index** - temporary partial index vector for the distances
    !!   to the neighbouring conspecifics.
    integer, dimension(size(consp_agents))  :: dist_index

    ! Temporary possible error status for sub-procedures
    logical :: err_flag

    !> - **irradiance_agent_depth** - local variable defining the irradiance
    !!   (illumination) at the current depth of the agent. Needed to calculate
    !!   the agent's visual range.
    real(SRP) :: irradiance_agent_depth

    !> - **sobject_area** - local variable defining the conspecific area.
    !!   Needed to calculate the agent's visual range.
    real(SRP), dimension(size(consp_agents)) :: sobject_area

    !> - **sobject_visual_range** - local variable defining the visual range
    !!   of the agent for detecting each of the conspecifics (with known areas)
    !!   at the agent's current depth.
    real(SRP), dimension(size(consp_agents)) ::  sobject_visual_range

    !> - **sobjects_percept_in_visrange** - local sorted array of conspecific
    !!   perception components that are within the visual range of the agent
    !!   for output. The array should normally have the size of
    !!   `commondata::consp_select_items_index_partial` elements, but only the
    !!   first `sobjects_n_visrange` elements of it are actually within the
    !!   visual range.
    type(CONSPEC_PERCEPT_COMP),dimension(CONSP_SELECT_ITEMS_INDEX_PARTIAL) :: &
                                              sobjects_percept_in_visrange

    !> - **sobjects_dist_sorted** - temporary local sorted array of distances
    !!   between the agent and each of the nearest neighbouring conspecifics,
    !!   sorted for output.
    real(SRP), dimension(CONSP_SELECT_ITEMS_INDEX_PARTIAL) ::                  &
                                              sobjects_dist_sorted

    !> - **consp_array_size** - the size of the input arrays of object
    !!   properties, local. Initially set from the size of the objects (class)
    !!   array, but `consp_sizes` and `consp_alive` must have identical sizes.
    integer :: consp_array_size

    !> - **sobjects_n_visrange** - local number of elements of
    !!   `sobjects_percept_in_visrange` for output that are within he visual
    !!   range of the agent.
    integer ::  sobjects_n_visrange

    ! Local counter
    integer :: i

    !> - **self_idx** - local index of itself, needed to exclude self from
    !!   debug messages and logs.
    !! .
    integer :: self_idx

    !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

    !> ### Implementation details ###
    !> #### Checks and preparations ####
    !> Initialise index and rank values. Uninitialised index arrays may result
    !! in invalid memory reference in `ARRAY_INDEX` (it is not safe by design).
    dist_neighbours = MISSING ; dist_index = UNKNOWN
    sobject_area = MISSING ; sobject_visual_range = MISSING
    sobjects_dist_sorted = MISSING

    self_idx = 0  !> Also zero-init self index.

    !> Check optional time step parameter. If unset, use global
    !! `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> Set the size for all the internal arrays, that is equal to the
    !! `consp_agents` objects array size.
    consp_array_size = size(consp_agents)

    !> Copy conspecifics array from the input `consp_agents` class into
    !! `agents_near`.
    call agents_near%position ( consp_agents%location() )

    !> Get local arrays for the conspecific sizes, alive status and sex using
    !! elemental array-based accessor functions.
    consp_sizes = consp_agents%get_length()
    consp_masses = consp_agents%get_mass()
    consp_alive = consp_agents%is_alive()
    consp_sex_is_male = consp_agents%is_male()

    !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    !> #### Step 1 ####
    !> First, we get, up to the maximum order (fast *partial indexing*)
    !! of `commondata::consp_select_items_index_partial`, neighbouring
    !! conspecifics that are in proximity of this agent. here we get
    !! **partial index** vector for the input array of objects: `dist_index`.
    !! The calculation backend is the_environment::spatial::neighbours().
    call this%neighbours( neighbours = agents_near,                           &
                          dist = dist_neighbours,                             &
                          index_vector = dist_index,                          &
                          rank_max = CONSP_SELECT_ITEMS_INDEX_PARTIAL,        &
                          error_flag = err_flag )

    if (err_flag) call LOG_MSG ( LTAG_WARN // PROCNAME // ": Got error flag"  &
                    // " from conspecific objects neighbours procedure.")

    !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    !> #### Step 2 ####
    !> Second, we select only those items within this set, which are within
    !! the visual range of the agent under the current conditions.
    !! To do this we, first, calculate the ambient illumination/irradiance
    !! level at the depth of the agent. Done using the
    !! the_environment::spatial::illumination() procedure.
    irradiance_agent_depth = this%illumination(time_step_model_here)

    !> Compute the array of "prey areas" for each conspecific.
    sobject_area(dist_index(1:CONSP_SELECT_ITEMS_INDEX_PARTIAL)) =            &
      length2sidearea_fish(                                                   &
          cm2m( consp_sizes(dist_index(1:CONSP_SELECT_ITEMS_INDEX_PARTIAL)) ) &
          )

    !> Compute the vector of the visual ranges for detecting each of
    !! the conspecifics by the agent.
    sobject_visual_range(dist_index(1:CONSP_SELECT_ITEMS_INDEX_PARTIAL))      &
          = m2cm (  visual_range (                                            &
                        irradiance = irradiance_agent_depth,                  &
                        prey_area = sobject_area(                             &
                            dist_index(1:CONSP_SELECT_ITEMS_INDEX_PARTIAL)    &
                            ),                                                &
                        prey_contrast = INDIVIDUAL_VISUAL_CONTRAST_DEFAULT    &
                    )                                                         &
                  )

    !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    !> #### Step 3 ####
    !> Now we can get the pre-output array `sobjects_percept_in_visrange`
    !! that contains the conspecifics available within the visual range of
    !! the agent. Also, we count their number for the output parameter
    !! `sobjects_n_visrange`.
    sobjects_n_visrange = 0
    do i=1, CONSP_SELECT_ITEMS_INDEX_PARTIAL
      if ( dist_neighbours(dist_index(i)) <                                   &
                              sobject_visual_range(dist_index(i)) ) then
        !> Include only agents non identical to oneself.
        !! TODO: Use individual ID, but have to pass as an additional
        !!       dummy parameter.
        if ( this%distance( agents_near(dist_index(i)) ) > MIN_DIST_SELF ) then
          !> Include only alive agents.
          if ( consp_alive(dist_index(i)) ) then
            sobjects_n_visrange = sobjects_n_visrange + 1
            call sobjects_percept_in_visrange(sobjects_n_visrange)%make(      &
                            location = agents_near(dist_index(i))%location(), &
                            size = consp_sizes(dist_index(i)),                &
                            mass = consp_masses(dist_index(i)),               &
                            dist = dist_neighbours(dist_index(i)),            &
                            cid = dist_index(i),                              &
                            is_male = consp_sex_is_male(dist_index(i)) )
          else
            call LOG_DBG( LTAG_INFO // PROCNAME //                            &
                          ": Dead neighbour excluded. Id:" //                 &
                          TOSTR(dist_index(i)) )
          end if
        else
          self_idx = i    !> Index of itself, will exclude from min. values.
          call LOG_DBG (LTAG_INFO // PROCNAME // ": Found self within " //    &
                        "the visual range. Idx:" // TOSTR(dist_index(self_idx)))
        end if
      end if
    end do
    !> Here we also log warning if no conspecifics found, when debugging. If
    !! the self index self_idx is non-zero, will output next from self value,
    !! usually 2.
    if (sobjects_n_visrange==0) call LOG_DBG( LTAG_WARN // PROCNAME //        &
          ": No conspecifics found within the visual range of the agent; " // &
          "The nearest conspecific distance=" //                              &
          TOSTR(dist_neighbours(dist_index(self_idx+1))) //                   &
          ", with visual range=" //                                           &
          TOSTR(sobject_visual_range(dist_index(self_idx+1))) //              &
          ", ID:" // TOSTR(dist_index(self_idx+1)) //                         &
          ", Idx:" // TOSTR(self_idx+1) )

    !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    !> #### Step 4 ####
    !> Finally, we can now create the output conspecific perception object,
    !! including only conspecifics that are within the current visual range
    !! of the agent.
    !! Init (create and allocate) the conspecific perception object,
    !! at first empty, using the food perception object (at first empty)
    !! using the the_neurobio::percept_conspecifics::init().
    call this%perceive_consp%init(sobjects_n_visrange)

    !> Fill the output perception object with the values obtained at
    !! the step 3. This is done using the
    !! the_neurobio::percept_conspecifics::make() backend procedure.
    call this%perceive_consp%make (                                           &
                    sobjects_percept_in_visrange(1:sobjects_n_visrange) )

  end subroutine consp_perception_get_visrange_objects

  !-----------------------------------------------------------------------------
  !> Check if the agent sees any conspecifics within the visual range.
  !! @warning Should be called after the `see_consp` method as it is only an
  !!          accessor get-function.
  !! @note    There is little sense to implement this accessor procedure in the
  !!          specific perception (up-level) class as every perception is not
  !!          a derivative of a common class, perceptions independent, so we'll
  !!          have to also implement agent-bound perception methods anyway.
  elemental function consp_perception_is_seeing_conspecifics(this) result (sees_consp)
    class(PERCEPTION), intent(in) :: this
    logical :: sees_consp

    sees_consp = .FALSE.
    if (this%perceive_consp%get_count() > 0 ) sees_consp = .TRUE.

  end function consp_perception_is_seeing_conspecifics

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with SPATIAL PERCEPTION
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Create a single arbitrary spatial object perception component at an
  !! undefined position with default properties.
  elemental subroutine spatialobj_percept_comp_create(this)
    class(SPATIALOBJ_PERCEPT_COMP), intent(inout) :: this

    !> ### Implementation notes ###
    !> Just set an undefined location of the object using
    !! standard interface function `the_environment::spatial::missing()`.
    call this%missing()

    !> Set cid to commondata::unknown.
    !> @warning random id on create is now disabled to allow elemental function,
    !!          because random are never pure. So care to set cid's elsewhere.
    this%cid = UNKNOWN

    !> Then we set the object size to commondata::missing.
    this%sobj_size = MISSING

    !> Init distance towards the object, initially also commondata::missing.
    this%sobj_distance = MISSING

  end subroutine spatialobj_percept_comp_create

  !-----------------------------------------------------------------------------
  !> Make a single arbitrary **spatial** object perception component.
  subroutine spatialobj_percept_make(this, location, size, dist, cid)
    class(SPATIALOBJ_PERCEPT_COMP), intent(inout) :: this

    !> @param Location of the spatial object perception component, as a
    !!        `the_environment::spatial` type container.
    type(SPATIAL), intent(in) :: location

    !> @param size This is the optional object size.
    real(SRP), optional, intent(in) :: size

    !> @param dist The distance towards this object.
    real(SRP), optional, intent(in) :: dist

    !> @param iid Optional cid for the object, e.g. number within an array.
    integer, optional, intent(in) :: cid

    !> ### Implementation notes ###
    !> Set the location of the object using standard interface function
    !! `the_envirnoment::spatial::position()`.
    call this%position(location)

    !> If individual id is provided, set it. If not, set random.
    if (present(cid)) then
      call this%set_cid(cid)
    else
      call this%set_cid()
    end if

    !> Set the object perception component size. Only nonzero size is accepted.
    if (present(size)) then
      this%sobj_size = max(ZERO, size)
    end if

    !> Also set the distance towards the conspecific if provided. If not
    !! provided, ignore.
    if (present(dist)) then
      this%sobj_distance = dist
    end if

  end subroutine spatialobj_percept_make

  !-----------------------------------------------------------------------------
  !> Get an arbitrary spatial object perception component size.
  elemental function spatialobj_percept_get_size (this) result (obj_size)
    class(SPATIALOBJ_PERCEPT_COMP), intent(in) :: this

    real(SRP) :: obj_size

    obj_size = this%sobj_size

  end function spatialobj_percept_get_size

  !-----------------------------------------------------------------------------
  !> Get the distance to an arbitrary spatial object perception component.
  elemental function spatialobj_percept_get_dist (this) result (dist_object)
    class(SPATIALOBJ_PERCEPT_COMP), intent(in) :: this

    real(SRP) :: dist_object

    dist_object = this%sobj_distance

  end function spatialobj_percept_get_dist

  !-----------------------------------------------------------------------------
  !> Calculate the visibility range of this spatial object. Wrapper to the
  !! `visual_range` function. This function calculates the distance from
  !! which this object can be seen by a visual object (e.g. predator or
  !! prey).
  !! @warning The `visual_range` procedures use meter for units, this
  !!          auto-converts to cm.
  !! @warning Cannot implement a generic function accepting also vectors of
  !!          this objects as only elemental object-bound array functions are
  !!          allowed by the standard. This function cannot be elemental, so
  !!          passed-object dummy argument must always be scalar.
  function spatialobj_percept_visibility_visual_range(this, object_area,      &
                                  contrast, time_step_model) result (visrange)
    class(SPATIALOBJ_PERCEPT_COMP), intent(in) :: this
    !> @param[in] object_area is optional area of the spatial object. This
    !!            parameter can be necessary because the area can be
    !!            calculated differently for different types of objects, e.g.
    !!            fish using commondata::length2sidearea_fish() or food
    !!            items using commondata::carea(). The default object area
    !!            if the parameter is absent, uses the fish backend.
    real(SRP), optional, intent(in) :: object_area
    !> @param[in] contrast is an optional inherent visual contrast of the
    !!            spatial object. The default contrast of all objects is
    !!            defined by the commondata::preycontrast_default parameter.
    real(SRP), optional, intent(in) :: contrast
    !> @param[in] optional time step of the model, if absent gets the current
    !!            time step as defined by the value of
    !!            `commondata::global_time_step_model_current`.
    integer, optional, intent(in) :: time_step_model
    !> @return The maximum distance from which this object can be seen.
    real(SRP) :: visrange

    ! Local copies of optionals
    integer :: time_step_model_here
    real(SRP) :: object_area_here, contrast_here

    ! Local variables
    real(SRP) :: irradiance_agent_depth

    !> ### Implementation details ###
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

    !> Second, check if the object area (`object_area`) parameter is provided.
    !! If not, calculate area from the `sobj_size` component assuming it is
    !! a fish. This is logical because the_neurobio::spatialobj_percept_comp
    !! class is mainly used in the perception of conspecifics and predators.
    if (present(object_area)) then
      object_area_here = object_area
    else
      object_area_here = length2sidearea_fish( cm2m( this%sobj_size ) )
    end if

    !> Calculate ambient illumination / irradiance at the depth of
    !! this object at the given time step.
    irradiance_agent_depth =  this%illumination(time_step_model_here)

    !> Return visual range to see this spatial object: its visibility range.
    visrange =  m2cm( visual_range ( irradiance = irradiance_agent_depth,     &
                                     prey_area = object_area_here,            &
                                     prey_contrast = contrast_here )  )

  end function spatialobj_percept_visibility_visual_range

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with PREDATOR PERCEPTION
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Create **predator** perception object, it is an array of
  !! spatial perception components.
  elemental subroutine percept_predator_create_init (this, maximum_number_predators)
    class(PERCEPT_PREDATOR), intent(inout) :: this

    !> @param maximum_number_predators The maximum number of predators
    !!        in the perception object. Normally equal to the partial
    !!        predator selection indexing order
    !!        `PREDATOR_SELECT_ITEMS_INDEX_PARTIAL`.
    integer, intent(in) :: maximum_number_predators

    !> ### Implementation notes ###
    !> Allocate the array of the predator's spatial perception components.
    if (.not. allocated(this%predators_seen))                                 &
               allocate(this%predators_seen(maximum_number_predators))

    !> Allocate the array of the predator attack rates.
    if (.not. allocated(this%predators_attack_rates))                         &
               allocate(this%predators_attack_rates(maximum_number_predators))

    !> And create all perception components (create is `elemental`).
    call this%predators_seen%create()

    !> Fill the predator's attack rates arrays with commondata::missing
    !! values.
    this%predators_attack_rates = MISSING

    !> Set the initial number of predators within the visual range
    !! to the maximum number provided.
    call this%number(maximum_number_predators)

  end subroutine percept_predator_create_init

  !-----------------------------------------------------------------------------
  !> Set the total number of **predators** perceived (seen) in the predator
  !! perception object. But do not reallocate the predator perception
  !! components so far.
  elemental subroutine percept_predator_number_seen(this, number_set)
    class(PERCEPT_PREDATOR), intent(inout) :: this

    !> @param[in] number_set Set the number of predators in the perception
    !!            object.
    integer, intent(in) :: number_set

    this%predators_seen_count = number_set

  end subroutine percept_predator_number_seen

  !-----------------------------------------------------------------------------
  !> Make the **predator** perception object, fill it with the actual arrays.
  !! @note Note that the size and allocation is set by the
  !! `the_neurobio::percept_predator::init()` method.
  pure subroutine percept_predator_make_fill_arrays(this, preds, attack_rate)
    class(PERCEPT_PREDATOR), intent(inout) :: this
    !> @param[in] preds an array of predator (spatial,
    !!            `the_neurobio::spatialobj_percept_comp`) perception
    !!            components that form the perception object.
    type(SPATIALOBJ_PERCEPT_COMP), intent(in), dimension(:) ::  preds

    real(SRP), optional, intent(in), dimension(:) :: attack_rate

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME =                                 &
                                          "(percept_predator_make_fill_arrays)"

    !> ### Implementation notes ###
    !> Fill the dynamic conspecific perception object with the data from
    !! the input array.
    this%predators_seen = preds

    !> The arrays for the body sizes and attack rates of the predators are
    !! set only if these arrays are present in the dummy arguments to this
    !! procedure. If they are not provided, default values are used as defined
    !! by commondata::predator_attack_rate_default parameter.
    if (present(attack_rate)) then
      this%predators_attack_rates = attack_rate
    else
      this%predators_attack_rates = PREDATOR_ATTACK_RATE_DEFAULT
    end if

  end subroutine percept_predator_make_fill_arrays

  !-----------------------------------------------------------------------------
  !> Set an array of the attack rates for the predator perception object.
  pure subroutine percept_predator_set_attack_rate_vector(this, attack_rate)
    class(PERCEPT_PREDATOR), intent(inout) :: this

    real(SRP), intent(in), dimension(:) :: attack_rate

    this%predators_attack_rates = attack_rate

  end subroutine percept_predator_set_attack_rate_vector

  !-----------------------------------------------------------------------------
  !> Set an array of the attack rates for the predator perception object.
  pure subroutine percept_predator_set_attack_rate_scalar(this, attack_rate)
    class(PERCEPT_PREDATOR), intent(inout) :: this

    real(SRP), intent(in) :: attack_rate

    this%predators_attack_rates = attack_rate

  end subroutine percept_predator_set_attack_rate_scalar

  !-----------------------------------------------------------------------------
  !> Get the number (count) of predators seen. Trivial.
  elemental function percept_predator_get_count_seen (this) result (count_obj)
    class(PERCEPT_PREDATOR), intent(in) :: this
    integer :: count_obj

    count_obj = this%predators_seen_count

  end function percept_predator_get_count_seen

  !-----------------------------------------------------------------------------
  !> Deallocate and delete a **predator** perception object.
  elemental subroutine percept_predator_destroy_deallocate(this)
    class(PERCEPT_PREDATOR), intent(inout) :: this

    if (allocated(this%predators_seen)) deallocate(this%predators_seen)
    if (allocated(this%predators_attack_rates)) deallocate(this%predators_attack_rates)
    this%predators_seen_count = UNKNOWN

  end subroutine percept_predator_destroy_deallocate

  !-----------------------------------------------------------------------------
  !> Get available predators perception objects within the visual range of
  !! the agent, which the agent can perceive and therefore respond to.
  !! @note Note that there are three similar procedures that detect spatial
  !!       objects within the visual range of the agent:
  !!       - the_neurobio::perception::see_food -- perception of food items;
  !!       - the_neurobio::perception::see_consp -- perception of conspecifics:
  !!       - the_neurobio::perception::see_pred -- perception of predators.
  !!       .
  !!       All these procedures were actually implemented using the first
  !!       (the_neurobio::perception::see_food) as a template. All three
  !!       implement partial indexing of the nearest spatial objects to
  !!       accelerate computation of large arrays of spatial objects.
  !! @note This procedure also used the conspecific perception as a template,
  !!       but removed extra tests for "self".
  subroutine predator_perception_get_visrange_objects (this,                  &
                                                spatl_agents,                 &
                                                time_step_model )
    class(PERCEPTION), intent(inout) :: this

    !> @param[in] spatl_agents An array of spatial objects where we are
    !!            looking for the nearest available perception objects (array).
    class(PREDATOR), dimension(:), intent(in)  :: spatl_agents

    !> @param[in] time_step_model The current time step of the model.
    integer, optional, intent(in) :: time_step_model

    ! Local copy of the time step of the model.
    integer :: time_step_model_here

    !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME =                                 &
                                    "(predator_perception_get_visrange_objects)"

    !> ### Notable variables and parameters ###
    !> - **MIN_DIST_SELF** - exclude self from the neighbours. Because we
    !!   cannot (easily) use recursive reference to the individual agent class
    !!   from this lower-order perception class, we have to pass some
    !!   parameters of the *this* agent as dummy parameters to the subroutine.
    !!   E.g. individual ID, if ID is incorrect or not passed, the only way to
    !!   distinguish self from other agents in the neighbourhood is by
    !!   different location, i.e. the distance should be non-zero. This
    !!   parameter sets the tolerance limit for the difference in the distance
    !!   for considering the neighbour not-self. Possibly can be equal to the
    !!   parameter `commondata::zero`, or one can allow a higher discrepancy
    !!   (this also might correct some errors).
    real(SRP), parameter :: MIN_DIST_SELF = 0.001_SRP

    !> - **agents_near** - temporary array of predators in proximity to this
    !!   agent.
    type(SPATIAL), dimension(size(spatl_agents)) :: agents_near

    !> - **dist_neighbours** - temporary array of the distances to the
    !!   neighbouring predators.
    real(SRP), dimension(size(spatl_agents)) ::  dist_neighbours

    !> - **dist_index** - temporary partial index vector for the distances
    !!   to the neighbouring predators.
    integer, dimension(size(spatl_agents))  :: dist_index

    ! Temporary possible error status for sub-procedures
    logical :: err_flag

    !> - **irradiance_agent_depth** - local variable defining the irradiance
    !!   (illumination) at the current depth of the agent.  Needed to
    !!   calculate the agent's visual range.
    real(SRP) :: irradiance_agent_depth

    !> - **spatl_sizes** -local copy of the body lengths of the predators.
    real(SRP), dimension(size(spatl_agents)) :: spatl_sizes

    ! Local copy of the attack rates of the predators
    real(SRP), dimension(size(spatl_agents)) :: pred_attack_rates

    !> - **sobject_area** - local variable defining the conspecific area.
    !!   Needed to calculate the agent's visual range.
    !    It is the `prey_area` parameter in the equation.
    real(SRP), dimension(size(spatl_agents)) :: sobject_area

    !> - **sobject_visual_range** - local variable defining the visual range
    !!   of the agent for detecting each of the predators (with known areas)
    !!   at the agent's current depth.
    real(SRP), dimension(size(spatl_agents)) ::  sobject_visual_range

    !> - **sobjects_percept_in_visrange** - local sorted array of the
    !!   perception components that are within the visual range of the agent
    !!   for output. The array should normally have the size of
    !!   `commondata::pred_select_items_index_partial` elements, but only
    !!   the first `sobjects_n_visrange` elements of it are actually within
    !!   the visual range.
    type(SPATIALOBJ_PERCEPT_COMP), dimension(                                 &
            PRED_SELECT_ITEMS_INDEX_PARTIAL) :: sobjects_percept_in_visrange

    !> - **pred_attack_rates_in_visrange** - local variable containing the
    !!   attack rates of the predators that are within the visual range of
    !!   the agent.  The array should normally have the size of
    !!   `commondata::pred_select_items_index_partial` elements, but only
    !!   the first `sobjects_n_visrange` elements of it are actually within
    !!   the visual range.
    real(SRP) , dimension(                                                    &
            PRED_SELECT_ITEMS_INDEX_PARTIAL) :: pred_attack_rates_in_visrange

    !> - **index_max_size** - the size of the input arrays of object
    !!   properties, local. Initially set from the size of the objects (class)
    !!   array, but `spatl_sizes` must have identical size.
    integer :: index_max_size

    !> - **sobjects_n_visrange** - local number of elements of
    !!   `food_items_percept_in_visrange` for output that are within he visual
    !!   range of the agent.
    !! .
    integer ::  sobjects_n_visrange

    ! Local counter
    integer :: i

    !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

    !> ### Implementation details ###
    !> #### Checks and preparations ####
    !> Initialise index and rank values. Uninitialised index arrays may result
    !! in invalid memory reference in `ARRAY_INDEX` (it is not safe by design).
    dist_neighbours = MISSING ; dist_index = UNKNOWN
    sobject_area = MISSING ; sobject_visual_range = MISSING

    !> Check optional time step parameter. If unset, use global
    !! `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> This is the maximum size of the index. If the number of spatial objects
    !! is huge, we use partial indexing of the neighbours array. Then it is
    !! equal to the partial indexing
    !! `commondata::pred_select_items_index_partial`. However, if the number
    !! of potential neighbouring objects is smaller than the partial index
    !! size, we use full indexing. In the later case `index_max_size` is
    !! equal to the actual size of the neighbouring objects array.
    !! @note Distinguishing between the partial indexing parameter and the
    !!       max size of the index makes sense only in cases where small
    !!       number of neighbours can be expected. We normally have large
    !!       populations of agents and huge number of food items, well
    !!       exceeding the partial indexing parameter
    !!       `commondata::pred_select_items_index_partial`. However, the
    !!       number of predators can be smaller than this.
    index_max_size = min( size(spatl_agents), PRED_SELECT_ITEMS_INDEX_PARTIAL )

    !> Copy predators array from the input `spatl_agents` class into
    !! `agents_near`.
    call agents_near%position ( spatl_agents%location() )

    !> Get an array of the body sizes of the predators using array-based
    !! elemental function (This now works ok in Intel Fortran 17).
    spatl_sizes = spatl_agents%get_size()

    !> Get an array of the attack rates of the predators using array-based
    !! elemental function.
    pred_attack_rates = spatl_agents%get_attack_rate()

    !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    !> #### Step 1 ####
    !> First, we get, up to the maximum order (fast *partial indexing*)
    !! of `commondata::pred_select_items_index_partial`, neighbouring
    !! predators that are in proximity of this agent. here we get partial
    !! index vector for  the input array of objects: `dist_index`.
    !! **Partial indexing** is the most typical case as we have normally
    !! quite large number of agents within the population and food
    !! items within the habitat. However, this might be important for
    !! **predators**. Predators can be quite infrequent within the
    !! habitat, their number can be smaller than the maximum indexing
    !! parameter `commondata::pred_select_items_index_partial`. Hence
    !! the check is much more important here than in similar
    !! procedures for food items and conspecifics. The neighbours are
    !! computed using the the_environment::spatial::neighbours() procedure.
    if ( size(spatl_agents) < PRED_SELECT_ITEMS_INDEX_PARTIAL ) then
      !> Here we check if the number of neighbouring agents is smaller than
      !! the partial indexing parameter
      !! commondata::pred_select_items_index_partial and if yes, do **full
      !! indexing**.
      call this%neighbours( neighbours = agents_near,                         &
                          dist = dist_neighbours,                             &
                          index_vector = dist_index,                          &
                          error_flag = err_flag )
    else
      !> However, if the number of potential neighbouring objects is big, do
      !! **partial indexing**.
      call this%neighbours( neighbours = agents_near,                         &
                            dist = dist_neighbours,                           &
                            index_vector = dist_index,                        &
                            rank_max = PRED_SELECT_ITEMS_INDEX_PARTIAL,       &
                            error_flag = err_flag )
    end if
    if (err_flag) call LOG_MSG ( LTAG_WARN // PROCNAME // ": Got error flag"  &
                    // " from conspecific objects neighbours procedure.")

    !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    !> #### Step 2 ####
    !> Second, we select only those items within this set, which are within
    !! the visual range of the agent under the current conditions.
    !! To do this we, first, calculate the ambient illumination/irradiance
    !! level at the depth of the agent. Done using the
    !! the_environment::spatial::illumination() procedure.
    irradiance_agent_depth = this%illumination(time_step_model_here)

    !> Compute the array of "prey areas" for each conspecific.
    !! So far `prey_contrast` is not set, use default value
    !! `commondata::individual_visual_contrast_default`.
    sobject_area(dist_index(1:index_max_size)) =                              &
      length2sidearea_fish(                                                   &
          cm2m( spatl_sizes(dist_index(1:index_max_size)) )                   &
          )

    !> Compute the vector of the visual ranges for detecting each of
    !! the predators by the agent.
    sobject_visual_range(dist_index(1:index_max_size))                        &
          = m2cm (  visual_range (                                            &
                        irradiance = irradiance_agent_depth,                  &
                        prey_area = sobject_area(                             &
                            dist_index(1:index_max_size)                      &
                            ),                                                &
                        prey_contrast = INDIVIDUAL_VISUAL_CONTRAST_DEFAULT    &
                    )                                                         &
                  )

    !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    !> #### Step 3 ####
    !> Now we can get the pre-output array `sobjects_percept_in_visrange`
    !! that contains the predators available within the visual range of
    !! the agent. Also, we count their number for the output parameter
    !! `sobjects_n_visrange`.
    sobjects_n_visrange = 0
    do i=1, index_max_size
      if ( dist_neighbours(dist_index(i)) <                                   &
                              sobject_visual_range(dist_index(i)) ) then
        sobjects_n_visrange = sobjects_n_visrange + 1
        call sobjects_percept_in_visrange(sobjects_n_visrange)%make(          &
                        location = agents_near(dist_index(i))%location(),     &
                        size = spatl_sizes(dist_index(i)),                    &
                        dist = dist_neighbours(dist_index(i)),                &
                        cid = dist_index(i) )
        !> The inherent attack rates of each of the predators within the
        !! visual range is also collected here into the
        !! `pred_attack_rates_in_visrange` array.
        pred_attack_rates_in_visrange(sobjects_n_visrange) =                  &
                                               pred_attack_rates(dist_index(i))
      end if
    end do
    !> Here we also log warning if no objects found, when debugging.
    !! (see commondata::is_debug).
    if (sobjects_n_visrange==0) call LOG_DBG( LTAG_WARN // PROCNAME //        &
          ": No objects found within the visual range of the agent; " //      &
          "The nearest object distance=" //                                   &
          TOSTR(dist_neighbours(dist_index(1))) //                            &
          ", with visual range=" //                                           &
          TOSTR(sobject_visual_range(dist_index(1))) //                       &
          ", ID:" // TOSTR(dist_index(1)) )

    !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    !> #### Step 4 ####
    !> Finally, we can now create the output conspecific perception object,
    !! including only predators that are within the current visual range
    !! of the agent.
    !! Init (create and allocate) the predator perception object,
    !! at first empty, with the_neurobio::percept_predator::init().
    call this%perceive_predator%init(sobjects_n_visrange)

    !> Fill the output perception object with the values obtained at
    !! the step 3 using the the_neurobio::percept_predator::make() method.
    if (AGENT_CAN_ASSESS_PREDATOR_ATTACK_RATE) then
      !> Note that if the commondata::agent_can_assess_predator_attack_rate
      !! parameter is set to TRUE, the agents can access and assess the
      !! inherent attack rate of the predator. This can be for example
      !! possible if the agent can assess the hunger level of the predator.
      !! The attack rate is then set from the array of the inherent attack
      !! rates of the predators `pred_attack_rates_in_visrange`. May need to
      !! add a predator perception error to this value, but it is not
      !! implemented  yet.
      call this%perceive_predator%make (                                      &
                      sobjects_percept_in_visrange(1:sobjects_n_visrange),    &
                      pred_attack_rates_in_visrange(1:sobjects_n_visrange) )
    else
      !> Note that if the commondata::agent_can_assess_predator_attack_rate
      !! parameter is set to FALSE, the agents cannot access the
      !! inherent attack rate of the predator. The attack rate is then fixed
      !! from the default parameter commondata::predator_attack_rate_default
      !! value.
      call this%perceive_predator%make (                                      &
                      sobjects_percept_in_visrange(1:sobjects_n_visrange) )
    end if

  end subroutine predator_perception_get_visrange_objects

  !-----------------------------------------------------------------------------
  !> Check if the agent sees any predators within the visual range.
  !! @warning Should be called after the `the_neurobio::perception::see_food()`
  !!          method as it is just an accessor function.
  !  @note    There is little sense to implement this accessor procedure in the
  !           specific perception (up-level) class as every perception is not
  !           a derivative of a common class, perceptions independent, so we'll
  !           have to also implement agent-bound perception methods anyway.
  elemental function predator_perception_is_seeing_predators(this)            &
                                                            result (sees_pred)
    class(PERCEPTION), intent(in) :: this
    logical :: sees_pred

    sees_pred = .FALSE.
    if (this%perceive_predator%get_count() > 0 ) sees_pred = .TRUE.

  end function predator_perception_is_seeing_predators

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with LIGHT PERCEPTION
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Make en empty light perception component. Really necessary only when
  !! perception objects are all allocatable.
  elemental subroutine percept_light_create_init(this)
    class(PERCEPT_LIGHT), intent(inout) :: this

    this%illumination = MISSING

  end subroutine percept_light_create_init

  !-----------------------------------------------------------------------------
  !> Get the current perception of the illumination.
  elemental function percept_light_get_current(this) result (illumination_here)
    class(PERCEPT_LIGHT), intent(in) :: this
    real(SRP) :: illumination_here

    illumination_here = this%illumination

  end function percept_light_get_current

  !-----------------------------------------------------------------------------
  !> Set the current **light** level into the perception component.
  subroutine percept_light_set_current(this, timestep, depth)
    class(PERCEPT_LIGHT), intent(inout) :: this
    integer, intent(in) :: timestep
    real(SRP), intent(in) :: depth

    !> Here we, calculate the ambient illumination/irradiance level at the
    !! current depth of the agent.
    !> @note `is_stochastic` logical parameter is TRUE in `light_surface`, that
    !!       sets a stochastic illumination level at the surface and therefore
    !!       also at the agent's current depth.
    this%illumination = light_depth ( depth=depth,                            &
                          surface_light =                                     &
                              light_surface(tstep=timestep,                   &
                                            is_stochastic=DAYLIGHT_STOCHASTIC)&
                          )

  end subroutine percept_light_set_current

  !-----------------------------------------------------------------------------
  !> Destroy / deallocate **light** perception component. Really necessary only when
  !! perception objects are all allocatable.
  elemental subroutine percept_light_destroy_deallocate(this)
    class(PERCEPT_LIGHT), intent(inout) :: this

    this%illumination = MISSING

  end subroutine percept_light_destroy_deallocate

  !-----------------------------------------------------------------------------
  !> Get **light** perception objects into the individual PERCEPTION
  !! object layer.
  subroutine light_perception_get_object(this, time_step_model)
    class(PERCEPTION), intent(inout) :: this

     !> @param[in] time_step_model The current time step of the model.
    integer, optional, intent(in) :: time_step_model

    !> Local copy of the time step of the model.
    integer :: time_step_model_here

    !> Check optional time step parameter. If unset, use global
    !! `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> Allocate and init the perception object (needed only when it is
    !! allocatable)
    call this%perceive_light%init()

    call this%perceive_light%set_current( time_step_model_here, this%dpos() )

  end subroutine light_perception_get_object

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with DEPTH PERCEPTION
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Make en empty depth perception component. Really necessary only when
  !! perception objects are all allocatable.
  elemental subroutine percept_depth_create_init(this)
    class(PERCEPT_DEPTH), intent(inout) :: this

    this%depth = MISSING

  end subroutine percept_depth_create_init

  !-----------------------------------------------------------------------------
  !> Get the current perception of the **depth**.
  elemental function percept_depth_get_current(this) result (depth_now)
    class(PERCEPT_DEPTH), intent(in) :: this
    real(SRP) :: depth_now

    depth_now = this%depth

  end function percept_depth_get_current

  !-----------------------------------------------------------------------------
  !> Set the current **depth** level into the perception component.
  subroutine percept_depth_set_current(this, cdepth)
    class(PERCEPT_DEPTH), intent(inout) :: this
    real(SRP), intent(in) :: cdepth

    this%depth = cdepth

  end subroutine percept_depth_set_current

  !-----------------------------------------------------------------------------
  !> Destroy / deallocate **depth** perception component. Really necessary
  !! only when perception objects are all allocatable.
  elemental subroutine percept_depth_destroy_deallocate(this)
    class(PERCEPT_DEPTH), intent(inout) :: this

    this%depth = MISSING

  end subroutine percept_depth_destroy_deallocate

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with REPRODUCTIVE FACTOR PERCEPTION
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Make en empty reproductive factor perception component. Really necessary
  !! only when perception objects are all allocatable.
  elemental subroutine percept_reprfac_create_init(this)
    class(PERCEPT_REPRFACT), intent(inout) :: this

    this%reproduct_fact = MISSING

  end subroutine percept_reprfac_create_init

  !-----------------------------------------------------------------------------
  !> Get the current perception of the **reproductive factor**.
  elemental function percept_reprfac_get_current(this) result (rf_now)
    class(PERCEPT_REPRFACT), intent(in) :: this
    real(SRP) :: rf_now

    rf_now = this%reproduct_fact

  end function percept_reprfac_get_current

  !-----------------------------------------------------------------------------
  !> Set the current **reproductive factor** level into perception component.
  subroutine percept_reprfac_set_current(this, reprfac)
    class(PERCEPT_REPRFACT), intent(inout) :: this
    real(SRP), intent(in) :: reprfac

    this%reproduct_fact = reprfac

  end subroutine percept_reprfac_set_current

  !-----------------------------------------------------------------------------
  !> Destroy / deallocate **reproductive factor** perception component. Really
  !! necessary only when perception objects are all allocatable.
  elemental subroutine percept_reprfac_destroy_deallocate(this)
    class(PERCEPT_REPRFACT), intent(inout) :: this

    this%reproduct_fact = MISSING

  end subroutine percept_reprfac_destroy_deallocate

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with GET OBJECTS PERCEPTION
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Get **depth** perception objects into the **individual** PERCEPTION
  !! object layer.
  subroutine depth_perception_get_object(this)
    class(PERCEPTION), intent(inout) :: this

    !> Allocate and init the perception object (needed only when it is
    !! allocatable)
    call this%perceive_depth%init()

    call this%perceive_depth%set_current( this%dpos() )

  end subroutine depth_perception_get_object

  !-----------------------------------------------------------------------------
  !> Get the **stomach capacity** perception objects into the **individual**
  !! PERCEPTION object layer.
  subroutine stomach_perception_get_object(this)
    class(PERCEPTION), intent(inout) :: this

    !> Allocate and init the perception object (needed only when it is
    !! allocatable)
    call this%perceive_stomach%init()

    !> Calculate the available stomach capacity, i.e. maximum stomach mass
    !! minus current stomach content, and set the value into the stomach
    !! perception object. Body mass and maxstomcap are from the `CONDITION`
    !! layer.
    call this%perceive_stomach%set_available( this%body_mass * this%maxstomcap &
                                          - this%get_stom_content() )

  end subroutine stomach_perception_get_object

  !-----------------------------------------------------------------------------
  !> Get the **body mass** perception objects into the **individual**
  !! PERCEPTION object layer.
  subroutine bodymass_perception_get_object(this)
    class(PERCEPTION), intent(inout) :: this

    !> Allocate and init the perception object (needed only when it is
    !! allocatable)
    call this%perceive_body_mass%init()

    !> Get the body mass from the individual, put it to the perception object.
    call this%perceive_body_mass%set_current( this%mass() )

  end subroutine bodymass_perception_get_object

  !-----------------------------------------------------------------------------
  !> Get the **energy reserves** perception objects into the **individual**
  !! PERCEPTION object layer.
  subroutine energy_perception_get_object(this)
    class(PERCEPTION), intent(inout) :: this

    !> Allocate and init the perception object (needed only when it is
    !! allocatable)
    call this%perceive_energy%init()

    !> Get the energy reserves from the individual, put it to the perception
    !! object.
    call this%perceive_energy%set_current( this%get_energy() )

  end subroutine energy_perception_get_object

  !-----------------------------------------------------------------------------
  !> Get the **age** perception objects into the **individual**
  !! PERCEPTION object layer.
  subroutine age_perception_get_object(this)
    class(PERCEPTION), intent(inout) :: this

    !> Allocate and init the perception object (needed only when it is
    !! allocatable).
    call this%perceive_age%init()

    !> Get the age from the individual, put it to the perception
    !! object.
    call this%perceive_age%set_current( this%get_age() )

  end subroutine age_perception_get_object

  !-----------------------------------------------------------------------------
  !> Get the **reproductive factor** perception objects into the **individual**
  !! PERCEPTION object layer.
  subroutine repfac_perception_get_object(this)
    class(PERCEPTION), intent(inout) :: this

    !> Allocate and init the perception object (needed only when it is
    !! allocatable).
    call this%perceive_reprfac%init()

    !> Get the the_hormones::hormones::reproductive_factor() from the
    !! individual, put it to the perception object.
    call this%perceive_reprfac%set_current( this%reproductive_factor() )

  end subroutine repfac_perception_get_object


  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with PERCEPTION MEMORY STACK
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Add perception components into the memory stack.
  elemental subroutine percept_memory_add_to_stack(this, light, depth, food,  &
                foodsize, fooddist, consp, pred, stom, bdmass, energ, reprfac )

    class(MEMORY_PERCEPTUAL), intent(inout) :: this !> This memory object.

    !> The parameters of the subroutine are the actual values that are added
    !! to the perceptual memory stack arrays.
    real(SRP), intent(in) :: light    !> illuimination
    real(SRP), intent(in) :: depth    !> depth
    integer  , intent(in) :: food     !> number of food items
    real(SRP), intent(in) :: foodsize !> average size of food items
    real(SRP), intent(in) :: fooddist !> average distance to food items
    integer  , intent(in) :: consp    !> number of conspecifics
    integer  , intent(in) :: pred     !> number of predators
    real(SRP), intent(in) :: stom     !> stomach capacity
    real(SRP), intent(in) :: bdmass   !> body mass
    real(SRP), intent(in) :: energ    !> energy
    real(SRP), intent(in) :: reprfac  !> reproductive factor

    !> Each of the memory stack components corresponds to the respective
    !! dummy parameter. So arrays are updated at each step.
    call add_to_history( this%memory_light,   light    )
    call add_to_history( this%memory_depth,   depth    )
    call add_to_history( this%memory_food ,   food     )
    call add_to_history( this%memory_foodsiz, foodsize )
    call add_to_history( this%memory_foodist, fooddist )
    call add_to_history( this%memory_consp,   consp    )
    call add_to_history( this%memory_pred ,   pred     )
    call add_to_history( this%memory_stom ,   stom     )
    call add_to_history( this%memory_bdmass,  bdmass   )
    call add_to_history( this%memory_energ ,  energ    )
    call add_to_history( this%memory_reprfac, reprfac  )

  end subroutine percept_memory_add_to_stack

  !-----------------------------------------------------------------------------
  !> Cleanup and destroy the perceptual memory stack.
  elemental subroutine percept_memory_cleanup_stack(this)

    class(MEMORY_PERCEPTUAL), intent(inout) :: this !> This memory object.

    !> cleanup procedure uses whole array assignment.
    this%memory_light   =  MISSING
    this%memory_depth   =  MISSING
    this%memory_food    =  UNKNOWN
    this%memory_foodsiz =  MISSING
    this%memory_foodist =  MISSING
    this%memory_consp   =  UNKNOWN
    this%memory_pred    =  UNKNOWN
    this%memory_stom    =  MISSING
    this%memory_bdmass  =  MISSING
    this%memory_energ   =  MISSING
    this%memory_reprfac =  MISSING

  end subroutine percept_memory_cleanup_stack

  !-----------------------------------------------------------------------------
  !> Get the total number of food items within the whole perceptual memory
  !! stack.
  elemental function percept_memory_food_get_total (this)                     &
                                                            result (total_count)
    class(MEMORY_PERCEPTUAL), intent(in) :: this
    !> @returns Total count of predators in the memory stack.
    integer :: total_count

    !> Calculate the overall sum excluding missing values (masked).
    total_count = sum(this%memory_food, this%memory_food /= UNKNOWN)

  end function percept_memory_food_get_total

  !-----------------------------------------------------------------------------
  !> Get the **average number** of food items per single time step within the
  !! whole perceptual memory stack.
  !! @note There are several similar procedures with very similar
  !!       implementation:
  !!       - the_neurobio::percept_memory_food_get_mean_n() - get mean
  !!         **number** of food items from the memory;
  !!       - the_neurobio::percept_memory_food_get_mean_size() - get
  !!         mean **size** of food items from the memory.
  !!       - the_neurobio::percept_memory_predators_get_mean() - get
  !!         average number of predators from the memory.
  !!       .
  elemental function percept_memory_food_get_mean_n (this, last)              &
                                                            result (mean_count)
    class(MEMORY_PERCEPTUAL), intent(in) :: this

    !> @param last Limit to only this number of latest components in history.
    integer, optional, intent(in) :: last

    !> @returns Mean count of food items in the memory stack.
    real(SRP) :: mean_count

    !> Local copy of optional last
    integer :: last_here

    ! History stack size. We determine it from the size of the actual array
    ! rather than `commondata::history_size_perception` for further safety.
    integer, parameter :: HIST_SIZE = size(this%memory_food)

    !> ### Implementation notes ###
    !> Check if we are given the parameter requesting the latest history size.
    !! if parameter `last` absent or bigger than the array size, get whole
    !! stack array.
    if (present(last)) then
      if ( last < HIST_SIZE ) then
        last_here = last
      else
        last_here = HIST_SIZE - 1
      end if
    else
      last_here = HIST_SIZE - 1
    end if

    !> Calculate the average excluding missing values (masked) using
    !! commondata::average().
    mean_count=average( this%memory_food( HIST_SIZE-last_here+1:HIST_SIZE ), &
                        undef_ret_null=.TRUE. )

  end function percept_memory_food_get_mean_n

  !-----------------------------------------------------------------------------
  !> Get the **average number** of food items per single time step within the
  !! perceptual memory stack, split to the first (older) and second (newer)
  !! parts. The whole memory stack ('sample') is split by the `split_val`
  !! parameter and two means are calculated: before the `split_val` and after
  !! it.
  !! @note There are several similar procedures with very similar
  !!       implementation:
  !!       - the_neurobio::percept_memory_food_mean_n_split() - get mean
  !!         **number** of food items from the memory;
  !!       - the_neurobio::percept_memory_food_mean_size_split() - get
  !!         mean **size** of food items from the memory;
  !!       - the_neurobio::percept_memory_predators_mean_split() - get
  !!         average number of predators.
  !!       .
  elemental subroutine percept_memory_food_mean_n_split(  this, window,       &
                                                      split_val, older, newer )
    class(MEMORY_PERCEPTUAL), intent(in) :: this
    !> @param[in] window is the whole memory window which is analysed, if
    !!            not present, the whole memory stack is used.
    integer, optional, intent(in) :: window
    !> @param[in] split_val is the split value for the separation of the
    !!            older and newer averages. If not present, just splits the
    !!            memory window evenly in two halves.
    integer, optional, intent(in) :: split_val
    !> @param[out] older is the output average number of the food items in the
    !!            first (older) part of the memory window.
    real(SRP), intent(out) :: older
    !> @param[out] newer is the output average number of the food items in the
    !!            second (newer) part of the memory window.
    real(SRP), intent(out) :: newer

    ! Local copies of optionals.
    integer :: window_loc, split_val_loc

    integer, parameter :: HIST_SIZE = size(this%memory_food)

    !> ### Implementation details ###
    !> First, check optional parameters: the memory window `window` and the
    !> split value `split_val`. If either is not provided, defaults are used.
    if (present(window)) then
      window_loc = window
      !> (Also, a check is made so that a window exceeding the history stack
      !! length is reduced accordingly to the whole memory size).
      if (window_loc >= HIST_SIZE) window_loc = HIST_SIZE
    else
      !> - whole size of the perceptual memory stack
      !!   commondata::history_size_perception for the memory window
      window_loc = HIST_SIZE
    end if

    if (present(split_val)) then
      split_val_loc = split_val
    else
      !> - half of the memory window for the `split_val`.
      !! .
      split_val_loc = floor( real( window_loc, SRP ) / 2.0 )
    end if

    !> A sanity check is also done, if the split value happen to exceed the
    !! `window` parameter, it is reduced to the default 1/2 of the `window`.
    if (split_val_loc >= window_loc)                                          &
                    split_val_loc = floor( real( window_loc, SRP ) / 2.0 )

    !> Second, the `older` and the `newer` output average values are calculated.
    !! Here is the illustration of the calculation:
    !! @verbatim
    !!   Such 'window' and 'split_val'
    !!   values...
    !!
    !!            |<----- window ----->|
    !!   +--------+--------------------+
    !!   +        |        :|:         +
    !!   +--------+--------------------+
    !!                      ^ split_val
    !!
    !!
    !!   ... result in these means:
    !!
    !!   +--------+---------------------+
    !!   +        | mean for | mean for +
    !!   +        | 'older'  | 'newer'  +
    !!   +--------+---------------------+
    !! @endverbatim
    older=average( this%memory_food(HIST_SIZE-window_loc+1 :                  &
                                       HIST_SIZE-window_loc+split_val_loc),   &
                   undef_ret_null=.TRUE. )

    newer=average( this%memory_food(HIST_SIZE-window_loc+split_val_loc+1 :    &
                                       HIST_SIZE),                            &
                   undef_ret_null=.TRUE. )

  end subroutine percept_memory_food_mean_n_split

  !-----------------------------------------------------------------------------
  !> Get the **average size** of food item per single time step within the
  !! whole perceptual memory stack.
  !! @note There are several similar procedures with very similar
  !!       implementation:
  !!       - the_neurobio::percept_memory_food_get_mean_n() - get mean
  !!         **number** of food items from the memory;
  !!       - the_neurobio::percept_memory_food_get_mean_size() - get
  !!         mean **size** of food items from the memory.
  !!       - the_neurobio::percept_memory_predators_get_mean() - get
  !!         average number of predators from the memory.
  !!       .
  elemental function percept_memory_food_get_mean_size (this, last)           &
                                                            result (mean_val)
    class(MEMORY_PERCEPTUAL), intent(in) :: this

    !> @param last Limit to only this number of latest components in history.
    integer, optional, intent(in) :: last

    !> @returns Mean size of food items in the memory stack.
    real(SRP) :: mean_val

    !> Local copy of optional last
    integer :: last_here

    ! History stack size. We determine it from the size of the actual array
    ! rather than `commondata::history_size_perception` for further safety.
    integer, parameter :: HIST_SIZE = size(this%memory_foodsiz)

    !> ### Implementation notes ###
    !> Check if we are given the parameter requesting the latest history size.
    !! if parameter `last` absent or bigger than the array size, get whole
    !! stack array.
    if (present(last)) then
      if ( last < HIST_SIZE ) then
        last_here = last
      else
        last_here = HIST_SIZE
      end if
    else
      last_here = HIST_SIZE
    end if

    !> Calculate the average excluding missing values (masked) using
    !! commondata::average().
    mean_val = average(this%memory_foodsiz(HIST_SIZE-last_here+1:HIST_SIZE),  &
                       undef_ret_null=.TRUE.)

  end function percept_memory_food_get_mean_size

  !-----------------------------------------------------------------------------
  !> Get the **average size** of food items per single time step within the
  !! perceptual memory stack, split to the first (older) and second(newer)
  !! parts. The whole memory stack 'sample' is split by the `split_val`
  !! parameter and two means are calculated: before the `split_val` and after
  !! it.
  !! @note There are several similar procedures with very similar
  !!       implementation:
  !!       - the_neurobio::percept_memory_food_mean_n_split() - get mean
  !!         **number** of food items from the memory;
  !!       - the_neurobio::percept_memory_food_mean_size_split() - get
  !!         mean **size** of food items from the memory;
  !!       - the_neurobio::percept_memory_predators_mean_split() - get
  !!         average number of predators.
  !!       .
  elemental subroutine percept_memory_food_mean_size_split(  this, window,    &
                                                      split_val, older, newer )
    class(MEMORY_PERCEPTUAL), intent(in) :: this
    !> @param[in] window is the whole memory window which is analysed, if
    !!            not present, the whole memory stack is used.
    integer, optional, intent(in) :: window
    !> @param[in] split_val is the split value for the separation of the
    !!            older and newer averages. If not present, just splits the
    !!            memory window evenly in two halves.
    integer, optional, intent(in) :: split_val
    !> @param[out] older is the output average sizes of the food items in the
    !!            first (older) part of the memory window.
    real(SRP), intent(out) :: older
    !> @param[out] newer is the output average sizes of the food items in the
    !!            second (newer) part of the memory window.
    real(SRP), intent(out) :: newer

    ! Local copies of optionals.
    integer :: window_loc, split_val_loc

    integer, parameter :: HIST_SIZE = size(this%memory_foodsiz)

    !> ### Implementation details ###
    !> First, check optional parameters: the memory window `window` and the
    !> split value `split_val`. If either is not provided, defaults are used.
    if (present(window)) then
      window_loc = window
      !> (Also, a check is made so that a window exceeding the history stack
      !! length is reduced accordingly to the whole memory size).
      if (window_loc >= HIST_SIZE) window_loc = HIST_SIZE
    else
      !> - whole size of the perceptual memory stack
      !!   commondata::history_size_perception for the memory window
      window_loc = HIST_SIZE
    end if

    if (present(split_val)) then
      split_val_loc = split_val
    else
      !> - half of the memory window for the `split_val`.
      !! .
      split_val_loc = floor( real( window_loc, SRP ) / 2.0 )
    end if

    !> A sanity check is also done, if the split value happen to exceed the
    !! `window` parameter, it is reduced to the default 1/2 of the `window`.
    if (split_val_loc >= window_loc)                                          &
                    split_val_loc = floor( real( window_loc, SRP ) / 2.0 )

    !> Second, the `older` and the `newer` output average values are calculated.
    !! Here is the illustration of the calculation:
    !! @verbatim
    !!   Such 'window' and 'split_val'
    !!   values...
    !!
    !!            |<----- window ----->|
    !!   +--------+--------------------+
    !!   +        |        :|:         +
    !!   +--------+--------------------+
    !!                      ^ split_val
    !!
    !!
    !!   ... result in these means:
    !!
    !!   +--------+---------------------+
    !!   +        | mean for | mean for +
    !!   +        | 'older'  | 'newer'  +
    !!   +--------+---------------------+
    !! @endverbatim
    older=average( this%memory_foodsiz(HIST_SIZE-window_loc+1 :               &
                                       HIST_SIZE-window_loc+split_val_loc),   &
                   undef_ret_null=.TRUE. )

    newer=average( this%memory_foodsiz(HIST_SIZE-window_loc+split_val_loc+1 : &
                                       HIST_SIZE),                            &
                   undef_ret_null=.TRUE. )

  end subroutine percept_memory_food_mean_size_split

  !-----------------------------------------------------------------------------
  !> Get the **average distance** to food item per single time step within the
  !! whole perceptual memory stack.
  !! @note There are several similar procedures with very similar
  !!       implementation:
  !!       - the_neurobio::percept_memory_food_get_mean_n() - get mean
  !!         **number** of food items from the memory;
  !!       - the_neurobio::percept_memory_food_get_mean_size() - get
  !!         mean **size** of food items from the memory.
  !!       - the_neurobio::percept_memory_predators_get_mean() - get
  !!         average number of predators from the memory.
  !!       .
  elemental function percept_memory_food_get_mean_dist (this, last,           &
                                              undef_ret_null) result (mean_val)
    class(MEMORY_PERCEPTUAL), intent(in) :: this
    !> @param last Limit to only this number of latest components in history.
    integer, optional, intent(in) :: last
    !> @param undef_ret_null Optional flag if undefined value with sample size
    !!        should return zero mean value; if absent is set to TRUE and zero
    !!        mean is returned. Note that this behaviour is the opposite of the
    !!        standard commondata::average(). It is because this function is
    !!        mainly for perception memory, where zero value is appropriate in
    !!        absence of any food items.
    logical, optional, intent(in) :: undef_ret_null
    !> @returns Mean distance to food items in the memory stack.
    real(SRP) :: mean_val

    !> Local copies of optionals
    logical :: undef_ret_null_loc
    integer :: last_here

    ! History stack size. We determine it from the size of the actual array
    ! rather than `commondata::history_size_perception` for further safety.
    integer, parameter :: HIST_SIZE = size(this%memory_foodist)

    !> ### Implementation notes ###
    !> Check if we are given the parameter requesting the latest history size.
    !! if parameter `last` absent or bigger than the array size, get whole
    !! stack array.
    if (present(last)) then
      if ( last < HIST_SIZE ) then
        last_here = last
      else
        last_here = HIST_SIZE
      end if
    else
      last_here = HIST_SIZE
    end if

    if (present(undef_ret_null)) then
      undef_ret_null_loc = undef_ret_null
    else
      undef_ret_null_loc = .TRUE.
    end if

    !> Calculate the average excluding missing values (masked) using
    !! commondata::average().
    mean_val = average(this%memory_foodist(HIST_SIZE-last_here+1:HIST_SIZE),  &
                       undef_ret_null=undef_ret_null_loc)

  end function percept_memory_food_get_mean_dist

  !-----------------------------------------------------------------------------
  !> Get the **average distance** to food items per single time step within the
  !! perceptual memory stack, split to the first (older) and second(newer)
  !! parts. The whole memory stack 'sample' is split by the `split_val`
  !! parameter and two means are calculated: before the `split_val` and after
  !! it.
  !! @note There are several similar procedures with very similar
  !!       implementation:
  !!       - the_neurobio::percept_memory_food_mean_n_split() - get mean
  !!         **number** of food items from the memory;
  !!       - the_neurobio::percept_memory_food_mean_size_split() - get
  !!         mean **size** of food items from the memory;
  !!       - the_neurobio::percept_memory_predators_mean_split() - get
  !!         average number of predators.
  !!       .
  elemental subroutine percept_memory_food_mean_dist_split(this, window,      &
                                                      split_val, older, newer )
    class(MEMORY_PERCEPTUAL), intent(in) :: this
    !> @param[in] window is the whole memory window which is analysed, if
    !!            not present, the whole memory stack is used.
    integer, optional, intent(in) :: window
    !> @param[in] split_val is the split value for the separation of the
    !!            older and newer averages. If not present, just splits the
    !!            memory window evenly in two halves.
    integer, optional, intent(in) :: split_val
    !> @param[out] older is the output average distance to the food items in the
    !!            first (older) part of the memory window.
    real(SRP), intent(out) :: older
    !> @param[out] newer is the output average distance to the food items in the
    !!            second (newer) part of the memory window.
    real(SRP), intent(out) :: newer

    ! Local copies of optionals.
    integer :: window_loc, split_val_loc

    integer, parameter :: HIST_SIZE = size(this%memory_foodist)

    !> ### Implementation details ###
    !> First, check optional parameters: the memory window `window` and the
    !> split value `split_val`. If either is not provided, defaults are used.
    if (present(window)) then
      window_loc = window
      !> (Also, a check is made so that a window exceeding the history stack
      !! length is reduced accordingly to the whole memory size).
      if (window_loc >= HIST_SIZE) window_loc = HIST_SIZE
    else
      !> - whole size of the perceptual memory stack
      !!   commondata::history_size_perception for the memory window
      window_loc = HIST_SIZE
    end if

    if (present(split_val)) then
      split_val_loc = split_val
    else
      !> - half of the memory window for the `split_val`.
      !! .
      split_val_loc = floor( real( window_loc, SRP ) / 2.0 )
    end if

    !> A sanity check is also done, if the split value happen to exceed the
    !! `window` parameter, it is reduced to the default 1/2 of the `window`.
    if (split_val_loc >= window_loc)                                          &
                    split_val_loc = floor( real( window_loc, SRP ) / 2.0 )

    !> Second, the `older` and the `newer` output average values are calculated.
    !! Here is the illustration of the calculation:
    !! @verbatim
    !!   Such 'window' and 'split_val'
    !!   values...
    !!
    !!            |<----- window ----->|
    !!   +--------+--------------------+
    !!   +        |        :|:         +
    !!   +--------+--------------------+
    !!                      ^ split_val
    !!
    !!
    !!   ... result in these means:
    !!
    !!   +--------+---------------------+
    !!   +        | mean for | mean for +
    !!   +        | 'older'  | 'newer'  +
    !!   +--------+---------------------+
    !! @endverbatim
    older=average( this%memory_foodist(HIST_SIZE-window_loc+1 :               &
                                       HIST_SIZE-window_loc+split_val_loc),   &
                   undef_ret_null=.TRUE. )

    newer=average( this%memory_foodist(HIST_SIZE-window_loc+split_val_loc+1 : &
                                       HIST_SIZE),                            &
                   undef_ret_null=.TRUE. )

  end subroutine percept_memory_food_mean_dist_split

  !-----------------------------------------------------------------------------
  !> Get the **average number** of conspecifics per single time step within the
  !! whole perceptual memory stack.
  elemental function percept_memory_consp_get_mean_n (this, last)              &
                                                            result (mean_count)
    class(MEMORY_PERCEPTUAL), intent(in) :: this

    !> @param last Limit to only this number of latest components in history.
    integer, optional, intent(in) :: last

    !> @returns Mean count of conspecifics in the memory stack.
    real(SRP) :: mean_count

    !> Local copy of optional last
    integer :: last_here

    ! History stack size. We determine it from the size of the actual array
    ! rather than `commondata::history_size_perception` for further safety.
    integer, parameter :: HIST_SIZE = size(this%memory_consp)

    !> ### Implementation notes ###
    !> Check if we are given the parameter requesting the latest history size.
    !! if parameter `last` absent or bigger than the array size, get whole
    !! stack array.
    if (present(last)) then
      if ( last < HIST_SIZE ) then
        last_here = last
      else
        last_here = HIST_SIZE - 1
      end if
    else
      last_here = HIST_SIZE - 1
    end if

    !> Calculate the average excluding missing values (masked) using
    !! commondata::average().
    mean_count=average( this%memory_consp( HIST_SIZE-last_here+1:HIST_SIZE ), &
                        undef_ret_null=.TRUE. )

  end function percept_memory_consp_get_mean_n

  !-----------------------------------------------------------------------------
  !> Get the total number of predators within the whole perceptual memory stack.
  elemental function percept_memory_predators_get_total (this)                &
                                                            result (total_count)
    class(MEMORY_PERCEPTUAL), intent(in) :: this
    !> @returns Total count of predators in the memory stack.
    integer :: total_count

    !> Calculate the overall sum excluding missing values (masked).
    total_count = sum(this%memory_pred, this%memory_pred /= UNKNOWN)

  end function percept_memory_predators_get_total

  !-----------------------------------------------------------------------------
  !> Get the average number of predators per single time step within the
  !! whole perceptual memory stack.
  !! @note There are several similar procedures with very similar
  !!       implementation:
  !!       - the_neurobio::percept_memory_food_get_mean_n() - get mean
  !!         **number** of food items from the memory;
  !!       - the_neurobio::percept_memory_food_get_mean_size() - get
  !!         mean **size** of food items from the memory.
  !!       - the_neurobio::percept_memory_predators_get_mean() - get
  !!         average number of predators from the memory.
  !!       .
  elemental function percept_memory_predators_get_mean (this, last)           &
                                                            result (mean_count)
    class(MEMORY_PERCEPTUAL), intent(in) :: this

    !> @param last Limit to only this number of latest components in the
    !!        history.
    integer, optional, intent(in) :: last

    !> @returns Mean count of predators in the memory stack.
    real(SRP) :: mean_count

    !> Local copy of optional last
    integer :: last_here

    !> History stack size. We determine it from the size of the actual array
    !! rather than `HISTORY_SIZE_PERCEPTION` for further safety.
    integer, parameter :: HIST_SIZE = size(this%memory_pred)

     !> Check if we are given the parameter requesting the latest history size.
    !! if parameter `last` absent or bigger than the array size, get whole
    !! stack array.
    if (present(last)) then
      if ( last < HIST_SIZE ) then
        last_here = last
      else
        last_here = HIST_SIZE
      end if
    else
      last_here = HIST_SIZE
    end if

    !> Calculate the average excluding missing values (masked).
    mean_count=average(this%memory_pred( HIST_SIZE-last_here+1:HIST_SIZE ),   &
                       undef_ret_null=.TRUE. )

  end function percept_memory_predators_get_mean

  !-----------------------------------------------------------------------------
  !> Get the **average number** of predators per single time step within the
  !! perceptual memory stack, split to the first (older) and second(newer)
  !! parts. The whole memory stack ('sample') is split by the `split_val`
  !! parameter and two means are calculated: before the `split_val` and after
  !! it.
  !! @note There are several similar procedures with very similar
  !!       implementation:
  !!       - the_neurobio::percept_memory_food_mean_n_split() - get mean
  !!         **number** of food items from the memory;
  !!       - the_neurobio::percept_memory_food_mean_size_split() - get
  !!         mean **size** of food items from the memory;
  !!       - the_neurobio::percept_memory_predators_mean_split() - get
  !!         average number of predators.
  !!       .
  elemental subroutine percept_memory_predators_mean_split(this, window,      &
                                                      split_val, older, newer)
    class(MEMORY_PERCEPTUAL), intent(in) :: this
    !> @param[in] window is the whole memory window which is analysed, if
    !!            not present, the whole memory stack is used.
    integer, optional, intent(in) :: window
    !> @param[in] split_val is the split value for the separation of the
    !!            older and newer averages. If not present, just splits the
    !!            memory window evenly in two halves.
    integer, optional, intent(in) :: split_val
    !> @param[out] older is the output average number of predators in the
    !!            first (older) part of the memory window.
    real(SRP), intent(out) :: older
    !> @param[out] newer is the output average number of predators in the
    !!            second (newer) part of the memory window.
    real(SRP), intent(out) :: newer

    ! Local copies of optionals.
    integer :: window_loc, split_val_loc

    integer, parameter :: HIST_SIZE = size(this%memory_pred)

    !> ### Implementation details ###
    !> First, check optional parameters: the memory window `window` and the
    !> split value `split_val`. If either is not provided, defaults are used.
    if (present(window)) then
      window_loc = window
      !> (Also, a check is made so that a window exceeding the history stack
      !! length is reduced accordingly to the whole memory size).
      if (window_loc >= HIST_SIZE) window_loc = HIST_SIZE
    else
      !> - whole size of the perceptual memory stack
      !!   commondata::history_size_perception for the memory window
      window_loc = HIST_SIZE
    end if

    if (present(split_val)) then
      split_val_loc = split_val
    else
      !> - half of the memory window for the `split_val`.
      !! .
      split_val_loc = floor( real( window_loc, SRP ) / 2.0 )
    end if

    !> A sanity check is also done, if the split value happen to exceed the
    !! `window` parameter, it is reduced to the default 1/2 of the `window`.
    if (split_val_loc >= window_loc)                                          &
                    split_val_loc = floor( real( window_loc, SRP ) / 2.0 )

    !> Second, the `older` and the `newer` output average values are calculated.
    !! Here is the illustration of the calculation:
    !! @verbatim
    !!   Such 'window' and 'split_val'
    !!   values...
    !!
    !!            |<----- window ----->|
    !!   +--------+--------------------+
    !!   +        |        :|:         +
    !!   +--------+--------------------+
    !!                      ^ split_val
    !!
    !!
    !!   ... result in these means:
    !!
    !!   +--------+---------------------+
    !!   +        | mean for | mean for +
    !!   +        | 'older'  | 'newer'  +
    !!   +--------+---------------------+
    !! @endverbatim
    older=average( this%memory_pred(HIST_SIZE-window_loc+1 :                  &
                                       HIST_SIZE-window_loc+split_val_loc),   &
                   undef_ret_null=.TRUE. )

    newer=average( this%memory_pred(HIST_SIZE-window_loc+split_val_loc+1 :    &
                                       HIST_SIZE),                            &
                   undef_ret_null=.TRUE. )

  end subroutine percept_memory_predators_mean_split

  !-----------------------------------------------------------------------------
  !> Add the various perception objects to the memory stack object. This
  !! procedure is called **after** all the perceptual components (light, depth
  !! food, conspecifics, predators, etc.) are collected (using `set`
  !! object-bound subroutines) into the perception bundle, so all the values
  !! are known and ready to be used.
  elemental subroutine perception_objects_add_memory_stack(this)
    class(PERCEPTION), intent(inout) :: this

    !> Now collect all perception variables into the whole memory stack object.
    call this%memory_stack%add_to_memory(                                     &
                        light  = this%perceive_light%get_current(),           &
                        depth  = this%perceive_depth%get_current(),           &
                        food   = this%perceive_food%get_count(),              &
                        foodsize = this%perceive_food%get_meansize(),         &
                        fooddist = this%perceive_food%get_meandist(),         &
                        consp  = this%perceive_consp%get_count(),             &
                        pred   = this%perceive_predator%get_count(),          &
                        stom   = this%perceive_stomach%get_available(),       &
                        bdmass = this%perceive_body_mass%get_current(),       &
                        energ  = this%perceive_energy%get_current(),          &
                        reprfac= this%perceive_reprfac%get_current()          &
                                                                          )

  end subroutine perception_objects_add_memory_stack

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with PERCEPTION INITS
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !> A single umbrella subroutine to get all **environmental** perceptions:
  !! light, depth. This procedure invokes these calls:
  !! - the_neurobio::perception::feel_light()
  !! - the_neurobio::perception::feel_depth()
  !! .
  !! See also the_neurobio::perception::perceptions_inner().
  subroutine perception_objects_get_all_environmental(this)
    class(PERCEPTION), intent(inout) :: this

    call this%feel_light()
    call this%feel_depth()

  end subroutine perception_objects_get_all_environmental

  !-----------------------------------------------------------------------------
  !> A single umbrella subroutine wrapper to get all **inner** perceptions:
  !! stomach, body mass, energy, age. Invokes all these procedures:
  !! - the_neurobio::perception::feel_stomach()
  !! - the_neurobio::perception::feel_bodymass()
  !! - the_neurobio::perception::feel_energy()
  !! - the_neurobio::perception::feel_age()
  !! - the_neurobio::perception::feel_repfac()
  !! .
  !! See also the_neurobio::perception::perceptions_environ().
  !!
  !! Splitting between the procedures for getting the inner and outer
  !! perceptions is for convenience only, this inner perceptions
  !! subroutine has no other parameters.
  !! @warning It would **not** be easy to implement such a wrapper for the
  !!          **outer** perceptions because the population and various
  !!          environmental objects are not yet available at this object level.
  !! @note    **Templates for outer environmental perceptions**:
  !!          `call proto_parents%individual(ind)%see_food(                   &
  !!                      food_resource_available = habitat_safe%food,        &
  !!                      time_step_model = 1)`
  !!
  !!          `call proto_parents%individual(ind)%see_consp(                  &
  !!                      consp_agents = proto_parents%individual,            &
  !!                      time_step_model = 1 )`
  !!
  !!          `call proto_parents%individual(ind)%see_pred(                   &
  !!                      spatl_agents = predators,                           &
  !!                      time_step_model = 1 )`
  !!           `call proto_parents%individual(ind)%feel_light(timestep)`
  !!           `call proto_parents%individual(ind)%feel_depth()`
  subroutine perception_objects_get_all_inner(this)
    class(PERCEPTION), intent(inout) :: this

    call this%feel_stomach()
    call this%feel_bodymass()
    call this%feel_energy()
    call this%feel_age()
    call this%feel_repfac()

  end subroutine perception_objects_get_all_inner

  !-----------------------------------------------------------------------------
  !> Initialise all the perception objects for the current agent. Do not fill
  !! perception objects with the real data yet.
  elemental subroutine perception_objects_init_agent(this)
    class(PERCEPTION), intent(inout) :: this

    !> Init all perception objects within the agent.
    call this%perceive_light%init()
    call this%perceive_depth%init()
    call this%perceive_food%init(0)     ! food 0
    call this%perceive_consp%init(0)    ! conspecifics 0
    call this%perceive_predator%init(0) ! predators 0
    call this%perceive_stomach%init()
    call this%perceive_body_mass%init()
    call this%perceive_energy%init()
    call this%perceive_age%init()
    call this%perceive_reprfac%init()

    !> Init and cleanup perceptual memory stack at start.
    call this%memory_stack%memory_cleanup()

  end subroutine perception_objects_init_agent

  !-----------------------------------------------------------------------------
  !> Destroy and deallocate all perception objects.
  elemental subroutine perception_objects_destroy(this, clean_memory)
    class(PERCEPTION), intent(inout)  :: this
    !> @param[in] clean_memory Logical flag to cleanup perceptual memory stack.
    logical, optional, intent(in) :: clean_memory

    !> Use the `destroy` method for all perception objects within the agent.
    call this%perceive_light%destroy()
    call this%perceive_depth%destroy()
    call this%perceive_food%destroy()
    call this%perceive_consp%destroy()
    call this%perceive_predator%destroy()
    call this%perceive_stomach%destroy()
    call this%perceive_body_mass%destroy()
    call this%perceive_energy%destroy()
    call this%perceive_age%destroy()
    call this%perceive_reprfac%destroy()

    !> Init and cleanup perceptual memory stack if clean_memory is set to TRUE.
    if (present(clean_memory)) then
      if (clean_memory) call this%memory_stack%memory_cleanup()
    end if

  end subroutine perception_objects_destroy

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Other functions linked with PERCEPTION sub-objects
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Calculate the risk of **predation** as being **perceived / assessed**
  !! by this agent.
  !! @note  It can be placed either to `PERCEPTION` (which might seem more
  !!        logical as it is basically the *perception* of predation risk by
  !!        the agent) or `APPRAISAL` level class. Here it is in the
  !!        `APPRAISAL` because it is actually used here. It may also be safer
  !!        here as we need completed perception objects and perception memory
  !!        stack to assess the objective predation risk.
  elemental function perception_predation_risk_objective(this)                &
                                                        result (predation_risk)
    class(PERCEPTION), intent(in) :: this !> @param[in] this Self.
    !> @returns assessment of the predation risk based on both the
    !!          perception object and its linked perceptual memory component.
    real(SRP) :: predation_risk

    !> ### Notable parameters ###
    !> **WEIGHT_DIRECT**  is the relative weight  given to the immediate
    !! perception of predators over the predators counts in the memory stack.
    !! Obtained from global parameters
    !! (`commondata::predation_risk_weight_immediate`).
    real(SRP), parameter :: WEIGHT_DIRECT = PREDATION_RISK_WEIGHT_IMMEDIATE

    !> **MEM_WIND** is the size of the memory window when assessing the
    !! predator risk, only this number of the latest elements from the memory
    !! stack is taken into account. So we further weight the direct threat
    !! over the background risk when making the decision.
    !! @note  Note that we take into account the whole memory size
    !!        (commondata::history_size_perception).
    integer, parameter :: MEM_WIND = HISTORY_SIZE_PERCEPTION

    !> ### Implementation details ###
    !> Here we analyse the predator perception object and memory stack to get
    !! the **predation risk** perception value, that will be fed into the
    !! sigmoid function via `neuro_resp` at the next step.
    !! Perception of the predation risk is the weighted sum of the **total
    !! number of  predators in the memory stack** and the **current count**
    !! of predators within the visual range of the agent. The actual
    !! calculatio is done by the comon backend that is used for objective and
    !! subjective assessment of risk: the_neurobio::predation_risk_backend().
    predation_risk = predation_risk_backend(                                  &
                        this%perceive_predator%get_count(),                   &
                        this%memory_stack%get_pred_mean(MEM_WIND),            &
                        WEIGHT_DIRECT )

  end function perception_predation_risk_objective

  !-----------------------------------------------------------------------------
  !> Simple computational backend for the risk of predation that is used in
  !! objective risk function the_neurobio::perception_predation_risk_objective()
  !! and the subjective risk function.
  elemental function predation_risk_backend(pred_count, pred_memory_mean,     &
                                            weight_direct)                    &
                                                                  result (risk)
    !> @param[in] pred_count The number of predators in the current perception
    !!            object. This is an estimate of the direct risk of predation
    !!            @f$ r_{d} @f$.
    integer, intent(in) :: pred_count
    !> @param[in] pred_memory_mean The mean number of predators in the memory
    !!            window. The size of the memory window is not set here.
    !!            It is an estimate of the indirect risk of predation
    !!            @f$ r_{id} @f$.
    real(SRP), intent(in) :: pred_memory_mean
    !> @param[in] weight_direct an optional weighting factor for the immediate
    !!            risk (the number of predators in the current perception
    !!            object), @f$ \omega @f$. If not provided, the default value
    !!            is set by the commondata::predation_risk_weight_immediate
    !!            parameter.
    real(SRP), optional, intent(in) :: weight_direct
    real(SRP) :: risk

    ! Local copies of optionals
    real(SRP) :: weight_direct_loc

    !> ### Implementation details ###
    !> First, check if the optional direct risk weighting factor
    !! @f$ \omega @f$) is provided as a dummy parameter. If not provided,
    !! use the default value that is set by the
    !! commondata::predation_risk_weight_immediate parameter.
    if (present(weight_direct)) then
      weight_direct_loc = weight_direct
    else
      weight_direct_loc = PREDATION_RISK_WEIGHT_IMMEDIATE
    end if

    !> Second, calculate the predation risk as a weighted sum of the direct
    !! risk (number of immediately perceived predators, @f$ r_{d} @f$) and
    !! indirect risk (average number of predators in the memory,
    !! @f$ r_{id} @f$):
    !! @f[ R = r_{d} \cdot \omega + r_{id} \cdot (1 - \omega) @f]
    risk = real(pred_count, SRP) * weight_direct +                            &
           pred_memory_mean * (1.0_SRP - weight_direct)

  end function predation_risk_backend

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with NEUROBIOLOGICAL STATE sub-objects
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Initialise the attention components of the emotional state to their
  !! default parameter values. Attention sets weights to individual perceptual
  !! components when the overall weighted sum is calculated. The default
  !! weights are parameters defined in `COMMONDATA`.
  !! @warning The number and nature of the attention components is equal
  !!          to the number of perceptual components, they agree 1 to 1.
  !! @note    The perception weights `weight_` parameters are not passed
  !!          as an array to (a) allow for elemental function, (b) allow
  !!          disabling attention components at init when weights are not
  !!          provided = set to zero.
  !! @note    The **precedence** order of the parameters `all_vals_fix`,
  !!          `all_one` and then `weight_`s, i.e. if `all_vals_fix` is
  !!          provided, all other are ignored (see `return` in if-present-test
  !!          blocks).
  elemental subroutine perception_components_attention_weights_init( this,    &
                                                          all_vals_fix,       &
                                                          all_one,            &
                                                          weight_light,       &
                                                          weight_depth,       &
                                                          weight_food_dir,    &
                                                          weight_food_mem,    &
                                                          weight_conspec,     &
                                                          weight_pred_dir,    &
                                                          weight_predator,    &
                                                          weight_stomach,     &
                                                          weight_bodymass,    &
                                                          weight_energy,      &
                                                          weight_age,         &
                                                          weight_reprfac      )
    class(PERCEPT_COMPONENTS_MOTIV), intent(inout) :: this
    !> @param[in] all_vals_fix Optional parameter setting all weights equal to
    !!            a specific fixed value.
    real(SRP), optional, intent(in) :: all_vals_fix

    !> @param[in] all_one Optional logical parameter setting all weights to 1.0,
    !!            so the perceptual components go into unchanged form (weight=1)
    !!            into the weighted sum (overall primary motivation value).
    logical, optional, intent(in) :: all_one

    !> Optional attention weights for specific perception components.
    !! @note If absent, set to **zero**.
    real(SRP), optional, intent(in) ::                                        &
                  weight_light,          weight_depth,                        &
                  weight_food_dir,       weight_food_mem,                     &
                  weight_conspec,        weight_pred_dir,  weight_predator,   &
                  weight_stomach,        weight_bodymass,                     &
                  weight_energy,         weight_age,                          &
                  weight_reprfac

    !> Local copies of the optional parameters.
    real(SRP) ::                                                              &
                  here_weight_light,     here_weight_depth,                   &
                  here_weight_food_dir,  here_weight_food_mem,                &
                  here_weight_conspec,   here_weight_pred_dir, here_weight_predator, &
                  here_weight_stomach,   here_weight_bodymass,                &
                  here_weight_energy,    here_weight_age,                     &
                  here_weight_reprfac

    real(SRP), parameter :: P1 = 1.0_SRP !> **Unity weight** for unchanged vals.
    real(SRP), parameter :: P0 = 0.0_SRP !> **Zero weight** for unchanged vals.

    !> If `all_vals_fix` is set, set all weights to this fixed value.
    !! @note We do not have option to set all values to an **array**
    !!       to be able to have this procedure **elemental**.
    if (present(all_vals_fix)) then
      this%light     = all_vals_fix
      this%depth     = all_vals_fix
      this%food_dir  = all_vals_fix
      this%food_mem  = all_vals_fix
      this%conspec   = all_vals_fix
      this%pred_dir  = all_vals_fix
      this%predator  = all_vals_fix
      this%stomach   = all_vals_fix
      this%bodymass  = all_vals_fix
      this%energy    = all_vals_fix
      this%age       = all_vals_fix
      this%reprfac   = all_vals_fix
      return         !> Return after setting values.
    end if

    !> If `all_one` is present and set to TRUE, init all
    !! attention weights to 1.0
    if (present(all_one)) then
      if (all_one) then
        this%light     = P1
        this%depth     = P1
        this%food_dir  = P1
        this%food_mem  = P1
        this%conspec   = P1
        this%pred_dir  = P1
        this%predator  = P1
        this%stomach   = P1
        this%bodymass  = P1
        this%energy    = P1
        this%age       = P1
        this%reprfac   = P1
        return         !> Return after setting values.
      end if
    end if

    ! -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
    !> Set individual attention weights
    if ( present(weight_light) ) then
      here_weight_light = weight_light
    else
      here_weight_light = P0
    end if

    if ( present(weight_depth) ) then
      here_weight_depth = weight_depth
    else
      here_weight_depth = P0
    end if

    if ( present(weight_food_dir) ) then
      here_weight_food_dir = weight_food_dir
    else
      here_weight_food_dir = P0
    end if

    if ( present(weight_food_mem) ) then
      here_weight_food_mem = weight_food_mem
    else
      here_weight_food_mem = P0
    end if

    if ( present(weight_conspec) ) then
      here_weight_conspec = weight_conspec
    else
      here_weight_conspec = P0
    end if

    if ( present(weight_pred_dir) ) then
      here_weight_pred_dir = weight_pred_dir
    else
      here_weight_pred_dir = P0
    end if

    if ( present(weight_predator) ) then
      here_weight_predator = weight_predator
    else
      here_weight_predator = P0
    end if

    if ( present(weight_stomach) ) then
      here_weight_stomach = weight_stomach
    else
      here_weight_stomach = P0
    end if

    if ( present(weight_bodymass) ) then
      here_weight_bodymass = weight_bodymass
    else
      here_weight_bodymass = P0
    end if

    if ( present(weight_energy) ) then
      here_weight_energy = weight_energy
    else
      here_weight_energy = P0
    end if

    if ( present(weight_age) ) then
      here_weight_age = weight_age
    else
      here_weight_age = P0
    end if

    if ( present(weight_reprfac) ) then
      here_weight_reprfac = weight_reprfac
    else
      here_weight_reprfac = P0
    end if
    ! -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    !> If nothing is provided, set attention weights
    !! from the dummy parameters of this procedure.
    this%light     = here_weight_light
    this%depth     = here_weight_depth
    this%food_dir  = here_weight_food_dir
    this%food_mem  = here_weight_food_mem
    this%conspec   = here_weight_conspec
    this%pred_dir  = here_weight_pred_dir
    this%predator  = here_weight_predator
    this%stomach   = here_weight_stomach
    this%bodymass  = here_weight_bodymass
    this%energy    = here_weight_energy
    this%age       = here_weight_age
    this%reprfac   = here_weight_reprfac

  end subroutine perception_components_attention_weights_init

  !-----------------------------------------------------------------------------
  !> Set and calculate individual perceptual components for **this**
  !! motivational state using the **neuronal response** function, for
  !! **this_agent**.
  !! @note The **this_agent** has intent [inout], so can be changed as a result
  !!       of this procedure, gene labels are set for genes involved in the
  !!       neuronal response.
  !! @note TODO: huge parameter list- ugly coding, try to fix.
  !! @note This procedure uses labelled if constructs with inline call of
  !!       the neuronal response function `neuro_resp`, unlike this, the intent
  !!       [in] procedure `perception_components_neuronal_response_calculate`
  !!       uses inner subroutines.
  !  TODO: change the intent[in] removing inner subroutines? Or leave as is?
  subroutine perception_components_neuronal_response_init_set(                &
                                                this, this_agent,             &
                                                ! Boolean G x P matrices:
                                                param_gp_matrix_light,        &
                                                param_gp_matrix_depth,        &
                                                param_gp_matrix_food_dir,     &
                                                param_gp_matrix_food_mem,     &
                                                param_gp_matrix_conspec,      &
                                                param_gp_matrix_pred_dir,     &
                                                param_gp_matrix_predator,     &
                                                param_gp_matrix_stomach,      &
                                                param_gp_matrix_bodymass,     &
                                                param_gp_matrix_energy,       &
                                                param_gp_matrix_age,          &
                                                param_gp_matrix_reprfac,      &
                                                ! G x P variances:
                                                param_gerror_cv_light,        &
                                                param_gerror_cv_depth,        &
                                                param_gerror_cv_food_dir,     &
                                                param_gerror_cv_food_mem,     &
                                                param_gerror_cv_conspec,      &
                                                param_gerror_cv_pred_dir,     &
                                                param_gerror_cv_predator,     &
                                                param_gerror_cv_stomach,      &
                                                param_gerror_cv_bodymass,     &
                                                param_gerror_cv_energy,       &
                                                param_gerror_cv_age,          &
                                                param_gerror_cv_reprfac,      &
                                                ! labels for genes:
                                                param_gene_label_light,       &
                                                param_gene_label_depth,       &
                                                param_gene_label_food_dir,    &
                                                param_gene_label_food_mem,    &
                                                param_gene_label_conspec,     &
                                                param_gene_label_pred_dir,    &
                                                param_gene_label_predator,    &
                                                param_gene_label_stomach,     &
                                                param_gene_label_bodymass,    &
                                                param_gene_label_energy,      &
                                                param_gene_label_age,         &
                                                param_gene_label_reprfac   )

    class(PERCEPT_COMPONENTS_MOTIV), intent(inout) :: this
    !> @param[inout] this_agent The actor agent.
    class(APPRAISAL), intent(inout) :: this_agent

    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! Pass boolean G x P parameter matrices from `COMMONDATA` :

    !> ### Boolean G x P matrices ###
    !> Input structure of the fixed parameters that define the boolean
    !! @ref aha_buildblocks_gp_matrix_intro "genotype x phenotype matrices"
    !! for each perceptual component of motivational state defined in
    !! @ref commondata.
    !! @warning There should be **exactly** as many `param_g_p_matrix`
    !!          parameters as perceptual components for this motivation
    !!          (`the_neurobio::percept_components_motiv`).
    !! @warning The dimensionality of the parameter arrays must be **exactly**
    !!          the same as in @ref commondata. This is why **assumed shape
    !!          arrays** (:,:) are **not used** here.
    !> @param[in] param_gp_matrix_light      boolean *G* x *P* matrix for
    !!            light;
    !! @param[in] param_gp_matrix_depth      boolean *G* x *P* matrix for
    !!            depth;
    !! @param[in] param_gp_matrix_food_dir   boolean *G* x *P* matrix for
    !!            direct food
    !! @param[in] param_gp_matrix_food_mem   boolean *G* x *P* matrix for
    !!            number of food items in memory;
    !! @param[in] param_gp_matrix_conspec    boolean *G* x *P* matrix for
    !!            number of conspecifics;
    !! @param[in] param_gp_matrix_pred_dir   boolean *G* x *P* matrix for
    !!            direct predation risk;
    !! @param[in] param_gp_matrix_predator   boolean *G* x *P* matrix for
    !!            number of predators;
    !! @param[in] param_gp_matrix_stomach    boolean *G* x *P* matrix for
    !!            stomach contents;
    !! @param[in] param_gp_matrix_bodymass   boolean *G* x *P* matrix for
    !!            body mass;
    !! @param[in] param_gp_matrix_energy     boolean *G* x *P* matrix for
    !!            energy reserves;
    !! @param[in] param_gp_matrix_age        boolean *G* x *P* matrix for
    !!            age;
    !! @param[in] param_gp_matrix_reprfac    boolean *G* x *P* matrix for
    !!            reproductive factor.
    logical, dimension(MAX_NALLELES,N_CHROMOSOMES), optional, intent(in) ::   &
                                              param_gp_matrix_light,          &
                                              param_gp_matrix_depth,          &
                                              param_gp_matrix_food_dir,       &
                                              param_gp_matrix_food_mem,       &
                                              param_gp_matrix_conspec,        &
                                              param_gp_matrix_pred_dir,       &
                                              param_gp_matrix_predator,       &
                                              param_gp_matrix_stomach,        &
                                              param_gp_matrix_bodymass,       &
                                              param_gp_matrix_energy,         &
                                              param_gp_matrix_age,            &
                                              param_gp_matrix_reprfac

    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! Pass Gaussian error parameters from `COMMONDATA`:

    !> ### Coefficients of variation parameters ###
    !> Input structures that define the coefficient of variation for the
    !! Gaussian perception error parameters for each of the perceptual
    !! components. Normally they are also defined in @ref commondata.
    !! @param[in] param_gerror_cv_light     coefficient of variation for
    !!            light
    !! @param[in] param_gerror_cv_depth     coefficient of variation for
    !!            depth
    !! @param[in] param_gerror_cv_food_dir  coefficient of variation for
    !!            direct food;
    !! @param[in] param_gerror_cv_food_mem  coefficient of variation for
    !!            number of food items in memory;
    !! @param[in] param_gerror_cv_conspec   coefficient of variation for
    !!            number of conspecific;
    !! @param[in] param_gerror_cv_pred_dir  coefficient of variation for
    !!            direct predation risk;
    !! @param[in] param_gerror_cv_predator  coefficient of variation for
    !!            number of predators;
    !! @param[in] param_gerror_cv_stomach   coefficient of variation for
    !!            stomach contents;
    !! @param[in] param_gerror_cv_bodymass  coefficient of variation for
    !!            body mass;
    !! @param[in] param_gerror_cv_energy    coefficient of variation for
    !!            energy reserves;
    !! @param[in] param_gerror_cv_age       coefficient of variation for
    !!            age;
    !! @param[in] param_gerror_cv_reprfac   coefficient of variation for
    !!            reproductive factor.
    real(SRP), optional, intent(in) ::        param_gerror_cv_light,          &
                                              param_gerror_cv_depth,          &
                                              param_gerror_cv_food_dir,       &
                                              param_gerror_cv_food_mem,       &
                                              param_gerror_cv_conspec,        &
                                              param_gerror_cv_pred_dir,       &
                                              param_gerror_cv_predator,       &
                                              param_gerror_cv_stomach,        &
                                              param_gerror_cv_bodymass,       &
                                              param_gerror_cv_energy,         &
                                              param_gerror_cv_age,            &
                                              param_gerror_cv_reprfac

    !> ### Perception component labels ###
    !> Input parameters for the gene labels that code specific neuronal
    !! response genes.
    !  @note  Do not need local copies of the label parameters as they are
    !         not present in the neuronal response function call.
    !> @param[in] param_gene_label_light     label for light;
    !! @param[in] param_gene_label_depth     label for depth;
    !! @param[in] param_gene_label_food_dir  label for direct food;
    !! @param[in] param_gene_label_food_mem  label for number of food items
    !!            in memory;
    !! @param[in] param_gene_label_conspec   label for number of conspecific;
    !! @param[in] param_gene_label_pred_dir  label for direct predation risk;
    !! @param[in] param_gene_label_predator  label for number of predators;
    !! @param[in] param_gene_label_stomach   label for stomach contents;
    !! @param[in] param_gene_label_bodymass  label for body mass;
    !! @param[in] param_gene_label_energy    label for energy reserves;
    !! @param[in] param_gene_label_age       label for age;
    !! @param[in] param_gene_label_reprfac   label for reproductive factor;
    character(len=*), optional, intent(in) :: param_gene_label_light,         &
                                              param_gene_label_depth,         &
                                              param_gene_label_food_dir,      &
                                              param_gene_label_food_mem,      &
                                              param_gene_label_conspec,       &
                                              param_gene_label_pred_dir,      &
                                              param_gene_label_predator,      &
                                              param_gene_label_stomach,       &
                                              param_gene_label_bodymass,      &
                                              param_gene_label_energy,        &
                                              param_gene_label_age,           &
                                              param_gene_label_reprfac

    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! Local variables

    !> Local copies of **genotype x phenotype** boolean matrices.
    logical, dimension(MAX_NALLELES,N_CHROMOSOMES) ::                         &
                                        here_param_gp_matrix_light,           &
                                        here_param_gp_matrix_depth,           &
                                        here_param_gp_matrix_food_dir,        &
                                        here_param_gp_matrix_food_mem,        &
                                        here_param_gp_matrix_conspec,         &
                                        here_param_gp_matrix_pred_dir,        &
                                        here_param_gp_matrix_predator,        &
                                        here_param_gp_matrix_stomach,         &
                                        here_param_gp_matrix_bodymass,        &
                                        here_param_gp_matrix_energy,          &
                                        here_param_gp_matrix_age,             &
                                        here_param_gp_matrix_reprfac

    !> Local copies of the Gaussian perception error coefficients of variation.
    real(SRP) ::                        here_param_gerror_cv_light,           &
                                        here_param_gerror_cv_depth,           &
                                        here_param_gerror_cv_food_dir,        &
                                        here_param_gerror_cv_food_mem,        &
                                        here_param_gerror_cv_conspec,         &
                                        here_param_gerror_cv_pred_dir,        &
                                        here_param_gerror_cv_predator,        &
                                        here_param_gerror_cv_stomach,         &
                                        here_param_gerror_cv_bodymass,        &
                                        here_param_gerror_cv_energy,          &
                                        here_param_gerror_cv_age,             &
                                        here_param_gerror_cv_reprfac

    !- - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - - -
    !> ### Implementation notes ###
    !> We check input boolean G x P matrices and calculate the perceptual
    !! components of **this** motivation state only when the boolean matrix
    !! is provided as a parameter. Also check the corresponding variance/CV
    !! and reset to deterministic (variance zero) is not provided as a dummy
    !! parameter parameter.
    !! @warning There should be **exactly** as many `param_g_p_matrix` and
    !!          `param_gerror_cv_light` parameters as perceptual components
    !!          for this motivation (`the_neurobio::percept_components_motiv`).
    LIGHT: if (present(param_gp_matrix_light)) then
      here_param_gp_matrix_light = param_gp_matrix_light
      if (present(param_gerror_cv_light)) then
        here_param_gerror_cv_light = param_gerror_cv_light
      else
        here_param_gerror_cv_light = 0.0_SRP
      end if
      !> - calculate the perceptual component for **light**.
      if (present(param_gene_label_light)) then
        !>   @note  The function is almost the same as in
        !!          `the_neurobio::appraisal` but
        !!          **does** set the label so `the_agent` has
        !!          **intent[inout]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%light,                                      &
                g_p_matrix = here_param_gp_matrix_light,                      &
                init_val = this_agent%perceive_light%get_current(),           &
                gerror_cv = here_param_gerror_cv_light,                       &
                label = param_gene_label_light )
      else
        ! @note  The function is almost the same as in `APPRAISAL` but does
        !        **not** set the label so `the_agent` has the **intent[in]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%light,                                      &
                g_p_matrix = here_param_gp_matrix_light,                      &
                init_val = this_agent%perceive_light%get_current(),           &
                gerror_cv = here_param_gerror_cv_light )
      end if
    end if LIGHT

    DEPTH: if (present(param_gp_matrix_depth)) then
      here_param_gp_matrix_depth = param_gp_matrix_depth
      if (present(param_gerror_cv_depth)) then
        here_param_gerror_cv_depth = param_gerror_cv_depth
      else
        here_param_gerror_cv_depth = 0.0_SRP
      end if
      !> - calculate the perceptual component for **depth**.
      if (present(param_gene_label_depth))  then
        ! @note  The function is almost the same as in `APPRAISAL` but
        !        **does** set the label so `the_agent` has **intent[inout]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%depth,                                      &
                g_p_matrix = here_param_gp_matrix_depth,                      &
                init_val = this_agent%perceive_depth%get_current(),           &
                gerror_cv = here_param_gerror_cv_depth,                       &
                label = param_gene_label_depth )
      else
        ! @note  The function is almost the same as in `APPRAISAL` but does
        !        **not** set the label so `the_agent` has the **intent[in]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%depth,                                      &
                g_p_matrix = here_param_gp_matrix_depth,                      &
                init_val = this_agent%perceive_depth%get_current(),           &
                gerror_cv = here_param_gerror_cv_depth )
      end if
    end if DEPTH

    FOOD_DIR: if (present(param_gp_matrix_food_dir)) then
      here_param_gp_matrix_food_dir = param_gp_matrix_food_dir
      if (present(param_gerror_cv_food_dir)) then
        here_param_gerror_cv_food_dir = param_gerror_cv_food_dir
      else
        here_param_gerror_cv_food_dir = 0.0_SRP
      end if
      !> - calculate the perceptual component for **food_dir**.
      if (present(param_gene_label_food_dir)) then
        ! @note  The function is almost the same as in `APPRAISAL` but
        !        **does** set the label so `the_agent` has **intent[inout]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%food_dir,                                   &
                g_p_matrix = here_param_gp_matrix_food_dir,                   &
                init_val = real(this_agent%perceive_food%get_count(), SRP),   &
                gerror_cv = here_param_gerror_cv_food_dir,                    &
                label = param_gene_label_food_dir  )
      else
        ! @note  The function is almost the same as in `APPRAISAL` but does
        !        **not** set the label so `the_agent` has the **intent[in]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%food_dir,                                   &
                g_p_matrix = here_param_gp_matrix_food_dir,                   &
                init_val = real(this_agent%perceive_food%get_count(), SRP),   &
                gerror_cv = here_param_gerror_cv_food_dir )
      end if
    end if FOOD_DIR

    FOOD_MEM: if (present(param_gp_matrix_food_mem)) then
      here_param_gp_matrix_food_mem = param_gp_matrix_food_mem
      if (present(param_gerror_cv_food_mem)) then
        here_param_gerror_cv_food_mem = param_gerror_cv_food_mem
      else
        here_param_gerror_cv_food_mem = 0.0_SRP
      end if
      !> - calculate the perceptual component for **food_mem**.
      if (present(param_gene_label_food_mem)) then
        ! @note  The function is almost the same as in `APPRAISAL` but
        !        **does** set the label so `the_agent` has **intent[inout]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%food_mem,                                   &
                g_p_matrix = here_param_gp_matrix_food_mem,                   &
                init_val = this_agent%memory_stack%get_food_mean_n(),         &
                gerror_cv = here_param_gerror_cv_food_mem,                    &
                label = param_gene_label_food_mem )
      else
        ! @note  The function is almost the same as in `APPRAISAL` but does
        !        **not** set the label so `the_agent` has the **intent[in]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%food_mem,                                   &
                g_p_matrix = here_param_gp_matrix_food_mem,                   &
                init_val = this_agent%memory_stack%get_food_mean_n(),         &
                gerror_cv = here_param_gerror_cv_food_mem )
      end if
    end if FOOD_MEM

    CONSPEC: if (present(param_gp_matrix_conspec)) then
      here_param_gp_matrix_conspec = param_gp_matrix_conspec
      if (present(param_gerror_cv_conspec)) then
        here_param_gerror_cv_conspec = param_gerror_cv_conspec
      else
        here_param_gerror_cv_conspec = 0.0_SRP
      end if
      !> - calculate the perceptual component for **conspec**.
      if (present(param_gene_label_conspec)) then
        ! @note  The function is almost the same as in `APPRAISAL` but
        !        **does** set the label so `the_agent` has **intent[inout]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%conspec,                                    &
                g_p_matrix = here_param_gp_matrix_conspec,                    &
                init_val = real(this_agent%perceive_consp%get_count(), SRP),  &
                gerror_cv = here_param_gerror_cv_conspec,                     &
                label = param_gene_label_conspec )
      else
        ! @note  The function is almost the same as in `APPRAISAL` but does
        !        **not** set the label so `the_agent` has the **intent[in]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%conspec,                                    &
                g_p_matrix = here_param_gp_matrix_conspec,                    &
                init_val = real(this_agent%perceive_consp%get_count(), SRP),  &
                gerror_cv = here_param_gerror_cv_conspec )
      end if
    end if CONSPEC

    PRED_DIR: if (present(param_gp_matrix_pred_dir)) then
      here_param_gp_matrix_pred_dir = param_gp_matrix_pred_dir
      if (present(param_gerror_cv_pred_dir)) then
        here_param_gerror_cv_pred_dir = param_gerror_cv_pred_dir
      else
        here_param_gerror_cv_pred_dir = 0.0_SRP
      end if
      !> - calculate the perceptual component for **direct predation**.
      if (present(param_gene_label_pred_dir)) then
        ! @note  The function is almost the same as in `APPRAISAL` but
        !        **does** set the label so `the_agent` has **intent[inout]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%pred_dir,                                   &
                g_p_matrix = here_param_gp_matrix_pred_dir,                   &
                init_val = this_agent%risk_pred(),                            &
                gerror_cv = here_param_gerror_cv_pred_dir,                    &
                label = param_gene_label_pred_dir )
      else
        ! @note  The function is almost the same as in `APPRAISAL` but does
        !        **not** set the label so `the_agent` has the **intent[in]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%pred_dir,                                   &
                g_p_matrix = here_param_gp_matrix_pred_dir,                   &
                init_val = this_agent%risk_pred(),                            &
                gerror_cv = here_param_gerror_cv_pred_dir )
      end if
    end if PRED_DIR

    PREDATOR: if (present(param_gp_matrix_predator)) then
      here_param_gp_matrix_predator = param_gp_matrix_predator
      if (present(param_gerror_cv_predator)) then
        here_param_gerror_cv_predator = param_gerror_cv_predator
      else
        here_param_gerror_cv_predator = 0.0_SRP
      end if
      !> - calculate the perceptual component for **predator**.
      if (present(param_gene_label_predator)) then
        ! @note  The function is almost the same as in `APPRAISAL` but
        !        **does** set the label so `the_agent` has **intent[inout]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%predator,                                   &
                g_p_matrix = here_param_gp_matrix_predator,                   &
                init_val = this_agent%predation_risk(),                       &
                gerror_cv = here_param_gerror_cv_predator,                    &
                label = param_gene_label_predator )
      else
        ! @note  The function is almost the same as in `APPRAISAL` but does
        !        **not** set the label so `the_agent` has the **intent[in]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%predator,                                   &
                g_p_matrix = here_param_gp_matrix_predator,                   &
                init_val = this_agent%predation_risk(),                       &
                gerror_cv = here_param_gerror_cv_predator )
      end if
    end if PREDATOR

    STOMACH: if (present(param_gp_matrix_stomach)) then
      here_param_gp_matrix_stomach = param_gp_matrix_stomach
      if (present(param_gerror_cv_stomach)) then
        here_param_gerror_cv_stomach = param_gerror_cv_stomach
      else
        here_param_gerror_cv_stomach = 0.0_SRP
      end if
      !> - calculate the perceptual component for **stomach**.
      if (present(param_gene_label_stomach)) then
        ! @note  The function is almost the same as in `APPRAISAL` but
        !        **does** set the label so `the_agent` has **intent[inout]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%stomach,                                    &
                g_p_matrix = here_param_gp_matrix_stomach,                    &
                init_val = this_agent%perceive_stomach%get_available(),       &
                gerror_cv = here_param_gerror_cv_stomach,                     &
                label = param_gene_label_stomach )
      else
        ! @note  The function is almost the same as in `APPRAISAL` but does
        !        **not** set the label so `the_agent` has the **intent[in]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%stomach,                                    &
                g_p_matrix = here_param_gp_matrix_stomach,                    &
                init_val = this_agent%perceive_stomach%get_available(),       &
                gerror_cv = here_param_gerror_cv_stomach )
      end if
    end if STOMACH

    BODYMASS: if (present(param_gp_matrix_bodymass)) then
      here_param_gp_matrix_bodymass = param_gp_matrix_bodymass
      if (present(param_gerror_cv_bodymass)) then
        here_param_gerror_cv_bodymass = param_gerror_cv_bodymass
      else
        here_param_gerror_cv_bodymass = 0.0_SRP
      end if
      !> - calculate the perceptual component for **bodymass**.
      if (present(param_gene_label_bodymass)) then
        ! @note  The function is almost the same as in `APPRAISAL` but
        !        **does** set the label so `the_agent` has **intent[inout]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%bodymass,                                   &
                g_p_matrix = here_param_gp_matrix_bodymass,                   &
                init_val = this_agent%perceive_body_mass%get_current(),       &
                gerror_cv = here_param_gerror_cv_bodymass,                    &
                label = param_gene_label_bodymass )
      else
        ! @note  The function is almost the same as in `APPRAISAL` but does
        !        **not** set the label so `the_agent` has the **intent[in]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%bodymass,                                   &
                g_p_matrix = here_param_gp_matrix_bodymass,                   &
                init_val = this_agent%perceive_body_mass%get_current(),       &
                gerror_cv = here_param_gerror_cv_bodymass )
      end if
    end if BODYMASS

    ENERGY: if (present(param_gp_matrix_energy)) then
      here_param_gp_matrix_energy = param_gp_matrix_energy
      if (present(param_gerror_cv_energy)) then
        here_param_gerror_cv_energy = param_gerror_cv_energy
      else
        here_param_gerror_cv_energy = 0.0_SRP
      end if
      !> - calculate the perceptual component for **energy**.
      if (present(param_gene_label_energy)) then
        ! @note  The function is almost the same as in `APPRAISAL` but
        !        **does** set the label so `the_agent` has **intent[inout]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%energy,                                     &
                g_p_matrix = here_param_gp_matrix_energy,                     &
                init_val = this_agent%perceive_energy%get_current(),          &
                gerror_cv = here_param_gerror_cv_energy,                      &
                label = param_gene_label_energy )
      else
        ! @note  The function is almost the same as in `APPRAISAL` but does
        !        **not** set the label so `the_agent` has the **intent[in]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%energy,                                     &
                g_p_matrix = here_param_gp_matrix_energy,                     &
                init_val = this_agent%perceive_energy%get_current(),          &
                gerror_cv = here_param_gerror_cv_energy )
      end if
    end if ENERGY

    AGE: if (present(param_gp_matrix_age)) then
      here_param_gp_matrix_age = param_gp_matrix_age
      if (present(param_gerror_cv_age)) then
        here_param_gerror_cv_age = param_gerror_cv_age
      else
        here_param_gerror_cv_age = 0.0_SRP
      end if
      !> - calculate the perceptual component for **age**.
      if (present(param_gene_label_age)) then
        ! @note  The function is almost the same as in `APPRAISAL` but
        !        **does** set the label so `the_agent` has **intent[inout]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%age,                                        &
                g_p_matrix = here_param_gp_matrix_age,                        &
                init_val = real(this_agent%perceive_age%get_current(), SRP),  &
                gerror_cv = here_param_gerror_cv_age,                         &
                label = param_gene_label_age )
      else
        ! @note  The function is almost the same as in `APPRAISAL` but does
        !        **not** set the label so `the_agent` has the **intent[in]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%age,                                        &
                g_p_matrix = here_param_gp_matrix_age,                        &
                init_val = real(this_agent%perceive_age%get_current(), SRP),  &
                gerror_cv = here_param_gerror_cv_age )
      end if
    end if AGE

    REPRFAC: if (present(param_gp_matrix_reprfac)) then
      here_param_gp_matrix_reprfac = param_gp_matrix_reprfac
      if (present(param_gerror_cv_reprfac)) then
        here_param_gerror_cv_reprfac = param_gerror_cv_reprfac
      else
        here_param_gerror_cv_reprfac = 0.0_SRP
      end if
      !> - calculate the perceptual component for **reproduct. factor**.
      if (present(param_gene_label_reprfac)) then
        ! @note  The function is almost the same as in `APPRAISAL` but
        !        **does** set the label so `the_agent` has **intent[inout]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%reprfac,                                    &
                g_p_matrix = here_param_gp_matrix_reprfac,                    &
                init_val = this_agent%perceive_reprfac%get_current(),         &
                gerror_cv = here_param_gerror_cv_reprfac,                     &
                label = param_gene_label_reprfac )
      else
        ! @note  The function is almost the same as in `APPRAISAL` but does
        !        **not** set the label so `the_agent` has the **intent[in]**.
        call this_agent%neuro_resp(                                           &
                this_trait = this%reprfac,                                    &
                g_p_matrix = here_param_gp_matrix_reprfac,                    &
                init_val = this_agent%perceive_reprfac%get_current(),         &
                gerror_cv = here_param_gerror_cv_reprfac )
      end if
    end if REPRFAC

  end subroutine perception_components_neuronal_response_init_set

  !-----------------------------------------------------------------------------
  !> Calculate  individual perceptual components for **this** motivational
  !! state using the **neuronal response** function, for an **this_agent**.
  !! @note The **this_agent** has intent [in], so is unchanged as a result
  !!       of this procedure. Unlike the above intent [inout] procedure,
  !!       this accepts optional perception parameters that override those
  !!       stored in `this_agent` data structure. This is done for calculating
  !!       representation **expectancies** from possible behaviour.
  !!
  !  @note TODO: huge parameter list- ugly coding, try to fix.
  subroutine perception_components_neuronal_response_calculate(               &
                                                this, this_agent,             &
                                                ! Boolean G x P matrices:
                                                param_gp_matrix_light,        &
                                                param_gp_matrix_depth,        &
                                                param_gp_matrix_food_dir,     &
                                                param_gp_matrix_food_mem,     &
                                                param_gp_matrix_conspec,      &
                                                param_gp_matrix_pred_dir,     &
                                                param_gp_matrix_predator,     &
                                                param_gp_matrix_stomach,      &
                                                param_gp_matrix_bodymass,     &
                                                param_gp_matrix_energy,       &
                                                param_gp_matrix_age,          &
                                                param_gp_matrix_reprfac,      &
                                                ! G x P variances:
                                                param_gerror_cv_light,        &
                                                param_gerror_cv_depth,        &
                                                param_gerror_cv_food_dir,     &
                                                param_gerror_cv_food_mem,     &
                                                param_gerror_cv_conspec,      &
                                                param_gerror_cv_pred_dir,     &
                                                param_gerror_cv_predator,     &
                                                param_gerror_cv_stomach,      &
                                                param_gerror_cv_bodymass,     &
                                                param_gerror_cv_energy,       &
                                                param_gerror_cv_age,          &
                                                param_gerror_cv_reprfac,      &
                                                ! Override raw perceptions
                                                perception_override_light,    &
                                                perception_override_depth,    &
                                                perception_override_food_dir, &
                                                perception_override_food_mem, &
                                                perception_override_conspec,  &
                                                perception_override_pred_dir, &
                                                perception_override_predator, &
                                                perception_override_stomach,  &
                                                perception_override_bodymass, &
                                                perception_override_energy,   &
                                                perception_override_age,      &
                                                perception_override_reprfac   &
                                                                              )

    class(PERCEPT_COMPONENTS_MOTIV), intent(inout) :: this
    !> @param[inout] this_agent The actor agent.
    class(APPRAISAL), intent(in) :: this_agent

    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! Pass boolean G x P parameter matrices from `COMMONDATA` :

    !> ### Boolean G x P matrices ###
    !> Input structure of the fixed parameters that define the boolean
    !! @ref aha_buildblocks_gp_matrix_intro "genotype x phenotype matrices"
    !! for each perceptual component of motivational state defined in
    !! @ref commondata.
    !! @warning There should be **exactly** as many `param_g_p_matrix`
    !!          parameters as perceptual components for this motivation
    !!          (`the_neurobio::percept_components_motiv`).
    !! @warning The dimensionality of the parameter arrays must be **exactly**
    !!          the same as in @ref commondata. This is why **assumed shape
    !!          arrays** (:,:) are **not used** here.
    !> @param[in] param_gp_matrix_light     boolean *G* x *P* matrix
    !!            for light;
    !! @param[in] param_gp_matrix_depth     boolean *G* x *P* matrix
    !!            for depth;
    !! @param[in] param_gp_matrix_food_dir  boolean *G* x *P* matrix
    !!            for direct food;
    !! @param[in] param_gp_matrix_food_mem  boolean *G* x *P* matrix
    !!            for number of food items in memory;
    !! @param[in] param_gp_matrix_conspec   boolean *G* x *P* matrix
    !!            for number of conspecifics;
    !! @param[in] param_gp_matrix_pred_dir  boolean *G* x *P* matrix
    !!            for direct predation risk;
    !! @param[in] param_gp_matrix_predator  boolean *G* x *P* matrix
    !!            for number of predators;
    !! @param[in] param_gp_matrix_stomach   boolean *G* x *P* matrix
    !!            for stomach contents;
    !! @param[in] param_gp_matrix_bodymass  boolean *G* x *P* matrix
    !!            for body mass;
    !! @param[in] param_gp_matrix_energy    boolean *G* x *P* matrix
    !!            for energy reserves;
    !! @param[in] param_gp_matrix_age       boolean *G* x *P* matrix
    !!            for age;
    !! @param[in] param_gp_matrix_reprfac   boolean *G* x *P* matrix
    !!            for reproductive factor.
    logical, dimension(MAX_NALLELES,N_CHROMOSOMES), optional, intent(in) ::   &
                                              param_gp_matrix_light,          &
                                              param_gp_matrix_depth,          &
                                              param_gp_matrix_food_dir,       &
                                              param_gp_matrix_food_mem,       &
                                              param_gp_matrix_conspec,        &
                                              param_gp_matrix_pred_dir,       &
                                              param_gp_matrix_predator,       &
                                              param_gp_matrix_stomach,        &
                                              param_gp_matrix_bodymass,       &
                                              param_gp_matrix_energy,         &
                                              param_gp_matrix_age,            &
                                              param_gp_matrix_reprfac

    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! Pass Gaussian error parameters from `COMMONDATA`:

    !> ### Coefficients of variation parameters ###
    !> Input structures that define the coefficient of variation for the
    !! Gaussian perception error parameters for each of the perceptual
    !! components. Normally they are also defined in @ref commondata.
    !> @param[in] param_gerror_cv_light      coefficient of variation
    !!            for light;
    !! @param[in] param_gerror_cv_depth      coefficient of variation
    !!            for depth;
    !! @param[in] param_gerror_cv_food_dir   coefficient of variation
    !!            for direct food;
    !! @param[in] param_gerror_cv_food_mem   coefficient of variation
    !!            for number of food items in memory;
    !! @param[in] param_gerror_cv_conspec    coefficient of variation
    !!            for number of conspecific;
    !! @param[in] param_gerror_cv_pred_dir   coefficient of variation
    !!            for direct predation risk;
    !! @param[in] param_gerror_cv_predator   coefficient of variation
    !!            for number of predators;
    !! @param[in] param_gerror_cv_stomach    coefficient of variation
    !!            for stomach contents;
    !! @param[in] param_gerror_cv_bodymass   coefficient of variation
    !!            for body mass;
    !! @param[in] param_gerror_cv_energy     coefficient of variation
    !!            for energy reserves;
    !! @param[in] param_gerror_cv_age        coefficient of variation
    !!            for age;
    !! @param[in] param_gerror_cv_reprfac    coefficient of variation
    !!            for reproductive factor.
    real(SRP), optional, intent(in) ::        param_gerror_cv_light,          &
                                              param_gerror_cv_depth,          &
                                              param_gerror_cv_food_dir,       &
                                              param_gerror_cv_food_mem,       &
                                              param_gerror_cv_conspec,        &
                                              param_gerror_cv_pred_dir,       &
                                              param_gerror_cv_predator,       &
                                              param_gerror_cv_stomach,        &
                                              param_gerror_cv_bodymass,       &
                                              param_gerror_cv_energy,         &
                                              param_gerror_cv_age,            &
                                              param_gerror_cv_reprfac

    !> ### Perception overrides (fake perceptions) ###
    !> @anchor percept_overrides_lst
    !> Optional parameters to override the perception value of `this_agent`
    !! that will be passed through the neuronal response function.
    !! We need to be able to pass arbitrary perception values to neuronal
    !! response to assess expectancies of different behaviours for this agent.
    !! @warning Note that the data types of the perception override values
    !!          **must** agree with the `init_val` parameter of the neuronal
    !!          response function, i.e. be **real**. But the perception
    !!          object accessor (get) function of the respective perception
    !!          object (`PERCEPT_`) sometimes have integer type. In such cases
    !!          use inline real conversion function **when calling this**
    !!          procedure.
    !> @param[in] perception_override_light     perception override
    !!            for light;
    !! @param[in] perception_override_depth     perception override
    !!            for depth;
    !! @param[in] perception_override_food_dir  perception override
    !!            for direct food (convert from integer);
    !! @param[in] perception_override_food_mem  perception override
    !!            for number of food items in memory;
    !! @param[in] perception_override_conspec   perception override
    !!            for number of conspecific (convert from integer);
    !! @param[in] perception_override_pred_dir  perception override
    !!            for direct predation risk;
    !! @param[in] perception_override_predator  perception override
    !!            for number of predators;
    !! @param[in] perception_override_stomach   perception override
    !!            for stomach contents;
    !! @param[in] perception_override_bodymass  perception override
    !!            for body mass;
    !! @param[in] perception_override_energy    perception override
    !!            for energy reserves;
    !! @param[in] perception_override_age       perception override
    !!            for age (convert from integer);
    !! @param[in] perception_override_reprfac   perception override
    !!            for reproductive factor.
    real(SRP), optional, intent(in) :: perception_override_light,             &
                                       perception_override_depth,             &
                                       perception_override_food_dir,   & ! integer
                                       perception_override_food_mem,          &
                                       perception_override_conspec,    & ! integer
                                       perception_override_pred_dir,          &
                                       perception_override_predator,          &
                                       perception_override_stomach,           &
                                       perception_override_bodymass,          &
                                       perception_override_energy,            &
                                       perception_override_age,        &  ! integer
                                       perception_override_reprfac

    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! Local variables

    ! Local copies of **genotype x phenotype** boolean matrices.
    logical, dimension(MAX_NALLELES,N_CHROMOSOMES) ::                         &
                                        here_param_gp_matrix_light,           &
                                        here_param_gp_matrix_depth,           &
                                        here_param_gp_matrix_food_dir,        &
                                        here_param_gp_matrix_food_mem,        &
                                        here_param_gp_matrix_conspec,         &
                                        here_param_gp_matrix_pred_dir,        &
                                        here_param_gp_matrix_predator,        &
                                        here_param_gp_matrix_stomach,         &
                                        here_param_gp_matrix_bodymass,        &
                                        here_param_gp_matrix_energy,          &
                                        here_param_gp_matrix_age,             &
                                        here_param_gp_matrix_reprfac

    ! Local copies of the Gaussian perception error coefficients of variation.
    real(SRP) ::                        here_param_gerror_cv_light,           &
                                        here_param_gerror_cv_depth,           &
                                        here_param_gerror_cv_food_dir,        &
                                        here_param_gerror_cv_food_mem,        &
                                        here_param_gerror_cv_conspec,         &
                                        here_param_gerror_cv_pred_dir,        &
                                        here_param_gerror_cv_predator,        &
                                        here_param_gerror_cv_stomach,         &
                                        here_param_gerror_cv_bodymass,        &
                                        here_param_gerror_cv_energy,          &
                                        here_param_gerror_cv_age,             &
                                        here_param_gerror_cv_reprfac

    !- - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - - -
    !> ### Implementation notes ###
    !> We check input boolean G x P matrices and calculate the perceptual
    !! components of **this** motivation state only when the boolean matrix
    !! is provided as a parameter. Also check the corresponding variance/CV
    !! and reset to deterministic (variance zero) is not provided as a dummy
    !! parameter parameter.
    !! @warning There should be **exactly** as many `param_g_p_matrix_` and
    !!          `param_gerror_cv_` parameters as perceptual components
    !!          for this motivation (the_neurobio::percept_components_motiv).
    LIGHT: if (present(param_gp_matrix_light)) then
      here_param_gp_matrix_light = param_gp_matrix_light
      if (present(param_gerror_cv_light)) then
        here_param_gerror_cv_light = param_gerror_cv_light
      else
        here_param_gerror_cv_light = 0.0_SRP
      end if
      !> - calculate the perceptual component for **light**.
      if (present(perception_override_light)) then
        call this_agent%neuro_resp(                                           &
                  this_trait = this%light,                                    &
                  g_p_matrix = here_param_gp_matrix_light,                    &
                  init_val = perception_override_light,                       &
                  gerror_cv = here_param_gerror_cv_light )
      else
        call this_agent%neuro_resp(                                           &
                  this_trait = this%light,                                    &
                  g_p_matrix = here_param_gp_matrix_light,                    &
                  init_val = this_agent%perceive_light%get_current(),         &
                  gerror_cv = here_param_gerror_cv_light )
      end if
    end if LIGHT

    DEPTH: if (present(param_gp_matrix_depth)) then
      here_param_gp_matrix_depth = param_gp_matrix_depth
      if (present(param_gerror_cv_depth)) then
        here_param_gerror_cv_depth = param_gerror_cv_depth
      else
        here_param_gerror_cv_depth = 0.0_SRP
      end if
      !> - calculate the perceptual component for **depth**.
      if (present(perception_override_depth)) then
        call this_agent%neuro_resp(                                           &
                  this_trait = this%depth,                                    &
                  g_p_matrix = here_param_gp_matrix_depth,                    &
                  init_val = perception_override_depth,                       &
                  gerror_cv = here_param_gerror_cv_depth )
      else
        call this_agent%neuro_resp(                                           &
                  this_trait = this%depth,                                    &
                  g_p_matrix = here_param_gp_matrix_depth,                    &
                  init_val = this_agent%perceive_depth%get_current(),         &
                  gerror_cv = here_param_gerror_cv_depth )

      end if
    end if DEPTH

    FOOD_DIR: if (present(param_gp_matrix_food_dir)) then
      here_param_gp_matrix_food_dir = param_gp_matrix_food_dir
      if (present(param_gerror_cv_food_dir)) then
        here_param_gerror_cv_food_dir = param_gerror_cv_food_dir
      else
        here_param_gerror_cv_food_dir = 0.0_SRP
      end if
      !> - calculate the perceptual component for **food_dir**.
      if (present(perception_override_food_dir)) then
        call this_agent%neuro_resp(                                           &
                  this_trait = this%food_dir,                                 &
                  g_p_matrix = here_param_gp_matrix_food_dir,                 &
                  init_val = perception_override_food_dir,                    &
                  gerror_cv = here_param_gerror_cv_food_dir )
      else
        call this_agent%neuro_resp(                                           &
                  this_trait = this%food_dir,                                 &
                  g_p_matrix = here_param_gp_matrix_food_dir,                 &
                  init_val = real(this_agent%perceive_food%get_count(), SRP), &
                  gerror_cv = here_param_gerror_cv_food_dir )
      end if
    end if FOOD_DIR

    FOOD_MEM: if (present(param_gp_matrix_food_mem)) then
      here_param_gp_matrix_food_mem = param_gp_matrix_food_mem
      if (present(param_gerror_cv_food_mem)) then
        here_param_gerror_cv_food_mem = param_gerror_cv_food_mem
      else
        here_param_gerror_cv_food_mem = 0.0_SRP
      end if
      !> - calculate the perceptual component for **food_mem**.
      if (present(perception_override_food_mem)) then
        call this_agent%neuro_resp(                                           &
                this_trait = this%food_mem,                                   &
                g_p_matrix = here_param_gp_matrix_food_mem,                   &
                init_val = perception_override_food_mem,                      &
                gerror_cv = here_param_gerror_cv_food_mem )
      else
        call this_agent%neuro_resp(                                           &
                  this_trait = this%food_mem,                                 &
                  g_p_matrix = here_param_gp_matrix_food_mem,                 &
                  init_val = this_agent%memory_stack%get_food_mean_n(),       &
                  gerror_cv = here_param_gerror_cv_food_mem )
      end if
    end if FOOD_MEM

    CONSPEC: if (present(param_gp_matrix_conspec)) then
      here_param_gp_matrix_conspec = param_gp_matrix_conspec
      if (present(param_gerror_cv_conspec)) then
        here_param_gerror_cv_conspec = param_gerror_cv_conspec
      else
        here_param_gerror_cv_conspec = 0.0_SRP
      end if
      !> - calculate the perceptual component for **conspec**.
      if (present(perception_override_conspec)) then
        call this_agent%neuro_resp(                                           &
                  this_trait = this%conspec,                                  &
                  g_p_matrix = here_param_gp_matrix_conspec,                  &
                  init_val = perception_override_conspec,                     &
                  gerror_cv = here_param_gerror_cv_conspec )
      else
        call this_agent%neuro_resp(                                           &
                  this_trait = this%conspec,                                  &
                  g_p_matrix = here_param_gp_matrix_conspec,                  &
                  init_val = real(this_agent%perceive_consp%get_count(), SRP),&
                  gerror_cv = here_param_gerror_cv_conspec )

      end if
    end if CONSPEC

    PRED_DIR: if (present(param_gp_matrix_pred_dir)) then
      here_param_gp_matrix_pred_dir = param_gp_matrix_pred_dir
      if (present(param_gerror_cv_pred_dir)) then
        here_param_gerror_cv_pred_dir = param_gerror_cv_pred_dir
      else
        here_param_gerror_cv_pred_dir = 0.0_SRP
      end if
      !> - calculate the perceptual component for **direct predation**.
      if (present(perception_override_pred_dir)) then
        call this_agent%neuro_resp(                                           &
                  this_trait = this%pred_dir,                                 &
                  g_p_matrix = here_param_gp_matrix_pred_dir,                 &
                  init_val = perception_override_pred_dir,                    &
                  gerror_cv = here_param_gerror_cv_pred_dir )
      else
        call this_agent%neuro_resp(                                           &
                  this_trait = this%pred_dir,                                 &
                  g_p_matrix = here_param_gp_matrix_pred_dir,                 &
                  init_val = this_agent%risk_pred(),                          &
                  gerror_cv = here_param_gerror_cv_pred_dir )
      end if
    end if PRED_DIR

    PREDATOR: if (present(param_gp_matrix_predator)) then
      here_param_gp_matrix_predator = param_gp_matrix_predator
      if (present(param_gerror_cv_predator)) then
        here_param_gerror_cv_predator = param_gerror_cv_predator
      else
        here_param_gerror_cv_predator = 0.0_SRP
      end if
      !> - calculate the perceptual component for **predator**.
      if (present(perception_override_predator)) then
        call this_agent%neuro_resp(                                           &
                  this_trait = this%predator,                                 &
                  g_p_matrix = here_param_gp_matrix_predator,                 &
                  init_val = perception_override_predator,                    &
                  gerror_cv = here_param_gerror_cv_predator )
      else
        call this_agent%neuro_resp(                                           &
                  this_trait = this%predator,                                 &
                  g_p_matrix = here_param_gp_matrix_predator,                 &
                  init_val = this_agent%predation_risk(),                     &
                  gerror_cv = here_param_gerror_cv_predator )
      end if
    end if PREDATOR

    STOMACH: if (present(param_gp_matrix_stomach)) then
      here_param_gp_matrix_stomach = param_gp_matrix_stomach
      if (present(param_gerror_cv_stomach)) then
        here_param_gerror_cv_stomach = param_gerror_cv_stomach
      else
        here_param_gerror_cv_stomach = 0.0_SRP
      end if
      !> - calculate the perceptual component for **stomach**.
      if (present(perception_override_stomach)) then
        call this_agent%neuro_resp(                                           &
                  this_trait = this%stomach,                                  &
                  g_p_matrix = here_param_gp_matrix_stomach,                  &
                  init_val = perception_override_stomach,                     &
                  gerror_cv = here_param_gerror_cv_stomach )
      else
        call this_agent%neuro_resp(                                           &
                  this_trait = this%stomach,                                  &
                  g_p_matrix = here_param_gp_matrix_stomach,                  &
                  init_val = this_agent%perceive_stomach%get_available(),     &
                  gerror_cv = here_param_gerror_cv_stomach )
      end if
    end if STOMACH

    BODYMASS: if (present(param_gp_matrix_bodymass)) then
      here_param_gp_matrix_bodymass = param_gp_matrix_bodymass
      if (present(param_gerror_cv_bodymass)) then
        here_param_gerror_cv_bodymass = param_gerror_cv_bodymass
      else
        here_param_gerror_cv_bodymass = 0.0_SRP
      end if
      !> - calculate the perceptual component for **bodymass**.
      if (present(perception_override_bodymass)) then
        call this_agent%neuro_resp(                                           &
                  this_trait = this%bodymass,                                 &
                  g_p_matrix = here_param_gp_matrix_bodymass,                 &
                  init_val = perception_override_bodymass,                    &
                  gerror_cv = here_param_gerror_cv_bodymass )
      else
        call this_agent%neuro_resp(                                           &
                  this_trait = this%bodymass,                                 &
                  g_p_matrix = here_param_gp_matrix_bodymass,                 &
                  init_val = this_agent%perceive_body_mass%get_current(),     &
                  gerror_cv = here_param_gerror_cv_bodymass )
      end if
    end if BODYMASS

    ENERGY: if (present(param_gp_matrix_energy)) then
      here_param_gp_matrix_energy = param_gp_matrix_energy
      if (present(param_gerror_cv_energy)) then
        here_param_gerror_cv_energy = param_gerror_cv_energy
      else
        here_param_gerror_cv_energy = 0.0_SRP
      end if
      !> - calculate the perceptual component for **energy**.
      if (present(perception_override_energy)) then
        call this_agent%neuro_resp(                                           &
                  this_trait = this%energy,                                   &
                  g_p_matrix = here_param_gp_matrix_energy,                   &
                  init_val = perception_override_energy,                      &
                  gerror_cv = here_param_gerror_cv_energy )
      else
        call this_agent%neuro_resp(                                           &
                  this_trait = this%energy,                                   &
                  g_p_matrix = here_param_gp_matrix_energy,                   &
                  init_val = this_agent%perceive_energy%get_current(),        &
                  gerror_cv = here_param_gerror_cv_energy )
      end if
    end if ENERGY

    AGE: if (present(param_gp_matrix_age)) then
      here_param_gp_matrix_age = param_gp_matrix_age
      if (present(param_gerror_cv_age)) then
        here_param_gerror_cv_age = param_gerror_cv_age
      else
        here_param_gerror_cv_age = 0.0_SRP
      end if
      !> - calculate the perceptual component for **age**.
      if (present(perception_override_age)) then
        call this_agent%neuro_resp(                                           &
                  this_trait = this%age,                                      &
                  g_p_matrix = here_param_gp_matrix_age,                      &
                  init_val = perception_override_age,                         &
                  gerror_cv = here_param_gerror_cv_age )
      else
        call this_agent%neuro_resp(                                           &
                  this_trait = this%age,                                      &
                  g_p_matrix = here_param_gp_matrix_age,                      &
                  init_val = real(this_agent%perceive_age%get_current(), SRP),&
                  gerror_cv = here_param_gerror_cv_age )
      end if
    end if AGE

    REPRFAC: if (present(param_gp_matrix_reprfac)) then
      here_param_gp_matrix_reprfac = param_gp_matrix_reprfac
      if (present(param_gerror_cv_reprfac)) then
        here_param_gerror_cv_reprfac = param_gerror_cv_reprfac
      else
        here_param_gerror_cv_reprfac = 0.0_SRP
      end if
      !> - calculate the perceptual component for **reproductive factor**.
      !! .
      if (present(perception_override_reprfac)) then
        call this_agent%neuro_resp(                                           &
                  this_trait = this%reprfac,                                  &
                  g_p_matrix = here_param_gp_matrix_reprfac,                  &
                  init_val = perception_override_reprfac,                     &
                  gerror_cv = here_param_gerror_cv_reprfac )
      else
        call this_agent%neuro_resp(                                           &
                  this_trait = this%reprfac,                                  &
                  g_p_matrix = here_param_gp_matrix_reprfac,                  &
                  init_val = this_agent%perceive_reprfac%get_current(),       &
                  gerror_cv = here_param_gerror_cv_reprfac )
      end if
    end if REPRFAC


  end subroutine perception_components_neuronal_response_calculate

  !-----------------------------------------------------------------------------
  !> Standard "get" function for the state neuronal **light** effect
  !! component.
  elemental function state_motivation_light_get(this) result (value_get)
    class(STATE_MOTIVATION_BASE), intent(in) :: this
    !> @returns function value: light
    real(SRP) :: value_get

    value_get = this%percept_component%light

  end function state_motivation_light_get

  !-----------------------------------------------------------------------------
  !> Standard "get" function for the state neuronal **depth** effect
  !! component.
  elemental function state_motivation_depth_get(this) result (value_get)
    class(STATE_MOTIVATION_BASE), intent(in) :: this
    !> @returns function value: depth
    real(SRP) :: value_get

    value_get = this%percept_component%depth

  end function state_motivation_depth_get

  !-----------------------------------------------------------------------------
  !> Standard "get" function for the state neuronal **directly seen
  !! food** effect component.
  elemental function state_motivation_food_dir_get(this) result (value_get)
    class(STATE_MOTIVATION_BASE), intent(in) :: this
    !> @returns function value: food
    real(SRP) :: value_get

    value_get = this%percept_component%food_dir

  end function state_motivation_food_dir_get

  !-----------------------------------------------------------------------------
  !> Standard "get" function for the state neuronal **food items from
  !! past memory** effect component.
  elemental function state_motivation_food_mem_get(this) result (value_get)
    class(STATE_MOTIVATION_BASE), intent(in) :: this
    !> @returns function value: food
    real(SRP) :: value_get

    value_get = this%percept_component%food_mem

  end function state_motivation_food_mem_get

  !-----------------------------------------------------------------------------
  !> Standard "get" function for the state neuronal **conspecifics**
  !! effect component.
  elemental function state_motivation_conspec_get(this) result (value_get)
    class(STATE_MOTIVATION_BASE), intent(in) :: this
    !> @returns function value: conspecifics
    real(SRP) :: value_get

    value_get = this%percept_component%conspec

  end function state_motivation_conspec_get

  !-----------------------------------------------------------------------------
  !> Standard "get" function for the state neuronal **direct predation**
  !! effect component.
  elemental function state_motivation_pred_dir_get(this) result (value_get)
    class(STATE_MOTIVATION_BASE), intent(in) :: this
    !> @returns function value: predators
    real(SRP) :: value_get

    value_get = this%percept_component%pred_dir

  end function state_motivation_pred_dir_get

  !-----------------------------------------------------------------------------
  !> Standard "get" function for the state neuronal **predators**
  !! effect component.
  elemental function state_motivation_predator_get(this) result (value_get)
    class(STATE_MOTIVATION_BASE), intent(in) :: this
    !> @returns function value: predators
    real(SRP) :: value_get

    value_get = this%percept_component%predator

  end function state_motivation_predator_get

  !-----------------------------------------------------------------------------
  !> Standard "get" function for the state neuronal **stomach**
  !! effect component.
  elemental function state_motivation_stomach_get(this) result (value_get)
    class(STATE_MOTIVATION_BASE), intent(in) :: this
    !> @returns function value: stomach
    real(SRP) :: value_get

    value_get = this%percept_component%stomach

  end function state_motivation_stomach_get

  !-----------------------------------------------------------------------------
  !> Standard "get" function for the state neuronal **body mass**
  !! effect component.
  elemental function state_motivation_bodymass_get(this) result (value_get)
    class(STATE_MOTIVATION_BASE), intent(in) :: this
    !> @returns function value: body mass
    real(SRP) :: value_get

    value_get = this%percept_component%bodymass

  end function state_motivation_bodymass_get

  !-----------------------------------------------------------------------------
  !> Standard "get" function for the state neuronal **energy reserves**
  !! effect component.
  elemental function state_motivation_energy_get(this) result (value_get)
    class(STATE_MOTIVATION_BASE), intent(in) :: this
    !> @returns function value: energy reserves
    real(SRP) :: value_get

    value_get = this%percept_component%energy

  end function state_motivation_energy_get

  !-----------------------------------------------------------------------------
  !> Standard "get" function for the state neuronal **age**
  !! effect component.
  elemental function state_motivation_age_get(this) result (value_get)
    class(STATE_MOTIVATION_BASE), intent(in) :: this
    !> @returns function value: age
    real(SRP) :: value_get

    value_get = this%percept_component%age

  end function state_motivation_age_get

  !-----------------------------------------------------------------------------
  !> Standard "get" function for the state neuronal **reproductive factor**
  !! effect component.
  elemental function state_motivation_reprfac_get(this) result (value_get)
    class(STATE_MOTIVATION_BASE), intent(in) :: this
    !> @returns function value: age
    real(SRP) :: value_get

    value_get = this%percept_component%reprfac

  end function state_motivation_reprfac_get

  !-----------------------------------------------------------------------------
  !> Standard "get" function for the root state, get the overall
  !! **primary motivation value** (before modulation).
  elemental function state_motivation_motivation_prim_get(this) result (value_get)
    class(STATE_MOTIVATION_BASE), intent(in) :: this
    !> @returns function value: age
    real(SRP) :: value_get

    value_get = this%motivation_prim

  end function state_motivation_motivation_prim_get

  !-----------------------------------------------------------------------------
  !> Standard "get" function for the root state, get the overall
  !! **final motivation value** (after modulation).
  elemental function state_motivation_motivation_get(this) result (value_get)
    class(STATE_MOTIVATION_BASE), intent(in) :: this
    !> @returns function value: age
    real(SRP) :: value_get

    value_get = this%motivation_finl

  end function state_motivation_motivation_get

  !-----------------------------------------------------------------------------
  !> Check if the root state is the dominant state in GOS.
  elemental function state_motivation_is_dominant_get(this) result (gos_dominant)
    class(STATE_MOTIVATION_BASE), intent(in) :: this

    !> @returns TRUE if this motivational state is **dominant** in the GOS,
    !!          and FALSE otherwise.
    logical :: gos_dominant

    gos_dominant = this%dominant_state

  end function state_motivation_is_dominant_get

  !-----------------------------------------------------------------------------
  !> Get the fixed label for this motivational state. Note that the label
  !! is fixed and cannot be changed.
  elemental function state_motivation_fixed_label_get(this) result (label_get)
    class(STATE_MOTIVATION_BASE), intent(in) :: this

    !> @returns Returns the fixed label for this motivation state.
    character(len=LABEL_LENGTH) :: label_get

    label_get = this%label

  end function state_motivation_fixed_label_get

  !-----------------------------------------------------------------------------
  !> Transfer attention weights between two motivation state components.
  !! The main use of this subroutine would be to transfer attention from the
  !! actor agent's main motivation's attention component to the behaviour's GOS
  !! expectancy object.
  !! @note Note that the procedure `behaviour_root_attention_weights_transfer`
  !!       which does this is not using this procedure and transfers objects
  !!       directly.
  pure subroutine state_motivation_attention_weights_transfer (this, copy_from)
    class(STATE_MOTIVATION_BASE), intent(inout) :: this
    class(STATE_MOTIVATION_BASE), intent(in) :: copy_from

    associate ( TO_THIS => this%attention_weight,                             &
                FROM_THIS => copy_from%attention_weight  )
      TO_THIS%light =    FROM_THIS%light
      TO_THIS%depth =    FROM_THIS%depth
      TO_THIS%food_dir = FROM_THIS%food_dir
      TO_THIS%food_mem = FROM_THIS%food_mem
      TO_THIS%conspec =  FROM_THIS%conspec
      TO_THIS%pred_dir = FROM_THIS%pred_dir
      TO_THIS%predator = FROM_THIS%predator
      TO_THIS%stomach =  FROM_THIS%stomach
      TO_THIS%bodymass = FROM_THIS%bodymass
      TO_THIS%energy =   FROM_THIS%energy
      TO_THIS%age =      FROM_THIS%age
      TO_THIS%reprfac =  FROM_THIS%reprfac
    end associate

  end subroutine state_motivation_attention_weights_transfer

  !-----------------------------------------------------------------------------
  !> Calculate the **maximum** value over all the perceptual components.
  elemental function perception_component_maxval(this) result (maxvalue)
    class(PERCEPT_COMPONENTS_MOTIV), intent(in) :: this

    !> @return the maximum value among all the perceptual components.
    real(SRP) :: maxvalue

    maxvalue = maxval( [                                                      &
                          this%light,                                         &
                          this%depth,                                         &
                          this%food_dir,                                      &
                          this%food_mem,                                      &
                          this%conspec,                                       &
                          this%pred_dir,                                      &
                          this%predator,                                      &
                          this%stomach,                                       &
                          this%bodymass,                                      &
                          this%energy,                                        &
                          this%age,                                           &
                          this%reprfac ] )

  end function perception_component_maxval

  !-----------------------------------------------------------------------------
  !> Calculate the **maximum** value over all the perceptual components of
  !! this motivational state component.
  !! @note Used in `motivation_primary_calc` procedure.
  elemental function state_motivation_percept_maxval(this) result (maxvalue)
    class(STATE_MOTIVATION_BASE), intent(in) :: this

    !> @return the maximum value among all the perceptual components.
    real(SRP) :: maxvalue

    maxvalue = this%percept_component%max_value()

  end function state_motivation_percept_maxval

  !-----------------------------------------------------------------------------
  !> Calculate the level of **primary motivation** for this **specific**
  !! emotional state **component**.
  !! @note Used in `motivation_primary_calc` procedure.
  elemental function state_motivation_calculate_prim(this, maxvalue)          &
                                                        result (motivation_prim)
    class(STATE_MOTIVATION_BASE), intent(in) :: this

    !> The maximum value across all appraisal perception components, needed
    !! to standardise and rescale the latter to the range 0:1 before they
    !! are summed up.
    real(SRP), optional, intent(in) :: maxvalue

    !> @return The value of the primary motivation for this
    !!         motivation component.
    real(SRP) :: motivation_prim

    !> Local parameters defining 0.0 and 1.0 for rescale.
    real(SRP), parameter :: P0 = 0.0_SRP, P1 = 1.0_SRP

    !> Local copy of optional `maxvalue`.
    real(SRP) :: maxvalue_here

    CHECK_MAX_OPTNL: if (present(maxvalue)) then
      maxvalue_here = maxvalue
    else CHECK_MAX_OPTNL
      !> Normally we **rescale** all values within the perceptual motivation
      !! components coming from the appraisal level into a [0..1] range within
      !! the agent, so that they are comparable across the motivations. To do
      !! this we need the maximum perception value **over all perception
      !! objects**: `maxvalue`. Normally `maxvalue` is an input parameter
      !! taking account of all motivation all state components. But if it is
      !! not provided, we calculate local maximum for **this** motivational
      !! component only.
      maxvalue_here = this%max_perception()
    end if CHECK_MAX_OPTNL

    !> Calculate the primary motivation for this motivational state by summing
    !! up (**averaging**) all the perceptual components for this motivation
    !! weighted by their respective attention weights; components are
    !! **rescaled* from the potential global range 0:maxvalue to the range 0:1.
    !! @note `maxvalue` should normally be the maximum value for **all**
    !!        available motivation states, not just this.
    !! TODO: make maxvalue a structure reflecting motivational components.
    motivation_prim = average( [                                              &
        rescale(this%percept_component%light,    P0, maxvalue_here, P0, P1) * &
                this%attention_weight%light,    &

        rescale(this%percept_component%depth,    P0, maxvalue_here, P0, P1) * &
                this%attention_weight%depth,    &

        rescale(this%percept_component%food_dir, P0, maxvalue_here, P0, P1) * &
                this%attention_weight%food_dir, &

        rescale(this%percept_component%food_mem, P0, maxvalue_here, P0, P1) * &
                this%attention_weight%food_mem, &

        rescale(this%percept_component%conspec,  P0, maxvalue_here, P0, P1) * &
                this%attention_weight%conspec,  &

        rescale(this%percept_component%pred_dir, P0, maxvalue_here, P0, P1) * &
                this%attention_weight%pred_dir, &

        rescale(this%percept_component%predator, P0, maxvalue_here, P0, P1) * &
                this%attention_weight%predator, &

        rescale(this%percept_component%stomach,  P0, maxvalue_here, P0, P1) * &
                this%attention_weight%stomach,  &

        rescale(this%percept_component%bodymass, P0, maxvalue_here, P0, P1) * &
                this%attention_weight%bodymass, &

        rescale(this%percept_component%energy,   P0, maxvalue_here, P0, P1) * &
                this%attention_weight%energy,   &

        rescale(this%percept_component%age,      P0, maxvalue_here, P0, P1) * &
                this%attention_weight%age,      &

        rescale(this%percept_component%reprfac,  P0, maxvalue_here, P0, P1) * &
                this%attention_weight%reprfac  ] )

  end function state_motivation_calculate_prim

  !.............................................................................

  !-----------------------------------------------------------------------------
  !> Initialise perception components for a motivation state object.
  elemental subroutine perception_component_motivation_init_zero(this)
    class(PERCEPT_COMPONENTS_MOTIV), intent(inout) :: this

    this%light    = MISSING
    this%depth    = MISSING
    this%food_dir = MISSING
    this%food_mem = MISSING
    this%conspec  = MISSING
    this%pred_dir = MISSING
    this%predator = MISSING
    this%stomach  = MISSING
    this%bodymass = MISSING
    this%energy   = MISSING
    this%age      = MISSING
    this%reprfac  = MISSING

  end subroutine perception_component_motivation_init_zero

  !-----------------------------------------------------------------------------
  !> Init and cleanup **hunger** motivation object. The only difference from
  !! the base root STATE_MOTIVATION_BASE is that it sets unique label.
  elemental subroutine state_hunger_zero(this)
    class(STATE_HUNGER), intent(inout) :: this

    this%label    = "HUNGER"

    call this%percept_component%init()

    call this%attention_weight%attention_init(                                &
                  weight_light    = ATTENTION_SWITCH_HUNGER_LIGHT,            &
                  weight_depth    = ATTENTION_SWITCH_HUNGER_DEPTH,            &
                  weight_food_dir = ATTENTION_SWITCH_HUNGER_FOOD_DIR,         &
                  weight_food_mem = ATTENTION_SWITCH_HUNGER_FOOD_MEM,         &
                  weight_conspec  = ATTENTION_SWITCH_HUNGER_CONSPEC,          &
                  weight_pred_dir = ATTENTION_SWITCH_HUNGER_PRED_DIR,         &
                  weight_predator = ATTENTION_SWITCH_HUNGER_PREDATOR,         &
                  weight_stomach  = ATTENTION_SWITCH_HUNGER_STOMACH,          &
                  weight_bodymass = ATTENTION_SWITCH_HUNGER_BODYMASS,         &
                  weight_energy   = ATTENTION_SWITCH_HUNGER_ENERGY,           &
                  weight_age      = ATTENTION_SWITCH_HUNGER_AGE,              &
                  weight_reprfac  = ATTENTION_SWITCH_HUNGER_REPRFAC )

    this%motivation_prim = MISSING
    this%motivation_finl = MISSING

    this%dominant_state = .FALSE.

  end subroutine state_hunger_zero

  !-----------------------------------------------------------------------------
  !> Init and cleanup **fear state** motivation object. The only
  !! difference from the base root STATE_MOTIVATION_BASE is that it sets
  !! unique label.
  elemental subroutine state_fear_defence_zero(this)
    class(STATE_FEAR_DEFENCE), intent(inout) :: this

    this%label    = "ACTIVE_AVOID"

    call this%percept_component%init()

    call this%attention_weight%attention_init(                                &
                  weight_light    = ATTENTION_SWITCH_AVOID_ACT_LIGHT,         &
                  weight_depth    = ATTENTION_SWITCH_AVOID_ACT_DEPTH,         &
                  weight_food_dir = ATTENTION_SWITCH_AVOID_ACT_FOOD_DIR,      &
                  weight_food_mem = ATTENTION_SWITCH_AVOID_ACT_FOOD_MEM,      &
                  weight_conspec  = ATTENTION_SWITCH_AVOID_ACT_CONSPEC,       &
                  weight_pred_dir = ATTENTION_SWITCH_AVOID_ACT_PRED_DIR,      &
                  weight_predator = ATTENTION_SWITCH_AVOID_ACT_PREDATOR,      &
                  weight_stomach  = ATTENTION_SWITCH_AVOID_ACT_STOMACH,       &
                  weight_bodymass = ATTENTION_SWITCH_AVOID_ACT_BODYMASS,      &
                  weight_energy   = ATTENTION_SWITCH_AVOID_ACT_ENERGY,        &
                  weight_age      = ATTENTION_SWITCH_AVOID_ACT_AGE,           &
                  weight_reprfac  = ATTENTION_SWITCH_AVOID_ACT_REPRFAC )

    this%motivation_prim = MISSING
    this%motivation_finl = MISSING

    this%dominant_state = .FALSE.

  end subroutine state_fear_defence_zero

  !-----------------------------------------------------------------------------
  !> Init and cleanup **reproductive** motivation object. The only
  !! difference from the base root STATE_MOTIVATION_BASE is that it sets
  !! unique label.
  elemental subroutine state_reproduce_zero(this)
    class(STATE_REPRODUCE), intent(inout) :: this

    this%label    = "REPRODUCTION"

    call this%percept_component%init()

    call this%attention_weight%attention_init(                                &
                  weight_light    = ATTENTION_SWITCH_REPRODUCE_LIGHT,         &
                  weight_depth    = ATTENTION_SWITCH_REPRODUCE_DEPTH,         &
                  weight_food_dir = ATTENTION_SWITCH_REPRODUCE_FOOD_DIR,      &
                  weight_food_mem = ATTENTION_SWITCH_REPRODUCE_FOOD_MEM,      &
                  weight_conspec  = ATTENTION_SWITCH_REPRODUCE_CONSPEC,       &
                  weight_pred_dir = ATTENTION_SWITCH_REPRODUCE_PRED_DIR,      &
                  weight_predator = ATTENTION_SWITCH_REPRODUCE_PREDATOR,      &
                  weight_stomach  = ATTENTION_SWITCH_REPRODUCE_STOMACH,       &
                  weight_bodymass = ATTENTION_SWITCH_REPRODUCE_BODYMASS,      &
                  weight_energy   = ATTENTION_SWITCH_REPRODUCE_ENERGY,        &
                  weight_age      = ATTENTION_SWITCH_REPRODUCE_AGE,           &
                  weight_reprfac  = ATTENTION_SWITCH_REPRODUCE_REPRFAC )

    this%motivation_prim = MISSING
    this%motivation_finl = MISSING

    this%dominant_state = .FALSE.

  end subroutine state_reproduce_zero

  !-----------------------------------------------------------------------------
  !> Init the expectancy components to a zero state.
  elemental subroutine motivation_init_all_zero(this)
    class(MOTIVATION), intent(inout) :: this

    !> Expectancy components.
    call this%hunger%clean_init()
    call this%fear_defence%clean_init()
    call this%reproduction%clean_init()

    !> Also set the private and fixed "number of motivational states"
    !! constant, we obviously have 3 motivations.
    this%number_of_states = 3

  end subroutine motivation_init_all_zero

  !-----------------------------------------------------------------------------
  !> Reset all GOS indicators for this motivation object.
  elemental subroutine motivation_reset_gos_indicators(this)
    class(MOTIVATION), intent(inout) :: this

     !> Reset dominant status to FALSE for all motivational states.
    this%hunger%dominant_state = .FALSE.
    this%fear_defence%dominant_state = .FALSE.
    this%reproduction%dominant_state = .FALSE.

  end subroutine motivation_reset_gos_indicators

  !-----------------------------------------------------------------------------
  !> Calculate maximum value of the perception components across all
  !! motivations.
  elemental function motivation_max_perception_calc(this)                     &
                                                        result (max_motivation)
    class(MOTIVATION), intent(in) :: this

    !> @returns Returns the maximum value of the perception components
    !!          across all motivations.
    real(SRP) :: max_motivation

    max_motivation =  maxval([  this%hunger%max_perception(),                 &
                                this%fear_defence%max_perception(),           &
                                this%reproduction%max_perception() ])

  end function motivation_max_perception_calc

  !-----------------------------------------------------------------------------
  !> Return the vector of final motivation values for all motivational
  !! state components.
  pure function motivation_return_final_as_vector(this)                       &
                                                      result (final_vals_vector)
    class(MOTIVATION), intent(in) :: this
    real(SRP), allocatable, dimension(:) :: final_vals_vector

    final_vals_vector = [ this%hunger%motivation_finl,                        &
                          this%fear_defence%motivation_finl,                  &
                          this%reproduction%motivation_finl ]

  end function motivation_return_final_as_vector

  !-----------------------------------------------------------------------------
  !> Calculate the maximum value of the final motivations across all
  !! motivational state components.
  elemental function motivation_maximum_value_motivation_finl(this)           &
                                                              result (maxvalue)
    class(MOTIVATION), intent(in) :: this !< @param[in] this self
    real(SRP) :: maxvalue                 !< @returns Maximum final motivation.
    !> An equivalent "manual" form not using `finals` function :
    !! maxvalue = maxval( [    this%hunger%motivation_finl,                   &
    !!                         this%fear_defence%motivation_finl,             &
    !!                         this%reproduction%motivation_finl   ] )
    maxvalue = maxval( this%finals() )

  end function motivation_maximum_value_motivation_finl

  !-----------------------------------------------------------------------------
  !> Checks if the test value is the maximum **final** motivation value
  !! across all motivational state components.
  !! @note This is a scalar form, inputs a single scalar value for testing.
  elemental function motivation_val_is_maximum_value_motivation_finl (        &
                                          this, test_value)  result(is_maximum)
    class(MOTIVATION), intent(in) :: this
    real(SRP), intent(in) :: test_value
    logical :: is_maximum

     !  An equivalent "manual" form not using `finals` function :
     !  if ( is_maxval(test_value, [ this%hunger%motivation_finl,             &
     !                               this%fear_defence%motivation_finl,       &
     !                               this%reproduction%motivation_finl ]) )   &
     !     is_maximum = .TRUE.
    if ( is_maxval( test_value, this%finals() ) ) is_maximum = .TRUE.

  end function motivation_val_is_maximum_value_motivation_finl

  !-----------------------------------------------------------------------------
  !> Checks if the test value is the maximum **final** motivation value
  !! across all motivational state components.
  !! @note This is object form, inputs a whole motivation state object
  elemental function motivation_val_is_maximum_value_motivation_finl_o (      &
                                      this, test_motivation) result(is_maximum)
    class(MOTIVATION), intent(in) :: this
    class(STATE_MOTIVATION_BASE), intent(in) :: test_motivation
    logical :: is_maximum

    is_maximum = .FALSE.

    if ( is_maxval( test_motivation%motivation_value(), this%finals() ) )     &
            is_maximum = .TRUE.

  end function motivation_val_is_maximum_value_motivation_finl_o

  !-----------------------------------------------------------------------------
  !> Calculate the **primary motivations** from motivation-specific perception
  !! appraisal components. The **primary motivations** are motivation values
  !! before the modulation takes place.
  !  @note Note that we use array intrinsic functions `maxval` and `sum` here,
  !        combining arrays by array constructors using square brackets.
  elemental subroutine motivation_primary_sum_components(this, max_val)
    class(MOTIVATION), intent(inout) :: this

    !> @param[in] max_val optional parameter that sets the maximum perception
    !!            value for rescaling all perceptions to a common currency.
    !!            @note  Needed to standardise and rescale the appraisal
    !!                   perception components to the range 0:1 before they
    !!                   are summed up.
    !             TODO: set from maximum perceptual memory
    real(SRP), optional, intent(in) :: max_val

    ! Local copy of `max_val`, if the latter is absent, set to the maximum
    ! value across all appraisal perception components, needed to standardise
    ! and rescale the latter to the range 0:1 before they are summed up.
    real(SRP) :: appmaxval

    !> ### Implementation notes ###
    !> - Rescale all values within the perceptual motivation components coming
    !!   from the appraisal level into a [0..1] range within the agent, so
    !!   that they are comparable across the motivations. To do this we need
    !!   the maximum perception value overall perception objects: `appmaxval`.
    if (present(max_val)) then
      !>   - If the maximum rescale perception is provided as a parameter,
      !!     use it.
      appmaxval = max_val
    else
      !>   - If the parameter value is not provided, calculate maximum rescale
      !!     perceptions from the currently available perception components
      !!     of each specific motivational state.
      !!   .
      appmaxval = this%max_perception()
    end if

    !> - Calculate each of the motivations by summing up all state
    !!   perceptual components for a particular motivation that are
    !!   rescaled from the potential global range 0:appmaxval to the
    !!   range 0:1. This is done calling the
    !!   the_neurobio::state_motivation_base::motivation_calculate() method
    !!   for each of the @ref aha_buildblocks_gp_motivations
    !!   "motivational states".
    !! .
    this%hunger%motivation_prim =                                             &
                        this%hunger%motivation_calculate(appmaxval)

    this%fear_defence%motivation_prim =                                       &
                        this%fear_defence%motivation_calculate(appmaxval)

    this%reproduction%motivation_prim =                                       &
                        this%reproduction%motivation_calculate(appmaxval)

  end subroutine motivation_primary_sum_components

  !-----------------------------------------------------------------------------
  !> Produce **modulation** of the primary motivations, that result in
  !! the **final motivation** values (`_finl`). In this subroutine,
  !! **modulation is absent**, so the final motivation values are equal
  !! to the primary motivations.
  elemental subroutine motivation_modulation_absent(this)
    class(MOTIVATION), intent(inout) :: this

    !> Here the final motivations are just equal to their primary values
    this%hunger%motivation_finl =                                             &
                                    this%hunger%motivation_prim

    this%fear_defence%motivation_finl =                                       &
                                    this%fear_defence%motivation_prim

    this%reproduction%motivation_finl =                                       &
                                    this%reproduction%motivation_prim

  end subroutine motivation_modulation_absent






















  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with APPRAISAL
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !! @note  We do not use **set** function for the `STATE_....` component
  !!        (e.g. use `this%hunger%light` directly instead of
  !!        `call this%hunger%set_light`) for brevity and simplicity.
  !!        Otherwise, a temporal object may be needed with the second
  !!        assignment in `this%hunger%set_...` procedure after the main
  !!        `this%trait_init`. It is reasonable because the state component
  !!        is within the same module so access granted. Unlike this neuronal
  !!        response procedure requiring `set`, `get`-functions may be used
  !!        in other, upwards,  modules, so raw data components might be
  !!        unaccessible there if declared `private`.
  !! @note  `trait_init` function takes care of the neural response
  !!        calculation from perception as well as the perception error
  !!        and genome.
  !! @note  Local rename `trait_init` to `neuro_resp`. May use different
  !!        `trait_init` procedures, e.g. direct rescale-like gene to phenotype
  !!        effects.

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with APPRAISAL - generating
  !                       PERCEPTUAL MOTIVATION COMPONENTS
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Initialise and cleanup all appraisal object components and sub-objects.
  elemental subroutine appraisal_init_zero_cleanup_all(this)
    class(APPRAISAL), intent(inout) :: this

    !> Init and clean all motivational components.
    call this%motivations%init()

    !> Also cleanup the emotional memory stack.
    call this%memory_motivations%memory_cleanup()

  end subroutine appraisal_init_zero_cleanup_all

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
  !! @note The `dies` method is implemented at the @ref gos_global to allow
  !!       "cleaning" of all neurobiological objects when `dies` is called
  !!       when performing the behaviours upwards in the object hierarchy.
  elemental subroutine appraisal_agent_set_dead(this)
    class(APPRAISAL), intent(inout) :: this

    call this%set_dead()          !> - Set the agent "dead";
    call this%init_reproduction() !> - emptify reproduction objects;
    call this%init_perception()   !> - emptify all neurobiological objects.
    call this%init_appraisal()    !> .

  end subroutine appraisal_agent_set_dead

  !-----------------------------------------------------------------------------
  !> Get the perceptual components of all motivational states by passing
  !! perceptions via the neuronal response function.
  !! @warning  Here we use the intent[inout] procedure that **does change** the
  !!           actor agent: sets the labels for the genes. This procedure,
  !!           therefore is used only for initialisation and not in prediction.
  subroutine appraisal_perceptual_comps_motiv_neur_response_calculate(this)
    class(APPRAISAL), intent(inout) :: this

    !> ### Implementation notes ###
    !! Call the neuronal response initialisation procedure
    !! the_neurobio::percept_components_motiv::motivation_components_init()
    !! for all the motivation states:
    !! - the_neurobio::state_hunger
    !! - the_neurobio::state_fear_defence
    !! - the_neurobio::state_reproduce
    !! .
    ! @note  **Appraisal** assessment for **hunger** motivation, using
    !        `PERCEPT_COMPONENTS_MOTIV`-bound procedure with **intent[inout]**
    !        for `this_agent` now.
    call this%motivations%hunger%percept_component%motivation_components_init &
      (this,                                                                  &
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
      ! Parameters :: labels for genes:
      param_gene_label_light   = "HUNGER_LIGHT",                              &
      param_gene_label_depth   = "HUNGER_DEPTH",                              &
      param_gene_label_food_dir= "HUNGER_FOODMEM",                            &
      param_gene_label_food_mem= "HUNGER_FOODMEM",                            &
      param_gene_label_conspec = "HUNGER_CONSP_N",                            &
      param_gene_label_pred_dir= "HUNGER_PREDDIR",                            &
      param_gene_label_predator= "HUNGER_PRED",                               &
      param_gene_label_stomach = "HUNGER_STOM",                               &
      param_gene_label_bodymass= "HUNGER_BODYMAS",                            &
      param_gene_label_energy  = "HUNGER_ENERGY",                             &
      param_gene_label_age     = "HUNGER_AGE",                                &
      param_gene_label_reprfac = "HUNGER_REPRFAC"  )

    ! @note  **Appraisal** assessment for **fear_defence** motivation,
    !        using `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[inout]
    !        for `this_agent` now.
    call this%motivations%fear_defence%percept_component%motivation_components_init   &
      (this,                                                                  &
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
      ! Parameters :: labels for genes:
      param_gene_label_light   = "AAVOID_LIGHT",                              &
      param_gene_label_depth   = "AAVOID_DEPTH",                              &
      param_gene_label_food_dir= "AAVOID_FOODMEM",                            &
      param_gene_label_food_mem= "AAVOID_FOODMEM",                            &
      param_gene_label_conspec = "AAVOID_CONSP_N",                            &
      param_gene_label_pred_dir= "AAVOID_PREDDIR",                            &
      param_gene_label_predator= "AAVOID_PRED",                               &
      param_gene_label_stomach = "AAVOID_STOM",                               &
      param_gene_label_bodymass= "AAVOID_BODYMAS",                            &
      param_gene_label_energy  = "AAVOID_ENERGY",                             &
      param_gene_label_age     = "AAVOID_AGE",                                &
      param_gene_label_reprfac = "AAVOID_REPRFAC"  )

    ! @note  **Appraisal** assessment for **reproduction** motivation, using
    !        `PERCEPT_COMPONENTS_MOTIV`-bound procedure with intent[inout] for
    !        `this_agent` now.
    call this%motivations%reproduction%percept_component%motivation_components_init &
      (this,                                                                  &
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
      param_gerror_cv_age      = AGE_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,   &
      param_gerror_cv_reprfac  = REPRFAC_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV,&
      ! Parameters :: labels for genes:
      param_gene_label_light   = "REPROD_LIGHT",                              &
      param_gene_label_depth   = "REPROD_DEPTH",                              &
      param_gene_label_food_dir= "REPROD_FOODMEM",                            &
      param_gene_label_food_mem= "REPROD_FOODMEM",                            &
      param_gene_label_conspec = "REPROD_CONSP_N",                            &
      param_gene_label_pred_dir= "REPROD_PREDDIR",                            &
      param_gene_label_predator= "REPROD_PRED",                               &
      param_gene_label_stomach = "REPROD_STOM",                               &
      param_gene_label_bodymass= "REPROD_BODYMAS",                            &
      param_gene_label_energy  = "REPROD_ENERGY",                             &
      param_gene_label_age     = "REPROD_AGE",                                &
      param_gene_label_reprfac = "REPROD_REPRFAC"  )

  end subroutine appraisal_perceptual_comps_motiv_neur_response_calculate

  !-----------------------------------------------------------------------------
  !> Calculate primary motivations from perceptual components of each
  !! motivation state.
  elemental subroutine appraisal_primary_motivations_calculate(this,          &
                                                        rescale_max_motivation)
    class(APPRAISAL), intent(inout) :: this

     !> @param[in] rescale_max_motivation maximum motivation value for
    !!            rescaling all motivational components for comparison
    !!            across all motivation and perceptual components and behaviour
    !!            units.
    real(SRP), optional, intent(in) :: rescale_max_motivation

    ! Local variables
    real(SRP) :: max_motivation !< Local max. over all motivation components.


    !> ### Implementation notes ###
    !> - Check if the maximum motivation value for rescale is provided as
    !! a parameter.
    if (present(rescale_max_motivation)) then
      !>   - Check if global maximum motivation across all behaviours
      !!     and perceptual components is provided for rescaling.
      max_motivation = rescale_max_motivation
    else
      !>   - If not, use local maximum value for this behaviour only.
      !!   .
      !! .
      max_motivation =  this%motivations%max_perception()
    end if

    !> Finally, the primary motivation values are calculated using the
    !! the_neurobio::motivation::motivation_primary_calc() method.
    call this%motivations%motivation_primary_calc(max_motivation)

  end subroutine appraisal_primary_motivations_calculate

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with generation of the MOTIVATION states (SUMMATOR).
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Produce **modulation** of the primary motivations, that result in
  !! the **final motivation** values (`_finl`). Modulation here is non-genetic
  !! and involves a fixed transformation of the primary motivation values.
  subroutine appraisal_motivation_modulation_non_genetic(this,                &
                                                         no_modulation)
    class(APPRAISAL), intent(inout) :: this
    !> @param[in] no_genetic_modulation chooses if genetic modulation is
    !!            calculated at all, if set to TRUE, then genetic modulation
    !!            is **not** calculated and the final motivational values
    !!            are just equal to the primary motivations.
    logical, optional, intent(in) :: no_modulation

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter ::                                            &
                    PROCNAME = "(appraisal_motivation_modulation_non_genetic)"

    real(SRP) :: weight_reprfac

    !> ### Notable variables and parameters ###
    !> - `AGE_ARRAY_ABSCISSA` is the interpolation grid abscissa for the
    !!    weighting factor applied to reproductive motivation. It is defined
    !!    by the parameter commondata::reprod_modulation_devel_abscissa.
    real(SRP), parameter, dimension(*) :: AGE_ARRAY_ABSCISSA =                &
                                          REPROD_MODULATION_DEVEL_ABSCISSA

    !> - `AGE_ARRAY_ORDINATE` is the interpolation grid ordinate. Its first
    !!    and last values are set as 0.0 and 1.0, and the middle is defined by
    !!    the parameter commondata::reprod_modulation_devel_w2.
    !!    @verbatim
    !!      htintrpl.exe [ 7000, 8555, 11666 ] [ 0, 0.10, 1.0 ]
    !!    @endverbatim
    !! .
    real(SRP), parameter, dimension(*) :: AGE_ARRAY_ORDINATE =                &
                          [ ZERO, REPROD_MODULATION_DEVEL_W2, 1.0_SRP ]

    !> ### Implementation notes ###
    !> First, *initialise* the **final motivation** values from the
    !! **no modulation** method the_neurobio::motivation::modulation_none().
    call this%motivations%modulation_none()

    !> Then check if developmental or genetic (or any other) modulation is
    !! disabled by the parameters commondata::modulation_appraisal_disable_all.
    if ( MODULATION_APPRAISAL_DISABLE_ALL ) return

    !> Check if `no_genetic_modulation` parameter is set to TRUE and if
    !! yes, return without no further processing.
    if (present(no_modulation)) then
      if ( no_modulation ) return     ! no further processing.
    end if

    !+ + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
    ! Explicit non-genetic modulation starts below:

    !> #### Developmental modulation of reproductive factor ####
    !> Reproductive factor the_hormones::hormones::reproductive_factor() is
    !! accumulated by the sex hormone level whenever the agent is growing.
    !! Such accumulation can increase motivation for reproduction. However,
    !! reproduction is not possible in young and small agents. Therefore,
    !! this procedure implements a developmental modulation of the
    !! reproductive factor: reproductive motivation
    !! the_neurobio::state_reproduce is weighted out while the agent does
    !! not reach a target body length and age. This weighting is defined by
    !! nonlinear interpolation using the abscissa array `AGE_ARRAY_ABSCISSA`
    !! and ordinate `AGE_ARRAY_ORDINATE`. Such weighting, thus, allows
    !! non-zero reproductive motivation only when the agent reaches the age
    !! exceeding the first abscissa value `AGE_ARRAY_ABSCISSA`, age > *L/2*,
    !! as here the weighting factor exceeds zero. Furthermore, when the
    !! age of the agent exceeds the last  value of `AGE_ARRAY_ABSCISSA`,
    !! the weighting factor is equal to 1.0, so reproductive motivation is
    !! not limited any more.
    weight_reprfac                                                            &
      = within( DDPINTERPOL( AGE_ARRAY_ABSCISSA, AGE_ARRAY_ORDINATE,          &
                             real(this%get_age(), SRP) ),                     &
                ZERO, 1.0_SRP )

    !> Interpolation plots can be saved in the @ref intro_debug_mode
    !! "debug mode" using this plotting command:
    !! commondata::debug_interpolate_plot_save().
    !  @warning Calling `debug_interpolate_plot_save` here precludes making
    !           this function **pure** and **elemental**. Comment-out upon
    !           full testing and debugging!
    call debug_interpolate_plot_save(                                         &
            grid_xx=AGE_ARRAY_ABSCISSA, grid_yy=AGE_ARRAY_ORDINATE,           &
            ipol_value=weight_reprfac,                                        &
            algstr="DDPINTERPOL",                                             &
            output_file="plot_debug_reproduction_modulal_ageweight_" //       &
                    TOSTR(Global_Time_Step_Model_Current) //                  &
                    MMDD // "_a_"// trim(this%individ_label()) // "_" //      &
                    RAND_STRING(LABEL_LENGTH, LABEL_CST,LABEL_CEN) // PS )

    this%motivations%reproduction%motivation_finl =                           &
      this%motivations%reproduction%motivation_prim * weight_reprfac

  end subroutine appraisal_motivation_modulation_non_genetic

  !-----------------------------------------------------------------------------
  !> Produce **modulation** of the primary motivations, that result in
  !! the **final motivation** values (`_finl`). Modulation involves
  !! effects of such characteristics of the agent as body mass and age on
  !! the primary motivations (hunger, active and passive avoidance and
  !! reproduction) mediated by the genome effects. Here the genome determines
  !! the coefficients that set the degree of the influence of the agent's
  !! characteristics on the motivations.
  subroutine appraisal_motivation_modulation_genetic (this,                   &
                                                          no_genetic_modulation)
    class(APPRAISAL), intent(inout) :: this
    !> @param[in] no_genetic_modulation chooses if genetic modulation is
    !!            calculated at all, if set to TRUE, then genetic modulation
    !!            is **not** calculated and the final motivational values
    !!            are just equal to the primary motivations.
    logical, optional, intent(in) :: no_genetic_modulation

    ! Genetically determined **gamma** modulation coefficient that mediates
    ! the effect of sex on reproductive motivation.
    real(SRP) :: modulation_gamma

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME =                                 &
                                    "(appraisal_motivation_modulation_genetic)"

    !> ### Implementation notes ###
    !> First, *initialise* the **final motivation** values from the
    !! **no modulation** method the_neurobio::motivation::modulation_none().
    call this%motivations%modulation_none()

    !> Then check if developmental or genetic (or any other) modulation is
    !! disabled by the parameters commondata::modulation_appraisal_disable_all.
    if ( MODULATION_APPRAISAL_DISABLE_ALL ) return

    !> Check if `no_genetic_modulation` parameter is set to TRUE and if
    !! yes, return without no further processing.
    if (present(no_genetic_modulation)) then
      if ( no_genetic_modulation ) return     ! no further processing.
    end if

    !+ + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
    ! Explicit modulation starts below:

    !+ + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
    !> Sex modulation of the **reproduction** motivation state for **male**.
    !  @warning This code uses the `block` construct from the F2008 standard,
    !          which may not be supported by all compilers (e.g. ifort v 13
    !          doesn't support it). It is here only for convenience and can be
    !          commented-out if unsupported (but don't forget to disable the
    !          `end block` too).
    MALE_REPROD: block

      !> - First, use the sigmoid function and genome to set the phenotypic
      !!   value of the **gamma** modulation coefficient that mediates the
      !!   effect of the agent's sex on reproductive motivation.
      call this%trait_init(                                                   &
                    this_trait = modulation_gamma,                            &
                    g_p_matrix = SEX_MALE_MODULATION_REPRODUCE_GENOTYPE,      &
                    init_val = this%motivations%reproduction%motivation_prim, &
                    gerror_cv = SEX_MALE_MODULATION_REPRODUCE_GERROR_CV,      &
                    label = "M_MALE_REPRO" )

      !> - Second, add the modulation factor to the actual motivation value.
      !!   If the agent is male, then its reproductive motivation is increased
      !!   by an additive component that is an ::asymptotic() function of the
      !!   genome based `modulation_gamma` parameter. The maximum modulatory
      !!   increase ever possible is the double value of the raw primary
      !!   motivation.
      !! .
      if ( this%is_male() ) then
        this%motivations%reproduction%motivation_finl =                       &
                    this%motivations%reproduction%motivation_prim +           &
                    this%motivations%reproduction%motivation_prim *           &
                    asymptotic(this%motivations%reproduction%motivation_prim, &
                                                              modulation_gamma)
      end if

    end block MALE_REPROD

    !+ + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
    !> Sex modulation of the **reproduction** motivation state for **female**.
    !  @warning   See warning on `block`-construct in `MALE_REPROD` above.
    FEMALE_REPROD: block

      !> - First, use the sigmoid function and genome to set the phenotypic
      !!   value of the **gamma** modulation coefficient that mediates the
      !!   effect of the agent's sex on reproductive motivation.
      call this%trait_init(                                                   &
                    this_trait = modulation_gamma,                            &
                    g_p_matrix = SEX_FEMALE_MODULATION_REPRODUCE_GENOTYPE,    &
                    init_val = this%motivations%reproduction%motivation_prim, &
                    gerror_cv = SEX_FEMALE_MODULATION_REPRODUCE_GERROR_CV,    &
                    label = "M_FEMALE_REPRO" )

      !> - Second, add the modulation factor to the actual motivation value.
      !!   If the agent is male, then its reproductive motivation is increased
      !!   by an additive component that is an ::asymptotic() function of the
      !!   genome based `modulation_gamma` parameter. The maximum modulatory
      !!   increase ever possible is the double value of the raw primary
      !!   motivation.
      !! .
      if ( this%is_female() ) then
        this%motivations%reproduction%motivation_finl =                       &
                    this%motivations%reproduction%motivation_prim +           &
                    this%motivations%reproduction%motivation_prim *           &
                    asymptotic(this%motivations%reproduction%motivation_prim, &
                                                              modulation_gamma)
      end if

    end block FEMALE_REPROD

    !> The values are logged in the@ref intro_debug_mode "debug mode".
    call LOG_DBG( LTAG_INFO // "Modulation REPRODUCTION " //                  &
                  "gamma: " // TOSTR(modulation_gamma) //                     &
                  ", primary: " //                                            &
                  TOSTR(this%motivations%reproduction%motivation_prim) //     &
                  ", final: " //                                              &
                  TOSTR(this%motivations%reproduction%motivation_finl),       &
                  PROCNAME, MODNAME )

  contains
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !> Definition of the asymptotic function for converting the primary
    !! genotype-based modulation coefficient *gamma* to the actual additive
    !! multiplication value:
    !! @f$ y = \frac{M\cdot x}{1 + x} @f$, where @f$ M @f$ is the asymptotic
    !! value (maximum)
    !! @note wxMaxima quick code:
    !! @code
    !!    wxplot2d(0.7*x/(1+x), [x, 0, 10]);
    !! @endcode
    elemental function asymptotic(max_level, x) result (out_value)

      real(SRP) :: out_value
      real(SRP), intent(in) :: max_level, x

      ! We do not accept zero asymptotic maximum value.
      if (max_level <= ZERO) then
        out_value = 0.0_SRP
        return
      end if
      ! We also do not accept negative x-argument values, set to zero.
      if (x < ZERO) then
        out_value = 0.0_SRP
      else
        out_value = (max_level * x) / (1 + x)
      end if

    end function asymptotic

  end subroutine appraisal_motivation_modulation_genetic

  !-----------------------------------------------------------------------------
  !> Add individual final emotional state components into the emotional
  !! memory stack. This is a wrapper to the
  !! the_neurobio::memory_emotional::add_to_memory method.
  elemental subroutine appraisal_add_final_motivations_memory(this)
    class(APPRAISAL), intent(inout) :: this

    call this%memory_motivations%add_to_memory(                               &
                            this%motivations%hunger%motivation_finl,          &
                            this%motivations%fear_defence%motivation_finl,    &
                            this%motivations%reproduction%motivation_finl  )

  end subroutine appraisal_add_final_motivations_memory

  !-----------------------------------------------------------------------------
  !> Calculate the instantaneous probability of successful reproduction.
  !! @note  Note that this function is bound to class the_neurobio::appraisal
  !!        rather than the_neurobio::reproduce. Probability of successful
  !!        reproduction is a dynamic property of this agent that depends on
  !!        the nearby conspecifics and their sex and size/mass.
  function reproduce_do_probability_reproduction_calc(this, weight_baseline,  &
                                      allow_immature ) result (p_reproduction)
    class(APPRAISAL), intent(in) :: this
    !> @param[in] weight_baseline is the weighting factor for the baseline
    !!            probability of successful reproduction @f$ \varphi @f$ (see
    !!            details below).
    real(SRP), optional, intent(in) :: weight_baseline

    !> @param[in] allow_immature a logical switch that allows calculation
    !!            (non-zero probability) if the agent is not ready to
    !!            reproduction as determined by the
    !!            the_body::reproduction::is_ready_reproduce() method.
    !!            Normally, immature agents (for which this method returns
    !!            FALSE) have zero probability of reproduction.
    !!            The default is FALSE, i.e. not to allow reproduction to
    !!            immature agents.
    logical, optional, intent(in) :: allow_immature
    !> @returns instantaneous probability of successful reproduction.
    real(SRP) :: p_reproduction

    ! Local copies of the optionals.
    real(SRP) :: weight_baseline_here
    logical :: allow_immature_loc

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter ::                                            &
                PROCNAME = "(reproduce_do_probability_reproduction_calc)"

    !> ### Implementation details ###
    !> The probability of successful reproduction depends on the number of
    !! conspecifics of the same and the opposite sex within the `this` agent's
    !! visual range. So the starting point here is the number of conspecifics
    !! within the current conspecifics perception object.
    integer :: n_conspecifics_perception
    integer :: n_same_sex_perception, n_opposite_sex_perception

    ! Intermediate probabilities.
    real(SRP) :: p_reproduction_baseline

    ! Difference in mass with the other same-sex agents.
    real(SRP) :: delta_mass

    ! Sum of the same sex conspecifics mass, used in calculation of delta_mass.
    real(SRP) ::  sum_mass_samesex

    ! Local counters.
    integer :: i

    !> **First,** determine if the hormonal system of the agent is ready for
    !! reproduction using the_body::reproduction::is_ready_reproduce().
    if (present(allow_immature)) then
      allow_immature_loc = allow_immature
    else
      allow_immature_loc = .FALSE.
    end if
    !> If this agent is not ready to reproduce, a zero probability of
    !! reproduction returned. However, if the optional parameter
    !! `allow_immature` is explicitly set to TRUE, this check is not done
    !! and the probability of reproduction is calculated as follows.
    CHECK_MATURE: if ( .not. allow_immature_loc  ) then
      if ( .not. this%is_ready_reproduce() ) then
        p_reproduction = 0.0
        ! @warning Calling LOG_DBG precludes making this function **pure**.
        call LOG_DBG( LTAG_INFO // "Not ready to reproduce (steroids).",      &
                    PROCNAME, MODNAME)
        return
      end if
    end if CHECK_MATURE

    !> **Second,** determine if there are any conspecifics in the perception,
    !! if there are no, reproduction is impossible. Return straight away zero
    !! probability in such a case.
    CHECK_IS_ALONE: if ( .NOT. this%has_consp() ) then
      p_reproduction = 0.0_SRP
      ! @warning Calling LOG_DBG precludes making this function **pure**.
      call LOG_DBG( LTAG_INFO // "No conspecifics in perception",             &
                    PROCNAME, MODNAME)
      return
    end if CHECK_IS_ALONE

    ! Check optional baseline probability weighting factor @f$ \varphi @f$.
    if (present(weight_baseline)) then
      weight_baseline_here = weight_baseline
    else
      weight_baseline_here = PROBABILITY_REPRODUCTION_BASE_FACTOR
    end if

    !> **Second,** extract the number of conspecifics `n_conspecifics_perception`
    !! from the perception object.
    n_conspecifics_perception = this%perceive_consp%get_count()

    !> Also, initialise the number of same- and opposite-sex conspecifics
    !! (integer counters) as well as the total mass of same-sex conspecifics
    !! (real) to zero.
    n_same_sex_perception = 0
    n_opposite_sex_perception = 0
    sum_mass_samesex = 0.0_SRP

    !> **Third,** determine how many of the conspecifics in perception
    !! have the same and the opposite sex. Calculate total mass of same sex
    !! conspecifics.
    CHECK_SEX: if ( this%is_male() ) then
      do concurrent (i=1:n_conspecifics_perception)
          if ( this%perceive_consp%conspecifics_seen(i)%is_male() ) then
            n_same_sex_perception = n_same_sex_perception + 1
            sum_mass_samesex = sum_mass_samesex +                             &
                            this%perceive_consp%conspecifics_seen(i)%get_mass()
          else
            n_opposite_sex_perception = n_opposite_sex_perception + 1
          end if
      end do
    else CHECK_SEX
      do concurrent (i=1:n_conspecifics_perception)
          if ( this%perceive_consp%conspecifics_seen(i)%is_female() ) then
            n_same_sex_perception = n_same_sex_perception + 1
            sum_mass_samesex = sum_mass_samesex +                             &
                            this%perceive_consp%conspecifics_seen(i)%get_mass()
          else
            n_opposite_sex_perception = n_opposite_sex_perception + 1
          end if
      end do
    end if CHECK_SEX

    !> Additionally, check if the number of opposite sex agents is zero.
    !! in such a case zero probability of reproduction is obviously returned.
    if (n_opposite_sex_perception == 0) then
      p_reproduction = 0.0_SRP
      ! @warning Calling LOG_DBG precludes making this function **pure**.
      call LOG_DBG(LTAG_INFO // " No oposite-sex conspecifics in " //         &
                   "perception, return zero probability.", PROCNAME, MODNAME)
      return
    end if

    !> **Fourth,** calculate the **baseline probability** of reproduction.
    !! This probability is proportional to the proportions of the same- and
    !! opposite-sex agents within the visual range.
    !! @f[ p_{0} = \frac{N_{os}}{1+N_{ss}} ; 0 \leq p_{0}\leq 1, @f] where
    !! @f$ N_{os} @f$ is the number of the opposite-sex agents,
    !! @f$ N_{ss} @f$ is the number of same-sex agents.
    !! We also adjust the baseline probability of successful reproduction by
    !! a parameter factor @f$ \varphi @f$, so that this probability never
    !! reaches 1:
    !! @f[ p_{0} = \frac{N_{os}}{1+N_{ss}} \cdot \varphi @f]
    !! For example, if there is only one agent of the opposite sex and no
    !! same-sex in proximity the baseline probability of reproduction is
    !! 1/(1+0) = 1.0 (note that the this agent also adds to the same-sex count,
    !! hence "1+..."). If there are 3 opposite-sex agents and 3 same-sex
    !! agents, the baseline probability is calculated as 3/(1+3) = 0.75. This
    !! doesn't take account of the @f$ \varphi @f$ multiplier factor.
    p_reproduction_baseline =                                                 &
        within( real(n_opposite_sex_perception, SRP) /                        &
                 (1.0_SRP + real(n_same_sex_perception, SRP)),                &
                0.0_SRP, 1.0_SRP ) *  weight_baseline_here

    !> **Fifth,** to get the final successful reproduction probability,
    !! the baseline value @f$ p_{0} @f$ is multiplied by a function
    !! @f$ \Phi @f$ that depends on the relative body mass of the `this` agent
    !! with respect to all the *same-sex* agents in proximity.
    !! @f[ p_{rep} = p_{0} \cdot \Phi(\Delta \overline{m_{i}}),
    !! 0 \leq p_{rep} \leq 1 , @f] where
    !! @f$ p_{rep} @f$ is the final probability of successful reproduction.
    !! This is done to model direct within-sex competition for mates.
    !! Therefore, if the `this` agent is smaller than all the other same-sex
    !! agents here, the probability of successful reproduction significantly
    !! reduces. On the other hand, if the agent is larger than all the others,
    !! this probability would increase. The form of the @f$ \Phi @f$
    !! function is calculated on the bases of the *ratio* of the `this`
    !! agent body mass to the average body mass of all same sex agents within
    !! the visual range:
    !!   @f[ \Delta \overline{m_{i}} = \frac{ M }{ \overline{m_{i}} } . @f]
    CHECK_CALCP: if (n_same_sex_perception == 0) then
      !> Note that if there are *no same sex agents* (i.e. intra-sexual
      !! competition is absent) the probability of
      !! reproduction takes  the baseline value @f$ p_{0} @f$ :
      !! @f[ p_{rep} = p_{0} . @f]
      !! No debug interpolation plot is produced in such a degenerate case.
      p_reproduction = p_reproduction_baseline
      ! @warning Calling LOG_DBG precludes making this function **pure**.
      call LOG_DBG(LTAG_INFO // " No same-sex conspecifics in " //            &
                   "perception, return baseline: " //                         &
                   TOSTR(p_reproduction), PROCNAME, MODNAME)
    else CHECK_CALCP
      delta_mass =                                                            &
        within( this%get_mass() /                                             &
                  (sum_mass_samesex / real(n_same_sex_perception, SRP)),      &
                minval(PROBABILITY_REPRODUCTION_DELTA_MASS_ABSCISSA),         &
                maxval(PROBABILITY_REPRODUCTION_DELTA_MASS_ABSCISSA)          &
              )
      !> The @f$ \Phi(\Delta \overline{m_{i}}) @f$ function itself is obtained
      !! from a nonlinear interpolation of grid values defined by the parameter
      !! arrays `commondata::probability_reproduction_delta_mass_abscissa` and
      !! `commondata::probability_reproduction_delta_mass_ordinate`.
      !! @image html img_doxy_reprod_prob.svg
      !! @image latex img_doxy_reprod_prob.eps "Relationship between body mass ratio and probability of reproduction" width=14cm
      !! So the **final reproduction probability** value is obtained by
      !! multiplication of the baseline value by the @f$ \Phi @f$ function.
      !! The final probability of reproduction value is limited to lie
      !! within the range @f$ 0 \leq p_{rep} \leq 1 \cdot \varphi @f$.
      p_reproduction                                                          &
        = within(                                                             &
                 p_reproduction_baseline *                                    &
                    DDPINTERPOL(PROBABILITY_REPRODUCTION_DELTA_MASS_ABSCISSA, &
                                PROBABILITY_REPRODUCTION_DELTA_MASS_ORDINATE, &
                                delta_mass),                                  &
                 0.0_SRP, weight_baseline_here )
      ! @warning Calling LOG_DBG precludes making this function **pure**.
      call LOG_DBG(LTAG_INFO //                                               &
                          " P reproduction: " // TOSTR(p_reproduction) //     &
                          ", average mass ratio of same sex " //              &
                          "conspecifics in percept: " // TOSTR(delta_mass) // &
                          ", N of same-sex conspecifics: " //                 &
                          TOSTR(n_same_sex_perception) //                     &
                          ", N of opposite-sex conspecifics: " //             &
                          TOSTR(n_opposite_sex_perception) //                 &
                          ", baseline P reproduction: " //                    &
                          TOSTR(p_reproduction_baseline), PROCNAME, MODNAME)

      !> Interpolation plots can be saved in the @ref intro_debug_mode
      !! "debug mode" using this plotting command:
      !! commondata::debug_interpolate_plot_save().
      !  @warning Calling `debug_interpolate_plot_save` here precludes making
      !           this function **pure** and **elemental**. Comment-out upon
      !           full testing and debugging!
      call debug_interpolate_plot_save(                                       &
              grid_xx=PROBABILITY_REPRODUCTION_DELTA_MASS_ABSCISSA,           &
              grid_yy=PROBABILITY_REPRODUCTION_DELTA_MASS_ORDINATE,           &
              ipol_value=delta_mass,                                          &
              algstr="DDPINTERPOL",                                           &
              output_file="plot_debug_reproduction_probability_" //           &
                      TOSTR(Global_Time_Step_Model_Current) //                &
                      MMDD // "_a_"// trim(this%individ_label()) // "_" //    &
                      RAND_STRING(LABEL_LENGTH, LABEL_CST,LABEL_CEN) // PS )

    end if CHECK_CALCP

  end function reproduce_do_probability_reproduction_calc

  !-----------------------------------------------------------------------------
  !> @brief   Determine a stochastic outcome of **this** agent reproduction.
  !!          Returns TRUE if the agent has reproduced successfully.
  !! @param[in] prob optional fixed probability of reproduction to override.
  !! @return  TRUE if reproduction is successful.
  !! @warning This function cannot be made elemental/pure due to random
  !!          number call.
  function reproduction_success_stochast(this, prob) result (success)
    class(APPRAISAL), intent(in) :: this  ! This actor agent.
    ! @param[in] prob fixed probability of reproduction.
    real(SRP), optional, intent(in) :: prob
    ! return@ TRUE if reproduction is actually successful.
    logical :: success

    ! Local copy of `prob` parameter.
    real(SRP) :: prob_here

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME="(reproduction_success_stochast)"

    success = .FALSE.                     ! Init to FALSE.

    !> ### Implementation details ###
    !> Check if `prob` is present, if not, the probability of reproduction is
    !! calculated based on the perception objects of the actor agent `this`
    !! using the `probability_reproduction()` method.
    if (present(prob)) then
      prob_here = prob
    else
      prob_here = this%probability_reproduction()
    end if

    !> Determine the reproduction success stochastically based on the
    !! probability of repriduction (`prob_here`) value.
    if ( RAND_R4() < prob_here ) success = .TRUE.

  end function reproduction_success_stochast

  !-----------------------------------------------------------------------------
  !> Add emotional components into the memory stack.
  elemental subroutine emotional_memory_add_to_stack(this,                    &
          v_hunger, v_defence_fear, v_reproduction,                       &
          v_gos_label, v_gos_arousal, v_gos_repeated )

    class(MEMORY_EMOTIONAL), intent(inout) :: this !> This memory object.

    !> The parameters of the subroutine are the actual values that are added
    !! to the emotional memory stack arrays.
    !! @param[in] v_hunger             value for hunger;
    !! @param[in] v_defence_fear   value for fear state;
    !! @param[in] v_reproduction       value for reproduction;
    !! @param[in] v_gos_label          value for GOS label;
    !! @param[in] v_gos_arousal        value for GOS arousal value;
    !! @param[in] v_gos_repeated       value for repeated counter for GOS.
    real(SRP), intent(in)              :: v_hunger
    real(SRP), intent(in)              :: v_defence_fear
    real(SRP), intent(in)              :: v_reproduction
    character(*), optional, intent(in) :: v_gos_label
    real(SRP), optional, intent(in)    :: v_gos_arousal
    integer, optional, intent(in)      :: v_gos_repeated

    !> Each of the memory stack components corresponds to the respective
    !! dummy parameter. These arrays are updated at each step (mandatory
    !! procedure arguments):
    !! - v_hunger
    !! - v_defence_fear
    !! - v_reproduction
    call add_to_history( this%hunger,             v_hunger )
    call add_to_history( this%defence_fear,       v_defence_fear )
    call add_to_history( this%reproduction,       v_reproduction )

    !> However, GOS parameters are optional and updated only if provided for
    !! invocation of this method (optional arguments):
    !! - v_gos_label;
    !! - v_gos_arousal;
    !! - v_gos_repeated.
    if (present(v_gos_label))                                                 &
                      call add_to_history( this%gos_main, v_gos_label )
    if (present(v_gos_arousal))                                               &
                      call add_to_history( this%gos_arousal, v_gos_arousal )
    if (present(v_gos_repeated))                                              &
                      call add_to_history( this%gos_repeated, v_gos_repeated )

  end subroutine emotional_memory_add_to_stack

  !-----------------------------------------------------------------------------
  !> Add the current GOS label or/and arousal value and/or arousal repeat
  !! count into the emotional memory stack.
  elemental subroutine emotional_memory_add_gos_to_stack(this, v_gos_label,   &
                                                v_gos_arousal, v_gos_repeated )
    class(MEMORY_EMOTIONAL), intent(inout) :: this
    !> @param[in] v_gos_label Text label for the current GOS.
    character(*), optional, intent(in) :: v_gos_label
    !> @param[in] v_gos_arousal The maximum motivation (arousal) value for the
    !!            current GOS.
    real(SRP), optional, intent(in)    :: v_gos_arousal
    !> @param[in] v_gos_repeated
    integer, optional, intent(in)      :: v_gos_repeated

    !> ### Implementation notes ###
    !> GOS label is added to the memory stack.
    if (present(v_gos_label)) call add_to_history( this%gos_main, v_gos_label )

    !> GOS arousal is added to the memory stack.
    if (present(v_gos_arousal))                                               &
                    call add_to_history( this%gos_arousal, v_gos_arousal )

    !> The GOS repeated counter (gos_repeated) is added to the memory stack.
    if (present(v_gos_repeated))                                               &
                      call add_to_history( this%gos_repeated, v_gos_repeated )

  end subroutine emotional_memory_add_gos_to_stack

  !-----------------------------------------------------------------------------
  !> Cleanup and destroy the emotional memory stack.
  elemental subroutine emotional_memory_cleanup_stack(this)

    class(MEMORY_EMOTIONAL), intent(inout) :: this !> This memory object.

    !> cleanup procedure uses whole array assignment to the
    !! commondata::missing values.
    this%hunger             =  MISSING
    this%defence_fear       =  MISSING
    this%reproduction       =  MISSING
    this%gos_main           =  ""
    this%gos_arousal        =  MISSING
    this%gos_repeated       =  UNKNOWN

  end subroutine emotional_memory_cleanup_stack

  !-----------------------------------------------------------------------------
  !> Get the average value of the hunger motivation state within the
  !! whole emotional memory stack.
  elemental function emotional_memory_hunger_get_mean (this, last)            &
                                                            result (mean_value)
    class(MEMORY_EMOTIONAL), intent(in) :: this
    !> @returns Total count of predators in the memory stack.
    real(SRP) :: mean_value
    !> @param last Limit to only this number of latest components in the
    !!        history.
    integer, optional, intent(in) :: last

    ! Local copy of optional last
    integer :: last_here

    ! History stack size. We determine it from the size of the actual array
    ! rather than `HISTORY_SIZE_MOTIVATION` for further safety.
    integer, parameter :: HIST_SIZE = size(this%hunger)

    !> ### Implementation notes ###
    !> - Check if we are given the parameter requesting the latest history size.
    !!   If the `last` parameter is absent or bigger than the array size,
    !!   get the whole stack array.
    if (present(last)) then
      if ( last < HIST_SIZE ) then
        last_here = last
      else
        last_here = HIST_SIZE
      end if
    else
      last_here = HIST_SIZE
    end if

    !> - Calculate the average excluding missing values (masked) within the
    !!   subarray of interest.
    !! .
    mean_value = average( this%hunger( HIST_SIZE-last_here+1:HIST_SIZE ),     &
                          undef_ret_null=.TRUE. )

  end function emotional_memory_hunger_get_mean

  !-----------------------------------------------------------------------------
  !> Get the average value of the fear state motivation state within the
  !! whole emotional memory stack.
  elemental function emotional_memory_actve_avoid_get_mean (this, last)      &
                                                            result (mean_value)
    class(MEMORY_EMOTIONAL), intent(in) :: this
    !> @returns Total count of predators in the memory stack.
    real(SRP) :: mean_value
    !> @param last Limit to only this number of latest components in the
    !!        history.
    integer, optional, intent(in) :: last

    ! Local copy of optional last
    integer :: last_here

    ! History stack size. We determine it from the size of the actual array
    ! rather than `HISTORY_SIZE_MOTIVATION` for further safety.
    integer, parameter :: HIST_SIZE = size(this%defence_fear)

    !> ### Implementation notes ###
    !> - Check if we are given the parameter requesting the latest history size.
    !!   if the `last` parameter is absent or bigger than the array size, get
    !!   the whole stack array.
    if (present(last)) then
      if ( last < HIST_SIZE ) then
        last_here = last
      else
        last_here = HIST_SIZE
      end if
    else
      last_here = HIST_SIZE
    end if

    !> - Calculate the average excluding missing values (masked) within the
    !!   subarray of interest.
    !! .
    mean_value = average(                                                     &
                this%defence_fear( HIST_SIZE-last_here+1:HIST_SIZE ),         &
                undef_ret_null=.TRUE. )

  end function emotional_memory_actve_avoid_get_mean

  !-----------------------------------------------------------------------------
  !> Get the average value of the reproductive motivation state within the
  !! whole emotional memory stack.
  elemental function emotional_memory_reproduct_get_mean (this, last)         &
                                                            result (mean_value)
    class(MEMORY_EMOTIONAL), intent(in) :: this
    !> @returns Total count of predators in the memory stack.
    real(SRP) :: mean_value
    !> @param last Limit to only this number of latest components in the
    !!        history.
    integer, optional, intent(in) :: last

    ! Local copy of optional last
    integer :: last_here

    ! History stack size. We determine it from the size of the actual array
    ! rather than `HISTORY_SIZE_MOTIVATION` for further safety.
    integer, parameter :: HIST_SIZE = size(this%reproduction)

    !> ### Implementation notes ###
    !> - Check if we are given the parameter requesting the latest history size.
    !!   if the `last` parameter is absent or bigger than the array size, get
    !!   the whole stack array.
    if (present(last)) then
      if ( last < HIST_SIZE ) then
        last_here = last
      else
        last_here = HIST_SIZE
      end if
    else
      last_here = HIST_SIZE
    end if

    !> - Calculate the average excluding missing values (masked) within the
    !!   subarray of interest.
    !! .
    mean_value = average(                                                     &
                this%reproduction( HIST_SIZE-last_here+1:HIST_SIZE ),         &
                undef_ret_null=.TRUE. )

  end function emotional_memory_reproduct_get_mean

  !-----------------------------------------------------------------------------
  !> Get the average value of the GOS arousal within the whole emotional
  !! memory stack.
  elemental function emotional_memory_arousal_mean (this, last)               &
                                                            result (mean_value)
    class(MEMORY_EMOTIONAL), intent(in) :: this
    !> @returns Total count of predators in the memory stack.
    real(SRP) :: mean_value
    !> @param last Limit to only this number of latest components in the
    !!        history.
    integer, optional, intent(in) :: last

    ! Local copy of optional last
    integer :: last_here

    ! History stack size. We determine it from the size of the actual array
    ! rather than `HISTORY_SIZE_MOTIVATION` for further safety.
    integer, parameter :: HIST_SIZE = size(this%gos_arousal)

    !> ### Implementation notes ###
    !> - Check if we are given the parameter requesting the latest history size.
    !!   If the `last` parameter is absent or bigger than the array size, get
    !!   the whole stack array.
    if (present(last)) then
      if ( last < HIST_SIZE ) then
        last_here = last
      else
        last_here = HIST_SIZE
      end if
    else
      last_here = HIST_SIZE
    end if

    !> - Calculate the average excluding missing values (masked) within the
    !!   subarray of interest.
    !! .
    mean_value = average(                                                     &
                this%gos_arousal( HIST_SIZE-last_here+1:HIST_SIZE ),          &
                undef_ret_null=.TRUE. )

  end function emotional_memory_arousal_mean

  !-----------------------------------------------------------------------------
  !> Find and set the **Global Organismic State (GOS)** of the agent based on
  !! the various available motivation values. The motivation values linked with
  !! the different stimuli compete with the current GOS and among themselves.
  !! ### General principle ###
  !! The GOS competition threshold is a function of the current GOS arousal
  !! level: if it is very low, it would be very difficult to switch to a
  !! different GOS. However, if the current GOS has a high arousal, then
  !! switching to a competing motivation is relatively easy: a very small
  !! motivational surplus is enough for winning the competition with the
  !! current GOS.
  !! @image html img_doxy_aha2-gos-threshold.svg
  !! @image latex img_doxy_aha2-gos-threshold.eps "Global organismcs state" width=14cm
  !! @note  GOS generation is a little changed in the new generation model.
  !!        1. We try to avoid constant switching of the GOS by requiring that
  !!           the difference between motivational components should exceed
  !!           some threshold value, if it does not, retain old GOS. So minor
  !!           fluctuations in the stimulus field are ignored. Threshold is
  !!           a dynamic parameter, so can also be zero.
  !!        2. The threshold is inversely related to the absolute value of the
  !!           motivations compared, when the motivations are low, the
  !!           threshold is big, when their values are approaching 1, the
  !!           threshold approaches zero. So motivations have relatively little
  !!           effects.
  subroutine gos_find_global_state(this)
    class(GOS_GLOBAL), intent(inout) :: this

    !> ### Implementation details ###
    !! #### Notable class data members ####
    !! **Public attribute of the `GOS_GLOBAL` class: `gos_arousal`** keeps the
    !! current level of the GOS arousal (*A*, see below). If GOS does
    !! switch as a result of competition with the other motivational states,
    !! it gets the value of its *winning* (maximum) motivation, if GOS does
    !! not switch as a result of competition, the `gos_arousal` value
    !! dissipates spontaneously to a lower value and the `gos_repeated`
    !! attribute of `GOS_GLOBAL` gets the successive number of repetitions
    !! of the same out of competition GOS state.
    !! #### Notable local variables ####
    !! **Local variable: `arousal_new`** is the maximum level of motivation
    !! among all new incoming motivations *A*. It is this motivation
    !! value that competes with the current GOS arousal value (*G*
    !! the `gos_arousal` public attribute of the `GOS_GLOBAL` class).
    real(SRP) :: arousal_new

    !> **Local variable `gos_dthreshold`** is a dynamic threshold
    !! factor for GOS change @f$ \Delta @f$ (see below). It determines the
    !! threshold that a new competing motivation has to exceed to win the
    !! competition with the previous (and still current up to this point)
    !! motivation.
    real(SRP) :: gos_dthreshold

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(gos_find_global_state)"

    ! *Arousal* is the maximum level among all available motivations (**final**
    ! motivational components). This is the **new** state depending on all
    ! the currently incoming perceptions.
    arousal_new = this%motivations%max_final()

    call LOG_DBG( LTAG_INFO // "Current GOS arousal: " //                     &
                  TOSTR(this%gos_arousal), PROCNAME, MODNAME )
    call LOG_DBG( LTAG_INFO // "Motivations (final) array: " //               &
                  TOSTR([ this%motivations%hunger%motivation_finl,            &
                          this%motivations%fear_defence%motivation_finl,      &
                          this%motivations%reproduction%motivation_finl ]),   &
                  PROCNAME, MODNAME )
    call LOG_DBG( LTAG_INFO // "Arousal max (compet): " // TOSTR(arousal_new),&
                  PROCNAME, MODNAME )

    !> #### GOS competition ####
    !> The GOS competition threshold is a function of the current GOS arousal
    !! level @f$ G @f$: if it is very low, we need a relatively high competing
    !! motivation to win competition, if it is high then very small difference
    !! is enough. The global organismic state will switch to a competing state
    !! only if its maximum motivation @f$ A @f$ exceeds the current GOS's
    !! arousal level @f$ G @f$ by more than @f$ \Delta @f$ units of @f$ G @f$:
    !! @f[ A - G > \Delta \cdot G . @f]
    !! Here the @f$ \Delta @f$ threshold factor is set by a nonparametric
    !! function that is calculated from nonlinear interpolation of the grid
    !! values:
    !! @image html img_doxy_aha-gos-delta.svg
    !! @image latex img_doxy_aha-gos-delta.eps "GOS competition threshold factor" width=14cm
    !! So if the agent currently has a low GOS arousal *G*=0.1, it requires a
    !! competing state to be at least *A*=0.155 to win (with @f$ \Delta @f$
    !! =0.55, 0.155 = 0.1 + 0.1 * 0.55). However, if the agent has a high GOS
    !! motivation *G*=0.8, almost any exceeding  motivation (>0.808) will win.
    !! The actual value of the nonparametric interpolation function are
    !! obtained by nonlinear interpolation of the grid values
    !! defined by the `MOTIVATION_COMPET_THRESHOLD_CURVE_` parameter arrays.
    !! @note In this implementation, the exact **type** of the competing
    !!       motivation is not considered in the GOS competition procedure.
    !!       For example, The current hunger GOS competes with all motivations,
    !!       including itself. Consider an agent that is starving and has
    !!       a high level of hunger GOS. At each step this hunger competes
    !!       with all other motivations, including hunger. If hunger continues
    !!       to increase, still, at many time steps, the new level of hunger
    !!       can outcompete the current hunger and GOS switches from hunger
    !!       to ... hunger. There can also be situations when current GOS
    !!       hunger wins competition from all other motivations several times,
    !!       dissipates and then is outcompeted by hunger again, that would
    !!       lead to a relatively long streak of the same GOS. This mechanism
    !!       would preclude switching out of (and losing) a continuously high
    !!       but still appropriate motivational state.
    gos_dthreshold = DDPINTERPOL( MOTIVATION_COMPET_THRESHOLD_CURVE_ABSCISSA, &
                                  MOTIVATION_COMPET_THRESHOLD_CURVE_ORDINATE, &
                                  this%gos_arousal )

    call LOG_DBG( LTAG_INFO // "GOS threshold from interpolation: " //        &
                  TOSTR(gos_dthreshold), PROCNAME, MODNAME )

    !> The interpolation plots are saved in the @ref intro_debug_mode
    !! "debug mode" to a disk file using an external command by the
    !! `commondata::debug_interpolate_plot_save()` procedure.
    !! @warning Enabling plotting can produce a **huge** number of plots and
    !!          should normally be disabled.
    call debug_interpolate_plot_save(                                         &
            grid_xx=MOTIVATION_COMPET_THRESHOLD_CURVE_ABSCISSA,               &
            grid_yy=MOTIVATION_COMPET_THRESHOLD_CURVE_ORDINATE,               &
            ipol_value=this%gos_arousal, algstr="DDPINTERPOL",                &
            output_file="plot_debug_arousal_gos_threshold_" //                &
                        TOSTR(Global_Time_Step_Model_Current) //              &
                        MMDD // "_a_"// trim(this%individ_label()) //         &
                        "_" // RAND_STRING(LABEL_LENGTH, LABEL_CST,LABEL_CEN) &
                        // PS )

    !> Once the dynamic threshold is calculated, we can compare each of the
    !! competing motivation levels with the current arousal. If the maximum
    !! value of these motivations exceeds the current arousal by more than the
    !! threshold @f$ \Delta @f$ factor, the GOS switches to the new motivation.
    !! If not, we are still left with the previous GOS.
    AROUSAL_THRESHOLD: if (arousal_new - this%gos_arousal <                   &
                                        gos_dthreshold * this%gos_arousal) then
      !> ##### Threshold not exceeded #####
      !> If the maximum competing motivation does not exceed the threshold,
      !! we are left with the old GOS. However, we reduce the current arousal
      !! spontaneously using a simple linear or some non-linear dissipation
      !! pattern using the \%gos_repeated parameter that sets the number of
      !! repeated occurrences of the same (current) GOS.
      !! First, increment GOS repeat counter.
      this%gos_repeated = this%gos_repeated + 1
      !> And spontaneously decrease, **dissipate**, the current arousal level.
      !! Spontaneous dissipation of arousal is implemented by multiplying the
      !! current level by a factor within the range [0.0..1.0] that can depend
      !! on the number of times this GOS is repeated.
      !! @note Note that the dissipation function is local to this procedure.
      !!       `arousal_decrease_factor_fixed` = fixed value
      !!       `arousal_decrease_factor_nonpar` = nonlinear, nonparametric,
      !!       based on nonlinear interpolation.
      !! @plot `aha_gos_arousal_dissipation.svg`
      !! @note Can use either `arousal_decrease_factor_fixed` or
      !!       `arousal_decrease_factor_nonpar`.
      this%gos_arousal = this%gos_arousal *                                   &
                              arousal_decrease_factor_fixed(this%gos_repeated)
      call LOG_DBG( LTAG_INFO // "Threshold not exceeded, " //                &
                    "GOS repeated incremented to: " //                        &
                    TOSTR(this%gos_repeated), PROCNAME, MODNAME )
    else AROUSAL_THRESHOLD
      !> ##### Threshold is exceeded #####
      !> If the maximum competing motivation exceeds the threshold, we get to a
      !! **new GOS**. That is, the **highest** among the competing
      !! motivations defines the new GOS.
      !  @note Use `associate` construct to set alias for long object hierarchy.
      !> @note Note that `gos_repeated` is initialised to 1.0 at
      !!       `gos_reset`.
      associate ( MOT => this%motivations )
        !> ### Check **hunger** ###
        GOS_IS_MAX: if (MOT%is_max_final(MOT%hunger)) then
          !> Reset all motivations to *non-dominant*.
          call this%gos_reset()
          !> Set new GOS for hunger...
          MOT%hunger%dominant_state = .TRUE.
          this%gos_main = MOT%hunger%label
          this%gos_arousal = MOT%hunger%motivation_finl
        !> ### Check **fear_defence** ###
        else if (MOT%is_max_final(MOT%fear_defence)) then GOS_IS_MAX
          !> Reset all motivations to *non-dominant*.
          call this%gos_reset()
          !> Set new GOS for fear_defence...
          MOT%fear_defence%dominant_state = .TRUE.
          this%gos_main = MOT%fear_defence%label
          this%gos_arousal = MOT%fear_defence%motivation_finl
        !> ### Check **reproduction** ###
        else if (MOT%is_max_final(MOT%reproduction)) then GOS_IS_MAX
          !> Reset all motivations to *non-dominant*.
          call this%gos_reset()
          !> Set new GOS for reproduction...
          MOT%reproduction%dominant_state = .TRUE.
          this%gos_main = MOT%reproduction%label
          this%gos_arousal = MOT%reproduction%motivation_finl
        end if GOS_IS_MAX
      end associate
      call LOG_DBG( LTAG_INFO // "Threshold exceeded, arousal: " //           &
                    TOSTR(this%gos_arousal) // ", label: " //                 &
                    this%gos_main, PROCNAME, MODNAME )
    end if AROUSAL_THRESHOLD

    !> #### Other finalising procedures ####
    !! Add the current GOS parameters to the emotional memory stack
    !! @note Note that the memory stack arrays are defined in
    !!       APPRAISAL and cleaned/init in `init_appraisal`
    ! @note We can use the dedicated procedures. Here disabled so far to avoid
    !       a small speed overhead.
    ! @code{.unparsed}
    ! call this%memory_motivations%gos_to_memory(                            &
    !                 v_gos_label=this%gos_main,                             &
    !                 v_gos_arousal= this%gos_arousal,                       &
    !                 v_gos_repeated=this%gos_repeated  )
    ! @endcode
    call add_to_history(this%memory_motivations%gos_main, this%gos_main)
    call add_to_history(this%memory_motivations%gos_arousal, this%gos_arousal)
    call add_to_history(this%memory_motivations%gos_repeated, this%gos_repeated)

    !> Finally recalculate the attention weights for all the states' perception
    !! components using attention_modulate(). The dominant GOS state will now
    !! get its default attention weights whereas all non-dominant states will
    !! get modulated values, i.e. values recalculated from a non-linear
    !! interpolation based **attention modulation curve**.
    call this%attention_modulate()

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !> ### Fixed spontaneous arousal dissipation factor ###
    !!     At each step, `gos_arousal` is reduced by a constant factor,
    !!     `AROUSAL_GOS_DISSIPATION_FACTOR` (e.g. reduced by 0.5)
    !!     independently on the current GOS time step.
    !!     TODO: make dependent on the genome.
    pure function arousal_decrease_factor_fixed (time_step)                  &
                                                        result (arousal_factor)
      !> @param[in] time_step The number of repetitions of the same GOS.
      !! @note  Note that the `time_step` dummy parameter is **not used** in
      !!        calculations here. But it is still present as an optional
      !!        parameter for compatibility with the other possible dissipation
      !!        pattern functions that can really depend on the time GOS repeat
      !!        step.
      integer, optional, intent(in) :: time_step
      !> @returns Arousal dissipation factor.
      real(SRP) :: arousal_factor

      !> At each GOS step the `gos_arousal` is reduced by the factor
      !! `AROUSAL_GOS_DISSIPATION_FACTOR` that is **independent** of
      !! the `time_step`.
      arousal_factor = AROUSAL_GOS_DISSIPATION_FACTOR

    end function arousal_decrease_factor_fixed

    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !> ### Nonparametric spontaneous arousal dissipation pattern ###
    !! @details Here the arousal dissipation factor is defined by a
    !!          function of the GOS repeated time step based on polynomial
    !!          or linear interpolation over a grid defined by
    !!          `AROUSAL_GOS_DISSIPATION_NONPAR_ABSCISSA` (X) and
    !!          `AROUSAL_GOS_DISSIPATION_NONPAR_ORDINATE` (Y).
    !!          For example, multiplied by 0.9 at the first time step,
    !!          0.5 at the 10s step, 0.7 at 20s step.
    !!          TODO: make dependent on the genome.
    !! @param[in] time_step The number of repetitions of the same GOS.
    !! @returns Arousal dissipation factor. In this version, arousal
    !!          dissipation factor is determined by a nonlinear
    !!          interpolation-based function.
    !! @image html img_doxygen_dissipate_nonpar.svg
    !! @image latex img_doxygen_dissipate_nonpar.eps "Nonparametric spontaneous arousal dissipation function" width=14cm
    !! @n
    !! Save Interpolation plot:
    !! @code
    !! htintrpl.exe  [ 1.0, 2.00, 5.00, 10.0, 15.0, 18.0, 20.0 ] \
    !!               [ 1.0, 0.98, 0.80, 0.40, 0.22, 0.18, 0.17 ] [2] \
    !!               [img_doxygen_dissipate_nonpar.ps]
    !! @endcode
    function arousal_decrease_factor_nonpar(time_step)                        &
                                                        result (arousal_factor)
      ! @param[in] time_step The number of repetitions of the same GOS.
      integer, intent(in) :: time_step
      ! @returns Arousal dissipation factor. In this version, arousal
      !          dissipation factor is determined by a nonlinear
      !          interpolation-based function.
      real(SRP) :: arousal_factor

      arousal_factor = DDPINTERPOL(                                           &
                                AROUSAL_GOS_DISSIPATION_NONPAR_ABSCISSA,      &
                                AROUSAL_GOS_DISSIPATION_NONPAR_ORDINATE,      &
                                real(time_step, SRP) )

      !> Interpolation plots can be saved in the @ref intro_debug_mode
      !! "debug mode" using this plotting command:
      !! commondata::debug_interpolate_plot_save().
      !! @warning Disabling the plot output allows the function to be declared
      !!          as **pure** with all the benefits.
      call debug_interpolate_plot_save(                                       &
          grid_xx=AROUSAL_GOS_DISSIPATION_NONPAR_ABSCISSA,                    &
          grid_yy=AROUSAL_GOS_DISSIPATION_NONPAR_ORDINATE,                    &
          ipol_value=real(time_step, SRP), algstr="DDPINTERPOL",              &
          output_file="plot_debug_arousal_dissipation_factor_" //             &
                      TOSTR(Global_Time_Step_Model_Current) //                &
                      MMDD // "_a_"// trim(this%individ_label()) // "_"  //   &
                      RAND_STRING(LABEL_LENGTH, LABEL_CST,LABEL_CEN) // PS )

    end function arousal_decrease_factor_nonpar

  end subroutine gos_find_global_state

  !> Initialise GOS engine components to a zero state. The values are set to
  !! commondata::missing, commondata::unknown, string to "undefined".
  elemental subroutine gos_init_zero_state(this)
    class(GOS_GLOBAL), intent(inout) :: this

    this%gos_main = "undefined"
    !> @note the GOS arousal value is initialised to commondata::missing,
    !!       which is a big negative value. Therefore, any competing motivation
    !!       initially wins in the the_neurobio::gos_find_global_state()
    !!       procedure. There seems to be no sense initialising arousal to 0.0.
    this%gos_arousal = MISSING
    this%gos_repeated = UNKNOWN

  end subroutine gos_init_zero_state

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
  !! @note The `dies` method is implemented at the @ref gos_global to allow
  !!       "cleaning" of all neurobiological objects when `dies` is called
  !!       when performing the behaviours upwards in the object hierarchy.
  elemental subroutine gos_agent_set_dead(this)
    class(GOS_GLOBAL), intent(inout) :: this

    call this%set_dead()          !> - Set the agent "dead";
    call this%init_reproduction() !> - emptify reproduction objects;
    call this%init_perception()   !> - emptify all neurobiological objects.
    call this%init_appraisal()    !> .
    call this%init_gos()

  end subroutine gos_agent_set_dead

  !-----------------------------------------------------------------------------
  !> Reset all motivation states as *not* dominant with respect to the GOS.
  !! @note This subroutine is used in the_neurobio::gos_find_global_state().
  elemental subroutine gos_reset_motivations_non_dominant(this)
    class(GOS_GLOBAL), intent(inout) :: this

    !> ### Implementation notes ###
    !> Reset dominant status to FALSE for all motivational states calling
    !! the the_neurobio::motivation::gos_ind_reset().
    call this%motivations%gos_ind_reset()

    !> Also reset the number of GOS repeated occurrences to 1.
    this%gos_repeated = 1

  end subroutine gos_reset_motivations_non_dominant

  !-----------------------------------------------------------------------------
  !> Get the current global organismic state (GOS).
  elemental function gos_global_get_label(this) result (return_gos)
    class(GOS_GLOBAL), intent(in) :: this

    !> @returns Global organismic state label.
    character(len=LABEL_LENGTH) :: return_gos

    !> Check which of the currently implemented motivational state
    !! components (`STATE_`) has the **dominant** flag. Can call
    !! motivation-type-bound function \%is_dominant().
    !! @note  Only one component can be "dominant".
    !         TODO: consider several simultaneously overlapping GOSs. In
    !               such a case the function should return an array-based
    !               object.
    !  @note  Note that type-bound functions can be used (although this makes
    !         sense only outside of this module to avoid a small function-call
    !         overhead): `if ( this%motivations%hunger%is_dominant() ) then`.
    !         For the motivational state label we can use the accessor
    !         function \%label_is :
    !         `return_gos = this%motivations%hunger%label_is()` (it is
    !         **mandatory** outside of this module as label is declared
    !         `private`).
    if (this%motivations%hunger%dominant_state) then
      return_gos = this%motivations%hunger%label
    else if (this%motivations%fear_defence%dominant_state) then
      return_gos = this%motivations%fear_defence%label
    else if (this%motivations%reproduction%dominant_state) then
      return_gos = this%motivations%reproduction%label
    end if

  end function gos_global_get_label

  !-----------------------------------------------------------------------------
  !> Get the overall level of arousal. Arousal is the current level
  !! of the dominant motivation that has brought about the current GOS at the
  !! previous time step.
  elemental function gos_get_arousal_level(this) result (arousal_out)
    class(GOS_GLOBAL), intent(in) :: this

    ! Arousal is the current level of motivation that has brought about GOS.
    real(SRP) :: arousal_out

    ! It is saved in this GOS-object component.
    arousal_out = this%gos_arousal

  end function gos_get_arousal_level

  !-----------------------------------------------------------------------------
  !> Modulate the attention weights to suppress all perceptions alternative
  !! to the current GOS. This is done using the attention modulation
  !! interpolation curve.
  !! @warning This subroutine is called from within `gos_find()` and should
  !!          normally **not** be called separately.
  subroutine gos_attention_modulate_weights(this)
    class(GOS_GLOBAL), intent(inout) :: this

    ! Local variable, the weight given to the attention weight components
    ! of all the non-dominant motivation states. Based on nonlinear
    ! interpolation.
    real(SRP) :: percept_w

    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    !> ### Implementation details ###
    !> #### Overview ####
    !> Each of the perceptions is weighted by an attention factor. The
    !! attention factor is in turn modulated (weighted) by the current
    !! Global Organismic State (GOS). When the current arousal is relatively
    !! high, all irrelevant perceptions are effectively filtered out
    !! (weighted by near-zero) and do not (largely) contribute to the GOS
    !! at the *next* time step. For example, the agent just does not "see"
    !! food items when it is in a high fear state.
    !! @image html img_doxy_aha2-attention-modulation.svg
    !! @image latex img_doxy_aha2-attention-modulation.eps "Perception weights" width=14cm
    !!
    !! Thus, perception is weighted by the attention suppression factor
    !! separately for each motivational (emotional) state according to the
    !! scheme below:
    !! @image html aha_attention_modulate_weights.svg "Attention suppression"
    !! @image latex aha_attention_modulate_weights.eps "Attention suppression" width=10cm
    !! Also see @ref aha_buildblocks_cogn_arch "Cognitive architecture" section.

    !> #### Specific details ####
    !> **First**, we calculate the attention weight given to all non-dominant
    !! perceptions via nonlinear interpolation. Interpolation is based on the
    !! grid defined by two parameters: `ATTENTION_MODULATION_CURVE_ABSCISSA`
    !! and `ATTENTION_MODULATION_CURVE_ORDINATE`.
    !! @note Interpolation plot can be produced using this command, assuming
    !!       the plotting tools are installed on the system.
    !!       @verbatim
    !!          htintrpl.exe [0.0, 0.3, 0.5, 1.0] [1.0, 0.98, 0.9, 0.0] [2]
    !!       @endverbatim
    !>
    percept_w = DDPINTERPOL( ATTENTION_MODULATION_CURVE_ABSCISSA,            &
                             ATTENTION_MODULATION_CURVE_ORDINATE,            &
                             this%gos_arousal )

    !> Interpolation plots can be saved in the @ref intro_debug_mode
    !! "debug mode" using this plotting command:
    !! commondata::debug_interpolate_plot_save().
    !! @warning Involves **huge** number of plots, should normally be
    !!          disabled.
    call debug_interpolate_plot_save(                                         &
            grid_xx=ATTENTION_MODULATION_CURVE_ABSCISSA,                      &
            grid_yy=ATTENTION_MODULATION_CURVE_ORDINATE,                      &
            ipol_value=this%gos_arousal, algstr="DDPINTERPOL",                &
            output_file="plot_debug_attention_modulation_" //                 &
                        TOSTR(Global_Time_Step_Model_Current) //              &
                        MMDD // "_a_"// trim(this%individ_label()) //         &
                        "_" // RAND_STRING(LABEL_LENGTH, LABEL_CST,LABEL_CEN) &
                        // PS )

    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !> **Second**, we reset the attention weights for the **dominant GOS
    !! state** to their **default** parameter values whereas for all other
    !! states, to the **recalculated** `percept_w` modulated/weighted
    !! value. The the_neurobio::percept_components_motiv::attention_init()
    !! method is used to adjust the attention weights.
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! The *dominant* state is **hunger**:
    RESET_DOMINANT: if ( this%motivations%hunger%is_dominant() ) then

      ! @note Dominant is **hunger**.
      call this%motivations%hunger%attention_weight%attention_init            &
          (weight_light    = ATTENTION_SWITCH_HUNGER_LIGHT,                   &
           weight_depth    = ATTENTION_SWITCH_HUNGER_DEPTH,                   &
           weight_food_dir = ATTENTION_SWITCH_HUNGER_FOOD_DIR,                &
           weight_food_mem = ATTENTION_SWITCH_HUNGER_FOOD_MEM,                &
           weight_conspec  = ATTENTION_SWITCH_HUNGER_CONSPEC,                 &
           weight_pred_dir = ATTENTION_SWITCH_HUNGER_PRED_DIR,                &
           weight_predator = ATTENTION_SWITCH_HUNGER_PREDATOR,                &
           weight_stomach  = ATTENTION_SWITCH_HUNGER_STOMACH,                 &
           weight_bodymass = ATTENTION_SWITCH_HUNGER_BODYMASS,                &
           weight_energy   = ATTENTION_SWITCH_HUNGER_ENERGY,                  &
           weight_age      = ATTENTION_SWITCH_HUNGER_AGE,                     &
           weight_reprfac  = ATTENTION_SWITCH_HUNGER_REPRFAC )

      call this%motivations%fear_defence%attention_weight%attention_init      &
          (weight_light    = ATTENTION_SWITCH_AVOID_ACT_LIGHT * percept_w,    &
           weight_depth    = ATTENTION_SWITCH_AVOID_ACT_DEPTH * percept_w,    &
           weight_food_dir = ATTENTION_SWITCH_AVOID_ACT_FOOD_DIR * percept_w, &
           weight_food_mem = ATTENTION_SWITCH_AVOID_ACT_FOOD_MEM * percept_w, &
           weight_conspec  = ATTENTION_SWITCH_AVOID_ACT_CONSPEC * percept_w,  &
           weight_pred_dir = ATTENTION_SWITCH_AVOID_ACT_PRED_DIR * percept_w, &
           weight_predator = ATTENTION_SWITCH_AVOID_ACT_PREDATOR * percept_w, &
           weight_stomach  = ATTENTION_SWITCH_AVOID_ACT_STOMACH * percept_w,  &
           weight_bodymass = ATTENTION_SWITCH_AVOID_ACT_BODYMASS * percept_w, &
           weight_energy   = ATTENTION_SWITCH_AVOID_ACT_ENERGY * percept_w,   &
           weight_age      = ATTENTION_SWITCH_AVOID_ACT_AGE * percept_w,      &
           weight_reprfac  = ATTENTION_SWITCH_AVOID_ACT_REPRFAC * percept_w )

      call this%motivations%reproduction%attention_weight%attention_init      &
          (weight_light    = ATTENTION_SWITCH_REPRODUCE_LIGHT * percept_w,    &
           weight_depth    = ATTENTION_SWITCH_REPRODUCE_DEPTH * percept_w,    &
           weight_food_dir = ATTENTION_SWITCH_REPRODUCE_FOOD_DIR * percept_w, &
           weight_food_mem = ATTENTION_SWITCH_REPRODUCE_FOOD_MEM * percept_w, &
           weight_conspec  = ATTENTION_SWITCH_REPRODUCE_CONSPEC * percept_w,  &
           weight_pred_dir = ATTENTION_SWITCH_REPRODUCE_PRED_DIR * percept_w, &
           weight_predator = ATTENTION_SWITCH_REPRODUCE_PREDATOR * percept_w, &
           weight_stomach  = ATTENTION_SWITCH_REPRODUCE_STOMACH * percept_w,  &
           weight_bodymass = ATTENTION_SWITCH_REPRODUCE_BODYMASS * percept_w, &
           weight_energy   = ATTENTION_SWITCH_REPRODUCE_ENERGY * percept_w,   &
           weight_age      = ATTENTION_SWITCH_REPRODUCE_AGE * percept_w,      &
           weight_reprfac  = ATTENTION_SWITCH_REPRODUCE_REPRFAC * percept_w )

    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! The *dominant* state is **fear_defence**:
    else if ( this%motivations%fear_defence%is_dominant() ) then RESET_DOMINANT

      call this%motivations%hunger%attention_weight%attention_init            &
          (weight_light    = ATTENTION_SWITCH_HUNGER_LIGHT * percept_w,       &
           weight_depth    = ATTENTION_SWITCH_HUNGER_DEPTH * percept_w,       &
           weight_food_dir = ATTENTION_SWITCH_HUNGER_FOOD_DIR * percept_w,    &
           weight_food_mem = ATTENTION_SWITCH_HUNGER_FOOD_MEM * percept_w,    &
           weight_conspec  = ATTENTION_SWITCH_HUNGER_CONSPEC * percept_w,     &
           weight_pred_dir = ATTENTION_SWITCH_HUNGER_PRED_DIR * percept_w,    &
           weight_predator = ATTENTION_SWITCH_HUNGER_PREDATOR * percept_w,    &
           weight_stomach  = ATTENTION_SWITCH_HUNGER_STOMACH * percept_w,     &
           weight_bodymass = ATTENTION_SWITCH_HUNGER_BODYMASS * percept_w,    &
           weight_energy   = ATTENTION_SWITCH_HUNGER_ENERGY * percept_w,      &
           weight_age      = ATTENTION_SWITCH_HUNGER_AGE * percept_w,         &
           weight_reprfac  = ATTENTION_SWITCH_HUNGER_REPRFAC * percept_w  )

      ! @note Dominant is **fear_defence**.
      call this%motivations%fear_defence%attention_weight%attention_init      &
          (weight_light    = ATTENTION_SWITCH_AVOID_ACT_LIGHT,                &
           weight_depth    = ATTENTION_SWITCH_AVOID_ACT_DEPTH,                &
           weight_food_dir = ATTENTION_SWITCH_AVOID_ACT_FOOD_DIR,             &
           weight_food_mem = ATTENTION_SWITCH_AVOID_ACT_FOOD_MEM,             &
           weight_conspec  = ATTENTION_SWITCH_AVOID_ACT_CONSPEC,              &
           weight_pred_dir = ATTENTION_SWITCH_AVOID_ACT_PRED_DIR,             &
           weight_predator = ATTENTION_SWITCH_AVOID_ACT_PREDATOR,             &
           weight_stomach  = ATTENTION_SWITCH_AVOID_ACT_STOMACH,              &
           weight_bodymass = ATTENTION_SWITCH_AVOID_ACT_BODYMASS,             &
           weight_energy   = ATTENTION_SWITCH_AVOID_ACT_ENERGY,               &
           weight_age      = ATTENTION_SWITCH_AVOID_ACT_AGE,                  &
           weight_reprfac  = ATTENTION_SWITCH_AVOID_ACT_REPRFAC )

      call this%motivations%reproduction%attention_weight%attention_init      &
          (weight_light    = ATTENTION_SWITCH_REPRODUCE_LIGHT * percept_w,    &
           weight_depth    = ATTENTION_SWITCH_REPRODUCE_DEPTH * percept_w,    &
           weight_food_dir = ATTENTION_SWITCH_REPRODUCE_FOOD_DIR * percept_w, &
           weight_food_mem = ATTENTION_SWITCH_REPRODUCE_FOOD_MEM * percept_w, &
           weight_conspec  = ATTENTION_SWITCH_REPRODUCE_CONSPEC * percept_w,  &
           weight_pred_dir = ATTENTION_SWITCH_REPRODUCE_PRED_DIR * percept_w, &
           weight_predator = ATTENTION_SWITCH_REPRODUCE_PREDATOR * percept_w, &
           weight_stomach  = ATTENTION_SWITCH_REPRODUCE_STOMACH * percept_w,  &
           weight_bodymass = ATTENTION_SWITCH_REPRODUCE_BODYMASS * percept_w, &
           weight_energy   = ATTENTION_SWITCH_REPRODUCE_ENERGY * percept_w,   &
           weight_age      = ATTENTION_SWITCH_REPRODUCE_AGE * percept_w,      &
           weight_reprfac  = ATTENTION_SWITCH_REPRODUCE_REPRFAC * percept_w )

    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! The *dominant* state is **reproduction**:
    else if ( this%motivations%reproduction%is_dominant() ) then RESET_DOMINANT

      call this%motivations%hunger%attention_weight%attention_init            &
          (weight_light    = ATTENTION_SWITCH_HUNGER_LIGHT * percept_w,       &
           weight_depth    = ATTENTION_SWITCH_HUNGER_DEPTH * percept_w,       &
           weight_food_dir = ATTENTION_SWITCH_HUNGER_FOOD_DIR * percept_w,    &
           weight_food_mem = ATTENTION_SWITCH_HUNGER_FOOD_MEM * percept_w,    &
           weight_conspec  = ATTENTION_SWITCH_HUNGER_CONSPEC * percept_w,     &
           weight_pred_dir = ATTENTION_SWITCH_HUNGER_PRED_DIR * percept_w,    &
           weight_predator = ATTENTION_SWITCH_HUNGER_PREDATOR * percept_w,    &
           weight_stomach  = ATTENTION_SWITCH_HUNGER_STOMACH * percept_w,     &
           weight_bodymass = ATTENTION_SWITCH_HUNGER_BODYMASS * percept_w,    &
           weight_energy   = ATTENTION_SWITCH_HUNGER_ENERGY * percept_w,      &
           weight_age      = ATTENTION_SWITCH_HUNGER_AGE * percept_w,         &
           weight_reprfac  = ATTENTION_SWITCH_HUNGER_REPRFAC * percept_w )

      call this%motivations%fear_defence%attention_weight%attention_init      &
          (weight_light    = ATTENTION_SWITCH_AVOID_ACT_LIGHT * percept_w,    &
           weight_depth    = ATTENTION_SWITCH_AVOID_ACT_DEPTH * percept_w,    &
           weight_food_dir = ATTENTION_SWITCH_AVOID_ACT_FOOD_DIR * percept_w, &
           weight_food_mem = ATTENTION_SWITCH_AVOID_ACT_FOOD_MEM * percept_w, &
           weight_conspec  = ATTENTION_SWITCH_AVOID_ACT_CONSPEC * percept_w,  &
           weight_pred_dir = ATTENTION_SWITCH_AVOID_ACT_PRED_DIR * percept_w, &
           weight_predator = ATTENTION_SWITCH_AVOID_ACT_PREDATOR * percept_w, &
           weight_stomach  = ATTENTION_SWITCH_AVOID_ACT_STOMACH * percept_w,  &
           weight_bodymass = ATTENTION_SWITCH_AVOID_ACT_BODYMASS * percept_w, &
           weight_energy   = ATTENTION_SWITCH_AVOID_ACT_ENERGY * percept_w,   &
           weight_age      = ATTENTION_SWITCH_AVOID_ACT_AGE * percept_w,      &
           weight_reprfac  = ATTENTION_SWITCH_AVOID_ACT_REPRFAC * percept_w )

      ! @note Dominant **reproduction**.
      call this%motivations%reproduction%attention_weight%attention_init      &
          (weight_light    = ATTENTION_SWITCH_REPRODUCE_LIGHT,                &
           weight_depth    = ATTENTION_SWITCH_REPRODUCE_DEPTH,                &
           weight_food_dir = ATTENTION_SWITCH_REPRODUCE_FOOD_DIR,             &
           weight_food_mem = ATTENTION_SWITCH_REPRODUCE_FOOD_MEM,             &
           weight_conspec  = ATTENTION_SWITCH_REPRODUCE_CONSPEC,              &
           weight_pred_dir = ATTENTION_SWITCH_REPRODUCE_PRED_DIR,             &
           weight_predator = ATTENTION_SWITCH_REPRODUCE_PREDATOR,             &
           weight_stomach  = ATTENTION_SWITCH_REPRODUCE_STOMACH,              &
           weight_bodymass = ATTENTION_SWITCH_REPRODUCE_BODYMASS,             &
           weight_energy   = ATTENTION_SWITCH_REPRODUCE_ENERGY,               &
           weight_age      = ATTENTION_SWITCH_REPRODUCE_AGE,                  &
           weight_reprfac  = ATTENTION_SWITCH_REPRODUCE_REPRFAC )

    end if RESET_DOMINANT

  end subroutine gos_attention_modulate_weights

  !-----------------------------------------------------------------------------
  !> Calculate the number of food items in the perception object that are
  !! located **below** the actor agent.
  elemental function perception_food_items_below_calculate(this)              &
                                                          result (number_below)
    class(PERCEPTION), intent(in) :: this
    !> @return The number of food items within the perception object that are
    !!         located below (under) the actor agent.
    integer :: number_below

    !> ### Implementation details ###
    !> First, initialise the counter to zero.
    number_below = 0

    !> Then, check if the agent has any food items in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_food()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one food item
    !! in the perception object. Calculate food items within the
    !! perception that are *below* the agent.
    number_below = count( this%perceive_food%foods_seen .below. this )

  end function perception_food_items_below_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the number of food items in the perception object that are
  !! located **below** the actor agent within a specific vertical horizon
  !! [hz_lower,hz_upper]. The horizon limits are relative, in that they start
  !! from the depth position of the `this` actor agent:
  !! [z+hz_lower, z+hz_upper].
  !! @image html aha_percept_food_up_lower.svg
  !! @image latex aha_percept_food_up_lower.eps "Calculation of food items above and below" width=8cm
  elemental function perception_food_items_below_horiz_calculate( this,       &
                                                                  hz_lower,   &
                                                                  hz_upper  ) &
                                                          result (number_below)
    class(PERCEPTION), intent(in) :: this
    !> @param[in] hz_lower The upper limit for the vertical horizon
    real(SRP), intent(in) :: hz_lower
    !> @param[in] hz_upper The lower limit for the vertical horizon
    real(SRP), intent(in) :: hz_upper
    !> @return The number of food items within the perception object that are
    !!         located below (under) the actor agent.
    integer :: number_below

    ! Local counter
    integer :: i

    !> ### Implementation details ###
    !> First, initialise the counter to zero.
    number_below = 0

    !> Then, check if the agent has any food items in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_food()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one food item
    !! in the perception object. Loop through the food items within the later
    !! and calculate the total number.
    do concurrent (i=1:this%perceive_food%get_count())
    ! Can use the operator .within.
    ! if ( this%perceive_food%foods_seen(i)%dpos() .within.                   &
    !      [this%dpos()+hz_lower, this%dpos()+hz_upper] ) then
      if ( is_within( this%perceive_food%foods_seen(i)%dpos(),                &
                      this%dpos() + hz_lower,                                 &
                      this%dpos() + hz_upper                   ) )  then
        number_below = number_below + 1
      end if
    end do

  end function perception_food_items_below_horiz_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the average mass of a food item from all the items in the
  !! current perception object that are **below** the actor agent.
  elemental function perception_food_mass_below_calculate(this)               &
                                                      result (mean_mass_below)
    class(PERCEPTION), intent(in) :: this
    !> @return Average mass of food items within the perception object that are
    !!         located below (under) the actor agent.
    real(SRP) ::mean_mass_below

    ! Local counters
    integer :: i, n_counter

    !> ### Implementation details ###
    !> First, initialise the return average mass and the counter
    !! for calculating the average both to zero.
    mean_mass_below = 0.0_SRP
    n_counter = 0

    !> Then, check if the agent has any food items in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_food()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one food item
    !! in the perception object. Calculation of the average mass of the food
    !! items **below** is done by concurrent looping through the food items
    !! within the perception object.
    do concurrent (i=1:this%perceive_food%get_count())
      !> This is done by checking the condition:
      !! @verbatim
      !!   if ( food_item .below. this ) ...
      !! @endverbatim
      !! @note This uses the user defined operator `.below.` that is
      !!       implemented in `the_environment` module.
      if ( this%perceive_food%foods_seen(i) .below. this )  then
        !> The average mass of the food items is calculated using the food
        !! items mass values returned by the function `get_mass`
        !! (`the_environment::food_item::get_mass()`) .
        mean_mass_below =                                                     &
                mean_mass_below + this%perceive_food%foods_seen(i)%get_mass()
        n_counter = n_counter + 1
      end if
    end do

    !> Final average value is calculated, obviously, by division of the
    !! total mass by the total count. In case the count is zero, also
    !! return zero mean.
    if ( n_counter > 0 ) then
      mean_mass_below = mean_mass_below / real(n_counter, SRP)
    else
      mean_mass_below = 0.0_SRP
    end if

  end function perception_food_mass_below_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the average mass of a food item from all the items in the
  !! current perception object that are **below** the actor agent within a
  !! specific vertical horizon [hz_lower,hz_upper]. The horizon limits are
  !! relative, in that they start from the depth position of the `this` actor
  !! agent: [z+hz_lower, z+hz_upper].
  !! @image html aha_percept_food_up_lower.svg
  !! @image latex aha_percept_food_up_lower.eps "Calculation of food items above and below" width=8cm
  elemental function perception_food_mass_below_horiz_calculate(this,         &
                                                                  hz_lower,   &
                                                                  hz_upper )  &
                                                      result (mean_mass_below)
    class(PERCEPTION), intent(in) :: this
    !> @param[in] hz_lower The upper limit for the vertical horizon
    real(SRP), intent(in) :: hz_lower
    !> @param[in] hz_upper The lower limit for the vertical horizon
    real(SRP), intent(in) :: hz_upper
    !> @return Average mass of food items within the perception object that are
    !!         located below (under) the actor agent.
    real(SRP) ::mean_mass_below

    ! Local counters
    integer :: i, n_counter

    !> ### Implementation details ###
    !> First, initialise the return average mass and the counter
    !! for calculating the average both to zero.
    mean_mass_below = 0.0_SRP
    n_counter = 0

    !> Then, check if the agent has any food items in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_food()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one food item
    !! in the perception object. Calculation of the average mass of the food
    !! items **below** is done by concurrent looping through the food items
    !! within the perception object.
    do concurrent (i=1:this%perceive_food%get_count())
    ! Can use the operator .within.
    ! if ( this%perceive_food%foods_seen(i)%dpos() .within.                   &
    !      [this%dpos()+hz_lower, this%dpos()+hz_upper] ) then
      if ( is_within( this%perceive_food%foods_seen(i)%dpos(),                &
                      this%dpos() + hz_lower,                                 &
                      this%dpos() + hz_upper                   ) )  then
        !> The average mass of the food items is calculated using the food
        !! items mass values returned by the function `get_mass`
        !! (`the_environment::food_item::get_mass()`) .
        mean_mass_below =                                                     &
                mean_mass_below + this%perceive_food%foods_seen(i)%get_mass()
        n_counter = n_counter + 1
      end if
    end do

    !> Final average value is calculated, obviously, by division of the
    !! total mass by the total count. In case the count is zero, also
    !! return zero mean.
    if ( n_counter > 0 ) then
      mean_mass_below = mean_mass_below / real(n_counter, SRP)
    else
      mean_mass_below = 0.0_SRP
    end if

  end function perception_food_mass_below_horiz_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the number of food items in the perception object that are
  !! located **above** the actor agent.
  elemental function perception_food_items_above_calculate(this)              &
                                                          result (number_above)
    class(PERCEPTION), intent(in) :: this
    !> @return The number of food items within the perception object that are
    !!         located above (over) the actor agent.
    integer :: number_above

    !> ### Implementation details ###
    !> First, initialise the counter to zero.
    number_above = 0

    !> Then, check if the agent has any food items in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_food()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one food item
    !! in the perception object. Calculate food items within the
    !! perception that are *above* the agent.
    number_above = count( this%perceive_food%foods_seen .above. this )

  end function perception_food_items_above_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the number of food items in the perception object that are
  !! located **above** the actor agent within a specific vertical horizon
  !! [hz_lower,hz_upper]. The horizon limits are relative, in that they start
  !! from the depth position of the `this` actor agent:
  !! [z-hz_upper, z-hz_upper].
  !! @image html aha_percept_food_up_lower.svg
  !! @image latex aha_percept_food_up_lower.eps "Calculation of food items above and below" width=8cm
  elemental function perception_food_items_above_horiz_calculate( this,       &
                                                                  hz_lower,   &
                                                                  hz_upper  ) &
                                                          result (number_above)
    class(PERCEPTION), intent(in) :: this
    !> @param[in] hz_lower The upper limit for the vertical horizon
    real(SRP), intent(in) :: hz_lower
    !> @param[in] hz_upper The lower limit for the vertical horizon
    real(SRP), intent(in) :: hz_upper
    !> @return The number of food items within the perception object that are
    !!         located above (over) the actor agent.
    integer :: number_above

    ! Local counter
    integer :: i

    !> ### Implementation details ###
    !> First, initialise the counter to zero.
    number_above = 0

    !> Then, check if the agent has any food items in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_food()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one food item
    !! in the perception object. Loop through the food items within the later
    !! and calculate the total number.
    do concurrent (i=1:this%perceive_food%get_count())
    ! Can use the operator .within.
    ! if ( this%perceive_food%foods_seen(i)%dpos() .within.                   &
    !      [this%dpos()-hz_upper, this%dpos()-hz_lower] ) then
      if ( is_within( this%perceive_food%foods_seen(i)%dpos(),                &
                      this%dpos() - hz_upper,                                 &
                      this%dpos() - hz_lower                   ) )  then
        number_above = number_above + 1
      end if
    end do

  end function perception_food_items_above_horiz_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the average mass of a food item from all the items in the
  !! current perception object that are **above** the actor agent.
  elemental function perception_food_mass_above_calculate(this)               &
                                                        result (mean_mass_above)
    class(PERCEPTION), intent(in) :: this
    real(SRP) ::mean_mass_above

    ! Local counters
    integer :: i, n_counter

    !> ### Implementation details ###
    !> First, initialise the return average mass and the counter
    !! for calculating the average both to zero.
    mean_mass_above = 0.0_SRP
    n_counter = 0

    !> Then, check if the agent has any food items in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_food()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one food item
    !! in the perception object. Calculation of the average mass of the food
    !! items **above** is done by concurrent looping through the food items
    !! within the perception object.
    do concurrent (i=1:this%perceive_food%get_count())
      !> This is done by checking the condition:
      !! @verbatim
      !!   if ( food_item .above. this ) ...
      !! @endverbatim
      !! @note This uses the user defined operator `.above.` that is
      !!       implemented in `the_environment` module.
      if ( this%perceive_food%foods_seen(i) .above. this )  then
        !> The average mass of the food items is calculated using the food
        !! items mass values returned by the function `get_mass`
        !! (`the_environment::food_item::get_mass()`) .
        mean_mass_above =                                                     &
                mean_mass_above + this%perceive_food%foods_seen(i)%get_mass()
        n_counter = n_counter + 1
      end if
    end do

    !> Final average value is calculated, obviously, by division of the
    !! total mass by the total count. In case the count is zero, also
    !! return zero mean.
    if ( n_counter > 0 ) then
      mean_mass_above = mean_mass_above / real(n_counter, SRP)
    else
      mean_mass_above = 0.0_SRP
    end if

  end function perception_food_mass_above_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the average mass of a food item from all the items in the
  !! current perception object that are **above** the actor agent within a
  !! specific vertical horizon [hz_lower,hz_upper]. The horizon limits are
  !! relative, in that they start from the depth position of the `this` actor
  !! agent: [z-hz_upper, z-hz_upper].
  !! @image html aha_percept_food_up_lower.svg
  !! @image latex aha_percept_food_up_lower.eps "Calculation of food items above and below" width=8cm
  elemental function perception_food_mass_above_horiz_calculate(this,         &
                                                                  hz_lower,   &
                                                                  hz_upper )  &
                                                      result (mean_mass_above)
    class(PERCEPTION), intent(in) :: this
    !> @param[in] hz_lower The upper limit for the vertical horizon
    real(SRP), intent(in) :: hz_lower
    !> @param[in] hz_upper The lower limit for the vertical horizon
    real(SRP), intent(in) :: hz_upper
    !> @return Average mass of food items within the perception object that are
    !!         located above (over) the actor agent.
    real(SRP) ::mean_mass_above

    ! Local counters
    integer :: i, n_counter

    !> ### Implementation details ###
    !> First, initialise the return average mass and the counter
    !! for calculating the average both to zero.
    mean_mass_above = 0.0_SRP
    n_counter = 0

    !> Then, check if the agent has any food items in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_food()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one food item
    !! in the perception object. Calculation of the average mass of the food
    !! items **above** is done by concurrent looping through the food items
    !! within the perception object.
    do concurrent (i=1:this%perceive_food%get_count())
    ! Can use the operator .within.
    ! if ( this%perceive_food%foods_seen(i)%dpos() .within.                   &
    !      [this%dpos()-hz_upper, this%dpos()-hz_lower] ) then
      if ( is_within( this%perceive_food%foods_seen(i)%dpos(),                &
                      this%dpos() - hz_upper,                                 &
                      this%dpos() - hz_lower                   ) )  then
        !> The average mass of the food items is calculated using the food
        !! items mass values returned by the function `get_mass`
        !! (`the_environment::food_item::get_mass()`) .
        mean_mass_above =                                                     &
                mean_mass_above + this%perceive_food%foods_seen(i)%get_mass()
        n_counter = n_counter + 1
      end if
    end do

    !> Final average value is calculated, obviously, by division of the
    !! total mass by the total count. In case the count is zero, also
    !! return zero mean.
    if ( n_counter > 0 ) then
      mean_mass_above = mean_mass_above / real(n_counter, SRP)
    else
      mean_mass_above = 0.0_SRP
    end if

  end function perception_food_mass_above_horiz_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the number of conspecifics in the perception object that are
  !! located **below** the actor agent.
  elemental function perception_conspecifics_below_calculate(this)            &
                                                          result (number_below)
    class(PERCEPTION), intent(in) :: this
    !> @return The number of conspecifics within the perception object that are
    !!         located below (under) the actor agent.
    integer :: number_below

    !> ### Implementation details ###
    !> First, initialise the counter to zero.
    number_below = 0

    !> Then, check if the agent has any conspecifics in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_consp()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one conspecific
    !! in the perception object. Loop through the conspecifics and count
    !! their total number.
    number_below = count( this%perceive_consp%conspecifics_seen .below. this )

  end function perception_conspecifics_below_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the number of conspecifics in the perception object that are
  !! located **above** the actor agent.
  elemental function perception_conspecifics_above_calculate(this)            &
                                                          result (number_above)
    class(PERCEPTION), intent(in) :: this
    !> @return The number of conspecifics within the perception object that are
    !!         located above (over) the actor agent.
    integer :: number_above

    !> ### Implementation details ###
    !> First, initialise the counter to zero.
    number_above = 0

    !> Then, check if the agent has any conspecifics in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_consp()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one conspecific
    !! in the perception object. Loop through the conspecifics  and count
    !! their total number.
    number_above = count ( this%perceive_consp%conspecifics_seen .above. this )

  end function perception_conspecifics_above_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the number of conspecifics in the perception object that are
  !! located **below** the actor agent within a specific vertical horizon
  !! [hz_lower,hz_upper]. The horizon limits are relative, in that they start
  !! from the depth position of the `this` actor agent:
  !! [z+hz_lower, z+hz_upper].
  elemental function perception_conspecifics_below_horiz_calculate( this,     &
                                                                    hz_lower, &
                                                                    hz_upper )&
                                                          result (number_below)
    class(PERCEPTION), intent(in) :: this
    !> @param[in] hz_lower The upper limit for the vertical horizon
    real(SRP), intent(in) :: hz_lower
    !> @param[in] hz_upper The lower limit for the vertical horizon
    real(SRP), intent(in) :: hz_upper
    !> @return The number of conspecifics within the perception object that are
    !!         located below (under) the actor agent.
    integer :: number_below

    ! Local counter
    integer :: i

    !> ### Implementation details ###
    !> First, initialise the counter to zero.
    number_below = 0

    !> Then, check if the agent has any conspecifics in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_consp()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one conspecific
    !! in the perception object. Loop through the conspecifics within the later
    !! and calculate their number.
    do concurrent (i=1:this%perceive_consp%get_count())
      if ( this%perceive_consp%conspecifics_seen(i)%dpos() .within.           &
           [this%dpos()+hz_lower, this%dpos()+hz_upper] ) then
        number_below = number_below + 1
      end if
    end do

  end function perception_conspecifics_below_horiz_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the number of conspecifics in the perception object that are
  !! located **above** the actor agent within a specific vertical horizon
  !! [hz_lower,hz_upper]. The horizon limits are relative, in that they start
  !! from the depth position of the `this` actor agent:
  !! [z-hz_upper, z-hz_upper].
  elemental function perception_conspecifics_above_horiz_calculate( this,     &
                                                                    hz_lower, &
                                                                    hz_upper )&
                                                          result (number_above)
    class(PERCEPTION), intent(in) :: this
    !> @param[in] hz_lower The upper limit for the vertical horizon
    real(SRP), intent(in) :: hz_lower
    !> @param[in] hz_upper The lower limit for the vertical horizon
    real(SRP), intent(in) :: hz_upper
    !> @return The number of conspecifics within the perception object that are
    !!         located above (over) the actor agent.
    integer :: number_above

    ! Local counter
    integer :: i

    !> ### Implementation details ###
    !> First, initialise the counter to zero.
    number_above = 0

    !> Then, check if the agent has any conspecifics in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_food()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one conspecific
    !! in the perception object. Loop through the conspecifics within and
    !! calculate their number.
    do concurrent (i=1:this%perceive_consp%get_count())
      if ( this%perceive_consp%conspecifics_seen(i)%dpos() .within.           &
           [this%dpos()-hz_upper, this%dpos()-hz_lower] ) then
        number_above = number_above + 1
      end if
    end do

  end function perception_conspecifics_above_horiz_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the number of predators in the perception object that are
  !! located **below** the actor agent.
  elemental function perception_predator_below_calculate(this)            &
                                                          result (number_below)
    class(PERCEPTION), intent(in) :: this
    !> @return The number of predators within the perception object that are
    !!         located below (under) the actor agent.
    integer :: number_below

    !> ### Implementation details ###
    !> First, initialise the counter to zero.
    number_below = 0

    !> Then, check if the agent has any predators in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_pred()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one conspecific
    !! in the perception object. Loop through the predators and count
    !! their total number.
    number_below = count( this%perceive_predator%predators_seen .below. this )

  end function perception_predator_below_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the number of predators in the perception object that are
  !! located **above** the actor agent.
  elemental function perception_predator_above_calculate(this)            &
                                                          result (number_above)
    class(PERCEPTION), intent(in) :: this
    !> @return The number of predators within the perception object that are
    !!         located above (over) the actor agent.
    integer :: number_above

    !> ### Implementation details ###
    !> First, initialise the counter to zero.
    number_above = 0

    !> Then, check if the agent has any predators in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_pred()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one conspecific
    !! in the perception object. Loop through the predators  and count
    !! their total number.
    number_above = count( this%perceive_predator%predators_seen .above. this )

  end function perception_predator_above_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the number of predators in the perception object that are
  !! located **below** the actor agent within a specific vertical horizon
  !! [hz_lower,hz_upper]. The horizon limits are relative, in that they start
  !! from the depth position of the `this` actor agent:
  !! [z+hz_lower, z+hz_upper].
  elemental function perception_predator_below_horiz_calculate( this,     &
                                                                    hz_lower, &
                                                                    hz_upper )&
                                                          result (number_below)
    class(PERCEPTION), intent(in) :: this
    !> @param[in] hz_lower The upper limit for the vertical horizon
    real(SRP), intent(in) :: hz_lower
    !> @param[in] hz_upper The lower limit for the vertical horizon
    real(SRP), intent(in) :: hz_upper
    !> @return The number of predators within the perception object that are
    !!         located below (under) the actor agent.
    integer :: number_below

    ! Local counter
    integer :: i

    !> ### Implementation details ###
    !> First, initialise the counter to zero.
    number_below = 0

    !> Then, check if the agent has any predators in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_pred()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one conspecific
    !! in the perception object. Loop through the predators within the later
    !! and calculate their number.
    do concurrent (i=1:this%perceive_predator%get_count())
      if ( this%perceive_predator%predators_seen(i)%dpos() .within.           &
           [this%dpos()+hz_lower, this%dpos()+hz_upper] ) then
        number_below = number_below + 1
      end if
    end do

  end function perception_predator_below_horiz_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the number of predators in the perception object that are
  !! located **above** the actor agent within a specific vertical horizon
  !! [hz_lower,hz_upper]. The horizon limits are relative, in that they start
  !! from the depth position of the `this` actor agent:
  !! [z-hz_upper, z-hz_upper].
  elemental function perception_predator_above_horiz_calculate( this,     &
                                                                    hz_lower, &
                                                                    hz_upper )&
                                                          result (number_above)
    class(PERCEPTION), intent(in) :: this
    !> @param[in] hz_lower The upper limit for the vertical horizon
    real(SRP), intent(in) :: hz_lower
    !> @param[in] hz_upper The lower limit for the vertical horizon
    real(SRP), intent(in) :: hz_upper
    !> @return The number of predators within the perception object that are
    !!         located above (over) the actor agent.
    integer :: number_above

    ! Local counter
    integer :: i

    !> ### Implementation details ###
    !> First, initialise the counter to zero.
    number_above = 0

    !> Then, check if the agent has any predators in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_pred()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one conspecific
    !! in the perception object. Loop through the predators within and
    !! calculate their number.
    do concurrent (i=1:this%perceive_predator%get_count())
      if ( this%perceive_predator%predators_seen(i)%dpos() .within.           &
           [this%dpos()-hz_upper, this%dpos()-hz_lower] ) then
        number_above = number_above + 1
      end if
    end do

  end function perception_predator_above_horiz_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the average distance to all food items in the current
  !! perception object that are **below** the actor agent.
  elemental function perception_food_dist_below_calculate(this)               &
                                                      result (mean_dist)
    class(PERCEPTION), intent(in) :: this
    !> @return The average distance to food items within the perception
    !!         object that are located below (under) the actor agent.
    real(SRP) ::mean_dist

    ! Local counters
    integer :: i, n_counter

    !> ### Implementation details ###
    !> First, initialise the return average and the counter to zero.
    mean_dist = 0.0_SRP
    n_counter = 0

    !> Then, check if the agent has any food items in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_food()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one food item
    !! in the perception object. Calculation of the average distance to the
    !! food items **below** is done by concurrent looping through the food
    !! items within the perception object and calculating the distance from
    !! the agent.
    do concurrent (i=1:this%perceive_food%get_count())
      !> This is done by checking the condition:
      !! @verbatim
      !!   if ( food_item .below. this ) ...
      !! @endverbatim
      if ( this%perceive_food%foods_seen(i) .below. this )  then
        mean_dist =                                                           &
                !mean_dist + this%perceive_food%foods_seen(i)%distance( this )
                mean_dist + this%distance( this%perceive_food%foods_seen(i) )
        n_counter = n_counter + 1
      end if
    end do

    !> Final average value is calculated, obviously, by division of the
    !! total distance by the count. In case the count is zero, also
    !! return commondata::missing mean. Note that zero is not returned here
    !! because zero distance to food item would result in the highest
    !! probability of  capture which is not what is intended (zero probability
    !! should be  invoked for null food items).
    if ( n_counter > 0 ) then
      mean_dist = mean_dist / real(n_counter, SRP)
    else
      mean_dist = MISSING
    end if

  end function perception_food_dist_below_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the average distance to all food items in the current
  !! perception object that are **above** the actor agent.
  elemental function perception_food_dist_above_calculate(this)               &
                                                      result (mean_dist)
    class(PERCEPTION), intent(in) :: this
    !> @return The average distance to food items within the perception
    !!         object that are located above (over) the actor agent.
    real(SRP) ::mean_dist

    ! Local counters
    integer :: i, n_counter

    !> ### Implementation details ###
    !> First, initialise the return average and the counter to zero.
    mean_dist = 0.0_SRP
    n_counter = 0

    !> Then, check if the agent has any food items in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_food()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one food item
    !! in the perception object. Calculation of the average distance to the
    !! food items **above** is done by concurrent looping through the food
    !! items within the perception object and calculating the distance from
    !! the agent.
    do concurrent (i=1:this%perceive_food%get_count())
      !> This is done by checking the condition:
      !! @verbatim
      !!   if ( food_item .above. this ) ...
      !! @endverbatim
      if ( this%perceive_food%foods_seen(i) .above. this )  then
        mean_dist =                                                           &
                !mean_dist + this%perceive_food%foods_seen(i)%distance( this )
                mean_dist + this%distance( this%perceive_food%foods_seen(i) )
        n_counter = n_counter + 1
      end if
    end do

    !> Final average value is calculated, obviously, by division of the
    !! total distance by the count. In case the count is zero, also
    !! return commondata::missing mean. Note that zero is not returned here
    !! because zero distance to food item would result in the highest
    !! probability of  capture which is not what is intended (zero probability
    !! should be  invoked for null food items).
    if ( n_counter > 0 ) then
      mean_dist = mean_dist / real(n_counter, SRP)
    else
      mean_dist = MISSING
    end if

  end function perception_food_dist_above_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the average distance to all conspecifics in the current
  !! perception object that are **below** the actor agent.
  elemental function perception_consp_dist_below_calculate(this)              &
                                                      result (mean_dist)
    class(PERCEPTION), intent(in) :: this
    !> @return The average distance to conspecifics within the perception
    !!         object that are located below (under) the actor agent.
    real(SRP) ::mean_dist

    ! Local counters
    integer :: i, n_counter

    !> ### Implementation details ###
    !> First, initialise the return average and the counter to zero.
    mean_dist = 0.0_SRP
    n_counter = 0

    !> Then, check if the agent has any conspecifics in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_consp()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one conspecific
    !! in the perception object. Calculation of the average distance to the
    !! conspecifics **below** is done by concurrent looping through the
    !! conspecifics within the perception object and calculating the distance
    !! from the agent.
    do concurrent (i=1:this%perceive_consp%get_count())
      !> This is done by checking the condition:
      !! @verbatim
      !!   if ( food_item .below. this ) ...
      !! @endverbatim
      if ( this%perceive_consp%conspecifics_seen(i) .below. this )  then
        mean_dist =                                                           &
                mean_dist + this%distance(                                    &
                            this%perceive_consp%conspecifics_seen(i) )
        n_counter = n_counter + 1
      end if
    end do

    !> Final average value is calculated, obviously, by division of the
    !! total distance by the count. In case the count is zero, also
    !! return commondata::missing mean.
    if ( n_counter > 0 ) then
      mean_dist = mean_dist / real(n_counter, SRP)
    else
      mean_dist = MISSING
    end if

  end function perception_consp_dist_below_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the average distance to all conspecifics in the current
  !! perception object that are **above** the actor agent.
  elemental function perception_consp_dist_above_calculate(this)              &
                                                      result (mean_dist)
    class(PERCEPTION), intent(in) :: this
    !> @return The average distance to conspecifics within the perception
    !!         object that are located above (over) the actor agent.
    real(SRP) ::mean_dist

    ! Local counters
    integer :: i, n_counter

    !> ### Implementation details ###
    !> First, initialise the return average and the counter to zero.
    mean_dist = 0.0_SRP
    n_counter = 0

    !> Then, check if the agent has any conspecifics in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_consp()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one conspecific
    !! in the perception object. Calculation of the average distance to the
    !! conspecifics **above** is done by concurrent looping through the
    !! conspecifics within the perception object and calculating the distance
    !! from the agent.
    do concurrent (i=1:this%perceive_consp%get_count())
      !> This is done by checking the condition:
      !! @verbatim
      !!   if ( food_item .above. this ) ...
      !! @endverbatim
      if ( this%perceive_consp%conspecifics_seen(i) .above. this )  then
        mean_dist =                                                           &
                mean_dist + this%distance(                                    &
                            this%perceive_consp%conspecifics_seen(i) )
        n_counter = n_counter + 1
      end if
    end do

    !> Final average value is calculated, obviously, by division of the
    !! total distance by the count. In case the count is zero, also
    !! return commondata::missing mean.
    if ( n_counter > 0 ) then
      mean_dist = mean_dist / real(n_counter, SRP)
    else
      mean_dist = MISSING
    end if

  end function perception_consp_dist_above_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the average distance to all predators in the current
  !! perception object that are **below** the actor agent.
  elemental function perception_predator_dist_below_calculate(this)           &
                                                      result (mean_dist)
    class(PERCEPTION), intent(in) :: this
    !> @return The average distance to predators within the perception
    !!         object that are located below (under) the actor agent.
    real(SRP) ::mean_dist

    ! Local counters
    integer :: i, n_counter

    !> ### Implementation details ###
    !> First, initialise the return average and the counter to zero.
    mean_dist = 0.0_SRP
    n_counter = 0

    !> Then, check if the agent has any predators in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_pred()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one conspecific
    !! in the perception object. Calculation of the average distance to the
    !! predators **below** is done by concurrent looping through the
    !! predators within the perception object and calculating the distance
    !! from the agent.
    do concurrent (i=1:this%perceive_predator%get_count())
      !> This is done by checking the condition:
      !! @verbatim
      !!   if ( food_item .below. this ) ...
      !! @endverbatim
      if ( this%perceive_predator%predators_seen(i) .below. this )  then
        mean_dist =                                                           &
                mean_dist + this%distance(                                    &
                            this%perceive_predator%predators_seen(i) )
        n_counter = n_counter + 1
      end if
    end do

    !> Final average value is calculated, obviously, by division of the
    !! total distance by the count. In case the count is zero, also
    !! return commondata::missing mean.
    if ( n_counter > 0 ) then
      mean_dist = mean_dist / real(n_counter, SRP)
    else
      mean_dist = MISSING
    end if

  end function perception_predator_dist_below_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the average distance to all predators in the current
  !! perception object that are **above** the actor agent.
  elemental function perception_predator_dist_above_calculate(this)           &
                                                      result (mean_dist)
    class(PERCEPTION), intent(in) :: this
    !> @return The average distance to predators within the perception
    !!         object that are located above (over) the actor agent.
    real(SRP) ::mean_dist

    ! Local counters
    integer :: i, n_counter

    !> ### Implementation details ###
    !> First, initialise the return average and the counter to zero.
    mean_dist = 0.0_SRP
    n_counter = 0

    !> Then, check if the agent has any predators in the perception;
    !! if not, return zero straight away.
    if (.not. this%has_pred()) then
      return
    end if

    !> From now on it is assumed that the agent has at least one conspecific
    !! in the perception object. Calculation of the average distance to the
    !! predators **above** is done by concurrent looping through the
    !! predators within the perception object and calculating the distance
    !! from the agent.
    do concurrent (i=1:this%perceive_predator%get_count())
      !> This is done by checking the condition:
      !! @verbatim
      !!   if ( food_item .above. this ) ...
      !! @endverbatim
      if ( this%perceive_predator%predators_seen(i) .above. this )  then
        mean_dist =                                                           &
                mean_dist + this%distance(                                    &
                            this%perceive_predator%predators_seen(i) )
        n_counter = n_counter + 1
      end if
    end do

    !> Final average value is calculated, obviously, by division of the
    !! total distance by the count. In case the count is zero, also
    !! return commondata::missing mean.
    if ( n_counter > 0 ) then
      mean_dist = mean_dist / real(n_counter, SRP)
    else
      mean_dist = MISSING
    end if

  end function perception_predator_dist_above_calculate

  !-----------------------------------------------------------------------------
  !> Calculate the probability of attack and capture of the `this` agent by
  !! the predator `this_predator`. This probability is a function of the
  !! distance between the predator and the agent and is calculated by the
  !! predator-class-bound procedure the_environment::predator::risk_fish().
  !! Example call:
  !! @verbatim
  !!   risk=proto_parents%individual(ind)%risk_pred(                          &
  !!     proto_parents%individual(ind)%perceive_predator%predators_seen(i),   &
  !!     proto_parents%individual(ind)%perceive_predator%predators_attack_rates(i))
  !! @endverbatim
  !! @note Note that this version of the procedure accepts `this_predator`
  !!       parameter as class the_neurobio::spatialobj_percept_comp that is
  !!       used for keeping the predator representations in the **perception
  !!       object**. This representation keeps two separate array for
  !!       the_neurobio::spatialobj_percept_comp spatial objects and the
  !!       attack rate.
  function predator_capture_probability_calculate_spatobj(this,               &
                                                this_predator, attack_rate,   &
                                                is_freezing, time_step_model) &
                                                            result (risk_pred)
    class(PERCEPTION), intent(in) :: this
    !> @param[in] this_predator the predator that is about to attack the agent.
    !!            @note Note that the predator has the SPATIALOBJ_PERCEPT_COMP
    !!                  type that is used in the predator perception object
    class(SPATIALOBJ_PERCEPT_COMP), intent(in) :: this_predator
    !> @param[in] attack_rate attack rate of the predator.
    !!            @note Note that the predator perception object keeps a
    !!                  separate array of the attack rate.
    real(SRP), intent(in) :: attack_rate
    !> @param[in] is_freezing optional logical flag indicating that the fish
    !!            prey agent is immobile (freezing) that would result in
    !!            reduced predation risk. Default value is FALSE.
    logical, optional, intent(in) :: is_freezing
    !> @param[in] time_step_model optional time step of the model, if absent,
    !!            set from the current time step
    !!            commondata::global_time_step_model_current.
    integer, optional, intent(in) :: time_step_model

    real(SRP) :: risk_pred

    ! Temporary predator object
    type(PREDATOR) :: tmp_predator

    ! Local copies of optionals.
    integer :: time_step_model_here
    logical :: is_freezing_loc

    ! Distance to the predator
    real(SRP) :: distance_pred
    ! Postscript file name for the debug plot
    character(FILENAME_LENGTH) :: debug_plot_file

    !> ### Checks ###
    !> First, check if the agent has any predators and return zero
    !! and exit if there are no predators in the agent's perception
    !! object.
    !! @note This assumes that the predator is much larger than the agent,
    !!       so the visual range the agent has for detecting the predator is
    !!       longer than the visual range of the predator for detecting the
    !!       prey agent.
    !! @warning The version working with the agent's perception component
    !!          the_neurobio::predator_capture_probability_calculate_pred()
    !!          returns a small non-zero probability of capture in contrast to
    !!          this version accepting `this_predator` object as a class
    !!          SPATIALOBJ_PERCEPT_COMP. This is because the former normally
    !!          calculated the objective predation risk whereas this version,
    !!          subjective risk in the agent's perception. The agent cannot be
    !!          aware of a predator that is outside of its perception.
    if (.not. this%has_pred()) then
        risk_pred = 0.0_SRP
        return
    end if

    !> Second, Check optional time step parameter. If unset, use global
    !! variable `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    ! Check is_freezing dummy parameter.
    if (present(is_freezing)) then
      is_freezing_loc = is_freezing
    else
      is_freezing_loc = .FALSE.
    end if

    !> Third, create a temporary PREDATOR type object using the standard method
    !! `make`. The body size and the spatial position are obtained directly
    !! from the  `this_predator` object. However, the attack rate is obtained
    !! from the second dummy argument `attack_rate` to this procedure.
    !  @note Note that the `select type` construct used in the initial version
    !        that allows implementation of a single procedure for both
    !        PREDATOR and SPATIALOBJ_PERCEPT_COMP types requires attack rate
    !        as an optional parameter which is not safe: if not provided when
    !        required, it could lead to non-initialised attack rate in
    !        calculations.
    call tmp_predator%make( body_size=this_predator%get_size(),               &
                            attack_rate=attack_rate,                          &
                            position=this_predator%location(),                &
                            label="tmp_object" )

    !> ### Implementation ###
    !> Calculate the distance between the agent and predator.
    distance_pred = this%distance( this_predator )

    !> Set the debug plot file name that will be passed to the
    !! predator-class-bound function the_environment::predator::risk_fish().
    debug_plot_file = "plot_debug_predation_risk_" &
                          // TOSTR(Global_Time_Step_Model_Current) // "_" //  &
                          MMDD // "_a_" // trim(this%individ_label())         &
                          // "_"  //                                          &
                          RAND_STRING(LABEL_LENGTH, LABEL_CST,LABEL_CEN) // PS

    !> Calculate the probability of capture of the `this` prey agent by the
    !! predator. See the_environment::predator::risk_fish() for the details of
    !! the calculation.
    risk_pred = tmp_predator%risk_fish(prey_spatial=this%location(),          &
                                       prey_length=this%get_length(),         &
                                       prey_distance=distance_pred,           &
                                       is_freezing=is_freezing_loc,           &
                                       time_step_model=time_step_model_here,  &
                                       debug_plot_file=debug_plot_file )

  end function predator_capture_probability_calculate_spatobj

  !-----------------------------------------------------------------------------
  !> Calculate the probability of attack and capture of the `this` agent by
  !! the predator `this_predator`. This probability is a function of the
  !! distance between the predator and the agent and is calculated by the
  !! predator-class-bound procedure the_environment::predator::risk_fish().
  !! @note Note that this version of the procedure accepts `this_predator`
  !!       parameter as class the_neurobio::predator, i.e. for the **objective
  !!       predator object**.
  function predator_capture_probability_calculate_pred(this, this_predator,   &
                                                is_freezing, time_step_model) &
                                                            result (risk_pred)
    class(PERCEPTION), intent(in) :: this
    !> @param[in] this_predator the predator that is about to attack the agent.
    class(PREDATOR), intent(in) :: this_predator
    !> @param[in] is_freezing optional logical flag indicating that the fish
    !!            prey agent is immobile (freezing) that would result in
    !!            reduced predation risk. Default value is FALSE.
    logical, optional, intent(in) :: is_freezing
    !> @param[in] time_step_model optional time step of the model, if absent,
    !!            set from the current time step
    !!            commondata::global_time_step_model_current.
    integer, optional, intent(in) :: time_step_model

    real(SRP) :: risk_pred

    ! Local copies of optionals
    integer :: time_step_model_here
    logical :: is_freezing_loc

    ! Distance to the predator
    real(SRP) :: distance_pred
    ! Postscript file name for the debug plot
    character(FILENAME_LENGTH) :: debug_plot_file

    !> ### Checks ###
    !> First, check if the agent has any predators in the perception object.
    !! Return a near-zero value defined by the
    !! commondata::predator_attack_capture_probability_min parameter
    !! constant, and exit if there are no predators in the agent's perception
    !! object.
    !! @note This assumes that the predator is much larger than the agent,
    !!       so the visual range the agent has for detecting the predator is
    !!       longer than the visual range of the predator for detecting the
    !!       prey agent.
    !! @warning The version working with the agent's **perception** component
    !!          the_neurobio::predator_capture_probability_calculate_spatobj()
    !!          returns **zero** probability in contrast to this version
    !!          accepting `this_predator` object as a type PREDATOR. This is
    !!          because the former normally calculated the subjective
    !!          assessment of the predation risk whereas this version,
    !!          objective risk.
    if (.not. this%has_pred()) then
        risk_pred = this_predator%attack_rate *                               &
                                    PREDATOR_ATTACK_CAPTURE_PROBABILITY_MIN
        return
    end if

    !> Second, Check optional time step parameter. If unset, use global
    !! variable `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    ! Check is_freezing dummy parameter.
    if (present(is_freezing)) then
      is_freezing_loc = is_freezing
    else
      is_freezing_loc = .FALSE.
    end if

    !> ### Implementation ###
    !> Calculate the distance between the agent and predator.
    distance_pred = this%distance( this_predator )

    !> Set the debug plot file name that will be passed to the
    !! predator-class-bound function the_environment::predator::risk_fish().
    debug_plot_file = "plot_debug_predation_risk_" &
                          // TOSTR(Global_Time_Step_Model_Current) // "_" //  &
                          MMDD // "_a_" // trim(this%individ_label())         &
                          // "_"  //                                          &
                          RAND_STRING(LABEL_LENGTH, LABEL_CST,LABEL_CEN) // PS

    !> Calculate the probability of capture of the `this` prey agent by the
    !! predator. See the_environment::predator::risk_fish() for the details of
    !! the calculation.
    risk_pred = this_predator%risk_fish(prey_spatial=this%location(),         &
                                        prey_length=this%get_length(),        &
                                        prey_distance=distance_pred,          &
                                        is_freezing=is_freezing_loc,          &
                                        time_step_model=time_step_model_here, &
                                        debug_plot_file=debug_plot_file )

  end function predator_capture_probability_calculate_pred

  !-----------------------------------------------------------------------------
  !> Calculate the overall direct predation risk for the agent, i.e.
  !! the probability of attack and capture by the nearest predator.
  function predation_capture_probability_risk_wrapper(this, is_freezing)      &
                                                                 result (risk)
    class(PERCEPTION), intent(in) :: this
    !> @param[in] is_freezing optional logical flag indicating that the fish
    !!            prey agent is immobile (freezing) that would result in
    !!            reduced predation risk. Default value is FALSE.
    logical, optional, intent(in) :: is_freezing
    !> @return Returns the probability of capture by the nearest predator.
    real(SRP) :: risk

    ! Local copies of optionals
    logical :: is_freezing_loc

    ! Check is_freezing dummy parameter.
    if (present(is_freezing)) then
      is_freezing_loc = is_freezing
    else
      is_freezing_loc = .FALSE.
    end if

    if (this%has_pred()) then
      risk = this%risk_pred( this%perceive_predator%predators_seen(1),        &
                             this%perceive_predator%predators_attack_rates(1),&
                             is_freezing=is_freezing_loc )
    else
      risk= 0.0_SRP
    end if

  end function predation_capture_probability_risk_wrapper

  !-----------------------------------------------------------------------------
  !> Get the body size property of a polymorphic object. The object can be
  !! of the following extension of the basic the_environment::spatial class:
  !! - the_neurobio::conspec_percept_comp - perception object
  !! - the_body::condition - real conspecific.
  !! .
  !! @note Other specific classes can be similarly implemented.
  !! @warning This is not a type-bound function because the base class
  !!          the_environment::spatial is defined in a different down-level
  !!          module. Usage: `M = get_props_size(object)`.
  elemental function get_prop_size(this) result (size)
    class(SPATIAL), intent(in) :: this
    !> @return the body size of the input the_environment::spatial class object.
    real(SRP) :: size

    !> #### Implementation notes ####
    !> Get the properties of the conspecific from the perception object
    !! or real physical conspecific data. This is done by determining the
    !! `this` data type with "select type" construct.
    !!
    select type (this)
      !> - if the `this` is a conspecific from the perception object,
      !!   its body length is obtained from the respective
      !!   data components of the_neurobio::conspec_percept_comp.
      class is (CONSPEC_PERCEPT_COMP)
        size = this%get_size()
      !> - if the `this` is real conspecific (the_neurobio::condition
      !!   class), its body length is obtained from  the
      !!   the_body::condition::get_length() and
      !!   the_body::condition::get_mass() methods.
      class is (CONDITION)
        size = this%get_length()
      !> - in the case construct "default" case, the class is undefined,
      !!   return commondata::missing value.
      !> .
      class default
        size = MISSING
    end select

  end function get_prop_size

  !-----------------------------------------------------------------------------
  !> Get the body mass property of a polymorphic object. The object can be
  !! of the following extension of the basic the_environment::spatial class:
  !! - the_neurobio::conspec_percept_comp - perception object
  !! - the_body::condition - real conspecific.
  !! .
  !! @note Other specific classes can be similarly implemented.
  !! @warning This is not a type-bound function because the base class
  !!          the_environment::spatial is defined in a different down-level
  !!          module. Usage: `M = get_props_mass(object)`.
  elemental function get_prop_mass(this) result (mass)
    class(SPATIAL), intent(in) :: this
    !> @return the body mass of the input the_environment::spatial class object.
    real(SRP) :: mass

    !> #### Implementation notes ####
    !> Get the properties of the conspecific from the perception object
    !! or real physical conspecific data. This is done by determining the
    !! `this` data type with "select type" construct.
    !!
    select type (this)
      !> - if the `this` is a conspecific from the perception object,
      !!   its body length is obtained from the respective
      !!   data components of the_neurobio::conspec_percept_comp.
      class is (CONSPEC_PERCEPT_COMP)
        mass = this%get_mass()
      !> - if the `this` is real conspecific (the_neurobio::condition
      !!   class), its body length is obtained from  the
      !!   the_body::condition::get_length() and
      !!   the_body::condition::get_mass() methods.
      class is (CONDITION)
        mass = this%get_mass()
      !> - in the case construct "default" case, the class is undefined,
      !!   return commondata::missing value.
      !> .
      class default
        mass = MISSING
    end select

  end function get_prop_mass

  !-----------------------------------------------------------------------------

end module THE_NEUROBIO
