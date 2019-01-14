!> @file m_env.f90
!! The environmental objects of the AHA Model.
!! @author Sergey Budaev <sergey.budaev@uib.no>
!! @author Jarl Giske <jarl.giske@uib.no>
!! @date 2016-2017

!-------------------------------------------------------------------------------
! $Id$
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief Definition of environmental objects.
!> @section the_environment_module THE_ENVIRONMENT module
!> This module defines various environment objects and primitives, starting
!! from the basic primitive a spatial object the_environment::spatial. This
!! object represents a point in a three-dimensional space. The agent class
!! hierarchy starts from this spatial primitive as the agent is a spatial
!! object.
module THE_ENVIRONMENT

  use COMMONDATA

  implicit none

  character (len=*), parameter, private :: MODNAME = "(THE_ENVIRONMENT)"

  !> Default dimensionality of the environment universe.
  integer, parameter, private :: DIMENSIONALITY_DEFAULT = 3

  !> The number of corners for an environment object in the 2D *X*x*Y* plane
  !! @warning Valid only in the simplistic box-like implementation of
  !!          environment objects and should be reimplemented if the
  !!          environment is set as an arbitrary polyhedron.
  integer, parameter  :: DIM_ENVIRON_CORNERS = 4

  !> Definition of a spatial object. Spatial object determines the position of
  !! the agent, food items and other things in the simulated space. Here we
  !! use continuous 3D environment (real type coordinates)
  !! @note IMPORTANT: We will use **position** method to **set** location of
  !!       a spatial object and **location** method to **get** its location.
  !! @note We use `position_v` method to **set** define spatial position using
  !!       raw 3D coordinates, x, y, z -- it lacks extensibility -- for
  !!       convenience only.
  !! @warning Note that we **do not** set **ID** at the elementary `SPATIAL`
  !!          objects. This is done to make type constructor `SPATIAL(x, y, z)`
  !!          (that is used frequently in different places) shorter and easier
  !!          to use. We do not need to include an ID then. IDs are set at
  !!          higher levels in the object hierarchy, e.g. `FOOD_ITEM` has
  !!          `food_iid` integer data component.
  type, public :: SPATIAL
    !> We define three-dimensional environment: x, y and depth.
    real(SRP) :: x, y, depth
    contains
      !> Create an empty spatial object.
      !! @note `position` is the standard method for placing spatial object,
      !!        returns object type. `position_v` is non-extensible method
      !!        that returns raw 3D coordinates, for convenience only.
      !!        `position` and `position_v` are overriden in moving
      !!        objects below. Not sure if we really need `position_v`.
      !! See `the_environment::spatial_create_empty()`
      procedure, public :: create => spatial_create_empty
      !> Place spatial object into a 3D space, define the object's current
      !! coordinates. Object-based procedure.
      !! See `the_environment::spatial_fix_position_3d_o()`
      procedure, public :: position => spatial_fix_position_3d_o
       !> Place spatial object into a 3D space, define the object's current
      !! coordinates. Vector-based procedure.
      !! See `the_environment::spatial_fix_position_3d_s()`
      procedure, public :: position_v => spatial_fix_position_3d_s
      !> Assign all `MISSING` coordinates to the `SPATIAL` object.
      !! See `the_environment::spatial_make_missing()`
      procedure, public :: missing => spatial_make_missing
      !> Calculate the Euclidean distance between two spatial objects.
      !! See `the_environment::spatial_distance_3d()`
      procedure, public :: distance => spatial_distance_3d
      !> Calculates the minimum distance from a the_environment::spatial class
      !! object to a line segment delimited by two the_environment::spatial
      !! endpoints in the 2D *XY* plane (the depth coordinate is ignored).
      !! See `the_environment::geo_poly2d_dist_point_to_section()`.
      procedure, public :: distance_segment2d =>                              &
                                            geo_poly2d_dist_point_to_section
      !> Calculates the minimum distance from a the_environment::spatial class
      !! object to a line segment delimited by two the_environment::spatial
      !! class endpoints in the 3D *XY* space.
      !! See `the_environment::geo_poly3d_dist_point_to_section()`.
      procedure, public :: distance_segment =>                                &
                                            geo_poly3d_dist_point_to_section
      !> Calculate the Euclidean distance between the current and previous
      !! position of a single spatial object.
      !! See `the_environment::spatial_self_distance_3d()`
      procedure, public :: way => spatial_self_distance_3d
      !> Function to check if this spatial object is located within an area
      !! set by an environmental object
      !! See `the_environment::spatial_check_located_within_3d()`
      procedure, public :: is_within => spatial_check_located_within_3d
      !> Identify in which environment from the input list this spatial
      !! agent is currently in.
      !! See `the_environment::spatial_get_environment_in_pos()`.
      procedure, public :: find_environment => spatial_get_environment_in_pos
      !> Logical function to check if the argument spatial object(s) is(are)
      !! located **below** this spatial object.
      !! See `the_environment::spatial_check_located_below()`
      procedure, public :: is_below => spatial_check_located_below
      !> Logical function to check if the argument spatial object(s) is(are)
      !! located **above** this spatial object.
      !! See `the_environment::spatial_check_located_above()`
      procedure, public :: is_above => spatial_check_located_above
      !> Determine the nearest spatial object to **this** spatial object among
      !! an array of other spatial objects.
      !! See `the_environment::spatial_get_nearest_object()`
      procedure, public :: nearest => spatial_get_nearest_object
      !> Determine the nearest spatial object to **this** spatial object among
      !! an array of other spatial objects.
      !! See `the_environment::spatial_get_nearest_id()`
      procedure, public :: nearest_num => spatial_get_nearest_id
      !> Calculate the distances between **this** spatial object and an array of
      !! its neighbours. Optionally output the distances, sorting index vector
      !! and rankings vector for each of these neighbours. Optionally do only
      !! partial indexing, up to the order `rank_max` for computational speed.
      !! See `the_environment::spatial_neighbours_distances()`
      procedure, public :: neighbours => spatial_neighbours_distances
      !> Get the current spatial position of a `SPATIAL` object. Object-based.
      !! See `the_environment::spatial_get_current_pos_3d_o()`
      procedure, public :: now_o => spatial_get_current_pos_3d_o
      !> Get the current spatial position of a `SPATIAL` object. Vector-based.
      !! See `the_environment::spatial_get_current_pos_3d_v()`
      procedure, public :: now_v => spatial_get_current_pos_3d_v
        !> Get the current spatial position of a `SPATIAL` object.
        !! Generic interface/alias.
        generic, public :: location => now_o, now_v
        !> Get the current spatial position of a `SPATIAL` object.
        !! Generic interface/alias.
        generic, public :: now => now_o, now_v
      !> Get the current `X` position of a `SPATIAL` object.
      !! See `the_environment::spatial_get_current_pos_x_3d()`
      procedure, public :: xpos => spatial_get_current_pos_x_3d
      !> Get the current `Y` position of a `SPATIAL` object.
      !! See `the_environment::spatial_get_current_pos_y_3d()`
      procedure, public :: ypos => spatial_get_current_pos_y_3d
      !> Get the current `Z` (depth) position of a `SPATIAL` object.
      !! See `the_environment::spatial_get_current_pos_d_3d()`
      procedure, public :: dpos => spatial_get_current_pos_d_3d
      !> Calculate the illumination (background irradiance) at the depth of the
      !! spatial object at an arbitrary time step of the model.
      !! See `the_environment::spatial_calc_irradiance_at_depth()`
      procedure, public :: illumination => spatial_calc_irradiance_at_depth
      !> Calculate the visibility range of a spatial object. Wrapper to the
      !! `visual_range` function. This function calculates the distance from
      !! which this object can be seen by a visual object (e.g. predator or
      !! prey).
      !! See `the_environment::spatial_visibility_visual_range_cm()`.
      !! @warning The function interface for the basic spatial type
      !!          the_environment::spatial is called `visibility_basic`
      !!          to distinguish it from similar `visibility` methods
      !!          defined for several extension classes, such as
      !!          the_environment::predator, the_environment::food_item and
      !!          the_body::condition because this function is unrelated to
      !!          them, otherwise it must have the same parameters as in
      !!          the class extensions.
      procedure, public :: visibility => spatial_visibility_visual_range_cm
  end type SPATIAL

  !> Definition of a movable spatial object. It extends the
  !! the_environment::spatial object, but also adds its previous position
  !! history in stack-like arrays. The history is maintained with the
  !! commondata::add_to_history() subroutine, adding a single
  !! element on the top (end) of the history stack.
  type, public, extends(SPATIAL) :: SPATIAL_MOVING
    !> We define prior historical values of the `SPATIAL` positions.
    !! @note Historical stack has the same the_environment::spatial type but
    !!       is an array of prior values (i.e. array of
    !!       the_environment::spatial objects). The
    !!       the_environment::spatial_moving::position() method overrides
    !!       the standard function defined for the_environment::spatial
    !!       (the_environment::spatial::position()), it not only sets the
    !!       current position, but also moves the previous position of the
    !!       object into the history stack.
    type(SPATIAL), dimension(HISTORY_SIZE_SPATIAL) :: history
    contains
      !> Create a new spatial moving object. Initially it has no position, all
      !! coordinate values are commondata::missing or commondata::invalid for
      !! real type coordinates.
      !! See `the_environment::spatial_moving_create_3d()`
      procedure, public :: create => spatial_moving_create_3d
      !> Place spatial movable object into a 3D space, define the object's current
      !! coordinates, but first save previous coordinates. Object-based.
      !! See `the_environment::spatial_moving_fix_position_3d_o()`
      procedure, public :: position => spatial_moving_fix_position_3d_o
      !> Repeat/re-save the current position into the positional history stack.
      !! See `the_environment::spatial_moving_repeat_position_history_3d()`.
      procedure, public :: repeat_position =>                                 &
                                      spatial_moving_repeat_position_history_3d
      !> Place spatial movable object into a 3D space, define the object's current
      !! coordinates, but first save previous coordinates. Vector-based.
      !! See `the_environment::spatial_moving_fix_position_3d_v()`
      procedure, public :: position_v => spatial_moving_fix_position_3d_v
      !> Create a new empty history of positions for spatial moving object.
      !! Assign all values to the commondata::missing value code.
      !! See `the_environment::spatial_moving_clean_hstory_3d()`
      procedure, public :: spatial_history_clean => spatial_moving_clean_hstory_3d
      !> Calculate the Euclidean distance between the current and previous
      !! position of a single spatial movable object. Optionally, it also
      !! calculates the total distance traversed during the `from_history` points
      !! from the history stack along with the distance from the current position
      !! and the last historical value.
      !! See `the_environment::spatial_moving_self_distance_3d()`.
      procedure, public :: way => spatial_moving_self_distance_3d
      !> The spatial moving object **ascends**, goes up the depth with specific
      !! fixed step size. See `the_environment::spatial_moving_go_up()`.
      procedure, public :: go_up => spatial_moving_go_up
      !> The spatial moving object **decends**, goes down the depth with
      !! specific fixed step size.
      !! See `the_environment::spatial_moving_go_down()`.
      procedure, public :: go_down => spatial_moving_go_down
      !> Implements an optionally environment-restricted Gaussian random
      !! walk in 3D.
      !! See `the_environment::spatial_moving_randomwalk_gaussian_step_3d()`.
      procedure, public :: rwalk3d => spatial_moving_randomwalk_gaussian_step_3d
      !> Implements an optionally environment-restricted Gaussian random
      !! walk in a "2.5 dimensions", i.e. 2D x y with separate walk
      !! parameters for the third depth dimension.
      !! See `the_environment::spatial_moving_randomwalk_gaussian_step_25d()`.
      procedure, public :: rwalk25d=>spatial_moving_randomwalk_gaussian_step_25d
        !> Implements an optionally environment-restricted Gaussian random
        !! walk. Generic interface for 3D and 3.5D moves.
        !! See `the_environment::spatial_moving_randomwalk_gaussian_step_3d()`
        !! and `the_environment::spatial_moving_randomwalk_gaussian_step_25d()`.
        generic, public :: rwalk => rwalk3d, rwalk25d
      !> Implements an optionally environment-restricted **correlated
      !! directional** Gaussian random walk in 3D towards (or away of)
      !! an the_environment::spatial class `target` object.
      !! See `the_environment::spatial_moving_corwalk_gaussian_step_3d()`.
      procedure, public :: corwalk3d => spatial_moving_corwalk_gaussian_step_3d
      !> Implements an optionally environment-restricted **correlated
      !! directional** Gaussian random walk in 3D towards (or away of)
      !! an the_environment::spatial class `target` object.
      !! See `the_environment::spatial_moving_corwalk_gaussian_step_25d()`.
      procedure, public :: corwalk25d =>spatial_moving_corwalk_gaussian_step_25d
        !> Implements an optionally environment-restricted **correlated
        !! directional** Gaussian random walk. `corwalk` is a generic
        !! interface for 3D and "2.5"D moves. For details see the 3d version
        !! and a version with separate *X,Y* and *depth* random parameters.
        !! - `the_environment::spatial_moving_corwalk_gaussian_step_3d()`;
        !! - `the_environment::spatial_moving_corwalk_gaussian_step_25d()`;
        !! .
        generic, public :: corwalk => corwalk3d, corwalk25d
      !> Implements an optionally environment-restricted **directional**
      !! Gaussian random walk in 3D towards a `target` the_environment::spatial
      !! object.
      !! See `the_environment::spatial_moving_dirwalk_gaussian_step_3d()`
      !! @warning obsolete, will be removed!
      procedure, public :: dirwalk3d => spatial_moving_dirwalk_gaussian_step_3d
      !> Implements an optionally environment-restricted **directional**
      !! Gaussian random walk in "2.5"D towards a `target` object.  i.e.
      !! 2D x y with separate walk parameters for the third depth
      !! dimension.
      !! See `the_environment::spatial_moving_dirwalk_gaussian_step_25d()`
      !! @warning obsolete, will be removed!
      procedure, public :: dirwalk25d=>spatial_moving_dirwalk_gaussian_step_25d
        !> Implements an optionally environment-restricted **directional**
        !! Gaussian random walk. Generic interface for 3D and "2.5"D moves.
        !! @warning obsolete, will be removed!
        generic, public :: dirwalk => dirwalk3d, dirwalk25d
  end type SPATIAL_MOVING

  !> Definition of the overall **environment**. Environment is a general
  !! container for all habitats, patches and other similar objects where the
  !! whole life of the agents takes place. Environment just provides
  !! the *limits* for all these objects.
  !! @warning In this version, the environment objects are the most simplistic
  !!       form: 3D "boxes". An arbitrary convex *polyhedron*-based environment
  !!       can be implemented but this requires a more complex computational
  !!       geometry backend.
  !! @note Coordinate system should **always** use the `SPATIAL` type objects,
  !!       we don't define `_v`-type procedures (it is also unclear are `_v`
  !!       procedures really necessary).
  type, public :: ENVIRONMENT
    !> Set shape and limits of the whole environment, by default a
    !! rectangle with Cartesian coordinates based on ENVIRONMENT_WHOLE_SIZE.
    !! The minimum and maximum coordinates are set through the `SPATIAL`
    !! object.
    type(SPATIAL) :: coord_min, coord_max
    contains
      !> Create the highest level container environment. Vector-based.
      !! See `the_environment::environment_whole_build_vector()`
      procedure, public :: build_vector => environment_whole_build_vector
      !> Create the highest level container environment. Object-based.
      !! See `the_environment::environment_whole_build_object()`
      procedure, public :: build_object => environment_whole_build_object
      !> Build an **unlimited environment**, with the spatial coordinates limited
      !! by the maximum machine supported values based on the intrinsic `huge`
      !! function.
      !! See `the_environment::environment_build_unlimited()`
      procedure, public :: build_unlimited => environment_build_unlimited
        !> Create the highest level container environment. Generic interface.
        !! See `the_environment::environment_whole_build_vector()`,
        !! `the_environment::environment_whole_build_object()` and
        !! `the_environment::environment_build_unlimited()`
        generic, public :: build => build_vector, build_object, build_unlimited
      !> Return an environment object that is shrunk by a fixed value in the 2D
      !! XxY plane.
      !! See `the_environment::environment_shrink_xy_fixed()`.
      procedure, public :: shrink2d => environment_shrink_xy_fixed
      !> Function to get the **minimum** spatial limits (coordinates) of
      !! the environment.
      !! See `the_environment::environment_get_minimum_obj()`
      procedure, public :: lim_min => environment_get_minimum_obj
      !> Function to get the **maximum** spatial limits (coordinates) of
      !! the environment.
      !! See `the_environment::environment_get_maximum_obj()`
      procedure, public :: lim_max => environment_get_maximum_obj
      !> Get the **minimum depth** in this environment.
      !! See `the_environment::environment_get_minimum_depth()`.
      procedure, public :: depth_min => environment_get_minimum_depth
      !> Get the **maximum depth** in this environment.
      !! See `the_environment::environment_get_maximum_depth()`.
      procedure, public :: depth_max => environment_get_maximum_depth
      !> Check if a spatial object is actually within this environment.
      !! See `the_environment::environment_check_located_within_3d()`
      procedure, public :: is_within => environment_check_located_within_3d
      !> Get the corners of the environment in the 2D X Y plane.
      !! See `the_environment::environment_get_corners_2dxy()`.
      procedure, public :: corners2d => environment_get_corners_2dxy
      !> Get the spatial point position within this environment that is
      !! nearest to an arbitrary spatial object located outside of the this
      !! environment. If the spatial object is actually located in this
      !! environment,return its own spatial position.
      !! See `the_environment::environment_get_nearest_point_in_outside_obj()`.
      procedure, public :: nearest_target =>                                  &
                                environment_get_nearest_point_in_outside_obj
      !> Determine the centroid of the environment.
      !! See `the_environment::environment_centre_coordinates_3d()`
      procedure, public :: centre => environment_centre_coordinates_3d
      !> Generate a random spatial object with the uniform distribution within
      !! (i.e. bound to) **this** environment.
      !! See `the_environment::environment_random_uniform_spatial_3d()`
      procedure, public :: uniform_s => environment_random_uniform_spatial_3d
      !> Generate a random spatial object with the uniform distribution within
      !! (i.e. bound to) **this** environment, the third depth coordinate is
      !! fixed.
      !! See `the_environment::environment_random_uniform_spatial_2d()`
      procedure, public :: uniform2_s => environment_random_uniform_spatial_2d
      !> Generate a vector of random spatial objects with the uniform distribution
      !! within (i.e. bound to) **this** environment. Full 3D procedure.
      !! See `the_environment::environment_random_uniform_spatial_vec_3d()`
      procedure, public :: uniform_v =>environment_random_uniform_spatial_vec_3d
      !> Generate a vector of random spatial objects with the uniform distribution
      !! within (i.e. bound to) **this** environment. The third, depth coordinate
      !! is non-stochastic, and provided as an array parameter.
      !! See `the_environment::environment_random_uniform_spatial_vec_2d()`
      procedure, public :: uniform2_v=>environment_random_uniform_spatial_vec_2d
        !> Generate a vector of random spatial objects with the uniform
        !! distribution within (i.e. bound to) **this** environment. Generic
        !! interface.
        generic, public :: uniform => uniform_s,uniform2_s,uniform_v,uniform2_v
      !> Generates a vector of random spatial object with Gaussian coordinates
      !! within (i.e. bound to) **this** environment. Full 3D procedure.
      !! See `the_environment::environment_random_gaussian_spatial_3d()`
      procedure, public :: gaussian3d => environment_random_gaussian_spatial_3d
      !> Generates a vector of random spatial object with Gaussian coordinates
      !! within (i.e. bound to) **this** environment. The depth coordinate is
      !! set separately and can be non-random (fixed for the whole output array)
      !! or Gaussian with separate variance.
      !! See `the_environment::environment_random_gaussian_spatial_2d()`
      procedure, public :: gaussian2d => environment_random_gaussian_spatial_2d
  end type ENVIRONMENT

  !> Definition of a single food item. Food item is a spatial object that has
  !! specific location in space. It can be "created" and eaten ("disappear").
  !! Food item is an immobile SPATIAL object that has a position in 3D space.
  type, public, extends(SPATIAL_MOVING) :: FOOD_ITEM
    !> Food item has a size (radius) that determines its visibility and
    !! nutritional value for the predatory agent.
    real(SRP) :: size
    !> Food item can be present or absent (eaten by the agent, =.TRUE.).
    logical :: eaten
    !> Unique ID of this food item. Needed in the resource array.
    integer :: food_iid
    contains
      !> Create a single food item at an undefined position with default size.
      !! See `the_environment::food_item_create()`
      procedure, public :: create => food_item_create
      !> Make a single food item, i.e. place it into a specific position
      !! in the model environment space and set the size.
      !! See `the_environment::food_item_make()`
      procedure, public :: make => food_item_make
      !> Stochastic outcome of **this** food item capture by an agent.
      !! Returns TRUE if the food item is captured.
      !! See `the_environment::food_item_capture_success_stochast()`
      procedure, public :: capture_success => food_item_capture_success_stochast
      !> Calculate the probability of capture of **this** food item by a predator
      !! agent depending on the distance between the agent and this food item.
      !! See `the_environment::food_item_capture_probability_calc()`
      procedure, public :: capture_probability => food_item_capture_probability_calc
      !> Calculate the visibility range of this food item. Wrapper to the
      !! `visual_range` function. This function calculates the distance from
      !! which this food item can be seen by a predator (i.e. the default
      !! predator's visual range).
      !! See `the_environment::food_item_visibility_visual_range()`
      procedure, public :: visibility => food_item_visibility_visual_range
      !> Make the food item "disappear" and take the "eaten" state, i.e.
      !! impossible for consumption by the agents.
      !! See `the_environment::food_item_disappear()`
      procedure, public :: disappear => food_item_disappear
      !> Logical check-indicator function for the food item being eaten and not
      !! available.
      !! See `the_environment::food_item_is_eaten_unavailable()`
      procedure, public :: is_unavailable => food_item_is_eaten_unavailable
      !> Logical check-indicator function for the food item being available.
      !! @returns Logical indicator TRUE if the food item is present
      !! in the environment and therefore available.
      !! See `the_environment::food_item_is_available()`
      procedure, public :: is_available => food_item_is_available
      !> Get the size component of the food item object.
      !! See `the_environment::food_item_get_size()`
      procedure, public :: get_size => food_item_get_size
      !> Calculate and get the mass of the food item.
      !! See `the_environment::food_item_get_mass()`
      procedure, public :: get_mass => food_item_get_mass
      !> Get the unique id of the food item object.
      !! See `the_environment::food_item_get_iid()`
      procedure, public :: get_iid => food_item_get_iid
      !> Set unique id for the food item object.
      !! See `the_environment::food_item_set_iid()`
      procedure, public :: set_iid => food_item_set_iid
      !> Clone the properties of this food item to another food item.
      !! See `the_environment::food_item_clone_assign()`
      procedure, public :: clone => food_item_clone_assign
  end type FOOD_ITEM

  !> Definition of the super-type FOOD resource type. This is a superclass,
  !! several sub-classes can be defined for different kinds of food and prey
  !! objects.
  type, public :: FOOD_RESOURCE
    !> Food resource type label
    character (len=LABEL_LENGTH) :: food_label
    !> Availability of this kind of food, number of food objects that are
    !! provided into the environment.
    integer :: number_food_items
    !> Food resource consists of an array of `FOOD_ITEM`'s
    type(FOOD_ITEM), allocatable, dimension(:) :: food
    contains
      !> Make food resource object. This class standard constructor.
      !! See `the_environment::food_resource_make()`
      procedure, public :: make => food_resource_make
      !> Replenish and restore food resource: the food resource is restored to
      !! its initial state as set by the_environment::food_resource::make() or
      !! to a **smaller** abundance.
      !!  See `the_environment::food_resource_replenish_food_items_all()`
      procedure, public :: replenish => food_resource_replenish_food_items_all
      !> Delete and deallocate food resource object. This class destructor.
      !! See `the_environment::food_resource_destroy_deallocate()`
      procedure, public :: destroy => food_resource_destroy_deallocate
      !> Sort the food resource objects within the array by their sizes.
      !! The two subroutines below are a variant of the recursive quick-sort
      !! algorithm adapted for sorting real components of the the `FOOD_RESOURCE`
      !! object.
      !! See `the_environment::food_resource_sort_by_size()`
      procedure, public :: sort => food_resource_sort_by_size
      !> Reset individual iid for the food resource. Individual iids must normally
      !! coincide with the array order index. If it is changed after sorting,
      !! iids no longer reflect the correct index. So this subroutine resets iids
      !! to be coinciding with each food item index.
      !! See `the_environment::food_resource_reset_iid_all()`
      procedure, public :: reindex => food_resource_reset_iid_all
      !> Get the label of the this food resource.
      !! See `the_environment::food_resource_get_label()`.
      procedure, public :: get_label => food_resource_get_label
      !> Get the number of food items in the food resource.
      !! See `the_environment::food_resource_get_abundance()`.
      procedure, public :: abundance => food_resource_get_abundance
      !> Get the location object array (array of the_environment::spatial
      !! objects) of a food resource object.
      !! See `the_environment::food_resource_locate_3d()`
      procedure, public :: location => food_resource_locate_3d
      !> Calculate the average distance between food items within a resource.
      !! See `the_environment::food_resource_calc_average_distance_items()`
      procedure, public :: distance_average =>                                &
                                    food_resource_calc_average_distance_items
      !> Collapse several food resources into one. The collapsed resource can then
      !! go into the perception system. The properties of the component resources
      !! are retained in the collapsed resource.
      !! See `the_environment::food_resources_collapse()`
      procedure, public :: join => food_resources_collapse
      !> Transfer back the resulting food resources into their original objects
      !! out from a collapsed object from `food_resources_collapse`.
      !! See `the_environment::food_resources_update_back()`
      procedure, public :: unjoin => food_resources_update_back
      !> Implement vertical migration of all the food items in the resource in
      !! a sinusoidal pattern.
      !! See `the_environment::food_resource_migrate_move_items()`.
      procedure, public :: migrate_vertical => food_resource_migrate_move_items
      !> Perform a random walk step for all food items within the food
      !! resource with default parameters.
      !! See `the_environment::food_resource_rwalk_items_default()`.
      procedure, public :: rwalk => food_resource_rwalk_items_default
      !> Save characteristics of food items in the resource into a CSV file.
      !! See `the_environment::food_resource_save_foods_csv()`.
      procedure, public :: save_csv => food_resource_save_foods_csv
  end type FOOD_RESOURCE

  !> Definition of the `PREDATOR` objects. **Predator** is a moving agent that
  !! hunts on the evolving AHA agents but its internal structure is very
  !! simplistic (although we can in principle doit as a full AHA complexity
  !! with genome, GOS etc...).
  type, public, extends(SPATIAL_MOVING) :: PREDATOR
    !> The label of the predator.
    character (len=LABEL_LENGTH) :: label
    !> Individual body size of the predator, can be stochastic or not. Can
    !! affect attack rate (e.g. larger predators more dangerous).
    real(SRP) :: body_size
    !> The attack rate of the predator, i.e. the baseline probability of
    !! attacking catching the prey agent if the latter is found in proximity
    !! (within the visual range).
    real(SRP) :: attack_rate
    contains
      !> Initialise a predator object.
      !! See `the_environment::predator_make_init()`
      procedure, public :: make => predator_make_init
      !> Set label for the predator, if not provided, set it random.
      !! See `the_environment::predator_label_set()`
      procedure, public :: label_set => predator_label_set
      !> Accessor function for the predator body size (length).
      !! See `the_environment::predator_get_body_size()`
      procedure, public :: get_size => predator_get_body_size
      !> Accessor function for the predator attack rate .
      !! See `the_environment::predator_get_capture_efficiency()`
      procedure, public :: get_attack_rate => predator_get_attack_rate
      !> Calculates the risk of capture of the `prey_spatial` idealised spatial
      !! object with the body length `prey_length`. This is a backend function.
      !! See `the_environment::predator_capture_risk_calculate_fish()`.
      procedure, public :: risk_fish => predator_capture_risk_calculate_fish
      !> Calculates the risk of capture by a specific predator of an
      !! array of the fish agents with the spatial locations array
      !! defined by `prey_spatial` and the body length array
      !! `prey_length`. This subroutine takes account of both the predator
      !! dilution and confusion effects and risk adjusted by the distance
      !! towards the predator.
      !! See `the_environment::predator_capture_risk_calculate_fish_group() `.
      procedure, public :: risk_fish_group =>                                 &
                                    predator_capture_risk_calculate_fish_group
      !> Calculate the visibility range of this predator. Wrapper to the
      !! `visual_range` function. This function calculates the distance from
      !! which this predator can be seen by a visual object (e.g. prey).
      !! See `the_environment::predator_visibility_visual_range()`.
      procedure, public :: visibility => predator_visibility_visual_range
  end type PREDATOR

  !> Definition of the **environment habitat** `HABITAT` object.
  !! There can potentially be of several types of habitats (patches etc.), so
  !! the superclass HABITAT defines the most general properties and procedures.
  !! More specific procedures are defined in sub-objects. Such procedures can
  !! be overriden from super-object to sub-objects providing for procedure
  !! polymorphism.
  type, public, extends(ENVIRONMENT) :: HABITAT
    !> The name of the habitat
    character (len=LABEL_LENGTH) :: habitat_name
    !> Other agent mortality risks
    real(SRP) :: risk_mortality
    !> Egg mortality risk
    real(SRP) :: risk_egg_mortality
    !> Number of predators that dwell in the habitat.
    !... Habitat-specific predation ............................................
    integer :: predators_number
    !> Habitat has an array of predators (i.e. `PREDATOR` objects).
    !! @note The implementation of predators is very simplistic here, just
    !!       a single type of predators integrated into the `HABITAT` object,
    !!       without a separate predator container. This is, for example,
    !!       different from the food resources made as a FOOD_RESOURCE
    !!       container (below) that allows several types of food.
    !!       A more advanced version should implement a specific container
    !!       like `FOOD_RESOURCE` and, ultimately, a full implementation
    !!       of an AHA predator (with the genome, neurobiology etc.). Do we
    !!       need several types of predators or predation bound functions?
    type(PREDATOR), allocatable, dimension(:) :: predators
    !... Habitat-specific food resources .......................................
    !> Habitat has a food resource (i.e. FOOD_RESOURCE` object) which is
    !! an array of `FOOD_ITEM`s.
    !! @note A container object `FOOD_RESOURCE`is used for the food resource
    !!       rather than just raw number of food items and array of food items
    !!       (as done with predation) to allow implementation of several
    !!       different food resources more easily.
    type(FOOD_RESOURCE) :: food
    contains
      !> Make an instance of the habitat object.
      !! See `the_environment::habitat_make_init()`
      procedure, public :: make => habitat_make_init
      !> Return the name (label) of the habitat.
      !! See `the_environment::habitat_name_get()`.
      procedure, public :: get_label => habitat_name_get
      !> Get the mortality risk associated with this habitat.
      !! See `the_environment::habitat_get_risk_mortality()`.
      procedure, public :: get_mortality => habitat_get_risk_mortality
      !> Get the egg mortality risk associated with this habitat.
      !! See `the_environment::habitat_get_risk_mortality_egg()`.
      procedure, public :: get_egg_mort => habitat_get_risk_mortality_egg
      !> Save the predators with their characteristics into a CSV file.
      !! See `the_environment::habitat_save_predators_csv()`.
      procedure, public :: save_predators_csv => habitat_save_predators_csv

  end type HABITAT

  !> A list (array) of all the the_environment::habitat objects available
  !! to the agents. This single array should encompass all the locations that
  !! the agent can potentially be in (e.g. migrate from one to another).
  !!
  !! It is then very important that the separate habitat objects that are
  !! defined in the model are actually different data entities than the global
  !! array. If any change is made to the habitat objects after the global
  !! array was assembled, these must be synchronised with the array and vice
  !! versa.
  !!
  !! To determine where the agent (or any other spatial object) is currently
  !! located within use the the_environment::spatial::find_environment() method.
  !! The simplest form of assembling the global array is
  !! @code
  !!      allocate(Global_Habitats_Available(2))
  !!      Global_Habitats_Available = [habitat_safe, habitat_dangerous]
  !! @endcode
  !! A more powerful alternative is using the the_environment::assemble()
  !! procedure:
  !! @code
  !!      call assemble(habitat_safe, habitat_dangerous, reindex=.TRUE.)
  !! @endcode
  !! See the_environment::assemble() and the_environment::disassemble()
  !! procedures for more information on creating the global array of habitat
  !! objects and disassembling individual habitat objects back (updating the
  !! internal data components and arrays for each of the individual habitats.
  !!
  !! Here is an example of the steps necessary to use joined food resource
  !! from several assembled habitats:
  !! @code
  !!   ! 1. Assemble the global array of habitat objects
  !!   !    Global_Habitats_Available.
  !!   call assemble( habitat_test1, habitat_test2,                           &
  !!                  habitat_test3, habitat_test4 )
  !!
  !!   ! 2. Join returns a single food resource object out of those in the
  !!   !    global array Global_Habitats_Available
  !!   joined_food_res2 = join( reindex=.TRUE. )
  !!
  !!   ! 3. Modify the joined single food resource object in some way.
  !!   !    Here it just resets the sizes of the food items for a part
  !!   !    of the data.
  !!   joined_food_res2%food( 1:size(habitat_test1%food%food) )%size = 100.0
  !!
  !!   ! 4. Unjoin updates the food resources from the single global object
  !!   !    back to the global array Global_Habitats_Available.
  !!   call unjoin( joined_food_res2, reindex=.TRUE. )
  !!
  !!   ! 5. To complete unjoin, the updated food habitat and resource data
  !!   !    should be transferred back to the original separate habitat objects
  !!   !    usint `disassemble`
  !!   call disassemble( habitat_test1, habitat_test2,                       &
  !!                     habitat_test3, habitat_test4 )
  !! @endcode
  !! @note    Determining the environment object the agent is currently in
  !!          can be done by the_environment::spatial::find_environment()
  !!          method in this way:
  !!          @verbatim
  !!            ...
  !!                    environment_limits = Global_Habitats_Available(       &
  !!                                           this_agent%find_environment(   &
  !!                                             Global_Habitats_Available) )
  !!            ...
  !!          @endverbatim
  !! @note    Using a list of the_environment::habitat's rather than
  !!          the_environment::environment's because the agent dwells in a
  !!          habitat object and extended properties (e.g. habitat name) are
  !!          easily available in such a case.
  !! @warning It is not possible to define this global variable in the
  !!          commondata module because all environmental objects are defined
  !!          in a higher level hierarchy module the_environment.
  !! @warning This array must be initialised immediately after creating the
  !!          environmental objects / habitats:
  !!          the_evolution::init_environment_objects().
  !! @warning This global array definition cannot be moved to the start of
  !!          the module (for convenience), this results in the "object used
  !!          before it is defined" compiler error.
  type(HABITAT), dimension(:), allocatable, public :: Global_Habitats_Available

  !> Calculate  *surface light* intensity (that is subject to diel variation)
  !! for specific time step of the model. Irradiance can be *stochastic* if
  !! an optionallogical `stochastic` flag is set to `TRUE`.
  !! @details Light (`surlig`) is calculated from a sine function. Light
  !!          intensity just beneath the surface is modelled by assuming a
  !!          50 % loss by scattering at the surface:
  !!          @f[ L_{t} = L_{max} 0.5 sin(\pi dt / \Omega ) . @f]
  !! **Usage:**
  !! - deterministic:
  !!     @code
  !!       surface_light(1)
  !!     @endcode
  !! - stochastic:
  !!     @code
  !!       surface_light(1,YES)
  !!     @endcode
  !>
  !! @note    Note that it is impossible to do a simple single whole-elemental
  !!          implementation for this function as `random_number` is *never
  !!          pure* but  elemental can only work with all pure functions.
  interface light_surface
    module procedure light_surface_deterministic
    module procedure light_surface_stochastic_scalar
    module procedure light_surface_stochastic_vector
  end interface light_surface

  !> Calculate *underwater background irradiance* at specific depth
  !! @details Underwater light is attenuated following Beer’s law,
  !!          @f[ E_{b}(z,t) = L_{t} e^{-K z} , @f] where @f$ E_{b}(z,t) @f@
  !!          is background irradiance at depth z at time t and K is the
  !!          attenuation coefficient for downwelling irradiance.
  !! @note    The generic interface includes two elemental functions for integer
  !!          and real depth.
  interface light_depth
    module procedure light_depth_integer
    module procedure light_depth_real
  end interface light_depth

  !> Calculate visual range of predator  using Dag Aksnes's procedures
  !! `srgetr()`, `easyr()` and `deriv()`.
  !! @note This is a non-pure/elemental version with **debugging log output**.
  !! @warning The main interface name is `visual_range()`, it is this name
  !!       which is used throughout the code.
  !! @note It is possible to use either the "debug" (this) or "fast" (next)
  !!       generic interface for `visual_range()` by tweaking the interface
  !!       name, e.g. to switch to the debug version rename
  !!       `visual_range_debug()` to `visual_range()` and the next version to
  !!       `visual_range_disable()`.
  !!
  !! ### Specific implementations ###
  !! See specific implementations:
  !! - the_environment::visual_range_scalar() for scalar argument
  !! - the_environment::visual_range_vector() for vector argument
  !! - the_environment::visual_range_fast() elemental (parallel-safe) version
  !!   lacking sanity checks and extended debugging.
  !! .
  interface visual_range
    module procedure visual_range_scalar
    module procedure visual_range_vector
  end interface visual_range

  !> Calculate visual range of predator  using Dag Aksnes's procedures
  !! `srgetr()`, `easyr()` and `deriv()`.
  !! @note This is a pure/elemental version with **no** debugging log output.
  !! @warning The main interface name is `visual_range`, it is this name
  !!       which is used throughout the code.
  !! @warning  The parameter `prey_contrast` to the **vector**-based function
  !!           call must be an **scalar**. Otherwise a segmentation fault
  !!           runtime error results. Vector-based call is analogous to calling
  !!           `visual_range_vector()` with `prey_contrast_vect` parameter.
  interface visual_range_new
    module procedure visual_range_fast
  end interface visual_range_new

  !> Internal distance calculation backend engine.
  interface dist
    module procedure dist_scalar
    module procedure dist_vector_nd
  end interface dist

  !> An alias for the the_environment::food_resources_collapse_global_object()
  !! method for joining food resources into a single global food resource out
  !! of the global array the_environment::global_habitats_available.
  !! See the_environment::unjoin() for how to unjoin an array of food resources
  !! back into an array.
  interface join
    module procedure food_resources_collapse_global_object
  end interface join

  !> An alias to the_environment::food_resources_update_back_global_object()
  !! method to transfer (having been modified) food resource objects out from
  !! the single united object back to the global array
  !! the_environment::global_habitats_available.
  !! See the_environment::join() for how to join an array of food resources
  !! into a single global object.
  !! @warning Note that complete restoring the food resources back to each of
  !!          the individual habitat objects out of the global array must be
  !!          done using the the_environment::disassemble() procedure.
  interface unjoin
    module procedure food_resources_update_back_global_object
  end interface

  !> Interface to the procedure to **assemble** the global array of habitat
  !! objects the_environment::global_habitats_available from a list of separate
  !! habitat object components.
  !! This call
  !! @code
  !!   assemble(hab_a, hab_b, hab_c)
  !! @endcode
  !! is equivalent to
  !! @code
  !!   Global_Habitats_Available = [ hab_a, hab_b, hab_c ]
  !! @endcode
  !! See the_environment::global_habitats_assemble() for the backend
  !! implementation.
  interface assemble
    module procedure global_habitats_assemble
  end interface

  !> Interface to the procedure to **disassemble** the global habitats objects
  !! array the_environment::global_habitats_available back into separate
  !! habitat object components.
  !! See the_environment::global_habitats_disassemble() for the backend
  !! implementation.
  interface disassemble
    module procedure global_habitats_disassemble
  end interface disassemble

  !> Interface operator to concatenate two arrays of the spatial
  !! the_environment::spatial or spatial moving the_environment::spatial_moving
  !! objects.
  !! @code
  !!   object1%location() .cat. object2%location()
  !! @endcode
  !! See the_environment::spatial_stack2arrays() and
  !! the_environment::spatial_moving_stack2arrays() for backend implementation.
  !> @warning This operator works with fixed **types** rather than class.  All
  !!          input and output parameters are defined as **type**, so this
  !!          is not class-safe.
  interface operator (.cat.)
      procedure spatial_stack2arrays
      procedure spatial_moving_stack2arrays
  end interface operator (.cat.)

  !> Interface operator to concatenate the **location** components of two
  !! arrays ofthe_environment::spatial **class** objects.
  !! @code
  !!   all_objects%position%( object1 .catloc. object2 )
  !! @endcode
  !> @note Unlike the .cat. operator implemented using the
  !!       the_environment::spatial_stack2arrays() and
  !!       the_environment::spatial_moving_stack2arrays() methods, this
  !!       procedure is class-safe and can be used with any class upwards,
  !!       but it concatenates **only** the location data (returns **type**
  !!       the_environment::spatial).
  interface operator (.catloc.)
    procedure spatial_class_stack2arrays_locs
  end interface operator (.catloc.)

  !> Interface operators `.within.` for testing whether a spatial object (first
  !! argument lies within an environment (second argument). Usage:
  !! @code
  !!   if ( object .within. environment ) then
  !! @endcode
  !! See `the_environment::spatial_check_located_within_3d()`.
  interface operator (.within.)
    procedure spatial_check_located_within_3d
  end interface operator (.within.)

  !> Interface operators `.contains.` for testing whether an environment
  !! object (first argument) contains a `SPATIAL` object (second argument).
  !! Usage:
  !! @code
  !!   if ( environment .contains. object ) then
  !! @endcode
  !! See `the_environment::environment_check_located_within_3d()`.
  interface operator (.contains.)
    procedure environment_check_located_within_3d
  end interface operator (.contains.)

  !> Interface operators .above. for spatial objects. Usage:
  !! @code
  !!   object1 .above. object2
  !! @endcode
  !! Tests the condition of `object1` is above `object2`
  !! The operator can be used in two ways:
  !! - as an expression, with both scalar and array values:
  !!   @code
  !!     parents%ind(i) .above. parents%ind(i)%perceive_food%foods_seen
  !!   @endcode
  !! - in if blocks, only **scalars**:
  !!   @code
  !!     if ( parents%ind(i) .above. parents%ind(i)%perceive_food%foods_seen(1) )
  !!   @endcode
  !! .
  !! @note Note that the operator `.above.` refers to the "below" procedure
  !!       `the_environment::spatial_check_located_below` as the dummy
  !!       parameters have reverse order in this implementation procedure.
  interface operator (.above.)
    procedure spatial_check_located_below
  end interface operator (.above.)

  !> Interface operators .below. for spatial objects. Usage:
  !! @code
  !!   object1 .below. object2
  !! @endcode
  !! Tests the condition of `object1` is below `object2`
  !! The operator can be used in two ways:
  !! - as an expression, with both scalar and array values:
  !!   @code
  !!     parents%ind(i) .below. parents%ind(i)%perceive_food%foods_seen
  !!   @endcode
  !! - in if blocks, only **scalars**:
  !!   @code
  !!     if ( parents%ind(i) .below. parents%ind(i)%perceive_food%foods_seen(1) )
  !!   @endcode
  !! .
  !! @note Note that the operator `.below.` refers to the "above" procedure
  !!       `the_environment::spatial_check_located_above` as the dummy
  !!       parameters have reverse order in this implementation procedure.
  interface operator (.below.)
    procedure spatial_check_located_above
  end interface operator (.below.)

  !> Interface operator "-" for the the_environment::environment spatial
  !! container objects. Return an environment object that is shrunk by a
  !! fixed value in the 2D XxY plane.
  !! See `the_environment::environment_shrink_xy_fixed()`.
  !! The operator can be used as follows:
  !! @code
  !!    temp_hab = habitat_safe - 0.5_SRP
  !! @endcode
  interface operator (-)
    procedure environment_shrink_xy_fixed
  end interface operator (-)

  !-----------------------------------------------------------------------------

  !> These are public access functions, but probably we don't need to allow
  !!   public access to functions inside generic interfaces
  public  ::  light_surface, light_depth, visual_range
  !> We do not need specific functions outside of this module, always use
  !! generic functions.
  private ::  light_surface_deterministic,                                    &
              light_surface_stochastic_scalar,                                &
              light_surface_stochastic_vector,                                &
              light_depth_integer,                                            &
              light_depth_real,                                               &
              visual_range_scalar,                                            &
              visual_range_vector,                                            &
              srgetr, easyr, deriv

contains ! ........ implementation of procedures for this level ................

  !-----------------------------------------------------------------------------
  !> Create an empty spatial object. The object's starting coordinates get
  !! all `MISSING` values.
  elemental subroutine spatial_create_empty(this)
    class(SPATIAL), intent(inout) :: this

    call this%missing()

  end subroutine spatial_create_empty

  !-----------------------------------------------------------------------------
  !> Create the highest level container environment.
  !! Set the size of the 3D environment container as two coordinate vectors
  !! setting the minimum  and maximum coordinate limits:
  !! `min_coord(1)` for *x*, `min_coord(2)` for *y*, `min_coord(3)` for *z*
  !! The size of the environment should be set from parameter vectors
  !! in `COMMONDATA`.
  !! @param min_coord Minimum coordinate bound for the environment.
  !! @param max_coord Maximum coordinate bound for the environment.
  !! @note This version accepts simple *arrays* as the environment coordinates.
  !! @warning Not-extensible version. TODO: Do we need it? Deprecate?
  !!          There is a generic function `build` that should normally be used.
  subroutine environment_whole_build_vector(this, min_coord, max_coord)
    class(ENVIRONMENT), intent(inout) :: this
    ! Set the size of the 3D environment container as two coordinate vectors
    ! setting the minimum  and maximum coordinate limits:
    ! `min_coord(1)` for *x*, `min_coord(2)` for *y*, `min_coord(3)` for *z*
    ! The size of the environment should be set from parameter vectors
    ! in `COMMONDATA`.
    real(SRP), dimension(3), intent(in) :: min_coord, max_coord

    ! Set the environment limits from the parameter vectors.
    ! @note We use standard type-bound function `position` with
    !       type constructor for `SPATIAL` here.
    call this%coord_min%position( SPATIAL(                                    &
                                    min_coord(1), min_coord(2), min_coord(3)) )
    call this%coord_max%position( SPATIAL(                                    &
                                    max_coord(1), max_coord(2), max_coord(3)) )

  end subroutine environment_whole_build_vector

  !-----------------------------------------------------------------------------
  !> Create the highest level container environment.
  !! Set the size of the 3D environment container as two coordinate vectors
  !! setting the minimum  and maximum coordinate limits. The parameters
  !! `min_coord` and `max_coord` are SPATIAL objects.
  !! @param min_coord Minimum coordinate bound for the environment,
  !!        `SPATIAL` object.
  !! @param max_coord Maximum coordinate bound for the environment,
  !!        `SPATIAL` object.
  !! @note This version accepts `SPATIAL` *objects* as the environment
  !!       coordinates.
  subroutine environment_whole_build_object(this, min_coord, max_coord)
    class(ENVIRONMENT), intent(inout) :: this
    ! Set the size of the 3D environment container as two coordinate vectors
    ! setting the minimum  and maximum coordinate limits. The parameters
    ! `min_coord` and `max_coord` are SPATIAL objects.
    type(SPATIAL), intent(in) :: min_coord, max_coord

    ! Set the environment limits from the parameter SPATIAL container objects.
    this%coord_min = min_coord
    this%coord_max = max_coord

  end subroutine environment_whole_build_object

  !-----------------------------------------------------------------------------
  !> Build an **unlimited environment**, with the spatial coordinates limited
  !! by the maximum machine supported values based on the intrinsic `huge`
  !! function.
  subroutine environment_build_unlimited(this)
    class(ENVIRONMENT), intent(inout) :: this

    ! Local constant parameter setting the largest coordinate possible for the
    ! unlimited environment. This is actually the biggest real type constant
    ! that can be represented by the machione CPU. Also the min. coordinates.
    real(SRP), parameter :: MAX_COORD=huge(0.0)
    real(SRP), parameter :: MIN_COORD=-1_SRP*huge(0.0)

    call this%build( SPATIAL(MIN_COORD, MIN_COORD, MIN_COORD),                &
                     SPATIAL(MAX_COORD, MAX_COORD, MAX_COORD) )

  end subroutine environment_build_unlimited

  !-----------------------------------------------------------------------------
  !> Return an environment object that is shrunk by a fixed value in the 2D
  !! XxY plane.
  !!
  !> Here is an illustration of the function. The outer box is the
  !! input environment, the inner box is the shrunken environment that is
  !! returned. The shrinkage value is fixed, defined by the second function
  !! parameter. The depth is ignored in this function working with the
  !! simplistic box-like environment objects.
  !> @verbatim
  !! min_coord is obtained   +---------------------+
  !! by coordinate addition; | +                   |
  !!                         |   +-------------+   |
  !!                         |   |             |   |
  !!                         |-->|             |<--|
  !!                         |   |             |   |
  !!                         |   |             |   |
  !!                         |   +-------------+   |
  !!                         |                   - |  max_coord is obtained by
  !!                         +---------------------+  coordinate subtraction.
  !! @endverbatim
  !> There is a user defined operator `-` (minus), that can be used as follows:
  !! @code
  !!    temp_hab = habitat_safe - 0.5_SRP
  !! @endcode
  !! @warning Valid only for the simplistic box-like environments;
  !!          should be reimplemented if the environment is implemented as an
  !!          arbitrary polyhedron.
  function environment_shrink_xy_fixed(this, shrink_value) result (shrunken)
    class(ENVIRONMENT), intent(in) :: this
    real(SRP), intent(in) :: shrink_value
    type(ENVIRONMENT) :: shrunken

    call shrunken%build( [ this%coord_min%x + shrink_value,                   &
                           this%coord_min%y + shrink_value,                   &
                           this%coord_min%depth ],                            &
                         [ this%coord_max%x - shrink_value,                   &
                           this%coord_max%y - shrink_value,                   &
                           this%coord_max%depth ]             )

  end function environment_shrink_xy_fixed

  !-----------------------------------------------------------------------------
  !> Function to get the **minimum** spatial limits (coordinates) of
  !! the environment.
  !! @returns The minimum spatial bound of the environment, as
  !!          a `SPATIAL` object.
  function environment_get_minimum_obj(this) result (posout)
    class(ENVIRONMENT), intent(in) :: this

    ! @returns The minimum spatial bound of the environment, as
    !          a `SPATIAL` object.
    type(SPATIAL) :: posout

    posout = this%coord_min%location()

  end function environment_get_minimum_obj

  !-----------------------------------------------------------------------------
  !> Function to get the **maximum** spatial limits (coordinates) of
  !! the environment.
  !! @returns The maximum spatial bound of the environment, as
  !!          a `SPATIAL` object.
  function environment_get_maximum_obj(this) result (posout)
    class(ENVIRONMENT), intent(in) :: this

    ! @returns The maximum spatial bound of the environment, as
    !          a `SPATIAL` object.
    type(SPATIAL) :: posout

    posout = this%coord_max%location()

  end function environment_get_maximum_obj

  !-----------------------------------------------------------------------------
  !> Get the **minimum depth** in this environment.
  elemental function environment_get_minimum_depth(this) result (mindepth)
    class(ENVIRONMENT), intent(in) :: this
    !> @return The maximum depth of this environment
    real(SRP) :: mindepth

    mindepth = min( this%coord_min%dpos(), this%coord_max%dpos() )

  end function environment_get_minimum_depth

  !-----------------------------------------------------------------------------
  !> Get the **maximum depth** in this environment.
  elemental function environment_get_maximum_depth(this) result (maxdepth)
    class(ENVIRONMENT), intent(in) :: this
    !> @return The maximum depth of this environment
    real(SRP) :: maxdepth

    maxdepth = max( this%coord_min%dpos(), this%coord_max%dpos() )

  end function environment_get_maximum_depth

  !-----------------------------------------------------------------------------
  !> Get the corners of the environment in the 2D X Y plane. This is a very
  !! simplistic procedure that works only with the box environmental objects.
  !! @warning Should be reimplemented if the environment is implemented as an
  !!          arbitrary polyhedron.
  pure function environment_get_corners_2dxy (this, ref_depth, offset)        &
                                                              result (corners)
    class(ENVIRONMENT), intent(in) :: this
    !> @param[in] ref_depth optional parameter setting the fixed depth for the
    !!            returned corner objects. If not provided, a fixed default
    !!            value equal to the local parameter `REF_DEPTH_DEF=0.0` is
    !!            used.
    real(SRP), optional, intent(in) :: ref_depth
    !> @param[in] offset The offset that can be set to make the borders and
    !!            corners of the environment inside at a fixed value. If
    !!            offset is absent, the actual corners of the environment
    !!            are returned. The offset parameter works as the `shrink2d`
    !!            function (the_environment::environment::shrink2d()`.
    !>            @verbatim
    !!              .---------------------.
    !!              | +                   |
    !!              |   *-------------*   |
    !!              |   |             |   |
    !!              |   |             |<--| offset shrinks corners
    !!              |   |             |   |
    !!              |   |             |   |
    !!              |   *-------------*   |
    !!              |                   - |
    !!              .---------------------.
    !!            @endverbatim
    real(SRP), optional, intent(in) :: offset
    !> @return Returns an array of the environment corners in the
    !!         2D *X* x *Y* plane. The number of corners for the environment
    !!         object in the 2D *X* x *Y* plane is defined by the parameter
    !!         constant the_environment::dim_environ_corners.
    type(SPATIAL), dimension(DIM_ENVIRON_CORNERS) :: corners

    ! Default reference depth, i.e. the depth coordinate of the
    ! returned corners objects.
    real(SRP), parameter :: REF_DEPTH_DEF = 0.0_SRP

    ! Local copies of optionals
    real(SRP) :: ref_depth_loc, offset_loc

    ! Check optional parameter and est default.
    if (present(ref_depth)) then
      ref_depth_loc = ref_depth
    else
      ref_depth_loc = REF_DEPTH_DEF
    end if
    if (present(offset)) then
      offset_loc = offset
    else
      offset_loc = 0.0_SRP
    end if

    !> ### Implementation details ###
    !> The corners 1,2,3,4 of the simplistic box-like environmental object are
    !! defined as follows:
    !! @verbatim
    !!   (1:minX,minY) ----- (2:maxX,minY)
    !!    |                         |
    !!   (4:minX,maxY) ----- (3:maxX,maxY)
    !! @endverbatim
    !! These four points are returned as the the_environment::spatial
    !! class objects.
    !! @warning The order of the points is important because it is also
    !!          used in the `nearest_target` procedure
    !!          the_environment::environment_get_nearest_point_in_outside_obj().
    corners(1) = SPATIAL(   this%coord_min%x + offset_loc,                    &
                            this%coord_min%y + offset_loc, ref_depth_loc)

    corners(2) = SPATIAL(   this%coord_max%x - offset_loc,                    &
                            this%coord_min%y + offset_loc, ref_depth_loc)

    corners(3) = SPATIAL(   this%coord_max%x - offset_loc,                    &
                            this%coord_max%y - offset_loc, ref_depth_loc)

    corners(4) = SPATIAL(   this%coord_min%x + offset_loc,                    &
                            this%coord_max%y - offset_loc, ref_depth_loc)

  end function environment_get_corners_2dxy

  !-----------------------------------------------------------------------------
  !> Check if a spatial object is actually within this environment.
  !! @returns TRUE if the spatial object is located within the environment.
  !! @param check_object A spatial object (`SPATIAL` or any **extension**)
  !!        to check.
  !> There is a user-defined operator:
  !! @code
  !!   if ( environment .contains. object ) then
  !! @endcode
  elemental function environment_check_located_within_3d(this, check_object)  &
                                                        result(is_within)
    class(ENVIRONMENT), intent(in) :: this

    ! @returns TRUE if the spatial object is located within the environment.
    logical :: is_within

    ! @param check_object A spatial object (`SPATIAL` or any **extension**)
    !        to check.
    class(SPATIAL), intent(in) :: check_object

    ! Here we just compare the 3D coordinates of the two objects.
    ! @note Check if we can use extensible within object specific functions
    !       instead of raw object components for 3D `SPATIAL` environment.
    if (check_object%x     >= this%coord_min%x     .and.                      &
        check_object%y     >= this%coord_min%y     .and.                      &
        check_object%depth >= this%coord_min%depth .and.                      &
        check_object%x     <= this%coord_max%x     .and.                      &
        check_object%y     <= this%coord_max%y     .and.                      &
        check_object%depth <= this%coord_max%depth ) then
      is_within = .TRUE.
    else
      is_within = .FALSE.
    end if

  end function environment_check_located_within_3d

  !-----------------------------------------------------------------------------
  !> Generate a random spatial object with the uniform distribution within
  !! (i.e. bound to) **this** environment.
  !! @returns uniformly distributed random `SPATIAL` object bound to
  !!          `this` environment.
  function environment_random_uniform_spatial_3d(this) result (uniform)
    class(ENVIRONMENT), intent(in) :: this

    ! @returns uniformly distributed random `SPATIAL` object bound to
    !          `this` environment.
    type(SPATIAL) :: uniform

    ! @note Use type constructor here, but can use position_v type bound
    !       function instead. Didn't actually use it as try to survive
    !       without such `_v` non-object functions. If succeed, remove all
    !       of them.
    uniform = SPATIAL( RAND(this%coord_min%x,     this%coord_max%x),          &
                       RAND(this%coord_min%y,     this%coord_max%y),          &
                       RAND(this%coord_min%depth, this%coord_max%depth) )

  end function environment_random_uniform_spatial_3d

  !-----------------------------------------------------------------------------
  !> Generate a random spatial object with the uniform distribution within
  !! (i.e. bound to) **this** environment, the third depth coordinate is fixed.
  !! @returns uniformly distributed random `SPATIAL` object bound to
  !!          `this` environment.
  function environment_random_uniform_spatial_2d(this, fixdepth) result(uniform)
    class(ENVIRONMENT), intent(in) :: this

    real(SRP), intent(in) :: fixdepth

    ! @returns uniformly distributed random `SPATIAL` object bound to
    !          `this` environment.
    type(SPATIAL) :: uniform

    ! @note Use type constructor here, but can use position_v type bound
    !       function instead. Didn't actually use it as try to survive
    !       without such `_v` non-object functions. If succeed, remove all
    !       of them.
    uniform = SPATIAL( RAND(this%coord_min%x,     this%coord_max%x),          &
                       RAND(this%coord_min%y,     this%coord_max%y),          &
                       fixdepth                                       )

  end function environment_random_uniform_spatial_2d

  !-----------------------------------------------------------------------------
  !> Generate a vector of random spatial objects with the uniform distribution
  !! within (i.e. bound to) **this** environment.
  !! @param the dimension(size) of the vector to generate
  !! @returns uniformly distributed random `SPATIAL` object bound to
  !!          `this` environment.
  !! @warning **Intel Fortran porting note**. This whole array function **does
  !!          not work under Intel Fortran 13**, issues *stack overflow*
  !!          runtime error, although compiles without issues:
  !!          `loc_food_here = this%uniform(this%food%number_food_items)`.
  function environment_random_uniform_spatial_vec_3d(this, num) result (uniform)
    class(ENVIRONMENT), intent(in) :: this

    ! @param the dimension(size) of the vector to generate
    integer, intent(in) :: num

    ! @returns uniformly distributed random `SPATIAL` object bound to
    !          `this` environment.
    type(SPATIAL), dimension(num) :: uniform

    ! Local counter.
    integer :: i

    ! We call the type bound `uniform_s=>environment_random_uniform_spatial_3d`
    ! to get a vector of uniformly-distributed spatial objects.
    do i=1, num
      ! @warning **Intel Fortran porting note**. Setting objects directly
      !          without using type-bound function:
      !            `uniform(i) = this%uniform_s()`
      !          does **not** avoid intel fortran stack overflow runtime
      !          error. Still **have** to use scalar rather than vector
      !          function `uniform` for scattering lots of objects.
      !uniform(i) = SPATIAL( RAND(this%coord_min%x,     this%coord_max%x),    &
      !                      RAND(this%coord_min%y,     this%coord_max%y),    &
      !                      RAND(this%coord_min%depth, this%coord_max%depth) )
      ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ! @note    Intel Fortran 17 should now work well, restored call to
      !          the type-bound function.
      uniform(i) = this%uniform_s()

    end do

  end function environment_random_uniform_spatial_vec_3d

  !-----------------------------------------------------------------------------
  !> Generate a vector of random spatial objects with the uniform distribution
  !! within (i.e. bound to) **this** environment. The third, depth coordinate
  !! is non-stochastic, and provided as an array parameter.
  !! @param the dimension(size) of the vector to generate
  !! @returns uniformly distributed random `SPATIAL` object bound to
  !!          `this` environment.
  function environment_random_uniform_spatial_vec_2d(this, fixdep_array)      &
                                                              result (uniform)
    class(ENVIRONMENT), intent(in) :: this

    ! @param the dimension(size) of the vector to generate
    real(SRP), dimension(:), intent(in) :: fixdep_array

    ! @returns uniformly distributed random `SPATIAL` object bound to
    !          `this` environment.
    type(SPATIAL), dimension(size(fixdep_array)) :: uniform

    ! Local counter.
    integer :: i

    !> ### Implementation details ###
    !> We call the type bound `uniform_s=>environment_random_uniform_spatial_3d`
    !! to get a vector of uniformly-distributed spatial objects.
    do i=1, size(fixdep_array)
      ! @warning **Intel Fortran porting note**. Setting objects directly
      !          without using type-bound function:
      !            `uniform(i) = this%uniform_s()`
      !          does **not** avoid intel fortran stack overflow runtime
      !          error. Still **have** to use scalar rather than vector
      !          function `uniform` for scattering lots of objects.
      uniform(i) = SPATIAL( RAND(this%coord_min%x,     this%coord_max%x),     &
                            RAND(this%coord_min%y,     this%coord_max%y),     &
                            fixdep_array(i)                              )
    end do

  end function environment_random_uniform_spatial_vec_2d

  !-----------------------------------------------------------------------------
  !> Generates a vector of random spatial object with Gaussian coordinates
  !! within (i.e. bound to) **this** environment.
  !! @param num the dimension(size) of the vector to generate
  !! @param centroid Optional centroid of the Gaussian scatter. If not
  !!        provided, will select random uniformly distributed point.
  !! @param variance Gaussian variance parameter.
  !! @returns Gaussian ranrom SPATIAL object bound to `this` environment.
  function environment_random_gaussian_spatial_3d (this,                      &
                                    num, centroid, variance) result (gaussian)
    class(ENVIRONMENT), intent(in) :: this

    ! @param num the dimension(size) of the vector to generate
    integer, intent(in) :: num

    ! @param centroid Optional centroid of the Gaussian scatter. If not
    !        provided, will select random uniformly distributed point.
    class(SPATIAL), optional, intent(in) :: centroid

    ! @param variance Gaussian variance parameter.
    real(SRP), optional, intent(in) :: variance

    ! @returns Gaussian ranrom SPATIAL object bound to `this` environment.
    type(SPATIAL), dimension(num) :: gaussian

    ! Local copy of the centroid
    type(SPATIAL) :: centroid_here

    ! Local random coordinates
    real(SRP) :: x_coord, y_coord, d_coord, variance_here

    ! Local counter
    integer :: i

    !> ### Implementation details ###
    !> First, check optional centroid and set local centroid.
    if (present(centroid)) then
      if ( this%is_within(centroid) ) then
        !> The centroid that is provided is accepted only if it is within
        !! **this** environment.
        call centroid_here%position( centroid%location() )
      else
        !> If a centroid is provided but is outside of the environment,
        !! reset it to random uniform.
        call centroid_here%position( this%uniform() )
      end if
    else
      call centroid_here%position( this%uniform() )
    end if

    ! Check if variance parameter is provided.
    if (present(variance)) then
      variance_here = variance
    else
      variance_here = 1.0_SRP
    end if

    !> Now, generate Gaussian spatial objects and fill the output array.
    do i=1, num
      !> First, generate random Gaussian coordinanes for a temporary spatial
      !! object.
      x_coord = RNORM( centroid_here%x, variance_here )
      y_coord = RNORM( centroid_here%y, variance_here )
      d_coord = RNORM( centroid_here%depth, variance_here )
      !> Make sure this spatial object is within the bounding environment.
      do while ( this%is_within( SPATIAL(x_coord, y_coord, d_coord) ) )
        x_coord = RNORM( centroid_here%x, variance_here )
        y_coord = RNORM( centroid_here%y, variance_here )
        d_coord = RNORM( centroid_here%depth, variance_here )
      end do
      !> Finally, set assign the output array component to this object.
      gaussian(i) = SPATIAL(x_coord, y_coord, d_coord)
    end do

  end function environment_random_gaussian_spatial_3d

  !-----------------------------------------------------------------------------
  !> Generates a vector of random spatial object with Gaussian coordinates
  !! within (i.e. bound to) **this** environment. The depth coordinate is
  !! set separately and can be non-random (fixed for the whole output array)
  !! or Gaussian with separate variance.
  !! @param num the dimension(size) of the vector to generate
  !! @param centroid Optional centroid of the Gaussian scatter. If not
  !!        provided, will select random uniformly distributed point.
  !! @param fixdepth Optional fixed depth for the generated Gaussian objects.
  !! @param variance Gaussian variance parameter.
  !! @param variance_depth Gaussian variance parameter for the fixed depth.
  !! @returns Gaussian random SPATIAL object bound to `this` environment.
  function environment_random_gaussian_spatial_2d (this,                      &
                          num, centroid, fixdepth, variance, variance_depth)  &
                                                              result (gaussian)
    class(ENVIRONMENT), intent(in) :: this

    ! @param num the dimension(size) of the vector to generate
    integer, intent(in) :: num

    ! @param centroid Optional centroid of the Gaussian scatter. If not
    !        provided, will select random uniformly distributed point.
    class(SPATIAL), optional, intent(in) :: centroid

    ! @param fixdepth Optional fixed depth for the generated Gaussian objects.
    real(SRP), optional, intent(in) :: fixdepth

    ! @param variance Gaussian variance parameter.
    real(SRP), optional, intent(in) :: variance

    ! @param variance_depth Gaussian variance parameter for the fixed depth.
    real(SRP), optional, intent(in) :: variance_depth

    ! @returns Gaussian random SPATIAL object bound to `this` environment.
    type(SPATIAL), dimension(num) :: gaussian

    ! Local copy of the centroid
    type(SPATIAL) :: centroid_here

    ! Local random coordinates
    real(SRP) :: x_coord, y_coord, d_coord, variance_here

    ! Local counter
    integer :: i

    !> ### Implementation details ###
    !> First, check optional centroid parameter and set local centroid.
    CHECK_CENTROID: if (present(centroid)) then
      CHECK_DEPTH1: if (present(fixdepth)) then
        !> Make sure fixdepth is within the allowed environmental range.
        if ( fixdepth >= this%coord_min%dpos() .and.                          &
             fixdepth <= this%coord_max%dpos()  ) then
          !> If fixed depth is provided, we use the centroid but take the depth
          !! from the `fixdepth` parameter.
          call centroid_here%position(                                        &
                    SPATIAL( centroid%xpos(), centroid%ypos(), fixdepth )  )
          !> And check if the resulting centroid is within this environment.
          !! If not, make it random uniform making use of the fixed depth.
          if (.not. this%is_within( centroid_here ) ) then
            centroid_here = centroid_urandom( fixdepth )
          end if
        else
          !> If the fixdepth is not conformant with this environment, use
          !! centroid provided and discard the fixdepth parameter
          call centroid_here%position( centroid%location() )
          !> And check if the resulting centroid is within this environment,
          !! if not, discard both the centroid and the fixed depth parameters
          !! and use random uniform centroid as the last resort.
          if (.not. this%is_within( centroid_here ) ) then
            centroid_here = centroid_urandom( )
          end if
        end if
      else CHECK_DEPTH1
        !> If fixed depth is not provided, however, we use the fixed depth
        !! from the centroid
        if ( this%is_within(centroid) ) then
          !> The centroid that is provided is accepted only if it is within
          !! **this** environment.
          call centroid_here%position( centroid%location() )
        else
          !> If a centroid is provided but is outside of the environment,
          !! reset it to random uniform.
          centroid_here = centroid_urandom( centroid%dpos() )
          !call centroid_here%position( this%uniform( centroid%dpos() ) )
          ! But make sure the random centroid is still within this environment.
          !do while ( this%is_within(centroid_here) )
          !  call centroid_here%position( this%uniform( centroid%dpos() ) )
          !end do
        end if
      end if CHECK_DEPTH1
    else CHECK_CENTROID
      CHECK_DEPTH2: if (present(fixdepth)) then
        if ( fixdepth >= this%coord_min%dpos() .and.                          &
             fixdepth <= this%coord_max%dpos()  ) then
          !> Check if it's conformant with the environment. Use if if okay.
          call centroid_here%position( this%uniform( fixdepth ) )
        else
          !> If the depth provided is not conformant, discard and use random.
          call centroid_here%position( this%uniform( ) )
        end if
      else CHECK_DEPTH2
        !> Finally, if neither centroid nor depth is provided, use random
        !! uniform.
        call centroid_here%position( this%uniform( ) )
      end if CHECK_DEPTH2
    end if CHECK_CENTROID

    ! Check if variance parameter is provided.
    if (present(variance)) then
      variance_here = variance
    else
      variance_here = 1.0_SRP
    end if

    !> Check if separate variance parameter for the depth is provided. This
    !! sets if **stochastic depth** is to be generated.
    DEPTH_STOCHASTIC: if (present(variance_depth)) then
      !> If separate depth variance parameter is provided, depth is stochastic
      !! Gaussian. Generate Gaussian spatial objects and fill the output array.
      do i=1, num
        !> First, generate random Gaussian coordinanes for a temporary spatial
        !! object.
        x_coord = RNORM( centroid_here%x, variance_here )
        y_coord = RNORM( centroid_here%y, variance_here )
        d_coord = RNORM( centroid_here%depth, variance_depth )
        ! And make sure this spatial object is within the bounding environment.
        do while ( .not. this%is_within( SPATIAL(x_coord, y_coord, d_coord) ) )
          x_coord = RNORM( centroid_here%x, variance_here )
          y_coord = RNORM( centroid_here%y, variance_here )
          d_coord = RNORM( centroid_here%depth, variance_depth )
        end do
        !> Finally, set assign the output array component to this object.
        gaussian(i) = SPATIAL(x_coord, y_coord, d_coord)
      end do
    else DEPTH_STOCHASTIC
      !> If there is no separate variance parameter for the depth,
      !! **depth fixed** deterministic, identical in for the whole array.
      !! Set the fixed depth.
      d_coord = centroid_here%dpos()
      !> Now, generate Gaussian spatial objects and fill the output array.
      do i=1, num
        !> First, generate random Gaussian coordinanes for a temporary spatial
        !! object.
        x_coord = RNORM( centroid_here%x, variance_here )
        y_coord = RNORM( centroid_here%y, variance_here )
        ! And make sure this spatial object is within the bounding environment.
        do while ( .not. this%is_within( SPATIAL(x_coord, y_coord, d_coord) ) )
          x_coord = RNORM( centroid_here%x, variance_here )
          y_coord = RNORM( centroid_here%y, variance_here )
        end do
        !> Finally, set assign the output array component to this object.
        gaussian(i) = SPATIAL(x_coord, y_coord, d_coord)
      end do
    end if DEPTH_STOCHASTIC

    contains
      !> Make a random centroid with fixed depth bound within **this**
      !! environment.
      function centroid_urandom( fixed_depth ) result ( centroid_out )
      type(SPATIAL) :: centroid_out
      real(SRP), optional :: fixed_depth

      if (present(fixed_depth)) then
        ! Make a random uniform centroid.
        centroid_out = this%uniform( fixed_depth )
        ! Making sure it is within the bounding environment.
        do while ( .not. this%is_within(centroid_out) )
          centroid_out = this%uniform( fixed_depth )
        end do
      else
        ! Make a random uniform centroid.
        centroid_out = this%uniform( )
        ! Making sure it is within the bounding environment.
        do while ( .not. this%is_within(centroid_out) )
          centroid_out = this%uniform( )
        end do
      end if

      end function centroid_urandom

  end function environment_random_gaussian_spatial_2d

  !-----------------------------------------------------------------------------
  !> Get the spatial point position within this environment that is nearest to
  !! an arbitrary spatial object located outside of the this environment. If
  !! the spatial object is actually located in this environment, return its own
  !! position.
  !! @note    This function is necessary for the implementation of migration
  !!          behaviour across two or several environments or habitats: it
  !!          allows to set the (nearest) target point in the desired target
  !!          environment.
  !! @warning Valid only for the simplistic box-like environments;
  !!          should be reimplemented if the environment is implemented as an
  !!          arbitrary polyhedron.
  subroutine environment_get_nearest_point_in_outside_obj(this, outside_object,&
                                        offset_into, point_spatial, point_dist)
    class(ENVIRONMENT), intent(in) :: this
    !> @param[in] outside_object is the outside object, the minimum distances
    !!            to the environment and the closest point are evaluated for
    !!            this object.
    class(SPATIAL), intent(in) :: outside_object
    !> @param[in] offset_into optional offset guaranteeing that the nearest
    !!            point is located more or less deeply within the this target
    !!            environment rather than just at the border of the
    !!            environment. This parameter is useful if the nearest point
    !!            in the environment actually sets the target point, to which
    !!            the agent represented by the `outside_object` spatial is
    !!            relocating. In such a case, the agent is therefore moving
    !!            into the environment, at a distance `offset_into` rather
    !!            than just to the edge of this target environment.
    !!            This is illustrated by the below:
    !>            @verbatim
    !!              .----------------------. this environment
    !!              | +                    |
    !!              |   *--------------*   |
    !!              |   | target point |   |          outside_object
    !!              |   | is inside   (*)<------------<*=><
    !!              |   |              |   |
    !!              |   |             >|---|<-------- offset_into
    !!              |   *--------------*   |
    !!              |                    - |
    !!              .------------------ ---.
    !!            @endverbatim
    real(SRP), optional, intent(in) :: offset_into

    !> @return The point within the this environment that is the nearest to the
    !!         `outside_object` the_environment::spatial class target object.
    type(SPATIAL), optional, intent(out) :: point_spatial

    real(SRP), optional, intent(out) :: point_dist

    ! Local counter
    integer :: i

    ! Local variable representing the four corners of the this environment
    type(SPATIAL), dimension(DIM_ENVIRON_CORNERS) :: corners

    ! Arrays of the nearest objects between the outside point and all the
    ! outer segments of the this environment.
    type(SPATIAL), dimension(DIM_ENVIRON_CORNERS) :: segment_nearest_obj
    real(SRP), dimension(DIM_ENVIRON_CORNERS) :: segment_distance

    !> ### Implementation notes ###
    !> First, check if the outside object is actually located within the target
    !! environment.
    !! - If so, the distance between the object and the environment
    !!   is zero. This is a degenerate case that is treated separately: the
    !!   nearest point within the environment coincides with the location of
    !!   the outside object itself.
    !! .
    if ( this%is_within(outside_object) ) then
      if (present(point_spatial))                                             &
            call point_spatial%position( outside_object%location() )
      if (present(point_dist)) point_dist = 0.0_SRP
      return
    end if

    !> If the object is indeed outside,  determine the four corners of the
    !! `this` environment. These corners are delimiting the outside of the
    !! this environment. The reference depth is set to the depth of the
    !! outside object.
    if (present(offset_into)) then
      !> @nolte Note that if the `offset_into` offset parameter is set, the
      !!        corners are adjusted to this offset value, so that they are
      !!        actually inside the this environment object.
      corners = this%corners2d( ref_depth = outside_object%depth,             &
                                offset=offset_into )
    else
      corners = this%corners2d( ref_depth = outside_object%depth )
    end if

    !> Calculate the distances and the nearest points between the
    !! outside object and the four outer segments (1,2), (2,3), (3,4), (4,1).
    !! @warning This fast but **unsafe** implementation requires
    !!          **the same order of points** to be set also in the
    !!          the_environment::environment_get_corners_2dxy().
    !!          For arbitrary order of points, a full cross loop is needed,
    !!          with the sizes of `the segment_nearest_obj` `segment_distance`
    !!          arrays set to the square
    !!          `DIM_ENVIRON_CORNERS * DIM_ENVIRON_CORNERS`.
    !! @verbatim
    !!   1, 2               1------------2
    !!   2, 3               |            |
    !!   3, 4               |            |
    !!   4, 1               4------------3
    !! @endverbatim
    do i=1, size(corners)-1
      call outside_object%distance_segment2d(                                 &
                    sectp1=corners( i ),                                      &
                    sectp2=corners( i+1 ),                                    &
                    min_dist=segment_distance( i ),                           &
                    point_segment=segment_nearest_obj( i ))
    end do
    call outside_object%distance_segment2d(                                   &
                    sectp1=corners( size(corners) ),                          &
                    sectp2=corners( 1 ),                                      &
                    min_dist=segment_distance( size(corners) ),               &
                    point_segment=segment_nearest_obj( size(corners) ))


    !> Finally, the returned nearest point is the point where the distance
    !! between the outside point and any of the outer segments of the
    !! environment reaches the minimum value.
    if (present(point_spatial))                                               &
        point_spatial = segment_nearest_obj( minloc(segment_distance,1) )

    if (present(point_dist)) point_dist = minval(segment_distance)

  end subroutine environment_get_nearest_point_in_outside_obj

  !-----------------------------------------------------------------------------
  !> Place spatial object into a 3D space, define the object's current
  !! coordinates
  !! @param coordinates The spatial objects will now get these coordinates.
  !! @note    This is actually the **constructor** for the `SPATIAL` object
  !!          giving it its value and existence.
  !! @note    This version takes *scalar coordinates* as the argument.
  !! @warning This implementation is **not extensible**.
  subroutine spatial_fix_position_3d_s(this, x, y, depth)
    class(SPATIAL), intent(inout) :: this

    ! @param coordinates The spatial objects will now get these coordinates.
    real(SRP), intent(in) :: x,y,depth

    this%x = x
    this%y = y
    this%depth = depth

  end subroutine spatial_fix_position_3d_s

  !-----------------------------------------------------------------------------
  !> Place spatial object into a 3D space, define the object's current
  !! coordinates.
  !! @param location SPATIAL location that will be assigned to the current
  !!        spatial object.
  !! @note This is actually the **constructor** for the `SPATIAL` object giving
  !!       it its value and existence.
  !! @note This version takes a `SPATIAL` object as argument.
  elemental subroutine spatial_fix_position_3d_o(this, location)
    class(SPATIAL), intent(inout) :: this

    ! @param location SPATIAL location that will be assigned to the current
    !        spatial object.
    type(SPATIAL), intent(in) :: location

    this%x = location%x
    this%y = location%y
    this%depth = location%depth

  end subroutine spatial_fix_position_3d_o

  !-----------------------------------------------------------------------------
  !> Assign all commondata::missing` coordinates to the_environment::spatial
  !! object.
  elemental subroutine spatial_make_missing(this)
    class(SPATIAL), intent(inout) :: this

    ! Use standard object function to set it missing, use type constructor
    ! to set specific values.
    call this%position( SPATIAL(MISSING,MISSING,MISSING) )

  end subroutine spatial_make_missing

  !-----------------------------------------------------------------------------
  !> Get the current spatial position of a `SPATIAL` object.
  !! @return Current spatial coordinates as a `SPATIAL` object.
  !! @note  This function returns **SPATIAL** type object (3D coordinates).
  !! @note  This is a standard extensible version.
  elemental function spatial_get_current_pos_3d_o(this) result(coordinates)
    class(SPATIAL), intent(in) :: this
    ! @return Current spatial coordinates as a `SPATIAL` object.
    type(SPATIAL) :: coordinates

    coordinates%x = this%x
    coordinates%y = this%y
    coordinates%depth = this%depth

  end function spatial_get_current_pos_3d_o

  !-----------------------------------------------------------------------------
  !> Get the current spatial position of a `SPATIAL` object.
  !! @param vector flag indicating if we return a 3D vector rather than
  !!        `SPATIAL` type object. **Note:**  We need this logical
  !!        parameter to avoid ambiguity in calling the generic
  !!        function: `Error: 'spatial_get_current_pos_3d' and
  !!        'spatial_get_current_pos_3d_v' for GENERIC 'now' at (1) are
  !!        ambiguous`. The parameter itself is **not used**.
  !!        So, for vector-output must always be TRUE.
  !! @return Current spatial coordinates as a 3-dimensional array.
  !! @note    This function returns **array** of 3D coordinates.
  !! @warning This function is **non extensible**.
  !! @note    The *vector form* of the `location` function is particularly
  !!          convenient for data output into the `LOGGER` module that does
  !!          not accept object data, but does accept simple array data, e.g.:
  !!          @code
  !!            call LOG_DBG ("location=" //                                  &
  !!                     TOSTR(proto_parents%individual(ind)%location(.TRUE.)))
  !!          @endcode
  pure function spatial_get_current_pos_3d_v(this, vector) result(coordinates)
    class(SPATIAL), intent(in) :: this
    ! @param vector flag indicating if we return a 3D vector rather than
    !!        `SPATIAL` type object.
    !! @note  We need this logical parameter to avoid ambiguity in calling
    !!        the generic function: `Error: 'spatial_get_current_pos_3d' and
    !!        'spatial_get_current_pos_3d_v' for GENERIC 'now' at (1) are
    !!        ambiguous`. The parameter itself is **not used**.
    !!        So, for vector-output **must always be TRUE**!
    logical, intent(in) :: vector     ! Not used in calculations
    ! @return Current spatial coordinates as a 3-dimensional array.
    real(SRP), dimension(DIMENSIONALITY_DEFAULT) :: coordinates

    coordinates(1) = this%x
    coordinates(2) = this%y
    coordinates(3) = this%depth

  end function spatial_get_current_pos_3d_v

  !-----------------------------------------------------------------------------
  !> Get the current `X` position of a `SPATIAL` object.
  !! @returns x_pos current X coordinate of the `SPATIAL` object.
  !! @note Not sure if really much needed.
  elemental function spatial_get_current_pos_x_3d(this) result (x_pos)
    class(SPATIAL), intent(in) :: this
    ! @returns x_pos current X coordinate of the `SPATIAL` object
    real(SRP) :: x_pos

    x_pos = this%x

  end function spatial_get_current_pos_x_3d

  !-----------------------------------------------------------------------------
  !> Get the current `Y` position of a `SPATIAL` object.
  !! @returns x_pos current X coordinate of the `SPATIAL` object.
  !! @note Not sure if really much needed.
  elemental function spatial_get_current_pos_y_3d(this) result (y_pos)
    class(SPATIAL), intent(in) :: this
    ! @returns x_pos current X coordinate of the `SPATIAL` object
    real(SRP) :: y_pos

    y_pos = this%y

  end function spatial_get_current_pos_y_3d

  !-----------------------------------------------------------------------------
  !> Get the current `DEPTH` position of a `SPATIAL` object.
  !! @returns x_pos current X coordinate of the `SPATIAL` object.
  !! @note Not sure if really much needed.
  elemental function spatial_get_current_pos_d_3d(this) result (depth_pos)
    class(SPATIAL), intent(in) :: this
    ! @returns x_pos current X coordinate of the `SPATIAL` object.
    real(SRP) :: depth_pos

    depth_pos = this%depth

  end function spatial_get_current_pos_d_3d

  !-----------------------------------------------------------------------------
  !> Calculate the illumination (background irradiance) at the depth of the
  !! spatial object at an arbitrary time step of the model.
  !! @warning Cannot implement a generic function accepting also vectors of
  !!          this objects as only elemental object-bound array functions are
  !!          allowed by the standard. This function cannot be elemental, so
  !!          passed-object dummy argument must always be scalar.
  function spatial_calc_irradiance_at_depth(this, time_step_model)            &
                                                    result (irradiance_at_depth)
    class(SPATIAL), intent(in) :: this
    integer, optional, intent(in) :: time_step_model
    real(SRP) :: irradiance_at_depth

    ! Local copies of optionals
    integer :: time_step_model_here

    !> ### Implementation details ###
    !> Check optional time step parameter. If unset, use global
    !! `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> Calculate ambient illumination / irradiance at the depth of
    !! this food item at the given time step.
    irradiance_at_depth =                                                     &
            light_depth ( depth=this%dpos(),                                  &
                          surface_light = light_surface(                      &
                                            tstep=time_step_model_here,       &
                                            is_stochastic=DAYLIGHT_STOCHASTIC) )

  end function spatial_calc_irradiance_at_depth

  !-----------------------------------------------------------------------------
  !> Calculate the visibility range of a spatial object. Wrapper to the
  !! the_environment::visual_range() function. This function calculates
  !! the distance from which this object can be seen by a visual object
  !! (e.g. predator or prey).
  !! @warning Cannot implement a generic function accepting also vectors of
  !!          this objects as only elemental object-bound array functions are
  !!          allowed by the standard. This function cannot be elemental, so
  !!          passed-object dummy argument must always be scalar.
  function spatial_visibility_visual_range_cm(this, object_area, contrast,    &
                                            time_step_model) result (visrange)
    class(SPATIAL), intent(in) :: this
    !> @param[in] object_area is the **mandatory** area of the spatial object
    !!       (m).
    !! @note object_area has optional attribute here with the base
    !!       the_environemnt::spatial class object can be only optional because
    !!       it is optional in all extension classes
    !!       (the_environment::food_item, the_environment::predator,
    !!       the_body::condition,the_neurobio::spatialobj_percept_comp).
    !!       However, it is actually **mandatory** here and not providing
    !!       object size results in wrong calculations. Such a case is logged
    !!       with the commondata::ltag_error logger tag.
    real(SRP), optional, intent(in) :: object_area
    !> @param[in] contrast is optional inherent contrast of this spatial
    !!            object. the default contrast of all objects is defined
    !!            by the commondata::preycontrast_default parameter.
    real(SRP), optional, intent(in) :: contrast
    !> @param[in] optional time step of the model, if absent gets the current
    !!            time step as defined by the value of
    !!            `commondata::global_time_step_model_current`.
    integer, optional, intent(in) :: time_step_model
    !> @return The maximum distance (m) from which this object can be seen.
    real(SRP) :: visrange

    ! Local copies of optionals
    integer :: time_step_model_here
    real(SRP) :: object_area_here, contrast_here

    ! Local variables
    real(SRP) :: irradiance_agent_depth

    !> PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME =                                 &
                                        "(spatial_visibility_visual_range_m)"

    !> ### Implementation details ###
    !> Check if optional object area parameter is present. For a base
    !! the_environment::spatial object, providing explicit value is
    !! **mandatory**. If object area is absent, commondata::missing value
    !! is used as a default, which results in **wrong calculations**.
    if (present(object_area)) then
      object_area_here = object_area
    else
      object_area_here = MISSING
      !> - Such a case is logged with the commondata::ltag_error logger tag.
      !!   (check logger errors with `grep ERROR: *log`).
      !! .
      call LOG_MSG(LTAG_ERROR // "Object area ('object_area') parameter is" //&
                   " not provided for a base SPATIAL class object in" //      &
                   PROCNAME // ": MISSING value is used for area.")
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
    !! this object at the given time step.
    irradiance_agent_depth =  this%illumination(time_step_model_here)

    !> Return visual range to see this spatial object: its visibility range.
    visrange = m2cm( visual_range( irradiance = irradiance_agent_depth,       &
                              prey_area = object_area_here,                   &
                              prey_contrast = contrast_here ) )

  end function spatial_visibility_visual_range_cm

  !-----------------------------------------------------------------------------
  !> Identify in which environment from the input list this spatial agent is
  !! currently in.
  !! Example call:
  !! @code
  !!   ienv = object%find_environment( [habitat_safe, habitat_dangerous] )
  !! @endcode
  !! @note Because the habitat object is an extension of the environment, this
  !!       method also works with the habitats.
  !!
  !> Determining the environment object the agent is currently in
  !! can be done by the_environment::spatial::find_environment()
  !! method in this way:
  !! @verbatim
  !!   ...
  !!   environment_limits = Global_Habitats_Available(                        &
  !!                  this_agent%find_environment(Global_Habitats_Available) )
  !!   ...
  !! @endverbatim
  !! This uses the the_environment::global_habitats_available global array
  !! containing all available environments, initialised in
  !! the_evolution::init_environment_objects().
  pure function spatial_get_environment_in_pos(this, environments_array)      &
                                                  result (environ_list_number)
    class(SPATIAL), intent(in) :: this
    !> @param[in] environments_array An array of environment objects, where the
    !!            `this` object can be. If this parameter is omitted, the
    !!            environment objects are obtained from the default global
    !!            array the_environment::global_habitats_available.
    !!            @warning The environment objects within the array must be
    !!                     non-overlapping, otherwise, the results are
    !!                     undefined due to parallel algorithm.
    class(ENVIRONMENT), optional, dimension(:), intent(in) :: environments_array
    !> @return Returns the number of the environment from the input array,
    !!         where this spatial object is currently in.
    integer :: environ_list_number

    ! Local number of the environments in the input array
    integer :: number_environments

    ! Local counters
    integer :: i

    ARRAY_PROVIDED: if (present(environments_array)) then

      !> ### Implementation details ###
      !> First, determine the size of the input array of the environments among
      !! which the check is done.
      number_environments = size(environments_array)

      !> Also, initialise the return value to zero.
      environ_list_number = 0

      !> Then cycle over all the input environments whether the this spatial
      !! object is within it.
      do concurrent (i=1:number_environments)
        if (this .within. environments_array(i) ) then
          !> The `.within.` operator is used for checking (defined by
          !! `the_environment::environment_check_located_within_3d()`).
          !! Then, return the number of the environment within the input array
          !! and exit.
          environ_list_number = i
          !  exit ! do concurrent does not accept premature exit.
        end if
      end do

    else ARRAY_PROVIDED

      !> If the `environments_array` array is not provided, default habitats
      !! are obtained from the the_environment::global_habitats_available
      !! global array .
      !  @note The code is repeated from the `environments_array` "provided"
      !        part above to work around the requirement to conversion between
      !        objects the_environment::environment and
      !        the_environment::habitat because the input `environments_array`
      !        parameter is `class`.
      number_environments = size(Global_Habitats_Available)

      ! Also, initialise the return value to zero.
      environ_list_number = 0

      ! Then cycle over all the input environments whether the this spatial
      ! object is within it.
      do concurrent (i=1:number_environments)
        if (this .within. Global_Habitats_Available(i) ) then
          !  The `.within.` operator is used for checking (defined by
          !  `the_environment::environment_check_located_within_3d()`).
          !  Then, return the number of the environment within the input array
          !  and exit.
          environ_list_number = i
          !  exit ! do concurrent does not accept premature exit.
        end if
      end do

    end if ARRAY_PROVIDED

  end function spatial_get_environment_in_pos

  !-----------------------------------------------------------------------------
  !> Place spatial movable object into a 3D space, define the object's current
  !! coordinates, but first save previous coordinates.
  !! @param x The spatial objects will now get these coordinates.
  !! @param y The spatial objects will now get these coordinates.
  !! @param depth The spatial objects will now get these coordinates.
  subroutine spatial_moving_fix_position_3d_v(this, x, y, depth)
    class(SPATIAL_MOVING), intent(inout) :: this

    ! The spatial objects will now get these coordinates
    real(SRP), intent(in) :: x,y,depth

    !> ### Implementation details ###
    !> Save previous coordinates into the history stacks.
    call add_to_history(this%history%x, this%x)
    call add_to_history(this%history%y, this%y)
    call add_to_history(this%history%depth, this%depth)

    !> Finally, position the object now with the coordinates provided.
    this%x = x
    this%y = y
    this%depth = depth

  end subroutine spatial_moving_fix_position_3d_v

  !-----------------------------------------------------------------------------
  !> Place spatial movable object into a 3D space, define the object's current
  !! coordinates, but first save previous coordinates.
  !! @param location The spatial objects will now get these coordinates.
  elemental subroutine spatial_moving_fix_position_3d_o(this, location)
    class(SPATIAL_MOVING), intent(inout) :: this

    ! The spatial objects will now get these coordinates
    type(SPATIAL), intent(in) :: location

    !> ### Implementation details ###
    !> Save previous coordinates into the history stacks.
    call add_to_history(this%history%x, this%x)
    call add_to_history(this%history%y, this%y)
    call add_to_history(this%history%depth, this%depth)

    !> Finally, position the object now with the coordinates provided.
    this%x = location%x
    this%y = location%y
    this%depth = location%depth

  end subroutine spatial_moving_fix_position_3d_o

  !-----------------------------------------------------------------------------
  !> Repeat (re-save) the current position into the positional history stack.
  !! @note Re-saving the current position is necessary to keep the
  !!       full positional history even for the the_behavior::behaviour's that
  !!       do not involve spatial displacement (movement).
  elemental subroutine spatial_moving_repeat_position_history_3d(this)
    class(SPATIAL_MOVING), intent(inout) :: this

    ! Re-save current coordinates into the history stacks.
    call add_to_history( this%history%x,     this%x     )
    call add_to_history( this%history%y,     this%y     )
    call add_to_history( this%history%depth, this%depth )

  end subroutine spatial_moving_repeat_position_history_3d

  !-----------------------------------------------------------------------------
  !> Calculate the Euclidean distance between two spatial objects. This is a
  !! type-bound function.
  !! @returns distance Euclidean distance between two spatial objects.
  !! @param other another spatial object that we measure distance between.
  !! @note Note that this version uses the vector-based backend
  !!       for calculation. The vector-based backend is equivalent to the
  !!       scalar-based, but might be more general as it easily works with
  !!       other dimensionality (e.g. 2D or 4D). The negative side is that
  !!       the vector-based backend cannot be used to make an
  !!       **elemental** function.
  !! **Example:** `x_dist = object%distance(other_object)`
  elemental function spatial_distance_3d (this, other)                        &
                                                  result (distance_euclidean)
    class(SPATIAL), intent(in) :: this
    ! @returns distance Euclidean distance between two spatial objects.
    real(SRP) :: distance_euclidean
    ! @param other another spatial object that we measure distance between.
    class(SPATIAL), intent(in) :: other

    !> ### Implementation details ###
    !> Calculate the distance between these two spatial objects.
    !! @note Note that this version uses the vector-based backend
    !!       for calculation. The vector-based backend is equivalent to the
    !!       scalar-based, but might be more general as it easily works with
    !!       other dimensionality (e.g. 2D or 4D). The negative side is that
    !!       the vector-based backend cannot be used to make an
    !!       **elemental** function.
    distance_euclidean = dist( [this%x,  this%y,  this%depth],                &
                               [other%x, other%y, other%depth] )

  end function spatial_distance_3d

   !-----------------------------------------------------------------------------
  !> Concatenate two arrays of the_environment::spatial objects `a` and `b`.
  !! This procedure uses array slices which would be faster in most cases than
  !! the intrinsic `[a,b]` method.
  !! @note There is a user defined operator `.cat.` making use of this
  !!       procedure that can be used like this:
  !!       @code
  !!         object1%location() .cat. object2%location()
  !!       @endcode
  !> @warning This is the the_environment::spatial **type** version. All input
  !!          and output parameters are defined as **type**, so this is not
  !!          class-safe.
  pure function spatial_stack2arrays(a, b) result (c)
    !> @param[in] a first array
    type(SPATIAL), intent(in), dimension(:) :: a
    !> @param[in] b second array
    type(SPATIAL), intent(in), dimension(:) :: b
    !> @return an array [a, b]
    type(SPATIAL), dimension(:), allocatable :: c

    allocate(c(size(a)+size(b)))
    c(1:size(a)) = a
    c(size(a)+1:size(a)+size(b)) = b
    ! The easier method using intrinsic array joining [a, b] is probably slower.

  end function spatial_stack2arrays

  !-----------------------------------------------------------------------------
  !> Concatenate two arrays of the_environment::spatial_moving objects `a` and
  !! `b`. This procedure uses array slices which would be faster in most cases
  !! than the intrinsic `[a,b]` method.
  !! @note There is a user defined operator `.cat.` making use of this
  !!       procedure that can be used like this:
  !!       @code
  !!         object1%location() .cat. object2%location()
  !!       @endcode
  !> @warning This is the the_environment::spatial_moving **type** version.
  !!          All input and output parameters are defined as **type**, so this
  !!          is not class-safe.
  pure function spatial_moving_stack2arrays(a, b) result (c)
    !> @param[in] a first array
    type(SPATIAL_MOVING), intent(in), dimension(:) :: a
    !> @param[in] b second array
    type(SPATIAL_MOVING), intent(in), dimension(:) :: b
    !> @return an array [a, b]
    type(SPATIAL_MOVING), dimension(:), allocatable :: c

    allocate(c(size(a)+size(b)))
    c(1:size(a)) = a
    c(size(a)+1:size(a)+size(b)) = b
    ! The easier method using intrinsic array joining [a, b] is probably slower.

  end function spatial_moving_stack2arrays

  !-----------------------------------------------------------------------------
  !> Concatenate the **location** components of two arrays of
  !! the_environment::spatial class objects a and b. This procedure uses
  !! array slices which would be faster in most cases than the intrinsic
  !! `[a,b]` method.
  !! @note There is a user defined operator `.cat.` making use of this
  !!       procedure that can be used like this:
  !!       @code
  !!         all_objects%position%( object1 .catloc. object2 )
  !!       @endcode
  !> @warning Unlike the the_environment::spatial_stack2arrays() and
  !!          the_environment::spatial_moving_stack2arrays() methods, this
  !!          procedure is class-safe and can be used with any class upwards,
  !!          but it concatenates **only** the location data (returns **type**
  !!          the_environment::spatial).
  pure function spatial_class_stack2arrays_locs(a, b)  result (c)
    !> @param[in] a first array
    class(SPATIAL), intent(in), dimension(:) :: a
    !> @param[in] b second array
    class(SPATIAL), intent(in), dimension(:) :: b
    !> @return an array [a, b]
    type(SPATIAL), dimension(:), allocatable :: c

    allocate(c(size(a)+size(b)))
    call c(1:size(a))%position( a%location() )
    call c(size(a)+1:size(a)+size(b))%position( b%location() )

  end function spatial_class_stack2arrays_locs

  !-----------------------------------------------------------------------------
  !> This is a **non-type-bound** version of the distance calculation function.
  !! @note    Note that this is an **elemental** function that can also accept
  !!          arrays (but these must be conforming).
  !! Example:
  !! @code
  !!   x_dist = dist3d(object, other_object)    # scalar variant
  !! @endcode
  !! @code
  !!   D=dist3d( habitat_safe%food%food(1:3),                                 &
  !!             habitat_safe%food%food(4:6) )  # vector variant
  !! @endcode
  elemental function dist3d (this, other) result (distance_euclidean)
    class(SPATIAL), intent(in) :: this
    !> @returns distance Euclidean distance between two spatial objects.
    real(SRP) :: distance_euclidean
    !> @param other another spatial object that we measure distance between.
    class(SPATIAL), intent(in) :: other

    !> ### Implementation details ###
    !> Calculate the distance between these two spatial objects.
    !! @note Note that this version uses the scalar-based backend
    !!       for calculation. The scalar-based backend is equivalent to the
    !!       vector-based one, but is linked with the 3D space only (has to be
    !!       re-implemented in case of 2D or 4D space). But its positive side
    !!       is that it is an **elemental** function that can be used to make
    !!       further elemental functions.
    distance_euclidean = dist(this%x,     other%x,                            &
                              this%y,     other%y,                            &
                              this%depth, other%depth)

  end function dist3d

  !-----------------------------------------------------------------------------
  !> Calculate the Euclidean distance between the current and previous
  !! position of a single spatial object.
  !! @param from_history We have to calculate total distance from this point
  !!        in the spatial stack history (< HISTORY_SIZE_SPATIAL).
  !!        Not used here, always 0.0.
  !! @returns distance Euclidean distance between two spatial objects.
  elemental function spatial_self_distance_3d (this, from_history)            &
                                                result (distance_euclidean)
    class(SPATIAL), intent(in) :: this ! NOT used here, always results in 0.0
    ! @param from_history We have to calculate total distance from this point
    ! in the spatial stack history (< HISTORY_SIZE_SPATIAL)
    integer, optional, intent(in) :: from_history ! NOT used here, always 0.0.
    ! @returns distance Euclidean distance between two spatial objects
    real(SRP) :: distance_euclidean

    !> ### Implementation details ###
    !> The distance between two positions of an immobile object is zero
    !! in all cases. It is a fixed value in this procedure.
    distance_euclidean=0.0_SRP

  end function spatial_self_distance_3d

  !-----------------------------------------------------------------------------
  !> Calculate the Euclidean distance between the current and previous
  !! position of a single spatial movable object. Optionally, it also
  !! calculates the total distance traversed during the `from_history` points
  !! from the history stack along with the distance from the current position
  !! and the last historical value. For example, to calculate the **average**
  !! distance throughout the whole history (`HISTORY_SIZE_SPATIAL`
  !! points) do this:
  !! @code
  !!     object_name%way(HISTORY_SIZE_SPATIAL - 1) / HISTORY_SIZE_SPATIAL
  !! @endcode
  !! This is because for `N` points in history we can calculate `N-1`
  !! distances, but the sample size is `N`, that is `N-1` plus an additional
  !! distance between the latest historical point and the current position.
  !! @param from_history We have to calculate total distance from this point
  !!        in the spatial stack history (< `HISTORY_SIZE_SPATIAL`)
  !! @returns distance Euclidean distance between two spatial objects.
  elemental function spatial_moving_self_distance_3d (this, from_history)     &
                                              result (distance_euclidean)
    class(SPATIAL_MOVING), intent(in) :: this
    ! @param from_history We have to calculate total distance from this point
    ! in the spatial stack history (< `HISTORY_SIZE_SPATIAL`)
    integer, optional, intent(in) :: from_history
    ! @returns distance Euclidean distance between two spatial objects
    real(SRP) :: distance_euclidean

    ! Local copy of optional
    integer :: from_history_here

    ! Local variable to keep the size of the history array
    integer :: history_size

    ! Local counter
    integer :: i

    !> ### Implementation details ###
    !> We get the history size  from the history stack size (`history`),
    !! alternatively, can get from the `HISTORY_SIZE_SPATIAL` parameter
    !! directly.
    history_size = size(this%history)

    !> Check if we are asked to calculate distance traversed using the history
    !! stack. If the number provided exceed the limit, set maximum. If no
    !! history requested (parameter not present or 0) calculate the distance
    !! between the current point and the latest historical point.
    !! @note If we have history stack of size **N**, we can calculate maximum
    !!       **N-1** historical distances between **N** successive points.
    if (present(from_history)) then
      if (from_history <= HISTORY_SIZE_SPATIAL-1) then
        from_history_here = from_history
      else
        from_history_here = HISTORY_SIZE_SPATIAL-1
      end if
    else
      from_history_here = 0
    end if

    !> Calculate the distance between the current position and
    !> the last historical stack record.
    distance_euclidean=dist( this%x,     this%history(history_size)%x,        &
                             this%y,     this%history(history_size)%y,        &
                             this%depth, this%history(history_size)%depth )

    !> Now cycle backwards through the `from_history` steps of the history
    !! stack and calculate the sum = total distance traversed.
    ! @warning The `do concurrent` construct is F2008 and can not (yet) be
    !          implemented in all compilers. Use normal `do` in such a case.
    ! @warning Note that this `do concurrent` uses negative step -1.
    !          TODO: Parallel processing needs testing for correctness and
    !          speed benefit! So far **disabled** `do concurrent`.
    !do concurrent ( i = history_size:history_size-from_history_here+1:-1 )
    do i = history_size, history_size-from_history_here+1, -1
      distance_euclidean = distance_euclidean +                               &
                      dist( this%history(i)%x,     this%history(i-1)%x,       &
                            this%history(i)%y,     this%history(i-1)%y,       &
                            this%history(i)%depth, this%history(i-1)%depth )
    end do

  end function spatial_moving_self_distance_3d

  !-----------------------------------------------------------------------------
  !> Create a new spatial moving object. Initially it has no position, all
  !! coordinate values are `MISSING` or `INVALID` for real type coordinates.
  !! @note This is the **constructor** for `SPATIAL_MOVING` object type that
  !!       makes it into existence and gives it its value.
  elemental subroutine spatial_moving_create_3d(this)
    class(SPATIAL_MOVING), intent(inout) :: this

    !> ### Implementation details ###
    !> Assign `MISSING` values to the new object, use interface function
    !! and type constructor.
    !call this%position( SPATIAL(MISSING,MISSING,MISSING) )
    call this%missing()

    !> This also cleanups the history stack, i.e. fills it with `MISSING`
    !! values.
    call this%spatial_history_clean()

  end subroutine spatial_moving_create_3d

  !-----------------------------------------------------------------------------
  !> Create a new empty history of positions for spatial moving object.
  !! Assign all values to the MISSING value code.
  elemental subroutine spatial_moving_clean_hstory_3d(this)
    class(SPATIAL_MOVING), intent(inout) :: this

    !> ### Implementation details ###
    !> Set all historical stack **arrays** to `MISSING`.
    this%history%x     = MISSING
    this%history%y     = MISSING
    this%history%depth = MISSING

  end subroutine spatial_moving_clean_hstory_3d

  !-----------------------------------------------------------------------------
  !> The spatial moving object **ascends**, goes up the depth with specific
  !! fixed step size.
  elemental subroutine spatial_moving_go_up(this, step)
    class(SPATIAL_MOVING), intent(inout) :: this
    !> @param[in] step is the step size for the upwards walk. If it is too
    !!            large (the object would extend beyond its current
    !!            environment), it is adjusted autiomatically.
    real(SRP), optional, intent(in) :: step

    ! Local minimum depth
    real(SRP) :: min_depth

    ! Local copy of the step
    real(SRP) :: step_here

    step_here = step

    !> ### Implementation details ###
    !> Calculate the minimum depth in the environment currently
    !! occupied by the spatial object.
    !! - Use a combination of the_environment::spatial::find_environment()
    !!   and the_environment::environment::depth_min().
    !! .
    min_depth = Global_Habitats_Available(                                    &
                            this%find_environment(                            &
                                              Global_Habitats_Available)      &
                            )%depth_min()

    !> Check if the target depth is likely to go beyond the environment
    !! depth limits and reduce the upwnward walk step size
    !! accordingly. Namely, if the depth coordinate of the object
    !! minus the depth step exceeds the minimum depth, the step is reduced
    !! to be within the available environment:
    !! @f$ d_{o} - D_{min} - \varepsilon @f$, where @f$ D_{min} @f$ is the
    !! maximum depth, @f$ d_{o} @f$ is the current depth of the object and
    !! @f$ \varepsilon @f$ is a very small constant defined by the parameter
    !! commondata::zero to guarantee the object remains within the current
    !! environment.
    if (this%dpos() - step_here <= min_depth )                                &
                step_here = max( 0.0_SRP, this%dpos() - min_depth - ZERO )


    !> Relocate the object so that its X and Y coordinates remain intact
    !! but the depth reduces by the step size.
    !! - Here, if the object is a the_environment::food_item class object,
    !!   also check if it is the_environment::food_item::is_available() and
    !!   move the object only if yes.
    !! .
    select type(this)
      class is (FOOD_ITEM)
        if (this%is_available())                                              &
            call this%position( SPATIAL( x = this%x,                          &
                                         y = this%y,                          &
                                         depth = this%depth - step_here ) )
      class default
        call this%position( SPATIAL( x = this%x,                              &
                                     y = this%y,                              &
                                     depth = this%depth - step_here ) )
    end select

  end subroutine spatial_moving_go_up

  !-----------------------------------------------------------------------------
  !> The spatial moving object **decends**, goes down the depth with specific
  !! fixed step size.
  elemental subroutine spatial_moving_go_down(this, step)
    class(SPATIAL_MOVING), intent(inout) :: this
    !> @param[in] step is the step size for the upwards walk. If it is too
    !!            large (the object would extend beyond its current
    !!            environment), it is adjusted autiomatically.
    real(SRP), optional, intent(in) :: step

    ! Local minimum depth
    real(SRP) :: max_depth

    ! Local copy of the step
    real(SRP) :: step_here

    step_here = step

    !> ### Implementation details ###
    !> Calculate the maximum depth in the environment currently
    !! occupied by the spatial object.
    !! - Use a combination of the_environment::spatial::find_environment()
    !!   and the_environment::environment::depth_max().
    !! .
    max_depth = Global_Habitats_Available(                                    &
                            this%find_environment(                            &
                                              Global_Habitats_Available)      &
                            )%depth_max()

    !> Check if the target depth is likely to go beyond the environment
    !! depth limits and reduce the downward walk step size
    !! accordingly. Namely, if the depth coordinate of the object
    !! plus the depth step exceeds the maximum depth, the step is reduced
    !! to be within the available environment:
    !! @f$ D_{max} - d_{o} - \varepsilon @f$, where @f$ D_{max} @f$ is the
    !! maximum depth, @f$ d_{o} @f$ is the current depth of the object and
    !! @f$ \varepsilon @f$ is a very small constant defined by the parameter
    !! commondata::zero to guarantee the object remains within the current
    !! environment.
    if (this%dpos() + step_here >= max_depth )                                &
                step_here = max( 0.0_SRP, max_depth - this%dpos() - ZERO )

    !> Relocate the object so that its X and Y coordinates remain intact
    !! but the depth reduces by the step size.
    !! - Here, if the object is a the_environment::food_item class object,
    !!   also check if it is the_environment::food_item::is_available() and
    !!   move the object only if yes.
    !! .
    select type(this)
      class is (FOOD_ITEM)
        if (this%is_available())                                              &
            call this%position( SPATIAL( x = this%x,                          &
                                         y = this%y,                          &
                                         depth = this%depth + step_here ) )
      class default
        call this%position( SPATIAL( x = this%x,                              &
                                     y = this%y,                              &
                                     depth = this%depth + step_here ) )
    end select

  end subroutine spatial_moving_go_down

  !-----------------------------------------------------------------------------
  !> @brief   Implements an optionally environment-restricted Gaussian random
  !!          walk in 3D.
  !! @param   meanshift the mean shift along any of the three dimensions.
  !! @param   cv_shift the coefficient of variation for a single elementary
  !!          shift in any of the three dimensions.
  !! @param   environment_limits Limits of the environment area available for
  !!          the random walk. The moving object cannot get beyond this limit.
  !! @details The moving object walks in three dimensions. The process is
  !!          simple, first shift along the x axis for some random Gaussian
  !!          length (with the mean `meanshift` and the variance/CV `cv_shift`)
  !!          then  walk another Gaussian length the y axis. The, walk in the
  !!          same manner along the z axis. The optional restriction is that
  !!          the whole walk must not exceed specific spatial location set by
  !!          the environment parameter.
  subroutine spatial_moving_randomwalk_gaussian_step_3d(this, meanshift,      &
                                                  cv_shift, environment_limits)
    class(SPATIAL_MOVING), intent(inout) :: this

    ! @param meanshift the mean shift along any of the three dimensions.
    real(SRP), intent(in) :: meanshift
    ! @param cv_shift the coefficient of variation for a single elementary
    !        shift in any of the three dimensions.
    real(SRP), intent(in) :: cv_shift
    ! @param environment_limits Limits of the environment area available for
    !        the random walk. The moving object cannot get beyond this limit.
    class(ENVIRONMENT), intent(in), optional :: environment_limits

    ! Local test object that we test, should lay within the
    ! `environment_limits` environmental object.
    type(SPATIAL) :: current_pos, test_object

    !...........................................................................

    !> ### Implementation details ###
    !> First, we get the current coordinates of the spatial object.
    current_pos = this%now()

    !> And set a temporary spatial test object with the new coordinates,
    !! advancing/adding our random walk step. This is done via the
    !! `.radd.` operator and calling RNORM (see HEDTOOLS).
    call test_object%position( SPATIAL(                                       &
                    x=current_pos%x .radd.                                    &
                        RNORM( meanshift, cv2variance(cv_shift, meanshift) ), &
                    y=current_pos%y .radd.                                    &
                        RNORM( meanshift, cv2variance(cv_shift, meanshift) ), &
                    depth=current_pos%depth .radd.                            &
                        RNORM( meanshift, cv2variance(cv_shift, meanshift) )  ))

    !> Environment restriction part of the random walk, if `environment_limits`
    !! parameter object is provided. Here the object is not allowed to go beyond
    !! its bounding environment: get new position while outside of the target
    !! environment.
    ENVIRON_RESTRICT: if (present(environment_limits)) then
      !> - Loop while this new test spatial object is outside
      !!   of our target environment. It must be **strictly** within.
      !! .
      do while (.NOT. test_object%is_within(environment_limits))
        ! (If the `test_object` is outside the environment, we create new
        ! randomly updated coordinates.)
        call test_object%position( SPATIAL(                                   &
                    x=current_pos%x .radd.                                    &
                        RNORM( meanshift, cv2variance(cv_shift, meanshift) ), &
                    y=current_pos%y .radd.                                    &
                        RNORM( meanshift, cv2variance(cv_shift, meanshift) ), &
                    depth=current_pos%depth .radd.                            &
                        RNORM( meanshift, cv2variance(cv_shift, meanshift) )  ))
      end do
    end if ENVIRON_RESTRICT

    !> Finally, change the current position of the `this` object to the
    !! position defined by the `test_object`. The standard function `position`
    !! for the `SPATIAL_MOVING`is used, that keeps the movement history.
    call this%position(test_object)

  end subroutine spatial_moving_randomwalk_gaussian_step_3d

  !-----------------------------------------------------------------------------
  !> @brief   Implements an optionally environment-restricted Gaussian random
  !!          walk in a "2.5 dimensions", i.e. 2D x y with separate walk
  !!          parameters for the third depth dimension.
  !! @param   meanshift the mean shift along any of the three dimensions.
  !! @param   cv_shift the coefficient of variation for a single elementary
  !!          shift in any of the three dimensions.
  !! @param   environment_limits Limits of the environment area available for
  !!          the random walk. The moving object cannot get beyond this limit.
  !! @details The moving object walks in three dimensions. The process is
  !!          simple, first shift along the x axis for some random Gaussian
  !!          length (with the mean `meanshift` and the variance/CV `cv_shift`)
  !!          then  walk another Gaussian length the y axis. The, walk in the
  !!          same manner along the z axis. But here z axis has separate walk
  !!          parameters (`meanshift` and `cv_shift`) that are much smaller
  !!          than the x and y parameters. The optional restriction is that
  !!          the whole walk must not exceed specific spatial location set by
  !!          the environment parameter.
  !! @note    The mean and CV is different for the 2D x y movement and the
  !!          third dimension *depth* movement.
  subroutine spatial_moving_randomwalk_gaussian_step_25d(this,                &
                                     meanshift_xy, cv_shift_xy,               &
                                     meanshift_depth, cv_shift_depth,         &
                                     environment_limits)
    class(SPATIAL_MOVING), intent(inout) :: this

    ! @param meanshift the mean shift along any of the three dimensions.
    real(SRP), intent(in) :: meanshift_xy, meanshift_depth
    ! @param cv_shift the coefficient of variation for a single elementary
    !        shift in any of the three dimensions.
    real(SRP), intent(in) :: cv_shift_xy, cv_shift_depth
    ! @param environment_limits Limits of the environment area available for
    !        the random walk. The moving object cannot get beyond this limit.
    class(ENVIRONMENT), intent(in), optional :: environment_limits

    ! Local test object that we test, should lay within the
    ! `environment_limits` environmental object.
    type(SPATIAL) :: current_pos, test_object

    !...........................................................................

    !> ### Implementation details ###
    !> First, we get the current coordinates of the spatial object.
    current_pos = this%now()

    !> And set a temporary spatial test object with the new coordinates,
    !! advancing/adding our random walk step. This is done via the
    !! `.radd.` operator and calling the `RNORM` function (see HEDTOOLS).
    call test_object%position( SPATIAL(                                       &
                x=current_pos%x .radd.                                        &
                    RNORM( meanshift_xy,                                      &
                           cv2variance(cv_shift_xy, meanshift_xy) ),          &
                y=current_pos%y .radd.                                        &
                    RNORM( meanshift_xy,                                      &
                           cv2variance(cv_shift_xy, meanshift_xy) ),          &
                depth=current_pos%depth .radd.                                &
                    RNORM( meanshift_depth,                                   &
                           cv2variance(cv_shift_depth, meanshift_depth) )   ) )

    !> Environment restriction part of the random walk, if `environment_limits`
    !! parameter object is provided. Here the object is not allowed to go beyond
    !! its bounding environment: get new position while outside of the target
    !! environment.
    ENVIRON_RESTRICT: if (present(environment_limits)) then
      !> - Loop while this new test spatial object is outside
      !!   of our target environment. It must be **strictly** within.
      !! .
      do while (.NOT. test_object%is_within(environment_limits))
        ! (if the `test_object` is outside the environment, we create new
        ! randomly updated coordinates.)
        call test_object%position( SPATIAL(                                   &
                x=current_pos%x .radd.                                        &
                    RNORM( meanshift_xy,                                      &
                           cv2variance(cv_shift_xy, meanshift_xy) ),          &
                y=current_pos%y .radd.                                        &
                    RNORM( meanshift_xy,                                      &
                           cv2variance(cv_shift_xy, meanshift_xy) ),          &
                depth=current_pos%depth .radd.                                &
                    RNORM( meanshift_depth,                                   &
                           cv2variance(cv_shift_depth, meanshift_depth) )   ) )
      end do
    end if ENVIRON_RESTRICT

    !> Finally, change the current position of the `this` object to the
    !! position defined by the `test_object`. The standard function `position`
    !! for the `SPATIAL_MOVING`is used, that keeps the movement history.
    call this%position(test_object)

  end subroutine spatial_moving_randomwalk_gaussian_step_25d

  !-----------------------------------------------------------------------------
  !> @brief     Implements an optionally environment-restricted **correlated
  !!            directional** Gaussian random walk in 3D towards (or away of)
  !!            an the_environment::spatial class `target` object.
  !! @details   The moving object walks in three dimensions towards (or away
  !!            of) a the_environment::spatial class target.
  !! Here is an example of walks:
  !! @image html  img_doxygen_corwalk_3d.svg "Correlated Gaussian random walk in 3D"
  !! @image latex img_doxygen_corwalk_3d.eps "Correlated Gaussian random walk in 3D" width=14cm
  subroutine spatial_moving_corwalk_gaussian_step_3d(this, target,            &
                                                           meanshift,         &
                                                           cv_shift,          &
                                                           is_away,           &
                                                           ci_lim,            &
                                                           environment_limits,&
                                                           is_converged,      &
                                                           debug_reps       )
    class(SPATIAL_MOVING), intent(inout) :: this
    !> @param[in] target The target of the random walk.
    class(SPATIAL), intent(in) :: target
    !> @param meanshift the mean shift along any of the three dimensions.
    real(SRP), intent(in) :: meanshift
    !> @param cv_shift the coefficient of variation for a single elementary
    !>        shift in any of the three dimensions.
    real(SRP), intent(in) :: cv_shift
    !> @param[in] is_away optional logical flag, if set to TRUE, the walk
    !!            is actually directed out of the target, so that the object
    !!            is going to maximise rather than minimise the distance to
    !!            the target.
    logical, optional, intent(in) :: is_away
    !> @param[in] ci_lim This parameter sets the convergence criterion; it is
    !!            the confidence interval limit for the distance between the
    !!            object and the target. The walk is considered "converged" if
    !!            the distance between the object and the target is smaller
    !!            than `ci_lim` std. deviations of the average step distance
    !!            (set by `meanshift`). The default value is the 95% confidence
    !!            interval (1.95996).
    !               | Conf. interval |  ci_lim  |
    !               | -------------: | :------: |
    !               |     25.0%      | 0.31860  |
    !               |     50.0%      | 0.67449  |
    !               |     75.0%      | 1.15035  |
    !               |     90.0%      | 1.64485  |
    !               |     95.0%      | 1.95996  |
    !               |     97.0%      | 2.17009  |
    !               |     99.0%      | 2.57583  |
    !               |     99.9%      | 3.29053  |
    real(SRP), optional, intent(in) :: ci_lim
    !> @param environment_limits optional limits of the environment area
    !!        available for the random walk. The moving object cannot get
    !!        beyond this limit.
    class(ENVIRONMENT), intent(in), optional :: environment_limits
    !> @param[out] is_converged optional logical flag for the "converged"
    !!             state, i.e. the distance between the object and the
    !!             target is smaller than `ci_lim` standard deviations  of
    !!             the average step distance.
    logical, optional, intent(out) :: is_converged
    !> @param[out] debug_reps optional internal counter for repeated samplings
    !!             of random positions for prospective spatial advancement
    !!             of the object. If the counter reaches a too high value,
    !!             a "no convergence" state is reached. In case of the avoiding
    !!             environmentally limited walk, when the object maximises its
    !!             distance from the target, such a condition may indicate that
    !!             there is no further space to avoid the target in the
    !!             available environment.
    integer, optional, intent(out) :: debug_reps

    ! Local test object that we test, should lay within the
    ! `environment_limits` environmental object.
    type(SPATIAL) :: current_pos, test_object

    ! Local copy of is_away, if TRUE the object is maximising the distance
    ! to the target, i.e. tries to avoid target.
    logical :: move_out

    ! Local copy of optional ci_lim
    real(SRP) :: perc_ci

    ! Default 95% confidence limit.
    real(SRP), parameter :: PERC_CI_DEF = 1.95996

    ! Maximum number of iterations, convergence limit.
    integer, parameter :: CONVERG = 100

    ! Counter for convergence repetitions.
    integer :: erep

    !...........................................................................

    ! Check optional parameter
    if (present(is_away)) then
      move_out = is_away
    else
      move_out = .FALSE. ! default is to approach target.
    end if
    if(present(ci_lim)) then
      perc_ci = ci_lim
    else
      perc_ci = PERC_CI_DEF
    end if

    erep = 0

    !> ### Implementation details ###
    !> First, check if the convergence criterion is reached. The walk is
    !! considered "converged" if the distance between the object and the
    !! target is smaller than `ci_lim` std. deviations of the average step
    !! distance (which is set by `meanshift`).
    if ( .not. move_out .and.                                                 &
         this%distance(target) < perc_ci * meanshift * cv_shift ) then
      if (present(is_converged)) then
        is_converged = .TRUE.
      end if
      if(present(debug_reps)) then
        debug_reps = erep
      end if
      !> If the convergence condition is met, the object *does not* change its
      !! position any more, no further walks are performed.
      return
    end if

    !> If the convergence condition is not yet met, the current coordinates of
    !! the spatial object are recorded.
    current_pos = this%now()

    !> Then a temporary spatial test object (`test_object`) is set the new
    !! coordinates for the spatial moving object, advancing to a random walk
    !! step. This is done by randomly adding or subtracting a Gaussian step
    !! size with the mean `meanshift` and variance defined by `cv_shift`
    !! along all three spatial coordinates.
    call test_object%position( SPATIAL(                                       &
                x=current_pos%x .radd.                                        &
                    RNORM( meanshift, cv2variance(cv_shift, meanshift) ),     &
                y=current_pos%y .radd.                                        &
                    RNORM( meanshift, cv2variance(cv_shift, meanshift) ),     &
                depth=current_pos%depth .radd.                                &
                    RNORM( meanshift, cv2variance(cv_shift, meanshift) )   ) )

    !> A series of conditions is then checked, the main is that the new
    !! position defined by the temporary spatial object should be such that
    !! the distance from the target must be smaller (if the object is intended
    !! to moved towards the target) or larger (if the object is intended to
    !! move away of the target).
    !!
    !! Environment restriction is also applied if `environment_limits`
    !! parameter is provided: the object is not allowed to go beyond
    !! its bounding environment in such a case.
    !!
    !! There is also a safeguard against poor convergence: If the number of
    !! iterations exceeds a fixed value (`CONVERG` local parameter), force to
    !! exit from the condition loops without changing the object position (i.e.
    !! no walk is done).
    AWAY: if (move_out) then
      ENVIRON_RESTRICT1: if (present(environment_limits)) then
        ! Loop while this new test spatial object is outside
        ! our target environment. It must be **strictly** within.
        do while ( test_object%distance(target) < this%distance(target) .or.  &
                   .NOT. test_object%is_within(environment_limits) )
          erep = erep + 1
          if (erep > CONVERG) exit
          ! if the `test_object` is outside the environment or not advancing
          ! to the correct direction, we create new randomly updated
          ! coordinates and iterate while this wrong condition still holds.
          call test_object%position( SPATIAL(                                 &
                  x=current_pos%x .radd.                                      &
                      RNORM( meanshift, cv2variance(cv_shift, meanshift) ),   &
                  y=current_pos%y .radd.                                      &
                      RNORM( meanshift, cv2variance(cv_shift, meanshift) ),   &
                  depth=current_pos%depth .radd.                              &
                      RNORM( meanshift, cv2variance(cv_shift, meanshift) )   ) )
        end do
      else ENVIRON_RESTRICT1
        do while ( test_object%distance(target) < this%distance(target) )
          erep = erep + 1
          if (erep > CONVERG) exit
          ! if the `test_object` is outside the environment or not advancing
          ! to the correct direction, we create new randomly updated
          ! coordinates and iterate while this wrong condition still holds.
          call test_object%position( SPATIAL(                                 &
                  x=current_pos%x .radd.                                      &
                      RNORM( meanshift, cv2variance(cv_shift, meanshift) ),   &
                  y=current_pos%y .radd.                                      &
                      RNORM( meanshift, cv2variance(cv_shift, meanshift) ),   &
                  depth=current_pos%depth .radd.                              &
                      RNORM( meanshift, cv2variance(cv_shift, meanshift) )   ) )
        end do
      end if ENVIRON_RESTRICT1
    else AWAY
      ENVIRON_RESTRICT2: if (present(environment_limits)) then
        ! Loop while this new test spatial object is outside
        ! our target environment. It must be **strictly** within.
        do while ( test_object%distance(target) > this%distance(target) .or. &
                   .NOT. test_object%is_within(environment_limits) )
          erep = erep + 1
          if (erep > CONVERG) exit
          ! if the `test_object` is outside the environment or not advancing
          ! to the correct direction, we create new randomly updated
          ! coordinates and iterate while this wrong condition still holds.
          call test_object%position( SPATIAL(                                 &
                  x=current_pos%x .radd.                                      &
                      RNORM( meanshift, cv2variance(cv_shift, meanshift) ),   &
                  y=current_pos%y .radd.                                      &
                      RNORM( meanshift, cv2variance(cv_shift, meanshift) ),   &
                  depth=current_pos%depth .radd.                              &
                      RNORM( meanshift, cv2variance(cv_shift, meanshift) )   ) )
        end do
      else ENVIRON_RESTRICT2
        do while ( test_object%distance(target) > this%distance(target)  )
          erep = erep + 1
          if (erep > CONVERG) exit
          ! if the `test_object` is outside the environment or not advancing
          ! to the correct direction, we create new randomly updated
          ! coordinates and iterate while this wrong condition still holds.
          call test_object%position( SPATIAL(                                 &
                  x=current_pos%x .radd.                                      &
                      RNORM( meanshift, cv2variance(cv_shift, meanshift) ),   &
                  y=current_pos%y .radd.                                      &
                      RNORM( meanshift, cv2variance(cv_shift, meanshift) ),   &
                  depth=current_pos%depth .radd.                              &
                      RNORM( meanshift, cv2variance(cv_shift, meanshift) )   ) )
        end do
      end if ENVIRON_RESTRICT2
    end if AWAY
    !> Finally, change the current position of the `this` object to the
    !! position defined by the `test_object`. Such a change is done only if
    !! the non-convergence condition is not detected.
    if (erep < CONVERG) call this%position(test_object)

    !> At the end, check and return the optional intent[out] parameters
    !! `is_converged` and `debug_reps`.
    if ( .not. move_out .and.                                                 &
         this%distance(target) < perc_ci * meanshift * cv_shift ) then
      if (present(is_converged)) then
        is_converged = .TRUE.
      end if
    end if
    ! return debug erep counter.
    if(present(debug_reps)) then
      debug_reps = erep
    end if

  end subroutine spatial_moving_corwalk_gaussian_step_3d

  !-----------------------------------------------------------------------------
  !> @brief     Implements an optionally environment-restricted **correlated
  !!            directional** Gaussian random walk in 3D towards (or away of)
  !!            an the_environment::spatial class `target` object.
  !! @details   The moving object walks in three dimensions towards (or away
  !!            of) a the_environment::spatial class target.
  !! @image html  img_doxygen_corwalk_3d.svg "Correlated Gaussian random walk in 3D"
  !! @image latex img_doxygen_corwalk_3d.eps "Correlated Gaussian random walk in 3D" width=14cm
  subroutine spatial_moving_corwalk_gaussian_step_25d(this, target,           &
                                                           meanshift_xy,      &
                                                           cv_shift_xy,       &
                                                           meanshift_depth,   &
                                                           cv_shift_depth,    &
                                                           is_away,           &
                                                           ci_lim,            &
                                                           environment_limits,&
                                                           is_converged,      &
                                                           debug_reps       )
    class(SPATIAL_MOVING), intent(inout) :: this
    !> @param[in] target The target of the random walk.
    class(SPATIAL), intent(in) :: target
    !> @param meanshift_xy the mean shift along any of the three dimensions.
    real(SRP), intent(in) :: meanshift_xy
    !> @param cv_shift_xy the coefficient of variation for a single elementary
    !>        shift in any of the three dimensions.
    real(SRP), intent(in) :: cv_shift_xy
    !> @param meanshift_depth the mean shift along any of the three dimensions.
    real(SRP), intent(in) :: meanshift_depth
    !> @param cv_shift_depth the coefficient of variation for a single
    !>        elementary shift in any of the three dimensions.
    real(SRP), intent(in) :: cv_shift_depth
    !> @param[in] is_away optional logical flag, if set to TRUE, the walk
    !!            is actually directed out of the target, so that the object
    !!            is going to maximise rather than minimise the distance to
    !!            the target.
    logical, optional, intent(in) :: is_away
    !> @param[in] ci_lim This parameter sets the convergence criterion; it is
    !!            the confidence interval limit for the distance between the
    !!            object and the target. The walk is considered "converged" if
    !!            the distance between the object and the target is smaller
    !!            than `ci_lim` std. deviations of the average step distance
    !!            (set by `meanshift`). The default value is the 95% confidence
    !!            interval (1.95996).
    !               | Conf. interval |  ci_lim  |
    !               | -------------: | :------: |
    !               |     25.0%      | 0.31860  |
    !               |     50.0%      | 0.67449  |
    !               |     75.0%      | 1.15035  |
    !               |     90.0%      | 1.64485  |
    !               |     95.0%      | 1.95996  |
    !               |     97.0%      | 2.17009  |
    !               |     99.0%      | 2.57583  |
    !               |     99.9%      | 3.29053  |
    real(SRP), optional, intent(in) :: ci_lim
    !> @param environment_limits optional limits of the environment area
    !!        available for the random walk. The moving object cannot get
    !!        beyond this limit.
    class(ENVIRONMENT), intent(in), optional :: environment_limits
    !> @param[out] is_converged optional logical flag for the "converged"
    !!             state, i.e. the distance between the object and the
    !!             target is smaller than `ci_lim` standard deviations  of
    !!             the average step distance.
    logical, optional, intent(out) :: is_converged
    !> @param[out] debug_reps optional internal counter for repeated samplings
    !!             of random positions for prospective spatial advancement
    !!             of the object. If the counter reaches a too high value,
    !!             a "no convergence" state is reached. In case of the avoiding
    !!             environmentally limited walk, when the object maximises its
    !!             distance from the target, such a condition may indicate that
    !!             there is no further space to avoid the target in the
    !!             available environment.
    integer, optional, intent(out) :: debug_reps

    ! Local test object that we test, should lay within the
    ! `environment_limits` environmental object.
    type(SPATIAL) :: current_pos, test_object

    ! Local copy of is_away, if TRUE the object is maximising the distance
    ! to the target, i.e. tries to avoid target.
    logical :: move_out

    ! Local copy of optional ci_lim
    real(SRP) :: perc_ci

    ! Default 95% confidence limit.
    real(SRP), parameter :: PERC_CI_DEF = 1.95996

    ! Maximum number of iterations, convergence limit.
    integer, parameter :: CONVERG = 100

    ! Counter for convergence repetitions.
    integer :: erep

    !...........................................................................

    ! Check optional parameter
    if (present(is_away)) then
      move_out = is_away
    else
      move_out = .FALSE. ! default is to approach target.
    end if
    if(present(ci_lim)) then
      perc_ci = ci_lim
    else
      perc_ci = PERC_CI_DEF
    end if

    erep = 0

    !> ### Implementation details ###
    !> First, check if the convergence criterion is reached. The walk is
    !! considered "converged" if the distance between the object and the
    !! target is smaller than `ci_lim` std. deviations of the average step
    !! distance (which is set by `meanshift`).
    if ( .not. move_out .and.                                                 &
         this%distance(target) < perc_ci * meanshift_xy * cv_shift_xy ) then
      if (present(is_converged)) then
        is_converged = .TRUE.
      end if
      if(present(debug_reps)) then
        debug_reps = erep
      end if
      !> If the convergence condition is met, the object *does not* change its
      !! position any more, no further walks are performed.
      return
    end if

    !> If the convergence condition is not yet met, the current coordinates of
    !! the spatial object are recorded.
    current_pos = this%now()

    !> Then a temporary spatial test object (`test_object`) is set the new
    !! coordinates for the spatial moving object, advancing to a random walk
    !! step. This is done by randomly adding or subtracting a Gaussian step
    !! size with the mean `meanshift` and variance defined by `cv_shift`
    !! along all three spatial coordinates.
    call test_object%position( SPATIAL(                                       &
                x=current_pos%x .radd.                                        &
                    RNORM( meanshift_xy,                                      &
                           cv2variance(cv_shift_xy, meanshift_xy) ),          &
                y=current_pos%y .radd.                                        &
                    RNORM( meanshift_xy,                                      &
                           cv2variance(cv_shift_xy, meanshift_xy) ),          &
                depth=current_pos%depth .radd.                                &
                    RNORM( meanshift_depth,                                   &
                           cv2variance(cv_shift_depth, meanshift_depth) )   ) )

    !> A series of conditions is then checked, the main is that the new
    !! position defined by the temporary spatial object should be such that
    !! the distance from the target must be smaller (if the object is intended
    !! to moved towards the target) or larger (if the object is intended to
    !! move away of the target).
    !!
    !! Environment restriction is also applied if `environment_limits`
    !! parameter is provided: the object is not allowed to go beyond
    !! its bounding environment in such a case.
    !!
    !! There is also a safeguard against poor convergence: If the number of
    !! iterations exceeds a fixed value (`CONVERG` local parameter), force to
    !! exit from the condition loops without changing the object position (i.e.
    !! no walk is done).
    AWAY: if (move_out) then
      ENVIRON_RESTRICT1: if (present(environment_limits)) then
        ! Loop while this new test spatial object is outside
        ! our target environment. It must be **strictly** within.
        do while ( test_object%distance(target) < this%distance(target) .or.  &
                   .NOT. test_object%is_within(environment_limits) )
          erep = erep + 1
          if (erep > CONVERG) exit
          ! if the `test_object` is outside the environment or not advancing
          ! to the correct direction, we create new randomly updated
          ! coordinates and iterate while this wrong condition still holds.
          call test_object%position( SPATIAL(                                 &
                  x=current_pos%x .radd.                                      &
                      RNORM( meanshift_xy,                                    &
                             cv2variance(cv_shift_xy, meanshift_xy) ),        &
                  y=current_pos%y .radd.                                      &
                      RNORM( meanshift_xy,                                    &
                             cv2variance(cv_shift_xy, meanshift_xy) ),        &
                  depth=current_pos%depth .radd.                              &
                      RNORM( meanshift_depth,                                 &
                             cv2variance(cv_shift_depth, meanshift_depth) )  ))
        end do
      else ENVIRON_RESTRICT1
        do while ( test_object%distance(target) < this%distance(target) )
          erep = erep + 1
          if (erep > CONVERG) exit
          ! if the `test_object` is outside the environment or not advancing
          ! to the correct direction, we create new randomly updated
          ! coordinates and iterate while this wrong condition still holds.
          call test_object%position( SPATIAL(                                 &
                  x=current_pos%x .radd.                                      &
                      RNORM( meanshift_xy,                                    &
                             cv2variance(cv_shift_xy, meanshift_xy) ),        &
                  y=current_pos%y .radd.                                      &
                      RNORM( meanshift_xy,                                    &
                             cv2variance(cv_shift_xy, meanshift_xy) ),        &
                  depth=current_pos%depth .radd.                              &
                      RNORM( meanshift_depth,                                 &
                             cv2variance(cv_shift_depth, meanshift_depth) )  ))
        end do
      end if ENVIRON_RESTRICT1
    else AWAY
      ENVIRON_RESTRICT2: if (present(environment_limits)) then
        ! Loop while this new test spatial object is outside
        ! our target environment. It must be **strictly** within.
        do while ( test_object%distance(target) > this%distance(target) .or. &
                   .NOT. test_object%is_within(environment_limits) )
          erep = erep + 1
          if (erep > CONVERG) exit
          ! if the `test_object` is outside the environment or not advancing
          ! to the correct direction, we create new randomly updated
          ! coordinates and iterate while this wrong condition still holds.
          call test_object%position( SPATIAL(                                 &
                  x=current_pos%x .radd.                                      &
                      RNORM( meanshift_xy,                                    &
                             cv2variance(cv_shift_xy, meanshift_xy) ),        &
                  y=current_pos%y .radd.                                      &
                      RNORM( meanshift_xy,                                    &
                             cv2variance(cv_shift_xy, meanshift_xy) ),        &
                  depth=current_pos%depth .radd.                              &
                      RNORM( meanshift_depth,                                 &
                             cv2variance(cv_shift_depth, meanshift_depth) )  ))
        end do
      else ENVIRON_RESTRICT2
        do while ( test_object%distance(target) > this%distance(target)  )
          erep = erep + 1
          if (erep > CONVERG) exit
          ! if the `test_object` is outside the environment or not advancing
          ! to the correct direction, we create new randomly updated
          ! coordinates and iterate while this wrong condition still holds.
          call test_object%position( SPATIAL(                                 &
                  x=current_pos%x .radd.                                      &
                      RNORM( meanshift_xy,                                    &
                             cv2variance(cv_shift_xy, meanshift_xy) ),        &
                  y=current_pos%y .radd.                                      &
                      RNORM( meanshift_xy,                                    &
                             cv2variance(cv_shift_xy, meanshift_xy) ),        &
                  depth=current_pos%depth .radd.                              &
                      RNORM( meanshift_depth,                                 &
                             cv2variance(cv_shift_depth, meanshift_depth) )  ))
        end do
      end if ENVIRON_RESTRICT2
    end if AWAY
    !> Finally, change the current position of the `this` object to the
    !! position defined by the `test_object`. Such a change is done only if
    !! the non-convergence condition is not detected.
    if (erep < CONVERG) call this%position(test_object)

    !> At the end, check and return the optional intent[out] parameters
    !! `is_converged` and `debug_reps`.
    if ( .not. move_out .and.                                                 &
         this%distance(target) < perc_ci * meanshift_xy * cv_shift_xy ) then
      if (present(is_converged)) then
        is_converged = .TRUE.
      end if
    end if
    ! return debug erep counter.
    if(present(debug_reps)) then
      debug_reps = erep
    end if

  end subroutine spatial_moving_corwalk_gaussian_step_25d

  !-----------------------------------------------------------------------------
  !> @brief   Implements an optionally environment-restricted **directional**
  !!          Gaussian random walk in 3D towards a `target`
  !!          the_environment::spatial class object.
  !! @details The moving object walks in three dimensions towards a target.
  !!          The process is simple, first shift along the x axis for some
  !!          random Gaussian length (with the mean `meanshift` and the
  !!          variance/CV `cv_shift`) in the direction that minimises the
  !!          coordinate-bound distance from the target. If the target is
  !!          located at a distance not exceeding the `meanshift` we have
  !!          got towards the target. Then the process is repeated for the
  !!          y and z axes. The optional restriction is that the whole walk
  !!          must not exceed specific spatial location set by the environment
  !!          parameter.
  !! @note    This `dirwalk` is a obsolete suboptimal implementation
  !!          See `the_environment::spatial_moving_corwalk_gaussian_step_3d()`
  !!          and `the_environment::spatial_moving_corwalk_gaussian_step_25d()`
  !!          for a better alternative.
  !! @param[in] target The target of the random walk, the walk should converge
  !!            to the target within finite number of steps.
  !! @param[in] meanshift the mean shift along any of the three dimensions.
  !! @param[in] cv_shift the coefficient of variation for a single elementary
  !!            shift in any of the three dimensions.
  !! @param[in] environment_limits Limits of the environment area available for
  !!            the random walk. The moving object cannot get beyond this limit.
  subroutine spatial_moving_dirwalk_gaussian_step_3d(this, target,            &
                                                           meanshift,         &
                                                           cv_shift,          &
                                                           environment_limits)
    class(SPATIAL_MOVING), intent(inout) :: this
    ! @param target The target of the random walk, the walk should converge
    !        to the target within finite number of steps.
    class(SPATIAL), intent(in) :: target
    ! @param meanshift the mean shift along any of the three dimensions.
    real(SRP), intent(in) :: meanshift
    ! @param cv_shift the coefficient of variation for a single elementary
    !        shift in any of the three dimensions.
    real(SRP), intent(in) :: cv_shift
    ! @param environment_limits Limits of the environment area available for
    !        the random walk. The moving object cannot get beyond this limit.
    class(ENVIRONMENT), intent(in), optional :: environment_limits

    ! Local test object that we test, should lay within the
    ! `environment_limits` environmental object.
    type(SPATIAL) :: current_pos, test_object

    !...........................................................................

    !> ### Implementation details ###
    !> First, we get the current coordinates of the spatial object.
    current_pos = this%now()

    !> And set a temporary spatial test object with the new coordinates,
    !! advancing/adding our random walk step. This is done via the
    !! `updated_position` sub-function.
    call test_object%position( SPATIAL(                                       &
                          updated_position(target%x,     current_pos%x),      &
                          updated_position(target%y,     current_pos%y),      &
                          updated_position(target%depth, current_pos%depth) ) )

    !> Environment restriction part of the random walk, if `environment_limits`
    !! parameter object is provided. Here the object is not allowed to go beyond
    !! its bounding environment.
    ENVIRON_RESTRICT: if (present(environment_limits)) then
      !> Loop while this new test spatial object is outside
      !! our target environment. It must be **strictly** within.
      do while (.NOT. test_object%is_within(environment_limits))
        !> (if the `test_object` is outside the environment, we create new
        !! randomly updated coordinates.)
        call test_object%position( SPATIAL(                                   &
                          updated_position(target%x,     current_pos%x),      &
                          updated_position(target%y,     current_pos%y),      &
                          updated_position(target%depth, current_pos%depth) ) )
      end do
    end if ENVIRON_RESTRICT

    !> Finally, change the current position of the `this` object to the
    !! position defined by the `test_object`. The standard function `position`
    !! for the `SPATIAL_MOVING`is used, that keeps the movement history.
    call this%position(test_object)

  contains
    !> Calculate a Gaussian random updated coordinate for multidimensional
    !! Gaussian *targeted* random walk along any of the dimensions. The dalta
    !! shift has value and variance but may be either forward or backward
    !! so as to *minimise the distance from the target*.
    function updated_position(coord_target, coord_object) result (coord_new)
      !> @return Updated coordinate.
      real(SRP) :: coord_new
      !> @param coord_target axis-bound coordinate of the target.
      !> @param coord_object actual axis-bound coordinate of the moving
      !!        spatial object.
      real(SRP), intent(in) :: coord_target, coord_object
      ! Local random Gaussian coordinate shift.
      real(SRP) :: delta
      ! We first check if the axis-bound distance between the object and the
      ! target is less then the mean shift....
      if ( abs(coord_target-coord_object) < meanshift ) then
        ! is so, we have now reached the target successfully.
        coord_new = coord_target
      else
        ! We first calculate a Gaussian random shift along this coordinate,
        ! the absolute value.
        delta = RNORM( meanshift, cv2variance(cv_shift, meanshift) )
        ! Then we should make sure the object is actually approaching
        ! the target. So we choose the direction that minimises the new
        ! coordinate-bound distance between the object and the target.
        if ( abs(coord_target-(coord_object-delta)) <                         &
             abs(coord_target-(coord_object+delta))   ) then
          coord_new = coord_object-delta
        else
          coord_new = coord_object+delta
        end if
      end if
    end function updated_position

  end subroutine spatial_moving_dirwalk_gaussian_step_3d

  !-----------------------------------------------------------------------------
  !> @brief   Implements an optionally environment-restricted **directional**
  !!          Gaussian random walk in "2.5"-D towards a `target`
  !!          the_environment::spatial class object.  i.e. 2D x y with
  !!          separate walk parameters for the third depth dimension.
  !! @details The moving object walks in three dimensions towards a target.
  !!          The process is simple, first shift along the x axis for some
  !!          random Gaussian length (with the mean `meanshift` and the
  !!          variance/CV `cv_shift`) in the direction that minimises the
  !!          coordinate-bound distance from the target. If the target is
  !!          located at a distance not exceeding the `meanshift` we have
  !!          got towards the target. Then the process is repeated for the
  !!          y and z axes. The optional restriction is that the whole walk
  !!          must not exceed specific spatial location set by the environment
  !!          parameter.
  !! @note    This `dirwalk` is a obsolete suboptimal implementation
  !!          See `the_environment::spatial_moving_corwalk_gaussian_step_3d()`
  !!          and `the_environment::spatial_moving_corwalk_gaussian_step_25d()`
  !!          for a better alternative.
  !! @param[in] target The target of the random walk, the walk should converge
  !!            to the target within finite number of steps.
  !! @param[in] meanshift_xy the mean shift along the X and Y dimensions.
  !! @param[in] cv_shift_xy the coefficient of variation for a single
  !!            elementary shift the X and Ydimensions.
  !! @param[in] meanshift_depth the mean shift along the depth dimension.
  !! @param[in] cv_shift_depth the coefficient of variation for a single
  !!            elementary shift in the depth dimension.
  !! @param[in] environment_limits Limits of the environment area available for
  !!            the random walk. The moving object cannot get beyond this limit.
  subroutine spatial_moving_dirwalk_gaussian_step_25d(this, target,           &
                                                            meanshift_xy,     &
                                                            cv_shift_xy,      &
                                                            meanshift_depth,  &
                                                            cv_shift_depth,   &
                                                            environment_limits)
    class(SPATIAL_MOVING), intent(inout) :: this
    ! @param target The target of the random walk, the walk should converge
    !        to the target within finite number of steps.
    class(SPATIAL), intent(in) :: target
    ! @param meanshift the mean shift along any of the three dimensions.
    real(SRP), intent(in) :: meanshift_xy, meanshift_depth
    ! @param cv_shift the coefficient of variation for a single elementary
    !        shift in any of the three dimensions.
    real(SRP), intent(in) :: cv_shift_xy, cv_shift_depth
    ! @param environment_limits Limits of the environment area available for
    !        the random walk. The moving object cannot get beyond this limit.
    class(ENVIRONMENT), intent(in), optional :: environment_limits

    ! Local test object that we test, should lay within the
    ! `environment_limits` environmental object.
    type(SPATIAL) :: current_pos, test_object

    !...........................................................................

    !> ### Implementation details ###
    !> First, we get the current coordinates of the spatial object.
    current_pos = this%now()

    !> And set a temporary spatial test object with the new coordinates,
    !! advancing/adding our random walk step. This is done via the
    !! `updated_position` sub-function.
    call test_object%position( SPATIAL(                                       &
                          updated_position(target%x,     current_pos%x,       &
                                          meanshift_xy,    cv_shift_xy),      &
                          updated_position(target%y,     current_pos%y,       &
                                          meanshift_xy,    cv_shift_xy),      &
                          updated_position(target%depth, current_pos%depth,   &
                                          meanshift_depth, cv_shift_depth) ) )

    !> Environment restriction part of the random walk, if `environment_limits`
    !! parameter object is provided. Here the object is not allowed to go beyond
    !! its bounding environment.
    ENVIRON_RESTRICT: if (present(environment_limits)) then
      !> Loop while this new test spatial object is outside
      !! our target environment. It must be **strictly** within.
      do while (.NOT. test_object%is_within(environment_limits))
        !> (if the `test_object` is outside the environment, we create new
        !! randomly updated coordinates.)
        call test_object%position( SPATIAL(                                   &
                          updated_position(target%x,     current_pos%x,       &
                                          meanshift_xy,    cv_shift_xy),      &
                          updated_position(target%y,     current_pos%y,       &
                                          meanshift_xy,    cv_shift_xy),      &
                          updated_position(target%depth, current_pos%depth,   &
                                          meanshift_depth, cv_shift_depth) ) )
      end do
    end if ENVIRON_RESTRICT

    !> Finally, change the current position of the `this` object to the
    !! position defined by the `test_object`. The standard function `position`
    !! for the `SPATIAL_MOVING`is used, that keeps the movement history.
    call this%position(test_object)

  contains
    !> Calculate a Gaussian random updated coordinate for multidimensional
    !! Gaussian *targeted* random walk along any of the dimensions. The dalta
    !! shift has value and variance but may be either forward or backward
    !! so as to *minimise the distance from the target*.
    function updated_position(coord_target, coord_object, meanshift, cv_shift) &
                                            result (coord_new)
      !> @return Updated coordinate.
      real(SRP) :: coord_new
      !> @param coord_target axis-bound coordinate of the target.
      !> @param coord_object actual axis-bound coordinate of the moving
      !!        spatial object.
      real(SRP) :: coord_target, coord_object
      !! @param[in] meanshift Gaussian distribution parameters for the step size:
      !!            mean positional shift.
      !! @param[in] cv_shift Gaussian distribution parameters for the step size:
      !!            coefficient of variation.
      real(SRP) :: meanshift, cv_shift
      ! Local random Gaussian coordinate shift.
      real(SRP) :: delta
      !> ### Implementation details ###
      !> We first check if the axis-bound distance between the object and the
      !! target is less then the mean shift...
      if ( abs(coord_target-coord_object) < meanshift ) then
        !> If so, we have now reached the target successfully.
        coord_new = coord_target
      else
        !> If not, first calculate a Gaussian random shift along this
        !! coordinate, the absolute value.
        delta = RNORM( meanshift, cv2variance(cv_shift, meanshift) )
        !> Then we should make sure the object is actually approaching
        !! the target. So we choose the direction that minimises the new
        !! coordinate-bound distance between the object and the target.
        if ( abs(coord_target-(coord_object-delta)) <                         &
             abs(coord_target-(coord_object+delta))   ) then
          coord_new = coord_object-delta
        else
          coord_new = coord_object+delta
        end if
      end if
    end function updated_position

  end subroutine spatial_moving_dirwalk_gaussian_step_25d

  !-----------------------------------------------------------------------------
  !> Perform one or several steps of random walk by an array of
  !! the_environment::spatial_moving class objects. This is a 3D version
  !! with the same walk parameters for the horizontal *XxY* plane and *depth*.
  subroutine rwalk3d_array( this, dist_array, cv_array,                       &
                                  dist_all, cv_all,                           &
                                  environment_limits, n_walks )
    !> @param[in] this is an array of the_environment::spatial class objects.
    class(SPATIAL_MOVING), dimension(:), intent(inout) :: this
    !> @param[in] step_size_array an array of step sizes for each object.
    real(SRP), optional, dimension(:), intent(in) :: dist_array
    !> @param[in]cv_array Coefficients of variation for the walk.
    real(SRP), optional, dimension(:), intent(in) :: cv_array
    !> @param[in] dist_all the value of the walk step size that is identical in
    !!            all objects in the array.
    real(SRP), optional, intent(in) :: dist_all
    !> @param[in] cv_all the value of the walk coefficient of variation that is
    !!            identical in all objects in the array.
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
    real(SRP), dimension(size(this)) :: dist_array_here, cv_array_here
    integer :: n_walks_here

    ! Local params.
    real(SRP), dimension(size(this)) :: step_size_walk
    integer :: j, i, ind, pop_n
    integer, dimension(size(this)) :: pop_permutation

    ! Default step size
    real(SRP), parameter :: STEP_DEFAULT = 1.0_SRP

    ! Default walk step CV.
    real(SRP), parameter ::  CV_DEFAULT = 0.5_SRP

    !> ### Implementation details ###
    !> - Calculate the distance array size.
    pop_n = size(this)

    if (present(dist_array)) then
      dist_array_here = dist_array
    else
      dist_array_here = STEP_DEFAULT
    end if

    if (present(cv_array)) then
      cv_array_here = cv_array
    else
      cv_array_here = CV_DEFAULT
    end if

    if (present(dist_all)) then
      dist_array_here = dist_all
    else
      dist_array_here = STEP_DEFAULT
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
    select type (this)
      !>    - if the input objects array is of the_environment::food_item,
      !!      also check if it is available (not eaten) using the
      !!      the_environment::food_item::is_available() method.
      !!    .
      class is (FOOD_ITEM)
        ENVIRON_RESTRICT_FOOD: if (present(environment_limits)) then
          do j=1, n_walks_here
            do i=1, pop_n
              ind = pop_permutation(i)
              if (this(ind)%is_available())                                   &
                call this(ind)%rwalk( step_size_walk(ind),                    &
                                      cv_array_here(ind),                     &
                                      environment_limits  )
            end do
          end do
        else ENVIRON_RESTRICT_FOOD
          do j=1, n_walks_here
            do i=1, pop_n
              ind = pop_permutation(i)
              if (this(ind)%is_available())                                   &
                call this(ind)%rwalk( step_size_walk(ind),                    &
                                      cv_array_here(ind),                     &
                                      Global_Habitats_Available(              &
                                          this(ind)%find_environment(         &
                                                  Global_Habitats_Available)) )
            end do
          end do
        end if ENVIRON_RESTRICT_FOOD
      !>    - in the default class case, no such check is made.
      !!    .
      !! .
      class default
        ENVIRON_RESTRICT_DEF: if (present(environment_limits)) then
          do j=1, n_walks_here
            do i=1, pop_n
              ind = pop_permutation(i)
              call this(ind)%rwalk( step_size_walk(ind),                      &
                                    cv_array_here(ind),                       &
                                    environment_limits  )
            end do
          end do
        else ENVIRON_RESTRICT_DEF
          do j=1, n_walks_here
            do i=1, pop_n
              ind = pop_permutation(i)
              call this(ind)%rwalk( step_size_walk(ind),                      &
                                    cv_array_here(ind),                       &
                                    Global_Habitats_Available(                &
                                        this(ind)%find_environment(           &
                                                  Global_Habitats_Available)) )
            end do
          end do
        end if ENVIRON_RESTRICT_DEF
    end select
    ! @note Note that the `select type` construct is placed out of the loops;
    !       this results in code duplication but avoids multiple calling of
    !       `select type` construct within the loops, which would increase
    !       speed.

  end subroutine rwalk3d_array

  !-----------------------------------------------------------------------------
  !> Perform one or several steps of random walk by an array of
  !! the_environment::spatial_moving class objects. This is a 2.5D version
  !! with separate walk parameters for the horizontal *XxY* plane and *depth*.
  subroutine rwalk25d_array ( this, dist_array_xy, cv_array_xy,               &
                                    dist_array_depth, cv_array_depth,         &
                                    dist_all_xy, cv_all_xy,                   &
                                    dist_all_depth, cv_all_depth,             &
                                    environment_limits, n_walks )
    !> @param[in] this is an array of the_environment::spatial class objects.
    class(SPATIAL_MOVING), dimension(:), intent(inout) :: this
    !> @param[in] dist_array_xy an array of step sizes for each object.
    real(SRP), optional, dimension(:), intent(in) :: dist_array_xy
    !> @param[in]cv_array_xy Coefficients of variation for the walk.
    real(SRP), optional, dimension(:), intent(in) :: cv_array_xy
    !> @param[in] dist_array_depth an array of step sizes for each object.
    real(SRP), optional, dimension(:), intent(in) :: dist_array_depth
    !> @param[in]cv_array_depth Coefficients of variation for the walk.
    real(SRP), optional, dimension(:), intent(in) :: cv_array_depth
    !> @param[in] dist_all_xy the value of the walk step size for horizontal
    !!            plane that is identical in all objects in the array.
    real(SRP), optional, intent(in) :: dist_all_xy
    !> @param[in] cv_all_xy the value of the walk coefficient of variation in
    !!            the horizontal plane that is identical in all objects within
    !!            the array.
    real(SRP), optional, intent(in) :: cv_all_xy
    !> @param[in] dist_all_depth the value of the walk step size for the depth
    !!            plane that is identical in all objects in the array.
    real(SRP), optional, intent(in) :: dist_all_depth
    !> @param[in] cv_all_depth the value of the walk coefficient of variation
    !!            in the depth plane that is identical in all objects in the
    !!            array.
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
    real(SRP), dimension(size(this)) :: dist_array_xy_here, cv_array_xy_here
    real(SRP), dimension(size(this)) ::                                       &
                                      dist_array_depth_here, cv_array_depth_here
    integer :: n_walks_here

    ! Local params.
    real(SRP), dimension(size(this)) :: step_size_walk_xy, step_size_walk_depth
    integer :: j, i, ind, pop_n
    integer, dimension(size(this)) :: pop_permutation

    ! Default step size
    real(SRP), parameter :: STEP_DEFAULT = 1.0_SRP

    ! Default walk step CV.
    real(SRP), parameter ::  CV_DEFAULT = 0.5_SRP

    !> ### Implementation details ###
    !> - Calculate the distance array size.
    pop_n = size(this)

    if (present(dist_array_xy)) then
      dist_array_xy_here = dist_array_xy
    else
      dist_array_xy_here = STEP_DEFAULT
    end if

    if (present(cv_array_xy)) then
      cv_array_xy_here = cv_array_xy
    else
      cv_array_xy_here = CV_DEFAULT
    end if

    !> - If the depth walk step distance is not provided as a parameter,
    !!   1/2 of the default step size is used as the default value. Thus,
    !!   it is assumed that the extent of random movements of the agents
    !!   in the horizontal plane is greater than vertical movements.
    if (present(dist_array_depth)) then
      dist_array_depth_here = dist_array_depth
    else
      dist_array_depth_here = STEP_DEFAULT / 2.0_SRP
    end if

    if (present(cv_array_depth)) then
      cv_array_depth_here = cv_array_depth
    else
      cv_array_depth_here = CV_DEFAULT
    end if

    if (present(dist_all_xy)) then
      dist_array_xy_here = dist_all_xy
    else
      dist_array_xy_here = STEP_DEFAULT
    end if

    if (present(cv_all_xy)) then
      cv_array_xy_here = cv_all_xy
    else
      cv_array_xy_here = CV_DEFAULT
    end if

    if (present(dist_all_depth)) then
      dist_array_depth_here = dist_all_depth
    else
      dist_array_depth_here = STEP_DEFAULT / 2.0_SRP
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

    !> - Perform Gaussian random walks for each of the objects in a random
    !!   order that is set by the `pop_permutation` array.
    !! .
    select type (this)
      !>    - if the input objects array is of the_environment::food_item,
      !!      also check if it is available (not eaten) using the
      !!      the_environment::food_item::is_available() method.
      !!    .
      class is (FOOD_ITEM)
        ENVIRON_RESTRICT_FOOD: if (present(environment_limits)) then
          do j=1, n_walks_here
            do i=1, pop_n
              ind = pop_permutation(i)
              if(this(ind)%is_available())                                    &
                call this(ind)%rwalk25d                                       &
                        ( meanshift_xy = step_size_walk_xy(ind),              &
                          cv_shift_xy = cv_array_xy_here(ind),                &
                          meanshift_depth = step_size_walk_depth(ind),        &
                          cv_shift_depth = cv_array_depth_here(ind),          &
                          environment_limits = environment_limits  )
            end do
          end do
        else ENVIRON_RESTRICT_FOOD
          do j=1, n_walks_here
            do i=1, pop_n
              ind = pop_permutation(i)
              if(this(ind)%is_available())                                    &
                call this(ind)%rwalk25d                                       &
                        ( meanshift_xy = step_size_walk_xy(ind),              &
                          cv_shift_xy = cv_array_xy_here(ind),                &
                          meanshift_depth = step_size_walk_depth(ind),        &
                          cv_shift_depth = cv_array_depth_here(ind),          &
                          environment_limits=Global_Habitats_Available(       &
                                          this(ind)%find_environment(         &
                                                  Global_Habitats_Available) ) )
            end do
          end do
        end if ENVIRON_RESTRICT_FOOD
      !>    - in the default class case, no such check is made.
      !!    .
      !! .
      class default
        ENVIRON_RESTRICT_DEF: if (present(environment_limits)) then
          do j=1, n_walks_here
            do i=1, pop_n
              ind = pop_permutation(i)
              call this(ind)%rwalk25d                                         &
                        ( meanshift_xy = step_size_walk_xy(ind),              &
                          cv_shift_xy = cv_array_xy_here(ind),                &
                          meanshift_depth = step_size_walk_depth(ind),        &
                          cv_shift_depth = cv_array_depth_here(ind),          &
                          environment_limits = environment_limits  )
            end do
          end do
        else ENVIRON_RESTRICT_DEF
          do j=1, n_walks_here
            do i=1, pop_n
              ind = pop_permutation(i)
              call this(ind)%rwalk25d                                         &
                        ( meanshift_xy = step_size_walk_xy(ind),              &
                          cv_shift_xy = cv_array_xy_here(ind),                &
                          meanshift_depth = step_size_walk_depth(ind),        &
                          cv_shift_depth = cv_array_depth_here(ind),          &
                          environment_limits=Global_Habitats_Available(       &
                                          this(ind)%find_environment(         &
                                                  Global_Habitats_Available) ) )
            end do
          end do
        end if ENVIRON_RESTRICT_DEF
    end select
    ! @note Note that the `select type` construct is placed out of the loops;
    !       this results in code duplication but avoids multiple calling of
    !       `select type` construct within the loops, which would increase
    !       speed.

  end subroutine rwalk25d_array

  !-----------------------------------------------------------------------------
  !> Function to check if this spatial object is located within an area
  !! set by an environmental object (parameter). This should be similar
  !! to an analogous function defined for the environment object.
  !! @param environment_limits `the_environment::environment` object that
  !!        sets the limits that we check the current spatial object to
  !!        be within.
  !! @returns Logical flag TRUE if the the_environment::spatial object is
  !!          within the `environment_limits` environment.
  !! @note Can be used as a user-defined operator:
  !!       @code
  !!         if ( object .within. environment ) then
  !!       @endcode
  !! @note We need it to implement environment-restricted Gaussian
  !!       random walk.
  elemental function spatial_check_located_within_3d(this, environment_limits)&
                                          result (is_within)
    class(SPATIAL), intent(in) :: this

    ! @param environment_limits `the_environment::environment` object that sets
    !        the limits that we check the current spatial object to be within.
    class(ENVIRONMENT), intent(in) :: environment_limits
    ! @returns Logical flag TRUE if the the_environment::spatial object is
    !          within the `environment_limits` environment.
    logical :: is_within

    is_within = environment_limits%is_within(this)

  end function spatial_check_located_within_3d

  !-----------------------------------------------------------------------------
  !> Logical function to check if the **argument** spatial object(s)
  !! (`check_object`) is (are) located **below** the **this** reference
  !! spatial object. Elemental function that also works with arrays. Use as:
  !! @code
  !!   reference_object%is_below(check_object)
  !! @endcode
  !! See also the user-defined operator `.below.`.
  !! The `.below.` operator can be used in two ways:
  !! - as an expression, with both scalar and array values:
  !!   @code
  !!     parents%ind(i) .below. parents%ind(i)%perceive_food%foods_seen
  !!   @endcode
  !! - in if blocks, only **scalars**:
  !!   @code
  !!     if ( parents%ind(i) .below. parents%ind(i)%perceive_food%foods_seen(1) )
  !!   @endcode
  !! .
  elemental function spatial_check_located_below(this, check_object)          &
                                                            result (are_below)
    class(SPATIAL), intent(in) :: this
    class(SPATIAL), intent(in) :: check_object
    logical :: are_below

    if ( check_object%dpos() > this%dpos() ) then
      are_below = .TRUE.
    else
      are_below = .FALSE.
    end if

  end function spatial_check_located_below

  !-----------------------------------------------------------------------------
  !> Logical function to check if the **argument** spatial object(s)
  !! (`check_object`) is (are) located **above** the **this** reference
  !! spatial object. Elemental function that also works with arrays. Use as:
  !! @code
  !!   reference_object%is_above(check_object)
  !! @endcode
  !! See also the user-defined operator `.above.`.
  !! The `.above.` operator can be used in two ways:
  !! - as an expression, with both scalar and array values:
  !!   @code
  !!     parents%ind(i) .above. parents%ind(i)%perceive_food%foods_seen
  !!   @endcode
  !! - in if blocks, only **scalars**:
  !!   @code
  !!     if ( parents%ind(i) .above. parents%ind(i)%perceive_food%foods_seen(1) )
  !!   @endcode
  !! .
  elemental function spatial_check_located_above(this, check_object)          &
                                                            result (are_above)
    class(SPATIAL), intent(in) :: this
    class(SPATIAL), intent(in) :: check_object
    logical :: are_above

    if ( check_object%dpos() < this%dpos() ) then
      are_above = .TRUE.
    else
      are_above = .FALSE.
    end if

  end function spatial_check_located_above

  !-----------------------------------------------------------------------------
  !> Determine the nearest spatial object to **this** spatial object among
  !! an array of other spatial objects.
  !! @note These two functions closely related, they return the nearest *object*
  !!       and its *id*
  !!       - the_environment::spatial_get_nearest_object()
  !!       - the_environment::spatial_get_nearest_id()
  !!       .
  !!       However, each of them can also return the other output parameter
  !!       as an intent(out) optional argument (in the subroutine style).
  !!       For example the_environment::spatial_get_nearest_object() returns
  !!       the  nearest *object* but can also provide its *id* as an
  !!       intent(out) dummy parameter. This is done for convenience of the
  !!       function use.
  !! @param neighbours the array of spatial objects that we search for the
  !!        nearest one.
  !! @param number Optional number of the nearest object within the
  !!        neighbours array.
  !! @returns Returns the nearest spatial object among the array.
  !! @note This function returns the nearest spatial **object itself** with
  !!       optionally its number. See also the next function
  !!       `spatial_get_nearest_id`.
  function spatial_get_nearest_object (this, neighbours, number) result (object)
    class(SPATIAL), intent(in) :: this

    ! @param neighbours the array of spatial objects that we search for the
    !        nearest one.
    class(SPATIAL), dimension(:), intent(in) :: neighbours

    ! @param number Optional number of the nearest object within the
    !        neighbours array.
    integer, optional, intent(out) :: number

    ! @returns Returns the nearest spatial object among the array.
    type(SPATIAL) :: object

    ! Local counter and the copy of the optional output number of the
    ! nearest neighbour.
    integer :: i, number_here

    ! Local array of distances between this spatial object and its
    ! neighbours (the array))
    real(SRP), dimension(size(neighbours)) :: distance

    ! Initialise the number of the closest neighbour.
    number_here = UNKNOWN

    !> ### Implementation details ###
    !> Calculate an array of distances between this object and the
    !! neighbouring objects using the_environment::spatial::distance().
    ! @warning The `do concurrent` construct is F2008 and can not (yet) be
    !          implemented in all compilers. Use normal `do` in such a case.
    !          Using `do concurrent` seems cleaner than this one-liner:
    !          `distance = spatial_distance_3d( this, neighbours )`.
    do concurrent ( i = 1:size(neighbours) )
      distance(i) = this%distance(neighbours(i))
    end do

    !> Locate the index of the minimum distance.
    number_here = minloc(distance,1)

    !> And return the the_environment::spatial::location() function result for
    !! this neighbour as the **resultant object**.
    object = neighbours(number_here)%location()

    !> Also return the optional nearest neighbour index number if requested.
    if(present(number)) number = number_here

  end function spatial_get_nearest_object

  !-----------------------------------------------------------------------------
  !> Determine the nearest spatial object to **this** spatial object among
  !! an array of other spatial objects.
  !! @note These two functions closely related, they return the nearest *object*
  !!       and its *id*
  !!       - the_environment::spatial_get_nearest_object()
  !!       - the_environment::spatial_get_nearest_id()
  !!       .
  !!       However, each of them can also return the other output parameter
  !!       as an intent(out) optional argument (in the subroutine style).
  !!       For example the_environment::spatial_get_nearest_object() returns
  !!       the  nearest *object* but can also provide its *id* as an
  !!       intent(out) dummy parameter. This is done for convenience of the
  !!       function use.
  !! @param neighbours the array of spatial objects that we search for the
  !!        nearest one.
  !! @param object optional spatial object that is the nearest neighbour
  !!        to **this** object.
  !! @returns The id number of the nearest object within the
  !!          neighbours array
  !! @note This function returns the **id number** of the nearest spatial
  !!       object and optionally the object itself. See also the previous
  !!       function `spatial_get_nearest_object`.
  function spatial_get_nearest_id (this, neighbours, object) result (id)
    class(SPATIAL), intent(in) :: this

    ! @param neighbours the array of spatial objects that we search for the
    !        nearest one.
    class(SPATIAL), dimension(:), intent(in) :: neighbours

    ! @param object optional spatial object that is the nearest neighbour
    !        to **this** object.
    type(SPATIAL), optional, intent(out) :: object

    ! @returns The id number of the nearest object within the
    !          neighbours array
    integer :: id

    ! Local counter.
    integer :: i

    ! Local array of distances between this spatial object and its
    ! neighbours (the array))
    real(SRP), dimension(size(neighbours)) :: distance

    ! Initialise the optional nearest object as MISSING.
    if (present(object)) call object%missing()

    !> ### Implementation details ###
    !> Calculate an array of distances between this object and the
    !! neighbouring objects. Note that we cannot use a whole-array
    !! single-liner as `distance` is a type bound *function*.
    ! @warning The `do concurrent` construct is F2008 and can not (yet) be
    !          implemented in all compilers. Use normal `do` in such a case.
    !          Using `do concurrent` seems cleaner than this one-liner:
    !          `distance = spatial_distance_3d( this, neighbours )`.
    do concurrent ( i = 1:size(neighbours) )
      distance(i) = this%distance(neighbours(i))
    end do

    !> Return the index of the nearest neighbour.
    id = minloc(distance,1)

    !> Also return the optional nearest neighbour object itself if requested.
    if(present(object)) call object%position( neighbours(id)%location() )

  end function spatial_get_nearest_id

  !-----------------------------------------------------------------------------
  !> @brief   Make an instance of the habitat object (an environment superset).
  !! @details Make / build habitat object and set parameters or defaults.
  !! @warning This subroutine seems to become quite *long* and difficult to
  !!          understand. It also combines *several tasks*, making habitat
  !!          limits, then array of predators then food resource. On the
  !!          other hand, this init procedure is normally called only once.
  !!          TODO: Consider splitting to a few shorter task-specific pieces.
  subroutine habitat_make_init(this, coord_min, coord_max, label,             &
                                      otherrisks, eggmortality,               &
                                      predators_number, loc_predators,        &
                                      food_abundance, loc_food, sizes_food )
    class(HABITAT), intent(inout) :: this

    ! Parameters of the habitat, only label is mandatory, other can be
    ! obtained from defaults.
    type(SPATIAL), intent(in) :: coord_min,coord_max  ! coordinate limits;
    character (len=*),optional, intent(in)   :: label ! name of the habitat;
    real(SRP), optional, intent(in) :: otherrisks     ! other mortality risk;
    real(SRP), optional, intent(in) :: eggmortality   ! predation risk.

    ! @param predators_number **mandatory** number of predators in the habitat.
    integer :: predators_number

    ! @param loc_predators An array of the spatial locations of each of
    !        the above predators. If not provided, will be uniformly
    !        distributed within this habitat.
    type(SPATIAL), dimension(:), optional :: loc_predators

    ! @param food_abundance **mandatory** food abundance, the number of food
    !        items within the habitat.
    integer :: food_abundance

    ! @param loc_food an optional array of the locations of the
    !        food items within (bounded) this habitat. If not provided,
    !        will be uniformly distributrd within this habitat.
    type(SPATIAL), dimension(:), optional :: loc_food

    ! @param sizes_food an optional array of the food sizes. If not provided,
    !         will be Gaussian stochastic.
    real(SRP), dimension(:), optional :: sizes_food

    ! Copies of optional parameters.
    real(SRP) :: predation_here                       ! predation risk;
    real(SRP) :: otherrisks_here                      ! other mortality risk;
    real(SRP) :: eggmortality_here                    ! predation risk.

    ! Local array of the locations of the predators.
    type(SPATIAL), allocatable, dimension(:) :: loc_pred_here

    ! Local array of locations of food items.
    type(SPATIAL), allocatable, dimension(:) :: loc_food_here

    ! Local copy of the food size array
    real(SRP), allocatable, dimension(:) :: sizes_food_here

    integer :: i    ! Local counter.

    !...........................................................................

    !> ### Implementation details ###
    !> #### A. Build the **general properties** of the habitat ####

    !> Build the physical spatial **environment** for this **habitat**,
    !! set the habitat coordinate limits.
    call this%build(coord_min, coord_max)

    !> Set label if provided or default random label otherwise.
    if (present(label)) then
      this%habitat_name = label
    else
      this%habitat_name = "HAB_" // RAND_STRING( LABEL_LENGTH - len("HAB_"),  &
                                                 LABEL_CST,LABEL_CEN )
    end if

    ! Set the level of other mortality risks.
    if (present(otherrisks)) then
      otherrisks_here = otherrisks
    else
      otherrisks_here = OTHER_RISKS_DEF
    end if
    this%risk_mortality = otherrisks_here

    ! Set egg mortality risk.
    if (present(eggmortality)) then
      eggmortality_here = eggmortality
    else
      eggmortality_here = EGGMORTALITY_DEF
    end if
    this%risk_egg_mortality = eggmortality_here

    !...........................................................................
    !> #### B. Build the **population of predators** in the habitat ####
    !> Set the number of predators.
    this%predators_number = predators_number

    !> Allocate the local array of predator locations.
    if (.not. allocated(loc_pred_here))                                       &
               allocate(loc_pred_here(this%predators_number))

    !> If we are provided with the array of spatial locations of each of
    !! the predators, we set the local array `loc_pred_here` from it.
    if (present(loc_predators)) then
      loc_pred_here = loc_predators
    else
      !> If the array of locations is *not* present, construct *uniform*
      !! random distriibution of the predators locations within the present
      !! environment (bounded uniformly distributed locations). Now use
      !! the type bound function `uniform`.
      loc_pred_here = this%uniform(this%predators_number)
    end if

    !> Now we can **allocate** the **array of predators** in the habitat.
    !! @note Note that we have to allocate predators here, unlike the food
    !!       resource part below as predators are just a raw array in this
    !!       type definition. In contrast, food resource is an object itself
    !!       with its own `make` procedure (that does allocation).
    if (.not. allocated(this%predators))                                      &
               allocate(this%predators(this%predators_number))

    !> Make each of the predators using the call parameters of the
    !! object bound init function `make (attack_rate, position, label)`.
    !! @note Note that the attack rate is assumed to be Gaussian among
    !!       the predators. Also, we do two separate loops for speed (avoid
    !!       multiple repeated *if*'s within cycles)
    if ( is_near_zero(PREDATOR_ATTACK_RATE_CV, ZERO) ) then
      !> If we happen to get zero variance, do deterministic predators.
      ! @warning The `do concurrent` construct is F2008 and can not (yet) be
      !          implemented in all compilers. Use normal `do` in such a case.
      do concurrent ( i = 1:this%predators_number )
        call this%predators(i)%make(                                          &
                            body_size=PREDATOR_BODY_SIZE,                     &
                            attack_rate=PREDATOR_ATTACK_RATE_DEFAULT,         &
                            position=loc_pred_here(i),                        &
                            label="PRED_" // TOSTR(i,this%predators_number) )
      end do
    else
      !> Generate Gaussian stochastic predators with the object bound `make`
      !! procedure.
      ! @warning Cannot use `do concurrent` here as `RNORM` is not **pure**.
      do i = 1, this%predators_number
        call this%predators(i)%make(                                          &
                            body_size=PREDATOR_BODY_SIZE,                     &
                            attack_rate=RNORM(PREDATOR_ATTACK_RATE_DEFAULT,   &
                                  cv2variance(PREDATOR_ATTACK_RATE_CV,        &
                                              PREDATOR_ATTACK_RATE_DEFAULT)), &
                            position=loc_pred_here(i),                        &
                            label="PRED_" // TOSTR(i,this%predators_number) )
      end do
    end if

    !> Deallocate the local array of predator locations, we do not need them
    !! any more further.
    if (allocated(loc_pred_here)) deallocate (loc_pred_here)

    !...........................................................................
    !> #### C. Build the **food resource(s)** of the habitat ####
    !> Set the number of food items in the food resource within the habitat.
    this%food%number_food_items = food_abundance

    !> Allocate the local array of food item locations.
    if (.not. allocated(loc_food_here))                                       &
               allocate(loc_food_here(this%food%number_food_items))

    ! Check if we are provided with the locations of the food items.
    ! @note We use the class-safe type-bound function and loop rather
    !       than array assignment.
    if (present(loc_food)) then
      ! @warning The `do concurrent` construct is F2008 and can not (yet) be
      !          implemented in all compilers. Use normal `do` in such a case.
      !do concurrent ( i=1:this%food%number_food_items )
      !  call loc_food_here(i)%position(loc_food(i))
      !end do
      ! @note Now do not use loop, use whole array with elemental function
      !       as it (seems) working well in Intel Fortran 17.
      call loc_food_here%position(loc_food)
    else
      !> If the array of locations is *not* present, construct *uniform*
      !! random distribution of the predators locations within the present
      !! environment (bounded uniformly distributed locations). Now use
      !! the type bound function `uniform`.
      ! @warning **Intel Fortran porting note**. This whole array function
      !          **does not work under Intel Fortran 13**, issues *stack
      !          overflow* runtime error, although compiles without issues:
      !          `loc_food_here = this%uniform(this%food%number_food_items)`.
      !do i=1, this%food%number_food_items
      !  loc_food_here(i)=this%uniform()
      !end do
      ! @note Now do not use loop, use whole array with elemental function
      !       as it (seems) working well in Intel Fortran 17.
      loc_food_here = this%uniform(this%food%number_food_items)
    end if

    !> And also allocate the local array of food sizes
    if (.not. allocated(sizes_food_here))                                     &
               allocate(sizes_food_here(this%food%number_food_items))

    !> If the food size array `sizes_food` is provided, use it, if not,
    !! make a Gaussian stochastic food with parameters from `COMMONDATA`.
    if (present(sizes_food)) then
      sizes_food_here = sizes_food
    else
      if ( is_near_zero(FOOD_ITEM_SIZE_DEFAULT_CV, ZERO) ) then
        !> if we happened to get zero variance, do deterministic food resource.
        sizes_food_here = FOOD_ITEM_MEAN_SIZE
      else
        !> Otherwise, generate random Gaussian array of food sizes.
        call RNORM_ARRAY( sizes_food_here,                                    &
                          FOOD_ITEM_MEAN_SIZE,                                &
                          cv2variance(FOOD_ITEM_SIZE_DEFAULT_CV,              &
                                      FOOD_ITEM_MEAN_SIZE) )
      end if
    end if

    !> **Make the food resource** now using the standard object-bound `make`
    !! function.
    !! @note Note that the food resource label is composed of `FOOD_` and
    !!       the remaining part of the habitat label.
    !! @note Note that we do **not** allocate the **food resource object**
    !!       in this procedure as it is allocated automatically by the
    !!       food-resource-bound subroutine `make`.
    call this%food%make( "FOOD_" // this%habitat_name,                        &
                         this%food%number_food_items,                         &
                         loc_food_here,                                       &
                         sizes_food_here )

    !> Deallocate temporary array at the end.
    if (allocated(loc_food_here)) deallocate(loc_food_here)
    if (allocated(sizes_food_here)) deallocate(sizes_food_here)

  end subroutine habitat_make_init

  !-----------------------------------------------------------------------------
  !> Return the name of the habitat.
  function habitat_name_get(this) result(habitat_name)
    class(HABITAT), intent(in) :: this
    !> @return The habitat name (text string label).
    character(len=LABEL_LENGTH) :: habitat_name

    habitat_name = this%habitat_name

  end function habitat_name_get

  !-----------------------------------------------------------------------------
  !> Get the mortality risk associated with this habitat.
  function habitat_get_risk_mortality(this) result (value_out)
    class(HABITAT), intent(in) :: this
    !> @return The mortality risk in this population that is not linked
    !!         with explicit predation.
    real(SRP) :: value_out

    value_out = this%risk_mortality

  end function habitat_get_risk_mortality

  !-----------------------------------------------------------------------------
  !> Get the egg mortality risk associated with this habitat.
  function habitat_get_risk_mortality_egg(this) result (value_out)
    class(HABITAT), intent(in) :: this
    !> @return The mortality risk in this population that is not linked
    !!         with explicit predation.
    real(SRP) :: value_out

    value_out = this%risk_egg_mortality

  end function habitat_get_risk_mortality_egg

  !-----------------------------------------------------------------------------
  !> Save the predators with their characteristics into a CSV file.
  subroutine habitat_save_predators_csv(this, csv_file_name, is_success)
    class(HABITAT), intent(inout) :: this
    !> @param[in] csv_file_name optional file name to save the predators.
    !!            Generated automatically if not provided.
    character(len=*), optional, intent(in) :: csv_file_name

    logical, optional, intent(out) :: is_success

    ! Local copies of optionals.
    character(len=FILENAME_LENGTH) :: csv_file_name_here
    logical :: is_success_write

    ! Counter
    integer :: i

    ! These are two character arrays to convert the numerical data into,
    ! due to the character label reshaped into the same array file.
    character(len=LABEL_LENGTH), dimension(size(this%predators%label)) ::     &
                                                body_size_str, attack_rate_str

    !> ### Implementation notes ###
    !! First, check if the optional CSV file name is provided, if not,
    !! generate it automatically.
    if (present(csv_file_name)) then
      csv_file_name_here = csv_file_name
    else
      csv_file_name_here = "predators_" // trim(this%habitat_name) // "_" //  &
                             MODEL_NAME // "_" // MMDD // "_gen_" //          &
                             TOSTR(Global_Generation_Number_Current,          &
                                   GENERATIONS) // csv
    end if

    !> Second, save the predators data using the
    !! [CSV_MATRIX_WRITE()](http://ahamodel.uib.no/doc/ar01s08.html#_subroutine_csv_matrix_write)
    !! from [HEDTOOLS](http://ahamodel.uib.no/doc/).
    ! @note Note that all numerical data are converted to strings before
    !       reshape, this is because label is a string type and all data
    !       must be the same type in reshape.
    do concurrent (i=1:this%predators_number)
      body_size_str(i) = TOSTR(this%predators(i)%body_size)
      attack_rate_str(i) = TOSTR(this%predators(i)%attack_rate)
    end do
    is_success_write = .FALSE.
    call CSV_MATRIX_WRITE(  reshape(                                          &
                              [ body_size_str,                                &
                                attack_rate_str,                              &
                                this%predators%label ],                       &
                                [ this%predators_number, 3 ] ),               &
                                                       ! ^ N. vars in reshape.
                            csv_file_name_here,                               &
                            [ character(len=LABEL_LENGTH) ::                  &
                                "BODY_SIZE", "ATTACK_RATE", "LABEL" ],        &
                            is_success_write  )

    if (present(is_success)) is_success = is_success_write

    !> The CSV output data file can be optionally compressed with the
    !! commondata::cmd_zip_output command if commondata::is_zip_outputs is set
    !! to TRUE.
    if ( IS_ZIP_OUTPUTS ) then
      call call_external(command=CMD_ZIP_OUTPUT // " " // csv_file_name_here, &
                         suppress_output=.TRUE.,                              &
                         is_background_task=ZIP_OUTPUTS_BACKGROUND )
    end if

  end subroutine habitat_save_predators_csv

  !-----------------------------------------------------------------------------
  !> Save diagnostics data that shows the dynamics of the light and the
  !! average depth of the food items, light at the average depth of the food
  !! items etc at each time step of the model.
  !!
  !! Code to generate plots from these data with gnuplot. Only 1 to 100s rows
  !! are plotted, pattern is sinusoidal.
  !! @code
  !!  set datafile separator ","
  !!  set xlabel "Time steps of the model"
  !!  set ylabel "SURFACE_LIGHT"
  !!  plot "init_dynamics.csv" every ::1::100 using 1:2 with lines, \
  !!       "init_dynamics.csv" every ::1::100 using 1:3 with lines, \
  !!       "init_dynamics.csv" every ::1::100 using 1:4 with lines, \
  !!       "init_dynamics.csv" every ::1::100 using 1:5 with lines
  !! @endcode
  subroutine save_dynamics(maxdepth, csv_file_name, is_success)
    use FILE_IO
    use CSV_IO
    !> @param[in] maxdepth is an optional maximum depth, if absent, is set to
    !!            the global maximum depth (autodetected) across all habitats.
    real(SRP), optional, intent(in) :: maxdepth
    !> @param[in] csv_file_name is the file name to save the data. The
    !!            file format is CSV.
    character(len=*), intent(in) :: csv_file_name
    !> @param[out] is_success Flag showing that data save was successful
    !!             (if TRUE).
    logical, optional, intent(out) :: is_success

    ! Local counter
    integer :: i

    ! File handle object and the string record that keeps each row of data.
    type(FILE_HANDLE) :: out_file
    character(len=:), allocatable :: record_string

    ! The maximum depth
    real(SRP) :: maxdepth_loc

    ! A standard spatial object to calculate visibility
    type(SPATIAL) :: object_std
    ! Visibility of a standard food item of mean size.
    real(SRP) :: visibility_food

    ! Column labels for the output file.
    !> The following data are saved in the CSV file:
    !! 1.  `TIMESTEP` -- the time step of the model
    !! 2.  `SURFACE_LIGHT` -- light at the surface
    !! 3.  `LIGHT_DEP_10` -- light at 1/10 of the maximum depth
    !! 4.  `LIGHT_DEP_HLF` -- light at 1/2 of the maximum depth
    !! 5.  `LIGHT_DEP_MAX` -- light at the maximum depth
    !! 6.  `MEAN_DEPTH` -- the mean depth of the food items (target depth)
    !! 7.  `LIGHT_MDEPTH` -- light at the mean depth of the food items.
    !! 8.  `FOOD_VIS_SURF` -- visibility range of a standard food item at the
    !!       surface (zero depth)
    !! 9.  `FOOD_VIS_10` -- visibility range of a standard food item at 1/10
    !!      of maximum depth
    !! 10. `FOOD_VIS_HLF` -- visibility range of a standard food item at a half
    !!      of the maximum depth.
    !! 11. `FOOD_VIS_DPMAX` -- visibility range of a standard food item at the
    !!      maximum depth
    !! 12. `FOOD_VIS_MDEPT` -- visibility range of a standard food item at the
    !!      target mean depth `MEAN_DEPTH`
    !! 13. `DEP_VR_UND_200` -- depth at which the visibility of the standard
    !!      average food item falls below 100 cm
    !! 14. `DEP_VR_UND_100` -- depth at which the visibility of the standard
    !!      average food item falls below 100 cm
    !! 15. `DEP_VR_UND_020` -- depth at which the visibility of the standard
    !!      average food item falls below 20 cm
    !! 16. `DEP_VR_UND_005` -- depth at which the visibility of the standard
    !!      average food item falls below 5 cm
    !! .
    character(len=LABEL_LENGTH), dimension(*), parameter :: COLNAMES =        &
        [ character(len=LABEL_LENGTH) ::                                      &
            "TIMESTEP", "SURFACE_LIGHT", "LIGHT_DEP_10", "LIGHT_DEP_HLF",     &
            "LIGHT_DEP_MAX", "MEAN_DEPTH", "LIGHT_MDEPTH", "FOOD_VIS_SURF",   &
            "FOOD_VIS_10", "FOOD_VIS_HLF", "FOOD_VIS_DPMAX", "FOOD_VIS_MDEPT",&
            "DEP_VR_UND_400", "DEP_VR_UND_200", "DEP_VR_UND_100",             &
            "DEP_VR_UND_020" ]

    ! The data (columns) that are calculated and saved:
    real(SRP) :: surface_light        ! - surface light
    real(SRP) :: target_depth         ! - the average depth of the food items
    real(SRP) :: target_depth_light   ! - light at the target depth
    real(SRP) :: depth_minimum_fall   ! - depth at which visibility falls below
                                      !   specific values.

    if (present(maxdepth)) then
      maxdepth_loc = maxdepth
    else
      ! Maxdepth is set the the global maximum.
      maxdepth_loc = maxval( Global_Habitats_Available%depth_max() )
    end if

    ! Open CSV file for writing
    call out_file%open_write( trim(csv_file_name), FORMAT_CSV )
    if ( .not. out_file%is_success() ) then
      if (present(is_success)) is_success = .FALSE.
      call out_file%close()
      return
    end if

    ! Make the first record the column names from the `COLNAMES` string array.
    record_string = repeat(" ", LABEL_LENGTH*size(COLNAMES)+size(COLNAMES)*4)
    call csv_record_append( record_string, COLNAMES )
    call out_file%record_write(record_string)
    if ( .not. out_file%is_success() ) then
      if (present(is_success)) is_success = .FALSE.
      call out_file%close()
      return
    end if

    ! Save other records one by one; one record is a single time step
    ! of the model.
    RECORDS_LS: do i = 1, LIFESPAN

      record_string = repeat(" ", LABEL_LENGTH*size(COLNAMES)+size(COLNAMES)*4)

      ! TIMESTEP
      call CSV_RECORD_APPEND( record_string, i )
      ! SURFACE_LIGHT
      surface_light = light_surface( i )
      call CSV_RECORD_APPEND( record_string, surface_light )
      ! LIGHT_DEP_10
      target_depth_light = light_depth( depth = maxdepth_loc / 10.0_SRP,      &
                                        surface_light = surface_light,        &
                                        is_stochastic=.FALSE.    )
      call CSV_RECORD_APPEND( record_string, target_depth_light )
      ! LIGHT_DEP_HLF
      target_depth_light = light_depth( depth = maxdepth_loc / 2.0_SRP,       &
                                        surface_light = surface_light,        &
                                        is_stochastic=.FALSE.    )
      call CSV_RECORD_APPEND( record_string, target_depth_light )
      ! LIGHT_DEP_MAX
      target_depth_light = light_depth( depth = maxdepth_loc,                 &
                                        surface_light = surface_light,        &
                                        is_stochastic=.FALSE.    )
      call CSV_RECORD_APPEND( record_string, target_depth_light )
      ! MEAN_DEPTH
      target_depth = center_depth_sinusoidal( i, maxdepth_loc )
      call CSV_RECORD_APPEND( record_string, target_depth )
      ! LIGHT_MDEPTH
      target_depth_light = light_depth( depth = target_depth,                 &
                                        surface_light = surface_light,        &
                                        is_stochastic=.FALSE.    )
      call CSV_RECORD_APPEND( record_string, target_depth_light )
      ! FOOD_VIS_SURF
      call object_std%position_v( 1.0_SRP, 2.0_SRP, 0.0 )
      visibility_food = object_std%visibility(                                &
                                      object_area =                           &
                                          carea( cm2m(FOOD_ITEM_MEAN_SIZE) ), &
                                      contrast = PREYCONTRAST_DEFAULT,        &
                                      time_step_model = i )
      call CSV_RECORD_APPEND( record_string, visibility_food )
      ! FOOD_VIS_10
      call object_std%position_v( 1.0_SRP, 2.0_SRP, maxdepth_loc / 10.0_SRP )
      visibility_food = object_std%visibility(                                &
                                      object_area =                           &
                                          carea( cm2m(FOOD_ITEM_MEAN_SIZE) ), &
                                      contrast = PREYCONTRAST_DEFAULT,        &
                                      time_step_model = i )
      call CSV_RECORD_APPEND( record_string, visibility_food )
      ! FOOD_VIS_HLF
      call object_std%position_v( 1.0_SRP, 2.0_SRP, maxdepth_loc / 2.0_SRP )
      visibility_food = object_std%visibility(                                &
                                      object_area =                           &
                                          carea( cm2m(FOOD_ITEM_MEAN_SIZE) ), &
                                      contrast = PREYCONTRAST_DEFAULT,        &
                                      time_step_model = i )
      call CSV_RECORD_APPEND( record_string, visibility_food )
      ! FOOD_VIS_DPMAX
      call object_std%position_v( 1.0_SRP, 2.0_SRP, maxdepth_loc )
      visibility_food = object_std%visibility(                                &
                                      object_area =                           &
                                          carea( cm2m(FOOD_ITEM_MEAN_SIZE) ), &
                                      contrast = PREYCONTRAST_DEFAULT,        &
                                      time_step_model = i )
      call CSV_RECORD_APPEND( record_string, visibility_food )
      ! FOOD_VIS_MDEPT
      call object_std%position_v( 1.0_SRP, 2.0_SRP, target_depth )
      visibility_food = object_std%visibility(                                &
                                      object_area =                           &
                                          carea( cm2m(FOOD_ITEM_MEAN_SIZE) ), &
                                      contrast = PREYCONTRAST_DEFAULT,        &
                                      time_step_model = i )
      call CSV_RECORD_APPEND( record_string, visibility_food )
      ! DEP_VR_UND_400
      depth_minimum_fall = minimum_depth_visibility(                          &
                            400.0_SRP,                                        &
                            object_area = carea( cm2m(FOOD_ITEM_MEAN_SIZE) ), &
                            time_step_model = i )
      call CSV_RECORD_APPEND( record_string, depth_minimum_fall )
      ! DEP_VR_UND_200
      depth_minimum_fall = minimum_depth_visibility(                          &
                            200.0_SRP,                                        &
                            object_area = carea( cm2m(FOOD_ITEM_MEAN_SIZE) ), &
                            time_step_model = i )
      call CSV_RECORD_APPEND( record_string, depth_minimum_fall )
      ! DEP_VR_UND_100
      depth_minimum_fall = minimum_depth_visibility(                          &
                            100.0_SRP,                                        &
                            object_area = carea( cm2m(FOOD_ITEM_MEAN_SIZE) ), &
                            time_step_model = i )
      call CSV_RECORD_APPEND( record_string, depth_minimum_fall )
      ! DEP_VR_UND_020
      depth_minimum_fall = minimum_depth_visibility(                          &
                            20.0_SRP,                                         &
                            object_area = carea( cm2m(FOOD_ITEM_MEAN_SIZE) ), &
                            time_step_model = i )
      call CSV_RECORD_APPEND( record_string, depth_minimum_fall )

      call out_file%record_write(record_string)
      if ( .not. out_file%is_success() ) then
        if (present(is_success)) is_success = .FALSE.
        call out_file%close()
        return
      end if

    end do RECORDS_LS

    call out_file%close()
    if ( out_file%is_success() ) then
      if (present(is_success)) is_success = .TRUE.
    else
      if (present(is_success)) is_success = .FALSE.
    end if

  end subroutine save_dynamics

  !-----------------------------------------------------------------------------
  !> Determine the centroid of the environment.
  !! @returns habitat centre coordinates, spatial object type
  !! @param nodepth Logical flag indicating that **depth** should not change.
  function environment_centre_coordinates_3d(this, nodepth)                   &
                                                      result (habitat_centre)
    class(ENVIRONMENT), intent(in) :: this
    ! @returns habitat centre coordinates, spatial object type
    type(SPATIAL) :: habitat_centre

    ! @param nodepth Logical flag indicating that **depth** should not change.
    logical, optional, intent(in) :: nodepth

    ! Calculate centre of the habitat as the averages of all coordinates.
    habitat_centre%x = (this%coord_max%x - this%coord_min%x)/2.0_SRP
    habitat_centre%y = (this%coord_max%y - this%coord_min%y)/2.0_SRP

    ! Check optional `nodepth` flag for leaving the depth level untouched.
    if (present(nodepth)) then
      if (nodepth .eqv. .FALSE.) then
        ! nodepth is FALSE, so **do** calculate centroid depth.
        habitat_centre%depth = (this%coord_max%depth -                        &
                                            this%coord_min%depth)/2.0_SRP
      end if
    else
      ! nodepth is not provided (absent), so **do** calculate centroid depth.
      habitat_centre%depth = (this%coord_max%depth -                          &
                                          this%coord_min%depth)/2.0_SRP
    end if

  end function environment_centre_coordinates_3d

  !-----------------------------------------------------------------------------
  !> Wrapper for calculating *visual range of a fish predator* using
  !! the Dag Aksnes's procedures `srgetr()`, `easyr()` and `deriv()`.
  !! See `srgetr()` for computational details.
  !! @note Note that this is a **scalar** version. The measurement unit here
  !!       is meter, might need conversion if other units are used.
  !!
  !! @param[in] irradiance background irradiance at specific depth
  !! @param[in] prey_area prey area, m^2
  !! @param[in] prey_contrast optional prey inherent contrast or default
  !!        parameter if not present.
  !! @return Returns visual range of the fish predator.
  !!
  !> Example call:
  !! @code
  !!     visual_range( light_depth( 30., light_surface(100,.TRUE.) ) )
  !! @endcode
  !!
  !! ### Specific implementations ###
  !! See specific implementations:
  !! - the_environment::visual_range_scalar() for scalar argument
  !! - the_environment::visual_range_vector() for vector argument
  !! - the_environment::visual_range_fast() elemental (parallel-safe) version
  !!   lacking sanity checks and extended debugging.
  !! .
  function visual_range_scalar(irradiance, prey_area, prey_contrast)          &
                                                result(visual_range_calculate)

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(visual_range_scalar)"

    ! @param irradiance background irradiance at specific depth
    real(SRP), intent(in) :: irradiance

    ! @param prey_area prey area, m^2
    real(SRP), optional, intent(in) :: prey_area

    ! @param prey_contrast optional prey inherent contrast or default
    !        parameter if not present.
    real(SRP), optional, intent(in) :: prey_contrast

    ! @return Returns visual range of the fish predator
    real(SRP) :: visual_range_calculate

    ! Local **high precision** value of `visual_range_calculate`, explicitly
    ! converted to `SRP` at the end.
    real(HRP) :: visual_range_HRP_here

    !> ### Notable parameters ###
    !> #### VISUAL_RANGE_MAX_OVERFLOW ####
    !! `VISUAL_RANGE_MAX_OVERFLOW = 1300.0_HRP`
    !! The maximum ceiling value of visual range (cm) calculable using
    !! `srgetr()` under the `commondata::q_prec_128` numerical
    !! precision model (referred as commondata::hrp). This value is set to
    !! the visual range in the cases of numerical overflow.
    !! @note  The real 128 bit limit (commondata::q_prec_128) is sufficient to
    !!        calculate visual range up to the fish length of approximately
    !!        700 cm (area 153.9 cm^2). At this level, the maximum visual
    !!        range is 1343.34 cm. This would be sufficient for non-whales.
    real(HRP), parameter :: VISUAL_RANGE_MAX_OVERFLOW = 1300.0_HRP

    ! Local error flag
    integer :: error_flag

    !> #### error_msg ####
    !> Local error message, character array:
    !! - 1 = NO_CONVERGENCE
    !! - 2 = DIVISION_ZERO
    !! - 3 = NEGATIVE_RANGE
    !! .
    character(len=LABEL_LENGTH), parameter, dimension(3) ::                   &
            error_msg = ["NO_CONVERGENCE", "DIVISION_ZERO ", "NEGATIVE_RANGE"]

    ! Local copies of optional parameters
    real(SRP) :: prey_area_here, prey_contrast_here

    !> ### Implementation notes ###
    !> The computational backend for the visual range computation
    !! `srgetr()`, `easyr()` and `deriv()` now uses the
    !! `commondata::q_prec_128` 128 bit numerical precision model. This
    !! precision is sufficient to calculate visual range up to the the
    !! object radius of approximately 700 cm (area 153.9 cm^2) without x86 FPU
    !! overflow errors. At this level, the maximum visual range is 1343.34 cm.
    !> #### Visual range plots ####
    !> The visual range plots below are generated by
    !! `HEDTOOLS/tools/visrange_plot.f90`.
    !! @image html img_doxy_visrange-005.svg
    !! @image latex img_doxy_visrange-005.eps  "Visual range, 5.0" width=14cm
    !! @image html img_doxy_visrange-250.svg
    !! @image latex img_doxy_visrange-250.eps  "Visual range, 250" width=14cm
    !! @image html img_doxy_visrange-500.svg
    !! @image latex img_doxy_visrange-500.eps  "Visual range, 500" width=14cm
    !> #### Calculation details ####
    !! First, check if background irradiance is below commondata::zero and
    !! just return zero visual range.
    if(irradiance < ZERO) then
      visual_range_calculate = 0.0_SRP
      return
    end if

    !> Check if prey area dummy parameter is provided, if not, use the
    !! default value `commondata::preyarea_default`.
    if (present(prey_area)) then
      prey_area_here=prey_area
    else
      prey_area_here=PREYAREA_DEFAULT
    end if

    ! Initialise the error flag.
    error_flag = 0

    !> Check if prey contrast dummy parameter is provided, if not, use the
    !! default value `commondata::preycontrast_default`.
    if (present(prey_contrast)) then
      prey_contrast_here=prey_contrast
    else
      prey_contrast_here=PREYCONTRAST_DEFAULT
    end if

    !> Call the main computational backend `the_environment::srgetr()`
    !! Note that `the_environment::srgetr()` and the whole computational
    !! backend are in `commondata::hrp` precision to reduce numerical
    !! rounding and avoid overflow errors.
    call srgetr(  visual_range_HRP_here,                                      &
                  real(BEAMATT,HRP), real(prey_contrast_here,HRP),            &
                  real(prey_area_here,HRP), real(VISCAP,HRP),                 &
                  real(EYESAT,HRP), real(irradiance,HRP), error_flag  )

    !> The visual range calculation backend `srgetr()` seems
    !! computationally suboptimal and with large object size
    !! leads to numerical overflow, so the visual range is -Infinity.
    !! This is corrected in two steps. (1) in `deriv()`, a problematic
    !! part of the computation now checks proactively for potential
    !! overflow (comparing to `log(huge())` for the current FPU
    !! precision level), so no NaNs or Infinity are produced and
    !! no FPU invalid arithmetic errors occur. This is reported to the logger.
    if (error_flag /= 0) then
      call LOG_MSG("ERROR: In " // PROCNAME //                                 &
                   ": (srgetr) issued error code " //                         &
                   TOSTR(error_flag) // " :: " // error_msg(error_flag) //    &
                   ". Object area (prey_area)=" // TOSTR(prey_area) //        &
                   ", object contrast (prey_contrast)=" //                    &
                      TOSTR(prey_contrast) //                                 &
                   ". Visual range calculated as " //                         &
                      TOSTR(visual_range_HRP_here) // " m" )
      !> If the visual range value returned by `srgetr` is negative, we reset
      !! it to the maximum overflow ceiling value `VISUAL_RANGE_MAX_OVERFLOW`.
      !! It has previously be set to `easyr()` approximation, but that was
      !! grossly wrong (hugely overestimating). It is safer to just limit
      !! visual range for detecting bigger objects to such a fixed value.
      if (visual_range_HRP_here < ZERO ) then
        visual_range_HRP_here = VISUAL_RANGE_MAX_OVERFLOW
        call LOG_MSG("ERROR: In " // PROCNAME // ":  Visual range "  //        &
                    "recalculated using `VISUAL_RANGE_MAX_OVERFLOW` ceiling: " &
                     // TOSTR(cm2m(visual_range_HRP_here)) // " m for HRP " // &
                     "real kind=" // TOSTR(HRP) // " precision model." )
      end if
    end if

    !> Finally, do explicit conversion of the final return value from
    !! the high `commondata::hrp` to the standard precision `commondata::srp`.
    visual_range_calculate = real(visual_range_HRP_here, SRP)

  end function visual_range_scalar

  !-----------------------------------------------------------------------------
  !> Wrapper for calculating *visual range of a fish predator* using
  !! the Dag Aksnes's procedures `srgetr()`, `easyr()` and `deriv()`.
  !! See `srgetr()` for computational details.
  !! @note This is a **vector** version, `prey_area` is mandatory and also
  !!       defines the vector size for all other vector parameters including
  !!       the returned function value vector. This is useful for selecting
  !!       among a swarm of prey with different sizes when vector is processed.
  !!       The measurement unit here is meter. Might need conversion if other
  !!       units are used.
  !! @param[in] irradiance background irradiance at specific depth
  !! @param[in] prey_area prey area, m^2; Mandatory parameter.
  !! @param[in] prey_contrast_vect optional prey inherent contrast or default
  !!        parameter if not present. This parameter sets **individual vector**
  !!        prey contrast, so can be used for providing stochastic contrast
  !!        data for each object.
  !! @param[in] prey_contrast optional prey inherent contrast or default
  !!        parameter if not present. This parameter sets **common scalar**
  !!        prey contrast for the whole vector.
  !! @return Returns visual range of the fish predator.
  !!
  !! ### Specific implementations ###
  !! See specific implementations:
  !! - the_environment::visual_range_scalar() for scalar argument
  !! - the_environment::visual_range_vector() for vector argument
  !! - the_environment::visual_range_fast() elemental (parallel-safe) version
  !!   lacking sanity checks and extended debugging.
  !! .
  function visual_range_vector(irradiance, prey_area, prey_contrast_vect,     &
                                  prey_contrast) result(visual_range_calculate)

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(visual_range_vector)"

    ! @param irradiance background irradiance at specific depth
    real(SRP), intent(in) :: irradiance

    ! @param prey_area prey area, m^2
    ! @note  Mandagtory parameter.
    real(SRP), dimension(:), intent(in) :: prey_area

    ! @param prey_contrast_vect optional prey inherent contrast or default
    !        parameter if not present.
    ! @note  This parameter sets **individual vector** prey contrast, so can
    !        be used for providing stochastic contrast data for each object.
    real(SRP), optional, dimension(size(prey_area)), intent(in) ::           &
                                                            prey_contrast_vect

    ! @param prey_contrast optional prey inherent contrast or default
    !        parameter if not present.
    ! @note  This parameter sets **common scalar** prey contrast for the whole
    !        vector.
    real(SRP), optional, intent(in) :: prey_contrast

    ! @return Returns visual range of the fish predator
    real(SRP), dimension(size(prey_area)) :: visual_range_calculate

    ! Local copies of optional parameters
    real(SRP), dimension(size(prey_area)) :: prey_contrast_here

    ! Local counter
    integer :: i

    !> ### Implementation notes ###
    !> The computational backend for the visual range computation
    !! `srgetr()`, `easyr()` and `deriv()` now uses the
    !! `commondata::q_prec_128` 128 bit numerical precision model. This precision
    !! is sufficient to calculate visual range up to the the object radius of
    !! approximately 700 cm (area 153.9 cm^2) without FPU overflow errors. At
    !! this level, the maximum visual range is 1343.34 cm.
    !> #### Visual range plots ####
    !> The visual range plots below are generated by
    !! `HEDTOOLS/tools/visrange_plot.f90`.
    !! @image html img_doxy_visrange-005.svg
    !! @image latex img_doxy_visrange-005.eps "Visual range, 0.5" width=14cm
    !! @image html img_doxy_visrange-250.svg
    !! @image latex img_doxy_visrange-250.eps "Visual range, 250" width=14cm
    !! @image html img_doxy_visrange-500.svg
    !! @image latex img_doxy_visrange-500.eps "Visual range, 500" width=14cm
    !> Check if prey contrast dummy parameter is provided, if not, use the
    !! default value `commondata::preycontrast_default`.
    if (present(prey_contrast_vect)) then
      prey_contrast_here=prey_contrast_vect
    else
      prey_contrast_here=PREYCONTRAST_DEFAULT
    end if

    !> Note that the function can use either a (possibly stochastic) vector or
    !! a scalar value (same for all) parameters for prey contrast as optional
    !! arguments. Scalar value takes precedence if both are provided.
    if (present(prey_contrast)) then
      prey_contrast_here = prey_contrast
    end if

    !> #### Calculation of the visual range ####
    !> The main body of calculation is actually a loop calling the
    !! scalar-based function `the_environment::visual_range_scalar()`.
    ! @warning Converting this loop to `do concurrent` requires purifying
    !          `visual_range_scalar()`: deleting calls to LOG_MSG and probably
    !          adding optional output variables for error reporting out of
    !          the procedure call (not in-place logging).
    ! @note    The backend engine is now elemental and parallel-safe. There
    !          is also a non-debug and no log **elemental** `visual_range()`
    !          procedure `visual_range_fast`().
    do i=1, size(visual_range_calculate)
      visual_range_calculate(i) = visual_range_scalar(irradiance,             &
                                      prey_area(i), prey_contrast_here(i))
    end do

  end function visual_range_vector

  !-----------------------------------------------------------------------------
  !> Wrapper for calculating *visual range of a fish predator* using
  !! the Dag Aksnes's procedures `srgetr()`, `easyr()` and `deriv()`.
  !! This is a new **elemental** and parallel-ready visual range function
  !! wrapper  making use the elemental-procedures based computational backend.
  !! See notes on `visual_range_scalar()` and `srgetr()` for computational
  !! details.
  !! @param[in] irradiance background irradiance at specific depth
  !! @param[in] prey_area prey area, m^2
  !! @param[in] prey_contrast optional prey inherent contrast or default
  !!            parameter if not present.
  !! @return    Returns visual range of the fish predator
  !! @warning   It is simplified, e.g. **no error reporting** is done.
  !!            Nonetheless, debugging the old code has shown that it works
  !!            okay up to the `MAX_LOG` non-whale size limit. Use the
  !!            non-elemental version whenever debugging or logging is required!
  !!            The parameter `prey_contrast` to the **vector**-based function
  !!            call must be an **scalar**. Otherwise a segmentation fault
  !!            runtime error results. Vector-based call is analogous to calling
  !!            `visual_range_vector` with `prey_contrast_vect` parameter.
  !!
  !! ### Specific implementations ###
  !! See specific implementations:
  !! - the_environment::visual_range_scalar() for scalar argument
  !! - the_environment::visual_range_vector() for vector argument
  !! - the_environment::visual_range_fast() elemental (parallel-safe) version
  !!   lacking sanity checks and extended debugging.
  !! .
  elemental function visual_range_fast(irradiance, prey_area, prey_contrast)  &
                                                result (visual_range_calculate)

    ! @param irradiance background irradiance at specific depth
    real(SRP), intent(in) :: irradiance

    ! @param prey_area prey area, m^2
    real(SRP), optional, intent(in) :: prey_area

    ! @param prey_contrast optional prey inherent contrast or default
    !        parameter if not present.
    real(SRP), optional, intent(in) :: prey_contrast

    ! @param error_string optional error description, if no error was produced
    !        returns "SUCCESS       " (note trailing spaces, use `trim`
    !        while testing!)
    ! @warning Cannot be used for **output** as in pure functions arguments
    !          must have intent **in** only.
    !character(len=LABEL_LENGTH), optional, intent(out) :: error_string

    ! @return Returns visual range of the fish predator
    real(SRP) :: visual_range_calculate

    ! Local **high precision** value of `visual_range_calculate`, explicitly
    ! converted to `SRP` at the end.
    real(HRP) :: visual_range_HRP_here

    ! Local copies of optional parameters
    real(SRP) :: prey_area_here, prey_contrast_here

    ! Local error flag
    integer :: error_flag

    ! Local error message.
    ! @warning Must have all the same length.
    ! @note `error_msg` and `error_string` are not used for normal non-debug
    !       operation and are disabled in the code here.
    !character(len=*), parameter, dimension(0:3) :: error_msg =     &
    !    ["SUCCESS       ", "NO_CONVERGENCE", "DIVISION_ZERO ", "NEGATIVE_RANGE"]

    ! Final error description (array), cannot be output externally as an
    ! intent(out) parameter but can  be used for internal debugging.
    ! @note `error_string` is not used for normal non-debug operation and is
    !       disabled in the code here.
    ! character(len=LABEL_LENGTH) :: error_string

    if (present(prey_area)) then
      prey_area_here=prey_area
    else
      prey_area_here=PREYAREA_DEFAULT
    end if

    if (present(prey_contrast)) then
      prey_contrast_here=prey_contrast
    else
      prey_contrast_here=PREYCONTRAST_DEFAULT
    end if

    ! Initialise the error flag. Normally, no error reporting is done here
    ! to keep the intent requirements of the pure function.
    error_flag = 0

    !> ### Implementation details ###
    !> This version of the visual_range procedure does not call the error
    !! correction code and does not report errors into the logger. However,
    !! this allows declaring it as **elemental**.
    !! @note Note that `srgetr()` and the whole computational backend are now
    !!       in `commondata::hrp` precision to avoid numerical overflow errors.
    call srgetr(  visual_range_HRP_here,                                      &
                  real(BEAMATT,HRP), real(prey_contrast_here,HRP),            &
                  real(prey_area_here,HRP), real(VISCAP,HRP),                 &
                  real(EYESAT,HRP), real(irradiance,HRP), error_flag   )


    ! Optionally produce an error description array. Can be used only for
    ! internal debugging, not for output in this pure function.
    ! @note `error_string` is not used for normal non-debug operation and is
    !       disabled in the code here.
    !if (present(error_string)) error_string = error_msg(error_flag)
    !error_string = error_msg(error_flag)

    !> Finally, do explicit conversion from `commondata::hrp` to
    !! `commondata::srp`.
    visual_range_calculate = real(visual_range_HRP_here, SRP)

  end function visual_range_fast

  !=============================================================================
  !> @name Visual range calculation backend.
  !! The subroutines the_environment::srgetr(), the_environment::easyr() and
  !! the_environment::deriv() should be better isolated into a separate
  !! module or form a submodule, but it is not used here as submodules are a
  !! F2008 feature not supported by all compiler systems. Anyway, submodule
  !! is not essential here.
  !! @note Note that all these backend procedures are now **pure** and
  !!        therefore parallel-safe.
  !! @{
  ! submodule AKVISRANGE ! Dag AKsnes VISual RANGEe utilities.
  ! @warning The commondata::hrp 128 precision model should be used, anything
  !          smaller can produce FPU overflows! Precision is defined in the
  !          @ref commondata module.
  ! @code
  !   integer, parameter, public :: Q_PREC_128 = selected_real_kind(33, 4931)
  !   integer, parameter, public :: HRP = Q_PREC_128
  ! @endcode
  !-----------------------------------------------------------------------------
  !> Obtain visual range by solving the non-linear equation
  !! by means of Newton-Raphson iteration and derivation in
  !! subroutine the_environment::deriv(). Initial value is calculated in
  !! the_environment::easyr(). The calculation is based on the model described
  !! in Aksnes & Utne (1997) Sarsia 83:137-147.
  !! @note Programmed and tested 29 January 2001 Dag L Aksnes.
  !! @note This subroutine is left almost intact with only the most crucial
  !!       changes.
  !!       (a) added commondata::hrp precision specifier (128 bit precision
  !!           model) to real type specifiers and `_HRP` for literal constants;
  !!       (b) restored diagnostic `IER` output from archival Hed11.f90.
  !!       (c) added explicit `intent` and declared the procedures as
  !!           `pure` that is required for being parallel-friendly.
  ! **Input parameters:**
  !> @param[in]  RST  start value of r calculated by the_environment::easyr();
  !! @param[in]  c    beam attenuation coefficient `(m-1)`;
  !! @param[in]  C0   prey inherent contrast;
  !! @param[in]  Ap   prey area `(m^2)`;
  !! @param[in]  Vc   parameter characterising visual capacity (d.l.)
  !! @param[in]       this parameter is denoted E' in Aksnes & Utne;
  !! @param[in]  Ke   saturation parameter `(uE m-2 s-1)`;
  !! @param[in]  Eb   background irradiance at depth `DEPTH`;
  ! **Output parameters:**
  !> @param[out] r    the predator's visual range (when `F1=0`);
  !! @param[out] IER  **1** = No convergence after IEND steps, error return;
  !!                  **2** = Return in case of zero divisor;
  !!                  **3** = r out of allowed range (negative);
  !!                  **0** = valid r returned.
  !!
  !> **Notable variables:**
  !> - **F1**     function value of equation in `deriv`;
  !! - **FDER**   the derivative of the function.
  !! .
  elemental subroutine srgetr(r, c, C0, Ap, Vc, Ke, Eb, IER)
  ! Input parameters
  !     RST       : start value of r calculated by `easyr`
  !     c         : beam attenuation coefficient (m-1)
  !     C0        : prey inherent contrast
  !     Ap        : prey area (m^2)
  !     Vc        : parameter characterising visual capacity (d.l.)
  !                 this parameter is denoted E' in Aksnes & Utne
  !     Ke        : saturation parameter (uE m-2 s-1)
  !     Eb        : background irradiance at depth DEPTH
  ! Output parameters
  !     F1     : function value of equation in `deriv`
  !     FDER   : the derivative of the function
  !     r      : the predator's visual range (when F1=0)
  !     IER    : = 1, No convergence after IEND steps. Error return.
  !              = 2, Return in case of zero divisor.
  !              = 3, r out of allowed range (negative)
  !              = 0, valid r returned
  real(HRP), intent(in)   :: c, C0, Ap, Vc, Ke, Eb
  real(HRP), intent(out)  :: r
  integer, optional, intent(out) :: IER

  real(HRP) :: AS, EPS, RST, TOL, TOLF, F1, FDER, DX
  integer   :: IEND, I

  !.............................................................................

    ! Initial guess of visual range (RST)
    call easyr(RST,C0,Ap,Vc,Ke,Eb)

    ! Upper boundary of allowed error of visual range.
    EPS = TOLERANCE_HIGH_DEF_HRP
    ! Maximum number of iteration steps
    IEND = 100

    ! Prepare iteration
    r = RST
    TOL = r

    call deriv(r,F1,FDER,c,C0,Ap,Vc,Ke,Eb)
    TOLF = 100.0_HRP * EPS

    ! Start iteration expmt
    ! @warning Cannot probably be converted to `do concurrent` due to `exit`s
    !!         from the loop.
    do 6 I = 1, IEND
      if (F1 .feq. 0.0_HRP) goto 7

      ! Equation is not satisfied by r
      if (FDER .feq. 0.0_HRP) goto 8

      ! Iteration is possible
      DX = F1/FDER
      r = r-DX

      ! Test on allowed range
      if (r .LT. 0.0_HRP) goto 9

      TOL = r

      call deriv(r,F1,FDER,c,C0,Ap,Vc,Ke,Eb)

      ! Test on satisfactory accuracy
      TOL = EPS
      AS = abs(r)
      if ((AS-1.0_HRP) > 0.0_HRP) TOL = TOL*AS
      TOL = TOL*AS
      if ((abs(DX)-TOL) > 0.0_HRP) goto 6
      if ((abs(F1)-TOLF) .LE. 0.0_HRP) goto 7
  6   continue

      ! No convergence after IEND steps. Error return.
      if (present(IER)) IER = 1
  7   return
      ! Return in case of zero divisor
  8   if (present(IER)) IER = 2
      return
      ! r out of allowed range (negative)
  9   if (present(IER)) IER = 3
      return

  end subroutine srgetr

  !-----------------------------------------------------------------------------
  !> Obtain a first estimate of visual range by using a simplified
  !! expression of visual range. See `srgetr()` for more details.
  !! @note This subroutine is left almost intact, only (a) added
  !!       commondata::hrp for real type (`HRP` is *high real precision*
  !!       (128 bit) and is defined in @ref commondata).
  elemental subroutine easyr(r, C0, Ap, Vc, Ke, Eb)
    real(HRP), intent(out) :: r
    real(HRP), intent(in)  :: C0, Ap, Vc, Ke, Eb
    real(HRP) :: R2
    ! See the calling routine the_environment::srgetr() for explanation of
    ! parameters
    R2 = abs(C0)*Ap*Vc*Eb/(Ke+Eb)
    r  = sqrt(R2)
    return
  end subroutine easyr

  !-----------------------------------------------------------------------------
  !> Derivation of equation for visual range of a predator.
  !! See the_environment::srgetr() for more details.
  !! @note This is a high precision version. But higher precision alone is
  !!       not sufficient to prevent numerical exponentiation overflow.
  !! @note This subroutine is left almost intact, only (a) added
  !!       commondata::hrp for literal constants and real type (`HRP` is the
  !!       *high real precision* (128 bit) and is defined in @ref commondata),
  !!       (b) added numerical overflow safeguard code based on `MAX_LOG`
  !!       exponentiability limit; added logging of overflow using `LOG_MSG`.
  ! **Output parameters:**
  !> @param[out]   F1     function value of equation in
  !!                      the_environment::deriv();
  !! @param[out]   FDER   the derivative of the function;
  !! @param[inout] r      the predator's visual range (when `F1=0`).
  !!
  !> **Input parameters:**
  !!
  !> See explanation in calling routine the_environment::srgetr().
  !!
  !> The function and the derivative is calculated on the basis of the
  !! log-transformed expression.
  elemental subroutine deriv(r, F1, FDER, c, C0, Ap, Vc, Ke, Eb)
    ! Input parameters
    !     See explanation in calling routine
    ! Output parameters
    !     F1     : function value of equation in `deriv`
    !     FDER   : the derivative of the function
    !     r      : the predator's visual range (when F1=0)
    !
    !    The function and the derivative is calculated on the basis of the
    !    log-transformed expression
    real(HRP), intent(inout) :: r
    real(HRP), intent(out)   :: F1, FDER
    real(HRP), intent(in)    :: c, C0, Ap, Vc, Ke, Eb

    real(HRP) :: FR1, FR2

    !> ### Implementation notes ###
    !! `MAX_LOG` is a parameter determining the safe limit of `exp` function
    !! overflow in the current float point precision model, well below this.
    !! We cannot calculate precise exponent of a value exceeding this
    !! parameter. The maximum possible exponentiation-able value is set
    !! to the maximum **128-bit** real value kind `Q_PREC_128`, this bottom
    !! line value would set a safe limit for `HRP` calculations.
    !! A benefit of this approach is that it doesn't require IEEE exception
    !! handling that depends on not fully portable IEEE modules.
    !! @note  The real 128 bit limit (Q_PREC_128) is sufficient to calculate
    !!        visual range up to the fish length of approximately 700 cm
    !!        (area 153.9 cm2). At this level, the maximum visual range is
    !!        1343.34 cm. This would be sufficient for non-whales.
    real(HRP), parameter :: HUGE_REAL = huge(0.0_HRP)
    real(HRP), parameter :: MAX_LOG = log(HUGE_REAL)

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(deriv)"

    FR2=log(abs(C0)*Ap*Vc)

    ! @note    Here `exp(c*r)` can result in huge FPU overflow (Infinity),
    !          so we now check if the exponent is likely to be so high and
    !          **if yes**, just set the huge ceiling. In such a case one needs
    !          to *rescale* variables (e.g. from meters to tenth or hundreds
    !          of meters).
    ! @warning The resulting calculations are then most probably grossly
    !          wrong but we nonetheless avoid FPU runtime error: incorrect
    !          arithmetic calculations.
    if (c*r < MAX_LOG) then
      FR1=log(((Ke+Eb)/Eb)*r*r*exp(c*r))
    else
      FR1=HUGE_REAL
      ! Subroutine `LOG_MSG` is defined in `LOGGER` module. It reports
      ! numerical overflow errors.
      ! @warning `LOG_MSG` is **disabled** here as it cannot be **pure** and
      !          hampers purifying `deriv` for parallel processing.
      !          A negative side is no error report logging. But the `HRP`
      !          64 bit precision model seems to work okay. There is also
      !          a guarantee against overflow and gross errors with the
      !          `HUGE_REAL` parameter.
      !call LOG_MSG (  "ERROR in " // PROCNAME // ":  FR1 overflow "  //      &
      !                "exceeding " // TOSTR(HUGE_REAL) // " limit :" //      &
      !                " c*r =" // TOSTR(c*r)  )
    end if

    F1 = FR1-FR2
    FDER = c + 2.0_HRP/r
    return

  end subroutine deriv

  ! end submodule AKVISRANGE
  !> @}
  !=============================================================================

  !-----------------------------------------------------------------------------
  !> Calculate deterministic surface light at specific time step of the model.
  !! Light (`surlig`) is calculated from a sine function. Light intensity
  !! just beneath the surface is modelled by assuming a 50 % loss by scattering
  !! at the surface: @f[ L_{t} = L_{max} 0.5 sin(\pi dt / \Omega ) @f].
  !! @returns surface light intensity.
  !! @param tstep time step of the model, limited by maximum
  !!        commondata::lifespan.
  !! @note This is a deterministic version.
  !! Code for wxMaxima for quickcalc:
  !! @code
  !!    surlig(a, span) := 500*0.5*(1.01+sin(3.14*2.*50*a/(1.*span)));
  !!    surlig(a, span) := 500*0.5*(1.01+sin(3.14*2.*50*a/span));
  !!    wxplot2d(surlig(a, 14000), [a,0, 1400]);
  !! @endcode
  !! @note Note that this is an elemental function that accepts both scalar
  !!       and array parameter.
  elemental function light_surface_deterministic(tstep) result (surlig)
    ! @returns surface light intensity
    real(SRP) :: surlig
    ! @param tstep time step of the model, limited by maximum
    !        commondata::lifespan.
    integer, optional, intent(in) :: tstep

    ! Local copies of optionals.
    integer :: tstep_loc

    if (present(tstep)) then
      tstep_loc = tstep
    else
      tstep_loc = Global_Time_Step_Model_Current
    end if

    ! TODO: what is 1.01 in old code?
    surlig = DAYLIGHT * 0.5_SRP * (1.01_SRP + sin(PI * 2.0_SRP *              &
                        DIELCYCLES * real(tstep_loc,SRP) / real(LIFESPAN,SRP)))

  end function light_surface_deterministic

  !-----------------------------------------------------------------------------
  !> Calculate stochastic surface light at specific time step of the model.
  !! Light (`surlig`) is calculated from a sine function. Light intensity
  !! just beneath the surface is modelled by assuming a 50 % loss by scattering
  !! at the surface: @f[ L_{t} = L_{max} 0.5 sin(\pi dt / \Omega ) @f].
  !! This deterministic value sets the *mean* for the stochastic final value,
  !! which is Gaussian with CV equal to `DAYLIGHT_CV`.
  !! @returns surface light intensity
  !! @param tstep time step of the model, limited by maximum
  !!        commondata::lifespan.
  !! @param is_stochastic logical indicator for stochastic light intensity
  !!        if TRUE, then Gaussian stochastic version is used, if FALSE,
  !!        deterministic is used.
  !! Code for wxMaxima for quickcalc:
  !! @code
  !!    surlig(a, span) := 500*0.5*(1.01+sin(3.14*2.*50*a/(1.*span)));
  !!    wxplot2d(surlig(a, 14000), [a,0, 1400]);
  !! @endcode
  function light_surface_stochastic_scalar(tstep, is_stochastic) result (surlig)
    ! @returns surface light intensity
    real(SRP) :: surlig
    ! @param tstep time step of the model, limited by maximum
    !        commondata::lifespan
    integer, optional, intent(in) :: tstep
    ! @param is_stochastic logical indicator for stochastic light intensity
    !        if TRUE, then Gaussian stochastic version is used, if FALSE,
    !        deterministic is used.
    logical, intent(in) :: is_stochastic

    ! Local variable, deterministic light intensity that sets a mean value.
    real(SRP) :: surlig_deterministic

    ! Local copies of optionals.
    integer :: tstep_loc

    if (present(tstep)) then
      tstep_loc = tstep
    else
      tstep_loc = Global_Time_Step_Model_Current
    end if

    surlig_deterministic = light_surface_deterministic(tstep_loc)

    if (is_stochastic) then
      ! If `is_stochastic` flag is TRUE, then the light intensity is a random
      ! Gaussian variate with the mean equal to the deterministic value and
      ! coefficient of variation DAYLIGHT_CV.
      surlig = RNORM(surlig_deterministic,(surlig_deterministic*DAYLIGHT_CV)**2)
    else
      ! If `is_stochastic` flag is FALSE, deterministic *scalar* value is used.
      surlig = surlig_deterministic
    end if

  end function light_surface_stochastic_scalar

  !-----------------------------------------------------------------------------
  !> Calculate stochastic surface light at specific time step of the model.
  !! @param tstep time step of the model, limited by maximum
  !!        commondata::lifespan.
  !! @returns surface light intensity.
  !! @param is_stochastic logical indicator for stochastic light intensity
  !!        if TRUE, then Gaussian stochastic version is used, if FALSE,
  !!        deterministic is used.
  !! @note    This function accepts vector arguments.
  !! @warning Note that the `tstep` array parameter is *mandatory* here
  !!          (otherwise the generic interface is ambiguous).
  function light_surface_stochastic_vector(tstep, is_stochastic) result (surlig)
    ! @param tstep time step of the model, limited by maximum
    !        commondata::lifespan.
    integer, intent(in), dimension(:) :: tstep
    ! @returns surface light intensity
    real(SRP), dimension(size(tstep)) :: surlig
    ! @param is_stochastic logical indicator for stochastic light intensity
    !        if TRUE, then Gaussian stochastic version is used, if FALSE,
    !        deterministic is used.
    logical, intent(in) :: is_stochastic

    ! Local variable, deterministic light intensity that sets a mean value.
    real(SRP), dimension(size(tstep)) :: surlig_deterministic

    ! Local counter
    integer :: i

    !> ### Implementation details ###
    surlig_deterministic = light_surface_deterministic(tstep)

    if (is_stochastic) then
      !> If `is_stochastic` flag is TRUE, then the light intensity is a random
      !> Gaussian vector with *each element* mean equal to the deterministic
      !> value and coefficient of variation `DAYLIGHT_CV`.
      do i=1, size(tstep)
        !surlig(i) = surface_light_stochastic_scalar(i,.TRUE.) ! small overhead
        surlig(i) = RNORM( surlig_deterministic(i),                           &
                           (surlig_deterministic(i) * DAYLIGHT_CV)**2 )
      end do
    else
      !> If `is_stochastic` flag is FALSE, deterministic *vector* value is used.
      surlig = surlig_deterministic
    end if

  end function light_surface_stochastic_vector

  !-----------------------------------------------------------------------------
  !> Calculate underwater light at specific depth given specific surface light.
  !! @details Underwater light is attenuated following Beer’s law,
  !!          @f[ E_{b}(z,t) = L_{t} e^{-K z} , @f] where @f$ E_{b}(z,t) @f$
  !!          is background irradiance at depth z at time t and K is the
  !!          attenuation coefficient for downwelling irradiance. The value
  !!          of K in the old code was set very high to allow the vertical
  !!          dynamics to take place within 30 depth cells.
  !! @returns   Eb background irradiance at specific depth.
  !! @param[in] depth The integer depth horizon where we get background
  !! @param[in] surface_light Irradiance at the surface, normally calculated
  !!            at specific time point of the model with the
  !!            the_environment::light_surface() function. If this parameter is
  !!            absent, surface light at the current time step is obtained. The
  !!            time step in such case is obtained from
  !!            commondata::global_time_step_model_current.
  !! @param[in] is_stochastic stochastic indicator for the surface light in
  !!            the_environment::light_surface() function. If this parameter
  !!            is absent, the default commondata::daylight_stochastic
  !!            parameter value is used.
  !! @note      Note that this function accepts **integer** depth, a separate
  !!            function should be used for physical real type depth.
  !! @note      Note that it is an elemental function that accepts both scalar
  !!            and array parameters.
  function light_depth_integer(depth, surface_light, is_stochastic) result(Eb)
    ! @returns Eb background irradiance at specific depth.
    real(SRP) :: Eb
    ! @param[in] depth The integer depth horizon where we get background
    !            irradiance.
    integer, intent(in) :: depth
    ! @param[in] surface_light Irradiance at the surface, normally calculated
    !            at specific time point of the model with `light_surface()`
    !            generic function.
    real(SRP), optional, intent(in) :: surface_light
    ! @param[in] is_stochastic stochastic indicator for the surface light in
    !            the_environment::light_surface() function.  If this parameter
    !            is absent, the default commondata::daylight_stochastic
    !            parameter value is used.
    logical, optional, intent(in) :: is_stochastic

    ! Local copies of optionals.
    real(SRP) :: surface_light_loc
    logical :: is_stochastic_loc

    if (present(is_stochastic)) then
      is_stochastic_loc = is_stochastic
    else
      is_stochastic_loc = DAYLIGHT_STOCHASTIC
    end if

    if (present(surface_light)) then
      surface_light_loc = surface_light
    else
      surface_light_loc = light_surface(is_stochastic=is_stochastic_loc)
    end if

    !> @note Note that the commondata::lightdecay parameter is in cm.
    ! @note Old code implementation can differ (HED24)!
    !   In *SRinitage*, *SRhabitat* and *SRgrowth* calculations use an
    !   iterative formula that does not exactly follow the Beer's law
    !   formula:
    ! @code
    !   Eb = autosurlig(age)  ! Eb is background irradiance at a depth
    !   do dep = 1, depth
    !     Eb = Eb * exp(-lightdecay)
    !   end do
    ! @endcode
    !   Eb = surface_light * (exp(-LIGHTDECAY))**(depth)
    !   or in *SRdecision* it follows the Beer's law formula:
    ! @code
    !   comp1 = autosurlig(age)*exp(-lightdecay*(z))
    ! @endcode
    ! @note wxMaxima quick code for plotting (assuming surface light 500.0):
    ! @code
    !    wxplot2d( 500.0*exp(-0.002 * D), [D, 0., 3000.] );
    ! @endcode
    Eb = surface_light_loc * exp(-LIGHTDECAY * real(depth,SRP))

  end function light_depth_integer

  !-----------------------------------------------------------------------------
  !> Calculate underwater light at specific depth given specific surface light.
  !! @details Underwater light is attenuated following Beer’s law,
  !!          @f[ E_{b}(z,t) = L_{t} e^{-K z} , @f] where @f$ E_{b}(z,t) @f$
  !!          is background irradiance at depth z at time t and K is the
  !!          attenuation coefficient for downwelling irradiance.
  !! @returns Eb background irradiance at specific depth.
  !! @param[in] depth The integer depth horizon where we get background.
  !! @param[in] surface_light Irradiance at the surface, normally calculated
  !!            at specific time point of the model with the
  !!            the_environment::light_surface() function. If this parameter is
  !!            absent, surface light at the current time step is obtained. The
  !!            time step in such case is obtained from
  !!            commondata::global_time_step_model_current.
  !! @param[in] is_stochastic stochastic indicator for the surface light in
  !!            the_environment::light_surface() function. If this parameter
  !!            is absent, the default commondata::daylight_stochastic
  !!            parameter value is used.
  !! @note      Note that this function accepts **real** depth.
  function light_depth_real(depth, surface_light, is_stochastic) result(Eb)
    ! @returns Eb background irradiance at specific depth.
    real(SRP) :: Eb
    ! @param depth The integer depth horizon where we get background
    !        irradiance.
    real(SRP), intent(in) :: depth
    ! @param surface_light Irradiance at the surface, normally calculated
    !        at specific time point of the model with
    !        the_environment::light_surface() function.
    real(SRP), optional, intent(in) :: surface_light
    ! @param[in] is_stochastic stochastic indicator for the surface light in
    !            the_environment::light_surface() function.  If this parameter
    !            is absent, the default commondata::daylight_stochastic
    !            parameter value is used.
    logical, optional, intent(in) :: is_stochastic

    ! Local copies of optionals.
    real(SRP) :: surface_light_loc
    logical :: is_stochastic_loc

    if (present(is_stochastic)) then
      is_stochastic_loc = is_stochastic
    else
      is_stochastic_loc = DAYLIGHT_STOCHASTIC
    end if

    if (present(surface_light)) then
      surface_light_loc = surface_light
    else
      surface_light_loc = light_surface(is_stochastic=is_stochastic_loc)
    end if

    !> @note Note that the commondata::lightdecay parameter is in cm.
    !> @note wxMaxima quick code for plotting (assuming surface light 500.0):
    !! @code
    !!    wxplot2d( 500.0*exp(-0.002 * D), [D, 0., 3000.] );
    !! @endcode
    Eb = surface_light_loc * exp(-LIGHTDECAY * depth)

  end function light_depth_real

  !-----------------------------------------------------------------------------
  !> Calculate distance between 3D or 2D points. This is a function engine
  !! for use  within type bound procedures.
  !! **Example** (dist_scalar):
  !! @code
  !!    dist(1.0,10.0,   2.0,20.0,   3.0,30.0 )
  !! @endcode
  !! @note This version accepts individual scalar coordinates.
  !! @note Note that it is an elemental function, accepting also arrays (should
  !!       have equal size and shape for elemental operations).
  elemental function dist_scalar(x1, x2, y1, y2, z1, z2) result (distance)
    real(SRP) :: distance
    real(SRP), intent(in) :: x1, x2, y1, y2
    real(SRP), intent(in), optional :: z1, z2

    if (present(z1)) then
      if (present(z2)) then
        !> @note 3D distance is calculated only if z1 and z2 are provided,
        !!       otherwise an orphaned z coordinate is ignored.
        distance = sqrt( (x1-x2)**2 + (y1-y2)**2 + (z1-z2)**2 )
      else
        distance = sqrt( (x1-x2)**2 + (y1-y2)**2 )
      end if
    else
      distance = sqrt( (x1-x2)**2 + (y1-y2)**2 )
    end if

  end function dist_scalar

  !-----------------------------------------------------------------------------
  !> Calculate distance between N-dimensional points. This is a function engine
  !! for use  within other type bound procedures.
  !! @returns the distance between two N-dimensional vectors.
  !! @param cvector N-dimensional vectors for the two points we calculate
  !!        the distance between. For a 3D case the vercors look like:
  !!        x = cvector(1), y = cvector(2), z = cvector(3).
  !! **Example** dist_vector:
  !! @code
  !!    dist( [1.0, 2.0, 3.0], [10.0, 20.0, 30.0] )
  !! @endcode
  !! @note This version accepts vectors of coordinates for each of
  !!       the two objects.
  !! @warning The shapes and sizes of the two arrays must be equal
  pure function dist_vector_nd(cvector1, cvector2) result (distance)
    ! @returns the distance between two N-dimensional vectors.
    real(SRP) :: distance
    ! @param cvector N-dimensional vectors for the two points we calculate
    !        the distance between. For a 3D case the vercors look like:
    !        x = cvector(1), y = cvector(2), z = cvector(3).
    real(SRP), intent(in), dimension(:) :: cvector1
    real(SRP), intent(in), dimension(:) :: cvector2

    distance = sqrt(sum( (cvector1-cvector2)**2 ))

  end function dist_vector_nd

  !-----------------------------------------------------------------------------
  !> Calculate the squared distance between two *N*-dimensional points.
  !! @note This function is useful in some cases when squared distances
  !!       are used to save on calculation of square root.
  pure function dist2_vector(cvector1, cvector2) result (distance)
    ! @returns the squared distance between two N-dimensional vectors.
    real(SRP) :: distance
    ! @param cvector N-dimensional vectors for the two points we calculate
    !        the distance between. For a 3D case the vercors look like:
    !        x = cvector(1), y = cvector(2), z = cvector(3).
    real(SRP), intent(in), dimension(:) :: cvector1
    real(SRP), intent(in), dimension(:) :: cvector2

    distance = abs( sum( (cvector1-cvector2)**2 ) )

  end function dist2_vector

  !-----------------------------------------------------------------------------
  !> Calculate the magnitude of an arbitrary *N*-dimensional vector. This is
  !! a raw vector backend.
  pure function vect_magnitude(vector) result (vlength)
    !> @param[in] vector a vector in *N* dimensions.
    real(SRP), intent(in), dimension(:) :: vector
    !> @return The magnitude of the vector.
    real(SRP) :: vlength

    !> Vector length is trivially calculated as the euclidean norm:
    !! @f[ \left \| x \right \| = \sum x_{i}^{2} . @f]
    vlength = sqrt( sum(vector**2) )

  end function vect_magnitude

  !-----------------------------------------------------------------------------
  !> Calculate the unit step along a single coordinate axis given the average
  !! distance between any two points in a N-dimensional Gaussian random walk.
  !! @return Unit step length along a single x,y,or z axis
  !! @param average_distance The average distance traversed by the moving
  !!        point during a single step of the Gaussian random walk.
  !! @param dimensionality The dimensionality of the random walk
  !! **Example:** dist2step Generate a Gaussian random walk in 3D with discrete
  !!          step size, i.e. the **true distance between the points** (rather
  !!          than coordinate shifts) equal to 10.0:
  !! @code
  !!          call moving_zzz%rwalk( dist2step(10.0), 0.5 )
  !! @endcode
  elemental function dist2step(average_distance, dimensionality)             &
                                                              result (unit_step)
    ! @return Unit step length along a single x,y,or z axis
    real(SRP) :: unit_step

    ! @param average_distance The average distance traversed by the moving
    !        point during a single step of the Gaussian random walk.
    real(SRP), intent(in) :: average_distance

    ! @param dimensionality The dimensionality of the random walk
    integer, optional, intent(in) :: dimensionality

    ! Local copy of the `dimensionality` parameter.
    integer :: dim_here

    if (present(dimensionality)) then
      dim_here = dimensionality
    else
      dim_here = DIMENSIONALITY_DEFAULT
    end if

    unit_step = sqrt( (average_distance**2) / dim_here  )

  end function dist2step

  !-----------------------------------------------------------------------------
  !> Create a single food item at an undefined position with default size.
  elemental subroutine food_item_create(this)
    class(FOOD_ITEM), intent(inout) :: this

    ! We here just set an undefined location of the food object using
    !! standard interface function `missing`.
    call this%missing()

    !> This also cleanups the history stack, i.e. fills it with `MISSING`
    !! values.
    call this%spatial_history_clean()

    ! Then we set the food item size.
    this%size = FOOD_ITEM_SIZE_DEFAULT

    ! This food item is NOT eaten from creation, so set the status
    this%eaten = .FALSE.

    ! Set `UNKNOWN` iid at create.
    ! Initially, set a random iid.
    ! @warning random id on create is now disabled to allow elemental function,
    !          because random are never pure. So care to set iid's elsewhere.
    !          `call this%set_iid()`
    this%food_iid = UNKNOWN

  end subroutine food_item_create

  !-----------------------------------------------------------------------------
  !> Make a single food item, i.e. place it into a specific position
  !! in the model environment space and set the size.
  !! @param Location of the food item as a `SPATIAL` type container
  !! @param size This is the optional size of the food item. If absent
  !!        then default food size is used as defined in `COMMONDATA`
  !! @param iid id for the food item. Note: There are no random iids for
  !!        food items, iid should agree with the item index within the
  !!        food resource object.
  elemental subroutine food_item_make(this, location, size, iid)
    class(FOOD_ITEM), intent(inout) :: this

    ! @param Location of the food item as a `SPATIAL` type container
    type(SPATIAL), intent(in) :: location

    ! @param size This is the optional size of the food item. If absent
    !        then default food size is used as defined in `COMMONDATA`
    real(SRP), optional, intent(in) :: size

    ! @param iid id for the food item.
    ! @note  There are no random iids for food items, iid should agree with
    !        the item index within the food resource.
    integer, intent(in) :: iid

    !> ### Implementation details ###
    !> We here just set the location of the food object using
    !! standard interface function `position`.
    call this%position(location)

    !> Also, clean up the history stack, i.e. fills it with `MISSING`
    !! values.
    call this%spatial_history_clean()

    !> Then we set the food item size. Check if optional size is provided and
    !! left untouched if not.
    !! @note Note that if the value provided is very small (e.g. zero or below),
    !!       a minimum default value is used as a "floor".
    if (present(size)) then
      this%size = max(FOOD_ITEM_MINIMUM_SIZE, size)
    end if

    !> This food item is NOT eaten from creation, so set the status FALSE.
    this%eaten = .FALSE.

    !> Set the individual id `iid`.
    call this%set_iid(iid)

  end subroutine food_item_make

  !-----------------------------------------------------------------------------
  !> @brief   Stochastic outcome of **this** food item capture by an agent.
  !!          Returns TRUE if the food item is captured.
  !! @details In this version, food item capture depends only on the **fixed
  !!          probability** set by default as `FOOD_ITEM_CAPTURE_PROBABILITY`.
  !!          Could also implement more complex patterns, e.g. dependent
  !!          on the food item size (e.g. capture probability increases with
  !!          food item size).
  !! @param[in] prob fixed probability of food item capture.
  !! return@ TRUE if capture success.
  !! @warning This function does not change the state of the food item object,
  !!          only returns success.
  !! @warning This function cannot be made elemental / puredue to random
  !!          number call.
  function food_item_capture_success_stochast(this, prob) result (success)
    class(FOOD_ITEM), intent(in) :: this  ! This food object.
    ! @param[in] prob fixed probability of food item capture.
    real(SRP), optional, intent(in) :: prob
    ! return@ TRUE if capture success.
    logical :: success

    ! Local copy of `prob` parameter.
    real(SRP) :: prob_here

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME="(food_item_capture_success_stochast)"

    success = .FALSE.                     ! Init to FALSE.

    !> ### Implementation details ###
    !> Check if `prob` is present, if not, use default parameter
    !! `FOOD_ITEM_CAPTURE_PROBABILITY` value, assuming this food item is in
    !! proximity of the predator agent (see function `capture_probability`.
    if (present(prob)) then
      prob_here = prob
    else
      prob_here = FOOD_ITEM_CAPTURE_PROBABILITY
    end if

    !> First check if this food item is available, if it is not, this may
    !! mean it was an error (e.g. only available food items should get into
    !! the agent perception object).
    if ( this%is_available() ) then
      !> @note  Note that the probability of capture is fixed by the input
      !!        value and does not currently depend on the properties of
      !!        the food item itself. TODO: make option to depend on **this**
      !!        food item properties, e.g. its size.
      if ( RAND_R4() < prob_here ) success = .TRUE.
    else
      call LOG_DBG( LTAG_WARN // PROCNAME // ", Cannot capture food item " // &
                "as it is not available (has been already eaten?). Check code.")
    end if

  end function food_item_capture_success_stochast

  !-----------------------------------------------------------------------------
  !> Calculate the probability of capture of **this** food item by a predator
  !! agent depending on the distance between the agent and this food item.
  !! @details Capture probability is determined on the basis of a non-parametric
  !!          relationship (interpolation) between the distance between the
  !!          predator agent and this food item. It is equal to the the
  !!          baseline value set by `commondata::food_item_capture_probability`
  !!          parameter at the distance 0.0 and approaches a nearly zero value
  !!          set by the parameter
  !!          `commondata::food_item_capture_probability_min = 0.1`, at the
  !!          distance equal to the visual range of the agent.
  !! @param[in] distance optional distance to the food item.
  !! @param[in] time_step_model optional time step of the model, if absent,
  !!       obtained from the global variable
  !!       `commondata::global_time_step_model_current`.
  !! @returns The probability of capture of this food item.
  function food_item_capture_probability_calc( this,                          &
                                               distance, time_step_model )    &
                                                            result (capt_prob)
    class(FOOD_ITEM), intent(in) :: this
    ! @param[in] distance optional distance to the food item.
    real(SRP), optional, intent(in) :: distance
    ! @param[in] time_step_model optional time step of the model, if absent,
    !       obtained from the global variable
    !       `commondata::global_time_step_model_current`.
    integer, optional, intent(in) :: time_step_model
    ! @returns The probability of capture of this food item.
    real(SRP) :: capt_prob

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter ::                                            &
                                PROCNAME="(food_item_capture_probability_calc)"

    ! Local copies of optional dummy parameters.
    real(SRP) :: distance_here
    integer :: time_step_model_here

    ! Local variables.
    real(SRP) :: visrange_predator

    ! Interpolation grid parameters.
    real(SRP), dimension(3) :: interpol_abscissa, interpol_ordinate

    ! Check optional time step parameter. If unset, use global
    ! `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> ### Implementation details ###
    !> #### Visual range ####
    !> First, the visual range for a predator @f$ R_{v} @f$ to detect this
    !! food item is calculated using
    !! the_environment::food_item_visibility_visual_range().
    visrange_predator = this%visibility()

    !> The probability of capture of this food item by a predator is obtained
    !! by a nonparametric function that is based on nonlinear interpolation.
    !> #### Interpolation grid abscissa ####
    !> The distance for calculating  the probability of capture of this food
    !! item by a predator is expressed  in terms of the predator's visual
    !! range. The interpolation grid abscissa is set to the distance zero,
    !! half of the visual range (@f$ 1/2 R_{v} @f$) and full visual range
    !! (@f$ R_{v} @f$) of the predator agent.
    interpol_abscissa = [ 0.0_SRP,                                            &
                          visrange_predator/2.0_SRP,                          &
                          visrange_predator           ]

    !> #### Interpolation grid ordinate ####
    !> It is assumed that the probability of capture of this food item is
    !! equal to the `commondata::food_item_capture_probability` parameter at
    !! the distance zero, and reduces to 0.8 of this value at @f$ 1/2 R_{v} @f$,
    !! and further reduces to
    !! `commondata::food_item_capture_probability_min = 0.1` at
    !! the full visual range distance (@f$ R_{v} @f$).
    !! @image html img_doxygen_capture_prob.svg
    !! @image latex img_doxygen_capture_prob.eps  "Capture probability and visual range" width=14cm
    !! @note Interpolation plot command:
    !!       `htintrpl.exe  [0.0 0.5 1.0] [0.85, 0.68, 0.1]` (0.68=0.85*0.8).
    interpol_ordinate = [ FOOD_ITEM_CAPTURE_PROBABILITY,                      &
                          FOOD_ITEM_CAPTURE_PROBABILITY*0.8_SRP,              &
                          FOOD_ITEM_CAPTURE_PROBABILITY_MIN       ]

    !> #### Optional distance parameter ####
    !> Check optional distance dummy parameter and set local value or
    !! default (half a visual range) if the parameter is not provided.
    if (present(distance)) then
      if (distance > visrange_predator) then
        !> Also check if the distance provided is longer than the visual range
        !! so this food item cannot be detected by the agent. There should be
        !! normally no such cases, if this occurs, it may point to a bug.
        call LOG_DBG( LTAG_WARN // "distance to food item exceeds " //        &
                      "the visual range.", PROCNAME, MODNAME )
        !> If this is the case, set the capture probability to zero and exit.
        capt_prob = 0.0_SRP
        return
      end if
      !> Finally, he distance provided should not be equal to the missing value
      !! commondata::missing. Return zero capture probability in such a case.
      if (distance .feq. MISSING) then
        capt_prob = 0.0_SRP
        return
      end if
      distance_here = distance
    else
      distance_here = visrange_predator/2.0_SRP
    end if

    !> #### Final calculations ####
    !> Finally, the probability of capture of this food item by a predator is
    !! obtained by **nonlinear** (`DDPINTERPOL`) interpolation of the distance
    !! value expressed in terms of the visual range over the grid set by
    !! `interpol_abscissa` and `interpol_ordinate`. There is an additional
    !! condition *0 < p < 1* that is enforced by commondata::within().
    ! @note Interpolation algorithm can be `LINTERPOL` (linear) or
    !       `DDPINTERPOL` (nonlinear) or any other supported by HEDTOOLS.
    capt_prob = within( DDPINTERPOL(interpol_abscissa, interpol_ordinate,     &
                                    distance_here),                           &
                        0.0_SRP, 1.0_SRP )

    !> #### Extended debugging outputs ####
    !> Log the capture probability visual range calculated, along with the
    !! distance and the visual range.
    call LOG_DBG("INFO: Calculated food item capture probability: " //        &
            TOSTR(capt_prob) // ", Distance: " // TOSTR(distance_here) //     &
            ", Visual range:" // TOSTR(visrange_predator),  PROCNAME, MODNAME)

    !> Interpolation plots can be saved in the @ref intro_debug_mode
    !! "debug mode" using this plotting command:
    !! commondata::debug_interpolate_plot_save().
    call debug_interpolate_plot_save(                                         &
            grid_xx=interpol_abscissa, grid_yy=interpol_ordinate,             &
            ipol_value=distance_here,                       &
            algstr="DDPINTERPOL",     & ! @warning Must be as in `capt_prob`!
            output_file="plot_debug_capture_probability_s_" //                &
              TOSTR(Global_Time_Step_Model_Current) // "_" //                 &
              TOSTR(this%get_iid()) // "_" //                                 &
              RAND_STRING(LABEL_LENGTH, LABEL_CST, LABEL_CEN) // PS )

  end function food_item_capture_probability_calc

  !-----------------------------------------------------------------------------
  !> Calculate the visibility range of this food item. Wrapper to the
  !! the_environment::visual_range() function. This function calculates
  !! the distance from which this food item can be seen by a predator
  !! (i.e. the default predator's visual range).
  !! @warning The `visual_range` procedures use meter for units, this
  !!          auto-converts to cm.
  !! @warning Cannot implement a generic function accepting also vectors of
  !!          this objects as only elemental object-bound array functions are
  !!          allowed by the standard. This function cannot be elemental, so
  !!          passed-object dummy argument must always be scalar.
  function food_item_visibility_visual_range(this, object_area, contrast,     &
                                            time_step_model) result (visrange)
    class(FOOD_ITEM), intent(in) :: this
    !> @param[in] object_area is optional area of the food item (m). If not
    !!            provided, obtained from the this object `size` attribute
    !!            (the_environment::food_item::size).
    real(SRP), optional, intent(in) :: object_area
    !> @param[in] contrast is optional inherent visual contrast of the food
    !!            item. the default contrast of all objects is defined by the
    !!            commondata::preycontrast_default parameter.
    real(SRP), optional, intent(in) :: contrast
    !> @param[in] optional time step of the model, if absent gets the current
    !!            time step as defined by the value of
    !!            `commondata::global_time_step_model_current`.
    integer, optional, intent(in) :: time_step_model
    real(SRP) :: visrange

    ! Local copies of optionals
    real(SRP) :: object_area_here, contrast_here

    ! Local variables
    real(SRP) :: irradiance_agent_depth
    integer :: time_step_model_here

    ! Check optional object area, the default value, if this parameter is
    ! absent, is the circle area of this food item converted to m.
    if (present(object_area)) then
      object_area_here = object_area
    else
      object_area_here = carea( cm2m( this%size ) )
    end if

    ! Check optional `contrast` parameter. If unset, use global
    ! `commondata::preycontrast_default`.
    if (present(contrast)) then
      contrast_here = contrast
    else
      contrast_here = PREYCONTRAST_DEFAULT
    end if

    ! Check optional time step parameter. If unset, use global
    ! `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> ### Implementation details ###
    !> Calculate ambient illumination / irradiance at the depth of
    !! this food item at the given time step.
    irradiance_agent_depth =  this%illumination(time_step_model_here)

    !> Return visual range of a predator to see this food item.
    visrange = m2cm (  visual_range ( irradiance = irradiance_agent_depth,    &
                                      prey_area = object_area_here,           &
                                      prey_contrast = contrast_here )  )

  end function food_item_visibility_visual_range

  !-----------------------------------------------------------------------------
  !> Find the depth at which the visibility of a spatial object becomes
  !! smaller than a specific distance value `target_range`.
  !! @note This is a diagnostic function.
  function minimum_depth_visibility( target_range,                            &
                                     depth_range_min, depth_range_max,        &
                                     object_area, object_contrast,            &
                                     time_step_model )    result (depth_out)
    !> @param[in] target_range This is the target visual range (visibility)
    !!            value: this function calculates the depth at which visibility
    !!            becomes smaller than this target range.
    real(SRP), intent(in) :: target_range

    !> !> @param[in] depth_range_min sets the optimisation range for depth,
    !!               this is the **minimum** depth where  the search is done.
    !!               This is an optional parameter, if absent, is set to the
    !!               global minimum  depth in the global habitats array
    !!               commondata::global_habitats_available.
    real(SRP), optional, intent(in) :: depth_range_min
    !> !> @param[in] depth_range_max sets the optimisation range for depth,
    !!               this is the **maximum** depth where  the search is done.
    !!               This is an optional parameter, if absent, is set to the
    !!               global maximum  depth in the global habitats array
    !!               commondata::global_habitats_available.
    real(SRP), optional, intent(in) :: depth_range_max

    !> @param[in] object_area is the optional area of the spatial object (m) for
    !!            which the depth is calculated. If absent, is calculated for
    !!            the default average food item
    !!            commondata::food_item_size_default.
    real(SRP), optional, intent(in) :: object_area
    !> @param[in] object_contrast optional contrast of the spatial object for
    !!            which the calculation is done; if absent is set from the
    !!            default commondata::preycontrast_default.
    real(SRP), optional, intent(in) :: object_contrast

    !> @param[in] time_step_model is the time step of the model. If absent, is
    !!            obtained from the global variable
    !!            commondata::global_time_step_model_current.
    integer, optional, intent(in) :: time_step_model
    real(SRP) :: depth_out

    ! Local copies of optionals
    real(SRP) :: depth_range_min_loc, depth_range_max_loc
    real(SRP) :: object_area_here, contrast_here
    integer :: time_step_model_here

    ! Check optional parameters and calculate the default range of the depths.
    ! Note that if depth range values are not set as parameters explicitly,
    ! they are calculated from the global habitats array
    ! commondata::global_time_step_model_current.
    if (present(depth_range_min)) then
      depth_range_min_loc = depth_range_min
    else
      depth_range_min_loc = minval( Global_Habitats_Available%depth_min() )
    end if

    if (present(depth_range_max)) then
      depth_range_max_loc = depth_range_max
    else
      depth_range_max_loc = maxval( Global_Habitats_Available%depth_max() )
    end if

    ! Check optional object area, the default value, if this parameter is
    ! absent, is the circle area of the default food item converted to m,
    ! commondata::food_item_size_default.
    if (present(object_area)) then
      object_area_here = object_area
    else
      object_area_here = carea( cm2m( FOOD_ITEM_SIZE_DEFAULT ) )
    end if

    ! Check optional `contrast` parameter. If unset, use global
    ! `commondata::preycontrast_default`.
    if (present(object_contrast)) then
      contrast_here = object_contrast
    else
      contrast_here = PREYCONTRAST_DEFAULT
    end if

    ! Check optional time step parameter. If unset, use global
    ! `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> ### Implementation details ###
    !! The depth when the visibility of the spatial object becomes smaller
    !! than the target distance `target_range` is calculated using the Brent's
    !! commondata::zeroin() optimisation algorithm, see Brent, R., (1973).
    !! Algorithms for Minimization Without Derivatives, Prentice-Hall, Inc.
    !!
    !! The function for calculating the visibility of this spatial object
    !! for optimisation by `zeroin` is calculated by ::visibility_loc().
    !! This function is local to this function and is further wrapped into
    !! also local ::visibility_loc_diff() (it is set as the parameter f to
    !! commondata::zeroin()).
    depth_out = zeroin( depth_range_min_loc,                                  &
                        depth_range_max_loc,                                  &
                        visibility_loc_diff,                                  &
                        TOLERANCE_HIGH_DEF_SRP )

    !> If the the depth cannot be calculated, further checks are done and
    !! an appropriate limiting value is set for this function return.
    if ( depth_out .feq. MISSING ) then
      if ( visibility_loc(depth_range_max_loc) > target_range ) then
        depth_out = depth_range_max_loc
      else if ( visibility_loc(depth_range_min_loc) < target_range ) then
        depth_out = depth_range_min_loc
      end if
    end if

    contains
      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !> This function calculates the visibility range of the spatial object
      !! at the depth given by the argument `depth`.
      !! This function is internal to
      !! the_environment::minimum_depth_visibility().
      function visibility_loc(depth) result (vis_out)
        real(SRP), intent(in) :: depth
        real(SRP) :: vis_out

        real(SRP) :: irradiance_at_depth

        !> Calculate ambient illumination / irradiance at the depth of
        !! this food item at the given time step.
        irradiance_at_depth =                                                 &
            light_depth ( depth=depth,                                        &
                          surface_light = light_surface(                      &
                                            tstep=time_step_model_here,       &
                                            is_stochastic=.FALSE.) )

        !> Return the visibility range for this spatial object.
        vis_out = m2cm ( visual_range ( irradiance = irradiance_at_depth,     &
                                        prey_area = object_area_here,         &
                                        prey_contrast = contrast_here )  )

      end function visibility_loc

      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !> This is a wrapper function that calculates the visibility range minus
      !! target minimum distance.
      !! This function is internal to
      !! the_environment::minimum_depth_visibility().
      real(SRP) function visibility_loc_diff(depth)
        !> @param[in] depth the depth of the spatial object.
        real(SRP), intent(in) :: depth
        visibility_loc_diff = visibility_loc(depth) - target_range
      end function visibility_loc_diff

  end function minimum_depth_visibility

  !-----------------------------------------------------------------------------
  !> Make the food item "disappear" and take the "eaten" state, i.e.
  !! impossible for consumption by the agents.
  elemental subroutine food_item_disappear(this)
    class(FOOD_ITEM), intent(inout) :: this

    this%eaten = .TRUE.

  end subroutine food_item_disappear

  !-----------------------------------------------------------------------------
  !> Logical check-indicator function for the food item being eaten and not
  !! available.
  !! @returns Logical indicator TRUE if the food item is eaten and not
  !!          available any more.
  elemental function food_item_is_eaten_unavailable(this) result(not_available)
    class(FOOD_ITEM), intent(in) :: this

    ! @returns Logical indicator TRUE if the food item is eaten and not
    !          available any more.
    logical :: not_available

    if  (this%eaten) then
      not_available = .TRUE.
    else
      not_available = .FALSE.
    end if

  end function food_item_is_eaten_unavailable

  !-----------------------------------------------------------------------------
  !> Logical check-indicator function for the food item being available.
  !! @returns Logical indicator TRUE if the food item is present
  !! in the environment and therefore available.
  !! @note It is the opposite of the above function
  !!       the_environment::food_item::is_unavailable().
  elemental function food_item_is_available(this) result(available)
    class(FOOD_ITEM), intent(in) :: this

    ! @returns Logical indicator TRUE if the food item is present
    !          in the environment and therefore available.
    logical :: available

    if (this%eaten) then
      available = .FALSE.
    else
      available = .TRUE.
    end if

  end function food_item_is_available

  !-----------------------------------------------------------------------------
  !> Get the size component of the food item object.
  !! @returns item_size The size of the food item.
  elemental function food_item_get_size(this) result (item_size)
    class(FOOD_ITEM), intent(in) :: this

    ! @returns item_size The size of the food item.
    real(SRP) :: item_size

    item_size = this%size

  end function food_item_get_size

  !-----------------------------------------------------------------------------
  !> Calculate the mass of a food item, the non-OO backend.
  !! @details The food item mass depends on the density of the food and its
  !!          volume @f[ \rho \cdot  \frac{4}{3}\pi r^{3} , @f]
  !!          where @f$ \rho @f$ is the food density and
  !!          @f[ V = \frac{4}{3}\pi r^{3} @f] is the food item volume.
  !! @note    Food item density is set by commondata::food_item_density
  !!          parameter.
  elemental function size2mass_food(radius) result (this_mass)
    real(SRP), intent(in) :: radius
    real(SRP) :: this_mass

    this_mass = FOOD_ITEM_DENSITY * (4.0_SRP/3.0_SRP * PI * radius**3)

  end function size2mass_food

  !-----------------------------------------------------------------------------
  !> Calculate the size (radius) of a food item, a reverse function of
  !! the_environment::size2mass_food():
  !! @f[ = \sqrt[3]{ \frac{M}{\rho \cdot 4/3 \pi} } , @f]
  !! where @f$ M @f$ is the food item mass,  @f$ \rho @f$ is its density.
  !! @note    Food item density is set by commondata::food_item_density
  !!          parameter.
  elemental function mass2size_food(mass) result (radius)
    real(SRP), intent(in) :: mass
    real(SRP) :: radius

    radius = ( mass / (FOOD_ITEM_DENSITY*4.0_SRP/3.0_SRP*PI) )**(1_SRP/3_SRP)

  end function mass2size_food

  !-----------------------------------------------------------------------------
  !> Calculate and get the mass of the food item.
  !! @returns item_mass The mass of the food item.
  !! @note This is an OO frontend/wrapper.
  elemental function food_item_get_mass(this) result (item_mass)
    class(FOOD_ITEM), intent(in) :: this

    ! @returns item_mass The mass of the food item.
    real(SRP) :: item_mass

    !> ### Implementation details ###
    !> The food item mass depends on the density of the food and its volume
    !! @f[ \rho \cdot  \frac{4}{3}\pi r^{3} , @f] where @f$ \rho @f$ is the food
    !! density and @f[ V = \frac{4}{3}\pi r^{3} @f] is the food item volume.
    ! @note Use the `size2mass_food` non-OO backend for calculation.
    item_mass = size2mass_food(this%size)

  end function food_item_get_mass

  !-----------------------------------------------------------------------------
  !> Set unique id for the food item object.
  !! @param iid individual id number for the food item.
  elemental subroutine food_item_set_iid(this, iid)
    class(FOOD_ITEM), intent(inout) :: this

    ! @param iid individual id number for the food item.
    integer, intent(in) :: iid

    ! Set the food item iid
    this%food_iid = iid

  end subroutine food_item_set_iid

  !-----------------------------------------------------------------------------
  !> Clone the properties of this food item to another food item.
  !! @note Note that the this food item serves as the source and the other,
  !!       as the destination for cloning. So when used like this
  !!       `food_item_source%clone( cloned_to_this_destination_item )`
  elemental subroutine food_item_clone_assign(this, the_other)
    class(FOOD_ITEM), intent(in) :: this
    !> param[inout] the_other Target food item to which the properties of
    !!              the **this** item are cloned. It is declared as a
    !!              polymorphic `class` so can get an extension type.
    class(FOOD_ITEM), intent(inout) :: the_other

    !> ### Implementation details ###
    !> Use make method to transfer all the properties from `this` to
    !! `the_other`.
    call the_other%make(this%location(), this%size, this%food_iid)
    the_other%eaten = this%eaten

  end subroutine food_item_clone_assign

  !-----------------------------------------------------------------------------
  !> Get the unique id of the food item object.
  !! @returns iid the individual id number of this food item.
  elemental function food_item_get_iid(this) result(iid)
    class(FOOD_ITEM), intent(in) :: this

    ! @returns iid the individual id number of this food item.
    integer :: iid

    iid = this%food_iid

  end function food_item_get_iid

  !-----------------------------------------------------------------------------
  !> Make food resource object. This class standard constructor.
  !! @param label Label for this food resource object.
  !! @param abundance the number of food items in the resource.
  !! @param locations An array of `SPATIAL` locations of each food item.
  !! @param sizes An array of sizes of each food item.
  pure subroutine food_resource_make(this, label, abundance, locations, sizes)
    class(FOOD_RESOURCE), intent(inout) :: this

    ! @param label Label for this food resource
    character(len=*), intent(in) :: label

    ! @param abundance the number of food items in the resource
    integer, intent(in) :: abundance

    ! @param locations An array of `SPATIAL` locations of each food item
    type(SPATIAL), dimension(:), intent(in) :: locations

    ! @param sizes An array of sizes of each food item.
    real(SRP), dimension(:), intent(in) :: sizes

    ! Local counter
    integer :: i

    !> ### Implementation details ###
    !> First, we allocate the array of the food item objects with the size
    !! of `abundance` parameter.
    if (.not. allocated(this%food)) allocate(this%food(abundance))

    !> Set label from input data.
    this%food_label = label

    !> Set abundance from input data.
    this%number_food_items = abundance

    !> Create all food items in the resource,
    ! @note Note that `create` method is elemental function so we can
    !       create a whole array. This might be faster and can parallelizse.
    call this%food%create()

    !> Set locations and food item sizes from input data vectors.
    ! But first, initialise locations of all these food items by `create` to
    ! `MISSING` and sizes to the default `FOOD_ITEM_SIZE_DEFAULT`. This
    ! would guard against non-initialised objects if `locations` and `sizes`
    ! would guard happen to have different sizes.
    do concurrent ( i=1:size(this%food) )
      !call this%food(i)%create()   !> Non-elemental variant of `create`.
      call this%food(i)%make( locations(i), sizes(i), i )
    end do

  end subroutine food_resource_make

  !-----------------------------------------------------------------------------
  !> Get the number of food items in the food resource.
  elemental function food_resource_get_abundance(this) result (abundance_out)
    class(FOOD_RESOURCE), intent(in) :: this
    !> @return The number of food items in this food resource.
    integer :: abundance_out

    abundance_out = this%number_food_items

  end function food_resource_get_abundance

  !-----------------------------------------------------------------------------
  !> Get the label of the this food resource.
  elemental function food_resource_get_label(this) result (label_out)
    class(FOOD_RESOURCE), intent(in) :: this
    !> @return The label of this food resource.
    character(len=LABEL_LENGTH) :: label_out

    label_out = this%food_label

  end function food_resource_get_label

  !-----------------------------------------------------------------------------
  !> Delete and deallocate food resource object. This class standard destructor.
  pure subroutine food_resource_destroy_deallocate(this)
    class(FOOD_RESOURCE), intent(inout) :: this

    ! Deallocate the array of the food item objects.
    if (allocated(this%food)) deallocate(this%food)

    ! Set undefined label from input data.
    this%food_label = "undefined"

    ! Set unknown abundance.
    this%number_food_items = UNKNOWN

  end subroutine food_resource_destroy_deallocate

  !-----------------------------------------------------------------------------
  !> Get the location object array (array of `SPATIAL` objects) of a
  !! food resource object.
  !! @returns locate_array an array of `SPATIAL` location objects for all
  !!          food items within the resource.
  pure function food_resource_locate_3d(this) result (locate_array)
    class(FOOD_RESOURCE), intent(in) :: this

    ! @returns locate_array an array of `SPATIAL` location objects for all
    !          food items within the resource.
    type(SPATIAL), dimension(size(this%food)) :: locate_array

    ! Local counter
    integer :: i

    ! @warning The `do concurrent` construct is F2008 and can not (yet) be
    !          implemented in all compilers. Use normal `do` in such a case.
    do concurrent ( i=1:size(this%food) )
      locate_array(i) = this%food(i)%location()
    end do

  end function food_resource_locate_3d

  !-----------------------------------------------------------------------------
  !> Calculate the average distance between food items within a resource.
  !! e.g. to compare it with the agent's random walk step size.
  function food_resource_calc_average_distance_items(this, n_sample)          &
                                            result (average_distance_food_items)
    class(FOOD_RESOURCE), intent(in) :: this
    integer, optional, intent(in) :: n_sample
    real(SRP) :: average_distance_food_items

    ! Local copies of optionals
    integer :: n_sample_here

    ! Default sample size if not provided
    integer, parameter :: N_SAMPLE_DEFAULT = 100

    if (present(n_sample)) then
      n_sample_here = n_sample
    else
      n_sample_here = N_SAMPLE_DEFAULT
    end if

    average_distance_food_items = distance_average(this%food, n_sample_here)

  end function food_resource_calc_average_distance_items

  !-----------------------------------------------------------------------------
  !> Replenish and restore food resource: the food resource is restored to its
  !! initial state or another "zero" state with a **smaller** food abundance
  !! as set by the `max_number` parameter. However, unlike the
  !! the_environment::food_resource::make() method, the sizes and the
  !! positions of the food items within the resource are reused from the
  !! previous positions (previously explicitly set set by the
  !! the_environment::food_resource::make() method). Thus, max_number cannot
  !! increase beyond the previous original position. If `max_number` exceeds
  !! the previous number of food items, it is initialised to the previous value.
  !! @warning This method cannot be used to build the food resource for the
  !!          first time (init). Use the_environment::food_resource::make() to
  !!          do this instead. This method is only for *modifying* the food
  !!          resource object.
  subroutine food_resource_replenish_food_items_all(this, max_number)
    class(FOOD_RESOURCE), intent(inout) :: this
    !> @param[in] max_number Optional number of food items in the re-initialised
    !!            food resource, if absent, does not change from the previous
    !!            initialised value.
    integer, optional, intent(inout) :: max_number

    ! Local copies of optionals
    integer :: max_number_loc

    ! Local index array for random selection of food items
    integer, dimension(size(this%food)) :: indx_rnd

    !> ### Implementation notes ###
    !> First, check optional `max_number` parameter. If it is present check
    !! whether it exceeds the initial value and re-set the minimum. If not
    !! present, reset `max_number` equal to the original for returning back.
    if (present(max_number)) then
      if ( max_number > this%number_food_items ) then
        call LOG_MSG( LTAG_WARN // "Reinitialising food resource "  //        &
                      this%food_label // " to the initial " //                &
                      TOSTR(this%number_food_items) //                        &
                      " instead of "//TOSTR(max_number) // " that is larger." )
        max_number = this%number_food_items
      end if
      max_number_loc = max_number
    else
      max_number_loc = this%number_food_items
    end if

    !> Check if the size of the food resource is about to be changed
    !! (reduced).
    !! - If yes, deallocate the food resource array and
    if (max_number_loc /= this%number_food_items) then
      if (allocated(this%food)) deallocate(this%food)
      !>   re-build the new food resource using the
      !!   the_environment::food_resource::make() method. But note that
      !!   the locations and sizes of the food items are randomly drawn from
      !!   the previous (larger) food resource.
      indx_rnd = PERMUTE_RANDOM(this%number_food_items)
      call this%make(                                                         &
                  label=this%food_label,                                      &
                  abundance=max_number_loc,                                   &
                  locations=this%food(indx_rnd(1:max_number_loc))%location(), &
                  sizes=this%food(indx_rnd(1:max_number_loc))%size  )
    !> - If the size of the food resource does not change, just reset the
    !!   the_environment::food_resource::food::eaten indicator of each food
    !!   item to FALSE. Thus, the food resource is just reset to a fully
    !!   available state. This allows to avoid allocation operations on big
    !!   arrays.
    !! .
    !! @note Note that the food items within the food resource retain their
    !!       sizes and positions unchanged. If fully stochastic resource
    !!       is required, use the the_environment::food_resource::make()
    !!       method.
    else
      this%food%eaten = .FALSE.
    end if

  end subroutine food_resource_replenish_food_items_all

  !-----------------------------------------------------------------------------
  !> This subroutine implements the migration of all the food items in the
  !! resource according to the plankton migration pattern from the G1 model
  !! (HED18). Briefly, the movement of each of the food items has two
  !! components:
  !! - deterministic: regular vertical migration movement;
  !! - stochastic: small scale random Gaussian displacement.
  !! .
  !!
  !! #### Global habitats array ####
  !! If the habitats are assembled into the global array
  !! the_environment::global_habitats_available and the migration is done
  !! on the original habitat-bound food resource objects, these original
  !! habitat objects and the global array require constant synchronisation
  !! with the the_environment::assemble() and the_environment::disassemble()
  !! procedures. Here is an example:
  !! @code
  !! call disassemble( habitat_test1, habitat_test2 )
  !! call habitat_test1%food%migrate_vertical()
  !! call habitat_test2%food%migrate_vertical()
  !! call assemble ( habitat_test1, habitat_test2 )
  !! @endcode
  !!
  !! To avoid this, a separate procedure that works directly on an *array* of
  !! habitat objects is implemented: the_environment::migrate_food_vertical().
  !! Its use is more efficient:
  !! @code
  !! call migrate_food_vertical( Global_Habitats_Available )
  !! @endcode
  subroutine food_resource_migrate_move_items(this, max_depth, time_step_model)
    class(FOOD_RESOURCE), intent(inout) :: this
    !> @param[in] max_depth optional maximum depth of deterministic vertical
    !!            migration; if this parameter is absent, the maximum depth
    !!            in the habitat container for this food resource is used
    real(SRP), optional, intent(in) :: max_depth
    !> @param[in] time_step_model Optional time step of the model. If absent,
    !!            the time step is obtained from the global array
    !!            commondata::global_habitats_available.
    integer, optional, intent(in) :: time_step_model

    ! Local copies of optionals
    real(SRP) :: depth_loc
    integer :: tstep_loc

    ! Fixed habitat number in the global array
    integer :: habitat_res

    ! Local counter for food items.
    integer ifood

    ! The new position for the food item that involves only vertical shift.
    type(SPATIAL) :: position_new

    real(SRP) :: target_depth

    !> ### Implementation notes ###
    !> #### Checks and preparations ####
    !> First, the procedure checks if the maximum depth for the vertical
    !! migration of food items is provided. If not, the maximum depth is
    !! determined as the maximum depth in the habitat of a single randomly
    !! chosen food item in this resource (see
    !! the_environment::spatial::find_environment()). Because a single food
    !! resource is nested within a specific habitat, all items are located
    !! in this environment, so the maximum depth is the same for all items of
    !! the resource.
    ifood = RAND_I(1, this%number_food_items)
    !> Thus, `habitat_res` keeps the number of the habitat in the global array
    !! the_environment::global_habitats_available
    habitat_res = this%food(ifood)%find_environment()
    if (present(max_depth)) then
      depth_loc = max_depth
    else
      depth_loc = Global_Habitats_Available(habitat_res)%depth_max()
    end if

    !> Second, a check is done if the time step is provided. If no, the global
    !! time step from commondata::global_time_step_model_current is used.
    if (present(time_step_model)) then
      tstep_loc = time_step_model
    else
      tstep_loc = Global_Time_Step_Model_Current
    end if

    !> #### Deterministic move ####
    !> The depth centre of the food item vertical distribution is determined
    !! using the code from the HED18 model with minimum adaptations.
    !! This code is isolated into the the_environment::center_depth_sinusoidal()
    !! function.
    target_depth = center_depth_sinusoidal(tstep_loc, depth_loc)

    !> Once the target depth for the food items is known, all the food
    !! resource is moved up or down to the target depth.
    do concurrent (ifood=1:this%number_food_items)
      position_new = SPATIAL( this%food(ifood)%xpos(),                        &
                              this%food(ifood)%ypos(),                        &
                              target_depth  )
      call this%food(ifood)%position( position_new )
    end do

    !> #### Stochastic walk ####
    !> The second phase of the food item migration involves a stochastic
    !! random walk of each of the food items. This stochastic movement is
    !! described by the following parameters of the
    !! the_environment::spatial_moving::rwalk():
    !! - commondata::food_item_migrate_xy_mean,
    !! - commondata::food_item_migrate_xy_cv ,
    !! - commondata::food_item_migrate_depth_mean,
    !! - commondata::food_item_migrate_depth_cv.
    !! - the limiting habitat for random walk is defined by the `habitat_res`
    !!   element of the global array the_environment::global_habitats_available
    !!   the same for all items as food resource is nested within the habitat.
    !! .
    !! @note Note that the stochastic random walk coincides with the default
    !!       the_environment::food_resource::rwalk() method. The code is
    !!       retained independently here because the parameters might not
    !!       coincide with the default walk. Otherwise use call the default
    !!        @code
    !!         call this%rwalk()
    !!        @endcode
    do ifood=1, this%number_food_items
      call this%food(ifood)%rwalk( meanshift_xy = FOOD_ITEM_MIGRATE_XY_MEAN,  &
                            cv_shift_xy  = FOOD_ITEM_MIGRATE_XY_CV,           &
                            meanshift_depth = FOOD_ITEM_MIGRATE_DEPTH_MEAN,   &
                            cv_shift_depth = FOOD_ITEM_MIGRATE_DEPTH_CV,      &
                            environment_limits =                              &
                              Global_Habitats_Available(habitat_res) )
    end do

  end subroutine food_resource_migrate_move_items

  !-----------------------------------------------------------------------------
  !> Perform a random walk step for all food items within the food resource.
  !! The walk is performed with the default parameters:
  !! - commondata::food_item_migrate_xy_mean,
  !! - commondata::food_item_migrate_xy_cv ,
  !! - commondata::food_item_migrate_depth_mean,
  !! - commondata::food_item_migrate_depth_cv.
  !! .
  subroutine food_resource_rwalk_items_default(this)
    class(FOOD_RESOURCE), intent(inout) :: this

    ! Fixed habitat number in the global array
    ! the_environment::global_habitats_available
    integer :: ifood, habitat_res

    !> ### Implementation notes ###
    !> First, the habitat number within the global habitats array
    !! the_environment::global_habitats_available is determined for a
    !! single randomly chosen food item within this resource. Because
    !! the food resource is bound to the habitat, this identifies the
    !! habitat.
    ifood = RAND_I(1, this%number_food_items)
    habitat_res = this%food(ifood)%find_environment()

    !> Then, all food items within the resource are subjected to Gaussian
    !! random walk the_environment::spatial_moving::rwalk() with the
    !! following parameters:
    !! - commondata::food_item_migrate_xy_mean,
    !! - commondata::food_item_migrate_xy_cv ,
    !! - commondata::food_item_migrate_depth_mean,
    !! - commondata::food_item_migrate_depth_cv.
    !! - the limiting habitat for random walk is defined by the `habitat_res`
    !!   element of the global array the_environment::global_habitats_available
    !!   the same for all items as food resource is nested within the habitat.
    !! .
    do ifood=1, this%number_food_items
      call this%food(ifood)%rwalk( meanshift_xy = FOOD_ITEM_MIGRATE_XY_MEAN,  &
                            cv_shift_xy  = FOOD_ITEM_MIGRATE_XY_CV,           &
                            meanshift_depth = FOOD_ITEM_MIGRATE_DEPTH_MEAN,   &
                            cv_shift_depth = FOOD_ITEM_MIGRATE_DEPTH_CV,      &
                            environment_limits =                              &
                              Global_Habitats_Available(habitat_res) )
    end do

  end subroutine food_resource_rwalk_items_default

  !-----------------------------------------------------------------------------
  !> Migrate food items in a whole array of food resources. The array is
  !! normally the the_environment::global_habitats_available.
  !! @code
  !! call migrate_food_vertical( Global_Habitats_Available )
  !! @endcode
  !! @note This is a not type-bound procedure.
  !!
  !! #### Global habitats array ####
  !! All the habitat objects are normally assembled into the global array
  !! the_environment::global_habitats_available.  If so, this procedure makes
  !! it unnecessary to synchronise the habitat objects with the global array
  !! constantly by calling the_environment::habitat::disassemble() and
  !! the_environment::habitat::assemble() procedures whenever the habitat
  !! objects are changed (foods migrated). It makes the migration change
  !! directly on the *global array*.
  !!
  !! The the_environment::food_resource::migrate_vertical() method that
  !! operates directly on *food resource object* in such case would require
  !! constant synchronisation/update between the global array and each
  !! habitat-bound food resource object, e.g.:
  !! @code
  !! call disassemble( habitat_test1, habitat_test2 )
  !! call habitat_test1%food%migrate_vertical()
  !! call habitat_test2%food%migrate_vertical()
  !! call assemble ( habitat_test1, habitat_test2 )
  !! @endcode
  subroutine migrate_food_vertical(habitats,  time_step_model)
    !> @param[inout] habitats is an array of habitats
    class(HABITAT), dimension(:), intent(inout) :: habitats
    !> @param[in] time_step_model Optional time step of the model. If absent,
    !!            the time step is obtained from the global array
    !!            commondata::global_time_step_model_current.
    integer, optional, intent(in) :: time_step_model

    ! Local copies of optionals
    integer :: tstep_loc

    integer :: i

    !> ### Implementation notes ###
    !> A check is done if the time step is provided. If no, the global
    !! time step from commondata::global_time_step_model_current is used.
    if (present(time_step_model)) then
      tstep_loc = time_step_model
    else
      tstep_loc = Global_Time_Step_Model_Current
    end if

    !> Then food resources within each of the habitats within the `habitats`
    !! array is subjected to the
    !! the_environment::food_resource::migrate_vertical() method.
    do i=1, size(habitats)
      call habitats(i)%food%migrate_vertical(time_step_model = tstep_loc)
    end do

  end subroutine migrate_food_vertical

  !-----------------------------------------------------------------------------
  !> Perform a random walk of food items in a whole array of food resources.
  !! The array is normally the the_environment::global_habitats_available.
  !! This procedure is a wrapper for the_environment::food_resource::rwalk()
  !! to do a walk on a whole array of habitats and linked food resources.
  !! @code
  !! call rwalk_food_step( Global_Habitats_Available )
  !! @endcode
  !! @note This is a not type-bound procedure.
  subroutine rwalk_food_step(habitats)
    !> @param[inout] habitats is an array of habitats
    class(HABITAT), dimension(:), intent(inout) :: habitats

    integer :: i

    !> ### Implementation notes ###
    !> All the food resources within each of the habitats within the `habitats`
    !! array is subjected to the
    !! the_environment::food_resource::rwalk() method.
    do i=1, size(habitats)
      call habitats(i)%food%rwalk()
    end do

  end subroutine rwalk_food_step

  !-----------------------------------------------------------------------------
  !> This function calculates the target depth for the sinusoidal vertical
  !! migration pattern of the food items at each time step of the model.
  !! See the_environment::food_resource_migrate_move_items() for the calling
  !! procedure.
  !! @note  The depth centre of the food item vertical distribution
  !!        is determined using the code from the HED18 model with
  !!        minimum adaptations. This function isolates this HED18 code.
  !!        The pattern coincides with the COPDVM = 1 in the HED18 model.
  !!        Different pattern(s) is easy to add in separate procedures.
  !! @note  This procedure is isolated into a separate one, so
  !!        diagnostic data/plots can be saved.
  function center_depth_sinusoidal(tstep, depth) result (copcenterdepth)
    !> @param[in] tstep time step of the model.
    integer, intent(in) :: tstep
    !> @param[in] depth sets the maximum target depth for vertical migration.
    real(SRP), intent(in) :: depth
    !> @returns The target depth for the regular migration of the food
    !!          items.
    real(SRP) :: copcenterdepth

    ! Local variable.
    real(SRP) :: copsindepth

    !> Verbatim code from HED18:
    !! @code
    !! do a = 1, flifespan
    !!   !find center of copepod distribution
    !!   copsindepth = sin(3.14*a*2.*dielcycles/(flifespan+1.))
    !!   copcenterdepth = int(depth/2 + 0.33*depth*copsindepth)
    !! @endcode
    copsindepth = sin( PI * real(tstep,srp) * 2.0_SRP *                       &
                       real(DIELCYCLES, SRP)/( real(LIFESPAN, SRP) + 1.0_SRP) )
    copcenterdepth = depth / 2.0_SRP + 0.499_SRP * depth*copsindepth

  end function center_depth_sinusoidal

  !-----------------------------------------------------------------------------
  !> Save characteristics of food items in the resource into a CSV file.
  subroutine food_resource_save_foods_csv(this, csv_file_name, is_success)
    class(FOOD_RESOURCE), intent(in) :: this
    character(len=*), optional, intent(in) :: csv_file_name
    logical, optional, intent(out) :: is_success

    ! Local copies of optionals.
    character(len=FILENAME_LENGTH) :: csv_file_name_here
    logical :: is_success_write

    !> ### Implementation notes ###
    !! First, check if the optional CSV file name is provided, if not,
    !! generate it automatically.
    if (present(csv_file_name)) then
      csv_file_name_here = csv_file_name
    else
      csv_file_name_here = "food_items_" // trim(this%food_label) // "_" //   &
                             MODEL_NAME // "_" // MMDD // "_gen_" //          &
                             TOSTR(Global_Generation_Number_Current,          &
                                   GENERATIONS) // csv
    end if

    !> Then, data for the food resource is saved using the
    !! [CSV_MATRIX_WRITE()](http://ahamodel.uib.no/doc/ar01s08.html#_subroutine_csv_matrix_write)
    !! procedure from [HEDTOOLS](http://ahamodel.uib.no/doc/).
    is_success_write = .FALSE.
    call CSV_MATRIX_WRITE ( reshape(                                          &
                            [ this%food%x,                                    &
                              this%food%y,                                    &
                              this%food%depth,                                &
                              this%food%size,                                 &
                              size2mass_food(this%food%size),                 &
                              conv_l2r(this%food%eaten),                      &
                              real(this%food%food_iid,SRP) ],                 &
                            [this%number_food_items, 7]),                     &
                            csv_file_name_here,                               &
                            [ character(len=LABEL_LENGTH) ::                  &
                                "X","Y", "DPTH", "SIZE",                      &
                                "MASS", "EATEN", "IID"],                      &
                            is_success_write  )

    if (present(is_success)) is_success = is_success_write

    !> The CSV output data file can be optionally compressed with the
    !! commondata::cmd_zip_output command if commondata::is_zip_outputs is set
    !! to TRUE.
    if ( IS_ZIP_OUTPUTS ) then
      call call_external(command=CMD_ZIP_OUTPUT // " " // csv_file_name_here, &
                         suppress_output=.TRUE.,                              &
                         is_background_task=ZIP_OUTPUTS_BACKGROUND )
    end if

  end subroutine food_resource_save_foods_csv

  !-----------------------------------------------------------------------------
  !> Sort the food resource objects within the array by their sizes.
  !! The two subroutines below are a variant of the recursive quick-sort
  !! algorithm adapted for sorting real components of the the `FOOD_RESOURCE`
  !! object.
  !! @param[in] reindex is a logical flag enabling re-indexing the
  !!            food resource after it is sorted. The default is **NOT**
  !!            to reindex.
  elemental subroutine food_resource_sort_by_size(this, reindex)
    class(FOOD_RESOURCE), intent(inout) :: this
    ! @param[in] reindex is a logical flag enabling re-indexing the
    !            food resource after it is sorted. The default is **NOT**
    !            to reindex.
    logical, optional, intent(in) :: reindex

    call qsort(this%food) ! This is the array component we sort, work on array.

    ! @note Note that re-indexing of food resource after sorting is NOT
    !       enabled by default.
    if (present(reindex)) then
      if (reindex) call this%reindex()
    end if

    contains

    !...........................................................................
    !> `qsort` and `qs_partition_` are the two parts of the recursive sort
    !! algorithm `qsort` is the recursive frontend. Sorts an array of food
    !! items by size within the resource object.
    !! @param `A` has the same type as the individual component objects
    !!         of the array-object that we are going to sort.
    recursive pure subroutine qsort(A)

      ! @param `A` has the same type as the individual component objects
      !         of the array-object that we are going to sort.
      type(FOOD_ITEM), intent(in out), dimension(:) :: A
      integer :: iq

      if(size(A) > 1) then
        call qs_partition_size(A, iq)
        call qsort(A(:iq-1))
        call qsort(A(iq:))
      endif

    end subroutine qsort

    !...........................................................................
    !> `qsort` and `qs_partition_` are the two parts of the recursive sort
    !! algorithm `qs_partition_size` is a pivot backend, here it sorts
    !! food items within the food resource object by real size components.
    pure subroutine qs_partition_size(A, marker)

      type(FOOD_ITEM), intent(inout), dimension(:) :: A
      integer, intent(out) :: marker
      integer :: i, j
      type(FOOD_ITEM) :: temp
      ! @note Pivot point `x`, has the same type **as
      !       the sorted object component**.
      real(SRP) :: x

      ! we sort `FOOD_ITEM` objects within the `FOOD_RESOURCE by their
      ! `size` components (hardwired).
      x = A(1)%size
      i= 0
      j= size(A) + 1

      do
        j = j-1
        do
            if (A(j)%size <= x) exit
            j = j-1
        end do
        i = i+1
        do
            if (A(i)%size >= x) exit
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

    end subroutine qs_partition_size

  end subroutine food_resource_sort_by_size

  !-----------------------------------------------------------------------------
  !> Reset individual iid for the food resource. Individual iids must normally
  !! coincide with the array order index. If it is changed after sorting,
  !! iids no longer reflect the correct index. So this subroutine resets iids
  !! to be coinciding with each food item index.
  !! @param start_iid is an optional parameters that sets the starting value
  !!        for reindexing. For example, indexing from 1000 rather than 1.
  !! @warning Always reindex food resource after food items have been sorted!
  pure subroutine food_resource_reset_iid_all(this, start_iid)
    class(FOOD_RESOURCE), intent(inout) :: this
    ! @param start_iid is an optional parameters that sets the starting value
    !        for reindexing. For example, indexing from 1000 rather than 1.
    integer, optional, intent(in) :: start_iid

    ! Local copies of optionals
    integer :: start_iid_loc

    ! Local variables
    integer :: i

    if (present(start_iid)) then
      start_iid_loc = start_iid
    else
      start_iid_loc = 1
    end if

    do concurrent(i=1:this%number_food_items)
      this%food(i)%food_iid = i + start_iid_loc - 1
      ! call this%food(i)%set_iid( i + start_iid_loc - 1 )
    end do

  end subroutine food_resource_reset_iid_all

  !-----------------------------------------------------------------------------
  !> Reset and reindex iids for an input list of several food resources. As
  !! the result of this subroutine all food items across all the resources
  !! within the whole list will have unique iids.
  !! @param[inout] resource_1, resource_2, ... a list of food resources to
  !!               reindex
  !! @note    The calculation does not use an array of food resources because
  !!          this can create problems in setting input dummy parameters in
  !!          the array constructor. It just accepts raw resource objects and
  !!          does all the operations directly on them. The number of food
  !!          resources is probably never big, so the hard-coded limit of
  !!          20 components would probably never be exceeded. But the object
  !!          list implementation is rather wordy, code-duplicating and prone
  !!          to editing bugs. **The main aim** of this wordy, code-dubbing and
  !!          mistype-prone approach was to allow easy passage of the whole
  !!          original resource objects back from the collapsed object
  !!          retaining all the changes that were introduced (e.g. the `eaten`
  !!          status) while the resource objects were processed as the joined
  !!          super-object.
  !!          TODO: Perhaps it could be reimplemented in a better style using
  !!          an extension object type.
  !! @warning Note that this is not a type-bound subroutine. It should not
  !!          be declared in the `FOOD_RESOURCE` type.
  !> @note    See notes on `food_resources_collapse()`,
  !!          `food_resources_update_back()` and `reindex_food_resources()`.
  subroutine reindex_food_resources(                  resource_1,             &
                                                      resource_2,             &
                                                      resource_3,             &
                                                      resource_4,             &
                                                      resource_5,             &
                                                      resource_6,             &
                                                      resource_7,             &
                                                      resource_8,             &
                                                      resource_9,             &
                                                      resource_10,            &
                                                      resource_11,            &
                                                      resource_12,            &
                                                      resource_13,            &
                                                      resource_14,            &
                                                      resource_15,            &
                                                      resource_16,            &
                                                      resource_17,            &
                                                      resource_18,            &
                                                      resource_19,            &
                                                      resource_20   )

    class(FOOD_RESOURCE), optional, intent(inout) ::  resource_1,             &
                                                      resource_2,             &
                                                      resource_3,             &
                                                      resource_4,             &
                                                      resource_5,             &
                                                      resource_6,             &
                                                      resource_7,             &
                                                      resource_8,             &
                                                      resource_9,             &
                                                      resource_10,            &
                                                      resource_11,            &
                                                      resource_12,            &
                                                      resource_13,            &
                                                      resource_14,            &
                                                      resource_15,            &
                                                      resource_16,            &
                                                      resource_17,            &
                                                      resource_18,            &
                                                      resource_19,            &
                                                      resource_20

    integer, parameter :: n_resources = 4

    integer :: start_value_resource

    start_value_resource = 1

    R1: if (present(resource_1)) then
      ! Reindex the i-th resorce using the `start_value_resource`
      call resource_1%reindex(start_value_resource)
      ! Update the starting index for the **next** resource component array.
      start_value_resource = resource_1%number_food_items + 1
    end if R1

    R2: if (present(resource_2)) then
      call resource_2%reindex(start_value_resource)
      start_value_resource = resource_2%number_food_items + 1
    end if R2

    R3: if (present(resource_3)) then
      call resource_3%reindex(start_value_resource)
      start_value_resource = resource_3%number_food_items + 1
    end if R3

    R4: if (present(resource_4)) then
      call resource_4%reindex(start_value_resource)
      start_value_resource = resource_4%number_food_items + 1
    end if R4

    R5: if (present(resource_5)) then
      call resource_5%reindex(start_value_resource)
      start_value_resource = resource_5%number_food_items + 1
    end if R5

    R6: if (present(resource_6)) then
      call resource_6%reindex(start_value_resource)
      start_value_resource = resource_6%number_food_items + 1
    end if R6

    R7: if (present(resource_7)) then
      call resource_7%reindex(start_value_resource)
      start_value_resource = resource_7%number_food_items + 1
    end if R7

    R8: if (present(resource_8)) then
      call resource_8%reindex(start_value_resource)
      start_value_resource = resource_8%number_food_items + 1
    end if R8

    R9: if (present(resource_9)) then
      call resource_9%reindex(start_value_resource)
      start_value_resource = resource_9%number_food_items + 1
    end if R9

    R10: if (present(resource_10)) then
      call resource_10%reindex(start_value_resource)
      start_value_resource = resource_10%number_food_items + 1
    end if R10

    R11: if (present(resource_11)) then
      call resource_11%reindex(start_value_resource)
      start_value_resource = resource_11%number_food_items + 1
    end if R11

    R12: if (present(resource_12)) then
      call resource_12%reindex(start_value_resource)
      start_value_resource = resource_12%number_food_items + 1
    end if R12

    R13: if (present(resource_13)) then
      call resource_13%reindex(start_value_resource)
      start_value_resource = resource_13%number_food_items + 1
    end if R13

    R14: if (present(resource_14)) then
      call resource_14%reindex(start_value_resource)
      start_value_resource = resource_14%number_food_items + 1
    end if R14

    R15: if (present(resource_15)) then
      call resource_15%reindex(start_value_resource)
      start_value_resource = resource_15%number_food_items + 1
    end if R15

    R16: if (present(resource_16)) then
      call resource_16%reindex(start_value_resource)
      start_value_resource = resource_16%number_food_items + 1
    end if R16

    R17: if (present(resource_17)) then
      call resource_17%reindex(start_value_resource)
      start_value_resource = resource_17%number_food_items + 1
    end if R17

    R18: if (present(resource_18)) then
      call resource_18%reindex(start_value_resource)
      start_value_resource = resource_18%number_food_items + 1
    end if R18

    R19: if (present(resource_19)) then
      call resource_19%reindex(start_value_resource)
      start_value_resource = resource_19%number_food_items + 1
    end if R19

    R20: if (present(resource_20)) then
      call resource_20%reindex(start_value_resource)
      start_value_resource = resource_20%number_food_items + 1
    end if R20

  end subroutine reindex_food_resources

  !-----------------------------------------------------------------------------
  !> Collapse several food resources into one. The collapsed resource can then
  !! go into the perception system. The properties of the component resources
  !! are retained in the collapsed resource.
  !! @param[out] food_resource_collapsed  output resource object that is
  !!          formed by joining the list of component resource objects.
  !! @param[in] resource_1, resource_2, ... a list of component resource
  !!            objects.
  !! @param[in] reindex logical flag to reindex the joined resource (TRUE)
  !!            upon joining. The default is **no** reindexing.
  !! @param[in] label Label for the joined food resource, if absent set to
  !!            'tmp_object'.
  !>
  !> ### Implementation notes ###
  !! The calculations in this procedure does not use an array of food
  !! resources because this can create problems in setting input dummy
  !! parameters in the array constructor. It just accepts raw resource
  !! objects and does all the operations directly on them. The number of
  !! food resources is probably never big, so the hard-coded limit of
  !! 20 components would probably never be exceeded. But the object
  !! list implementation is rather wordy, code-duplicating and prone
  !! to editing bugs. **The main aim** of this wordy, code-dubbing and
  !! mistype-prone approach was to allow easy passage of the whole
  !! original resource objects back from the collapsed object
  !! retaining all the changes that were introduced (e.g. the `eaten`
  !! status) while the resource objects were processed as the joined
  !! super-object. TODO: Perhaps it could be reimplemented in a better
  !! style using an extension object type.
  !> The joined food resource object retains the original component
  !! ID's. If common unique iids are needed, `reindex` method should
  !! be called upon joining! This is performed now using the optional
  !! 'reindex' parameter.
  !! If the food items are re-sorted within the joined food resource,
  !! their iid's no longer correspond to the original id's. Therefore,
  !! unjoining will result in totally new order and id's and the
  !! stacking of the original component resources is **broken**:
  !! correct unjoining is then impossible.
  !> ### Usage example ###
  !! This example shows how to use the `join` and `unjoin` methods
  !! to collapse and split back food resources.
  !! @code
  !!   ! Join two resources into `joined_food_res_tmp`
  !!   call joined_food_res_tmp%join( habitat_safe%food,                  &
  !!                                  habitat_dangerous%food,             &
  !!                                  reindex=.TRUE. )
  !!   ! We can then work with the collapsed resource, e.g. get
  !!   ! the perception and use the "eat" method.
  !!   call proto_parents%individual(ind)%see_food(joined_food_res_tmp)
  !!   call proto_parents%individual(ind)%do_eat_food_item(               &
  !!                           food_item_selected, joined_food_res_tmp)
  !!   ! after this we use the "unjoin" method to get back the
  !!   ! original component food resources, they are now in an updated
  !!   ! state, e.g. the "eaten" flag is transferred from the collapsed
  !!   ! food resource object back to the component objects.
  !!   call joined_food_res_tmp%unjoin( habitat_safe%food,                &
  !!                                    habitat_dangerous%food,           &
  !!                                    reindex=.TRUE. )
  !!   ! destroy the temporary collapsed resource object.
  !!   call joined_food_res_tmp%destroy()
  !! @endcode
  !> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !> @note See notes on `food_resources_collapse()`,
  !!       `food_resources_update_back()` and `reindex_food_resources()`.
  subroutine food_resources_collapse (  food_resource_collapsed,              &
                                        resource_1,                           &
                                        resource_2,                           &
                                        resource_3,                           &
                                        resource_4,                           &
                                        resource_5,                           &
                                        resource_6,                           &
                                        resource_7,                           &
                                        resource_8,                           &
                                        resource_9,                           &
                                        resource_10,                          &
                                        resource_11,                          &
                                        resource_12,                          &
                                        resource_13,                          &
                                        resource_14,                          &
                                        resource_15,                          &
                                        resource_16,                          &
                                        resource_17,                          &
                                        resource_18,                          &
                                        resource_19,                          &
                                        resource_20,                          &
                                        reindex,                              &
                                        label          )

    ! @note Note that `food_resource_collapsed` is actually the `this` object
    !       in the type-bound procedure. It has been initially defined in
    !       a previous non-type-bound version as **type** to enable strict
    !       typing. But this would not work with type-bound, so had to
    !       declare as a polymorphic `class`. However, it should normally
    !       only accept the  `FOOD_RESOURCE` type objects.
    ! TODO: check if food_resource_collapsed should be safer with intent inout.
    class(FOOD_RESOURCE), intent(out) ::            food_resource_collapsed

    class(FOOD_RESOURCE), optional, intent(in) ::   resource_1,               &
                                                    resource_2,               &
                                                    resource_3,               &
                                                    resource_4,               &
                                                    resource_5,               &
                                                    resource_6,               &
                                                    resource_7,               &
                                                    resource_8,               &
                                                    resource_9,               &
                                                    resource_10,              &
                                                    resource_11,              &
                                                    resource_12,              &
                                                    resource_13,              &
                                                    resource_14,              &
                                                    resource_15,              &
                                                    resource_16,              &
                                                    resource_17,              &
                                                    resource_18,              &
                                                    resource_19,              &
                                                    resource_20

  ! @param[in] reindex logical flag to reindex the joined resource (TRUE)
  !            upon joining. The default is **no** reindexing.
  logical, optional, intent(in) :: reindex
  ! @param[in] label Label for the joined food resource, if absent set to
  !            'tmp_object'.
  character(len=*), optional, intent(in) :: label

  ! Local total abundance of the collapsed food resource object.
  integer :: abundance_total

  ! Local object parts/components for each of the component resources.
  type(SPATIAL), dimension(:), allocatable ::                                 &
                    locations_res_1, locations_res_2, locations_res_3,        &
                    locations_res_4, locations_res_5, locations_res_6,        &
                    locations_res_7, locations_res_8, locations_res_9,        &
                    locations_res_10, locations_res_11, locations_res_12,     &
                    locations_res_13, locations_res_14, locations_res_15,     &
                    locations_res_16, locations_res_17, locations_res_18,     &
                    locations_res_19, locations_res_20

  real(SRP), dimension(:), allocatable ::                                     &
          sizes_res_1, sizes_res_2, sizes_res_3, sizes_res_4,                 &
          sizes_res_5, sizes_res_6, sizes_res_7, sizes_res_8,                 &
          sizes_res_9, sizes_res_10, sizes_res_11, sizes_res_12,              &
          sizes_res_13, sizes_res_14, sizes_res_15, sizes_res_16,             &
          sizes_res_17, sizes_res_18, sizes_res_19, sizes_res_20

  logical, dimension(:), allocatable  ::                                      &
          eaten_res_1, eaten_res_2, eaten_res_3, eaten_res_4,                 &
          eaten_res_5, eaten_res_6, eaten_res_7, eaten_res_8,                 &
          eaten_res_9, eaten_res_10, eaten_res_11, eaten_res_12,              &
          eaten_res_13, eaten_res_14, eaten_res_15, eaten_res_16,             &
          eaten_res_17, eaten_res_18, eaten_res_19, eaten_res_20

  integer, dimension(:), allocatable ::                                       &
          old_iid_res_1, old_iid_res_2, old_iid_res_3, old_iid_res_4,         &
          old_iid_res_5, old_iid_res_6, old_iid_res_7, old_iid_res_8,         &
          old_iid_res_9, old_iid_res_10, old_iid_res_11, old_iid_res_12,      &
          old_iid_res_13, old_iid_res_14, old_iid_res_15, old_iid_res_16,     &
          old_iid_res_17, old_iid_res_18, old_iid_res_19, old_iid_res_20

  ! Local copy of optional label
  character(len=LABEL_LENGTH) :: label_loc

  abundance_total = 0

  if (present(label)) then
    label_loc = label
  else
    label_loc = "tmp_object"
  end if

  !> ### Implementation details ###
  !> For each food resource that is provided in the list we take the
  !! temporary transfer arrays for parameter copying from the component
  !! food resource into the `food_resource_collapsed`.
  ! @warning The `associate` construct to hide `resource_XXX` and components
  !          would **not** work with allocatable, had to work without assign
  !          shortcuts.
  if (present(resource_1)) then
    abundance_total = abundance_total + resource_1%number_food_items
    allocate(locations_res_1(resource_1%number_food_items))
    locations_res_1 = resource_1%location()
    allocate(sizes_res_1(resource_1%number_food_items))
    sizes_res_1 = resource_1%food%size
    allocate(eaten_res_1(resource_1%number_food_items))
    eaten_res_1 = resource_1%food%eaten
    allocate(old_iid_res_1(resource_1%number_food_items))
    old_iid_res_1 = resource_1%food%food_iid
  else
    allocate(locations_res_1(0))
    allocate(sizes_res_1(0))
    allocate(eaten_res_1(0))
    allocate(old_iid_res_1(0))
  end if

  if (present(resource_2)) then
    abundance_total = abundance_total + resource_2%number_food_items
    allocate(locations_res_2(resource_2%number_food_items))
    locations_res_2 = resource_2%location()
    allocate(sizes_res_2(resource_2%number_food_items))
    sizes_res_2 = resource_2%food%size
    allocate(eaten_res_2(resource_2%number_food_items))
    eaten_res_2 = resource_2%food%eaten
    allocate(old_iid_res_2(resource_2%number_food_items))
    old_iid_res_2 = resource_2%food%food_iid
  else
    allocate(locations_res_2(0))
    allocate(sizes_res_2(0))
    allocate(eaten_res_2(0))
    allocate(old_iid_res_2(0))
  end if

  if (present(resource_3)) then
    abundance_total = abundance_total + resource_3%number_food_items
    allocate(locations_res_3(resource_3%number_food_items))
    locations_res_3 = resource_3%location()
    allocate(sizes_res_3(resource_3%number_food_items))
    sizes_res_3 = resource_3%food%size
    allocate(eaten_res_3(resource_3%number_food_items))
    eaten_res_3 = resource_3%food%eaten
    allocate(old_iid_res_3(resource_3%number_food_items))
    old_iid_res_3 = resource_3%food%food_iid
  else
    allocate(locations_res_3(0))
    allocate(sizes_res_3(0))
    allocate(eaten_res_3(0))
    allocate(old_iid_res_3(0))
  end if

  if (present(resource_4)) then
    abundance_total = abundance_total + resource_4%number_food_items
    allocate(locations_res_4(resource_4%number_food_items))
    locations_res_4 = resource_4%location()
    allocate(sizes_res_4(resource_4%number_food_items))
    sizes_res_4 = resource_4%food%size
    allocate(eaten_res_4(resource_4%number_food_items))
    eaten_res_4 = resource_4%food%eaten
    allocate(old_iid_res_4(resource_4%number_food_items))
    old_iid_res_4 = resource_4%food%food_iid
  else
    allocate(locations_res_4(0))
    allocate(sizes_res_4(0))
    allocate(eaten_res_4(0))
    allocate(old_iid_res_4(0))
  end if

  if (present(resource_5)) then
    abundance_total = abundance_total + resource_5%number_food_items
    allocate(locations_res_5(resource_5%number_food_items))
    locations_res_5 = resource_5%location()
    allocate(sizes_res_5(resource_5%number_food_items))
    sizes_res_5 = resource_5%food%size
    allocate(eaten_res_5(resource_5%number_food_items))
    eaten_res_5 = resource_5%food%eaten
    allocate(old_iid_res_5(resource_5%number_food_items))
    old_iid_res_5 = resource_5%food%food_iid
  else
    allocate(locations_res_5(0))
    allocate(sizes_res_5(0))
    allocate(eaten_res_5(0))
    allocate(old_iid_res_5(0))
  end if

  if (present(resource_6)) then
    abundance_total = abundance_total + resource_6%number_food_items
    allocate(locations_res_6(resource_6%number_food_items))
    locations_res_6 = resource_6%location()
    allocate(sizes_res_6(resource_6%number_food_items))
    sizes_res_6 = resource_6%food%size
    allocate(eaten_res_6(resource_6%number_food_items))
    eaten_res_6 = resource_6%food%eaten
    allocate(old_iid_res_6(resource_6%number_food_items))
    old_iid_res_6 = resource_6%food%food_iid
  else
    allocate(locations_res_6(0))
    allocate(sizes_res_6(0))
    allocate(eaten_res_6(0))
    allocate(old_iid_res_6(0))
  end if

  if (present(resource_7)) then
    abundance_total = abundance_total + resource_7%number_food_items
    allocate(locations_res_7(resource_7%number_food_items))
    locations_res_7 = resource_7%location()
    allocate(sizes_res_7(resource_7%number_food_items))
    sizes_res_7 = resource_7%food%size
    allocate(eaten_res_7(resource_7%number_food_items))
    eaten_res_7 = resource_7%food%eaten
    allocate(old_iid_res_7(resource_7%number_food_items))
    old_iid_res_7 = resource_7%food%food_iid
  else
    allocate(locations_res_7(0))
    allocate(sizes_res_7(0))
    allocate(eaten_res_7(0))
    allocate(old_iid_res_7(0))
  end if

  if (present(resource_8)) then
    abundance_total = abundance_total + resource_8%number_food_items
    allocate(locations_res_8(resource_8%number_food_items))
    locations_res_8 = resource_8%location()
    allocate(sizes_res_8(resource_8%number_food_items))
    sizes_res_8 = resource_8%food%size
    allocate(eaten_res_8(resource_8%number_food_items))
    eaten_res_8 = resource_8%food%eaten
    allocate(old_iid_res_8(resource_8%number_food_items))
    old_iid_res_8 = resource_8%food%food_iid
  else
    allocate(locations_res_8(0))
    allocate(sizes_res_8(0))
    allocate(eaten_res_8(0))
    allocate(old_iid_res_8(0))
  end if

  if (present(resource_9)) then
    abundance_total = abundance_total + resource_9%number_food_items
    allocate(locations_res_9(resource_9%number_food_items))
    locations_res_9 = resource_9%location()
    allocate(sizes_res_9(resource_9%number_food_items))
    sizes_res_9 = resource_9%food%size
    allocate(eaten_res_9(resource_9%number_food_items))
    eaten_res_9 = resource_9%food%eaten
    allocate(old_iid_res_9(resource_9%number_food_items))
    old_iid_res_9 = resource_9%food%food_iid
  else
    allocate(locations_res_9(0))
    allocate(sizes_res_9(0))
    allocate(eaten_res_9(0))
    allocate(old_iid_res_9(0))
  end if

  if (present(resource_10)) then
    abundance_total = abundance_total + resource_10%number_food_items
    allocate(locations_res_10(resource_10%number_food_items))
    locations_res_10 = resource_10%location()
    allocate(sizes_res_10(resource_10%number_food_items))
    sizes_res_10 = resource_10%food%size
    allocate(eaten_res_10(resource_10%number_food_items))
    eaten_res_10 = resource_10%food%eaten
    allocate(old_iid_res_10(resource_10%number_food_items))
    old_iid_res_10 = resource_10%food%food_iid
  else
    allocate(locations_res_10(0))
    allocate(sizes_res_10(0))
    allocate(eaten_res_10(0))
    allocate(old_iid_res_10(0))
  end if

  if (present(resource_11)) then
    abundance_total = abundance_total + resource_11%number_food_items
    allocate(locations_res_11(resource_11%number_food_items))
    locations_res_11 = resource_11%location()
    allocate(sizes_res_11(resource_11%number_food_items))
    sizes_res_11 = resource_11%food%size
    allocate(eaten_res_11(resource_11%number_food_items))
    eaten_res_11 = resource_11%food%eaten
    allocate(old_iid_res_11(resource_11%number_food_items))
    old_iid_res_11 = resource_11%food%food_iid
  else
    allocate(locations_res_11(0))
    allocate(sizes_res_11(0))
    allocate(eaten_res_11(0))
    allocate(old_iid_res_11(0))
  end if

  if (present(resource_12)) then
    abundance_total = abundance_total + resource_12%number_food_items
    allocate(locations_res_12(resource_12%number_food_items))
    locations_res_12 = resource_12%location()
    allocate(sizes_res_12(resource_12%number_food_items))
    sizes_res_12 = resource_12%food%size
    allocate(eaten_res_12(resource_12%number_food_items))
    eaten_res_12 = resource_12%food%eaten
    allocate(old_iid_res_12(resource_12%number_food_items))
    old_iid_res_12 = resource_12%food%food_iid
  else
    allocate(locations_res_12(0))
    allocate(sizes_res_12(0))
    allocate(eaten_res_12(0))
    allocate(old_iid_res_12(0))
  end if

  if (present(resource_13)) then
    abundance_total = abundance_total + resource_13%number_food_items
    allocate(locations_res_13(resource_13%number_food_items))
    locations_res_13 = resource_13%location()
    allocate(sizes_res_13(resource_13%number_food_items))
    sizes_res_13 = resource_13%food%size
    allocate(eaten_res_13(resource_13%number_food_items))
    eaten_res_13 = resource_13%food%eaten
    allocate(old_iid_res_13(resource_13%number_food_items))
    old_iid_res_13 = resource_13%food%food_iid
  else
    allocate(locations_res_13(0))
    allocate(sizes_res_13(0))
    allocate(eaten_res_13(0))
    allocate(old_iid_res_13(0))
  end if

  if (present(resource_14)) then
    abundance_total = abundance_total + resource_14%number_food_items
    allocate(locations_res_14(resource_14%number_food_items))
    locations_res_14 = resource_14%location()
    allocate(sizes_res_14(resource_14%number_food_items))
    sizes_res_14 = resource_14%food%size
    allocate(eaten_res_14(resource_14%number_food_items))
    eaten_res_14 = resource_14%food%eaten
    allocate(old_iid_res_14(resource_14%number_food_items))
    old_iid_res_14 = resource_14%food%food_iid
  else
    allocate(locations_res_14(0))
    allocate(sizes_res_14(0))
    allocate(eaten_res_14(0))
    allocate(old_iid_res_14(0))
  end if

  if (present(resource_15)) then
    abundance_total = abundance_total + resource_15%number_food_items
    allocate(locations_res_15(resource_15%number_food_items))
    locations_res_15 = resource_15%location()
    allocate(sizes_res_15(resource_15%number_food_items))
    sizes_res_15 = resource_15%food%size
    allocate(eaten_res_15(resource_15%number_food_items))
    eaten_res_15 = resource_15%food%eaten
    allocate(old_iid_res_15(resource_15%number_food_items))
    old_iid_res_15 = resource_15%food%food_iid
  else
    allocate(locations_res_15(0))
    allocate(sizes_res_15(0))
    allocate(eaten_res_15(0))
    allocate(old_iid_res_15(0))
  end if

  if (present(resource_16)) then
    abundance_total = abundance_total + resource_16%number_food_items
    allocate(locations_res_16(resource_16%number_food_items))
    locations_res_16 = resource_16%location()
    allocate(sizes_res_16(resource_16%number_food_items))
    sizes_res_16 = resource_16%food%size
    allocate(eaten_res_16(resource_16%number_food_items))
    eaten_res_16 = resource_16%food%eaten
    allocate(old_iid_res_16(resource_16%number_food_items))
    old_iid_res_16 = resource_16%food%food_iid
  else
    allocate(locations_res_16(0))
    allocate(sizes_res_16(0))
    allocate(eaten_res_16(0))
    allocate(old_iid_res_16(0))
  end if

  if (present(resource_17)) then
    abundance_total = abundance_total + resource_17%number_food_items
    allocate(locations_res_17(resource_17%number_food_items))
    locations_res_17 = resource_17%location()
    allocate(sizes_res_17(resource_17%number_food_items))
    sizes_res_17 = resource_17%food%size
    allocate(eaten_res_17(resource_17%number_food_items))
    eaten_res_17 = resource_17%food%eaten
    allocate(old_iid_res_17(resource_17%number_food_items))
    old_iid_res_17 = resource_17%food%food_iid
  else
    allocate(locations_res_17(0))
    allocate(sizes_res_17(0))
    allocate(eaten_res_17(0))
    allocate(old_iid_res_17(0))
  end if

  if (present(resource_18)) then
    abundance_total = abundance_total + resource_18%number_food_items
    allocate(locations_res_18(resource_18%number_food_items))
    locations_res_18 = resource_18%location()
    allocate(sizes_res_18(resource_18%number_food_items))
    sizes_res_18 = resource_18%food%size
    allocate(eaten_res_18(resource_18%number_food_items))
    eaten_res_18 = resource_18%food%eaten
    allocate(old_iid_res_18(resource_18%number_food_items))
    old_iid_res_18 = resource_18%food%food_iid
  else
    allocate(locations_res_18(0))
    allocate(sizes_res_18(0))
    allocate(eaten_res_18(0))
    allocate(old_iid_res_18(0))
  end if

  if (present(resource_19)) then
    abundance_total = abundance_total + resource_19%number_food_items
    allocate(locations_res_19(resource_19%number_food_items))
    locations_res_19 = resource_19%location()
    allocate(sizes_res_19(resource_19%number_food_items))
    sizes_res_19 = resource_19%food%size
    allocate(eaten_res_19(resource_19%number_food_items))
    eaten_res_19 = resource_19%food%eaten
    allocate(old_iid_res_19(resource_19%number_food_items))
    old_iid_res_19 = resource_19%food%food_iid
  else
    allocate(locations_res_19(0))
    allocate(sizes_res_19(0))
    allocate(eaten_res_19(0))
    allocate(old_iid_res_19(0))
  end if

  if (present(resource_20)) then
    abundance_total = abundance_total + resource_20%number_food_items
    allocate(locations_res_20(resource_20%number_food_items))
    locations_res_20 = resource_20%location()
    allocate(sizes_res_20(resource_20%number_food_items))
    sizes_res_20 = resource_20%food%size
    allocate(eaten_res_20(resource_20%number_food_items))
    eaten_res_20 = resource_20%food%eaten
    allocate(old_iid_res_20(resource_20%number_food_items))
    old_iid_res_20 = resource_20%food%food_iid
  else
    allocate(locations_res_20(0))
    allocate(sizes_res_20(0))
    allocate(eaten_res_20(0))
    allocate(old_iid_res_20(0))
  end if

  !> Make this food resource from the component objects that are provided.
  ! @note  Normally, `make` method allocates objects automatically, so no
  !        no need to call `allocate`on `food_resource_collapsed%food`.
  call food_resource_collapsed%make(                                          &
              label=label_loc,                                                &
              abundance=abundance_total,                                      &
              locations=[ locations_res_1,                                    &
                          locations_res_2,                                    &
                          locations_res_3,                                    &
                          locations_res_4,                                    &
                          locations_res_5,                                    &
                          locations_res_6,                                    &
                          locations_res_7,                                    &
                          locations_res_8,                                    &
                          locations_res_9,                                    &
                          locations_res_10,                                   &
                          locations_res_11,                                   &
                          locations_res_12,                                   &
                          locations_res_13,                                   &
                          locations_res_14,                                   &
                          locations_res_15,                                   &
                          locations_res_16,                                   &
                          locations_res_17,                                   &
                          locations_res_18,                                   &
                          locations_res_19,                                   &
                          locations_res_20  ],                                &
              sizes=[     sizes_res_1,                                        &
                          sizes_res_2,                                        &
                          sizes_res_3,                                        &
                          sizes_res_4,                                        &
                          sizes_res_5,                                        &
                          sizes_res_6,                                        &
                          sizes_res_7,                                        &
                          sizes_res_8,                                        &
                          sizes_res_9,                                        &
                          sizes_res_10,                                       &
                          sizes_res_11,                                       &
                          sizes_res_12,                                       &
                          sizes_res_13,                                       &
                          sizes_res_14,                                       &
                          sizes_res_15,                                       &
                          sizes_res_16,                                       &
                          sizes_res_17,                                       &
                          sizes_res_18,                                       &
                          sizes_res_19,                                       &
                          sizes_res_20      ]  )

  !> And add extra object component arrays that do not get modified by `make`:
  !! the **eaten** status and the id number (**iid**).
  food_resource_collapsed%food%eaten = [    eaten_res_1,                      &
                                            eaten_res_2,                      &
                                            eaten_res_3,                      &
                                            eaten_res_4,                      &
                                            eaten_res_5,                      &
                                            eaten_res_6,                      &
                                            eaten_res_7,                      &
                                            eaten_res_8,                      &
                                            eaten_res_9,                      &
                                            eaten_res_10,                     &
                                            eaten_res_11,                     &
                                            eaten_res_12,                     &
                                            eaten_res_13,                     &
                                            eaten_res_14,                     &
                                            eaten_res_15,                     &
                                            eaten_res_16,                     &
                                            eaten_res_17,                     &
                                            eaten_res_18,                     &
                                            eaten_res_19,                     &
                                            eaten_res_20   ]

  food_resource_collapsed%food%food_iid = [ old_iid_res_1,                    &
                                            old_iid_res_2,                    &
                                            old_iid_res_3,                    &
                                            old_iid_res_4,                    &
                                            old_iid_res_5,                    &
                                            old_iid_res_6,                    &
                                            old_iid_res_7,                    &
                                            old_iid_res_8,                    &
                                            old_iid_res_9,                    &
                                            old_iid_res_10,                   &
                                            old_iid_res_11,                   &
                                            old_iid_res_12,                   &
                                            old_iid_res_13,                   &
                                            old_iid_res_14,                   &
                                            old_iid_res_15,                   &
                                            old_iid_res_16,                   &
                                            old_iid_res_17,                   &
                                            old_iid_res_18,                   &
                                            old_iid_res_19,                   &
                                            old_iid_res_20  ]

    !> If the `reindex` flag is present and is TRUE, do reindexing the new
    !! joined food resource.
    if (present(reindex)) then
      if (reindex) call food_resource_collapsed%reindex()
    end if

  end subroutine food_resources_collapse

  !-----------------------------------------------------------------------------
  !> Join food resources into a single global food resource out of the global
  !! array  the_environment::global_habitats_available.
  !! See the_environment::unjoin() for how to unjoin an array of food resources
  !! back into an array.
  !! The joined resource can then go into the perception system. The
  !! properties of the component resources are retained in the collapsed
  !! resource.
  !! @note    This procedure is intended to use a short interface name `join`,
  !!          see the_environment::join().
  !! @note    A similar procedure using a **list** of input component resources
  !!          is implemented in the_environment::food_resources_collapse().
  function food_resources_collapse_global_object (reindex, label)             &
                                              result (food_resource_collapsed)
    !> @param[in] reindex logical flag to reindex the joined resource (TRUE)
    !!            upon joining. The default is **no** reindexing.
    logical, optional, intent(in) :: reindex
    !> @param[in] label Label for the joined food resource, if absent set to
    !!            'tmp_object'.
    character(len=*), optional, intent(in) :: label
    !> @return A collapsed food resource joining the input array.
    type(FOOD_RESOURCE) :: food_resource_collapsed

    ! Local total number of food items in the output collapsed food resource.
    integer :: abundance_total

    ! Local copy of optional label
    character(len=LABEL_LENGTH) :: label_loc

    ! Local counters
    integer :: i, j, k, size_arr_step

    !> ### Implementation details ###
    !! This procedure builds the output `food_resource_collapsed` resource
    !! object from scratch fully substituting the normal `make`
    !! the_environment::food_resource::make() method. Check out the main method
    !! the_environment::food_resource::make() in case of reimplementation.
    !!
    !> - Determine the total number of food items in the collapsed food
    !!   resource, it equals to the sum of the items across all components
    !!   of the the_environment::global_habitats_available array.
    abundance_total = sum (Global_Habitats_Available%food%number_food_items)

    !> - Make a label for the collapsed object, if not present as a dummy
    !!   parameter, set to 'tmp_object'.
    if (present(label)) then
      label_loc = label
    else
      label_loc = "tmp_object"
    end if
    food_resource_collapsed%food_label = label_loc

    !> - Allocate the food items array for the collapsed food resource.
    if (.not. allocated( food_resource_collapsed%food ))                      &
                    allocate( food_resource_collapsed%food(abundance_total) )

    !> - Set the abundance for the new output object equal to the sum across
    !!   all the components.
    food_resource_collapsed%number_food_items = abundance_total

    !> - Create all the food items in the new object using the
    !!   the_environment::food_item::create() method.
    call food_resource_collapsed%food%create()

    !> - Finally, copy individual food items from the component resources
    !!   into the new joined resource looping over all resources and appending
    !!   arrays.
    !   @note Implementation note: GNU gfortran accepts copying the array
    !         by slices in previous version. But Intel ifort resulted in
    !         segmentation faults. Therefore, instead of working with the
    !         array slices, an inner loop over `j` is implemented: it doesn't
    !         break ifort now.
    size_arr_step = 0
    COPY_FOODS: do i=1, size(Global_Habitats_Available)
      k=0
      do j = size_arr_step + 1, size_arr_step +                               &
                           Global_Habitats_Available(i)%food%number_food_items
        K = k + 1
        food_resource_collapsed%food(j) =                                     &
                                      Global_Habitats_Available(i)%food%food(k)
      end do
      size_arr_step = size_arr_step +                                         &
                            Global_Habitats_Available(i)%food%number_food_items
    end do COPY_FOODS

    !> - If the `reindex` flag is present and is TRUE, do reindexing the new
    !!   joined food resource.
    !! .
    if (present(reindex)) then
      if (reindex) call food_resource_collapsed%reindex()
    end if

  end function food_resources_collapse_global_object

  !-----------------------------------------------------------------------------
  !> Transfer back the resulting food resources into their original objects
  !! out from a collapsed object from `food_resources_collapse`.
  !> @param[inout] resource_1, resource_2, ... a list of food resources to
  !!               restore from the joined state.
  !> @param[in]    reindex logical flag to reindex the joined resource (TRUE)
  !!               upon joining. Default is **no** reindexing.
  !! @note    The calculation does not use an array of food resources because
  !!          this can create problems in setting input dummy parameters in
  !!          the array constructor. It just accepts raw resource objects and
  !!          does all the operations directly on them. The number of food
  !!          resources is probably never big, so the hard-coded limit of
  !!          20 components would probably never be exceeded. But the object
  !!          list implementation is rather wordy, code-duplicating and prone
  !!          to editing bugs. **The main aim** of this wordy, code-dubbing and
  !!          mistype-prone approach was to allow easy passage of the whole
  !!          original resource objects back from the collapsed object
  !!          retaining all the changes that were introduced (e.g. the `eaten`
  !!          status) while the resource objects were processed as the joined
  !!          super-object. TODO: Perhaps it could be reimplemented in a better
  !!          style using an extension object type.
  !! @warning The un-joined food resource objects retain the joined object
  !!          ID's. If individual id indexing are required, `reindex` method
  !!          should be called for each of the unjoined food resource object
  !!          upon joining!
  !> @note See notes on `food_resources_collapse()`,
  !!          `food_resources_update_back()` and `reindex_food_resources()`.
  subroutine food_resources_update_back( food_resource_collapsed,             &
                                        resource_1,                           &
                                        resource_2,                           &
                                        resource_3,                           &
                                        resource_4,                           &
                                        resource_5,                           &
                                        resource_6,                           &
                                        resource_7,                           &
                                        resource_8,                           &
                                        resource_9,                           &
                                        resource_10,                          &
                                        resource_11,                          &
                                        resource_12,                          &
                                        resource_13,                          &
                                        resource_14,                          &
                                        resource_15,                          &
                                        resource_16,                          &
                                        resource_17,                          &
                                        resource_18,                          &
                                        resource_19,                          &
                                        resource_20,                          &
                                        reindex     )

    ! @note Note that `food_resource_collapsed` is actually the `this` object
    !       in the type-bound procedure. It has been initially defined in
    !       a previous non-type-bound version as **type** to enable strict
    !       typing. But this would not work with type-bound, so had to
    !       declare as a polymorphic `class`. However, it should normally
    !       only accept the  `FOOD_RESOURCE` type objects.
    class(FOOD_RESOURCE), intent(in) ::               food_resource_collapsed
    ! @param[inout] resource_1, resource_2, ... a list of food resources to
    !               restore from the joined state.
    class(FOOD_RESOURCE), optional, intent(inout) ::  resource_1,             &
                                                      resource_2,             &
                                                      resource_3,             &
                                                      resource_4,             &
                                                      resource_5,             &
                                                      resource_6,             &
                                                      resource_7,             &
                                                      resource_8,             &
                                                      resource_9,             &
                                                      resource_10,            &
                                                      resource_11,            &
                                                      resource_12,            &
                                                      resource_13,            &
                                                      resource_14,            &
                                                      resource_15,            &
                                                      resource_16,            &
                                                      resource_17,            &
                                                      resource_18,            &
                                                      resource_19,            &
                                                      resource_20

    ! @param[in] reindex logical flag to reindex the joined resource (TRUE)
    !            upon joining. Default is **no** reindexing.
    logical, optional, intent(in) :: reindex

    ! Local counters, global (all component resource objects) and local (each
    ! of the resource).
    integer :: global_count_collapsed, i

    !> ### Implementation details ###

    global_count_collapsed = 0

    if (present(resource_1)) then
      associate ( RES => resource_1 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          !> For each food item within the resource we copy all the object
          !! component values from the **collapsed** object to the original
          !! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        !> If `reindex` is explicitly set to TRUE, we reindex the component
        !! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_2)) then
      associate ( RES => resource_2 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_3)) then
      associate ( RES => resource_3 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_4)) then
      associate ( RES => resource_4 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_5)) then
      associate ( RES => resource_5 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_6)) then
      associate ( RES => resource_6 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_7)) then
      associate ( RES => resource_7 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_8)) then
      associate ( RES => resource_8 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_9)) then
      associate ( RES => resource_9 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_10)) then
      associate ( RES => resource_10 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_11)) then
      associate ( RES => resource_11 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_12)) then
      associate ( RES => resource_12 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_13)) then
      associate ( RES => resource_13 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_14)) then
      associate ( RES => resource_14 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_15)) then
      associate ( RES => resource_15 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_16)) then
      associate ( RES => resource_16 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_17)) then
      associate ( RES => resource_17 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_18)) then
      associate ( RES => resource_18 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_19)) then
      associate ( RES => resource_19 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

    if (present(resource_20)) then
      associate ( RES => resource_20 )
        ! @warning We cannot use `do concurrent` construct here since global
        !          counter `global_count_collapsed` is updated sequentially
        !          and depends on the local counter `i`.
        do i=1, RES%number_food_items
          global_count_collapsed = global_count_collapsed + 1
          ! For each food item within the resource we copy all the object
          ! component values from the **collapsed** object to the original
          ! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        ! If reindex is explicitly set to TRUE, we reindex the component
        ! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end if

  end subroutine food_resources_update_back

  !-----------------------------------------------------------------------------
  !> Transfer the (having been modified) food resource objects from the single
  !! united object `food_resource_collapsed` back to the global array
  !! the_environment::global_habitats_available array.
  !! See the_environment::join() for how to join an array of food resources
  !! into a single global object.
  subroutine food_resources_update_back_global_object(food_resource_collapsed,&
                                                      reindex )
    !> @param[in] A collapsed food resource previously joining the input array.
    type(FOOD_RESOURCE), intent(in) :: food_resource_collapsed
    !> @param[in] reindex logical flag to reindex the unjoined resource (TRUE)
    !!            upon unjoining. The default is **no** reindexing.
    logical, optional, intent(in) :: reindex

    ! Local counters, global (all component resource objects) and local (each
    ! of the resource).
    integer :: global_count_collapsed, res_num, i, j

    !> ### Implementation details ###
    !! This procedure restores individual food resources into the global array
    !! the_environment::global_habitats_available array from the  collapsed
    !! resource `food_resource_collapsed`.
    !!
    global_count_collapsed = 0

    do concurrent (res_num=1:size(Global_Habitats_Available))
      associate ( RES => Global_Habitats_Available(res_num)%food )
        do concurrent (i=1:RES%number_food_items)
          !prev_res = 0
          !do j=1, res_num-1
          !  prev_res = prev_res + size( resources_in_loc(j)%food ) ! safer than + %number_food_items
          !end do
          global_count_collapsed = sum(                                       &
                    [( size(Global_Habitats_Available(j)%food%food),          &
                       j=1, res_num-1 )] ) + i
          !> For each food item within the resource we copy all the object
          !! component values from the **collapsed** object to the original
          !! component object.
          call RES%food(i)%position(                                          &
                food_resource_collapsed%food(global_count_collapsed)%location())
          RES%food(i)%size = food_resource_collapsed%food(                    &
                                              global_count_collapsed)%size
          RES%food(i)%eaten = food_resource_collapsed%food(                   &
                                              global_count_collapsed)%eaten
          RES%food(i)%food_iid = food_resource_collapsed%food(                &
                                              global_count_collapsed)%food_iid
        end do
        !> If `reindex` is explicitly set to TRUE, we reindex the component
        !! resources upon unjoining.
        if (present(reindex)) then
          if (reindex) call RES%reindex()
        end if
      end associate
    end do

  end subroutine food_resources_update_back_global_object

  !-----------------------------------------------------------------------------
  !> Assemble the global habitats objects array
  !! the_environment::global_habitats_available from a list of separate
  !! habitat objects.
  !! This call
  !! @code
  !!   assemble(hab_a, hab_b, hab_c)
  !! @endcode
  !! is equivalent to
  !! @code
  !!   Global_Habitats_Available = [ hab_a, hab_b, hab_c ]
  !! @endcode
  !! @note    But note that the `reindex` parameter allows automatic reindexing
  !!          of the global array the_environment::global_habitats_available.
  subroutine global_habitats_assemble               ( habitat_1,             &
                                                      habitat_2,             &
                                                      habitat_3,             &
                                                      habitat_4,             &
                                                      habitat_5,             &
                                                      habitat_6,             &
                                                      habitat_7,             &
                                                      habitat_8,             &
                                                      habitat_9,             &
                                                      habitat_10,            &
                                                      habitat_11,            &
                                                      habitat_12,            &
                                                      habitat_13,            &
                                                      habitat_14,            &
                                                      habitat_15,            &
                                                      habitat_16,            &
                                                      habitat_17,            &
                                                      habitat_18,            &
                                                      habitat_19,            &
                                                      habitat_20,            &
                                                      reindex     )

    !> @param[inout] habitat_1, ... a list (up to 20) of food resources to
    !!               restore from the joined state.
    !!               @warning elementary habitats in the list are strictly
    !!                        **type**, extension (class) objects are not
    !!                        supported.
    type(HABITAT), optional, intent(in) ::            habitat_1,             &
                                                      habitat_2,             &
                                                      habitat_3,             &
                                                      habitat_4,             &
                                                      habitat_5,             &
                                                      habitat_6,             &
                                                      habitat_7,             &
                                                      habitat_8,             &
                                                      habitat_9,             &
                                                      habitat_10,            &
                                                      habitat_11,            &
                                                      habitat_12,            &
                                                      habitat_13,            &
                                                      habitat_14,            &
                                                      habitat_15,            &
                                                      habitat_16,            &
                                                      habitat_17,            &
                                                      habitat_18,            &
                                                      habitat_19,            &
                                                      habitat_20

    !> @param[in] reindex logical flag to reindex the global joined food
    !!            resource array (TRUE) linked to each of the habitats upon
    !!            assemble. Default is **no** reindexing.
    logical, optional, intent(in) :: reindex

    ! Local counters, global (all component resource objects) and local (each
    ! of the resource).
    integer :: global_count_collapsed

    global_count_collapsed = 0

    !> ### Implementation notes ###
    !> - Stage 1: Calculate how many habitat objects are there in the
    !!   input parameter list.
    if (present(habitat_1)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_2)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_3)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_4)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_5)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_6)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_7)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_8)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_9)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_10)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_11)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_10)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_13)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_14)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_15)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_16)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_17)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_18)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_19)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    if (present(habitat_20)) then
      global_count_collapsed = global_count_collapsed + 1
    end if

    !> - Stage 2: Allocate the the_environment::global_habitats_available
    !!   global array of habitat objects the above number of elements.
    if (.not. allocated(Global_Habitats_Available)) then
      allocate(Global_Habitats_Available(global_count_collapsed))
    else
      deallocate(Global_Habitats_Available)
      allocate(Global_Habitats_Available(global_count_collapsed))
    end if

    !> - Stage 3: Build the global array of habitat objects one by one from
    !!   the input list of individual habitat objects.
    global_count_collapsed = 0

    if (present(habitat_1)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_1
    end if

    if (present(habitat_2)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_2
    end if

    if (present(habitat_3)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_3
    end if

    if (present(habitat_4)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_4
    end if

    if (present(habitat_5)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_5
    end if

    if (present(habitat_6)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_6
    end if

    if (present(habitat_7)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_7
    end if

    if (present(habitat_8)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_8
    end if

    if (present(habitat_9)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_9
    end if

    if (present(habitat_10)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_10
    end if

    if (present(habitat_11)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_11
    end if

    if (present(habitat_12)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_12
    end if

    if (present(habitat_13)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_13
    end if

    if (present(habitat_14)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_14
    end if

    if (present(habitat_15)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_15
    end if

    if (present(habitat_16)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_16
    end if

    if (present(habitat_17)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_17
    end if

    if (present(habitat_18)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_18
    end if

    if (present(habitat_19)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_19
    end if

    if (present(habitat_20)) then
      global_count_collapsed = global_count_collapsed + 1
      Global_Habitats_Available(global_count_collapsed) = habitat_20
    end if

    !> - Stage 4: Optionally reindex each element of the the global array
    !!   the_environment::global_habitats_available.
    !! .
    if (present(reindex)) then
      if (reindex) then
        do concurrent (global_count_collapsed=1:size(Global_Habitats_Available))
          call Global_Habitats_Available(global_count_collapsed)%food%reindex()
        end do
      end if
    end if

  end subroutine global_habitats_assemble

  !-----------------------------------------------------------------------------
  !> Disassemble the global habitats objects array
  !! the_environment::global_habitats_available into separate habitat object.
  subroutine global_habitats_disassemble            ( habitat_1,             &
                                                      habitat_2,             &
                                                      habitat_3,             &
                                                      habitat_4,             &
                                                      habitat_5,             &
                                                      habitat_6,             &
                                                      habitat_7,             &
                                                      habitat_8,             &
                                                      habitat_9,             &
                                                      habitat_10,            &
                                                      habitat_11,            &
                                                      habitat_12,            &
                                                      habitat_13,            &
                                                      habitat_14,            &
                                                      habitat_15,            &
                                                      habitat_16,            &
                                                      habitat_17,            &
                                                      habitat_18,            &
                                                      habitat_19,            &
                                                      habitat_20,            &
                                                      reindex     )

    !> @param[inout] habitat_1, ... a list (from 2 to 20) of food resources to
    !!               restore from the joined state.
    !!               @warning elementary habitats in the list are strictly
    !!                        **type**, extension (class) objects are not
    !!                        supported.
    type(HABITAT), intent(out) ::                     habitat_1

    type(HABITAT), optional, intent(out) ::           habitat_2,             &
                                                      habitat_3,             &
                                                      habitat_4,             &
                                                      habitat_5,             &
                                                      habitat_6,             &
                                                      habitat_7,             &
                                                      habitat_8,             &
                                                      habitat_9,             &
                                                      habitat_10,            &
                                                      habitat_11,            &
                                                      habitat_12,            &
                                                      habitat_13,            &
                                                      habitat_14,            &
                                                      habitat_15,            &
                                                      habitat_16,            &
                                                      habitat_17,            &
                                                      habitat_18,            &
                                                      habitat_19,            &
                                                      habitat_20


    !> @param[in] reindex logical flag to reindex the joined food resource
    !!            (TRUE) linked to each of the habitats upon disassemble.
    !!            Default is **no** reindexing.
    logical, optional, intent(in) :: reindex

    ! Local counters, global (all component resource objects) and local (each
    ! of the resource).
    integer :: global_count_collapsed

    global_count_collapsed = 0

    !if (present(habitat_1)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_1 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_1%food%reindex()
      end if
    !end if

    if (present(habitat_2)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_2 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_2%food%reindex()
      end if
    end if

    if (present(habitat_3)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_3 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_3%food%reindex()
      end if
    end if

    if (present(habitat_4)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_4 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_4%food%reindex()
      end if
    end if

    if (present(habitat_5)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_5 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_5%food%reindex()
      end if
    end if

    if (present(habitat_6)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_6 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_6%food%reindex()
      end if
    end if

    if (present(habitat_7)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_7 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_7%food%reindex()
      end if
    end if

    if (present(habitat_8)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_8 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_8%food%reindex()
      end if
    end if

    if (present(habitat_9)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_9 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_9%food%reindex()
      end if
    end if

    if (present(habitat_10)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_10 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_10%food%reindex()
      end if
    end if

    if (present(habitat_11)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_11 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_11%food%reindex()
      end if
    end if

    if (present(habitat_12)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_12 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_12%food%reindex()
      end if
    end if

    if (present(habitat_13)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_13 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_13%food%reindex()
      end if
    end if

    if (present(habitat_14)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_14 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_14%food%reindex()
      end if
    end if

    if (present(habitat_15)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_15 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_15%food%reindex()
      end if
    end if

    if (present(habitat_16)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_16 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_16%food%reindex()
      end if
    end if

    if (present(habitat_17)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_17 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_17%food%reindex()
      end if
    end if

    if (present(habitat_18)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_18 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_18%food%reindex()
      end if
    end if

    if (present(habitat_19)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_19 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_19%food%reindex()
      end if
    end if

    if (present(habitat_20)) then
      global_count_collapsed = global_count_collapsed + 1
      habitat_20 = Global_Habitats_Available( global_count_collapsed )
      if (present(reindex)) then
        if (reindex) call habitat_20%food%reindex()
      end if
    end if

  end subroutine global_habitats_disassemble

  !-----------------------------------------------------------------------------
  !> Calculate the distances between **this** spatial object and an array of
  !! its neighbours. Optionally output the distances, sorting index vector
  !! and rankings vector for each of these neighbours. Optionally do only
  !! partial indexing, up to the order `rank_max` for computational speed.
  !! Procedure `ARRAY_INDEX()` from HEDTOOLS is used as the computational
  !! backend for partial segmented indexing.
  !! @param[in] neighbours an array of spatial objects that we
  !!        sort by distance from **this** target object.
  !! @param[out] dist optional vector of the distance between each of the
  !!        neighbours and **this** spatial object.
  !! @param[out] index_vector a vector for sort order indexing of the
  !!        neighbours. see documentation for `ARRAY_INDEX()` in `HEDTOOLS`
  !!        for more details on sort order indexing.
  !! @param[out] ranks  optional vector of rank ordering scores of each of
  !!        the distances.
  !! @param[in] rank_max sets the maximum limit on the objects to rank/index
  !!        we are interested in, i.e. for partial indexing (see manual
  !!        for `ARRAY_INDEX`).
  !! @param[out] error_flag optional error flag, normally should be FALSE.
  !! @warning Cannot be made `pure` because of I/O calls.
  subroutine spatial_neighbours_distances(this, neighbours, dist,             &
                                          index_vector, ranks, rank_max,      &
                                          error_flag)
    class(SPATIAL), intent(in) :: this

    ! @param neighbours an array of spatial objects that we
    !        sort by distance from **this** target object.
    class(SPATIAL), dimension(:), intent(in) :: neighbours

    ! @param dist optional vector of the distance between each of the
    !        neighbours and **this** spatial object.
    real(SRP), dimension(:), optional, intent(out) :: dist

    ! @param index_vector a vector for sort order indexing of the neighbours.
    !        see documentation for `ARRAY_INDEX` in `HEDTOOLS` for more
    !        details on sort order indexing.
    integer, dimension(:), optional, intent(out) :: index_vector

    ! @param ranks  optional vector of rank ordering scores of each of
    !        the distances.
    integer, dimension(:), optional, intent(out) :: ranks

    ! @param rank_max sets the maximum limit on the objects to rank/index
    !        we are interested in, i.e. for partial indexing (see manual
    !        for `ARRAY_INDEX`).
    integer, optional, intent(in) :: rank_max

    ! @param error_flag optional error flag, normally should be FALSE
    logical, optional, intent(out) :: error_flag

    ! Local copy of distances vector between **this** spatial object and
    ! each of the neighbours.
    real(SRP), dimension(size(neighbours)) :: dist_here
    integer, dimension(size(neighbours)) :: index_vec_here

    ! PROCNAME is for error reporting and debugging
    character (len=*), parameter :: PROCNAME = "(spatial_neighbours_distances)"

    ! Initialise `error_flag` if it is provided.
    if (present(error_flag)) error_flag=.FALSE.

    !> ### Implementation details ###
    !> First, calculate the distances between this object ant all of its
    !! neighbours. This is done using the parallel `do concurrent` construct
    !! from F2008.
    !do concurrent ( i = 1:size(neighbours) )
    !  dist_here(i)  = this%distance(neighbours(i))
    !end do
    dist_here  = this%distance(neighbours)

    ! Check if we have to output these distances.
    if (present(dist)) dist = dist_here

    !> Iterative vector sorting and ranking indexes can be slow to calculate
    !! when there are many neighbours. So we need to know are they really
    !! necessary (parameters present). Also check that all vectors have the
    !! same sizes.
    if (present(index_vector)) then
      ! If we need index vector then calculate it.
      if ( size(neighbours) /= size(index_vector) ) then
        if (present(error_flag)) error_flag=.TRUE.
        call LOG_MSG( LTAG_WARN // PROCNAME //                                &
                     ": INDEX_VECTOR mismatch neighbours vector!" )
      end if
      if (present(rank_max)) then
        !> Partial indexing is used if `rank_max` parameter is provided. This
        !! will avoid full indexing of all objects which may be much faster for
        !! big arrays.
        call ARRAY_INDEX(dist_here, index_vector, rank_max)
      else
        ! Full indexing otherwise.
        call ARRAY_INDEX(dist_here, index_vector)
      end if
      if (present(ranks)) then
        !> Then calculate ranks if we need them.
        if ( size(neighbours) /= size(ranks) ) then
          if (present(error_flag)) error_flag=.TRUE.
          call LOG_MSG( LTAG_WARN // PROCNAME //                              &
                      ": RANKS mismatch neighbours vector!")
        end if
        call ARRAY_RANK(index_vector, ranks)
      end if
    else
      !> If we need ranks, calculate both index vector and ranks
      if (present(ranks)) then
        if ( size(neighbours) /= size(ranks) ) then
          call LOG_MSG( LTAG_WARN // PROCNAME //                              &
                      ": RANKS mismatch neighbours vector!")
        end if
        if (present(rank_max)) then
          !> Use partial indexing if `rank_max` parameter is provided. This
          !! will avoid full indexing of all objects which may be much faster
          !! for big arrays.
          call ARRAY_INDEX(dist_here, index_vec_here, rank_max)
        else
          !> Full indexing otherwise.
          call ARRAY_INDEX(dist_here, index_vec_here)
        end if
        call ARRAY_RANK(index_vec_here, ranks)
        index_vector = index_vec_here
      end if
    end if

  end subroutine spatial_neighbours_distances

  !-----------------------------------------------------------------------------
  !> Initialise a predator object.
  !! @param body_size the body size of the predator.
  !! @param attack_rate baseline attack rate of the predator.
  !! @param environment The environment within which the predator is located
  !!        initially.
  !! @param label optional label for the predator.
  elemental subroutine predator_make_init(this, body_size, attack_rate,       &
                                                                position, label)
    class(PREDATOR), intent(inout) :: this

    ! @param body_size the body size of the predator.
    real(SRP), intent(in) :: body_size
    ! @param attack_rate baseline attack rate of the predator.
    real(SRP), intent(in) :: attack_rate
    ! @param environment The environment within which the predator is located
    !!        initially
    type(SPATIAL), intent(in), optional :: position

    ! @param label optional label for the predator
    character(len=*), optional, intent(in) :: label

    !> ### Implementation details ###
    !> We first create an empty spatial sub-object for the predator.
    call this%create()

    !> Set the body size parameter, with a limitation that it must exceed zero.
    this%body_size = max( ZERO, body_size )

    !> Set the capture attack rate parameter, limited to be within the range
    !! [0:1].
    this%attack_rate = within(attack_rate, 0.0_SRP, 1.0_SRP)

    !> Set the initial position if it is provided (will remain `MISSING` as
    !! initialised in `create` method above otherwise).
    if (present(position)) call this%position(position)

    !> Finally, set label for the predator if provided, empty if absent.
    if (present(label)) then
      this%label = label
    else
      this%label = ""
    end if

  end subroutine predator_make_init

  !-----------------------------------------------------------------------------
  !> Set label for the predator, if not provided, set it random.
  !! @param label optional label for the predator.
  subroutine predator_label_set(this, label)
    class(PREDATOR), intent(inout) :: this
    ! @param label optional label for the predator.
    character(len=*), optional :: label

    if (present(label)) then
      this%label = label
    else
      this%label = "PRED_" // RAND_STRING( LABEL_LENGTH-len("PRED_"),         &
                                           LABEL_CST, LABEL_CEN)
    end if

  end subroutine predator_label_set

  !-----------------------------------------------------------------------------
  !> Accessor function for the predator body size (length).
  elemental function predator_get_body_size(this) result (body_size_get)
    class(PREDATOR), intent(in) :: this
    real(SRP) :: body_size_get

    body_size_get = this%body_size

  end function predator_get_body_size

  !-----------------------------------------------------------------------------
  !> Accessor function for the predator attack rate.
  elemental function predator_get_attack_rate(this) result (capt_get)
    class(PREDATOR), intent(in) :: this
    real(SRP) :: capt_get

    capt_get = this%attack_rate

  end function predator_get_attack_rate

  !-----------------------------------------------------------------------------
  !> Calculates the risk of capture of the fish with the spatial location
  !! defined by `prey_spatial` and the body length equal to `prey_length`.
  !! This is a backend function bound to the predator rather than prey
  !! object.
  !! @note This procedure calculates the probability of capture from the the
  !!       predator object side:
  !!       @code
  !!         predator%risk_fish( agent%location, agent%get_length() )
  !!       @endcode
  !!       It is not possible for the predator-object-bound function to
  !!       determine the properties of the prey agent (that is the dummy
  !!       parameter) as these parameters are defined in the_neurobio module
  !!       later in the class hierarchy. For example, body mass of the agent
  !!       is set in the_body::condition whereas the perception object
  !!       bound function the_neurobio::perception::has_pred is defined in
  !!       the the_neurobio::perception class. This is why the properties of
  !!       the prey, the length and the distance, are set via mandatory dummy
  !!       parameters.  The capture probability should normally be calculated
  !!       for the agent the other way round using the frontend function
  !!       the_neurobio::perception::risk_pred():
  !!       @code
  !!         agent%risk_pred( predator )
  !!       @endcode
  function predator_capture_risk_calculate_fish(this,                         &
                              prey_spatial, prey_length,                      &
                              prey_distance, is_freezing,                     &
                              time_step_model, debug_plot_file) result (risk_out)
    class(PREDATOR), intent(in) :: this
    !> @param[in] prey_spatial the spatial position of a fish/agent prey.
    class(SPATIAL), intent(in) :: prey_spatial
    !> @param[in] prey_length the length of the prey fish agent.
    real(SRP), intent(in) :: prey_length
    !> @param[in] prey_distance optional distance between the `this` predator
    !!            and the prey fish agent.
    real(SRP), optional, intent(in) :: prey_distance
    !> @param[in] is_freezing Optional logical indicator that the prey fish
    !!            agent is immobile.
    logical, optional, intent(in) :: is_freezing
    !> @param[in] time_step_model optional time step of the model.
    integer, optional, intent(in) :: time_step_model
    !> @param[in] debug_plot_file optional file name for the debug
    !!            nonparametric function (interpolation) plot.
    character(len=*), optional, intent(in) :: debug_plot_file

    !> @return The probability of successful capture of the prey by the `this`
    !!         predator
    real(SRP) :: risk_out

    character(len=*), parameter :: PROCNAME =                                 &
                                        "(predator_capture_risk_calculate_fish)"

    ! Local copies of optionals
    real(SRP) :: prey_distance_here
    logical :: is_freezing_loc
    integer :: time_step_here
    character(FILENAME_LENGTH) :: debug_plot_file_here

    ! Local variables:
    ! Illumination at the prey agent depth.
    real(SRP) :: irradiance_agent_depth
    ! The side area of the fish agent.
    real(SRP) :: prey_fish_area_m
    ! The visibility of the prey fish agent, calculated using the
    ! visual range backend.
    real(SRP) :: prey_visibility

    !> ### Check optional parameters ###
    if (present(is_freezing)) then
      is_freezing_loc = is_freezing
    else
      is_freezing_loc = .FALSE.
    end if

    !> Check optional time step parameter. If unset, use global variable
    !! `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_here = time_step_model
    else
      time_step_here = Global_Time_Step_Model_Current
    end if

    !> Check the distance to the prey dummy parameter. If present, the dummy
    !! parameter for the distance is used.
    if (present(prey_distance)) then
      prey_distance_here = prey_distance
    else
      !> However, if the dummy parameter is not provided, the distance between
      !! the `this` predator and the prey spatial object is calculated using
      !! the spatial::distance() function.
      prey_distance_here = this%distance( prey_spatial )
    end if

    !> Check optional debug  plot file name, if absent, a random name is
    !! generated based on the predator's iid.  However, the prey agent's
    !! iid cannot yet be determined as it is defined in the following
    !! module layers. Threfore, it should be provided, if necessary,  via
    !! the optional file name parameter.
    if (present(debug_plot_file)) then
      debug_plot_file_here = debug_plot_file
    else
      debug_plot_file_here = "plot_debug_predation_risk_" //                  &
                           TOSTR(Global_Time_Step_Model_Current) // "_" //    &
                           RAND_STRING(LABEL_LENGTH, LABEL_CST,LABEL_CEN) // PS
    end if

    !> ### Implementation details ###
    !> #### Calculate the visibility of the prey fish object ####
    !> First, calculate the **illumination** (background irradiance) at the
    !! depth of the prey spatial object.
    irradiance_agent_depth = prey_spatial%illumination(time_step_here)

    !> Second, calculate the **fish prey area** (m) that will later go into
    !! the visual range calculation backend engine.
    prey_fish_area_m = length2sidearea_fish( cm2m( prey_length ) )

    !> Third, calculate the visual range for the predator for detecting the
    !! fish prey object defined by the `prey_spatial` spatial object.
    !! It is assumed here that the predator is much larger than the agent
    !! prey.
    !! @image html img_doxygen_predator_visrange.svg
    !! @image latex img_doxygen_predator_visrange.eps "Visual ranges for the agent and the predator" width=14cm
    !! At the figure above, @f$ VR_{p} @f$ is the visual range for the agent
    !! to detect the predator and @f$ VR_{a} @f$ is the visual range for the
    !! predator to see the agent prey.
    !> This is why he calculations of the capture probability are
    !! based on the latter, @f$ VR_{a} @f$, the visibility of the prey for the
    !! predator.
    prey_visibility =                                                         &
        m2cm ( visual_range (                                                 &
                  irradiance = irradiance_agent_depth,                        &
                  prey_area = prey_fish_area_m,                               &
                  prey_contrast = INDIVIDUAL_VISUAL_CONTRAST_DEFAULT          &
              ) )

    !> #### Check prey agent visibility ####
    !> Check if the prey agent is visible to the predator. Predator can only
    !! attack a prey agent that it can see. Otherwise zero predation risk is
    !! returned
    if ( prey_visibility < prey_distance_here ) then
      call LOG_DBG(LTAG_INFO // "Prey agent is invisible to the predator:" // &
                   " visibility=" // TOSTR(prey_visibility) // " < " //       &
                   " distance=" // TOSTR(prey_distance_here), PROCNAME, MODNAME)
      risk_out = 0.0_SRP
      return
    end if

    !> #### Calculate the predation risk ####
    !> Calculation is conducted differently depending on whether the agent is
    !! **moving** or **immobile** (freezing). Because the dimensionality
    !! of the interpolation grid arrays can be different in these two cases
    !! (requiring different declarations) they are isolated into two Fortran
    !! named block constructs.
    MOVING_VS_FREEZING: if (is_freezing_loc) then

      !> ##### Freezing (immobile) agent #####
      !! Calculations of the predation risk for a freezing agent are
      !! conducted in the named block construct `FREEZING`.
      FREEZING: block

        ! Interpolation grid, the dimensionality can be different in
        ! moving and freezing agents:
        ! The dimensionality of the grid.
        integer, parameter :: INTERPOL_DIM = 4
        ! Interpolation grid abscissa and ordinate.
        real(SRP), dimension(INTERPOL_DIM) :: interpol_abscissa,              &
                                              interpol_ordinate

        !> - Calculate the interpolation grid that is then used to calculate
        !!   the predation risk.
        !> - The interpolation **abscissa** is defined by the visual range:
        !!   from zero, half, 0.75 of the visual range and a full visual range.
        !!   It is assumed that tha ability of the predator to locate and
        !!   attack an immobile (freezing) agent is much smaller than a moving
        !!   agent.
        interpol_abscissa = [ 0.0_SRP,                                        &
                              prey_visibility * 0.50_SRP,                     &
                              prey_visibility * 0.75_SRP,                     &
                              prey_visibility  ]

        !> - The interpolation grid **ordinate** determines the nonparametric
        !!   relationship between the distance between the predator and prey
        !!   and the predation risk, i.e. the probability of successful capture
        !!   of the prey fish agent. The probability of capture at the zero
        !!   distance is fixed and equal to the predator's inherent **attack
        !!   rate** (the the_environment::predator::attack_rate data component)
        !!   that is always less than the theoretically possible probability
        !!   1.0, whereas at the end of the visual range, the probability of
        !!   capture is zero. Therefore, the interpolation function is defined
        !!   by the two middle points: 0.5 and 0.75 of the visual range:
        !!   - commondata::predator_attack_capture_prob_frz_50
        !!   - commondata::predator_attack_capture_prob_frz_75
        !!   .
        interpol_ordinate = [ this%attack_rate,                               &
                              this%attack_rate *                              &
                                    PREDATOR_ATTACK_CAPTURE_PROB_FRZ_50,      &
                              this%attack_rate *                              &
                                    PREDATOR_ATTACK_CAPTURE_PROB_FRZ_75,      &
                              0.0_SRP ]

        !> - Finally, the probability of capture of the target prey fish agent is
        !!   a nonparametric function of the distance between the
        !!   predator and the prey calculated using the above grid arrays.
        !!   There is an additional condition that this probability must be
        !!   *0 < p < 1* that is enforced by commondata::within().
        !!   @image html img_doxygen_predator_capt_frz.svg
        !!   @image latex img_doxygen_predator_capt_frz.eps "Predator capture probability of a freezing agent" width=14cm
        !>   Produce plot with:
        !!   @verbatim
        !!     htintrpl.exe  [0 0.5 0.75 1] [1 0.1 0.01 0] [0.5] [nonlinear]
        !!   @endverbatim
        risk_out = within( DDPINTERPOL( interpol_abscissa,                    &
                                        interpol_ordinate,                    &
                                        prey_distance_here ), 0.0_SRP, 1.0_SRP )

        !> - Interpolation plots can be saved in the @ref intro_debug_mode
        !!   "debug mode" using this plotting command:
        !!   commondata::debug_interpolate_plot_save().
        !! .
        call debug_interpolate_plot_save(                                     &
                    grid_xx=interpol_abscissa, grid_yy=interpol_ordinate,     &
                    ipol_value=prey_distance_here,                            &
                    algstr="DDPINTERPOL",     & ! Must be as in `risk_out`!
                    output_file=trim(debug_plot_file_here) )

      end block FREEZING

    else MOVING_VS_FREEZING

      !> ##### Normal moving agent #####
      !! Calculations of the predation risk for the normal moving agent are
      !! conducted in the named block construct `NORMAL_MOVING`.
      NORMAL_MOVING: block

        ! Interpolation grid, the dimensionality can be different in moving and
        ! freezing agents:
        ! The dimensionality of the grid.
        integer, parameter :: INTERPOL_DIM = 3
        ! Interpolation grid abscissa and ordinate.
        real(SRP), dimension(INTERPOL_DIM) :: interpol_abscissa,              &
                                              interpol_ordinate

        !> - Calculate the interpolation grid that is then used to calculate
        !!   the predation risk.
        !> - The interpolation **abscissa** is defined by the visual range:
        !!   zero, distance half of the visual range, full visual range.
        interpol_abscissa = [0.0_SRP, prey_visibility/2.0_SRP, prey_visibility]

        !> - The interpolation grid **ordinate** determines the nonparametric
        !!   relationship between the distance between the predator and prey
        !!   and the predation risk, i.e. the probability of successful capture
        !!   of the prey fish agent. The probability of capture at the zero
        !!   distance is fixed and equal to the predator's inherent **attack
        !!   rate** (the the_environment::predator::attack_rate data component)
        !!   that is always less than the theoretically possible probability
        !!   1.0, whereas at the end of the visual range, the probability of
        !!   capture is much lower and is defined by the
        !!   commondata::predator_attack_capture_probability_min parameter.
        !!   Therefore, there is only one value that can be set independently:
        !!   the probability of capture at the distance equal to 1/2 of the
        !!   visual range: it is defined as the parameter constant
        !!   commondata::predator_attack_capture_probability_half.
        interpol_ordinate = [ this%attack_rate,                               &
                              this%attack_rate *                              &
                                    PREDATOR_ATTACK_CAPTURE_PROBABILITY_HALF, &
                              this%attack_rate *                              &
                                    PREDATOR_ATTACK_CAPTURE_PROBABILITY_MIN ]

        !> - Finally, the probability of capture of the target prey fish agent
        !!   is a nonparametric function of the distance between the
        !!   predator and the prey calculated using the above grid arrays.
        !!   There is an additional condition that this probability must be
        !!   *0 < p < 1* that is enforced by commondata::within().
        !!   @image html img_doxygen_predator_capt.svg
        !!   @image latex img_doxygen_predator_capt.eps "Predator capture probability" width=14cm
        !>   Produce plot with:
        !!   @verbatim
        !!     htintrpl.exe  [0 0.5 0.75 1] [1 0.1 0.01 0] [0.5] [nonlinear]
        !!   @endverbatim
        risk_out = within( DDPINTERPOL( interpol_abscissa,                    &
                                        interpol_ordinate,                    &
                                        prey_distance_here ), 0.0_SRP, 1.0_SRP )

        !> - Interpolation plots can be saved in the @ref intro_debug_mode
        !!   "debug mode" using this plotting command:
        !!   commondata::debug_interpolate_plot_save().
        !! .
        call debug_interpolate_plot_save(                                     &
                    grid_xx=interpol_abscissa, grid_yy=interpol_ordinate,     &
                    ipol_value=prey_distance_here,                            &
                    algstr="DDPINTERPOL",     & ! Must be as in `risk_out`!
                    output_file=trim(debug_plot_file_here) )

      end block NORMAL_MOVING

    end if MOVING_VS_FREEZING

    !> #### Extended debugging outputs ####
    !> Log the capture probability visual range calculated, along with the
    !! distance and the visual range.
    if ( is_freezing_loc ) call LOG_DBG(LTAG_INFO // "Agent is freezing.",    &
            PROCNAME, MODNAME)
    call LOG_DBG(LTAG_INFO // "Calculated predator's capture probability: " //&
            TOSTR(risk_out) // ", Distance: " // TOSTR(prey_distance_here) // &
            ", Visual range:" // TOSTR(prey_visibility),  PROCNAME, MODNAME)

  end function predator_capture_risk_calculate_fish

  !-----------------------------------------------------------------------------
  !> Calculates the risk of capture by a specific predator of an
  !! array of the fish agents with the spatial locations array
  !! defined by `prey_spatial` and the body length array
  !! `prey_length`. This subroutine takes account of both the predator
  !! dilution and confusion effects and risk adjusted by the distance
  !! towards the predator.
  !! @note This procedure accepts prey agents as a series of separate
  !!       component arrays: `prey_spatial`, `prey_length`, `is_freezing`
  !!       rather than a sigle the_individual::individual_agent type/class.
  !!       This is done because the agent class hierarchy is defined
  !!       specifically in the upstream modules and is not yet accessible
  !!       at this level. The procedure here has the bare minimum requirements
  !!       for the prey agents: the class the_environment::spatial.
  subroutine predator_capture_risk_calculate_fish_group(this,                 &
                                    prey_spatial, prey_length, is_freezing,   &
                                    time_step_model, risk, risk_indexed,      &
                                    index_dist)
    class(PREDATOR), intent(in) :: this
    !> @param[in] prey_spatial the spatial position of a fish/agent prey.
    class(SPATIAL), dimension(:), intent(in) :: prey_spatial
    !> @param[in] prey_length the length of the prey fish agent.
    real(SRP), dimension(:), intent(in) :: prey_length
    !> @param[in] is_freezing Optional logical indicator that the prey fish
    !!            agent is immobile.
    logical, optional, dimension(:), intent(in) :: is_freezing
    !> @param[in] time_step_model optional time step of the model.
    integer, optional, intent(in) :: time_step_model
    !> @param[out] risk is an optional array of predation risk estimates for
    !!             each agent in the group. The values of the risk for all
    !!             agents that are not visible to the predator get zero risk.
    !!             Note that this array has the normal order of the objects
    !!             as in the input arrays, i.e. as in the normal array of
    !!             the agents.
    real(SRP), optional, dimension(:), intent(out) :: risk
    !> @param[out] risk_indexed is an optional array of predation risk estimates
    !!             for each agent in the group. Risk for all agents that are
    !!             not visible to the predator get zero risk. Note that this
    !!             array is in the "raw" order, as the distances between the
    !!             predator and the agents (not the normal order of the agents
    !!             within the input group). Therefore, to translate this "raw"
    !!             order it to the normal order, one requires the partial index
    !!             array `index_dist`. The nature of the partial indexing
    !!             dictates that only values with the index in the range from
    !!             1 to commondata::predator_risk_group_select_index_partial,
    !!             i.e. the nearest neighbours of the predator make sense. All
    !!             other index values are commondata::unknown.
    !!@warning     The size of this array cannot be smaller than the partial
    !!             indexing parameter
    !!             commondata::predator_risk_group_select_index_partial
    real(SRP), optional, dimension(:), intent(out) :: risk_indexed
    !> @param[out] index_dist optional partial index array that shows the
    !!             partial sorting of the agents with respect to their
    !!             distance to the predator. Note that The nature of the
    !!             partial indexing dictates that only values with the index
    !!             in the range from
    !!             1 to commondata::predator_risk_group_select_index_partial,
    !!             i.e. the nearest neighbours of the predator make sense. All
    !!             other index values are commondata::unknown.
    !!@warning     The size of this array cannot be smaller than the partial
    !!             indexing parameter
    !!             commondata::predator_risk_group_select_index_partial
    integer, optional, dimension(:), intent(out) :: index_dist

    ! PROCNAME is the procedure name for logging and debugging.
    character(len=*), parameter :: PROCNAME =                                 &
                            "(predator_capture_risk_calculate_fish_group)"

    ! Local copies of optionals
    integer :: time_step_model_here
    logical, dimension(size(prey_spatial)) :: is_freezing_here

    ! Temporary possible error status for sub-procedures
    logical :: err_flag

    !> ### Implementation details ###
    !> #### Alternative indexing schemes ####
    !! Two kinds of indexing of the output adjusted risk are possible to get:
    !! normal and "raw". The array in the normal order (`risk`) is provided
    !! with the same indexing as the input arrays (e.g. the array of the
    !! prey agents. The array in the "raw" order `risk_indexed` is arranged
    !! in the order of the distances between the predator and the prey agents
    !! (the first is the nearest). The latter "raw" array can be transformed
    !! to the normal ordering using the `index_dist` indexing array. See
    !! [partial array indexing](http://ahamodel.uib.no/doc/ar01s07.html#_subroutines_array_index_and_array_rank)
    !! procedure in HEDTOOLS.
    !! The table below presents an example of the two modes of ordering.
    !!  "raw" | normal | Adjusted risk
    !! ------:|:------:|:-------------:
    !!   1    |   249  |  0.571031094
    !!   2    |   433  |  0.445715338
    !!   3    |   272  |  0.202977672
    !!   4    |   713  |  0.113853760
    !!   5    |   359  |  0.086924118
    !!   6    |   665  |  0.000000000
    !!
    !! An example of the code requesting the normal array of risks. This array
    !! is arranges in the same order as as the prey `prey_spatial`:
    !! @code
    !! call habitat_safe%predators(i)%risk_fish_group(                       &
    !!                   prey_spatial=proto_parents%individual,              &
    !!                   prey_length=proto_parents%individual%body_length,   &
    !!                   risk = group_risk_array )
    !! @endcode
    !!
    !! An example of the code requesting "raw" indexed array of risks along
    !! with the partial index array:
    !! @code
    !! call this_predator%risk_fish_group(                                   &
    !!                   prey_spatial = this%individual%location(),          &
    !!                   prey_length = this%individual%get_length(),         &
    !!                   is_freezing = this%individual%freeze%is_executed(), &
    !!                   time_step_model = time_step_model_here,             &
    !!                   risk_indexed = p_risk,                              &
    !!                   index_dist = prey_index )
    !! @endcode
    !!
    !> #### Notable local variables ####
    !> The calculations potentially involve looping over a huge array of
    !! potential prey agents, even though not all of them are at a close
    !! enough distance to the predator to have any risk. Only the agents that
    !! are within the visibility range (i.e. can be visible to the predator)
    !! count here out of the huge whole-population array.
    !! Finding the agents neighbouring to the predator benefits from
    !! partial indexing. Only a small small subarray within the input array
    !! of spatial prey agent objects is indexed and analysed. The maximum size
    !! of such a subarray is defined by the partial index size:
    !! commondata::predator_risk_group_select_index_partial (so all the work
    !! subarrays benefiting from partial indexing have such number of elements).
    !! - **dist_index** is the partial array sorting index, see `ARRAY_INDEX()`
    !!   procedure from HEDTOOLS for more details. Note that This vector comes
    !!   into the the_environment::neighbours() spatial indexing procedure and
    !!   must have the full size of the spatial array being indexed. So its
    !!   size here is equal to the `prey_spatial`array size.
    integer, dimension(size(prey_spatial)) :: dist_index

    !> - **risk_adjusted** is an array of predation risk estimates for each
    !!   agent in the group. Risk for all agents that are not visible
    !!   to the predator get null risk. This is the main array that is indexed
    !!   exactly as the output.
    real(SRP), dimension(size(prey_spatial)) :: risk_adjusted

    !> - **risk_adjusted_indexed** is an array of predation risk estimates for
    !!   each agent in the group. Risk for all agents that are not visible
    !!   to the predator get null risk. This array that is indexed in the order
    !!   of the distances between the predator and each of the nearest agents.
    !!   Translation of the true index array order to this raw order requires
    !!   the indexing array `dist_index`.
    real(SRP), dimension(PREDATOR_RISK_GROUP_SELECT_INDEX_PARTIAL) ::         &
                                                          risk_adjusted_indexed

    !! - **dist_neighbours** is the partial array of the distances of each of
    !!   the agents from the predator. Note that this array contains valid
    !!   values only for commondata::predator_risk_group_select_index_partial
    !!    agents that are located in proximity of the predator.
    real(SRP), dimension(size(prey_spatial)) :: dist_neighbours

    !> - **risk_agent_is_visible** is a logical flag array indicating that
    !!   the i-th prey agent in the visual field of the predator is visible
    !!   (i.e. within the visual range). Array limited by the maximum index
    !!   size commondata::predator_risk_group_select_index_partial.
    logical, dimension(PREDATOR_RISK_GROUP_SELECT_INDEX_PARTIAL)              &
                                                    :: risk_agent_is_visible

    !> - **risk_agent_visibility** is the visibility range of each prey agent
    !!   in proximity of this predator. Array limited by the maximum index
    !!   size commondata::predator_risk_group_select_index_partial.
    real(SRP), dimension(PREDATOR_RISK_GROUP_SELECT_INDEX_PARTIAL)            &
                                                    :: risk_agent_visibility

    !> - **risk_agent_rank** is the integer rank order of each agent in the
    !!   visual field of the predator with respect to the distance from the
    !!   predator. Only agents visible to the predator (distance < maximum
    !!   visibility range) count. One potential caveat is that because the
    !!   prey agents  are stochastic, there can be cases when a tiny agent is
    !!   the nearest to the predator, but its visibility range is very small,
    !!   smaller than the distance to the predator. Such agent is not counted
    !!   and has undefined (commondata::unknown) risk_agent_rank.
    integer, dimension(PREDATOR_RISK_GROUP_SELECT_INDEX_PARTIAL)              &
                                                    :: risk_agent_rank

    !> - **rank_visible** is the overall counter and the total number of
    !!   "ranked" prey agents, i.e. those that are "visible".
    integer :: rank_visible

    !> - **risk_agent_baseline** is the baseline risk of predation for each
    !!   of the prey agents in proximity of the predator.  Array limited by
    !!   the index size commondata::predator_risk_group_select_index_partial.
    real(SRP), dimension(PREDATOR_RISK_GROUP_SELECT_INDEX_PARTIAL)            &
                                                    :: risk_agent_baseline

    ! The weighting coefficient adjusting the baseline predation risk for
    ! predator confusion and predator dilution effects.
    real(SRP) :: dilution_weight

    ! Local counter and minimum of the dimensions
    integer :: i, min_dim

    ! File name for debug CSV data.
    character(len=:), allocatable :: tmp_debug_file

    !> #### Checks and preparations ####
    !> Initialise index and rank values. Uninitialised index arrays may result
    !! in invalid memory reference in `ARRAY_INDEX` (it is not safe by design,
    !! see notes on the HEDTOOLS
    !! [array indexing](http://ahamodel.uib.no/doc/ar01s07.html#_subroutines_array_index_and_array_rank)
    !! procedures.
    dist_neighbours = MISSING ; dist_index = UNKNOWN
    risk_agent_rank = UNKNOWN ; risk_agent_is_visible = .FALSE.
    risk_agent_visibility = MISSING ; risk_agent_baseline = 0.0_SRP

    risk_adjusted = 0.0_SRP; risk_adjusted_indexed = 0.0_SRP

    !> Check if the optional `is_freezing` parameter array is provided. If not,
    !! it is assumed that the prey agents are NOT freezing.
    if (present(is_freezing)) then
      is_freezing_here = is_freezing
    else
      is_freezing_here = .FALSE.
    end if

    !> Check optional time step parameter. If unset, use global
    !! `commondata::global_time_step_model_current`.
    if (present(time_step_model)) then
      time_step_model_here = time_step_model
    else
      time_step_model_here = Global_Time_Step_Model_Current
    end if

    !> Check the size of the input arrays of prey agents, prey length and
    !! freezing indicztors. The sizes of all three arrays must be equal.
    if ( .not. ( size(prey_spatial) == size(prey_length) .and.                &
                 size(prey_length)  == size(is_freezing_here)  ) ) then
      !> However, if they have different sizes, log warning, this may point to
      !! a potential bug.
      call LOG_MSG( LTAG_WARN // "Unequal agent input arrays in " // PROCNAME &
                    // ": " // TOSTR(size(prey_spatial)) // ", " //           &
                    TOSTR(size(prey_length)) // ", " //                       &
                    TOSTR(size(is_freezing)) )
    end if

    !> #### Step 1 ####
    !> First, we get, up to the maximum order (fast *partial indexing*)
    !! of `commondata::predator_risk_group_select_index_partial`, neighbouring
    !! agents that are in proximity of this predator. Here we get
    !! **partial index** vector for the input array of objects: `dist_index`.
    call this%neighbours( neighbours = prey_spatial,                          &
                          dist = dist_neighbours,                             &
                          index_vector = dist_index,                          &
                          rank_max = PREDATOR_RISK_GROUP_SELECT_INDEX_PARTIAL,&
                          error_flag = err_flag )

    if (err_flag) call LOG_MSG ( LTAG_WARN // PROCNAME // ": Got error flag"  &
                    // " from conspecific objects (neighbours) procedure.")

    !>  If the `index_dist` optional parameter is present in the list of
    !!  parameters, it returns the indexing vector `dist_index`.
    if (present(index_dist)) then
      index_dist = UNKNOWN
      min_dim = min(size(index_dist),size(dist_index))
      index_dist(1:min_dim) = dist_index(1:min_dim)
    end if

    !> #### Step 2 ####
    !> Get a vector of agents sorted by their distances from the predator and
    !! calculate the baseline risk of predation for each of the agents using
    !! the the_environment::predator::risk_fish() method. The size of the array
    !! is limited by the maximum partial rank index
    !! `commondata::predator_risk_group_select_index_partial`, so only this
    !! number of nearest agents is taken into account. The other are too far
    !! at this moment and have null risk anyway (probably).
    !!
    !! The boolean flag array
    !! `risk_agent_is_visible` indicating that this `i`-th agent is within the
    !! "neighbours" group is also set to TRUE.
    !!
    !! The baseline risk of predation is the risk not taking account of the
    !! predator dilution effect by the other agent's group members in
    !! proximity.
    rank_visible = 0
    ! @warning It is not possible to use do concurrent for this loop as
    !          impure procedures are called there, e.g. illumination (can be
    !          stochastic).
    do i = 1, PREDATOR_RISK_GROUP_SELECT_INDEX_PARTIAL
      !> - First, calculate the visibility (visual range) of each neighbouring
      !!   prey agent.
      risk_agent_visibility(i) =                                              &
          prey_spatial(dist_index(i))%visibility(                             &
                        object_area =  length2sidearea_fish(cm2m(             &
                                            prey_length(dist_index(i)))),     &
                        time_step_model = time_step_model_here )

      !> - The risk of predation is non-zero only for those agents which are
      !!   located from the predator at a distance smaller than their
      !!   visibility distance, otherwise they fall outside of the visual
      !!   range of the predator.
      if ( dist_neighbours(dist_index(i)) < risk_agent_visibility(i) ) then
        !>   - Such agents are marked with the `risk_agent_is_visible` boolean
        !!     vector value TRUE.
        risk_agent_is_visible(i) = .TRUE.
        !>   - Each of such visible agent is also assigned the consecutive rank,
        !!     with the nearest agent having the rank 1 while the furtherst
        !!     agent having the rank *N* (*N* is less than the maximum indexing
        !!     rank commondata::predator_risk_group_select_index_partial).
        !!     This is illustrated by the following plot.
        !!     @image html img_doxygen_predator_grouprisk.svg "Calculation of predation risk in a group of prey agents"
        !!     @image latex img_doxygen_predator_grouprisk.eps "Calculation of predation risk in a group of prey agents" width=14cm
        !!     Here the prey agents P1, P3 and P4 are invisible to the
        !!     predator (in the centre) because their visibility ranges are
        !!     smaller that the distance towards the predator. P2 has the
        !!     rank R=1 as it has the smallest distance to the predator among
        !!     all the agents that are visible (P2, P5, P6). Notably, the
        !!     agent P1 is invisible due to small body size that leads to
        !!     very short visibility range (even though it is actually the
        !!     closest to the predator!) and therefore zero baseline
        !!     probability capture at the distance to the predator (a plot
        !!     of the relationship between the distance and the baseline
        !!     probability of capture is also overlaid at the agent P1).
        !!   .
        rank_visible = rank_visible + 1
        risk_agent_rank(i) = rank_visible
        !>   - For all visible agents, the baseline risk of predation is
        !!     calculated using the the_environment::predator::risk_fish()
        !!     method. The baseline risk of predation is the risk not taking
        !!     account of the predator confusion and dilution effect by the
        !!     other agent's group members in proximity.
        !>     @note Note that the baseline risk is zero if the distance to
        !!           the predator exceeds the visual range, it is done
        !!           automatically in the_environment::predator::risk_fish().
        !!
        !>   .
        risk_agent_baseline(i) =                                              &
            this%risk_fish( prey_spatial = prey_spatial(dist_index(i)),       &
                            prey_length = prey_length(dist_index(i)),         &
                            is_freezing = is_freezing_here(dist_index(i)),    &
                            time_step_model = time_step_model_here )
      end if
    end do

    !> - The step 2 is finalised by checking if there were any prey agents
    !!   visible to the predator. If none were visible, return from the
    !!   procedure with the consequence that all adjusted risk values are
    !!   equal to the initialisation value `risk_adjusted = 0.0`. This
    !!   situation is also logged in the DEBUG mode.
    !! .
    if ( rank_visible==0 ) then
      call LOG_DBG(LTAG_INFO // "This predator does not see any potential" // &
                   " prey agents.", PROCNAME, MODNAME)
      if (present(risk)) risk=risk_adjusted
      if (present(risk_indexed)) then
        risk_indexed = MISSING
        min_dim = min( size(risk_adjusted_indexed),size(risk_indexed) )
        risk_indexed(1:min_dim) = risk_adjusted_indexed(1:min_dim)
      end if
      return
    end if

    !> #### Step 3 ####
    !! Given that at the step 2 we have defined which of the prey agents in the
    !! group are actually visible to the predator, their rank order with
    !! respect to the distance, and the total number of such visible agents,
    !! this final step calculates the array of the adjusted risk values.
    !!
    !! This can be done in several ways:
    !! - ::adjust_risk_nonpar_noadjust() - no specific adjustment is made
    !!   for predator confusion or dilution, adjusted risk equals the baseline.
    !! - ::adjust_risk_nonpar_fixed() - fixed predator confusion/dilution
    !!   effect;
    !! - ::adjust_risk_dilute_nofirst() - all prey except the nearest have
    !!   diluted risk, on average by *1/(N-1)*, the nearest agent has the
    !!   baseline risk.
    !! - ::adjust_risk_dilute_all() - all prey except the closest have
    !!   diluted risk, on average by *1/N*.
    !! .
    !! These procedires are implemented as subroutines within this main
    !! `the_environment::predator_capture_risk_calculate_fish_group()`.
    !call adjust_risk_nonpar_noadjust()
    call adjust_risk_nonpar_fixed()
    !call adjust_risk_dilute_nofirst()
    !call adjust_risk_dilute_all()

    !> Calculate the raw indexed array of the risk that is intended to be
    !! returned (output) along with the partial index.
    do concurrent (i=1:PREDATOR_RISK_GROUP_SELECT_INDEX_PARTIAL)
      risk_adjusted_indexed(i) = risk_adjusted(dist_index(i))
    end do

    !> Finally, in the DEBUG mode also save debug data to CSV file.
    !! Below is an example data from one simulation using the fixed predation
    !! adjustment effect ::adjust_risk_nonpar_fixed().
    !!
    !! AGENT | VISIBLE | RANK  | VISIBILITY | DISTANCE  | RISK_BASE | RISK_ADJ
    !! -----:|:-------:|:-----:|:----------:|:---------:|:---------:|:--------:
    !!   P1  |    1    | 1     |   297.934  |  148.458  | 0.597635  | 0.597635
    !!   P2  |    1    | 2     |   310.591  |  238.575  | 0.375258  | 0.179290
    !!   P3  |    0    | -9999 |  *305.785* | *306.733* | 0         | 0
    !!   P4  |    0    | -9999 |  *296.327* | *309.332* | 0         | 0
    !!   P5  |    1    | 3     |   338.934  |  316.221  | 0.192204  | 0.034169
    !!   P6  |    0    | -9999 |  *283.830* | *318.537* | 0         | 0
    !!   P7  |    1    | 4     |   343.421  |  339.065  | 0         | 0
    !!   P8  |    0    | -9999 |   311.020  |  366.969  | 0         | 0
    !!   P9  |    0    | -9999 |   360.617  |  378.106  | 0         | 0
    !!  P10  |    0    | -9999 |   312.113  |  395.021  | 0         | 0
    !!  P11  |    0    | -9999 |   322.244  |  435.172  | 0         | 0
    !!  P12  |    0    | -9999 |   335.271  |  442.236  | 0         | 0
    !!  P13  |    0    | -9999 |   309.192  |  456.375  | 0         | 0
    !!  P14  |    0    | -9999 |   269.335  |  477.448  | 0         | 0
    !!  P15  |    0    | -9999 |   349.299  |  486.160  | 0         | 0
    !!  P16  |    0    | -9999 |   273.446  |  501.956  | 0         | 0
    !!  P17  |    0    | -9999 |   311.889  |  516.109  | 0         | 0
    !!  P18  |    0    | -9999 |   361.142  |  533.081  | 0         | 0
    !!  P19  |    0    | -9999 |   277.595  |  547.986  | 0         | 0
    !!  P20  |    0    | -9999 |   267.683  |  561.175  | 0         | 0
    !!
    !> @note Note that the prey agents 3 and 4 are closer to the predator than
    !!       7, but are not visible due to small visibility range (small body
    !!       sizes).
    if (IS_DEBUG) then
      tmp_debug_file = "debug_predator_dilution_" //                          &
                TOSTR(Global_Time_Step_Model_Current) // "_" //               &
                RAND_STRING(LABEL_LENGTH, LABEL_CST,LABEL_CEN) // csv

      call CSV_MATRIX_WRITE ( reshape(                                        &
               [conv_l2r(risk_agent_is_visible),                     & ! 1
                real(risk_agent_rank, kind=SRP),                     & ! 2
                risk_agent_visibility,                               & ! 3
                dist_neighbours(dist_index(                          & ! 4
                    1:PREDATOR_RISK_GROUP_SELECT_INDEX_PARTIAL)),             &
                risk_agent_baseline,                                 & ! 5
                risk_adjusted(dist_index(                            & ! 6
                    1:PREDATOR_RISK_GROUP_SELECT_INDEX_PARTIAL))],            &
               [PREDATOR_RISK_GROUP_SELECT_INDEX_PARTIAL, 6]),                &
               tmp_debug_file,                                                &
               ["IS_VISIBLE", "RANK      ", "VISIBILITY",                     &
                "DIST      ", "RISK_BASE ", "RISK_ADJ  "]                     &
              )
      call LOG_DBG( LTAG_INFO // "Saved debug predator dilution data" //      &
                    ", CSV file: " // tmp_debug_file, PROCNAME, MODNAME )
    end if

    !> Finally, calculate the optional output arrays if the are requested as
    !! optional parameters: `risk` and `risk_indexed`
    if (present(risk)) risk=risk_adjusted
    if (present(risk_indexed)) then
      risk_indexed = 0.0_SRP
      min_dim = min( size(risk_adjusted_indexed),size(risk_indexed))
      risk_indexed(1:min_dim) = risk_adjusted_indexed(1:min_dim)
    end if

    contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      !> Adjust the predation risk for confusion and dilution effects.
      !!
      !! In this version, the adjusted risk is equal to the baseline risk, i.e.
      !! there are no specific predator confusion or dilution effects.
      !!
      !! See the main container procedure
      !! `the_environment::predator_capture_risk_calculate_fish_group()`.
      subroutine adjust_risk_nonpar_noadjust()

        !> ### Implementation details ###
        do concurrent (i = 1 : PREDATOR_RISK_GROUP_SELECT_INDEX_PARTIAL)
          if ( risk_agent_is_visible(i) )  then
            !> The adjusted risk of predation in this version is simplistic
            !! and just equals the baseline risk. So no adjustment is actually
            !! made.
            !!
            !> Note that the adjusted risk is calculated only for a small
            !! subarray within the potentially huge input array of spatial prey
            !! agents, commondata::predator_risk_group_select_index_partial
            !! maximum elements. All other values are nulls.
            risk_adjusted(dist_index(i))=risk_agent_baseline(i)
          end if
        end do

      end subroutine adjust_risk_nonpar_noadjust

      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !> Adjust the predation risk for confusion and dilution effects.
      !!
      !! This version is based on a fixed pattern linking the rank of the
      !! prey agent within the predator's visual field and the adjusted risk.
      !! However, the basic pattern scales independently on the specific
      !! number of prey agents that the predator sees.
      !!
      !! See the main container procedure
      !! `the_environment::predator_capture_risk_calculate_fish_group()`.
      subroutine adjust_risk_nonpar_fixed()

        !> PROCNAME is the procedure name for logging and debugging
        character(len=*), parameter :: PROCNAME = "(adjust_risk_nonpar_fixed)"

        !> ### Notable variables ###
        !> - **predator_risk_group_dilution_abscissa** is the abscissa for the
        !!   non-parametric function that links the baseline unadjusted
        !!   predation risk for any prey agent within a group and the risk
        !!   estimate that takes account of predator confusion and predator
        !!   dilution effects. The ordinate of the grid is defined by the
        !!   parameter array commondata::predator_risk_group_dilution_ordinate.
        !! .
        real(SRP), dimension(size(PREDATOR_RISK_GROUP_DILUTION_ORDINATE))     &
                                      :: predator_risk_group_dilution_abscissa

        !> ### Implementation details ###
        !> The grid abscissa for the nonparametric function (see below)
        !! depends on the number visible prey agents within the visual field
        !! of the predator *N*. It is constructed as a 3-element array (the
        !! second point is a middle interval):
        !! [ 1.0, 1 + (*N-1*/2.0), *N* ].
        !! The ordinate of the grid is defined by the parameter array
        !! commondata::predator_risk_group_dilution_ordinate.
        predator_risk_group_dilution_abscissa =                               &
                        [ 1.0_SRP,                                            &
                          1.0_SRP + (real(rank_visible - 1, SRP)/2.0_SRP),    &
                          real(rank_visible, SRP) ]

        do concurrent (i = 1 : PREDATOR_RISK_GROUP_SELECT_INDEX_PARTIAL)
          if ( risk_agent_is_visible(i) )  then
            !>   - The estimate of the adjusted predation risk (@f$ R_{a} @f$)
            !!     for each agent in proximity to this predator then depends
            !!     on the rank order (@f$ r @f$) of the agent in within the
            !!     visual field of the  predator:
            !!     @f[ R_{a} = R_{b} \cdot \varsigma(r) , @f] where
            !!     @f$ R_{b} @f$ is unadjusted risk and @f$ \varsigma @f$ is a
            !!     non-parametric weighting function that depends on the rank
            !!     order @f$ r @f$ of the prey agent.
            !!     The weighting function @f$ \varsigma(r) @f$ is in turn
            !!     calculated as a nonlinear nonparametric function defined by
            !!     the grid arrays:
            !!     - `predator_risk_group_dilution_abscissa`
            !!     - `commondata::predator_risk_group_dilution_ordinate`.
            !!     .
            !!
            !!     In this way, the first prey agent (closest distance) is
            !!     subject to the predation risk equal to the baseline
            !!     unadjusted risk, whereas the last prey agent (furtherest
            !!     from the predator in the group) has fully diluted predation
            !!     risk equal to zero. The agent with the middle rank in the
            !!     group has the adjusted risk somewhere in between the
            !!     unadjusted risk and null. Thus, the predator confusion and
            !!     dilution effects are relative and depend on the agent's
            !!     position with respect to the predator.
            !!     @image html img_doxy_pred_risk_group_nonpar.svg "Nonparametric predator confusion/dilution factor"
            !!     @image latex img_doxy_pred_risk_group_nonpar.eps "Nonparametric predator confusion/dilution factor" width=14cm
            !>     Nonetheless, the pattern is independent on the specific
            !!     number of prey agents in the group, e.g. it is the same for
            !!     3 and 9 agents.
            dilution_weight=DDPINTERPOL( predator_risk_group_dilution_abscissa,&
                                   PREDATOR_RISK_GROUP_DILUTION_ORDINATE,     &
                                   real(risk_agent_rank(i), SRP) )
            !>     Interpolation plots can be saved in the
            !!     @ref intro_debug_mode "debug mode" using the
            !!     command: `commondata::debug_interpolate_plot_save()`.
            !!     @warning Involves **huge** number of plots, should
            !!              normally be disabled.
            !!     @warning This is **disabled** (commented out) here to allow
            !!              parallel `do concurrent` construction. If debug
            !!              plots are enabled, `do concurrent` has to be
            !!              altered to normal `do`.
            !!
            !>   .
            !> .
           !call debug_interpolate_plot_save(                                 &
           !    grid_xx=predator_risk_group_dilution_abscissa,                &
           !    grid_yy=PREDATOR_RISK_GROUP_DILUTION_ORDINATE,                &
           !    ipol_value=real(rank_visible, SRP), algstr="DDPINTERPOL",     &
           !    output_file="plot_debug_predator_dilution_" //                &
           !                TOSTR(Global_Time_Step_Model_Current) // "_" //   &
           !                RAND_STRING(LABEL_LENGTH, LABEL_CST,LABEL_CEN) &
           !                // PS )

            !> Note that the adjusted risk is calculated only for a small
            !! subarray within the potentially huge input array of spatial prey
            !! agents, commondata::predator_risk_group_select_index_partial
            !! maximum elements. All other values are nulls.
            risk_adjusted(dist_index(i))=risk_agent_baseline(i)*dilution_weight
          end if
        end do

      end subroutine adjust_risk_nonpar_fixed

      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !> Adjust the predation risk of a group of *N* prey agents for predator
      !! dilution effect.
      !!
      !! In this version, the nearest agent has a high risk equal to the
      !! baseline unadjusted risk whereas the other agents in the group have
      !! the risk diluted, on average, by their total number *N-1* (the
      !! nearest agent is excluded). The risks of the non-nearest agents
      !! still depend on the individual rank order. Whereas the average
      !! adjusted risk for all non-rank-1 agents is equal to *1/(N-1)*,
      !! each of the agents has specific risk. For the rank-2 agent it is
      !! @f$ R_{b} \cdot 2/(N-1) @f$ while for the furtherest agent the risk
      !! is zero: @f$ R_{b} \cdot 0.0 @f$.
      !!
      !! See the main container procedure
      !! `the_environment::predator_capture_risk_calculate_fish_group()`.
      subroutine adjust_risk_dilute_nofirst()

        !> ### Notable variables ###
        !> - **predator_risk_dilution** is the predator dilution weighting
        !!   factor array. It is selected such that the average value over
        !!   the whole array is *1/(N-1)* where *N* is the total number of
        !!   prey agents in the group including the nearest agent.
        !! .
        real(SRP), allocatable, dimension(:) :: predator_risk_dilution

        !> ### Implementation details ###
        !> Calculate the array of dilution factor that is equally linearly
        !! spaced from 2/(N-1) to 0.0 (the furtherest agent). The average
        !! dilution factor for this group is therefore *1/(N-1)*.
        if (rank_visible == 1) then
          !> Nonetheless, if there is only one prey agent, its risk
          !! is calculated as the full baseline risk @f$ R_{b} @f$.
          risk_adjusted(dist_index(1)) = risk_agent_baseline(1)
          return
        elseif (rank_visible == 2) then
          !> If, on the other hand, only two prey agents are visible to
          !! the predator, the first (nearest, rank 1) gets the baseline
          !! risk @f$ R_{b} @f$ and the second @f$ R_{b} / 2.0  @f$.
          risk_adjusted(dist_index(1)) = risk_agent_baseline(1)
          risk_adjusted(dist_index(2)) = risk_agent_baseline(2) / 2.0_SRP
          return
        else
          !> The `LINSPACE()` procedure for generating linearly equally
          !! spaced array from HEDTOOLS is used for calculation of the
          !! dilution factor array `predator_risk_dilution`.
          allocate(predator_risk_dilution(rank_visible-1))
          predator_risk_dilution = LINSPACE( 2.0_SRP/(rank_visible-1),        &
                                             0.0_SRP,                         &
                                             (rank_visible-1) )
          call LOG_DBG( LTAG_INFO // "Predator dilution factor array: " //    &
                        TOSTR(predator_risk_dilution), PROCNAME, MODNAME )
        end if

        do concurrent (i = 1 : PREDATOR_RISK_GROUP_SELECT_INDEX_PARTIAL)
          if ( risk_agent_is_visible(i) )  then
            if ( risk_agent_rank(i) == 1 ) then
              !>   - The adjusted risk is equal to the baseline risk for the
              !!     prey agent that has the rank one, i.e. the closest to the
              !!     predator.
              risk_adjusted(dist_index(i)) = risk_agent_baseline(i)
            else
              !>   - However, for all other agents in the group the adjusted
              !!     risk is diluted by the remaining group size (i.e.
              !!     excluding the nearest agent **N-1**).
              !!     @image html img_doxy_pred_risk_group_dilute.svg "Adjustment of the predation risk by dilution factor"
              !!     @image latex img_doxy_pred_risk_group_dilute.eps "Adjustment of the predation risk by dilution factor" width=14cm
              !!     In the example plot above, the baseline risk for the
              !!     nearest prey agent in a group of 6 is 0.7, it is unchanged.
              !!     But the risk is diluted for all the remaining (rank>1)
              !!     prey agents on average by *6-1=5*, even though it is still
              !!     dependent on their rank by a linearly evenly spaced
              !!     dilution factor `predator_risk_dilution` (the plot also
              !!     for simplicity assumes the baseline risk is also
              !!     @f$ R_{b}=0.7 @f$ and is identical for all agents; in
              !!     real data, the linearly spaced factor is used as a weight
              !!     for specific baseline risk values @f$ R_{b} @f$, so the
              !!     pattern deviates from the perfect straight line).
              !!   .
              risk_adjusted(dist_index(i)) =  risk_agent_baseline(i) *        &
                                   predator_risk_dilution(risk_agent_rank(i)-1)

            end if
          end if
        end do

      end subroutine adjust_risk_dilute_nofirst

      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !> Adjust the predation risk of a group of *N* prey agents for predator
      !! dilution effect.
      !!
      !! In this version, all prey agents in the group have
      !! the risk diluted, on average, by their total number *N*. The risks
      !! of the agents are not fixed and depend on the individual rank order.
      !! Whereas the average adjusted risk is equal to *1/(N)*,
      !! each of the agents has specific risk. For the rank-1 agent it is
      !! @f$ R_{b} \cdot 2/N @f$ while for the furtherest agent the risk
      !! is zero: @f$ R_{b} \cdot 0.0 @f$.
      !!
      !! See the main container procedure
      !! `the_environment::predator_capture_risk_calculate_fish_group()`.
      subroutine adjust_risk_dilute_all()

        !> ### Notable variables ###
        !> - **predator_risk_dilution** is the predator dilution weighting
        !!   factor array. It is selected such that the average value over
        !!   the whole array is *1/N* where *N* is the total number of
        !!   prey agents in the group including the nearest agent.
        !! .
        real(SRP), allocatable, dimension(:) :: predator_risk_dilution

        !> ### Implementation details ###
        !> Calculate the array of dilution factor that is equally linearly
        !! spaced from 2/N (nearest agent) to 0.0 (the furtherest agent).
        !! The average dilution factor for this group is therefore *1/N*.
        if (rank_visible == 1) then
          !> Nonetheless, if there is only one prey agent, its risk
          !! is calculated as the full baseline risk @f$ R_{b} @f$.
          risk_adjusted(dist_index(1)) = risk_agent_baseline(1)
          return
        elseif (rank_visible == 2) then
          !> If, on the other hand, only two prey agents are visible to
          !! the predator, they both get the same risk equal to a half of
          !! baseline @f$ R_{b} / 2.0  @f$.
          risk_adjusted(dist_index(1)) = risk_agent_baseline(1) / 2.0_SRP
          risk_adjusted(dist_index(2)) = risk_agent_baseline(2) / 2.0_SRP
          return
        else
          !> The `LINSPACE()` procedure for generating linearly equally
          !! spaced array from HEDTOOLS is used for calculation of the
          !! dilution factor array `predator_risk_dilution`.
          allocate(predator_risk_dilution(rank_visible))
          predator_risk_dilution = LINSPACE( 2.0_SRP/rank_visible,            &
                                             0.0_SRP,                         &
                                             rank_visible )
          call LOG_DBG( LTAG_INFO // "Predator dilution factor array: " //    &
                        TOSTR(predator_risk_dilution), PROCNAME, MODNAME )
        end if

        do concurrent (i = 1 : PREDATOR_RISK_GROUP_SELECT_INDEX_PARTIAL)
          if ( risk_agent_is_visible(i) )  then
            !>   - The average adjusted risk is diluted by the group size *N*.
            !!     However, individual prey agents have the adjusted risk
            !!     values that are weighted by the linearly equally spaced
            !!     dilution value `predator_risk_dilution`. Thus, even though
            !!     individual values of the adjusted risk are ranked by the
            !!     distance from the predator, their average values for the
            !!     whole group is diluted by the group size *N*.
            !!   .
            risk_adjusted(dist_index(i)) = risk_agent_baseline(i) *           &
                                     predator_risk_dilution(risk_agent_rank(i))
          end if
        end do

      end subroutine adjust_risk_dilute_all

  end subroutine predator_capture_risk_calculate_fish_group

  !-----------------------------------------------------------------------------
  !> Calculate the visibility range of this predator. Wrapper to the
  !! the_environment::visual_range() function. This function calculates the
  !! distance from which this predator can be seen by a visual object
  !! (e.g. the agent).
  !! @warning The `visual_range` procedures use meter for units, this
  !!          auto-converts to cm.
  !! @warning Cannot implement a generic function accepting also vectors of
  !!          this objects as only elemental object-bound array functions are
  !!          allowed by the standard. This function cannot be elemental, so
  !!          passed-object dummy argument must always be scalar.
  function predator_visibility_visual_range(this, object_area, contrast,      &
                                            time_step_model)  result (visrange)
    class(PREDATOR), intent(in) :: this
    !> @param[in] object_area optional area of the spatial object, m.
    !!            if not provided (normally), calculated from the
    !!            the_environment::predator::body_size attribute of this
    !!            predator object.
    real(SRP), optional, intent(in) :: object_area
    !> @param[in] contrast is optional inherent visual contrast of the predator.
    !!            the default contrast of all objects is defined by the
    !!            commondata::preycontrast_default parameter.
    real(SRP), optional, intent(in) :: contrast
    !> @param[in] optional time step of the model, if absent gets the current
    !!            time step as defined by the value of
    !!            `commondata::global_time_step_model_current`.
    integer, optional, intent(in) :: time_step_model
    !> @return The maximum distance from which this predator can be seen.
    real(SRP) :: visrange

    ! Local copies of optionals
    real(SRP) :: object_area_here, contrast_here

    ! Local variables
    real(SRP) :: irradiance_agent_depth
    integer :: time_step_model_here

    !> ### Implementation details ###
    !! **Checks.** Check optional object area, the default value, if
    !! this parameter is absent, the area is calculated from the
    !! the_environment::predator::body_size attribute of the predator
    !! object with inline conversion to m. Note that the body side area
    !! of a fish object is calculated from the body length using the
    !! commondata::length2sidearea_fish() function.
    if (present(object_area)) then
      object_area_here = object_area
    else
      object_area_here = length2sidearea_fish( cm2m( this%body_size ) )
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
    !! this predator object at the given time step using the
    !! the_environment::spatial::illumination() method.
    irradiance_agent_depth =  this%illumination(time_step_model_here)

    !> Return visual range for (to detect) this predator using
    !! the_environment::visual_range() wrapper function.
    visrange =                                                                &
        m2cm( visual_range ( irradiance = irradiance_agent_depth,             &
                             prey_area = object_area_here,                    &
                             prey_contrast = contrast_here )  )

  end function predator_visibility_visual_range

  !-----------------------------------------------------------------------------
  !> Calculates the average nearest neighbour distance amongst an array of
  !! spatial objects (class) by sampling `sample_size` of them. The sample size
  !! `sample_size` is optional, if not provided set to `SAMPLE_SIZE_DEFAULT=25`.
  !! @param spatial_objects An array of spatial class objects for which we
  !!        calculate the average nearest neighbour distance.
  !! @param sample_size Optional sample size for the calculation.
  !! @returns Returns a (sample-based) estimate of the mean nearest neighbour
  !!          distance among an array of the spatial objects.
  function distance_average(spatial_objects, sample_size)                     &
                                                            result(mean_nndist)

    ! @param spatial_objects An array of spatial class objects for which we
    !        calculate the average nearest neighbour distance.
    class(SPATIAL), dimension(:), intent(in) :: spatial_objects

    ! @param sample_size Optional sample size for the calculation.
    integer, optional, intent(in) :: sample_size

    ! @returns Returns a (sample-based) estimate of the mean nearest neighbour
    !          distance among an array of the spatial objects.
    real(SRP) :: mean_nndist

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(distance_average)"

    !> ### Notable local variables ###
    !> - **SAMPLE_SIZE_DEFAULT** is the local default sample size
    !! - **SAMPLE_SIZE_WARN** is the minimum warning sample size that results
    !!   in warning logging.
    integer, parameter :: SAMPLE_SIZE_DEFAULT=25, SAMPLE_SIZE_WARN=20

    !> - **MAX_ARRAY_DIMENSIONALITY**: the maximum dimensionality of the input
    !!   array of
    !!   spatial objects for doing full non-sampling based calculations.
    !!   @details Maximum size of the input array of spatial objects for full
    !!          element by element selection without random sampling. This
    !!          maximum dimensionality can be obtained by solving this
    !!          square equation: @f[ M = x^{2}+x , @f] where @f$ M @f$ is
    !!          the maximum number of permutations. So, the maximum
    !!          dimensionality of the input array of objects is
    !!          @f[ \frac{\sqrt{4\cdot M + 1} + 1}{2} . @f] The value obtained
    !!          is then rounded to the nearest whole integer using `nint`.
    !!   @note  Note that for the standard default `SAMPLE_SIZE_DEFAULT=25`,
    !!          `MAX_ARRAY_DIMENSIONALITY` equals 6.
    !! .
    integer, parameter :: MAX_ARRAY_DIMENSIONALITY = nint(                    &
            (sqrt(4.0_SRP * SAMPLE_SIZE_DEFAULT + 1) + 1.0_SRP) / 2.0_SRP )

    ! Local variables.
    integer :: N                        ! Local sample size.
    integer :: i, j, k, perm            ! Counters.

    ! Extra long local integers, needed to fit after exponentiation.
    integer(LONG) :: array_size       ! Input spatial objects array size.
    integer(LONG) :: max_permutations ! Maximum number of permutations.

    ! Randomly sampled spatial object
    type(SPATIAL) :: spatial_object_sampled

    ! Array of the original spatial objects excluding the above randomly
    ! sampled object
    type(SPATIAL), dimension(size(spatial_objects)-1) :: spatial_all_other

    ! Nearest neighbour of the above randomly sampled object
    type(SPATIAL) :: spatial_object_nearest_neighbour

    if (present(sample_size)) then
      N = sample_size
    else
      N = SAMPLE_SIZE_DEFAULT
    end if

    ! Set the array size from its dimension.
    array_size = size(spatial_objects)

    ! Maximum number of full cross pairwise permutations.
    ! @note Note that integers are declared as `kind=LONG` so they can fit
    !       the very big numbers resulting from the exponentiation if the
    !       number of objects is big.
    max_permutations = ((array_size**2)-array_size)

    !> ### Implementation details ###
    !> Check the array size. Big and small arrays are treated differently.
    ARRAY_SIZE_TREAT: select case (array_size)

      !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      !> If the array size is zero or one, there is no sense calculating
      !! average nearest neighbour distance, just return zero and log warning.
      case (0:1)

        call LOG_MSG( LTAG_WARN // PROCNAME // ": too small array size " //   &
                  TOSTR(int(array_size)) // ",  returned " // PROCNAME //     &
                  " value is ZERO." )
        mean_nndist = 0.0_SRP

      !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      !> If the array size is small enough, do full element by element
      !! selection and permutation. We do not then really use the
      !! `sample_size` (or `N`) parameter.
      case (2:MAX_ARRAY_DIMENSIONALITY)

        call LOG_MSG(LTAG_INFO // PROCNAME // ": object array size is " //    &
                    "small enough for quick full object by object " //        &
                    "calculation; Random object sampling is NOT used." )

        ! Initialise the output mean distance with zero.
        mean_nndist = 0.0_SRP

        !> Do full N x N permutations in such a case.
        !! @note Note that we here do full crossing rather than half cutting
        !!       `j=i+1,array_size`. Nearest neighbour distance are **not**
        !!       transitive.
        BASE_OBJ: do i = 1, array_size

          ! Get the first spatial object for which we calculate the distances
          call spatial_object_sampled%position( spatial_objects(i)%location() )
          ! Get an array of all other objects excluding the already sampled.
          k = 0
          do j = 1, array_size
            if (i /= j) then
              k = k + 1
              call spatial_all_other(k)%position(spatial_objects(j)%location())
            end if
          end do

          ! Then we find the nearest neighbour of the i-th object from the input
          ! array of spatial objects.
          spatial_object_nearest_neighbour =                                  &
                            spatial_object_sampled%nearest( spatial_all_other )

          ! Calculate the distance between the consecutive ith object
          ! and its nearest neighbour `spatial_object_nearest_neighbour` and
          ! update the overall sum.
          mean_nndist = mean_nndist + spatial_object_sampled%distance(        &
                                              spatial_object_nearest_neighbour )
        end do BASE_OBJ

        ! Finally, calculate the mean distance over all these permutations.
        mean_nndist = mean_nndist / real(array_size, SRP)

      !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      !> In all other cases do random sampling from the input array of spatial
      !! objects and calculate sample-based average nearestneighbour distance.
      case default

        !> Set the maximum sample size not exceeding 1/2 of all possible
        !! permutations in case the input object array is relatively large.
        !! The default value 1/2 of all possible permutations may be quite big
        !! with a huge array, but the logic behind the decision is that the
        !! requested sample size was also big.
        !! @note  Note that we have to **convert** `integer(LONG)` to the
        !!        default `integer` using the intrinsic function `int` to make
        !!        it acceptable for the `TOSTR` function from HEDTOOLS.
        !!        Hopefully the final `max_permutations` would not be too huge.
        if (N > max_permutations / 2) then
            call LOG_MSG( LTAG_WARN // PROCNAME //                            &
                ": requested sample size " //                                 &
                TOSTR(N) // " exceeds 1/2 maximum number of permutations " // &
                TOSTR(int(max_permutations)) // ", use 1/2 of the latter.")
            N = max_permutations / 2
        end if

        !> If the requested sample size is very small, just do a warning. The
        !! logic behind NOT changing N to, e.g. `SAMPLE_SIZE_DEFAULT` is that
        !! if a small N was requested, this must have serious grounds, e.g.
        !! if the agent is basing its decision making process on an incomplete
        !! information. So the small value is left as small as requested.
        if (N < SAMPLE_SIZE_WARN)                                             &
        call LOG_MSG( LTAG_WARN // PROCNAME // ": requested sample size " //  &
                TOSTR(N) // " is quite small, average value may be imprecise.")


        !> Initialise the output mean distance with zero.
        mean_nndist = 0.0_SRP

        !> Do permutations, using `PERMUTE` named do-block:
        PERMUTE: do perm = 1, N
          !> - We sample a single random spatial objects from the available
          !!   array.
          !!   @note Note that we have to **convert** `integer(LONG)` to the
          !!         default`integer` using the intrinsic function `int` to
          !!         make it acceptable for the `RAND_I` function from HEDTOOLS.
          !!         Hopefully the final `array_size` would not be too huge.
          i = RAND_I(1,int(array_size))
          call spatial_object_sampled%position( spatial_objects(i)%location() )

          !> - Get an array of all other objects excluding the already sampled.
          !!   @note There seems to be no class-safe way to do whole-array
          !!         assignments of `SPATIAL` objects for high speed, e.g.
          !!         using the index slice:
          !!         `[ [(j, j=1,i-1)], [(j,j=i+1,array_size)] ]` as we use the
          !!         type-bound function `location` and `position` to copy
          !!         arays. So using the explicit old-fashioned do-loop here.
          k = 0
          do j=1, array_size
            if (i /= j) then
              k = k + 1
              call spatial_all_other(k)%position(spatial_objects(j)%location())
            end if
          end do

          !> - Then we find the nearest neighbour of the i-th object out
          !!   from the input array of spatial objects.
          spatial_object_nearest_neighbour =                                  &
                            spatial_object_sampled%nearest( spatial_all_other )

          !> - Calculate the distance between the (randomly selected) ith object
          !!   and its nearest neighbour `spatial_object_nearest_neighbour` and
          !!   update the overall sum.
          !! .
          mean_nndist = mean_nndist + spatial_object_sampled%distance(        &
                                              spatial_object_nearest_neighbour )
        end do PERMUTE

        !> Finally, calculate the mean distance over all these permutations.
        mean_nndist = mean_nndist / real(N, SRP)

    end select ARRAY_SIZE_TREAT

  end function distance_average

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !> @name Computational geometry backend: **polygon2D**
  !> Rudimentary *computational geometry* (geo_) procedures, based on
  !! *2D polygons* (poly2d) with fixed depth or depth ignored.
  !! @note Manually constructed using 2D *X* x *Y* vectors ignoring the *depth*
  !!       dimension.
  !> @{

  !-----------------------------------------------------------------------------
  !> Calculates the minimum distance from a the_environment::spatial class
  !! object to a line segment delimited by two the_environment::spatial class
  !! endpoints in the 2D *XY* plane (the depth coordinate is ignored).
  !! (The algorithm is partially based on
  !! [this](https://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment).)
  subroutine geo_poly2d_dist_point_to_section(  point, sectp1, sectp2,        &
                                                min_dist, point_segment  )
    !> @param[in] point is the reference point to which the distance is
    !!            calculated
    class(SPATIAL), intent(in) :: point
    !> @param[in] sectp1 is the first end of the line segment.
    class(SPATIAL), intent(in) :: sectp1
    !> @param[in] sectp2 is the second end point of the line segment.
    class(SPATIAL), intent(in) :: sectp2
    !> @param[out] min_dist is the output minimum distance between the
    !!         reference point and the line segment delimited by `sectp1`
    !!         and `sectp2`.
    real(SRP), intent(out) :: min_dist
    !> @param[out] point_segment is the optional output coordinates of the
    !!         nearest point **PN** on the **P1 P2** segment returned in the
    !!         form of the_environment::spatial type object. But note that
    !!         the third `depth` coordinate of this object is copied from
    !!         the input `point` object.
    type(SPATIAL), optional, intent(out) :: point_segment

    ! Local variables
    real(SRP) :: refval, dist2_p1_p2

    !> ### Implementation details ###
    !> A scheme of the calculation is presented on this figure:
    !! @image html img_doxygen_dist_segment.svg"Calculation of the distance from a spatial object to a line segment"
    !! @image latex img_doxygen_dist_segment.eps "Calculation of the distance from a spatial object to a line segment" width=14cm
    !! First, calculate the squared distance between the end points of
    !! the line  segment **P1** and **P2** using the backend function
    !! the_environment::dist2_vector() that accepts vectors of arbitrary
    !! dimensionality: @f$ D^{2}(\mathbf{p1},\mathbf{p2}) @f$
    dist2_p1_p2 = dist2_vector( [sectp1%x, sectp1%y], [sectp2%x, sectp2%y] )

    !> - if the distance between the end points is zero, the line segment
    !!   actually has zero length and the distance between the spatial object
    !!   **P0** and the segment is trivial to determine:
    !!   @f$ D_{min} = D(\mathbf{p_0},\mathbf{p_1}) = D(\mathbf{p_0},\mathbf{p_2}) @f$.
    !! .
    if ( dist2_p1_p2 < ZERO ) then
      min_dist = dist( [point%x, point%y], [sectp1%x, sectp1%y] )
      return
    end if

    !> Second, determine the reference value @f$ r @f$ (a normalised distance
    !! from **P1** to the closest point) that is calculated as follows:
    !! @f[ r = \frac{\left|\mathbf{p}_{0} - \mathbf{p}_{1}\right| \cdot \left|\mathbf{p}_{2} - \mathbf{p}_{1}\right|}{D^{2}(\mathbf{p_1},\mathbf{p_2})} , @f]
    !! where @f$ D^{2}(\mathbf{p_1},\mathbf{p_2}) @f$ is the distance between
    !! the end points **P1** and **P2**, and the numerator is the dot product
    !! of the vectors @f$ \left|\mathbf{p}_{0} - \mathbf{p}_{1}\right| @f$ and
    !! @f$ \left|\mathbf{p}_{2} - \mathbf{p}_{1}\right| @f$.
    refval = dot_product( [point%x,point%y]-[sectp1%x, sectp1%y],             &
                          [sectp2%x,sectp2%y]-[sectp1%x, sectp1%y] )/dist2_p1_p2

    !> - The @f$ r < 0 @f$ indicates that the projection of the spatial object
    !!   **P0** onto the **P1 P2** line is located in front of the **P1**
    !!   end, thus the minimum distance is calculated as
    !!   @f[ D_{min} = D(\mathbf{p_0},\mathbf{p_1}) . @f] Also, the nearest
    !!   point **PN** coordinates coincide with the *X* and *Y* coordinates of
    !!   **P1**.
    if ( refval < 0.0_SRP ) then
      min_dist = dist( [point%x, point%y], [sectp1%x, sectp1%y] )
      if (present(point_segment)) then
        point_segment = SPATIAL( sectp1%x, sectp1%y, point%depth )
      end if
    !> - If @f$ r > 1 @f$, the projection of the spatial object **P0** on the
    !!   line **P1 P2** is behind the **P2** end, so the minimum distance is
    !!   @f[ D_{min} = D(\mathbf{p_0},\mathbf{p_2}) . @f] Also, the nearest
    !!   point **PN** coordinates coincide with the *X* and *Y* coordinates of
    !!   **P2**.
    else if ( refval > 1.0_SRP ) then
      min_dist = dist( [point%x, point%y], [sectp2%x, sectp2%y] )
      if (present(point_segment)) then
        point_segment = SPATIAL( sectp2%x, sectp2%y, point%depth )
      end if
    !> - In other cases, the spatial object **P0** projects to a specific point
    !!   on the **P1 P2** line. The distance between the spatial object **P0**
    !!   and this projection is calculated as:
    !!   @f[ D_{min}= \frac{\left | (y_2-y_1)x_0-(x_2-x_2)y_0+x_2y_1-y_2x_1 \right |}{D^2(\mathbf{p_1},\mathbf{p_2})} , @f]
    !!   where @f$ (x_0, y_0) @f$ are the coordinates of the spatial object
    !!   **P0**, @f$ (x_1, y_1) @f$ are the coordinates of the point **P1**
    !!   and @f$ (x_2, y_2) @f$ are the coordinates of the point **P2**.
    !!   The nearest point **PN** coordinates are calculated as
    !!   @f[ \left | \mathbf{p_1}+(\mathbf{p_2}-\mathbf{p_1}) r \right | . @f]
    !! .
    else
      min_dist =                                                              &
        abs( (sectp2%y-sectp1%y)*point%x - (sectp2%x-sectp1%x)*point%y +      &
              sectp2%x*sectp1%y - sectp2%y*sectp1%x ) / sqrt( dist2_p1_p2 )
      if (present(point_segment)) then
        point_segment = SPATIAL( sectp1%x + (sectp2%x-sectp1%x) * refval,     &
                                 sectp1%y + (sectp2%y-sectp1%y) * refval,     &
                                 point%depth  )
      end if
    end if

  end subroutine geo_poly2d_dist_point_to_section

  !-----------------------------------------------------------------------------
  !> Calculates the minimum distance from a the_environment::spatial class
  !! object to a line segment delimited by two the_environment::spatial class
  !! endpoints in the 3D *XY* space.
  !! (The algorithm is partially based on
  !! [this](https://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment).)
  subroutine geo_poly3d_dist_point_to_section(  point, sectp1, sectp2,        &
                                                min_dist, point_segment  )
    !> @param[in] point is the reference point to which the distance is
    !!            calculated
    class(SPATIAL), intent(in) :: point
    !> @param[in] sectp1 is the first end of the line segment.
    class(SPATIAL), intent(in) :: sectp1
    !> @param[in] sectp2 is the second end point of the line segment.
    class(SPATIAL), intent(in) :: sectp2
    !> @param[out] min_dist is the output minimum distance between the
    !!         reference point and the line segment delimited by `sectp1`
    !!         and `sectp2`.
    real(SRP), intent(out) :: min_dist
    !> @param[out] point_segment is the optional output coordinates of the
    !!         nearest point **PN** on the **P1 P2** segment returned in the
    !!         form of the_environment::spatial type object.
    type(SPATIAL), optional, intent(out) :: point_segment

    ! Local variables
    real(SRP) :: refval, dist2_p1_p2

    ! Local copy of optional
    type(SPATIAL) :: point_segment_loc

    !> ### Implementation details ###
    !> A scheme of the calculation is presented on this figure:
    !! @image html img_doxygen_dist_segment.svg"Calculation of the distance from a spatial object to a line segment"
    !! @image latex img_doxygen_dist_segment.eps "Calculation of the distance from a spatial object to a line segment" width=14cm
    !! First, calculate the squared distance between the end points of
    !! the line segment **P1** and **P2** using the backend function
    !! the_environment::dist2_vector() that accepts vectors of arbitrary
    !! dimensionality: @f$ D^{2}(\mathbf{p1},\mathbf{p2}) @f$
    dist2_p1_p2 = dist2_vector( [sectp1%x, sectp1%y, sectp1%depth],           &
                                [sectp2%x, sectp2%y, sectp2%depth] )

    !> - if the distance between the end points is zero, the line segment
    !!   actually has zero length and the distance between the spatial object
    !!   **P0** and the segment is trivial to determine:
    !!   @f$ D_{min} = D(\mathbf{p_0},\mathbf{p_1}) = D(\mathbf{p_0},\mathbf{p_2}) @f$.
    !! .
    if ( dist2_p1_p2 < ZERO ) then
      min_dist = dist( [point%x, point%y, point%depth],                       &
                       [sectp1%x, sectp1%y, sectp1%depth] )
      return
    end if

    !> Second, determine the reference value @f$ r @f$ (a normalised distance
    !! from **P1** to the closest point) that is calculated as follows:
    !! @f[ r = \frac{\left|\mathbf{p}_{0} - \mathbf{p}_{1}\right| \cdot \left|\mathbf{p}_{2} - \mathbf{p}_{1}\right|}{D^{2}(\mathbf{p_1},\mathbf{p_2})} , @f]
    !! where @f$ D^{2}(\mathbf{p_1},\mathbf{p_2}) @f$ is the distance between
    !! the end points **P1** and **P2**, and the numerator is the dot product
    !! of the vectors @f$ \left|\mathbf{p}_{0} - \mathbf{p}_{1}\right| @f$ and
    !! @f$ \left|\mathbf{p}_{2} - \mathbf{p}_{1}\right| @f$.
    refval = dot_product( [point%x,point%y,point%depth]-                      &
                                [sectp1%x,sectp1%y,sectp1%depth],             &
                          [sectp2%x,sectp2%y,sectp2%depth]-                   &
                                [sectp1%x,sectp1%y,sectp1%depth] ) /          &
             dist2_p1_p2

    !> - The @f$ r < 0 @f$ indicates that the projection of the spatial object
    !!   **P0** onto the **P1 P2** line is located in front of the **P1**
    !!   end, thus the minimum distance is calculated as
    !!   @f[ D_{min} = D(\mathbf{p_0},\mathbf{p_1}) . @f]
    if ( refval < 0.0_SRP ) then
      min_dist = dist( [point%x, point%y, point%depth],                       &
                       [sectp1%x, sectp1%y, sectp1%depth] )
      if (present(point_segment)) then
        point_segment = SPATIAL( sectp1%x, sectp1%y, sectp1%depth )
      end if
    !> - If @f$ r > 1 @f$, the projection of the spatial object **P0** on the
    !!   line **P1 P2** is behind the **P2** end, so the minimum distance is
    !!   @f[ D_{min} = D(\mathbf{p_0},\mathbf{p_2}) . @f]
    else if ( refval > 1.0_SRP ) then
      min_dist = dist( [point%x, point%y, point%depth],                       &
                       [sectp2%x, sectp2%y, sectp2%depth] )
      if (present(point_segment)) then
        point_segment = SPATIAL( sectp2%x, sectp2%y, sectp2%depth )
      end if
    !> - In other cases, the spatial object **P0** projects to a specific point
    !!   on the **P1 P2** line, that has these spatial coordinates:
    !    @f[
    !        \left\{\begin{matrix}
    !        x_1 + r (x_2 - x_1), \\
    !        y_1 + r (y_2 - y_1), \\
    !        z_1 + r (z_2 - z_1)
    !        \end{matrix}\right.
    !    @f]
    !>   @image html  img_doxygen_dist_point3d_formula_1.svg
    !!   @image latex img_doxygen_dist_point3d_formula_1.eps "" width=14cm
    !!   It is then trivial to calculate the distance between the spatial
    !!   object **P0** and this projection point.
    !! .
    else
      point_segment_loc =                                                     &
            SPATIAL( sectp1%x     + (sectp2%x     - sectp1%x)     * refval,   &
                     sectp1%y     + (sectp2%y     - sectp1%y)     * refval,   &
                     sectp1%depth + (sectp2%depth - sectp1%depth) * refval  )
      min_dist = dist( [point%x,         point%y,         point%depth],       &
                       [point_segment_loc%x, point_segment_loc%y,             &
                                                    point_segment_loc%depth] )
      if (present(point_segment)) then
        point_segment = point_segment_loc
      end if
    end if

  end subroutine geo_poly3d_dist_point_to_section

  !-----------------------------------------------------------------------------
  !> Calculate a the_environment::spatial target with an offset.
  !!
  !! This function calculate the coordinates of a point **C** in between two
  !! objects, a reference object **A** and a target object **B**, but at a
  !! smaller distance with specific offset @f$ \Delta @f$ from the target
  !! **B**.
  !> @image html img_doxygen_dist_offset.svg
  !! @image latex img_doxygen_dist_offset.eps "" width=8cm
  function offset_dist(obj_a, obj_b, offset) result (obj_c)
    !> @param[in] obj_a reference spatial object (**A**)
    !> @param[in] obj_b target spatial object(**B**)
    class(SPATIAL), intent(in) :: obj_a, obj_b
    !> @param[in] offset distance offset for the target object
    real(SRP), intent(in) :: offset
    !> @return Returns the coordinates of the updated target object **C**
    !!         that is located at a smaller distance from **A**, by the value
    !!         of the offset parameter. If the distance between **A** and **B**
    !!         is smaller that the `offset` value the returned error spatial
    !!         object has commondata::missing coordinates. There is no error
    !!         code variable with intent(out) to keep the function pure.
    type(SPATIAL) :: obj_c

    ! distance between the **A** and **B**.
    real(SRP) :: dist

    dist = obj_a%distance(obj_b)

    !> The coordinate @f$ x_c @f$ of the new target **C** are defined as:
    !! @f[ x_c = x_a + dist(A,B)-\Delta \cdot \frac {x_b-x_a} {dist(A,B)} , @f]
    !! where @f$ x_a, x_b @f$ are the *x* coordinates of the points **A**
    !! and **B** and @f$ dist(A,B) @f$ is the distance between **A** and **B**.
    !! The *y* and *depth* coordinates of the point **C** are defined in the
    !! same way.
    if (dist - offset > ZERO) then
      obj_c%x     =                                                           &
            obj_a%x     + (dist - offset) * (obj_b%x     - obj_a%x)     / dist
      obj_c%y     =                                                           &
            obj_a%y     + (dist - offset) * (obj_b%y     - obj_a%y)     / dist
      obj_c%depth =                                                           &
            obj_a%depth + (dist - offset) * (obj_b%depth - obj_a%depth) / dist
    else
      obj_c = SPATIAL( MISSING, MISSING, MISSING )
    end if

  end function offset_dist

  !> @}
  ! end of computational geometry backend: **polygon2D**

end module THE_ENVIRONMENT
