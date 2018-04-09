# Modern Fortran features #

The AHA Model is implemented in the modern Fortran **F2003** and partially
**F2008** standards. Because not all widespread compilers support the full
set of these language standards, the model code uses only those that are
supported by the following minimum compiler versions:

- GNU gfortran 4.8
- Intel Fortran 17

Modern Fortran fully supports object oriented programming with polymorphic
objects and rich set of array functions which are also usable in the object
oriented code.

This document provides a very brief overview of the main features of modern
Fortran that are widely used in the AHA Model code.

## Object oriented programming and type-bound procedures ##

Stated simply, the object-oriented programming paradigm is based on the
notion of **object.** Here object is an entity that integrates **data**
and **procedures** that are implemented to manipulate these data. In the
simplest case, data can be considered as the "properties"" or "attributes"
that describe the object. Procedures that are linked with the object, on the
other hand, provide other derived attributes of the object or describe what
the object can "do".

Different objects can be arranged in various ways (e.g. form more complex
objects like arrays). For instance a population of agents (another object)
can be simply formed by arranging individual agents (other objects) into an
array. Various agents can also interact with each other.

For example, a single "agent" object is an entity having such attributes as
sex, spatial position, body mass, body length etc. It can also have such
boolean attributes as "is alive" (true or false). For any such object,
one can calculate instantaneous risk of predation and other transient derived
properties. Also, the agent can interact with objects of various other kinds.
For example, an agent can change its spatial position (its position attribute
is changed), approach a food item and "eat" it (basically, absorb the mass
attribute of the item, the item is destroyed thereafter). Agent can also do
many other things, e.g. "die". The functions that are linked to the object
are usually called **methods.**

When an instance of the object is created, it is initialised in a function
(e.g. `init`) that is often called the **constructor**. Another procedure is
sometimes implemented to destroy and deallocate the object, it is the
**destructor**.

Object-oriented code in modern Fortran is based on what is called
**type-bound procedures**.

Briefly, a derived type is declared using the `type` keyword; it can contain
several intrinsic and other derived types. Thus, a **data structure** is
implemented.

    type, public :: SPATIAL_POINT
      real(SRP) :: x, y, depth
      character(len=LABEL_LENGTH) :: label
      ....
    end type SPATIAL_POINT

Any **instances** of the object can be declared using `type` keyword.

    type(SPATIAL_POINT) :: some_point, another_point

A procedure can then be declared that operates specifically on this type. The
first parameter `this` refers to the object that the procedure operates on.

The base object `this` is declared as `class` in the procedure, which allows
to accept any **extension** of the `this` object as the first parameter. This
is called "polymorphic objects."

Note that the other parameters (non `this`) can be declared as `class` or as
`type`. In the former case, the procedure could accept any extensions (the
procedure is then **polymorphic**) of the object, while in the latter, only
this specific `type` (non-polymorphic procedure).

Components of the object are separated from its name with the percent sign
`%`, e.g. the `x` coordinate is this\%x.

    function spatial_distance_3d (this, other) result (distance_euclidean)
      class(SPATIAL_POINT), intent(in) :: this
      real(SRP) :: distance_euclidean
      class(SPATIAL_POINT), intent(in) :: other
      distance_euclidean = dist( [this%x,  this%y,  this%depth],              &
                                 [other%x, other%y, other%depth] )
    end function spatial_distance_3d

The procedure is then included into the derived type declaration.

The name of the procedure that is implemented (e.g. `spatial_distance_3d`
in the example above) is not called directly in calculations and can be
declared `private`. Instead, a **public interface** name is declared in the
derived type that defines how the procedure should be called, in the example
below it is `distance`.

Note that the interface name can coincide for several different objects,
however the actual procedure name (`spatial_distance_3d`) must be unique
within the module that defines the derived type and its procedures.

    type, public :: SPATIAL_POINT
      real(SRP) :: x, y, depth
      character(len=LABEL_LENGTH) :: label
      ....
      contains
      procedure, public :: distance => spatial_distance_3d
      ....
    end type SPATIAL_POINT

Now, the procedure is called for the specific instance of the object (it comes
to the procedure as the `this` first "self" parameter) using the public
interface name (`distance`) rather than the "actual" procedure name
(`spatial_distance_3d`).

    type(SPATIAL_POINT) :: point_a, point_b
    ...
    distance_between_points = point_a%distance( point_b )

An **extension** object can be declared using `extends` keyword, that will
use all the properties and type-bound procedures of the base object and add
its own additional ones. This allows creating complex inheritance hierarchies
across objects.

    type, public, extends(SPATIAL_POINT) :: SPATIAL_MOVING
      ! The following component adds an array of history of the object
      ! movements:
      type(SPATIAL_POINT), dimension(HISTORY_SIZE_SPATIAL) :: history
      ...
      contains
        ....
        procedure, public :: go_up => spatial_moving_go_up
        procedure, public :: go_down => spatial_moving_go_down
        ....
    end type SPATIAL_MOVING

Thus, the structure of the module that defines an inheritance hierarchy of
objects and their type-bound functions is like this:

    module SPATIAL_OBJECTS
      ! Declarations of objects:
      type, public :: SPATIAL_POINT
        real(SRP) :: x, y, depth
        character(len=LABEL_LENGTH) :: label
        ....
        contains
        procedure, public :: distance => spatial_distance_3d
        ....
      end type SPATIAL_POINT
      ....
      type, public, extends(SPATIAL_POINT) :: SPATIAL_MOVING
        ! The following component adds an array of history of the object
        ! movements:
        type(SPATIAL_POINT), dimension(HISTORY_SIZE_SPATIAL) :: history
        ...
        contains
          ....
          procedure, public :: go_up => spatial_moving_go_up
          procedure, public :: go_down => spatial_moving_go_down
          ....
      end type SPATIAL_MOVING
      .....
      ! other declarations
      .....
      contains
        ! Here go all the procedures declared in this module
        function spatial_distance_3d (this, other) result (distance_euclidean)
          class(SPATIAL_POINT), intent(in) :: this
          real(SRP) :: distance_euclidean
          class(SPATIAL_POINT), intent(in) :: other

          distance_euclidean = dist( [this%x,  this%y,  this%depth],              &
                                     [other%x, other%y, other%depth] )

        end function spatial_distance_3d
        ....
        ! Any other procedures
        ......
    end module SPATIAL_OBJECTS

Relationships between different kinds of objects can be represented graphically
in a [class diagram](http://158.37.63.57/doc/ar01s05.html#_class_diagram). See
[Object-oriented programming and modelling](http://158.37.63.57/doc/ar01s05.html)
section of the HEDTOOLS manual for more information.

## Elemental procedures ##

Modern Fortran includes a powerful concept of **elemental procedures**. Such
procedures (subroutines of functions) are declared using simple scalar
parameters. However, they can also accept parameters that are arbitrary
**arrays**.

There are strict limits on the procedures that can be `elemental`. Basically,
there should be no side effects in the procedure code (the code should only
affect the defined parameters that are in the parameter list), if the code
calls any procedures or functions, these should be declared as `pure` (free of
any side effects), each parameter must have explicit `intent` declared.
Notably, calling random numbers (`random_number`) or any input/output are
not allowed.

Here is an example of a simple elemental function:

    elemental function add (a,b) result (sum)
      real :: a, b, sum
      sum = a + b
    end function add

It can be used with scalar arguments as normal:

    real :: x, y, z
    z = add(x, y)

However, it can also be used with arrays and combinations of arrays and scalars:

    real :: a
    real, dimension(100,100) :: x, y, z1, z2
    z1 = add(a, x)         ! scalar added to array
    z2 = add(x, y)         ! two arrays are added element by element

In such a case, the function is executed in element-wise manner. For this
reason, the arrays should have conforming dimensionality and sizes.

Elemental procedures also work with the object oriented code. This allows
implementation complex algorithm in a very concise manner, avoiding code
duplication across different-sized arrays.

For example, the simple function below allows to get the spatial position of
a spatial object or any extension.

    elemental function spatial_get_current_pos_3d_o(this) result(coordinates)
      class(SPATIAL_POINT), intent(in) :: this
      type(SPATIAL_POINT) :: coordinates
      coordinates%x = this%x
      coordinates%y = this%y
      coordinates%depth = this%depth
    end function spatial_get_current_pos_3d_o

However, it can also be used with an array of such spatial objects and returns
an array of their positions. For example, it can return an array of spatial
positions for a whole population of agents, or for its slice.

    call LOG_DBG("Coordinates: " // TOSTR(parents%individual(1:10)%location()))

Using elemental procedures in the object oriented Fortran code allows to
substitute such loop-based code:

    do i = 1, size(neighbours)
      dist_here(i)  = this%distance(neighbours(i))
    end do

by a shorter, simpler (and usually better optimised compiled code) whole-array
based code:

    dist_here  = this%distance(neighbours)

## Whole array procedures ##

Most arithmetic functions in modern Fortran accept scalar as well as array
arguments (usually elemental). There are also many other useful intrinsic
array functions that allow writing very concise and easily understandable code.
An additional advantage is that these procedures are usually highly optimised
by the compiler and can be executed in parallel multi-threading mode if
automatic parallelisation is enabled.

    ! Maximum and minimum value of an array:
    a = maxval(array)
    b = minval(array)

    ! ... and their locations:
    i = maxloc(array)
    j = minloc(array)

    ! array sum:
    total = sum(array)

    ! count elements with a mask:
    i = count( x > y )

    ! masked array assignment:
    where ( x == missing ) x = y

    ! indexed parallel array assignment:
    forall ( i=1:popsize ) x(i) = 1.0/i

    ! ... and many more

Using such whole-array based functions allows implementation of very concise
and comprehensible code that is also highly optimised for multithreading
execution.

    ! Number of agents alive
    call csv_record_append(file_record, count(parents%individual%is_alive()))
    ....
    ! Average body mass
    call csv_record_append(file_record, average(parents%individual%body_mass))

## User defined operators ##

Modern Fortran allows to define arbitrary operators and redefine intrinsic
operators.

User defined operators are declared using `interface operator` and must
refer to a specific function that implements the operator.

    interface operator (.above.)
      procedure spatial_check_located_below
    end interface operator (.above.)
    ....
    ....
    function spatial_check_located_below(this, check_object) result (are_below)
      class(SPATIAL_POINT), intent(in) :: this
      class(SPATIAL_POINT), intent(in) :: check_object
      logical :: are_below
      if ( check_object%dpos() > this%dpos() ) then
        are_below = .TRUE.
      else
        are_below = .FALSE.
      end if
    end function spatial_check_located_below

Once defined, such operators can be used with object oriented code like the
intrinsic operators:

    do i=1, max
      if ( this%perceive_consp%conspecifics_seen(i) .above. the_agent ) then
        number_above = number_above + 1
      end if
    end do

or using whole-array operators even simpler:

    number_above=count(this%perceive_consp%conspecifics_seen .above. the_agent)

User-defined operators can refer to elemental functions, however can be used
only with scalar arguments.

## The associate construct ##

The `associate` construct allows to declare a shortcut that is then used in
place of a long and complex expression or variable. This includes indexing
variables in `do` loops and parts of the object hierarchy.

Here is an example:

    do ind = 1, size(this%individual)
      ......
      associate ( AGENT => this%individual(ind) )
        call csv_record_append(file_record, AGENT%person_number        )
        call csv_record_append(file_record, AGENT%genome_label         )
        call csv_record_append(file_record, conv_l2r(AGENT%alive)      )
        call csv_record_append(file_record, conv_l2r(AGENT%sex_is_male))
        call csv_record_append(file_record, AGENT%body_length          )
        call csv_record_append(file_record, AGENT%body_length_birth    )
        call csv_record_append(file_record, AGENT%control_unselected   )
        call csv_record_append(file_record, AGENT%body_mass            )
        ....
      end associate
      .....
    end do

## Pointers ##

Modern Fortran supports **pointers,** in the same way as many other
programming languages like C or C++. However, the use of pointers is more
limited in Fortran, which is also not bad because inaccurate use of
pointers can cause severe problems with the memory (e.g. when a pointer
refers to a nonexistent object).

Basically, pointer is an alias to some other object, e.g. a variable or array.
The use of pointers can make code more flexible and dynamic.

The pointer is declared as a specific type (intrinsic or derived) with the
`pointer` attribute. The object that is used as the target for the pointer
should be declared with the same type and the `target` attribute.

    type(POPULATION), public, target ::  generation_one
    type(POPULATION), public, pointer :: parents

After this, one can simply make the object `parents` an alias of
`generation_one`:

    parents => generation_one

Now the object `parents` points to the location in the computer memory where
the object `generation_one` resides. It can consequently be used instead of
the "target." For example, all the type-bound functions that are defined for
`generation_one` can be called for `parents`.

    call parents%sort_by_fitness()


## References ##

- Adams, J. C., et al., (2009). The Fortran 2003 Handbook. Springer.
- Akin, E. (2003). Object-Oriented Programming via Fortran 90/95. Cambridge
  University Press.
- Brainerd, W. S. (2015). Guide to Fortran 2008 Programming. Springer.
- Chapman, S. J. (2007). Fortran 95/2003 for Scientists and Engineers.
  McGraw-Hill.
- Clerman, N. S., & Spector, W. (2012). Modern Fortran: Style and
  usage. Cambridge University Press.
