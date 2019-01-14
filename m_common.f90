!> @file m_common.f90
!! This module defines common global parameters and objects for the AHA Model.
!! It also contains a general overview of the AHA Model in Doxygen notation.
!! @author Sergey Budaev <sergey.budaev@uib.no>
!! @author Jarl Giske <jarl.giske@uib.no>
!! @date 2016-2017

!-------------------------------------------------------------------------------
! $Id$
!-------------------------------------------------------------------------------

! Note: This is the starting main Doxygen documentation page. It should be
!       subdivided using @section @subsection @subsubsection tags rather than
!       the Markdown ###-style.

!> @mainpage The AHA Model: Evolution of decision making and behaviour
!> @section intro_aha_intro Overview of The AHA Model
!! This is a large scale simulation model (under development) that implements
!! a general **decision-making architecture** in **evolutionary agents**. Each
!! agent is programmed as a whole virtual organism including the genome,
!! rudimentary physiology, the hormonal system, a cognitive architecture and
!! behavioural repertoire. They "live" in a stochastic spatially explicit
!! virtual environment with physical gradients, predators and prey. The primary
!! aim of the whole modelling machinery is to understand the evolution of
!! decision making mechanisms, personality, emotion and behavioural plasticity
!! within a realistic ecological framework. An object-oriented approach coupled
!! with a highly modular design not only allows to cope with increasing layers
!! of complexity inherent in such a model system but also provides a framework
!! for the system generalizability to a wide variety of systems. We also use
!! a "physical-machine-like" implementation philosophy and a coding standard
!! integrating the source code with parallel detailed documentation that
!! increases understandability, replicability and reusability of this model
!! system.
!!
!> The cognitive architecture of the organism is based on a set of
!! motivational (emotional) systems that serves as a common currency for
!! decision making. Then, the decision making is based on **predictive
!! assessment** of external and internal stimuli as well as the agent's own
!! motivational (emotional) state. The agent makes a subjective assessment
!! and selects, from the available repertoire, the behaviour that would reduce
!! the expected motivational (emotional) arousal. Thus, decision making is
!! based on predicting one's own internal state. As such, the decision-making
!! architecture integrates motivation, emotion, and a very simplistic model
!! of consciousness.
!!
!! The **purpose** of the AHA model is to investigate a general framework for
!! modelling proximate decision-making and behavior. From this we will
!! investigate adaptive goal-directed behaviour that is both guided by the
!! external environment and still is endogeneously generated.
!!
!! Other research topics include individual differences, personality as well
!! as consequences of emotion and personality to population ecology.
!!
!! We think that understanding and modelling complex adaptive behaviour
!! requires both extraneous (environmental) factors and stimuli as well as
!! endogeneous mechanisms that produce the behaviour. Explicit proximate
!! representation of the motivation and emotion systems, self-prediction can
!! be an important component in linking environment, genes, physiology,
!! behavior, personality and consciousness.
!!
!! - The AHA! Project Development Pages: http://ahamodel.uib.no
!! - Building blocks of the AHA Model: @ref aha_builblocks_main
!! - The Cognitive Architecture of the agent: @ref aha_buildblocks_cogn_arch
!! .
!!
!! Fortran is used due to its simplicity and efficiency. For example, check out
!! this paper:
!! [Why physicists still use Fortran?](http://www.moreisdifferent.com/2015/07/16/why-physicsts-still-use-fortran).
!!
!! Main features of modern Fortran that are used in the AHA model code
!! are briefly outlined in the
!! [README_NF](http://ahamodel.uib.no/doxydoc/md__r_e_a_d_m_e__n_f.html) .
!!
!> @section intro_version Version information
!! This is the model version information parsed from the main Subversion
!! repository https://svn.uib.no/aha-fortran or Bitbucket Mercurial-based
!! repository https://bitbucket.org/ahaproject/hedg2_01 (the latter is
!! currently used just as a mirror of the Subversion branch).
!! @verbatim
!! $Id: m_common.f90 851df748b488 2018/02/17 20:25:54 sergey $
!! @endverbatim
!! Version information is also saved as two variables that can be passed to
!! the logger (see commondata::logger_init()) and outputs and file names:
!! - commondata::svn_version_string, full version string as above;
!! - commondata::svn_version, version number (hex id).
!! .
!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!!
!> @section intro_main Working with the Model
!! A "Getting Started" introduction is available in the
!! [README.md](http://ahamodel.uib.no/doxydoc/md__r_e_a_d_m_e.html) file.
!!
!> @subsection intro_overview_modules Overview of the AHA Fortran modules
!! The Modelling framework is composed of two separate components:
!! - [HEDTOOLS](http://ahamodel.uib.no/doc/HEDTOOLS.pdf)
!!   (also [mirror at bitbucket](https://bitbucket.org/teg_uib/hedtools)),
!!   modelling utilities and tools implemented as portable Fortran
!!   modules, not object-oriented, that have general applicability and are
!!   used for data conversion, output, random number generation and
!!   execution logging. HEDTOOLS modules are designed such that they can be
!!   used in many different simulation projects, not only the AHA model;
!! - @ref intro_aha_intro "The AHA model", an object oriented evolutionary
!!   agents simulation framework implementing reusable module components.
!! @image html aha_modules.svg  "Overview of AHA Fortran modules"
!! @image latex aha_modules.eps "Overview of AHA Fortran modules" width=14cm
!!
!> @subsection intro_running Running the model
!! Building and running the mode is based on the GNU Make system.
!! - To **build** the model from the source code issue:
!!   @code{.sh}
!!     make
!!   @endcode
!! - To build and **run** the model issue this command:
!!   @code{.sh}
!!     make run
!!   @endcode
!! - **Delete** all build-related, data and temporary files:
!!   @code{.sh}
!!     make distclean
!!   @endcode
!! - Get a quick **help** from the make system:
!!   @code{.sh}
!!     make help
!!   @endcode
!! - The compiler can be provided via `FC` variable, e.g.
!!   @code{.sh}
!!     make run FC=ifort
!!   @endcode
!! - Building with debug symbols is controlled by setting the `DEBUG` variable.
!!   @code{.sh}
!!     make run FC=ifort DEBUG=1
!!   @endcode
!! .
!! See @ref Makefile for build configuration. To get more information on GNU
!! Make see [AHA Modelling Tools Manual](http://ahamodel.uib.no/doc/ar01s13.html),
!! [Using Microsoft Visual Studio](http://ahamodel.uib.no/doc/ar01s14.html) and
!! [Using Code::Blocks IDE](http://ahamodel.uib.no/doc/ar01s15.html).
!!
!> @subsection intro_environ_vars Environment variables
!! The model makes use of several environment variables to control
!! certain global aspects of the execution. These variables have the `AHA_`
!! prefix and are listed below.
!! - `AHA_DEBUG=TRUE` sets the @ref intro_debug_mode "debug mode";
!! - `AHA_SCREEN=YES` sets logger to write to the standard output in addition
!!    to the log file;
!! - `AHA_DEBUG_PLOTS=YES` enables generation of debug plots;
!! - `AHA_ZIP_FILES=YES` enables background compression of big output
!!    data files;
!! - `AHA_CHECK_EXTERNALS=NO` disables checking for external executable
!!    modules (this is a workaround against Intel Fortran compiler bug).
!! .
!! They are described in details in the `commondata::system_init()` section.
!! **Setting** the environment variable on different platforms:
!! **Linux/Unix:**
!! @code{.sh}
!!   export AHA_DEBUG=1
!! @endcode
!! **Windows:**
!! @code{.sh}
!!   set AHA_DEBUG=1
!! @endcode
!! **Checking** if the environment variable is set and what is it value
!! On Unix/Linux:
!! @code{.sh}
!!   echo $AHA_DEBUG
!! @endcode
!! On Windows
!! @code{.sh}
!!   echo %AHA_DEBUG%
!! @endcode
!! To check if any of the above environment variables are set is easy on Linux:
!! @code{.sh}
!!   env | grep AHA_
!! @endcode
!!
!> @subsection intro_debug_mode The DEBUG mode
!! The protected global variable `IS_DEBUG` (commondata::is_debug) sets
!! up the **debug mode** of execution. The debug mode results in huge
!! amount of output and logs that significantly slows down execution.
!! Debug mode can be set using the environment variable
!! `AHA_DEBUG=1`, `AHA_DEBUG=YES` or `AHA_DEBUG=TRUE`.
!> Debug mode can also be set by setting the runtime **command line**
!! parameter to `DEBUG`, `DEBUG=1`, `DEBUG=YES` or `DEBUG=TRUE`
!! to the model executable, e.g.
!! @code{.sh}
!!   ./MODEL.exe DEBUG
!! @endcode
!! This runtime command line parameter is automatically set if the model
!! is started using the DEBUG variable for the GNU Make:
!! @code{.sh}
!!   make run DEBUG=1
!! @endcode
!! *Build* Note also that `DEBUG` with make build command
!! @code{.sh}
!!   make DEBUG=1
!! @endcode
!! will build the executable for the model in the debug mode also. All
!! compiler optimisations are turned off (`-O0`), debugger symbols (`-g`) and
!! tracebacks are enabled, various compiler error checking options and
!! warnings are also enabled, along with extended runtime checks.
!!
!! Notably, the Intel Fortran compiler enables initialisation of variables
!! with the signalling NaNs (`-init=snan`). Any operations on non-initialised
!! variable(s) would then result in the runtime crash with traceback, making
!! it easier to catch non-initialised variables.
!!
!! Normal build without setting `DEBUG`, in contrast, enables various compiler
!! optimisations making the executable run faster.
!! @code{.sh}
!!   make
!! @endcode
!! Notably, with the Intel Fortran compiler, the model is built with automatic
!! parallelisation (`-parallel`), so whole array operations, `do concurrent`
!! and `pure` and `elemental` functions are run in the
!! [multi-threaded mode](http://ahamodel.uib.no/intel/hh_goto.htm?index.htm#GUID-29986DD5-C17F-49BB-AC9B-365B077C3909.html)
!! whenever the compiler is "sure" that these operations can be safely
!! performed.
!! See the @ref Makefile for specific compiler and debugging options and the
!! [GNU make](http://ahamodel.uib.no/doc/ar01s13.html) for a general overview of
!! the system. A overview of Intel Fortran parallel processing options can be
!! found
!! [here](http://ahamodel.uib.no/intel/hh_goto.htm#GUID-1E91DFFD-D7CD-4AF5-B911-7E5D1CCDBBA5.html)
!! and [here](http://ahamodel.uib.no/intel/hh_goto.htm?index.htm#GUID-06B54325-1C5C-41E7-A9CD-0E3A8542DC05.html).
!!
!! Combining the shell environment variable `AHA_DEBUG` and the `DEBUG` make
!! variable allows to control how the model executable is build and run.
!! For example, *building* it in *non-debug* mode, i.e. with all the
!! optimisations for fast (and possibly multi-threaded) execution:
!! @code{.sh}
!!   make
!! @endcode
!! can be combined with *running* the model in the *debug* mode:
!! @code{.sh}
!!   export AHA_DEBUG=YES
!!   make run
!! @endcode
!! In such a case, the model is built and executed in the "fast" optimised
!! and possibly multi-threaded mode, but still prints all the numerous
!! debugging outputs into the logger.
!!
!! The system initialisation procedure commondata::system_init() and
!! commondata::logger_init() provide more details on initialisation and
!! logging.
!!
!> @subsection intro_overview_lockfile The lock file
!! At the start of the simulation, the program creates an empty **lock file**
!! defined by commondata::lock_file. The lock file is created at the end of
!! commondata::system_init() and and is deleted by the procedure
!! commondata::system_halt().
!!
!! The lock file indicates that the simulation is (still) running and could be
!! used by various scripts, cron jobs etc to make sure the simulation is
!! finished. For example, a script can check from time to time if the lock
!! file exists and if not, issue a command to zip all data outputs and transfer
!! them to a remote analysis server.
!!
!> @subsection intro_overview_files_csv Output numerical data
!! Data could be output from the model. The standard format for data output is
!! [CSV](http://ahamodel.uib.no/doc/ar01s08.html). Its advantage is that it is
!! based on plain text and is human readable nut can be easily imported into
!! spreadsheets and statistical packages. The most straightforward use for CSV
!! is for vector-based data and two-dimensional matrices, including sets of
!! vectors (e.g. 'variables'/columns with 'observations'/rows).
!!
!! CSV output is based on the [CSV_IO](http://ahamodel.uib.no/doc/ar01s08.html)
!! module in [HEDTOOLS](http://ahamodel.uib.no/doc/). There is also an object
!! oriented wrapper for CSV_IO: @ref file_io.
!!
!! The model also includes a few standardised CSV output procedures for saving
!! various characteristics for a whole population of agents:
!! - the_population::population::save_csv() -- save various condition data;
!! - the_population::population::save_genomes_csv() -- save the complete genome
!!   data of the agents;
!! - the_population::population::save_memory_csv() -- save the perceptual and
!!   emotional memory stacks of the agents;
!! - the_population::population::save_movements_csv() -- save the latest
!!   movement history of the agents.
!! - the_population::population::save_behaviour_csv() -- save the behaviour
!!   history of the agents: successive labels of behaviours executed.
!! .
!! Additionally, the genetic algorithm subroutine
!! the_evolution::generations_loop_ga() implements a sub-procedure
!! - @ref generation_stats_record_write() -- produces various generation-wise
!!   statistics, such as the number  of surviving agents, average body mass
!!   etc.
!! .
!!
!! The following procedures output the properties of the @ref the_environment
!! at a particular time step:
!! - the_environment::food_resource::save_csv() -- saves all the (stochastic)
!!   food items that compose the the_environment::food_resource.
!! - the_environment::habitat::save_predators_csv --saves all the (possibly
!!   stochastic) predators that are implemented within the habitat.
!! .
!!
!! @subsection intro_descriptors The Model descriptors
!! The model includes several descriptors that can be used for descriptive
!! notes on the model.
!! - commondata::model_name -- model name: a short name of the model;
!! - commondata::model_descr -- model description: a longer, one line
!!   description of the model;
!! - The Model Abstract -- A brief text that can span several lines of text
!!   and is kept in a separate file with the name defined by the
!!   commondata::model_abstract_file parameter. If this file is absent,
!!   model description is used.
!! .
!! These descriptors can appear in the model logger output and form parts of
!! the output data files.
!!
!! In particular, because the Model Abstract is stored in a separate text file,
!! it can contain dynamically updated information, such as the latest version
!! control message log output. Subversion and Mercurial commit hooks can be
!! used to implement such a functionality.
!!
!! To append version information (Subversion) to the Model Abstract file
!! manually do this:
!! @code
!!  svn log -l1 >> abstract.txt
!! @endcode
!!
!> @subsection intro_style_rules Coding and documenting style
!! Using a consistent coding style increases the readability and
!! understandability of the program. It is also easier to search and locate
!! specific parts. For example, using specific rules for version control
!! commit messages make it easier to find specific changes by using regular
!! expression syntax. The coding rules for the AHA model are relatively light.
!!
!> @subsubsection intro_coding_style Fortran coding style
!! - Clean, readable and understandable code is much better than "tricky" but
!!   more computationally efficient code.
!! - Obsolete, outdated and non-standard (e.g. vendor extensions) features
!!   of the Fortran language should *never* be used.
!! - Very modern Fortran features (e.g. many F2008 and F2015) not well
!!   supported by the available compilers should be avoided whenever possible.
!! - Portability is crucial: the model should be easy to build and run using
!!   different compilers, GNU free software *gfortran* is the compiler of
!!   priority.
!! - Frequent checking for errors (and even *possible* errors) is important;
!!   correct computation has much higher priority than just speed.
!! - Use generic programming techniques (e.g. optional arguments, generic
!!   interfaces) for higher extensibility.
!! - Always use explicit `intent` declared for all subroutines and functions.
!! - The line length should not exceed 80 characters; use continuation lines
!!   and indents to make the structure clear.
!! - Spaces (not tabs) are used for indentation, standard indent is 2
!!   characters.
!! - Use *lowercase* names for all local and Fortran intrinsic
!!   objects.
!! - Global variables (not constants) should be in "CamelCase".
!! - Public or local *constants* (that are not changed) are in
!!   UPPERCASE.
!! - External or library objects, e.g. those from the HEDTOOLS are in
!!   UPPERCASE
!! - Module names are UPPERCASE, class names are UPPERCASE.
!! - Spatial objects (`SPATIAL` and `SPATIAL_MOVING`) use
!!   `position` to *set* spatial position and `location` to
!!   *get* it.
!! - `create` method is used to create and initialise an
!!   empty object (e.g. `SPATIAL` with missing coordinates),
!!   should not take other parameters and must be elemental
!!   (so cannot include calls to random);
!! - `make` or `build` method is used to create a working object
!!   with random initialisation etc.
!! .
!! See the [AHA Modelling Tools Manual](http://ahamodel.uib.no/doc/) for more
!! details (http://ahamodel.uib.no/doc/HEDTOOLS.pdf).
!!
!! The [README_NF](http://ahamodel.uib.no/doxydoc/md__r_e_a_d_m_e__n_f.html)
!! provides a brief outline of the main features of modern Fortran (F2003/2008)
!! that are used in the model code.
!!
!> @subsubsection intro_commit_style Version control style
!! - Commit messages noting specific subroutines or functions should
!!   include the names of these procedures in parentheses:
!!   @code{.sh}
!!     svn ci -m "hope function (hope) now accepts arbitrary raw grid arrays"
!!   @endcode
!! - Commit messages chenging only the Doxygen comments should start with
!!   'doc:', e.g.
!!   @code{.sh}
!!     svn ci -m "doc: still notes render poorly, try top put to return"
!!   @endcode
!! .
!> @subsubsection intro_document_style Doxygen self-document style
!! - If something is changed in the *code*, the documentation comments
!!   should also be checked for *correspondence with the code*, Doxygen
!!   comments should *always* reflect the logic of the code and should
!!   be never outdated.
!! - Document procedure purpose and brief implementation in the
!!   over-the-procedure header comment block.
!! - *Local variables* are **not** in the Doxygen tags unless they
!!   are very important, then include full paragraph description.
!! - Porting and F2008 compatibility notes and warnings are not in
!!   the Doxygen tags unless they are very important.
!! - Unimportant implementation notes can be documented within
!!   the body of the procedure *without* Doxygen "`!>`" tags. So they
!!   are never parsed.
!! - *Important* implementation details are fully documented in
!!   Doxygen tags. So they can be rendered if `HIDE_IN_BODY_DOCS` is
!!   `NO`.
!! - Implementation details within the procedure body should be under
!!   the level 3 headers enclosed in `###`:
!!   @code
!!     !> ### Implementation details ###
!!   @endcode
!! - Full Fortran variable object hierarchy for instantiated objects must be
!!   as normal text, i.e. **not enclosed in reverse single quotes** (as
!!   verbatim code), because percent sign is not escaped and parsed wrongly
!!   then:
!!   @verbatim
!!     this\%reprfact_decrement_testosterone
!!   @endverbatim
!!   not
!!   @verbatim
!!     `this\%reprfact_decrement_testosterone`
!!   @endverbatim
!!   or
!!   @verbatim
!!     this%reprfact_decrement_testosterone
!!   @endverbatim
!!   or still
!!   @verbatim
!!     `this%reprfact_decrement_testosterone`
!!   @endverbatim
!!   as the percent sign disappears in the parsed output.
!! - Reference to other subroutines and functions of the code must
!!   be in the C++ style, referring the class/module in **lowercase**:
!!   @verbatim
!!     !! values resulting from executing this behaviour (`reproduce::do_this()`
!!     !! = the_neurobio::reproduce_do_this() method). This is repeated for
!!   @endverbatim
!!   This makes them appear as cross-links in the parsed document.
!! - Documenting specific implementation procedure for in-type interface
!!   name declaration should follow this style (include link to implementation):
!!   @verbatim
!!     !> Calculate the Euclidean distance between two spatial objects.
!!     !! See `the_environment::spatial_distance_3d()`
!!     procedure, public :: distance => spatial_distance_3d
!!   @endverbatim
!! - Reference to a procedure that is defined within this (being documented)
!!   procedure (i.e. below `contains`) is using the hanging `::` notation:
!!   @verbatim
!!     !! The subjective capture probabilityis calculated by the sub-function
!!     !! `::subjective_capture_prob()`.
!!   @endverbatim
!! - Documentation sections/subsections/subsubsections with tags are inserted
!!   like this:
!!   @verbatim
!!      !> @subsubsection aha_buildblocks_genome_genome Individual genome
!!   @endverbatim
!!   They can be referred to in the documentation text using the tag like this:
!!   @verbatim
!!      !> @ref aha_buildblocks_individual "The individual agent" is also ...
!!   @endverbatim
!! - The \@name  tag defines an arbitrary **member group** of
!!   Doxygen objects. Membership range is defined by the \@{ and
!!   \@} tags. But this does not seem to work within `type`
!!   definitions to  delimiter outer procedure interfaces.
!!
!> @subsection intro_computation_notes Brief notes on computation
!> @subsubsection intro_computation_real Float point computations
!! There are many possible quirks and caveats with the real type (float point)
!! calculations on the computer. The rules of float point computations deviate
!! from the basic arithmetic. The main issue is that real type numbers are
!! represented as bits and have finite and limited precision. Furthermore,
!! numerical precision can deteriorate due to rounding in computations.
!!
!! The precision of the float point number representation in Fortran is
!! controlled by the kind parameter (e.g. `real(kind=some_number`) ).
!!
!! **Numerical precision modes**. In the AHA Model, there are two basic
!! numerical precision modes:
!! - commondata::srp, "Standard Real Precision" for real numbers that is
!!   normally used for all computations:
!!   @code
!!     real(SRP) :: value
!!   @endcode
!!   @note Note that commondata::srp precision model should be used in most
!!         cases.
!! - commondata::hrp, "High Real Precision", an extended precision that is used
!!   in a few cases where commondata::srp is not enough for valid computation
!!   (insufficient precision or inability to represent huge numbers):
!!   @code
!!     real(HRP) :: value
!!   @endcode
!!   Parenthetically, there is also an extended precision integer type defined
!!   by commondata::long parameter.
!! .
!!
!! **Constants.** There is also a useful commondata::zero constant, which sets
!! some "minimum distinguishable non-zero" value: it is the smallest real
!! number *E* such that @f$ 1 + E > 1 @f$.
!!
!! The smallest positive real value is defined by the commondata::tiny_srp
!! constant. It is used in the definition of the default numerical tolerance
!! value for high precision calculations:
!! - commondata::tolerance_low_def_srp;
!! - commondata::tolerance_low_def_hrp (the same constant for the high
!!   commondata::hrp precision with commondata::tiny_hrp).
!! .
!! @note Note that the commondata::tiny_srp and commondata::tiny_hrp values are
!!       much smaller than the commondata::zero parameter. Also, the default
!!       tolerance limits commondata::tolerance_low_def_srp and
!!       commondata::tolerance_low_def_hrp are also much smaller than the
!!       commondata::zero.
!!
!! Because the low tolerance based on the commondata::tiny_srp and
!! commondata::tiny_hrp may be too small and restrictive in many cases, the
!! second set of tolerance limits for low-precision calculations is based on
!! the commondata::zero parameter:
!! - commondata::tolerance_high_def_srp (commondata::srp real);
!! - commondata::tolerance_high_def_hrp (commondata::hrp real).
!! .
!! @note These high tolerance values should be used as the standard `epsilon`
!!       in most cases.
!!
!! The default commondata::srp values of these parameters calculated on an
!! x86_64 platform under Linux are (an example only!):
!! @verbatim
!!   ZERO: 1.19209290E-07
!!   TINY_SRP: 1.17549435E-38
!!   TOLERANCE_LOW_DEF_SRP: 5.87747175E-38
!!   TOLERANCE_HIGH_DEF_SRP: 1.19209290E-04
!! @endverbatim
!! These constants are reported at the start of the logger output.
!!
!! **Real type equality**. One possible quirk in float point computation
!! involves equality comparison, e.g.
!! @code
!!   if ( a == b) then ...
!! @endcode
!! With real type data `a` and `b`, such a condition can lead to unexpected
!! results due to finite precision and even tiny rounding errors: the numbers
!! that are deemed *equal* may in fact *differ* by a tiny fraction leading to
!! `a == b` condition being `FALSE`.
!!
!! Instead of the exact comparison, one should test whether the absolute
!! difference is smaller than than some predefined @f$ \varepsilon @f$
!! tolerance value (in the simplest case):
!! @f[ \left | a-b \right | < \varepsilon @f]
!! The @f$ \varepsilon @f$ is chosen based on the nature of the data and the
!! computational algorithm.
!!
!! The AHA Model framework includes a specific function for testing
!! *approximate equality* of two reals: commondata::float_equal(). With this
!! function, correct comparison is:
!! @code
!!   if ( float_equal(a, b, epsilon) ) then ...
!! @endcode
!! There is also a user defined operator "float equality" `.feq.` that works
!! as the commondata::float_equal() function, but uses a fixed default
!! `epsilon` equal to the default tolerance commondata::tolerance_low_def_srp
!! (or commondata::tolerance_low_def_hrp for high precision).  Its benefit is
!! that the usage almost coincides with the `==` (`.eq.`) operator usage:
!! @code
!!   if ( a .feq. b) then ...
!! @endcode
!! See the backend procedures commondata::float_equal_srp_operator() and
!! commondata::float_equal_hrp_operator() for details.
!! Another similar operator is "approximate equality" `.approx.` has a much
!! higher level of tolerance (larger error accepted)
!! @code
!!   if ( a .approx. b) then ...
!! @endcode
!! See the backend procedures commondata::float_approx_srp_operator() and
!! commondata::float_approx_hrp_operator() for details.
!!
!! There is also a function for testing if a real value is approximately equal
!! to zero: commondata::is_near_zero():
!! @code
!!   if ( is_near_zero(a) ) then      ! correct equivalent of if ( a == 0.0 )
!! @endcode
!!
!! @subsubsection intro_computation_missing Initialisation undefined constants
!! When a variable is created but not yet initialised, its value is
!! "undefined". However, it can take some haphazard values depending on the
!! compiler and the platform. There are special "initialisation" constants
!! defined in @ref commondata that set "missing" or "undefined" variable
!! status: commondata::missing (real) and commondata::unknown (integer).
!! By default, they are set to an unusual negative number -9999, so that any
!! bugs are clearly exposed if a variable inadvertently uses such an
!! "undefined" value.
!!
!! @subsubsection intro_computation_nonpar Nonparametric functions
!! The model in many cases makes use of **nonparametric relationships** between
!! parameters. This is based on the linear and non-linear interpolation
!! procedures implemented in HEDTOOLS:
!! [Interpolation routines](http://ahamodel.uib.no/doc/ar01s07.html#_interpolation_linterpol_ddpinterpol_interp_linear_interp_lagrange)
!!
!! Instead of defining specific function equation linking, say, parameters
!! *X* and *Y*: *Y=f(X)*, the relationship is defined by an explicit **grid**
!! of *X* and *Y* values without specifying any equation.
!!
!! In the simplest two-dimensional case, such a grid is defined by two
!! parameter arrays, for the *abscissa* and the *ordinate* of the nonparametric
!! function. Any values of this function can then be calculated based on a
!! nonlinear interpolation. This makes it very easy to specify various
!! patterns of relationships even when exact function is unknown or not
!! feasible.
!!
!! An example of such a nonparametric function is
!! the_neurobio::gos_global::gos_find().
!!
!> @subsection intro_webresources_model Links for more information
!> **AHA/BEAST Resources:**
!> - AHA Model repository mirror on Bitbucket:
!!   https://bitbucket.org/ahaproject/hedg2_01
!! - Full documentation for the AHA Model in PDF:
!!   http://ahamodel.uib.no/doxydoc/refman.pdf
!! - HEDTOOLS repository mirror on Bitbucket:
!!   https://bitbucket.org/teg_uib/hedtools
!! - HEDTOOLS documentation in HTML format: http://ahamodel.uib.no/doc
!! - HEDTOOLS documentation as a single PDF file:
!!   http://ahamodel.uib.no/doc/HEDTOOLS.pdf
!! - Development statistics for the model:
!!   http://ahamodel.uib.no/devstat/dir_branches_budaev_HEDG2_01.html
!! - TEG development statistics from Subversion: http://ahamodel.uib.no/devstat
!!
!> **Tools web resources:**
!! - GNU Fortran documentation: https://gcc.gnu.org/onlinedocs/
!!   (note that the model code targets features of the pre-version 5 because
!!   of their wider availability in Linux repos.)
!!   - GNU Fortran 4.9: https://gcc.gnu.org/onlinedocs/gcc-4.9.4/gfortran/
!!   .
!! - Intel Fortran compiler documentation: http://ahamodel.uib.no/intel/
!!   (note that the model code targets version 17.0)
!!   - Automatic parallelisation is [here](http://ahamodel.uib.no/intel/index.htm#GUID-06B54325-1C5C-41E7-A9CD-0E3A8542DC05.html)
!!   .
!! - Doxygen documentation: http://www.doxygen.org/
!!   - Special tags: http://www.stack.nl/~dimitri/doxygen/manual/commands.html
!!   - Formulas: http://www.stack.nl/~dimitri/doxygen/manual/formulas.html
!!   .
!! - GNU Make: https://www.gnu.org/software/make/
!! .
!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!> @section aha_builblocks_main Building blocks of the AHA model
!> @subsection aha_buildblocks_environ The environment
!! The environment is a full 3D space, that is has the class
!! the_environment::spatial as the elementary base primitive. The
!! the_environment::spatial is a single object that has *X, Y and Z* (depth)
!! coordinates. the_environment::spatial_moving extends the basic
!! the_environment::spatial object by allowing it to move. Furthermore,
!! the_environment::spatial_moving includes a history stack that records the
!! latest history of such a spatial moving object. Examples of spatial moving
!! objects can be food items (the_environment::food_item), predators
!! (the_environment::predator), and more complex objects composed of several
!! the_environment::spatial components like the_environment::environment.
!!
!! The basic environment where the agents "live" is very simplistic in this
!! version of the model. It is just and empty box. The box is delimited by the
!! basic environmental container: the_environment::environment class.
!! The the_environment::habitat class is the ecological "habitat", an extension
!! of the basic the_environment::environment that adds various ecological
!! characteristics and objects such as the_environment::food_resource, array
!! of the_environment::predator objects etc.
!!
!! Normally, the movement of the agent is limited to a specific
!! the_environment::environment container with its own the_environment::habitat.
!! All the habitats that are available for the agents are arranged into a
!! single global public array the_environment::global_habitats_available.
!!
!! @ref aha_buildblocks_individual "The individual agent" is also an extension
!! of the the_environment::spatial class: the_environment::spatial →
!! the_environment::spatial_moving → the_genome::individual_genome → ... →
!! the_population::member_population.
!!
!! @image html aha_environment_diagr.svg
!! @image latex aha_environment_diagr.eps "Environmental objects" width=14cm
!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!!
!> @subsection aha_buildblocks_genome The genome structure
!! A brief outline of the genetic architecture, defined in the_genome, is
!! presented on this scheme.
!! - the_genome::gene
!! - the_genome::chromosome
!! - the_genome::individual_genome
!! .
!> @subsubsection aha_buildblocks_genome_gene Gene
!! The agent has genes (class the_genome::gene) that are arranged into
!! chromosomes (class the_genome::chromosome). Each gene also
!! includes an arbitrary number of additive components (see the_genome::gene).
!! @image html aha_genome_01.svg
!! @image latex aha_genome_01.eps "The genome structure" width=14cm
!> @subsubsection aha_buildblocks_genome_chromosome Chromosome
!! Here is a brief outline of the chromosome structure. The chromosomal
!! architecture allows arbitrary ploidity (however haploid is not supported,
!! although can be easily added), i.e. agents with diploid and polyploid
!! genomes can be implemented. Ploidity is defined by a single parameter
!! commondata::chromosome_ploidy.
!!
!> Correspondence between the genotype and the phenotype (hormones,
!! neurobiological modules etc.) is represented by boolean
!! **Gene x Phenotype matrices**. Any arbitrary structure can be implemented,
!! many traits controlled by a single gene, many genes controlling a specific
!! single trait.
!! @anchor aha_buildblocks_gp_matrix_intro
!! @image html aha_genome_02.svg
!! @image latex aha_genome_02.eps "Genotype x phenotype matrix" width=14cm
!!
!! An example of such a structure is the genetic sex determination:
!!  commondata::sex_genotype_phenotype.
!!
!! There is a small utility script `tools\gpmatrix.tcl` that assists in
!! automatic production of the Fortran code for such a matrix.
!! @image html img_doxygen_gpmat.png "gpmatrix.tcl utility"
!! @image latex img_doxygen_gpmat.png "gpmatrix.tcl utility" width=12cm
!! @remark Windows distribution for Tcl/Tk language is obtained
!!         [here](http://www.activestate.com/activetcl)).
!!
!> @subsubsection aha_buildblocks_genome_genome Individual genome
!! The the_genome::individual_genome class defines the basic higher-level
!! component properties of the whole organism, such as sex (logical type
!! the_genome::individual_genome::sex_is_male), genome size
!! (the_genome::individual_genome::genome_size), individual string "name"
!! of the agent (the_genome::individual_genome::genome_label) etc.
!!
!! The the_genome::individual_genome class also includes such type-bound
!! procedures as  the_genome::individual_genome::lives() that gives the agent
!! the "alive" status,  the_genome::individual_genome::dies() for making the
!! agent "dead". It also has linked procedures implementing genetic crossover
!! - the_genome::individual_genome::recombine_random(),
!! - the_genome::individual_genome::recombine_partial(),
!! - the_genome::individual_genome::crossover().
!! .
!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!!
!> @subsection aha_buildblocks_individual The individual agent
!> The individual agent has a `the_environment::spatial_moving` class as its
!! base (but this class is an extension of the simple the_environment::spatial)
!! and is then composed of several layers by class extensions.
!!
!! Each of these main layers that create the individual agent is defined in
!! separate Fortran module:
!! - genome (the_genome module);
!! - hormones (the_hormones module);
!! - the body characteristics and condition (the_body);
!! - neurobiological architecture (the_neurobio);
!! - behaviour architecture (the_behaviour) that builds on the neurobiology;
!! - finally, the agent is a member of a population (the_individual and
!!   the_population::member_population).
!! .
!!
!! @image html aha_individual_class.svg
!! @image latex aha_individual_class.eps "The individual class layers" width=14cm
!!
!! The model code benefits from Fortran intrinsic "elemental" and array-based
!! procedures. To initialise a whole population of random agents (with full
!! object hierarchy) use the the_population::population::init() method:
!! @code
!!   call generation_one%init(POPSIZE)
!! @endcode
!! Invocation of the 'init' method calls a whole cascade of various elementary
!! object-bound procedures that create the whole population object.
!!
!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!! @subsection aha_buildblocks_visrange Localist interactions
!! The model is based on (almost) fully proximate and localist philosophy.
!! All objects (agents, predators, prey) can obtain information about only
!! those objects that in proximity. No one is considered omniscient. Thus,
!! objects can only interact locally, e.g. the agent can eat only food items
!! that it could see, a predator could only attack agents that are visible to
!! it etc.
!!
!! The distance at which the objects can get information about each other
!! is based on the visibility (visual range). Thus, the agents and predators
!! are considered to be fully visual creatures that can only sense the world
!! with their vision.
!!
!! Visibility, i.e. the distance from which an object can be detected ("seen")
!! depends on the ambient illumination at a specific depth, the area of the
!! object, its contrast etc. Visual range is calculated for the different
!! kinds of objects using the the_environment::visual_range() backend.
!!
!! Examples of the visual range are the visibility distance of an agent
!! the_body::condition::visibility(), food item
!! the_environment::food_item::visibility(), and predator
!! the_environment::predator::visibility().
!!
!! Importantly, the @ref aha_buildblocks_percept "perception" of various
!! kinds of environmental objects by the agent uses the
!! the_environment::visual_range() calculation engine.
!!
!! Furthermore, probabilities of stochastic events typically have non-linear
!! relationships with the visual range. One example is the probability of
!! capture of a food item by the agent. This probability is high in close
!! proximity but strongly reduces at the distance equal to the visual range
!! limit: the_environment::food_item::capture_probability(). Similarly, the
!! risk that the agent is killed by a predator is highest at a small distances
!! and significantly reduces at a distance equal to the visibility limit (i.e.
!! the maximum distance the predator can see the agent):
!! the_neurobio::perception::risk_pred(). A more complex procedure is
!! implemented for the probability of successful reproduction:
!! the_neurobio::appraisal::probability_reproduction().
!!
!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!! @subsection aha_buildblocks_cogn_arch The Cognitive Architecture
!! The cognitive architecture of the agent is represented on the scheme below.
!! It includes several functional units, "bundles," each representing specific
!! motivation or emotion state.
!! @note Note that the scheme below includes three such bundles for the states
!!       *A*, *B*, *C*. For simplicity, there are also only three stimuli
!!       @f$ S_1, S_2, S_3 @f$ .
!!
!> @image html aha_global_scheme_neuroresp.svg "General cognitive architecture of the agent"
!! @image latex aha_global_scheme_neuroresp.eps "General cognitive architecture of the agent" width=14cm
!!
!! **General.** The agent perceives the outer and its own inner environment,
!! obtaining perception (signal) values. The agent also has several motivation
!! (emotional) states, such that only one can be active at any time step of the
!! model. Thus, the states compete for this position. The winning motivation
!! (emotion) becomes the dominant emotional state of the agent, its  Global
!! Organismic State. This @ref aha_buildblocks_neurobioflow_gos
!! "Global Organismic States" of the agent determines how the agent weights
!! different options during its decision making process and therefore
!! determines what kind of behaviour (action) it will execute.
!!
!! **Perception and appraisal.** The agent obtains
!! @ref aha_buildblocks_neurobioflow_perc "perceptions" (*P*) from its
!! external and internal environments. These perceptions are fed into the
!! @ref aha_buildblocks_neurobioflow_appr "Appraisal" modules, separate for
!! each of the motivation/emotion state.
!!
!! Here perception signals are first fed into the
!! @ref aha_buildblocks_neuronal_resp "neuronal response functions" (based on
!! the sigmoidal function commondata::gamma2gene()). The neuronal response
!! function is a function of both the perception signal (*P*) and the
!! genome (*G*) of the agent. Perception signal is also distorted by a
!! random Gaussian error. Each neuronal response function returns the neuronal
!! response (*R*) value.
!!
!! The neuronal responses *R* for each stimulus are summed for the same
!! motivation module to get the *primary motivation* values
!! (@f$ M_1 @f$) for this motivational state. These primary motivations can
!! then be subjected to genetic or developmental (e.g. age-related)
!! **modulation**, resulting in the *final motivation* values (@f$ M_{f} @f$).
!! Such modulation could strengthen or weaken the motivation values and
!! therefore shift the outcome of competition between the different
!! motivational states. In absence of modulation @f$ M_1 = M_{f} @f$.
!!
!! **Global Organismic State.** Final motivations (@f$ M_{f} @f$) for different
!! motivations (emotions) are competing, so that the winning state that is
!! characterised by the highest final motivation value becomes the dominant
!! emotional state of the agent: its @ref aha_buildblocks_neurobioflow_gos
!! "Global Organismic State" at the next time step. Additionally, the final
!! motivation value of this state becomes the *GOS arousal* level.
!!
!! The competition mechanism is complex and dynamic. It depends on the
!! current arousal level, such that relatively minor fluctuations in the
!! stimuli and their associated motivation values are ignored and do not
!! result in switching of the GOS to a different state.
!!
!! Furthermore, the relative difference (surplus) that the competing motivation
!! must have to win competition against the current state depends on the
!! current level of GOS arousal. If the current arousal level of the agent is
!! relatively low (motivation or emotion is weak), a competing state must
!! exceed a relatively higher threshold to win. However, if the current arousal
!! level is very high (high motivation or emotion), a competing state can win
!! even if it only slightly exceeds the current arousal level.  Thus, the
!! emotional state of the agent is characterised by a degree of continuity or
!! "inertia" and such inetria is lower the higher is the current level of
!! arousal. The dynamic threshold mechanism for GOS competition is described
!! in details in the_neurobio::gos_find_global_state() procedure documentation
!! section.
!!
!! **Attention focus.** Whenever the agent has a specific Global Organismic
!! State, this state also affects the agent's *perception*. All the perception
!! inputs that belong to motivations other than the currently dominant (i.e.
!! the current GOS) are suppressed by *attention weights*. For example, if the
!! motivation *B* is the GOS, all the perception values linked with Motivation
!! *A* and *C* are suppressed. The suppression weights are proportional to the
!! current GOS arousal of the agent.
!!
!! Thus, the attention mechanism effectively filters out or "focus" the agent
!! on the stimuli that are linked with the current dominant emotional state.
!! Moreover, the stronger is the current GOS arousal, the stronger is such
!! attention focusing. Attention weight mechanism is described in details in
!! the the_neurobio::gos_attention_modulate_weights() procedure section.
!!
!! **Perception-to-arousal path.** This process, *perception → neuronal
!! response → motivation → GOS → arousal* is repeated at each time step of the
!! model as the agent acts in (e.g. moves through) its stochastic environment.
!! In effect, the dominant motivational and emotional state of the agent
!! changes adapting to to the latest changes in the inner and external
!! environment.
!!
!! **Self-predictive decision making**. Furthermore, the same processes (and
!! computer code procedures) are also evoked when the agent is *making the
!! decision* about what *behavioural action* to choose at each time step.
!!
!! Basically, the agent predicts what would be its perceptions and, for each
!! of the behavioural action available, runs the same process
!! (perception → neuronal response → motivation → GOS → arousal) and finally
!! selects the behaviour that would result in the lowest predicted GOS arousal.
!! Perceptions in this process are predicted from the agent's internal or
!! local external environment ("fake" perceptions in the `do_this` method for
!! each of the behaviour units). They are also subjected to attention
!! suppression, however, attention weights are transferred from the
!! agent's own current Global Organismic State by
!! the_behaviour::behaviour_base::attention_transfer() method.
!!
!! Thus, decision making of the agent is based on predicting one's own
!! emotional state. The emotional arousal therefore becomes a common currency
!! in decision making. See @ref aha_buildblocks_decision
!! "Predictive decision making" for more details.
!!
!! **Goal directed behaviour.** The cognitive architecture implemented in the
!! AHA model effectively produces goal-directed behaviour in the agents. The
!! "goal" is defined by the evolutionary process, the @ref aha_buildblocks_ga
!! "Genetic Algorighm". Specifically , the target "goal" is to survive, grow,
!! accumulate energy and reproduce. The agents that do best in this respect
!! pass their genes into the next generation.
!!
!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!! @subsection aha_buildblocks_ga The Genetic Algorithm
!! @subsubsection aha_buildblocks_ga1 Fixed explicit fitness GA
!! This version of the Genetic Algorithm (GA) is based on an explicitly
!! defined "fitness" value for each individual agent. The evolution then
!! optimises agents with respect to this fitness: only agents with high
!! fitness values pass their genes into the next generations.
!!
!! This algorithm for this kind of GA is simple and is implemented in the
!! the_evolution::generations_loop_ga() procedure:
!! - Initialise the first generation of agents (population) from random
!!   genomes. This first generation becomes the "parents".
!! - Start the main GA generations loop
!!    - All "parent"-population agents go via the full life cycle
!!      (with many time steps), the_population::population::lifecycle_step().
!!    - Fixed fitness is calculated for each individual agent showing how good
!!      it did during the life cycle by the
!!      the_individual::individual_agent::fitness_calc() method.
!!    - All agents are sorted according to their fitness by
!!      the_population::population::sort_by_fitness().
!!    - Some number of the "best fitting" agents (i.e. those with the highest
!!      fitness values) are selected for the new generation: their genomes
!!      pass to the new "offspring" generation in an unchanged form (i.e.
!!      this is an *elitism*-based GA algorithm): the_evolution::selection().
!!    - The rest of the offspring population are obtained from the genomes of
!!      the best fitting agents, however,
!!        - parent's genomes randomly exchange their genetic material,
!!        - the resulting offspring genomes are subject to random mutations.
!!        .
!!      These steps are implemented in the_evolution::mate_reproduce().
!!    - This new offspring population now becomes the "parents". A mechanism
!!      based on pointer swapping implemented in
!!      the_evolution::generations_swap() avoids copying big arrays of agents.
!!    .
!! - Finally, the algorithm goes to the next generation loop cycle.
!! .
!!
!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!> @subsection aha_buildblocks_lifecycle Life cycle of the agent
!! @subsubsection aha_buildblocks_life_init Initialisation
!! The life cycle of the agent begins with birth. In the first generation of the
!! Genetic Algorithm each agent is initialised from a random genome by calling
!! the the_individual::individual_agent::init() method. This method evokes a
!! cascade of procedures that initialise all levels of the
!! @ref aha_buildblocks_individual "individual agent" object hierarchy.
!!
!! All subsequent generations use pre-existing genomes that are passed from
!! the ancestral agents subject to genetic crossover and mutation. The
!! the_individual::individual_agent::init() procedure has a logical switch that
!! controls if a random genome should be generated.
!!
!! Because the population of agents is an object (class
!! the_population::population), a whole population of agents is initialised
!! by the the_population::population::init() method. After the birth, all
!! agents get random location within their initial the_environment::habitat
!! object by calling the_population::population::scatter_uniform().
!!
!! @subsubsection aha_buildblocks_life_time Life cycle step
!! Then, the agents pass through many time steps (commondata::lifespan).
!! At each time step each agent gets internal and environmental
!! @ref aha_buildblocks_percept "perceptions". Based on these instantaneous
!! perceptions, they obtain their main internal state: the
!! @ref aha_neurobio_flow "Global Organismic State" (GOS). Finally, each agent
!! must make a decision to execute a single specific
!! @ref aha_buildblocks_behaviour "behaviour" depending on its GOS, internal
!! and external environment (@ref aha_buildblocks_percept "perception"). Some
!! examples of such behaviour are eat a food item, approach a conspecific,
!! escape from a predator, move, freeze, migrate to a novel environment,
!! reproduce, etc.
!!
!! Each of the behaviour chosen by the agent has specific consequences and
!! can
!! - affect the agent (e.g. incurs energetic cost, or relocation of the
!!   whole agent into a novel environment, the agent is killed if it does not
!!   escape a nearby predator),
!! - affect the other agents in proximity (e.g. emigration of an agent could
!!   change the risk of predation for other agents by changing the spatial
!!   configuration and therefore modifying predation dilution and confusion
!!   effects for the agents that remain in place),
!! - affect the external environment (e.g. food item disappears when an agent
!!   eat it).
!! .
!!
!! Whenever the agent successfully consumes food items, it grows
!! its mass and length: the_body::condition::mass_grow(),
!! the_body::condition::len_grow().
!! On the other hand, every movement incurs some energetic cost:
!! the_body::condition::cost_swim(). There are also additional costs linked
!! with specific behavioural actions, e.g. food processing cost
!! the_body::condition::food_process_cost() or the cost of successful
!! (the_body::reproduction::reproduction_cost()) or unsuccessful
!! (the_body::reproduction::reproduction_cost_unsuccess()) reproduction.
!!
!! There is also an overall living cost that is subtracted
!! (the_body::condition::subtract_living_cost()). Additionally, digestion
!! occurs by emptying the agent's stomach by a fixed fraction
!! (the_body::condition::stomach_empify() at each time step.
!!
!! @subsubsection aha_buildblocks_life_predation Predation and mortality
!! At each time step, the agent can be subjected by random disastrous events,
!! such as habitat-specific random mortality
!! the_population::population::mortality_habitat() or predatory attacks
!! the_population::population::attacked(). If the predatory attack is
!! successful, the agent the_genome::individual_genome::dies() and cannot
!! normally pass its genome into the next generation.
!!
!! The agents are also checked at various stages of their life cycle for
!! starvation: the_body::condition::starved_death(). If the agent is starved,
!! it also the_genome::individual_genome::dies().
!!
!! Predation risk (the probability of the agent being caught by the predator)
!! is automatically adjusted if the predator can simultaneously perceive
!! several conspecific agents. Depending on the number of such agents in the
!! perceived group and individual distances between the predator and each
!! agent, a predator dilution and confusion effects are calculated (see
!! the_environment::predator::risk_fish_group()). By the way, when the agent
!! makes the decision to approach conspecific in presence of a predator, it
!! also evaluates how many other conspecifics are there and what is
!! the relative distances between the conspecifics and the predator (see
!! the_behaviour::approach_conspec::do_this()). The agent's decision therefore
!! adjusts for the classical "selfish herd" effect.
!!
!! @subsubsection aha_buildblocks_life_compete Food competition
!! Because many agents are dwelling within the same habitat with limited food
!! resource (fixed number of the_environment::food_item objects),
!! they also compete for food. For example, if two agents find themselves near
!! the same food item, one or the other can eat it, whoever is the first to
!! make the decision to the_behaviour::eat_food. If an agent successfully
!! eats such food item, it is marked as the_environment::food_item::eaten and
!! is not available for everyone else.
!!
!! Because the model is @ref aha_buildblocks_visrange  "localist" and
!! explicitly implements individual capture of each food item by each agent
!! within spatially explicit habitats, any density- and frequency-dependent
!! effects would appear automatically.
!!
!! @subsubsection aha_buildblocks_life_reprod Reproduction
!! Whenever the agent the_body::reproduction::is_ready_reproduce(), it can
!! make decision to the_behaviour::reproduce at a specific time step.
!! The probability of successful reproduction in specific stochastic conditions
!! is calculated by the_neurobio::appraisal::probability_reproduction(). As a
!! consequence of successful reproduction, some number
!! (the_body::reproduction::offspring_number()) of offspring are produced.
!! How subjective value of reproduction is evaluated by the agent is dictated
!! by the_behaviour::reproduce::do_this().
!!
!! @subsubsection aha_buildblocks_life_select Agent order and competition
!! To avoid systematic biases, the agents make their behavioural choices in a
!! random order (see
!! [PERMUTE_RANDOM()](http://ahamodel.uib.no/doc/ar01s09.html#_random_permutation_permute_random_function)).
!! However, it is also easy to implement any other sorted order,
!! e.g. selecting agents according to their body weight: the body weight
!! then would provide a strong competitive advantage.
!!
!! Any arbitrary order of the agents can be implemented using
!! [array indexing](http://ahamodel.uib.no/doc/ar01s07.html#_subroutines_array_index_and_array_rank)
!! mechanism. See the_population::population::lifecycle_step() for more
!! discussion.
!!
!! @subsubsection aha_buildblocks_life_conclude Concluding remarks
!! The long sequence of such decision makings in a stochastic environment at
!! each time step  constitutes the life cycle of the agent. Depending on how
!! well the agent does during its life cycle (e.g. grows and reproduces)
!! determines the chance that its genome passes to the next generation by the
!! Genetic algorithm. Thus, the whole process simulates the behaviour, decision
!! making and evolution.
!!
!! The following sections provide general information about the implementation
!! and the object class hierarchy of the model code.
!!
!> @subsection aha_buildblocks_percept The perception mechanism
!> @subsubsection aha_buildblocks_percept_overview Overview
!! Perception is defined in the the_neurobio::perception class. Perception
!! objects can be of three types:
!!  - Internal perception objects, depend on the body.
!!    - the_neurobio::percept_stomach
!!    - the_neurobio::percept_energy
!!    - the_neurobio::percept_age
!!    - the_neurobio::percept_reprfact
!!    .
!!  - Direct environmental perceptions (e.g. light).
!!    - the_neurobio::percept_light
!!    - the_neurobio::percept_depth
!!    .
!!  - External spatial perception objects, depend on the visual range.
!!    - the_neurobio::percept_food
!!    - the_neurobio::percept_predator
!!    - the_neurobio::percept_conspecifics
!!    .
!! .
!! @image html aha_neurobio_percept.svg
!! @image latex aha_neurobio_percept.eps "Perception objects" width=14cm
!!
!> @subsubsection aha_buildblocks_percept_spatial Spatial perceptions
!! External spatial perception components are truly "localist" and proximate,
!! they get individual environmental objects (e.g. food items, individual
!! conspecifics or predators) dynamically as the agent and the spatial objects
!! move within the model 3D environment. This allows aha_buildblocks_gp_matrix_introto produce very complex
!! environmental structures (e.g. patchy foods with Gaussian scatter). Also,
!! every individual environmental object is perceived only if it is within
!! the specific visual range. This means that, as food items are stochastic
!! (have random Gaussian size), perception of each food item depends on its
!! individually-specific visibility range. The same is true also for
!! conspecifics, predators, etc.
!! @image html img_doxygen_spatial_visrange.svg  "Spatial perception"
!! @image latex img_doxygen_spatial_visrange.eps "Spatial perception" width=14cm
!!
!> Selection of the nearest environmental objects that are within the current
!! visual range (the_environment::spatial::neighbours()) is based on partial
!! indexing (spatial segmentation) of potentially huge arrays of different
!! objects (e.g. thousands of individual stochastic food items, each with
!! specific visual range). Partial indexing allows very fast processing of
!! only a subset of spatial objects that are in proximity of the agent (and
!! therefore  could fit into the visibility range), ignoring al other, more
!! remote,  objects in the same environment.
!!
!> @subsection aha_neurobio_flow From perception to GOS
!! The overview below shows the sequence of the main procedures from perception
!! (the_neurobio::perception class) through the_neurobio::appraisal to the
!! determination of the Global Organismic State (GOS, the_neurobio::gos_global
!! class).
!> @image html aha_neurobio_appraisal.svg
!> @image latex aha_neurobio_appraisal.eps "Appraisal objects" width=14cm
!!
!> @subsubsection aha_buildblocks_neurobioflow_perc Perception
!> **Perception**: first, the agent obtains perceptions from its inner
!! environment (such as age, stomach contents etc), the external environment
!!  (e.g. light, depth), as well as spatial perceptions (e.g. food items,
!! conspecifics, predators). These *perception components* are described by
!! the the_neurobio::percept_components_motiv class. Perception
!! components represent a crucial component of each of the *motivational
!! states*. Perceptions are obtained by calling these procedures:
!! - the_neurobio::perception::perceptions_inner() - inner perceptions (an
!!   umbrella for several perceptions), these include:
!!   - the_neurobio::perception::feel_stomach()
!!   - the_neurobio::perception::feel_bodymass()
!!   - the_neurobio::perception::feel_energy()
!!   - the_neurobio::perception::feel_age()
!!   - the_neurobio::perception::feel_repfac()
!!   .
!! - the_neurobio::perception::perceptions_environ() - environmental
!!   perceptions (an umbrella procedure for several perceptions) including
!!   - the_neurobio::perception::feel_light()
!!   - the_neurobio::perception::feel_depth()
!!   .
!! - the_neurobio::perception::see_food() - get food;
!! - the_neurobio::perception::see_pred() - get predators;
!! - the_neurobio::perception::see_consp() - get conspecifics;
!! .
!! Notably, the spatial perceptions dynamically access stochastic external
!! spatial objects using a fast algorithm based on partial distance ranking
!! of spatial objects, a kind of spatial segmentation. This largely reduces
!! the need to multiply loop across the objects and the agents over many
!! time steps and generations.
!> @subsubsection aha_buildblocks_neurobioflow_appr Appraisal
!> **Appraisal**: perceptions are weighted by the *attention weights* and go
!! into the *neuronal response* function
!! the_genome::individual_genome::neuro_resp(), then summed to get the
!! *primary* motivation value for each of the motivation states.
!!
!! #### Neuronal response function ####
!! The neuronal response function @anchor aha_buildblocks_neuronal_resp has
!! a central role in the model. It links the sensory perception from a specific
!! environmental stimulus *P* (including the inner organism's environment), and
!! the strength of the response to this stimulus (the neuronal response) *R*
!! mediated by the genome. Neuronal response function is based on the sigmoidal
!! equation that is implemented in commondata::gamma2gene() procedure.
!!
!! Note that this function is not based on a mechanistic understanding of the
!! relationship between perception and neuronal response. Lacking such
!! understanding, the genetic algorithm may produce a series of potentially
!! adaptive relationships. Depending upon the allele values of the genes, the
!! function can appear concave, sigmoidal, nearly linear, or convex in
!! the *0 ≤ P ≤1* range. We use a sigmoidal function to avoid total lack of
!! (emotional) interest at very low *P*.
!!
!! #### From neuronal responses to motivations and emotions ####
!! There are currently four motivational states that the agent can have:
!! @anchor aha_buildblocks_gp_motivations
!! - the_neurobio::state_hunger
!! - the_neurobio::state_fear_defence
!! - the_neurobio::state_reproduce
!! .
!! All these motivational states represent extensions of the basic *abstract
!! class* the_neurobio::state_motivation_base that is not used directly.
!!
!! Calculating the *primary* motivational values involves calling these
!! procedures:
!! - the_neurobio::appraisal::motivations_percept_components() -- perceptions
!!   are fed into the neuronal response function, obtain perceptual
!!   components for each of the above motivational states ;
!! - the_neurobio::appraisal::motivations_primary_calc() -- perception
!!   components are fed into this procedure resulting in calculation of
!!   *primary* motivations, i.e. values of the above motivational states
!!   before they are subjected to any modulation.
!! .
!! These primary motivation values for each of the above four motivation
!! states are subjected to genetic or developmental modulation, resulting
!! in *final* motivation value for each of the motivational states:
!! - the_neurobio::appraisal::modulation();
!! .
!! They are also recorded into the emotional memory stack by calling
!! - the_neurobio::appraisal::motivations_to_memory();
!! .
!> @subsubsection aha_buildblocks_neurobioflow_gos GOS
!! **GOS**: finally, a single *Global Organismic State* (GOS) is determined
!! on the basis of the competition between the final values of all
!! motivational states. GOS is determined in this procedure:
!! - the_neurobio::gos_global::gos_find();
!!   - gos_find() also internally calls the attention modulation procedure
!!     the_neurobio::gos_global::attention_modulate() that limits attention
!!     all perceptions that are "irrelevant" for the current GOS;
!!   .
!! .
!! @subsubsection aha_buildblocks_neurobioflow_code Code example
!! The code below is an example of the above steps, from
!! @ref aha_buildblocks_neurobioflow_perc "perception" to @ref
!! aha_buildblocks_neurobioflow_appr "appraisal" and
!! @ref aha_buildblocks_neurobioflow_gos "GOS":
!! @code{.f90}
!!   ! Perception:
!!   call proto_parents%individual(ind)%perceptions_inner()
!!   call proto_parents%individual(ind)%perceptions_environ()
!!
!!   call proto_parents%individual(ind)%see_food( food_resource )
!!   call proto_parents%individual(ind)%see_consp( proto_parents%individual )
!!   call proto_parents%individual(ind)%see_pred( habitat_safe%predators )
!!
!!   ! Add perceptions to the memory:
!!   call proto_parents%individual(ind)%perception_to_memory()
!!
!!   ! Appraisal:
!!   call proto_parents%individual(ind)%motivations_percept_components()
!!   call proto_parents%individual(ind)%motivations_primary_calc()
!!   call proto_parents%individual(ind)%modulation()
!!   call proto_parents%individual(ind)%motivations_to_memory()
!!
!!   ! GOS:
!!   call proto_parents%individual(ind)%gos_find()
!! @endcode
!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!!
!> @subsection aha_buildblocks_behaviour The behavioural repertoire
!> The *behavioural repertoire* of the agent is composed of several *behaviour
!! units*, which are extensions of the basic the_behaviour::behaviour_base class
!! (this is an abstract class and is not used directly). Thus, each component
!! of the behavioural repertoire is a *separate object* `class` not linked
!! with the *agent* class hierarchy (from the_genome::individual_genome up to
!! the_neurobio::gos_global).
!!
!! However, all the individual components of the behavioural repertoire are
!! collected together in the the_behaviour::behaviour class (that is in the
!! agent class hierarchy). Thus, the behavioural repertoire of the agent is
!! here *constructed* from individual behavioural components.
!!
!! @image html aha_neurobio_behav.svg "Construction of the behaviour"
!! @image latex aha_neurobio_behav.eps "Construction of the behaviour" width=14cm
!!
!> @subsubsection aha_buildblocks_behav_elements Behavioural units
!! Here is the behavioural repertoire of the agent:
!! - the_behaviour::eat_food
!! - the_behaviour::reproduce
!! - the_behaviour::walk_random
!! - the_behaviour::freeze
!! - the_behaviour::escape_dart
!! - the_behaviour::approach
!! - the_behaviour::approach_conspec
!! - the_behaviour::migrate
!! - the_behaviour::go_down_depth
!! - the_behaviour::go_up_depth
!! .
!! The inheritance structure of these behavioural units is shown on the scheme
!! below.
!!
!! @image html aha_neurobio_behaviour.svg "The behaviour repertoire"
!! @image latex aha_neurobio_behaviour.eps "The behaviour repertoire" width=10cm
!!
!! Some of the behaviours involve "movement", i.e. change of the spatial
!! location. Such behaviours are programmed as extensions of the base class
!! the_behaviour::move (this is also an abstract class that is not used
!! directly).
!! - the_behaviour::walk_random
!! - the_behaviour::freeze
!! - the_behaviour::escape_dart
!! - the_behaviour::approach
!! - the_behaviour::approach_conspec (an extension of the_behaviour::approach)
!! - the_behaviour::migrate
!! - the_behaviour::go_down_depth
!! - the_behaviour::go_up_depth
!! .
!! Each of these *movements* can differ by its *distance* or length (although
!! the_behaviour::freeze always has zero distance):
!! the_behaviour::move::distance. Thus, there can be a whole repertoire of
!! each of the above movements, such as Gaussian random walks or vertical
!! migrations with different steps.
!!
!! the_behaviour::eat_food also involves movement but indirectly, so it is not
!! related to the the_behaviour::move base class.
!!
!! Finally, certain behaviours involve specific "target object":
!! - the_behaviour::eat_food
!! - the_behaviour::escape_dart
!! - the_behaviour::approach
!! - the_behaviour::approach_conspec
!! - the_behaviour::migrate
!! .
!! Such a target can be, for example, food items (the_environment::food_item),
!! predators (the_environment::predator), conspecifics
!! (the_neurobio::appraisal) or an environmental container
!! (the_environment::environment).
!!
!! Combinations of several behavioural units with multiple *distances* (for
!! movements) and *targets* creates an additional diversity, complexity and
!! flexibility of the behaviour in the agents.
!!
!> @subsubsection aha_buildblocks_behav_complex Double role of motivation
!! Notably, the the_neurobio::motivation class is included both into the
!! **agent's** the_neurobio::appraisal (motivations) as well as  into the
!! **behaviour unit** the_behaviour::behaviour_base (expectancy).
!!
!! Such a complex design composed of separate behavioural units (separate
!! classes although inheritances of the same base the_neurobio::motivation)
!! makes it possible to generate behaviours that:
!! - depend on inputs from the_neurobio::perception of the agent, both
!!   "objective" perceptions that flow from the environment and "subjective"
!!   perceptions that are internally generated as a part of the subjective
!!   evaluation of various possible outcomes of behaviour;
!! - link **perception** to **motivation** both in the agent itself and
!!   in each of the behavioural units;
!! - have multiple behaviours with their linked motivational expectancies that
!!   are collected (plugged) back into the agent class hierarchy;
!! - so that the agent can compare the motivational expectancies from each
!!   of the possible behaviour units, also in response to each of the multiple
!!   perception targets (e.g. specific food items, conspecifics and predators);
!! - the agent can select the behaviour (and the specific target) that depends
!!   on the expected arousal, for *execution*;
!! - finally, this optimal behaviour is "executed" (with its specific target)
!!   by calling its specific `do_` method in the the_behaviour::behaviour class.
!! - exactly the same class data structures and procedure(s) are employed
!!   to calculate the motivation values:
!!   - for the the_neurobio::appraisal::motivations of the **agent** and
!!   - for motivational the_behaviour::behaviour_base::expectancy of the each
!!     **behaviour unit**.
!!   .
!! .
!!
!! @image html aha_neurobio_motivation.svg
!! @image latex aha_neurobio_motivation.eps "Motivation and behaviour" width=14cm
!!
!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!> @subsection aha_buildblocks_decision Predictive decision making
!> @subsubsection aha_buildblocks_decis_overview Overview
!! Decision making in the AHA Model is centered around three concepts:
!! - motivation (emotion)
!! - global organismic state (GOS)
!! - expected arousal
!! .
!! The main principle of decision maming is simple. It involves evaluating
!! each of the available behavioural alternative (each behaviour unit with
!! specific parameters and targets) to find those alternative which minimises
!! the expected arousal. This option is then considered the *optimal* behaviour
!! and and is finally *executed*. This is a kind of a "brute force"
!! optimisation mechanism that aims to get the most optimal behavioiral
!! outcome from the agent's knowledge of the local environment and its own
!! internal state.
!!
!! Thus, each agent evaluates the environment in the context and in terms of
!! *its own predicted internal state*. Thie outcome of such a process therefore
!! depends both on the local instantaneous environmental context (depicted by
!! the agent's local perception of food, conspecifics and predators), and the
!! current internal state of the agent (its current motivation, the global
!! organismic state, GOS).
!!
!! The emotional state of the agent represents the common currency, the GOS
!! arousal, for predicting the optimal behaviour: selection of a single action
!! out of numerous available alternatives.
!!
!> @subsubsection aha_buildblocks_decis_behaviour The umbrella class
!! *Selection* and *execution* of the optimal behaviours is implemented in the
!! the_behaviour::behaviour class, which is a pert of the individual agent class
!! hierarchy. The the_behaviour::behaviour class collects all the separate
!! behavioural units ans thereby serves an umbrella construict for the whole
!! behavioural repertoire of the agent. It also includes high level execution
!! procedures for each of the behavioural unit:
!! - the_behaviour::behaviour::do_eat_food_item();
!! - the_behaviour::behaviour::do_reproduce();
!! - the_behaviour::behaviour::do_walk();
!! - the_behaviour::behaviour::do_freeze();
!! - the_behaviour::behaviour::do_escape();
!! - the_behaviour::behaviour::do_approach();
!! - the_behaviour::behaviour::do_migrate();
!! - the_behaviour::behaviour::do_go_down();
!! - the_behaviour::behaviour::do_go_up();
!! .
!! These execution "do_" procedures basically call the "execute" methods for
!! their respective specific behaviour unit (each behaviour unit is implemented
!! as a separate class class, see @ref aha_buildblocks_behav_elements
!! "Behavioural units"). For example,
!! the_behaviour::behaviour::do_eat_food_item() calls
!! the_behaviour::eat_food::execute(). This provides a connection between
!! the the_behaviour::behaviour umbrella class and each of the
!! @ref aha_buildblocks_behav_elements "behavioural units". Furthermore,
!! this connection allows to call a whole cascade of methods to assess and
!! predict possible consequences of executing each of the possible behaviour.
!!
!> @subsubsection aha_buildblocks_decis_do_behave do_behave method
!! The the_behaviour::behaviour class also implements the
!! the_behaviour::behaviour::do_behave() method which provides a unitary
!! interface for the evaluation, selection and execution of the optimal
!! behaviour unit, i.e. the behaviour minimising the expected GOS arousal.
!!
!! The the_behaviour::behaviour::do_behave() method
!! - Evaluates each of the available behavioural alternative given the
!!   specific current context (internal and external perception objects).
!!   This is implemented by the various lower order `select` procedures
!!   within `do_behave()`:
!!    - ::eat_food_select();
!!    - ::reproduce_select();
!!    - ::walk_random_select();
!!    - ::freeze_select();
!!    - ::escape_dart_select();
!!    - ::approach_consp_select();
!!    - ::migrate_select();
!!    - ::go_down_select();
!!    - ::go_up_select().
!!    .
!!   These `select` procedures calculate the arousal expectancy for each
!!   of the behavioural unit. If specific behaviour unit has multiple
!!   targets (e.g. several food items or several conspecifics) or parameters
!!   (e.g. random walks of different Gaussian length or vertical migrations
!!   with different step sizes), each such option is also evaliuated and the
!!   optimal one is determined (e.g. the "best" food item, eating which would
!!   result in lowest arousal).
!! - Once the expected arousal level is known for each of the behaviours, the
!!   procedure determines the one that minimises the GOS arousal overall.
!! - Finally, the behaviour (with specific optimal target or parameters) that
!!   would minimise the resulting GOS arousal is executed by calling respective
!!   `do_` procedure (e.g. the_behaviour::behaviour::do_eat_food_item() for the
!!   optimal food item if feeding was selected as minimising the overall
!!   arousal).
!! .
!! This process is schematically depicted in the figure below.
!! @image html img_doxy_predictive.svg "Predictive decision making by minimising expected GOS arousal"
!! @image latex img_doxy_predictive.eps "Predictive decision making by minimising expected GOS arousal" width=14cm
!!
!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!-------------------------------------------------------------------------------
!> @brief   COMMONDATA -- definitions of global constants and procedures
!> @section commondata_module Commondata module
!> Module **COMMONDATA** is used for defining various global
!! parameters like model name, tags, population size etc.
!! Everything that has global scope and should be passed to many
!! subroutines/functions, should be defined in `COMMONDATA`.
!! It is also safe to include public keyword to declarations.
!! `COMMONDATA` may also include subroutines/functions that have
!! general scope and used by many other modules of the model.
!! ### Accessibility of objects ###
!! By default, all **data objects** in `COMMONDATA` should be accessible to all
!! other modules. However, procedures defined here must have granular access
!! rights, with the generic name declared *public* while specific
!! implementations *private*.
module COMMONDATA

  use, intrinsic :: ISO_FORTRAN_ENV
  use BASE_UTILS
  use LOGGER
  use BASE_RANDOM
  use BASE_STRINGS,     only : LOWERCASE
  use CSV_IO,           only : MAX_FILENAME, CSV_MATRIX_WRITE,                &
                               CSV_FILE_LINES_COUNT, GET_FREE_FUNIT, FS_UNLINK
  use BASE_STRINGS,     only : PARSE
  implicit none

  ! Access note:
  ! By default, all **data objects** in COMMONDATA should be accessible to all
  ! other modules. However, procedures defined here must have granular access
  ! rights, with the generic name *public* while specific implementations
  ! *private*.
  public

  !.............................................................................
  !.............................................................................

  ! Doxygen comments note: The @name  tag defines an arbitrary **member group**
  !                        of Doxygen objects. Membership range is defined by
  !                        the @{  and @} tags.

  !> @name Precision control for real type and IEEE float math in the model
  !! @{

  ! Precision control for real type and IEEE float math in the model
  ! ================================================================

  !> Standard precision for real data type. We first define 32, 64 and
  !! 128 bit real kinds.
  !! @warning `HEDTOOLS` cannot accept precision higher than kind 8 so far.
  !!          So 128 bit reals are for example only here. Have to implement
  !!          higher precision `HEDTOOLS` routines if they are really used.
  integer, parameter, public :: S_PREC_32  = selected_real_kind( 6,   37)
  integer, parameter, public :: D_PREC_64  = selected_real_kind(15,  307)
  integer, parameter, public :: Q_PREC_128 = selected_real_kind(33, 4931)

  !> @brief   Definition of the **standard** real type precision (*SRP*).
  !> @details `SRP` is defined as the standard precision that should normally
  !!          be used for all real variables and constants. **SRP** stands for
  !!          **Standard Real Precision** (Naming note: const name should be
  !!          short to not produce too long real definitions, e.g.
  !!          `real(SRP) :: alpha`).
  !! @warning All float (and other) constants should ideally be defined in the
  !!          definition section of COMMONDATA or another module, **not** in
  !!          the code. It is for easier maintainability and precision control.
  !! @warning **All standard real variables** should be defined as:
  !!          `real(SRP) :: real_var`.
  !!          **Literal constants** should normally add `_SRP`: `1.234_SRP`
  !!          (although it is less crucial).
  integer, parameter, public :: SRP = S_PREC_32

  !> Definition of the **high** real precision (**HRP**).  This real type kind
  !! is used in pieces where a higher level of FPU precision is required, e.g.
  !! to avoid overflow/underflow and similar errors.
  integer, parameter, public :: HRP = Q_PREC_128

  !> In some (perhaps quite rare) cases of exponentiation we may also need huge
  !! integers, those in 64 bit would probably be enough. So whenever we need
  !! such a big integer, declare it as:
  !! @verbatim
  !!   integer(LONG) :: bignum
  !! @endverbatim
  !! @warning HEDTOOLS **do not** currently work with these `LONG`
  !!          kind integers. So they are only for "internal"-calculation use.
  !!          Alternatively, use the intrinsic function `int` to convert to the
  !!          default integer type inline before use, e.g.:
  !!          `TOSTR(int(max_permutations))`.
  integer, parameter, public :: LONG = selected_int_kind(16)

  !> @}

  !.............................................................................
  !.............................................................................

  !> @name Accessory parameters
  !! @{

  ! Accessory parameters
  ! ====================

  !> MODNAME always refers to the name of the current module for use by
  !! the LOGGER function LOG_DBG. Note that in the @ref intro_debug_mode
  !! "debug mode" (if IS_DEBUG=TRUE) LOGGER should normally produce additional
  !! messages that are helpful for debugging and locating possible sources of
  !! errors. MODNAME is declared private and is not accessible outside of this
  !! module. Each procedure should also have a similar private constant
  !! `commondata::procname`.
  !! @note `MODNAME` must have the same case as the module name itself and must
  !!        be enclosed in parentheses, e.g. `"(THE_MODULE)"`.
  character(len=*), parameter, private :: MODNAME = "(COMMONDATA)"

  !> PROCNAME is the procedure name for logging and debugging
  !! (with `commondata::modname`).
  !! @note `PROCNAME` must have the same case as the subroutine itself and must
  !!        be enclosed in parentheses, e.g. `"(function_or_subroutine_name)"`,
  !!        so that it is easier to find in the code and easy to search by
  !!        regex in the long output logs (use parentheses to search).
  !!        Here is a template to insert into the code (procedure name is to
  !!        be filled in the parentheses):
  !!        @code
  !!          ! PROCNAME is the procedure name for logging and debugging
  !!          character(len=*), parameter :: PROCNAME = "()"
  !!        @endcode
  character(len=*), parameter, private :: PROCNAME = ""

  !> **Subversion** or *Mercurial* revision number (or ID) of the model code.
  !! @warning The revision string is **updated automatically** at svn commmit
  !!          (or hg commit). It is also not fully portable and may **not**
  !!          auto update if a different version control system is used.
  !!          Note that *Mercurial* has a **keyword** extension that works
  !!          similar to Subversion and should auto-update the keywords.
  character(len=*), parameter, public ::                                      &
      SVN_VERSION_STRING = "$Revision: 851df748b488 $"

  !> **Subversion** or *Mercurial* revision number that is parsed by
  !! `commondata::parse_svn_version()`. It is shorter than
  !! `commondata::svn_version_string` and does not contain blanks.
  !! Therefore, it can be used for building output file names.
  !! @note    Note that the SVN parse function is called at initialising the
  !!          log `LOGGER_INIT`, so `SVN_Version` is initialised.
  !! @warning `SVN_Version` is a string, version ID in *Subversion* is numeric,
  !!          but in other version control systems (hg or git) it can be an
  !!          arbitrary non-numeric hash string.
  !! @note    Because it has allocatable attribute, its actual length is
  !!          obtained automatically and no `trim(SVN_Version)` is necessary.
  character(len=:), allocatable, protected, public :: SVN_Version

  !> Safety parameter avoid errors in logical values, so we can now
  !! refer to standard Fortran `.TRUE.` and `.FALSE.` as `YES` and `NO`
  !! or `TRUE` and `FALSE`
  logical, parameter, public :: TRUE=.TRUE., FALSE=.FALSE.
  logical, parameter, public :: YES=.TRUE.,  NO=.FALSE.

  !> Some parameters should never be zero or below. In such cases they could
  !! be set to some smallest distinguishable non-zero value. Here set as
  !! the Fortran intrinsic `epsilon` function, a value that is almost
  !! negligible compared to one, i.e. the smallest `real` number *E* such
  !! that @f$ 1 + E > 1 @f$. In some cases it is also reasonable to set the
  !! tolerance limit to this parameter (see @ref intro_computation_real
  !! "Float point computations").
  !! @note The value of this parameter computed on a x86_64 Linux platform is
  !!       `1.19209290E-07`.
  real(SRP), parameter, public :: ZERO = epsilon(0.0_SRP)

  !> The smallest positive number in the commondata::srp standard real model.
  !! @note This parameter is used for definition of numerical tolerance.
  !! See @ref intro_computation_real "Float point computations".
  !! @note The value of this parameter computed on a x86_64 Linux platform is
  !!       `1.17549435E-38`.
  real(SRP), parameter, public :: TINY_SRP=tiny(1.0_SRP)

  !> The smallest positive number in the commondata::hrp high precision real
  !! model. See @ref intro_computation_real "Float point computations".
  !! @note This parameter is used for definition of numerical tolerance.
  real(HRP), parameter, public :: TINY_HRP=tiny(1.0_HRP)

  !> Default value of *low* tolerance (*high precision*). This is the
  !! *standard* commondata::srp precision. See @ref intro_computation_real
  !! "Float point computations".
  !! @note The value of this parameter computed on a x86_64 Linux platform is
  !!       `5.87747175E-38`.
  real(SRP), parameter, public :: TOLERANCE_LOW_DEF_SRP = TINY_SRP * 5.0_SRP

  !> Default value of *low* tolerance (*high precision*). This is the *high*
  !! commondata::hrp precision. See @ref intro_computation_real
  !! "Float point computations".
  real(HRP), parameter, public :: TOLERANCE_LOW_DEF_HRP = TINY_HRP * 5.0_HRP

  !> Default value of *high* tolerance (*low precision*). This is the
  !! *standard* commondata::srp precision real. See @ref intro_computation_real
  !! "Float point computations".
  !! @note The value of this parameter computed on a x86_64 Linux platform is
  !!       `1.19209290E-04`.
  real(SRP), parameter, public :: TOLERANCE_HIGH_DEF_SRP = ZERO * 1000.0_SRP

  !> Default value of *high* tolerance (*low precision*). This is the
  !! *high* commondata::hrp precision real. See @ref intro_computation_real
  !! "Float point computations".
  !! @note The value of this parameter computed on a x86_64 Linux platform is
  !!       `1.92592994438723585305597794258492732E-0031`.
  real(HRP), parameter, public :: TOLERANCE_HIGH_DEF_HRP =                    &
                                                  epsilon(0.0_HRP) * 1000.0_HRP

  !> Numerical code for *missing* and *invalid* **real type** values.
  !! @note We deliberately set an unisually *big negative value* for `MISSING`
  !!       because it will reveal bugs by clearly strange/invalid negative
  !!       results that will propagate in calculations.
  !! @warning It is **safe** to assign integer `UNKNOWN` constant to a **real**
  !!          type variable, see the next definition.
  real(SRP), parameter, public :: MISSING = -9999.0_SRP, INVALID = -9999.0_SRP

  !> Numerical code for invalid or *missing* **integer** counts.
  !! @note It is safe to assign integer `UNKNOWN` to a **real** type variable,
  !!       e.g. `real(SRP) :: value=UNKNOWN`.
  integer, parameter, public :: UNKNOWN = -9999

  !> @brief   The **PI** number.
  !! @details Pi number @f$ \pi = 4.0 \cdot tg(1.0) @f$ `[4.*atan(1.0)]`,
  !!          numerically equal to (64 bit real)
  !!          `PI=3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803_Q_PREC_64`.
  real(SRP), parameter, public :: PI=4.0_SRP*atan(1.0_SRP)

  !> Standard data file extension for data output is now .csv
  character(len=*), parameter, public :: csv=".csv"

  !> Standard file extension for debug and other PostScript plots
  character(len=*), parameter, public :: PS=".ps"

  !> Set the standard length of the file name, are 255 characters enough?
  !! @warning Do not forget to use `trim()` function to delete trailing
  !!          spaces from the file name if it is declared as a fixed-length
  !!          character string.
  integer, parameter, public :: FILENAME_LENGTH = 255

  !> Logical flag for setting if POSIX direct filesystem procedures are used.
  !! These utilities are implemented in HEDTOOLS for standard POSIX C call
  !! via the Fortran interface. They should work safer, better and faster
  !! than indirect procedure wrappers (e.g. calling `system()`) but are not
  !! fully portable and might not work as expected on all systems and
  !! compilers.
  logical, parameter, public :: USE_POSIX_FS_UTILS = .TRUE.

  !> The length of standard character string labels. We use labels for various
  !! objects, like alleles, perceptual and neural components / bundles etc.
  !! For simplicity, they all have the same length. It should be big enough to
  !! fit the longest whole label
  integer, parameter, public :: LABEL_LENGTH = 14

  !> The length of long labels.
  integer, parameter, public :: LONG_LABEL_LENGTH = 128

  !> This parameter defines the range of characters that is used for generating
  !! random labels, 97:122 corresponds to lowercase Latin letters
  integer, parameter, public :: LABEL_CST = 97, LABEL_CEN = 122

  !> The name of the lock file. The lock file is created at the start of the
  !! simulation and is deleted at the end of the simulation. It can be used to
  !! signal that simulation is still ongoing to external utilities and scripts.
  !! See @ref intro_overview_lockfile "The lock file".
  !! @note    Lock file operation uses native Fortran intrinsic `open`
  !!          statement rather than any higher level procedures like
  !!          @ref file_io. This is because the file is for signalling
  !!          only (intended to be empty) and nothing is actually written
  !!          into it.
  character(len=*), parameter :: LOCK_FILE = "lock_simulation_running.lock"

  !> This is the unit number that identifies the lock file. The lock file is
  !! created at the start of the simulation and is deleted at the end of the
  !! simulation. It can be used to signal that simulation is still ongoing to
  !! external utilities and scripts.
  !! See @ref intro_overview_lockfile "The lock file".
  !! @note    Lock file operation uses native Fortran intrinsic `open`
  !!          statement rather than any higher level procedures like
  !!          @ref file_io. This is because the file is for signalling
  !!          only (intended to be empty) and nothing is actually written
  !!          into it.
  integer, protected, public :: Lock_File_Unit

  !> Runtime platform ID constants. Use these constants for determining the
  !! current runtime platform, e.g. `Platform_Running = PLATFORM_WINDOWS`.
  !! See `commondata::platform_running`.
  integer, parameter, public :: PLATFORM_WINDOWS = 100
  integer, parameter, public :: PLATFORM_UNIX    = 111

  !> Global variable that shows what is the current platform. Should use the
  !! above platform constants, e.g. `Platform_Running = PLATFORM_WINDOWS`.
  !! See `commondata::platform_windows` and `commondata::platform_unix`.
  integer, public :: Platform_Running

  !> There are a few **external programs** which are called from the model code.
  !! The name of the **interpolation** program (htintrpl.f90 from HEDTOOLS)
  !! executable.
  character(len=*), parameter, public :: EXEC_INTERPOLATE = "htintrpl.exe"

  !> The name of the **scatterplot** program (htscatter.f90 from HEDTOOLS)
  !! executable.
  character(len=*), parameter, public :: EXEC_SCATTERPLOT = "htscatter.exe"

  !> The name of the **histogram** program (hthist.f90 from HEDTOOLS)
  !! executable.
  character(len=*), parameter, public :: EXEC_HISTOGRAM = "hthist.exe"

  !> **Tag prefixes** for the logger system. The log may use tags for
  !! some common information pieces, so they are easily found within.
  !! The tags are normally set the prefix for the log:
  !!    017-01-31 13:33:22 INFO: Saving histogram, data: debug_hist.csv
  !! Some common tags are:
  !!    STAGE   STAGE: 2017-01-31 16:03:15 INFO: Generation 7 took 448.3279s.
  !!    INFO    INFO: some information
  !!    TIMER   TIMER: Calculating distances took 0.001 s
  !! Tag meaning: @n
  !!  + **MAJOR**     major stages of the simulation, e.g. next generation;
  !!  + **STAGE**     minor stages of the simulation, e.g. time step change;
  !!  + **INFO**      some information about model running;
  !!  + **WARNING**   warnings on possible issues and minor errors;
  !!  + **ERROR**     error reports that do not normally halt running;
  !!  + **CRITICAL**  critical errors that would stop execution;
  !!  + **TIMER**     reports from the timers and stopwatches.
  character(len=*), parameter, public :: LTAG_MAJOR = "IMPORTANT: "
  character(len=*), parameter, public :: LTAG_STAGE = "STAGE: "
  character(len=*), parameter, public :: LTAG_INFO  = "INFO: "
  character(len=*), parameter, public :: LTAG_WARN  = "WARNING: "
  character(len=*), parameter, public :: LTAG_ERROR = "ERROR: "
  character(len=*), parameter, public :: LTAG_CRIT  = "CRITICAL: "
  character(len=*), parameter, public :: LTAG_TIMER = "TIMER: "
  character(len=*), parameter, public :: LTAG_STATS = "STATS: "

  !> @}

  !> @name System-wide fatal errors
  !!       The description of errors that pertain to the whole system.
  !! @{

  !> Error message for **"no automatic intrinsic array allocation"**.
  !! Fortran compilers support automatic allocation of arrays on
  !! intrinsic assignment. This feature should work by default in
  !! GNU gfortran v.4.6 and Intel ifort v.17.0.1. Automatic allocation
  !! allows to avoid a possible bug when the number of array elements
  !! in the `allocate` statement is not updated when the components of the
  !! array are updated in the array constructor.
  character(len=*), parameter, public :: ERROR_NO_AUTOALLOC =                 &
                                        "No automatic array allocation"

  !> Error message for **"no automatic determination of the size in parameter"**
  !! arrays in the style:
  !! @code
  !! real(SRP), parameter, dimension(*) :: ARRAY=[ 1.0, 2.0, 3.0, 4.0 ]
  !! @endcode
  character(len=*), parameter, public :: ERROR_AUTO_PARAM_ARRAYS =            &
                                        "No automatic size in parameter arrays"

  !> Error message **"Cannot allocate array or object"** is issued if
  !! an array or an object is checked and turns out to be not allocated while
  !! it must be.
  character(len=*), parameter, public :: ERROR_ALLOCATION_FAIL =              &
                                              "Cannot allocate array or object"

  character(len=*), parameter, public :: ERROR_LOCK_PREEXISTS =               &
      "Lock file '" // LOCK_FILE // "' exists. Is another simulation running?"

  !> @}

  !> @name Stopwatch Timer class
  !!       Used for execution timing of certain parts of the code.
  !! @{

  !> CPU timer container object for debugging and speed/performance control.
  !! Arbitrary timers can be instantiated for different parts of the code and
  !! also global. Using a specific timer (`stopwatch`) is like this:
  !! @code
  !!    call stopwatch%start("Output all agents data")
  !! @endcode
  !! to **start** the stopwatch with specific title, then the function
  !! @code
  !!    ... = stopwatch%elapsed()
  !! @endcode
  !! returns the **elapsed time**. Then, the function stopwatch\%title()
  !! outputs the title of this timer. A few other functions build on this
  !! simple functionality to provide typical shortcuts: stopwatch\%show()
  !! and stopwatch\%log().
  !! @note The near-trivial nature of this object makes it ideal for learning
  !!       how to implement objects in modern Fortran.
  type, public :: TIMER_CPU
    !> All object data components private, we should never use them directly.
    private
    !> Define start time for the stopwatch.
    !  @note    We need to keep only the start time as raw values coming out
    !           of `cpu_time` are machine-dependent
    !  @note    It does not seem good to move `TIMER_CPU` to *HEDTOOLS* as they
    !           are for portability (require only F90) and do not use OO.
    !           `TIMER_CPU`  uses full OO extensible class implementation so
    !           requires *F2003* minimum.
    real(SRP) :: cpu_time_start
    !> Optional title for the stopwatch. Useful if we use many timers and for
    !! logger.
    !  @warning Not allocatable as deferred character length strings within
    !           derived types may not be supported by all recent widespread
    !           compilers, e.g. GNU gfortran 4.8.4 does not support it.
    character (len=LONG_LABEL_LENGTH) :: cpu_time_title
    contains
      !> Start the timer object, stopwatch is now ON.
      !! See `commondata::timer_cpu_start()`
      procedure, public :: start => timer_cpu_start
      !> Calculate the time elapsed since the stopwatch subroutine was called
      !! for this instance of the timer container object. Can be called several
      !! times showing elapsed time since the grand start.
      !! See `commondata::timer_cpu_elapsed()`
      procedure, public :: elapsed => timer_cpu_elapsed
      !> Return the title of the current timer object.
      !! See `commondata::timer_cpu_title()`
      procedure, public :: title => timer_cpu_title
      !> A ready to use in output function that returns a formatted
      !! string for a timer combining its title and the elapsed time.
      !! See `commondata::timer_cpu_show()`
      procedure, public :: show => timer_cpu_show
      !> A ready to use shortcut to be used in logger, just adds the TIMER: tag
      !! in front of the normal `show`output.
      !! See `commondata::timer_cpu_log()`
      procedure, public :: log => timer_cpu_log
  end type TIMER_CPU
  ! @note Note that all component procedures are private.
  private ::  timer_cpu_start, timer_cpu_elapsed, timer_cpu_title,            &
              timer_cpu_show, timer_cpu_log

  !> @}

  !.............................................................................
  !.............................................................................

  !> @name General Parameters
  !! @{

  ! General Parameters
  ! ==================

  !> Model name for tags, file names etc. Must be very short.
  !! See @ref intro_descriptors "Model descriptors".
  character (len=*), parameter, public :: MODEL_NAME = "HEDG2_04"

  !> Model description - a fixed descriptive text, used in text outputs etc.
  !! See @ref intro_descriptors "Model descriptors".
  character (len=*), parameter, public :: MODEL_DESCR =                       &
    "AHA, single fear, body size non-genetic."

  !> The name of the file that contains the Model abstract, a short description
  !! that can span several lines of text ans is kept in a separate file. The
  !! file is read, if it exists, and its contents is logged at the start the
  !! simulation.
  !! The separate Model Abstract file is useful because it can integrate
  !! dynamic information, such as the latest version control log(s) via
  !! Subversion or Mercurial hooks mechanism.
  !! See @ref intro_descriptors "Model descriptors".
  character (len=*), parameter, private :: MODEL_ABSTRACT_FILE = "abstract.txt"

  !> Sets the model in the @ref intro_debug_mode "debug mode" if TRUE. The
  !! Debug  mode generates huge additional outputs and logs. Also, the logs
  !! by default go to the screen (standard output).
  !! See `commondata::system_init()` for details.
  !! @warning This is a protected variable not fixed parameter. Can be set at
  !!          start from the command line parameter or environment variable.
  !! @note Debug mode can be set in three ways (see `system_init` subroutine):
  !!       1. by setting this variable at the initialisation here to TRUE
  !!          (although this is **not-normal** as requires recompile);
  !!       2. by setting the shell environment variable `AHA_DEBUG=1`,
  !!          `AHA_DEBUG=TRUE` or `AHA_DEBUG=YES`;
  !!       3. by setting the command line parameter `DEBUG` when calling this
  !!          executable program.
  !! @warning `IS_DEBUG` can also be declared as a `parameter`, see
  !!           @ref system_debug_optimise "Compiler optimisation of debug mode"
  !!           in commondata::system_init() for details.
  !! @note    `IS_DEBUG` can also be declared as a normal global variable, not
  !!           "protected" (by removing the `protected` attribute) In such case
  !!           it could be changed everywhere in the program. A potential
  !!           benefit is that only a section of the program, e.g. a single
  !!           function, can then produce extended debugging output. However,
  !!           this would also make compiler optimisation more difficult and
  !!           reduce performance. See commondata::system_init() for more
  !!           discussion.
  !! @remark   A short discussion on the protected attribute for module variable
  !!           is at [comp.lang.fortran](https://groups.google.com/forum/#!topic/comp.lang.fortran/K-JkeVFXZMo).
  logical, protected, public :: IS_DEBUG=.FALSE.
  ! DEBUG_COMPILER_OPTIMISE: see 'Compiler optimisation of debug mode'
  ! logical, parameter, public :: IS_DEBUG=.FALSE.

  !> This parameter controls if the debug plots are produced. They can be
  !! huge number that takes lots of space. Also, debug plots are called as
  !! separate processes that can run at the background and easily exceed the
  !! system-specific limit on child processes (if run in asynchronous mode).
  !! Generation of debug plots can be controlled by the environment variable
  !! `AHA_DEBUG_PLOTS`: if it is set to TRUE, 1, or YES, debug plots are
  !! enabled. See `commondata::system_init()` for details.
  logical, protected, public :: IS_PLOTTING=.TRUE.

  !> Sets the model in screen output mode. If TRUE, the logger output goes
  !! to the screen (standard output device). Can be manipulated  using the
  !! environment variable `AHA_SCREEN`. If `AHA_SCREEN` is set to TRUE or 1 or
  !! yes, logger screen output is enabled. See `commondata::system_init()` for
  !! details.
  logical, protected, public :: IS_SCREEN_OUTPUT=.FALSE.

  !> This parameter enables or disables post-processing compression of the
  !! data: if TRUE, the data are compressed using the command defined by the
  !! commondata::cmd_zip_output string parameter. Note that not all data
  !! files are compressed, only potentially big ones are (e.g. agent population
  !! data and habitat data).
  !! @note    Note that each data file is compressed individually. That is,
  !!          no 'archive' containing multiple files is made.
  !! @warning This parameter is by default set to FALSE because calling wrong
  !!          compression program might hang this program (e.g. if the child
  !!          process is waiting for data on standard input). Enabling
  !!          background data compression is normally done by setting the
  !!          environment variable `AHA_ZIP_FILES=TRUE`.
  !!          See commondata::system_init() for details.
  logical, protected, public :: IS_ZIP_OUTPUTS=.FALSE.

  !> This parameter defines if the output files are compressed in the
  !! background in the parallel mode or the program should wait for
  !! termination of the child zipping process.
  logical, parameter, public :: ZIP_OUTPUTS_BACKGROUND=.TRUE.

  !> This parameter defines the compression program that is executed to "zip"
  !! the data files if commondata::is_zip_outputs is enabled (TRUE).
  !! The normal compression utility is "gzip," that is found on almost any
  !! Linux/Unix system. gzip compresses each file individually and by default
  !! automatically deletes the original file. The compressed file extension
  !! is defined by commondata::zip_file_extenssion. See http://www.gzip.org/.
  !! Alternative compressors that are fairly widespread are `bzip2`, `lzma`
  !! and `xz`.
  character(len=*), parameter, public :: CMD_ZIP_OUTPUT = "gzip"

  !> This parameter defines the compressed file extension for the external
  !! compression utility defined by the commondata::cmd_zip_output.
  character(len=*), parameter, public :: ZIP_FILE_EXTENSSION = ".gz"

  !> `MMDD` tag, year, month and day, used in file names and outputs.
  !! The value of the tag should be obtained only once at the start of
  !! the simulation, normally by  calling the commondata::tag_mmdd()
  !! function at commondata::system_init(). It does not make much sense
  !! to generate these data tags on the fly as the simulations can be very
  !! long, several days, and so the file tags will be inconsistent.
  !! @note  MMDD normally has a fixed length 8 as in `20161228`, see
  !!        commondata::tag_mmdd(). Because it has allocatable attribute,
  !!        its actual length is obtained automatically and no `trim(MMDD)`
  !!        is necessary.
  character(len=:), allocatable, protected, public :: MMDD

  !> Maximum population size
  integer, parameter, public :: POPSIZE = 10000

  !> Maximum number of  generations in GA
  integer, parameter, public :: GENERATIONS = 100

  !> The current global **generation number**. This is a global non
  !! fixed-parameter variable that is updated in subroutines.
  !! @warning There might be no guarantee that it is saved in all subroutines
  integer, public :: Global_Generation_Number_Current

  !> Number of time steps in the agent's maximum life length
  integer, parameter, public :: LIFESPAN = 14000

  !> Number of time steps in the agent's life at the pre-evolution stage.
  integer, parameter, public :: PREEVOL_TSTEPS = 560

  !> Number of time steps in the agent's life at the fixed fitness
  !! pre-evolution stage. This parameter **forces** a smaller fixed value
  !! that is used for debugging only. Thus, adaptive time steps calculated by
  !! the_evolution::preevol_steps_adaptive() are disabled.
  !! To enable this fixed time steps, set this parameter
  !! commondata::preevol_tsteps_force_debug_enabled to TRUE.
  !! @warning This is used for debugging and testing purposes only and should
  !!          normally be disabled.
  integer, parameter, public :: PREEVOL_TSTEPS_FORCE_DEBUG = 280

  !> This parameter enables the forced smaller fixed number of time steps
  !! set by the commondata::preevol_tsteps_force_debug parameter.
  !! @warning This is used for debugging and testing purposes only and should
  !!          normally be disabled, set to **FALSE**.
  logical, parameter, public :: PREEVOL_TSTEPS_FORCE_DEBUG_ENABLED = .FALSE.

  !> This parameter completely disables predation in the GA life cycle
  !! procedure.
  !! @warning Can be enabled for debugging purposes only. Normally should be
  !!          set to **FALSE**.
  logical, parameter, public :: LIFECYCLE_PREDATION_DISABLED_DEBUG = .FALSE.

  !> The current global **time step** of the model. This is a global non
  !! fixed-parameter variable that is updated in subroutines.
  integer, public :: Global_Time_Step_Model_Current

  !> The current global time frame. Frames are time steps within the time step
  !! defined by the commondata::global_time_step_model_current
  integer, public :: Global_Frame_Number

  !> Default perception error in the commondata::gamma2gene() neuronal
  !! response functions. Note that this parameter defines stochastic error as
  !! the Coefficient of Variation (CV).
  real(SRP), parameter, public :: PERCEPT_ERROR_CV_DEF = 0.01_SRP

  !> @}

  !.............................................................................
  !.............................................................................

  !> @name Basic agent parameters
  !! @{

  ! Basic agent parameters
  ! ======================

  !> Minimum body length possible
  real(SRP), parameter, public :: BODY_LENGTH_MIN = 0.2_SRP

  !> Maximum body length
  real(SRP), parameter, public :: BODY_LENGTH_MAX = 100.0_SRP

  !> Minimum possible body mass, hard limit.
  !! @note Note that body mass is calculated from condition factor and length.
  !!       = k * l**3
  real(SRP), parameter, public :: BODY_MASS_MIN = 0.1_SRP

  !> This parameter determines if the agents are initialised at a fixed
  !! depth at the initialisation. Agents are normally placed uniformly,
  !! the_environment::uniform(), at the initialisation. However, the depth
  !! can be fixed. In such a case they are scattered uniformly in the X and Y
  !! coordinates but with fixed depth that is set by the
  !! commondata::init_agents_depth parameter.
  !> @warning The agents can also be initialised with Gaussian depth. This is
  !!         controlled by the parameter commondata::init_agents_depth_is_gauss.
  !!         However, this parameter has precedence over Gaussian, so remember
  !!         to set it FALSE if Gaussian depth initialisation is required.
  !! See the_population::member_population::place_uniform().
  logical, parameter, public :: INIT_AGENTS_DEPTH_IS_FIXED = .FALSE.

  !> This parameter determines if the agents are initialised at a fixed
  !! depth at the initialisation. Agents are placed uniformly,
  !! the_environment::uniform(), at the initialisation. However, the depth
  !! can be a Gaussian value with the
  !! - mean set by the commondata::init_agents_depth
  !! - CV set by the commondata::init_agents_depth_cv
  !! .
  !! Such a Gaussian depth patter is switched by this parameter. The other
  !! coordinates of the agents, X and Y are then uniform.
  !! @warning If Gaussian depth initialisation is required, the parameter
  !!          commondata::init_agents_depth_is_fixed must be set to FALSE as
  !!          it has a higher precedence.
  !! See the_population::member_population::place_uniform().
  logical, parameter, public :: INIT_AGENTS_DEPTH_IS_GAUSS = .TRUE.

  !> The fixed depth at which the agents are initialised at the start of the
  !! simulation. The other coordinates are normally set
  !! the_environment::uniform() within the initialisation environment
  !! container. See the_population::member_population::place_uniform().
  !! @note If the parameter commondata::init_agents_depth_is_fixed is FALSE,
  !!       then the agents are scattered uniformly in the whole 3D space
  !!       and this parameter is ignored.
  real(SRP), parameter, public :: INIT_AGENTS_DEPTH = 1833.0_SRP

  !> This parameter sets the Coefficient of Variation for the Gaussian depth
  !! initialisation of the agents that is controlled by
  !! commondata::init_agents_depth_is_gauss.
  !! See the_population::member_population::place_uniform().
  real(SRP), parameter, public :: INIT_AGENTS_DEPTH_CV = 0.2_SRP

  !> The energetic cost of reproduction in terms of the agent's body mass
  !! loss.
  !! @warning This parameter applies to the fixed cost version of the
  !!          procedure `the_body::reproduction_cost_energy_fix()`.
  real(SRP), parameter, public :: REPRODUCTION_COST_BODY_MASS_FIX = 0.2_SRP

  !> The component of the energetic cost of reproduction in **males** that is
  !! proportional to the total offspring mass. For details see the procedure
  !! `the_body::reproduction_cost_energy_dynamic()`.
  real(SRP), parameter, public ::                                             &
                          REPRODUCTION_COST_OFFSPRING_FRACT_MALE = 0.3_SRP

  !> The component of the energetic cost of reproduction in **females** that is
  !! proportional to the total offspring mass. For details see the procedure
  !! `the_body::reproduction_cost_energy_dynamic()`.
  real(SRP), parameter, public ::                                             &
                          REPRODUCTION_COST_OFFSPRING_FRACT_FEMALE = 1.0_SRP

  !> The component of the energetic cost of reproduction in **males** that is
  !! proportional to the agent's body mass. For details see the procedure
  !! `the_body::reproduction_cost_energy_dynamic()`.
  real(SRP), parameter, public ::                                             &
                          REPRODUCTION_COST_BODY_MASS_FACTOR_MALE = 0.4_SRP

  !> The component of the energetic cost of reproduction in **females** that is
  !! proportional to the agent's body mass. For details see the procedure
  !! `the_body::reproduction_cost_energy_dynamic()`.
  real(SRP), parameter, public ::                                             &
                          REPRODUCTION_COST_BODY_MASS_FACTOR_FEMALE = 0.1

  !> The energetic cost of unsuccessful reproduction in terms of the agent's
  !! body mass lost. This is a fraction of the **full cost of reproduction**,
  !! that is described by the `REPRODUCTION_COST_BODY_MASS` parameter.
  real(SRP), parameter, public :: REPRODUCTION_COST_UNSUCCESS = 0.1_SRP

  !> The array defining the **abscissa** (X) of the nonparametric function
  !! curve that defines the relationship between the agent's body mass and
  !! the overall mass of all offspring as a fraction of the agent's body mass.
  !! @warning Must have the same dimensionality as
  !!         commondata::reproduct_body_mass_offspr_ordinate.
  real(SRP), dimension(*),                                                    &
                parameter, public :: REPRODUCT_BODY_MASS_OFFSPR_ABSCISSA =    &
                [ BODY_MASS_MIN, 3.0_SRP, 10.5_SRP, 12.0_SRP ]

  !> The array defining the **ordinate** (Y) of the nonparametric function
  !! curve that defines the relationship between the agent's body mass and
  !! the overall mass of all offspring as a fraction of the agent's body mass.
  !! Plotting command for the interpolator:
  !! @verbatim
  !!   htintrpl.exe [0.1 100 350 400] [0.0 0.1 0.199 0.20] [0] [nonlinear]
  !!   htintrpl.exe [0.1 3 10.5 12.0] [0.0 0.1 0.199 0.20] [0] [nonlinear]
  !! @endverbatim
  !! @warning Must have the same dimensionality as
  !!         commondata::reproduct_body_mass_offspr_abscissa.
  real(SRP), dimension(*),                                                    &
                parameter, public :: REPRODUCT_BODY_MASS_OFFSPR_ORDINATE =    &
                [ 0.0_SRP, 0.1_SRP, 0.199_SRP, 0.20_SRP ]

  !> @}

  !.............................................................................
  !.............................................................................

  !> @name Parameters of the environment
  !! @{

  ! Parameters of the environment
  ! =============================

  !.............................................................................
  ! ## General environmental parameters ##

  !> @brief   Overall size of the global 3D universe of the model.
  !! @details Physical sizes of the 3D "universe" environment for the agents'
  !!          life. The *minimum* coordinates (`UNIVERSE_MIN_COORD`) are all
  !!          zeroes for simplicity. So here set the *maximum* coordinates
  !!          vector [x,y,z] limiting the maximum environment size:
  !!          `UNIVERSE_WHOLE_SIZE`.
  !! @note    Note that the depth is limited by 30, so the environment is like
  !!          a shallow lake: it is limited and depth is relatively small.
  !! @warning The dimensionality is only **3** for three-dimensional space.
  !! @note    Universe has been deprecated in @ref the_evolution module as
  !!          it has not been used. However, the parameters are still defined
  !!          as the arrays could be used in definitions of habitats, if found
  !!          feasible.
  real(SRP), parameter, dimension(3), public :: UNIVERSE_MIN_COORD =          &
                                        [0.0_SRP, 0.0_SRP, 0.0_SRP]
  real(SRP), parameter, dimension(3), public :: UNIVERSE_WHOLE_SIZE =         &
                                        [20000.0_SRP, 10000.0_SRP, 3000.0_SRP]

  !> Number of days and nights in a lifespan, `DIELCYCLES=500`.
  integer, parameter, public :: DIELCYCLES = 100

  !> The size of the history for spatial moving objects, i.e. how many time
  !! steps positions to remember in stack arrays.
  integer, parameter, public :: HISTORY_SIZE_SPATIAL = 50

  !.............................................................................
  !.............................................................................
  ! ## Parameters of the HABITATs ##

  !> @brief   Definition of the habitat spatial limits.
  !! @details We define two habitats **within the global universe**
  !!          (`UNIVERSE_WHOLE_SIZE`) of the model. They are called
  !!          "The safe" and "The dangerous" habitats and primarily
  !!          differ in the level of predator risk. The habitats
  !!          represent two adjacent squares that form a rectangular
  !!          "universe" (the universe defined by
  !!          `UNIVERSE_WHOLE_SIZE`).
  !! @code
  !!   0           10000000            20000000
  !!   +------------------+-------------------+
  !!   |                  |                   |
  !!   |                  |                   |
  !!   |  Safe habitat    |  Dangerous        |
  !! Y |                  |         habitat   |
  !!   |                  |                   |
  !!   |                  |                   |
  !!   |                  |                   |
  !!   |                  |                   |
  !!   |                  |                   |
  !! 0 +------------------+-------------------+ 20000000
  !!                      X
  !! @endcode
  !!  Safe habitat: 0:1000000 x 0:1000000 x 0:3000 cm (NB: units cm!)
  !! @warning The dimensionality is only **3** for three-dimensional space.
  real(SRP), parameter, dimension(3), public ::                               &
              HABITAT_SAFE_MIN_COORD =   [0.0_SRP, 0.0_SRP, 0.0_SRP]
  real(SRP), parameter, dimension(3), public ::                               &
              HABITAT_SAFE_MAX_COORD =   [10000.0_SRP, 10000.0_SRP, 3000.0_SRP]
  !!  Dangerous habitat: 10000:20000 x 0:10000 x 0:30
  !! @warning The dimensionality is only **3** for three-dimensional space.
  real(SRP), parameter, dimension(3), public ::                               &
              HABITAT_DANGER_MIN_COORD = [10000.0_SRP, 0.0_SRP, 0.0_SRP]
  real(SRP), parameter, dimension(3), public ::                               &
              HABITAT_DANGER_MAX_COORD = [20000.0_SRP, 10000.0_SRP, 3000.0_SRP]

  ! Setting other parameters of the two habitats.

  !> The **number of predators** in the **safe** habitat.
  integer, parameter, public :: PREDATORS_NUM_HABITAT_SAFE  = 100

  !> The **number of predators** in the **dangerous** habitat.
  integer, parameter, public :: PREDATORS_NUM_HABITAT_DANGER  = 500

  !> The **food abundance** in the **safe** habitat.
  integer, parameter, public :: FOOD_ABUNDANCE_HABITAT_SAFE   = 20000

  !> The **food abundance** in the **dangerous** habitat.
  integer, parameter, public :: FOOD_ABUNDANCE_HABITAT_DANGER = 40000

  !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! ### Default parameters of all the HABITAT objects. ###

  !> Default level of other mortality risks in the habitat
  real(SRP), parameter, public :: OTHER_RISKS_DEF = 0.01_SRP

  !> Habitat-specific mortality risk (not linked with predation) in the
  !! **safe** habitat.
  real(SRP), parameter, public :: OTHER_RISKS_HABITAT_SAFE = 0.01_SRP

  !> Habitat-specific mortality risk (not linked with predation) in the
  !! **dangerous** habitat.
  real(SRP), parameter, public :: OTHER_RISKS_HABITAT_DANGER = 0.05_SRP

  !> Default level of egg mortality in the habitat
  real(SRP), parameter, public :: EGGMORTALITY_DEF = 0.01_SRP

  !> Default individually-specific mortality risk. It can increase or decrease
  !! depending on various factors. The individually-specific mortality risk
  !! is normally a Gaussian variable with the variability set by the
  !! commondata::individual_mortality_risk_cv.
  real(SRP), parameter, public :: INDIVIDUAL_MORTALITY_RISK_DEF = 0.01_SRP

  !> The coefficient of variation for Gaussian stochastic individually-specific
  !! mortality risk of the agent.
  real(SRP), parameter, public :: INDIVIDUAL_MORTALITY_RISK_CV = 0.05_SRP

  !.............................................................................
  ! Parameters of the predator attack efficiency. Predators is an array
  ! (population of predators) that are initialised with Gaussian random
  ! parameters.

  !> The body size of the predator. In this version all predators have the
  !! same body size set by this parameter, but can be Gaussian stochastic.
  !! Moreover, in such a case predator attack efficiency can depend on
  !! the body size, e.g. larger predators are more dangerous.
  !! compare to the agents maximum body size `BODY_LENGTH_MAX=100.0`
  real(SRP), parameter, public :: PREDATOR_BODY_SIZE = 100.0_SRP

  !> Mean rate of a single predator attack
  real(SRP), parameter, public :: PREDATOR_ATTACK_RATE_DEFAULT = 0.9_SRP

  !> Coefficient of variation for a single predator attack among the
  !! whole population of stochastic predators.
  real(SRP), parameter, public :: PREDATOR_ATTACK_RATE_CV = 0.1_SRP

  !> The probability of capture of a fish agent by a predator at the distance
  !! equal to 1/2 of the visual range. For more details see
  !! `the_environment::predator_capture_risk_calculate_fish()`.
  real(SRP), parameter, public ::                                             &
                            PREDATOR_ATTACK_CAPTURE_PROBABILITY_HALF = 0.8_SRP

  !> Minimum probability of capture, e.g. at a distance exceeding the visual
  !! range. The latter assumes that the predator could detect the agent beyond
  !! the visual range and pursue it. For more details see
  !! `the_environment::predator_capture_risk_calculate_fish()`.
  real(SRP), parameter, public ::                                             &
                            PREDATOR_ATTACK_CAPTURE_PROBABILITY_MIN = 0.1_SRP

  !> A parameter factor defining the probability of capture of an immobile
  !! (freezing) agent by a predator: interpolation ordinate for the distance
  !! equal to **0.25 of the visual range**.
  !! See `the_environment::predator_capture_risk_calculate_fish()` for details.
  real(SRP), parameter, public :: PREDATOR_ATTACK_CAPTURE_PROB_FRZ_50 = 0.10_SRP
  !> A parameter factor defining the probability of capture of an immobile
  !! (freezing) agent by a predator: interpolation ordinate for the distance
  !! equal to **0.40 of the visual range**.
  !! See `the_environment::predator_capture_risk_calculate_fish()` for details.
  real(SRP), parameter, public :: PREDATOR_ATTACK_CAPTURE_PROB_FRZ_75 = 0.01_SRP

  !> A logical flag of whether the agents can assess the individual inherent
  !! attack rates of the predators. If yes, these inherent individual attack
  !! rates are collated into the perception object. If no, the default
  !! attack rate set by the commondata::predator_attack_rate_default parameter
  !! is used.
  logical, parameter, public :: AGENT_CAN_ASSESS_PREDATOR_ATTACK_RATE = .TRUE.

  !> Sets the limit for partial indexing and ranking of **prey agents**
  !! in the visual range of the predator. The risk of predation, i.e. the
  !! probability of attack and capture of each agent in a group of agents,
  !! will be calculated individually for distance-ranked agents only up to
  !! this parameter value.
  integer, parameter, public :: PREDATOR_RISK_GROUP_SELECT_INDEX_PARTIAL = 20

  !> The array defining the ordinate grid values for the weighting
  !! nonparametric function linking the  distance rank of the agent within
  !!  the visual field of the predator and the weighting factor adjusting
  !! for predator confusion and predator dilution effects. The grid abscissa
  !! is calculated dynamically in the
  !! the_environment::predator_capture_risk_calculate_fish_group() procedure.
  !! @note Note that the middle value equal to 0.5 results in a linear
  !!       function.
  !!
  !! This command produces the function plot:
  !! @verbatim
  !!   htintrpl.exe [1 30 60] [1 0.3 0.0] [1]
  !! @endverbatim
  real(SRP), dimension(*), parameter, public                                  &
    :: PREDATOR_RISK_GROUP_DILUTION_ORDINATE = [1.0_SRP, 0.3_SRP, 0.1_SRP]

  !.............................................................................
  ! ## Parameters of the food resources ##

  !> Default size of a single food item.
  !! @note Note that the maximum stomach capacity of the agent
  !!       (the_condition::condition::maxstomcap) is 0.15 of the
  !!       agent body mass (@f$ M_a @f$). So if the average agent mass
  !!       is 41.0, the maximum capacity is about 6.15.
  !!       Thus, to get the food item size corresponding to such a mass
  !!       (food item mass 6.15), the *size* of the food item should be:
  !!       @f[ s=\sqrt[3]{\frac{3\cdot 0.15 \cdot M_a}{4\pi\rho}} \Rightarrow
  !!           s=\sqrt[3]{\frac{0.45M_a}{4\pi\rho}} . @f]
  !!       Calculate: `( 0.45 * 41 / (4 * 3.1415926 * 0.1 ))^(1/3) = 2.45`
  !!       In reality, it makes sense to make the mean food item size smaller
  !!       than exactly 0.15 of the maxstomcap to accomodate its Gaussian
  !!       variability, so more than 50% of items would fit into the stomach.
  !!       An average food item mass of 0.09 of the agent mass might be better.
  !!       Calculate: `( 3 * 0,09 * 41 / (4 * 3.1415926 * 0.1 ))^(1/3) = 2.065`
  real(SRP), parameter, public :: FOOD_ITEM_SIZE_DEFAULT = 2.1_SRP

  !> The above is also the average size of a stochastic Gaussian
  !! food items.
  real(SRP), parameter, public :: FOOD_ITEM_MEAN_SIZE = FOOD_ITEM_SIZE_DEFAULT

  !> Coefficient of variation for Gaussian food items.
  real(SRP), parameter, public :: FOOD_ITEM_SIZE_DEFAULT_CV = 0.1_SRP

  !> The minimum size of a food item. This is the "floor" in case the
  !! stochastically generated (e.g. Gaussian) value gets zero or below.
  real(SRP), parameter, public :: FOOD_ITEM_MINIMUM_SIZE = 1.0_SRP

  !> The (physical) density of a single food item. TODO: need to parametrise!
  real(SRP), parameter, public :: FOOD_ITEM_DENSITY = 0.1_SRP

  !> The cost of the food item catching, in terms of the **food item mass**
  !! (proportional cost). So, if the agent does an unsuccessful attempt to
  !! catch a food item, the cost still applies.
  real(SRP), parameter, public :: FOOD_ITEM_CAPTURE_PROP_COST = 0.05_SRP

  !> The **baseline** probability that the food item is captured.
  !! See the_neurobio::food_item_capture_probability_calc().
  !! @note Interpolation plot command:
  !!       `htintrpl.exe  [0.0 0.5 1.0] [0.85, 0.68, 0.1]` (0.68=0.85*0.8).
  real(SRP), parameter, public :: FOOD_ITEM_CAPTURE_PROBABILITY = 0.99_SRP

  !> The **minimum** probability of capture a food item, when the item is
  !! at a distance equal to the visual range from the predator agent.
  real(SRP), parameter, public :: FOOD_ITEM_CAPTURE_PROBABILITY_MIN = 0.1_SRP

  !> Subjective error assessing the food item capture probability when
  !! assessing the subjective GOS expectancies of food items. The subjective
  !! assessment value of the capture probability is equal to the objective
  !! value plus random error with the CV equal to this parameter.
  real(SRP), parameter, public ::                                             &
                      FOOD_ITEM_CAPTURE_PROBABILITY_SUBJECTIVE_ERRORR_CV = 0.1

  !.............................................................................
  ! ## Parameters of stochastic movement of food items ##
  ! Food items do regular deterministic vertical migrations. Additionally,
  ! each of the food item is are also displaced randomly in a Gaussian pattern.

  !> Mean shift parameter for the local random walk movement of food items in
  !! the horizontal plane.
  real(SRP), parameter, public :: FOOD_ITEM_MIGRATE_XY_MEAN =                 &
                                            FOOD_ITEM_SIZE_DEFAULT * 10.0_SRP

  !> Mean shift parameter for the local random walk movement of food items in
  !! the vertical (depth) plane.
  real(SRP), parameter, public :: FOOD_ITEM_MIGRATE_DEPTH_MEAN =              &
                                            FOOD_ITEM_SIZE_DEFAULT * 100.0_SRP

  !> Coefficient of variation parameter for the local random walk movement of
  !! food items in the horizontal plane.
  real(SRP), parameter, public :: FOOD_ITEM_MIGRATE_XY_CV =                   &
                                                    FOOD_ITEM_SIZE_DEFAULT_CV

  !> Coefficient of variation parameter for the local random walk movement of
  !! food items in the vertical (depth) plane.
  real(SRP), parameter, public :: FOOD_ITEM_MIGRATE_DEPTH_CV =                &
                                                    0.8_SRP

  !.............................................................................
  ! ## Visual perception parameters ##
  ! Visual perception and range parameters, taken from the old code. They
  ! are normally left unchanged. These are used for the **food items / prey**,
  ! and may be set separately for the **agent** itself.

  !> Maximum above-surface light intensity at midday, DAYLIGHT=500.0.
  !! @note Can be deterministic or stochastic.
  real(SRP), parameter, public :: DAYLIGHT=500.0_SRP

  !> Flag for stochastic daylight pattern (if TRUE) or deterministic sinusoidal
  !! (when FALSE). Check out the next parameter `DAYLIGHT_CV` for variability.
  logical, parameter, public :: DAYLIGHT_STOCHASTIC = .TRUE.

  !> Coefficient of variation for stochastic DAYLIGHT,
  !! @note if = 0.0 then deterministic sinusoidal daylight pattern is used.
  real(SRP), parameter, public :: DAYLIGHT_CV=0.2_SRP

  !> Beam attenuation coefficient of water (m-1),BEAMATT = 1.0.
  !! @note See `the_environment::visual_range()` and
  !!       `the_environment::srgetr()` for more details.
  real(SRP), parameter, public :: BEAMATT=1.0_SRP

  !> Inherent contrast of prey, CONTRAST =1.0.
  !! @note See `the_environment::visual_range()` and
  !!       `the_environment::srgetr()` for more details.
  real(SRP), parameter, public :: PREYCONTRAST_DEFAULT = 1.0_SRP

  !> Area of prey (m2), PREYAREA = 3.E-6.
  !! @note See `the_environment::visual_range()` and
  !!       `the_environment::srgetr()` for more details.
  real(SRP), parameter, public :: PREYAREA_DEFAULT=3.E-6_SRP

  !> Dimensionless descriptor of fish eye quality, VISCAP=1.6E6.
  !! @note See `the_environment::visual_range()` and
  !!       `the_environment::srgetr()` for more details.
  real(SRP), parameter, public :: VISCAP=1.6E6_SRP

  !> Saturation parameter of eye (Ke) (uE m-2 s-1), EYESAT=500.0.
  !! @note See `the_environment::visual_range()` and
  !!       `the_environment::srgetr()` for more details.
  real(SRP), parameter, public :: EYESAT=500.0_SRP

  !> Vertical conservation of light, per depth (old code lightdecay=0.2).
  !! @note set = 0 if light constant with depth
  !! @note Because body size of the agent is set in cm, and the environment
  !!       size is also in cm, we need to scale depth appropriately, in the old
  !!       code it was within the range 0:30 m or unitless?, now the range
  !!       is 0:3000 cm. So the appropriate scaling factor is 0.002.
  !! @note wxMaxima quick code for plotting (assuming surface light 500.0):
  !! @code
  !!       wxplot2d( 500.0*exp(-0.002 * D), [D, 0., 3000.] );
  !! @endcode
  real(SRP), parameter, public :: LIGHTDECAY = 0.002_SRP

  !> @}

  !.............................................................................
  !.............................................................................

  !> @name Genetic architecture parameters
  !! @{

  ! Genetic architecture parameters
  ! ===============================

  ! ## Alleles ##

  ! The following two parameters define the range of possible allele values
  !> The minimum possible value of alleles (allele range minimum)
  !! See implementation notes on `the_genome::gene::allele_value` component
  !! of the `the_genome::gene` derived type and `commondata::alleleconv()` and
  !! `commondata::allelescale()` functions.
  !! @warning The minimum value should **not** be zero, as it will result in
  !!          *division by zero* condition in `gamma2gene`, `(P/y)**x`.
  integer, parameter, public:: ALLELERANGE_MIN = 1

  !> The maximum possible value of alleles (allele range maximum)
  !! See implementation notes on `the_genome::gene::allele_value` component
  !! of the `the_genome::gene` derived type and `commondata::alleleconv()` and
  !! `commondata::allelescale()` functions.
  integer, parameter, public :: ALLELERANGE_MAX = 10000

  !> Number of additive allele components
  integer, parameter, public :: ADDITIVE_COMPS = 3

  ! ## Genes ##

  !> Mutation rate for point allele mutations
  real(SRP), parameter, public :: MUTATIONRATE_POINT = 0.1_SRP

  !> Maximum point mutation rate in the adaptive Fixed Fitness Genetic
  !! Algorithm
  real(SRP), parameter, public :: GA_MUTATIONRATE_POINT_MAX = 0.25_SRP

  !> Mutation rate for point allele mutations, a whole batch of allele
  !! components.
  real(SRP), parameter, public :: MUTATIONRATE_BATCH = 0.05_SRP

  !> Maximum batch mutation rate in the adaptive Fixed Fitness Genetic
  !! Algorithm
  real(SRP), parameter, public :: GA_MUTATIONRATE_BATCH_MAX = 0.1_SRP

  !> Mutation rate for chromosome relocation, i.e. probability of a gene
  !! moving to a different position on the same chromosome: There are two
  !! kinds of relocations, swapping genes between two positions and
  !! moving a gene with subsequent shift. So we have two constants for the
  !! respective rates
  real(SRP), parameter, public :: RELOCATION_SWAP_RATE = 0.05_SRP
  real(SRP), parameter, public :: RELOCATION_SHIFT_RATE = 0.01_SRP

  ! ## Chromosomes ##

  !> The number of chromosomes for the agents.
  !! @warning This is the base for setting dimensionality of several other
  !!          parameter arrays below!
  integer, parameter, public :: N_CHROMOSOMES = 6

  !> The number of alleles in each of the chromosomes. NOTE: This must be an
  !! array (vector) of the size `commondata::n_chromosomes`. We use new
  !! Fortran array constructor here to set the array values.
  !! @warning The dimensionality of this array must always coincide
  !!          with `commondata::n_chromosomes`!
  integer, parameter, dimension(N_CHROMOSOMES), public :: LEN_CHROMOSOMES =   &
                  [ 6, 5, 12, 12, 12, 12 ]

  !> This parameter defines the maximum number of alleles within the chromosome
  !! It *IS NOT* intended to vary freely/independently. Used in definitions
  !! of `_GENOTYPE_PHENOTYPE` matrices, equal to the `maxval(LEN_CHROMOSOMES)`.
  !! @note    This parameter is only used for setting the genotype x phenotype
  !!          parameter matrices.
  !! @warning `maxval(LEN_CHROMOSOMES)` cannot be used for array declaration
  !!          in many compilers, so should be set manually from values
  !!          of LEN_CHROMOSOMES above. Or may be a scalar *exceeding*
  !!          `maxval(LEN_CHROMOSOMES)`, in such case the extra values are
  !!          padded with `.FALSE.` in `reshape` (see `reshape` in the
  !!          parameter matricess below).
  integer, parameter, public :: MAX_NALLELES = 12

  !> Set the labels of the chromosomes. NOTE, must be an array(vector) ) of
  !! the size `commondata::n_chromosomes`. We use new Fortran array constructor
  !! here to set the array values.
  !! @note  Note that the length of the string values in the array declaration
  !!        below are all the same. Many compilers will issue an error
  !!        otherwise. Hence, the assignment is arranged vertically.
  !! @warning The dimensionality of this array must always coincide
  !!          with `commondata::n_chromosomes`!
  character(len=*), parameter, dimension(N_CHROMOSOMES), public ::            &
                  LAB_CHROMOSOMES =  [ "C_1_SEX ",                            &
                                       "C_2_BODY",                            &
                                       "C_3_HORM",                            &
                                       "C_4_HUNG",                            &
                                       "C_5_FEAR",                            &
                                       "C_6_REPR" ]

  !> The ploidy of the chromosome set. Can theoretically be haploid (=1),
  !! diploid (=2) or, polyploid (>2).
  !! @warning The `gamma2gene` code does not work with *haploid* genotype
  !!          `commondata::chromosome_ploidy=1`, in such a case there is no
  !!          need to select random homologous chromosome and it is impossible
  !!          to set the two parameters of the `gamma2gene` function.
  integer, parameter, public ::CHROMOSOME_PLOIDY = 2

  !> The ratio of the genome that inherited from the **mother**. The other part
  !! is inherited from the father. See implementation details of the
  !! **random independent** genetic recombination procedure
  !! `procedures the_genome::individual_genome::recombine()`.
  real(SRP), parameter, public :: GENOME_RECOMBINATION_RATIO_MOTHER = 0.8_SRP

  !> Boolean 2D matrix that determines the pattern of **fixed** chromosome
  !! **crossover**. For each chromosome, the alleles that are marked with the
  !! TRUE (YES) values are inherited from the **mother** whereas those marked
  !! FALSE (NO) are inherited from the **father**. See implementation details
  !! of the **fixed genetic crossover** procedure
  !! `procedures the_genome::individual_genome::crossover()`.
  !! @note   Note that the parameter order is **reversed**: alleles are
  !!         presented by rows, chromosomes by columns,just as in the
  !!         genotype x phenotype boolean matrices.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES), parameter,public            &
           :: GENOME_CROSSOVER_FIXED_MOTHER = reshape (                       &
  ! ............................................................................
                                  [ &   !   1   2   3   4   5   6
                                        !-------------------------
                                          YES,YES,YES,YES,YES,YES,   & !  1
                                          YES,YES,YES,YES,YES,YES,   & !  2
                                          YES,YES,YES,YES,YES,YES,   & !  3
                                           NO, NO,YES,YES,YES,YES,   & !  4
                                           NO, NO,YES,YES,YES,YES,   & !  5
                                           NO, NO,YES,YES,YES,YES,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows

  ! ## Sex determination ##

  !> Sex ratio for initialising genomes @f[ \frac{N_{males}}{N_{females}} @f]
  !! should evolve and change during the evolution
  real(SRP), parameter, public :: SEX_RATIO = 0.5_SRP

  !> Labels for the sex locus alleles (gene) - vector as we don't need to label
  !! individual alleles. `LABEL_LENGTH` is here to avoid a GF warning
  character(len=LABEL_LENGTH), parameter :: SEXLOCUS_LABEL="SEX_LOCUS"

  !> Set names of the sexes -- the allele labels
  character(len=*), parameter, public :: MALE="male", FEMALE="female"

  !> Sex definition can be implemented differently from all other traits.
  !! Here is an example of the **phenotype** x **genotype matrix** code for
  !! sex determination:
  !! @code
  !! !> Sex definition can be implemented differently from all other traits.
  !! !! @note   Note that the parameter order is **reversed**: alleles are
  !! !!         presented by rows, chromosomes by columns.
  !! logical, dimension(MAX_NALLELES,N_CHROMOSOMES), parameter,public         &
  !!        :: SEX_GENOTYPE_PHENOTYPE = reshape (                             &
  !! ! .........................................................................
  !!                              [ &   !   1   2   3   4   5   6
  !!                                    !-------------------------
  !!                                      YES, NO, NO, NO, NO, NO,   & !  1
  !!                                       NO, NO, NO, NO, NO, NO,   & !  2
  !!                                       NO, NO, NO, NO, NO, NO,   & !  3
  !!                                       NO, NO, NO, NO, NO, NO,   & !  4
  !!                                       NO, NO, NO, NO, NO, NO,   & !  5
  !!                                       NO, NO, NO, NO, NO, NO,   & !  6
  !!                                       NO, NO, NO, NO, NO, NO,   & !  7
  !!                                       NO, NO, NO, NO, NO, NO,   & !  8
  !!                                       NO, NO, NO, NO, NO, NO,   & !  9
  !!                                       NO, NO, NO, NO, NO, NO,   & ! 10
  !!                                       NO, NO, NO, NO, NO, NO,   & ! 11
  !!                                       NO, NO, NO, NO, NO, NO ], & ! 12
  !!                      !                -------------------------
  !!                      ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
  !!                      [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  !! ! Note: additional reshape params:   array shape |pad | order
  !! !                                                |with| by rows
  !! ! ............................................................................
  !! @endcode
  !! @note   Note that the parameter order is **reversed**: alleles are
  !!         presented by rows, chromosomes by columns.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES), parameter,public            &
           :: SEX_GENOTYPE_PHENOTYPE = reshape (                              &
  ! ............................................................................
                                  [ &   !   1   2   3   4   5   6
                                        !-------------------------
                                          YES, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> @}

  !.............................................................................
  !.............................................................................

  !> @name Hormonal parameters
  !! @{

  ! Hormonal parameters
  ! ===================

  !> @brief   Genotype x Phenotype matrix for **growth hormone**.
  !! @details This two-dimensional array defines the phenotypic structure of the
  !!          hormone objects, i.e. the correspondence between the gene objects
  !!          and the trait values (produced by the sigmoid function). That is,
  !!          which genes on which chromosomes contribute to the pheontypic
  !!          values of the trait objects. This is a two dimensional array of
  !!          the `logical` type that defines the allele and chromosome
  !!          contributes to this specific trait.
  !! @note    Note that the dimensions of the genetic structure array must fit
  !!          within the  `N_CHROMOSOMES x maxval(LEN_CHROMOSOMES(chrom_nr))`.
  !! @note    Note that the parameter order is **reversed**: alleles are
  !!          presented by rows, chromosomes by columns.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
           :: GROWHORM_GENOTYPE_PHENOTYPE = reshape (                         &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO,YES, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Genotype to phenotype gamma2gene initialisation value for **growth
  !! hormone**
  real(SRP), parameter, public :: GROWHORM_INIT = 0.5_SRP

  !> Genotype to phenotype gamma2gene Gaussian error parameter. This is
  !! really the coefficient of variation of the output hormone level with
  !! respect to an ideal value (initially 0).
  real(SRP), parameter, public :: GROWHORM_GERROR_CV = 0.5_SRP

  !-----------------------------------------------------------------------------

  !> @brief   Genotype x Phenotype matrix for **thyroid**.
  !! @details This two-dimensional array defines the phenotypic structure of the
  !!          hormone objects, i.e. the correspondence between the gene objects
  !!          and the trait values (produced by the sigmoid function). That is,
  !!          which genes on which chromosomes contribute to the pheontypic
  !!          values of the trait objects. This is a two dimensional array of
  !!          the `logical` type that defines the allele and chromosome
  !!          contributes to this specific trait.
  !! @note    Note that the dimensions of the genetic structure array must fit
  !!          within the  `N_CHROMOSOMES x maxval(LEN_CHROMOSOMES(chrom_nr))`.
  !! @note    Note that the parameter order is **reversed**: alleles are
  !!          presented by rows, chromosomes by columns.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
           :: THYROID_GENOTYPE_PHENOTYPE = reshape (                          &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO,YES, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Genotype to phenotype gamma2gene initialisation value for **thyroid**
  real(SRP), parameter, public :: THYROID_INIT = 0.5_SRP

  !> Genotype to phenotype gamma2gene Gaussian error parameter. This is
  !! really the coefficient of variation of the output hormone level with
  !! respect to an ideal value (initially 0).
  real(SRP), parameter, public :: THYROID_GERROR_CV = 0.5_SRP

  !-----------------------------------------------------------------------------

  !> @brief  Genotype x Phenotype matrix for **adrenaline**
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
           :: ADRENALINE_GENOTYPE_PHENOTYPE = reshape (                       &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO,YES, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Genotype to phenotype gamma2gene initialisation value for **adrenaline**
  real(SRP), parameter, public :: ADRENALINE_INIT = 0.5_SRP

  !> Genotype to phenotype gamma2gene Gaussian error parameter. This is
  !! really the coefficient of variation of the output hormone level with
  !! respect to an ideal value (initially 0).
  real(SRP), parameter, public :: ADRENALINE_GERROR_CV = 0.5_SRP

  !-----------------------------------------------------------------------------

  !> @brief  Genotype x Phenotype matrix for **cortisol**.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
           :: CORTISOL_GENOTYPE_PHENOTYPE = reshape (                         &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO,YES, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Genotype to phenotype gamma2gene initialisation value for **cortisol**
  real(SRP), parameter, public :: CORTISOL_INIT = 0.5_SRP

  !> Genotype to phenotype gamma2gene Gaussian error parameter. This is
  !! really the coefficient of variation of the output hormone level with
  !! respect to an ideal value (initially 0).
  real(SRP), parameter, public :: CORTISOL_GERROR_CV = 0.5_SRP

  !-----------------------------------------------------------------------------

  !> @brief  Genotype x Phenotype matrix for **testosterone**.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
           :: TESTOSTERONE_GENOTYPE_PHENOTYPE = reshape (                     &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO,YES, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Genotype to phenotype gamma2gene initialisation value for **testosterone**
  real(SRP), parameter, public :: TESTOSTERONE_INIT = 0.01_SRP

  !> Genotype to phenotype gamma2gene Gaussian error parameter. This is
  !! really the coefficient of variation of the output hormone level with
  !! respect to an ideal value (initially 0).
  real(SRP), parameter, public :: TESTOSTERONE_GERROR_CV = 0.5_SRP

  !-----------------------------------------------------------------------------

  !> @brief  Genotype x Phenotype matrix for ESTROGEN
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
           :: ESTROGEN_GENOTYPE_PHENOTYPE = reshape (                         &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO,YES, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Genotype to phenotype gamma2gene initialisation value for **estrogen**
  real(SRP), parameter, public :: ESTROGEN_INIT = 0.01_SRP

  !> Genotype to phenotype gamma2gene Gaussian error parameter. This is
  !! really the coefficient of variation of the output hormone level with
  !! respect to an ideal value (initially 0).
  real(SRP), parameter, public :: ESTROGEN_GERROR_CV = 0.5_SRP

  !-----------------------------------------------------------------------------
  ! ## Sex steroid increment parameters ##

  !> The number of the latest historical values that are checked for change
  !! when setting an increment of the **sex steroid** hormones.
  integer, parameter, public :: SEX_STEROIDS_CHECK_HISTORY = 3

  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !> @note Note that these parameters link sex steroid increment
  !! with the agent's **age**.

  !> The array defining the **abscissa** (X) of the nonparametric function
  !! curve that defines the relationship between the age of the agent
  !! and the steroid increment factor for this specific age.
  !! @warning The LIFESPAN parameter has to be converted to the real type.
  !! @warning All these arrays must have the same dimensionality:
  !!          - commondata::sex_steroids_increment_factor_age_curve_abscissa
  !!          - commondata::sex_steroids_increment_factor_age_curve_ordinate
  !!          - commondata::sex_steroids_increment_factor_len_curve_abscissa
  !!          - commondata::sex_steroids_increment_factor_len_curve_ordinate
  !!          .
  real(SRP), dimension(*),              &
      parameter, public :: SEX_STEROIDS_INCREMENT_FACTOR_AGE_CURVE_ABSCISSA = &
                [ 0.0_SRP, real(LIFESPAN, SRP)*0.25_SRP, real(LIFESPAN, SRP) ]

  !> The array defining the **ordinate** (Y) of the nonparametric function
  !! curve that defines the relationship between the age of the agent
  !! and the steroid increment factor for this specific age.
  !! @warning All these arrays must have the same dimensionality:
  !!          - commondata::sex_steroids_increment_factor_age_curve_abscissa
  !!          - commondata::sex_steroids_increment_factor_age_curve_ordinate
  !!          - commondata::sex_steroids_increment_factor_len_curve_abscissa
  !!          - commondata::sex_steroids_increment_factor_len_curve_ordinate
  !!          .
  real(SRP), dimension(*),              &
      parameter, public :: SEX_STEROIDS_INCREMENT_FACTOR_AGE_CURVE_ORDINATE = &
                [ 0.0_SRP, 0.01_SRP, 0.1_SRP ]

  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !> @note Note that these parameters link sex steroid increment
  !! with the agent's **body length**.

  !> The array defining the **abscissa** (X) of the nonparametric function
  !! curve that defines the relationship between the body length of the agent
  !! and the steroid increment factor for this specific length.
  !! @warning All these arrays must have the same dimensionality:
  !!          - commondata::sex_steroids_increment_factor_age_curve_abscissa
  !!          - commondata::sex_steroids_increment_factor_age_curve_ordinate
  !!          - commondata::sex_steroids_increment_factor_len_curve_abscissa
  !!          - commondata::sex_steroids_increment_factor_len_curve_ordinate
  !!          .
  real(SRP), dimension(*),              &
      parameter, public :: SEX_STEROIDS_INCREMENT_FACTOR_LEN_CURVE_ABSCISSA = &
                [ 0.0_SRP, BODY_LENGTH_MAX*0.2_SRP, BODY_LENGTH_MAX ]

  !> The array defining the **ordinate** (Y) of the nonparametric function
  !! curve that defines the relationship between the body length of the agent
  !! and the steroid increment factor for this specific length.
  !! @warning All these arrays must have the same dimensionality:
  !!          - commondata::sex_steroids_increment_factor_age_curve_abscissa
  !!          - commondata::sex_steroids_increment_factor_age_curve_ordinate
  !!          - commondata::sex_steroids_increment_factor_len_curve_abscissa
  !!          - commondata::sex_steroids_increment_factor_len_curve_ordinate
  !!          .
  real(SRP), dimension(*),              &
      parameter, public :: SEX_STEROIDS_INCREMENT_FACTOR_LEN_CURVE_ORDINATE = &
                [ 0.0_SRP, 0.01_SRP, 0.1_SRP ]

  !> @}

  !.............................................................................
  !.............................................................................

  !> @name Body parameters, individual physiology and condition
  !! @{

  ! Body parameters, individual physiology and condition
  ! ====================================================

  !> History stack size for the agent's basic properties, such as body length
  !! and body mass. Normally they are saved only for the analysis and currently
  !! not used in the perception.
  integer, parameter, public :: HISTORY_SIZE_AGENT_PROP = 100

  !> Living cost in terms of food consumed. metabolic costs, p
  !! roportional to body size
  real(SRP), parameter, public :: LIVING_COST = 4.0_SRP

  !> A minimum body mass increment when any linear growth is possible,
  !! in units of the body mass (e.g. 0.05 = 5%)
  real(SRP), parameter, public :: MASS_GROWTH_THRESHOLD = 0.0001_SRP

  !> Growth exponent linking linear growth and body mass growth. Based on
  !! Fulton's condition factor "cube law."
  !! @note It is real and can get noninteger values due to nonisometric growth.
  real(SRP), parameter, public :: LINEAR_GROWTH_EXPONENT = 3.0_SRP

  !> The array defining the **abscissa** (X) of the nonparametric function
  !! curve that defines the function linking the relationship between the
  !! growth hormone and the relative linear growth increment.
  !! @warning Note that these are raw values that should go via the gamma2gene
  !!          fake "guess" calculation version.
  !! @warning Must have the same dimensionality as
  !!         commondata::linear_growth_hormone_increment_factor_curve_ordinate.
  real(SRP), dimension(*),                                                    &
          parameter, public ::                                                &
              LINEAR_GROWTH_HORMONE_INCREMENT_FACTOR_CURVE_ABSCISSA =         &
                [ 0.0_SRP, GROWHORM_INIT,         GROWHORM_INIT*3.0_SRP,      &
                           GROWHORM_INIT*5.0_SRP, GROWHORM_INIT*20.0_SRP  ]

  !> The array defining the **ordinate** (Y) of the nonparametric function
  !! curve that defines the function linking the relationship between the
  !! growth hormone and the relative linear growth increment.
  !! @warning Must have the same dimensionality as
  !!         commondata::linear_growth_hormone_increment_factor_curve_abscissa.
  real(SRP), dimension(*),                                                    &
          parameter, public ::                                                &
              LINEAR_GROWTH_HORMONE_INCREMENT_FACTOR_CURVE_ORDINATE =         &
              [0.0_SRP, 0.6_SRP, 0.9_SRP, 0.98, 1.00_SRP]

  !> Set the maximum stomach capacity default value -- fraction of the
  !! body mass available for food. Can be overriden in different agent types.
  !! Normally 15%
  real(SRP), parameter, public :: MAX_STOMACH_CAPACITY_DEF = 0.15_SRP

  !> @brief   Stomach content emptify factor at each step.
  !! @details Stomach contents S(t) is emptied by a constant fraction each
  !!          time step @f[ S_{t} =S_{t+1} \frac{ K }{\Omega }  @f],
  !!          where @f$ K @f$ is the stomach content emptify factor.
  real(SRP), parameter, public :: STOMACH_CONTENT_EMPTIFY_FACTOR = 100.0_SRP

  !> Set average stomach capacity at birth/init in units of body weight,
  !! @note it is a random Gaussian variable, this just sets the mean value,
  !!       the next parameter sets coefficient of variation
  real(SRP), parameter, public :: STOMACH_CONTENT_INIT = 0.01_SRP

  !> Set the coefficient of variation for the stomach capacity at init
  real(SRP), parameter, public :: STOMACH_CONTENT_INIT_CV = 0.05_SRP

  !> Set the weighting factor parameter of burst swimming cost in terms of
  !! the agent body size and the distance expressed in terms of the agent
  !! body lengths. In the_body::condition_cost_swimming_burst(), this
  !! parameter sets the @f$ \beta @f$ coefficient.
  real(SRP), parameter, public :: SWIMMING_SPEED_COST_BURST = 0.1E-4_SRP

  !> Set the cost of foraging in terms of SMR.
  real(SRP), parameter, public :: COST_FACTOR_FORAGING_SMR = 0.1E-5_SRP

  !-----------------------------------------------------------------------------

  !> @brief The initial value of the **energy reserves** at birth is genetically
  !!        determined. This is the Genotype x Phenotype matrix for energy
  !!        reserves.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES), parameter, public           &
           :: ENERGY_GENOTYPE_PHENOTYPE = reshape (                      &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> This is the initial value of the energy reserves, non-genetic mean.
  real(SRP), parameter, public :: ENERGY_INIT = 0.18

  !> Genotype to phenotype initialisation, Gaussian error parameter. Coefficient
  !! of variation for the `ENERGY_INIT` value
  real(SRP), parameter, public :: ENERGY_GERROR_CV = 0.01

  !-----------------------------------------------------------------------------

  !> @brief The initial value of the body length at birth is genetically
  !!        determined. This is the Genotype x Phenotype matrix for body length.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
           :: BODY_LENGTH_GENOTYPE_PHENOTYPE = reshape (                      &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> The initial value of **body length**, the average (gon-genetic).
  real(SRP), parameter, public :: BODY_LENGTH_INIT = 2.3_SRP

  !> Genotype to phenotype initialisation, Gaussian error parameter. Coefficient
  !! of variation for the `BODY_LENGTH_INIT` value
  real(SRP), parameter, public :: BODY_LENGTH_GERROR_CV = 0.1_SRP

  !-----------------------------------------------------------------------------

  !> @brief The initial value of the **control unselected** trait. This trait
  !!        is is genetically determined but is not selected or used. So it
  !!        can be used to control for random genetic drift. This is the
  !!        Genotype x Phenotype matrix.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
           :: CONTROL_UNSELECTED_GENOTYPE_PHENOTYPE = reshape (               &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO,YES, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> The initial value of the **control unselected** trait that goes through
  !! the gamma2gene.
  real(SRP), parameter, public :: CONTROL_UNSELECTED_INIT = 0.5_SRP

  !> Genotype to phenotype initialisation, Gaussian error parameter. Coefficient
  !! of variation for the control unselected trait.
  real(SRP), parameter, public :: CONTROL_UNSELECTED_GERROR_CV = 0.5_SRP

  !-----------------------------------------------------------------------------

  !> @brief The initial value of the standard metabolic rate (SMR) at birth is
  !!        genetically determined. This is the Genotype x Phenotype matrix
  !!        for SMR.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
           :: SMR_GENOTYPE_PHENOTYPE = reshape (                              &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO,YES, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> This is the initial value of SMR that goes through the gamma2gene
  real(SRP), parameter, public :: SMR_INIT = 0.5_SRP

  !> Genotype to phenotype initialisation, Gaussian error parameter. Coefficient
  !! of variation for the `SMR_LENGTH_INIT` value
  real(SRP), parameter, public :: SMR_GERROR_CV = 0.5_SRP

  !> Minimum SMR value, anything lower is not allowed.
  real(SRP), parameter, public :: SMR_MIN = 0.01_SRP

  !> Default swimming cost body mass exponent parameter for **laminar**
  !! flow. See doi:10.1242/jeb.01484 (https://dx.doi.org/10.1242/jeb.01484)
  !! and the_body::condition_cost_swimming_burst() for details.
  real(SRP), parameter :: SWIMMING_COST_EXPONENT_LAMINAR = 0.5_SRP

  !> Default swimming cost body mass exponent parameter for **turbulent**
  !! flow. See doi:10.1242/jeb.01484 (https://dx.doi.org/10.1242/jeb.01484)
  !! and the_body::condition_cost_swimming_burst() for details.
  real(SRP), parameter :: SWIMMING_COST_EXPONENT_TURBULENT = 0.6_SRP

  !> This parameter defines the cost of the buoyancy-based locomotion as a
  !! fraction of normal laminar flow propulsion for lowering downwards.
  real(SRP), parameter, public :: SWIMMING_COST_FACTOR_BUOYANCY_DOWN = 0.01_SRP

  !> This parameter defines the cost of the buoyancy-based locomotion as a
  !! fraction of normal laminar flow propulsion for lowering downwards.
  !! @note Note that the relative cost of buoyancy-based swimming up is
  !!       higher than down because gas secretion is more slow and demanding
  !!       than absorption.
  real(SRP), parameter, public :: SWIMMING_COST_FACTOR_BUOYANCY_UP = 0.1_SRP


  !> @}

  !.............................................................................
  !.............................................................................

  !> @name Parameters of the neurobiological architecture
  !! @{

  ! Parameters of the neurobiological architecture
  ! ==============================================

  ! ## Perception ##

  !. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! ### Parameters of food objects/items **perception** ###

  !> Sets the limit for partial indexing and ranking of **food items** in
  !! the visual range of the agents.
  !! @note Calculating the distances between the agent and food items
  !!       may be very slow if the number of food items is large (and
  !!       every agent should do this repeatedly!!!). So we use partial
  !!       indexing, we need only small number of neighbouring food
  !!       items anyway.
  integer, parameter, public :: FOOD_SELECT_ITEMS_INDEX_PARTIAL = 20

  !> Sets the limit for partial indexing and ranking of **conspecifics**
  !! in the visual range of the agent.
  integer, parameter, public :: CONSP_SELECT_ITEMS_INDEX_PARTIAL = 20

  !> Sets the limit for partial indexing and ranking of **predators**
  !! in the visual range of the agent.
  integer, parameter, public :: PRED_SELECT_ITEMS_INDEX_PARTIAL = 20

  !> Inherent contrast of the **agent**, It is used in determining
  !! the visual range of an agent in perception of conspecifics, and also
  !! for assessing the agent's detectability by predator. Default Contrast of
  !! food items is set separately.
  real(SRP), parameter, public :: INDIVIDUAL_VISUAL_CONTRAST_DEFAULT = 1.0_SRP

  !> Sets the size of the perception memory stack.
  integer, parameter, public :: HISTORY_SIZE_PERCEPTION = HISTORY_SIZE_SPATIAL

  !.............................................................................

  ! ## Appraisal ##

  !.............................................................................

  !> Sets the size of the emotional state memory stack.
  integer, parameter, public :: HISTORY_SIZE_MOTIVATION = HISTORY_SIZE_SPATIAL

  !.............................................................................
  ! ### Parameters for the HUNGER motivational state ###

  !-----------------------------------------------------------------------------
  !> The genotype structure for **light** perception effects on **hunger**
  !! that goes via gamma2gene perception to neuronal response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: LIGHT_HUNGER_GENOTYPE_NEURONAL = reshape (                       &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO,YES, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **light** perception
  !! effects on **hunger**.
  real(SRP), parameter, public ::                                             &
              LIGHT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **depth** perception effects on **hunger**
  !! that goes via gamma2gene perception to neuronal response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: DEPTH_HUNGER_GENOTYPE_NEURONAL = reshape (                       &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO,YES, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **depth** perception
  !! effects on **hunger**.
  real(SRP), parameter, public ::                                             &
              DEPTH_HUNGER_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **food items count** perception effects on
  !! **hunger** that goes via gamma2gene perception to neuronal response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: FOODCOUNT_HUNGER_GENOTYPE_NEURONAL = reshape (                   &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO,YES, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **food items count**
  !! perception effects on **hunger**.
  real(SRP), parameter, public ::                                             &
            FOODCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **food items count** perception effects on
  !! **hunger** that goes via gamma2gene perception to neuronal response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: FOOD_MEM_HUNGER_GENOTYPE_NEURONAL = reshape (                    &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO,YES, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **food items count**
  !! perception effects on **hunger**.
  real(SRP), parameter, public ::                                             &
            FOOD_MEM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **conspecifics number** perception effects on
  !! **hunger** that goes via gamma2gene perception to neuronal response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL = reshape (                  &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO,YES, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **conspecifics number count**
  !! perception effects on **hunger**.
  real(SRP), parameter, public ::                                             &
          CONSPCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **direct predation** perception effects on
  !! **hunger** that goes via gamma2gene perception to neuronal response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL = reshape (                 &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO,YES, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **direct predation**
  !! perception effects on **hunger**.
  real(SRP), parameter, public ::                                             &
          PRED_DIRECT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **mean predator number** perception effects on
  !! **hunger** that goes via gamma2gene perception to neuronal response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL = reshape (              &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO,YES, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **mean predator number**
  !! perception effects on **hunger**.
  real(SRP), parameter, public ::                                             &
      PRED_MEANCOUNT_HUNGER_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !> The weight of the immediately seen predators over those in the perceptual
  !! memory stack. Those in the memory then have this weight in the predation
  !! risk estimate: `1-PREDATION_RISK_WEIGHT_IMMEDIATE`.
  !! See the_neurobio::predation_risk_backend().
  !! @note  Note that this parameter also works in appraisal functions for the
  !!        fear state neuronal respones components.
  real(SRP), parameter, public :: PREDATION_RISK_WEIGHT_IMMEDIATE = 0.7

  !> The length of the perceptual memory window that is taken into account and
  !! weighted against the currently seen number of predators when calculating
  !! the motivation value.
  !! @note Note that the total memory window is `HISTORY_SIZE_PERCEPTION`.
  integer, parameter, public :: PREDATION_RISK_WEIGHT_MEMORY_WINDOW =         &
              nint(HISTORY_SIZE_PERCEPTION / 2.0) !! Here set to 1/2 of total.

  !-----------------------------------------------------------------------------
  !> The genotype structure for **stomach** perception effects on **hunger**
  !! that goes via gamma2gene perception to neuronal response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: STOM_HUNGER_GENOTYPE_NEURONAL = reshape (                        &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO,YES, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **stomach** perception
  !! effects on **hunger**.
  real(SRP), parameter, public ::                                             &
              STOM_HUNGER_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **body mass** perception effects on **hunger**
  !! that goes via gamma2gene perception to neuronal response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: BODYMASS_HUNGER_GENOTYPE_NEURONAL = reshape (                    &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO,YES, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **body mass** perception
  !! effects on **hunger**.
  real(SRP), parameter, public ::                                             &
            BODYMASS_HUNGER_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **energy reserves** perception effects on
  !! **hunger** that goes via gamma2gene perception to neuronal response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: ENERGY_HUNGER_GENOTYPE_NEURONAL = reshape (                      &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO,YES, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **energy reserves** perception
  !! effects on **hunger**.
  real(SRP), parameter, public ::                                             &
              ENERGY_HUNGER_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **age** perception effects on
  !! **hunger** that goes via gamma2gene perception to neuronal response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: AGE_HUNGER_GENOTYPE_NEURONAL = reshape (                    &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO,YES, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **age** perception
  !! effects on **hunger**.
  real(SRP), parameter, public ::                                             &
                  AGE_HUNGER_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **reproductive factor** perception effects on
  !! **hunger** that goes via gamma2gene perception to neuronal response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: REPRFAC_HUNGER_GENOTYPE_NEURONAL = reshape (                     &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO,YES, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **reproductive factor**
  !! perception effects on **hunger**.
  real(SRP), parameter, public ::                                             &
              REPRFAC_HUNGER_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !.............................................................................
  ! ### Parameters for the FEAR motivational state ###

  !-----------------------------------------------------------------------------
  !> The genotype structure for **light** perception effects on
  !! **fear state** that goes via gamma2gene perception to neuronal
  !! response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL = reshape (                   &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO,YES, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **light** perception
  !! effects on **fear state**.
  real(SRP), parameter, public ::                                             &
            LIGHT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **depth** perception effects on **active
  !! avoidance** that goes via gamma2gene perception to neuronal response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL = reshape (                   &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO,YES, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **depth** perception
  !! effects on **fear state**.
  real(SRP), parameter, public ::                                             &
            DEPTH_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **food items count** perception effects on
  !! **fear state** that goes via gamma2gene perception to neuronal
  !! response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL = reshape (               &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO,YES, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **food items count**
  !! perception effects on **fear state**.
  real(SRP), parameter, public ::                                             &
        FOODCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **food items count** perception effects on
  !! **fear state** that goes via gamma2gene perception to neuronal
  !! response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL = reshape (                &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO,YES, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **food items count**
  !! perception effects on **fear state**.
  real(SRP), parameter, public ::                                             &
        FOOD_MEM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **conspecifics number** perception effects on
  !! **fear state** that goes via gamma2gene perception to neuronal
  !! response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL = reshape (              &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO,YES, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **conspecifics number count**
  !! perception effects on **fear state**.
  real(SRP), parameter, public ::                                             &
      CONSPCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **direct predation** perception effects on
  !! **fear state** that goes via gamma2gene perception to neuronal
  !! response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL = reshape (             &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO,YES, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **direct predation**
  !! perception effects on **fear state**.
  real(SRP), parameter, public ::                                             &
      PRED_DIRECT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **mean predator number** perception effects on
  !! **fear state** that goes via gamma2gene perception to neuronal
  !! response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL = reshape (          &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO,YES, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **mean predator number**
  !! perception effects on **fear state**.
  real(SRP), parameter, public ::                                             &
   PRED_MEANCOUNT_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **stomach** perception effects on **active
  !! avoidance** that goes via gamma2gene perception to neuronal response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: STOM_ACTV_AVOID_GENOTYPE_NEURONAL = reshape (                    &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO,YES, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **stomach** perception
  !! effects on **fear state**.
  real(SRP), parameter, public ::                                             &
            STOM_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **body mass** perception effects on **active
  !! avoidance** that goes via gamma2gene perception to neuronal response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL = reshape (                &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO,YES, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **body mass** perception
  !! effects on **fear state**.
  real(SRP), parameter, public ::                                             &
        BODYMASS_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **energy reserves** perception effects on
  !! **fear state** that goes via gamma2gene perception to neuronal
  !! response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL = reshape (                  &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO,YES, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **energy reserves** perception
  !! effects on **fear state**.
  real(SRP), parameter, public ::                                             &
          ENERGY_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **age** perception effects on
  !! **fear state** that goes via gamma2gene perception to neuronal
  !! response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
            :: AGE_ACTV_AVOID_GENOTYPE_NEURONAL = reshape (                   &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO,YES, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **age** perception
  !! effects on **fear state**.
  real(SRP), parameter, public ::                                             &
              AGE_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **reproductive factor** perception effects on
  !! **fear state** that goes via gamma2gene perception to neuronal
  !! response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
            :: REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL = reshape (               &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO,YES, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **reproductive factor**
  !! perception effects on **fear state**.
  real(SRP), parameter, public ::                                             &
          REPRFAC_ACTV_AVOID_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !.............................................................................
  ! ### Parameters for the REPRODUCTION motivational state ###

  !-----------------------------------------------------------------------------
  !> The genotype structure for **light** perception effects on
  !! **reproduction** that goes via gamma2gene perception to neuronal
  !! response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: LIGHT_REPRODUCE_GENOTYPE_NEURONAL = reshape (                   &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO,YES,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **light** perception
  !! effects on **reproduction**.
  real(SRP), parameter, public ::                                             &
            LIGHT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **depth** perception effects on **reproduction**
  !! that goes via gamma2gene perception to neuronal response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: DEPTH_REPRODUCE_GENOTYPE_NEURONAL = reshape (                   &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO,YES,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **depth** perception
  !! effects on **reproduction**.
  real(SRP), parameter, public ::                                             &
            DEPTH_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **food items count** perception effects on
  !! **reproduction** that goes via gamma2gene perception to neuronal
  !! response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL = reshape (               &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO,YES,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **food items count**
  !! perception effects on **reproduction**.
  real(SRP), parameter, public ::                                             &
        FOODCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **food items count** perception effects on
  !! **reproduction** that goes via gamma2gene perception to neuronal
  !! response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL = reshape (                &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO,YES,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **food items count**
  !! perception effects on **reproduction**.
  real(SRP), parameter, public ::                                             &
          FOOD_MEM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **conspecifics number** perception effects on
  !! **reproduction** that goes via gamma2gene perception to neuronal
  !! response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL = reshape (               &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO,YES,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **conspecifics number count**
  !! perception effects on **reproduction**.
  real(SRP), parameter, public ::                                             &
        CONSPCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **direct predation** perception effects on
  !! **reproduction** that goes via gamma2gene perception to neuronal
  !! response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL = reshape (              &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO,YES,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **direct predation**
  !! perception effects on **reproduction**.
  real(SRP), parameter, public ::                                             &
      PRED_DIRECT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **mean predator number** perception effects on
  !! **reproduction** that goes via gamma2gene perception to neuronal
  !! response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL = reshape (           &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO,YES,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **mean predator number**
  !! perception effects on **reproduction**.
  real(SRP), parameter, public ::                                             &
    PRED_MEANCOUNT_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **stomach** perception effects on
  !! **reproduction** that goes via gamma2gene perception to neuronal response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: STOM_REPRODUCE_GENOTYPE_NEURONAL = reshape (                     &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO,YES,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **stomach** perception
  !! effects on **reproduction**.
  real(SRP), parameter, public ::                                             &
              STOM_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **body mass** perception effects on
  !! **reproduction** that goes via gamma2gene perception to neuronal response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: BODYMASS_REPRODUCE_GENOTYPE_NEURONAL = reshape (                 &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO,YES,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **body mass** perception
  !! effects on **reproduction**.
  real(SRP), parameter, public ::                                             &
          BODYMASS_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **energy reserves** perception effects on
  !! **reproduction** that goes via gamma2gene perception to neuronal
  !! response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
          :: ENERGY_REPRODUCE_GENOTYPE_NEURONAL = reshape (                   &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO,YES,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **energy reserves** perception
  !! effects on **reproduction**.
  real(SRP), parameter, public ::                                             &
            ENERGY_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **age** perception effects on
  !! **reproduction** that goes via gamma2gene perception to neuronal
  !! response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
            :: AGE_REPRODUCE_GENOTYPE_NEURONAL = reshape (                   &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO,YES,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **age** perception
  !! effects on **reproduction**.
  real(SRP), parameter, public ::                                             &
              AGE_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !-----------------------------------------------------------------------------
  !> The genotype structure for **age** perception effects on
  !! **reproduction** that goes via gamma2gene perception to neuronal
  !! response.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
            :: REPRFAC_REPRODUCE_GENOTYPE_NEURONAL = reshape (                &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO,YES ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian perception error parameter (cv) for **age** perception
  !! effects on **reproduction**.
  real(SRP), parameter, public ::                                             &
          REPRFAC_REPRODUCE_GENOTYPE_NEURONAL_GERROR_CV = PERCEPT_ERROR_CV_DEF

  !.............................................................................
  ! ## Modulation parameters ##
  ! Parameters for the GENETIC and NON-GENETIC MODULATION of the motivational
  ! state. Non-genetic modulation includes developmental modulation of the
  ! reproductive modulation by weighting it out at early ages.

  !-----------------------------------------------------------------------------
  !> Disable developmental or genetic modulation of the `APPRAISAL` completely.
  !! If set to TRUE, **no modulation** is performed and the final motivation
  !! values are just equal to the primary modulation values in `APPRAISAL`.
  logical, parameter, public :: MODULATION_APPRAISAL_DISABLE_ALL = .FALSE.
  !-----------------------------------------------------------------------------

  !> Developmental modulation of reproductive motivation. This parameter
  !! defines the starting age (time steps) at which reproductive motivation
  !! can rise above zero (at all previous ages weight=0.0).
  !! See the_neurobio::appraisal_motivation_modulation_non_genetic().
  real(SRP), parameter, public ::                                             &
    REPROD_MODULATION_DEVEL_AGESTART = LIFESPAN / 2.0_SRP

  !> Developmental modulation of reproductive motivation. This parameter
  !! defines the age (time steps) at which reproductive motivation is weighted
  !! fully (weight = 1.0).
  !! See the_neurobio::appraisal_motivation_modulation_non_genetic().
  real(SRP), parameter, public ::                                             &
    REPROD_MODULATION_DEVEL_AGEFULL = REPROD_MODULATION_DEVEL_AGESTART +      &
                           REPROD_MODULATION_DEVEL_AGESTART * 2.0_SRP / 3.0_SRP

  !> Developmental modulation of reproductive motivation. This is the
  !! interpolation grid abscissa defining at which age reproduction motivation
  !! can have non-zero values (reproduction is possible). It defines the
  !! weighting factor applied to reproductive motivation.
  !!
  !! It is calculated as follows. First, the first grid value *A* is set from
  !! the parameter commondata::reprod_modulation_devel_agestart (it takes the
  !! weight 0.0). Second, the last value of the abscissa grid array *B* is set
  !! from the parameter commondata::reprod_modulation_devel_agefull (it has
  !! the weight 1.0, full reproductive factor). Third, the interval between
  !! *A* and *B* is further split into three intervals and the first point
  !! is taken as the second value of the interpolation abscissa array
  !! (assigned the weight given by commondata::reprod_modulation_devel_w2).
  !! Thus, the interval reproductive factor weighting abscissa looks like this:
  !! @verbatim
  !! +-------------------------------#----------+------------#----------+
  !! 1                               ^                       ^       LIFESPAN
  !!                                 A                       B
  !!
  !!                                 #-------#-------|-------#
  !!                                         ^
  !!                                     A+(B-A)*1/3
  !!
  !! Resulting array abscissa: [ A,   A + (B - A) / 3,            B   ]
  !!                 ordinate: [ 0.0, REPROD_MODULATION_DEVEL_W2, 1.0 ]
  !! @endverbatim
  !! See the_neurobio::appraisal_motivation_modulation_non_genetic().
  real(SRP), parameter, dimension(*) :: REPROD_MODULATION_DEVEL_ABSCISSA =    &
                        [ REPROD_MODULATION_DEVEL_AGESTART,                   &
                          REPROD_MODULATION_DEVEL_AGESTART +                  &
                            ( REPROD_MODULATION_DEVEL_AGEFULL -               &
                              REPROD_MODULATION_DEVEL_AGESTART ) / 3.0_SRP,   &
                          REPROD_MODULATION_DEVEL_AGEFULL   ]

  !> Developmental modulation of reproductive motivation. This parameter sets
  !! the interpolation array weight that defines how fast the reproduction
  !! motivation the_neurobio::state_reproduce is allowed to raise when the age
  !! of the agent exceeds the reproductive age. For details see
  !! the_neurobio::appraisal_motivation_modulation_non_genetic().
  real(SRP), parameter, public :: REPROD_MODULATION_DEVEL_W2 = 0.1_SRP

  !-----------------------------------------------------------------------------
  !> The genotype structure for sex modulation coefficient affecting
  !! **reproduction** motivation state in **males**.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
            :: SEX_MALE_MODULATION_REPRODUCE_GENOTYPE = reshape (             &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                          YES, NO, NO, NO, NO, NO,   & !  2
                                           NO, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian error parameter (cv) for the sex modulation coefficient
  !! affecting **reproduction** motivation state in **males**.
  real(SRP), parameter, public ::                                             &
                            SEX_MALE_MODULATION_REPRODUCE_GERROR_CV = 0.1_SRP

  !-----------------------------------------------------------------------------
  !> The genotype structure for sex modulation coefficient affecting
  !! **reproduction** motivation state in **females**.
  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             &
            :: SEX_FEMALE_MODULATION_REPRODUCE_GENOTYPE = reshape (           &
  ! ............................................................................
                                  [ &    !  1   2   3   4   5   6
                                         !------------------------
                                           NO, NO, NO, NO, NO, NO,   & !  1
                                           NO, NO, NO, NO, NO, NO,   & !  2
                                          YES, NO, NO, NO, NO, NO,   & !  3
                                           NO, NO, NO, NO, NO, NO,   & !  4
                                           NO, NO, NO, NO, NO, NO,   & !  5
                                           NO, NO, NO, NO, NO, NO,   & !  6
                                           NO, NO, NO, NO, NO, NO,   & !  7
                                           NO, NO, NO, NO, NO, NO,   & !  8
                                           NO, NO, NO, NO, NO, NO,   & !  9
                                           NO, NO, NO, NO, NO, NO,   & ! 10
                                           NO, NO, NO, NO, NO, NO,   & ! 11
                                           NO, NO, NO, NO, NO, NO ], & ! 12
                          !              -------------------------
                          ! max. n alleles: 6,  5, 12, 12, 12, 12, 12
                          [MAX_NALLELES,N_CHROMOSOMES],[NO],[2,1] )
  ! Note: additional reshape params:      array shape |pad | order
  !                                                   |with| by rows
  ! ............................................................................

  !> Gaussian error parameter (cv) for the sex modulation coefficient
  !! affecting **reproduction** motivation state in **females**.
  real(SRP), parameter, public ::                                             &
                            SEX_FEMALE_MODULATION_REPRODUCE_GERROR_CV = 0.1_SRP

  !.............................................................................
  ! ## Baseline attention switches ##

  !> *Baseline attention switches* @f$ \Psi _{i,j} @f$ control which perception
  !! components (*i*) can affect each of the motivational state (*j*). They
  !! should be defined for all combinations between the perception components
  !! (light, depth... food... predators... etc) and the motivational states.
  !! See @ref aha_buildblocks_cogn_arch "Cognitive architecture".
  !!
  !! These values serve as weight factors are baseline *before* attention
  !! is suppressed (the_neurobio::gos_global::attention_modulate()).
  !! They can be used for switching specific perceptual components
  !! the_neurobio::percept_components_motiv of motivation ON (1.0) or
  !! OFF (0.0) . Therefore, they should provide all possible combinations of
  !! the motivational states \%state_ (as in the_neurobio::motivation) and
  !! perception components the_neurobio::percept_components_motiv.
  !!
  !> The `ATTENTION_SWITCH_` pattern matrix for all perception components and
  !! motivation states is presented in the table below:
  !!
  !! |            |   HUNGER   | FEAR_DEFENCE | REPRODUCE   |
  !! | ---------- | ---------- | ------------ | ----------- |
  !! |  LIGHT     |     0.0    |      1.0     |      0.0    |
  !! |  DEPTH     |     0.0    |      1.0     |      0.0    |
  !! |  FOOD_DIR  |     1.0    |      0.0     |      0.0    |
  !! |  FOOD_MEM  |     1.0    |      0.0     |      0.0    |
  !! |  CONSPEC   |     1.0    |      1.0     |      1.0    |
  !! |  PRED_DIR  |     0.0    |      1.0     |      0.0    |
  !! |  PREDATOR  |     0.0    |      1.0     |      0.0    |
  !! |  STOMACH   |     1.0    |      0.0     |      0.0    |
  !! |  BODYMASS  |     1.0    |      0.0     |      1.0    |
  !! |  ENERGY    |     1.0    |      0.0     |      1.0    |
  !! |  AGE       |     0.0    |      0.0     |      1.0    |
  !! |  REPRFAC   |     0.0    |      0.0     |      1.0    |
  real(SRP), parameter, public :: ATTENTION_SWITCH_HUNGER_LIGHT        = 0.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_HUNGER_DEPTH        = 0.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_HUNGER_FOOD_DIR     = 1.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_HUNGER_FOOD_MEM     = 1.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_HUNGER_CONSPEC      = 1.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_HUNGER_PRED_DIR     = 0.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_HUNGER_PREDATOR     = 0.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_HUNGER_STOMACH      = 1.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_HUNGER_BODYMASS     = 1.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_HUNGER_ENERGY       = 1.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_HUNGER_AGE          = 0.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_HUNGER_REPRFAC      = 0.0_SRP

  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_AVOID_ACT_LIGHT     = 1.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_AVOID_ACT_DEPTH     = 1.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_AVOID_ACT_FOOD_DIR  = 0.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_AVOID_ACT_FOOD_MEM  = 0.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_AVOID_ACT_CONSPEC   = 1.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_AVOID_ACT_PRED_DIR  = 1.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_AVOID_ACT_PREDATOR  = 1.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_AVOID_ACT_STOMACH   = 0.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_AVOID_ACT_BODYMASS  = 0.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_AVOID_ACT_ENERGY    = 0.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_AVOID_ACT_AGE       = 0.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_AVOID_ACT_REPRFAC   = 0.0_SRP

  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_REPRODUCE_LIGHT     = 0.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_REPRODUCE_DEPTH     = 0.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_REPRODUCE_FOOD_DIR  = 0.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_REPRODUCE_FOOD_MEM  = 0.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_REPRODUCE_CONSPEC   = 1.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_REPRODUCE_PRED_DIR  = 0.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_REPRODUCE_PREDATOR  = 0.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_REPRODUCE_STOMACH   = 0.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_REPRODUCE_BODYMASS  = 1.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_REPRODUCE_ENERGY    = 1.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_REPRODUCE_AGE       = 1.0_SRP
  !> Baseline attention switch, see commondata::attention_switch_hunger_light.
  real(SRP), parameter, public :: ATTENTION_SWITCH_REPRODUCE_REPRFAC   = 1.0_SRP

  !.............................................................................
  ! Attention modulation curve parameters. Attention modulation by the GOS
  ! is obtained via nonlinear interpolation of the basic grid points.

  !> The array defining the **abscissa** (X) of the nonparametric function
  !! that defines the **attention modulation curve** by the current Global
  !! Organismic State (GOS).
  !! @warning Must have the same dimensionality as
  !!         commondata::attention_modulation_curve_ordinate.
  real(SRP), dimension(*), parameter, public ::                               &
      ATTENTION_MODULATION_CURVE_ABSCISSA=[0.0_SRP, 0.3_SRP, 0.5_SRP, 1.0_SRP]

  !> The array defining the **ordinate** (Y) of the nonparametric function
  !! that defines the **attention modulation curve** by the current Global
  !! Organismic State (GOS).
  !! @warning Must have the same dimensionality as
  !!         commondata::attention_modulation_curve_abscissa.
  real(SRP), dimension(*), parameter, public ::                               &
      ATTENTION_MODULATION_CURVE_ORDINATE=[1.0_SRP, 0.98_SRP, 0.9_SRP, 0.0_SRP]

  !.............................................................................
  ! ## Parameters for the GOS, Global Organismic State ##

  !> The array defining the **abscissa** (X) of the nonparametric function
  !! curve that defines the threshold for motivation competition in GOS.
  !! @warning Must have the same dimensionality as
  !!          commondata::motivation_compet_threshold_curve_ordinate.
  real(SRP), dimension(*), parameter,                                         &
          public :: MOTIVATION_COMPET_THRESHOLD_CURVE_ABSCISSA =              &
          [0.0_SRP, 0.2_SRP, 0.60_SRP, 0.80_SRP, 0.90_SRP, 1.0_SRP, 1.1_SRP]

  !> The array defining the **ordinate** (Y) of the nonparametric function
  !! curve that defines the threshold for motivation competition in GOS.
  !! @warning Must have the same dimensionality as
  !!          commondata::motivation_compet_threshold_curve_abscissa.
  real(SRP), dimension(*), parameter,                                         &
          public :: MOTIVATION_COMPET_THRESHOLD_CURVE_ORDINATE =              &
          [1.0_SRP, 0.3_SRP, 0.04_SRP, 0.01_SRP, 0.001_SRP, 0.0_SRP, 0.0_SRP]

  !> Spontaneous arousal dissipation level when a simple **fixed** dissipation
  !! factor pattern is used. At each step, `gos_arousal` is reduced by a
  !! constant factor, AROUSAL_GOS_DISSIPATION_FACTOR` (e.g. reduced by 0.5)
  !!     independently on the current GOS time step.
  real(SRP), parameter, public :: AROUSAL_GOS_DISSIPATION_FACTOR = 0.5_SRP

  !> This is the array defining the **abscissa** (X) of the nonparametric
  !! spontaneous arousal dissipation factor function involving polynomial
  !! (or linear) interpolation is used.
  !! @warning Must have the same dimensionality as
  !!          commondata::arousal_gos_dissipation_nonpar_ordinate.
  real(SRP), dimension(*), parameter,                                         &
                        public :: AROUSAL_GOS_DISSIPATION_NONPAR_ABSCISSA =   &
                                    [ 1.0, 2.00, 5.00, 10.0, 15.0, 18.0, 20.0 ]

  !> This is the array defining the **ordinate** (Y) of the nonparametric
  !! spontaneous arousal dissipation factor function involving polynomial
  !! (or linear) interpolation is used.
  !! @warning Must have the same dimensionality as
  !!          commondata::arousal_gos_dissipation_nonpar_abscissa.
  real(SRP), dimension(*), parameter,                                         &
                        public :: AROUSAL_GOS_DISSIPATION_NONPAR_ORDINATE =   &
                                    [ 1.0, 0.98, 0.80, 0.40, 0.22, 0.18, 0.17 ]

  !> Global maximum sensory information that is updated for the whole
  !! population of agents.
  !! @note This roughly corresponds to the `MaxPercept` array in the G1 model.
  real(SRP), public :: Global_Rescale_Maximum_Motivation

  !-----------------------------------------------------------------------------

  !.............................................................................
  ! ## Parameters for the behaviours ##

  !> The size of the behaviour labels history stack, i.e. for how many time
  !! steps should the stack remember record the behaviour labels.
  integer, parameter, public :: HISTORY_SIZE_BEHAVIOURS = HISTORY_SIZE_SPATIAL

  !> Default weighting factor for the baseline probability of successful
  !! reproduction @f$ \varphi @f$. See implementation details for the function
  !! the_neurobio::reproduce_do_probability_reproduction_calc().
  real(SRP), parameter, public ::  PROBABILITY_REPRODUCTION_BASE_FACTOR = 0.90

  !> Interpolation grid **abscissa** for the body mass ratio factor that
  !! scales the probability of reproduction. For details see
  !! the_neurobio::reproduce_do_probability_reproduction_calc() procedure.
  !! Commands (template) to produce interpolation plots:
  !! @verbatim
  !!   htintrpl.exe [0.5 1  2] [0 1 1.8] 0.2
  !! @endverbatim
  !! @warning Must have the same dimensionality as
  !!         commondata::probability_reproduction_delta_mass_ordinate.
  real(SRP), dimension(*), parameter,                                         &
              public :: PROBABILITY_REPRODUCTION_DELTA_MASS_ABSCISSA =        &
              [0.5_SRP, 1.0_SRP, 2.0_SRP]

  !> Interpolation grid **ordinate** for the body mass ratio factor that
  !! scales the probability of reproduction. For details see
  !! the_neurobio::reproduce_do_probability_reproduction_calc() procedure.
  !! Commands (template) to produce interpolation plots:
  !! @verbatim
  !!   htintrpl.exe [0.5 1  2] [0 1 1.8] 0.2
  !! @endverbatim
  !! @warning Must have the same dimensionality as
  !!         commondata::probability_reproduction_delta_mass_abscissa.
  real(SRP), dimension(*), parameter,                                         &
              public :: PROBABILITY_REPRODUCTION_DELTA_MASS_ORDINATE =        &
              [0.0_SRP, 1.0_SRP, 1.8_SRP]

  !> This parameter defines the threshold of the current gonadal steroids
  !! level that should exceed the baseline value determined by the genome,
  !! for reproduction to be possible.
  !! @note Note that this parameter should normally exceed 1.0
  real(SRP), parameter, public :: SEX_STEROIDS_REPRODUCTION_THRESHOLD = 1.3_SRP

  !> The weighting factor used in calculation of the default random walk
  !! distance, in terms of the agent's body length.
  real(SRP), parameter, public ::                                             &
                          WALK_RANDOM_DISTANCE_DEFAULT_FACTOR = 10.0_SRP

  !> The coefficient of variation of the distance for stochastic Gaussian
  !! random walk (distance is in terms of the agent's body length). Note that
  !! for deterministic walk, cv is zero.
  real(SRP), parameter, public :: WALK_RANDOM_DISTANCE_STOCHASTIC_CV = 0.5_SRP

  !> The maximum walk distance, **in units of the average distance to food
  !! items** in the current perception object, when the expected food gain is
  !! calculated on the bases of the current food availability, not using the
  !! the_behaviour::hope() function mechanism. If the average walk distance
  !! exceeds this value, the expectancy is based on the the_behaviour::hope()
  !! function.
  real(SRP), parameter, public :: WALK_RANDOM_FOOD_GAIN_HOPE = 4.0_SRP

  !> The maximum walk distance, **in units of the agent body length**, when
  !! the expected food gain is calculated on the bases of the current food
  !! availability, not using the the_behaviour::hope() function mechanism.
  !! If the average walk distance exceeds this value, the expectancy is based
  !! on the the_behaviour::hope() function.
  !! @note This parameter is used for switching between the food gain
  !!       calculation methods based on current perception or hope function
  !!       if the agent has no food items in perception object. Normally, if
  !!       there are food items, the commondata::walk_random_food_gain_hope
  !!       is used.
  real(SRP), parameter, public :: WALK_RANDOM_FOOD_GAIN_HOPE_AGENTL = 100.0_SRP

  !> The maximum walk distance, **in units of the agent body length**, when
  !! the expected predation risk is calculated on the basis of the current
  !! perception value, not using the the_behaviour::hope() function mechanism.
  !! If the average walk distance exceeds this value, the risk expectancy is
  !! based on the the_behaviour::hope() function.
  real(SRP), parameter, public :: WALK_RANDOM_PRED_RISK_HOPE_AGENTL = 150.0_SRP

  !> The ratio of the vertical to main horizontal shift parameters of the
  !! agent's Gaussian random walk. Random walk is done in the "2.5D" mode
  !! (the_environment::spatial_moving::rwalk25d()), i.e. with separate
  !! parameters for the main horizontal shift and the vertical depth shift.
  !! This is done to avoid a potentially too large vertical displacement
  !! of the agent during the movement. Thus, the vertical shift distance
  !! should normally be smaller than the horizontal shift. The difference
  !! between the main horizontal and (the smaller) vertical shifts is defined
  !! by this parameter. For example, if it is equal to 0.5, then the vertical
  !! depth shift is 0.5 of the main horizontal shift.
  !! See the_behaviour::walk_random_do_execute() for more details.
  real(SRP), parameter, public :: WALK_RANDOM_VERTICAL_SHIFT_RATIO = 0.5_SRP

  !> The ratio of the vertical to the main horizontal coefficients of variation
  !! for the **vertical** depth distance in the stochastic Gaussian random walk
  !! of the agent. Should normally be equal to the main default value set by
  !! commondata::walk_random_distance_stochastic_cv. That is 1.0
  real(SRP), parameter, public :: WALK_RANDOM_VERTICAL_SHIFT_CV_RATIO = 1.0_SRP

  !> This parameter defines the hope function for calculating the food
  !! perception expectancy in the the_behaviour::walk_random behaviour.
  !! This is the abscissa for the hope function grid array.
  !! Plotting: `htintrpl.exe [0 1 3.5 ] [2, 1, 0]`.
  !! See the_behaviour::walk_random_do_this().
  !! @note Note that the "maximum hope" value for normal random walk is
  !!       smaller than in the hope function grid for the migration
  !!       behaviour commondata::migrate_food_gain_maximum_hope. Thus,
  !!       the maximum incentive for movement is lower here.
  !! @warning Must have the same dimensionality as
  !!         commondata::walk_random_food_hope_ordinate.
  real(SRP), dimension(*), parameter, public ::                               &
                WALK_RANDOM_FOOD_HOPE_ABSCISSA = [ 0.0_SRP, 1.0_SRP, 3.5_SRP ]

  !> This parameter defines the hope function for calculating the food
  !! perception expectancy in the the_behaviour::walk_random behaviour.
  !! This is the ordinate for the hope function grid array.
  !! Plotting: `htintrpl.exe [0 1 3.5 ] [2, 1, 0]`.
  !! See the_behaviour::walk_random_do_this().
  !! @note Note that the "zero hope" value for normal random walk is
  !!       higher (non-zero) than in the hope function grid for the
  !!       migration behaviour. Thus, the maximum incentive for movement
  !!       is lower here.
  !! @warning Must have the same dimensionality as
  !!         commondata::walk_random_food_hope_abscissa.
  real(SRP), dimension(*), parameter, public ::                               &
                WALK_RANDOM_FOOD_HOPE_ORDINATE = [ 2.0_SRP, 1.0_SRP, 0.0_SRP ]

  !> Default offset for approach, offset is the difference between the
  !! approaching agent and the target object.
  real(SRP), parameter, public :: APPROACH_OFFSET_DEFAULT =                   &
                                                        TOLERANCE_HIGH_DEF_SRP

  !> Multiplication factor for the general risk of predation used when the
  !! agent evaluates the approach to a target conspecific.
  real(SRP), parameter, public ::                                             &
                        APPROACH_CONSPECFIC_DILUTE_GENERAL_RISK = 0.5_SRP

  !> Multiplication factor for subjective assessment of the direct risk of
  !! predation when the actor agent moves behind the target conspecific, i.e.
  !! when the distance between the agent and predator is going to become
  !! longer than the distance between the target conspecific and the agent.
  !! See the_behaviour::approach_conspecifics_do_this() for details.
  real(SRP), parameter, public ::                                             &
                        APPROACH_CONSPECFIC_DILUTE_ADJUST_PAIR_BEHIND = 0.5_SRP

  !> The grid **abscissa** defining the nonparametric relationship
  !! that determines the expected food gain for the "approach conspecifics"
  !! behaviour (the_behaviour::approach_conspec class). The function is a
  !! weighting factor depending on the ratio of the agent body mass to the
  !! target conspecific body mass, for the baseline expected food gain.
  !! @verbatim
  !!   htintrpl.exe [ 0 0.1 1 1.5 ] [ 0 0.01 0.5 1 ]
  !! @endverbatim
  !! @note The (last) maximum value of the grid defines the body mass ratio
  !!       that guarantees 100% expectancy of winning of competition for food
  !!       against the target conspecific. For example, the value of 1.5
  !!       means that an agent is guaranteed to get the whole baseline
  !!       expected food gain if its body weight is 1.5 of the target
  !!       conspecific.
  !! See the_behaviour::approach_conspecifics_do_this() for details.
  !! @warning Must have the same dimensionality as
  !!         commondata::approach_food_gain_compet_factor_ordinate.
  real(SRP), dimension(*), parameter,                                         &
      public :: APPROACH_FOOD_GAIN_COMPET_FACTOR_ABSCISSA =                   &
                              [ 0.00_SRP, 0.10_SRP, 1.00_SRP, 1.50_SRP ]

  !> The grid **ordinate** defining the nonparametric relationship
  !! that determines the expected food gain for the "approach conspecifics"
  !! behaviour (the_behaviour::approach_conspec class). The function is a
  !! weighting factor depending on the ratio of the agent body mass to the
  !! target conspecific body mass, for the baseline expected food gain.
  !! @verbatim
  !!   htintrpl.exe [ 0 0.1 1 1.5 ] [ 0 0.01 0.5 1 ]
  !! @endverbatim
  !! See the_behaviour::approach_conspecifics_do_this() for details.
  !! @warning Must have the same dimensionality as
  !!         commondata::approach_food_gain_compet_factor_abscissa.
  real(SRP), dimension(*), parameter,                                         &
      public :: APPROACH_FOOD_GAIN_COMPET_FACTOR_ORDINATE =                   &
                              [ 0.00_SRP, 0.01_SRP, 0.50_SRP, 1.00_SRP ]

  !> The weighting factor for the distance to the expected food item if the
  !! actual distance is uncertain (e.g. no food items currently in perception).
  !! See the_behaviour::walk_random_motivations_expect().
  real(SRP), parameter, public :: DIST_EXPECT_FOOD_UNCERTAIN_FACT = 0.7_SRP

  !> The size of the memory window that is used in the assessment of predation
  !! risk, as a portion of the commondata::history_size_perception.
  !! See the_behaviour::walk_random_do_this() and
  !! the_behaviour::walk_random_motivations_expect().
  real(SRP), parameter, public :: HISTORY_PERCEPTION_WINDOW_PRED = 0.3_SRP

  !> The size of the memory window that is used in the assessment of food
  !! gain, as a portion of the commondata::history_size_perception.
  !! See the_behaviour::walk_random_do_this() and
  !! the_behaviour::walk_random_motivations_expect().
  real(SRP), parameter, public :: HISTORY_PERCEPTION_WINDOW_FOOD = 0.3_SRP

  !> The weighting factor used in calculation of the default escape distance.
  !! The escape distance is equal to the visibility range of the predator
  !! multiplied by this factor. Therefore, it should normally exceed 1.0.
  !! Otherwise, the escaping object is still within the visibility range
  !! of the predator after the escape.
  !! See the_behaviour::escape_dart_do_this() for more details.
  real(SRP), parameter, public :: ESCAPE_DART_DISTANCE_DEFAULT_FACTOR = 1.5_SRP

  !> For stochastic escape, this parameter determines the coefficient
  !! of variation of the escape walk. See the_behaviour::escape_dart_do_this()
  !! for more details.
  real(SRP), parameter, public ::                                             &
                ESCAPE_DART_DISTANCE_DEFAULT_STOCH_CV = 0.5_SRP

  !> The default size of the up and down walks performed by the GO_DOWN_DEPTH
  !! and GO_UP_DEPTH, see the_behaviour::go_down_depth and
  !! the_behaviour::go_up_depth classes as well as
  !! the_behaviour::go_down_do_this() and the_behaviour::go_up_do_this() methods.
  real(SRP), parameter, public :: UP_DOWN_WALK_STEP_STDLENGTH_FACTOR = 4.0_SRP

  !> The maximum distance (in units of the agent body length) a migrating
  !! agent can pass for a single time step of the model. This is basically
  !! limited by (an implicit) maximum speed of the agent, in terms of its
  !! body length. This parameter sets the limit on the length of a single
  !! migration bout.
  real(SRP), parameter, public :: MIGRATE_DIST_MAX_STEP = 800.0_SRP

  !> Default maximum distance towards the target environment (in units of the
  !! agent's body size) when the agent could emigrate into this target
  !! environment. See the_behaviour::behaviour_do_migrate_random() for details.
  real(SRP), parameter, public :: MIGRATE_RANDOM_MAX_DIST_TARGET = 10.0_SRP

  !> The offset, in terms of the body length of the actor agent, for
  !! initial penetrating into the target environment when the agent is
  !! migrating into this environment. See the_environment::migrate_do_this().
  real(SRP), parameter, public :: MIGRATE_DIST_PENETRATE_OFFSET = 1.0_SRP

  !> This parameter defines the hope function for calculating the food gain
  !! expectancy in the migration behaviour. This is the maximum value of the
  !! hope function that is achieved at zero ratio of the old to new food gain
  !! memory values. Plotting: `htintrpl.exe [0 1 3.5] [2 1 0]`.
  !! See the_behaviour::migrate_do_this().
  real(SRP), parameter, public :: MIGRATE_FOOD_GAIN_MAXIMUM_HOPE = 2.0_SRP

  !> This parameter defines the hope function for calculating the food gain
  !! expectancy in the migration behaviour. This is the maximum ratio of the
  !! old to new food gain memory values that leads to virtually zero value
  !! of the hope function. Plotting:  `htintrpl.exe [0 1 3.5] [2 1 0]`.
  !! See the_behaviour::migrate_do_this().
  real(SRP), parameter, public :: MIGRATE_FOOD_GAIN_RATIO_ZERO_HOPE = 3.5_SRP

  !> This parameter defines the hope function for calculating the general
  !! predation risk expectancy in the migration behaviour. This is the
  !! maximum value of the hope function that is achieved at zero ratio of the
  !! old to new predation values in the memory stack.
  !! Plotting: `htintrpl.exe [0 1 3.5] [2 1 0]`.
  !! See the_behaviour::migrate_do_this().
  real(SRP), parameter, public :: MIGRATE_PREDATOR_MAXIMUM_HOPE = 2.0_SRP

  !> This parameter defines the hope function for calculating the general
  !! predation risk expectancy in the migration behaviour. This is the maximum
  !! ratio of the old to new predation values in the memory stack that leads
  !! to virtually zero value of the hope function.
  !! Plotting: `htintrpl.exe [0 1 3.5] [2 1 0]`.
  !! See the_behaviour::migrate_do_this().
  real(SRP), parameter, public :: MIGRATE_PREDATOR_ZERO_HOPE = 3.5_SRP

  !> This parameter array defines the repertoire of predetermined static walk
  !! step sizes, in units of the agent's body length, for the
  !! the_behaviour::walk_random behavioural unit as executed in the
  !! the_behaviour::behaviour::walk_random class level.
  !! See the_behaviour::behaviour::select() method for details.
  real(SRP), dimension(*), parameter, public :: BEHAV_WALK_STEP_STDLEN_STATIC  &
      = [ 1.0_SRP, 10.0_SRP, 25.0_SRP, 50.0_SRP, 100.0_SRP ]

  !> This parameter array defines the step sizes, in units of the agent's body
  !! length, for the the_behaviour::go_down_depth and the_behaviour::go_up_depth
  !! behavioural unit as executed in the the_behaviour::behaviour::depth_down
  !! and the_behaviour::behaviour::depth_up class level(s).
  !! See the_behaviour::behaviour::select() method for details.
  real(SRP), dimension(*), parameter, public ::                               &
              BEHAV_GO_UP_DOWN_STEP_STDLEN_STATIC =                           &
              [ 10.0_SRP, 20.0_SRP, 50.0_SRP, 75.0_SRP, 100.0_SRP ]


  !> @}

  !.............................................................................
  !.............................................................................

  !> @name Parameters of the Genetic Algorithm
  !> @{

  !> Percentage of the best reproducing agents in the pre-evolution phase.
  real(SRP), parameter, public :: GA_REPRODUCE_PR = 0.05_SRP

  !> Upper limit on the number of reproducing individuals in the
  !! fixed-fitness pre-evolution phase.
  integer, parameter, public :: GA_REPRODUCE_N = int(POPSIZE * GA_REPRODUCE_PR)

  !> Fitness value ascribed to dead agent in pre-evol. See
  !! the_individual::individual_agent::fitness_calc().
  integer, parameter, public :: GA_FITNESS_DEAD = 2000

  !> Fitness threshold for the inclusion of the agent into the reproducing
  !! elite group
  integer, parameter, public :: GA_FITNESS_SELECT = 900

  !> Minimum proportion of reproducing agents, but note that the number of
  !! number reproducers cannot be smaller than the absolute minimum
  !! commondata::ga_reproduce_n_min.
  !! See the_population::population::ga_reproduce_max().
  integer, parameter, public :: GA_REPRODUCE_MIN_PROP = 0.005

  !> Absolute minimum number of reproducing agents in the adaptive GA procedure.
  !! See the_population::population::ga_reproduce_max().
  integer, parameter, public :: GA_REPRODUCE_N_MIN = 20

  !> @}

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  ! ## Generic interfaces for global COMMONDATA procedures ##
  ! Generic interfaces for the general-scope functions go below.
  ! Generic interfaces allow to use a general name so that the program
  ! selects the appropriate specific function from the list depending
  ! on the function parameters. For example, we have different `gamma2gene`
  ! functions under the same name, e.g. accepting different type
  ! arguments and processing them with different logic.

  !> @brief   Sigmoidal relationship between environmental factor and the
  !!          organism response, as affected by the genotype and environmental
  !!          error, e.g. perception and neuronal response or intrinsic baseline
  !!          and phenotypic hormone levels.
  !! @details The real function `gamma2gene` finds the sigmoid relationship for
  !!          a multicomponent allele impact on the neuronal response:
  !!          @f[ R= \frac{(P/y_{1})^{x_{1}}}{1+(P/y_{1})^{x_{1}}} +
  !!                 \frac{(P/y_{2})^{x_{2}}}{1+(P/y_{2})^{x_{2}}} +
  !!                 \frac{(P/y_{3})^{x_{3}}}{1+(P/y_{3})^{x_{3}}} ... @f]
  !!          Here, R is the neuronal response, P the strength of the sensory
  !!          input (scaled 0-1), and x and y are two genes. The indices refer
  !!          to the additive components of the alleles. Note that their
  !!          number is set by the parameter `ADDITIVE_COMPS`. Further,
  !!          `erpcv` defines the coefficient of variation for the perception
  !!          error (with respect to its true value).
  !!          *Perception* is calculated as @f[ P=\bar{P}+\varepsilon , @f]
  !!          where @f$ \bar{P} @f$ is the true environmental variable and
  !!          @f$ \varepsilon @f$ is Gaussian error. The perception value with
  !!          error is implemented as a normal Gaussian variate with the mean
  !!          equal to the true `signal` value @f$ \bar{P} @f$ and the
  !!          coefficient of variation equal to the `erpcv` input parameter:
  !!          @f$ erpcv= \frac{\sigma}{\bar{P}} . @f$ Therefore, the raw
  !!          error variance in the `RNORM` function is equal to the square:
  !!          @f$ (erpcv \cdot signal)^{2} @f$. We also impose strict *limit*
  !!          on perception: @f[ P > 0 . @f]
  !!          The `gamma2gene` function is used as a calculation backend in the
  !!          "umbrella" procedure `the_genome::individual_genome::neuro_resp()`
  !!          that translates to `the_genome::individual_genome::trait_init()`
  !!          and `the_genome::individual_genome::trait_set()`.
  !! @note   This is the generic interface for `gamma2gene`, currently there
  !!         are two version making use of the additive allele
  !!         components (normally used) accepting allele values either integer
  !!         (assumed raw alleles) or real (assumed values rescaled to 0:1)
  !!         type.
  !! @note   The function involving additive allele components has two variants:
  !!         If called with *integer* parameters one and two (`gs` and `gh`) it
  !!         automatically invokes the `allelescale` function to convert integer
  !!         allele values to real raw values within the range 1:0 that go to
  !!         the sigmoid function (via alleleconv). However, when these
  !!         arguments are *real* type, it is assumed that it is the true
  !!         converted 0:1 real gene values and `allelescale` is *not* invoked
  !!         prior to `alleleconv`.
  !! @note   It would be more economical to use a single allele conversion
  !!         function that does both allelescale and alleleconv. It is here
  !!         for compatibility with the earlier model. Also, this might be
  !!         a little more intuitive.
  interface gamma2gene
    module procedure gamma2gene_additive_i4
    module procedure gamma2gene_additive_r4
    module procedure gamma2gene_fake_vals
  end interface gamma2gene
  ! @note Note that all component procedures are private.
  private :: gamma2gene_additive_i4,gamma2gene_additive_r4,gamma2gene_fake_vals


  interface gene2gamma
    module procedure gamma2gene_reverse
  end interface gene2gamma
  ! @note Note that all component procedures are private.
  private :: gamma2gene_reverse, gene2gamma

  !-----------------------------------------------------------------------------
  !> Simple history stack function, add to the end of the stack. We need
  !! only to add components on top (end) of the stack and retain
  !! `HISTORY_SIZE_SPATIAL` elements of the prior history (for a spatial moving
  !! object). The stack works as follows, assuming 100 and 200 are added:
  !! @n                 [1 2 3 4 5 6 7  8   9  10]
  !! @n                 [2 3 4 5 6 7 8  9  10 **100**]
  !! @n                 [3 4 5 6 7 8 9 10 100 **200**]
  interface add_to_history
    module procedure add_to_history_i4
    module procedure add_to_history_r
    module procedure add_to_history_char
  end interface add_to_history
  ! @note Note that all component procedures are private.
  private :: add_to_history_i4, add_to_history_r, add_to_history_char

  !-----------------------------------------------------------------------------
  !> @brief  Convert cm to m.
  !! @details This is needed because some of the the sizes are expressed
  !!          in cm but certain functions (e.g. visual range estimator SRGETR)
  !!          require parameters in m. E.g. FOOD_ITEM_SIZE_DEFAULT is set
  !!          around 0.5 cm while SRGETR requires prey area in m².
  interface cm2m
    module procedure cm2m_r
    module procedure cm2m_hr
    module procedure cm2m_i
  end interface cm2m
  ! @note Note that all component procedures are private.
  private :: cm2m_r, cm2m_hr, cm2m_i

  !> @brief   Convert m to cm.
  !! @details This is needed because some of the the sizes are expressed
  !!          in cm but certain functions (e.g. visual range estimator SRGETR)
  !!          require parameters in m. Therefore, conversion back from visual
  !!          range function should use these functions.
  interface m2cm
    module procedure m2cm_r
    module procedure m2cm_hr
    module procedure m2cm_i
  end interface m2cm
  ! @note Note that all component procedures are private.
  private :: m2cm_r, m2cm_hr, m2cm_i

  !> @brief  Convert mm to m.
  !! @note   This is a similar functions(s) as above sm2m, but converting mm.
  interface mm2m
    module procedure mm2m_r
    module procedure mm2m_i
  end interface mm2m
  ! @note Note that all component procedures are private.
  private :: mm2m_r, mm2m_i

  !> @brief   Arbitrary rescales value(s) from one range (A:B) to
  !!          another (A1:B1).
  !! @details Rescales values from A:B to A1:B1, or (if only A1:B1 are provided)
  !!          from 0:1 to A1:B1.
  !! @note    Elemental functions.
  interface rescale
    module procedure rescale_1
    module procedure rescale_full
  end interface rescale
  ! @note Note that all component procedures are private.
  private :: rescale_1, rescale_full

  !> Force a value within the range set by the vmin and vmax dummy parameter
  !! values.
  interface within
    module procedure within_i
    module procedure within_r
  end interface within
  ! @note Note that all component procedures are private.
  private :: within_i, within_r

  !> Logical function to check if a value is within a specific range,
  !! **lower <= X <= upper**.
  !! @note See commondata::is_within_operator_r() and
  !!       commondata::is_within_operator_i() for a related user-defined
  !!       operator `.within.`.
  interface is_within
    module procedure is_within_r
    module procedure is_within_i
    ! @note Note that intel fortran does not allow the same procedures
    !       to be placed both in generic interface and user defined operator.
    !       That issues this compiler error:
    !           The procedure name of the INTERFACE block conflicts with a
    !           name in the encompassing scoping unit.
    !       Therefore, references to is_within_operator_r and
    !       is_within_operator_i are disabled here.
    !module procedure is_within_operator_r
    !module procedure is_within_operator_i
  end interface is_within
  ! @note Note that all component procedures are private.
  private :: is_within_r, is_within_i

  !> Checks if a real number is near 0.0.
  !! Thus function can be used for comparing two real values like this:
  !! @code
  !!   if ( is_near_zero(a) ) then ...
  !! @endcode
  !! See the backend procedures commondata::is_near_zero_srp() and
  !! commondata::is_near_zero_hrp() for details.
  interface is_near_zero
    module procedure is_near_zero_srp
    module procedure is_near_zero_hrp
  end interface is_near_zero
  ! @note Note that all component procedures are private.
  private :: is_near_zero_srp, is_near_zero_hrp

  !> Check if two real values are nearly equal using the
  !! commondata::is_near_zero().
  !! Thus function can be used for comparing two real values like this:
  !! - The exact float comparison (incorrect due to possible rounding):
  !!   @code
  !!     if ( a == b ) ...
  !!   @endcode
  !! - should be substituted by such comparison:
  !!   @code
  !!     if ( float_equal(a, b) ) ...
  !!   @endcode
  !! .
  !! See the backend procedures commondata::float_equal_srp() and
  !! commondata::float_equal_hrp() for details.
  interface float_equal
    module procedure float_equal_srp
    module procedure float_equal_hrp
  end interface float_equal
  ! @note Note that all component procedures are private.
  private :: float_equal_srp, float_equal_hrp

  !> "Float equality" operator: Check if two real values are *nearly equal*
  !! using the commondata::is_near_zero() function. Thus function can be used
  !! for comparing two real values like the below.
  !! - The exact real comparison (incorrect due to possible rounding):
  !!   @code
  !!     if ( a == b ) ...
  !!   @endcode
  !! - should be substituted by such comparison:
  !!   @code
  !!     if ( a .feq. b) ...
  !!   @endcode
  !! .
  !! See the backend procedures commondata::float_equal_srp_operator() and
  !! commondata::float_equal_hrp_operator() for details and
  !! @ref intro_computation_real "Float point" for an introduction.
  interface operator (.feq.)
    procedure float_equal_srp_operator
    procedure float_equal_hrp_operator
  end interface operator (.feq.)
  ! @note Note that all component procedures are private.
  private :: float_equal_srp_operator, float_equal_hrp_operator

  !> "Approximatel equality" operator: Check if two real values are
  !! *approximately equal* using the commondata::is_near_zero() function.
  !! Thus function can be used for comparing two real values like the below.
  !! - The exact real comparison (incorrect due to possible rounding):
  !!   @code
  !!     if ( a == b ) ...
  !!   @endcode
  !! - should be substituted by such *approximately equal* comparison:
  !!   @code
  !!     if ( a .approx. b) ...
  !!   @endcode
  !! .
  !! See the backend procedures commondata::float_approx_srp_operator() and
  !! commondata::float_approx_hrp_operator() for details and
  !! @ref intro_computation_real "Float point" for an introduction.
  !! @note This `.approx.` operator differs from the `.feq.` by a very high
  !!       *epsilon* tolerance, that is, a much higher error
  !!       (commondata::zero * 1000.0) is tolerated.
  interface operator (.approx.)
    procedure float_approx_srp_operator
    procedure float_approx_hrp_operator
  end interface operator (.approx.)
  ! @note Note that all component procedures are private.
  private :: float_approx_srp_operator, float_approx_hrp_operator

  !> Interface operators `.within.` for testing whether a value (first
  !! argument) lies within the limits set by a two-element array (second
  !! argument). All the values/parameters are Fortran intrinsic types, real
  !! or integer. Usage of the operator:
  !! @code
  !!   if ( value .within. [lower, upper] ) then
  !! @endcode
  !! See `commondata::is_within_operator_r()` and
  !! `commondata::is_within_operator_i()` for more details.
  interface operator (.within.)
    procedure is_within_operator_r
    procedure is_within_operator_i
  end interface operator (.within.)
  ! @note Note that all component procedures are private.
  private :: is_within_operator_r, is_within_operator_i

  !> Concatenate two arrays a and b. This procedure uses array slices which
  !! would be faster in most cases than the intrinsic `[a,b]` method.
  !! @code
  !!   a .cat. b ! equivalent to [a, b]
  !! @endcode
  !! See commondata::stack2arrays_r() and commondata::stack2arrays_i() for
  !! details.
  interface operator (.cat.)
    procedure stack2arrays_r
    procedure stack2arrays_i
  end interface operator (.cat.)
  ! @note Note that all component procedures are private.
  private :: stack2arrays_r, stack2arrays_i

  !> Calculate an average of an array excluding missing code values.
  interface average
    module procedure average_r
    module procedure average_i
  end interface average
  ! @note Note that all component procedures are private.
  private :: average_r, average_i

  !> Check if a value is the maximum value of an array.
  interface is_maxval
    module procedure is_maxval_r
    module procedure is_maxval_i
  end interface is_maxval
  ! @note Note that all component procedures are private.
  private :: is_maxval_r, is_maxval_i

  !> Check if a value is the minimum value of an array.
  interface is_minval
    module procedure is_minval_r
    module procedure is_minval_i
  end interface is_minval
  ! @note Note that all component procedures are private.
  private :: is_minval_r, is_minval_i

  !> Interface operator `.radd.` performs a random addition or subtraction
  !! of two numbers with equal probability.
  !! See `commondata::random_add_subtract()`.
  !! The operator can be used as follows:
  !! @code
  !!    temp_hab = a .radd. b
  !! @endcode
  !! @note Used in the correlated random Gaussian walk routines
  !!       the_environment::corwalk()
  interface operator (.radd.)
    procedure random_add_subtract
  end interface operator (.radd.)
! @note Note that all component procedures are private.
  private :: random_add_subtract

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! These are public access functions, but probably we don't need to allow
  !   public access to functions inside generic interfaces
  ! public  :: alleleconv, allelescale, gamma2gene, gene2gamma,               &
  !            LOG_DBG
  ! We do not need specific functions outside of this module, always use
  ! generic functions.
  private :: logger_init, parse_svn_version, tag_mmdd

contains !---- Various globally-used accessory subroutines and functions here --

  !-----------------------------------------------------------------------------
  !> @brief   Convert cm to m.
  !! @param   value_cm value in cm
  !! @returns value in m
  !! @note    This version gets real type argument.
  !! @details This is needed because some of the the sizes are expressed
  !!          in cm but certain functions (e.g. visual range estimator SRGETR)
  !!          require parameters in m. E.g. FOOD_ITEM_SIZE_DEFAULT is set
  !!          around 0.5 cm while SRGETR requires prey area in m².
  elemental function cm2m_r(value_cm) result(value_m)
    real(SRP), intent(in) :: value_cm           ! @param value_cm value in cm
    real(SRP) :: value_m                        ! @returns value in m
    real(SRP), parameter :: CONV_F = 0.01_SRP   ! conversion factor

    value_m = value_cm * CONV_F

  end function cm2m_r

  !-----------------------------------------------------------------------------
  !> @brief  Convert cm to m.
  !! @param value_cm value in cm
  !! @returns value in m
  !! @note   This version uses the **high** `HRP` numerical precision model.
  elemental function cm2m_hr(value_cm) result(value_m)
    real(HRP), intent(in) :: value_cm           ! @param value_cm value in cm
    real(HRP) :: value_m                        ! @returns value in m
    real(HRP), parameter :: CONV_F = 0.01_HRP   ! conversion factor

    value_m = value_cm * CONV_F

  end function cm2m_hr

  !-----------------------------------------------------------------------------
  !> @brief  Convert cm to m.
  !! @returns value in m
  !! @param value_cm value in cm
  !! @note   This version gets integer argument (albeit returns real).
  elemental function cm2m_i(value_cm) result(value_m)
    integer, intent(in) :: value_cm             ! @param value_cm value in cm
    real(SRP) :: value_m                        ! @returns value in m
    real(SRP), parameter :: CONV_F = 0.01_SRP   ! conversion factor

    value_m = real(value_cm, SRP) * CONV_F

  end function cm2m_i

  !-----------------------------------------------------------------------------
  !> @brief  Convert m to cm.
  !! @param value_cm value in cm
  !! @returns value in m
  !! @note   This version gets real type argument.
  elemental function m2cm_r(value_m) result(value_cm)
    real(SRP), intent(in) :: value_m             ! @param value_cm value in cm
    real(SRP) :: value_cm                        ! @returns value in m
    real(SRP), parameter :: CONV_F = 100.0_SRP   ! conversion factor

    value_cm = value_m * CONV_F

  end function m2cm_r

  !-----------------------------------------------------------------------------
  !> @brief  Convert m to cm.
  !! @returns value in m
  !! @param value_cm value in cm
  !! @note   This version uses the **high** `HRP` numerical precision model.
  elemental function m2cm_hr(value_m) result(value_cm)
    real(HRP), intent(in) :: value_m             ! @param value_cm value in cm
    real(HRP) :: value_cm                        ! @returns value in m
    real(HRP), parameter :: CONV_F = 100.0_HRP   ! conversion factor

    value_cm = value_m * CONV_F

  end function m2cm_hr

  !-----------------------------------------------------------------------------
  !> @brief  Convert m to cm.
  !! @param value_cm value in cm
  !! @returns value in m
  !! @note   This version gets integer argument (albeit returns real).
  elemental function m2cm_i(value_m) result(value_cm)
    integer, intent(in) :: value_m               ! @param value_cm value in cm
    real(SRP) :: value_cm                        ! @returns value in m
    real(SRP), parameter :: CONV_F = 100.0_SRP   ! conversion factor

    value_cm = real(value_m, SRP) * CONV_F

  end function m2cm_i

  !-----------------------------------------------------------------------------
  !> @brief  Convert mm to m.
  !! @param value_cm value in cm
  !! @returns value in m
  !! @note   This version gets real type argument.
  elemental function mm2m_r(value_mm) result(value_m)
    real(SRP), intent(in) :: value_mm           ! @param value_cm value in cm
    real(SRP) :: value_m                        ! @returns value in m
    real(SRP), parameter :: CONV_F = 0.001_SRP  ! conversion factor

    value_m = value_mm * CONV_F

  end function mm2m_r

  !-----------------------------------------------------------------------------
  !> @brief  Convert mm to m.
  !! @param value_cm value in cm
  !! @returns value in m
  !! @note   This version gets integer argument (albeit returns real).
  elemental function mm2m_i(value_mm) result(value_m)
    integer, intent(in) :: value_mm             ! @param value_cm value in cm
    real(SRP) :: value_m                        ! @returns value in m
    real(SRP), parameter :: CONV_F = 0.001_SRP  ! conversion factor

    value_m = real(value_mm, SRP) * CONV_F

  end function mm2m_i

  !-----------------------------------------------------------------------------
  !> @brief   Calculate a circle area.
  !! @details Calculate the area of the circle.
  !! @param   R circle radius.
  !! @note    Food items are viewed as circular objects with the default
  !!          **radius** equal to the **size** `FOOD_ITEM_SIZE_DEFAULT` or
  !!          `FOOD_ITEM_MEAN_SIZE`.
  elemental function carea(R) result (area_circ)
    real(SRP), intent(in) :: R
    real(SRP) :: area_circ

    area_circ = PI * R * R

  end function carea

  !-----------------------------------------------------------------------------
  !> @brief  A function linking **body length** with the body **area** in fish.
  !! @details For fish, based on the paper by O'Shea et al., 2006,
  !!          DOI: 10.1111/j.1365-2761.2006.00728.x
  !!          Approximate formula for the the **whole surface area** is
  !!          @f[ S = 0.7 \cdot L^{1.9} . @f] Because the side area is 1/2
  !!          of the total surface area (0.7 / 2 = 0.35), the function takes
  !!          this half. So the final formula is:
  !!          @f[ S = 0.35 \cdot L ^{1.8} . @f]
  elemental function length2sidearea_fish (body_length) result (side_area)
    real(SRP), intent(in) :: body_length
    real(SRP) :: side_area

    side_area = 0.35_SRP * body_length ** 1.8_SRP

  end function length2sidearea_fish

  !-----------------------------------------------------------------------------
  !> @brief    Rescale a real variable with the range A:B to have the new
  !!           range A1:B1.
  !! @details  Linear transformation of the input value `value_in` such
  !!           `k * value_in + beta`, where the `k` and `beta` coefficients
  !!           are found by solving a simple linear system:
  !!           @f$  \left\{\begin{matrix}
  !!                A_{1}= k \cdot A + \beta;   \\
  !!                B_{1}= k \cdot B + \beta
  !!                \end{matrix}\right. @f$. It has this solution:
  !!           @f$ k=\frac{A_{1}-B_{1}}{A-B},
  !!               \beta=-\frac{A_{1} \cdot B-A \cdot B_{1}}{A-B} @f$
  !! @warning  The function does not check if `value_in` lies within [A:B].
  !! @note     Code for wxMaxima equation solve:
  !! @code
  !!           solve(  [A1=A*k+b, B1=B*k+b] ,[k,b] );
  !! @endcode
  elemental function rescale_full(value_in, A, B, A1, B1) result(rescaled)
    real(SRP), intent(in) :: value_in
    real(SRP), intent(in) :: A, B, A1, B1
    real(SRP) :: rescaled

    ! Local variables
    real(SRP) :: ck, cb

    !> ### Implementation details ###

    !> First, find the linear coefficients `ck` and `cb`
    !! from the simple linear system.
    ck = (A1-B1) / (A-B)
    cb = -1.0_SRP * ((A1*B - A*B1) / (A-B))

    !> Second, do the actual linear rescale of the input value.
    rescaled = value_in*ck + cb

  end function rescale_full

  !-----------------------------------------------------------------------------
  !> @brief   Rescale a real variable with the range 0:1 to have the new
  !!          range A1:B1.
  !! @warning The function does not check if `value_in` lies within [0:1].
  !! @note    Code for wxMaxima equation solve:
  !! @code
  !!          solve(  [A1=0*k+b, B1=1*k+b] ,[k,b] );
  !! @endcode
  elemental function rescale_1(value_in, A1, B1) result(rescaled)
    real(SRP), intent(in) :: value_in
    real(SRP), intent(in) :: A1, B1
    real(SRP) :: rescaled

    ! Local variables
    real(SRP) :: ck, cb

    !> ### Implementation details ###

    !> First, find the linear coefficients `ck` and `cb`
    !! from the simple linear system.
    ck = B1 - A1
    cb = A1

    !> Second, do the actual linear rescale of the input value.
    rescaled = value_in*ck + cb

  end function rescale_1

  !-----------------------------------------------------------------------------
  !> Force a value within the range set by the vmin and vmax dummy parameter
  !! values. If the value is within the range, it does not change, if it
  !! falls outside, the output force value is obtained as
  !! min( max( value, FORCE_MIN ), FORCE_MAX )
  !! @param[in] value_in Input value for forcing transformation.
  !! @param[in] vmin minimum value of the force-to range (lower limit), if
  !!            not present, a lower limit of 0.0 is used.
  !! @param[in] vmax maximum value of the force-to range (upper limit)
  !! @returns   an input value forced to the range.
  !! @note      Note that this is the **real** precision version of the
  !!            generic `within` function.
  elemental function within_r(value_in, vmin, vmax) result (value_out)
    real(SRP), intent(in) :: value_in
    real(SRP), optional, intent(in) :: vmin
    real(SRP), intent(in) :: vmax
    real(SRP) :: value_out

    ! Local copies of optionals.
    real(SRP) :: vmin_here

    ! Check optional minimum value, if absent, set to a default value 0.0.
    if (present(vmin)) then
      vmin_here =  vmin
    else
      vmin_here = 0.0_SRP
    end if

    value_out = min( max( value_in, vmin_here ), vmax )

  end function within_r

  !-----------------------------------------------------------------------------
  !> Force a value within the range set by the vmin and vmax dummy parameter
  !! values. If the value is within the range, it does not change, if it
  !! falls outside, the output force value is obtained as
  !! min( max( value, FORCE_MIN ), FORCE_MAX )
  !! @param[in] value_in Input value for forcing transformation.
  !! @param[in] vmin minimum value of the force-to range (lower limit), if
  !!            not present, a lower limit of 0.0 is used.
  !! @param[in] vmax maximum value of the force-to range (upper limit)
  !! @returns   an input value forced to the range.
  !! @note      Note that this is the **integer** version of the generic
  !!            `within` function.
  elemental function within_i(value_in, vmin, vmax) result (value_out)
    integer, intent(in) :: value_in
    integer, optional, intent(in) :: vmin
    integer, intent(in) :: vmax
    integer :: value_out

    ! Local copies of optionals.
    integer :: vmin_here

    ! Check optional minimum value, if absent, set to a default value 0.0.
    if (present(vmin)) then
      vmin_here =  vmin
    else
      vmin_here = 0.0_SRP
    end if

    ! Calculate the force-limit output value.
    value_out = min( max( value_in, vmin_here ), vmax )

  end function within_i

  !-----------------------------------------------------------------------------
  !> Logical function to check if a value is within a specific range,
  !! **lower <= X <= upper**. The reverse (upper <= x <= lower) range limits
  !! can also be used; a corrective adjustment is automatically made.
  !! @note This is the real type version.
  !! @note See commondata::is_within_operator_r() and
  !!       commondata::is_within_operator_i() for a related user-defined
  !!       operator `.within.`.
  !! @param[in] x the value to test
  !! @param[in] lower the lower limit for the range tested.
  !! @param[in] upper the upper limit of the range tested.
  !! @return Returns TRUE if `x` lies within [lower,upper] and FALSE otherwise.
  elemental function is_within_r (x, lower, upper) result (within_yes)
    real(SRP), intent(in) :: x, lower, upper
    logical :: within_yes

    ! Local copies of swappable variables.
    real(SRP) :: lower_loc, upper_loc

    !> First, make sure the lower bound is actually smaller than the upper
    !! bound. If not, swapped bounds are used to set the range: [upper,lower].
    if (lower < upper) then
      lower_loc = lower
      upper_loc = upper
    else
      lower_loc = upper
      upper_loc = lower
    end if

    if ( x >= lower_loc .and. x <= upper_loc ) then
      within_yes = .TRUE.
    else
      within_yes = .FALSE.
    end if

  end function is_within_r

  !-----------------------------------------------------------------------------
  !> Logical function to check if a value is within a specific range,
  !! **lower <= X <= upper**. The reverse (upper <= x <= lower) range limits
  !! can also be used; a corrective adjustment is automatically made.
  !! @note This is the integer type version.
  !! @note See commondata::is_within_operator_r() and
  !!       commondata::is_within_operator_i() for a related user-defined
  !!       operator `.within.`.
  !! @param[in] x the value to test
  !! @param[in] lower the lower limit for the range tested.
  !! @param[in] upper the upper limit of the range tested.
  !! @return Returns TRUE if `x` lies within [lower,upper] and FALSE otherwise.
  elemental function is_within_i (x, lower, upper) result (within_yes)
    integer, intent(in) :: x, lower, upper
    logical :: within_yes

    ! Local copies of swappable variables.
    integer :: lower_loc, upper_loc

    !> First, make sure the lower bound is actually smaller than the upper
    !! bound. If not, swapped bounds are used to set the range: [upper,lower].
    if (lower < upper) then
      lower_loc = lower
      upper_loc = upper
    else
      lower_loc = upper
      upper_loc = lower
    end if

    if ( x >= lower_loc .and. x <= upper_loc ) then
      within_yes = .TRUE.
    else
      within_yes = .FALSE.
    end if

  end function is_within_i

  !-----------------------------------------------------------------------------
  !> A wrapper function for commondata::is_within() to build a user defined
  !! operator. Basically, it is the same as `is_within`, but the lower and
  !! upper limits are set as a two-element array.
  !! Usage of the operator:
  !! @code
  !!   if ( value .within. [lower, upper] ) then
  !! @endcode
  !! @note This is real type version of the procedure.
  pure function is_within_operator_r(x, limits) result (return_yesno)
    !> @param[in] x the value to test
    real(SRP), intent(in) :: x
    !> @param[in] limits an array of two elements that sets the lower and
    !!            upper limits for the range tested.
    real(SRP), dimension(2), intent(in) :: limits
    !> @return Returns TRUE if `x` lies within [lower,upper] and FALSE
    !!         otherwise.
    logical :: return_yesno

    !> **The implementation** just calls the real type function
    !! commondata::is_within_r().
    return_yesno = is_within_r(x, limits(1), limits(2))

  end function is_within_operator_r

  !-----------------------------------------------------------------------------
  !> A wrapper function for commondata::is_within() to build a user defined
  !! operator. Basically, it is the same as `is_within`, but the lower and
  !! upper limits are set as a two-element array.
  !! Usage of the operator:
  !! @code
  !!   if ( value .within. [lower, upper] ) then
  !! @endcode
  !! @note This is an integer type version of the procedure.
  pure function is_within_operator_i(x, limits) result (return_yesno)
    !> @param[in] x the value to test
    integer, intent(in) :: x
    !> @param[in] limits an array of two elements that sets the lower and
    !!            upper limits for the range tested.
    integer, dimension(2), intent(in) :: limits
    !> @return Returns TRUE if `x` lies within [lower,upper] and FALSE
    !!         otherwise.
    logical :: return_yesno

    !> **The implementation** just calls the integer type function
    !! commondata::is_within_i().
    return_yesno = is_within_i(x, limits(1), limits(2))

  end function is_within_operator_i

  !-----------------------------------------------------------------------------
  !> Calculate an average value of a real array, excluding MISSING values.
  !! @param vector_in The input data vector
  !! @param missing_code Optional parameter setting the missing data code,
  !!        to be excluded from the calculation of the mean.
  !! @param undef_ret_null Optional parameter, if TRUE, the function returns
  !!        zero rather than undefined if the sample size is zero.
  !! @returns The mean value of the vector.
  !! @note This is a real array version.
  pure function average_r (array_in, missing_code, undef_ret_null)            &
                                                              result (mean_val)

    ! @param vector_in The input data vector
    real(SRP), dimension(:), intent(in) :: array_in

    ! @param missing_code Optional parameter setting the missing data code,
    !        to be excluded from the calculation of the mean.
    real(SRP), optional, intent(in) :: missing_code

    ! @param undef_ret_null Optional parameter, if TRUE, the function returns
    ! zero rather than undefined if the sample size is zero.
    logical, optional, intent(in) :: undef_ret_null

    ! @returns The mean value of the vector.
    real(SRP) :: mean_val

    ! Local missing code.
    real(SRP) :: missing_code_here

    ! Local sample size, N of valid values.
    integer :: count_valid

    !> ### Implementation details ###

    !> Check if missing data code is provided from dummy input.
    !! If not, use global parameter.
    if (present(missing_code)) then
      missing_code_here = missing_code
    else
      missing_code_here = MISSING
    end if

    !> Fist, count how many valid values are there in the array.
    count_valid = count(array_in /= missing_code_here)

    !> If there are no valid values in the array, mean is undefined.
    if (count_valid==0) then
      if (present(undef_ret_null)) then
        if (undef_ret_null) then
          mean_val = 0.0_SRP    !> still return zero if undef_ret_null is TRUE.
        else
          mean_val = MISSING
        end if
      else
        mean_val = MISSING
      end if
      return
    end if

    mean_val = sum(array_in, array_in /= missing_code_here) / count_valid

  end function average_r

  !-----------------------------------------------------------------------------
  !> Calculate an average value of an integer array, excluding MISSING values.
  !! @returns The mean value of the vector
  !! @param vector_in The input data vector
  !! @param missing_code Optional parameter setting the missing data code,
  !!        to be excluded from the calculation of the mean.
  !! @param undef_ret_null Optional parameter, if TRUE, the function returns
  !!        zero rather than undefined if the sample size is zero.
  !! @note This is an integer array version.
  pure function average_i (array_in, missing_code, undef_ret_null)            &
                                                              result (mean_val)

    ! @param vector_in The input data vector
    integer, dimension(:), intent(in) :: array_in

    ! @param missing_code Optional parameter setting the missing data code,
    !        to be excluded from the calculation of the mean.
    integer, optional, intent(in) :: missing_code

    ! @param undef_ret_null Optional parameter, if TRUE, the function returns
    !        zero rather than undefined if the sample size is zero.
    logical, optional, intent(in) :: undef_ret_null

    ! @returns The mean value of the vector
    real(SRP) :: mean_val

    ! Local missing code
    integer :: missing_code_here

    ! Local sample size, N of valid values.
    integer :: count_valid

    !> ### Implementation details ###

    !> Check if missing data code is provided from dummy input.
    !! If not, use global parameter.
    if (present(missing_code)) then
      missing_code_here = missing_code
    else
      missing_code_here = UNKNOWN
    end if

    !> Fist, count how many valid values are there in the array.
    count_valid = count(array_in /= missing_code_here)

    !> If there are no valid values in the array, mean is undefined.
    if (count_valid==0) then
      if (present(undef_ret_null)) then
        if (undef_ret_null) then
          mean_val = 0.0_SRP    !> still return zero if undef_ret_null is TRUE.
        else
          mean_val = MISSING
        end if
      else
        mean_val = MISSING
      end if
      return
    end if

    mean_val = real(sum(array_in, array_in /= missing_code_here), SRP) /      &
               real(count_valid, SRP)

  end function average_i

  !-----------------------------------------------------------------------------
  !> Calculate standard deviation using trivial formula:
  !! @f[ \sigma=\sqrt{\frac{\sum (x-\overline{x})^{2}}{N-1}} . @f]
  !! @note This is a real array version.
  function std_dev(array_in, missing_code, undef_ret_null) result (stddev)
    !> @param vector_in The input data vector
    real(SRP), dimension(:), intent(in) :: array_in
    !> @param missing_code Optional parameter setting the missing data code,
    !!        to be excluded from the calculation of the mean.
    real(SRP), optional, intent(in) :: missing_code
    !> @param undef_ret_null Optional parameter, if TRUE, the function returns
    !! zero rather than undefined if the sample size is zero.
    logical, optional, intent(in) :: undef_ret_null
    !> @returns The standard deviation of the data vector.
    real(SRP) :: stddev

    ! Local missing code.
    real(SRP) :: missing_code_here

    ! Local sample size, N of valid values.
    integer :: count_valid

    ! Minimum sample size resulting in real calculation, everythin less
    ! than this returns invalid.
    integer, parameter :: MIN_N = 4

    ! An array of squared deviations
    real(SRP), dimension(size(array_in)) :: dev2

    ! Mean value
    real(SRP) :: sample_mean

    !> Check if missing data code is provided from dummy input.
    !! If not, use global parameter.
    if (present(missing_code)) then
      missing_code_here = missing_code
    else
      missing_code_here = MISSING
    end if

    count_valid = count(array_in /= missing_code_here)

    !> If there are no valid values in the array, std. dev. is undefined.
    if (count_valid <  MIN_N) then
      if (present(undef_ret_null)) then
        if (undef_ret_null) then
          stddev = 0.0_SRP    !> still return zero if undef_ret_null is TRUE.
        else
          stddev = MISSING
        end if
      else
        stddev = MISSING
      end if
      return
    end if

    sample_mean = average ( array_in, missing_code_here )

    where ( array_in /= missing_code_here )
      dev2 = ( array_in - sample_mean ) ** 2
    elsewhere
      dev2 = missing_code_here
    end where

    stddev = sqrt( sum( dev2, dev2 /= missing_code_here ) / (count_valid - 1) )

  end function std_dev

  !-----------------------------------------------------------------------------
  !> Concatenate two arrays a and b. This procedure uses array slices which
  !! would be faster in most cases than the intrinsic `[a,b]` method.
  !> @note This is the **real** type version.
  pure function stack2arrays_r(a, b) result (c)
    !> param[in] a first array
    real(SRP), intent(in), dimension(:) :: a
    !> param[in] b second array
    real(SRP), intent(in), dimension(:) :: b
    !> return an array [a, b]
    real(SRP), dimension(:), allocatable :: c

    allocate(c(size(a)+size(b)))
    c(1:size(a)) = a
    c(size(a)+1:size(a)+size(b)) = b
    ! The easier method using intrinsic array joining [a, b] is probably slower.

  end function stack2arrays_r

  !-----------------------------------------------------------------------------
  !> Concatenate two arrays a and b. This procedure uses array slices which
  !! would be faster in most cases than the intrinsic `[a,b]` method.
  !> @note This is the **real** type version.
  pure function stack2arrays_i(a, b) result (c)
    !> param[in] a first array
    integer, intent(in), dimension(:) :: a
    !> param[in] b second array
    integer, intent(in), dimension(:) :: b
    !> return an array [a, b]
    integer, dimension(:), allocatable :: c

    allocate(c(size(a)+size(b)))
    c(1:size(a)) = a
    c(size(a)+1:size(a)+size(b)) = b
    ! The easier method using intrinsic array joining [a, b] is probably slower.

  end function stack2arrays_i


  !-----------------------------------------------------------------------------
  !> Checks if a real number is near 0.0.
  !! Thus function can be used for comparing two real values like the below.
  !! @code
  !!   if ( is_near_zero(a) ) then ...
  !! @endcode
  !! @note Note that commondata::float_equal() can be used for approximate real
  !!       comparisons of two real type values.
  !! @note Modified from `Near0_dp()` function from Clerman & Spector 2012,
  !!       p. 250-251.
  !! @note This is the **standard** precision function (commondata::srp).
  elemental function is_near_zero_srp(test_number, epsilon) result (ret_val)
    !> @param[in] test_number the number to check for being near-zero.
    real(SRP), intent (in) :: test_number
    !> @param[in] epsilon optional (very small) tolerance value.
    real(SRP), intent (in), optional :: epsilon
    !> @return TRUE if the `test_number` is near-zero.
    logical :: ret_val

    real(kind(epsilon)) :: local_epsilon
    local_epsilon = TOLERANCE_LOW_DEF_SRP

    if ( present(epsilon) ) then
      if ( abs(epsilon) >= TINY_SRP ) local_epsilon = abs(epsilon)
    end if

    ret_val = abs(test_number) < local_epsilon

  end function is_near_zero_srp

  !-----------------------------------------------------------------------------
  !> Checks if a real number is near 0.0.
  !! Thus function can be used for comparing two real values like the below.
  !! @code
  !!   if ( is_near_zero(a) ) then ...
  !! @endcode
  !! @note Note that commondata::float_equal() can be used for approximate real
  !!       comparisons of two real type values.
  !! @note Modified from `Near0_dp()` function from Clerman & Spector 2012,
  !!       p. 250-251.
  !! @note This is the **high** precision function (commondata::hrp).
  elemental function is_near_zero_hrp(test_number, epsilon) result (ret_val)
    !> @param[in] test_number the number to check for being near-zero.
    real(HRP), intent (in) :: test_number
    !> @param[in] epsilon optional (very small) tolerance value.
    real(HRP), intent (in), optional :: epsilon
    !> @return TRUE if the `test_number` is near-zero.
    logical :: ret_val

    real(kind(epsilon)) :: local_epsilon
    local_epsilon = TOLERANCE_LOW_DEF_HRP

    if ( present(epsilon) ) then
      if ( abs(epsilon) >= TINY_HRP ) local_epsilon = abs(epsilon)
    end if

    ret_val = abs(test_number) < local_epsilon

  end function is_near_zero_hrp

  !-----------------------------------------------------------------------------
  !> Check if two real values are nearly equal using the
  !! commondata::is_near_zero().
  !! Thus function can be used for comparing two real values like the below.
  !! The exact comparison (incorrect due to possible rounding):
  !! @code
  !!   if ( a == b ) ...
  !! @endcode
  !! should be substituted by such comparison:
  !! @code
  !!   if ( float_equal(a, b, 0.00001) ) ...
  !! @endcode
  !! There is also a user defined operator `.feq.` for approximate float point
  !! equality. It differs from this function in that the later allows to set
  !! an arbitrary epsilon tolerance value whereas the operator does not (it
  !! uses the default epsilon based on the intrinsic `tiny()` function.
  !! @note This is the **standard** precision function (commondata::srp).
  elemental function float_equal_srp(value1, value2, epsilon) result (equal)
    !> @param[in] value1 The first value for comparison.
    real(SRP), intent(in) :: value1
    !> @param[in] value2 The second value for comparison.
    real(SRP), intent(in) :: value2
    !> @param[in] epsilon optional (very small) tolerance value.
    real(SRP), intent (in), optional :: epsilon
    !> @return TRUE if the values `value1` and `value2` are nearly equal.
    logical :: equal

    real(kind(epsilon)) :: local_epsilon
    local_epsilon = TOLERANCE_LOW_DEF_SRP

    if ( present(epsilon) ) then
      if ( abs(epsilon) >= TINY_SRP ) local_epsilon = abs(epsilon)
    end if

    equal = is_near_zero( abs(value1 - value2), local_epsilon )

  end function float_equal_srp

  !-----------------------------------------------------------------------------
  !> Check if two real values are nearly equal using the
  !! commondata::is_near_zero().
  !! Thus function can be used for comparing two real values like the below.
  !! The exact comparison (incorrect due to possible rounding):
  !! @code
  !!   if ( a == b ) ...
  !! @endcode
  !! should be substituted by such comparison:
  !! @code
  !!   if ( float_equal(a, b, 0.00001) ) ...
  !! @endcode
  !! There is also a user defined operator `.feq.` for approximate float point
  !! equality. It differs from this function in that the later allows to set
  !! an arbitrary epsilon tolerance value whereas the operator does not (it
  !! uses the default epsilon based on the intrinsic `tiny()` function.
  !! @note This is the **high** precision function (commondata::hrp).
  elemental function float_equal_hrp(value1, value2, epsilon) result (equal)
    !> @param[in] value1 The first value for comparison.
    real(HRP), intent(in) :: value1
    !> @param[in] value2 The second value for comparison.
    real(HRP), intent(in) :: value2
    !> @param[in] epsilon optional (very small) tolerance value.
    real(HRP), intent (in), optional :: epsilon
    !> @return TRUE if the values `value1` and `value2` are nearly equal.
    logical :: equal

    real(kind(epsilon)) :: local_epsilon
    local_epsilon = TOLERANCE_LOW_DEF_HRP

    if ( present(epsilon) ) then
      if ( abs(epsilon) >= TINY_HRP ) local_epsilon = abs(epsilon)
    end if

    equal = is_near_zero( abs(value1 - value2), local_epsilon )

  end function float_equal_hrp

  !-----------------------------------------------------------------------------
  !> This is a wrapper for the commondata::float_equal_srp() for building
  !! the user defined operator `.feq.` with default tolerance
  !! (`epsilon` parameter).
  !! The exact real comparison (incorrect due to possible rounding):
  !! @code
  !!   if ( a == b ) ...
  !! @endcode
  !! should be substituted by such comparison:
  !! @code
  !!   if ( a .feq. b) ...
  !! @endcode
  !! @warning This function is not intended for direct use.
  elemental function float_equal_srp_operator(value1, value2) result (equal)
    !> @param[in] value1 The first value for comparison.
    real(SRP), intent(in) :: value1
    !> @param[in] value2 The second value for comparison.
    real(SRP), intent(in) :: value2
    !> @return TRUE if the values `value1` and `value2` are nearly equal.
    logical :: equal
    ! EPSILON_LOC sets the tolerance limit for the default .feq. operator.
    real(SRP), parameter :: EPSILON_LOC = TOLERANCE_LOW_DEF_SRP
    equal = is_near_zero( abs(value1 - value2), EPSILON_LOC )
  end function float_equal_srp_operator

  !-----------------------------------------------------------------------------
  !> This is a wrapper for the commondata::float_equal_hrp() for building
  !! the user defined operator `.feq.` with default tolerance
  !! (`epsilon` parameter).
  !! The exact real comparison (incorrect due to possible rounding):
  !! @code
  !!   if ( a == b ) ...
  !! @endcode
  !! should be substituted by such comparison:
  !! @code
  !!   if ( a .feq. b) ...
  !! @endcode
  !! @warning This function is not intended for direct use.
  elemental function float_equal_hrp_operator(value1, value2) result (equal)
    !> @param[in] value1 The first value for comparison.
    real(HRP), intent(in) :: value1
    !> @param[in] value2 The second value for comparison.
    real(HRP), intent(in) :: value2
    !> @return TRUE if the values `value1` and `value2` are nearly equal.
    logical :: equal
    ! EPSILON_LOC sets the tolerance limit for the default .feq. operator.
    real(HRP), parameter :: EPSILON_LOC = TOLERANCE_LOW_DEF_HRP
    equal = is_near_zero( abs(value1 - value2), EPSILON_LOC )
  end function float_equal_hrp_operator

  !-----------------------------------------------------------------------------
  !> This is a wrapper for the commondata::float_equal_srp() for building
  !! the user defined operator `.approx.` with **very high** tolerance
  !! (`epsilon` parameter).
  !! The exact real comparison (incorrect due to possible rounding):
  !! @code
  !!   if ( a == b ) ...
  !! @endcode
  !! should be substituted by such comparison:
  !! @code
  !!   if ( a .approx. b) ...
  !! @endcode
  !! @warning This function is not intended for direct use.
  elemental function float_approx_srp_operator(value1, value2) result (equal)
    !> @param[in] value1 The first value for comparison.
    real(SRP), intent(in) :: value1
    !> @param[in] value2 The second value for comparison.
    real(SRP), intent(in) :: value2
    !> @return TRUE if the values `value1` and `value2` are approximately equal.
    logical :: equal
    ! EPSILON_LOC sets the tolerance limit for the default `.approx.` operator.
    ! @note Note that tolerance value here is very high.
    real(SRP), parameter :: EPSILON_LOC = TOLERANCE_HIGH_DEF_SRP
    equal = is_near_zero( abs(value1 - value2), EPSILON_LOC )
  end function float_approx_srp_operator

  !-----------------------------------------------------------------------------
  !> This is a wrapper for the commondata::float_equal_hrp() for building
  !! the user defined operator `.approx.` with **very high** tolerance
  !! (`epsilon` parameter).
  !! The exact real comparison (incorrect due to possible rounding):
  !! @code
  !!   if ( a == b ) ...
  !! @endcode
  !! should be substituted by such comparison:
  !! @code
  !!   if ( a .approx. b) ...
  !! @endcode
  !! @warning This function is not intended for direct use.
  elemental function float_approx_hrp_operator(value1, value2) result (equal)
    !> @param[in] value1 The first value for comparison.
    real(HRP), intent(in) :: value1
    !> @param[in] value2 The second value for comparison.
    real(HRP), intent(in) :: value2
    !> @return TRUE if the values `value1` and `value2` are approximately equal.
    logical :: equal
    ! EPSILON_LOC sets the tolerance limit for the default `.approx.` operator.
    ! @note Note that tolerance value here is very high.
    real(HRP), parameter :: EPSILON_LOC = TOLERANCE_HIGH_DEF_HRP
    equal = is_near_zero( abs(value1 - value2), EPSILON_LOC )
  end function float_approx_hrp_operator

  !-----------------------------------------------------------------------------
  !> This function calculates a zero of a function f(x) in the interval
  !! (ax,bx).
  !! - Brent, R.P., (1973). Algorithms for Minimization Without
  !!   Derivatives, Prentice-Hall, Inc.
  !! - Brent, R.P. (1971). An algorithm with guaranteed convergence for
  !!   finding a zero of a function, Computer J.  14, 422–425.
  !! .
  !! Author: Richard Brent, https://maths-people.anu.edu.au/~brent/
  !! Source: http://www.netlib.org/go/
  !! With some minor changes by Sergey Budaev.
  !! @note This function is used in
  !!       the_environment::minimum_depth_visibility().
  !> @param[in] ax left endpoint of initial interval
  !> @param[in] bx right endpoint of initial interval
  !> @param[in] f  function subprogram which evaluates f(x) for any x in
  !!            the interval (ax,bx).
  !> @param[in] tol desired length of the interval of uncertainty of the
  !!            final result (.ge.0.)
  !! @returns   Abscissa approximating a zero of f(x) in the
  !!            interval (ax,bx). Note that this function returns
  !!            commondata::missing if f(ax) and f(bx) do not have
  !!            different signs (so there is no function zero within the
  !!            range).
  real(SRP) function zeroin(ax,bx,f,tol)
      !  @param[in] ax left endpoint of initial interval
      !  @param[in] bx right endpoint of initial interval
      !  @param[in] f  function subprogram which evaluates f(x) for any x in
      !             the interval (ax,bx).
      !  @param[in] tol desired length of the interval of uncertainty of the
      !             final result (.ge.0.)
      !  @returns   abscissa approximating a zero of f(x) in the
      !             interval (ax,bx). Note that this function returns
      !             commondata::missing if f(ax) and f(bx) do not have
      !             different signs (so there is no function zero within the
      !             range).
      real(SRP), intent(in) :: ax,bx,tol
      real(SRP) :: f
    !
    !      a zero of the function  f(x)  is computed in the interval ax,bx .
    !
    !  input..
    !
    !  ax     left endpoint of initial interval
    !  bx     right endpoint of initial interval
    !  f      function subprogram which evaluates f(x) for any x in
    !         the interval  ax,bx
    !  tol    desired length of the interval of uncertainty of the
    !         final result (.ge.0.)
    !
    !  output..
    !
    !  zeroin abscissa approximating a zero of  f  in the interval ax,bx
    !
    !      it is assumed  that   f(ax)   and   f(bx)   have  opposite  signs
    !  this is checked, and an error message is printed if this is not
    !  satisfied.   zeroin  returns a zero  x  in the given interval
    !  ax,bx  to within a tolerance  4*macheps*abs(x)+tol, where macheps  is
    !  the  relative machine precision defined as the smallest representable
    !  number such that  1.+macheps .gt. 1.
    !      this function subprogram is a slightly  modified  translation  of
    !  the algol 60 procedure  zero  given in  Richard Brent, Algorithms for
    !  Minimization Without Derivatives, Prentice-Hall, Inc. (1973).
    !
      real(SRP) ::  a,b,c,d,e,eps,fa,fb,fc,tol1,xm,p,q,r,s
      eps = ZERO
      tol1 = eps+1.0_SRP

      a=ax
      b=bx
      fa=f(a)
      fb=f(b)
    !     check that f(ax) and f(bx) have different signs
      if (fa .eq.0.0_SRP .or. fb .eq. 0.0_SRP) go to 20
      if (fa * (fb/abs(fb)) .le. 0.0_SRP) go to 20
    !        write(6,2500)
    !2500    format(1x,'f(ax) and f(bx) do not have different signs, aborting')
         zeroin = MISSING
         return
   20 c=a
      fc=fa
      d=b-a
      e=d
   30 if (abs(fc).ge.abs(fb)) go to 40
      a=b
      b=c
      c=a
      fa=fb
      fb=fc
      fc=fa
   40 tol1=2.0_SRP*eps*abs(b)+0.5_SRP*tol
      xm = 0.5_SRP*(c-b)
      if ((abs(xm).le.tol1).or.(fb.eq.0.0_SRP)) go to 150
    !
    ! see if a bisection is forced
    !
      if ((abs(e).ge.tol1).and.(abs(fa).gt.abs(fb))) go to 50
      d=xm
      e=d
      go to 110
   50 s=fb/fa
      if (a.ne.c) go to 60
    !
    ! linear interpolation
    !
      p=2.0_SRP*xm*s
      q=1.0_SRP-s
      go to 70
    !
    ! inverse quadratic interpolation
    !
   60 q=fa/fc
      r=fb/fc
      p=s*(2.0_SRP*xm*q*(q-r)-(b-a)*(r-1.0_SRP))
      q=(q-1.0_SRP)*(r-1.0_SRP)*(s-1.0_SRP)
   70 if (p.le.0.0_SRP) go to 80
      q=-q
      go to 90
   80 p=-p
   90 s=e
      e=d
      if (((2.0_SRP*p).ge.(3.0_SRP*xm*q-abs(tol1*q))) .or.                &
          (p.ge. abs(0.5d0*s*q))) go to 100
      d=p/q
      go to 110
  100 d=xm
      e=d
  110 a=b
      fa=fb
      if (abs(d).le.tol1) go to 120
      b=b+d
      go to 140
  120 if (xm.le.0.0_SRP) go to 130
      b=b+tol1
      go to 140
  130 b=b-tol1
  140 fb=f(b)
      if ((fb*(fc/abs(fc))).gt.0.0_SRP) go to 20
      go to 30
  150 zeroin=b
      return
  end function zeroin

  !-----------------------------------------------------------------------------
  !> @brief     Converts and rescales integer allele value to real value for
  !!            neural response function
  !! @param[in] raw_value raw input value, integer within
  !!            `commondata::allelerange_min` and `commondata::allelerange_max`
  !! @retval    Returns the value of conversion function: integer alleles
  !!            to real internal value 0 to 1
  !! @details   Allele conversion function for the relationship between the
  !!            genome integer allele value and its converted real value in
  !!            the neuronal response function. The function rescales integer
  !!            allele value @f$ I_{i} @f$ within the range
  !!            @f$ [I_{min},I_{max}] @f$ to real values @f$ r @f$ within the
  !!            range @f$ [0,1] @f$ using the formula:
  !!               @f[ r = \frac{I - I_{min}}{I_{max}-I_{min}} @f]
  !!            See implementation notes on `allele_value` component of the
  !!            `GENE` derived type.
  !! @note      Note that it is an *elemental function* that can accept both
  !!            scalar and vector arguments.
  elemental function allelescale(raw_value) result(converted)

    ! @param[in] raw_value raw input value, integer within
    !           `commondata::allelerange_min` and `commondata::allelerange_max`.
    integer, intent(in) :: raw_value
    ! @retval Returns the value of conversion function: integer alleles
    !         to real internal value 0 to 1
    real(SRP) :: converted

    converted = (real(raw_value,SRP) - ALLELERANGE_MIN) / &
                       (ALLELERANGE_MAX - ALLELERANGE_MIN)

    !> @warning We must never accept zero values of alleles as they will
    !!          result in **division by zero** in `commondata::gamma2gene()`.
    if (converted < ZERO) converted = ZERO

  end function allelescale

  !-----------------------------------------------------------------------------
  !> @details Rescales the relationship between the numerical value of an
  !!          allele in the genome and the numerical value to be used in the
  !!          neuronal response function.
  !! @param   raw_value Raw input value from the genome.
  !! @retval  Returns the rescaled value.
  !! @note    This is the same component as in the original model ALLELECONV
  !!          parameter. Thus, if the allele value is integer in `gamma2gene`,
  !!          then `commondata::allelescale()` is called first (convert
  !!          integer to real 0..1), then follows `::alleleconv()`
  !!          (rescale 0..1 to rescaled value).
  !! @note    Note that it is an *elemental function* that can accept both
  !!          scalar and vector arguments.
  elemental function alleleconv(raw_value) result (converted)
    ! @param Raw input value from the genome
    real(SRP), intent(in) :: raw_value
    ! @retval Returns the rescaled value
    real(SRP) :: converted

    !> Scale factor for the simple linear conversion (Type 3).
    real(SRP), parameter :: SCALE_FACTOR = 10.

    !> ### Implementation details ###

    !> ### Type 1 ###
    !> *Type 1:* no conversion from 0:1 to output allele value
    !! @note   identical to old alleleconv 1
    !!         @code converted = raw_value @endcode
    converted = raw_value

    !> ### Type 2 ###
    !> *Type 2:* exponential conversion from 0:1 to output allele value,
    !!         to allow finer resolution at weak perception strengths.
    !! @warning This version can produce NaNs.
    !! @note   identical to identical to old alleleconv 2
    !!         @code converted = (10 * raw_value) ** 2 @endcode
    !converted = (10 * raw_value) ** 2

    !> ### Type 3 ###
    !> *Type 3:* linear conversion from 0:1 to output allele value,
    !!         has been the most used value, although not perfect.
    !> @note   identical to old alleleconv 3
    !!         @code converted = raw_value * SCALE_FACTOR @endcode
    !converted = raw_value * SCALE_FACTOR

  end function alleleconv

  !-----------------------------------------------------------------------------
  !> @brief   Calculate the variance from the coefficient of variation.
  !! @details The coefficient of variation `cv`
  !!          @f$ cv= \frac{\sigma}{\bar{P}} @f$. Therefore, the raw
  !!          variance @f$ {\sigma}^{2} @f$ in the `RNORM` function is equal to
  !!          @f[ {\sigma}^{2} = {(cv \cdot \bar{P})}^{2} . @f]
  !! @returns Variance @f$ {\sigma}^{2} @f$
  !! @param cv Coefficient of variation.
  !! @param mean Average.
  !! @note We need this function because Gaussian parameters in `commondata`
  !!       are set using the *average* and *CV* to be more intuitive, scale
  !!       independent. But the function `RNORM` in `HEDTOOLS` gets variance
  !!       as a parameter.
  !! @note Note that this is an elemental function that gets both array and
  !!       scalar parameters.
  elemental function cv2variance (cv, mean) result (variance)
    ! @returns Variance
    real(SRP) :: variance
    ! @param cv Coefficient of variation.
    real(SRP), intent(in) :: cv
    ! @param mean Average.
    real(SRP), intent(in) :: mean

    variance = (mean*cv)**2

  end function cv2variance

  !-----------------------------------------------------------------------------
  !> @brief   The function gamma2gene finds the sigmoid relationship for
  !!          a complex multicomponent 2-allele impact on the neuronal response.
  !! @details The real function gamma2gene finds the sigmoid relationship for a
  !!          multicomponent allele impact on the neuronal response:
  !!          @f[ R= \frac{(P/y_{1})^{x_{1}}}{1+(P/y_{1})^{x_{1}}} +
  !!                 \frac{(P/y_{2})^{x_{2}}}{1+(P/y_{2})^{x_{2}}} +
  !!                 \frac{(P/y_{3})^{x_{3}}}{1+(P/y_{3})^{x_{3}}} ... @f]
  !!          Here, R is the neuronal response, P the strength of the sensory
  !!          input (scaled 0-1), and x and y are two genes. The indices refer
  !!          to the additive components of the alleles. Note that their
  !!          number is set by the parameter `ADDITIVE_COMPS`. Further,
  !!          `erpcv` defines the coefficient of variation for the perception
  !!          error (with respect to its true value).
  !! @returns returns the neuronal response.
  !! @param[in] gs shape: Gene/constant determining the shape of the gamma
  !!            function.
  !!            Note that the raw integer gene values are accepted by this
  !!            function as `commondata::allelescale()` is called
  !!            automatically inside.
  !! @param[in] gh half-max effect: Gene/constant for the signal strength
  !!            giving half max effect.
  !!            Note that the raw integer gene values are accepted by this
  !!            function as commondata::allelescale() is called automatically
  !!            inside.
  !! @param[in] signal perception: Input value of (external or internal)
  !!            stimulus perception.
  !! @param[in] erpcv error: Additive error of stimulus perception, Gaussian
  !!            variance added to the true environmental variable.
  !!            If this parameter is absent, no perception error is
  !!            introduced.
  !!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !!          Maxima function for quick calc:
  !!          @code g2gene(P,x,y,n) := n * ( (P/y)^x / (1+(P/y)^x) ); @endcode
  !> @warning This version of `gamma2gene` accepts *integer* arrays. It *does*
  !!          invoke  `commondata::allelescale()` automatically inside.
  function gamma2gene_additive_i4(gs,gh,signal,erpcv)                         &
                result (neuronal_response)

    ! @retval returns the neuronal response.
    real(SRP) :: neuronal_response

    ! @param[in] gs shape: Gene/constant determining the shape of the gamma
    !            function.
    ! @note      Note that the raw integer gene values are accepted by this
    !            function as `allelescale` is called automatically inside.
    integer, dimension(ADDITIVE_COMPS), intent(in) :: gs

    ! @param[in] gh half-max effect: Gene/constant for the signal strength
    !            giving half max effect.
    ! @note      Note that the raw integer gene values are accepted by this
    !            function as allelescale is called automatically inside.
    integer, dimension(ADDITIVE_COMPS), intent(in) :: gh

    ! @param[in] signal perception: Input value of (external or internal)
    !            stimulus perception.
    real(SRP), intent(in) :: signal

    ! @param[in] erpcv error: Additive error of stimulus perception, Gaussian
    !            variance added to the true environmental variable.
    ! @note      If this parameter is absent, no perception error is
    !            introduced.
    real(SRP), optional, intent(in) :: erpcv

    ! Local variables:
    real(SRP) :: perception
    real(SRP) :: d1
    integer :: i

    ! Parameter that specifies the minimum perception. Can be commondata::zero
    ! or commondata::tolerance_low_def_srp
    real(SRP), parameter :: FORCED_MIN_PERCEPT = ZERO

    !> ### Implementation details ###
    !! Perception is calculated as @f[ P=\bar{P}+\varepsilon , @f]
    !! where @f$ \bar{P} @f$ is the true environmental variable and
    !! @f$ \varepsilon @f$ is a Gaussian error. The perception value with
    !! error is implemented as a normal Gaussian variate with the mean
    !! equal to the true `signal` value @f$ \bar{P} @f$ and the coefficient
    !! of variation equal to the `erpcv` input parameter:
    !! @f$ erpcv= \frac{\sigma}{\bar{P}} @f$. Therefore, the raw
    !! error variance in the `RNORM` function is equal to the square of
    !! `erpcv*signal`. We also impose strict limit on perception @f$ P>0 . @f$
    if (present(erpcv)) then
      if (erpcv > TOLERANCE_HIGH_DEF_SRP) then
        perception = max(FORCED_MIN_PERCEPT, RNORM(signal,(erpcv*signal)**2))
      else
        perception = max(FORCED_MIN_PERCEPT, signal)
      end if
    else
      perception = max(FORCED_MIN_PERCEPT, signal)
    end if

    neuronal_response = 0.0_SRP

    ! @warning The `do concurrent` construct is F2008 and can not (yet) be
    !          implemented in all compilers. Use normal `do` in such a case.
    do concurrent (i=1:ADDITIVE_COMPS)
      d1 = ( perception / alleleconv( allelescale(gh(i)) )                    &
            ) ** alleleconv( allelescale(gs(i)) )
      neuronal_response = neuronal_response + d1/(1.0_SRP+d1)
    end do

  end function gamma2gene_additive_i4

  !-----------------------------------------------------------------------------
  !> @brief     The function gamma2gene finds the sigmoid relationship for
  !!            a complex multicomponent 2-allele impact on the neuronal
  !!            response.
  !! @returns   returns the neuronal response.
  !! @param[in] shape: Gene/constant determining the shape of the gamma
  !!            function.
  !!            Note that the raw integer gene values are accepted by this
  !!            function as `commondata::allelescale()` is called automatically
  !!            inside.
  !! @param[in] half-max effect: Gene/constant for the signal strength giving
  !!            half max effect.
  !!             Note that the raw integer gene values are accepted by this
  !!            function as `commondata::allelescale()` is called automatically
  !!            inside.
  !! @param[in] perception: Input value of (external or internal) stimulus
  !!            perception.
  !! @param[in] error: Additive error of stimulus perception, Gaussian
  !!            variance added to the true environmental variable.
  !!            If this parameter is absent, no perception error is
  !!            introduced.
  !! @details The real function gamma2gene finds the sigmoid relationship for a
  !!          multicomponent allele impact on the neuronal response:
  !!          @f[ R= \frac{(P/y_{1})^{x_{1}}}{1+(P/y_{1})^{x_{1}}} +
  !!                 \frac{(P/y_{2})^{x_{2}}}{1+(P/y_{2})^{x_{2}}} +
  !!                 \frac{(P/y_{3})^{x_{3}}}{1+(P/y_{3})^{x_{3}}} ... @f]
  !!          Here, R is the neuronal response, P the strength of the sensory
  !!          input (scaled 0-1), and x and y are two genes. The indices refer
  !!          to the additive components of the alleles. Note that their
  !!          number is set by the parameter `ADDITIVE_COMPS`. Further,
  !!          `erpcv` defines the coefficient of variation for the perception
  !!          error (with respect to its true value).
  !!          - - - - - - - -
  !!          Maxima function for quick calc:
  !!          @code g2gene(P,x,y,n) := n * ( (P/y)^x / (1+(P/y)^x) ); @endcode
  !> @warning This version of `gamma2gene` accepts *real* arrays. It does *not*
  !!          invoke  `commondata::allelescale()` automatically inside.
  function gamma2gene_additive_r4(gs,gh,signal,erpcv)  result (neuronal_response)

    ! @returns  returns the neuronal response.
    real(SRP) :: neuronal_response

    ! @param[in] shape: Gene/constant determining the shape of the gamma
    !            function.
    ! @note      Note that the raw integer gene values are accepted by this
    !            function as `allelescale` is called automatically inside.
    real(SRP), dimension(ADDITIVE_COMPS), intent(in) :: gs

    ! @param[in] half-max effect: Gene/constant for the signal strength giving
    !            half max effect.
    ! @note      Note that the raw integer gene values are accepted by this
    !            function as `allelescale` is called automatically inside.
    real(SRP), dimension(ADDITIVE_COMPS), intent(in) :: gh

    ! @param[in] perception: Input value of (external or internal) stimulus
    !            perception.
    real(SRP), intent(in) :: signal

    ! @param[in] error: Additive error of stimulus perception, Gaussian
    !            variance added to the true environmental variable.
    ! @note      If this parameter is absent, no perception error is
    !            introduced.
    real(SRP), optional, intent(in) :: erpcv

    ! Local variables:
    real(SRP) :: perception
    real(SRP) :: d1
    integer :: i

    ! Parameter that specifies the minimum perception. Can be commondata::zero
    ! or commondata::tolerance_low_def_srp
    real(SRP), parameter :: FORCED_MIN_PERCEPT = ZERO

    !> Perception is calculated as @f[ P=\bar{P}+\varepsilon , @f]
    !! where @f$ \bar{P} @f$ is the true environmental variable and
    !! @f$ \varepsilon @f$ is a Gaussian error. The perception value with
    !! error is implemented as a normal Gaussian variate with the mean
    !! equal to the true `signal` value @f$ \bar{P} @f$ and the coefficient
    !! of variation equal to the `erpcv` input parameter:
    !! @f$ erpcv= \frac{\sigma}{\bar{P}} @f$. Therefore, the raw
    !! error variance in the `RNORM` function is equal to the square of
    !! `erpcv*signal`. We also impose strict limit on perception min=0.
    if (present(erpcv)) then
      if (erpcv > TOLERANCE_HIGH_DEF_SRP) then
        perception = max(FORCED_MIN_PERCEPT, RNORM(signal,(erpcv*signal)**2))
      else
        perception = max(FORCED_MIN_PERCEPT, signal)
      end if
    else
      perception = max(FORCED_MIN_PERCEPT, signal)
    end if

    neuronal_response = 0.0_SRP

    ! @warning The `do concurrent` construct is F2008 and can not (yet) be
    !          implemented in all compilers. Use normal `do` in such a case.
    do concurrent (i=1:ADDITIVE_COMPS)
      d1 = (perception / alleleconv(gh(i))                                    &
            ) ** alleleconv(gs(i))
      neuronal_response = neuronal_response + d1/(1.0_SRP+d1)
    end do

  end function gamma2gene_additive_r4

  !-----------------------------------------------------------------------------
  !> This "fake" version of the `gamma2gene` is used to guess the response
  !! values in calculations.
  !! @retval predicted_val a predicted value (scalar or array) of the
  !!         sigmoidal neuronal response function. See gamma2gene for details.
  elemental function gamma2gene_fake_vals(signal, gs, gh, n_acomps)           &
                                                        result (predicted_val)
    real(SRP), intent(in) :: signal
    real(SRP), optional, intent(in) :: gs, gh
    integer, optional, intent(in) :: n_acomps
    real(SRP) :: predicted_val

    ! Local copies of optionals.
    real(SRP) :: gs_loc, gh_loc
    integer :: n_acomps_loc

    if (present(gs)) then
      gs_loc = gs
    else
      gs_loc = 0.5_SRP
    end if

    if (present(gh)) then
      gh_loc = gh
    else
      gh_loc = 0.5_SRP
    end if

    if (present(n_acomps)) then
      n_acomps_loc = n_acomps
    else
      n_acomps_loc = ADDITIVE_COMPS
    end if

    predicted_val = n_acomps_loc * ( (signal/gh_loc)**gs_loc /                &
                                            (1.0_SRP+(signal/gh_loc)**gs_loc) )

  end function gamma2gene_fake_vals

  !-----------------------------------------------------------------------------
  !> @brief   Reverse-calculate perception value from the given neural
  !!          response value.
  !! @details Calculates the value of the raw perception from the neural
  !!          response function. This is the reverse of the gamma2gene with
  !!          many components. It is assumed that all x and y values are the
  !!          same, so the equation solved for the most trivial case.
  !!          Calculated according to the formula:
  !!          @f[ P = y\left ( \frac{R}{n-R} \right )^{1/x} , @f] where
  !!          @f$ P @f$ is the perception value, @f$ R @f$ is the neural
  !!          response, @f$ x @f$ and @f$ y @f$ are two genes.
  !! @returns Signal level for specific neural response, back calculated.
  !! @param[in] neuronal_response neuronal response.
  !! @param[in] gs shape parameter of the sigmoid function.
  !! @param[in] gh half-max parameter of the sigmoid function.
  !! @param[in] nc  Number of additive components. Optional, if absent
  !!            assumed 1 (single component).
  !! @n
  !! @note   This function is useful for guessing the average start values
  !!         of genetically determined traits with Gaussian distribution.
  !! @note   Note that it is quite difficult to get really small `gamma2gene`
  !!         values as the signal value should be really small:
  !!         e.g. to get neural response 1.5E-5 (Fulton condition), we need
  !!         signal = 2E-12. So, the function very quickly loses precision
  !!         as we approach really low values. Need kind 8 or 16 precision?
  !! @warning This is quite a crude guess at low values. At lower values
  !!         *underestimates* R, real value is higher. This is due to the
  !!         limitation that R should never be below zero, causing above-zero
  !!         truncation.
  elemental function gamma2gene_reverse(neuronal_response, gs, gh, nc)        &
                                  result (signal)

    !> @returns Signal level for specific neural response, back calculated.
    real(SRP) :: signal
    ! @param[in] neuronal_response neuronal response.
    !            gs shape parameter of the sigmoid function.
    !            gh half-max parameter of the sigmoid function.
    real(SRP), intent(in) :: neuronal_response, gs, gh
    ! @param[in] nc  Number of additive components. Optional, if absent
    !            assumed 1 (single component).
    integer, intent(in), optional :: nc

    ! Local variable.
    integer :: n

    if (present(nc)) then
      n=nc
    else
      n=1
    end if

    !> Maxima function for quick calc:
    !! @code reverse_gamma(R,x,y,n) := y * (R/(n-R))^(1/x); @endcode
    signal = gh * (neuronal_response / n - neuronal_response )**(1.0_SRP/gs)

  end function gamma2gene_reverse

  !=============================================================================
  ! The two procedures below are for the history stack array

  !-----------------------------------------------------------------------------
  !> Simple history stack function, add to the end of the stack. We need
  !! only to add components on top of the stack and retain
  !! `commondata::history_size_spatial` elements of the prior history (for a
  !! spatial moving object). The stack works as follows, assuming 100 and 200
  !! are added:
  !! @n                [1 2 3 4 5 6 7  8   9  10];
  !! @n                [2 3 4 5 6 7 8  9  10 100];
  !! @n                [3 4 5 6 7 8 9 10 100 200]
  !! @param history_array Integer array that keeps the history.
  !! @param add_this we add this element to the end of the history array.
  !! @note This is the integer type version.
  pure subroutine add_to_history_i4(history_array, add_this)

    ! @param history_array Integer array that keeps the history.
    integer, dimension(:), intent(inout) :: history_array
    ! @param add_this we add this element to the end of the history array.
    integer, intent(in) :: add_this

    ! Local variable to keep the size of the history array
    integer :: history_size

    history_size = size(history_array)

    history_array(1:history_size-1) = history_array(2:history_size)
    history_array(history_size) = add_this

  end subroutine add_to_history_i4

  !-----------------------------------------------------------------------------
  !> Simple history stack function, add to the end of the stack. We need
  !! only to add components on top of the stack and retain
  !! `commondata::history_size_spatial` elements of the prior history (for a
  !! spatial moving object).
  !! @param history_array Integer array that keeps the history.
  !! @param add_this we add this element to the end of the history array.
  !! @note This is the real type version.
  pure subroutine add_to_history_r(history_array, add_this)

    ! @param history_array Integer array that keeps the history.
    real(SRP), dimension(:), intent(inout) :: history_array
    ! @param add_this we add this element to the end of the history array.
    real(SRP), intent(in) :: add_this

    ! Local variable to keep the size of the history array.
    integer :: history_size

    history_size = size(history_array)

    history_array(1:history_size-1) = history_array(2:history_size)
    history_array(history_size) = add_this

  end subroutine add_to_history_r

  !-----------------------------------------------------------------------------
  !> Simple history stack function, add to the end of the stack. We need
  !! only to add components on top of the stack and retain
  !! `commondata::history_size_spatial` elements of the prior history.
  !! @param history_array Integer array that keeps the history.
  !! @param add_this we add this element to the end of the history array.
  !! @note This is the character string type version
  pure subroutine add_to_history_char(history_array, add_this)

    ! @param history_array Integer array that keeps the history.
    character(*), dimension(:), intent(inout) :: history_array
    ! @param add_this we add this element to the end of the history array.
    character(*), intent(in) :: add_this

    ! Local variable to keep the size of the history array.
    integer :: history_size

    history_size = size(history_array)

    history_array(1:history_size-1) = history_array(2:history_size)
    history_array(history_size) = add_this

  end subroutine add_to_history_char

  !-----------------------------------------------------------------------------
  !> Converts logical to integer following a rule, default FALSE = 0, TRUE = 1.
  !! @note Note that this function is required to place logical data
  !!       like the survival status (alive) into the reshape array that is
  !!       saved as the CSV data file.
  elemental function conv_l2i(flag, code_false, code_true) result (num_out)
    logical, intent(in) :: flag
    integer, optional, intent(in) :: code_false
    integer, optional, intent(in) :: code_true
    integer :: num_out

    ! Local copies of optionals.
    integer :: code_false_loc, code_true_loc
    ! Local real parameters for the default FALSE and TRUE.
    integer, parameter :: FALSE_DEF=0, TRUE_DEF=1

    ! First, check optional parameters.
    if (present(code_false)) then
      code_false_loc = code_false
    else
      code_false_loc = FALSE_DEF
    end if

    if (present(code_true)) then
      code_true_loc = code_true
    else
      code_true_loc = TRUE_DEF
    end if

    ! Second, do the actual conversion.
    if (flag .eqv. .TRUE.) then
      num_out = code_true_loc
    else
      num_out = code_false_loc
    end if
  end function conv_l2i

  !-----------------------------------------------------------------------------
  !> Converts logical to standard (kind SRP) real, .FALSE. => 0, .TRUE. => 1
  !! @note Note that this function is required to place logical data
  !!       like the survival status (alive) into the reshape array that is
  !!       saved as the CSV data file.
  !!       **Example: **
  !!       @code
  !!          call CSV_MATRIX_WRITE ( reshape(                              &
  !!                     [ habitat_safe%food%food%x,                        &
  !!                       habitat_safe%food%food%y,                        &
  !!                       habitat_safe%food%food%depth,                    &
  !!                       conv_l2r(habitat_safe%food%food%eaten),          &
  !!                       habitat_safe%food%food%size],                    &
  !!                     [habitat_safe%food%number_food_items, 5]),         &
  !!                     "zzz_food_s" // MODEL_NAME // "_" // MMDD //       &
  !!                       "_gen_" // TOSTR(generat, GENERATIONS) // csv,   &
  !!                     ["X   ","Y   ", "D   ", "EATN", "SIZE"]            &
  !!                     ) @endcode
  elemental function conv_l2r(flag, code_false, code_true) result (num_out)
    logical, intent(in) :: flag
    real(SRP), optional, intent(in) :: code_false
    real(SRP), optional, intent(in) :: code_true
    real(SRP) :: num_out
    ! Local copies of optionals.
    real(SRP) :: code_false_loc, code_true_loc
    ! Local real parameters for the default FALSE and TRUE.
    real(SRP), parameter :: FALSE_DEF=0.0_SRP, TRUE_DEF=1.0_SRP

    !> First, check optional parameters.
    if (present(code_false)) then
      code_false_loc = code_false
    else
      code_false_loc = FALSE_DEF
    end if

    if (present(code_true)) then
      code_true_loc = code_true
    else
      code_true_loc = TRUE_DEF
    end if

    ! Second, do the actual conversion.
    if (flag .eqv. .TRUE.) then
      num_out = code_true_loc
    else
      num_out = code_false_loc
    end if
  end function conv_l2r

  !-----------------------------------------------------------------------------
  !> Function to check if the value is the maximum value of an array
  !! (returns TRUE), or not (return FALSE).
  !! @returns TRUE if `value` is indeed the maximum value of the `array` and
  !!          FALSE otherwise.
  !! @param[in] value The value to check
  !! @param[in] array The array to check within.
  !! @param[in] tolerance Optional tolerance threshold.
  pure function is_maxval_r (value, array, tolerance) result (is_max)

    ! @returns TRUE if `value` is indeed the maximum value of the `array` and
    !          FALSE otherwise.
    logical :: is_max
    ! @param[in] value The value to check
    real(SRP), intent(in) :: value
    ! @param[in] array The array to check within.
    real(SRP), dimension(:), intent(in) :: array
    ! @param[in] tolerance Optional tolerance threshold.
    real(SRP), optional, intent(in) :: tolerance

    ! Local tolerance threshold.
    real(SRP) :: tolerance_threshold

    !> @note Check if we are provided with the tolerance threshold, if not, use
    !!       the default parameter commondata::tolerance_low_def_srp value.
    tolerance_threshold = TOLERANCE_LOW_DEF_SRP
    if (present(tolerance)) then
      if ( abs(tolerance) >= TINY_SRP ) tolerance_threshold = abs(tolerance)
    end if

    ! Do the actual comparison.
    if ( float_equal(value, maxval(array), tolerance_threshold ) ) then
      is_max = .TRUE.
    else
      is_max = .FALSE.
    end if

  end function is_maxval_r

  !-----------------------------------------------------------------------------
  !> Function to check if the value is the maximum value of an array
  !! (returns TRUE), or not (return FALSE). Integer version.
  !! @returns TRUE if `value` is indeed the maximum value of the `array` and
  !!          FALSE otherwise.
  !! @param[in] value The value to check
  !! @param[in] array The array to check within.
  pure function is_maxval_i (value, array) result (is_max)

    ! @returns TRUE if `value` is indeed the maximum value of the `array` and
    !          FALSE otherwise.
    logical :: is_max
    ! @param[in] value The value to check
    integer, intent(in) :: value
    ! @param[in] array The array to check within.
    integer, dimension(:), intent(in) :: array

    ! Do the actual comparison.
    if ( value == maxval(array) )  then
      is_max = .TRUE.
    else
      is_max = .FALSE.
    end if

  end function is_maxval_i

  !-----------------------------------------------------------------------------
  !> Function to check if the value is the minimum value of an array
  !! (returns TRUE), or not (return FALSE).
  !! @returns TRUE if `value` is indeed the minimum value of the `array` and
  !!          FALSE otherwise.
  !! @param[in] value The value to check
  !! @param[in] array The array to check within.
  !! @param[in] tolerance Optional tolerance threshold.
  pure function is_minval_r (value, array, tolerance) result (is_max)

    ! @returns TRUE if `value` is indeed the minimum value of the `array` and
    !          FALSE otherwise.
    logical :: is_max
    ! @param[in] value The value to check
    real(SRP), intent(in) :: value
    ! @param[in] array The array to check within.
    real(SRP), dimension(:), intent(in) :: array
    ! @param[in] tolerance Optional tolerance threshold.
    real(SRP), optional, intent(in) :: tolerance

    ! Local tolerance threshold.
    real(SRP) :: tolerance_threshold

    !> @note Check if we are provided with the tolerance threshold, if not, use
    !!       the default parameter commondata::tolerance_low_def_srp value.
    tolerance_threshold = TOLERANCE_LOW_DEF_SRP
    if (present(tolerance)) then
      if ( abs(tolerance) >= TINY_SRP ) tolerance_threshold = abs(tolerance)
    end if

    ! Do the actual comparison.
    if ( float_equal(value, minval(array), tolerance_threshold ) ) then
      is_max = .TRUE.
    else
      is_max = .FALSE.
    end if

  end function is_minval_r

  !-----------------------------------------------------------------------------
  !> Function to check if the value is the minimum value of an array
  !! (returns TRUE), or not (return FALSE). Integer version.
  !! @returns TRUE if `value` is indeed the minimum value of the `array` and
  !!          FALSE otherwise.
  !! @param[in] value The value to check
  !! @param[in] array The array to check within.
  pure function is_minval_i (value, array) result (is_max)

    ! @returns TRUE if `value` is indeed the minimum value of the `array` and
    !          FALSE otherwise.
    logical :: is_max
    ! @param[in] value The value to check
    integer, intent(in) :: value
    ! @param[in] array The array to check within.
    integer, dimension(:), intent(in) :: array

    ! Do the actual comparison.
    if ( value == minval(array) )  then
      is_max = .TRUE.
    else
      is_max = .FALSE.
    end if

  end function is_minval_i

  !=============================================================================
  ! The two procedures below are for the CPU timer / stopwatch object
  ! # Timing (stopwatch) functions #

  !-----------------------------------------------------------------------------
  !> Start the timer object, stopwatch is now ON.
  !! @param title, an optional title for the stopwatch object
  ! @note We do not need exact low-level time as it is machine-specific.
  subroutine timer_cpu_start(this, timer_title)
    class(TIMER_CPU), intent(inout) :: this

    ! @param title, an optional title for the stopwatch object
    character(len=*), optional, intent(in) :: timer_title

    if (present(timer_title)) then
      this%cpu_time_title = timer_title
    else
      this%cpu_time_title = "DEFAULT_TIMER"
    end if

    ! This turns on the CPU stopwatch
    call cpu_time(this%cpu_time_start)

  end subroutine timer_cpu_start

  !-----------------------------------------------------------------------------
  !> Calculate the time elapsed since the stopwatch subroutine was called
  !! for this instance of the timer container object. Can be called several
  !! times showing elapsed time since the grand start.
  !! @returns the time elapsed since `timer_cpu_start` call (object-bound).
  function timer_cpu_elapsed (this) result (cpu_time_elapsed)
    class(TIMER_CPU), intent(in) :: this
    ! @returns the time elapsed since `timer_cpu_start` call (object-bound).
    real(SRP) :: cpu_time_elapsed

    ! Local var
    real(SRP) :: cpu_time_finish

    ! We use the intrinsic `cpu_time` to get the finish time point.
    call cpu_time(cpu_time_finish)

    ! Elapsed time is then trivial to get.
    cpu_time_elapsed = cpu_time_finish - this%cpu_time_start

  end function timer_cpu_elapsed

  !-----------------------------------------------------------------------------
  !> Return the title of the current timer object.
  !! @returns title of the stopwatch timer, allocatable with
  !!          automatic trimming.
  !! @note Useful for  outputs.
  function timer_cpu_title(this) result (timer_title)
    class(TIMER_CPU), intent(in) :: this

    ! @returns title of the stopwatch timer, allocatable with
    !          automatic trimming.
    character(len=:), allocatable :: timer_title

    timer_title = trim(this%cpu_time_title)

  end function timer_cpu_title

  !-----------------------------------------------------------------------------
  !> A ready to use in output function that returns a formatted
  !! string for a timer combining its title and the elapsed time.
  !! For example: `Calculating decomposition took 20s`.
  function timer_cpu_show(this) result (timer_string)
    class(TIMER_CPU), intent(in) :: this
    character(len=:), allocatable :: timer_string
    real(SRP) :: timer_value

    real(SRP), parameter :: ADD_HOURS_VAL_EXCEED = 120.0_SRP

    timer_value = this%elapsed()

    if ( timer_value > TOLERANCE_LOW_DEF_SRP ) then
      timer_string = trim(this%cpu_time_title) // " took " //                 &
                                TOSTR(timer_value) // "s"
      !> @note If the value in *seconds* is too big, hours passed are appended
      !!       in parentheses.
      if (timer_value > ADD_HOURS_VAL_EXCEED) timer_string =                  &
                      timer_string // " (" // TOSTR(timer_value/3600) // " h)"
    else
      timer_string = trim(this%cpu_time_title) //                             &
                               " took ZERO TIME (seems too fast, check data!)"
    end if

  end function timer_cpu_show

  !-----------------------------------------------------------------------------
  !> A ready to use shortcut function to be used in logger, just adds the
  !! TIMER: tag in front of the normal `show`output. @n
  !! **Example use:**
  !! @code
  !!    call stopwatch_1%start("Calculate matrix decomposition")
  !!    .....
  !!    call LOG_DBG( stopwatch_1%log() )
  !! @endcode
  function timer_cpu_log(this) result (timer_string)
    class(TIMER_CPU) :: this
    character(len=:), allocatable :: timer_string

    timer_string = LTAG_TIMER // this%show()

  end function timer_cpu_log

  !=============================================================================

  !-----------------------------------------------------------------------------
  !> Call an external program using a command line. Wrapper to two alternative
  !! system shell calling intrinsic procedures.
  !! @param[in] command is the command line that should be run by the system.
  !! @param[in] suppress_output is an optional logical flag indicating
  !!            whether to suppress any STDOUT output (silent mode).
  !!            Default is FALSE, i.e. do show command output (verbose).
  !! @param[in] suppress_error is a similar optional flag to suppress STDERR
  !!            reporting (silent mode). Default is FALSE.
  !! @param[in] is_background_task is a logical flag to set execution
  !!            on the background, based on wait=.false. parameter of
  !!            `EXECUTE_COMMAND_LINE`. The **default** value is TRUE, i.e.
  !!            **do** process the command at the background. **Warning:**
  !!            Setting command execution as a background task will make
  !!            `cmd_is_success` unusable because the exit code of the
  !!            child process is is deferred.
  !! @param[out] cmd_is_success is a logical flag indicating the command
  !!             result was *success* (zero exit code).
  !! @param[out] exit_code exit code of the child process.
  !! @warning All external calls should normally be
  !!          performed using this wrapper.
  subroutine call_external( command, suppress_output, suppress_error,         &
                                is_background_task, cmd_is_success, exit_code )
    ! @note Importing C_NULL_CHAR is a workaround against the Intel Fortran
    !       compiler bug, see http://ahamodel.uib.no/intel-compiler-bug.html
    !       https://software.intel.com/en-us/forums/intel-fortran-compiler-for-linux-and-mac-os-x/topic/743814
    !       `execute_command_line` intrinsic must add C null string to the end
    !       of the command line to avoid Intel Fortran crashing.
    use ISO_C_BINDING, only: C_NULL_CHAR
    ! @param[in] command is the command line that should be run by the system.
    character(len=*), intent(in) :: command
    ! @param[in] suppress_output is an optional logical flag indicating
    !            whether to suppress any STDOUT output (silent mode).
    !            Default is FALSE, i.e. do show command output (verbose).
    logical, optional, intent(in) :: suppress_output
    ! @param[in] suppress_error is a similar optional flag to suppress STDERR
    !            reporting (silent mode). Default is FALSE.
    logical, optional, intent(in) :: suppress_error
    ! @param[in] is_background_task is a logical flag to set execution
    !            on the background, based on wait=.false. parameter of
    !            `EXECUTE_COMMAND_LINE`. The **default** value is TRUE, i.e.
    !            **do** process the command at the background.
    ! @warning   Setting command execution as a background task will make
    !            `cmd_is_success` unusable because the exit code of the
    !            child process is is deferred.
    logical, optional, intent(in) :: is_background_task
    ! @param[out] cmd_is_success is a logical flag indicating the command
    !             result was *success* (zero exit code).
    logical, optional, intent(out) :: cmd_is_success
    ! @param[out] exit_code exit code of the child process.
    integer, optional, intent(out) :: exit_code

    ! Local copies of optionals.
    logical :: wait_exec

    ! Local finally built command.
    character(len=:), allocatable :: cmd_execute
    character(len=LONG_LABEL_LENGTH) :: cmd_error_msg

    !> ### Implementation details ###

    !> Output is suppressed by redirection to the null device, which is
    !! platform-specific.
    character(len=*), parameter  :: NO_STDOUT_UNIX    = " >/dev/null"
    character(len=*), parameter  :: NO_STDOUT_WINDOWS = " >:NULL"
    character(len=*), parameter  :: NO_STDOUT_DISABLE = ""

    !> Output of the STDERR is also redirected to the platform-specific
    !! null device. For the Windows platform STDERR redirection see
    !! http://support.microsoft.com/en-us/kb/110930.
    character(len=*), parameter  :: NO_STDERR_UNIX    = " 2>/dev/null"
    character(len=*), parameter  :: NO_STDERR_WINDOWS = " 2>&1"
    character(len=*), parameter  :: NO_STDERR_DISABLE = ""

    ! Final null device redirection string.
    character(len=:), allocatable :: null_redirect_stdout, null_redirect_stderr

    ! The `system`  intrinsic has the second optional parameter showing status.
    integer :: status_int, status_err

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(call_external)"

    ! Check disable STDOUT flag.
    if (present(suppress_output)) then
      if (suppress_output) then
        ! Check platform type to set redirection to null device.
        if ( Platform_Running == PLATFORM_WINDOWS ) then
          null_redirect_stdout = NO_STDOUT_WINDOWS
        else
          null_redirect_stdout = NO_STDOUT_UNIX
        end if
      else
        null_redirect_stdout = NO_STDOUT_DISABLE
      end if
    else
      null_redirect_stdout = NO_STDOUT_DISABLE
    end if

    ! Check disable STDERR flag
    if (present(suppress_error)) then
      if (suppress_error) then
        ! Check platform type to set redirection to null device.
        if ( Platform_Running == PLATFORM_WINDOWS ) then
          null_redirect_stderr = NO_STDERR_WINDOWS
        else
          null_redirect_stderr = NO_STDERR_UNIX
        end if
      else
        null_redirect_stderr = NO_STDERR_DISABLE
      end if
    else
      null_redirect_stderr = NO_STDERR_DISABLE
    end if

    !> Check background task optional option `is_background_task`.
    !! @note  If the older non-standard `system` (disabled) command is used
    !!        for executing background task on Unix systems may add "&" at the
    !!        end of the command, but cannot be easily implemented on Windows.
    !!        This functionality is currently not implemented as `system` is
    !!        disabled.
    wait_exec = .FALSE. !> Background parallel execution is enabled by default.
    if (present(is_background_task)) then
      if ( is_background_task .eqv. .FALSE. ) wait_exec = .TRUE.
    end if

    status_int = 0
    status_err = 0
    cmd_error_msg = "unknown"

    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !> The intrinsic procedure `system` is a GNU extension and may not be
    !! available on all platforms and compilers.
    !! It is currently disabled.
    !! @note    If the older non-standard `system` command is used
    !!          for executing background task on Unix systems may add "&" at
    !!          the end of the command, but cannot be easily implemented on
    !!          Windows. This functionality is currently not implemented as
    !!          `system` is normally disabled. A further caveat with `system`
    !!          is that the returned integer exit status parameter works only
    !!          using the IFPORT portability library on Intel Fortran.
    !! @warning The `system` is normally **disabled**. Should be enabled if
    !!          the standard-compliant `execute_command_line` is not supported
    !!          by the local compiler or results in errors.
    !! @warning On some systems, notably 'ahaworkshop', `execute_command_line`
    !!          issues runtime error: `Could not execute command line` when
    !!          calling the debug plotting utilities
    !!          - commondata::debug_histogram_save()
    !!          - commondata::debug_scatterplot_save()
    !!          - commondata::debug_interpolate_plot_save()
    !!          .
    !!          The reason of such crashes is unknown. Probably this caused by
    !!          system specific limitations on child processes when numerous
    !!          child processes of plotting are generated too quickly. A
    !!          workaround is to disable debug plots by setting the parameter
    !!          commondata::is_plotting FALSE using the environment variable
    !!          `AHA_DEBUG_PLOTS=NO`. If plotting is absolutely essential,
    !!          `execute_command_line` should be disabled and the inferior
    !!          `system` call used. The latter does not seem to result in
    !!          crashes, although this has not been well tested.
    !! @code
    !!  call system( cmd_execute, status_int )  ! GNU Fortran
    !!  call system( cmd_execute )              ! Intel Fortran requires IFPORT
    !! @endcode
    !> The F2008 `execute_command_line` should be better used here because it
    !! allows to control asynchronous/synchronous command execution and returns
    !! the exit status.
    !! @code
    !!   call execute_command_line( command  = cmd_execute,                   &
    !!                              exitstat = status_int, wait = wait_exec,  &
    !!                              cmdmsg = cmd_error_msg )
    !! @endcode
    !! @warning The intrinsic procedure `execute_command_line` is part of F2008
    !!          standard and may not be implemented yet on all platforms and
    !!          compilers.
    cmd_execute = command // null_redirect_stdout // null_redirect_stderr     &
                          // C_NULL_CHAR
    call LOG_DBG( LTAG_INFO // "Calling full command line " // cmd_execute // &
                  ", with synchronous mode " // TOSTR(wait_exec),             &
                  PROCNAME, MODNAME )
    call execute_command_line( command  = cmd_execute,                        &
                               wait = wait_exec,                              &
                               exitstat = status_int, cmdstat = status_err,   &
                               cmdmsg = cmd_error_msg )
    !call system( cmd_execute )
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    !> It is possible to get the command execution status if the logical
    !! `cmd_is_success` flag is provided.
    if (present(cmd_is_success)) then
      cmd_is_success = ( status_int == 0 .and. status_err == 0 )
    end if

    if (present(exit_code)) exit_code = status_int

    !> Finally, log the command and its reported exit status if in
    !! the DEBUG mode.
    call LOG_DBG( LTAG_INFO // "Called command line " // command //           &
                  ", with execution status: " // TOSTR(status_err) //         &
                  " and " // "exit code: " // TOSTR(status_int) //            &
                  ", error message: " // trim(cmd_error_msg),                 &
                  PROCNAME, MODNAME  )

  end subroutine call_external

  !-----------------------------------------------------------------------------
  !> Check if an external procedure is executable and can be run.
  function check_external(exec) result (is_valid)
    character(len=*), intent(in) :: exec
    logical :: is_valid

    call call_external( exec, suppress_output=.TRUE., suppress_error=.TRUE.,  &
                        is_background_task=.FALSE., cmd_is_success=is_valid )

  end function check_external

  !-----------------------------------------------------------------------------
  !> Check if an external procedure can be called and log the result.
  !! @param[in]  exec external executable name to call.
  !! @param[in]  debug_only flag indicating that checking is only
  !!             done in the @ref intro_debug_mode "debug mode".
  !! @param[out] is_valid returns if the external procedure is executable
  !!             (TRUE) or not (FALSE).
  subroutine log_check_external(exec, debug_only, is_valid)
    ! @param exec external executable name to call.
    character(len=*), intent(in) :: exec
    ! @param debug_only flag indicating that checking is only
    !        done in the @ref intro_debug_mode "debug mode".
    logical, optional, intent(in) :: debug_only
    ! @param[out] is_valid returns if the external procedure is executable
    !             (TRUE) or not (FALSE).
    logical, optional, intent(out) :: is_valid

    ! Local copy of optional.
    logical :: is_valid_here

    ! Local exit code of the external procedure.
    integer :: int_code

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME = "(log_check_external)"


    if (present(debug_only)) then
      if (debug_only .and. .not. IS_DEBUG) return
    end if

    call LOG_DBG( LTAG_INFO // "Calling command " // exec, PROCNAME, MODNAME )

    call call_external( exec, suppress_output=.TRUE., suppress_error=.TRUE.,  &
                        is_background_task=.FALSE.,                           &
                        cmd_is_success=is_valid_here,                         &
                        exit_code=int_code )

    if ( .not. is_valid_here .or. int_code /= 0 )                             &
      call LOG_MSG( LTAG_WARN // exec // " could not be started, exit code "  &
                    // TOSTR(int_code) // "." )

    if (present(is_valid)) is_valid = is_valid_here

  end subroutine log_check_external

  !-----------------------------------------------------------------------------
  !> Produce a **debug plot** of histogram using an external program
  !! `hthist` from HEDTOOLS tools.
  !! @param[in] x_data The data to be plotted.
  !! @param[in] delete_csv Logical flag, if TRUE, csv file will be deleted
  !!            after plot is done.
  !! @param[in] csv_out_file Optional plot file name, if absent will
  !!            be auto-generated.
  !! @param[in] enable_non_debug Optional flag to enable plot
  !!            even in NON- @ref intro_debug_mode "DEBUG mode". Normally,
  !!            all plot outputs are disabled in non- @ref intro_debug_mode
  !!            "DEBUG mode" (`IS_DEBUG` is `FALSE`, see commondata::is_debug)
  !!            because this may significantly slow down execution and produce
  !!            lots of big PostScript files.
  subroutine debug_histogram_save(x_data, delete_csv, csv_out_file,           &
                                                              enable_non_debug)
    ! @param[in] x_data The data to be plotted.
    real(SRP), dimension(:), intent(in) :: x_data
    ! @param[in] delete_csv Logical flag, if TRUE, csv file will be deleted
    !            after plot is done.
    logical , optional, intent(in) :: delete_csv
    ! @param[in] csv_out_file Optional plot file name, if absent will
    !            be auto-generated.
    character(len=*), optional, intent(in) :: csv_out_file
    ! @param[in] enable_non_debug Optional flag to enable plot
    !            even in NON- @ref intro_debug_mode "DEBUG mode".
    logical, optional, intent(in) :: enable_non_debug

    ! @warning Had to use fixed length string for file name as a workaround
    !          against logger disk file output halting with allocatable string.
    character(len=FILENAME_LENGTH) :: csv_file_here

    logical :: do_background

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(debug_histogram_save)"

    !> ### Implementation notes ###
    !> The histogram plot is actually produced using a separate program
    !! with the executable name set by the character parameter constant
    !! `commondata::exec_histogram` (that should be in the system
    !! path). The plotting program code is normally part of the
    !! HEDTOOLS modelling tools and is placed in HEDTOOLS%\tools folder.
    !! @note Building plotting tools is done with `"make tools"` in the main
    !!       HEDTOOLS file hierarchy. Building requires PGPLOT library and
    !!       can be easily done on Linux; on Windows building can be more
    !!       tricky. See code and output of `commondata::exec_histogram`.
    !! @note Debug plots are not essential, so are done by a separate program
    !!       that can be absent on the runtime system.
    !> Plots are generated only if the protected parameter
    !! commondata::is_plotting is set to `TRUE`.
    if (.not. IS_PLOTTING) return
    !> Normally, plots are generated only in the @ref intro_debug_mode
    !! "debug mode" or if the `enable_non_debug` parameter is explicitly set
    !! to `TRUE`. This is because the code can easily generate huge number of
    !! plots. Also calling external commands has big calculation speed
    !! overhead and can exhaust OS-specific limits on child processes.
    if (.not. IS_DEBUG )  then
      if (present(enable_non_debug)) then
        if (enable_non_debug .eqv. .FALSE.) return
      else
        return
      end if
    end if

    !> The plotting backend program obtains the input data for the scatterplot
    !! from a CSV file. Its name is normally provided on the command line.
    !> In this procedure, input data vector `x_data` is passed into the
    !! called plotting executable via a temporary CSV file. Its
    !! name can be automatically generated or provided explicitly as the
    !! `csv_out_file` dummy parameter. The CSV data file for plotting can also
    !! be saved. Therefore, the histogram could be easily regenerated from
    !! the data using an alternate program (e.g. high quality file prepared
    !! using a different program for publication).
    if (present(csv_out_file)) then
      csv_file_here = csv_out_file
    else
      csv_file_here = "debug_histogr_" // MMDD //                             &
                      "_rev_" // SVN_Version //                               &
                      RAND_STRING(8,LABEL_CST,LABEL_CEN) // csv
    end if

    do_background = .TRUE.
    if (present(delete_csv)) then
      if (delete_csv) do_background = .FALSE.
    end if

    call LOG_MSG( "INFO: Saving histogram, data :" // trim(csv_file_here) )

    !> First, the plot vector data are saved into the temporary CSV file.
    !! @note Note that the data vector is saved *vertical* (`vertical=.TRUE.`).
    call CSV_MATRIX_WRITE ( x_data, trim(csv_file_here), .TRUE. )

    !> Second, the external command to plot the data **histogram** is called
    !! using the `commondata::call_external()` wrapper procedure. We use only
    !! a one-dimensional vector of data and histogram is from the first column.
    !> No exit status or even the availability of
    !! `commondata::exec_scatterplot` is checked. However, the dummy
    !! parameter `is_background_task` controls whether the plotting program
    !! should be executed as a parallel background task (if `TRUE`) or the
    !! model executable should wait for the plotting program to terminate.
    !! The non-parallel (default) mode is safer because calling numerous
    !! child processes can exhaust the system-specific limit on child
    !! processes resulting in an uncontrollable  crash. However, non-parallel
    !! mode is obviously much slower.
    call call_external(                                                       &
          command = EXEC_HISTOGRAM // " 1 " // trim(csv_file_here) // " " //  &
                                               trim(csv_file_here) // PS,     &
          suppress_output = .TRUE., suppress_error = .TRUE.,                  &
          is_background_task = do_background )

    if (present(delete_csv)) then
      if (delete_csv) then
        ! @warning `unlink` is the easiest and most reliable way to
        !           delete a file, but it is not standard (GNU extension)
        !           and may not work on all compilers and systems.
        !           try different methods if this does not work:
        !           system command `del` and opening and closing the file
        !           with the `delete` status:
        !           @code
        !             open(1, file=filename, form='unformatted')
        !             ...
        !             close(1, status='delete')
        !           @endcode
        !call unlink(trim(csv_file_here))
        call file_delete(trim(csv_file_here))
      end if
    end if

  end subroutine debug_histogram_save

  !-----------------------------------------------------------------------------
  !> Produce a **debug plot** of 2-d scatterplot using an external program
  !! `htscatter` from HEDTOOLS tools.
  !! @param[in] x_data The data to be plotted.
  !! @param[in] delete_csv Logical flag, if TRUE, csv file will be deleted
  !!            after plot is done.
  !! @param[in] csv_out_file Optional plot data file name, if absent will
  !!            be auto-generated.
  !! @param[in] enable_non_debug Optional flag, if TRUE, the plot output is
  !!        saved even when *not* in the DEBUG mode. Normally, all plot outputs
  !!        are disabled in non-DEBUG mode (`commondata::is_debug` is `FALSE`,
  !!        see `commondata::is_debug`) because this may significantly slow
  !!        down execution and produce lots of big PostScript files.
  subroutine debug_scatterplot_save(x_data, y_data, delete_csv, csv_out_file, &
                                                              enable_non_debug)
    ! @param[in] x_data The data to be plotted.
    real(SRP), dimension(:), intent(in) :: x_data, y_data
    ! @param[in] delete_csv Logical flag, if TRUE, csv file will be deleted
    !            after plot is done.
    logical , optional, intent(in) :: delete_csv
    ! @param[in] csv_out_file Optional plot file name, if absent will
    !            be auto-generated.
    character(len=*), optional, intent(in) :: csv_out_file
    ! @param[in] enable_non_debug Optional flag to enable plot
    !            even in NON-DEBUG mode.
    logical, optional, intent(in) :: enable_non_debug

    ! @warning Had to use fixed length string for file name as a workaround
    !          against logger disk file output halting with allocatable string.
    character(len=FILENAME_LENGTH) :: csv_file_here

    logical :: do_background

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(debug_scatterplot_save)"

    !> ### Implementation notes ###
    !! The scatterplot is actually produced with a a separate program
    !! with the executable name set by the character parameter constant
    !! `commondata::exec_scatterplot` (that should be in the system
    !! path). The scatterplot program code is normally part of the
    !! HEDTOOLS modelling tools and is placed in `HEDTOOLS\tools` folder.
    !! @note Building plotting tools is done with `"make tools"` in the main
    !!       HEDTOOLS file hierarchy. Building requires PGPLOT library and
    !!       can be easily done on Linux; on Windows building can be more
    !!       tricky. See code and output of `commondata::exec_scatterplot`.
    !! @note Debug plots are not essential, so are done by a separate program.
    !!       They may be especially useful for controlling the quality of
    !!       polynomial `DDPINTERPOL` interpolation, that has a tendency
    !!       towards "wrapped" ends.
    !> Plots are generated only if the protected parameter
    !! commondata::is_plotting is set to `TRUE`.
    if (.not. IS_PLOTTING) return
    !> Plots are normally produced only in the @ref intro_debug_mode
    !! "debug mode" or if the `enable_non_debug` parameter is explicitly set
    !! to `TRUE`.
    if (.not. IS_DEBUG )  then
      if (present(enable_non_debug)) then
        if (enable_non_debug .eqv. .FALSE.) return
      else
        return
      end if
    end if

    !> The plotting backend program obtains the input data for the scatterplot
    !! from a CSV file. Its name is normally provided on the command line.
    !> In this procedure, input data vectors `x_data` and `y_data` are passed
    !! into the called plotting executable via a temporary CSV file. Its
    !! name can be automatically generated or provided explicitly as the
    !! `csv_out_file` dummy parameter. The CSV data file for plotting can also
    !! be saved. Therefore, the scatterplot could be easily regenerated from
    !! the data using an alternate program (e.g. high quality file prepared
    !! using a different program for publication).
    if (present(csv_out_file)) then
      csv_file_here = csv_out_file
    else
      csv_file_here = "debug_scatterplot_" // MMDD //                         &
                      "_rev_" // SVN_Version //                               &
                      RAND_STRING(8,LABEL_CST,LABEL_CEN) // csv
    end if

    do_background = .TRUE.
    if (present(delete_csv)) then
      if (delete_csv) do_background = .FALSE.
    end if

    call LOG_MSG( "INFO: Saving scatterplot, data :" // trim(csv_file_here) )

    !> First, the vector data are saved into the temporary CSV file.
    !! @note Note that if X and Y vectors are different size, the bigger is
    !!       used for the reshaped array and extra values are padded with
    !!       `MISSING` (see commondata::missing).
    call CSV_MATRIX_WRITE ( reshape(                                          &
                              [ x_data, y_data],                              &
                              [max(size(x_data),size(y_data)), 2], [MISSING]),&
                              trim(csv_file_here),  ["X_DATA","Y_DATA"]    )

    !> Second, the external command to produce the plot is called
    !! using the `commondata::call_external()` wrapper procedure.
    !> No exit status or even the availability of
    !! `commondata::exec_scatterplot` is checked. However, the dummy
    !! parameter `is_background_task` controls whether the plotting program
    !! should be executed as a parallel background task (if `TRUE`) or the
    !! model executable should wait for the plotting program to terminate.
    !! The non-parallel (default) mode is safer because calling numerous
    !! child processes can exhaust the system-specific limit on child
    !! processes resulting in an uncontrollable  crash. However, non-parallel
    !! mode is obviously much slower.
    call call_external(                                                       &
          command = EXEC_SCATTERPLOT // " " // trim(csv_file_here) // " " //  &
                                               trim(csv_file_here) // PS,     &
          suppress_output = .TRUE., suppress_error = .TRUE.,                  &
          is_background_task = do_background )

    if (present(delete_csv)) then
      if (delete_csv) then
        ! @warning `unlink` is the easiest and most reliable way to
        !          delete a file, but it is not standard (GNU extension)
        !          and may not work on all compilers and systems.
        !          @note try different methods if this does not work:
        !                system command `del` and opening and closing the file
        !                with the `delete` status:
        !                @code
        !                  open(1, file=filename, form='unformatted')
        !                  ...
        !                  close(1, status='delete')
        !                @endcode
        !call unlink(trim(csv_file_here))
        call file_delete(trim(csv_file_here))
      end if
    end if

  end subroutine debug_scatterplot_save

  !-----------------------------------------------------------------------------
  !> Produce a **debug plot** of the **interpolation data** using an external
  !! program `htinterp` from the HEDTOOLS tools.
  !! @param grid_xx Interpolation grid arrays.
  !! @param grid_yy Interpolation grid arrays.
  !! @param ipol_value Interpolation value.
  !! @param algstr Algorithm string.
  !! @param output_file The file name for debug plot output (PostScript).
  !! @param enable_non_debug Optional flag, if TRUE, interpolation plot is
  !!        saved even when *not* in the DEBUG mode. Normally, all plot outputs
  !!        are disabled in non-DEBUG mode (`IS_DEBUG` is `FALSE`, see
  !!        `commondata::is_debug`) because this may significantly slow
  !!        down execution and produce lots of big PostScript files.
  subroutine debug_interpolate_plot_save( grid_xx, grid_yy, ipol_value,       &
                                        algstr, output_file, enable_non_debug )

    ! Interpolation grid arrays.
    real(SRP), dimension(:), intent(in) :: grid_xx, grid_yy
    ! Interpolation value.
    real(SRP), intent(in) :: ipol_value
    ! Algorithm string.
    character(*), intent(in) :: algstr
    ! The file name for debug plot output (PostScript).
    character(len=*), intent(in) :: output_file
    ! Optional flag, if TRUE, interpolation plot is saved even when not in
    ! the @ref intro_debug_mode "debug mode".
    logical, optional, intent(in) :: enable_non_debug

    ! Local copy of optional
    logical :: enable_non_debug_here

    ! Local finally built command.
    character(len=:), allocatable :: cmd_execute

    !> ### Implementation notes ###
    !> Plots are generated only if the protected parameter
    !! commondata::is_plotting is set to `TRUE`. So the first thing
    !! is to check if it is so.
    if (.not. IS_PLOTTING) return

    !> By default, saving interpolation plots in non
    !! @ref intro_debug_mode "debug mode" is disabled (.FALSE.)
    if (present(enable_non_debug)) then
      enable_non_debug_here = enable_non_debug
    else
      enable_non_debug_here = .FALSE.
    end if

    !> Produce a **debug plot** of the interpolation data. The plot is done by
    !! a separate program with the executable name set by the character
    !! parameter `commondata::exec_interpolate` (that should be in the system
    !! path). This program  is called is called  using the
    !! `commondata::call_external()` wrapper procedure.
    !! The interpolation plot program code is normally part of the
    !! HEDTOOLS modelling tools and is placed in `HEDTOOLS\tools` folder.
    !! @note Building plotting tools is done with `"make tools"` in the main
    !!       HEDTOOLS file hierarchy. Building requires PGPLOT library and
    !!       can be easily built on Linux; on Windows building can be more
    !!       tricky. See code and output of `commondata::exec_interpolate`.
    !! @note Debug plots are not essential, so are done by a separate program.
    !!       They may be especially useful for controlling the quality of
    !!       polynomial `DDPINTERPOL` interpolation, that has a tendency
    !!       towards "wrapped" ends.
    if (IS_DEBUG .or. enable_non_debug_here ) then
      call LOG_MSG( "INFO: Saving interpolation plot: " // trim(output_file) )
      !> All the data for plotting are transferred into the plotting program
      !! via command line parameters. Each of the array or parameter should
      !! be in square brackets.
      !> No exit status or even the availability of
      !! `commondata::exec_interpolate` is checked. However, the dummy
      !! parameter `is_background_task` controls whether the plotting program
      !! should be executed as a parallel background task (if `TRUE`) or the
      !! model executable should wait for the plotting program to terminate.
      !! The non-parallel (default) mode is safer because calling numerous
      !! child processes can exhaust the system-specific limit on child
      !! processes resulting in an uncontrollable  crash. However, non-parallel
      !! mode is obviously much slower.
      !  @note If the program is NOT in PATH on Unix systems, should add
      !        the prefix "./", check platform and adjust in call_external?
      cmd_execute = EXEC_INTERPOLATE // " [" // TOSTR(grid_xx) // "] [" //    &
           TOSTR(grid_yy) // "] [" // TOSTR(ipol_value) // "] [" // algstr // &
           "] [" //  trim(output_file) // "]"
      call call_external(                                                     &
          command = cmd_execute,                                              &
          suppress_output = .TRUE., suppress_error = .TRUE.,                  &
          is_background_task = .FALSE. )
    end if

  end subroutine debug_interpolate_plot_save

  !-----------------------------------------------------------------------------
  !> Delete a file from the local file system using Fortran open status=delete
  !! or fast POSIX C call.
  !! @note The easiest way is to use `unlink` but it is non-standard (GNU
  !!       extension).
  !! @note HEDTOOLS now implement a few POSIX procedures  for manipulation
  !!       of the file system. Among them,  `FS_UNLINK()` is used to delete
  !!       a file (**not** directory) and `FS_REMOVE()` deletes a  file or
  !!       a directory. Example call:
  !!       @code
  !!         call FS_UNLINK("obsolete_file", iostat)
  !!       @endcode
  !!       `iostat` is an optional integer error status of the operation.
  !! @warning POSIX-based `FS_UNLINK()` is now used in this procedure if the
  !!          parameter `commondata::use_posix_fs_utils` is set to TRUE.
  !!          See portability note on this parameter.
  subroutine file_delete(file_name, success)
    character(len=*), intent(in) :: file_name
    logical, optional, intent(out) :: success

    integer :: file_unit, file_error_status
    logical :: openedq, existq

    ! PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(file_delete)"

    ! #### POSIX Utilities ####
    ! Try to use POSIX C calls if `commondata::use_posix_fs_utils` is set.
    POSIX_USED: if ( USE_POSIX_FS_UTILS ) then
      call FS_UNLINK( file_name, file_error_status )
      if (file_error_status == 0 ) then
        call LOG_DBG( LTAG_INFO // PROCNAME // " Deleted file " // file_name  &
                      // " using POSIX call, result is success." )
        if (present(success)) success = .TRUE.
      else
        call LOG_MSG( LTAG_ERROR // PROCNAME // " Could not delete file " //  &
                    file_name // " using POSIX call (unlink)." )
        if (present(success)) success = .FALSE.
      end if
      return
    end if POSIX_USED

    ! #### Fortran specific "manual" calls ####
    ! Check if the file is opened, this is important on Windows as we could
    ! not delete opened files, will get sharing violation error.
    ! can use the CHECK_FILE_OPEN() function from HEDTOOLS.
    inquire(file=file_name, opened=openedq, exist=existq,                     &
            iostat=file_error_status)

    ! If the file is accessible and not opened it can be safely deleted.
    if ( .not. openedq .and. existq .and. file_error_status==0 ) then
      ! Get a free file unit.
      file_unit = GET_FREE_FUNIT()
      ! Try to open
      open (unit=file_unit, file=file_name, status='old',                     &
            iostat=file_error_status)
      ! and close right away with `status='delete'`.
      close(unit=file_unit, status='delete', iostat= file_error_status)
      ! Report success status.
      if (present(success)) then
        success = (file_error_status == 0)
        call LOG_DBG( LTAG_INFO // PROCNAME // " Deleted file " // file_name  &
                      // " with success flag " // TOSTR(success) // "." )
      end if
      call LOG_MSG( LTAG_INFO // PROCNAME // " Deleted file " // file_name    &
                      // " , result is success." )
    else
      if (present(success)) success = .FALSE.
      call LOG_MSG( LTAG_INFO // PROCNAME // " Could not delete file " //     &
                    file_name // "." )
    end if

  end subroutine file_delete


  !-----------------------------------------------------------------------------
  !> Random operator, adds or subtracts two values with equal probability,
  !! used in the random walk functions.
  function random_add_subtract(x, y) result (out_val)
    !> @param[in] x is the first real number
    real(SRP), intent(in) :: x
    !> @param[in] y is the second real number
    real(SRP), intent(in) :: y
    !> @return *x + y* or *x - y* with equal 0.5 probability.
    real(SRP) :: out_val

    if (RAND_I(1,2)==1) then
      out_val =  x + y
    else
      out_val =  x - y
    end if

  end function random_add_subtract

  !-----------------------------------------------------------------------------
  !> @brief   Initialises the system environment and sets basic parameters.
  !! @note    This procedure also initialises the logger by a call of
  !!          `commondata::logger_init`.
  subroutine system_init()
    use CSV_IO, only : GET_FREE_FUNIT
    ! Local string variable to keep a temporary value while parsing parameters.
    ! @note The character string variable that gets its value from the
    !       environment variable or command line intrinsic procedures
    !       `get_environment_variable` or `get_command_argument` cannot
    !       be allocatable. In such a case it does not allocate and remains
    !       blank string value.
    character(len=LABEL_LENGTH) :: debug_string

    ! Checking validity (is executable) codes for external programs.
    logical :: check_1, check_2, check_3, check_4

    ! Flag to avoid checking external executables which breaks Intel Fortran.
    logical :: do_check_externals

    ! PROCNAME is the procedure name for logging and debugging
    character(len=*), parameter :: PROCNAME = "(system_init)"

    ! Local variable to check commondata::lock_file exists.
    logical :: lock_preexists

    ! Local variable used to get the lock file write iostat value.
    integer :: lock_iostat

    !> ### Check lock file ###
    !> First, the program checks if @ref intro_overview_lockfile
    !! "the lock file" exists. If it does, this can mean that a simulation
    !! is already running in the working directory. Thus, the current process
    !! is terminated before any files are written to avoid mess.
    !! @note If the commondata::lock_file has not been deleted automatically
    !!       for any reason but one is sure that no simulation is actually
    !!       running (e.g. there was a crash before or the program was closed
    !!       by Ctrl+C), just delete the commondata::lock_file and restart the
    !!       program.
    inquire( file=LOCK_FILE, exist=lock_preexists )
    if ( lock_preexists ) then
      call system_halt( is_error = .TRUE.,                                    &
                        message=ERROR_LOCK_PREEXISTS,                         &
                        ignore_lockfile = .TRUE.  )
    end if

    !> ### Random seed ###
    !> Then, the random seed is initialised by
    !! [RANDOM_SEED_INIT()](http://ahamodel.uib.no/doc/ar01s09.html#_subroutine_random_seed_init)
    !! from `HEDTOOLS`. Note that it is platform-dependent and uses the system
    !! entropy pool on Unix systems and the date on Windows.
    call RANDOM_SEED_INIT()

    !> ### Execution control ###
    !> #### The DEBUG mode ####
    !> The protected global variable `IS_DEBUG` (commondata::is_debug) sets
    !! up the @ref intro_debug_mode "debug mode" of execution. The debug mode
    !! results in huge amount of output and logs that significantly slows
    !! down execution. Debug mode can be set using the environment variable
    !! `AHA_DEBUG=1`, `AHA_DEBUG=YES` or `AHA_DEBUG=TRUE`.
    call get_environment_variable(name="AHA_DEBUG", value=debug_string)
    debug_string = LOWERCASE(debug_string)
    if (  trim(debug_string)=="1" .or.                                        &
          trim(debug_string)=="yes" .or.                                      &
          trim(debug_string)=="true"  ) then
      ! DEBUG_COMPILER_OPTIMISE: see 'Compiler optimisation of debug mode'
      IS_DEBUG =.TRUE.
      IS_SCREEN_OUTPUT = .TRUE.
    else if ( trim(debug_string)=="0" .or.                                    &
              trim(debug_string)=="no" .or.                                   &
              trim(debug_string)=="false"  ) then
      ! DEBUG_COMPILER_OPTIMISE: see 'Compiler optimisation of debug mode'
      IS_DEBUG =.FALSE.
      IS_SCREEN_OUTPUT = .FALSE.
    end if

    !> Debug mode can also be set by setting the runtime **command line**
    !! parameter to `DEBUG`, `DEBUG=1`, `DEBUG=YES` or `DEBUG=TRUE` to the
    !! model executable, e.g.
    !! @code{.sh}
    !!   ./MODEL.exe DEBUG
    !! @endcode
    !! See @ref intro_debug_mode "The DEBUG mode" for more details.
    !> #### Logging in the DEBUG mode ####
    !! The procedure `CALL_DBG()` (see commondata::log_dbg()) is used to
    !! issue logger messages only in the DEBUG mode. Those messages, that
    !! should be issued in any mode, both DEBUG and non-DEBUG, `LOG_MSG()`
    !! from the HEDTOOLS should be used.
    !!
    !! **Examples:** This code produces the logger message:
    !! @code
    !!   call LOG_MSG("Revision ID: " // SVN_Version // ".")
    !! @endcode
    !! However, the below code produces logger output only in the
    !! @ref intro_debug_mode "debug mode":
    !! @code
    !!   call LOG_DBG(LTAG_INFO // "Agent is freezing.", PROCNAME, MODNAME)
    !! @endcode
    !! Note the use of the logger tag commondata::ltag_info in the later
    !! example.
    call get_command_argument(number=1, value=debug_string)
    debug_string = LOWERCASE(debug_string)
    if (  trim(debug_string)=="debug" .or.                                    &
          trim(debug_string)=="debug=1" .or.                                  &
          trim(debug_string)=="debug=yes" .or.                                &
          trim(debug_string)=="debug=true"  ) then
      ! DEBUG_COMPILER_OPTIMISE: see 'Compiler optimisation of debug mode'
      IS_DEBUG =.TRUE.
      IS_SCREEN_OUTPUT = .TRUE.
    end if

    !> #### Compiler optimisation of the debug mode ####
    !> @anchor system_debug_optimise
    !! Setting the `IS_DEBUG` as a protected variable commondata::is_debug
    !! enables one to switch between the DEBUG and non-DEBUG mode of the
    !! program execution. However, it can have a performance penalty because
    !! all the calls to the debugging code remains in the model. The program
    !! then has to test numerous if conditions `if ( IS_DEBUG ) then...` which
    !! are likely to slow down execution. Setting commondata::is_debug as a
    !! fixed parameter (i.e. with the `parameter` attribute) would allow a
    !! highly optimising compiler to determine at the compile time that the
    !! numerous debugging instructions are never executed and remove them
    !! from the machine instructions that are then generated. However,
    !! declaring parameter won't allow to change the value of the `IS_DEBUG`.
    !! All such cases should be disabled. Within the code, such places are
    !! marked with the DEBUG_COMPILER_OPTIMISE tag. If `IS_DEBUG` is declared
    !! as a parameter, all these places should be disabled, e.g.
    !! @code
    !! trim(debug_string)=="true"  ) then
    !! ! DEBUG_COMPILER_OPTIMISE: see 'Compiler optimisation of debug mode'
    !! ! IS_DEBUG =.TRUE.
    !! IS_SCREEN_OUTPUT = .TRUE.
    !! @endcode
    !! In effect, it would not be possible to switch between the debug modes
    !! on the fly.

    !> #### Controlling the screen output ####
    !> The logger outputs normally go to the log file. But can also be shown
    !! on the terminal screen (standard output). This is controlled by the
    !! protected global variable `commondata::is_screen_output`.
    !!
    !! The Screen mode can be reset independently of DEBUG mode using the
    !! shell environment variable `AHA_SCREEN=1`, `AHA_SCREEN=YES` or
    !! `AHA_SCREEN=TRUE`.
    !! For example, on Linux it is done
    !! @code{.sh}
    !!   export AHA_SCREEN=1
    !! @endcode
    !! on Windows command line:
    !! @code{.sh}
    !!   set AHA_SCREEN=1
    !! @endcode
    !!
    !! It is possible to change the output channel of the logger during the run
    !! time using the `LOG_CONFIGURE()` procedure. For example certain log
    !! message can selectively appear on the terminal standard output, and then
    !! the default behaviour controlled by the commondata::is_screen_output
    !! parameter restored:
    !! @code
    !! call LOG_CONFIGURE("writeonstdout" , .TRUE.) ! Enable screen temporarily
    !! call LOG_MSG( "*** START DEBUG BLOCK ***" )  ! Send message
    !! call LOG_CONFIGURE("writeonstdout" , IS_SCREEN_OUTPUT) ! Enable default
    !! @endcode
    !! For more details see the
    !! [LOGGER Module](http://ahamodel.uib.no/doc/ar01s10.html) documentation of
    !! the HEDTOOLS.
    call get_environment_variable(name="AHA_SCREEN", value=debug_string)
    debug_string = LOWERCASE(debug_string)
    if (  trim(debug_string)=="1" .or.                                        &
          trim(debug_string)=="yes" .or.                                      &
          trim(debug_string)=="true"  ) IS_SCREEN_OUTPUT =.TRUE.
    if (  trim(debug_string)=="0" .or.                                        &
          trim(debug_string)=="no" .or.                                       &
          trim(debug_string)=="false"  ) IS_SCREEN_OUTPUT =.FALSE.

    !> #### Generation of debug plots ####
    !> The debug plots can be generated. However, their number is normally just
    !! huge. Also, debug plots are produced by calling external programs which,
    !! if done too frequently, can exhaust the system-specific limit on the
    !! number of child processes. Production of debug plots is globally
    !! by the protected global variable `commondata::is_plotting`.
    !!
    !! This can be reset independently of using the shell environment variable
    !! `AHA_DEBUG_PLOTS=1`, `AHA_DEBUG_PLOTS=YES` or `AHA_DEBUG_PLOTS=TRUE`.
    !! For example, on Linux it is done
    !! @code{.sh}
    !!   export AHA_DEBUG_PLOTS=1
    !! @endcode
    !! on Windows command line:
    !! @code{.sh}
    !!   set AHA_DEBUG_PLOTS=1
    !! @endcode
    call get_environment_variable(name="AHA_DEBUG_PLOTS", value=debug_string)
    debug_string = LOWERCASE(debug_string)
    if (  trim(debug_string)=="1" .or.                                        &
          trim(debug_string)=="yes" .or.                                      &
          trim(debug_string)=="true"       ) IS_PLOTTING =.TRUE.
    if (  trim(debug_string)=="0" .or.                                        &
          trim(debug_string)=="no" .or.                                       &
          trim(debug_string)=="false"      ) IS_PLOTTING =.FALSE.

    !> #### Compression of big output data files ####
    !> The model can generate big data files for the agent population and
    !! habitat objects. These files can be automatically compressed using the
    !! external command defined by the commondata::cmd_zip_output parameter.
    !! This compression option is determined by the protected global variable
    !! `commondata::is_zip_outputs`.
    !!
    !! This can be reset using the shell environment variable
    !! `AHA_ZIP_FILES=1`, `AHA_ZIP_FILES=YES` or `AHA_ZIP_FILES=TRUE`.
    !! For example, on Linux it is done
    !! @code{.sh}
    !!   export AHA_ZIP_FILES=1
    !! @endcode
    !! on Windows command line:
    !! @code{.sh}
    !!   set AHA_ZIP_FILES=1
    !! @endcode
    call get_environment_variable(name="AHA_ZIP_FILES", value=debug_string)
    debug_string = LOWERCASE(debug_string)
    if (  trim(debug_string)=="1" .or.                                        &
          trim(debug_string)=="yes" .or.                                      &
          trim(debug_string)=="true"       ) IS_ZIP_OUTPUTS =.TRUE.
    if (  trim(debug_string)=="0" .or.                                        &
          trim(debug_string)=="no" .or.                                       &
          trim(debug_string)=="false"      ) IS_ZIP_OUTPUTS =.FALSE.

    !> #### Checking external executables ####
    !> This procedure checks external procedures for existence and being
    !! executable. However, there seems to be a
    !! [bug in Intel Fortran](https://software.intel.com/en-us/forums/intel-fortran-compiler-for-linux-and-mac-os-x/topic/743814)
    !! implementation of the intrinsic `execute_command_line` procedure:
    !! if the called program is not found in the `PATH`, the whole program
    !! crashes with "Segmentation fault".
    !! @code{.sh}
    !!   forrtl: severe (174): SIGSEGV, segmentation fault occurred
    !! @endcode
    !! A workaround is to avoid checking the externals in such a case.
    !! This can be done by setting the
    !! environment variable `AHA_CHECK_EXTERNALS` to `0`, `no` or `false`.
    !! @note This is not normally required if GNU gfortran is used for
    !!       building.
    !! For example, on Linux it is done
    !! @code{.sh}
    !!   export AHA_CHECK_EXTERNALS=NO
    !! @endcode
    !! on Windows command line:
    !! @code{.sh}
    !!   set AHA_CHECK_EXTERNALS=NO
    !! @endcode
    do_check_externals =.TRUE.
    call get_environment_variable(name="AHA_CHECK_EXTERNALS", value=debug_string)
    debug_string = LOWERCASE(debug_string)
    if (  trim(debug_string)=="0" .or.                                        &
          trim(debug_string)=="no" .or.                                       &
          trim(debug_string)=="false"      ) do_check_externals =.FALSE.

    !> ### Implementation details ###
    !> - Get the time tag for the model in this format (YYYYMMDD) by call to
    !!   the `commondata::tag_mmdd()` procedure. commondata::mmdd is a global
    !!   public protected variable. It should be used for all date tagging.
    MMDD = tag_mmdd()

    !> - Initialise the system logger by `commondata::logger_init`. Some
    !!   system parameters,  e.g. the platform type are determined in the
    !!   `commondata::logger_init`.
    call logger_init()

    !> #### Quick integrity checks ####
    call LOG_MSG( LTAG_INFO // "Starting initial checks." )

    !> - Check external executables that are called from the model code.
    !!   These are basically not essential and mainly used only in the
    !!   @ref intro_debug_mode "debug mode".
    if (do_check_externals) then
      call LOG_DELIMITER(LOG_LEVEL_CHAPTER)
      call LOG_MSG (LTAG_INFO // "Checking external procedures. " //         &
                                 "Note: may not work on all platforms.")
      call LOG_MSG (LTAG_WARN // "THIS CAN CRASH THIS PROGRAM IF INTEL FORTRAN")
      call LOG_MSG (LTAG_WARN // "COMPILER IS USED FOR BUILDING.")
      call LOG_MSG (LTAG_WARN // "IN SUCH A CASE SET ENVIRONMENT VARIABLE")
      call LOG_MSG (LTAG_WARN // "'AHA_CHECK_EXTERNALS=FALSE' AND DISABLE")
      call LOG_MSG (LTAG_WARN // "DEBUG PLOTS BY SETTING ENVIRONMENT VARIABVLE")
      call LOG_MSG (LTAG_WARN // "'AHA_DEBUG_PLOTS=FALSE'.")
      call LOG_DELIMITER(LOG_LEVEL_CHAPTER)

      call log_check_external(EXEC_INTERPOLATE, is_valid = check_1)
      call log_check_external(EXEC_SCATTERPLOT, is_valid = check_2)
      call log_check_external(EXEC_HISTOGRAM, is_valid = check_3)

      !>  - If the commondata::is_zip_outputs is enabled (TRUE), a check is
      !!    done if the external compression program
      !!    (commondata::cmd_zip_output) can be called. It involves
      !!    compressing a small test file and getting the exit code.
      CHECK_ZIP: block
        character(len=*), parameter :: test_file_zip = "ztest_compression"
        integer :: test_file_unit
        check_4 = .TRUE.
        if ( IS_ZIP_OUTPUTS ) then
          test_file_unit = GET_FREE_FUNIT()
          open( unit=test_file_unit, file=test_file_zip,                      &
                iostat=lock_iostat, action='write', status='replace',         &
                position='append' )
          close(test_file_unit)
          call LOG_MSG( LTAG_INFO // "Checking if compression of data " //    &
                        "files is possible: " //                              &
                        CMD_ZIP_OUTPUT // " " // test_file_zip )
          call LOG_MSG( LTAG_WARN // "If this command hangs, disable " //     &
                        "data file compression by setting environment " //    &
                        "variable AHA_ZIP_FILES=NO." )
          !>      @warning commondata::log_check_external() procedure is called
          !!              in the synchronous mode and might hang the system if
          !!              the child process hangs for any reason, e.g. if the
          !!              compression utility is wrongly configured and waits
          !!              input from the standard input.
          call log_check_external(  CMD_ZIP_OUTPUT // " " // test_file_zip,   &
                                    is_valid = check_4 )
          call LOG_MSG( LTAG_INFO // "Command executed and not hanged.")
          !>   If compression fails, automatic background compression is
          !!   disabled. commondata::is_zip_outputs is set to FALSE.
          if ( check_4 .eqv. .FALSE. ) then
            call file_delete( test_file_zip )
            IS_ZIP_OUTPUTS = .FALSE.
            call LOG_MSG( LTAG_INFO // CMD_ZIP_OUTPUT//" "//test_file_zip //  &
                          ": Compression test failed, IS_ZIP_OUTPUTS disabled")
          else
            call file_delete( test_file_zip // ZIP_FILE_EXTENSSION )
          end if
        end if
      end block CHECK_ZIP

      !> - The results of these checks are reported in the logger.
      call LOG_MSG( LTAG_INFO // "Validity (is executable) codes: " //        &
                    TOSTR([check_1, check_2, check_3, check_4]) )

      !>   If **any** of the external *plotting utilities* cannot be executed:
      !!   - commondata::exec_interpolate
      !!   - commondata::exec_scatterplot
      !!   - commondata::exec_histogram
      !!   .
      !!   plotting is disabled by setting commondata::is_plotting to FALSE.
      if ( any([check_1,check_2,check_3] .eqv. .FALSE.) ) then
        call LOG_MSG( LTAG_WARN // "External procedures cannot be called" //  &
                      ", generation of debug plots is DISABLED: " //          &
                      "IS_PLOTTING set to FALSE." )
        IS_PLOTTING = .FALSE.
        call LOG_DELIMITER(LOG_LEVEL_VOLUME)
        call LOG_MSG( "  DEBUG is: " // TOSTR(IS_DEBUG) //                    &
                      "; SCREEN is: " // TOSTR(IS_SCREEN_OUTPUT) //           &
                      "; PLOTTING is: " // TOSTR(IS_PLOTTING) )
        call LOG_DELIMITER(LOG_LEVEL_VOLUME)
      end if
    end if

    !> - Check automatic allocation of arrays on intrinsic assignment. The
    !!   code here and there depends on this recent feature of Fortran.
    !!   If the test fails, manual allocation of such arrays with
    !!   [allocate](http://ahamodel.uib.no/intel/hh_goto.htm#GUID-2E8BB6B6-6454-4DB6-9982-5BD97DEB47C1.html)
    !!   should be recoded. The test is tagged as `TEST_AUTOALLOC`.
    TEST_AUTOALLOC: block
      real(SRP), allocatable, dimension(:) :: test_array
      test_array = [ 1.0_SRP, 2.0_SRP, 3.0_SRP ]
      if ( .not. allocated(test_array) .or. size(test_array) /= 3 ) then
        call LOG_DELIMITER(LOG_LEVEL_CHAPTER)
        call LOG_MSG( LTAG_CRIT // "TEST_AUTOALLOC: Automatic array "   //  &
                      "allocation is not enabled or supported" //           &
                      " by the compiler. " // PROCNAME // "::" // MODNAME )
        call system_halt(is_error=.TRUE., message=ERROR_NO_AUTOALLOC )
      end if
    end block TEST_AUTOALLOC

    !> - Check automatic determination of the size in parameter parameter
    !!   arrays of the dimension statement style: `dimension(*)`. All fairly
    !!   modern Fortran compilers should support this feature.
    TEST_AUTO_PARAM_ARRAYS: block
      !> @note If the compiler does not support `dimension(*)` it would
      !!       probably just fail to compile this code. So, it won't run
      !!       up to this point.
      real(SRP), parameter, dimension(*) :: test_array =                      &
                                                [ 1.0_SRP, 2.0_SRP, 3.0_SRP ]
      if ( size(test_array) /= 3 .or.                                         &
           test_array(1) /= 1.0_SRP .or.                                      &
           test_array(2) /= 2.0_SRP .or.                                      &
           test_array(3) /= 3.0_SRP ) then
        call LOG_DELIMITER(LOG_LEVEL_CHAPTER)
        call LOG_MSG( LTAG_CRIT // "TEST_AUTO_PARAM_ARRAYS: Constant array "  &
                      // "automatic size determination is not supported" //   &
                      " by the compiler. " // PROCNAME // "::" // MODNAME )
        call system_halt(is_error=.TRUE., message=ERROR_AUTO_PARAM_ARRAYS )
      end if
    end block TEST_AUTO_PARAM_ARRAYS

    !> - If fatal tests (`TEST_AUTOALLOC`, `TEST_AUTO_PARAM_ARRAYS`) fail,
    !!   commondata::system_halt() is called
    !! .
    !!
    !> Finally, a @ref intro_overview_lockfile "lock file" is created. This
    !! file keeps opened during the whole simulation and is closed and deleted
    !! at the end. Thus, its primary use is to signal that the simulation is
    !! still going. See commondata::lock_file and commondata::lock_file_unit
    !! and @ref intro_overview_lockfile "The lock file" for details.
    !! @note    Lock file operation uses native Fortran intrinsic `open`
    !!          statement rather than any higher level procedures like
    !!          @ref file_io. This is because the file is for signalling
    !!          only (intended to be empty) and nothing is actually written
    !!          into it.
    Lock_File_Unit = GET_FREE_FUNIT()
    open( unit=Lock_File_Unit, file=LOCK_FILE, iostat=lock_iostat,            &
          action='write', status='replace', position='append' )
    !> Note that if the lock file cannot be written, the simulation is not
    !! halted automatically, but an error is issued to the logger because
    !! such error signals severe problems with disk access (e.g. read only
    !! mode, no space left etc.).
    if ( lock_iostat /= 0 ) then
      call LOG_MSG( LTAG_ERROR // "Cannot write lock file: " // LOCK_FILE //  &
                    ". Trying to continue but errors and crashes are "   //   &
                    "expected due to disk write problems!" )
    else
      call LOG_MSG( LTAG_INFO // "Created Lock file " // LOCK_FILE //         &
                    ", unit " // TOSTR(Lock_File_Unit) )
    end if

  end subroutine system_init

  !-----------------------------------------------------------------------------
  !> Halt execution of the system with a specific message and exit code.
  !! The exit code is normally passed to the operating system. However, this
  !! behaviour is implementation dependent and can be unexpected on specific
  !! the platform(s) and the compiler(s).
  !! - Checking the exit code in bash can be done with "`$?`" variable.
  !! - Checking exit code on Windows is done using "`%ERRORLEVEL%`" variable
  !!   e.g.
  !!   @code
  !!     if %ERRORLEVEL% EQU 0 echo Normal termination
  !!   @endcode
  !! .
  !> @note Using specific exit code could allow to place the simulation model
  !!       into an automated batch job more easily. For example, several
  !!       simulations with different parameters could be processed. In such
  !!       a case it would be good to know if the program failed or not.
  !> An example termination call due to an error:
  !! @code
  !!   call system_halt(is_error=.TRUE., message=ERROR_NO_AUTOALLOC )
  !! @endcode
  subroutine system_halt(is_error, message, ignore_lockfile)

    !> The standard error unit is obtained from the `ISO_FORTRAN_ENV`.
    !! The final termination messages go to the standard error device rather
    !! than standard output to adhere the standard.
    use, intrinsic :: ISO_FORTRAN_ENV, only : ERROR_UNIT

    !> @param[in] is_error Optional flag that signals that the program is
    !!            terminating due to error. The default is normal error-free
    !!            termination with zero exit code.
    logical, optional, intent(in) ::  is_error
    !> @param[in] message Optional message that is passed the the logger
    !!            immediately before the program is terminated.
    character (len=*), optional, intent(in) :: message
    !> @param[in] ignore_lockfile is an optional flag to ignore closing and
    !!            deleting @ref intro_overview_lockfile "the lock file". The
    !!            default value is FALSE. If it is TRUE, the lock file is
    !!            not touched. This is primarily necessary to halt the
    !!            execution because the program discovers the lock file on
    !!            startup which may indicate another simulation is currently
    !!            running here. In such a case, deleting the lock file would
    !!            interfere with this pre-running simulation.
    logical, optional, intent(in) :: ignore_lockfile

    ! Local copies of optionals
    integer :: error_code
    character(len=LONG_LABEL_LENGTH) :: message_loc

    !> ### Implementation notes ###
    !> There are two possible exit code:
    !> - **EXIT_CODE_DEF** is the default exit code that is returned to
    !!   the operating system. The default value for error-free termination is
    !!   zero.
    integer, parameter :: EXIT_CODE_DEF = 0

    !> - **EXIT_CODE_ERROR** is the fixed exit code that is returned to the
    !!   operating  system in case of error.
    !! .
    integer, parameter :: EXIT_CODE_ERROR = 255

    ! Local variable used to get the lock file write iostat value.
    integer :: lock_iostat
    ! Logical flag to process the @ref intro_overview_lockfile "lock file".
    logical :: do_not_touch_lock

    error_code = EXIT_CODE_DEF
    if (present(is_error)) then
      if (is_error) error_code = EXIT_CODE_ERROR
    end if

    if (present(message)) then
      message_loc = message
    else
      message_loc = "Program terminated with exit code " //                   &
                    TOSTR(error_code) // "."
    end if

    !> The @ref intro_overview_lockfile "lock file" commondata::lock_file is
    !! closed and then deleted (commondata::file_delete()).
    !! See commondata::lock_file and commondata::lock_file_unit and
    !! @ref intro_overview_lockfile "The lock file" for details.
    !! @note Note that setting `ignore_lockfile` parameter to TRUE disables
    !!       checking and deleting the lock file.
    if (present(ignore_lockfile)) then
      do_not_touch_lock = ignore_lockfile
    else
      do_not_touch_lock = .FALSE.
    end if
    if ( do_not_touch_lock .eqv. .FALSE. ) then
      close(Lock_File_Unit, iostat=lock_iostat)
      if ( lock_iostat /= 0 ) then
        call LOG_MSG( LTAG_ERROR // "Cannot close lock file " // LOCK_FILE // &
                      ", it will NOT be deleted automatically!" )
      else
        call file_delete( LOCK_FILE )
      end if
    end if

    !> The logger is now issues the final messages and shuts down.
    call LOG_DELIMITER(LOG_LEVEL_VOLUME)
    call LOG_MSG( LTAG_MAJOR // "EXITING with message: " // message_loc )
    call LOG_DBG( LTAG_MAJOR // "Exit code returned to the OS: " //           &
                  TOSTR(error_code) )
    call LOG_DELIMITER(LOG_LEVEL_VOLUME)
    call LOG_SHUTDOWN()

    !> The final message goes to the standard error device.
    write(ERROR_UNIT,*) message_loc

    !> And the program terminates with specific exit code.
    select case (error_code)
      case (EXIT_CODE_ERROR)
        stop EXIT_CODE_ERROR
      case (EXIT_CODE_DEF)
        stop EXIT_CODE_DEF
      case default
        stop EXIT_CODE_DEF
    end select

  end subroutine system_halt

  !-----------------------------------------------------------------------------
  !> @brief   **logger_init** Initialise the system and the system logger.
  !! @details `logger_init` is called only once at `commondata::system_init()`
  !!          to set up the basic parameters and the logging facility.
  !!          For example, it sets the log file name, if timestamps should be
  !!          produced, if we need screen output, log delimiter characters and
  !!          any other similar parameters. They can be changed later if needed.
  !!          `logger_init` also writes some short initial information to the
  !!          log file like model name etc.
  subroutine logger_init()

    ! Subroutine/function name for DEBUG LOGGER. Every subroutine and function
    ! must contain a private parameter `PROCNAME` setting its name for
    ! the loger. But it is not used here.
    character (len=*), parameter :: PROCNAME = "(logger_init)"

    !> ### Implementation details ###
    !> #### Notable variables ####
    !> **logfile** character variable defines the file name for the main
    !! log output. Such a log file name is normally assembled from pieces,
    !! such as model name, date and time tag etc.
    ! @note Assigning logfile a specific name directly within this character
    !       variable declaration results in this gfortran internal
    !       **compiler error**:
    !       `f951: internal compiler error: in is_illegal_recursion, at
    !       fortran/resolve.c:1483
    !       Please submit a full bug report,
    !       with preprocessed source if appropriate.
    !       See <file:///usr/share/doc/gcc-4.8/README.Bugs> for instructions.`
    !       The same compiler error results if `logfile` is declared as a
    !       `parameter` constant (with assignment), the same with `(len=*)`
    !        or `(len=FILENAME_LENGTH)`.
    character (len=FILENAME_LENGTH) :: logfile

    !> **run_on_hostname** is an character string variable that keeps the
    !! hostname of the system the model is running on.
    !! @warning Hostname is determined using the `hostnm` function that is
    !!          non-standard and not portable. However, it is supported by
    !!          GNU and Intel Fortran compilers. If not supported by the
    !!          currently used compiler, call to this function should be
    !!          disabled in this procedure.
    character (len=FILENAME_LENGTH) :: run_on_hostname
    integer :: istat

    ! Logger raw parameters.
    integer :: logger_unit
    logical :: is_stdout, is_logfile

    ! Local counter
    integer :: i

    ! Model abstract from the abstract file
    character(len=LONG_LABEL_LENGTH), allocatable, dimension(:) ::            &
                                                          model_abstract_array

    ! @warning Assign value to `logfile` separately as doing this in the
    !          declaration line results in weird internal compiler error
    !!         with gfortran.
    logfile = "output_" // MODEL_NAME // "_" // MMDD // "_MAIN.log"
    logfile = "output_model_debug_MAIN.log" ! fixed name for debug only

    !...........................................................................
    !> #### The procedure ####
    !> We first set parameters of the log, switch timestamps, screen output
    !! when DEBUG=TRUE, set delimiters and file optionally unit.
    call LOG_CONFIGURE("timestamp", .TRUE.)         ! do timestamps in the log
    call LOG_CONFIGURE("writeonstdout" , IS_SCREEN_OUTPUT)

    !> Delimiters here have the length 60 character.
    call LOG_CONFIGURE("level_string_volume",     repeat("=",60)) ! delimiters
    call LOG_CONFIGURE("level_string_chapter",    repeat("-",60))
    call LOG_CONFIGURE("level_string_section",    repeat("*",60))
    call LOG_CONFIGURE("level_string_subsection", repeat("+",60))

    !> We then initialise the log and set the log file name. The second dummy
    !! parameter set to FALSE defines log file overwrite (don't append to the
    !! old log file).
    ! @note Note that we have to use `trim` to delete trailing blanks from
    !       the fixed length string.
    call LOG_STARTUP (trim(logfile), .FALSE.)

    !> We also print some some initial info to the log, like the name of the
    !! model.
    call LOG_DELIMITER(LOG_LEVEL_VOLUME)
    call LOG_MSG("Starting model:    " // MODEL_NAME // " --- Date Tag: " //  &
                 MMDD // ".")
    call LOG_MSG("Model description: " // MODEL_DESCR)
    call LOG_MSG("Model Revision string: " // SVN_VERSION_STRING)
    SVN_Version = parse_svn_version()       !< `SVN_Version` is global public.
    call LOG_MSG("      Revision ID: " // SVN_Version // ".")

    !> The Model Abstract is obtained from the abstract file
    !! commondata::model_abstract_file and logged.
    call LOG_DELIMITER(LOG_LEVEL_CHAPTER)
    model_abstract_array = parse_abstract()
    call LOG_MSG( "Model Abstract (" // TOSTR(size(model_abstract_array)) //  &
                  " lines):" )
    do i = 1, size( model_abstract_array )
      call LOG_MSG( model_abstract_array(i) )
    end do
    call LOG_DELIMITER(LOG_LEVEL_CHAPTER)

    !> Then detect the runtime platform and set integer platform ID that shows
    !! if we are running on Windows or Unix.
    if ( PLATFORM_IS_WINDOWS() ) then
      Platform_Running = PLATFORM_WINDOWS
      call LOG_MSG("  Running on Microsoft Windows platform: PLATFORM_WINDOWS.")
    else
      Platform_Running = PLATFORM_UNIX
      call LOG_MSG("  Running on a Unix/Linux platform: PLATFORM_UNIX " //    &
                   "(probably).")
    end if

    !> Determine the compiler version and the compiler parameters using the
    !! F2008 `compiler_version()` and `compiler_options()` inquiry functions.
    !! Because this functionality may not have been implemented in many
    !! current compilers (e.g. Intel Fortran 17), it is disabled here.
    ! @warning `compiler_version` and `compiler_options` are part of F2008 and
    !          may not be available on all compilers and systems. They are
    !          defined in `ISO_FORTRAN_ENV` (require `use ISO_FORTRAN_ENV`).
    ! @warning The F2008 `compiler_version()` and `compiler_options()` inquiry
    !          functions are **not** supported by Intel Fortran 2017. So
    !          **disabled** here.
    !call LOG_MSG("  Compiled by " // compiler_version() // " with options: " &
    !              // compiler_options() )

    !> Determine the hostname (computer name) the program is running on. Then
    !! log hostname if determined successfully.
    !! @warning Hostname is determined using the `hostnm` function that is
    !!          non-standard and not portable. However, it is supported by
    !!          GNU and Intel Fortran compilers. If not supported by the
    !!          currently used compiler, call to this function should be
    !!          disabled in this procedure. With Intel Fortran compiler,
    !!          using this functionality requires declaring.
    !!               use IFPORT, only : hostnm
    !!          Therefore, determining hostname is disabled so far.
    run_on_hostname = "undefined"
    !istat = hostnm( run_on_hostname )  ! @warning Nonstandard and not portable!
    !if ( istat == 0 ) then
    !  call LOG_MSG("  Hostname: " // trim(run_on_hostname))
    !end if

    call LOG_MSG("  DEBUG is: " // TOSTR(IS_DEBUG) //                         &
                 "; SCREEN is: " // TOSTR(IS_SCREEN_OUTPUT) //                &
                 "; PLOTTING is: " // TOSTR(IS_PLOTTING) )
    !> Print the logger parameters in to the log itself.
    call LOG_CGET("writeonstdout", is_stdout)
    call LOG_CGET("writeonlogfile", is_logfile)
    call LOG_CGET("logfileunit", logger_unit)
    call LOG_MSG("Logger configuration options:")
    call LOG_MSG("  Write standard output is " // TOSTR(is_stdout) //         &
                 ", write to logfile is " // TOSTR(is_logfile) //             &
                 " with unit number " // TOSTR(logger_unit))

    !> Print some basic constants, e.g. commondata::srp, commondata::hrp etc.
    call LOG_MSG("Basic constants: ")
    call LOG_MSG("   Standard (SRP) precision: " // TOSTR(SRP))
    call LOG_MSG("   High (HRP) precision: " // TOSTR(HRP))
    call LOG_MSG("   LONG integer precision: " // TOSTR(LONG))
    call LOG_MSG("   ZERO (SRP): " // TOSTR(ZERO))
    call LOG_MSG("   Smallest real value TINY_SRP: " // TOSTR(TINY_SRP))
    call LOG_MSG("   Default low tolerance TOLERANCE_LOW_DEF_SRP: " //        &
                 TOSTR(TOLERANCE_LOW_DEF_SRP))
    call LOG_MSG("   Default high tolerance TOLERANCE_HIGH_DEF_SRP: " //      &
                 TOSTR(TOLERANCE_HIGH_DEF_SRP))

    !> Print the main parameters of the model, population size etc.
    call LOG_MSG("Main parameters of the model:")
    call LOG_MSG("   Population size " // TOSTR(POPSIZE))
    call LOG_MSG("   Lifecycle length " // TOSTR(LIFESPAN))

    !> Print also some parameters of the Genetic Algorithm.
    call LOG_MSG("Genetic Algorithm parameters:")
    call LOG_MSG("   Pre-evolution selection of the best, proportion: " //    &
                 TOSTR(GA_REPRODUCE_PR) // ", N=" // TOSTR(GA_REPRODUCE_N)  )
    call LOG_MSG("   Pre-evolution life cycle starts from " //                &
                 TOSTR(PREEVOL_TSTEPS) // " cycles (full lifespan is " //     &
                 TOSTR(LIFESPAN) // ")" )

    !> Finally, issue a horizontal line delimiter into the log. This finishes
    !! initialising the logger.
    call LOG_DELIMITER(LOG_LEVEL_VOLUME)

  end subroutine logger_init

  !-----------------------------------------------------------------------------
  !> @brief `LOG_DBG`: debug message to the log. The message goes to the
  !!        logger only when running in the DEBUG mode.
  !! @param[in] message_string String text for the log message
  !! @param[in] procname Optional procedre name for debug messages
  !! @param[in] modname  Optional module name for debug messages
  !! @details This subroutine is a wrapper to `LOG_MSG()` from HEDTOOLS for
  !!          writing debug messages by the module `LOGGER`. The debug message
  !!          message defined by the `message_string` parameter is issued only
  !!          when the model runs in the @ref intro_debug_mode "debug mode",
  !!          i.e. if `IS_DEBUG=.TRUE.`
  !! @note    Standard `LOG_MSG()` procedure should be used for all logger
  !!          messages that are produced in the normal non
  !!          @ref intro_debug_mode "debug mode".
  !!          How the DEBUG mode is controlled is described in the
  !!          `commondata::system_init()` reference.
  subroutine LOG_DBG(message_string, procname, modname)

    ! @note PROCNAME is NOT used here as it conflicts with the dummy parameter.
    ! Calling parameters:
    ! @param[in] message_string String text for the log message
    character (len=*), intent(in) :: message_string
    ! @param[in] procname Optional procedre name for debug messages
    character (len=*), optional, intent(in) :: procname
    ! @param[in] modname  Optional module name for debug messages
    character (len=*), optional, intent(in) :: modname

    ! Local variables
    character (len=:), allocatable :: prefix_msg

    if (IS_DEBUG) then

      !> We first generate the message prefix = MODNAME PROCNAME
      !! if called with these parameters, so the location of the code
      !! in which the message has been issued is precisely known.
      if (present(procname)) then
        if (present(modname)) then
          prefix_msg="DEBUG: "   // modname // "::" //  procname // ": "
        else
          prefix_msg="DEBUG: ::" // procname // ": "
        end if
      else
        if (present(modname)) then
          prefix_msg="DEBUG: " // modname // ":: "
        else
          prefix_msg="DEBUG: "
        end if
      end if

      !> And then we issue the message to the log as usual.
      call LOG_MSG( prefix_msg // message_string )

    end if

  end subroutine LOG_DBG

  !-----------------------------------------------------------------------------
  !> Parse and cut revision **number** in form of string from the whole SVN
  !! revision string. SVN revision number can therefore be included into the
  !! model outputs and output file names. This is convenient because the model
  !! version is identified by a single SVN revision number.
  !! @returns  revision number from Subversion.
  !! @warning `STRINGS` module uses unsafe coding prone to bugs, e.g.
  !!           does not clearly state dummy parameters intent and doesn't
  !!           work correctly with `parameter`s.
  function parse_svn_version () result (sversion)
    character(len=:), allocatable :: sversion ! @returns revision number.
    ! @note Have to copy `SVN_VERSION_STRING` to this local variable as
    !       `PARSE` is broken when used with fixed parameters (no `intent`).
    character(len=len(SVN_VERSION_STRING)) ::                                 &
                                svn_string_copy = SVN_VERSION_STRING
    ! Delimiters for substrings.
    character(len=3) :: delims = " :$"
    ! String parts after parsing and cutting. Are 3 parts enough?
    character(len=len(SVN_VERSION_STRING)), dimension(3) :: sargs
    integer :: n_args

    !> ### Implementation notes ###
    !> Subversion has a useful feature: various keywords can be inserted and
    !! automatically updated in the source code under revision control, e.g.
    !! revision number, date, user etc. The character string parameter constant
    !! `commondata::svn_version_string` keeps the Subversion revision tag.
    !! This subroutine parses the tag striping all other characters out.
    call PARSE(svn_string_copy, delims, sargs, n_args)
    sversion = trim(sargs(n_args))      ! Version number is the last part.

  end function parse_svn_version

  !-----------------------------------------------------------------------------
  !> Get and parse the model abstract. Model abstract is a short descriptive
  !! text that can span several lines and is kept in a separate file that is
  !! defined by the commondata::model_abstract_file.
  !!
  !! The separate Model Abstract file is useful because it can integrate
  !! dynamic information, such as the latest version control log(s) via
  !! Subversion or Mercurial hooks mechanism.
  function parse_abstract(file_name) result (abstract_array)
    !> @param[in] file_name optional name of the abstract file. If this
    !!            parameter is absent, commondata::model_abstract_file is used.
    character(len=*), optional, intent(in) :: file_name
    !> @return An allocatable character string array that contains all the
    !!         lines from the Model Abstract file.
    character(len=LONG_LABEL_LENGTH),allocatable,dimension(:) :: abstract_array

    ! Local copies of optionals
    character(len=:), allocatable :: file_name_loc

    ! Local variables for reading the abstract file
    integer :: abstract_unit, file_error_status
    logical :: file_is_success

    ! Number of lines in the abstract file, it is also equal to the size of
    ! the output abstract character string array.
    integer :: abstract_file_lines

    ! Counter.
    integer :: i

    if (present(file_name)) then
      file_name_loc = file_name
    else
      file_name_loc = MODEL_ABSTRACT_FILE
    end if

    !> Try to open the abstract file and count its lines with the function
    !! CSV_FILE_LINES_COUNT() from HEDTOOLS.
    abstract_file_lines = CSV_FILE_LINES_COUNT(                               &
                                  csv_file_name=file_name_loc,                &
                                  numeric_only=.FALSE.,                       &
                                  csv_file_status=file_is_success )

    !> - If the function return status is 'error' or the number of lines is
    !!   less than 1, the abstract file is invalid and the abstract array is
    !!   automatically allocated to one and assigned the model description
    !!   string from commondata::model_descr.
    if ( .not. file_is_success .or. abstract_file_lines < 1 ) then
      abstract_array = [ MODEL_DESCR ]
      return
    end if

    !> - Otherwise,
    !!   - allocate the output abstract array to the number of lines
    !!     in the abstract file and initialise it to empty strings
    allocate( abstract_array(abstract_file_lines) )
    abstract_array = ""

    !>   - get the free file unit for reading...
    abstract_unit = GET_FREE_FUNIT()

    !>   - open the abstract file for reading
    open ( unit=abstract_unit, file=file_name_loc, status='old',              &
           iostat=file_error_status)

    if ( file_error_status /= 0 ) then
      abstract_array = [ MODEL_DESCR ]
      return
    end if

    !>   - And read all the contents of the file into the output abstract array.
    do i=1, abstract_file_lines
      read ( unit=abstract_unit, fmt="(a)", iostat=file_error_status )        &
                                                              abstract_array(i)
      if ( file_error_status /= 0 ) exit
    end do

    !>   - Finally, close the file.
    !!   .
    !! .
    close(abstract_unit)

  end function parse_abstract

  !-----------------------------------------------------------------------------
  !> Date (YYYYMMDD) tag for file names and logs.
  !! @warning Note that this procedure should be called only once during the
  !!          system initialisation. Then a fixed value is given to the global
  !!          variable commondata::mmdd: this global variable should be used
  !!          for all tags. This procedure has *private* accessibility status
  !!          and is not available outside of @ref commondata.
  function tag_mmdd() result (MMDD)

    character (len=*), parameter :: PROCNAME = "(tag_mmdd)"
    !> @retval MMDD Function returns an 8-character string for YYYYMMDD
    character(len=:), allocatable :: MMDD

    integer date_time(8)
    character(10) b(3)

    call date_and_time(b(1), b(2), b(3), date_time)

    MMDD = TOSTR(date_time(1),2016) // TOSTR(date_time(2),10) //              &
           TOSTR(date_time(3),10)

  end function tag_mmdd

end module COMMONDATA
