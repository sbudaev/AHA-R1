# Introduction and Getting Started #

## The AHA Model ###

This is a large scale simulation model (under development) that implements
a general **decision-making architecture** in **evolutionary agents**. Each
agent is programmed as a whole virtual organism including the genome,
rudimentary physiology, the hormonal system, a cognitive architecture and
behavioural repertoire. They "live" in a stochastic spatially explicit
virtual environment with physical gradients, predators and prey. The primary
aim of the whole modelling machinery is to understand the evolution of
decision making mechanisms, personality, emotion and behavioural plasticity
within a realistic ecological framework. An object-oriented approach coupled
with a highly modular design not only allows to cope with increasing layers
of complexity inherent in such a model system but also provides a framework
for the system generalizability to a wide variety of systems. We also use
a "physical-machine-like" implementation philosophy and a coding standard
integrating the source code with parallel detailed documentation that
increases understandability, replicability and reusability of this model
system.

The cognitive architecture of the organism is based on a set of
motivational (emotional) systems that serves as a common currency for
decision making. Then, the decision making is based on **predictive
assessment** of external and internal stimuli as well as the agent's own
motivational (emotional) state. The agent makes a subjective assessment
and selects, from the available repertoire, the behaviour that would reduce
the expected motivational (emotional) arousal. Thus, decision making is
based on predicting one's own internal state. As such, the decision-making
architecture integrates motivation, emotion, and a very simplistic model
of consciousness.

The **purpose** of the AHA model is to investigate a general framework
for modelling proximate decision-making and behavior. From this we will
investigate adaptive goal-directed behaviour that is both guided by the
external environment and still is endogeneously generated.

Other research topics include individual differences, personality as well
as consequences of emotion and personality to population ecology.

We think that understanding and modelling complex adaptive behaviour requires
both extraneous (environmental) factors and stimuli as well as endogeneous
mechanisms that produce the behaviour. Explicit proximate representation
of the motivation and emotion systems, self-prediction can be an important
component in linking environment, genes, physiology, behavior, personality
and consciousness.

Fortran is used due to its simplicity and efficiency. For example, check out
[this paper](http://www.moreisdifferent.com/2015/07/16/why-physicsts-still-use-fortran).

--------------------------------------------------------------------------------

## Subdirectories of the AHA Model code ##

- `\dox` -- contains pictures, plots and other resources for the full Doxygen
  documentation that is extracted from the source code.
- `\pfunit` -- unit tests using [pFUnit](http://pfunit.sourceforge.net/), so
  far rather rudimentary.
- `\tools` -- various accessory tools, post-processing of the data generated
  by the model etc.
.

--------------------------------------------------------------------------------

## Getting started ##

Building and running the mode is based on the GNU Make. Both the **AHA Model**
code and the **HEDTOOLS** (code or static library) are needed to build the
model. These two components of the AHA Model framework are described in the
[Overview of the AHA Fortran modules](http://ahamodel.uib.no/doxydoc/index.html#intro_overview_modules).

Normally, the *AHA Model* code and the *HEDTOOLS* code are placed in two
subdirectories of the working directory using two separate commands to get the
code of the AHA Model and HEDTOOLS from the repositories (`svn co https://...`
or `hg clone ssh://...` if Mercurial is used -- check out
[Subversion](http://ahamodel.uib.no/doc/ar01s04.html)).

With the current main Subversion repository, getting the code requires these
commands:

    svn co https://subversion.uib.no/repos/aha-fortran/tags/AHA_R/R1
    svn co https://subversion.uib.no/repos/aha-fortran/tags/HEDTOOLS/1.1

or these, if Mercurial is used (here local folders are capital):

    hg clone https://bitbucket.org/teg_uib/aha-r1 AHA-R1
    hg clone ssh://hg@bitbucket.org/teg_uib/hedtools HEDTOOLS

or still these, if GitHub git-based repo is used:

    git clone git@github.com:sbudaev/AHA-R1.git
    git clone git@github.com:sbudaev/HEDTOOLS.git

Thus, the layout of the working directory after `HEDG2_01` and `HEDTOOLS` are
downloaded is like this:

    workdir
     |
     |-- HEDG2_01
     |    |-- dox
     |    |-- pfunit
     |    `-- tools
     |
     `-- HEDTOOLS

Building the AHA Model is done in the model directory (here `HEDG2_01`). If
you use the terminal, go there with

    cd HEDG2_01

Here are the main commands to build and run the AHA Model:

- *Build* the model from the source code: `make`;
- Build and *run* the model:`make run`;
- Delete all build-related, data and temporary files `make distclean`;
- Get a quick help from the make system: `make help`.

See [Makefile](http://ahamodel.uib.no/doxydoc/Makefile.html) for build
configuration. To get more information on the GNU Make see
[AHA Modelling Tools Manual](http://ahamodel.uib.no/doc/ar01s13.html),
[Using Microsoft Visual Studio](http://ahamodel.uib.no/doc/ar01s14.html) and
[Using Code::Blocks IDE](http://ahamodel.uib.no/doc/ar01s15.html).

## Environment variables ##

The model makes use of several environment variables to control certain
global aspects of the execution. These variables have the `AHA_` prefix
and are listed below.

- `AHA_DEBUG=TRUE` sets the "debug mode";
- `AHA_SCREEN=NO` sets logger to write to the standard output as well
   as in the log file;
- `AHA_DEBUG_PLOTS=YES` enables generation of debug plots;
- `AHA_ZIP_FILES=YES` enables background compression of big output data files;
- `AHA_CHECK_EXTERNALS=NO` disables checking for external executable
   modules (this is a workaround against Intel Fortran compiler bug).

## Makefile: GNU Make build configuration ##

[Makefile](http://ahamodel.uib.no/doxydoc/Makefile.html) to build the model
executable from the source code.

This is a standard GNU Make
[Makefile](http://ahamodel.uib.no/doxydoc/Makefile.html) to build the model from
the sources. It also automatically generates some platform and
compiler specific include files for the `BASE_RANDOM` and the
IEEE math modules. See
[Working with the model](http://ahamodel.uib.no/doxydoc/index.html#intro_main)
section for details.

In addition to the GNU Make, this
[Makefile](http://ahamodel.uib.no/doxydoc/Makefile.html) also depends on the
`grep`, `cut`, `sed` and `awk` utilities that are installed on any
Unix/Linux system, but usually absent on Windows. For Windows
they can be obtained from several locations on the web, e.g.
Cygwin or GnuWin32. See
[AHA Modelling Tools Manual](http://ahamodel.uib.no/doc/ar01s01.html)
for more information.

### Main adjustable parameters ###

- **FC** sets the default compiler type. Normally GNU `gfortran` or Intel
  `ifort`.
- **HOST_HPC_ROOT** is the hostname to run the model executable in the
  HPC batch mode. If the hostname the
  [Makefile](http://ahamodel.uib.no/doxydoc/Makefile.html) is called in is this
  system, `make run` starts a new batch task. Otherwise, the model
  executable is just started normally.
- **SRC** is the name of the main source code (can be several files).
- **DRV** is the source code of the model "driver", that is the main
  Fortran program that produces the executable. It should be very
  very small. The "driver" is a separate file from the AHA Model
  modules.
- **OUT** is the executable file name that is actually run, on the
  Windows platform must have the `.exe` suffix. By default, the
  executable name is set to `MODEL.exe` for maximum portability.
- **DOXYCFG** the Doxygen documentation system configuration file name.
- **DOXYPATH** defines the Doxygen output directory where the
  documentation is generated. This could be set manually or parsed
  automatically using the `grep` command. Note that the output
  directory is set in the `Doxyfile` as:
  `OUTPUT_DIRECTORY = ./model_docs`.
- **HEDTOOLS** is the list of HEDTOOLS Fortran source files that are
  used in the AHA Model. Note that `BASE_STRINGS` in the list must
  go before `BASE_CSV_IO` because `BASE_CSV_IO` depends on
  procedures from `BASE_STRINGS`.
- **HEDTOOLSDIR** defines the path to the HEDTOOLS source code.
- **IEEEPATH** defines the path to the IEEE non-intrinsic math modules
  (part of HEDTOOLS).
- **WINRM** defines the command that is used to delete files and
  directories on the Windows platform. The native file delete command on
  the Windows platform is `del` or `erase`. However, if Cygwin is used,
  this native file removal command may not call with a "command not found"
  error. In such a case, use the Unix `rm` tool provided by Cygwin.
.

The source code of the [Makefile](http://ahamodel.uib.no/doxydoc/Makefile.html)
contains many other parameters and is well documented throughout.
