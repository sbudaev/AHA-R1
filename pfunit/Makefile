.PHONY: clean tests all
.DEFAULT_GOAL = tests

# Main source files of the AHA Model. This should be the same as in the
# main Makefile at the upper level of the directory structure.
SRC = m_common.f90 m_fileio.f90 m_env.f90 m_genome.f90 m_hormon.f90 \
      m_body.f90 m_neuro.f90 m_indiv.f90 m_popul.f90 m_evolut.f90

# Source names for tests in \tests dir
TESTS_SRC = testSuites.inc commondata.pf

TOP_DIR := $(shell pwd)
SRC_DIR=$(TOP_DIR)/..
TEST_DIR=$(TOP_DIR)/tests

VPATH = . $(SRC_DIR) $(TEST_DIR)

include $(PFUNIT)/include/base.mk

ifeq ($(USEMPI),YES)
   FC=mpif90
endif

EXE = tests$(EXE_EXT)

# NOTE: use the -robust flag
all: $(EXE)
ifeq ($(USEMPI),YES)
	echo  Warning:  Testing -robust flag with MPI, robust will override.
	echo            driver.F90 will call mpirun on itself with robust set.
	echo            "Top" level mpirun not needed, may be problemmatic.
	./$(EXE) -robust
	# mpirun -np 1 ./$(EXE) -robust
else
	./$(EXE) -robust -max-timeout-duration 120 -max-launch-duration 360
endif

SUT:
	make -C $(SRC_DIR) objects
	make -C $(TEST_DIR) tests

tests: all

$(EXE): $(TESTS_SRC) $(SRC) SUT
	$(FC) -o $@ -I$(PFUNIT)/mod -I$(PFUNIT)/include -Itests $(PFUNIT)/include/driver.F90 $(TEST_DIR)/*$(OBJ_EXT) $(SRC_DIR)/*$(OBJ_EXT) -L$(PFUNIT)/lib -lpfunit $(LIBS) $(FFLAGS) $(FPPFLAGS)

clean: local-E0-clean

local-E0-clean:
	make -C $(SRC_DIR) clean
	make -C $(TEST_DIR) clean
	rm -f tests.x
	rm -f *conflict* .syncthing* ~syncthing* fort.* *.tmp *.orig *.csv

export FC
export FPPFLAGS
export FFLAGS
export SRC_DIR
export TEST_DIR
export OBJ_EXT
export LIB_EXT
export EXE_EXT
