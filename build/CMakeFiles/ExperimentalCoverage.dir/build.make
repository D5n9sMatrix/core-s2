# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.22

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/denis/freebasic/core-s2

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/denis/freebasic/core-s2/build

# Utility rule file for ExperimentalCoverage.

# Include any custom commands dependencies for this target.
include CMakeFiles/ExperimentalCoverage.dir/compiler_depend.make

# Include the progress variables for this target.
include CMakeFiles/ExperimentalCoverage.dir/progress.make

CMakeFiles/ExperimentalCoverage:
	/usr/bin/ctest -D ExperimentalCoverage

ExperimentalCoverage: CMakeFiles/ExperimentalCoverage
ExperimentalCoverage: CMakeFiles/ExperimentalCoverage.dir/build.make
.PHONY : ExperimentalCoverage

# Rule to build all files generated by this target.
CMakeFiles/ExperimentalCoverage.dir/build: ExperimentalCoverage
.PHONY : CMakeFiles/ExperimentalCoverage.dir/build

CMakeFiles/ExperimentalCoverage.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/ExperimentalCoverage.dir/cmake_clean.cmake
.PHONY : CMakeFiles/ExperimentalCoverage.dir/clean

CMakeFiles/ExperimentalCoverage.dir/depend:
	cd /home/denis/freebasic/core-s2/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/denis/freebasic/core-s2 /home/denis/freebasic/core-s2 /home/denis/freebasic/core-s2/build /home/denis/freebasic/core-s2/build /home/denis/freebasic/core-s2/build/CMakeFiles/ExperimentalCoverage.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/ExperimentalCoverage.dir/depend

