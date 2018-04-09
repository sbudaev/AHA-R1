#!/usr/bin/gnuplot
#
# Plot the latest trajectory of the agent from the position history stack
# using *gnuplot*.
#
# Call (csv data is provided on the cmd with -e switch):
#  gnuplot -e "fname='file_name.csv'"  pos_hist.gnuplot
#-------------------------------------------------------------------------------
# $Id$

if ( ! exists("fname") ) fname='data.csv'
print "File is: ", fname

set term svg
set grid
set style data lines

# Data separator for csv is comma
set datafile separator ","

set xlabel "X"
set ylabel "Y"
set zlabel "Z"

# 2d plot:
set output fname . "_motion_2d.svg"
set title "Agent movement in 2D"
plot fname using 1:2

# 3d plot:
set output fname . "_motion_3d.svg"
set title "Agent movement in 3D"
splot fname using 1:2:3 with lines

quit
