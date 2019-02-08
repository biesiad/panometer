set datafile separator ','

input = ARG1
output = ARG2

set title input
set ylabel 'Height [mm]'
set y2label 'Temperature [*C]'
set xlabel 'Time [H:M]'

set terminal jpeg
set output output

set tics front
set ytics nomirror
set ytics 0, 5
set yrange [0:60]
set y2tics 0, 10
set y2range [0:30]

set xtics nomirror
set timefmt "%s"
set xdata time
set format x "%H:%M"
count = system("wc -l <".input)
first = system("expr $(head -n1 ".input." | cut -d, -f1) - $(head -n1 ".input." | cut -d, -f1) % 3600")
last = system("tail -n1 ".input." | cut -d, -f1")
# set xtics first, floor(count*2), last
set xtics first, 2*3600,last

plot input \
     using 1:4 title 'Height' \
     smooth bezier \
     with filledcurve x1 lc rgbcolor '#dddddd' \
     axis x1y1, \
     '' \
     using 1:2 title 'Temperature' \
     smooth bezier \
     lw 2 lc rgbcolor '#555555' \
     axis x1y2
