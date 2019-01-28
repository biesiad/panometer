set datafile separator ','

set title ARG1
set ylabel 'Height [mm]'
set y2label 'Temperature [*C]'
set xlabel 'Time [H:M]'

set terminal jpeg
set output ARG2

set ytics nomirror

set ytics 0, 5
set yrange [0:60]

set y2tics 0, 10
set y2range [0:30]

set timefmt "%s"
set xdata time
set format x "%H:%M"
set xtics nomirror

plot ARG1 \
     using 1:4 title 'Height' \
     smooth bezier \
     with filledcurve x1 lc rgbcolor '#dddddd' \
     axis x1y1, \
     '' \
     using 1:2 title 'Temperature' \
     smooth bezier \
     lw 2 lc rgbcolor '#555555' \
     axis x1y2




