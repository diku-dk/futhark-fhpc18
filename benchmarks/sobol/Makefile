GNUPLOT_SETTINGS=set nokey; set size square; set term png size 300,300; set format x ''; set format y ''; set lmargin 0; set rmargin 0; set tmargin 3; set bmargin 0.5; unset xtics; unset ytics; set title font 'Ariel,15'

all: run

%.exe: %.fut
	futhark-c -o $@ $<

%_32.dat: %.exe
	echo 32 | ./$< | ./2dconv.sh > $@

%_32.dat: %.exe
	echo 32 | ./$< | ./2dconv.sh > $@

%_200.dat: %.exe
	echo 200 | ./$< | ./2dconv.sh > $@

%_200.dat: %.exe
	echo 200 | ./$< | ./2dconv.sh > $@

%_1000.dat: %.exe
	echo 1000 | ./$< | ./2dconv.sh > $@

%_1000.dat: %.exe
	echo 1000 | ./$< | ./2dconv.sh > $@

%_10000.dat: %.exe
	echo 10000 | ./$< | ./2dconv.sh > $@

%_10000.dat: %.exe
	echo 10000 | ./$< | ./2dconv.sh > $@

sobol_2d_32.png: sobol_2d_32.dat
	gnuplot -e "$(GNUPLOT_SETTINGS); set title 'Sobol - 32 points'; set output 'sobol_2d_32.png'; p 'sobol_2d_32.dat' w p pt 7 ps 0.3 lc rgb 'black'"

random_2d_32.png: random_2d_32.dat
	gnuplot -e "$(GNUPLOT_SETTINGS); set title 'Random - 32 points'; set output 'random_2d_32.png'; p 'random_2d_32.dat' w p pt 7 ps 0.3 lc rgb 'black'"

sobol_2d_200.png: sobol_2d_200.dat
	gnuplot -e "$(GNUPLOT_SETTINGS); set title 'Sobol - 200 points'; set output 'sobol_2d_200.png'; p 'sobol_2d_200.dat' w p pt 7 ps 0.3 lc rgb 'black'"

random_2d_200.png: random_2d_200.dat
	gnuplot -e "$(GNUPLOT_SETTINGS); set title 'Random - 200 points'; set output 'random_2d_200.png'; p 'random_2d_200.dat' w p pt 7 ps 0.3 lc rgb 'black'"

sobol_2d_1000.png: sobol_2d_1000.dat
	gnuplot -e "$(GNUPLOT_SETTINGS); set title 'Sobol - 1000 points'; set output 'sobol_2d_1000.png'; p 'sobol_2d_1000.dat' w p pt 7 ps 0.2 lc rgb 'black'"

random_2d_1000.png: random_2d_1000.dat
	gnuplot -e "$(GNUPLOT_SETTINGS); set title 'Random - 1000 points'; set output 'random_2d_1000.png'; p 'random_2d_1000.dat' w p pt 7 ps 0.2 lc rgb 'black'"

sobol_2d_10000.png: sobol_2d_10000.dat
	gnuplot -e "$(GNUPLOT_SETTINGS); set title 'Sobol - 10.000 points'; set output 'sobol_2d_10000.png'; p 'sobol_2d_10000.dat' w p pt 7 ps 0.1 lc rgb 'black'"

random_2d_10000.png: random_2d_10000.dat
	gnuplot -e "$(GNUPLOT_SETTINGS); set title 'Random - 10.000 points'; set output 'random_2d_10000.png'; p 'random_2d_10000.dat' w p pt 7 ps 0.1 lc rgb 'black'"

.PHONY: run_sobol
run_sobol: sobol_2d.png
	qlmanage -p $<

%.png: %_32.png %_200.png %_1000.png %_10000.png
	convert $^ +append $@

.PHONY: run_random
run_random: random_2d.png
	qlmanage -p $<

all.png: random_2d.png sobol_2d.png
	convert $^ -append $@

.PHONY: run
run: all.png
	qlmanage -p $<

clean:
	rm -rf *.png *.exe *.dat *~ *.c

pi_%.dat: sobol_pi2.exe random_pi.exe
	/bin/echo -n $* > $@; /bin/echo -n ' ' >> $@
	/bin/echo $* | ./sobol_pi2.exe | sed -e "s/f64//" | tr -d '\n' >> $@
	/bin/echo -n ' ' >> $@
	/bin/echo $* | ./random_pi.exe | sed -e "s/f64//" >> $@

pi.dat: pi_4000.dat pi_8000.dat pi_16000.dat pi_32000.dat pi_64000.dat pi_128000.dat pi_256000.dat pi_500000.dat pi_1000000.dat pi_2000000.dat pi_4000000.dat pi_8000000.dat
	cat $^ > $@

GNUPLOT_SETTINGS2=set logscale x; set term png size 1024,768; set title font ',24'; set key font ',24'; set xlabel font ',18'; set xlabel 'Samples'; set xlabel offset 38,1.5; set tics font ',18'
PI_CONF=$(GNUPLOT_SETTINGS2); set output 'pi.png'; set notitle

pi.png: pi.dat
	gnuplot -e "$(PI_CONF); plot 'pi.dat' using 1:2 title 'Sobol' w lp pt 7 lw 2 ps 1.5 lc rgb 'blue', '' using 1:3 title 'Random' w lp pt 7 lw 2 ps 1.5 lc rgb 'red', 'pi.exact' using 1:2 title 'Pi' w lp pt 7 lw 2 ps 0.1 lc rgb 'orange'"
