pdf:
	xelatex -halt-on-error -output-directory output mnesia.tex

clean:
	cd output; rm *.aux *.log *.nav *.out *.snm *.toc *.vrb

show:
	evince output/mnesia.pdf
