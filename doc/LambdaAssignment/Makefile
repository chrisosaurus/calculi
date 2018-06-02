.POSIX:

all: clean semantics.pdf

semantics.pdf: clean
	# generate all needed files
	TEXFORMATS="../deps:" TEXINPUTS="../deps:" pdflatex semantics.tex
	TEXFORMATS="../deps:" TEXINPUTS="../deps:" pdflatex semantics.tex
	TEXFORMATS="../deps:" TEXINPUTS="../deps:" pdflatex semantics.tex

clean:
	for EXT in aux pdf out log toc tbc lot lof dvi fls fdb_latexmk bbl blg; do\
		find . -iname "*$${EXT}" -delete;\
	done;

view: clean semantics.pdf
	xdg-open semantics.pdf &

.PHONY: all clean view
