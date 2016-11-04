MASTER = test_ranger_runtime

all: $(MASTER).pdf

final: all
	cp $(MASTER).pdf pdf/`echo $(MASTER) | sed -e "s/-main//"`-`date +"%Y_%m_%d"`.pdf

$(MASTER).tex: $(MASTER).Rnw *.R Makefile
	echo "library(knitr); knit('$<')" | R --no-save

$(MASTER).pdf: $(MASTER).tex
	latexmk -pdf "$<"

clean:
	rm -f  $(MASTER).{tex,log,out,pdf,toc,aux,fdb_latexmk,fls,synctex.gz}  *~ */*~ cache/*
