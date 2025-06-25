## Rautoml package re-factor

## https://cygubicko.github.io/Rautoml/index.html 

current: target
-include target.mk

vim_session:
	bash -cl "vmt"

######################################################################

Sources += $(wildcard *.md .*.yml)
Sources += $(wildcard vignettes/*.md)
Sources += $(wildcard vignettes/*.Rmd)
Sources += $(wildcard *.R R/*.R)
Sources += $(wildcard man/*.Rd) NAMESPACE DESCRIPTION
Sources += $(wildcard man/figures/*)
Sources += $(wildcard docs/*)
Sources += $(wildcard docs/news/*)
Sources += $(wildcard docs/reference/*)
Sources += $(wildcard docs/articles/*)
Sources += .Rbuildignore

Ignore += README.html
Ignore += *.md.args
Ignore += vignettes/.gitignore
Ignore += vignettes/*.pdf
Ignore += .gitignore
Ignore += Rautoml_*.*.tar.gz

######################################################################

## We don't need to preserve yml as defined in $(knitmd)
makemd = echo "library(rmarkdown); render(\"$^\", \"md_document\")" | R --slave
makepdf = echo "library(rmarkdown); render(\"$^\", \"pdf_document\")" | R --slave

autopipeR = defined

Sources += README.md 
Ignore += README.Rmd 
#README.md: README.Rmd
#	$(makemd)

vignettes/Rautoml_intro.pdf: vignettes/Rautoml_intro.Rmd
	$(makepdf)

######################################################################

## Package main functions
methods.Rout: R/methods.R
utilities.Rout: R/utilities.R
plotfuns.Rout: R/plotfuns.R
custom_barplot.Rout: R/custom_barplot.R
custom_piechart.Rout: R/custom_piechart.R
custom_histogram.Rout: R/custom_histogram.R
custom_boxplot.Rout: R/custom_boxplot.R
custom_linegraph.Rout: R/custom_linegraph.R
pivottable_custom.Rout: R/pivottable_custom.R
custom_crosstab.Rout: R/custom_crosstab.R
custom_scatterplot.Rout: R/scatterplot_custom.R
custom_violin.Rout: R/custom_violin.R
preprocessing_utilities.Rout: R/preprocessing_utilities.R

######################################################################

## install required packages for vignettes 

quickinstall:
	rm Rautoml_1.0.0.*.gz
	R CMD build .
	make install-tarball

update:
	make vignettes/Rautoml_intro.pdf
	make install && make README.md
	make pkg-site

install:
	make update-doc && make check-package && make quickinstall

pkg-site:
	echo "pkgdown::build_site()" | R --slave

install-tarball:
	R CMD INSTALL Rautoml_1.0.0.*

check-package:
	echo "devtools::check('.')" | R --slave

update-doc:
	echo "devtools::document('.')" | R --slave

######################################################################

### Makestuff

Sources += Makefile

## Sources += content.mk
## include content.mk

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls makestuff/Makefile

makestuff/%.stamp:
	- $(RM) makestuff/*.stamp
	(cd makestuff && $(MAKE) pull) || git clone $(msrepo)/makestuff
	touch $@

-include makestuff/os.mk

-include makestuff/chains.mk
-include makestuff/texi.mk
-include makestuff/pipeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk
