# Determine package name and version from DESCRIPTION file
PKG_VERSION=$(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

# Name of built package
PKG_TAR=$(PKG_NAME)_$(PKG_VERSION).tar.gz

# Install package
install:
	cd .. && R CMD INSTALL $(PKG_NAME)

# Build documentation with roxygen
# 1) Remove old doc
# 2) Generate documentation
roxygen:
	rm -f man/*.Rd
	cd .. && Rscript -e "library(roxygen2); roxygenize('$(PKG_NAME)')"

# Generate PDF output from the Rd sources
# 1) Rebuild documentation with roxygen
# 2) Generate pdf, overwrites output file if it exists
pdf: roxygen
	cd .. && R CMD Rd2pdf --force $(PKG_NAME)

vignette:
	cd vignettes && Rscript -e "Sweave('mill'); tools::texi2pdf('mill.tex')"

# Generate README
README.md: README.Rmd
	Rscript -e "library(knitr); knit('README.Rmd')"

# Build and check package
check:
	cd .. && R CMD build $(PKG_NAME)
	cd .. && _R_CHECK_CRAN_INCOMING_=FALSE R CMD check \
          --no-stop-on-test-error --as-cran --run-dontrun $(PKG_TAR)

.PHONY: install roxygen pdf check vignette
