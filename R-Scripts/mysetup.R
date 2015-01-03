#setup of R
# Suitable for edx and probably coursera
# need additional package for U Austin Course


install.packages("XML")
install.packages("RJSONIO")
install.packages("Rcpp", dependencies = TRUE)
install.packages("httr")
install.packages("maps")
install.packages("reshape2")
install.packages("RCurl")
install.packages("devtools", dependencies = TRUE)
devtools::install_github("sweSCB","rOpenGov")
install.packages("calibrate")
install.packages("pheatmap")
install.packages("gplots")

# install bioconductor packages
# warning: compiling DESeq2 may take a long time
# do NOT interrupt the process!

source("http://bioconductor.org/biocLite.R")

# run following biocLite("BiocUpgrade") 
# if you are upgrading from an older Bioconductor version
# to Bioconductor version 3.0 (BiocInstaller 1.16.0)
# The current release of Bioconductor is version 3.0 
# it works with R version 3.1.1
# Users of older R and Bioconductor versions must update their installation
# to take advantage of new features.

biocLite("BiocUpgrade")
biocLite("Biobase",dependencies=TRUE)
biocLite("DESeq2",dependencies=TRUE) 
biocLite("biomaRt")
biocLite("org.Hs.eg.db")
biocLite("topGO",dependencies=TRUE)
biocLite("Rsubread")
biocLite("Rgraphviz")
biocLite("GEOquery")
biocLite("limma")

library(datasets)
