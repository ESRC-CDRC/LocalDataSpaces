#library(data.table)
require(knitr)
require(rmarkdown)
gc()
#- Clear workspace
rm(list=ls())
dir <- "."
allFiles <- list.files(dir)
rDir <- normalizePath(dirname(allFiles[1])) 
setwd(rDir)
reportRmd <- list.files(path=rDir,pattern = glob2rx('*.Rmd'))
rmarkdown::render(reportRmd,'html_document')

#reportHTML <- list.files(path=rDir,pattern = glob2rx('*.html'))
#browseURL(reportHTML)