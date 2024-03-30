library(shiny)
library(DT)
library(dplyr)
library(rmarkdown)
library(knitr)
rm(list=ls())


#on.exit(setwd(tempdir()))

Dir<-'' ##Agregar el directorio de la aplicación

source(paste0(Dir, '\\incertidumbrePropagacion.r'))
source(paste0(Dir, '\\FuncionesMC.r'))
source(paste0(Dir, '\\ModulosUI.r'))
source(paste0(Dir, '\\ModulosServer.r'))

shinyApp(ui = ui, server = server)
