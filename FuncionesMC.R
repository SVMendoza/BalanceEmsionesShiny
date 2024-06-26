## Renderizar tablas editables


renderDatosIngresados <- function(data, namess, captation) {
 
    DT::datatable(data, 
                  options = list(pageLength = 12, autoWidth = TRUE, searching = FALSE),
                  rownames = TRUE,
                  selection = 'none',
                  editable = 'cell',  
                  colnames = namess, 
                  caption = captation)
 
}

# Función para limpiar archivos temporales
limpiarArchivosTemporales <- function() {
  archivos_temporales <- c("reporte.xlsx", "reporte.pdf", 
                           "reporte.docx",'report.html','datos.rds','reporte.rmd','reporte.tex')
  
  # Elimina cada archivo temporal si existe
  for (archivo in archivos_temporales) {
    path <- file.path(getwd(), archivo)
    if (file.exists(path)) {
      file.remove(path)
    }
  }
}

################################################
## contenedor de datos

ContenedorDatos <- R6::R6Class("ContenedorDatos",
                           public = list(
                             Datos = list(),
                             
                             guardarTabla = function(nombre, tabla) {
                               self$Datos[[nombre]] <- as.data.frame(tabla)
                             },
                             
                             obtenerDatos = function(nombre) {
                               return(self$Datos[[nombre]])
                             },
                             DeleteDatos = function(nombre) {
                               if(is.null(nombre)) {self$Datos <- list()}
                               else {
                               if (nombre %in% names(self$Datos)) {
                                 self$Datos[[nombre]] <- NULL
                               } else {
                               self$Datos <- list()
                               }
                               }
                             }
                           )
)

# Crear una instancia de la clase ContenedorDatos
Contener <- ContenedorDatos$new()

###################################################
#### archivo markdown funcion
fRmd<-function(tempF, formato, Nombre, Reporte) {
  Fec<-Sys.Date()
  paste0("---\n",
         "title: ",Reporte,"\n",
         "author: ", Nombre, "\n",
         "date: ", Fec, "\n",
         "output: ", formato, "_document\n",
         "encoding: UTF-8\n",
         "---\n\n",
         "## Estimaciones\n\n",
         "### Reservorios\n\n",
         "```{r setup, include=FALSE}\n",
         "knitr::opts_chunk$set(echo = TRUE)\n",
         "L<-readRDS(file='",tempF,"')\n",
         "```\n\n",
         "```{r, echo=FALSE, warning=FALSE, message=FALSE}\n",
         "knitr::kable(L[[1]], caption = 'Resevorios $tC ha^-1$ ')\n",
         "knitr::kable(L[[2]], caption = 'Total de estimacines $tCO2e ha$')\n\n",
         "```\n\n","### Datos de actividad:\n\n",
         
         "```{r, echo=FALSE, warning=FALSE, message=FALSE}\n",
         "knitr::kable(L[[3]], caption = 'Referencia $ha  yrs^-1$')\n",
         "knitr::kable(L[[4]], caption = 'Resultados $ha  yrs^-1$')\n\n",
         "```\n\n",
         "```{r, echo=FALSE, warning=FALSE, message=FALSE}\n",
         "knitr::kable(L[[5]], caption = 'Referencia $(tCO2e  yrs^-1)$ Monte Carlo')\n",
         "knitr::kable(L[[6]], caption = 'Resultados $(tCO2e  yrs^-1)$ Monte Carlo')\n\n",
         "```\n\n",
         "### Balances:\n\n", 
         "```{r, echo=FALSE, warning=FALSE, message=FALSE}\n",
         "knitr::kable(L[[7]], caption = 'Referencia - Resultado $(ha yrs^-1)$ Monte Carlo')\n",
         "knitr::kable(L[[8]], caption = 'Referencia - Resultado $(tCO2e  yrs^-1)$ Monte Carlo')\n",
         "knitr::kable(L[[9]], caption = 'Balance FE y año $(tCO2e yrs^-1)$ Monte Carlo')\n",
         "knitr::kable(L[[10]], caption = 'Balance total por año $(tCO2e  yrs^-1)$ Monte Carlo')\n",
         "```\n\n")
}

###################################################

ReporteExcel <- function(formato, fRmd, L, Nombre, Reporte) {
  if (formato == "Excel") {
    wb <- openxlsx::createWorkbook()
    nxls <- c('Resevorios', 'ReservoriosMC', 'ReferenciaArea', 
              'ReferenciaMC', 'ResultadosArea', 'ResultadosMC', 
              'Balance áreas, RESMC-REFMC',
              'Balance RESMC-REFMC', 'BalanceFEYrs', 'BalanceTotalYrs')
    
    for (i in 1:length(L)) {
      openxlsx::addWorksheet(wb, nxls[i])
      openxlsx::writeData(wb, nxls[i], data.frame(L[[i]]))
    }
    
    excel_file <- "reporte.xlsx"
    
    openxlsx::saveWorkbook(wb, file = excel_file, overwrite = TRUE)
    return(readBin(excel_file, "raw", file.info(excel_file)$size))
    
    } else if(formato == "Word"){
      tempF <- paste0(getwd(), '\\datos.rds')
      saveRDS(L, file = tempF)
      
      tempF <- paste(strsplit(tempF, '\\', fixed = T)[[1]], collapse = '/')
      contenido <- fRmd(tempF = tempF, formato = "word", Nombre=Nombre, Reporte=Reporte)
      
      return(contenido)
    } else {
       contenido <- fRmd(tempF = tempF, formato = "PDF")
       return(contenido)
  }

}
  ####################
##SIMULACION
IMC<-cmpfun(function(propag.Vari, nsim, Mean, SE, nplot, CI, type.Propag='Subtraction', plot=TRUE) {
  
  if(is.null(propag.Vari) & (toupper(type.Propag)=='ADDITION' | toupper(type.Propag)=='MULTIPLICATION' | toupper(type.Propag)=='SUBTRACTION')) stop('The propagation variable is necessary')
  
  alpha<-1-(CI/100)
  Mean<-Mean
  SE<-SE
  x <- rbind(Mean, SE)
  
  if(toupper(type.Propag)=='ADDITION' | toupper(type.Propag)=='MULTIPLICATION' | toupper(type.Propag)=='SUBTRACTION'){
    propag.Vari<-propag.Vari
    
    sel<-which(apply(x,2,sum)!=0)
    x<-x[,sel]
    
    if(!is.matrix(x) & is.vector(x)) {
      salEnd<-data.frame(0, 0, 0,0,0)
      names(salEnd)<-c('Mean', 'S.E',paste0('Lower',' (',CI,'%)'), paste0('Upper',' (',CI,'%)'), paste0('Uncertainty',' ', CI,'%'))  
      return(salEnd)
    }
    else {   
      colnames(x)<-propag.Vari[sel]
      propag.Vari<-propag.Vari[sel]
      if(toupper(type.Propag) =='ADDITION') {EX1 <-str2expression(eval((paste(propag.Vari, collapse='+')))[1])}
      else if(toupper(type.Propag) =='MULTIPLICATION') {EX1 <-str2expression(eval((paste(propag.Vari, collapse='*')))[1]) }
      else if(toupper(type.Propag) =='SUBTRACTION') { EX1 <-str2expression(eval((paste(propag.Vari, collapse='-')))[1]) }
      
      if(!is.null(nplot))  PP<-propagate(EX1, data=x, alpha = alpha, nsim =nsim, df =nplot)
      else PP<-try(propagate(EX1, data=x, alpha = alpha, nsim =nsim),silent = TRUE)
      
    }
  }
  
  ########################
  
  else {
    EX1<-expression(x)
    x=c(Mean[1],SE[1])
    x<-as.data.frame(x)
    
    if(Mean==0)  {salEnd<-data.frame(0, 0, 0,0,0)
    names(salEnd)<-c('Mean', 'S.E',paste0('Lower',' (',CI,'%)'), paste0('Upper',' (',CI,'%)'), paste0('Uncertainty',' ', CI,'%'))  
    return(salEnd)
    }
    else {   
      if(!is.null(nplot))  PP<-propagate(EX1, data=x, alpha = alpha, nsim =nsim, df =nplot, type = "stat")
      else PP<-try(propagate(EX1, data=x, alpha = alpha, nsim =nsim),silent = TRUE)
      
    }
  }
  
  
  sal<-PP$sim 
  if(isTRUE(plot)) {hist(PP$resSIM, xlab='Simulation', main='') 
    abline(v= sal[5], col='red', lty=2)
    abline(v= sal[6], col='red')
    abline(v=sal[1], col='blue')
    # abline(v=sal[2], col='green')
  }
  #try(eval(EX1, envir = as.list(x[1, ])), silent = TRUE),
  salEnd<-data.frame( sal[1], sal[2], sal[5],sal[6],
                      (1/2*((sal[6]-sal[5])/sal[1])))
  names(salEnd)<-c('Mean', 'S.E',paste0('Lower',' (',CI,'%)'), paste0('Upper',' (',CI,'%)'), paste0('Uncertainty',' ', CI,'%'))
  
  return(salEnd)
})
