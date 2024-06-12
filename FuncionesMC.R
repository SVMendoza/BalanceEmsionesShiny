## Funciones
library(compiler)
library(propagate)
##Resetear la aplicacion

resetSession <- function() {
  rm(list = ls())
}



#### archivo markdown funcion

fRmd<-function() {
paste0("---\n",
       "title: \"Reporte emisiones\"\n",
       "author: \"Tu Nombre\"\n",
       "date: \"Fecha\"\n",
       "output:\n",
       "  ", formato,'_document\n',
       "    toc: true\n",
       "    number_sections: true\n",
       "---\n\n","# Reporte de estimaciones\n\n",
                           "### Reservorios\n\n",
                           "```{r setup, include=FALSE}\n",
                           "knitr::opts_chunk$set(echo = TRUE)\n",
                           "```\n\n",
                           "```{r, include=FALSE, warning=FALSE, message=FALSE}\n",
                           "knitr::kable(ResevorioDt, caption = 'Resevorios')\n",
                           "knitr::kable(ResevorioDtMC, caption = 'Total de estimación')\n\n",
                           "```\n\n","### Datos de actividad:\n\n",
                           
                           "```{r, include=FALSE, warning=FALSE, message=FALSE}\n",
                           "knitr::kable(RefereDt, caption = 'Referencia')\n",
                           "knitr::kable(ReSereDt, caption = 'Resultados')\n\n",
                           "```\n\n",
                           "```{r, include=FALSE, warning=FALSE, message=FALSE}\n",
                           "knitr::kable(RefereDtMC, caption = 'Referencia Carbono Monte Carlo')\n",
                           "knitr::kable(ReSereDtMC, caption = 'Resultados Carbono Monte Carlo')\n\n",
                           "```\n\n",
                           "### Balances:\n\n", 
                           "```{r, include=FALSE, warning=FALSE, message=FALSE}\n",
                           "knitr::kable(BalanceXEstratoFEYrsDt, caption = 'Resultados - Referencia. Monte Carlo')\n",
                           "knitr::kable(BalancexFEYrs, caption = 'Balance FE y Años. Monte Carlo')\n\n",
                           "knitr::kable(BalancexYrs, caption = 'Balance total por año. Monte Carlo')\n",
                           "```\n\n")
}

FuncDatosList<-function(x){
  ResevorioDt<-as.data.frame(x[[1]])
  ResevorioDtMC<-as.data.frame(x[[2]])
  
  #Referencia
  
  RefereDt<-as.data.frame(x[[3]])
  RefereDtMC<-as.data.frame(x[[4]])
  
  
  #Resultados
  ReSereDt<-as.data.frame(x[[5]])
  ReSereDtMC<-as.data.frame(x[[6]])
  
  BalanceXEstratoFEYrsDt<-as.data.frame(x[[7]])
  BalancexFEYrs<-as.data.frame(x[[8]])
  BalancexYrs<-as.data.frame(x[[9]])
  
  return(list(ResevorioDt, ResevorioDtMC, RefereDt, RefereDtMC,
              ReSereDt, ReSereDtMC, BalanceXEstratoFEYrsDt, BalancexFEYrs, BalancexYrs))
  
}


ReporteExcelRmrd<-function(output, datosReactivos, formato, filename) {
  contenidoReporte <- NULL
  print('dentroFUncion')
  if (formato %in% c("PDF", "Word")) {
     contenidoReporte <- fRmd(formato)  
   
    rmarkdown::render(contenidoReporte, output_file = file_name)
  } else {
    # Generar el reporte en Excel
    wb <- openxlsx::createWorkbook()
    nxls <- c('Resevorios', 'SumReservoriosMC', 'ReferenciaArea', 'ReferenciaMC', 'ResultadosArea', 'ResultadosMC', 'Balance RESMC-REFMC', 'BalanceFEYrs', 'BalanceYrs')
    
    for (i in 1:length(datosReactivos)) {
      openxlsx::addWorksheet(wb, nxls[i])
      openxlsx::writeData(wb, nxls[i], data.frame(datosReactivos[[i]]))
    }
    
    openxlsx::saveWorkbook(wb, file = file_name)
  }
}

ReporteExcel <- function(output, datosReactivos, formato, file) {
  if (formato == "Excel") {
    wb <- openxlsx::createWorkbook()
    nxls <- c('Resevorios', 'SumReservoriosMC', 'ReferenciaArea', 'ReferenciaMC', 'ResultadosArea', 'ResultadosMC', 'Balance RESMC-REFMC', 'BalanceFEYrs', 'BalanceYrs')
    
    for (i in 1:length(datosReactivos)) {
      openxlsx::addWorksheet(wb, nxls[i])
      openxlsx::writeData(wb, nxls[i], data.frame(datosReactivos[[i]]))
    }
    
    openxlsx::saveWorkbook(wb, file = 'reporte.xlsx')
  } else if (formato == "PDF") {
    # Generar el reporte en PDF
    pdf_file <- "Reporte.pdf"
    contenido_pdf <- generatePDFContent(datosReactivos)  # Función para generar contenido PDF
    write_pdf(contenido_pdf, pdf_file)  # Función para escribir el contenido en un PDF
  } else {
    # Generar el reporte en Word
    docx_file <- "Reporte.docx"
    contenido_docx <- generateDocxContent(datosReactivos)  # Función para generar contenido Word
    write_docx(contenido_docx, docx_file)  # Función para escribir el contenido en un archivo Word
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