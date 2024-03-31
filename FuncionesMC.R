## Funciones
library(compiler)
library(propagate)



#### archivo markdown funcion

fRmd<-function() {
paste0("# Reporte de estimaciones\n\n",
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

#source('C:\\R_ejerc\\shainyPrueba\\Incert\\Propagation\\incertidumbrePropagacion.r')
eventReporte<-function(datosRes, datosResMC, datosActRef, datosActRefMC, datosActReS, datosActReSMC,
                       datosBalanceMC, datosBalancexFExYrsMC, datosBalanceTotalXyrs) {
  
  
  ## Reservorios
  
  ResevorioDt<-as.data.frame(datosResMC()[[1]][[3]])
  ResevorioDtMC<-as.data.frame(datosRes()[[1]][[3]])
  
  ##Actividad
  
  #Referencia

  RefereDt<-as.data.frame(datosActRef()[[1]][[3]])
  RefereDtMC<-as.data.frame(datosActRefMC()[[1]][[3]])
  
  #Resultados
  ReSereDt<-as.data.frame(datosActReS()[[1]][[3]])
  ReSereDtMC<-as.data.frame(datosActReSMC()[[1]][[3]])

  BalanceXEstratoFEYrsDt<-as.data.frame(datosBalanceMC()[[1]][[3]])
  BalancexFEYrs<-as.data.frame(datosBalancexFExYrsMC()[[1]][[3]])
  BalancexYrs<-as.data.frame(datosBalanceTotalXyrs()[[1]][[3]])
  
  
 
  writeLines(fRmd(), "reporte_final.Rmd")
 }

ReportExcel<-function(datosRes, datosResMC, datosActRef, datosActRefMC, datosActReS, datosActReSMC,
                      datosBalanceMC, datosBalancexFExYrsMC, datosBalanceTotalXyrs) {
  
  ## Reservorios
  
  ResevorioDt<-as.data.frame(datosResMC()[[1]][[3]])
  ResevorioDtMC<-as.data.frame(datosRes()[[1]][[3]])

  #Referencia
  
  RefereDt<-as.data.frame(datosActRef()[[1]][[3]])
  RefereDtMC<-as.data.frame(datosActRefMC()[[1]][[3]])
  
  #Resultados
  ReSereDt<-as.data.frame(datosActReS()[[1]][[3]])
  ReSereDtMC<-as.data.frame(datosActReSMC()[[1]][[3]])
  
  BalanceXEstratoFEYrsDt<-as.data.frame(datosBalanceMC()[[1]][[3]])
  BalancexFEYrs<-as.data.frame(datosBalancexFExYrsMC()[[1]][[3]])
  BalancexYrs<-as.data.frame(datosBalanceTotalXyrs()[[1]][[3]])
  
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      if (input$format == "PDF") {
        "reporte_generado.pdf"
      } else if (input$format == "Word") {
        "reporte_generado.docx"
      } else {
        "reporte_generado.xlsx"
      }
    },
    content = function(file) {
      if (input$format %in% c("PDF", "Word")) {
        contenidoReporte<-fRmd()
        rmarkdown::render(text = contenidoReporte, output_format = paste0(input$format, "_document"), output_file = file)
      } else {
        
        wb <- openxlsx::createWorkbook()
        
        # Agregar datos y crear hojas de cálculo en el archivo Excel
        openxlsx::addWorksheet(wb, "Resevorios")
        openxlsx::writeData(wb, "Resevorios", data.frame(ResevorioDt))
        
        openxlsx::addWorksheet(wb, "SumReservoriosMC")
        openxlsx::writeData(wb, "SumReservoriosMC", data.frame(ResevorioDtMC))
        
        openxlsx::addWorksheet(wb, "ReferenciaArea")
        openxlsx::writeData(wb, "ReferenciaArea", data.frame(RefereDt))
        
        openxlsx::addWorksheet(wb, "ReferenciaMC")
        openxlsx::writeData(wb, "ReferenciaMC", data.frame(RefereDtMC))
        
        openxlsx::addWorksheet(wb, "ResultadosArea")
        openxlsx::writeData(wb, "ResultadosArea", data.frame(ReSereDt))
        
        openxlsx::addWorksheet(wb, "ResultadosMC")
        openxlsx::writeData(wb, "ResultadosMC", data.frame(ReSereDtMC))
        
        openxlsx::addWorksheet(wb, "BalanceRES-REF_MC")
        openxlsx::writeData(wb, "BalanceRES-REF_MC", data.frame(BalanceXEstratoFEYrsDt))
        
        openxlsx::addWorksheet(wb, "BalanceFEYrs")
        openxlsx::writeData(wb, "BalanceFEYrs", data.frame(BalancexFEYrs))
        
        openxlsx::addWorksheet(wb, "BalanceYrs")
        openxlsx::writeData(wb, "BalanceYrs", data.frame(BalancexYrs))
        
        # Guardar el archivo Excel
        openxlsx::saveWorkbook(wb, file)
      }
    }
  )
  
}

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