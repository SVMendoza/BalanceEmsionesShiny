## Funciones
library(compiler)
library(propagate)


#source('C:\\R_ejerc\\shainyPrueba\\Incert\\Propagation\\incertidumbrePropagacion.r')
eventReporte<-function() {
  
  ResevorioDt<-datosRes()[[1]][[3]]
  ResevorioDtMC<-datosResMC()[[1]][[3]]
  
  ##Actividad
  #Referencia
  RefereDt<-datosActRef()[[1]][[3]]
  RefereDtMC<-datosActRefMC()[[1]][[3]]
  
  #Resultados
  ReSereDt<-datosActReS()[[1]][[3]]
  ReSereDtMC<-datosActReSMC()[[1]][[3]]
  
  #Balances
  
  BalanceXEstratoFEYrsDt<-datosBalanceMC()[[1]][[3]]
  BalancexFEYrs<-datosBalancexFExYrsMC()[[1]][[3]]
  BalancexYrs<-datosBalanceTotalXyrs()[[1]][[3]]
  
  contenidoReporte <- paste0("# Reporte de estimaciones\n\n",
                             "### Reservorios\n\n",
                             "```{r setup, include=FALSE, warning=FALSE, message=FALSE}\n",
                             "knitr::kable(head(ResevorioDt), caption = 'Resevorios')\n",
                             "knitr::kable(head(ResevorioDtMC), caption = 'Total de estimación')\n\n",
                             "```","### Datos de actividad:\n\n", 
                             "knitr::kable(head(RefereDt), caption = 'Referencia')\n",
                             "knitr::kable(head(ReSereDt), caption = 'Resultados')\n\n",
                             "```",
                             "knitr::kable(head(RefereDtMC), caption = 'Referencia Carbono Monte Carlo')\n",
                             "knitr::kable(head(ReSereDtMC), caption = 'Resultados Carbono Monte Carlo')\n\n",
                             "### Balances:\n\n", 
                             "knitr::kable(head(BalanceXEstratoFEYrsDt), caption = 'Resultados - Referencia. Monte Carlo')\n",
                             "knitr::kable(head(BalancexFEYrs), caption = 'Balance FE y Años. Monte Carlo')\n\n",
                             "knitr::kable(head(BalancexYrs), caption = 'Balance total por año. Monte Carlo')\n")
  
  writeLines(contenidoReporte, "reporte_final.Rmd")
  
  output$renderedReporte <- renderUI({
    includeMarkdown(knitr::knit("reporte_final.Rmd")) 
  })
  
}

ReportExcel<-function() {output$downloadReport <- downloadHandler(
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
      # Generar PDF o Word a partir del contenido Markdown
      rmarkdown::render(text = contenidoReporte, output_format = paste0(input$format, "_document"), output_file = file)
    } else {
      # Guardar las tablas en un archivo Excel
      wb <- openxlsx::createWorkbook()
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