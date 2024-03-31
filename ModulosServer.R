## Modulos server


fServer0 <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    Restb <- reactiveValues(
      dRESER = data.frame(
        FE = character(),
        Reservorio = character(),
        Estrato = character(),
        Media = numeric(),
        E.E = numeric()
      )
    )
    
    observeEvent(input$Add, {
      tryCatch({
        validate(need(input$FE != "", "Por favor ingrese un valor para FE."))
        validate(need(input$Reserv != "", "Por favor ingrese un valor para Reservorio."))
        validate(need(input$Estrat != "", "Por favor ingrese un valor para Estrato."))
        validate(need(input$X != "", "Por favor ingrese un valor para Media."))
        validate(need(input$XEE != "", "Por favor ingrese un valor para el error estandar."))
        
        new_row <- data.frame(
          FE = input$FE,
          Reservorio = input$Reserv, 
          Estrato = input$Estrat, 
          Media = input$X, 
          E.E = input$XEE
        )
        Restb$dRESER <- data.frame(rbind(Restb$dRESER, new_row))
        
      }, error = function(e) {
        showNotification("Se produjo un error al agregar el nuevo registro. Por favor revise los datos e intente nuevamente.", type = "error")
      })
    })
    
    output$reserv <- renderDataTable({ 
      DT::datatable(Restb$dRESER,  
                    options = list(pageLength = 12, autoWidth = TRUE, dom = "rt"),
                    rownames = TRUE,
                    escape = FALSE,
                    editable = 'cell'
      )
    })
    
    observeEvent(input$reserv_cell_clicked, {
      if (!is.null(input$reserv_cell_clicked) && 
          !is.null(input$reserv_cell_clicked$row) && 
          !is.null(input$reserv_cell_clicked$col) && 
          !is.null(input$reserv_cell_clicked$value)) {
        
        info<-input$reserv_cell_clicked$row
        row <- input$reserv_cell_clicked$row
        col <- input$reserv_cell_clicked$col
        value <- input$reserv_cell_clicked$value
        
        if (col %in% c(1, 2, 3)) {
          Restb$dRESER[row, col] <- as.character(value)
          
        } else {
          Restb$dRESER[row, col] <- as.numeric(value)
        }
      } else {
        showNotification("Por favor seleccione una celda antes de continuar.", type = "warning")
      }
    })
    
    return(reactive({ DT::datatable(Restb$dRESER) }))
    
  })
}


#################################################
##################################################
## ACTIVIDAD MODULO
fServerAct <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ActReftb <- reactiveValues(
      dtRef = data.frame(
        FE = character(),
        Yrs = character(),
        Estrato = character(),
        Mean = numeric(),
        E.E = numeric()
      )
    )
    
    observeEvent(input$Add, {
      new_row <- data.frame(
        FE = input$FE,
        Yrs = input$yrs, 
        Estrato = input$Estrat, 
        Mean = input$X, 
        E.E = input$XEE
      )
      ActReftb$dtRef<-rbind(ActReftb$dtRef, new_row)
    })
    
    output$ActRefer <- renderDataTable({ 
      DT::datatable(ActReftb$dtRef,  
                    options = list(pageLength = 12, autoWidth = TRUE, dom = "rt"),
                    rownames = TRUE,
                    escape = FALSE,
                    editable = 'cell'
      )
    })
    
    observeEvent(input$reserv_cell_clicked, {
      if (!is.null(input$reserv_cell_clicked) && 
          !is.null(input$reserv_cell_clicked$row) && 
          !is.null(input$reserv_cell_clicked$col) && 
          !is.null(input$reserv_cell_clicked$value)) {
        
        info<-input$reserv_cell_clicked$row
        row <- input$reserv_cell_clicked$row
        col <- input$reserv_cell_clicked$col
        value <- input$reserv_cell_clicked$value
        
        if (col %in% c(1, 2, 3)) {
          ActReftb$dtRef[row, col] <- as.character(value)
          
        } else {
          ActReftb$dtRef[row, col] <- as.numeric(value)
        }
      } else {
        showNotification("Por favor seleccione una celda y cuando actualice de doble clik para continuar.", type = "warning")
      }
    })
    
    
    return(
      reactive({DT::datatable(ActReftb$dtRef) })
    )
  }) 
}
######################################

ModuloPropagacionMC <- function(id, datos) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$Run, {
      
      S<-try(datos()[[1]][[3]], silent=TRUE)
      
      if (class(S)=="try-error" || nrow(S)==0) {
        showModal(modalDialog(
          title = "Error",
          "No hay datos disponibles para la propagación de Monte Carlo.",
          easyClose = TRUE
        ))
      } else {
   
      set.seed(input$nseed)
      dMC<-as.data.frame(datos()[[1]][[3]]  )
      
      nsim<-input$num_simulaciones
      CI<-input$IC
      if(input$Propaga=="Adición") type.Propag<-'ADDITION' else if(input$Propaga=="Sustracción") type.Propag<-'SUBTRACTION' else type.Propag<-'MULTIPLICATION'
      
      FEnam<-unique(dMC$FE)
      Ldt<-list()
      
      f<-function(x) {
        sal<-try(IMC(propag.Vari=x$Reservorio, nsim=nsim, Mean=x$Media*input$RMCarbono, SE=x$E.E*input$RMCarbono, nplot=100, 
                     CI=CI, type.Propag=type.Propag, plot=FALSE),silent=TRUE)
        if(class(sal)=='try-error') { sal<-data.frame(NA,NA,NA,NA,NA) 
        names(sal)<-c('Mean','S.E','Lower (95%)','Upper (95%)','Uncertainty 95%')
        }
        else {sal<-round(sal,3)
        }
        sal
      }
      
      
      for(i in 1:length(FEnam)){
        subDMC<- subset(dMC, FE==FEnam[i])
        dMCL<-split(subDMC, subDMC$Estrato)
        L<-lapply(dMCL, function(x) f(x))
        Estratos=names(dMCL)
        Ldt[[i]]<-data.frame(FE=FEnam[i], Estrato=Estratos, do.call(rbind, L))
      }
      
      
      Ldt1<<-as.data.frame(do.call(rbind, Ldt))
      output$RESULT <-DT::renderDataTable({ DT::datatable(do.call(rbind, Ldt),  
                                                          options = list(pageLength = 12,autoWidth = TRUE,
                                                                         dom= "rt"),
                                                          rownames = FALSE,
                                                          escape   = FALSE,
                                                          editable = TRUE)
      })
 
      }
  })
    return(
      reactive({DT::datatable(Ldt1) })
    )
    
  })
}



###########################################################
#########MC REFERENCIA #######################
ModuloPropagacionMCREF <- function(id, dts1, dts2) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$Run, {
      
      S<-try(dts1()[[1]][[3]], silent=TRUE)
      S1<-try(dts2()[[1]][[3]], silent=TRUE)
      
      if (class(S)=="try-error" || class(S1)=="try-error") {
        showModal(modalDialog(
          title = "Error",
          "No hay datos disponibles para la propagación de Monte Carlo.",
          easyClose = TRUE
        ))
      } else {
      
      
      nsim<-input$num_simulaciones
      CI<-input$IC
      if(input$Propaga=="Adición") type.Propag<-'ADDITION' else if(input$Propaga=="Sustracción") type.Propag<-'SUBTRACTION' else type.Propag<-'MULTIPLICATION'
      
      f<-function(x) {
        sal<-try(IMC(propag.Vari=c('Var1', 'Var2'), nsim=nsim, Mean=c(x$Mean.x,x$Mean.y), 
                     SE=c(x$E.E, x$S.E), nplot=100, CI=CI, type.Propag=type.Propag, plot=FALSE), silent=TRUE)
        
        if(class(sal)=='try-error') { sal<-data.frame(NA,NA,NA,NA,NA) 
        names(sal)<-c('Mean','S.E','Lower (95%)','Upper (95%)','Uncertainty 95%')
        return(sal)}
        else {return(round(sal,3))
        }
      }
      
      
      set.seed(input$nseed)
      d1<-as.data.frame(dts1()[[1]][[3]])
      d2<-as.data.frame(dts2()[[1]][[3]])
      
      dd<-inner_join(d1, d2, by=c('FE','Estrato'), multiple = "first")
      
      L<-lapply(1:nrow(dd), function(i) f(dd[i, ]))
      
      
      dMCRef<<-data.frame(FE=d1$FE, Yrs=d1$Yrs, Estrato=d1$Estrato, do.call(rbind, L))
      
      
      output$RESULT<-DT::renderDataTable({ DT::datatable(dMCRef,  
                                                         options = list(pageLength = 12,autoWidth = TRUE,
                                                                        dom= "rt"),
                                                         rownames = FALSE,
                                                         escape   = FALSE,
                                                         editable = TRUE)
      })
      }
      
    })
    return(
      reactive({DT::datatable(dMCRef) })
    )
    
  })
  
}

###################################################################################
## Resultados

ModuloPropagacionMCRES <- function(id, dts1, dts2) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$Run, {
      
      S<-try(dts1()[[1]][[3]], silent=TRUE)
      S1<-try(dts2()[[1]][[3]], silent=TRUE)
      
      if (class(S)=="try-error" || class(S1)=="try-error") {
        showModal(modalDialog(
          title = "Error",
          "No hay datos disponibles para la propagación de Monte Carlo.",
          easyClose = TRUE
        ))
      } else {
      
      nsim<-input$num_simulaciones
      CI<-input$IC
      if(input$Propaga=="Adición") type.Propag<-'ADDITION' else if(input$Propaga=="Sustracción") type.Propag<-'SUBTRACTION' else type.Propag<-'MULTIPLICATION'
      
      f<-function(x) {
        sal<-try(IMC(propag.Vari=c('Var1', 'Var2'), nsim=nsim, Mean=c(x$Mean.x,x$Mean.y), 
                     SE=c(x$E.E, x$S.E), nplot=100, CI=CI, type.Propag=type.Propag, plot=FALSE), silent=TRUE)
        
        if(class(sal)=='try-error') { sal<-data.frame(NA,NA,NA,NA,NA) 
        names(sal)<-c('Mean','S.E','Lower (95%)','Upper (95%)','Uncertainty 95%')
        return(sal)}
        else {return(round(sal,3))
        }
      }
      
      
      set.seed(input$nseed)
      d1<-as.data.frame(dts1()[[1]][[3]])
      d2<-as.data.frame(dts2()[[1]][[3]])
      
      dd<-inner_join(d1, d2, by=c('FE','Estrato'), multiple = "first")
      
      L<-lapply(1:nrow(dd), function(i) f(dd[i, ]))
      
      
      dMCReS<<-data.frame(FE=d1$FE, Yrs=d1$Yrs, Estrato=d1$Estrato, do.call(rbind, L))
      
      
      output$RESULT<-DT::renderDataTable({ DT::datatable(dMCReS,  
                                                         options = list(pageLength = 12,autoWidth = TRUE,
                                                                        dom= "rt"),
                                                         rownames = FALSE,
                                                         escape   = FALSE,
                                                         editable = TRUE)
      })
      
      }
      
    })
    return(
      reactive({DT::datatable(dMCReS) })
    )
    
  })
  
}

###################################################################################
###########BALANCE

ModuloPropagacionMCBalance <- function(id, dts1, dts2) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$Run, {
      
      S<-try(dts1()[[1]][[3]], silent=TRUE)
      S1<-try(dts2()[[1]][[3]], silent=TRUE)
      
      if (class(S)=="try-error" || class(S1)=="try-error") {
        showModal(modalDialog(
          title = "Error",
          "No hay datos disponibles para la propagación de Monte Carlo.",
          easyClose = TRUE
        ))
      } else {    
      nsim<-input$num_simulaciones
      CI<-input$IC
      if(input$Propaga=="Adición") type.Propag<-'ADDITION' else if(input$Propaga=="Sustracción") type.Propag<-'SUBTRACTION' else type.Propag<-'MULTIPLICATION'
      
      f<-function(x) {
        sal<-try(IMC(propag.Vari=c('Var1', 'Var2'), nsim=nsim, Mean=c(x$Media,x$Media1), 
                     SE=c(x$EE, x$EE1), nplot=100, CI=CI, type.Propag=type.Propag, plot=FALSE), silent=TRUE)
        
        if(class(sal)=='try-error') { sal<-data.frame(NA,NA,NA,NA,NA) 
        names(sal)<-c('Mean','S.E','Lower (95%)','Upper (95%)','Uncertainty 95%')
        return(sal)}
        else {return(round(sal,3))
        }
      }
      
      set.seed(input$nseed)
      d1<-as.data.frame(dts1()[[1]][[3]])
      names(d1)<-c('n', 'FE', 'Yrs','Estrato',  'Media', 'EE', 'LS', 'LI', 'INCERT')
      
      d2<-as.data.frame(dts2()[[1]][[3]])
      names(d2)<-c('n','FE', 'Yrs1', 'Estrato', 'Media1', 'EE1', 'LS1', 'LI1', 'INCERT1')
      
      
      dd<-inner_join(d1, d2, by=c('FE','Estrato'), multiple = "first")
      
      
      L<-lapply(1:nrow(dd), function(i) f(dd[i, ]))
      
      
      dMCRef<<-data.frame(FE=d1$FE, Yrs=d1$Yrs, Estrato=d1$Estrato, do.call(rbind, L))
      
      
      output$RESULT<-DT::renderDataTable({ DT::datatable(dMCRef,  
                                                         options = list(pageLength = 12,autoWidth = TRUE,
                                                                        dom= "rt"),
                                                         rownames = FALSE,
                                                         escape   = FALSE,
                                                         editable = TRUE)
      })
      
      }
    })
    return(
      reactive({DT::datatable(dMCRef) })
    )
    
  })
  
}

##########################################################

ModuloPropagacionMCBalanceXFeTotal <- function(id, dtsTotal) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$Run, {
      
      S<-try(dtsTotal()[[1]][[3]], silent=TRUE)
     
      
      if (class(S)=="try-error") {
        showModal(modalDialog(
          title = "Error",
          "No hay datos disponibles para la propagación de Monte Carlo.",
          easyClose = TRUE
        ))
      } else {    
        
      
      set.seed(input$nseed)
      nsim<-input$num_simulaciones
      CI<-input$IC
      if(input$Propaga=="Adición") type.Propag<-'ADDITION' else if(input$Propaga=="Sustracción") type.Propag<-'SUBTRACTION' else type.Propag<-'MULTIPLICATION'
      
      f<-function(x) {
        sal<-try(IMC(propag.Vari=paste0('S',x$Estrato), nsim=nsim, Mean=x$Mean, SE=x$S.E, nplot=100, 
                     CI=CI, type.Propag=type.Propag, plot=FALSE),silent=TRUE)
        if(class(sal)=='try-error') { sal<-data.frame(NA,NA,NA,NA,NA) 
        names(sal)<-c('Mean','S.E','Lower (95%)','Upper (95%)','Uncertainty 95%')
        }
        else {sal<-round(sal,3)
        }
        sal
      }
      
      dFE<-as.data.frame(dtsTotal()[[1]][[3]]  )
      
      FEnam<-unique(dFE$FE)
      Ldtt<-list()
      
      for(i in 1:length(FEnam)){
        subDtEst<- subset(dFE, FE==FEnam[i])
        spDtEst<-split(subDtEst, subDtEst$Yrs)
        L<-lapply(spDtEst, function(x) f(x))
        Yrsname=names( L)
        Ldtt[[i]]<-data.frame(Yrs=Yrsname, FE=FEnam[i], do.call(rbind, L))
      }
      
      
      Ldttt<<-as.data.frame(do.call(rbind, Ldtt))
      
      output$RESULT <-DT::renderDataTable({ DT::datatable(Ldttt,  
                                                          options = list(pageLength = 12,autoWidth = TRUE,
                                                                         dom= "rt"),
                                                          rownames = FALSE,
                                                          escape   = FALSE,
                                                          editable = TRUE)
      })
      
      }
    })
    return(
      reactive({DT::datatable(Ldttt) })
    )
    
  })
}



################################################################

ModuloPropagacionMCBalanceTotal <- function(id, dtsTotall) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$Run, {
      
      set.seed(input$nseed)
      
      nsim<-input$num_simulaciones
      CI<-input$IC
      if(input$Propaga=="Adición") type.Propag<-'ADDITION' else if(input$Propaga=="Sustracción") type.Propag<-'SUBTRACTION' else type.Propag<-'MULTIPLICATION'
      
      f<-function(x) {
        sal<-try(IMC(propag.Vari=paste0('S',x$FE), nsim=nsim, Mean=x$Mean, SE=x$E.E, nplot=100, 
                     CI=CI, type.Propag=type.Propag, plot=FALSE),silent=TRUE)
        if(class(sal)=='try-error') { sal<-data.frame(NA,NA,NA,NA,NA) 
        names(sal)<-c('Mean','S.E','Lower (95%)','Upper (95%)','Uncertainty 95%')
        }
        else {sal<-round(sal,3)
        }
        sal
      } 
      
      
      
      dTot<-as.data.frame(dtsTotall()[[1]][[3]]  )
      
      if(length(unique(dTot$FE))>1) {
        spDtEst<-split(dTot, dTot$Yrs)
        L<-lapply(spDtEst, function(x) f(x))
        Yrsname=names(L)
        
        Ldtt2<<-data.frame(Yrs=Yrsname, do.call(rbind, L))
        
      }
      else {
        
        Ldtt2<<-dTot
        showNotification("Solamente tienes un FE. resultado igual a la ventana anterior", type = "message")
      }
      
      
      output$RESULT <-DT::renderDataTable({ DT::datatable(Ldtt2,  
                                                          options = list(pageLength = 12,autoWidth = TRUE,
                                                                         dom= "rt"),
                                                          rownames = FALSE,
                                                          escape   = FALSE,
                                                          editable = TRUE)
      })
    })
    return(
      reactive({DT::datatable(Ldtt2) })
    )
    
  })
}

############################################################################################
####################################
## REPORTE
Moduloreporte <- function(id, datosRes,datosResMC,  datosActRef, 
                          datosActRefMC, datosActReS, datosActReSMC, datosBalanceMC, 
                          datosBalancexFExYrsMC, datosBalanceTotalXyrs) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$RunReporte, {
     
      tryCatch({
        
        lista_datos <- list(datosResMC, datosRes, datosActRef, datosActRefMC, datosActReS, datosActReSMC,
                            datosBalanceMC, datosBalancexFExYrsMC, datosBalanceTotalXyrs)
        
        es_data_frame <- function(x) {is.data.frame(x()[[1]][[3]])}
        todos_data_frames <- all(sapply(lista_datos, es_data_frame))
        
        if (todos_data_frames) {
          
                        eventReporte(datosResMC, datosRes,datosActRef, datosActRefMC, datosActReS, datosActReSMC,
                       datosBalanceMC, datosBalancexFExYrsMC, datosBalanceTotalXyrs)
          
          output$renderedReporte<-renderUI({ 
            rmarkdown::render(file.path(getwd(), 'reporte_final.Rmd'))
            
          })
        
          
          ReportExcel(datosResMC, datosRes, datosActRef, datosActRefMC, datosActReS, datosActReSMC,
                      datosBalanceMC, datosBalancexFExYrsMC, datosBalanceTotalXyrs)
         
        } else {
          # Acciones a realizar si no todos los elementos son data frames
          showModal(modalDialog(title = "Error en los datos",
                                "No todos los elementos son data frames.",
                                easyClose = TRUE))
        }
        
      }, error = function(e) {
        if (grepl("object 'dMCRef' not found", e$message)) {
          showModal(modalDialog(title = "No hay acciones disponibles",
                                "No se ha definido ninguna acción para realizar.",
                                easyClose = TRUE))
        } else {
          # Otros errores que puedan surgir durante la ejecución
          showModal(modalDialog(title = "Error en la ejecución",
                                "Se ha producido un error durante la ejecución del reporte.",
                                easyClose = TRUE))
        }
      })
      
      
    })
  })
}
