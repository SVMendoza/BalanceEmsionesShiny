#################################################################
#################################################################
## Modulos ingreso de datos reservorios
ModuloReservorios <- function(id) {
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
      validate(need(input$FE != "", "Por favor ingrese un valor para FE."))
      validate(need(input$Reserv != "", "Por favor ingrese un valor para Reservorio."))
      validate(need(input$Estrat != "", "Por favor ingrese un valor para Estrato."))
      validate(need(input$X != "", "Por favor ingrese un valor para Media."))
      validate(need(input$XEE != "", "Por favor ingrese un valor para el error estandar."))
      
      nuevoIngreso <- data.frame(
        FE = input$FE,
        Reservorio = input$Reserv, 
        Estrato = input$Estrat, 
        Media = input$X, 
        E.E = input$XEE
      )
      Restb$dRESER <- rbind(Restb$dRESER, nuevoIngreso)
      Contener$guardarTabla("Reservorios", Restb$dRESER)
    })
    
     output$reserv <-  DT::renderDataTable({renderDatosIngresados(data=Restb$dRESER, names(Restb$dRESER), "", condi=TRUE)})
    
    proxy <- DT::dataTableProxy("reserv")
    observeEvent(input$reserv_cell_edit, {
        info <- input$reserv_cell_edit
        Restb$dRESER <<- editData(Restb$dRESER, info)
        replaceData(proxy, Restb$dRESER, resetPaging = FALSE) 
        Contener$guardarTabla("Reservorios", Restb$dRESER)
         
        }
       )
  })
}

####  Modulo propagación Reservorios
ModuloPropagReservMC <- function(id) {
  moduleServer(id, function(input, output, session) {
    
   observeEvent(input$Run, {
     
      S<-try(Contener$obtenerDatos("Reservorios"), silent=TRUE)
      if (!is.null(S) && !(class(S) == "try-error" || nrow(S) == 0)) {
     
        set.seed(input$nseed)
        dMC<-as.data.frame(S)
        
        nsim<-input$num_simulaciones
        CI<-input$IC
        type.Propag<-'ADDITION' 
        FEnam<-unique(dMC$FE)
        Ldt<-list()
        
        f<-function(x) {
          sal<-try(IMC(propag.Vari=x$Reservorio, nsim=nsim, Mean=x$Media*input$RMCarbono, SE=x$E.E*input$RMCarbono, nplot=100, 
                       CI=CI, type.Propag=type.Propag, plot=FALSE),silent=TRUE)
          if(class(sal)=='try-error') { sal<-data.frame(sum(x$Media*input$RMCarbono, na.rm=TRUE), NA,NA,NA,NA,NA) 
              }
          else {
            sal<-data.frame(Observado=round(sum(x$Media*input$RMCarbono, na.rm=TRUE),3), sal)
           
          }
          names(sal)<-c('Observado','Mean','S.E',paste0('Lower',' ','(',CI, '%)'),paste0('Upper',' ','(',CI, '%)'),paste0('Uncertainty',' ','(',CI, '%)'))
          sal
        }
       
        
        for(i in 1:length(FEnam)){
          subDMC<- subset(dMC, FE==FEnam[i])
          dMCL<-split(subDMC, subDMC$Estrato)
          L<-lapply(dMCL, function(x) f(x))
          Estratos=names(dMCL)
          Ldt[[i]]<-data.frame(FE=FEnam[i], Estrato=Estratos, do.call(rbind, L))
        }
        
        DmcReserv<-as.data.frame(do.call(rbind, Ldt))
        output$RESULT <-DT::renderDataTable({ DT::datatable(DmcReserv,
                                                            colnames = c('Factor de emisión', 'Estrato', 'Observado', 'Media', 'E.E', paste0('Limite inferior',' ','(',CI, '%)'),paste0('Limite superior',' ','(',CI, '%)'),paste0('Incertidumbre',' ','(',CI, '%)')),
                                                            options = list(pageLength = 12, 
                                                                           autoWidth = TRUE, 
                                                                           searching = FALSE),
                                                            rownames = TRUE,
                                                            selection = 'none',
                                                            editable = FALSE)
        })
        Contener$guardarTabla("MCReservorio", DmcReserv)
      }
     
   else { shinyalert(" ", "No es posible hacer estimaciones, revisa los datos.", type = "message") }
     
    })
  })
}

#################################################################
#################################################################
## ACTIVIDAD MODULO Referencia ingreso de datos
ModuloActRef <- function(id) {
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
      validate(need(input$FE != "", "Por favor ingrese un valor para FE."))
      validate(need(input$yrs != "", "Por favor ingrese un valor para Reservorio."))
      validate(need(input$Estrat != "", "Por favor ingrese un valor para Estrato."))
      validate(need(input$X != "", "Por favor ingrese un valor para Media."))
      validate(need(input$XEE != "", "Por favor ingrese un valor para el error estandar."))
      
      nuevoIngreso <- data.frame(
        FE = input$FE,
        Yrs = input$yrs, 
        Estrato = input$Estrat, 
        Mean = input$X, 
        E.E = input$XEE
      )
      ActReftb$dtRef<-rbind(ActReftb$dtRef,  nuevoIngreso)
      Contener$guardarTabla("AreaReferencia",  ActReftb$dtRef)
    })
    
    output$ActRefer <- DT::renderDataTable({renderDatosIngresados(data=ActReftb$dtRef, names(ActReftb$dtRef), "", condi=FALSE)})

    proxy <- DT::dataTableProxy("ActRefer")
    
    observeEvent(input$ActRefer_cell_edit, {
      info <- input$ActRefer_cell_edit
     
      ActReftb$dtRef <<- editData(ActReftb$dtRef, info)
      replaceData(proxy, ActReftb$dtRef, resetPaging = FALSE) 

      Contener$guardarTabla("AreaReferencia",  ActReftb$dtRef)

    })
  }) 
}

## Modulo propagación referencia

ModuloPropagMCRef <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$Run, {
      S<-try(Contener$obtenerDatos("AreaReferencia"), silent=TRUE)
      S1<-try(Contener$obtenerDatos("MCReservorio"), silent=TRUE)
      
      
      if ((is.null(S) || is.null(S1)) || (class(S)=="try-error" || class(S1)=="try-error") || nrow(S)==0) {
        shinyalert(" ", "No hay archivos o no hay una estructua de datos lógica.", type = "error")
        
      } else {
        
      
        nsim<-input$num_simulaciones
        CI<-input$IC
        type.Propag<-'MULTIPLICATION'
        
        f<-function(x) {
          sal<-try(IMC(propag.Vari=c('Var1', 'Var2'), nsim=nsim, Mean=c(x$Mean.x,x$Observado), 
                       SE=c(x$E.E, x$S.E), nplot=100, CI=CI, type.Propag=type.Propag, plot=FALSE), silent=TRUE)
          
          if(class(sal)=='try-error') { sal<-data.frame(NA,NA,NA,NA,NA) }
          else {sal<-round(sal,3)
          }
          names(sal)<-c('Mean','S.E',paste0('Lower',' ','(',CI, '%)'),paste0('Upper',' ','(',CI, '%)'),paste0('Uncertainty',' ','(',CI, '%)'))
          sal
        }
        
        set.seed(input$nseed)
        dd<-inner_join(S, S1, by=c('FE','Estrato'), multiple = "first")
        
        L<-lapply(1:nrow(dd), function(i) f(dd[i, ]))
        
        
        dMCRef<-data.frame(FE=S$FE, Yrs=S$Yrs, Estrato=S$Estrato, Observado=dd$Mean.x*dd$Observado, do.call(rbind, L))
        
        
        output$RESULT<-DT::renderDataTable({ DT::datatable(dMCRef, 
                                                           colnames = c('Factor de emisión', 'Año', 'Estrato', 'Observado', 'Media', 'E.E', paste0('Limite inferior',' ','(',CI, '%)'),paste0('Limite superior',' ','(',CI, '%)'),paste0('Incertidumbre',' ','(',CI, '%)')),
                                                           options = list(pageLength = 12, 
                                                                          autoWidth = TRUE, 
                                                                          searching = FALSE),
                                                           rownames = FALSE,
                                                           selection = 'none',
                                                           editable = FALSE)
        })
     
      Contener$guardarTabla("MCReferencia",  dMCRef)
      }
      })
  })
}

#################################################################
#################################################################
## ACTIVIDAD MODULO Resultados ingreso de datos
ModuloActRes <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
    ActRefREStb <- reactiveValues(
      dtRes = data.frame(
        FE = character(),
        Yrs = character(),
        Estrato = character(),
        Mean = numeric(),
        E.E = numeric()
      )
    )
    
    observeEvent(input$Add, {
      validate(need(input$FE != "", "Por favor ingrese un valor para FE."))
      validate(need(input$yrs != "", "Por favor ingrese un valor para Reservorio."))
      validate(need(input$Estrat != "", "Por favor ingrese un valor para Estrato."))
      validate(need(input$X != "", "Por favor ingrese un valor para Media."))
      validate(need(input$XEE != "", "Por favor ingrese un valor para el error estandar."))
      
      nuevoIngreso <- data.frame(
        FE = input$FE,
        Yrs = input$yrs, 
        Estrato = input$Estrat, 
        Mean = input$X, 
        E.E = input$XEE
      )
      ActRefREStb$dtRes<-rbind(ActRefREStb$dtRes, nuevoIngreso)
      Contener$guardarTabla("AreaResultados",  ActRefREStb$dtRes)
    })
    
    output$ActRefer <- DT::renderDataTable({renderDatosIngresados(data=ActRefREStb$dtRes, names(ActRefREStb$dtRes), "", condi=FALSE)})
      
    proxy <- DT::dataTableProxy("ActRefer")
    
    observeEvent(input$ActRefer_cell_edit, {
      info <- input$ActRefer_cell_edit
      
      ActRefREStb$dtRes <<- editData(ActRefREStb$dtRes, info)
      replaceData(proxy, ActRefREStb$dtRes, resetPaging = FALSE) 
      
      Contener$guardarTabla("AreaResultados",  ActRefREStb$dtRes)
      
    })
  }) 
}


## Resultados propagacion MC

ModuloPropagMCRes <- function(id) {
  moduleServer(id, function(input, output, session) {
    

    observeEvent(input$Run, {
      S<-try(Contener$obtenerDatos("AreaResultados"), silent=TRUE)
      S1<-try(Contener$obtenerDatos("MCReservorio"), silent=TRUE)
      
      if ((is.null(S) || is.null(S1)) || (class(S)=="try-error" || class(S1)=="try-error") || nrow(S)==0) {
      shinyalert(" ", "No hay archivos o no hay una estructua de datos lógica.", type = "error")
        
      } else {
      
      nsim<-input$num_simulaciones
      CI<-input$IC
      type.Propag<-'MULTIPLICATION'
      
      f<-function(x) {
        sal<-try(IMC(propag.Vari=c('Var1', 'Var2'), nsim=nsim, Mean=c(x$Mean.x,x$Mean.y), 
                     SE=c(x$E.E, x$S.E), nplot=100, CI=CI, type.Propag=type.Propag, plot=FALSE), silent=TRUE)
        
        if(class(sal)=='try-error') { sal<-data.frame(NA,NA,NA,NA,NA)}
        else {sal<-round(sal,3)
        }
        names(sal)<-c('Mean','S.E',paste0('Lower',' ','(',CI, '%)'),paste0('Upper',' ','(',CI, '%)'),paste0('Uncertainty',' ','(',CI, '%)'))
        sal
      }
      set.seed(input$nseed)
      dd<-inner_join(S, S1, by=c('FE','Estrato'), multiple = "first")
      
      L<-lapply(1:nrow(dd), function(i) f(dd[i, ]))
      dMCReS<-data.frame(FE=S$FE, Yrs=S$Yrs, Estrato=S$Estrato, Observado=dd$Mean.x*dd$Observado, do.call(rbind, L))

      output$RESULT<-DT::renderDataTable({ DT::datatable(dMCReS,
                                                         colnames = c('Factor de emisión', 'Año', 'Estrato', 'Observado', 'Media', 'E.E', paste0('Limite inferior',' ','(',CI, '%)'),paste0('Limite superior',' ','(',CI, '%)'),paste0('Incertidumbre',' ','(',CI, '%)')),
                                                         options = list(pageLength = 12, 
                                                                        autoWidth = TRUE, 
                                                                        searching = FALSE),
                                                         rownames = FALSE,
                                                         selection = 'none',
                                                         editable = FALSE)
      })
      Contener$guardarTabla("MCResultados", dMCReS)
    }
    })
  })
}

#################################################################
#################################################################
########### BALANCE
### Estimar diferencias Referencia-resultados
ModuloPropagMCRefRes <- function(id) {
  moduleServer(id, function(input, output, session) {

        observeEvent(input$Run, {
          
          S<-try(Contener$obtenerDatos("MCReferencia"), silent=TRUE)
          S1<-try(Contener$obtenerDatos("MCResultados"), silent=TRUE)
          
          S.1<-try(Contener$obtenerDatos("AreaReferencia"), silent=TRUE)
          S1.1<-try(Contener$obtenerDatos("AreaResultados"), silent=TRUE)
          
          
          if ((is.null(S) || is.null(S1)) || (class(S)=="try-error" || class(S1)=="try-error")) {
            shinyalert(" ", "No hay archivos o no hay una estructua de datos lógica.", type = "error")
            
          } else {
     
      nsim<-input$num_simulaciones
      CI<-input$IC
     
      
      f<-function(x) {
        sal<-try(IMC(propag.Vari=c('Var1', 'Var2'), nsim=nsim, Mean=c(x$Media,x$Media1), 
                     SE=c(x$EE, x$EE1), nplot=100, CI=CI, type.Propag='SUBTRACTION', plot=FALSE), silent=TRUE)
        
        if(class(sal)=='try-error') { sal<-data.frame(NA,NA,NA,NA,NA) }
        else {sal<-round(sal,3) }
        names(sal)<-c('Mean','S.E',paste0('Lower',' ','(',CI, '%)'),paste0('Upper',' ','(',CI, '%)'),paste0('Uncertainty',' ','(',CI, '%)'))
        sal
      }
      
      set.seed(input$nseed)
     
      names(S)<-c('FE', 'Yrs','Estrato','Observado', 'Media', 'EE', 'LI', 'LS', 'INCERT')
      names(S1)<-c('FE', 'Yrs1', 'Estrato', 'Observado1','Media1', 'EE1', 'LI1', 'LS1', 'INCERT1')
      
      #Areas########################################################
      
      names(S.1)<-c('FE', 'Yrs','Estrato','Media', 'EE')
      names(S1.1)<-c('FE', 'Yrs1','Estrato','Media1', 'EE1')
      dd.1<-try(inner_join(S1.1, S.1, by=c('FE','Estrato'), multiple = "first"), silent=TRUE)
      if(class(dd.1)=='try-error') { shinyalert("", "Revisar datos.", type = "warning")}
      
      L.1<-lapply(1:nrow(dd.1), function(i) f(dd.1[i, ]))
     
      dMCDeltaArea<-data.frame(Yrs=dd.1$Yrs1,dd.1$Estrato, dd.1=S$FE, Observado=dd.1$Media-dd.1$Media1, do.call(rbind, L.1))
     
      ###############################################################
      dd<-try(inner_join(S1, S, by=c('FE','Estrato'), multiple = "first"))
      if(class(dd.1)=='try-error') { shinyalert("", "Revisar datos.", type = "warning")}
      
      L<-lapply(1:nrow(dd), function(i) f(dd[i, ]))
      
      dMCDelta<-data.frame(Yrs=dd$Yrs1, FE=dd$FE, Estrato=dd$Estrato,Observado=dd$Observado-dd$Observado1, do.call(rbind, L))
      
      
      output$RESULT<-DT::renderDataTable({DT::datatable(dMCDelta, 
                                                        colnames = c('Año', 'Factor de emisión','Estrato', 'Observado', 'Media', 'E.E', paste0('Limite inferior',' ','(',CI, '%)'),paste0('Limite superior',' ','(',CI, '%)'),paste0('Incertidumbre',' ','(',CI, '%)')),
                                                        options = list(pageLength = 12, 
                                                                        autoWidth = TRUE, 
                                                                        searching = FALSE),
                                                         rownames = FALSE,
                                                         selection = 'none',
                                                         editable = FALSE)
      })
      
    
      Contener$guardarTabla("MCDeltas", dMCDelta)   
      Contener$guardarTabla("MCDeltasArea", dMCDeltaArea) 
      
      
          }
    })
  })
  
}

### Balance X factor de emisión

ModuloPropagMCBalanceFe <- function(id) {
  moduleServer(id, function(input, output, session) {
   
    observeEvent(input$Run, {
      
      S<-try(Contener$obtenerDatos("MCDeltas"), silent=TRUE)
     
      if (is.null(S) || class(S)=="try-error") {
        shinyalert(" ", "No hay una estructua de datos lógica/ No se enecuentran los datos.", type = "error")
       } else {    
        
      
      set.seed(input$nseed)
      nsim<-input$num_simulaciones
      CI<-input$IC
     
      
      f<-function(x) {
        
          if(nrow(x)==1) {
          shinyalert("", "Para al menos un reservorio, el resultado es igual a la ventana **Balance por estrato**.", type = "info")
          sal<-x[,4:9]
          
        } else {
        
        sal<-try(IMC(propag.Vari=paste0('S',x$Estrato), nsim=nsim, Mean=x$Mean, SE=x$S.E, nplot=100, 
                       CI=CI, type.Propag='ADDITION', plot=FALSE),silent=TRUE)
        if(class(sal)=='try-error') { sal<-data.frame(Observado=sum(x$Observado, na.rm=TRUE), NA,NA,NA,NA,NA) 
       # names(sal)<-c('Observado', 'Mean','S.E','Lower (95%)','Upper (95%)','Uncertainty 95%')
        }
        else {sal<-round(data.frame(Observado=sum(x$Observado, na.rm=TRUE), sal),3)
        }
       }
        names(sal)<-c('Observado','Mean','S.E',paste0('Lower',' ','(',CI, '%)'),paste0('Upper',' ','(',CI, '%)'),paste0('Uncertainty',' ','(',CI, '%)'))
        sal
      }
    
      FEnam<-unique(S$FE)
      Ldtt<-list()
      for(i in 1:length(FEnam)){
        subDtEst<- subset(S, FE==FEnam[i])
        spDtEst<-split(subDtEst, subDtEst$Yrs)
        L<-lapply(spDtEst, function(x) f(x))
        Yrsname=names(L)
        Ldtt[[i]]<-data.frame(Yrs=Yrsname, FE=FEnam[i],do.call(rbind, L))
        
      }
      
      dataBalFE<-as.data.frame(do.call(rbind, Ldtt))
      output$RESULT <-DT::renderDataTable({ DT::datatable(dataBalFE,  
                                                          colnames = c('Año', 'Factor de emisión', 'Observado', 'Media', 'E.E', paste0('Limite inferior',' ','(',CI, '%)'),paste0('Limite superior',' ','(',CI, '%)'),paste0('Incertidumbre',' ','(',CI, '%)')),
                                                          options = list(pageLength = 12, 
                                                                         autoWidth = TRUE, 
                                                                         searching = FALSE),
                                                          rownames = FALSE,
                                                          selection = 'none',
                                                          editable = TRUE)
      })
      
      
      Contener$guardarTabla("MCBalanceFE", dataBalFE)     
       }
    })
  })
}

### Balance total

ModuloPropagMCBalanceTotal <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$Run, {
      S<-try(Contener$obtenerDatos('MCBalanceFE'), silent=TRUE)
      
      if (is.null(S) || class(S)=="try-error") { 
        shinyalert("", "Los datos son insuficientes o no existen.", type = "warning")
      } else {
        if(nrow(S)==1) {
          shinyalert("", "Retorna el mismo resultado que la salida anterior.", type = "info")
          dataBalTot<- Contener$guardarTabla("BalanceTotal", S[,-2]) 
          #names(dataBalTot)<-c('Observado','Mean','S.E',paste0('Lower',' ','(',CI, '%)'),paste0('Upper',' ','(',CI, '%)'),paste0('Uncertainty',' ','(',CI, '%)'))
          
         } else {
          set.seed(input$nseed)
          nsim<-input$num_simulaciones
          CI<<-input$IC
          
          f<-function(x) {
            sal<-try(IMC(propag.Vari=paste0('S',x$FE), nsim=nsim, Mean=x$Mean, SE=x$S.E, nplot=100, 
                         CI=CI, type.Propag='ADDITION' , plot=FALSE),silent=TRUE)
            if(class(sal)=='try-error') { sal<-data.frame(sum(x$Observado, na.rm=TRUE), NA,NA,NA,NA,NA) 
            } else {sal<-data.frame(Observado=sum(x$Observado, na.rm=TRUE), sal)
            }
            names(sal)<-c('Observado','Mean','S.E',paste0('Lower',' ','(',CI, '%)'),paste0('Upper',' ','(',CI, '%)'),paste0('Uncertainty',' ','(',CI, '%)'))
            sal
          } 
          
          
            spDtEst<-split(S, S$Yrs)
            L<-lapply(spDtEst, function(x) f(x))
            Yrsname=names(L)
            dataBalTot<-data.frame(Yrs=Yrsname, do.call(rbind, L))
          
            
        }
        output$RESULT <-DT::renderDataTable({ DT::datatable(dataBalTot,  
                                                            colnames = c('Año', 'Observado', 'Media', 'E.E', paste0('Limite inferior',' ','(',CI, '%)'),paste0('Limite superior',' ','(',CI, '%)'),paste0('Uncertainty',' ','(',CI, '%)')),
                                                            options = list(pageLength = 12, 
                                                                           autoWidth = TRUE, 
                                                                           searching = FALSE),
                                                            rownames = FALSE,
                                                            selection = 'none',
                                                            editable = FALSE)
        })
        Contener$guardarTabla("BalanceTotal", dataBalTot) 
      }
    })
  })
}


############################################################################################
####################################
## REPORTE
Moduloreporte <- function(id) {
moduleServer(id, function(input, output, session) {
  
  observeEvent(input$RunReporte, {
    shinyalert(
      "Ingrese un titulo para el informe.", 
      type = "input",
      inputType = 'text',
      callbackR = function(x) {
        if (is.null(x) || x == "") {
          shinyalert("Ingrese un nombre válido para el informe.", type = "warning")
        } else {
          shinyalert(
            "Ingrese su nombre para el informe.", 
            type = "input",
            inputType = 'text',
            callbackR = function(x1) {
              if (is.null(x) || x1 == "") {
                shinyalert("Ingrese un nombre válido para el informe.", type = "warning")
              } else {
          
          Nombre <- x1
          Reporte<-x
          
          
          L <- list(
            Contener$obtenerDatos("Reservorios"),
            Contener$obtenerDatos("MCReservorio"),
            Contener$obtenerDatos("AreaReferencia"),
            Contener$obtenerDatos("MCReferencia"),
            Contener$obtenerDatos("AreaResultados"),
            Contener$obtenerDatos("MCResultados"),
            Contener$obtenerDatos("MCDeltasArea"),
            Contener$obtenerDatos("MCDeltas"),
            Contener$obtenerDatos("MCBalanceFE"),
            Contener$obtenerDatos("BalanceTotal")
          )
          
          print(L)
          tempF <- paste0(getwd(), '/datos.rds')
          saveRDS(L, file = tempF)
          
          contenido_html <- fRmd(tempF = tempF, formato = "html", Nombre = Nombre, Reporte=Reporte, CI=CI)
          temp_file <- paste0(getwd(), '/report.rmd')
          
          writeLines(contenido_html, temp_file)
          render(temp_file)
          document_html <- paste0(strsplit(temp_file, '.rmd', fixed = TRUE)[[1]][1], '.html')
          
          output$Reporte <- renderUI({
            HTML_content <- readLines(document_html)
            HTML_output <- paste(HTML_content, collapse = "\n")
            HTML(HTML_output)
          })
        }
        
      })
        }
      })
  })
  
  ###########
  #saveRDS(datos, file = "datos.rds")
  ###########
  
  output$descargaReport <- 
    downloadHandler(
    
    filename = function() {
      if(input$Formato == 'Excel') {
        return("reporte.xlsx")
      } else if(input$Formato == 'Word') {
        return("reporte.docx")
      } else {
        return("reporte.PDF")
      }
    },
    content = function(file) {
      
      L <- list(
        Contener$obtenerDatos("Reservorios"),
        Contener$obtenerDatos("MCReservorio"),
        Contener$obtenerDatos("AreaReferencia"),
        Contener$obtenerDatos("MCReferencia"),
        Contener$obtenerDatos("AreaResultados"),
        Contener$obtenerDatos("MCResultados"),
        Contener$obtenerDatos("MCDeltasArea"),
        Contener$obtenerDatos("MCDeltas"),
        Contener$obtenerDatos("MCBalanceFE"),
        Contener$obtenerDatos("BalanceTotal")
      )
     # formato <- isolate(input$Formato)
      
      if(input$Formato == 'Excel') {
        contenido<-ReporteExcel(formato=input$Formato, fRmd, L, Nombre, Reporte, CI)
        writeBin(contenido, file)
      } else if(input$Formato == 'Word') {
        contenido <- ReporteExcel(formato = input$Formato, fRmd, L, Nombre, Reporte,CI)
        writeLines(contenido, 'report.Rmd')
        rmarkdown::render('report.Rmd', output_file = file, output_format = "word_document")
      } else if(input$Formato == 'PDF') {
        contenido <- ReporteExcel(formato = input$Formato, fRmd, L, Nombre,CI)
        writeBin(charToRaw(contenido), file)
      }
    }
    )
})
}


Moduloayuda <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}