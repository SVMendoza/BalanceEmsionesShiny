server <- function(input, output, session) {
  
  
  ModuloReservorios("modulo0")
    ModuloPropagReservMC('modulo01')
  
  ModuloActRef('modulo1')
    ModuloPropagMCRef('modulo11')
    
  ModuloActRes('modulo2')
    ModuloPropagMCRes('modulo21')
  
  ModuloPropagMCRefRes('modulo3') 
  ModuloPropagMCBalanceFe ('modulo31')
  ModuloPropagMCBalanceTotal('modulo32')
  
  Moduloreporte('modulo4') 
  Moduloayuda('modulo5') 
  
  observeEvent(input$resetButton, {
    rm(list = ls())
    session$reload()
    Contener$DeleteDatos(nombre=NULL)
    filesL<-dir(path=getwd() ,pattern="reporte")
    file.remove(file.path(getwd(), filesL))
    if(exists(paste0(getwd(),'datos.rds')))  file.remove(file.path(getwd(), 'datos.rds'))
  })
}


