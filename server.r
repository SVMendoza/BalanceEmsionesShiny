server <- function(input, output, session) {
  
  
  datosReactGlob<-reactiveValues(datosG = NULL)

  
  
  ReservMC <- fServer0("modulo0")
  dtRes<-ModuloPropagacionMC('modulo01', datos = ReservMC)
  
  dtREF<-fServerAct('modulo1')
  dtREFMC<-ModuloPropagacionMCREF('modulo11', dts1=dtREF, dts2=dtRes)
  dtRESUL<-fServerAct('modulo2')
  dtRESULMC<-ModuloPropagacionMCRES('modulo21', dts1=dtRESUL, dts2=dtRes)
  
  dtBalance<-ModuloPropagacionMCBalance('modulo3', dts1=dtRESULMC, dts2=dtREFMC) 
  
  dtBalanceTotal<-ModuloPropagacionMCBalanceXFeTotal('modulo31', dtsTotal=dtBalance)
  dtBalanceTotalEnd<-ModuloPropagacionMCBalanceTotal('modulo32', dtsTotall=dtBalanceTotal)
  
  Moduloreporte('modulo4', datosRes = ReservMC,datosResMC=dtRes,  datosActRef=dtREF, 
                datosActRefMC=dtREFMC, datosActReS=dtRESUL, datosActReSMC=dtRESULMC, 
                datosBalanceMC=dtBalance, datosBalancexFExYrsMC=dtBalanceTotal, datosBalanceTotalXyrs=dtBalanceTotalEnd) 
  observeEvent(input$resetButton, {
    rm(list = ls())
    session$reload()
  })
  
}
