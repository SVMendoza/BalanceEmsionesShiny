ui <- fluidPage(
  titlePanel(title = span(img(src = "logo.png", height = 65),"Estimación de las reducciones de emisiones")),
  tabsetPanel(
    tabPanel("Reservorios", tabsetPanel(
      tabPanel("Entrada",ReservorioUI('modulo0')),
      tabPanel('result', ParaEntradaUI('modulo01'))
    )),      
    
    tabPanel("Datos de actividad (referencia)", tabsetPanel(
      tabPanel("Entrada", ActUI('modulo1')),
      tabPanel('result', ParaEntradaUI('modulo11')))),
    tabPanel("Datos de actividad (resultados)", tabsetPanel(
      tabPanel("Entrada", ActUI('modulo2')),
      tabPanel('result', ParaEntradaUI('modulo21')))),
    
    tabPanel("Balance emisiones", tabsetPanel(
      tabPanel('Resultados balance', ParaEntradaUI('modulo3')),
      tabPanel('Balance por estrato', ParaEntradaUI('modulo31')),
      tabPanel('Balance total', ParaEntradaUI('modulo32')))),
    tabPanel("Guardar Reporte" ,Report('modulo4'))
    
  ) 
)

