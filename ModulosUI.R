## Modulos Paneles
ReservorioUI<- function(id) {
  tagList(
    sidebarPanel(
      textInput(inputId = NS(id, "FE"), label = "Factor de emisión", value = "DEF"),
      textInput(inputId = NS(id, "Reserv"), label = "Reservorio", value = "Tree"),
      textInput(inputId = NS(id, "Estrat"), label = "Estrato", value = "1"),
      numericInput(inputId = NS(id, "X"), label = "Media", value = 200),
      numericInput(inputId = NS(id, "XEE"), label = "Error estandar", value = 2),
      fluidRow(
        column(width = 6,
               actionButton(inputId = NS(id, "Add"), label = "Agregar valor")
        ),
      )
    ),mainPanel(
      DT::DTOutput(outputId = NS(id, "reserv"))
    )
  )
}

#############
ParaEntradaUI0<- function(id) {
  tagList(
    sidebarPanel(
      numericInput(NS(id, "nseed"), "Semillas:", value = 126, min = 100, max = 200),
      numericInput(NS(id, "num_simulaciones"), "Número de Simulaciones:", value = 10000, min = 1000, max = 50000, step =1000),
      numericInput(NS(id, "IC"), "Nivel de confianza:", value = 95, min = 80, max = 99),
      numericInput(inputId = NS(id, "RMCarbono"), label = "Factor de conversión", min = 0, max = 100, step = NA, value = 3.666667),
      actionButton(inputId = NS(id, "Run"), label = "Ejecutar")
  ),mainPanel(
      DT::dataTableOutput(outputId = NS(id, "RESULT"))
    )
  )
}


ParaEntradaUI<- function(id) {
  tagList(
    sidebarPanel(
      numericInput(NS(id, "nseed"), "Semillas:", value = 126, min = 100, max = 200),
      numericInput(NS(id, "num_simulaciones"), "Número de Simulaciones:", value = 10000, min = 1000, max = 50000, step =1000),
      numericInput(NS(id, "IC"), "Nivel de confianza:", value = 90, min = 80, max = 99),
      actionButton(inputId = NS(id, "Run"), label = "Ejecutar")
    ),mainPanel(
     
      DT::dataTableOutput(outputId = NS(id, "RESULT"))
    )
  )
}

#############

ActUI <- function(id) {
  tagList(
    sidebarPanel(
      textInput(inputId = NS(id, "FE"), label = "Factor de emisión", value = "DEF"),
      textInput(inputId = NS(id, "yrs"), label = "Años", value = "2020"),
      textInput(inputId = NS(id, "Estrat"), label = "Estrato", value = "1"),
      numericInput(inputId = NS(id, "X"), label = "Media", value = 200),
      numericInput(inputId = NS(id, "XEE"), label = "Error estandar", value = 2),
      fluidRow(
        column(width = 6,
               actionButton(inputId = NS(id, "Add"), label = "Agregar valor")
        )
      )
    ),
    mainPanel(
      DT::dataTableOutput(outputId = NS(id, "ActRefer"))
    )
  )
}


####### PANEL RESUMEN

Report <- function(id) {
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(width = 8,
               actionButton(inputId = NS(id, "RunReporte"), label = HTML('<i class="bi bi-file-text-fill"></i>Generar reporte'))
        )
      ),
      tags$hr(),  
      downloadButton(outputId = NS(id, "descargaReport"), label='Descarga'),
      #actionButton(inputId = NS(id, "descargaReport"), label='Descarga'),  # Cambio a actionButton
      #tags$hr(),  
      selectInput(inputId = NS(id, "Formato"), "Seleccione el formato de salida:",
               choices = c("Word", "Excel"), selected = "Word"),
      tags$hr(), 
      fluidRow(
        column(width = 8,
               actionButton(inputId = NS(id, "save"), label = "Guardar datos")
        )
      ),
    ),
    mainPanel(
      htmlOutput(outputId = NS(id, "Reporte"))
    )
  )
}




Ayuda <- function(id) {
  mainPanel()
}