## Modulos Paneles

ReservorioUI<- function(id) {
  tagList(
    sidebarPanel(
      textInput(inputId = NS(id, "FE"), label = "Factor de emisión", value = "DEF"),
      textInput(inputId = NS(id, "Reserv"), label = "Reservorio", value = "Tree"),
      textInput(inputId = NS(id, "Estrat"), label = "Estrato", value = "1"),
      numericInput(inputId = NS(id, "X"), label = "Media", value = 200),
      numericInput(inputId = NS(id, "XEE"), label = "Error estandar", value = 2),
      #actionButton(inputId = NS(id, "Add"), label = "Agregar valor"),
      fluidRow(
        column(width = 6,
               actionButton(inputId = NS(id, "Add"), label = "Agregar valor")
        )#,
        # column(width = 6,
        #        actionButton(inputId = NS(id, "reserv_cell_clicked"), label = "Actualizar")
        # )
      )
    ),mainPanel(
      DT::dataTableOutput(outputId = NS(id, "reserv"))
    )
  )
}

#############

ParaEntradaUI<- function(id) {
  tagList(
    sidebarPanel(
      numericInput(NS(id, "nseed"), "Semillas:", value = 126, min = 100, max = 200),
      numericInput(NS(id, "num_simulaciones"), "Número de Simulaciones:", value = 10000, min = 1000, max = 50000, step =1000),
      numericInput(NS(id, "IC"), "Nivel de confianza:", value = 95, min = 80, max = 99),
      selectInput(NS(id,'Propaga'), 'Tipo de propagación',
                  choices = c("Adición", "Sustracción", 'Multiplicación'), 
                  selected = "Adición"),
      actionButton(inputId = NS(id, "Run"), label = "Ejecutar"),
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
        )#,
        #column(width = 6,
        #       actionButton(inputId = NS(id, "reserv_cell_clicked"), label = "Actualizar")
        # )
      )
    ),
    mainPanel(
      DT::dataTableOutput(outputId = NS(id, "ActRefer"))
    )
  )
}


####### PANEL RESUMEN

Report<- function(id) { sidebarLayout(
  sidebarPanel(
    actionButton(inputId = NS(id, "RunReporte"), label = "Generar reporte"),
    selectInput("formato", "Seleccione el formato de salida:",
                choices = c("Word", "PDF", "Excel")),
    downloadButton("downloadReport", "Descargar Reporte")
  ),
  mainPanel(
    uiOutput("renderedReporte")
  )
)
  
}

