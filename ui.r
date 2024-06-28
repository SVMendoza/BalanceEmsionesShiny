ui <- fluidPage(
  theme = shinythemes::shinytheme("lumen"),  # Tema responsivo con tonos pastel
  
  tags$head(
    tags$style(HTML("
      body {background-color: #e8f5e9;}  /* Fondo verde claro */
      .tabbable > .nav > li > a {color: #2e7d32; background-color: #a5d6a7;} /* Color verde oscuro para las pestañas */
      .tabbable > .nav > li.active > a {background-color: #388e3c !important; color: #ffffff !important;} /* Fondo verde oscuro para la pestaña activa */
      .navbar-default {background-color: #388e3c; border-color: #2e7d32;}  /* Navbar en verde oscuro */
      .btn-custom {background-color: #2e7d32; color: white;}  /* Botones personalizados en verde oscuro */
      .shiny-output-error { color: #f44336; } /* Color de error en rojo */

      .tab-content {background-color: #ffffff; padding: 20px; border: 1px solid #2e7d32;} /* Fondo blanco con borde verde oscuro */
      .form-group {margin-bottom: 15px;}  /* Espaciado entre grupos de formularios */

      @media (max-width: 768px) { /* Estilos para pantallas pequeñas */
        .tabbable > .nav > li > a {font-size: 14px;} /* Reducir el tamaño de fuente en las pestañas */
        .btn-custom {padding: 10px 20px;} /* Ajustar el tamaño de los botones */
      }

      /* Estilos para los elementos de los formularios */
      .form-control {border-radius: 0; border: 1px solid #2e7d32;} /* Borde de los inputs en verde oscuro */
      .selectize-input {border-radius: 0; border: 1px solid #2e7d32;} /* Borde de los selectize inputs en verde oscuro */
    "))
  ),
  
  titlePanel(
    div(
      style = "display: flex; align-items: center; justify-content: space-between;",
      span(
        style = "display: flex; align-items: center;",
        img(src = "logo.png", height = 65), 
        h2("Estimación de los balances de emisiones de CO2eq", style = "color: #2e7d32; margin-left: 15px;")
      ),
      tags$a(
        href = "https://github.com/SVMendoza/BalanceEmsionesShiny",
        tags$i(class = "fab fa-github", style = "font-size: 30px; color: black; margin-right: 15px;")
      ),
      # Botón con icono de árbol
      )
  ),
  actionButton(
    "resetButton", 
    label = tagList(icon("broom"), " Limpiar Sesión"), 
    style = "position: fixed; bottom: 20px; right: 20px;", 
    class = "btn btn-custom"
  ),
  
  tabsetPanel(
    tabPanel(
      title = tagList(icon("leaf"), " Reservorios"),
      tabsetPanel(
        tabPanel(
          title = tagList(icon("check"), "Entrada"),
          fluidRow(
            column(12, ReservorioUI('modulo0'))  # Formularios bien acoplados dentro de la pestaña
          )
        ),
        tabPanel(
          title = tagList(icon("chart-bar"), " Resultado"),
          fluidRow(
            column(12, ParaEntradaUI0('modulo01'))  # Formularios bien acoplados dentro de la pestaña
          )
        )
      )
    ),
    
    tabPanel(
      title = tagList(icon("database"), " Datos de actividad (referencia)"),
      tabsetPanel(
        tabPanel(
          title = tagList(icon("check"), " Entrada"),
          fluidRow(
            column(12, ActUI('modulo1'))  # Formularios bien acoplados dentro de la pestaña
          )
        ),
        tabPanel(
          title = tagList(icon("chart-line"), " Resultado"),
          fluidRow(
            column(12, ParaEntradaUI('modulo11'))  # Formularios bien acoplados dentro de la pestaña
          )
        )
      )
    ),
    
    tabPanel(
      title = tagList(icon("database"), " Datos de actividad (resultados)"),
      tabsetPanel(
        tabPanel(
          title = tagList(icon("check"), " Entrada"),
          fluidRow(
            column(12, ActUI('modulo2'))  # Formularios bien acoplados dentro de la pestaña
          )
        ),
        tabPanel(
          title = tagList(icon("chart-line"), " Resultado"),
          fluidRow(
            column(12, ParaEntradaUI('modulo21'))  # Formularios bien acoplados dentro de la pestaña
          )
        )
      )
    ),
    
    tabPanel(
      title = tagList(icon("balance-scale"), " Balance emisiones"),
      tabsetPanel(
        tabPanel(
          title = tagList(icon("chart-pie"), " Balance por estrato"),
          fluidRow(
            column(12, ParaEntradaUI('modulo3'))  # Formularios bien acoplados dentro de la pestaña
          )
        ),
        tabPanel(
          title = tagList(icon("layer-group"), " Balance por factor de emisión"),
          fluidRow(
            column(12, ParaEntradaUI('modulo31'))  # Formularios bien acoplados dentro de la pestaña
          )
        ),
        tabPanel(
          title = tagList(icon("chart-area"), " Balance total"),
          fluidRow(
            column(12, ParaEntradaUI('modulo32'))  # Formularios bien acoplados dentro de la pestaña
          )
        )
      )
    ),
    
    tabPanel(
      title = tagList(icon("save"), " Guardar Reporte"),
      fluidRow(
        column(12, Report('modulo4'))  # Formularios bien acoplados dentro de la pestaña
      )
    ),
    tabPanel(
      title = tagList(icon("info"), ""),
      fluidRow(
        column(12, Ayuda('modulo5'))  # Formularios bien acoplados dentro de la pestaña
      )
    )
  )
)
