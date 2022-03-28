library(shiny)
source("./global.r")

fluidPage(
  # theme = shinytheme("darkly"),
  
  includeCSS("styles.css"),
  
  # img(src="banner.jpg",height=100,width=1920),
  # titlePanel("Estudiantes de la Plata"),
  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
  tabsetPanel( 
    tabPanel("Por Jugador",
      sidebarLayout(
        sidebarPanel(
             checkboxGroupInput('jugador1', 'Jugador', filterjugador, selected = 2, inline = TRUE),
             selectizeInput('filtroremate1', 'Remate', filtroeventosremate, multiple=TRUE),
             selectizeInput('filtropases1', 'Pases', filtroeventospases, multiple=TRUE, selected = "Completado"),
             selectizeInput('filtrocentros1', 'Centros', filtroeventoscentros, multiple=TRUE),
             selectizeInput('filtroduelos1', 'Duelos', filtroeventosduelos, multiple=TRUE),
             selectizeInput('filtrofaltas1', 'Faltas', filtroeventosfaltas, multiple=TRUE),
             selectizeInput('filtrorecuperacion1', 'Recuperaciones', filtroeventosrecuperacion, multiple=TRUE),
             selectizeInput('filtroparada1', 'Pelota Parada', filtroeventosparada, multiple=TRUE),
             selectizeInput('filtroperdida1', 'Perdidas', filtroeventosperdida, multiple=TRUE),
             selectizeInput('filtroarquero1', 'Arquero', filtroeventosarquero, multiple=TRUE)
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Plot", plotlyOutput("plot1"),height = "auto", width="auto"),
            tabPanel("Table Evento", dataTableOutput("value1"),height = "auto",width="auto")
            )
        )
    )),
    tabPanel("Por Partido",
          sidebarLayout(
             sidebarPanel(
              selectInput('partido2', 'Partido', filterpartido, multiple=FALSE, selectize=FALSE),
              selectizeInput('filtroremate2', 'Remate', filtroeventosremate, multiple=TRUE),
              selectizeInput('filtropases2', 'Pases', filtroeventospases, selected = "Completado",multiple=TRUE),
              selectizeInput('filtrocentros2', 'Centros', filtroeventoscentros, multiple=TRUE),
              selectizeInput('filtroduelos2', 'Duelos', filtroeventosduelos, multiple=TRUE),
              selectizeInput('filtrofaltas2', 'Faltas', filtroeventosfaltas, multiple=TRUE),
              selectizeInput('filtrorecuperacion2', 'Recuperaciones', filtroeventosrecuperacion, multiple=TRUE),
              selectizeInput('filtroparada2', 'Pelota Parada', filtroeventosparada, multiple=TRUE),
              selectizeInput('filtroperdida2', 'Perdidas', filtroeventosperdida, multiple=TRUE),
              selectizeInput('filtroarquero2', 'Arquero', filtroeventosarquero, multiple=TRUE),
             ),
             mainPanel(
                         tabsetPanel(
                         tabPanel("Plot", plotlyOutput("plot2"),height = "auto",width="auto"),
                         tabPanel("Table Evento", dataTableOutput("value2"),height = "auto",width="auto")
                       )
                ))
    ),
    tabPanel("Por Jugador Por Partidos",
             sidebarLayout(
               sidebarPanel(
                   selectInput('partido3', 'Partido', filterpartido,selected = "Banfield", multiple=TRUE),
                   checkboxGroupInput('jugador3', 'Jugador', filterjugador, selected = 2, inline = TRUE),
                   selectizeInput('filtroremate3', 'Remate', filtroeventosremate, multiple=TRUE),
                   selectizeInput('filtropases3', 'Pases', filtroeventospases, selected = "Completado",multiple=TRUE),
                   selectizeInput('filtrocentros3', 'Centros', filtroeventoscentros, multiple=TRUE),
                   selectizeInput('filtroduelos3', 'Duelos', filtroeventosduelos, multiple=TRUE),
                   selectizeInput('filtrofaltas3', 'Faltas', filtroeventosfaltas, multiple=TRUE),
                   selectizeInput('filtrorecuperacion3', 'Recuperaciones', filtroeventosrecuperacion, multiple=TRUE),
                   selectizeInput('filtroparada3', 'Pelota Parada', filtroeventosparada, multiple=TRUE),
                   selectizeInput('filtroperdida3', 'Perdidas', filtroeventosperdida, multiple=TRUE),
                   selectizeInput('filtroarquero3', 'Arquero', filtroeventosarquero, multiple=TRUE),
               ),
               mainPanel(
                      tabsetPanel(
                        tabPanel("Plot", plotlyOutput("plot3"),height = "auto",width="auto"),
                        tabPanel("Table Evento", dataTableOutput("value3"),height = "auto",width="auto")
                      )
               ))
  )
  )
  )
    


