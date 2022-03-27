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
      fluidRow(
      column(6,
             br(),
             checkboxGroupInput('jugador1', 'Jugador', filterjugador, selected = 2, inline = TRUE ,width = "100%"),
             hr()
      )
      
    ),
    fluidRow(
      column(3,
             selectInput('filtroremate1', 'Remate', filtroeventosremate, multiple=TRUE, selectize=FALSE)
      ),
      column(3,
             selectInput('filtropases1', 'Pases', filtroeventospases, multiple=TRUE, selected = "Completado",  selectize=FALSE)
      ),
      column(3,
             selectInput('filtrocentros1', 'Centros', filtroeventoscentros, multiple=TRUE, selectize=FALSE)
      ),
      column(3,
             selectInput('filtroduelos1', 'Duelos', filtroeventosduelos, multiple=TRUE, selectize=FALSE)
      )
    ),
    fluidRow(
      column(3,
             selectInput('filtrofaltas1', 'Faltas', filtroeventosfaltas, multiple=TRUE, selectize=FALSE),
             hr()
      ),
      column(3,
             selectInput('filtrorecuperacion1', 'Recuperaciones', filtroeventosrecuperacion, multiple=TRUE, selectize=FALSE),
             hr()
      ),
      column(3,
             selectInput('filtroparada1', 'Pelota Parada', filtroeventosparada, multiple=TRUE, selectize=FALSE),
             hr()
      ),
      column(3,
             selectInput('filtroperdida1', 'Perdidas', filtroeventosperdida, multiple=TRUE, selectize=FALSE),
             hr()
      )
      
    ),
    fluidRow(
      column(3,
             selectInput('filtroarquero1', 'Arquero', filtroeventosarquero, multiple=TRUE, selectize=FALSE),
      ),
      column(8,
      tabsetPanel(
        tabPanel("Plot", plotlyOutput("plot1"),height = "auto", width="auto"),
        tabPanel("Table Evento", dataTableOutput("value1"),height = "auto",width="auto")
    )
    ))
    ),
    tabPanel("Por Partido",
             fluidRow(
              column(6,
                     br(),
                     selectInput('partido2', 'Partido', filterpartido, multiple=FALSE, selectize=FALSE),
              )),
              fluidRow(
                column(3,
                       selectInput('filtroremate2', 'Remate', filtroeventosremate, multiple=TRUE, selectize=FALSE)
                ),
                column(3,
                       selectInput('filtropases2', 'Pases', filtroeventospases, selected = "Completado",multiple=TRUE, selectize=FALSE)
                ),
                column(3,
                       selectInput('filtrocentros2', 'Centros', filtroeventoscentros, multiple=TRUE, selectize=FALSE)
                ),
                column(3,
                       selectInput('filtroduelos2', 'Duelos', filtroeventosduelos, multiple=TRUE, selectize=FALSE)
                )
              ),
              fluidRow(
                column(3,
                       selectInput('filtrofaltas2', 'Faltas', filtroeventosfaltas, multiple=TRUE, selectize=FALSE),
                       hr()
                ),
                column(3,
                       selectInput('filtrorecuperacion2', 'Recuperaciones', filtroeventosrecuperacion, multiple=TRUE, selectize=FALSE),
                       hr()
                ),
                column(3,
                       selectInput('filtroparada2', 'Pelota Parada', filtroeventosparada, multiple=TRUE, selectize=FALSE),
                       hr()
                ),
                column(3,
                       selectInput('filtroperdida2', 'Perdidas', filtroeventosperdida, multiple=TRUE, selectize=FALSE),
                       hr()
                )
                
              ),
              fluidRow(
                column(3,
                       selectInput('filtroarquero2', 'Arquero', filtroeventosarquero, multiple=TRUE, selectize=FALSE),
                ),
                column(8,
                       tabsetPanel(
                         tabPanel("Plot", plotlyOutput("plot2"),height = "auto",width="auto"),
                         tabPanel("Table Evento", dataTableOutput("value2"),height = "auto",width="auto")
                       )
                ))
    ),
    tabPanel("Por Jugador Por Partidos",
             fluidRow(
                column(3,
                       br(),
                   selectInput('partido3', 'Partido', filterpartido,selected = "Banfield", multiple=TRUE, selectize=FALSE),
                ),
                column(6,
                       br(),br(),br(),
                  checkboxGroupInput('jugador3', 'Jugador', filterjugador, selected = 2, inline = TRUE ,width = "100%"),
                  hr()
                )),
             fluidRow(
               column(3,
                      selectInput('filtroremate3', 'Remate', filtroeventosremate, multiple=TRUE, selectize=FALSE)
               ),
               column(3,
                      selectInput('filtropases3', 'Pases', filtroeventospases, selected = "Completado",multiple=TRUE, selectize=FALSE)
               ),
               column(3,
                      selectInput('filtrocentros3', 'Centros', filtroeventoscentros, multiple=TRUE, selectize=FALSE)
               ),
               column(3,
                      selectInput('filtroduelos3', 'Duelos', filtroeventosduelos, multiple=TRUE, selectize=FALSE)
               )
             ),
             fluidRow(
               column(3,
                      selectInput('filtrofaltas3', 'Faltas', filtroeventosfaltas, multiple=TRUE, selectize=FALSE),
                      hr()
               ),
               column(3,
                      selectInput('filtrorecuperacion3', 'Recuperaciones', filtroeventosrecuperacion, multiple=TRUE, selectize=FALSE),
                      hr()
               ),
               column(3,
                      selectInput('filtroparada3', 'Pelota Parada', filtroeventosparada, multiple=TRUE, selectize=FALSE),
                      hr()
               ),
               column(3,
                      selectInput('filtroperdida3', 'Perdidas', filtroeventosperdida, multiple=TRUE, selectize=FALSE),
                      hr()
               )
               
             ),
             fluidRow(
               column(3,
                      selectInput('filtroarquero3', 'Arquero', filtroeventosarquero, multiple=TRUE, selectize=FALSE),
               ),
               column(8,
                      tabsetPanel(
                        tabPanel("Plot", plotlyOutput("plot3"),height = "auto",width="auto"),
                        tabPanel("Table Evento", dataTableOutput("value3"),height = "auto",width="auto")
                      )
               ))
  )
  )
  )
    


