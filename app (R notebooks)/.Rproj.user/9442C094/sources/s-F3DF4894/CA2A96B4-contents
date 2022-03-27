require(shiny)
source("./global.r")

server <- function(input, output, session) {
  
  remate1 <- reactive({
    columnas <- input$filtroremate1
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("remate-",columnas[i],sep = "")
      }
    }
    else
      columnas <- 0
    return(columnas)
  })
  
  pase1 <- reactive({
    columnas <- input$filtropases1
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
      columnas[i] <-  paste("pase-",columnas[i],sep = "")
    }
    }
    else
      columnas <- 0
    return(columnas)
  })
  
  centros1 <- reactive({
    columnas <- input$filtrocentros1
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("centro-",columnas[i],sep = "")
    }}
    else
      columnas <- 0
    return(columnas)
  })
  
  duelos1 <- reactive({
    columnas <- input$filtroduelos1
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
      columnas[i] <-  paste("duelo-",columnas[i],sep = "")
    }}
    else
      columnas <- 0
    return(columnas)
  })
  
  faltas1 <- reactive({
    columnas <- input$filtrofaltas1
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
      columnas[i] <-  paste("falta-",columnas[i],sep = "")
    }}
    else
      columnas <- 0
    return(columnas)
  })
  
  recuperacion1 <- reactive({
    columnas <- input$filtrorecuperacion1
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
      columnas[i] <-  paste("recuperacion-",columnas[i],sep = "")
    }}
    else
      columnas <- 0
    return(columnas)
  })
  
  parado1 <- reactive({
    columnas <- input$filtroparada1
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
      columnas[i] <-  paste("parado-",columnas[i],sep = "")
    }}
    else
      columnas <- 0
    return(columnas)
  })
  
  perdida1 <- reactive({
    columnas <- input$filtroperdida1
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
      columnas[i] <-  paste("perdida-",columnas[i],sep = "")
    }}
    else
      columnas <- 0
    return(columnas)
  })
  
  arquero1 <- reactive({
    columnas <- input$filtroarquero1
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
      columnas[i] <-  paste("arquero-",columnas[i],sep = "")
    }}
    else
      columnas <- 0
    return(columnas)
  })
  

  output$value1 <- renderDataTable({ df <-  soccer %>% 
      select(Minuto.video,segundos,Jugador.1,Jugador.2,remate1(),pase1(),centros1(),duelos1(),faltas1(),recuperacion1(),parado1(),
             perdida1(),arquero1(),inicial_x,inicial_y,final_x,final_y, partido) %>% 
      filter(Jugador.1 %in% input$jugador1) 
  if(ncol(df) > 3){
    salida <- data.frame(Minuto.video = as.numeric(),
                         Jugador.1 = as.numeric(),
                         Jugador.2 = as.numeric())
    for (i in 4:ncol(df)) {
      tempsalida <- df %>% 
        filter(df[i] == 1)  
      salida <- full_join(salida,tempsalida)
    }
    
  }else{
    salida <- df
  }
    return(salida)
  })

  
  
  output$plot1 <- renderPlotly({
  
  df <-  soccer %>% 
    select(Minuto.video,Jugador.1,Jugador.2,remate1(),pase1(),centros1(),duelos1(),faltas1(),recuperacion1(),parado1(),
           perdida1(),arquero1(),inicial_x,inicial_y,final_x,final_y, partido, Evento, Subevento, Calificaciondelevento) %>% 
    filter(Jugador.1 %in% input$jugador1) 
  if(ncol(df) > 3){
    soccer_estandar_destino <- data.frame(Minuto.video = as.numeric(),
                         Jugador.1 = as.numeric(),
                         Jugador.2 = as.numeric())
    for (i in 4:ncol(df)) {
      tempsalida <- df %>% 
        filter(df[i] == 1)  
      soccer_estandar_destino <- full_join(soccer_estandar_destino,tempsalida)
    }
    
  }else{
    soccer_estandar_destino <- df
  }
    

  ggplotly(ggplot(soccer_estandar_destino) +
      annotate_pitch(colour = "white",
                     fill   = "#7fc47f",
                     limits = TRUE,
                     dimensions = pitch_opta) +
      geom_segment(aes(x = inicial_x, y = inicial_y, xend = final_x, yend = final_y, colour=Calificaciondelevento),
                   arrow = arrow(length = unit(0.25, "cm"),
                                 type = "closed")) +
      theme_pitch() +
      geom_point(aes(x = inicial_x, y = inicial_y, fill = Calificaciondelevento),
                 colour = "black",
                 pch = 21,
                 size = 5)  +
      direction_label(
        x_label = 50,
        y_label = -3,
        label_length = 20,
        colour = "dimgray") +
      annotate("text", x=soccer_estandar_destino$inicial_x,y=soccer_estandar_destino$inicial_y, label=soccer_estandar_destino$Jugador.1, color="white", size=3.5)
)
  })
  
  remate2 <- reactive({
    columnas <- input$filtroremate2
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("remate-",columnas[i],sep = "")
      }
    }
    else
      columnas <- 0
    return(columnas)
  })
  
  pase2 <- reactive({
    columnas <- input$filtropases2
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("pase-",columnas[i],sep = "")
      }
    }
    else
      columnas <- 0
    return(columnas)
  })
  
  centros2 <- reactive({
    columnas <- input$filtrocentros2
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("centro-",columnas[i],sep = "")
      }}
    else
      columnas <- 0
    return(columnas)
  })
  
  duelos2 <- reactive({
    columnas <- input$filtroduelos2
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("duelo-",columnas[i],sep = "")
      }}
    else
      columnas <- 0
    return(columnas)
  })
  
  faltas2 <- reactive({
    columnas <- input$filtrofaltas2
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("falta-",columnas[i],sep = "")
      }}
    else
      columnas <- 0
    return(columnas)
  })
  
  recuperacion2 <- reactive({
    columnas <- input$filtrorecuperacion2
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("recuperacion-",columnas[i],sep = "")
      }}
    else
      columnas <- 0
    return(columnas)
  })
  
  parado2 <- reactive({
    columnas <- input$filtroparada2
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("parado-",columnas[i],sep = "")
      }}
    else
      columnas <- 0
    return(columnas)
  })
  
  perdida2 <- reactive({
    columnas <- input$filtroperdida2
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("perdida-",columnas[i],sep = "")
      }}
    else
      columnas <- 0
    return(columnas)
  })
  
  arquero2 <- reactive({
    columnas <- input$filtroarquero2
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("arquero-",columnas[i],sep = "")
      }}
    else
      columnas <- 0
    return(columnas)
  })
  
  
  output$value2 <- renderDataTable({ df <-  soccer %>% 
    select(Minuto.video,segundos,Jugador.1,Jugador.2,remate2(),pase2(),centros2(),duelos2(),faltas2(),recuperacion2(),parado2(),
           perdida2(),arquero2(),inicial_x,inicial_y,final_x,final_y, partido) %>% 
    filter(partido %in% input$partido2) 
  if(ncol(df) > 3){
    salida <- data.frame(Minuto.video = as.numeric(),
                         Jugador.1 = as.numeric(),
                         Jugador.2 = as.numeric())
    for (i in 4:ncol(df)) {
      tempsalida <- df %>% 
        filter(df[i] == 1)  
      salida <- full_join(salida,tempsalida)
    }
    
  }else{
    salida <- df
  }
  return(salida)
  })
  
  
  
  output$plot2 <- renderPlotly({
    
    df <-  soccer %>% 
      select(Minuto.video,Jugador.1,Jugador.2,remate2(),pase2(),centros2(),duelos2(),faltas2(),recuperacion2(),parado2(),
             perdida2(),arquero2(),inicial_x,inicial_y,final_x,final_y, partido) %>% 
      filter(partido %in% input$partido2) 
    if(ncol(df) > 3){
      soccer_estandar_destino <- data.frame(Minuto.video = as.numeric(),
                                            Jugador.1 = as.numeric(),
                                            Jugador.2 = as.numeric())
      for (i in 4:ncol(df)) {
        tempsalida <- df %>% 
          filter(df[i] == 1)  
        soccer_estandar_destino <- full_join(soccer_estandar_destino,tempsalida)
      }
      
    }else{
      soccer_estandar_destino <- df
    }
    
    
    ggplotly( ggplot(soccer_estandar_destino) +
      annotate_pitch(colour = "white",
                     fill   = "#7fc47f",
                     limits = TRUE,
                     dimensions = pitch_opta) +
      geom_segment(aes(x = inicial_x, y = inicial_y, xend = final_x, yend = final_y, colour=Jugador.1),
                   arrow = arrow(length = unit(0.25, "cm"),
                                 type = "closed")) +
      theme_pitch() +
      geom_point(aes(x = inicial_x, y = inicial_y, fill = Jugador.1),
                 colour = "black",
                 pch = 21,
                 size = 5)  +
      direction_label(
        x_label = 50,
        y_label = -3,
        label_length = 20,
        colour = "dimgray") +
      annotate("text", x=soccer_estandar_destino$inicial_x,y=soccer_estandar_destino$inicial_y, label=soccer_estandar_destino$Jugador.1, color="white", size=3.5)
    )
  })
  
  remate3 <- reactive({
    columnas <- input$filtroremate3
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("remate-",columnas[i],sep = "")
      }
    }
    else
      columnas <- 0
    return(columnas)
  })
  
  pase3 <- reactive({
    columnas <- input$filtropases3
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("pase-",columnas[i],sep = "")
      }
    }
    else
      columnas <- 0
    return(columnas)
  })
  
  centros3 <- reactive({
    columnas <- input$filtrocentros3
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("centro-",columnas[i],sep = "")
      }}
    else
      columnas <- 0
    return(columnas)
  })
  
  duelos3 <- reactive({
    columnas <- input$filtroduelos3
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("duelo-",columnas[i],sep = "")
      }}
    else
      columnas <- 0
    return(columnas)
  })
  
  faltas3 <- reactive({
    columnas <- input$filtrofaltas3
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("falta-",columnas[i],sep = "")
      }}
    else
      columnas <- 0
    return(columnas)
  })
  
  recuperacion3 <- reactive({
    columnas <- input$filtrorecuperacion3
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("recuperacion-",columnas[i],sep = "")
      }}
    else
      columnas <- 0
    return(columnas)
  })
  
  parado3 <- reactive({
    columnas <- input$filtroparada3
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("parado-",columnas[i],sep = "")
      }}
    else
      columnas <- 0
    return(columnas)
  })
  
  perdida3 <- reactive({
    columnas <- input$filtroperdida3
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("perdida-",columnas[i],sep = "")
      }}
    else
      columnas <- 0
    return(columnas)
  })
  
  arquero3 <- reactive({
    columnas <- input$filtroarquero3
    if(length(columnas)>0){
      for (i in 1:length(columnas)) {
        columnas[i] <-  paste("arquero-",columnas[i],sep = "")
      }}
    else
      columnas <- 0
    return(columnas)
  })
  
  
  output$value3 <- renderDataTable({ df <-  soccer %>% 
    select(Minuto.video,Jugador.1,Jugador.2,remate3(),pase3(),centros3(),duelos3(),faltas3(),recuperacion3(),parado3(),
           perdida3(),arquero3(),inicial_x,inicial_y,final_x,final_y, partido) %>% 
    filter(partido %in% input$partido3, Jugador.1 %in% input$jugador3) 
  if(ncol(df) > 3){
    salida <- data.frame(Minuto.video = as.numeric(),
                         Jugador.1 = as.numeric(),
                         Jugador.2 = as.numeric())
    for (i in 4:ncol(df)) {
      tempsalida <- df %>% 
        filter(df[i] == 1)  
      salida <- full_join(salida,tempsalida)
    }
    
  }else{
    salida <- df
  }
  return(salida)
  })
  
  
  
  output$plot3 <- renderPlotly({
    
    df <-  soccer %>% 
      select(Minuto.video,Jugador.1,Jugador.2,remate3(),pase3(),centros3(),duelos3(),faltas3(),recuperacion3(),parado3(),
             perdida3(),arquero3(),inicial_x,inicial_y,final_x,final_y, partido,Evento,Subevento) %>% 
      filter(partido %in% input$partido3, Jugador.1 %in% input$jugador3) 
    if(ncol(df) > 3){
      soccer_estandar_destino <- data.frame(Minuto.video = as.numeric(),
                                            Jugador.1 = as.numeric(),
                                            Jugador.2 = as.numeric())
      for (i in 4:ncol(df)) {
        tempsalida <- df %>% 
          filter(df[i] == 1)  
        soccer_estandar_destino <- full_join(soccer_estandar_destino,tempsalida)
      }
      
    }else{
      soccer_estandar_destino <- df
    }
    
    
    ggplotly( ggplot(soccer_estandar_destino) +
      annotate_pitch(colour = "white",
                     fill   = "#7fc47f",
                     limits = TRUE,
                     dimensions = pitch_opta) +
      geom_segment(aes(x = inicial_x, y = inicial_y, xend = final_x, yend = final_y, colour=Evento),
                   arrow = arrow(length = unit(0.25, "cm"),
                                 type = "closed")) +
      theme_pitch() +
      geom_point(aes(x = inicial_x, y = inicial_y, fill = Evento),
                 colour = "black",
                 pch = 21,
                 size = 5)  +
      direction_label(
        x_label = 50,
        y_label = -3,
        label_length = 20,
        colour = "dimgray") +
      annotate("text", x=soccer_estandar_destino$inicial_x,y=soccer_estandar_destino$inicial_y, label=soccer_estandar_destino$Jugador.1, color="white", size=3.5)
)
  })
  
}