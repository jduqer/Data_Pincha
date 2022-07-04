# # Borrar variables del ambiente
# rm(list = objects())
# 
# # Carga de paquetes necesarios para hacer los gráficos
# 
# require(dplyr)
# require(tidyverse)
# require(shiny)
# require(readr)
# require(stringr)
# 

# 
# 
# #soccer <- read.csv("datos/9NA DIVISION VS BANFIELD FECHA 1 SUPERLIGA 16_03_2019.csv")
# soccer <- read.csv("datos/9NA DIVISION VS BANFIELD - tabla filtros.csv")
# 
# 
# soccer$Jugador.1 <- as.integer(soccer$Jugador.1)
# soccer$Jugador.2 <- as.integer(soccer$Jugador.2)
# soccer$Posicion.incial <- as.integer(soccer$Posicion.incial)
# soccer$Posicion.final <- as.integer(soccer$Posicion.final)
# 
# soccer$Evento <- tolower(soccer$Evento)
# soccer$Subevento <- tolower(soccer$Subevento)
# soccer$Evento <- str_trim(soccer$Evento)
# soccer$Subevento <- str_trim(soccer$Subevento)
# 
# filterevento <- unique(soccer$Evento)
# filterjugador <- unique(soccer$Jugador.1)
# 
# 
# soccer <- soccer[1:55,]
# soccer <- soccer %>% separate(coord.inicial,c('inicial_x','inicial_y')) %>% mutate(inicial_x=abs(as.numeric(inicial_x)),inicial_y=abs(as.numeric(inicial_y)-322))
# soccer <- soccer %>% separate(coord.final,c('final_x','final_y')) %>% mutate(final_x=abs(as.numeric(final_x)),final_y=abs(as.numeric(final_y)-322))
# 
# 
# 
# 
# 
# ggplot(vs_banfield_estandar_destino) +
#   annotate_pitch(colour = "white",
#                  fill   = "#7fc47f",
#                  limits = TRUE,
#                  dimensions = pitch_opta) +
#   geom_segment(aes(x = inicial_x, y = inicial_y-restar_y, xend = final_x, yend = final_y-restar_y),
#                arrow = arrow(length = unit(0.25, "cm"),
#                              type = "closed")) +
#   theme_pitch() +
#   geom_point(aes(x = inicial_x, y = inicial_y-restar_y),
#              colour = "black", 
#              fill = "darkred", 
#              pch = 21,
#              size = 5)  +
#   direction_label(
#     x_label = 50,
#     y_label = -3,
#     label_length = 20,
#     colour = "dimgray") +
#   annotate("text", x=vs_banfield_estandar_destino$inicial_x,y=vs_banfield_estandar_destino$inicial_y, label=vs_banfield_estandar_destino$`Jugador 1`, color="white", size=3.5)
# 
