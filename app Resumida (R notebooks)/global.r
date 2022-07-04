require(stringr)
require(ggsoccer)
library(dplyr)
library(readr)
library(shinythemes)
library(tidyr)
require(stringr)
require(tidyverse)
library(plotly)

# Define UI for app that draws a histogram ----
soccer <- read_csv("reserva22-completo.csv",
                   col_types = cols(coord.final = col_character(),
                                    coord.inicial = col_character()))



# Define UI for app that draws a histogram ----
# soccer <- read_csv("datos/PERDIDAS-fecha13-platense-tabla filtros-ESPEJO.csv",
#                    col_types = cols(coord.final = col_character(),
#                                    coord.inicial = col_character()))


# MAXIMOS Y MARGENES
max_x <- 273
max_y <- 165
margen_x <- 0
margen_y <- 0

#soccer  <- soccer %>% 
#  select(-c(X1))


soccer$Jugador.1[soccer$Jugador.1 == ""] <- "0" #Seria los que no tienen nada
soccer$Jugador.1[soccer$Jugador.1 == "s1"] <- "14"

soccer$Jugador.1 <- as.numeric(soccer$Jugador.1)
soccer$Jugador.2 <- as.numeric(soccer$Jugador.2)

soccer <- soccer %>% 
  separate(coord.inicial,c('inicial_x','inicial_y')) %>% 
  mutate(inicial_x=abs(as.numeric(inicial_x)),inicial_y=abs(max_y-as.numeric(inicial_y)))
soccer <- soccer %>% 
  separate(coord.final,c('final_x','final_y')) %>% 
  mutate(final_x=abs(as.numeric(final_x)),final_y=abs(max_y-as.numeric(final_y)))




#### CALIBRO COORDENADAS

# cambio las coordenadas del segundo tiempo en el mismo df
# soccer <- soccer %>% 
#   mutate(
#     inicial_x = ifelse(segundos>=2100 & partido != "Atl. Tucuman", max_x - inicial_x, inicial_x),
#     inicial_y = ifelse(segundos>=2100 & partido != "Atl. Tucuman", max_y - inicial_y, inicial_y),
#     final_x = ifelse(segundos>=2100 & partido != "Atl. Tucuman", max_x - final_x, final_x),
#     final_y = ifelse(segundos>=2100 & partido != "Atl. Tucuman", max_x - final_y, final_y)
#   )
# 
# # cambio las coordenadas del segundo tiempo en el mismo df
# soccer <- soccer %>% 
#   mutate(
#     inicial_x = ifelse(segundos<2100 & partido == "Atl. Tucuman", max_x - inicial_x, inicial_x),
#     inicial_y = ifelse(segundos<2100 & partido == "Atl. Tucuman", max_y - inicial_y, inicial_y),
#     final_x = ifelse(segundos<2100 & partido == "Atl. Tucuman", max_x - final_x, final_x),
#     final_y = ifelse(segundos<2100 & partido == "Atl. Tucuman", max_x - final_y, final_y)
#   )


# calibro coordenadas ( los llevo a coordenadas de rango 0-100 )
# soccer <- soccer %>%
#   mutate(inicial_x= (inicial_x-margen_x)/(max_x-(2*margen_x))*100,
#          inicial_y= (inicial_y-margen_y)/(max_y-(2*margen_y))*100) %>%
#   mutate(final_x= (final_x-margen_x)/(max_x-(2*margen_x))*100,
#          final_y= (final_y-margen_y)/(max_y-(2*margen_y))*100)

soccer <- soccer %>%
  mutate(inicial_x= inicial_x*100/max_x,
         inicial_y= (inicial_y*100)/max_y) %>%
  mutate(final_x= final_x*100/max_x,
         final_y= (final_y*100)/max_y )

# si supera 100+5 llevarlo a 101
# soccer <- soccer %>% 
#   mutate(inicial_x= ifelse(inicial_x>101,101,inicial_x),
#          inicial_y= ifelse(inicial_y>101,101,inicial_y),
#          final_x= ifelse(final_x>101,101,final_x),
#          final_y= ifelse(final_y>101,101,final_y))


# soccer <- soccer %>%
#   mutate(inicial_x=inicial_x/max(soccer$inicial_x,na.rm = TRUE)*100, inicial_y=inicial_y/max(soccer$inicial_y,na.rm = TRUE)*100) %>%
#   mutate(final_x=final_x/max(soccer$final_x,na.rm = TRUE)*100, final_y=final_y/max(soccer$final_y,na.rm = TRUE)*100)


filterjugador <- sort(unique(soccer$Apellido),decreasing=FALSE)

filterpartido <- unique(soccer$partido)


filtroeventos <- colnames(soccer)
filtroeventosremate <- c("Gol","Fuera","Al-arco","Interceptado","Cabeza")
filtroeventospases <-  c("Asistencia","Completado","Interceptado","Fuera","Inteligente")
filtroeventoscentros <-  c("Asistencia","Completado","Interceptado","Bloqueado","Fuera")
filtroeventosduelos <-  c("Aereo-Perdido","Aereo-Ganado","Disputa-suelo-Perdida","Disputa-suelo-Ganada",
                          "Gambeta-contra-Ganada","Gambeta-contra-Perdida","Gambeta-favor-Perdida",
                          "Gambeta-favor-Ganada","Pivote-Perdido","Pivote-Ganado")
filtroeventosfaltas <-  c("Cometida","Recibida","Penal-cometido","Penal-recibido","Offside","Tarjeta-amarilla","Tarjeta-roja")
filtroeventosrecuperacion <- c("Robo","Encontrada","Intercepcion-suelo","Intercepcion-aereo","Intercepcion-anticipo","Disputa-suelo")
filtroeventosparada <- c("Lateral-interceptado","Lateral-completado","Corner-interceptado","Corner-completado","Tiro-libre-completado","Tiro-libre-incompleto")
filtroeventosperdida <- c("Gambeta-favor","Robo","Intercepcion","Error-tecnico","Balon-fuera","Situacion-rival","Atajada")
filtroeventosarquero <- c("Gol-rival","Atajada","Intercepcion-aerea","Intercepcion","Anticipo","Saque-mano","Saque-pie","Saque-meta")
