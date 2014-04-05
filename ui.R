
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(ggplot2)
library(pwr)
source("samplesize.R")

shinyUI(navbarPage("Tamaño Muestra para medias y proporciones",
                   # Definición panel navegación y estructura UI para cada menu
                   tabPanel("Selecciona el problema",
                            # Configuración de parámetros
                            sidebarLayout(
                              # Panel de inputs
                              sidebarPanel(h3("Configuración del problema"),                                      
                                           selectInput("test", 
                                                       label = "Selecciona la situación a estudiar",
                                                       choices = c("1-p", 
                                                                   "2-p.Tiguales",
                                                                   "2-p.Tdistintas",
                                                                   "1-m",
                                                                   "2-m.Mindependientes",
                                                                   "2-m.Memparejadas",
                                                                   "2-m.Tdistintas",
                                                                   "ANOVA"),
                                                       selected = "2-m.Mindependientes"),br(),   
                                           sliderInput("potencia","Potencia",min=0.5,max=0.9,value=0.8,step=0.1),br(),
                                           sliderInput("signif","Nivel de Significatividad",min=0.01,max=0.1,value=0.05,step=0.01),br(),
                                           sliderInput("dropout","Porcentaje de drop-out",min=0.01,max=0.10,value=0.10,step=0.01,format="##%"),br(),
                                           sliderInput("grupos","Número de grupos (sólo para ANOVA)",min=3,max=8,value=4,step=1),br()
                              ),
                              mainPanel(verbatimTextOutput("text")
                                        ,textOutput("textobj"),br()
                                        ,h5("Situación 1")
                                        ,textOutput("text1")
                                        ,textOutput("text2"),br()
                                        ,tableOutput("values1")
                                        ,h5("Situación 2")
                                        ,textOutput("text3")
                                        ,textOutput("text4"),br()
                                        ,tableOutput("values2")
#                                         ,h5("Situación 3")
#                                         ,textOutput("text5")
#                                         ,textOutput("text6"),br()
#                                         ,tableOutput("values3")
                              )
                            )
                   ),
                   tabPanel("Ayuda")                  
))
