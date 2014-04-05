
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(ggplot2)
library(pwr)
source("samplesize.R")

shinyServer(function(input, output, session) { 

  output$text  <- renderText({
    args <- switch(input$test,
                   "1-p" = list("Contrastes sobre una proporción")
                   ,"2-p.Tiguales" = list("Contrastes sobre dos proporciones. Grupos mismo tamaño")
                   ,"2-p.Tdistintas" = list("Contrastes sobre dos proporciones. Grupos distinto tamaño")
                   ,"1-m" = list("Contrastes sobre una media")
                   ,"2-m.Mindependientes" = list("Contrastes sobre dos medias. Poblaciones independientes. Grupos mismo tamaño")
                   ,"2-m.Memparejadas" = list("Contrastes sobre dos medias. Poblaciones dependientes Grupos mismo tamaño")
                   ,"2-m.Tdistintas" = list("Contrastes sobre dos medias. Poblaciones independientes. Grupos distinto tamaño")
                   ,"ANOVA" = list(paste("Contraste ANOVA"))
    )
    do.call(texttoshow, args) 
  })
  
  
  output$textobj  <- renderText({
    args <- switch(input$test,
                   "1-p" = list("Objetivo: Estudiar si la proporción de exitus poblacional puede tomar cierto valor")
                   ,"2-p.Tiguales" = list("Objetivo: Estudiar si las porporciones de exitus de dos poblaciones pueden considerarse iguales")
                   ,"2-p.Tdistintas" = list("Objetivo: Estudiar si las porporciones de exitus de dos poblaciones pueden considerarse iguales")
                   ,"1-m" = list("Objetivo: Estudiar si la media poblacional puede tomar cierto valor")
                   ,"2-m.Mindependientes" = list("Objetivo: Estudiar si ls medias de dos poblaciones independientes (sujetos distintos) pueden considerarse iguales")
                   ,"2-m.Memparejadas" = list("Objetivo: Estudiar si ls medias de dos poblaciones emparejadas (mediciones antes-después para un mismo grupo de sujetos) pueden considerarse iguales")
                   ,"2-m.Tdistintas" = list("Objetivo: Estudiar si ls medias de dos poblaciones independientes (sujetos distintos) pueden considerarse iguales")
                   ,"ANOVA" = list(paste("Objetivo: Estudiar si ls medias de k poblaciones independientes (sujetos distintos) pueden considerarse iguales"))
    )
    do.call(texttoshow, args) 
  })
  
  
  
  #### Primera línea de texto
  output$text1  <- renderText({
    args <- switch(input$test,
                   "1-p" = list("H0: Proporción poblacional igual a p0")
                   ,"2-p.Tiguales" = list("H0: Diferencia de proporciones poblacionales igual a 0")
                   ,"2-p.Tdistintas" = list("H0: Diferencia de proporciones poblacionales igual a 0")
                   ,"1-m" = list("H0: Media poblacional igual a m0")
                   ,"2-m.Mindependientes" = list("H0: Diferencia de medias poblacionales igual a 0")
                   ,"2-m.Memparejadas" = list("H0: Diferencia de medias poblacionales igual a 0")
                   ,"2-m.Tdistintas" = list("H0: Diferencia de medias poblacionales igual a 0")
                   ,"ANOVA" = list("H0: Medias poblacionales iguales")
          )
    do.call(texttoshow, args) 
    })

  output$text2  <- renderText({
    ngr<-input$grupos-1
    args <- switch(input$test,
                   "1-p" = list("H1: Propoción poblacional distinta a p0")
                   ,"2-p.Tiguales" = list("H1: Diferencia de proporciones poblacionales distinta a 0")
                   ,"2-p.Tdistintas" = list("H1: Diferencia de proporciones poblacionales distinta a 0")
                   ,"1-m" = list("H1: Media poblacional distinta a m0")
                   ,"2-m.Mindependientes" = list("H1: Diferencia de medias poblacionales distinta a 0")
                   ,"2-m.Memparejadas" = list("H1: Diferencia de medias poblacionales distinta a 0")
                   ,"2-m.Tdistintas" = list("H1: Diferencia de medias poblacionales distinta a 0")
                   ,"ANOVA" = list(paste("H1: Al menos dos medias poblacionales distintas (",ngr," grupos)"))
    )
    do.call(texttoshow, args) 
  })  
  
  
  ### Primera tabla
  output$values1 <- renderTable({
    ngr<-input$grupos-1
    args<-list(input$signif,input$potencia,input$dropout,"two.sided",ngr,input$test)
    do.call(samplesize1,args)
    })
  
  #### Segunda Contraste
  output$text3  <- renderText({
    args <- switch(input$test,
                   "1-p" = list("H0: Proporción poblacional menor o igual a p0")
                   ,"2-p.Tiguales" = list("H0: Diferencia de proporciones poblacionales menor o igual a 0")
                   ,"2-p.Tdistintas" = list("H0: Diferencia de proporciones poblacionales menor o igual a 0")
                   ,"1-m" = list("H0: Media poblacional menor o igual  a m0")
                   ,"2-m.Mindependientes" = list("H0: Diferencia de medias poblacionales menor o igual a 0")
                   ,"2-m.Memparejadas" = list("H0: Diferencia de medias poblacionales menor o igual a 0")
                   ,"2-m.Tdistintas" = list("H0: Diferencia de medias poblacionales menor o igual a 0")
                   ,"ANOVA" = list("H0: Medias poblacionales iguales")
    )
    do.call(texttoshow, args) 
  })  
 
  output$text4  <- renderText({
    ngr<-input$grupos
    args <- switch(input$test,
                   "1-p" = list("H1: Propoción poblacional mayor a p0")
                   ,"2-p.Tiguales" = list("H1: Diferencia de proporciones poblacionales mayor a 0")
                   ,"2-p.Tdistintas" = list("H1: Diferencia de proporciones poblacionales mayor a 0")
                   ,"1-m" = list("H1: Media poblacional mayor a m0")
                   ,"2-m.Mindependientes" = list("H1: Diferencia de medias poblacionales mayor a 0")
                   ,"2-m.Memparejadas" = list("H1: Diferencia de medias poblacionales mayor a 0")
                   ,"2-m.Tdistintas" = list("H1: Diferencia de medias poblacionales mayor a 0")
                   ,"ANOVA" = list(paste("H1: Al menos dos medias poblacionales distintas (",ngr," grupos)"))
    )
    do.call(texttoshow, args) 
  })  
  
    
  ### Segunda tabla
  output$values2 <- renderTable({
    ngr<-input$grupos
    args<-list(input$signif,input$potencia,input$dropout,"greater",ngr,input$test)
    do.call(samplesize1,args)
  })  
  
#   #### Tercer Contraste
#   output$text5  <- renderText({
#     ngr<-input$grupos+1
#     args <- switch(input$test,
#                    "1-p" = list("H0: Proporción poblacional mayor o igual a p0")
#                    ,"2-p.Tiguales" = list("H0: Diferencia de proporciones poblacionales mayor o igual a 0")
#                    ,"2-p.Tdistintas" = list("H0: Diferencia de proporciones poblacionales mayor o igual a 0")
#                    ,"1-m" = list("H0: Media poblacional mayor o igual  a m0")
#                    ,"2-m.Mindependientes" = list("H0: Diferencia de medias poblacionales mayor o igual a 0")
#                    ,"2-m.Memparejadas" = list("H0: Diferencia de medias poblacionales mayor o igual a 0")
#                    ,"2-m.Tdistintas" = list("H0: Diferencia de medias poblacionales mayor o igual a 0")
#                    ,"ANOVA" = list(paste("H0: Medias poblacionales iguales"))
#     )
#     do.call(texttoshow, args) 
#   })  
# 
#   output$text6  <- renderText({
#     ngr<-input$grupos+1
#     args <- switch(input$test,
#                    "1-p" = list("H1: Propoción poblacional menor a p0")
#                    ,"2-p.Tiguales" = list("H1: Diferencia de proporciones poblacionales menor a 0")
#                    ,"2-p.Tdistintas" = list("H1: Diferencia de proporciones poblacionales menor a 0")
#                    ,"1-m" = list("H1: Media poblacional menor a m0")
#                    ,"2-m.Mindependientes" = list("H1: Diferencia de medias poblacionales menor a 0")
#                    ,"2-m.Memparejadas" = list("H1: Diferencia de medias poblacionales menor a 0")
#                    ,"2-m.Tdistintas" = list("H1: Diferencia de medias poblacionales menor a 0")
#                    ,"ANOVA" = list(paste("H1: Al menos dos medias poblacionales distintas (",ngr," grupos)"))
#     )
#     do.call(texttoshow, args) 
#   })   
#   
#   
#   ### Tercera tabla
#   output$values3 <- renderTable({
#     ngr<-input$grupos+1
#     args<-list(input$signif,input$potencia,input$dropout,"less",ngr,input$test)
#     do.call(samplesize1,args)
#   })    
  
  
})


