source("hagstofa.R")
source("núþákrónur.R")
source("radstofunarplot.R")
library(shiny)


tafla <- data.frame(
                    u24=aldurs("Eigidfje")[,1],
                    aldur2529=aldurs("Eigidfje")[,2],
                    aldur3034=aldurs("Eigidfje")[,3],
                    aldur3539=aldurs("Eigidfje")[,4],
                    aldur4044=aldurs("Eigidfje")[,5],
                    aldur4549=aldurs("Eigidfje")[,6],
                    aldur5054=aldurs("Eigidfje")[,7],
                    aldur5559=aldurs("Eigidfje")[,8],
                    aldur6066=aldurs("Eigidfje")[,9],
                    y67=aldurs("Eigidfje")[,10])

ymork <- c( min(tafla), max(tafla) )

control = wellPanel

ui <- shinyUI(fluidPage(titlePanel("Hagtölur"),
                         fluidRow(headerPanel("Eigið fé aldurshópa")),
                        fluidRow(tabsetPanel(
                          tabPanel("Eigið fé aldurshópa valið ár",plotOutput("eftirAri"),
                                   fluidRow(control(sliderInput("ar", "Ár", min=1997, max=2013, value=1997)))
                          ),
                          tabPanel("Eigið fé aldurshóps í gegnum árin",
                            sidebarLayout(sidebarPanel(radioButtons("aldur", "Aldursbil",
                                                 c("Undir 25"="u24","25-29"="aldur2529","30-34"="aldur3034",
                                                   "35-39"="aldur3539","40-44"="aldur4044","45-49"="aldur4549","50-54"="aldur5054","55-59"="aldur5559",
                                                   "60-66"="aldur6066","Yfir 66"="y67")
                            )),
                            mainPanel(plotOutput("eftirAldri")))),
                          tabPanel("Skipting ráðstöfunartekna",plotOutput("radstofunartekjur"))
                    ))
                ))

sulurit <- function(tafla, names.arg=c()) {
  barplot(diag(tafla), ylim=ymork, names.arg=names.arg, col=as.vector(ifelse(as.vector(tafla)>0, "black", "red")) )
}

aldursbil <- c("<25","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-66",">66")

srv <- shinyServer(function(input,output) {
  output$eftirAri <-   renderPlot({sulurit(tafla[1+(input$ar-1997),], aldursbil)})
  output$eftirAldri <- renderPlot({sulurit(tafla[,input$aldur], 1997:2013)})
  output$radstofunartekjur <- renderPlot({tekjudreifing(medaltekjur, 1, 10)})
})

runApp(shinyApp(ui,srv))