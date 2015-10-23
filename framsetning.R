source("núþákrónur.R")
source("eignir.R", encoding = "UTF-8")
source("tekjur.R", encoding = "UTF-8")

aldursflokkar <- lapply(seq_along(eignir), function(i) {
  as.data.frame(merge(eignir[i], tekjurOgGjold[i], by=c("Ar","Fjoldiihop")))
})

landshlutar  <- lapply(seq_along(landshlutaeignir), function(i) {
  merge(landshlutaeignir[i], tekjurOgGjold[i+15], by=c("Ar","Fjoldiihop"))
})

aldurs <- function(lysistaerd, deila=TRUE) {
  if(deila)
    sapply(aldursflokkar,
           function(aldur) {
             leidretta(t(aldur[lysistaerd]/aldur$Fjoldiihop))
           }
    )
  else
    sapply(aldursflokkar,
           function(aldur) {
             leidretta(t(aldur[lysistaerd]))
           }
    )
}

source("radstofunarplot.R") #þ.m. radstofunartekjur.R

library("shiny")

breytur <- list("Meðaleign"="MedaltalEigna","Miðeign"="MidgildiEigna",
 "Fasteignir"="Fasteignir","Ökutæki"="Okutaeki","Innlán"="Innlan","Verðbréf"="Verdbrjef",
"Eignir, aðrar"="AdrarEignir","Skuldir"="MedaltalSkulda","Miðgildi skulda"="MidgildiSkulda",
"Íbúðarlán"="Ibudarlan","Skuldir, aðrar"="Adrarskuldir","Eigið fé"="Eigidfje", "Hrein fasteign"="EgidFjeiFasteign",     
"Eigið fé, annað"="Eigidfje.annad","Meðaskuld skuldsettra"="MedaltalSkuldaSkuldsettra",
"Heildartekjur", "Meðaltekjur"="Medaltekjur", "Miðgildi tekna"="MidgildiTekna", "Atvinnutekjur", "Fjármagnstekjur"="Fjarmagnstekjur", "Aðrar tekjur"="AdrarTekjur", "Skattar", "Vaxta- & barnabætur"="VaxtaOgBarnabaetur", "Skattar & bætur"="Heildarskattar.SkattarVaxtaOgBarnabaetur.", "Vaxtagjöld v/íbúðalána"="VaxtagjoldV.Ibudalana", "Vaxtagjöld, önnur"="OnnurVaxtagjold", "Ráðstöfunartekjur"="Radstofunartekjur.Heildartekjur.Heildarskattar.", "Ráðstöfunartekjur2"="Radstofunartekjur2"
)

tiundaBreytur <- c("Eignir"="Heildareignir", "Fasteignir", "Ökutæki"="Okutaeki", "Innlán"="Innlan", "Verðbréf"="Verdbrjef", "Eignir, aðrar"="AdrarEignir", "Skuldir"="Heildarskuldir", "Íbúðalán"="Ibudalan", "Skuldir, aðrar"="Adrarskuldir", "Eigið fé"="Heildareigidfje", "Eigið fé í fasteign"="Eigidfjeifasteign", "Eigið fé, annað"="Eigidfjeannad", "Atvinnutekjur", "Fjármagnstekjur"="Fjarmagnstekjur", "Tekjur, aðrar"="Adrartekjur", "Tekjur"="Heildartekjur", "Skattar", "Vaxta- & barnabætur"="Vaxta.ogbarnabaetur", "Skattar"="Skattaralls", "Vaxtagjöld v/íbúðalána"="Vaxtagjoldv.ibudalana", "Önnur vaxtagjöld"="OnnurVaxtagjold", "Radstofunartekjur1", "Radstofunartekjur2")

tafla <- function(breyta) {
  data.frame(
    u24=aldurs(breyta)[,1],
    aldur2529=aldurs(breyta)[,2],
    aldur3034=aldurs(breyta)[,3],
    aldur3539=aldurs(breyta)[,4],
    aldur4044=aldurs(breyta)[,5],
    aldur4549=aldurs(breyta)[,6],
    aldur5054=aldurs(breyta)[,7],
    aldur5559=aldurs(breyta)[,8],
    aldur6066=aldurs(breyta)[,9],
    y67=aldurs(breyta)[,10])
}  

control = wellPanel

aldursheiti <- c("<24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-66","67+")
aldursbil <- c("Undir 25"="u24","25-29"="aldur2529","30-34"="aldur3034",
               "35-39"="aldur3539","40-44"="aldur4044","45-49"="aldur4549","50-54"="aldur5054","55-59"="aldur5559",
               "60-66"="aldur6066","Yfir 66"="y67")
aldurshopar <- c("Undir 25"=1,"25-29"=2,"30-34"=3,
                 "35-39"=4,"40-44"=5,"45-49"=6,"50-54"=7,"55-59"=8,
                 "60-66"=9,"Yfir 66"=10)
aldurstakkar <- checkboxGroupInput("aldur", "Aldursbil", aldurshopar, list())

aldursFerdarTakkar <- checkboxGroupInput("aldursFerdarHopar", "Aldurshópar", aldurshopar, 2:3)



breytutakkar <- function(nafn, buttons=radioButtons, moguleikar=breytur, default="Eigidfje", inline=FALSE) {
  control(buttons(nafn, "Breyta", moguleikar, default, inline=inline))
}

#aldurslitir = c("u24"="red", "aldur2529"="green", "aldur3034"="blue", "aldur3539"="black", "aldur4044"="maroon", "4549"="brown", "aldur5054"="violet", "aldur5559"="purple", "aldur6066"="royalblue", "y67"="salmon4")#, "orange", "pink", "orchid4", "oldlace", "ivory4", "cyan", "burlywood3")

tTiundir <- rev(c("Tekjulægstir"=1,"næsttekjulægstir"=2,"30%"=3,"40%"=4, "50%"=5, "60%"=6, "70%"=7, "80%"=8, "Næsttekjuhæstu 10%"=9, "Tekjuhæstu 10%"=10))

ui <- shinyUI(fluidPage(#titlePanel("Hagtölur"),
                        fluidRow(tabsetPanel(
                          tabPanel("Aldurshópar",
#                         fluidRow(headerPanel("Aldurshópar")),
                          tabsetPanel(
                           tabPanel("Graf",
                                    fluidRow(
                                      column(10,
                                             fluidRow(plotOutput("aldri")),
                                             column(7,conditionalPanel(
                                               condition="0==input.aldur.length",
                                               sliderInput("ar", "Ár", min=1997, max=2013, value=2013)
                                             )),
                                             column(3,aldurstakkar)
                                      ),
                                      column(2,
                                             fluidRow(breytutakkar("aldursBreyta")))
                                      )
                                    ),
            						   tabPanel("Ferðalag",
                                    fluidRow(column(10,plotOutput("aldursFerd")),
                                             column(2,aldursFerdarTakkar)
                                    ),
                                    breytutakkar("aldursFerdarBreyta",inline=TRUE),
                                    fluidRow(sliderInput("aldursFerdarAr", "Ár", min=1997, max=2013, value=2013))
                           )
                          )),
                          tabPanel("Byggðir",
                           fluidRow(headerPanel("Höfuðborg og landsbyggð")),
                           tabsetPanel(
                              tabPanel("Ferðalag",
                                       fluidRow(titlePanel("Milljónir króna")),
                                       fluidRow(plotOutput("byggdaFerd")),
                                       breytutakkar("byggdaFerdarBreyta",radioButtons,inline=TRUE),
                                       fluidRow(sliderInput("byggdaFerdarAr", "Ár", min=1997, max=2013, value=2013))
                              ),tabPanel("Graf",
                                         fluidRow(titlePanel("Milljónir króna")),
                                         fluidRow(plotOutput("byggdaGraf")),
                                         fluidRow(breytutakkar("byggdaGrafsBreyta", checkboxGroupInput))
                              )
                          )),
                          tabPanel("Tekjutíundir",
#                             fluidRow(headerPanel("Tekjutíundir")),
                            tabsetPanel(
                              tabPanel("Graf",
                                       
                                       fluidRow(column(10,plotOutput("tiundum")),
                                                column(2,checkboxGroupInput("grafTiundir","Tekjutíundir:",tTiundir,1:10))
                                       ),
                                       fluidRow(breytutakkar("tiundaGrafsBreyta",checkboxGroupInput,moguleikar=tiundaBreytur,default="Heildareigidfje",inline=TRUE))
                              ),
                              tabPanel("Ferðalag",
                                       fluidRow(column(10,plotOutput("tiundaFerd")),
                                               column(2,checkboxGroupInput("ferdarTiundir","Tekjutíundir:",tTiundir,1:10))
                                       ),
                                       breytutakkar("tiundaFerdarBreyta",radioButtons,moguleikar=tiundaBreytur,default="Heildareigidfje",inline=TRUE),
                                       fluidRow(sliderInput("tiundaFerdarAr", "Ár", min=1997, max=2013, value=2013))
                              )
                            )
                          )
                    ))
                ))

source("graf.R", encoding = "UTF-8")

teiknaByggdir <- function(breytur) {
  fjarhagsstadahopa(16:17, breytur, aldurstrue=FALSE)
}

byggdaFerdalag <- function(breyta, lokaAr) {
	hopaferdalag(16:17, c(breyta), 1997, lokaAr, aldurstrue=FALSE)
}

aldursFerdalag <- function(breyta, lokaAr, hopar) {
  hopaferdalag(as.numeric(hopar), c(breyta), 1997, lokaAr, aldurstrue=TRUE)
}

sulurit <- function(tafla, xlab="", names.arg=c(), ylab="") {
  ymork <- c(0, max(tafla) )
  barplot(diag(tafla), ylim=ymork, xlab=xlab, names.arg=names.arg, ylab=paste(ylab,"[m.kr.]"), col=as.vector(ifelse(as.vector(tafla)>0, "black", "red")) )
}

teiknaAldri <- function(breyta, aldurshopar, ar) {
  if(length(aldurshopar) == 0) {
    sulurit(tafla(breyta)[1+as.numeric(ar)-1997,], xlab="Aldurshópar", names.arg=aldursheiti, ylab=breyta)
  }
  else {
    fjarhagsstadahopa(as.numeric(aldurshopar), c(breyta), TRUE)
  }
}

ferdalagTiunda <- function(breyta, tiundir, ar) {
  tiundaferdalag(valbreyta=breyta, valtiundir=as.numeric(tiundir), ar1=1997, ar2=ar)
}
  
teiknaTiundir <- function(breytur, tiundir) {
  fjarhagsstadatiunda(breyturvaldar=breytur, hoparvaldir=as.numeric(tiundir))
}


srv <- shinyServer(function(input,output,session) {
    output$byggdaGraf <- renderPlot({teiknaByggdir(input$byggdaGrafsBreyta)})
	output$byggdaFerd <- renderPlot({byggdaFerdalag(input$byggdaFerdarBreyta, input$byggdaFerdarAr)})
    output$aldursFerd <- renderPlot({aldursFerdalag(input$aldursFerdarBreyta, input$aldursFerdarAr, input$aldursFerdarHopar)})
    output$aldri      <- renderPlot({teiknaAldri(input$aldursBreyta, input$aldur, input$ar)})
    output$tiundum    <- renderPlot({teiknaTiundir(input$tiundaGrafsBreyta, input$grafTiundir)})
    output$tiundaFerd <- renderPlot({ferdalagTiunda(input$tiundaFerdarBreyta, input$ferdarTiundir, input$tiundaFerdarAr)})
})

runApp(shinyApp(ui,srv), host="0.0.0.0", port=1337)

linurit <- function(tafla, xlab="Ártal", names.arg=c(), ylab="", col=1:6) {
  matplot(tafla, xaxt = "n", type="l", xlab=xlab, ylab=ylab, col=col)
  axis(1,at=seq_along(names.arg), labels=names.arg)
  abline(h=c(10,20,30,40),lty=2,col="yellow") 
}

