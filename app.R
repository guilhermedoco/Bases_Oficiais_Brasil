### AUTOR: GUILHERME DÔCO ROBERTI GIL
### BAIXANDO SERIES DO BANCO CENTRAL DO BRASIL

if(!require(jsonlite)){ install.packages("jsonlite"); require(jsonlite) }
if(!require(shinydashboard)){ install.packages("shinydashboard"); require(shinydashboard) }
if(!require(flexdashboard)){ install.packages("flexdashboard"); require(flexdashboard) }
if(!require(highcharter)){ install.packages("highcharter"); require(highcharter) }
if(!require(shiny)){ install.packages("shiny"); require(shiny) }
if(!require(tidyverse)){ install.packages("tidyverse"); require(tidyverse) }
if(!require(DT)){ install.packages("DT"); require(DT) }
if(!require(dygraphs)){ install.packages("dygraphs"); require(dygraphs) }
if(!require(openxlsx)){ install.packages("openxlsx"); require(openxlsx) }

Bases <- read.xlsx("Index de Bases.xlsx",sheet = 1,startRow = 1,colNames = TRUE,detectDates = TRUE)


header <- dashboardHeader(title = "Bases Oficiais",
                          tags$li(a(href = 'http://www.abgconsultoria.com.br',
                                    img(src = 'Figuras/Logo_ABG.png',
                                        title = "FRL", height = "30px"),
                                    style = "padding-top:10px; padding-bottom:10px"),
                                  class = "dropdown"))

#Sidebar
########
sidebar <- dashboardSidebar({
  sidebarMenu(
    id = "tabs",
    menuItem("Séries Históricas", tabName = "Menu0", icon = icon("desktop")),
    menuItem("Sobre", tabName = "Menu1", icon = icon("signal")),br(),br(),br(),br(),br(),
    br(),br(),br(),br(),br(), br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
    
             h6("Desenvolvido por",align="center"),h6("ABG Consultoria Estatística",align="center"),
             tags$li(a(href = 'http://www.abgconsultoria.com.br',
                       img(src = 'Figuras/Logo_ABG.png',
                           title = "ABG", height = "60px"),
                       style = "padding-top:10px; padding-left:80px; padding-bottom:10px"),
                     class = "dropdown")
   
  )})

#Body
#####
body <- {dashboardBody(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
                       tabItems(
                         tabItem(tabName = "Menu1",h1("WEBSCRAPING DATABASE - Beta", align="center")
                                 ,  br(), br(),"Esta ferramenta tem como objetivo realizar busca por bases de dados oficiais que
                                 estão disponíveis na web."
                         ),
                         tabItem(tabName = "Menu0",
                                 box(selectInput("Series", label = "Selecione a Série:",choices = levels(as.factor(Bases$Nome.completo)), selected = "Índice nacional de preços ao consumidor-amplo (IPCA)"),
                                     width = 12),
                                 fluidRow(box(dataTableOutput("Tabela"),width = 3),
                                          box(dygraphOutput("Grafico"),br(),
                                              fluidRow(column(6,downloadButton('downloadData', 'Download')),
                                                       column(6,textOutput('Fonte'))),width = 9))
                         )
                       )
                       
)}

#UI
#####
ui <- dashboardPage(header, sidebar, body,skin="blue")

#Server
#####
server <- function(input, output,session) {
  
 # session$onSessionEnded(function() {
 #   stopApp()
 #   q("no")
 # })
  
  output$Tabela <- renderDataTable({
    x<- Bases[Bases$Nome.completo==input$Series,1] 
    ibc = fromJSON(paste0("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",as.character(x),"/dados?formato=json"))
    ibc$valor = as.numeric(ibc$valor)
    ibc$data = as.Date(ibc$data,"%d/%m/%Y")            
    colnames(ibc) <- c("Data",Bases[Bases$Nome.completo==input$Series,3])
    
    datatable(ibc,rownames = FALSE, escape = FALSE, extensions = 'Scroller', options = list(
      deferRender = TRUE,
      scrollY = 350,
      scrollX = TRUE,
      scroller = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});",
        "}")
    )) 
})
  
  output$Grafico <- renderDygraph({
    x<-1
    x<- Bases[Bases$Nome.completo==input$Series,1] 
    ibc = fromJSON(paste0("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",as.character(x),"/dados?formato=json"))
    ibc$valor = as.numeric(ibc$valor)
    ibc$data = as.Date(ibc$data,"%d/%m/%Y") 
    ibc <- ibc[ibc$data >= "2000-01-01",]  
    data <- data.frame(Valor=ibc$valor)
    rownames(data) <- ibc$data
    
    data <- na.omit(data)
    head(data)
    Serie <- ts(data, frequency=7, start=c(2000,1,1))
    
    hw <- HoltWinters(Serie, alpha=NULL, beta=NULL, gamma=NULL, seasonal="additive")
    p <- predict(hw, n.ahead = 36, prediction.interval = TRUE, level=0.95, interval="prediction")
    all <- cbind(Serie, p)
    
    dygraph(all, main = input$Series) %>% 
      dyRangeSelector()%>%
      dySeries("Serie", label = "Atual") %>%
      dySeries("p.fit", label = "Predito")%>%
      dyOptions(fillGraph = TRUE, fillAlpha = 0.4)
  })

  output$Fonte <- renderText({
    x<- Bases[Bases$Nome.completo==input$Series,7] 
    paste0("Fonte: ",as.character(x))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-",as.character(Bases[Bases$Nome.completo==input$Series,2]), Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx(data, file, row.names = TRUE)
    }
  )
}

#Build Application
#####
shinyApp(ui, server)


