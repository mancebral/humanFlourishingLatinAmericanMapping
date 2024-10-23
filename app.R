library(shiny)
library(leaflet)
library(sf)
library(plotly)
library(htmltools)
library(shinyWidgets)
library(DT)
library(tidyr)
library(stringr)
library(grDevices)
library(dplyr)
library(shinyBS)
library(shinyjs)
library(bslib)
library(purrr)
library(glue)
library(shinycssloaders)
library(ggtext)
library(fst)
library(word2vec)
library(ggrepel) 
library(umap) 
library(googlesheets4)
library(gargle)
library(openxlsx)

options(gargle_oauth_cache = "secrets",
        gargle_oauth_email = "manuel.cebral@tec.mx")

#load data
countriesRelevance = readRDS("data/countriesRelevance.RDS")
relevantWords = read_fst("data/relevantWords.fst")
data = readRDS("data/authorsFRI.rds")
papers = readRDS("data/papersFRI.rds")
institutions = readRDS("data/institutionsFRI.rds")
papersByCountryYear = read_fst("data/papersByCountryYear.fst")
TopPapers = readRDS("data/topPapersIndex.rds")
LAcountries = read_fst("data/LAcountries.fst")
typesData = read_fst("data/typesData.fst")
setwd("data")
#path  <- system.file(package = "word2vec", "models", "cbow_modelBR.bin")
cbow_modelBR = read.word2vec("cbow_modelBR.bin")
cbow_modelMX = read.word2vec("cbow_modelMX.bin")
cbow_modelCH = read.word2vec("cbow_modelCH.bin")
cbow_modelCO = read.word2vec("cbow_modelCO.bin")

#crear paleta de color
my_palette <- grDevices::colorRampPalette(rev(c("#1455A3","#78A167","#D55227","#F4AA40", 
                                                "lightgrey")))(11)
my_paletteL <- grDevices::colorRampPalette(rev(c("#1455A3","#78A167","#D55227","#F4AA40", 
                                                 "lightgrey")))(5)

####USER INTERFACE####
ui <- tagList(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  useShinyjs(),
  
  #setBackgroundColor(color = "#8192A0"),
  setBackgroundImage(src = "background.png"),
  
  # Footer
  tags$footer(
    style = "position: fixed; bottom: 0; z-index: 9999;
    width: 100%; background-color: white; color: black;
    padding-left: 10px; padding-right: 10px; padding-top: 10px; padding-bottom: 10px;
    text-align: center; font-size: 10px;",
    HTML("This site has been conceived by the Human Flourishing Team of Tecnológico de Monterrey and 
    developed in R Programming Shiny Apps with the support of Templeton World Charity Foundation. 
                     More info about <a href='https://tec.mx/es/florecimiento-humano/entorno-para-florecer/mapeo-de-florecimiento-humano-en-latinoamerica' 
                     target='_blank'>Landscaping Regional Research and Leadership Capacities for the 
         Study of Human Flourishing in Mexico, Colombia, Chile, and Brazil</a>."
    )),
  
  navbarPage(collapsible = TRUE,

            ####INTRO TAB####
            tabPanel("Intro"),
            
            tabPanel("Home", 
                     HTML('<div id="frontPage"><div class="frontElements"><img id="img-titulo" src="title.png"><br><br>
                                             <p id="subtitle">Landscaping Regional Research and Leadership Capacities for the 
                                             Study of Human Flourishing in Mexico, Colombia, Chile, and Brazil.
                                             This portal offers several tools to gather information on academic research fed by a database 
                                             that includes scientific production in Scopus and OpenAlex between 2000 and 2023.</p><br>
                                             <a href="https://tec.mx/es/florecimiento-humano/entorno-para-florecer/mapeo-de-florecimiento-humano-en-latinoamerica" target="_blank"> 
                                             <img id="logos" src="logoTec.png"></a>  <a href="https://www.templetonworldcharity.org/" target="_blank"><img id="logos" src="logoTempleton.png"></a>
                          </div></div>')
            ),
            
            ####CRITERIA####
            tabPanel("Criteria", 
                     column(6, 
                            HTML('<div id="criteriaText"><h1 id="criteriaTitle">Criteria</h1><br>
                                   <p id="criteriaSubtitle">This page explains the criteria with which the documentation has been organized.
                                   The data have been classified according to three criteria: productivity, 
                                   influence and relevance. With these, an INDICATOR on flourishing research is built. 
                                   Productivity is related to the number of documents in the database. 
                                   Influence is related to the number of citations received by a paper or author. 
                                   Relevance is related to a paper’s content according to its keywords. 
                                   The Flourishing Research  Indicator (FRI) expresses how influential, 
                                 productive and relevant for the study of human flourishing documents and authors are. 
                                 </p></div>'),
                            
                            #defining criterias as acordions
                            radioGroupButtons(
                              inputId = "criterias",
                              label = "",
                              #status = "custom-class",
                              choices = c("Productivity",
                                          "Influence",
                                          "Relevance",
                                          "FRI"),
                              justified = TRUE
                            ),
                            htmlOutput("criteriaDefinition")
                            ),
                     column(6,
                            card(
                              card_header(HTML("<h3>RELEVANCE SCORE TABLE</h3>")),
                              height = 600,
                              style = "resize:vertical;",
                              card_body(
                                min_height = 500,
                                tableOutput("criteriaPlot")%>% withSpinner(color="#F4AA40")
                              )
                            )
                     )
                     ),
                     
            ####DATA TAB####
            tabPanel("Data", 
                     htmlOutput("dataInfo"),
                            column(4, offset = 4,  
                                   awesomeRadio("dataCountries", inline = TRUE, label = "",
                                                choices = c("Region", "Mexico",
                                                            "Colombia", "Chile", "Brazil"))),
                     column(6, 
                            plotlyOutput("typesPlot", height = "280px")%>% withSpinner(color="#F4AA40")
                     ),
                     column(6,
                            plotlyOutput("papersYear", height = "280px")%>% withSpinner(color="#F4AA40"),
                            HTML("<br><br>")
                     ),
                     column(12,
                            HTML("<p text-align='center';>By clicking on any country of the pie charts you can access to the data of that specific country</p><br><br>")),
                     column(4,
                            plotlyOutput("dataInfo1")%>% withSpinner(color="#F4AA40"),
                            HTML("<br>"),
                            column(12,
                                   column(11,
                            uiOutput("dataInfo1.1")),
                            column(1, 
                                   actionLink("resetPie11", "Reset", #icon = icon("recycle"),
                                                style='padding:0px; font-size:85%'),
                                   downloadLink('downloadData1', 'Download',
                                                style='padding:0px; font-size:85%'))
                            ),
                            column(12,
                            card(
                              height = 300,
                              width= "100%",
                              style = "resize:vertical;",
                              card_body(
                                min_height = 300,
                                tableOutput("dataInfo1.2")%>% withSpinner(color="#F4AA40")
                              )) %>% tagAppendAttributes(class="infoTable")
                     )),
                     column(4,
                            plotlyOutput("dataInfo2")%>% withSpinner(color="#F4AA40"),
                            HTML("<br>"),
                            column(12,
                                   column(11,
                                   htmlOutput("dataInfo2.1")),
                                   column(1, 
                                   actionLink("resetPie21", "Reset", #icon = icon("recycle"),
                                                style='padding:0px; font-size:85%'),
                                   downloadLink('downloadData2', 'Download',
                                                style='padding:0px; font-size:85%'))
                            ),
                            column(12,
                            card(
                              height = 300,
                              style = "resize:vertical;",
                              card_body(
                                min_height = 300,
                                tableOutput("dataInfo2.2")%>% withSpinner(color="#F4AA40")
                              )) %>% tagAppendAttributes(class="infoTable")
                     )),
                     column(4,
                            plotlyOutput("dataInfo3")%>% withSpinner(color="#F4AA40"),
                            HTML("<br>"),
                            column(12,
                            column(11,
                                   htmlOutput("dataInfo3.1")),
                            column(1, 
                                   actionLink("resetPie31", "Reset", #icon = icon("recycle"),
                                                style='padding:0px; font-size:85%'),
                                   downloadLink('downloadData3', 'Download',
                                                style='padding:0px; font-size:85%'))
                            ),
                            column(12,
                                   card(
                              height = 300,
                              style = "resize:vertical;",
                              card_body(
                                min_height = 300,
                                tableOutput("dataInfo3.2")%>% withSpinner(color="#F4AA40")
                              ))%>% tagAppendAttributes(class="infoTable")
                     ))
            ),
            
            ####COUNTRIES####
            tabPanel("Countries", 
                     column(12, 
                            HTML("In this page you can click on a country to display information about research in that country: 
                                 total documents, average relevance (according to our relevance criterion), 
                                 most frequent relevant words in academic production, and the most relevant institutions according 
                                 to human flourishing keywords.<br><br>")),
                     column(7,
                            leafletOutput("leafdown"),
                            absolutePanel(bottom= 10, left = 40,
                                          prettyRadioButtons("continents", "Range of display:",
                                                       choices = c("Just Latin America", "Collaborations with the rest of the world"),
                                                       selected = c("Just Latin America"),
                                                       inline = FALSE,
                                                       shape="square", thick = TRUE
                                          )
                            )
                     ),
                     column(5,
                            htmlOutput("instruction"),
                            htmlOutput("columnTitle"),
                            plotlyOutput("productivity", height = 100),
                            plotlyOutput("relevanceScore", height = 100),
                            plotlyOutput("relevanceWords", height = 300),
                            htmlOutput("tableTitle"),
                            card(
                              height = 300,
                              style = "resize:vertical;",
                              card_body(
                                min_height = 300,
                                dataTableOutput("authorsTable", width = "98%")
                              ))
                     )
                     ),
            
            ####AUTHORS####
            tabPanel("Authors", 
                     column(5,
                            HTML("In this page, authors can be searched by country with two combinations of search filters: 
                                 by relevance and productivity (total number of author's documents), 
                                 or by relevance and influence (total number of citations received by the author). 
                                 You can modify the value for all three search factors.<br><br>"),
                       prettyRadioButtons("radio", label = NULL,
                                    choices = list("Productivity (number of papers)" = "productivity", "Influence (number of citations)" = "TC"), 
                                    selected = "productivity",shape="square", thick = TRUE),
                       conditionalPanel(
                         condition = "input.radio == 'productivity'",
                         sliderInput("productivity",
                                     "Grade of Productivity:",
                                     min = min(data$productivity),
                                     max = max(data$productivity),
                                     value = 10,
                                     width = "100%")
                       ),
                       conditionalPanel(
                         condition = "input.radio == 'TC'",
                         sliderTextInput("citations",
                                     "Grade of influence:",
                                     choices = c(4,8,15,30,60,125,250,500,1000,2000,4000,8000, 16000),
                                     selected = 1000,
                                     grid = TRUE,
                                     width = "100%")
                       ),
                       sliderInput("relevance",
                                   "Grade of Relevance:",
                                   min = 5,
                                   max = 100,
                                   value = 25,
                                   width = "100%"),
                       HTML("<br><br><br>"),
                       textInput("nameAuthor", "You can also find works of an author directly filtering by his/her name",
                                placeholder="Enter last name of an author to search", width = "100%"),
                       actionButton("searchAuthor", "search!"),
                       HTML("<br>"),
                       HTML("<br>"),
                       textOutput("notification"),
                       radioGroupButtons("links", 
                                         label=NULL, 
                                         choices = c(1), 
                                         direction = "vertical")
                     ),
                     
                     column(7,
                            HTML("<h5 id='countriesColumn'>In the graph below, each point is an author, and you can consult their works by clicking on the points. 
                                 Some authors can appear duplicated since they belong to institutions in different countries. You can also filter by country by double clicking on them:</h5>"),
                            plotlyOutput("distPlot")%>% withSpinner(color="white"),
                            HTML("<br>"),
                            htmlOutput("authorName"),
                            downloadLink('downloadData4', 'Download Table',
                                         style='padding:0px; font-size:85%'),
                            card(
                              height = 300,
                              style = "resize:vertical;",
                              card_body(
                                min_height = 300,
                            dataTableOutput("worksTable")))
                     )
                     ),
            ####WORDS####
            tabPanel("Words", 
                     column(12, 
                            HTML("In this page, you can find the words with similar meanings (correlated) found in 
                                 the total documentation for Brazil, Chile, Colombia, and Mexico.<br><br>")),
                     column(3,
                            HTML("We trained a CBOW word embeddings model to extract the most similar 
                                 words to a given one within the corpus. We do it separately for each 
                                 of the countries involved in this study, so it is posible to compare 
                                 their semantic differences and even the clusters that they internally 
                                 form."),
                            textInput("wordEmbed", "",
                                      placeholder="Enter a word",
                                      width = "200px"),
                            actionButton("runCbow", "Find correlations!"),
                            HTML("<br><br>"),
                            textOutput("notification2"),
                            plotlyOutput("similarityPlot")
                     ),
                     column(9, 
                            card(
                              height = "80vh",
                              style = "resize:vertical;",
                              card_body(
                                min_height = "80vh",
                                column(12,
                            plotlyOutput("cbowPlotBR"),
                            plotlyOutput("cbowPlotMX"),
                            plotlyOutput("cbowPlotCH"),
                            plotlyOutput("cbowPlotCO"))
                              )))
            ),
                            
            ####ABOUT####
            tabPanel("About", 
                     column(6,
            HTML('<h1 id="aboutTitle">About</h1><br>
                                   <p id="aboutText">This site shows results of the 
                                   bibliometric exploration of the Latin American academic 
                                   research on Human Flourishing. This exploration is part of 
                                   a broader project that seeks an interdisciplinary systematic 
                                   mapping of human flourishing in this region. The present phase 
                                   includes information from 21 countries, with emphasis on Mexico, 
                                   Chile, Colombia and Brazil.
                                   This initiative is possible thanks to the alliance of Tecnologico 
                                   de Monterrey (Mexico), Universidad de Los Andes (Colombia), 
                                   Universidad Católica de Chile, the collaboration of Nossa 
                                   Terra Firme (Brazil) and the support of The Templeton World Charity Foundation. More information about Human Flourishing at Tecnológico de Monterrey, please visit:<a href="https://tec.mx/es/florecimiento-humano/entorno-para-florecer" target="_blank""> 
                                   https://tec.mx/es/florecimiento-humano/entorno-para-florecer</a>. More info about this project available at: <a href="https://tec.mx/es/florecimiento-humano/entorno-para-florecer/mapeo-de-florecimiento-humano-en-latinoamerica" target="_blank"">
                 https://tec.mx/es/florecimiento-humano/entorno-para-florecer/mapeo-de-florecimiento-humano-en-latinoamerica.</a></p><br><br>
                                   <h4 id="contact">Contact</h4> 
                                   <div id="aboutText2">Dirección de Florecimiento Humano, Tecnológico de Monterrey<br>
                                   <a href="mailto:fh@servicios.tec.mx">fh@servicios.tec.mx</a></div>')),
            column(6,
                   card(
                     height = "80vh",
                     style = "resize:vertical;",
                     card_body(
                       min_height = "80vh",
            column(12,
                   h3(" Project Leader", style="color: #F4AA40; background:transparent;"),
                   column(3,
                   img(src="https://tec.mx/sites/default/files/repositorio/sentido-humano/florecimiento-humano/enrique-tamez-entorno-florecer-florecimiento-humano-tec-monterey.jpg",
                       align="center",
                       class = "redonda",
                       width=100)),
                   column(9, 
                          HTML("<h5>Dr. Enrique Tamés</h5>
                               Tecnologico de Monterrey (Mexico)"))) %>% tagAppendAttributes(class="my_col_class"),
            column(12,
                   h3(" Core Researchers", style="color: #F4AA40; background:transparent;"),
                   column(3,
                   img(src="https://tec.mx/sites/default/files/repositorio/sentido-humano/florecimiento-humano/alberto-baqueiro-entorno-florecer-florecimiento-humano-tec-monterey.jpg",
                       align="center",
                       class = "redonda",
                       width=100)),
                   column(9,
                          HTML("<h5>Dr. Alberto Hernández-Baqueiro</h5>
                               Tecnologico de Monterrey (Mexico)"))),
            HTML("<hr>"),
            column(12,
                   column(3,
                   img(src="https://research.tec.mx/vivo-tec/file/n445/thumbnail_Manuel+Cebral+Loureda.jpg",
                       align="center",
                       class = "redonda",
                       width=100)),
                   column(9,
                          HTML("<h5>Dr. Manuel Cebral-Loureda</h5>
                               Tecnologico de Monterrey (Mexico)"))),
            h3(" Associate Researchers", style="color: #F4AA40; background:transparent;"),
            column(12,
                   column(3,
                          img(src="https://amazoninvestor.org/wp-content/uploads/2023/05/iara.jpg",
                              align="center",
                              class = "redonda",
                              width=100)),
                   column(9,
                          HTML("<h5>Iara Vicente, MPA ESP</h5>
                               Nossa Terra Firme (Brazil)"))),
            HTML("<hr>"),
            column(12,
                   column(3,
                   img(src="https://cdn.theconversation.com/avatars/278480/width238/tim_lomas.jpg",
                       align="center",
                       class = "redonda",
                       width=100)),
                   column(9,
                          HTML("<h5>Dr. Tim Lomas</h5>
                               Harvard (USA)"))),
            HTML("<hr>"),
            column(12,
                   column(3,
                          img(src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQ4hys_N90pC6BE-mAIy-31VvhnlvgCDc6_0Q&s",
                              align="center",
                              class = "redonda",
                              width=100)),
                   column(9,
                          HTML("<h5>Dr. Monica Zenil</h5>
                               UNAM (Mexico)"))),
            HTML("<hr>"),
            column(12,
                   column(3,
                   img(src="https://admonuniandes.b-cdn.net/wp-content/uploads/2021/11/eduardo-wills.jpg",
                       align="center",
                       class = "redonda",
                       width=100)),
                   column(9,
                          HTML("<h5>Dr. Eduardo Wills</h5>
                               Universidad de Los Andes (Colombia)"))),
            HTML("<hr>"),
            column(12,
                   column(3,
                          img(src="https://0.academia-photos.com/838587/295013/31455741/s200_giovanni.vecchio.jpg",
                              align="center",
                              class = "redonda",
                              width=100)),
                   column(9,
                          HTML("<h5>Dr. Giovanni Vecchio</h5>
                               Pontificia Universidad Catolica de Chile (Chile)"))),
            column(12,
                   h3(" Management", style="color: #F4AA40; background:transparent;"),
                   column(3,
                          img(src="https://tec.mx/sites/default/files/inline-images/aida-martinez-entorno-florecer-florecimiento-humano-tec-monterey.jpg",
                              align="center",
                              class = "redonda",
                              width=100)),
                   column(9,
                          HTML("<h5>Msc. Aída Martínez</h5>
                               Tecnologico de Monterrey (Mexico)")))
            )))
            ), 
            ####FORUM####
            tabPanel("Forum", 
                     column(4,
                            HTML("We value your feedback and contributions on this tool.<br><br>"),
                            textInput("feedbackName", "Please, enter your name"),
                            textInput("email", "Give us a contact email"),
                            textAreaInput("feedbackComment", "Share with us...", width = "100%",
                                          height = 200),
                            actionButton("sendFeedback", "Send feedback!"),
                            HTML("<br><br>"),
                            textOutput("thankyou")
                     ),
                     column(8, 
                            dataTableOutput("feedbackTable")
                            )
                     
                     ), id= "MainNavBar"
          #   )
     )
  )

# Define server logic required to draw a histogram
server <- function(session, input, output) {
  
  shinyjs::hide("links")
  shinyjs::disable("resetPie11")
  shinyjs::disable("resetPie21")
  shinyjs::disable("resetPie31")
  shinyjs::hide("downloadData4")
  shinyjs::hide("notification2")
  shinyjs::hide("thankyou")
  
####Register plotly events####
  plotlyEvent1 <- reactive({
    event_data(event = "plotly_click", source = "dataTab1")
  })
  
  plotlyEvent2 <- reactive({
    event_data(event = "plotly_click", source = "dataTab2")
  })
  
  plotlyEvent3 <- reactive({
    event_data(event = "plotly_click", source = "dataTab3")
  })
  
  plotlyEvent4 <- reactive({
    event_data(event = "plotly_click", source = "authorsTab")
  })
  
####HOME SERVER ####

  
####CRITERIA SERVER####
  
  observe({
    input$criterias
    if(input$criterias=="Productivity"){
      output$criteriaDefinition <- renderUI(
        HTML("<font size='2'>It reflects the number of documents that an actor (author, institution or country) 
             has within the collected database. Due to the size of this database, there are many actors who work 
             in very productive fields (e. g. medicine) that are little related to human flourishing, 
             but still related.</font>"))
    }
    if(input$criterias=="Influence"){
      output$criteriaDefinition <- renderUI(
        HTML("<font size='2'>It reflects the number of citations that an actor (author or institution) has within 
             the collected database. Due to size and variety of disciplinary fields included in this database, 
             there are many influential actors (those who receive many cites) that are of little interest to 
             human flourishing, but still related.</font>"))
    }
    if(input$criterias=="Relevance"){
      output$criteriaDefinition <- renderUI(
        HTML("<font size='2'>The relevance criterion rates scholarly products and authors based on their first five most frequent words keywords. 
        Documents and actors receive a score according to the use frequency of some keywords (those shown in the table at the right of the page). 
        Each keyword related to human flourishing has a number of points. The keywords contained in an article or in an author’s publications 
        (up to five keywords) give a sum of points which is its relevance rating. 
        In this process, 93 keywords in Spanish, Portuguese and English related to human flourishing were used 
        (see the keywords table at the right of this page). 
        Because the score is built only with the keywords, it is possible that an actor with a small number 
        of publications or citations still has a high relevance score.
             </font>"))
    }
    if(input$criterias=="FRI"){
      output$criteriaDefinition <- renderUI(
        HTML("<font size='2'>This is the Flourishing Research Indicator. 
        This criterion tries to balance the precedent ones, by combining the more quantitative values (productivity and influence) 
        with the more qualitative approach (relevance). The FRI prioritize the relevant score, but also giving weight to productivity and influence. 
        The applied formula is the three times multiplication of the relevance score ranking, one time the productivity ranking and one time the influence ranking:
             <b>FRI = Relevance<sup>3</sup> * Productivity * Influence</b></font>"))
    }
  })
  
  output$criteriaPlot <- renderTable(relevantWords %>%
                                       rename(Word=Words) %>% 
                                       rename(Relevance=score) %>% 
                                       arrange(desc(Relevance)),
                                     width= "100%")
  
####DATA SERVER####
  output$dataInfo <- renderUI(HTML("In this page you can search by subjects: Latin America, Brazil, Chile, Colombia, 
                                   and Mexico, about the number and type of papers, annual documentary production, 
                                   authors of papers, institutions where human flourishing is researched, and articles 
                                   produced. The results are ordered according to the Flourishing Research Indicator (FRI). 
                                   Click on the graph to perform a search. It is possible to download the results of a search 
                                   using the download button.<br><br><br>"))
  
    
  observeEvent(input$dataCountries,{
      
      if(input$dataCountries=="Region"){
        output$typesPlot <- renderPlotly({
        typesPlotly <- typesData %>% 
          mutate(Type=if_else(!Type%in%c("ARTICLE", "BOOK", "BOOK CHAPTER",
                                         "CONFERENCE PAPER", "REVIEW"), "OTHER", Type)) %>% 
          group_by(Type) %>% 
          summarise(n=n()) %>% 
          ungroup() %>% 
          mutate(Document="Document") %>% 
          mutate(Type=reorder(as.factor(Type), -n)) %>% 
          mutate(text=paste0("<b>REGION</b>", "\n", Type, ": ", n)) %>% 
          ggplot(aes(Document, n, fill=Type, text=text))+
          geom_col()+
          coord_flip()+
          xlab(NULL)+
          ylab(NULL)+
          ggtitle(paste0("Types of documents ", "<span style='font-size:10pt'>", "From a total of ", length(typesData$Type),"</span>"))+
          theme(
            panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill="transparent", color=NA),
            legend.background = element_rect(fill = "transparent"),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.title=element_blank())
        
        ggplotly(typesPlotly, tooltip = c("text")) %>% 
          config(displayModeBar = FALSE) %>% 
          layout(xaxis=list(fixedrange=TRUE)) %>% 
          layout(yaxis=list(fixedrange=TRUE))%>% 
          layout(legend=list(title=list(text='')))
      })} else {
        
        output$typesPlot <- renderPlotly({
        
          totaln <- typesData %>% 
            filter(str_detect(Countries, toupper(input$dataCountries)))
      
        typesPlotly <- totaln %>% 
          group_by(Type) %>% 
          summarise(n=n()) %>% 
          ungroup() %>% 
          group_by_all() %>% 
          mutate(totaln=sum(n)) %>% 
          ungroup() %>% 
          mutate(Document="Document") %>% 
          mutate(text=paste0("<b>", toupper(input$dataCountries), "</b>", "\n", 
                             Type, ": ", n)) %>% 
          ggplot(aes(Document, n, fill=Type, text=text))+
          geom_col()+
          coord_flip()+
          xlab(NULL)+
          ylab(NULL)+
          ggtitle(paste0("Types of documents ", "<span style='font-size:10pt'>", "From a total of ", length(totaln$Type),"</span>"))+
          theme(
            panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill="transparent", color=NA),
            legend.background = element_rect(fill = "transparent"),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.title=element_blank()) 
        
        ggplotly(typesPlotly, tooltip = c("text")) %>% 
          config(displayModeBar = FALSE) %>% 
          layout(xaxis=list(fixedrange=TRUE)) %>% 
          layout(yaxis=list(fixedrange=TRUE))%>% 
          layout(legend=list(title=list(text='')))
      })
      }
      })
  
  observeEvent(input$dataCountries,{
    if(input$dataCountries=="Region"){
    output$papersYear <- renderPlotly({
      papersPlot <- papersByCountryYear %>% 
        group_by(Year) %>% 
        mutate(n=sum(n)) %>% 
        mutate(Citations=sum(TC)) %>% 
        ungroup() %>% 
        select(Year, n, Citations) %>% 
        distinct() %>% 
        arrange(Year) %>% 
        mutate(text=paste0("<b>REGION</b>", "\n", 
                           "Year ", Year, "\n", 
                           n, " Documents", "\n",
                           Citations, " Citations")) %>% 
        ggplot(aes(Year, n, text=text, group = 1))+
        geom_point(aes(size = Citations,colour = Citations), show.legend = TRUE)+
        geom_path()+
        ylab(NULL)+
        xlab(NULL)+
        ggtitle("Documents by year")+
        theme(
          panel.grid = element_line(colour = "black"),
          panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill="transparent", color=NA),
          legend.background = element_rect(fill = "transparent"),
          plot.margin = unit(c(0, 0, 0, 0), "points"))
      
      ggplotly(papersPlot, tooltip = c("text")) %>% 
        config(displayModeBar = FALSE) %>% 
        layout(xaxis=list(fixedrange=TRUE)) %>% 
        layout(yaxis=list(fixedrange=TRUE))
    })
    } else {
      output$papersYear <- renderPlotly({
        papersPlot <- papersByCountryYear %>% 
          filter(str_detect(Countries, toupper(input$dataCountries))) %>% 
          group_by(Year) %>% 
          mutate(n=sum(n)) %>% 
          mutate(Citations=sum(TC)) %>% 
          ungroup() %>% 
          select(Year, n, Citations) %>% 
          distinct() %>% 
          arrange(Year) %>% 
          mutate(text=paste0("<b>",toupper(input$dataCountries),"</b>", "\n",
                              "Year ", Year, "\n", 
                             n, " Documents", "\n",
                             Citations, " Citations")) %>% 
          ggplot(aes(Year, n, text=text, group = 1))+
          geom_point(aes(size = Citations,colour = Citations), show.legend = TRUE)+
          geom_path()+
          ylab(NULL)+
          xlab(NULL)+
          ggtitle("Documents by year")+
          theme(
            panel.grid = element_line(colour = "black"),
            panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill="transparent", color=NA),
            legend.background = element_rect(fill = "transparent"),
            plot.margin = unit(c(0, 0, 0, 0), "points"))
        
        ggplotly(papersPlot, tooltip = c("text")) %>% 
          config(displayModeBar = FALSE) %>% 
          layout(xaxis=list(fixedrange=TRUE)) %>% 
          layout(yaxis=list(fixedrange=TRUE))
        
      })
    }
    })
  
  output$dataInfo1 <- renderPlotly({
    
    data %>% 
      filter(Countries%in%LAcountries$Countries) %>% 
      group_by(Countries) %>% 
      summarise(total=n()) %>% 
      ungroup() %>% plot_ly(labels = ~Countries, values = ~total, key=~Countries, type = 'pie',
                            source = "dataTab1",
                            showlegend = FALSE) %>% 
      layout(title = 'Authors by country',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
  })


  output$dataInfo1.1 <- renderUI({
    HTML(paste0("<h4 align='center'>Top 100 Index Authors ", plotlyEvent1()$key, "</h4>"))    
    })
  
  observeEvent(plotlyEvent1()$key,{
    shinyjs::enable("resetPie11")
  })
  
  observeEvent(input$resetPie11, {
      runjs("Shiny.setInputValue('plotly_click-dataTab1', null);")
    shinyjs::disable("resetPie11")
    })
  
  output$dataInfo1.2 <- renderTable({
    if(is.null(plotlyEvent1()$key)){
      dataPrint1 <<- data %>% 
        select(Authors, Affiliations, Index) %>% 
        distinct() %>% 
        group_by(Authors, Affiliations) %>% 
        mutate(Index=mean(Index)) %>% 
        ungroup() %>% 
        distinct() %>% 
        slice_max(order_by = Index, n = 100) %>% 
        mutate(Authors=paste0(Authors, " | ", Affiliations)) %>% 
        select(Authors, FRI=Index)
      dataPrint1
    } else {
      data1.2 <- data %>% 
        filter(Countries==plotlyEvent1()$key) %>% 
        select(Authors, Affiliations , Index) %>% 
        distinct() %>% 
        group_by(Authors, Affiliations) %>% 
        mutate(Index=mean(Index)) %>% 
        ungroup() %>% 
        distinct() %>% 
        slice_max(order_by = Index, n = 100) 
      
      #maxdata1.2 <- max(data1.2$Index)
      
      dataPrint1 <<- data1.2 %>% 
       # mutate(Index=Index/maxdata1.2*100) %>% 
        mutate(Authors=paste0(Authors," | ", Affiliations)) %>% 
        select(Authors, FRI=Index)
      dataPrint1
    }
  },
  width = "100%")
  
  output$dataInfo2 <- renderPlotly({
    
    data %>% 
      filter(Countries%in%LAcountries$Countries) %>% 
      select(Countries, Affiliations) %>% 
      distinct() %>% 
      group_by(Countries) %>%
      summarise(total=n()) %>% 
      ungroup() %>% plot_ly(labels = ~Countries, values = ~total, key=~Countries, type = 'pie',
                            source="dataTab2",
                            showlegend = FALSE) %>% 
      layout(title = 'Affiliations by country',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
  })
  
  output$dataInfo2.1 <- renderUI({
    HTML(paste0("<h4 align='center'>Top 100 Index Institutions ", plotlyEvent2()$key, "</h4>" ))
    })
  
  observeEvent(plotlyEvent2()$key,{
    shinyjs::enable("resetPie21")
  })
  
  observeEvent(input$resetPie21, {
    runjs("Shiny.setInputValue('plotly_click-dataTab2', null);")
    shinyjs::disable("resetPie21")
  })
  
  output$dataInfo2.2 <- renderTable({
    if(is.null(plotlyEvent2()$key)){
      dataPrint2 <<- institutions %>% 
        select(Affiliations, FRI=Index) %>% 
        distinct() %>% 
        slice_max(order_by = FRI, n = 100) 
      dataPrint2 } else {
        institutions2.2 <- institutions %>% 
            filter(Countries==plotlyEvent2()$key) %>% 
            select(Affiliations, FRI=Index)  %>% 
            distinct() %>% 
            slice_max(order_by = FRI, n = 100)
          
         # maxInstitutions2.2 <- max(institutions2.2$FRI)

           dataPrint2 <<- institutions2.2 
          #   mutate(FRI=FRI/maxInstitutions2.2*100)
           
           dataPrint2
                  }
  },
  width = "100%")
  
  output$dataInfo3 <- renderPlotly({
    
    papersByCountryYear %>% 
      separate_longer_delim(Countries, delim = ";") %>% 
      group_by(Countries) %>% 
      summarize(documents=sum(n)) %>% 
      ungroup() %>% 
      plot_ly(labels = ~Countries, values = ~documents, key=~Countries, type = 'pie',
              source = "dataTab3",
              showlegend = FALSE) %>% 
      layout(title = 'Papers by country',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
  })
  
  output$dataInfo3.1 <- renderUI({
    HTML(paste0("<h4 align='center'>Top 100 Index Papers ", plotlyEvent3()$key, "</h4>" ))})
  
  
  observeEvent(plotlyEvent3()$key,{
    shinyjs::enable("resetPie31")
  })
  
  observeEvent(input$resetPie31, {
    runjs("Shiny.setInputValue('plotly_click-dataTab3', null);")
    shinyjs::disable("resetPie31")
  })
  
  output$dataInfo3.2 <- renderTable({
    if(is.null(plotlyEvent3()$key)){
      dataPrint3 <<- TopPapers %>% 
        select(Title, FRI=Index) %>% 
        distinct() %>% 
        slice_max(order_by = FRI, n = 100) 
      dataPrint3 } else {
          TopPapers3.2 <- TopPapers %>% 
            filter(str_detect(Countries,plotlyEvent3()$key)) %>% 
            select(Title, FRI=Index) %>% 
            distinct() %>% 
            slice_max(order_by = FRI, n = 100) 
          
         # maxTopPapers3.2 <- max(TopPapers3.2$FRI)
          
           dataPrint3 <<- TopPapers3.2 
          #   mutate(FRI=FRI/maxTopPapers3.2*100)
           dataPrint3
        }},
    width= "100%")
  
####MAP SERVER####
  
  output$instruction <- renderUI(HTML("<h3 id='myInstruction'>Click on any country to display more information</h3>"))
  
  observeEvent(input$leafdown_shape_click, { # update the location selectInput on map clicks
    
    shinyjs::hide(id="instruction")})
  
  observeEvent(input$continents, {
    
    if(input$continents=="Just Latin America"){
      countriesRelevance <- countriesRelevance  %>% 
        filter(continent== "South America" | subregion=="Central America")
      
      
      #maxScore <- max(countriesRelevance$totalScore, na.rm = TRUE)
      myColors <- tibble(Productivity=pull(filter(countriesRelevance, !Countries=="BRAZIL")[,8]), 
                         color=my_palette[cut(pull(filter(countriesRelevance, !Countries=="BRAZIL")[,8]),
                                              11)]) %>% 
        add_row(Productivity=194486, color="black")
      
      myColorsL <- tibble(Productivity=c("0", "up to 25000", "up to 40000", "up to 70000"), 
                         color=my_paletteL[cut(c(0, 25000, 40000, 70000),
                                              4)]) %>% 
        add_row(Productivity="up to 200000", color="black")
      
      countriesRelevance <- countriesRelevance %>% 
        left_join(myColors) %>%
        distinct() 
      
      output$leafdown <- renderLeaflet({
        map <- leaflet(countriesRelevance$geom) %>% 
          setView(lat = -15, lng = -75, zoom = 3.4)
        
        map %>% 
          addPolygons(
            fillColor = countriesRelevance$color,
            weight = 1,
            opacity = 1,
            color = "white",
            dashArray = "",
            fillOpacity = 1,
            layerId = countriesRelevance$Countries,
            highlight = highlightOptions(
              weight = 4,
              color = "white",
              dashArray = "",
              fillOpacity = 1,
              bringToFront = TRUE),
            label = map(glue("<b>{as.character(countriesRelevance$Countries)}</b><br>
                             Productivity: <span>{as.character(countriesRelevance$Productivity)}</span><br>
                             Relevance Score: <span>{as.character(countriesRelevance$percen)}</span>"),
                        htmltools::HTML),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto")
          ) %>% 
          addLegend(colors = myColorsL$color,
                    labels = myColorsL$Productivity,
                    opacity = 1)
        
      })
      
      observeEvent(input$leafdown_shape_click, { # update the location selectInput on map clicks
        
        output$columnTitle <- renderPrint ({
          HTML("<h2 id='countriesColumn'>", input$leafdown_shape_click$id, "</h2>")
        })
        
        output$productivity <- renderPlotly({
          relevancePlot <- countriesRelevance %>% 
            mutate(text=paste0("<b>", Countries,"</b>", "<br>",
                               Productivity, " documents found")) %>% 
            filter(Countries==input$leafdown_shape_click$id) %>% 
            select(Countries, Productivity, text) %>% 
            distinct() %>% 
            ggplot(aes(Productivity, Countries, text=text))+
            geom_col(fill="#F4AA40")+
            geom_col(aes(194485, Countries), color="black", fill= "transparent", show.legend = FALSE)+
            ggtitle("Productivity (logarithmic scale)", 
                    subtitle = "The relevance score is the mean of the 5 most frequent relevant words")+
            ylab(NULL)+
            xlab(NULL)+
            scale_x_log10(limits = c(1,194486), expand = c(0, 0)) +
            #xlim(0, 100)+
            theme(axis.text.y=element_blank(), 
                  axis.ticks.y=element_blank())+
            theme(
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              title = element_text(colour = "#000000", size=10)
            )
          
          ggplotly(relevancePlot, tooltip = c("text")) %>% 
            config(displayModeBar = FALSE) %>% 
            layout(xaxis=list(fixedrange=TRUE)) %>% 
            layout(yaxis=list(fixedrange=TRUE))
        })
        
        output$relevanceScore <- renderPlotly({
          relevancePlot <- countriesRelevance %>% 
            mutate(text=paste0("<b>", Countries,"</b>", "<br>",
                               "Relevance Score ", percen, "%")) %>% 
            filter(Countries==input$leafdown_shape_click$id) %>% 
            select(Countries, totalScore, percen, text) %>% 
            distinct() %>% 
            ggplot(aes(percen, Countries, text=text))+
            geom_col(fill="#F4AA40")+
            geom_col(aes(99.9999, Countries), color="black", fill= "transparent", show.legend = FALSE)+
            ggtitle("Relevance Score (%)", 
                    subtitle = "The relevance score is the mean of the 5 most frequent relevant words")+
            ylab(NULL)+
            xlab(NULL)+
            scale_x_continuous(limits = c(0,100), expand = c(0, 0)) +
            theme(axis.text.y=element_blank(), 
                  axis.ticks.y=element_blank())+
            theme(
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              title = element_text(colour = "#000000", size=10)
            )
          
          ggplotly(relevancePlot, tooltip = c("text")) %>% 
            config(displayModeBar = FALSE) %>% 
            layout(xaxis=list(fixedrange=TRUE)) %>% 
            layout(yaxis=list(fixedrange=TRUE))
        })
        
        output$relevanceWords <- renderPlotly({
          relevantWords <- countriesRelevance %>%
            filter(Countries==input$leafdown_shape_click$id) %>%
            select(word, score, n) %>% 
            mutate(percen=n/sum(n)*100) %>% 
            distinct() %>% 
            mutate(text=paste0("<b>", word, "</b>", "<br>", 
                               "Frequency: ", n,"<br>",
                               round(percen, 2), "%")) %>% 
            mutate(word=if_else(percen<5, paste0("      ", word), 
                                if_else(percen<10, paste0("  ", word), word))) %>% 
            ggplot(aes(percen, reorder(as.factor(word), percen), text=text,
                       group=percen))+
            geom_col(fill="#F4AA40")+
            geom_col(aes(100, word), color="black", fill= "transparent", show.legend = FALSE)+
            ggtitle("Most Frequent Relevant Words")+
            ylab(NULL)+
            xlab(NULL)+
            scale_x_continuous(limits = c(0,100), expand = c(0, 0)) +
            theme(axis.text.y=element_blank(), 
                  axis.ticks.y=element_blank())+
            theme(
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              title = element_text(colour = "#000000", size=10)
            )+
            geom_text(aes(label = word, group=score), colour="black", 
                      vjust="inward",hjust="inward")
          
          ggplotly(relevantWords, tooltip = c("text")) %>% 
            config(displayModeBar = FALSE) %>% 
            layout(xaxis=list(fixedrange=TRUE)) %>% 
            layout(yaxis=list(fixedrange=TRUE))
        })
        
        output$tableTitle <- renderText(paste0(
          "<h4 style='margin-left:20px; color:white;'>100 Most Relevant Institutions of ", 
          input$leafdown_shape_click$id, "</h4>"
        ))
        
        output$authorsTable <- renderDataTable({
          institutions %>% 
            filter(Countries==input$leafdown_shape_click$id) %>%
            slice_max(order_by = relevance, n = 100) %>% 
            select(Institution=Affiliations, Relevance=relevance) %>% 
            datatable(extensions = 'Buttons',
                      rownames = FALSE,
                      options = list(
                        info = FALSE,
                        paging = FALSE,
                        searching = FALSE,
                        buttons = c('copy', 'csv', 'excel')
                      ),class = "display") %>% 
            DT::formatStyle(columns = 1:3, color="black", fontSize = '75%')
        })
      })
      
      #close just latin america
    }
    else{
      myColors <- tibble(Productivity=pull(filter(countriesRelevance, !Countries=="BRAZIL")[,8]), 
                         color=my_palette[cut(pull(filter(countriesRelevance, !Countries=="BRAZIL")[,8]),
                                              11)]) %>% 
        add_row(Productivity=194486, color="black")
      
      myColorsL <- tibble(Productivity=c("0", "up to 1000", "up to 10000", "up to 40000"), 
                          color=my_paletteL[cut(c(0, 1000, 10000, 40000),
                                                4)]) %>% 
        add_row(Productivity="up to 200000", color="black")
      
      countriesRelevance <- countriesRelevance %>% 
        left_join(myColors) %>%
        distinct() 
      
      output$leafdown <- renderLeaflet({
        map <- leaflet(countriesRelevance$geom) %>% 
          setView(lat = 30, lng = 18, zoom = 2) 
        
        map %>% 
          addPolygons(
            fillColor = countriesRelevance$color,
            weight = 1,
            opacity = 1,
            color = "white",
            dashArray = "",
            fillOpacity = 1,
            layerId = countriesRelevance$Countries,
            highlight = highlightOptions(
              weight = 4,
              color = "white",
              dashArray = "",
              fillOpacity = 1,
              bringToFront = TRUE),
            label = map(glue("<b>{as.character(countriesRelevance$Countries)}</b><br>
                             Productivity: <span>{as.character(countriesRelevance$Productivity)}</span><br>
                             Relevance Score: <span>{as.character(countriesRelevance$percen)}</span>"),
                        htmltools::HTML),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto")
          ) %>% 
          addLegend(opacity = 1,
                    colors = myColorsL$color,
                    labels = myColorsL$Productivity)
        
      })
      
      observeEvent(input$leafdown_shape_click, { # update the location selectInput on map clicks
        
        output$columnTitle <- renderPrint ({
          h2(input$leafdown_shape_click$id)
        })
        
        output$productivity <- renderPlotly({
          relevancePlot <- countriesRelevance %>% 
            mutate(text=paste0("<b>", Countries,"</b>", "<br>",
                               Productivity, " documents found")) %>% 
            filter(Countries==input$leafdown_shape_click$id) %>% 
            select(Countries, Productivity, text) %>% 
            distinct() %>% 
            ggplot(aes(Productivity, Countries, text=text))+
            geom_col(fill="#F4AA40")+
            geom_col(aes(194485, Countries), color="white", fill= "transparent", show.legend = FALSE)+
            ggtitle("Productivity (logarithmic scale)", 
                    subtitle = "The relevance score is the mean of the 5 most frequent relevant words")+
            ylab(NULL)+
            xlab(NULL)+
            scale_x_log10(limits = c(1,194486), expand = c(0, 0)) +
            theme(axis.text.y=element_blank(), 
                  axis.ticks.y=element_blank())+
            theme(
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              title = element_text(colour = "#ffffff", size=10)
            )
          
          ggplotly(relevancePlot, tooltip = c("text")) %>% 
            config(displayModeBar = FALSE) %>% 
            layout(xaxis=list(fixedrange=TRUE)) %>% 
            layout(yaxis=list(fixedrange=TRUE))
        })
        
        output$relevanceScore <- renderPlotly({
          relevancePlot <- countriesRelevance %>% 
            mutate(text=paste0("<b>", Countries,"</b>", "<br>",
                               "Relevance Score ", percen, "%")) %>% 
            filter(Countries==input$leafdown_shape_click$id) %>% 
            select(Countries, totalScore, percen, text) %>% 
            distinct() %>% 
            ggplot(aes(percen, Countries, text=text))+
            geom_col(fill="#F4AA40")+
            geom_col(aes(99.999, Countries), color="white", fill= "transparent", show.legend = FALSE)+
            ggtitle("Relevance Score (%)",
                    subtitle = "The relevance score is the mean of the 5 most frequent relevant words")+
            ylab(NULL)+
            xlab(NULL)+
            scale_x_continuous(limits = c(0,100), expand = c(0, 0)) +
            theme(axis.text.y=element_blank(), 
                  axis.ticks.y=element_blank())+
            theme(
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              title = element_text(colour = "#ffffff", size=10)
            )
          
          ggplotly(relevancePlot, tooltip = c("text")) %>% 
            config(displayModeBar = FALSE) %>% 
            layout(xaxis=list(fixedrange=TRUE)) %>% 
            layout(yaxis=list(fixedrange=TRUE)) 
        })
        
        output$relevanceWords <- renderPlotly({
          relevantWords <- countriesRelevance %>%
            filter(Countries==input$leafdown_shape_click$id) %>%
            select(word, score, n) %>% 
            mutate(percen=n/sum(n)*100) %>% 
            distinct() %>% 
            mutate(text=paste0("<b>", word, "</b>", "<br>", 
                               "Frequency: ", n,"<br>",
                               round(percen, 2), "%")) %>% 
            mutate(word=if_else(percen<5, paste0("      ", word), 
                                if_else(percen<10, paste0("  ", word), word))) %>% 
            ggplot(aes(percen, reorder(as.factor(word), percen), text=text,
                       group=percen))+
            geom_col(fill="#F4AA40")+
            geom_col(aes(100, word), color="white", fill= "transparent", show.legend = FALSE)+
            ggtitle("Most Frequent Relevant Words")+
            ylab(NULL)+
            xlab(NULL)+
            scale_x_continuous(limits = c(0,100), expand = c(0, 0)) +
            theme(axis.text.y=element_blank(), 
                  axis.ticks.y=element_blank())+
            theme(
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              title = element_text(colour = "#ffffff", size=10)
            )+
            geom_text(aes(label = word, group=score), colour="white", 
                      vjust="inward",hjust="inward")
          
          ggplotly(relevantWords, tooltip = c("text")) %>% 
            config(displayModeBar = FALSE) %>% 
            layout(xaxis=list(fixedrange=TRUE)) %>% 
            layout(yaxis=list(fixedrange=TRUE))
        })
        
        output$tableTitle <- renderText(paste0(
          "<h4 style='margin-left:20px; color:white;'>100 Most Relevant Institutions of ", 
          input$leafdown_shape_click$id, "</h4>"
        ))
        
        output$authorsTable <- renderDataTable({
          institutions %>% 
            filter(Countries==input$leafdown_shape_click$id) %>%
            slice_max(order_by = Score, n = 100) %>% 
            select(Institution=Affiliations, Relevance=Score) %>% 
            datatable(rownames = FALSE,
                      options = list(
                        info = FALSE,
                        paging = FALSE,
                        searching = FALSE
                      )) %>% 
            DT::formatStyle(columns = 1:3, color="white", fontSize = '75%')
        })
      })
      }}
    )
  
####AUTHORS SERVER####
  output$distPlot <- renderPlotly(if (input$radio=="productivity"){
    prepPlotScores <- data %>% 
      filter(productivity>=input$productivity) %>% 
      filter(relevance>=input$relevance) 
    
   maxPrepPlotScoresR <- max(prepPlotScores$relevance)+0.01
   minPrepPlotScoresR <- min(prepPlotScores$relevance)
   maxPrepPlotScoresP <- max(prepPlotScores$productivity)+10
   minPrepPlotScoresP <- min(prepPlotScores$productivity)
    
    plotScores <- prepPlotScores %>% 
      mutate(scoreN=(relevance-minPrepPlotScoresR)/(maxPrepPlotScoresR-minPrepPlotScoresR)) %>% 
      mutate(productivityN=(productivity-minPrepPlotScoresP)/(maxPrepPlotScoresP-minPrepPlotScoresP)) %>% 
      mutate(text=paste0("<b>",Authors, "</b>", "\n", 
                         Countries, "\n",
                         "Productivity: ", productivity, "\n",
                         "Influence: ", TC, "\n",
                         "Relevance: ", relevance)) %>% 
      ggplot(aes(scoreN, productivityN, color= Countries, text=text, key=Authors))+
      geom_jitter(alpha=0.6, height = 0, width = 0.005)+
      xlab("Normalized Relevance")+
      ylab("Normalized Productivity")+
      scale_x_continuous(minor_breaks = seq(0 , 1, 0.5), breaks = seq(0, 1, 0.5))+
      scale_y_continuous(minor_breaks = seq(0 , 1, 0.5), breaks = seq(0, 1, 0.5))+
      annotate("text", 
               x = c(0.1, 0.9, 0.1, 0.9), 
               y = c(0.4, 0.4, 0.6, 0.6), 
               colour = "black", size = 3,
               label = c("<i>low productivity\nlow relevance</i>", 
                         "<i>low productivity\nhigh relevance</i>", 
                         "<i>high productivity\nlow relevance</i>", 
                         "<i>high productivity\nhigh relevance</i>"),
               parse=TRUE)+
      theme(
        panel.grid = element_line(colour = "black"),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill = "transparent"))
    
    ggplotly(plotScores, source = "authorsTab", tooltip = "text")%>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
    
  } else {
    prepPlotScores <- data %>% 
      filter(TC>=input$citations) %>% 
      filter(relevance>=input$relevance) 
    
    maxPrepPlotScoresR <- max(prepPlotScores$relevance)
    minPrepPlotScoresR <- min(prepPlotScores$relevance)
    maxPrepPlotScoresTC <- max(prepPlotScores$TC)
    minPrepPlotScoresTC <- min(prepPlotScores$TC)
    
    plotScores <- prepPlotScores %>% 
      mutate(scoreN=(relevance-minPrepPlotScoresR)/(maxPrepPlotScoresR-minPrepPlotScoresR)) %>% 
      mutate(TCN=(TC-minPrepPlotScoresTC)/(maxPrepPlotScoresTC-minPrepPlotScoresTC)) %>% 
      mutate(text=paste0("<b>",Authors, "</b>", "\n", 
                         #Affiliations, "\n",
                         Countries, "\n",
                         "Productivity: ", productivity, "\n",
                         "Influence: ", TC, "\n",
                         "Relevance: ", relevance)) %>% 
      ggplot(aes(scoreN, TCN, color= Countries, text=text, key=Authors))+
      geom_jitter(alpha=0.6, height = 0, width = 0.005)+
      xlab("Normalized Relevance")+
      ylab("Normalized influence")+
      scale_x_continuous(minor_breaks = seq(0 , 1, 0.5), breaks = seq(0, 1, 0.5))+
      scale_y_continuous(minor_breaks = seq(0 , 1, 0.5), breaks = seq(0, 1, 0.5))+
      annotate("text", 
               x = c(0.1, 0.9, 0.1, 0.9), 
               y = c(0.4, 0.4, 0.6, 0.6), 
               label = c("<i>low influence\nlow relevance</i>", 
                         "<i>low influence\nhigh relevance</i>", 
                         "<i>high influence\nlow relevance</i>", 
                         "<i>high influence\nhigh relevance</i>"),
               parse=TRUE,
               colour = "black", size = 3)+
      theme(
        panel.grid = element_line(colour = "black"),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill = "transparent"))
    
    ggplotly(plotScores, source = "authorsTab",  tooltip = "text")%>% 
      config(displayModeBar = FALSE)  %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
  }
  )
  
  shinyjs::hide("authorName")
  
  observeEvent(plotlyEvent4()$key, {
    shinyjs::show("authorName")
    institutions <- data %>% filter(Authors==plotlyEvent4()$key) %>% 
      mutate(Affiliations=gsub(";", " | ", Affiliations)) %>%
      select(Affiliations) %>% distinct() %>% 
      pull(Affiliations)
    output$authorName <- renderUI(HTML(paste0("<H4 id='authorName'>Papers with the participation of <b>", 
                                              plotlyEvent4()$key, "</b></H4><br>", institutions, "<br><br>")))
  })
  
  observeEvent(plotlyEvent4()$key,{
  output$worksTable = renderDataTable({
    papers %>% 
      filter(str_detect(Authors, paste0("\\b", plotlyEvent4()$key, "\\b"))) %>% 
                        #event_data("plotly_click")$key)) %>% 
      arrange(desc(relevance)) %>% 
      select(Authors, Year, Title, Relevance=relevance, Influence=influence, FRI=Index) %>% 
      datatable(rownames = FALSE, escape = FALSE,
                options = list(searching = FALSE, pageLength = 25,lengthChange = FALSE,
                               lengthMenu = c(50, 100, 500, 1000), scrollX = T,
                               width="100%",autoWidth = TRUE)) %>% 
      DT::formatStyle(columns = 1:6, color="white", fontSize = '75%')
  })
  shinyjs::show("downloadData4")
  })
  
  observeEvent(input$searchAuthor,{
    
    myAuthors <- papers %>% 
      select(Authors) %>% 
      mutate(Authors=gsub(" et al.", "", Authors)) %>% 
      separate_longer_delim(Authors, delim = ";") %>% 
      distinct() %>% 
      pull(Authors)
    
    if(!any(str_detect(myAuthors, paste(toupper(input$nameAuthor), collapse = '|')))) {
      shinyjs::show("notification")
      output$notification <- renderText("We can not find such author; try with another or prove 
                                        another spelling")
      shinyjs::hide("links")
    } else {
    
      shinyjs::hide("notification")
    
    updateRadioGroupButtons(
      inputId = "links",
      label= "Which author do you refer?",
      choices=myAuthors[str_detect(myAuthors, paste(toupper(input$nameAuthor), collapse = '|'))])
    
    shinyjs::show("links", time = 2)}
    
      observeEvent(input$links,{
        
        shinyjs::show("authorName")
  
    
          output$worksTable = renderDataTable({
              
            papersPrint <- papers %>% 
              filter(str_detect(Authors, paste0(toupper(isolate(input$links))))) %>% 
              arrange(desc(relevance)) %>% 
              select(Authors, Year, Title, Relevance=relevance, Influence=influence, FRI=Index)
            
            papersPrint %>% 
              datatable(rownames = FALSE, escape = FALSE,
                        options = list(searching = FALSE, pageLength = 25,lengthChange = FALSE,
                                       lengthMenu = c(50, 100, 500, 1000), scrollX = T,
                                       width="100%",autoWidth = TRUE)) %>% 
              DT::formatStyle(columns = 1:6, color="white", fontSize = '75%')
            })

    
    institutions <- data %>% filter(Authors==input$links) %>% 
      mutate(Affiliations=gsub(";", " | ", Affiliations)) %>%
      select(Affiliations) %>% distinct() %>% 
      pull(Affiliations)
    
    output$authorName <- renderUI(HTML(paste0("<H4 id='authorName'>Papers with the participation of <b>",
                                              isolate(input$links), "</b></H4><br>", institutions, "<br><br>")))
    })
    
  })
  
  ####WORDS SERVER####
  observeEvent(input$runCbow, {
    
    myWord <- tolower(input$wordEmbed)
    
    if (myWord %in% row.names(as.matrix(cbow_modelBR))==FALSE |
        myWord %in% row.names(as.matrix(cbow_modelMX))==FALSE |
        myWord %in% row.names(as.matrix(cbow_modelCH))==FALSE |
        myWord %in% row.names(as.matrix(cbow_modelCO))==FALSE){
      
      shinyjs::show("notification2")
      output$notification2 <- renderText("This word is not in the vocabulary; 
      try with another word or prove another spelling")
      
    } else {
      
      shinyjs::hide("notification2")
    
    cbow_lookslikeBR <- predict(cbow_modelBR, myWord,
                            type = "nearest", top_n = 60)
    cbow_lookslikeMX <- predict(cbow_modelMX, myWord,
                                type = "nearest", top_n = 60)
    cbow_lookslikeCH <- predict(cbow_modelCH, myWord,
                                type = "nearest", top_n = 60)
    cbow_lookslikeCO <- predict(cbow_modelCO, myWord,
                                type = "nearest", top_n = 60)
    
    names(cbow_lookslikeBR) <- "myDF"
    names(cbow_lookslikeMX) <- "myDF"
    names(cbow_lookslikeCH) <- "myDF"
    names(cbow_lookslikeCO) <- "myDF"

    cbow_lookslikeBRDF <- cbow_lookslikeBR$myDF %>% tibble()
    cbow_lookslikeMXDF <- cbow_lookslikeMX$myDF %>% tibble()
    cbow_lookslikeCHDF <- cbow_lookslikeCH$myDF %>% tibble()
    cbow_lookslikeCODF <- cbow_lookslikeCO$myDF %>% tibble()
    
    cbow_embeddingBR <- predict(cbow_modelBR, cbow_lookslikeBRDF$term2,
                              type = "embedding")
    cbow_embeddingMX <- predict(cbow_modelMX, cbow_lookslikeMXDF$term2,
                              type = "embedding")
    cbow_embeddingCH <- predict(cbow_modelCH, cbow_lookslikeCHDF$term2,
                              type = "embedding")
    cbow_embeddingCO <- predict(cbow_modelCO, cbow_lookslikeCODF$term2,
                              type = "embedding")
    
    cbow_embeddingBR <- na.omit(cbow_embeddingBR)
    cbow_embeddingMX <- na.omit(cbow_embeddingMX)
    cbow_embeddingCH <- na.omit(cbow_embeddingCH)
    cbow_embeddingCO <- na.omit(cbow_embeddingCO)
    
    vizualizationBR <- umap(cbow_embeddingBR,
                        n_neighbors = 15,
                        n_threads = 2)
    vizualizationMX <- umap(cbow_embeddingMX,
                          n_neighbors = 15,
                          n_threads = 2)
    vizualizationCH <- umap(cbow_embeddingCH,
                          n_neighbors = 15,
                          n_threads = 2)
    vizualizationCO <- umap(cbow_embeddingCO,
                          n_neighbors = 15,
                          n_threads = 2)
     
    dfBR <- data.frame(word = rownames(cbow_embeddingBR),
                   xpos = gsub(".+//", "", rownames(cbow_embeddingBR)),
                   x = vizualizationBR$layout[, 1], y = vizualizationBR$layout[, 2],
                   stringsAsFactors = FALSE)
    dfMX <- data.frame(word = rownames(cbow_embeddingMX),
                     xpos = gsub(".+//", "", rownames(cbow_embeddingMX)),
                     x = vizualizationMX$layout[, 1], y = vizualizationMX$layout[, 2],
                     stringsAsFactors = FALSE)
    dfCH <- data.frame(word = rownames(cbow_embeddingCH),
                     xpos = gsub(".+//", "", rownames(cbow_embeddingCH)),
                     x = vizualizationCH$layout[, 1], y = vizualizationCH$layout[, 2],
                     stringsAsFactors = FALSE)
    dfCO <- data.frame(word = rownames(cbow_embeddingCO),
                     xpos = gsub(".+//", "", rownames(cbow_embeddingCO)),
                     x = vizualizationCO$layout[, 1], y = vizualizationCO$layout[, 2],
                     stringsAsFactors = FALSE)

    dfBR <- dfBR %>%
      left_join(cbow_lookslikeBRDF, by = c("word"="term2"))
    dfMX <- dfMX %>%
      left_join(cbow_lookslikeMXDF, by = c("word"="term2"))
    dfCH <- dfCH %>%
      left_join(cbow_lookslikeCHDF, by = c("word"="term2"))
    dfCO <- dfCO %>%
      left_join(cbow_lookslikeCODF, by = c("word"="term2"))
    
    dfPlotBR <- dfBR %>%
      mutate(text=paste0("<b>", word, "</b><br>","Similarity: ", round(similarity,3) )) %>% 
      ggplot(aes(x, y, colour = similarity, text=text))+
      geom_text(aes(label=word, size=similarity), show.legend = FALSE)+
      ggtitle("BRAZIL")+
      xlab(NULL)+
      ylab(NULL)+
      theme_void()+
      theme(
        panel.background = element_rect(fill='#B0BAC3'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill = "transparent"),
        panel.grid.major=element_line(colour="#B0BAC3"),
        panel.grid.minor=element_line(colour="#B0BAC3"))
    dfPlotMX <- dfMX %>%
      mutate(text=paste0("<b>", word, "</b><br>","Similarity: ", round(similarity,3) )) %>% 
      ggplot(aes(x, y, colour = similarity, text=text))+
      geom_text(aes(label=word, size=similarity), show.legend = FALSE)+
      ggtitle("MEXICO")+
      xlab(NULL)+
      ylab(NULL)+
      theme_void()+
      theme(
        panel.background = element_rect(fill='#B0BAC3'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill = "transparent"),
        panel.grid.major=element_line(colour="#B0BAC3"),
        panel.grid.minor=element_line(colour="#B0BAC3"))
    dfPlotCH <- dfCH %>%
      mutate(text=paste0("<b>", word, "</b><br>","Similarity: ", round(similarity,3) )) %>% 
      ggplot(aes(x, y, colour = similarity, text=text))+
      geom_text(aes(label=word, size=similarity), show.legend = FALSE)+
      ggtitle("CHILE")+
      theme_void()+
      xlab(NULL)+
      ylab(NULL)+
      theme(
        panel.background = element_rect(fill='#B0BAC3'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill = "transparent"),
        panel.grid.major=element_line(colour="#B0BAC3"),
        panel.grid.minor=element_line(colour="#B0BAC3"))
    dfPlotCO <- dfCO %>%
      mutate(text=paste0("<b>", word, "</b><br>","Similarity: ", round(similarity,3) )) %>% 
      ggplot(aes(x, y, colour = similarity, text=text))+
      geom_text(aes(label=word, size=similarity), show.legend = FALSE)+
      ggtitle("COLOMBIA")+
      theme_void()+
      xlab(NULL)+
      ylab(NULL)+
      theme(
        panel.background = element_rect(fill='#B0BAC3'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill = "transparent"),
        panel.grid.major=element_line(colour="#B0BAC3"),
        panel.grid.minor=element_line(colour="#B0BAC3"))
    
    output$cbowPlotBR <- renderPlotly(ggplotly(dfPlotBR, tooltip = c("text"))%>% 
                                        config(displayModeBar = FALSE)  %>% 
                                        layout(xaxis=list(fixedrange=TRUE)) %>% 
                                        layout(yaxis=list(fixedrange=TRUE)))
    output$cbowPlotMX <- renderPlotly(ggplotly(dfPlotMX, tooltip = c("text"))%>% 
                                        config(displayModeBar = FALSE)  %>% 
                                        layout(xaxis=list(fixedrange=TRUE)) %>% 
                                        layout(yaxis=list(fixedrange=TRUE)))
    output$cbowPlotCH <- renderPlotly(ggplotly(dfPlotCH, tooltip = c("text"))%>% 
                                        config(displayModeBar = FALSE)  %>% 
                                        layout(xaxis=list(fixedrange=TRUE)) %>% 
                                        layout(yaxis=list(fixedrange=TRUE)))
    output$cbowPlotCO <- renderPlotly(ggplotly(dfPlotCO, tooltip = c("text"))%>% 
                                        config(displayModeBar = FALSE)  %>% 
                                        layout(xaxis=list(fixedrange=TRUE)) %>% 
                                        layout(yaxis=list(fixedrange=TRUE)))
    
    dfBR <- dfBR %>% summarise(total=sum(similarity), Country= "Brazil") 
    dfMX <- dfMX %>% summarise(total=sum(similarity), Country="Mexico") 
    dfCH <- dfCH %>% summarise(total=sum(similarity), Country="Chile") 
    dfCO <- dfCO %>% summarise(total=sum(similarity), Country="Colombia") 
    
    dfAll <- bind_rows(dfBR, dfMX, dfCH, dfCO)
    
    output$similarityPlot <- renderPlotly(ggplotly(
      dfAll %>% 
        mutate(text=paste0(Country, ": ", round(total, 4))) %>% 
        ggplot(aes(total, reorder(as.factor(Country), total), text=text))+
        geom_col(fill="#F4AA40")+
        geom_text(aes(label = Country), colour="white", 
                  vjust="inward", hjust="inward")+
        theme_bw()+
        ggtitle(paste0(myWord, " similarity mean per country"))+
        xlab(NULL)+
        ylab(NULL)+
        theme(
          panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='transparent', color=NA),
          panel.grid.major=element_line(colour="transparent"),
          panel.grid.minor=element_line(colour="transparent"),
          panel.border = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()),
      tooltip = c("text")
      )%>% 
        config(displayModeBar = FALSE)  %>% 
        layout(xaxis=list(fixedrange=TRUE)) %>% 
        layout(yaxis=list(fixedrange=TRUE))
    )
    }
  })
  
  ####FORUM SERVER####
  observeEvent(input$sendFeedback, {
    tibble(Name=input$feedbackName, 
           Comment=input$feedbackComment,
           Contact=input$email,
           Time=Sys.time(),
           Status="Pendiente") %>% 
      sheet_append(., ss="1weSBqhUVNm57kAEdk4auF8fHvvvyVCoLhGbnJA6AFA4",
                 sheet = "comments")
    
    updateTextInput(session, "feedbackName", value = "") 
    updateTextInput(session, "feedbackComment", value = "") 
    updateTextInput(session, "email", value = "") 
    shinyjs::show("thankyou")
    output$thankyou <- renderText(print("Thank you for your feedback. It will appear 
                                        in the table beside soon."))
  })
  
  output$feedbackTable <- renderDT(read_sheet("1weSBqhUVNm57kAEdk4auF8fHvvvyVCoLhGbnJA6AFA4",
                                                  sheet = "comments") %>%
                                        #mutate(DATE=round(DATE, 0)) %>% 
                                     mutate(DATE=as.POSIXct(DATE,
                                                               format= "%Y-%m-%d %H:%M:%S",
                                                               origin='1970-01-01')) %>% 
                                     filter(STATUS=="Aceptado") %>% 
                                     select(-c(EMAIL, STATUS)) %>% 
                                     arrange(DATE) %>% 
                                     datatable()
                                       )
  
  ####DOWNLOADS####
  output$downloadData1 <- downloadHandler(
      filename = function() {
        paste('topIndexAuthors_', plotlyEvent1()$key, '.xlsx', sep='')
      },
      content = function(con) {
        write.xlsx(dataPrint1, con)
      }
    )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste('topIndexInstitutions_', plotlyEvent2()$key, '.xlsx', sep='')
    },
    content = function(con) {
      write.xlsx(dataPrint2, con)
    }
  )
  
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste('topIndexPapers_', plotlyEvent3()$key, '.xlsx', sep='')
    },
    content = function(con) {
      write.xlsx(dataPrint3, con)
    }
  )
  
  output$downloadData4 <- downloadHandler(
    filename = function() {
      paste('authorsPapers', '.xlsx', sep='')
    },
    content = function(con) {
      write.xlsx(papersPrint, con)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
