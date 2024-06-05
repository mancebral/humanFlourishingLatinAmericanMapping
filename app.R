library(shiny)
library(leaflet)
library(sf)
library(plotly)
library(htmltools)
library(shinyWidgets)
library(DT)
library(grDevices)
library(tidyverse)
library(shinyBS)
library(shinyjs)
library(bslib)
library(glue)
library(shinycssloaders)


#load data
countriesRelevance = readRDS("data/countriesRelevance.RDS")
topRelevantAuthors = readRDS("data/topRelevantAuthors.RDS")
data = readRDS("data/Words3_Authors.rds")
papers = readRDS("data/papersScoredT.rds")
relevantWords = readRDS("data/relevantWords.rds")

#crear paleta de color
my_palette <- grDevices::colorRampPalette(rev(c("#162F43","#3B4952","#55636D","#6D7E8B", 
                                                "#8192A0")))(11)
my_paletteL <- grDevices::colorRampPalette(rev(c("#162F43","#3B4952","#55636D","#6D7E8B", 
                                                 "#8192A0")))(5)

# noNAScores <- countriesRelevance %>% 
#   filter(!is.na(percen))

ui <- navbarPage(collapsible = TRUE,
  
  useShinyjs(),
  
  setBackgroundColor(color = "#8192A0"),
  
  #hide error due to not displaying the table at the beginning
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  #adjust lealfet map to canvas
  tags$style(type = "text/css", "#leafdown {height: calc(100vh - 80px) !important;}"),
  
  #more adjustments
  tags$head(
    tags$style(HTML("
                    .leaflet-container { background: #8192A0; }
                   
                   .nav-tabs {
                    border-bottom: 0px solid #ddd;
                   }
                   .continents{
                   background-color: white;
                   }
                  #frontPage {
                    margin: 0;
                    position: absolute;
                    top: 50%;
                    left: 50%;
                    transform: translate(-50%, -50%);
                  }
                  #about {
                    margin: 0;
                    position: absolute;
                    top: 50%;
                    left: 50%;
                    transform: translate(-50%, -50%);
                  }
                  #myTitle{
                    font-weight: bold;
                    font-size: 60px;
                    text-align: center;
                  }
                  #subtitle {
                    font-size: 16px;
                    text-align: center;
                  }
                  #criteriaText {
                    text-align: left;
                  }
                  #criteriaTitle{
                    font-weight: bold;
                    font-size: 60px;
                  }
                  #criteriaSubtitle {
                    font-size: 16px;
                  }
                  #authorName {
                    text-align: center;
                  }
                  #aboutTitle {
                    font-weight: bold;
                    font-size: 60px;
                    text-align: center;
                  }
                  #aboutText {
                    font-size: 16px;
                    text-align: center;
                  }
                  #countriesColumn {
                  margin-top: 0px;
                  }
                  body {
                    color: #fff;
                  }
                    a:link {
                    color: #B0BAC3;
                    }
                    div.datatables {
                    color: #fff;
                    }
                    #columnTitle {
                    margin-left: 20px;
                    }
                    #col-sm-5{
                    margin-top: 0px;
                    }
                    #authorsTable {
                    margin-left: 10px;
                    }
                    #navbar-collapse-9350 {
                    border-bottom: 0px solid #ddd;
                    margin-bottom: 10px;
                    background-color: #000000;
                    }
                    .nav>li>a {
                    color: white;
                    font-weight: bold;
                    background-color: transparent;
                    }
                    .nav-tabs>li.active>a {
                     font-weight: bold;
                    color: #C44A17;
                    background-color: transparent;
                    cursor: default;
                    border: 0px;
                    border-bottom-color: transparent;
                    }
                    .nav>li:hover>a {
                    color: #C44A17;
                    font-weight: bold;
                    background-color: transparent;
                    border: 0px;
                    }
                    .nav-tabs>li.active>a:focus {
                    color: #C44A17;
                    font-weight: bold;
                    background-color: transparent;
                    border: 0px;
                    }
                    "))),
  
  #color of slider bars
  tags$style(HTML(".irs--shiny .irs-bar {
                    top: 25px;
                    height: 8px;
                    border-top: 1px solid #C44A17;
                    border-bottom: 1px solid #C44A17;
                    background: #C44A17;
                    }
                  .irs--shiny .irs-single {
                    color: #fff;
                    text-shadow: none;
                    padding: 1px 3px;
                    background-color: #C44A17;
                    border-radius: 3px;
                    font-size: 11px;
                    line-height: 1.333;
                  }
                  body {
                  padding-bottom: 50px;
                  }"
                  )),
  
  #compress menu
  # tags$div(class = "myMenu",
  #          tags$link('rel="stylesheet" 
  #          href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"'),
  #          tags$a('href="javascript:void(0);" class="icon" onclick="myFunction()"')
  #          ),
  
  # Footer
  tags$footer(
    style = "position: fixed; bottom: 0; z-index: 9999;
    width: 100%; background-color: #8192A0; color: white; 
    padding-left: 10px; padding-right: 10px; padding-top: 10px; padding-bottom: 10px;
    text-align: center; font-size: 10px;",
    HTML("This App has been conceived by the Human Flourishing Team of the Tec de Monterrey and 
    developed in R Programming Shiny Apps with the support of Templeton World Charity Foundation. 
                     More info about <a href='https://tec.mx/es/florecimiento-humano/entorno-para-florecer/mapeo-de-florecimiento-humano-en-latinoamerica' 
                     target='_blank'>Landscaping Regional Research and Leadership Capacities for the 
         Study of Human Flourishing in Mexico, Colombia, Chile, and Brazil</a>."
    )),

        # #start of the general panel
        # wellPanel(
        #   tabsetPanel(id = "myMenu",
            
            ####INTRO TAB####
            tabPanel("Intro", HTML('<div id="frontPage"><h1 id="myTitle">Human Flourishing<br>in Latin America</h1><br>
                                   <p id="subtitle">Recognizing the regional research landscape and leadership capabilities
                                   in the study of Human Flourishing in Mexico, Colombia, Chile and Brazil.</p></div>')),
            
            ####CRITERIA####
            tabPanel("Criteria", 
                     column(6, 
                            HTML('<div id="criteriaText"><h1 id="criteriaTitle">Criteria</h1><br>
                                   <p id="criteriaSubtitle">Recognizing the regional research landscape and leadership capabilities
                                   in the study of Human Flourishing in Mexico, Colombia, Chile and Brazil.</p></div>')
                            ),
                     column(6,
                            card(
                              card_header(HTML("<h3>RELEVANCE SCORE TABLE</h3>")),
                              height = 800,
                              style = "resize:vertical;",
                              card_body(
                                min_height = 500,
                                tableOutput("criteriaPlot")
                              )
                            )
                     #tableOutput("criteriaPlot")
                     )
                     ),
                     
            ####COUNTRIES####
            tabPanel("Countries", 
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
                            dataTableOutput("authorsTable")#,
                            #verbatimTextOutput("test")
                     )
                     ),
            
            ####AUTHORS####
            tabPanel("Authors", 
                     column(5,
                            HTML("You can filter authors by comparing their <b>relevance vs. productivity</b> 
                                 (number of published documents within the filed) or their <b>relevance vs. 
                                 total citations</b>:<br><br>"),
                       prettyRadioButtons("radio", label = NULL,
                                    choices = list("Productivity" = "productivity", "Total Citations" = "TC"), 
                                    selected = "productivity",shape="square", thick = TRUE),
                       conditionalPanel(
                         condition = "input.radio == 'productivity'",
                         sliderInput("productivity",
                                     "Grade of Productivity:",
                                     min = min(data$productivity),
                                     max = max(data$productivity),
                                     value = 10)
                       ),
                       conditionalPanel(
                         condition = "input.radio == 'TC'",
                         sliderInput("citations",
                                     "Grade of Total Citations:",
                                     min = min(data$TC),
                                     max = max(data$TC),
                                     value = 1000)
                       ),
                       sliderInput("relevance",
                                   "Grade of Relevance:",
                                   min = min(data$score),
                                   max = max(data$score),
                                   value = 100),
                       plotlyOutput("summary")
                     ),
                     
                     column(7,
                            HTML("<h5 id='countriesColumn'>Each point is an author, you can consult their works by clicking on the points. 
                                 You can also filter by country by double clicking on them:</h5>"),
                            plotlyOutput("distPlot")%>% withSpinner(color="white"),
                            HTML("<br>"),
                            htmlOutput("authorName"),
                            dataTableOutput("worksTable")
                     )
                     ),
            tabPanel("Evolution Map",
                     
                     column(7,
                     HTML("<h4>2000:2007</h4>"),
                     plotlyOutput("thematic1")%>% withSpinner(color="white"),
                     HTML("<h4>2008:2015</h4>"),
                     plotlyOutput("thematic2")%>% withSpinner(color="white"),
                     HTML("<h4>2016:2024</h4>"),
                     plotlyOutput("thematic3")%>% withSpinner(color="white")
                     ),
                     column(5,
                     HTML('<div id="evolutionMapText"><h1 id="evolutionTitle">Evolution Map</h1><br></div>')
                       )
            ),
            tabPanel("About", 
            HTML('<div id="about"><h1 id="aboutTitle">About</h1><br>
                                   <p id="aboutText">This text is an example of how the content will be displayed. 
                 This text is an example of how the content will be displayed. 
                 This text is an example of how the content will be displayed. 
                 This text is an example of how the content will be displayed. 
                 This text is an example of how the content will be displayed.</p></div>')
            )
          #   )
          # )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ####CRITERIA SERVER####
  
  output$criteriaPlot <- renderTable(relevantWords %>%
                                       rename(Word=Words) %>% 
                                       rename(Relevance=score) %>% 
                                       arrange(desc(Relevance)),
                                     width= "100%")
  
  ####MAP SERVER####
  
  output$instruction <- renderUI(HTML("<h3 id='myInstruction'>Click on any country to display more information</h3>"))
  
  observeEvent(input$leafdown_shape_click, { # update the location selectInput on map clicks
    
    hide(id="instruction")})
  
  observeEvent(input$continents, {
    
    if(input$continents=="Just Latin America"){
      countriesRelevance <- countriesRelevance  %>% 
        filter(continent== "South America" | subregion=="Central America")
      
      
      #maxScore <- max(countriesRelevance$totalScore, na.rm = TRUE)
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
            #mutate(percen=totalScore/maxScore*100) %>%
            mutate(text=paste0("<b>", Countries,"</b>", "<br>",
                               Productivity, " documents found")) %>% 
            filter(Countries==input$leafdown_shape_click$id) %>% 
            select(Countries, Productivity, text) %>% 
            distinct() %>% 
            ggplot(aes(Productivity, Countries, text=text))+
            geom_col(fill="#C44A17")+
            geom_col(aes(194485, Countries), color="white", fill= "transparent", show.legend = FALSE)+
            ggtitle("Productivity", 
                    subtitle = "The relevance score is the mean of the 5 most frequent relevant words")+
            ylab(NULL)+
            xlab(NULL)+
            scale_x_continuous(limits = c(0,194486), expand = c(0, 0)) +
            #xlim(0, 100)+
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
            #mutate(percen=totalScore/maxScore*100) %>%
            mutate(text=paste0("<b>", Countries,"</b>", "<br>",
                               "Relevance Score ", percen, "%")) %>% 
            filter(Countries==input$leafdown_shape_click$id) %>% 
            select(Countries, totalScore, percen, text) %>% 
            distinct() %>% 
            ggplot(aes(percen, Countries, text=text))+
            geom_col(fill="#C44A17")+
            geom_col(aes(99.9999, Countries), color="white", fill= "transparent", show.legend = FALSE)+
            ggtitle("Relevance Score", 
                    subtitle = "The relevance score is the mean of the 5 most frequent relevant words")+
            ylab(NULL)+
            xlab(NULL)+
            scale_x_continuous(limits = c(0,100), expand = c(0, 0)) +
            #xlim(0, 100)+
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
            ggplot(aes(percen, reorder(as_factor(word), percen), text=text,
                       group=percen))+
            geom_col(fill="#C44A17")+
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
        
        output$tableTitle <- renderText(
          "<h4 style='margin-left:20px; color:white;'>Most Relevant Authors</h4>"
        )
        
        output$authorsTable <- renderDataTable({
          topRelevantAuthors %>% 
            filter(Countries==input$leafdown_shape_click$id) %>% 
            select(Authors, Affiliation=Affiliations, Relevance=score) %>% 
            datatable(rownames = FALSE,
                      options = list(
                        info = FALSE,
                        paging = FALSE,
                        searching = FALSE
                      )) %>% 
            DT::formatStyle(columns = 1:3, color="white", fontSize = '75%')
        })
        
        # output$test <- renderPrint({
        #   p <- input$leafdown_shape_click
        # print(p)
        # })
      })
      
      #close just latin america
    }
    else{
      #maxScore <- max(countriesRelevance$totalScore, na.rm = TRUE)
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
            #mutate(percen=totalScore/maxScore*100) %>%
            mutate(text=paste0("<b>", Countries,"</b>", "<br>",
                               Productivity, " documents found")) %>% 
            filter(Countries==input$leafdown_shape_click$id) %>% 
            select(Countries, Productivity, text) %>% 
            distinct() %>% 
            ggplot(aes(Productivity, Countries, text=text))+
            geom_col(fill="#C44A17")+
            geom_col(aes(194485, Countries), color="white", fill= "transparent", show.legend = FALSE)+
            ggtitle("Productivity (log scale)", 
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
              title = element_text(colour = "#ffffff", size=10)
            )
          
          ggplotly(relevancePlot, tooltip = c("text")) %>% 
            config(displayModeBar = FALSE) %>% 
            layout(xaxis=list(fixedrange=TRUE)) %>% 
            layout(yaxis=list(fixedrange=TRUE))
        })
        
        output$relevanceScore <- renderPlotly({
          relevancePlot <- countriesRelevance %>% 
            #mutate(percen=totalScore/maxScore*100) %>%
            mutate(text=paste0("<b>", Countries,"</b>", "<br>",
                               "Relevance Score ", percen, "%")) %>% 
            filter(Countries==input$leafdown_shape_click$id) %>% 
            select(Countries, totalScore, percen, text) %>% 
            distinct() %>% 
            ggplot(aes(percen, Countries, text=text))+
            geom_col(fill="#C44A17")+
            geom_col(aes(99.999, Countries), color="white", fill= "transparent", show.legend = FALSE)+
            ggtitle("Relevance Score",
                    subtitle = "The relevance score is the mean of the 5 most frequent relevant words")+
            ylab(NULL)+
            xlab(NULL)+
            scale_x_continuous(limits = c(0,100), expand = c(0, 0)) +
            #xlim(0, 100)+
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
            layout(yaxis=list(fixedrange=TRUE)) #%>% 
            # layout(title = list(text = paste0('<h4>Relevance Score</h4>','<br>', 
            #                                   'The relevance score is the mean of the 5 most frequent relevant words')))
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
            ggplot(aes(percen, reorder(as_factor(word), percen), text=text,
                       group=percen))+
            geom_col(fill="#C44A17")+
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
        
        output$tableTitle <- renderText(
          "<h4 style='margin-left:20px; color:white;'>Most Relevant Authors</h4>"
        )
        
        output$authorsTable <- renderDataTable({
          topRelevantAuthors %>% 
            filter(Countries==input$leafdown_shape_click$id) %>% 
            select(Authors, Affiliation=Affiliations, Relevance=score) %>% 
            datatable(rownames = FALSE,
                      options = list(
                        info = FALSE,
                        paging = FALSE,
                        searching = FALSE
                      )) %>% 
            DT::formatStyle(columns = 1:3, color="white", fontSize = '75%')
        })
        
        # output$test <- renderPrint({
        #   p <- input$leafdown_shape_click
        # print(p)
        # })
      })}})
  
  ####RELEVANCE APP SERVER####
  output$distPlot <- renderPlotly(if (input$radio=="productivity"){
    plotScores <- data %>% 
      #filter(if (!is.null(input$countries)) Countries==input$countries else Countries%in%Countries) %>% 
      #filter(Countries %in% input$countries) %>% 
      filter(productivity>=input$productivity) %>% 
      filter(score>=input$relevance) %>% 
      mutate(scoreN=(score-min(score))/(max(score)-min(score))) %>% 
      mutate(productivityN=(productivity-min(productivity))/(max(productivity)-min(productivity))) %>% 
      mutate(text=paste0("<b>",Authors, "</b>", "\n", 
                         Affiliations, "\n",
                         Countries, "\n",
                         "Productivity: ", productivity, "\n",
                         "Total Citations: ", TC, "\n",
                         "Relevance: ", score)) %>% 
      ggplot(aes(scoreN, productivityN, color= Countries, text=text, key=Authors))+
      geom_jitter(alpha=0.6, height = 0, width = 0.005)+
      xlab("Normalized Relevance")+
      ylab("Normalized Productivity")+
      scale_x_continuous(minor_breaks = seq(0 , 1, 0.5), breaks = seq(0, 1, 0.5))+
      scale_y_continuous(minor_breaks = seq(0 , 1, 0.5), breaks = seq(0, 1, 0.5))+
      annotate("text", 
               x = c(0.1, 0.9, 0.1, 0.9), 
               y = c(0.4, 0.4, 0.6, 0.6), 
               colour = "white", size = 3,
               label = c("<i>low productivity\nlow relevance</i>", 
                         "<i>low productivity\nhigh relevance</i>", 
                         "<i>high productivity\nlow relevance</i>", 
                         "<i>high productivity\nhigh relevance</i>"),
               parse=TRUE)+
      theme(
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='#B0BAC3', color=NA),
        legend.background = element_rect(fill = "transparent"))
    
    ggplotly(plotScores, tooltip = "text")%>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
    
  } else {
    plotScores <- data %>% 
      #filter(if (!is.null(input$countries)) Countries==input$countries else Countries%in%Countries) %>% 
      #filter(Countries %in% input$countries) %>% 
      filter(TC>=input$citations) %>% 
      filter(score>=input$relevance) %>% 
      mutate(scoreN=(score-min(score))/(max(score)-min(score))) %>% 
      mutate(TCN=(TC-min(TC))/(max(TC)-min(TC))) %>% 
      mutate(text=paste0("<b>",Authors, "</b>", "\n", 
                         Affiliations, "\n",
                         Countries, "\n",
                         "Productivity: ", productivity, "\n",
                         "Total Citations: ", TC, "\n",
                         "Relevance: ", score)) %>% 
      ggplot(aes(scoreN, TCN, color= Countries, text=text, key=Authors))+
      geom_jitter(alpha=0.6, height = 0, width = 0.005)+
      xlab("Normalized Relevance")+
      ylab("Normalized Total Citations")+
      scale_x_continuous(minor_breaks = seq(0 , 1, 0.5), breaks = seq(0, 1, 0.5))+
      scale_y_continuous(minor_breaks = seq(0 , 1, 0.5), breaks = seq(0, 1, 0.5))+
      annotate("text", 
               x = c(0.1, 0.9, 0.1, 0.9), 
               y = c(0.4, 0.4, 0.6, 0.6), 
               label = c("<i>low productivity\nlow relevance</i>", 
                         "<i>low productivity\nhigh relevance</i>", 
                         "<i>high productivity\nlow relevance</i>", 
                         "<i>high productivity\nhigh relevance</i>"),
               parse=TRUE,
               colour = "white", size = 3)+
      theme(
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='#B0BAC3', color=NA),
        legend.background = element_rect(fill = "transparent"))
    
    ggplotly(plotScores, tooltip = "text")%>% 
      config(displayModeBar = FALSE)  %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
  }
  )
  
  output$summary <- renderPlotly({
    summaryPlot <- data %>% 
      filter(productivity>=input$productivity) %>% 
      filter(score>=input$relevance) %>%
      group_by(Countries) %>% 
      summarize(items=n()) %>% 
      ungroup() %>% 
      mutate(text=paste0(Countries, "\n", "Total documents: ", items)) %>% 
      ggplot(aes(items, reorder(as.factor(Countries), items), fill=Countries, text=text))+
      geom_col()+
      xlab(NULL)+
      ylab(NULL)+
      guides(fill=FALSE)+
      theme(plot.background = element_rect(fill = "#f5f5f5"),
            panel.background = element_rect(fill = "#f5f5f5"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x=element_blank(), 
            axis.ticks.x=element_blank())+
      theme(
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA))
    
    ggplotly(summaryPlot, tooltip = "text") %>% config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE)) 
  })
  
  hide("authorName")
  
  observeEvent(event_data("plotly_click")$key, {
    show("authorName")
    output$authorName <- renderUI(HTML(paste0("<H4 id='authorName'>Papers with the participation of <b>", 
                                              event_data("plotly_click")$key, "</b></H4>")))
  })
  
  output$worksTable = renderDataTable({
    papers %>% 
      filter(str_detect(Authors, paste0("\\b", event_data("plotly_click")$key, "\\b"))) %>% 
                        #event_data("plotly_click")$key)) %>% 
      arrange(desc(Score)) %>% 
      datatable(rownames = FALSE, escape = FALSE,
                options = list(searching = FALSE, pageLength = 25,lengthMenu = c(50, 100, 500, 1000), scrollX = T,
                               width="100%",autoWidth = TRUE)) %>% 
      DT::formatStyle(columns = 1:6, color="white", fontSize = '75%')
  })
  
  ####THEMATIC EVOLUTION SERVER####
  output$thematic1 <- renderPlotly(
  readRDS("data/ggplotly_11.RDS")
  )
  
  output$thematic2 <- renderPlotly(
    readRDS("data/ggplotly_22.RDS")
  )
  
  output$thematic3 <- renderPlotly(
    readRDS("data/ggplotly_33.RDS")
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
