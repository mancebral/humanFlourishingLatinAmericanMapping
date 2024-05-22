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

#load data
countriesRelevance <- readRDS("data/countriesRelevance.RDS")
topRelevantAuthors <- readRDS("data/topRelevantAuthors.RDS")
data = readRDS("data/Words3_Authors.rds")
papers = readRDS("data/papersScoredT.rds")
relevantWords = readRDS("data/relevantWords.rds")


countryColors <-
  setNames(terrain.colors(n=27), levels(data$Countries))

#crear paleta de color
ncolors <- 5
my_palette <- grDevices::colorRampPalette(rev(c("white","lightblue","blue","darkblue", 
                                                "black")))(ncolors)

noNAScores <- countriesRelevance %>% 
  filter(!is.na(totalScore))

myColors <- tibble(score=quantile(noNAScores$totalScore, prob=c(0,.25,.5,.75,1), type=1), 
                   color=my_palette[cut(quantile(noNAScores$totalScore, prob=c(0,.25,.5,.75,1), type=1),
                                        ncolors)])

ui <- fluidPage(
  
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
                    .well {
                    min-height: 20px;
                    padding: 19px;
                    margin-bottom: 20px;
                    background-color: transparent;
                      border: 0px solid transparent;
                    border-radius: 0px;
                    -webkit-box-shadow: inset 0 0px 0px rgb(0 0 0 / 5%);
                    box-shadow: inset 0 0px 0px rgb(0 0 0 / 5%);
                    }
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
                    #authorsTable {
                    margin-left: 10px;
                    }
                    .nav-tabs {
                    border-bottom: 0px solid #ddd;
                    margin-bottom: 10px;
                    background-color: transparent;
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
                }")),

        #start of the general panel
        wellPanel(
          tabsetPanel(
            
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
                            #HTML("Click on any country to display more information"),
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
                            plotlyOutput("relevanceScore", height = 120),
                            plotlyOutput("relevanceWords", height = 300),
                            htmlOutput("tableTitle"),
                            dataTableOutput("authorsTable")#,
                            #verbatimTextOutput("test")
                     )
                     ),
            
            ####AUTHORS####
            tabPanel("Authors", 
                     column(5,
                       prettyRadioButtons("radio", label = "Criteria of Popularity",
                                    choices = list("Productivity" = "productivity", "Total Citations" = "TC"), 
                                    selected = "productivity",shape="square", thick = TRUE),
                       conditionalPanel(
                         condition = "input.radio == 'productivity'",
                         sliderInput("productivity",
                                     "Grade of Productivity:",
                                     min = min(data$productivity),
                                     max = max(data$productivity),
                                     value = 10)
                       )
                       ,
                       conditionalPanel(
                         condition = "input.radio == 'TC'",
                         sliderInput("citations",
                                     "Grade of Total Citations:",
                                     min = min(data$TC),
                                     max = max(data$TC),
                                     value = 1000)
                       )
                       ,
                       sliderInput("relevance",
                                   "Grade of Relevance:",
                                   min = min(data$score),
                                   max = max(data$score),
                                   value = 100),
                       plotlyOutput("summary")
                     ),
                     
                     column(7,
                            HTML("<h3>Each point is an author, you can consult their works by clicking on the points.</h3>"),
                            plotlyOutput("distPlot"),
                            HTML("<br>"),
                            htmlOutput("authorName"),
                            dataTableOutput("worksTable")
                     )
                     ),
            tabPanel("About", 
            HTML('<div id="about"><h1 id="aboutTitle">About</h1><br>
                                   <p id="aboutText">This text is an example of how the content will be displayed. 
                 This text is an example of how the content will be displayed. 
                 This text is an example of how the content will be displayed. 
                 This text is an example of how the content will be displayed. 
                 This text is an example of how the content will be displayed.</p></div>'))
            )
          )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ####CRITERIA SERVER####
  
  output$criteriaPlot <- renderTable(relevantWords %>%
                                       rename(Word=Words) %>% 
                                       rename(Relevance=score) %>% 
                                       arrange(desc(Relevance)),
                                     width= "100%")
  # output$criteriaPlot <- renderPlotly({
  #   relevantWordsPlot <- relevantWords %>% 
  #     mutate(text=paste(Words, score, sep = " ")) %>% 
  #     ggplot(aes(score, reorder(as_factor(Words), score), text=text))+
  #     geom_col(fill="#C44A17")+
  #     ylab(NULL)+
  #     theme(
  #       panel.background = element_rect(fill='transparent'),
  #       plot.background = element_rect(fill='transparent', color=NA),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       title = element_text(colour = "#ffffff", size=10)
  #     )
    
    #ggplotly(relevantWordsPlot, tooltip = c("text"))%>% config(displayModeBar = FALSE)
    
  #})
  
  ####MAP SERVER####
  
  output$instruction <- renderUI(HTML("<h3 id='myInstruction'>Click on any country to display more information</h3>"))
  
  observeEvent(input$leafdown_shape_click, { # update the location selectInput on map clicks
    
    hide(id="instruction")})
  
  observeEvent(input$continents, {
    
    if(input$continents=="Just Latin America"){
      countriesRelevance <- countriesRelevance  %>% 
        filter(continent== "South America" | subregion=="Central America")
      
      maxScore <- max(countriesRelevance$totalScore, na.rm = TRUE)
      
      output$leafdown <- renderLeaflet({
        map <- leaflet(countriesRelevance$geom) %>% 
          #setView(lat = 30, lng = 18, zoom = 2) 
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
            label = paste(countriesRelevance$Countries,
                          countriesRelevance$totalScore, sep = " "),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto")
          ) %>% 
          addLegend(colors = myColors$color,
                    labels = myColors$score)
        
      })
      
      observeEvent(input$leafdown_shape_click, { # update the location selectInput on map clicks
        
        output$columnTitle <- renderPrint ({
          h2(input$leafdown_shape_click$id)
        })
        
        output$relevanceScore <- renderPlotly({
          relevancePlot <- countriesRelevance %>% 
            mutate(percen=totalScore/maxScore*100) %>%
            mutate(text=paste0("<b>", Countries,"</b>", "<br>",
                               " Relevance Score ", round(percen, 2), "%")) %>% 
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
            mutate(word=if_else(score>92, paste0(word, "      "), 
                                if_else(score>89, paste0(word, "  "), word))) %>% 
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
      maxScore <- max(countriesRelevance$totalScore, na.rm = TRUE)
      
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
            fillOpacity = 0.7,
            layerId = countriesRelevance$Countries,
            highlight = highlightOptions(
              weight = 4,
              color = "white",
              dashArray = "",
              fillOpacity = 1,
              bringToFront = TRUE),
            label = paste(countriesRelevance$Countries,
                          countriesRelevance$totalScore, sep = " "),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto")
          ) %>% 
          addLegend(colors = myColors$color,
                    labels = myColors$score)
        
      })
      
      observeEvent(input$leafdown_shape_click, { # update the location selectInput on map clicks
        
        output$columnTitle <- renderPrint ({
          h2(input$leafdown_shape_click$id)
        })
        
        output$relevanceScore <- renderPlotly({
          relevancePlot <- countriesRelevance %>% 
            mutate(percen=totalScore/maxScore*100) %>%
            mutate(text=paste0("<b>", Countries,"</b>", "<br>",
                               " Relevance Score ", round(percen, 2), "%")) %>% 
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
            mutate(word=if_else(percen>92, paste0(word, "      "), 
                                if_else(percen>89, paste0(word, "  "), word))) %>% 
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
      mutate(text=paste0(Authors, "\n", 
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
      mutate(text=paste0(Authors, "\n", 
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
      filter(str_detect(Authors, event_data("plotly_click")$key)) %>% 
      arrange(desc(Score)) %>% 
      datatable(rownames = FALSE, escape = FALSE,
                options = list(searching = FALSE, pageLength = 25,lengthMenu = c(50, 100, 500, 1000), scrollX = T,
                               width="100%",autoWidth = TRUE)) %>% 
      DT::formatStyle(columns = 1:6, color="white", fontSize = '75%')
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
