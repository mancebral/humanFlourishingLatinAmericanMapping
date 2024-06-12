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
relevantWords = readRDS("data/relevantWords.rds")
data = readRDS("data/topWordsAuthors.rds")
papers = readRDS("data/scoredPapers.rds")
institutions = readRDS("data/scoredInstitutions.rds")
papersByCountry = readRDS("data/papersByCountry.rds")
TopPapers = readRDS("data/100TopPapers.rds")
LAcountries = readRDS("data/LAcountries.rds")
typesData = readRDS("data/typesData.rds")


#crear paleta de color
my_palette <- grDevices::colorRampPalette(rev(c("#162F43","#3B4952","#55636D","#6D7E8B", 
                                                "#8192A0")))(11)
my_paletteL <- grDevices::colorRampPalette(rev(c("#162F43","#3B4952","#55636D","#6D7E8B", 
                                                 "#8192A0")))(5)

####USER INTERFACE####
ui <- tagList(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  useShinyjs(),
  
  setBackgroundColor(color = "#8192A0"),
  
  # Footer
  tags$footer(
    style = "position: fixed; bottom: 0; z-index: 9999;
    width: 100%; background-color: #8192A0; color: white;
    padding-left: 10px; padding-right: 10px; padding-top: 10px; padding-bottom: 10px;
    text-align: center; font-size: 10px;",
    HTML("This site has been conceived by the Human Flourishing Team of the Tec de Monterrey and 
    developed in R Programming Shiny Apps with the support of Templeton World Charity Foundation. 
                     More info about <a href='https://tec.mx/es/florecimiento-humano/entorno-para-florecer/mapeo-de-florecimiento-humano-en-latinoamerica' 
                     target='_blank'>Landscaping Regional Research and Leadership Capacities for the 
         Study of Human Flourishing in Mexico, Colombia, Chile, and Brazil</a>."
    )),

        # #start of the general panel
        # wellPanel(
        #   tabsetPanel(id = "myMenu",
  
  navbarPage(collapsible = TRUE,

            ####INTRO TAB####
            tabPanel("Intro"
            # HTML('<div id="frontPage"><h1 id="myTitle">Human Flourishing<br>in Latin America</h1><br>
            #                         <p id="subtitle">Recognizing the regional research landscape and leadership capabilities
            #                         in the study of Human Flourishing in Mexico, Colombia, Chile and Brazil.</p></div>')
            ),
            
            tabPanel("Home", 
                     HTML('<div id="frontPage"><h1 class="myTitle">Human Flourishing<br>in Latin America</h1><br>
                                             <p id="subtitle">Recognizing the regional research landscape and leadership capabilities
                                             in the study of Human Flourishing in Mexico, Colombia, Chile and Brazil.</p>
                                             <img id="logo1" src="logoTec2.png"><img id="logo2" src="https://www.templeton.org/wp-content/uploads/2022/01/JTF_logo_wtagline.png">
                          </div>')
            ),
            
            ####DATA TAB####
            tabPanel("Data", 
                     htmlOutput("dataInfo"),
                     column(6, 
                            plotlyOutput("typesPlot", height = "300px")%>% withSpinner(color="white")
                            ),
                     column(6,
                            plotlyOutput("papersYear", height = "300px")%>% withSpinner(color="white"),
                            HTML("<br><br>")
                            ),
                     column(4,
                            plotlyOutput("dataInfo1")%>% withSpinner(color="white"),
                            HTML("<br>"),
                            htmlOutput("dataInfo1.1"),
                            card(
                              #card_header(HTML("<h3>RELEVANCE SCORE TABLE</h3>")),
                              height = 300,
                              style = "resize:vertical;",
                              card_body(
                                min_height = 300,
                                tableOutput("dataInfo1.2")%>% withSpinner(color="white")
                              ))%>% tagAppendAttributes(class="infoTable")
                            ),
                     column(4,
                            plotlyOutput("dataInfo2")%>% withSpinner(color="white"),
                            HTML("<br>"),
                            htmlOutput("dataInfo2.1"),
                            card(
                              #card_header(HTML("<h3>RELEVANCE SCORE TABLE</h3>")),
                              height = 300,
                              style = "resize:vertical;",
                              card_body(
                                min_height = 300,
                                tableOutput("dataInfo2.2")%>% withSpinner(color="white")
                              ))%>% tagAppendAttributes(class="infoTable")
                            ),
                     column(4,
                            plotlyOutput("dataInfo3")%>% withSpinner(color="white"),
                            HTML("<br>"),
                            htmlOutput("dataInfo3.1"),
                            card(
                              #card_header(HTML("<h3>RELEVANCE SCORE TABLE</h3>")),
                              height = 300,
                              style = "resize:vertical;",
                              card_body(
                                min_height = 300,
                                tableOutput("dataInfo3.2")%>% withSpinner(color="white")
                              ))%>% tagAppendAttributes(class="infoTable")
                     )
            ),
            
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
                                tableOutput("criteriaPlot")%>% withSpinner(color="white")
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
                            card(
                              #card_header(HTML("<h3>RELEVANCE SCORE TABLE</h3>")),
                              height = 300,
                              style = "resize:vertical;",
                              card_body(
                                min_height = 300,
                                dataTableOutput("authorsTable", width = "98%")
                              ))
                            #,
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
                                     value = 10,
                                     width = "100%")
                       ),
                       conditionalPanel(
                         condition = "input.radio == 'TC'",
                         sliderTextInput("citations",
                                     "Grade of Total Citations:",
                                     choices = c(4,8,15,30,60,125,250,500,1000,2000,4000,8000, 16000),
                                     selected = 1000,
                                     grid = TRUE,
                                     width = "100%")
                       ),
                       sliderInput("relevance",
                                   "Grade of Relevance:",
                                   min = min(data$score),
                                   max = max(data$score),
                                   value = 100,
                                   width = "100%"),
                       HTML("<br><br><br>"),
                       textInput("nameAuthor", "You can also find works of an author directly filtering by his/her name",
                                placeholder="Enter last name of an author to search", width = "100%"),
                       actionButton("searchAuthor", "search!"),
                       HTML("<br>"),
                       HTML("<br>"),
                       radioGroupButtons("links", 
                                         label=NULL, 
                                         choices = c(1), 
                                         direction = "vertical")
                       #selectInput("links", label=NULL, choices = c(1))
                       #plotlyOutput("summary")
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
            tabPanel("About", 
            HTML('<div id="about"><h1 id="aboutTitle">About</h1><br>
                                   <p id="aboutText">This text is an example of how the content will be displayed. 
                 This text is an example of how the content will be displayed. 
                 This text is an example of how the content will be displayed. 
                 This text is an example of how the content will be displayed. 
                 This text is an example of how the content will be displayed.</p></div>')
            ), id= "MainNavBar"
          #   )
     )
  )

# Define server logic required to draw a histogram
server <- function(session, input, output) {
  
  hide("links")
  
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
  # output$frontPage <- renderUI(HTML('<div><h1 id="myTitle">Human Flourishing<br>in Latin America</h1><br>
  #                                   <p id="subtitle">Recognizing the regional research landscape and leadership capabilities
  #                                   in the study of Human Flourishing in Mexico, Colombia, Chile and Brazil.</p>
  #                         </div>'))
  # 
  # 
  ####DATA SERVER####
  output$dataInfo <- renderUI(HTML("The information available on the current site was obtained from two huges 
  bibliographic databases: openAlex and Scopus. More than 300 thousand of academic document references were downloaded 
  following a search which was the result of a combination of queries. Due to human flourishing is not a very common 
  term in academic research, and even less in Latinamerican countries, we decided to use such strategy, which is explained in 
  the Criteria Tab. The data was obtained in December 2023.<br><br><br>"))
  
  output$typesPlot <- renderPlotly({
    
    typesPlotly <- typesData %>% 
      ggplot(aes(Document, n, fill=Type, text=text))+
      geom_col()+
      coord_flip()+
      xlab(NULL)+
      ylab(NULL)+
      ggtitle("Types of documents")+
      theme(
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill="transparent", color=NA),
        legend.background = element_rect(fill = "transparent"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
    
    ggplotly(typesPlotly, tooltip = c("text")) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
  })
    
    output$papersYear <- renderPlotly({
    papersPlot <- papers %>% 
      filter(Year<2024) %>% 
      group_by(Year) %>% 
      mutate(n=n()) %>% 
      mutate(TC=sum(Citations)) %>% 
      ungroup() %>% 
      select(Year, n, TC) %>% 
      distinct() %>% 
      arrange(Year) %>% 
      mutate(text=paste0("Year ", Year, "\n", 
                         n, " documents", "\n",
                         TC, " Citations")) %>% 
      ggplot(aes(Year, n, text=text, group = 1))+
      geom_point(aes(size = TC))+
      geom_path()+
      ylab(NULL)+
      xlab(NULL)+
      ggtitle("Documents by year")+
      theme(
        panel.background = element_rect(fill='#B0BAC3'),
        plot.background = element_rect(fill="transparent", color=NA),
        legend.background = element_rect(fill = "transparent"))
    
    ggplotly(papersPlot, tooltip = c("text")) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
    
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
    HTML(paste0("<h4 align='center'>Top 100 Authors ", plotlyEvent1()$key, "</h4>" ))})
  
  output$dataInfo1.2 <- renderTable({
    if(is.null(plotlyEvent1()$key)){
    data %>% 
      select(Authors, productivity) %>% 
      slice_max(order_by = productivity, n = 100)
      } else {
        data %>% 
          filter(Countries==plotlyEvent1()$key) %>% 
          select(Authors, productivity) %>% 
          slice_max(order_by = productivity, n = 100)
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
    HTML(paste0("<h4 align='center'>Top 100 Institutions ", plotlyEvent2()$key, "</h4>" ))})
  
  output$dataInfo2.2 <- renderTable({
    if(is.null(plotlyEvent2()$key)){
      data %>% 
        count(Affiliations, sort = TRUE) %>% 
        slice_max(order_by = n, n = 100)} else {
          data %>% 
            filter(Countries==plotlyEvent2()$key) %>% 
            count(Affiliations, sort = TRUE) %>% 
            slice_max(order_by = n, n = 100)
        }
  },
  width = "100%")
  
  output$dataInfo3 <- renderPlotly({
    
    papersByCountry %>% 
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
    HTML(paste0("<h4 align='center'>Top 100 Papers ", plotlyEvent3()$key, "</h4>" ))})
  
  output$dataInfo3.2 <- renderTable({
    if(is.null(plotlyEvent3()$key)){
      TopPapers %>% 
        slice_max(order_by = Citations, n = 100) %>% 
        select(Title, Citations)} else {
          TopPapers %>% 
            filter(Countries==plotlyEvent3()$key) %>% 
            slice_max(order_by = Citations, n = 100) %>% 
            select(Title, Citations)
          }},
    width= "100%")
  
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
  
  ####RELEVANCE APP SERVER####
  output$distPlot <- renderPlotly(if (input$radio=="productivity"){
    plotScores <- data %>% 
      filter(productivity>=input$productivity) %>% 
      filter(score>=input$relevance) %>% 
      mutate(scoreN=(score-min(score))/(max(score)-min(score))) %>% 
      mutate(productivityN=(productivity-min(productivity))/(max(productivity)-min(productivity))) %>% 
      mutate(text=paste0("<b>",Authors, "</b>", "\n", 
                         #Affiliations, "\n",
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
    
    ggplotly(plotScores, source = "authorsTab", tooltip = "text")%>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
    
  } else {
    plotScores <- data %>% 
      filter(TC>=input$citations) %>% 
      filter(score>=input$relevance) %>% 
      mutate(scoreN=(score-min(score))/(max(score)-min(score))) %>% 
      mutate(TCN=(TC-min(TC))/(max(TC)-min(TC))) %>% 
      mutate(text=paste0("<b>",Authors, "</b>", "\n", 
                         #Affiliations, "\n",
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
    
    ggplotly(plotScores, source = "authorsTab",  tooltip = "text")%>% 
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
  
  observeEvent(plotlyEvent4()$key, {
    show("authorName")
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
      arrange(desc(Score)) %>% 
      datatable(rownames = FALSE, escape = FALSE,
                options = list(searching = FALSE, pageLength = 25,lengthMenu = c(50, 100, 500, 1000), scrollX = T,
                               width="100%",autoWidth = TRUE)) %>% 
      DT::formatStyle(columns = 1:6, color="white", fontSize = '75%')
  })
  })
  
  #search author
  # myAuthors2 <- unique(papers %>% separate_longer_delim(Authors, delim = ";") %>% 
  #          pull(Authors))
  
  # updateSelectizeInput(session, inputId = "links", label = "direct author",
  #                      choices = myAuthors2, selected = NULL)
  
  observeEvent(input$searchAuthor,{
    
    myAuthors <- papers %>% 
      select(Authors) %>% 
      separate_longer_delim(Authors, delim = ";") %>% 
      distinct() %>% 
      pull(Authors)
    
    patterns <- strsplit(toupper(input$nameAuthor)," ")
    
    values <- mapply(function(word,string) all(str_detect(word,string)),
           myAuthors,
           patterns)
    
    myAuthors <- myAuthors[values]
    
    
    # myAuthors <- papers %>% 
    #   filter(str_detect(Authors, paste0("\\b",toupper(isolate(input$nameAuthor)), "\\b"))) %>%
    #   separate_longer_delim(Authors, delim = ";") %>% 
    #   filter(str_detect(Authors, paste0("\\b",toupper(isolate(input$nameAuthor)), "\\b"))) %>%
    #   select(Authors) %>% 
    #   distinct() %>% 
    #   pull(Authors)
    
    updateRadioGroupButtons(
      inputId = "links",
      label= "Which author do you refer?",
      choices=myAuthors)
    
    show("links", time = 2)
    
      observeEvent(input$links,{
      
      show("authorName")
    
        observe(
        tryCatch({
          
          output$worksTable = renderDataTable({
            papers %>% 
              filter(str_detect(Authors, paste0("\\b",toupper(isolate(input$links)), "\\b"))) %>% 
              #event_data("plotly_click")$key)) %>% 
              arrange(desc(Score)) %>% 
              datatable(rownames = FALSE, escape = FALSE,
                  options = list(searching = FALSE, pageLength = 25,lengthMenu = c(50, 100, 500, 1000), scrollX = T,
                                 width="100%",autoWidth = TRUE)) %>% 
              DT::formatStyle(columns = 1:6, color="white", fontSize = '75%')
    })
    
    institutions <- data %>% filter(Authors==input$links) %>% 
      mutate(Affiliations=gsub(";", " | ", Affiliations)) %>%
      select(Affiliations) %>% distinct() %>% 
      pull(Affiliations)
    
    output$authorName <- renderUI(HTML(paste0("<H4 id='authorName'>Papers with the participation of <b>",
                                              isolate(input$links), "</b></H4><br>", institutions, "<br><br>")))
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')
    },
    warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    }))
    })
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
