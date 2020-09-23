#############################
#CET412/CET522 2020 WINTER
#LEAH
#FINAL PROJECT
#LAST EDITED:3/16/2020
#############################

#------library----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(usethis)
library(devtools)
library(tidyverse)
library(readxl)
library(DT)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(ECharts2Shiny)

#------connect to SQL---------------------------------------------------------------
library(DBI)
library(odbc)
# build connnection
conn <- DBI::dbConnect(odbc::odbc(),
                       Driver   = "SQLServer",
                       Server   = "128.95.29.72",
                       Database = "Team08_W20",
                       UID      = "W20_Lu_Liu",
                       PWD      = "UAPwzvb5",
                       Port     = 1433)

#------Select all data from SQL-------------------
surveyQuery1 <- "SELECT * FROM [Locations$]"
Locations <- dbGetQuery(conn, surveyQuery1)
surveyQuery2 <- "SELECT * FROM [Counts$]"
Counts <- dbGetQuery(conn, surveyQuery2)
surveyQuery3 <- "SELECT * FROM [Surveys$]"
Surveys <- dbGetQuery(conn, surveyQuery3)

#initial_surveyData$timestamp <- as.character(as.POSIXct(initial_surveyData$timestamp, origin="1970-01-01", format="%d/%m/%Y %H:%M:%S"))

#------save data to database-------------------
saveCounts <- function(data) {
  print(data)
  table_columns <- dbListFields(conn, "Counts$")
  colnames(data) <- as.character(table_columns)
  dbWriteTable(conn, "Counts$", data, append = TRUE, overwrite = FALSE, row.names = FALSE)
}

saveSurveys <- function(data) {
  print(data)
  table_columns <- dbListFields(conn, "Surveys$")
  colnames(data) <- as.character(table_columns)
  dbWriteTable(conn, "Surveys$", data, append = TRUE, overwrite = FALSE, row.names = FALSE)
}


#------DATA processing------
Counts$Date2 <-as.Date(Counts$Date) 
Counts$sumvolume <- rowSums(Counts[, 7:18]) #sum volume for every location
results<-merge(x=Counts,y=Locations,by="LocationID",all.x=TRUE)  #combine the two tables together
plot1 <- aggregate(sumvolume ~ TimePeriod+Intersection, data = results, FUN = "max")
plot1 <- plot1[order(-plot1$sumvolume),] 

# List of choices for selectInput for intersections
choicesIntersection = data.frame(
  var = unique(results$Intersection),
  num = 1:length(unique(results$Intersection)))
Intersectionlist <- as.list(choicesIntersection$num)
names(Intersectionlist) <- choicesIntersection$var

choicesVonlunteer = data.frame(
  var = unique(Surveys$Volunteer),
  num = 1:length(unique(Surveys$Volunteer)))
Vonlunteerlist <- as.list(choicesVonlunteer$num)
names(Vonlunteerlist) <- choicesVonlunteer$var

# helmet aggregation
Helmet = results[,c("LocationID" ,'Intersection',"HelmetMale" ,"HelmetFemale" ,"NoHelmetMale" ,"NoHelmetFemale" ,"Weather" ,"TimePeriod" ,"Date2")]
Helmet  <- Helmet[order(Helmet$Date2),]
#Helmet$Female =  Helmet$HelmetFemale + Helmet$NoHelmetFemale
#Helmet$Male = Helmet$HelmetMale + Helmet$NoHelmetMale
#Helmet$Totol = Helmet$Female + Helmet$Male
Helmet$FemaleRatio = Helmet$HelmetFemale/(Helmet$HelmetFemale + Helmet$NoHelmetFemale)
Helmet$MaleRatio = Helmet$HelmetMale/(Helmet$HelmetMale + Helmet$NoHelmetMale)
Helmet$Ratio = (Helmet$HelmetMale+Helmet$HelmetFemale)/(Helmet$HelmetMale + Helmet$NoHelmetMale +Helmet$HelmetFemale + Helmet$NoHelmetFemale)
dataplotAllgender = aggregate(Helmet$Ratio,c(list(Helmet$Weather),list(Helmet$Date2), list(Helmet$TimePeriod)),mean)
dataplotMale = aggregate(Helmet$MaleRatio,c(list(Helmet$Weather),list(Helmet$Date2), list(Helmet$TimePeriod)),mean)
dataplotFemale = aggregate(Helmet$FemaleRatio,c(list(Helmet$Weather),list(Helmet$Date2), list(Helmet$TimePeriod)),mean)

##data for piechart
datapie  <- c(rep("Helmet", sum(Helmet$HelmetMale,Helmet$HelmetFemale)),
              rep("No Helmet", sum(Helmet$NoHelmetMale,Helmet$NoHelmetFemale)))



#------UI----------------------------------------------------------------
# UI
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}


ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Bicycle Count Program",
                                    dropdownMenu(type = "messages",
                                                 messageItem(
                                                   from = "Team 8 for Winter 2020",
                                                   message = "Lu Liu, Lu Yu, Roy Kim, Roberto Gomez"
                                                   #icon = icon("life-ring")
                                                 ))),
                    dashboardSidebar(
                      ##------sidebarMenu--------
                      sidebarMenu(
                        menuItem("Input", tabName = "Input", icon = icon("users"),
                                 menuSubItem("Rider Survey", tabName = "RS", icon = icon("angle-right")),
                                 menuSubItem("Count Survey", tabName = "CS", icon = icon("angle-right"))),
                        
                        menuItem("View", tabName = "View", icon = icon("dashboard"),
                                 menuSubItem("Rider Survey View", tabName = "RSV", icon = icon("angle-right")),
                                 menuSubItem("Count Survey View", tabName = "CSV", icon = icon("angle-right")),
                                 menuSubItem("Intersection class View", tabName = "IV", icon = icon("angle-right"))),
                        
                        menuItem("Analysis", tabName = "Analysis", icon = icon("th"),
                                 menuSubItem("Volume Analysis", tabName = "VA", icon = icon("angle-right")),
                                 menuSubItem("Helmet Analysis", tabName = "HA", icon = icon("angle-right")))
                      )
                    ),
                    ## Body content
                    dashboardBody(
                      tabItems(
                        #---------tabItems-------------------------------------------------------------
                        #---------Input--------First tab content--------------------------------------------
                        #---------Rider Survey--------------
                        tabItem(tabName = "RS",
                                # state using shinyjs based on Javascript
                                shinyjs::useShinyjs(),
                                fluidRow(
                                  tabBox( title = 'Rider Survey', width = 11,
                                          div(id = "form1",
                                              h1("Rider Survey"),
                                              h4("This section is devouted to individual cyclists who agree to answer a few detailed questions related
                            to the purpose of their trip."),
                                              box(solidHeader = TRUE,dateInput("Date1", labelMandatory("Date"),
                                                                               value = "2020-03-07"),
                                                  radioButtons("TimePeriod1", labelMandatory("Time"),
                                                               choices = list("7-9 AM" = '7-9am',
                                                                              "3-5 PM" = '3-5pm',
                                                                              "4-6 PM" = '4-6pm'),
                                                               selected = '7-9am'),
                                                  textInput("Volunteer1",labelMandatory("Volunteer Name"),
                                                            value = ""),
                                                  numericInput("LocationID1", "LocationID",2, 0, 300, 1)
                                              ),
                                              
                                              box("Trip Info",  solidHeader = TRUE,
                                                  textAreaInput("Purpose", "Trip purpose", height="50px"),
                                                  textAreaInput("Origin", "Trip origin",height="50px"),
                                                  textAreaInput("Destination", "Trip destination",height="50px"),
                                                  textAreaInput("Frequency", "Frequency of bicycle travel",height="50px")
                                              ),
                                              actionButton("submit1", "Submit", class = "btn-primary"),
                                              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                              br(),br(),br(),br(),br()
                                          ),
                                          shinyjs::hidden(
                                            # create an hidden div
                                            div(
                                              id = "thankyou_msg1",
                                              h3("Thanks, your response was submitted successfully!"),
                                              actionLink("submit_another1", "Submit another response")
                                            )
                                          )
                                  )
                                )
                        ),
                        #---------BICYCLE Survey-------------------------------------------------------------
                        tabItem(tabName = "CS",  # BICYCLE COUNT
                                # state using shinyjs based on Javascript
                                shinyjs::useShinyjs(),
                                fluidRow(
                                  tabBox( title = 'Count Survey', width = 11,
                                          div(id = "form2",
                                              tabPanel("Basic Info",  #solidHeader = TRUE,
                                                       h1("Count Survey"),
                                                       h4("This section is devoted to volunteers who stood at intersections to count how many bikers were observed in a
specific time period."),
                                                       box(solidHeader = TRUE,
                                                           numericInput("LocationID2", labelMandatory("Location:"),30, 0, 300, 1),
                                                           dateInput("Date2", labelMandatory("Date input"),
                                                                     value = "2020-03-07"),
                                                           radioButtons("TimePeriod2", labelMandatory("Time"),
                                                                        choices = list("7-9 AM" = '7-9am',
                                                                                       "3-5 PM" = '3-5pm',
                                                                                       "4-6 PM" = '4-6pm'),
                                                                        selected = '7-9am')),
                                                       box(solidHeader = TRUE,
                                                           selectInput("Weather2", "Weather",
                                                                       choices = list("cloudy, windy" = "cloudy, windy", "light rain" = "light rain",
                                                                                      "overcast" = "overcast", "overcast, no rain" = "overcast, no rain",
                                                                                      "rainning" = "rainning", "sunny and dry" = "sunny and dry" 
                                                                       ), selected ="sunny and dry" ),
                                                           sliderInput("Temperature2", "Temperature",
                                                                       min = 0, max = 100, value = 75),
                                                           textInput("Volunteer2","Volunteer Name",
                                                                     value = ""),
                                                           textInput("text","Notes",
                                                                     value = "")),
                                                       br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                                       br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                                       
                                              ),
                                              
                                              #-------Helmet Count---------------------------------------------------------
                                              tabPanel("Helmet Count",  #solidHeader = TRUE,
                                                       box( solidHeader = TRUE,
                                                            numericInput("MaleHel", "Male & Helmet", 30, 0, 1000, 1)),
                                                       box(solidHeader = TRUE,
                                                           numericInput("FemaleHel", "Female & Helmet", 30, 0, 1000, 1)
                                                       ),
                                                       box(solidHeader = TRUE,
                                                           numericInput("MaleNoHel", "Male & without Helmet", 30, 0, 1000, 1)),
                                                       box(solidHeader = TRUE,
                                                           numericInput("FemaleNoHel", "Female & without Helmet", 30, 0, 1000, 1)
                                                       ),
                                                       br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(), " ",
                                              ),
                                              
                                              #------Bicycle Count---------------------------------------------------------
                                              tabPanel("Bicycle Count",  #solidHeader = TRUE,
                                                       fluidRow( column(4, offset = 4 ,
                                                                        box(title = "North", solidHeader = TRUE, width = 16,
                                                                            numericInput("NL", "Turn left", 30, 0, 1000, 1),
                                                                            numericInput("NR", "Go straight", 30, 0, 1000, 1),
                                                                            numericInput("NT", "Turn right", 30, 0, 1000, 1),
                                                                        ) )),
                                                       fluidRow( column(4,
                                                                        box(title = "West",solidHeader = TRUE, width = 16,
                                                                            numericInput("WL", "Turn left", 30, 0, 1000, 1),
                                                                            numericInput("WR", "Go straight", 30, 0, 1000, 1),
                                                                            numericInput("WT", "Turn right", 30, 0, 1000, 1),
                                                                        ) ),
                                                                 column(4, offset = 4 ,
                                                                        box(title = "East",solidHeader = TRUE, width = 16,
                                                                            numericInput("EL", "Turn left", 30, 0, 1000, 1),
                                                                            numericInput("ER", "Go straight", 30, 0, 1000, 1),
                                                                            numericInput("ET", "Turn right", 30, 0, 1000, 1),
                                                                        ) )),
                                                       column(4, offset = 4 ,
                                                              box(title = "South",solidHeader = TRUE, width = 16,
                                                                  numericInput("SL", "Turn left", 30, 0, 1000, 1),
                                                                  numericInput("SR", "Go straight", 30, 0, 1000, 1),
                                                                  numericInput("ST", "Turn right", 30, 0, 1000, 1),
                                                                  actionButton("submit2", "Submit", class = "btn-primary")
                                                              ) ),
                                                       br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(), " ",
                                              )
                                          ),
                                          shinyjs::hidden(
                                            # create an hidden div
                                            div(
                                              id = "thankyou_msg2",
                                              h3("Thanks, your response was submitted successfully!"),
                                              actionLink("submit_another2", "Submit another response")
                                            )
                                          )
                                  )
                                )
                        ),
                        
                        
                        #---------View--------Second tab content--------------------------------------------
                        #------Rider survey view----
                        tabItem(tabName = "RSV",
                                fluidRow( box(dateInput("date11", "Date",
                                                        value = "2020-03-07"),
                                              selectInput("selectVolunteer", "Volunteer",
                                                          choices = Vonlunteerlist, selected = 1),
                                              radioButtons("checkGroup11", "Time Period",
                                                           choices = list("7-9 AM" = '7-9am',
                                                                          "3-5 PM" = '3-5pm',
                                                                          "4-6 PM" = '4-6pm')))),
                                fluidRow(actionButton(inputId = "refresh1", label = "Refresh ALL"),
                                         actionButton(inputId = "search1", label = "Search"),
                                         DT::dataTableOutput("tableSurveys"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
                        ),
                        #------Count survey view----
                        tabItem(tabName = "CSV",
                                fluidRow(box(dateInput("date12", "Date",
                                                       value = "2014-09-24"),
                                             selectInput("select3", "Location",
                                                         choices = Intersectionlist, selected = 68),
                                             radioButtons("checkGroup12", "Time",
                                                          choices = list("7-9 AM" = '7-9am',
                                                                         "3-5 PM" = '3-5pm',
                                                                         "4-6 PM" = "4-6pm")))),
                                fluidRow(actionButton(inputId = "refresh2", label = "Refresh ALL"),
                                         actionButton(inputId = "search2", label = "Search"),
                                         DT::dataTableOutput("tableCounts"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
                        ),
                        #------Intersection trafic class view----
                        tabItem(tabName = "IV",
                                h1("Intersection traffic class"),
                                h4("This section is devouted to show the traffic class of all intersections."),
                                leafletOutput(outputId = "mymap")
                        ),
                        
                        #---Visulization------Third tab content-------------------------------------------------------------------
                        #------Volume Analysis------------
                        tabItem(
                          tabName = "VA",
                          fluidRow(
                            tabBox( title = 'Volume Analysis', width = 11,
                                    #volunme analysis between different crossings
                                    tabPanel(title = "Overall trend",
                                             h4("This section is devouted to show the traffic volume of different intersections(top 15)."),
                                             radioButtons("checkGroupplot13", "Time",
                                                          c("AM" = 1,
                                                            "PM" = 2,
                                                            "All day" = 3),
                                                          selected = 3),
                                             plotOutput(outputId = "Plot1")),
                                    tabPanel(title = "Volume Comparison",
                                             h4("This section is devouted to show the traffic volume of different intersections at a specific date based on different time periods."),
                                             dateInput("dateplot2",
                                                       "Date",
                                                       value = "2014-05-20",
                                                       format = "yyyy-mm-dd"),
                                             plotOutput(outputId = "Plot2"),
                                             box(width = 12, solidHeader = TRUE,
                                                 DT::dataTableOutput("tablePlot2"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"),
                                             br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                    ),   #in 2014/5/20
                                    
                                    #volume analysis in a crossing
                                    tabPanel(title = "Volume Analysis",
                                             h4("This section is devouted to show the traffic volume at a designed intersection along with time."),
                                             selectInput("selectplot3", "Location",
                                                         choices = Intersectionlist, selected = 68),
                                             plotOutput(outputId = "Plot3")
                                    ),  #No.72
                                    
                                    tabPanel(title = "Volume with Weather",
                                             h4("This section is devouted to show the traffic volume over different weather at a designed intersection based on different time periods."),
                                             selectInput("selectplot4", "Location",
                                                         choices = Intersectionlist, selected = 68),
                                             plotOutput(outputId = "Plot4") ),       #No.72
                                    tabPanel(title = "Volume Map",
                                             h4("This section is devouted to show the traffic volume at a designed date"),
                                             dateInput("dateplot5", "Date",
                                                       value = "2014-05-20",
                                                       format = "yyyy-mm-dd"),
                                             leafletOutput(outputId = "mymap2"))
                            )
                          )
                        ),
                        #-------Helmet Analysis-----------
                        tabItem(
                          tabName = "HA",
                          fluidRow(
                            tabBox( title = 'Helmet Analysis', width = 11,
                                    tabPanel(title = "Date",
                                             h4("This section is devouted to show the helmet-wearing ratio trend along with time."),
                                             radioButtons("checkGroupplot6", "Gender",
                                                          c("Male" = 1,
                                                            "Female" = 2,
                                                            "All" = 3),
                                                          selected = 3),
                                             plotOutput(outputId = "Plot6"),
                                             #piechart
                                             loadEChartsLibrary(),
                                             br(),
                                             tags$div(id="pie1",style="width:400px; height:400px"),
                                             deliverChart(div_id = "pie1")),
                                    tabPanel(title = "Weather",
                                             h4("This section is devouted to show the relationship between the helmet-wearing ratio and weather by boxplot."),
                                             radioButtons("checkGroupplot7", "Gender",
                                                          c("Male" = "1",
                                                            "Female" = "2",
                                                            "All" = "3"),
                                                          selected = "3"),
                                             plotOutput(outputId = "Plot7"))
                            )
                          )
                        )
                      )
                    )
)

#---------server-------------------------------------------------------------
server <- function(input, output) {
  #----------define the header----------------------------------------
  fieldsCounts <- c("LocationID2","Volunteer2","Date2","Temperature2",       "Weather2",          
                    "TimePeriod2","NL","NR","NT","SL","SR","ST","EL","ER","ET","WL","WR","WT","MaleHel","FemaleHel",     
                    "MaleNoHel","FemaleNoHel")
  
  fieldsSurveys <- c("Volunteer1","Date1","TimePeriod1","LocationID1","Purpose","Origin","Destination","Frequency")
  
  #----------gather form data into a format----------  
  formCounts <- reactive({
    data2 <- sapply(fieldsCounts, function(x) input[[x]])
    #data <- c(data, timestamp = timestamp())
    data2 <- t(data2)
    data2 <- as.data.frame(data2)
    data2$Date2 <- as.Date(as.numeric(as.character(data2$Date2)),origin = "1970-01-01")
    data2
  })
  
  formSurveys <- reactive({
    data1 <- sapply(fieldsSurveys, function(x) input[[x]])
    #data <- c(data, timestamp = timestamp())
    data1 <- t(data1)
    data1 <- as.data.frame(data1)
    data1$Date1 <- as.Date(as.numeric(as.character(data1$Date1)),origin = "1970-01-01")
    data1
  })
  
  # action to take when submit button is pressed
  observeEvent(input$submit1, {
    saveSurveys(formSurveys())
    shinyjs::reset("form1")
    shinyjs::hide("form1")
    shinyjs::show("thankyou_msg1")
  })
  
  observeEvent(input$submit2, {
    saveCounts(formCounts())
    shinyjs::reset("form2")
    shinyjs::hide("form2")
    shinyjs::show("thankyou_msg2")
  })
  
  
  # action to take when submit_another button is pressed
  observeEvent(input$submit_another1, {
    shinyjs::show("form1")
    shinyjs::hide("thankyou_msg1")
  })    
  observeEvent(input$submit_another2, {
    shinyjs::show("form2")
    shinyjs::hide("thankyou_msg2")
  }) 
  
  #----------show the data in the view part----------  
  # initialize the tableData as the query result that stored in initial_surveyData
  rvSurveys <- reactiveValues(tableData = Surveys)
  
  output$tableSurveys <- renderDataTable(rvSurveys$tableData,
                                         options = list(paging = FALSE))
  
  observeEvent(input$refresh1, {
    # Select data from Surveys by condition
    surveyQuery3 <- "SELECT * FROM [Surveys$]"
    Surveys <- dbGetQuery(conn, surveyQuery3)
    rvSurveys$tableData <- Surveys 
  })
  
  observeEvent(input$search1, {
    surveyQuery3 <- "SELECT * FROM [Surveys$]"
    Surveys <- dbGetQuery(conn, surveyQuery3)
    rvSurveys$tableData <- filter(Surveys, VolunteerName == choicesVonlunteer$var[choicesVonlunteer$num == input$selectVolunteer] & 
                                    TimePeriod == toString(input$checkGroup11) &
                                    Date == input$date11)
  })
  # initialize the tableData as the query result that stored in initial_surveyData
  rvCounts <- reactiveValues(tableData = Counts)
  
  output$tableCounts <- renderDataTable(rvCounts$tableData,
                                        options = list(paging = FALSE))
  
  
  observeEvent(input$refresh2, {
    # Select all data from Counts
    surveyQuery2 <- "SELECT * FROM [Counts$]"
    Counts <- dbGetQuery(conn, surveyQuery2)
    rvCounts$tableData <- Counts
  })
  
  
  observeEvent(input$search2, {
    surveyQuery2 <- "SELECT * FROM [Counts$]"
    Counts <- dbGetQuery(conn, surveyQuery2)
    rvCounts$tableData <- filter(Counts, LocationID == Locations$LocationID[Locations$Intersection == choicesIntersection$var[choicesIntersection$num == input$select3]] & 
                                   TimePeriod == toString(input$checkGroup12) &
                                   Date == input$date12)
    rvCounts$tableData
  })
  
  
  
  
  
  #---plot of intersection class----------------------------------------------------------------------
  #define the color of for the traffic class of the intersections
  pal2 <- colorFactor(
    palette = c('blue', 'green', 'red'),
    domain = Locations$TrafficClass
  )
  #create the map
  output$mymap <- renderLeaflet({
    leaflet(Locations) %>%
      setView(lng = -122.6, lat = 45.5, zoom = 11)  %>% #setting the view over ~ center of North America
      addTiles() %>%
      addCircles(data = Locations, lat = Locations$Latitude, lng = Locations$Longitude, radius = 10, opacity = 1, color = ~pal2(TrafficClass)) %>%
      addMarkers(popup=~Intersection)  %>%
      addLegend(pal = pal2, values = ~TrafficClass, group = "circles", position = "bottomleft")
  })
  
  #---plot1----------------------------------------------------------------------
  output$Plot1 <- renderPlot({
    x <- 15
    if (input$checkGroupplot13 == 1) {
      dataplot1 = plot1 %>% filter(plot1$TimePeriod == '7-9am')
    } else if (input$checkGroupplot13 == 2) {
      dataplot1 = plot1 %>% filter(plot1$TimePeriod == '4-6pm')
    }  else (
      dataplot1 = plot1
    )
    ggplot(data = dataplot1[1:x,] ,
           aes(x = reorder(dataplot1[1:x,]$Intersection, dataplot1[1:x,]$sumvolume) ,
               y = dataplot1[1:x,]$sumvolume)) +
      geom_bar(stat = "identity",
               color = "black",
               alpha = 0.8) +
      scale_fill_continuous(high = "#8c8c8c", low = "#cccccc") +
      labs(x = NULL, y = "Bicycle Count") +
      guides(fill = "none") +
      theme_minimal() +
      coord_flip() +
      ggtitle("Total Count in All Directions") +
      geom_text(aes(label=sumvolume), hjust = -0.5, size = 3,
                position = position_dodge(width = 1),
                inherit.aes = TRUE) +
      theme(plot.title = element_text(face = "bold", size = 12))
  })
  
  #---plot2----------------------------------------------------------------------
  output$Plot2 <- renderPlot({
    dataplot2 = results %>% filter(results$Date2 == as.Date(input$dateplot2))
    ggplot(data = dataplot2,
           aes(x = Intersection,
               y = sumvolume,
               fill  = TimePeriod
               #,fill = TimePeriod
           )) + coord_flip() +
      ggtitle("Total Count in All Directions") +
      geom_text(aes(label=sumvolume), hjust = -0.5, size = 3,
                position = position_dodge(width = 1),
                inherit.aes = TRUE) +
      geom_bar(stat = "identity",
               color = "black",
               alpha = 0.8)
    #}
    #else{}
  })
  output$tablePlot2 <- renderDataTable(results[results$Date2 == as.Date(input$dateplot2),c('Intersection',"TimePeriod","NorthBoundLeft","NorthBoundRight","NorthBoundThrough","SouthBoundLeft","SouthBoundRight","SouthBoundThrough","EastBoundLeft","EastBoundRight","EastBoundThrough","WestBoundLeft","WestBoundRight","WestBoundThrough")],
                                       options = list(paging = FALSE))
  
  #---plot3----------------------------------------------------------------------
  output$Plot3 <- renderPlot({
    dataplot3 = results %>% filter(results$Intersection == choicesIntersection$var[choicesIntersection$num == input$selectplot3])
    dataplot3 <- dataplot3[order(dataplot3$Date2),]
    ggplot(data = dataplot3, mapping = aes(x = Date2, y = sumvolume, colour =  TimePeriod)) + geom_line() + xlab('Date') + ylab('Bicycle count') + geom_point()+
      geom_text(aes(label = sumvolume), size = 5, vjust = -1, hjust = .5, position = position_dodge(0.9))
  })
  
  #---plot4----------------------------------------------------------------------
  output$Plot4 <- renderPlot({
    dataplot4 = results %>% filter(results$Intersection == choicesIntersection$var[choicesIntersection$num == input$selectplot4])
    dataplot4 = aggregate(dataplot4$sumvolume,c(list(dataplot4$Weather),list(dataplot4$TimePeriod)),mean)
    ggplot(dataplot4, aes(x = Group.1, y = x ,fill = Group.2))+ geom_bar(stat = "identity", position = 'dodge')+ xlab('Weather') + ylab('Bicycle count')  +
      geom_text(aes(label = x), size = 5, vjust = -0.5, hjust = .5, position = position_dodge(0.9))
  })
  #---plot5----------------------------------------------------------------------
  output$mymap2 <- renderLeaflet({
    dataplot5 = results %>% filter(results$Date2 == as.Date(input$dateplot5))
    leaflet(dataplot5) %>%
      setView(lng = -122.6, lat = 45.5, zoom = 11)  %>% #setting the view over ~ center of North America
      addTiles() %>%
      addCircles(data = dataplot5, lat = dataplot5$Latitude, lng = dataplot5$Longitude, radius = 10, opacity = 0.5, color="red",weight=dataplot5$sumvolume/18)%>%
      addMarkers(popup=~Intersection)
  })
  #---plot6----------------------------------------------------------------------
  output$Plot6 <- renderPlot({
    if (input$checkGroupplot6 == 3) {
      data = dataplotAllgender
    } else if (input$checkGroupplot6 == 1) {
      data = dataplotMale
    }  else (
      data = dataplotFemale
    )
    p <- ggplot(data = data, mapping = aes(x = Group.2, y = x, colour =  Group.3)) + geom_line() + xlab('Date') + ylab('Helmet Ratio')
    p <- p + labs(color='Time Period')
    p
  })
  
  renderPieChart(div_id = "pie1", data = datapie)
  #---plot7----------------------------------------------------------------------
  rv <- reactiveValues(data = dataplotAllgender, test = 1)
  
  observeEvent(input$checkGroupplot7,{
    if(input$checkGroupplot7=="3"){
      rv$data <- dataplotAllgender}
    if(input$checkGroupplot7=="1"){
      rv$data <- dataplotMale}
    if(input$checkGroupplot7=="2"){
      rv$data <- dataplotFemale}
  })
  
  output$Plot7 <- renderPlot({
    p <- ggplot(data = rv$data, mapping = aes(x = Group.1, y = x, colour =  Group.3)) + geom_boxplot() + xlab('Weather') + ylab('Helmet Ratio')
    p <- p + labs(color='Time Period')
    p
  })
}

#-------shinyApp---------------------------------------------------------------
shinyApp(ui = ui, server = server)
