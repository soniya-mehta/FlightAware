library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyquant) 
library(tm) # text mining
library(twitteR)
library(wordcloud)
library(rtweet)
library(stringr)
library(reshape)
library(magrittr)
library(leaflet)
library(googleway)
library(lubridate)

#loading dataset + misc adjustments to dataset [Same as in Server]----------------------------------
delaydata <- read.csv("final_delay.csv")
#5% sample of dataset taken for leaflet as the original takes too long to load for leaflet visualisation
df <- read.csv("final_5%.csv")
delaydata$ORIGIN <- as.character(delaydata$ORIGIN)
delaydata$DEST <- as.character(delaydata$DEST)
#creating airport names
airport <- data.frame(code = as.character(delaydata$ORIGIN), name = as.character(delaydata$Airport.name))
extra <- data.frame(code = c("IFP", "LWB" ,"SHD" ,"CNY", "WYS", "GST", "TXK", "BKG" ,"PRC" ,"EAR", "CYS"), name = c("Laughlin/Bullhead International Airport", "Greenbrier Valley Airport", "Shenandoah Valley Regional Airport", "Canyonlands Field Airport", "Yellowstone Airport", "Gustavus Airport-Gst", "Texarkana Regional Airport", "Branson Airport", "Prescott Regional Airport", "Kearney Regional", "Cheyenne Regional Airport"))
airport <- rbind(airport, extra)
airport <- distinct(airport)

delaydata$ORIGIN <- as.character(delaydata$ORIGIN)
delaydata$DEST <- as.character(delaydata$DEST)
delaydata$FL_DATE <- as.Date(delaydata$FL_DATE)

#Header of dashboard ----------------------------------------------------------------------------
header <- dashboardHeader(
  titleWidth = 250,
  title = "FlightAware",
  # facebook link------------------------------------------------------------------------------- 
  tags$li(class = "dropdown",
          tags$a(href = "https://www.facebook.com/FlightAware/",
                 tags$img(height = "20px", src = "facebook.png")
          )
  ),
  # instagram link-------------------------------------------------------------------------------
  tags$li(class = "dropdown",
          tags$a(href = "https://www.instagram.com/flightaware/",
                 tags$img(height = "20px", src = "instagram.png")
          )
  ),
  # twitter link-------------------------------------------------------------------------------
  tags$li(class = "dropdown",
          tags$a(href = "https://twitter.com/flightaware",
                 tags$img(height = "20px", src = "twitter.png")
          )
  )

)
# end of header-------------------------------------------------------------------------------

#sidebar -------------------------------------------------------------------------------------
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    id = "menu",
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem(
      dateRangeInput("dateRange", label = "Date range:", 
                     start = Sys.Date()-2, end = Sys.Date()+2)),
    menuItem(
      selectInput("from", label = "From", 
                  choices = c(unique(as.character(airport$name))), 
                  multiple = FALSE, selected = "Chicago O'Hare International")),
    menuItem(
      selectInput("to", label = "To", 
                  choices = c(unique(as.character(airport$name))), 
                  multiple = FALSE, selected = "Orlando International" )),
    menuItem("Features",tabName = "feature", icon = icon("plus-square"), startExpanded = TRUE,
             menuSubItem("Flights", tabName = "flights", icon = icon("plane-departure")),
             menuSubItem("Statistics", tabName = "stats", icon = icon("chart-line")),
      menuSubItem("Alternative transport", tabName = "transport", icon = icon("car"))),
    menuItem("Trending", tabName = "trending", icon = icon("twitter"), badgeLabel = "New!", badgeColor = "green"),
    menuItem("About", tabName = "about", icon = icon("info"))
  )
)
# end of sidebar-------------------------------------------------------------------------------

# Body ---------------------------------------------------------------------------------------- 
body <- dashboardBody(
  tabItems(
    # Home screen-------------------------------------------------------------------------------
    tabItem(tabName = "home",
            p(""),
            box(status = "primary", width = 12,
                tags$p(class = "text-center",
                       tags$img(src = "FlightAware_logo.png", width = 200, align = "center")),
                h2("Welcome to FlightAware", align = "center"),
                p("Feeling overwhelmed with travel planning, unexpected flight delays, safety, and the financial strains of travelling?"),
                p(""),
                p("Created by a group of NUS Business students, FlightAware is an application like no other. We have multiple features to simplify your travel experience and explore more on your destination while keeping track of your flights."),
                p(""),
                p(" Many people travel as a way to relieve stress, but for some people travel can also induce travel stress. FlightAware is here to keep you unforgetable vacation while taking the stress out of it!"),
                p(""),
                h3("Start you journey with us today!", align = "center"),
                p(""),
                
                actionButton("button2", "Statistics",icon("chart-line"),
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
   
                actionButton("button3", "Flights", icon("plane-departure"),
                             style= "color: #fff; background-color: #4876FF ; border-color: #1E90FF"),
          
                actionButton("button4", "Alternative transport",icon("car"),
                             style = "color: #fff; background-color: #32CD32  ; border-color: #ADFF2F"),
      
                actionButton("button5", "Trending", icon("twitter"),
                             style = "color: #fff; background-color: #FF8247 ; border-color: #FF8C69"),

                p(""),
                p("*App best viewed in full window browser."),
                tags$p(class = "text-center",
                       tags$a(
                         target = "_blank",
                         tags$img(class = "image-responsive",
                                  src = "coverpage.webp",
                                  width = "100%"))
                       )
                )
    ),
    # Flights screen-------------------------------------------------------------------------------
    tabItem(tabName = "flights",
            h2("Flights"),
            fluidRow(box(tableOutput("table"),width=8),
                     box(title = "RoamRight Insurance",
                         width = 4,
                         tags$p(class = "text-center",
                                tags$img(class = "img-responsive img-rounded center-block",
                                         src="roamright.jpg", style = "max-width:150px")),
                         tags$p(class = "text-center",
                                icon("house-damage")," ",
                                tags$a(href = "https://www.roamright.com/travel-insurance-products/preferred-travel-insurance-plan/", "Find out more today!")),
                         tags$p("RoamRight is our partner insurance of choice. Expected coverage is estimated based on their Preferred plan. It is only calculated when Trip Delay exceeds 6 hours, which is the threshold that the delay would be compensated.")
                     )
            )
    ),
    # Statistics screen-------------------------------------------------------------------------------
    tabItem(tabName = "stats",
            (navbarPage("Useful Statistics", position = c("static-top"),
                          tabPanel("Interactive Plots",
                                   fluidRow(box( title = 'What do you want to know about your searched flight?',radioButtons("format", label = '',
                                                                                                                             choices = c("Typical Price","Typical Delay")), width = 12, solidHeader = TRUE,status = "warning", 
                                                 collapsible = TRUE, ribbon = TRUE)),
                                   fluidRow(box(withSpinner(plotOutput("plot2", height = 500)),  solidHeader = TRUE), 
                                            box(withSpinner(plotlyOutput("plot3", height = 500)), solidHeader = TRUE))),
                          tabPanel("Spatial Chart", fluidRow(leafletOutput('leafletoutput', height = 700))), 
                          tabPanel('Heatmap',plotOutput('heatmap', height = 700))))
    ),
    # Alternative transport screen--------------------------------------------------------------------
    tabItem(tabName = "transport",
            (navbarPage("Alternative Routes", position = c("static-top"),
                        #Driving tab
                        tabPanel("Driving",
                                 google_mapOutput(outputId = "mapWarsaw"),
                                 textInput(inputId = "origin", label = "Departure point"),
                                 textInput(inputId = "waypoint", label = "Waypoint 1"),
                                 textInput(inputId = "destination", label = "Destination point"),
                                 actionButton(inputId ="getRoute", label = "Get Route")),
                        #Transit tab
                        tabPanel("Transit", google_mapOutput(outputId = "mapWarsaw2"))
                        
                        
                        
            ))),
    # Trending screen-------------------------------------------------------------------------------
    tabItem(tabName = "trending",
            textOutput("currentTime"),
            fluidRow(box(background = "black", collapsible = T, width = 12, 
                         selectInput("trendingTable","Choose travel destination",c('Alabama','Alaska','Arizona','Arkansas','California','Colorado',
                                                                                   'Connecticut','Delaware','Florida','Georgia','Hawaii','Idaho',
                                                                                   'Illinois','Indiana','Iowa','Kansas','Kentucky','Louisiana','Maine',
                                                                                   'Maryland','Massachusetts','Michigan','Minnesota','Mississippi','Missouri',
                                                                                   'Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico',
                                                                                   'New York','North Carolina','North Dakota','Ohio,Oklahoma','Oregon',
                                                                                   'Pennsylvania','Rhode Island','South Carolina','South Dakota','Tennessee',
                                                                                   'Texas','Utah','Vermont','Virginia','Washington','West Virginia','Wisconsin',
                                                                                   'Wyoming'), selected = "New York", selectize = FALSE),
                         sliderInput(inputId = "no.of.tweets", label = "Select the number of tweets for analysis", min=5, max=500, value = 200))
            ),
            fluidRow(box(title = "Word Cloud", width = 6, status = "primary", solidHeader = TRUE,
                  withSpinner(plotOutput("word"))),
              box(title = "Overall Sentiment", width = 6, status = "primary", solidHeader = TRUE,
                  withSpinner(plotOutput("histScore")))
            ),
            fluidRow(box(title = "Positive Sentiment", width = 6, status = "success", solidHeader = TRUE,
                  withSpinner(plotOutput("histPos"))),
              box(title = "Negative Sentiment", width = 6, status = "danger", solidHeader = TRUE,
                  withSpinner(plotOutput("histNeg")))

            )
    ),
    # About page-------------------------------------------------------------------------------
    tabItem(tabName = "about",
            fluidRow(
              # About FlightAware
              box(title = "About FlightAware",
                  status = "primary",
                  width = 12,
                  tags$p(class = "text-center",
                         tags$a(
                           href = "https://www.r-project.org",
                           target = "_blank",
                           tags$img(class = "image-responsive",
                                    src = "https://www.r-project.org/logo/Rlogo.svg",
                                    style = "max-width: 100px")),
                         tags$a(
                           href = "https://rstudio.com",
                           target = "_blank",
                           tags$img(class = "image-responsive",
                                    src = "RStudio.svg",
                                    style = "max-width: 100px")),
                         tags$a(
                           href = "https://rtweet.info",
                           target = "_blank",
                           tags$img(class = "image-responsive",
                                    src = "rtweet.png",
                                    style = "max-width: 100px"))
                         ),
                  tags$p(class = "text-center",
                    "This dashboard was built in RStudio with",
                    tags$strong("shiny,"),
                    tags$strong("shinydashboard,"),
                    tags$strong("rtweet,"),
                    tags$strong("leaflet,"),
                    tags$strong("dplyr,"), "and many more packages."
                  )
                )
            ),
            fluidRow(
              # About Jun Kai
              box(title = "About us",
                  width = 4,
                  status = "warning",
                  # background = "blue",
                  tags$p(class = "text-center",
                         tags$img(class = "img-responsive img-rounded center-block",
                                  src="junkai2.jpg", style = "max-width:150px")),
                  tags$p(class = "text-center",
                         icon("linkedin")," ",
                         tags$a(href = "https://www.linkedin.com/in/junkai-lee/", "Connect with me!")),
                  tags$p(class = "text-center",
                         tags$strong("Hi! I am Jun Kai.")),
                  tags$p("I am a third year student in NUS Business School majoring in Finance and Business Analytics.")
              ),
              # About Yu Long
              box(title = "About us",
                  width = 4,
                  status = "warning",
                  tags$p(class = "text-center",
                         tags$img(class = "img-responsive img-rounded center-block",
                                  src="yulong.jpg", style = "max-width:150px")),
                  tags$p(class = "text-center",
                         icon("linkedin")," ",
                         tags$a(href = "https://www.linkedin.com/in/yulong-l-960ba486/", "Connect with me!")),
                  tags$p(class = "text-center",
                         tags$strong("Hi! I am Yu Long.")),
                  tags$p("I am a second year student in NUS Business School majoring in Finance and Business Analytics.")
              ),
              # About Bin Jie
              box(title = "About us",
                  width = 4,
                  status = "warning",
                  tags$p(class = "text-center",
                         tags$img(class = "img-responsive img-rounded center-block",
                                  src="bingjie.jpg", style = "max-width:150px")),
                  tags$p(class = "text-center",
                         icon("linkedin")," ",
                         tags$a(href = "https://www.linkedin.com/in/bin-jie-teo-57ba7116a/", "Connect with me!")),
                  tags$p(class = "text-center",
                         tags$strong("Hi! I am Bin Jie.")),
                  tags$p("I am a third year student in NUS Business School majoring in Finance and Business Analytics.")
              )
            ),
            fluidRow(
              # About Soniya
              box(title = "About us",
                  width = 4,
                  status = "warning",
                  tags$p(class = "text-center",
                         tags$img(class = "img-responsive img-rounded center-block",
                                  src="soniya.jpg", style = "max-width:150px")),
                  tags$p(class = "text-center",
                         icon("linkedin")," ",
                         tags$a(href = "http://linkedin.com/in/soniya-mehta-408621172", "Connect with me!")),
                  tags$p(class = "text-center",
                         tags$strong("Hi! I am Soniya.")),
                  tags$p("I am a third year student in NUS Business School majoring in Business Analytics.")
              ),
              # About Rachel
              box(title = "About us",
                  width = 4,
                  status = "warning",
                  tags$p(class = "text-center",
                         tags$img(class = "img-responsive img-rounded center-block",
                                  src="rachel.jpg", style = "max-width:150px")),
                  tags$p(class = "text-center",
                         icon("linkedin")," ",
                         tags$a(href = "https://www.linkedin.com/in/lo-hei-ting/", "Connect with me!")),
                  tags$p(class = "text-center",
                         tags$strong("Hi! I am Rachel.")),
                  tags$p("I am a third year student in NUS Business School majoring in Business Analytics.")
              )
            )
    )
    # End of about page
  )
)

dashboardPage(skin = "purple",
              header = header,
              sidebar = sidebar,
              body = body)


