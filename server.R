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

#Disclaimer: Give app some time for initial load due to large dataset

#loading dataset + misc adjustments to dataset---------------------------------------
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

#Keys --------------------------------------------------------------------------------
consumer_key = "4tIQsgv4UQLEL27bTmRtM3qa9"
consumer_secret = "XngGThRK4m2c9Nv3lVRzHVn23JGNVxCK6GMLPihoJFfSxAphYY"
access_token = "187155133-u6tKb9JVPJMkGoTFHW8FhHfWKGgve0pBEzOjbnG1"
access_secret = "Ven0KvFVJLaeAwcQeoBksjFnpg9twybtXixWCtswHhzL8"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Function-----------------------------------------------------------------------------
function(input, output, session){
  
  # Home Page ------------------------------------------------------------------------
  observeEvent(input$button2, {
    updateTabItems(session, "menu", "stats")
  })
  
  observeEvent(input$button3, {
    updateTabItems(session, "menu", "flights")
  })
  
  observeEvent(input$button4, {
    updateTabItems(session, "menu", "transport")
  })
  
  observeEvent(input$button5, {
    updateTabItems(session, "menu", "trending")
  })
  
  #Statistics  -----------------------------------------------------------------------
  
  #Reactive data for plotting of charts on statistics page
  get_data <- reactive({
     origin <- as.character(input$from)
     destination <- as.character(input$to)
     origin <- airport[airport$name == origin, "code"]
     destination <- airport[airport$name == destination, "code"]
     testing <- delaydata[delaydata$ORIGIN == origin & delaydata$DEST == destination, ]
     colnames(testing)[5] <- "Carrier"
     testing$Carrier <- as.factor(testing$Carrier)
     testing$MONTH <- as.factor(testing$MONTH)
     testing
  })
  

  output$plot2 <- renderPlot(
    if (input$format == "Typical Delay") {
      newplot()
    }
    else {
      priceplot()
    }
  )
  
  output$plot3 <- 
    renderPlotly(if (input$format == "Typical Delay") {
      newplot2()
    }
    else {
      priceplot2()
    })
  
  #Bargraph for average predicted delay by Carriers
  newplot <- reactive({
    x <- get_data() %>% group_by(Carrier) %>% dplyr::summarise(Predicted_Delay = mean(predicted_delay)) 
    
    x <- x %>% ggplot(aes(x = Carrier, y = Predicted_Delay, fill = Carrier)) + geom_bar(stat = 'identity')+  geom_text(aes(label=round(Predicted_Delay)), vjust=1.6, color="white", size=3.5) + theme_minimal() + ggtitle('Average Predicted Delay by Carriers') + theme(plot.title = element_text( hjust = 0.5))
    x
  })
  
  #Bargraph for average predicted delay by Month
  newplot2 <- reactive({
    
    x <- get_data() %>% group_by(MONTH) %>% dplyr::summarise(Predicted_Delay = mean(predicted_delay)) 
    g <- x %>% plot_ly(x = ~MONTH, y = ~Predicted_Delay, type= 'scatter', mode ='lines', marker = list(color = 'rgb(158,202,225)',
                                                                                                       line = list(color = 'rgb(8,48,107)',
                                                                                                                   width = 1.5))) %>% layout(title = "Average Predicted Delay For Each Month", titlefont = list(size=15, family='Arial'),
                                                                                                                                             xaxis = list(title = "Months",tickangle = -45),
                                                                                                                                             yaxis = list(title = "Average Predicted Delay"))
    g
  })
  #Bargraph for average price by Carriers
  priceplot <- reactive({
    j <- get_data() %>% group_by(Carrier) %>% dplyr::summarise(Avg_Price = mean(P3)) 
    j <- j%>% ggplot(aes(x = Carrier, y = Avg_Price , fill = Carrier)) + geom_bar(stat = 'identity')+  geom_text(aes(label=round(Avg_Price)), vjust=1.6, color="white", size=3.5) + theme_minimal() + ggtitle('Average Price by Carriers') + theme(plot.title = element_text( hjust = 0.5))
    j
  })
  
  #Bar graph for average price by month
  priceplot2 <- reactive({
   x <- get_data() %>% group_by(MONTH) %>% dplyr::summarise(Avg_Price = mean(P3)) 
    g <- x %>% plot_ly(x = ~MONTH, y = ~Avg_Price, type= 'scatter', mode ='lines', marker = list(color = 'rgb(158,202,225)',
                                                                                                 line = list(color = 'rgb(8,48,107)',
                                                                                                             width = 1.5))) %>% layout(title = "Average Price by Month", titlefont = list(size=15, family='Arial'),                                                                                                                                     xaxis = list(title = "Months",tickangle = -45),
                                                                                                                                       yaxis = list(title = "Average Price "))
    g })
  
  #All plots above are reactive to the user's input of "to" & "from" 
  
  #Leaflet for density visualisation of all intra flights in the US (Used 5% sampling as full data is too huge)
  df <- read.csv('final_5%.csv')
  df <- df[df$lat > 19 & df$lat < 65,]
  df <- df[df$lon > -162 & df$lon < -67,]
  m <- leaflet() %>% addTiles() %>% addCircleMarkers(data = df , lng = ~lon , lat = ~lat , popup = ~city,
                                                     radius = 5 , clusterOptions = markerClusterOptions())
  
  output$leafletoutput <- renderLeaflet(m)
  
  #Heatmap for visualisation of average delays in a calendar style

  source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")

  heatmap_func <- function(){
    data <- get_data()
    data$FL_DATE <- as.factor(data$FL_DATE)
    f <- data %>% group_by(FL_DATE) %>% dplyr::summarise(avg_delay = mean(predicted_delay))
    f$FL_DATE <- as.Date(f$FL_DATE)
    g2r <- c("#B5E384","#FFFFBD","#FFAE63","#D61818")
    #r2g <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")
    calendarHeat(f$FL_DATE, f$avg_delay, ncolors = 99, color = "g2r", varname="Average Predicted Delay")
  }
  
  output$heatmap <- renderPlot(heatmap_func())
  
  #Heat map is also reactive to inputs
  
  #Flights -------------------------------------------------------------------------
  flights <- reactive({
    origin <- input$from
    destination <- input$to
    origin <- airport[airport$name == origin, "code"]
    destination <- airport[airport$name == destination, "code"]
    datefrom <- as.Date(input$dateRange[1])
    datetill<- as.Date(input$dateRange[2])
    
    #We used a prediction model (training 2016,2017 data) to predict delay for 2018 [Prediction codes are in another RMD] 
    #We then assume 2018 is today's 'live' date
    year(datefrom)<-2018
    year(datetill)<-2018
    delaydata$predicted_delay <- signif(delaydata$predicted_delay,2)
    delaydata$P3 <- round(delaydata$P3,2)
    t<-delaydata%>%filter(ORIGIN == origin & DEST == destination)%>%
      mutate(DelayCount=ifelse(predicted_delay>6*60,predicted_delay/60,NA), #Only delays that are more than 6hrs have a coverage
             InsuranceCoverage=ceiling(DelayCount/24)*200)%>% #Daily coverage limit by insurance companies
      mutate(ExpectedCoverage=ifelse(InsuranceCoverage>1000,1000,InsuranceCoverage))%>% #Policy limit 
      mutate(CO2=DISTANCE*53/2.205)%>% #Average Carbon dioxide emission in KG by airplanes based on distance
      select(FL_DATE,OP_CARRIER,CRS_DEP_TIME,P3,predicted_delay,ExpectedCoverage,CO2)%>%
      filter(FL_DATE >= datefrom & FL_DATE<= datetill)
    t$FL_DATE<-as.character(format(t$FL_DATE,"%B %d"))
    colnames(t) <- c("Date", "Carrier", "Departure Time","Average Price ($)", "Predicted Delay (Mins)", "Insurance Coverage ($)", "CO2 Emission (Kg)")
    t
  })
  output$table<-renderTable(flights())
  
  #Alternative Transport -------------------------------------------------------------------------
  
  map_key <- "AIzaSyBs-6Zq37eHiDT7c17fpJtqI1ZF6us4efA"
  api_key <- "AIzaSyBs-6Zq37eHiDT7c17fpJtqI1ZF6us4efA"

  #Googlemap output when users click on the tab  
  output$mapWarsaw <- renderGoogle_map({
    google_map(key = map_key, 
               search_box = TRUE, 
               scale_control = TRUE, 
               height = 1000) %>%
      add_traffic()  })
  
  output$mapWarsaw2 <- renderGoogle_map({
    google_map(key = map_key, 
               search_box = TRUE, 
               scale_control = TRUE, 
               height = 500) %>%
      add_traffic()  })
  
 #When Get Route button is clicked 
  observeEvent(input$getRoute,{
    
    print("getting route")
    
    o <- input$origin
    w <- input$waypoint
    d <- input$destination
    
#Google directions api to find out the optimal route for DRIVING, inclusive of traffic conditions, distance and time
    res <- google_directions(key = api_key,
                             origin = o,
                             waypoints = list(stop = w),
                             destination = d,
                             optimise_waypoints = TRUE,
                             mode = 'driving')
    
    df_route <- data.frame(route = res$routes$overview_polyline$points)
    
    df_way <- cbind(
      res$routes$legs[[1]]$end_location,
      data.frame(address = res$routes$legs[[1]]$end_address, 
                 distance = res$routes$legs[[1]]$distance$text,
                 duration =res$routes$legs[[1]]$duration$text 
      ))
    
    df_way$order <- as.character(1:nrow(df_way))
    
# Adding additional information such as CO2 emission and expected cost of driving
    
    #Average fuel efficiency in US: 10.5KM/L
    #Current gas price per litre in US: $0.78 / L
    #Average price per KM for driving: $0.074/Km
    
    df_way$distance1 <- gsub(' km','',df_way$distance)
    df_way$distance1 <- gsub(',',"",df_way$distance1)
    df_way$distance1 <- as.numeric(df_way$distance1)
    df_way$price <-(df_way$distance1*0.074)
    df_way$price <- round(df_way$price,2)
    df_way$price <-paste0("$",df_way$price)
    
    #average carbon emission of car 1km = 0.251Kg CO2
    df_way$carbon <- (df_way$distance1*0.251)
    df_way$carbon <- round(df_way$carbon,2)
    df_way$carbon <- paste0(df_way$carbon," Kg of CO2 Emission")
    df_way$distance_duration <- paste0(df_way$address, sep= " | ", df_way$distance,sep = " | ",df_way$duration, sep= " | Expected Cost ",df_way$price,sep=" | ",df_way$carbon)
    
    
#Google directions for TRANSIT (railway)    
    res2 <- google_directions(key = api_key,
                              origin = o,
                              destination = d,
                              mode = 'transit',
                              transit_mode = 'rail')
    
    df_route2 <- data.frame(route = res2$routes$overview_polyline$points)
    
    df_way2 <- cbind(
      res2$routes$legs[[1]]$end_location,
      data.frame(address = res2$routes$legs[[1]]$end_address, 
                 distance = res2$routes$legs[[1]]$distance$text,
                 duration =res2$routes$legs[[1]]$duration$text)
    )
    
    df_way2$order <- as.character(1:nrow(df_way2))
    
#Price & CO2 information left out as transit information is too ambiguous 
    df_way2$distance_duration <- paste0(df_way2$address, sep= " | ", df_way2$distance,sep = " | ",df_way2$duration)

#Google map updating based on user's input of their origin and destination for driving        
    google_map_update(map_id = "mapWarsaw") %>%
      clear_polylines() %>%
      clear_markers() %>%
      add_traffic() %>%
      add_transit() %>%
      add_polylines(data = df_route,
                    polyline = "route",
                    stroke_colour = "#FF33D6",
                    stroke_weight = 7,
                    stroke_opacity = 0.7 ,
                    info_window = "New route",
                    load_interval = 100) %>%
      #Markers are added to be a 'mouse_over' to display information about the route
      add_markers(data = df_way,
                  info_window = "address",
                  mouse_over = "distance_duration",
                  label = "order")
    
#Updating for Transit   
    google_map_update(map_id = "mapWarsaw2") %>%
      clear_polylines() %>%
      clear_markers() %>%
      add_traffic() %>%
      add_transit() %>%
      add_polylines(data = df_route2,
                    polyline = "route",
                    stroke_colour = "#FF33D6",
                    stroke_weight = 7,
                    stroke_opacity = 0.7 ,
                    info_window = "New route",
                    load_interval = 100) %>%
      add_markers(data = df_way2,
                  mouse_over = "distance_duration",
                  label = "order")
    
    
  })
  
  # Trending Tab ----------------------------------------------------------------------
  # show current time at "Trending" tab
  output$currentTime <- renderText({invalidateLater(1000, session) 
    paste("Current time is: ",Sys.time())})
  
  # Search tweets and create a data frame 
  # Clean the tweets
  TweetFrame <- function(twtList)
  {
    
    df<- do.call("rbind",lapply(twtList,as.data.frame))
    # removal of emoticons
    df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub="")) 
    df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text)
    return (df$text)
  }
  
  # Function to create a data frame from tweets
  pos.words = scan('positive_words.txt', what='character', comment.char=';') 
  neg.words = scan('negative_words.txt', what='character', comment.char=';') 
  
  wordDatabase<-function()
  {
    pos.words<<-c(pos.words)
    neg.words<<-c(neg.words)
  }
  
  # Score analysis
  score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
  {
    list=lapply(sentences, function(sentence, pos.words, neg.words)
    {
      sentence = gsub('[[:punct:]]',' ',sentence)
      sentence = gsub('[[:cntrl:]]','',sentence)
      sentence = gsub('\\d+','',sentence)
      sentence = gsub('\n','',sentence)
      
      sentence = tolower(sentence)
      word.list = str_split(sentence, '\\s+')
      words = unlist(word.list)
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      pp = sum(pos.matches)
      nn = sum(neg.matches)
      score = sum(pos.matches) - sum(neg.matches)
      list1 = c(score, pp, nn)
      return (list1)
    }, pos.words, neg.words)
    score_new=lapply(list, `[[`, 1)
    pp1=score=lapply(list, `[[`, 2)
    nn1=score=lapply(list, `[[`, 3)
    
    scores.df = data.frame(score=score_new, text=sentences)
    positive.df = data.frame(Positive=pp1, text=sentences)
    negative.df = data.frame(Negative=nn1, text=sentences)
    
    list_df=list(scores.df, positive.df, negative.df)
    return(list_df)
  }
  
  wordDatabase()
  
  twtList<-reactive({twtList<-searchTwitter(input$trendingTable, n=input$no.of.tweets, lang="en") })
  tweets<-reactive({tweets<-TweetFrame(twtList() )})
  
  result<-reactive({result<-score.sentiment(tweets(), pos.words, neg.words, .progress='none')})
  
  table_final<-reactive({table_final<-sentimentAnalyser(  result() )})
  table_final_percentage<-reactive({table_final_percentage<-percentage(  table_final() )})
  
  output$tabledata<-renderTable(table_final_percentage())	
  
  # wordcloud
  wordclouds<-function(text)
  {
    corpus <- VCorpus(VectorSource(text)) 
    #clean text
    clean_text <- tm_map(corpus, removePunctuation)
    clean_text <- tm_map(clean_text, content_transformer(tolower))
    clean_text <- tm_map(clean_text, removeWords, stopwords("english"))
    clean_text <- tm_map(clean_text, removeNumbers)
    clean_text <- tm_map(clean_text, stripWhitespace)
    return (clean_text)
  }
  text_word<-reactive({text_word<-wordclouds( tweets() )})
  
  output$word <- renderPlot({ wordcloud(text_word(),random.order=F,max.words=80, col=rainbow(100), main="WordCloud", scale=c(4.5, 1)) })
  
  # Sentiment analysis
  sentimentAnalyser<-function(result)
  {
    # Creating a copy of result data frame
    test1=result[[1]]
    test2=result[[2]]
    test3=result[[3]]
    
    # Creating three different data frames for Score, Positive and Negative
    # Removing text column from data frame
    test1$text=NULL
    test2$text=NULL
    test3$text=NULL
    # Storing the first row(Containing the sentiment scores) in variable q
    q1=test1[1,]
    q2=test2[1,]
    q3=test3[1,]
    qq1=melt(q1, var='Score')
    qq2=melt(q2, var='Positive')
    qq3=melt(q3, var='Negative') 
    qq1['Score'] = NULL
    qq2['Positive'] = NULL
    qq3['Negative'] = NULL
    # Creating data frame
    table1 = data.frame(Text=result[[1]]$text, Score=qq1)
    table2 = data.frame(Text=result[[2]]$text, Score=qq2)
    table3 = data.frame(Text=result[[3]]$text, Score=qq3)
    
    # Merging three data frames into one
    table_final=data.frame(Text=table1$Text, Positive=table2$value, Negative=table3$value, Score=table1$value)
    
    return(table_final)
  }
  
  percentage<-function(table_final)
  {
    # Positive Percentage
    
    # Renaming
    posSc=table_final$Positive
    negSc=table_final$Negative
    
    # Adding column
    table_final$PosPercent = posSc/ (posSc+negSc)
    
    # Replacing Nan with zero
    pp = table_final$PosPercent
    pp[is.nan(pp)] <- 0
    table_final$PosPercent = pp*100
    
    # Negative Percentage
    
    # Adding column
    table_final$NegPercent = negSc/ (posSc+negSc)
    
    # Replacing Nan with zero
    nn = table_final$NegPercent
    nn[is.nan(nn)] <- 0
    table_final$NegPercent = nn*100
    return(table_final)
  }
  
  # Sentiment Histogram 
  output$histPos<- renderPlot({ hist(table_final()$Positive, col=rainbow(10), main="Histogram of Positive Sentiment", xlab = "Positive Score") })
  output$histNeg<- renderPlot({ hist(table_final()$Negative, col=rainbow(10), main="Histogram of Negative Sentiment", xlab = "Negative Score") })
  output$histScore<- renderPlot({ hist(table_final()$Score, col=rainbow(10), main="Histogram of Score Sentiment", xlab = "Overall Score") })

}