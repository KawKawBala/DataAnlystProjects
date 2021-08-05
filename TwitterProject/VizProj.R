#install.packages("ggplot2")
#install.packages("ggmap")
#install.packages("maps")
#install.packages("sf")
#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
#install.packages("rgeos")
library(ggplot2)
library(ggmap)
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

#install.packages("shiny")
#install.packages("rtweet")
#install.packages("tidyverse")
#install.packages("reactable")
#install.packages("shinydashboard")
#install.packages("gganimate")
#install.packages("gifski")
#install.packages("lubridate")
#install.packages("shinyWidgets")
#install.packages("httpuv")
#install.packages("stringr")
library(shiny)
library(tidyverse)
library(rtweet)
library(stringr)
library(httpuv)
library(dplyr)
library(glue)
library(reactable)
library(purrr)
library(shinydashboard)
library(gganimate)
library(gifski)
library(lubridate)
library(shinyWidgets)

#install.packages("rsconnect")
library(rsconnect)

############################  TWITTER TOKEN    #########################################################

#App name
an <- "Tweetmeroteca"
#API key
ak <- "LHrkeHvRJlF8q1vI1l86w0eF2"
#API secret key
ask <- "z4yxqh0gwO8EvgO7U2xOGO5MZcsCCJhgs7z9QPQi0TilwvxcHt"
#Access token
at <- "1250881489052545025-XIkRBTMI97JphnLXiBQ7T3C4DnXzV2"
#Access token secret
ats <- "N1PHOXgSG3bQTmrC5hKnvR9n8lPlh0HEa6U4ebvJzzTeo"

t <- create_token(an, ak, ask, at, ats)

############################################################################################################################

# Tweeter Sign in Credentials # Only required first time you run the APP
# User Name: Tweetmeroteca
# Password: Tweetmeroteca01

############################  Google Token    #########################################################


#Google Maps API for "geocode()" function to transform names into lat and long.
register_google(key="AIzaSyCV_a4OR2R2oSiIFzPvslOU7AolYS0R_Sw", write = TRUE)
############################################################################################################################

ui <- dashboardPage(
  
  
  # Application title
  dashboardHeader(title="Tweetmeroteca"),
  
  # Dashboard Sidebar 
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Tweets", tabName = "Tweets", icon = icon("table")),
    menuItem("Charts", tabName = "Charts", icon = icon("chart-bar")),
    menuItem("Map", tabName = "Map", icon = icon("map-marked")),
    br(),
    br(),
    sliderInput("num_tweets_to_download",
                "Number of tweets:",
                min = 0,
                max = 1000,
                value = 100,
                step = 10),
    
    textInput("hashtag_to_search",
              "Hashtag to search:",
              value = "#pizza"),
    dateRangeInput("date_picker", label = "Select dates:", start = Sys.Date()-7, end = Sys.Date()),
    actionButton("get_data", "Search", class = "btn-primary")
  )),
  
  dashboardBody(
    tags$img(src="https://www.adweek.com/wp-content/uploads/2017/04/perspective-twitter-head-content-2017-1320x650.png",
             style = "position: absolute; opacity:0.2"),
    
    tabItems( 
      
    #Tab 1
      tabItem(tabName = "dashboard",
              valueBoxOutput("Hashtag", width = 4),
              valueBoxOutput("Number", width = 4),
              valueBoxOutput("Tweeter", width = 4),
              valueBoxOutput("Break"),
              valueBoxOutput("Likes", width = 4),
              valueBoxOutput("Rt", width = 4),
              h1("Welcome to Tweetmeroteca"),
                  h4("Tweet, a post on the social media site + Hemeroteca, from the Greek: hemera, day, and theke, box or deposit, where a set of data is stored."),
                  h3("This page is intended to access past Tweets just changing the #Hashtag in the left panel. Once you have changed it click search to update"),
                  h3("In DASHBOARD, you will find the summary information for the selected #Hashtag"),
                  h3("In TWEETS, you will find a filterable and sort table of the results"),
                  h3("In CHARTS, a variety of the most relevant charts that show the behaviour of the table and 
                     focuses on the data rather than the content of the Tweet (Please, allow rendering time)"),
                  h3("In MAP, a world map with the most relevant locations that have twitted about
                     your same interest(please, allow some time and reduce your search size for faster results)")
              
      
       ),
      
    #Tab 2
      tabItem(tabName = "Tweets",
              h2("Tweets Table"),
              
      # Show results
      reactableOutput("tweet_table")
       ),
      
      
    # Tab 3
      tabItem(tabName = "Charts",
              h2("Analysis"),
              
      # Plots
      box(title=" Animated Plot (Please allow loading time)" ,imageOutput("Animate", height = 550)),
      box(title="Number of Tweets ",plotOutput("Tweet_plot", height = 250)),
      box(title="Number of Tweets per User",plotOutput("User",height = 250))
      
           ),
    
    
    
    # Tab 4
    tabItem(tabName = "Map",
            h2("Tweet Geo Location"),
            h5("Allow map to load (Estimated time 1 minute)"),
            
            # Chart 1
            plotOutput("world_map"),
            br(),
            textOutput("mytext4")
    )
    
    
    
    )
  )
)

# Define server logic 
server <- function(input, output, session) {
  
 
  
  #Make attached Tweet links a real link to be added to the table
  make_url_html <- function(url) {
    if(length(url) < 2) {
      if(!is.na(url)) {
        as.character(glue("<a title = {url} target = '_new' href = '{url}'>{url}</a>") )
      } else {
        ""
      }
    } else {
      paste0(purrr::map_chr(url, ~ paste0("<a title = '", .x, "' target = '_new' href = '", .x, "'>", .x, "</a>", collapse = ", ")), collapse = ", ")
    }
  }
  
  
  ######################################## Tab 2 and Base Reactive Table #############################################
  #Get data from Twitter
  tweet_df <- eventReactive(input$get_data, {
    search_tweets(input$hashtag_to_search, n = input$num_tweets_to_download, include_rts = FALSE, token=t)
  }) 
  
  #Create Reactive Table
  tweet_table_data <- reactive({
    req(tweet_df())
    tweet_df() %>%
      select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url, location) %>%
      filter(between(as.Date(created_at), input$date_picker[1], input$date_picker[2])) %>% 
      mutate(
        Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
        URLs = purrr::map_chr(urls_expanded_url, make_url_html)
      )%>%
      select(DateTime = created_at, User = screen_name,Geo=location, Tweet, Likes = favorite_count, RTs = retweet_count, URLs)
  })
  
  # Print Tweet Table
  output$tweet_table <- renderReactable({
    reactable::reactable(tweet_table_data(), 
                         filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                         showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), 
                         columns = list(
                           DateTime = colDef(defaultSortOrder = "asc"),
                           User = colDef(defaultSortOrder = "asc"),
                           Geo = colDef(html = TRUE),
                           Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                           Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                           RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                           URLs = colDef(html = TRUE)
                         )
    )
  })
  
  
  ################################ Tab 1-  Dashboard Boxes and Cards ###################################################
  
  output$Likes <- renderValueBox({
    valueBox( sum(tweet_table_data()$Likes), "Total Likes", icon = icon("heart"),color = "red") })
  
  output$Hashtag <- renderValueBox({
    valueBox( input$hashtag_to_search, "Hashtag", icon = icon("hashtag"),color = "blue") })
  
  output$Rt <- renderValueBox({
    valueBox( sum(tweet_table_data()$RTs), "Retweet", icon = icon("retweet"),color = "yellow") })
  
  output$Number <- renderValueBox({
    valueBox( count(tweet_table_data()), "Num of Tweets", icon = icon("bullhorn"),color = "green") })
  
  # Function to find most frequent name
  fun1 <- function(InVec) {
    names(which.max(table(InVec)))
  }
  
  output$Tweeter <- renderValueBox({
    valueBox( fun1(tweet_table_data()$User), "Main User", icon = icon("users"),color = "green") })
  

  
  
  ######################## Tab 3 - Plots and Graphs ###########################################################
  
  #User_data<-reactive({as.character(tweet_table_data()$User)})
  #Time_data<-reactive({as.Date(tweet_table_data()$DateTime)})
  #Likes_data<-reactive({as.character(tweet_table_data()$Likes)})
  
  
  
# Barplot of Most Active Users and Tweet frequency.
  output$User<-renderPlot({
    with(tweet_table_data(), barplot(sort(table(User),decreasing=TRUE)[1:10], col ="lightblue", ylab="Number of Tweets",
                                     xlab= "",las=3), type=h)
  })
  
  
  output$Tweet_plot<-renderPlot({tweet_table_data() %>%
      ggplot( aes(x=DateTime)) +
      stat_bin(aes(y=(..count..)),fill="lightblue", col="blue") +
      theme_classic()+
      ylab("Number of Tweets")
   # with(tweet_table_data(), hist(as.Date(DateTime,format="%Y/%m/%d"),User, breaks =7 ))
  })
  
  # Animated plot, need to create a GIF and delete afterwards.
  
  output$Animate <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    
  
  Animate_plot<-tweet_table_data() %>%
  ggplot( aes(x=DateTime,y=Likes)) +
   geom_line(stat = "identity") +
  theme_classic() +
  ggtitle("Evolution of Likes") +
  ylab("Number of Likes") +
  transition_reveal(DateTime)
  Animate_plot
  
  anim_save("outfile.gif", animate(Animate_plot))
  
  list(src = "outfile.gif",
       contentType = 'image/gif'
        #width = 400,
       # height = 300,
       # alt = "This is alternate text"
  )},
  deleteFile = TRUE)
  

  
    
    
  
  
  ############################# Tab 4 - Geo Output ##############################################
 
  Geo_data<-reactive({geocode(as.character(tweet_table_data()$Geo))})
  
  output$world_map<-renderPlot({
   world <- ne_countries(scale = "medium", returnclass = "sf")
    
  ggplot(data = world) +
   geom_sf()+
  geom_point(data=Geo_data(), aes(x=lon, y=lat), color="orange")
  })
  
  output$mytext4 <- renderText({
    textshow4<-paste("Location for tweets with Hashtag", input$hashtag_to_search)
     })
}


# Google Reference Required#

#@Article{,
#  author = {David Kahle and Hadley Wickham},
 # title = {ggmap: Spatial Visualization with ggplot2},
#  journal = {The R Journal},
# year = {2013},
# volume = {5},
# number = {1},
# pages = {144--161},
# url =   {https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf},}







# Run the application 
shinyApp(ui = ui, server = server)
