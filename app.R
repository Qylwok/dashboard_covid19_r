# CE LIEN A L'AIR INSANE : https://shiny.rstudio.com/gallery/covid19-tracker.html

####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)

library(ggplot2)
library(dplyr)
require(maps)
require(viridis)
theme_set(
  theme_void()
)
library("WHO")
library(tictoc)



################ CHARGEMENT DES DONNEES ################ 

tic("reading the csvs")
covid_csv <- as.data.frame(read.csv("https://raw.githubusercontent.com/gibello/whocovid19/master/global_who_data.csv"))
pop_csv <- as.data.frame(read.csv("https://raw.githubusercontent.com/datasets/population/master/data/population.csv"))
country_csv <- as.data.frame(read.csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"))
pop <- as.data.frame(filter(pop_csv, Year == max(pop_csv$Year)))
toc()

################ CONSTRUCTION DE LA DATAFRAME PAR JOINTURE ################ 

tic("making left joins")
df <- as.data.frame(left_join(covid_csv, pop, by = c("ISO.3166.code" = "Country.Code")))
df <- as.data.frame(left_join(df, country_csv, by = c("ISO.3166.code" = "alpha.3")))
df <- as.data.frame(df)
toc()

################ DATA PREPROCESSING ################ 

tic("preprocessing 1")
df$intermediate.region.code <- NULL
df <- df[complete.cases(df), ]
df$Date <- as.Date(df$Date , format = "%Y-%m-%d")
last_day <- max(df$Date)
df$Population <- df$Value
df$Value <- NULL
df$New.confirmed.cases <- df$New.cases
df$New.cases <- NULL
toc()

tic("preprocessing 2")
df$Logarithm.of.confirmed.cases <- log(1 + df$Confirmed.cases - min(df$Confirmed.cases))
df$Logarithm.of.deaths <- log(1 + df$Deaths - min(df$Deaths))
df$Logarithm.of.new.confirmed.cases <- log(1 + df$New.confirmed.cases - min(df$New.confirmed.cases))
df$Logarithm.of.new.deaths <- log(1 + df$New.deaths - min(df$New.deaths))
toc()

tic("preprocessing 3")
df$Proportion.of.deaths.x1M <- (df$Deaths / df$Population * 1000000)
df$Proportion.of.confirmed.cases.x1M <- df$Confirmed.cases / df$Population * 1000000
df$Proportion.of.new.deaths.x1M <- (df$New.deaths / df$Population * 1000000)
df$Proportion.of.new.confirmed.cases.x1M <- df$New.confirmed.cases / df$Population * 1000000
toc()

tic("preprocessing 4")
df$Logarithm.of.proportion.of.deaths.x1M <- log(1 + df$Deaths / df$Population * 1000000 - min(df$Deaths / df$Population * 1000000))
df$Logarithm.of.proportion.of.confirmed.cases.x1M <- log(1 + df$Confirmed.cases / df$Population * 1000000 - min(df$Confirmed.cases / df$Population * 1000000))
df$Logarithm.of.proportion.of.new.deaths.x1M <- log(1 + df$New.deaths / df$Population * 1000000 - min(df$New.deaths / df$Population * 1000000))
df$Logarithm.of.proportion.of.new.confirmed.cases.x1M <- log(1 + df$New.confirmed.cases / df$Population * 1000000 - min(df$New.confirmed.cases / df$Population * 1000000))
toc()

tic("preprocessing 5")
last_14_days <- unique(with(df, df[(Date >= last_day - 14), ])$Date)

dateMin  <- min(df$Date)
dateMax  <- max(df$Date)

data(worldgeojson, package = "highcharter")
df <- as.data.frame(df)
toc()

################ LIEN DU PROF ################ A TESTER
# https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
# tic("lien du prof life.exp") # 4.5 secondes ce fdp
# life.exp <- get_data("WHOSIS_000001")             # Retrieve the data
# life.exp <- life.exp %>%
#   filter(year == 2015 & sex == "Both sexes") %>%  # Keep data for 2015 and for both sex
#   select(country, value) %>%                      # Select the two columns of interest
#   rename(region = country, lifeExp = value) %>%   # Rename columns
#   # Replace "United States of America" by USA in the region column
#   mutate(
#     region = ifelse(region == "United States of America", "USA", region)
#   )  
# 
world_map <- map_data("world")
# life.exp.map <- left_join(life.exp, world_map, by = "region")
# toc()
# write.csv(life.exp.map,'lifeexp3.csv',fileEncoding = "UTF-8")
# write.table(life.exp.map,'lifeexp2.csv')

# print(tally(life.exp$region))
# print("")
# print(tally(df$region))
# print(lapply(life.exp, table))
# print("")
# print(lapply(df, table))



# tic("tmp0 and tmp00")
# tmp0 <- life.exp %>% group_by(region) %>% summarise(count=n())
# # print(tmp0)
# # print("")
# tmp00 <- df %>% filter(Date == '2020-06-01') %>% group_by(Country) %>% summarise(count=n())
# # print(tmp00)
# # print("")
# # print(tmp0[!(tmp0 %in% tmp00)])
# toc()


tic("last preprocessing : filtering df.map")
df.map <- df %>% 
  filter()
df.map <- select(
  left_join(
    df.map, 
    world_map, 
    by = c("Country" = "region")), 
  Confirmed.cases, Deaths, New.confirmed.cases, New.deaths, 
  
  Logarithm.of.confirmed.cases, Logarithm.of.deaths,
  Logarithm.of.new.confirmed.cases, Logarithm.of.new.deaths, 
  
  Proportion.of.deaths.x1M, Proportion.of.confirmed.cases.x1M, 
  Proportion.of.new.deaths.x1M, Proportion.of.new.confirmed.cases.x1M, 
  
  Logarithm.of.proportion.of.deaths.x1M, Logarithm.of.proportion.of.confirmed.cases.x1M, 
  Logarithm.of.proportion.of.new.deaths.x1M, Logarithm.of.proportion.of.new.confirmed.cases.x1M,
  
  Country, long, lat, group, order, subregion, Date)
df.map <- rename(df.map, region = Country)

whichpart <- function(x, n=30) {
  nx <- length(x)
  p <- nx-n
  xp <- sort(x, partial=p)[p]
  which(x > xp)
}
toc()

tic("constructing tmp")
tmp <- df.map %>% filter(Date == as.Date(dateMax,"%Y-%m-%d")) %>% 
  select(-c(long, lat, group, order, subregion)) %>%
  distinct()
toc()


















  ################################ INTERFACE UTILISATEUR  ############################### 
  ui <- fluidPage(theme = shinytheme("cosmo"),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "COVID-19 Dashboard in R",
      tabPanel("World Map",
               sidebarPanel(
                 # tags$h3("Input:"),
                 # textInput("txt1", "Given Name:", ""),
                 # textInput("txt2", "Surname:", ""),
                 selectInput("DataW", "What to display : ",
                             choices = c("Confirmed cases", "Deaths", "New confirmed cases", "New deaths")),
                 selectInput("ScaleW", "Scale : ",
                             choices = c("Linear", "Logarithmic", "Proportional", "Log(proportional)")),
                 sliderInput("DateW", "Slide the date to see the evolution!",
                             min = as.Date(dateMin,"%Y-%m-%d"),
                             max = as.Date(dateMax,"%Y-%m-%d"), 
                             value=as.Date(dateMax), 
                             timeFormat="%Y-%m-%d"),
                 # conditionalPanel(condition = "input.Distribution == 'Normal'",
                 #                  textInput("mean", "Please select the mean", 10),
                 #                  textInput("sd", "Please select the Standard Deviation", 3)),
                 # conditionalPanel(condition = "input.Distribution == 'Exponential'",
                 #                  textInput("Lamda", "Please select Exponential Lamda", 1)),
                 
                 
               ), # sidebarPanel
               mainPanel(
               #              h1("Header 1"),
               #              
               #              h4("Output 1"),
               #              verbatimTextOutput("txtout"),
                 plotOutput("WorldMap"),

               ) # mainPanel
               
      ), # Navbar 1, tabPanel1
      tabPanel("Graphics", 
               sidebarPanel(
                 selectInput("DataG", "What to display : ",
                             choices = c("Confirmed cases", "Deaths", "New confirmed cases", "New deaths")),
                 selectInput("ScaleG", "Scale : ",
                             choices = c("Linear", "Logarithmic", "Proportional", "Log(proportional)")),
                 sliderInput("NcountriesG", "Amount of countries to display",
                             min = 1,
                             max = length(unique(df.map$region)), 
                             value=10, 
                             # timeFormat="%Y-%m-%d", 
                 ), # sliderInput
               ), # sidebarPanel
               # tic("main panel graphics"),
               mainPanel(
                 plotOutput("Graphic"),
               ), # mainPanel
               toc()
      ), # Navbar 1, tabPanel 2
      tabPanel("Histograms", 
               "This panel is intentionally left blank"
      ) # Navbar 1, tabPanel 3
  
    ) # navbarPage
  ) # fluidPage
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################################  SERVEUR  ###############################   
  server <- function(input, output, session) {
    # Reactive dataframe : 
    # https://stackoverflow.com/questions/24680246/how-to-trigger-a-data-refresh-in-shiny
    # https://stackoverflow.com/questions/30443625/how-do-i-build-a-reactive-dataframe-in-r-shiny
    # ReactivePoll : 
    # https://shiny.rstudio.com/reference/shiny/latest/reactivePoll.html
    # Examples : 
    # https://gist.github.com/wch/9652222
    # https://stackoverflow.com/questions/55280716/use-reactivepoll-inside-an-observe-r-shiny 
    
    
    
    # str(df.map)
    # print(colnames(df.map))
    
    # df.date <- renderDataTable({
    #   df.date <- df.map %>% filter(Date == input$DateW)
    #   df.date
    # })
    tic("df.reactivew")
    df.reactiveW <- reactivePoll(1000, session, 
                 checkFunc = function() {
                   input$DateW
                   input$DataW
                   input$ScaleW
                 }, 
                 valueFunc = function() {
                   if(input$DataW == "Confirmed cases"){
                     if(input$ScaleW == "Linear"){
                       df.map %>% filter(Date == input$DateW) %>% 
                         # select(c(Confirmed.cases, long, lat, region)) %>% 
                         rename(Data = Confirmed.cases)
                     }
                     else if(input$ScaleW == "Logarithmic"){
                       df.map %>% filter(Date == input$DateW) %>% 
                         # select(c(Logarithm.of.confirmed.cases, long, lat, region)) %>% 
                         rename(Data = Logarithm.of.confirmed.cases)
                     }
                     else if(input$ScaleW == "Proportional"){
                       df.map %>% filter(Date == input$DateW) %>% 
                         # select(c(Proportion.of.confirmed.cases.x1M, long, lat, region)) %>% 
                         rename(Data = Proportion.of.confirmed.cases.x1M)
                     }
                     else if(input$ScaleW == "Log(proportional)"){
                       df.map %>% filter(Date == input$DateW) %>% 
                         # select(c(Logarithm.of.proportion.of.confirmed.cases.x1M, long, lat, region)) %>% 
                         rename(Data = Logarithm.of.proportion.of.confirmed.cases.x1M)
                     }
                     
                   }
                   else if(input$DataW == "Deaths"){
                     if(input$ScaleW == "Linear"){
                       df.map %>% filter(Date == input$DateW) %>% 
                         # select(c(Deaths, long, lat, region)) %>% 
                         rename(Data = Deaths)
                     }
                     else if(input$ScaleW == "Logarithmic"){
                       df.map %>% filter(Date == input$DateW) %>% 
                         # select(c(Logarithm.of.deaths, long, lat, region)) %>% 
                         rename(Data = Logarithm.of.deaths)
                     }
                     else if(input$ScaleW == "Proportional"){
                       df.map %>% filter(Date == input$DateW) %>% 
                         # select(c(Proportion.of.deaths.x1M, long, lat, region)) %>% 
                         rename(Data = Proportion.of.deaths.x1M)
                     }
                     else if(input$ScaleW == "Log(proportional)"){
                       df.map %>% filter(Date == input$DateW) %>% 
                         # select(c(Logarithm.of.proportion.of.deaths.x1M, long, lat, region)) %>% 
                         rename(Data = Logarithm.of.proportion.of.deaths.x1M)
                     }
                     
                   }
                   else if(input$DataW == "New confirmed cases"){
                     # print(df.map)
                     if(input$ScaleW == "Linear"){
                       df.map %>% filter(Date == input$DateW) %>% 
                         rename(Data = New.confirmed.cases)
                     }
                     else if(input$ScaleW == "Logarithmic"){
                       df.map %>% filter(Date == input$DateW) %>% 
                         rename(Data = Logarithm.of.new.confirmed.cases)
                     }
                     else if(input$ScaleW == "Proportional"){
                       df.map %>% filter(Date == input$DateW) %>% 
                         rename(Data = Proportion.of.new.confirmed.cases.x1M)
                     }
                     else if(input$ScaleW == "Log(proportional)"){
                       df.map %>% filter(Date == input$DateW) %>% 
                         rename(Data = Logarithm.of.proportion.of.new.confirmed.cases.x1M)
                     }
                     
                   }
                   else if(input$DataW == "New deaths"){
                     if(input$ScaleW == "Linear"){
                       df.map %>% filter(Date == input$DateW) %>% 
                         # select(c(new.deaths, long, lat, region)) %>% 
                         rename(Data = New.deaths)
                     }
                     else if(input$ScaleW == "Logarithmic"){
                       df.map %>% filter(Date == input$DateW) %>% 
                         # select(c(Logarithm.of.new.deaths, long, lat, region)) %>% 
                         rename(Data = Logarithm.of.new.deaths)
                     }
                     else if(input$ScaleW == "Proportional"){
                       df.map %>% filter(Date == input$DateW) %>% 
                         # select(c(Proportion.of.new.deaths.x1M, long, lat, region)) %>% 
                         rename(Data = Proportion.of.new.deaths.x1M)
                     }
                     else if(input$ScaleW == "Log(proportional)"){
                       df.map %>% filter(Date == input$DateW) %>% 
                         # select(c(Logarithm.of.proportion.of.new.deaths.x1M, long, lat, region)) %>% 
                         rename(Data = Logarithm.of.proportion.of.new.deaths.x1M)
                     }
                   }
                   
                 })
    toc()
      
    
    ################ PLOTS ################ 
    tic("render plot world map")
    output$WorldMap <- renderPlot({
      
      df.date <- df.reactiveW()
      # print(str(df.date))
      
      ################ LIEN DU PROF ################ A TESTER
      # https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
      
      # Option 2
      # ggplot(
      #   life.exp.map, 
      #   aes(map_id = region, 
      #       fill = lifeExp)
      # ) + geom_map(
      #       map = life.exp.map,  
      #       color = "white"
      # ) + expand_limits(
      #       x = life.exp.map$long, 
      #       y = life.exp.map$lat
      # ) + scale_fill_viridis_c(option = "C")
      
      
      ggplot(
        df.date,
        aes(map_id = region,
            fill = Data)
      ) + geom_map(
        map = df.date,
        color = "black"
      ) + expand_limits(
        x = df.date$long,
        y = df.date$lat
      ) + scale_fill_viridis_c(option = "C")
      
      # gg <- ggplot()
      # gg <- gg + geom_map(data=us, map=us,
      #                     aes(long, lat, map_id=region),
      #                     color="#2b2b2b", fill=NA, size=0.15)
      # gg <- gg + coord_map("polyconic")
      # gg <- gg + theme_map()
      # gg <- gg + theme(plot.margin=margin(20,20,20,20))
      # gg
    }) # RenderPlot WorldMap
    toc()
    
    
    
    
    
    
    
    
    
    
    tic("df.reactiveG")
    df.reactiveG <- reactivePoll(1000, session, 
       checkFunc = function() {
         input$NcountriesG
         input$DataG
         input$ScaleG
       }, 
       valueFunc = function() {
         # print("whichpart(df.map$Confirmed.cases, n=input$NcountriesG) : ")
         # print(whichpart(df.map$Confirmed.cases, n=input$NcountriesG))
         if(input$DataW == "Confirmed cases"){
           if(input$ScaleW == "Linear"){ # Confirmed.cases
             print("begin")
             tic("constructing tmp2")
             tmp2 <- tmp %>% arrange(Confirmed.cases)
             toc()
             tic("constructing countries_max")
             countries_max <- tail(tmp2$region, input$NcountriesG)
             toc()
             
             tic("outputing df.map filtered")
             df.map %>% filter(region %in% countries_max) %>%
               rename(Data = Confirmed.cases)
             toc()
             df.map %>% filter(region %in% countries_max) %>%
               rename(Data = Confirmed.cases)
           }
           else if(input$ScaleW == "Logarithmic"){ # Logarithm.of.confirmed.cases
             tmp2 <- tmp %>% arrange(Logarithm.of.confirmed.cases)
             countries_max <- tail(tmp2$region, input$NcountriesG)
             
             df.map %>% filter(region %in% countries_max) %>%
               rename(Data = Logarithm.of.confirmed.cases)
           }
           else if(input$ScaleW == "Proportional"){ # Proportion.of.confirmed.cases.x1M
             tmp2 <- tmp %>% arrange(Proportion.of.confirmed.cases.x1M)
             countries_max <- tail(tmp2$region, input$NcountriesG)
             
             df.map %>% filter(region %in% countries_max) %>%
               rename(Data = Proportion.of.confirmed.cases.x1M)
           }
           else if(input$ScaleW == "Log(proportional)"){ # Logarithm.of.proportion.of.confirmed.cases.x1M
             tmp2 <- tmp %>% arrange(Logarithm.of.proportion.of.confirmed.cases.x1M)
             countries_max <- tail(tmp2$region, input$NcountriesG)
             
             df.map %>% filter(region %in% countries_max) %>%
               rename(Data = Logarithm.of.proportion.of.confirmed.cases.x1M)
           }
           
         }
         else if(input$DataW == "Deaths"){
           if(input$ScaleW == "Linear"){ # Deaths
             tmp2 <- tmp %>% arrange(Deaths)
             countries_max <- tail(tmp2$region, input$NcountriesG)
             
             df.map %>% filter(region %in% countries_max) %>%
               rename(Data = Deaths)
           }
           else if(input$ScaleW == "Logarithmic"){ # Logarithm.of.deaths
             tmp2 <- tmp %>% arrange(Logarithm.of.deaths)
             countries_max <- tail(tmp2$region, input$NcountriesG)
             
             df.map %>% filter(region %in% countries_max) %>%
               rename(Data = Logarithm.of.deaths)
           }
           else if(input$ScaleW == "Proportional"){ # Proportion.of.deaths.x1M
             tmp2 <- tmp %>% arrange(Proportion.of.deaths.x1M)
             countries_max <- tail(tmp2$region, input$NcountriesG)
             
             df.map %>% filter(region %in% countries_max) %>%
               rename(Data = Proportion.of.deaths.x1M)
           }
           else if(input$ScaleW == "Log(proportional)"){ # Logarithm.of.proportion.of.deaths.x1M
             tmp2 <- tmp %>% arrange(Logarithm.of.proportion.of.deaths.x1M)
             countries_max <- tail(tmp2$region, input$NcountriesG)
             
             df.map %>% filter(region %in% countries_max) %>%
               rename(Data = Logarithm.of.proportion.of.deaths.x1M)
           }
           
         }
         else if(input$DataW == "New confirmed cases"){
           if(input$ScaleW == "Linear"){ #New.confirmed.cases
             tmp2 <- tmp %>% arrange(New.confirmed.cases)
             countries_max <- tail(tmp2$region, input$NcountriesG)
             
             df.map %>% filter(region %in% countries_max) %>%
               rename(Data = New.confirmed.cases)
           }
           else if(input$ScaleW == "Logarithmic"){ #Logarithm.of.new.confirmed.cases
             tmp2 <- tmp %>% arrange(Logarithm.of.new.confirmed.cases)
             countries_max <- tail(tmp2$region, input$NcountriesG)
             
             df.map %>% filter(region %in% countries_max) %>%
               rename(Data = Logarithm.of.new.confirmed.cases)
           }
           else if(input$ScaleW == "Proportional"){ #Proportion.of.new.confirmed.cases.x1M
             tmp2 <- tmp %>% arrange(Proportion.of.new.confirmed.cases.x1M)
             countries_max <- tail(tmp2$region, input$NcountriesG)
             
             df.map %>% filter(region %in% countries_max) %>%
               rename(Data = Proportion.of.new.confirmed.cases.x1M)
           }
           else if(input$ScaleW == "Log(proportional)"){ # Logarithm.of.proportion.of.new.confirmed.cases.x1M
             tmp2 <- tmp %>% arrange(Logarithm.of.proportion.of.new.confirmed.cases.x1M)
             countries_max <- tail(tmp2$region, input$NcountriesG)
             
             df.map %>% filter(region %in% countries_max) %>%
               rename(Data = Logarithm.of.proportion.of.new.confirmed.cases.x1M)
           }
           
         }
         else if(input$DataW == "New deaths"){
           if(input$ScaleW == "Linear"){ # New.deaths
             tmp2 <- tmp %>% arrange(New.deaths)
             countries_max <- tail(tmp2$region, input$NcountriesG)
             
             df.map %>% filter(region %in% countries_max) %>%
               rename(Data = New.deaths)
           }
           else if(input$ScaleW == "Logarithmic"){ # Logarithm.of.new.deaths
             tmp2 <- tmp %>% arrange(Logarithm.of.new.deaths)
             countries_max <- tail(tmp2$region, input$NcountriesG)
             
             df.map %>% filter(region %in% countries_max) %>%
               rename(Data = Logarithm.of.new.deaths)
           }
           else if(input$ScaleW == "Proportional"){ # Proportion.of.new.deaths.x1M
             tmp2 <- tmp %>% arrange(Proportion.of.new.deaths.x1M)
             countries_max <- tail(tmp2$region, input$NcountriesG)
             
             df.map %>% filter(region %in% countries_max) %>%
               rename(Data = Proportion.of.new.deaths.x1M)
           }
           else if(input$ScaleW == "Log(proportional)"){ # Logarithm.of.proportion.of.new.deaths.x1M
             tmp2 <- tmp %>% arrange(Logarithm.of.proportion.of.new.deaths.x1M)
             countries_max <- tail(tmp2$region, input$NcountriesG)
             
             df.map %>% filter(region %in% countries_max) %>%
               rename(Data = Logarithm.of.proportion.of.new.deaths.x1M)
           }
         }
       }) # reactivePoll
    toc()
    tic("render plot graphic")
    output$Graphic <- renderPlot({
      tic("constructing df.graphics")
      df.graphic <- df.reactiveG()
      toc()
      
      # tic("constructing ggplot graph")
      ggplot(df.graphic, 
             aes(
               x = Date,
               y = Data, 
               color = region, 
               group = region
             )) + 
        geom_point() + 
        geom_line()
      # toc()
      # ggplot(df.graphic, 
      #        aes(
      #          x = Date,
      #          y = Data, 
      #          color = region, 
      #          group = region
      #        )) + 
      #   geom_point() + 
      #   geom_line()
    })
    toc()
    
    # output$txtout <- renderText({
    #   paste( input$txt1, input$txt2, sep = " " )
    # })
  } # server
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################################  APPLICATION  ################################ 
  shinyApp(ui = ui, server = server)
