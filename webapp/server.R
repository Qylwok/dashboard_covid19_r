# France : http://dimension.usherbrooke.ca/dimension/ssrcartes.html 

################ LIEN DU PROF ################ À TESTER
# https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
# Le package WHO n'est plus disponible : il a été retiré pour "policy violation"
# https://cran.r-project.org/web/packages/WHO/index.html
# Contourné par GitHub  https://github.com/expersso/WHO 
library(ggplot2)
library(dplyr)
require(maps)
require(viridis)
theme_set(
  theme_void()
)
library("WHO")

################ STATIC MAP ################ : MARCHE 
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
# library(shiny)
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# library("rnaturalearth") # DONE
# library("rnaturalearthdata") # DONE
# library(sf) # DONE
# theme_set(theme_bw()) # classic dark-on-light theme for ggplot2 (theme_bw)

################ INTERACTIVE MAP ################ : MARCHE PAS (DF IS NOT A DATAFRAME)
# https://www.datanovia.com/en/lessons/highchart-interactive-world-map-in-r/
# library(tidyverse)
# library(data.table)
# library(highcharter)
# options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
# library("dplyr")


# library(shiny)
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# library(ggmap)
# library("rnaturalearth") # DONE
# library("rnaturalearthdata") # DONE
# library(sf) # DONE
# library(raster) # DONE
# library(highcharter)
# library(data.table)



shinyServer(
  function(input, output, session) {
    ################ CHARGEMENT DES DONNÉES ################ 
    
    covid_csv <- as.data.frame(read.csv("https://raw.githubusercontent.com/gibello/whocovid19/master/global_who_data.csv"))
    pop_csv <- as.data.frame(read.csv("https://raw.githubusercontent.com/datasets/population/master/data/population.csv"))
    country_csv <- as.data.frame(read.csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"))
    pop <- as.data.frame(filter(pop_csv, Year == max(pop_csv$Year)))
    # pop <- pop_csv %>% slice(which(pop_csv$Year == max(pop_csv$Year)))
    # head(pop)
    
    
    ################ CONSTRUCTION DE LA DATAFRAME PAR JOINTURE ################ 
    
    
    df <- as.data.frame(left_join(covid_csv, pop, by = c("ISO.3166.code" = "Country.Code")))
    # print("premier left join")
    # print(head(df))
    df <- as.data.frame(left_join(df, country_csv, by = c("ISO.3166.code" = "alpha.3")))
    # print("deuxieme left join")
    # print(head(df))
    # print("types avant changement : ")
    # myGlobals<-objects()
    # for(i in myGlobals){
    #   print(deparse(substitute(i)))
    #   # print(class(i))
    #   print(typeof(get(i)))     #prints 'character'
    #   print("")
    # }
    # print("")
    # print("")
    df <- as.data.frame(df)
    # print("as dataframe")
    # print(head(df))
    # print("types  après changement : ")
    # myGlobals<-objects()
    # for(i in myGlobals){
    #   print(deparse(substitute(i)))
    #   # print(class(i))
    #   print(typeof(get(i)))     #prints 'character'
    #   print("")
    # }
    # print("")
    # print("")
    
    # head(df)
    # for(column in colnames(df)){
    #     print(column)
    #     print(head(df[[column]]))
    #     print(sum(is.na(df[[column]])))
    #     print("===================")
    # }
    
    
    ################ DATA PREPROCESSING ################ 
    
    
    df$intermediate.region.code <- NULL
    df <- df[complete.cases(df), ]
    # head(df$Date)
    df$Date <- as.Date(df$Date , format = "%Y-%m-%d")
    # head(df$Date)
    last_day <- max(df$Date)
    # last_day
    # str(df)
    df$Logarithm.of.confirmed.cases <- log(df$Confirmed.cases)
    # head(df$Logarithm.of.confirmed.cases)
    df$Logarithm.of.deaths <- log(df$Deaths)
    # head(df$Logarithm.of.deaths)
    df$Population <- df$Value
    df$Value <- NULL
    df$Proportion.of.deaths.x1M <- (df$Deaths / df$Population * 1000000)
    df$Proportion.of.confirmed.cases.x1M <- df$Confirmed.cases / df$Population * 1000000
    df$Logarithm.of.proportion.of.deaths.x1M <- log(df$Deaths / df$Population * 1000000)
    df$Logarithm.of.proportion.of.confirmed.cases.x1M <- log(df$Confirmed.cases / df$Population * 1000000)
    last_14_days <- unique(with(df, df[(Date >= last_day - 14), ])$Date)
    # df$Date %>% filter(df$Date >= last_day - 14)
    # df[df$Date %in% unique(as.Date(format(df$Date + 28, "%Y-%m-%d")) - 1),]
    # last_14_days
    # print("apres plein d'operations")
    # print(head(df))
    
    # str(df)
    
    data(worldgeojson, package = "highcharter")
    # print("types avant changement : ")
    # myGlobals<-objects()
    # for(i in myGlobals){
    #   print(deparse(substitute(i)))
    #   # print(class(i))
    #   print(typeof(get(i)))     #prints 'character'
    #   print("")
    # }
    # print("")
    # print("")
    df <- as.data.frame(df)
    # print("as dataframe")
    # print(head(df))
    # print("types après changement : ")
    # myGlobals<-objects()
    # for(i in myGlobals){
    #   print(deparse(substitute(i)))
    #   # print(class(i))
    #   print(typeof(get(i)))     #prints 'character'
    #   print("")
    # }
    # print("")
    # print("")
    
    
    ################ LIEN DU PROF ################ À TESTER
    # https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
    life.exp <- get_data("WHOSIS_000001")             # Retrieve the data
    life.exp <- life.exp %>%
      filter(year == 2015 & sex == "Both sexes") %>%  # Keep data for 2015 and for both sex
      select(country, value) %>%                      # Select the two columns of interest
      rename(region = country, lifeExp = value) %>%   # Rename columns
      # Replace "United States of America" by USA in the region column
      mutate(
        region = ifelse(region == "United States of America", "USA", region)
      )  
    
    world_map <- map_data("world")
    life.exp.map <- left_join(life.exp, world_map, by = "region")
    print("world map")
    print(head(world_map))
    print("")
    print("")
    print("life exp map")
    print(head(life.exp.map))
    print("")
    print("")
    print("df")
    print(head(df))
    df.map <- df[[]]
    
    
    ################ PLOTS ################ 
    output$myPlot <- renderPlot({
      
      ################ LIEN DU PROF ################ À TESTER
      # https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
      
      # Option 1
      # ggplot(life.exp.map, aes(long, lat, group = group))+
      #   geom_polygon(aes(fill = lifeExp ), color = "white")+
      #   scale_fill_viridis_c(option = "C")
      
      # Option 2
      ggplot(life.exp.map, aes(map_id = region, fill = lifeExp))+
        geom_map(map = life.exp.map,  color = "white")+
        expand_limits(x = life.exp.map$long, y = life.exp.map$lat)+
        scale_fill_viridis_c(option = "C")
      
      ################ STATIC MAP ################ : MARCHE
      # https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
      
      # world <- ne_countries(scale = "medium", returnclass = "sf")
      # class(world)
      # ggplot(data = world) +
      #   geom_sf()
      
      ################ INTERACTIVE MAP ################ : MARCHE PAS (DF IS NOT A DATAFRAME)
      # data(worldgeojson, package = "highcharter")
      # hc <- highchart() %>%
      #   hc_add_series_map(
      #     worldgeojson, df$Logarithm.of.proportion.of.confirmed.cases.x1M, # value = "value", joinBy = c('name','country'),
      #     name = "Log(proportion of confirmed cases * 1M)"
      #   )  %>%
      #   hc_colorAxis(stops = color_stops()) %>%
      #   hc_title(text = "World Map") %>%
      #   hc_subtitle(text = "COVID-19 propagation")
      # hc
      
      ################ ORIGINAL ################ 
      
    # 
    #   if(distType == "Normal"){
    #     randomVec <- rnorm(size, mean = as.numeric(input$mean), sd = as.numeric(input$sd))
    #   }
    #   else if(distType == "Exponential"){
    #     randomVec <- rexp(size, rate = 1/as.numeric(input$Lamda))
    #   }
    #   hist(randomVec, col = "blue")
    }
    )
  }
)