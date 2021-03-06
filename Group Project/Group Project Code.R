#SSC 442 Group Project
#Team Awesome - Sayem Lincoln, Joshua Schwimmer, John Townshend.

library(tidyverse)
library(ggplot2)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)# for static and interactive maps
library(tmaptools)
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(shiny)
library(plotly)
library(gapminder)

#data files 

#CO2 emission datasets 
#annual Co2 emission per country 
annual <- read.csv(file = "annual-co2-emissions-per-country.csv")
colnames(annual)[which(names(annual) == "Annual.COâ...emissions..tonnes.")] <- "CO2_Emission: "  #changing column name
colnames(annual)[which(names(annual) == "Entity")] <- "name_long"  #changing column name
an_names<-data.frame(annual$name_long[!duplicated(annual$name_long)])
colnames(an_names)[which(names(an_names) == "annual.name_long..duplicated.annual.name_long..")] <- "Locations"  #changing column name

#C02 in atmosphere
atmosphere <- read.csv(file = "co2-concentration-long-term.csv") 
colnames(atmosphere)[which(names(atmosphere) == "COâ...concentration..parts.per.million.")] <- "CO2_Concentration"  #changing column name

#temperature change by hemispheres, tropics and global
temp <- read.csv(file = "temperature-anomaly.csv") 
colnames(temp)[which(names(temp) == "Median..â.f.")] <- "Median"  #changing column name
colnames(temp)[which(names(temp) == "Upper..â.f.")] <- "Upper"  #changing column name
colnames(temp)[which(names(temp) == "Lower..â.f.")] <- "Lower"  #changing column name

#Co2 emission per capita 
capita <- read.csv(file = "co-emissions-per-capita.csv")
capita$CO2.emission.per.capita<-round(capita$CO2.emission.per.capita,digit=2) #rounding the numbers to 2 decimal places 
colnames(capita)[which(names(capita) == "CO2.emission.per.capita")] <- "CO2_Emission: "  #changing column name
colnames(capita)[which(names(capita) == "Entity")] <- "name_long"  #changing column name

#production vs consumption based CO2 emissions
pvc <- read.csv(file = "production-vs-consumption-co2-emissions.csv") 
colnames(pvc)[which(names(pvc) == "Consumption.based..tonnes.")] <- "Consumption Based CO2_Emission"  #changing column name
colnames(pvc)[which(names(pvc) == "Production.based..tonnes..")] <- "Production Based CO2_Emission"  #changing column name
pvc_names<-data.frame(pvc$Entity[!duplicated(pvc$Entity)])
colnames(pvc_names)[which(names(pvc_names) == "pvc.Entity..duplicated.pvc.Entity..")] <- "Locations"  #changing column name


#1.	current impact of CO2 emissions on the atmosphere
atmosphere$row_num <- seq.int(nrow(atmosphere))

plot1 <- ggplot(atmosphere, mapping=aes(x=atmosphere$row_num,
                                         y=atmosphere$CO2_Concentration))+ 
  geom_point()+
  ggtitle("Carbon dioxide (CO2) concentration in atmosphere ")+
  labs( x= "Period of Years" ,y="Carbon dioxide (CO2), measured in parts per million(ppm)",
        subtitle = "Global average long-term atmospheric concentration of carbon dioxide (CO2), \nmeasured in parts per million(ppm).")

plot1

#2.	efforts to reduce CO2 emissions 

#annual Co2 emission per country 
# shinny app world heat map for annual c02 emission per country

ui_annual1 <- fluidPage(
  titlePanel("Annual C02 Emission"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id= "tabs",
                  tabPanel("Map", id = "Map", 
                           br(), 
                           p("Annual Carbon dioxide (CO2) emissions in tonnes per year."), 
                           sliderInput("Year", "Select the Year", min = 1800 , max = 2017, 
                                       value = 1800, dragRange= TRUE)
                  )
      )
    ),
    
    mainPanel(
      tabsetPanel(type= "tabs",
                  tabPanel("Map", leafletOutput(outputId = "annual1"))
      )
    )
  )
)

server_annual1<- function(input, output) {
  output$annual1 <- renderLeaflet({
    data1 <- left_join(world, annual, by = "name_long") 
    data1$iso_a2 <- NULL
    Annual_C02_Emission <- data1[data1$Year == input$Year,]
  
    annual1 = tm_shape(Annual_C02_Emission)+
      tm_polygons(col="CO2_Emission: ", n=5,
                  title = "Annual Carbon dioxide (CO2) Emission (in tonnes)" )+ tm_borders()+
      tm_style("col_blind")
    tmap_leaflet(annual1)
    
  })
}
shinyApp(ui_annual1, server_annual1)

#shiny app for making linear regression model plots for annual co2 emissions
#over period of years with prediction bands for every country 

ui_annual2 <- fluidPage(
  titlePanel("Linear regression model plots for Annual Carbon dioxide (CO2) emissions"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id= "tabs",
                  tabPanel("Location", id = "Map", 
                           br(), 
                           p("Annual Carbon dioxide (CO2) emissions over period of years."), 
                           selectizeInput("name_long", "Select a location", c(an_names) )
                  )
      )
    ),
    
    mainPanel(
      tabsetPanel(type= "tabs",
                  tabPanel("Linear Regression Model",
                           plotOutput(outputId = "annual2"))
      )
    )
  )
)

server_annual2 <- function(input, output) {
  output$annual2 <- renderPlot({
    
    anf <-  function(x){
      data1 <- annual[annual$name_long== x, ]
      
      #Building linear model 
      model <- lm(data1$CO2_Emission~ data1$Year, data = data1)
      
      #Adding predictions 
      pred.int <- predict(model, interval = "prediction")
      data2 <- cbind(data1, pred.int)
      
      plot <- ggplot(data2, aes(x=data1$Year, y=data1$CO2_Emission)) +
        geom_point() +
        stat_smooth(method = lm)+
        #prediction line in black
        geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
        geom_line(aes(y = upr), color = "red", linetype = "dashed")+ 
        ggtitle(paste0("Annual Carbon dioxide (CO2) emissions in ", x))+
        labs(x="Period of Years", y="Emissions (in tonnes)", subtitle = "With Prediction bands")
      
      return(plot)
    }
    
    annual2 <- anf(toString(req(input$name_long)))
    plot(annual2)
    
  }, height=500, width=800)
}
shinyApp(ui_annual2, server_annual2)

#3.	forecast of temperature change

# shinny app for change in temperature by regions with a predictive 
#linear model to present how the tempeatures changed with prediction bands 

ui_temp <- fluidPage(
  titlePanel("Forecast of temperature changes."),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id= "tabs",
                  tabPanel("Regions", id = "Map", 
                           br(), 
                           p("Global average land-sea temperature anomaly relative to the 1961-1990 
                           \naverage temperature in degrees celsius (°C)"), 
                           selectInput("Entity", "Select a region", 
                                       c("Global", "Northern Hemisphere" ,
                                         "Southern Hemisphere","Tropics"))
                  )
      )
    ),
    
    mainPanel(
      tabsetPanel(type= "tabs",
                  tabPanel("Linear Regression Model",
                           plotOutput(outputId = "temp1")) 
      )
    )
  )
)

server_temp <- function(input, output) {
  output$temp1 <- renderPlot({
    
    tempf <-  function(x){

      data1 <- temp[temp$Entity== x, ]
      
      #Building linear model 
      model <- lm(data1$Median ~ data1$Upper+data1$Lower, data = data1)
      
      #Adding predictions 
      pred.int <- predict(model, interval = "prediction")
      data2 <- cbind(data1, pred.int)
      
      plot <- ggplot(data2, mapping=aes(x=data1$Year, y=data1$Median)) +
        geom_point() +
        stat_smooth(method = lm)+
        #prediction line in black
        geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
        geom_line(aes(y = upr), color = "red", linetype = "dashed")+ 
        ggtitle(paste0("Average temperature change in ", x, " over period of years." ))+
        labs(x="Period of Years", y="Average temperature in degrees celsius (°C)", subtitle = "With Prediction bands")
      
      return(plot)
    }
    
    temp1 <- tempf( toString(req(input$Entity)))
    plot(temp1)
  }, height=500, width=800)
  
}
shinyApp(ui_temp, server_temp)

#4.	which country is emitting the most CO2 per capita
# shinny app world heat map for per capita c02 emission 

ui_capita <- fluidPage(
  titlePanel("Per Capita Carbon dioxide (CO2) Emission"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id= "tabs",
                  tabPanel("Map", id = "Map", 
                           br(), 
                           p("Average carbon dioxide (CO2) emissions per capita measured in tonnes per year."), 
                           sliderInput("Year", "Select the Year", min = 1800 , max = 2017, 
                                       value = 1800, dragRange= TRUE)
                  )
      )
    ),
    
    mainPanel(
      tabsetPanel(type= "tabs",
                  tabPanel("Map", leafletOutput(outputId = "capita1"))
      )
    )
  )
)

server_capita <- function(input, output) {
  output$capita1 <- renderLeaflet({
    world_test2 = left_join(world, capita, by = "name_long") 
    world_test2$iso_a2 <- NULL
    Per_Capita_C02_Emission <- world_test2[world_test2$Year == input$Year,]
    breaks =c(0,4,8,12,16,20,24,28)
    
    capita1 =tm_shape(Per_Capita_C02_Emission)+
      tm_polygons(col="CO2_Emission: ", breaks =breaks, 
                  title = "Per Capita Carbon dioxide (CO2) Emission (in tonnes)" )+ tm_borders()+
      tm_style("albatross")
    tmap_leaflet(capita1)
    
  })
}
shinyApp(ui_capita, server_capita)


#5. production versus consumption-based emissions for every country
#shiny app for production versus consumption-based emissions, plots graphs for all of them 

ui_pvc1<- fluidPage(
  titlePanel("Production vs. Consumption-based Carbon dioxide (CO2) emissions \nover period of years."),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id= "tabs",
                  tabPanel("Locations", id = "Map", 
                           br(), 
                           p("Annual consumption-based emissions are domestic emissions adjusted for trade. 
                           \nIf a country imports goods the Carbon dioxide (CO2) emissions needed to produce such goods are
                            \nadded to its domestic emissions; if it exports goods then this is subtracted."), 
                           selectizeInput("Entity", "Select a location", c(pvc_names) )
                  )
      )
    ),
    
    mainPanel(
      tabsetPanel(type= "tabs",
                  tabPanel("Graph.", 
                           plotOutput(outputId = "pvc1", height=550, width=900), 
                           br(),
                           p("Some countries will only have Production based emission plots, no data was found for their Consumption based emssion.
                           \nAdditionally, collection of Consumption based emssion data gathering started later, so the Consumption based emssion 
                             \nplots will start midway through the graph, instaed of starting at the beginning.")
                           )
      )
    )
  )
)

server_pvc1 <- function(input, output) {
  output$pvc1 <- renderPlot({
    
    pvcf <- function(x){
      data1 <- pvc[pvc$Entity== x, ]
      plot1 <- 
        ggplot()+ 
        geom_line(data1, mapping = aes(x=data1$Year, y=data1$`Production Based CO2_Emission`, color = "darkred"))+
        geom_point(data1, mapping = aes(x=data1$Year, y=data1$`Production Based CO2_Emission`, color = "darkred"))+
        geom_line(data1, mapping = aes(x=data1$Year, y=data1$`Consumption Based CO2_Emission` , color="steelblue"))+
        geom_point(data1, mapping = aes(x=data1$Year, y=data1$`Consumption Based CO2_Emission` , color="steelblue"))+
        labs( x= "Period of Years" ,y="Emissions (in tonnes)",
              title =
              paste0("Production vs. Consumption-based Carbon dioxide (CO2) emissions in ", x, " over period of years."))+
        scale_colour_manual(name = 'Plots', 
                            values =c('darkred'='darkred','steelblue'='steelblue'), labels = c('Production Based','Consumption Based'))
      
      return(plot1)
    } 
    pvc1 <- pvcf(toString(req(input$Entity)))
    plot(pvc1)
    
  })
}
shinyApp(ui_pvc1, server_pvc1)

#So the pitch is - we were hired as consultants to make apps 
#for the Environmental Protection Agency who wanted to see how carbon 
#emissions has impacted those 5 categories that we chose
#We made 2 world heat maps, 1 linear regression plot makers for each 
#country for a period of year, 1  linear regression for regions, 
#and 1 plot based outputting app for production vs consumption

#The 5 things we wrote down on our 1st group project memo =
#1.	current impact of carbon dioxide emissions on the atmosphere
plot1

#2.	efforts to reduce co2 emissions are significant

#annual Co2 emission per country 
# shinny app world heat map for annual c02 emission per country\
#world heat map
shinyApp(ui_annual1, server_annual1)

#shiny app for making linear regression model plots for annual co2 emissions
#over period of years with prediction bands for every country 
#linear regression model
shinyApp(ui_annual2, server_annual2)

#3.	forecast of the temperature changes for the future

# shinny app for change in temperature by regions 
#linear model to present how the tempeatures changed with prediction bands 
#linear regression model
shinyApp(ui_temp, server_temp)

#4.	which country is emitting the most CO2 per capita?

# shinny app world heat map for per capita c02 emission 
#world heat map
shinyApp(ui_capita, server_capita)


#5.	production versus consumption-based emissions for all countries

#shiny app for production versus consumption-based emissions, 
#plots graphs for all countries
shinyApp(ui_pvc1, server_pvc1)


runGitHub( "<NarglesCS/SSC442-Team-Awesome>", "<Sayemlincoln>")

