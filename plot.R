###  CAses versus Deaths #####
library(tidyverse)
library(shiny)
library(utils)
library(dplyr)
library(data.table)
library(ggplot2)
library(httr)
library(RCurl)


#read the Dataset sheet into “R”. The dataset will be called "data" from the ECDC.
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")


a <- data %>%
  group_by(data$countriesAndTerritories) %>%
  mutate(cum_cases = cumsum(cases), cum_death=cumsum(deaths) )



B <-setDT(a)[, .SD[which.max(cum_cases)],by=countriesAndTerritories]

B$case100 <- B$cum_cases*100000/B$popData2019
B$death100 <- B$cum_death*100000/B$popData2019
B$population <-B$popData2019
B$state <-B$countriesAndTerritories
B$hit <- B$cum_cases*10/B$popData2019
B$location <-"RoW"
B$continent <-B$continentExp


europe2 <- B[which(B$countriesAndTerritories=="France" | B$countriesAndTerritories=="Germany" | 
                     B$countriesAndTerritories=="Italy" |B$countriesAndTerritories=="Spain" | 
                     B$countriesAndTerritories=="United_Kingdom" |  B$countriesAndTerritories=="Belgium" | 
                     B$countriesAndTerritories=="Portugal" | B$countriesAndTerritories=="Sweden" | 
                     B$countriesAndTerritories=="Netherlands" |  B$countriesAndTerritories=="Denmark" |
                     B$countriesAndTerritories=="Finland" |  B$countriesAndTerritories=="Norway" |
                     B$countriesAndTerritories=="Austria" |  B$countriesAndTerritories=="Ireland" )]
europe2$location <- "EU"

europe3 <-B[which(B$continentExp=="Europe" & B$death100 >0  ) ]


regeu <- lm(europe2$death100 ~ europe2$case100)

eu <- europe2 %>%
  mutate(cum_cases2 = cumsum(cum_cases), cum_death2=cumsum(cum_death),cum_pop=cumsum(popData2019))



eu4 <-data.frame("death100"=europe2$death100, "case100"=europe2$case100, "state"=europe2$state, 
               "population"=europe2$population,"ehit"=europe2$hit,"location"=europe2$location,
               "continent"=europe2$continent)

eu5 <-data.frame("death100"=europe3$death100, "case100"=europe3$case100, "state"=europe3$state, 
                 "population"=europe3$population,"ehit"=europe3$hit,"location"=europe3$location,
                 "continent"=europe3$continent)



data2 <- read.csv(("https://covidtracking.com/api/v1/states/current.csv"), na.strings = "", fileEncoding = "UTF-8-BOM")

library(httr)
library(RCurl)

current <-read.csv(text=getURL("https://raw.githubusercontent.com/cmramirez/Covid-plots/master/state_pop.csv"))


population <-current$population
data3 <-cbind(data2,population)
data3$case100 <- data3$positive*100000/data3$population
data3$death100 <- data3$death*100000/data3$population
data3$hit <-data3$positive*10/data3$population
data3$location <-"USA"
data3$continent <-"USA"

usa <-data.frame("death100"=data3$death100, "case100"=data3$case100, "state"=data3$state, 
                "population"=data3$population, "ehit"=data3$hit, "location"=data3$location, 
                "continent"=data3$continent)


usa1 <-as_tibble(usa)
usa2 <-usa1 %>% filter(!is.na(death100), state !="NY", state !="NJ", state !="MA",
                state !="CT")




dataeuus <-rbind(eu4, usa2)
row.names(dataeuus) <-dataeuus$state


##ggplot  DOES NOT INCLUDE NY, NJ MA CT


df <- ggplot(dataeuus, aes(x=case100, y=death100, color=continent, label=rownames(dataeuus))) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              #se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE)
require(ggrepel)
set.seed(42)
df + geom_text_repel(aes(label = rownames(dataeuus)),
                     size = 3.5) +  labs(title="USA (minus NY, NJ, CT and MA) versus EU", x="Cases per 100k", 
                                         y="Deaths per 100k")





#all euopean continent

dataeuus6 <-rbind(eu5, usa2)
row.names(dataeuus6) <-dataeuus6$state
##ggplot


df6 <- ggplot(dataeuus6, aes(x=case100, y=death100, color=continent, label=rownames(dataeuus6))) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              #se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE)
require(ggrepel)
set.seed(42)
df6 + geom_text_repel(aes(label = rownames(dataeuus6)),
                      size = 3.5) +  labs(title="USA (without NY, NJ, CT, MA) versus All Countries in Europe", x="Cases per 100k", 
                                          y="Deaths per 100k")







####### Incldue NY NJ etc #####

dataeuus2 <-rbind(eu4, usa)
row.names(dataeuus2) <-dataeuus2$state
##ggplot


df2 <- ggplot(dataeuus2, aes(x=case100, y=death100, color=continent, label=rownames(dataeuus2))) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              #se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE)
require(ggrepel)
set.seed(42)
df2 + geom_text_repel(aes(label = rownames(dataeuus2)),
                     size = 3.5) +  labs(title="USA (all states) versus EU", x="Cases per 100k", 
                                         y="Deaths per 100k")


### ALL Europe countires even those not in EU

dataeuus5 <-rbind(eu5, usa)
row.names(dataeuus5) <-dataeuus5$state
##ggplot


df5 <- ggplot(dataeuus5, aes(x=case100, y=death100, color=continent, label=rownames(dataeuus5))) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              #se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE)
require(ggrepel)
set.seed(42)
df5 + geom_text_repel(aes(label = rownames(dataeuus5)),
                      size = 3.5) +  labs(title="USA (all states) versus All Countries in Europe", x="Cases per 100k", 
                                          y="Deaths per 100k")






###########################################################


datafile <-list(dataeuus5, dataeuus2, dataeuus, dataeuus6)
library(shiny)


#Define UI for application that plots cases versus deaths
ui <- fluidPage(
  headerPanel("Cases per 100k versus Deaths per 100k"),
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs: Select variables to plot
    sidebarPanel(
      # Select dataset for US
      selectInput(inputId = "dataset", label = "Pick a Dataset",
                  choices = c("USA (all states) versus Continental Europe"="1",
                              "USA (all states) versus Western Europe"="2",
                              "USA (minus NY, NJ, MA, CT) versus Western Europe"="3", 
                              "USA (minus NY, NJ, MA, CT) versus Continental Europe"="4"), 
                             
                  selected = "1"),
     
      
    
      
    ),
    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  ) 
  
)





# Define server function required to create the scatterplot
server <- function(input, output,session) {
  
  outVar <- reactive({
    temp <- datafile[[as.numeric(input$dataset)]]
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    
    
    
    plt <- ggplot(outVar(), aes(x=case100, y=death100, color=continent, label=rownames(outVar()))) + geom_point(shape=1) +
      scale_colour_hue(l=50) + # Use a slightly darker palette than normal
      geom_smooth(method=lm,   # Add linear regression lines
                  #se=FALSE,    # Don't add shaded confidence region
                  fullrange=TRUE)
    require(ggrepel)
    set.seed(42)
    plt + geom_text_repel(aes(label = rownames(outVar())),
                          size = 3.5) +  labs(title="", x="Cases per 100k", 
                                              y="Deaths per 100k")
    
  }) }



shinyApp(ui = ui, server = server)






###############################################















