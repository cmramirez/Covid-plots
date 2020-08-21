###  CAses versus Deaths #####
library(tidyverse)
library(shiny)
library(utils)
library(dplyr)
library(data.table)
library(ggplot2)
#read the Dataset sheet into “R”. The dataset will be called "data" from the ECDC.
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")

#totcase <- aggregate(x= data$cases, by=list(data$countriesAndTerritories), FUN=cumsum)



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
#usall <- B[which(B$countriesAndTerritories=="United_States_of_America")]

regeu <- lm(europe2$death100 ~ europe2$case100)

eu <- europe2 %>%
  mutate(cum_cases2 = cumsum(cum_cases), cum_death2=cumsum(cum_death),cum_pop=cumsum(popData2019))



eu2 <-setDT(eu)[, .SD[which.max(cum_cases2)]]

eu2$countriesAndTerritories <- "EU"
eu2$case100 <- eu2$cum_cases2*100000/eu2$cum_pop
eu2$death100 <- eu2$cum_death2*100000/eu2$cum_pop
eu2$population <-eu2$cum_pop
eu2$state <-eu2$countriesAndTerritories


eu3 <- data.frame("death100"=eu2$death100, "case100"=eu2$case100, "state"=eu2$state, 
                  "population"=eu2$population, "ehit"=eu2$hit, "location"=eu2$location)
eu4 <-data.frame("death100"=europe2$death100, "case100"=europe2$case100, "state"=europe2$state, 
               "population"=europe2$population,"ehit"=europe2$hit,"location"=europe2$location)

eu5 <-data.frame("death100"=europe3$death100, "case100"=europe3$case100, "state"=europe3$state, 
                 "population"=europe3$population,"ehit"=europe3$hit,"location"=europe3$location,
                 "continent"=europe3$continent)

us <-data.frame("death100"=usall$death100, "case100"=usall$case100, "state"=usall$state, 
                "population"=usall$population,"ehit"=usall$hit)
row <-  data.frame("death100"=B$death100, "case100"=B$case100, "state"=B$state, 
                   "population"=B$population, "ehit"=B$hit,"location"=B$location, "continent"=B$continent)


row1 <-as_tibble(row)
row2 <-row1 %>% filter(!is.na(death100), state !="United_States_of_America", death100 >0)
regrow <-lm(row2$death100 ~ row2$case100)

regeu5 <-lm(eu5$death100 ~eu5$case100)

data2 <- read.csv("https://covidtracking.com/api/v1/states/current.csv", na.strings = "", fileEncoding = "UTF-8-BOM")

current <-read.csv("~/Box/Shiny/current2.csv")

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


usa1 <-as.tibble(usa)
usa2 <-usa1 %>% filter(!is.na(death100), state !="NY", state !="NJ", state !="MA",
                state !="CT")

regus <- lm(usa$death100~usa$case100 )
regus2 <- lm(usa2$death100~usa2$case100 )

#dataeuus <-rbind(eu3,eu4,us, usa2)
dataeuus <-rbind(eu4, usa2)
row.names(dataeuus) <-dataeuus$state


##ggplot  DOES NOT INCLUDE NY, NJ MA CT


df <- ggplot(dataeuus, aes(x=case100, y=death100, color=location, label=rownames(dataeuus))) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              #se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE)
require(ggrepel)
set.seed(42)
df + geom_text_repel(aes(label = rownames(dataeuus)),
                     size = 3.5) +  labs(title="USA (minus NY, NJ, CT and MA) versus EU", x="Cases per 100k", 
                                         y="Deaths per 100k")




radius <- sqrt( dataeuus$population/ pi )
symbols(dataeuus$death100, dataeuus$case100, circles=radius, inches=0.35, fg="white", bg="red", xlab="Cases per 100k", 
        main="Total Cases per 100k vs Total Deaths per 100k",ylab="Deaths per 100k")
#abline(regus2)
#abline(regeu)
text(dataeuus$death100, dataeuus$case100, dataeuus$state, cex=0.5)


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


df2 <- ggplot(dataeuus2, aes(x=case100, y=death100, color=location, label=rownames(dataeuus2))) + geom_point(shape=1) +
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


regeuus <- lm(dataeuus5$death100 ~ dataeuus5$case100 + dataeuus5$continent)
regeuus2 <- lm(dataeuus5$death100 ~ dataeuus5$case100 )

#radius <- sqrt( dataeuus$population/ pi )


###### US versus rest of the world####

dataeuus3 <-rbind(row2, usa)
row.names(dataeuus3) <-dataeuus3$state
##ggplot


df3 <- ggplot(dataeuus3, aes(x=case100, y=death100, color=location, label=rownames(dataeuus3))) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              #se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE)
require(ggrepel)
set.seed(42)
df3 + geom_text_repel(aes(label = rownames(dataeuus3)),
                      size = 3.5) +  labs(title="USA (all states) versus Rest of the World", x="Cases per 100k", 
                                          y="Deaths per 100k")


##by continent
dataeuus3 <-rbind(row2, usa)
row.names(dataeuus3) <-dataeuus3$state
##ggplot


df3 <- ggplot(dataeuus3, aes(x=case100, y=death100, color=continent, label=rownames(dataeuus3))) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              #se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE)
require(ggrepel)
set.seed(42)
df3 + geom_text_repel(aes(label = rownames(dataeuus3)),
                      size = 3.5) +  labs(title="USA (all states) versus Rest of the World", x="Cases per 100k", 
                                          y="Deaths per 100k")



###US - early states vs >10/100k deaths


dataeuus4 <-rbind(row2, usa2)
row.names(dataeuus4) <-dataeuus4$state
##ggplot


df4 <- ggplot(dataeuus4, aes(x=case100, y=death100, color=location, label=rownames(dataeuus4))) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              #se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE)
require(ggrepel)
set.seed(42)
df4 + geom_text_repel(aes(label = rownames(dataeuus4)),
                      size = 3.5) +  labs(title="USA (Minus NY, NJ, MA and CT) versus Rest of the World with >=10cases/100k", x="Cases per 100k", 
                                          y="Deaths per 100k")



car::ncvTest(regus2)
lmtest::bptest(regus2)

car::ncvTest(regrow)
lmtest::bptest(regrow)

car::ncvTest(regeu5)
lmtest::bptest(regeu5)


library(gvlma)
#versy dignifiacnt heteroscedasticisty

rowBCMod <- caret::BoxCoxTrans(row2$death100)
print(rowBCMod)
row3 <- cbind(row2, rowBCMod = predict(rowBCMod, row2$death100))
regrow2 <- lm( rowBCMod~ case100, data = row3)
gvlma(regrow2)

euBC <- caret::BoxCoxTrans(eu5$death100)
print(euBC)

row3 <- cbind(row2, rowBCMod = predict(rowBCMod, row2$death100))
regrow2 <- lm( rowBCMod~ case100, data = row3)
gvlma(regrow2)


###########
radius <- sqrt( dataeuus$population/ pi )




symbols(dataeuus$case100, dataeuus$death100, circles=radius, inches=0.35, fg="white", bg="red", xlab="Cases per 100k", 
        main="Total Cases per 100k vs Total Deaths per 100k",ylab="Deaths per 100k")
abline(regus2)
text(dataeuus$case100, dataeuus$death100, dataeuus$state, cex=0.5)



radius <- sqrt( dataeuus$population/ pi )
symbols(dataeuus$death100, dataeuus$ehit, circles=radius, inches=0.35, fg="white", bg="red", xlab="Death per 100k", 
        main="Total Cases x 10 as Percentage of State Population",ylab="Cases as percent of Population")

text(dataeuus$death100, dataeuus$ehit, dataeuus$state, cex=0.5)


library(shiny)


#Define UI for application that plots features of movies
ui <- fluidPage(
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs: Select variables to plot
    sidebarPanel(
      # Select variable for y-axis
      selectInput(inputId = "y", label = "Y-axis:",
                  choices = c("case100", "death100", "state"),
                  selected = "cases100"),
      # Select variable for x-axis
      selectInput(inputId = "x", label = "X-axis:",
                  choices = c("case100", "death100", "state"),
                  selected = "death100")
    ),
    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  ) 
  
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = dataeuus, aes_string(x = input$x, y = input$y)) +
      geom_point()
    
  }) }

# Create the Shiny app object
shinyApp(ui = ui, server = server)


library(rsconnect)
rsconnect::setAccountInfo(name='covidvisuals', token='6F26A6BFE755D6C6915CBCB61DDD6672', 
                          secret='Wod8LEZK8e4OW5u0OSDWsCjGSdTLs8/BImKqDM36')

#rsconnect::deployApp('~/Box/Shiny')


deployApp()






###export data


write.csv(usa, "usa_all.csv")
write.csv(usa, "datae.csv")














