library(shiny)
library(maps)
library(readxl)
library(tidyverse)
library(stringr)
library(rsconnect)
​
# Read the data file
census_data <- read_xls("h08.xls", range = cell_rows(c(6,58)))
​
# The data is median income data for each state by year
​
# Make it a data frame
census_data <- data.frame(census_data)
​
# Delete the standard error part since it is a map
census_data <- census_data[,-c(3,5,7,9,11,13,15,17,19,21,23,25,27,
                               29,31,33,35,37,39,41,43,45,47,49,51,
                               53,55,57,59,61,63,65,67,69,71,73,75)]
​
# Delete replicated data in the table
census_data <- census_data[,-c(3,8)]
# Delete DC and the whole country
census_data <- census_data[-c(1,10),]
​
# Recreate the column name by year
varnames <- rep(0,dim(census_data)[2]-1)
for (i in (1:dim(census_data)[2]-1)){
  varnames[i] <- as.character(2019-i)
  varnames[i] <- paste("year",varnames[i],sep="")
}
​
varnames <- c("region",as.character(varnames))
colnames(census_data) <- varnames
census_data[,1] <- tolower(census_data[,1])
​
census[,1]
# Define UI for app that draws a graph
ui <- fluidPage(
  
  # App title ----
  titlePanel("Median Income of US"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "year",
                  label = "Map: year of timeline:",
                  min = 1984,
                  max = 2018,
                  value = 2000),
      selectInput(inputId = "region",
                  label = "Select a region",
                  choices = c("alabama","alaska","arizona","arkansas","california","colorado",
                              "connecticut","delaware","florida","georgia","hawaii","idaho",
                              "illinois","indiana","iowa","kansas","kentucky","louisiana",
                              "maine","maryland","massachusetts","michigan","minnesota","mississippi",
                              "missouri","montana","nebraska","nevada","new hampshire","new jersey",
                              "new mexico","new york","north carolina","north dakota","ohio","oklahoma",
                              "oregon","pennsylvania","rhode island","south carolina","south dakota","tennessee",
                              "texas","utah", "vermont","virginia","washington","west virginia",
                              "wisconsin","wyoming"))
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Trend Plot", plotOutput("tsplot")), 
        tabPanel("Map", plotOutput("distPlot")), 
        tabPanel("Bar Plot", plotOutput("bar1"))
      ),
      h6("Developed by Peter Crawford, Wenjun Han and Jingbin Xu.")
      
      # Output: map plot ----
   #    plotOutput(outputId = "distPlot")
      
    )
  )
)
​
# Define the server with graphs
server <- function(input, output) {
  output$tsplot <- renderPlot({
    n_region <- input$region
    plot(order(seq(1984, 2018, 1), decreasing = T)+1983, census_data[census_data$region == n_region, 2:36], 
         main = "",
         xlab = "Year",
         ylab = "Income",
         typle = "l",
         col = "lightblue",
         lwd = 4)
  })
  output$bar1 <- renderPlot({
    
    # Pivot the data longer:
    census_data_long <- census_data %>% pivot_longer(!region, names_to = "year", values_to = "med_income")
    census_data_long$year <- as.integer(str_replace(census_data_long$year, "year", ""))
    
    # Highlight state selected:
    census_data_long <- mutate(census_data_long, is_selected_state = ifelse(region == input$region, "Yes", "No"))
    
    # Bar plot of the State - Income
    ggplot(data = filter(census_data_long, year == input$year), 
           aes(y = reorder(region, med_income), x = med_income, fill = is_selected_state)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("#999999", "#E69F00")) +
      ggtitle("Bar Plot of Median U.S. Income") +
      ylab("Region") +
      xlab("Median Income") +
      labs(fill = "Selected State")
    
  })
  output$distPlot <- renderPlot({
    
    # Find the data of the corresponding input year 
    n_year <- input$year
    n_name <- paste("year",n_year,sep="")
    new_data <- data.frame(cbind(census_data[,1],census_data[, grepl(n_name,names(census_data))]))
    new_data[,2] <- as.numeric(new_data[,2])
    new_data_varnames <- c("region","value")
    colnames(new_data) <- new_data_varnames
    
    # Use the dplyr package to merge the MainStates and StatePopulation files
    MainStates <- map_data("state")
    MergedStates <- inner_join(MainStates, new_data, by = "region")
    # str(MergedStates)
    
    # Create a Choropleth map of the United States
    p <- ggplot()
    p <- p + geom_polygon(data=MergedStates, 
                          aes(x=long, y=lat, group=group, fill = value), 
                          color="white", size = 0.2) 
    
    p <- p + scale_fill_continuous(name=paste("Median Income of: ",as.character(n_year),sep=""), 
                                   low = "yellow", high = "blue",limits = c(15000,90000), 
                                   breaks=c(20000,30000,40000,50000,60000,70000,80000), 
                                   na.value = "grey50") +
      labs(title="State Median Income in the Mainland United States")
    p
    
  })
}
​
shinyApp(ui = ui, server = server)
