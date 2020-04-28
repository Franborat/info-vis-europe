#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Install packages if needed

if (!require(shiny)) install.packages('shiny')
library(shiny)

if (!require(shinydashboard)) install.packages('shinydashboard')
library(shinydashboard)

if (!require(DT)) install.packages('DT')
library(DT)

if (!require(oce)) install.packages('oce')
library(oce)

if (!require(leaflet)) install.packages('leaflet')
library(leaflet)

if (!require(plotly)) install.packages('plotly')
library(plotly)


## Load dataset and configure it ----
# europe <- read.csv("europe.csv", row.names=1)

# Restore the object
europeSp <- readRDS(file = "../data/europeSp.rds")
table <- europeSp
rownames(table@data) <- table@data$name

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  
  dashboardHeader(title = "EuropeApp"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Hierarchical Clustering", tabName = "1", icon = icon("dashboard")),
      menuItem("Dynamic Dataset Filtering", tabName = "2", icon = icon("th")),
      menuItem("Correlations", tabName = "3", icon = icon("th"))
      
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      
      
      #Fourth tab content
      tabItem(tabName = "1",
              sidebarLayout(
                sidebarPanel(
                  helpText("In this view, a Europe map is displayed to interect in between the data.
                           The interaction in this view is used in two ways. 1st: Select an input variable to
                           be displayed and see how it is distributed in the different countries. 2nd: Can be activated selecting the quantile box. 
                           Select the number of quantiles, and see how the quantilization for the selected variable is displayed.
                           *put the mouse pointer on top of the different countries to see a label with the exact value of the variable selected"),
                  
                  selectInput("var",
                              label = "Choose a variable to display",
                              # choices = c('Area', 'Inflation', 'Life.expect', 'Military', 'Pop.growth', 'Unemployment','GDP'),
                              # selected = "Area"),
                              choices = c('gdp', 'gdp_pps', 'population', 'fertility', 'life_expectancy', 'tertiary_education','high_tech_employment', 'unemployment', 'cows_milk'),
                              selected = 'gdp'),
                  
                  sliderInput("quantile", 
                              label = "Number of quantiles",
                              min = 1, max = 7, value = 3),
                  
                  checkboxInput("legend", "Show legend", TRUE),
                  
                  checkboxInput("showQuantile", "Quantile", FALSE)
                  
                  ),
                mainPanel(
                  leafletOutput("mymap")
                )
                )
      ),
      
      
      # First tab content
      tabItem(tabName = "2",
              sidebarLayout(
                sidebarPanel(
                  helpText("Dynamic filtering of information. Data can be filtered in two ways. At first, a checkbox is
                           provided to choose the variables (columns) shown in the table. Secondly, another checkbox is
                           provided to choose which countries (rows) are shown in the table. In the second tab of this view,
                           we can interact with the heat map of the dataset. This map can be filtered using the same checkboxes.
                           Also, they can configure the number of clusters for the output dendrogram that the heat map also plots."),
                  checkboxGroupInput("vars", "Show variables to use:", names(table@data[,c(3,4,5,6,7,8,9,10,11)]), selected = names(table@data[,c(3,4,5,6,7,8,9,10,11)]), inline = TRUE),
                  checkboxGroupInput("countries", "Show countries:", rownames(table@data), selected = rownames(table@data), inline = TRUE),
                  sliderInput("k_col",
                              label = "Number of centers for the countries dendrogram",
                              min = 1, max = 7, value = 4),
                  sliderInput("k_row",
                              label = "Number of centers for the variables dendrogram",
                              min = 1, max = 5, value = 2)
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Dataset Information", DT::dataTableOutput("table")),
                    tabPanel("Heat Map Plot", plotlyOutput("heatmap"))
                  )
                )
              )

      ),

      tabItem(tabName = "3",
              sidebarLayout(
                sidebarPanel(
                  helpText("In this view, a scatter-plot and a linear model between two of the variables is displayed. Below the graph, the most
                           important values are displayed. Select two of the variables to see the different results"),
                  selectInput("var1",
                              label = "Choose first variable",
                              # choices = c('Area', 'Inflation', 'Life.expect', 'Military', 'Pop.growth', 'Unemployment','GDP'),
                              # selected = "Area"),
                              choices = c('gdp', 'gdp_pps', 'population', 'fertility', 'life_expectancy', 'tertiary_education','high_tech_employment', 'unemployment', 'cows_milk'),
                              selected = 'gdp'),
                  selectInput("var2",
                              label = "Choose second variable",
                              # choices = c('Area', 'Inflation', 'Life.expect', 'Military', 'Pop.growth', 'Unemployment','GDP'),
                              # selected = "Area"),
                              choices = c('gdp', 'gdp_pps', 'population', 'fertility', 'life_expectancy', 'tertiary_education','high_tech_employment', 'unemployment', 'cows_milk'),
                              selected = 'fertility'),
                  checkboxInput("confidence", "Confidence Interval", value = FALSE, width = NULL)
                ),
                mainPanel(
                
                  plotOutput("corrplot"),
                  tableOutput("corrplotValues")
                )
              )   
      ))
  )
)

)
