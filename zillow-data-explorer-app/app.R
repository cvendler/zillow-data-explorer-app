
# Load the necessary libraries

library(scales)
library(tigris)
library(shinyLP)
library(plotly)
library(leaflet)
library(shinythemes)
library(shiny)
library(tidyverse)

# Set the tigris cache to TRUE so that each of the TIGER shapefiles have to be
# read in from the tigris package only once

options(tigris_use_cache = TRUE)

# Read in the TIGER shapefiles from the tigris library at the state, county, and
# zip code levels; these shapefiles will be used later to make leaflet
# chloropleth maps

states <- states()
counties <- counties()

# Read in each of the rds files I created using the Zillow data in the
# "zillow-data-explorer-script.R" data processing script; these files will be
# used for different parts of the app

zillow_historical_data <- read_rds("zillow_historical_data.rds")

county_forecast_map_data <- read_rds("county_forecast_map_data.rds")

state_forecast_map_data <- read_rds("state_forecast_map_data.rds")

zillow_home_value_forecast_data <- read_rds("zillow_home_value_forecast_data.rds")

# Create lists for various selectInput dropdown menus in the app; these are
# created outside of the actual selectInput functions because the maps/graphs
# require them to be named so that they can be used to create reactive hover
# labels, legend titles, and axis labels (for example, when code is used in this
# way:
# names(yearly_comparisons_metric_choices[which(yearly_comparisons_metric_choices
# == input$yearly_comparisons_metric)]))

yearly_comparisons_metric_choices <- c(
  "Median Zillow Home Value Index" = "median_zhvi_value",
  "Median Value per Square Foot" = "median_value_per_sqft",
  "Percentage of Homes that Increased in Value" = "percent_increased",
  "Percentage of Homes that Decreased in Value" = "percent_decreased"
)

looking_back_y_variable_choices <- c(
  "Median Zillow Home Value Index" = "median_zhvi_value",
  "Median Value per Square Foot" = "median_value_per_sqft",
  "Percentage of Homes that Increased in Value" = "percent_increased",
  "Percentage of Homes that Decreased in Value" = "percent_decreased"
)

looking_back_scope_choices <- c(
  "ZIP Code" = "zip_code",
  "City" = "city_name",
  "County" = "county_name",
  "State" = "state_name"
)

# Create the app's user interface

ui <- fluidPage(
  
  # Using the "shinythemes" package, set the app's theme to "cerulean"; I choose
  # this theme because its main color is light blue, which matches Zillow's
  # logo's color quite well
  theme = shinytheme("cerulean"),
  
  # Set up the ui to use shiny's "navbar" layout in which the page has a
  # navigation bar at the top of the website containing the title, which in this
  # case is "Zillow Data Explorer", and tabs for the Maps, Graphs, and About
  # sections of the app
  
  navbarPage(
    title = "Zillow Data Explorer",
    
    # Create the "Maps" tab to host the leaflet maps
    
    tabPanel(
      title = "Maps",
      
      # Set up the "Maps" tab to have two sub-tabs: "Yearly Comparisons" and
      # "Home Value Forecasts"
      
      tabsetPanel(
        type = "tabs",
        
        # Create the "Yearly Comparisons" tab using the sidebar layout defined
        # by shiny; set it up so that the sidebar has three menus with
        # selectable items (the first being the metric to compare, the second
        # being the geographical unit to compare, and the third being the year
        # of data to explore, which updates as time goes on) and the main panel
        # displays a leaflet map
        
        tabPanel(
          title = "Yearly Comparisons",
          
          sidebarLayout(
            sidebarPanel(
              selectInput(
                inputId = "yearly_comparisons_metric",
                label = "What values do you want to compare?",
                choices = yearly_comparisons_metric_choices
              ),
              
              selectInput(
                inputId = "yearly_comparisons_scope",
                label = "Across what geographical unit?",
                choices = c(
                  "County" = "county_name",
                  "State" = "state_name"
                ),
                selected = "state_name"
              ),
              
              numericInput(
                inputId = "yearly_comparisons_year",
                label = "In what year?",
                value = max(zillow_historical_data$year, na.rm = TRUE),
                min = min(zillow_historical_data$year, na.rm = TRUE),
                max = max(zillow_historical_data$year, na.rm = TRUE)
              )
            ),
            
            mainPanel(
              leafletOutput(
                "yearly_comparisons_map"
              )
            )
          )
        ),
        
        # Create the "Home Value Forecasts" tab using the sidebar layout defined
        # by shiny; set it up so that the sidebar has one menu with selectable
        # items (the geographical unit to compare) and the main panel displays a
        # leaflet map
        
        tabPanel(
          title = "Home Value Forecasts",
          sidebarLayout(
            sidebarPanel(
              selectInput(
                inputId = "forecasts_scope",
                label = "What forecasts do you want to compare?",
                choices = c(
                  "County" = "county_name",
                  "State" = "state_name"
                ),
                selected = "state_name"
              )
            ),
            
            mainPanel(
              leafletOutput(
                "forecasts_map"
              )
            )
          )
        )
      )
    ),
    
    # Create the "Graphs" tab to host the plotly graphs
    
    tabPanel(
      title = "Graphs",
      
      # Set up the "Graphs" tab to have two sub-tabs: "Looking Back" and
      # "Looking Forward"
      
      tabsetPanel(
        type = "tabs",
        
        # Create the "Looking Back" tab using the sidebar layout defined by
        # shiny; set it up so that the sidebar has three menus with selectable
        # items (the first being the metric to compare, to be displayed on the
        # y-axis, the second being the geographical unit to compare, and the
        # third being the specific areas within that kind of geographical unit
        # the user wants to compare, which updates with the user's choice in the
        # geographical unit section) and the main panel displays a plotly graph
        
        tabPanel(
          title = "Looking Back",
          sidebarLayout(
            sidebarPanel(
              selectInput(
                inputId = "looking_back_y_variable",
                label = "Dependent Variable:",
                choices = looking_back_y_variable_choices
              ),
              
              selectInput(
                inputId = "looking_back_scope",
                label = "Geographical Unit:",
                choices = looking_back_scope_choices,
                selected = "state_name"
              ),
              
              selectizeInput(
                inputId = "looking_back_areas",
                label = "Area(s) to Display:",
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Click to search")
              )
            ),
            
            mainPanel(
              plotlyOutput("looking_back_plot")
            )
          )
        ),
        
        # Create the "Looking Forward" tab using the sidebar layout defined by
        # shiny; set it up so that the sidebar has two menus with selectable
        # items (the first being the geographical unit to compare and the second
        # being the specific areas within that kind of geographical unit the
        # user wants to compare, which updates with the user's choice in the
        # geographical unit section) and the main panel displays a plotly graph
        
        tabPanel(
          title = "Looking Forward",
          sidebarLayout(
            sidebarPanel(
              selectInput(
                inputId = "looking_forward_scope",
                label = "Geographical Unit:",
                choices = c(
                  "City" = "City",
                  "County" = "County",
                  "State" = "State"
                ),
                selected = "State"
              ),
              
              selectizeInput(
                inputId = "looking_forward_areas",
                label = "Area(s) to Display:",
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Click to search")
              )
            ),
            
            mainPanel(
              plotlyOutput("looking_forward_plot")
            )
          )
        )
      )
    ),
    
    # Create the "About" tab to host text information about the app including
    # goals, directions, acknowledgements, and a link to the source code
    
    tabPanel(
      title = "About",
      
      mainPanel(
        panel_div(
          class_type = "primary",
          panel_title = "Goals",
          content = 'Ever since I was little, I have been interested in architecture, homes, and real estate.  I share this interest with both of my parents, who, in addition to being attorneys, manage various properties in the Los Angeles and Orange County areas in order to generate additional income.  Both my parents and I tend to be more Redfin users than Zillow ones, and I wondered why.  I explored what kinds of data visualizations Redfin offers its consumers and found that it has something called "Redfin Data Center," which includes interactive graphs that plot metrics like median sale price, home sales, price per square foot, and more.  These graphs can be customized to show trends from the zip code level to the national one, to show trends for different types of homes, to show month over month or year over year trends, and to show trends for specified periods of time.  I then went to Zillow\'s equivalent or competing data center, "Zillow Research," to see whether or not Zillow is providing its consumers with the same kinds of accessible, informative, and interactive data visualizations.  Zillow has a plethora of data available for the public to download (indeed even more than Redfin has) in its "Data" tab as well as some static graphics in its "Visuals" tab.  It has a few interactive data tables, some of which include lines showing overall trends in its "Markets" tab, but their interactivity is very limited.  You can reorder the rows of the data tables, hover over the lines for temporal and numerical information for those data tables that have lines, and search for specific metropolitan areas to see only the rows for those areas, but the data for each specific area is difficult to compare.  Because each exists in its own row separate from the rest, relative scales for metrics like Zillow Home Value Index are indistinguishable from one another.  Both New York and Philadelphia appear to follow the exact same trend even though New York’s ZHVI is almost twice that of Philadelphia.  The only metrics included are population, Zillow Home Value Index, Zillow Rent Index, for-sale inventory, and negative equity.  You cannot customize these data tables to see trends at any other level other than the metropolitan and national ones, and nowhere to be found are graphics for other metrics like price per square foot.  This absence of up-to-date, visualized, and interactive data is a major shortcoming of Zillow\'s: such raw data is accessible only to those who have the tools to understand and synthesize it into something comprehensible, a story; to those who do not, raw data is rather useless, even alienating.  I created this app with the intention of marketing it to Zillow so that it can better match up against its major competition, Redfin.  Zillow is behind the curve in its consumer-facing data visualizations, crippling it as a go-to source of real estate information.  I hope to help to launch Zillow into the future of data visualization with this app.'
        ),
        panel_div(
          class_type = "primary",
          panel_title = "Directions",
          content = "Use this app to explore various measures driving real estate investment decisions across the nation.  The first tab of the app contains two maps which help you to visualize the data: one for viewing historical data by year for median Zillow Home Value Index, median price per square foot, percentage of homes that increased in value, and percentage of homes that decreased in value in a given year; and one for viewing the forecasted year over year percent change in Zillow Home Value Index as estimated by the analysts at Zillow.  These maps can show estimates at both the county and state levels.  The second tab hosts two graphs for visualizing the data.  The first of these graphs again shows historical trends.  You can compare the median Zillow Home Value Index, median price per square foot, percentage of homes that increased in value, and percentage of homes that decreased in value for specific zip codes, counties, or states against other zip codes, counties, or states over the time period from 1996 to now.  The second graph compares forecasted year over year percent changes in Zillow Home Value Index for selected zip codes, counties, or states and depicts these values as columns ordered from largest forecasted percent increase to largest forecasted percent decrease.  Happy real estate data exploring!"
        ),
        panel_div(
          class_type = "primary",
          panel_title = "Acknowledgements",
          content = p(
            "The data driving this app comes from ",
            a('"Zillow Research"', href = "https://www.zillow.com/research/data/"),
            'on which Zillow has made their real estate data going back to 1996 publicly available.  I want to give a special thanks
            to David Kane for his teaching and guidance, without which I could not have made this app.  I also want to thank Kyle   
            Walker, author and maintaner of the R package "tigris," from which this app greatly benefited.'
          )
          ),
        panel_div(
          class_type = "primary",
          panel_title = "Source Code",
          content = p(
            "Interested in seeing the code that made this app?  Take a look at the GitHub repository found ",
            a("here.", href = "https://github.com/cvendler/zillow-data-explorer-app")
          )
        )
          )
    )
)
)
