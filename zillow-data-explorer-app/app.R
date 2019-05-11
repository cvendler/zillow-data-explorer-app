
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
        # displays a leaflet map; add help text explaning variable names

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
              ),

              p(helpText("Due to the size of the data and the number of shapefiles, maps may take a few minutes to load.  Please examine counties in only one map (either this one or the one in the next tab) at a time; switch back to state if you want to examine counties in the other map.  Hover over the areas for more information.  Thanks for your patience and enjoy!")),
              p(helpText('Median Zillow Home Value Index: "A smoothed, seasonally adjusted measure of the median estimated home value across a given region and housing type."  Here, the housing type is all homes.')),
              p(helpText('Median Value per Square Foot: "Median of the value of all homes per square foot. This number is calculated by taking the estimated home value for each home in a given region and dividing it by the home’s square footage."')),
              p(helpText('Percentage of Homes that Increased in Value: "The percentage of homes in [a] given region with values that have increased in the past year."')),
              p(helpText('Percentage of Homes that Decreased in Value: "The percentage of homes in [a] given region with values that have decreased in the past year."')),
              p(helpText('"All homes" includes "single-family, condominium and co-operative homes with a county record."')),
              p(helpText("If the geographical unit is anything other than zip code, each area's value for each metric corresponds to the median value of that metric across zip codes within that area.")),
              p(helpText("From top to bottom in the legends, colors correspond to the 0th-5th, 5th-25th, 25th-50th, 50th-75th, 75th-95th, and 95th-100th percentiles for each metric of comparison.")),
              p(helpText(
                "Variable descriptions courtesy of ",
                a('"Zillow Research."', href = "https://www.zillow.com/research/data/")
              ))
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
        # leaflet map; add help text explaning variable names

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
              ),

              p(helpText("Due to the size of the data and the number of shapefiles, maps may take a few minutes to load.  Please examine counties in only one map (either this one or the one in the previous tab) at a time; switch back to state if you want to examine counties in the other map.  Hover over the areas for more information.  Thanks for your patience and enjoy!")),
              p(helpText('Home Value Forecasts: Mapped are Zillow\'s Home Value Forecasts, one-year forecasts predicted in the most recent month available of the Zillow Home Value Index (a "smoothed, seasonally adjusted measure of the median estimated home value across a given region and housing type" where the housing type here is all homes).')),
              p(helpText("From top to bottom in the legends, colors correspond to the 0th-5th, 5th-25th, 25th-50th, 50th-75th, 75th-95th, and 95th-100th percentiles.")),
              p(helpText(
                "Variable descriptions courtesy of ",
                a('"Zillow Research."', href = "https://www.zillow.com/research/data/")
              ))
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
        # geographical unit section) and the main panel displays a plotly graph;
        # add help text explaning the page's functionality and variable names

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
              ),

              p(helpText('Here, you can examine Zillow\'s historical data for various metrics at the zip code, city, county, and state levels.  Compare trends for various zip codes, cities, counties, or states by picking areas to display.  Given the size of the data, it may take a few seconds for the options in "Area(s) to Display" to update.  Hover over the lines for more information.  Thanks for your patience and enjoy!')),
              p(helpText('Median Zillow Home Value Index: "A smoothed, seasonally adjusted measure of the median estimated home value across a given region and housing type."  Here, the housing type is all homes.')),
              p(helpText('Median Value per Square Foot: "Median of the value of all homes per square foot. This number is calculated by taking the estimated home value for each home in a given region and dividing it by the home’s square footage."')),
              p(helpText('Percentage of Homes that Increased in Value: "The percentage of homes in [a] given region with values that have increased in the past year."')),
              p(helpText('Percentage of Homes that Decreased in Value: "The percentage of homes in [a] given region with values that have decreased in the past year."')),
              p(helpText('"All homes" includes "single-family, condominium and co-operative homes with a county record."')),
              p(helpText("If the geographical unit is anything other than zip code, each area's value for each metric corresponds to the median value of that metric across zip codes within that area.")),
              p(helpText(
                "Variable descriptions courtesy of ",
                a('"Zillow Research."', href = "https://www.zillow.com/research/data/")
              ))
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
        # geographical unit section) and the main panel displays a plotly graph;
        # add help text explaning the page's functionality and variable names

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
              ),

              p(helpText('Here, you can examine Zillow\'s forecast data at the city, county, and state levels.  Compare Zillow\'s Home Value Forecasts for various cities, counties, or states by picking areas to display.  Given the size of the data, it may take a few seconds for the options in "Area(s) to Display" to update.  Hover over the points for more information.  Thanks for your patience and enjoy!')),
              p(helpText('Zillow Home Value Forecasts: One-year forecasts predicted in the most recent month available of the Zillow Home Value Index (a "smoothed, seasonally adjusted measure of the median estimated home value across a given region and housing type" where the housing type here is all homes).')),
              p(helpText(
                "Variable descriptions courtesy of ",
                a('"Zillow Research."', href = "https://www.zillow.com/research/data/")
              ))
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
          content = 'Ever since I was little, I have been interested in architecture, homes, and real estate.  I share this interest with both of my parents, who, in addition to being attorneys, manage various properties in the Los Angeles and Orange County areas in order to generate additional income.  Both my parents and I tend to be more Redfin users than Zillow ones, and I wondered why.  I explored what kinds of data visualizations Redfin offers its consumers and found that it has something called "Redfin Data Center," which includes interactive graphs that plot metrics like median sale price, home sales, price per square foot, and more.  These graphs can be customized to show trends from the zip code level to the national one, to show trends for different types of homes, to show month over month or year over year trends, and to show trends for specified periods of time.  I then went to Zillow\'s equivalent or competing data center, "Zillow Research," to see whether or not Zillow is providing its consumers with the same kinds of accessible, informative, and interactive data visualizations.  Zillow has a plethora of data available for the public to download (indeed even more than Redfin has) in its "Data" tab as well as some static graphics in its "Visuals" tab.  It has a few interactive data tables in its "Markets" tab, some of which include lines showing overall trends, but their interactivity is very limited.  You can reorder the rows of the data tables, hover over the lines for temporal and numerical information for those data tables that have lines, and search for specific metropolitan areas to see only the rows for those areas, but trends for different areas are difficult to compare.  Because each area exists in its own row separate from the rest, different scales for metrics like Zillow Home Value Index are indistinguishable from one another.  Both New York and Philadelphia appear to follow the exact same trend even though New York’s Zillow Home Value Index is almost twice that of Philadelphia.  The only metrics included are population, Zillow Home Value Index, Zillow Rent Index, for-sale inventory, and negative equity.  You cannot customize these data tables to see trends at any level other than the metropolitan and national ones, and nowhere to be found are graphics for other metrics like value per square foot.  This absence of up-to-date, visualized, and interactive data is a major shortcoming of Zillow\'s: raw data is accessible only to those who have the tools to understand and synthesize it into something comprehensible, a story; to those who do not, raw data is rather useless, even alienating.  I created this app with the intention of marketing it to Zillow so that it can better match up against its major competition, Redfin.  Zillow is behind the curve in its consumer-facing data visualizations, crippling it as a go-to source of real estate information.  I hope to help to launch Zillow into the future of data visualization with this app.'
        ),
        panel_div(
          class_type = "primary",
          panel_title = "Directions",
          content = "Use this app to explore various measures driving real estate investment decisions across the nation.  The first tab of the app contains two maps which help you visualize the data: one for viewing historical data by year for median Zillow Home Value Index, median value per square foot, percentage of homes that increased in value, and percentage of homes that decreased in value in a given year; and one for viewing the forecasted year over year percent change in Zillow Home Value Index as estimated by the analysts at Zillow.  These maps can show values at both the county and state levels.  The second tab hosts two graphs for visualizing the data.  The first of these graphs again shows historical trends.  You can compare the median Zillow Home Value Index, median value per square foot, percentage of homes that increased in value, and percentage of homes that decreased in value across zip codes, cities, counties, or states over the time period from 1996 to now.  The second graph compares forecasted year over year percent changes in Zillow Home Value Index for selected cities, counties, or states and depicts these values as columns ordered from largest forecasted percent increase to largest forecasted percent decrease.  Happy real estate data exploring!"
        ),
        panel_div(
          class_type = "primary",
          panel_title = "Acknowledgements",
          content = p(
            "The data driving this app comes from ",
            a('"Zillow Research,"', href = "https://www.zillow.com/research/data/"),
            'on which Zillow has made their real estate data going back to 1996 publicly available.  I want to give a special thanks
            to David Kane for his teaching and guidance, without which I could not have made this app.  I also want to thank Kyle   
            Walker, author and maintainer of the R package "tigris," from which this app greatly benefited.'
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

# Create the app's server

server <- function(input, output, session) {

  # Create a string that will paste onto various values in the "Yearly
  # Comparisons" tab's legends and labels that is reactive to whether the chosen
  # metric corresponds to a dollar value or a percent value

  yearly_comparisons_unit <- reactive({
    if (input$yearly_comparisons_metric %in% c("median_zhvi_value", "median_value_per_sqft")) {
      yearly_comparisons_unit <- "$"
    } else if (input$yearly_comparisons_metric %in% c("percent_increased", "percent_decreased")) {
      yearly_comparisons_unit <- "%"
    }
  })

  # Create the output for the "Yearly Comparisons" tab in the UI

  output$yearly_comparisons_map <- renderLeaflet({

    # Set up an "if-else" structure so that the map can go down three paths: one
    # if the geographical unit selected is "Zip Code," one if it is "County,"
    # and one if it is "State"

    # Begin if statement for "County"

    if (input$yearly_comparisons_scope == "county_name") {

      # Create an object to be joined to the county shapefile data by filtering
      # the data to keep observations only in the selected year. Then, group the
      # data by "year", "county_code", and "county_name" and create a new
      # variable called "fill_variable" using the "summarize" and "median"
      # functions so that each county code/name corresponds to one value
      # (determined by the user's metric selection) to be depicted on the map
      # based on the three user selections on the tab

      data_to_geo_join <- zillow_historical_data %>%
        filter(year == input$yearly_comparisons_year) %>%
        group_by(year, county_code, county_name) %>%
        summarize(fill_variable = median(get(input$yearly_comparisons_metric), na.rm = TRUE))

      # Using tigris' "geo_join" function, join the county shapefiles with the
      # dataframe that was just created so that it can be plotted on a
      # chloropleth map

      data_to_map <- geo_join(
        spatial_data = counties, data_frame = data_to_geo_join,
        by_sp = "GEOID", by_df = "county_code"
      )

      # Create colorbins that are reactive based on quantiles of the data, which
      # will be used with a diverging color palette to show deviation from the
      # data's median on the chloropleth map using rounded estimates of the min,
      # 5th, 25th, 50th, 75th, 95th, and max quantiles of the "fill_variable"

      bins <- c(
        floor(min(data_to_map$fill_variable, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.05, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.25, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.5, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.75, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.95, na.rm = TRUE)),
        ceiling(max(data_to_map$fill_variable, na.rm = TRUE))
      )

      # Create a color palette for the chloropleth map based on the
      # "fill_variable" using the bins defined above, a divergent color palette
      # ("RdYlBu") that does not have white as its middle value, and an NA color
      # matching the provider tiles to be used in the map. This palette is
      # reversed for the case when the selected "yearly_comparisons_metric" is
      # "percent_decreased" because in this case, "good" values are low (and
      # should thus be blue) and "bad" values are high (and should thus be red)

      if (input$yearly_comparisons_metric == "percent_decreased") {
        pal <- colorBin(
          palette = "RdYlBu",
          domain = data_to_map$fill_variable,
          na.color = "#FAFAF8",
          bins = bins,
          reverse = TRUE
        )
      } else {
        pal <- colorBin(
          palette = "RdYlBu",
          domain = data_to_map$fill_variable,
          na.color = "#FAFAF8",
          bins = bins
        )
      }

      # Create the labels which will pop up when the user hovers over a
      # shapefile on the map; these labels will include the county name in bold,
      # the chosen metric, its value, and the corresponding unit; it is
      # formatted using html tags, so the htmltools "HTML" function must be
      # iteratively applied to each label

      county_labels <- paste0(
        "<strong>",
        data_to_map$county_name,
        "</strong><br>",
        names(yearly_comparisons_metric_choices[which(yearly_comparisons_metric_choices == input$yearly_comparisons_metric)]), ": ",
        comma(data_to_map$fill_variable, accuracy = 1), yearly_comparisons_unit()
      ) %>%
        lapply(htmltools::HTML)

      # Create the leaflet map

      leaflet(data_to_map) %>%

        # Add provider tiles from the CartoDB API so that the world map appears
        # behind the shapefiles

        addProviderTiles(provider = "CartoDB.Positron") %>%

        # Using the "addPolygons" function, add the shapefiles to the map,
        # setting their fill color to be the palette I created earlier with an
        # opacity of 1, setting their outlines to black with weight of 0.25, and
        # setting their labels to be the labels I created earlier

        addPolygons(
          fillColor = ~ pal(fill_variable),
          color = "black",
          weight = 0.25,
          fillOpacity = 1,
          label = county_labels
        ) %>%

        # Add a legend in the bottom right corner of the map, in which the title
        # is the selected yearly comparison metric, the NA label is "No data
        # available", the color palette is the one I defined earlier, the values
        # correspond to the "fill_variable", the units have their proper suffix
        # following the values, and the opacity is 1

        addLegend(
          title = names(yearly_comparisons_metric_choices[which(yearly_comparisons_metric_choices == input$yearly_comparisons_metric)]),
          na.label = "No data available",
          pal = pal,
          values = data_to_map$fill_variable,
          labFormat = labelFormat(suffix = yearly_comparisons_unit()),
          opacity = 1,
          position = "bottomright"
        ) %>%

        # Set the view upon initialization to be centered at the middle of the
        # contiguous United States with a zoom of 3 so that the entire country,
        # including (part of) Alaska and Hawaii), is visible

        setView(lng = -98.5795, lat = 39.8283, zoom = 3)

      # Begin if statement for "State"
      
    } else if (input$yearly_comparisons_scope == "state_name") {

      # Create an object to be joined to the state shapefile data by filtering
      # the data to keep observations only in the selected year; then, group the
      # data by "year" and "state_name" and create a new variable called
      # "fill_variable" using the "summarize" and "median" functions so that
      # each county code/name corresponds to one value (determined by the user's
      # metric selection) to be depicted on the map based on the three user
      # selections on the tab

      data_to_geo_join <- zillow_historical_data %>%
        filter(year == input$yearly_comparisons_year) %>%
        group_by(year, state_name) %>%
        summarize(fill_variable = median(get(input$yearly_comparisons_metric), na.rm = TRUE))

      # Using tigris' "geo_join" function, join the state shapefiles with the
      # dataframe that was just created so that it can be plotted on a
      # chloropleth map

      data_to_map <- geo_join(
        spatial_data = states, data_frame = data_to_geo_join,
        by_sp = "NAME", by_df = "state_name"
      )

      # Create colorbins that are reactive based on quantiles of the data, which
      # will be used with a divergent color palette to show deviation from the
      # data's median on the chloropleth map using rounded estimates of the min,
      # 5th, 25th, 50th, 75th, 95th, and max quantiles of the "fill_variable"

      bins <- c(
        floor(min(data_to_map$fill_variable, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.05, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.25, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.5, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.75, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.95, na.rm = TRUE)),
        ceiling(max(data_to_map$fill_variable, na.rm = TRUE))
      )

      # Create a color palette for the chloropleth map based on the
      # "fill_variable" using the bins defined above, a divergent color palette
      # ("RdYlBu") that does not have white as its middle value, and an NA color
      # matching the provider tiles to be used in the map; this palette is
      # reversed for the case when the selected "yearly_comparisons_metric" is
      # "percent_decreased" because in this case, good values are low (and thus
      # should be blue) and bad values are high (and thus should be red)

      if (input$yearly_comparisons_metric == "percent_decreased") {
        pal <- colorBin(palette = "RdYlBu", domain = data_to_map$fill_variable, na.color = "#FAFAF8", bins = bins, reverse = TRUE)
      } else {
        pal <- colorBin(palette = "RdYlBu", domain = data_to_map$fill_variable, na.color = "#FAFAF8", bins = bins)
      }

      # Create the labels which will pop up when the user hovers over a
      # shapefile on the map; these labels will include the state name, the
      # chosen metric, its value, and the corresponding unit; it is formatted
      # using html tags, so the htmltools "HTML" function must be iteratively
      # applied to each label

      state_labels <- paste0(
        "<strong>",
        data_to_map$NAME,
        "</strong><br>",
        names(yearly_comparisons_metric_choices[which(yearly_comparisons_metric_choices == input$yearly_comparisons_metric)]), ": ",
        comma(data_to_map$fill_variable, accuracy = 1), yearly_comparisons_unit()
      ) %>%
        lapply(htmltools::HTML)

      # Create the leaflet map

      leaflet(data_to_map) %>%

        # Add provider tiles from the CartoDB API so that the world map appears
        # behind the shapefiles

        addProviderTiles(provider = "CartoDB.Positron") %>%

        # Using the "addPolygons" function, add the shapefiles to the map,
        # setting the fill color to the palette I created earlier with an
        # opacity of 1, setting the outline to black with weight of 0.25, and
        # setting their labels to be the labels I created earlier

        addPolygons(
          fillColor = ~ pal(fill_variable),
          color = "black",
          weight = 0.25,
          fillOpacity = 1,
          label = state_labels
        ) %>%

        # Add a legend in the bottom right corner of the map, in which the title
        # is the selected yearly comparison metric, the NA label is "No data
        # available", the color palette is the one I defined earlier, the values
        # correspond to the "fill_variable", the units have their proper suffix
        # following the values, and the opacity is 1

        addLegend(
          title = names(yearly_comparisons_metric_choices[which(yearly_comparisons_metric_choices == input$yearly_comparisons_metric)]),
          na.label = "No data available",
          pal = pal,
          values = data_to_map$fill_variable,
          labFormat = labelFormat(suffix = yearly_comparisons_unit()),
          opacity = 1,
          position = "bottomright"
        ) %>%

        # Set the view upon initialization to be centered at the middle of the
        # contiguous United States with a zoom of 3 so that the entire country,
        # inlcuding (part of) Alaska and Hawaii, is visible

        setView(lng = -98.5795, lat = 39.8283, zoom = 3)
    }
  })

  # Create the output for the "Home Value Forceasts" tab in the UI

  output$forecasts_map <- renderLeaflet({

    # Set up an "if-else" structure so that the map can go down two paths: one
    # if the geographical unit selected is "County" and one if it is "State"

    # Begin if statement for "County"

    if (input$forecasts_scope == "county_name") {

      # Create an object to be joined to the county shapefile data by grouping
      # the data by "county_code" and "county_name" and create a new variable
      # called "fill_variable" using the "summarize" and "median" function so
      # that each county code/name corresponds to its "forecast_yo_y_pct_change"
      # variable's value to be depicted on the map

      data_to_geo_join <- county_forecast_map_data %>%
        group_by(county_code, county_name) %>%
        summarize(fill_variable = median(forecast_yo_y_pct_change))

      # Using tigris' "geo_join" function, join the county shapefiles with the
      # dataframe that was just created so that it can be plotted on a
      # chloropleth map

      data_to_map <- geo_join(
        spatial_data = counties, data_frame = data_to_geo_join,
        by_sp = "GEOID", by_df = "county_code"
      )

      # Create colorbins that are reactive based on quantiles of the data, which
      # will be used with a divergent color palette to show deviation from the
      # data's median on the chloropleth map using rounded estimates of the min,
      # 5th, 25th, 50th, 75th, 95th, and max quantiles of the "fill_variable"

      bins <- c(
        floor(min(data_to_map$fill_variable, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.05, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.25, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.5, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.75, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.95, na.rm = TRUE)),
        ceiling(max(data_to_map$fill_variable, na.rm = TRUE))
      )

      # Create a color palette for the chloropleth map based on the
      # "fill_variable" using the bins defined above, a divergent color palette
      # ("RdYlBu") that does not have white as its middle value, and an NA color
      # matching the provider tiles to be used in the map

      pal <- colorBin(
        palette = "RdYlBu",
        domain = data_to_map$fill_variable,
        na.color = "#FAFAF8",
        bins = bins
      )

      # Create the labels which will pop up when the user hovers over a
      # shapefile on the map; these labels will include the county name, the
      # value for "forecasted_yo_y_pct_change", and the percent sign; it is
      # formatted using html tags, so the htmltools "HTML" function must be
      # iteratively applied to each label

      county_labels <- paste0(
        "<strong>",
        data_to_map$county_name,
        "</strong><br>",
        "Forecasted Percent Change: ",
        round(data_to_map$fill_variable, 1), "%"
      ) %>%
        lapply(htmltools::HTML)

      # Create the leaflet map

      leaflet(data_to_map) %>%

        # Add provider tiles from the CartoDB API so that the world map appears
        # behind the shapefiles

        addProviderTiles(provider = "CartoDB.Positron") %>%

        # Using the "addPolygons" function, add the shapefiles to the map,
        # setting the fill color to the palette I created earlier with an
        # opacity of 1, setting the outline to black with weight of 0.25, and
        # setting their labels to be the labels I created earlier

        addPolygons(
          fillColor = ~ pal(fill_variable),
          color = "black",
          weight = 0.25,
          fillOpacity = 1,
          label = county_labels
        ) %>%

        # Add a legend in the bottom right corner of the map, in which the title
        # is the selected yearly comparison metric, the NA label is "No data
        # available", the color palette is the one I defined earlier, the values
        # correspond to the "fill_variable", the units have their proper suffix
        # following the values, and the opacity is 1

        addLegend(
          title = "Forecasted Year over Year<br>Percent Change in ZHVI",
          na.label = "No data available",
          pal = pal,
          values = data_to_map$fill_variable,
          labFormat = labelFormat(suffix = "%"),
          opacity = 1,
          position = "bottomright"
        ) %>%

        # Set the view upon initialization to be centered at the middle of the
        # contiguous United States with a zoom of 3 so that the entire country,
        # including (part of) Alaska and Hawaii, is visible

        setView(lng = -98.5795, lat = 39.8283, zoom = 3)

      # Begin if statement for "State"
      
    } else if (input$forecasts_scope == "state_name") {

      # Create an object to be joined to the county shapefile data by grouping
      # the data by "state_name" and create a new variable called
      # "fill_variable" using the "summarize" and "median" function so that each
      # state name corresponds to its "forecast_yo_y_pct_change" variable's
      # value to be depicted on the map

      data_to_geo_join <- state_forecast_map_data %>%
        group_by(state_name) %>%
        summarize(fill_variable = median(forecast_yo_y_pct_change))

      # Using tigris' "geo_join" function, join the state shapefiles with the
      # dataframe that was just created so that it can be plotted on a
      # chloropleth map

      data_to_map <- geo_join(
        spatial_data = states, data_frame = data_to_geo_join,
        by_sp = "NAME", by_df = "state_name"
      )

      # Create colorbins that are reactive based on quantiles of the data, which
      # will be used with a divergent color palette to show deviation from the
      # data's median on the chloropleth map using rounded estimates of the min,
      # 5th, 25th, 50th, 75th, 95th, and max quantiles of the "fill_variable"

      bins <- c(
        floor(min(data_to_map$fill_variable, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.05, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.25, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.5, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.75, na.rm = TRUE)),
        round(quantile(data_to_map$fill_variable, 0.95, na.rm = TRUE)),
        ceiling(max(data_to_map$fill_variable, na.rm = TRUE))
      )

      # Create a color palette for the chloropleth map based on the
      # "fill_variable" using the bins defined above, a divergent color palette
      # ("RdYlBu") that does not have white as its middle value, and an NA color
      # matching the provider tiles to be used in the map

      pal <- colorBin(
        palette = "RdYlBu",
        domain = data_to_map$fill_variable,
        na.color = "#FAFAF8",
        bins = bins
      )

      # Create the labels which will pop up when the user hovers over a
      # shapefile on the map; these labels will include the state name, the
      # value for "forecasted_yo_y_pct_change", and the percent sign; it is
      # formatted using html tags, so the htmltools "HTML" function must be
      # iteratively applied to each label

      state_labels <- paste0(
        "<strong>",
        data_to_map$state_name,
        "</strong><br>",
        "Forecasted Percent Change: ",
        round(data_to_map$fill_variable, 1), "%"
      ) %>%
        lapply(htmltools::HTML)

      # Create the leaflet map

      leaflet(data_to_map) %>%

        # Add provider tiles from the CartoDB API so that the world map appears
        # behind the shapefiles

        addProviderTiles(provider = "CartoDB.Positron") %>%

        # Using the "addPolygons" function, add the shapefiles to the map,
        # setting their fill color to the palette I created earlier with an
        # opacity of 1, setting their outline to black with weight of 0.25, and
        # setting their labels to be the labels I created earlier

        addPolygons(
          fillColor = ~ pal(fill_variable),
          color = "black",
          weight = 0.25,
          fillOpacity = 1,
          label = state_labels
        ) %>%

        # Add a legend in the bottom right corner of the map, in which the title
        # is the selected yearly comparison metric, the NA label is "No data
        # available", the color palette is the one I defined earlier, the values
        # correspond to the "fill_variable", the units have their proper suffix
        # following the values, and the opacity is 1

        addLegend(
          title = "Forecasted Year over Year<br>Percent Change in ZHVI",
          na.label = "No data available",
          pal = pal,
          values = data_to_map$fill_variable,
          labFormat = labelFormat(suffix = "%"),
          opacity = 1,
          position = "bottomright"
        ) %>%

        # Set the view upon initialization to be centered at the middle of the
        # contiguous United States with a zoom of 3 so that the entire country,
        # including (part of) Alaska and Hawaii, is visible

        setView(lng = -98.5795, lat = 39.8283, zoom = 3)
    }
  })

  # Update the "looking_back_areas" options in the "Looking Back" tab based on
  # user choice for the "looking_back_scope" and "looking_back_y_variable"
  # selectInputs

  observeEvent(c(input$looking_back_scope, input$looking_back_y_variable), {

    # Set the choices to be a sorted list of unique values in the column of
    # "zillow_historical_data" that matches the user's choice for
    # "looking_back_scope"; rename the first element so that the user is
    # presented with the word "Options" followed by the list of options

    choices <- arrange(
      unique(select(
        filter(zillow_historical_data, !is.na(get(input$looking_back_y_variable))),
        match(input$looking_back_scope, names(zillow_historical_data))
      )),
      get(input$looking_back_scope)
    ) %>%
      rename(Options = 1)

    updateSelectizeInput(
      session = session,
      inputId = "looking_back_areas",
      choices = choices
    )
  })

  # Create a string that will paste onto various values in the "Looking Back"
  # tab's axis labels that is reactive to whether the chosen metric corresponds
  # to a dollar value or a percent value

  looking_back_unit <- reactive({
    if (input$looking_back_y_variable %in% c("median_zhvi_value", "median_value_per_sqft")) {
      looking_back_unit <- "$"
    } else if (input$looking_back_y_variable %in% c("percent_increased", "percent_decreased")) {
      looking_back_unit <- "%"
    }
  })

  # Create the output for the "Looking Back" tab in the UI

  output$looking_back_plot <- renderPlotly({

    # Require an input for "looking_back_areas", without which a plot will not
    # show up and the text "Select area(s) to display to begin!" will appear
    # instead

    validate(need(input$looking_back_areas, "Select area(s) to display to begin!"))

    # Create "data_to_plot" dataframe which will be used to create a line graph
    # using ggplot and ggplotly

    data_to_plot <- zillow_historical_data %>%

      # Rename the variable corresponding to the selected measure of geographic
      # scope to "color_variable" as it will be used to differentiate lines
      # based on its selected values

      rename(color_variable = match(input$looking_back_scope, names(zillow_historical_data))) %>%

      # Filter the data to keep only observations corresponding to the
      # geographic areas selected in "looking_back_areas"

      filter(color_variable %in% c(input$looking_back_areas)) %>%

      # Group the data by "period" and "color_variable" so that each unique
      # "color_variable" has values to be plotted on the line graph

      group_by(period, color_variable) %>%

      # Use the "summarize" and "median" functions to get an individual value
      # for each period-area to be plotted on the line graph

      summarize(y_value = median(get(input$looking_back_y_variable), na.rm = TRUE))

    # Create labels to appear when a user hovers over the line, containing the
    # geographical area, the year-month of the nearest data point, the
    # y-variable, and its value with the corresponding unit; it is formatted
    # using html tags, so the htmltools "HTML" function must be iteratively
    # applied to each label

    looking_back_labels <- paste0(
      data_to_plot$color_variable,
      "<br>Year-Month: ",
      str_sub(data_to_plot$period, end = -4),
      "<br>",
      names(looking_back_y_variable_choices[which(looking_back_y_variable_choices == input$looking_back_y_variable)]), ": ",
      comma(data_to_plot$y_value, accuracy = 1), looking_back_unit()
    ) %>%
      lapply(htmltools::HTML)

    # Create a ggplot item with "period" on the x-axis and "y_value" on the
    # y-axis that is colored by "color_variable" and displays
    # "looking_back_labels" when the user's mouse hovers over a line

    plot <- ggplot(data_to_plot, aes(x = period, y = y_value, color = color_variable, text = looking_back_labels)) +

      # Make the plot a line graph

      geom_line() +

      # Give the plot an x-axis title, give the plot a y-axis title that updates
      # according to the user's choice of y-variable, picking the appropriate
      # title from "looking_back_y_variable_choices", and give the plot a legend
      # title that similarly updates according to the user's choice of
      # geographical scope, picking the appropriate title from
      # "looking_back_scope_choices"

      labs(
        x = "Time",
        y = names(looking_back_y_variable_choices[which(looking_back_y_variable_choices == input$looking_back_y_variable)]),
        color = names(looking_back_scope_choices[which(looking_back_scope_choices == input$looking_back_scope)])
      ) +

      # Format the y-axis values so that they appear as comma-separated (when
      # needed) numbers with their corresponding units

      scale_y_continuous(labels = unit_format(unit = looking_back_unit(), big.mark = ",")) +

      # Set the x-axis breaks to occur at 3-year intervals, displaying only the
      # year (not the full period)

      scale_x_date(
        date_breaks = "3 years",
        labels = date_format("%Y")
      ) +

      # Set the theme to minimal for aesthetic purposes

      theme_minimal()

    # Plot the ggplot item "plot" using ggplotly, implementing hover label
    # functionality

    ggplotly(plot, tooltip = "text")
  })

  # Update the "looking_forward_areas" options in the "Looking Forward" tab
  # based on user choice for the "looking_forward_scope" selectInput

  observeEvent(input$looking_forward_scope, {

    # Set the choices to be a sorted list of unique values in the column of
    # "zillow_home_value_forecast_data" that matches the user's choice for
    # "looking_forward_scope"; rename the first element so that the user is
    # presented with the word "Options" followed by the list of options

    choices <- arrange(unique(select(filter(zillow_home_value_forecast_data, region == input$looking_forward_scope), region_name)), region_name) %>%
      rename(Options = 1)

    updateSelectizeInput(
      session = session,
      inputId = "looking_forward_areas",
      choices = choices
    )
  })

  # Create the output for the "Looking Forward" tab in the UI

  output$looking_forward_plot <- renderPlotly({

    # Require an input for "looking_forward_areas", without which a plot will
    # not show up and the text "Select area(s) to display to begin!" will appear
    # instead

    validate(need(input$looking_forward_areas, "Select area(s) to display to begin!"))

    # Create "data_to_plot" dataframe, which will be used to create a line graph
    # using ggplot

    data_to_plot <- zillow_home_value_forecast_data %>%

      # Filter the data to keep only observations where the "region" variable
      # corresponds to the selected value for "looking_forward_scope," and the
      # "region_name" variable corresponds to the selected values for
      # "looking_forward_areas"

      filter(
        region == input$looking_forward_scope,
        region_name %in% c(input$looking_forward_areas)
      )

    # Create labels to appear when a user hovers over the line, containing the
    # geographical area and the area's "forecast_yo_y_pct_change" value with the
    # corresponding unit (%); it is formatted using html tags, so the htmltools
    # "HTML" function must be iteratively applied to each label

    looking_forward_labels <- paste0(
      data_to_plot$region_name,
      "<br> Forecasted Percent Change: ",
      round(data_to_plot$forecast_yo_y_pct_change, 1), "%"
    ) %>%
      lapply(htmltools::HTML)

    # Create a ggplot item with "region_name" ordered by
    # "forecast_yo_y_pct_change" on the x-axis and "forecast_yo_y_pct_change" on
    # the y-axis that is colored by "region_name" and displays
    # "looking_forward_labels" when the user's mouse hovers over a line

    plot <- ggplot(data_to_plot, aes(
      x = reorder(region_name, forecast_yo_y_pct_change), y = forecast_yo_y_pct_change / 100,
      fill = region_name, color = region_name, text = looking_forward_labels
    )) +

      # Add narrow columns (for aesthetic purposes) to the graph, which begin at
      # y = 0 and end at a given area's forecasted percent change value

      geom_col(width = 0.03) +

      # Add a large point at every area's forecasted percent change value (the
      # end-point of every column) to emphasize the value

      geom_point(size = 4) +

      # Use the "coord_flip" function so that the graph is rotated across the
      # line y = x

      coord_flip() +

      # Give the plot a x-axis title that updates according to the user's choice
      # of geographical scope, and give the plot a y-axis title

      labs(
        x = input$looking_forward_scope,
        y = "Forecasted Year over Year Percent Change in ZHVI"
      ) +

      # Format the y-axis (now x-axis) values to be in percent-format, rounded
      # to the tenths place

      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +

      # Set the theme to minimal for aesthetic purposes

      theme_minimal()

    # Plot the ggplot item using ggplotly, hiding the legend from view and
    # implementing hover label functionality

    hide_legend(ggplotly(plot, tooltip = "text"))
  })
}

# Render the app

shinyApp(ui = ui, server = server)
