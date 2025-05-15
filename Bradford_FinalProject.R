# Emily Bradford
# ESCI 599: App Development
# May 14, 2025

# Assignment: Final Project

# ---------- packages ----------

library(shiny)
library(shinythemes)
library(vroom) # fast file loading
library(leaflet) # interactive map
library(tidyverse) # general data analysis, pipes, etc.; probably ggplot eventually
library(shinybusy) # busy indicator
library(leaflet.extras) # heatmap
# library(sf) - will eventually need for handling shapefiles. not needed yet
# probably going to need reactable eventually

# ---------- load data ----------

# wildlife sightings
# vroom is useful here because there are almost 225,000 observations
wildlifePoints <- vroom("OccurrenceClip.csv")

# to be included: shapefile of bellingham. hollow outline
# bellingham_outline <- read_sf([Bellingham shapefile path])

# to be included: shapefile of bellingham neighborhoods. translucent colors
# neighborhoods <- read_sf([Bellingham neighborhoods shapefile path])

# to be included: shapefile of bellingham parks. hollow outline
# parks <- read_sf([Bellingham parks shapefile path])

# ---------- app ----------

# ----- UI -----

ui <- fluidPage(
  # set theme
  theme = shinytheme("slate"),
  # busy indicator
  add_busy_spinner(spin = "fading-circle"),
  # name and date
  "Author: Emily Bradford",
  br(),
  # display date using a formula that always displays the current day.
  #    the gsub section removes leading zeroes from day numbers
  paste0("Date: ", gsub(" 0", " ", format(Sys.Date(), "%b %d, %Y"))),
  # header/title
  titlePanel("Bellingham Urban Wildlife Viewer"),
  br(),
  # somehow this should also visually filter the map such that only the selected
  #    neighborhood/park appears, as well as only the observations in that neighborhood/park.
  #    feel like i should allow filtering based on neighborhood OR park, not both at once
  #    to avoid the situation where a user could filter to a neighborhood and a park that
  #    isn't in that neighborhood, which would lead to nothing showing up
  #    (or maybe break the code) - either way, not good
  "[To be included: select box for neighborhood or park]",
  # main layout. may change to navbar eventually if i get ideas for more tabs
  sidebarLayout(
    # map
    mainPanel(
      # default height is too short; want map to be square-ish
      leafletOutput("main_map", height = 700)
    ),
    # sidebar
    sidebarPanel(
      "[To be included: Checkboxes that allow users to filter based on taxonomy]"
      # will be a nested structure - e.g. user can select all birds, or only passeriformes,
      #    or only corvids, or only crows and ravens, or only the common raven, etc.
      #    this will be complicated and somewhat time-consuming but it should be entirely doable
    )
  ),
  # somewhere here i'd like to include a chart showing the most common species in the selected area
  #    and maybe allow the user to filter based on class (i.e. all wildlife, birds, amphibians, reptiles, or mammals)
  #    otherwise it would just be Birds. nothing but Birds
  
  # citations
  "Wildlife sighting data from GBIF, 2025: GBIF.org (12 May 2025) GBIF Occurrence Download https://doi.org/10.15468/dl.a9edgc",
  br(),
  "Bellingham neighborhood and parks data from City of Bellingham, 2025: https://cob.org/services/maps/gis"
)

# ----- server -----

server <- function(input, output, session) {
 # define map
 output$main_map <- renderLeaflet({
   # leaflet for interactive map
   leaflet() %>%
     # use cartoDB basemap
     # eventually want to find one with a decent hillshade
     addProviderTiles(providers$CartoDB.Positron,
                      options = providerTileOptions(noWrap = TRUE)) %>%
     # center map on Bellingham, roughly
     fitBounds(-122.425, 48.7, -122.475, 48.8) %>%
     # add wildlife observations to map in form of heatmap
     # eventually there will be a reactive expression to only draw lat/long
     #    from the subset of wildlifePoints selected by the user
     addHeatmap(lng = wildlifePoints$decimalLongitude,
                lat = wildlifePoints$decimalLatitude,
                # mostly default settings with a bit of extra blur
                blur = 20,
                max = 1,
                radius = 25,
                # may give users control over symbology eventually
                gradient = "Spectral",
                # smooths the intensity of the heatmap to prevent it from being mostly low
                #    with a few intense dots
                # this produces a warning because the number of points is not evenly divisible
                #    by 4. that's fine, it doesn't seem to actually matter
                intensity = c(0, 0.5, 0.75, 1))
   # eventually the map will also include the city of bellingham outline and the neighborhoods
   
   # if the user selects a specific neighborhood, the city of bellingham outline is removed,
   #    as are all of the neighborhoods except for the selected neighborhood, which is replaced by
   #    a hollow outline. if the user selects a specific park, the city outline and neighborhoods
   #    are removed and a single outline for the selected park is added to the map. park outlines
   #    do not appear on the map unless the user filters to a specific park
   
   # i'd like to get the map to automatically zoom in on whatever neighborhood/park is selected
   
   # it would be nice to symbolize different taxa using different colors but i don't think that's feasible, unfortunately.
   #    i tried with points and it, without fail, caused R to crash thanks to a memory leak
   #    hence having to use heatmaps rather than points
   #    and overlapping heatmaps with different colors will NOT work as a visualization at all
 })
}

# ----- run app -----

shinyApp(ui, server)