# Emily Bradford
# ESCI 599: App Development
# May 27, 2025

# Assignment: Final Project

# ---------- packages ----------

library(shiny)
library(shinythemes) # theme
library(vroom) # fast file loading
library(leaflet) # interactive map
library(tidyverse) # general data analysis, pipes, ggplot, etc.
library(shinybusy) # busy indicator
library(leaflet.extras) # heatmap
library(sf) # for handling shapefiles
library(shinyWidgets) # tree selector, background image, etc.
library(reactable) # tables
library(ggdark) # ggplot themes

# ---------- load data ----------

# wildlife sightings
# vroom is useful here because there are almost 225,000 observations
# NOTE: i ran an identity tool in arcgis to extract neighborhood and park values for each point
wildlifePoints <- vroom("Data//WildlifePoints.csv")

# shapefile of bellingham
bellingham_outline <- read_sf("Data//Bellingham_Project.shp")

# shapefile of bellingham neighborhoods
neighborhoods <- read_sf("Data//Neighborhoods_Project.shp")

# shapefile of bellingham parks
parks <- read_sf("Data//Parks_Project.shp") %>%
  # removes parks with zero points
  filter(ParkName %in% wildlifePoints$ParkName)

# create tree for tree selection
tree <- wildlifePoints %>%
  # group by species
  group_by(commonName) %>%
  # summarize to get a table containing only the unique species
  summarize(n = n(),
            # extract all taxonomic names
            className = first(className),
            orderName = first(orderName),
            # pretty sure suborder and subfamily break the app for some reason
            # including them here in case i figure out a fix
            # but for now the select only extracts the other taxonomic names
            suborderName= first(suborderName),
            familyName = first(familyName),
            subfamilyName = first(subfamilyName),
            genusName = first(genusName)) %>%
  # drop count, suborder, subfamily
  select(className, orderName, familyName, genusName, commonName) %>%
  create_tree()

# ---------- app ----------

# ----- UI -----

ui <- fluidPage(
  # set theme
  theme = shinytheme("slate"),
  # background image
  # one interesting side effect of using this particular image is that the deer's faces are only visible
  #    when the app is loading things. so every time the app needs to think, the user gets stared at by deer.
  #    to quote todd howard: it just works
  #    (this was not intentional at first, but now it's a feature, not a bug)
  setBackgroundImage(src = "deer.jpg"),
  # busy indicator. the app takes a second or so to think every time the neighborhood/park is changed
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
  # allow user to select any combination of neighborhoods and parks
  fluidRow(
    column(width = 4,
           virtualSelectInput("RegionSelect", "Select neighborhood or park:",
                              # could also do "unique(wildlifePoints$NEIGHBORHO)
                              #    but this feels cleaner and means there's no chance of NAs
                              choices = list(
                                "Neighborhoods" = neighborhoods$NEIGHBORHO,
                                "Parks" = parks$ParkName
                                ),
                              # the default selection is every neighborhood and no parks
                              selected = neighborhoods$NEIGHBORHO,
                              # allow selecting multiple neighborhoods/parks
                              multiple = TRUE,
                              # allow searching (useful since there's a LOT of parks)
                              search = TRUE)
      ),
    column(width = 3,
           # allow user to select whether to view native species, introduced species, or both
           prettyCheckboxGroup("NativeSelect", "View native or introduced species?",
                               # multiple selections allowed
                               choices = c("Native", "Introduced"),
                               # default is both selected
                               selected = c("Native", "Introduced"),
                               # display horizontally
                               inline = TRUE)
      )
  ),
  # to add: fun facts about various taxa, if i have time to
  # main layout. may change to navbar eventually if i get ideas for more tabs
  # one idea is allowing users to compare graphs from multiple neighborhoods
  # another is to run a quick cluster analysis for neighborhoods
  #    though i suspect that might not be very interesting since the clusters
  #    will probably just be "here's coastal neighborhoods. here's lake whatcom and lake padden.
  #    here's everything else"
  sidebarLayout(
    # map
    mainPanel(
      # default height is too short; want map to be square-ish
      leafletOutput("main_map",
                    height = 750),
      # takes up 58% of layout's horizontal space
      width = 7
    ),
    # sidebar
    sidebarPanel(
      # tree selector using tree generated above
      # i would really like this to be searchable, but i'm not sure that's possible
      # one stopgap solution might be to let users manually search for and select individual species
      # using a different selector? not sure how that would work
      treeInput("TaxonomySelect", "Select taxa:",
                choices = tree,
                # default selection is Every Species
                selected = unique(wildlifePoints$commonName),
                # default is to show class only; classes can then be expanded to show orders, etc.
                closeDepth = 0),
      br(),
      # display plot below tree selector
      plotOutput("main_plot",
                 height = 250),
      br(),
      # display table below plot
      reactableOutput("table"),
      # takes up 42% of layout's horizontal space
      width = 5
    )
  ),
  # to add: second page comparing neighborhoods (clustering etc.)
  # to add: native-or-introduced checkbox thing
  # citations
  "Wildlife sighting data from GBIF, 2025: GBIF.org (12 May 2025) GBIF Occurrence Download https://doi.org/10.15468/dl.a9edgc",
  br(),
  "Bellingham neighborhood and parks data from City of Bellingham, 2025: https://cob.org/services/maps/gis",
  br(),
  "Background image from City of Bellingham, 2017: https://cob.org/services/environment/no-deer-feeding"
)

# ----- server -----

server <- function(input, output, session) {
  # reactive function to select only a subset of wildlife points
  selected <- reactive(
    wildlifePoints %>%
      # filter to points selected by taxonomy tree
      filter(commonName %in% input$TaxonomySelect &
               # filter to points in only selected neighborhood/park
               (NEIGHBORHO %in% input$RegionSelect | ParkName %in% input$RegionSelect) &
               # filter to user's choice of native species, introduced species, or both
               introduced %in% input$NativeSelect)
  )
  
  # reactive function to select only the neighborhood selected by user
  neighborhoodSelect <- reactive(
    neighborhoods %>%
      filter(NEIGHBORHO %in% input$RegionSelect)
  )
  
  # reactive function to select only the park selected by user
  parkSelect <- reactive(
    parks %>%
      filter(ParkName %in% input$RegionSelect)
  )

  # get mean latitude and mean longitude of selected points for zooming purposes
  # (not sure why, but mean function wasn't working correctly, so calculated it manually)
  meanLat <- reactive(
    (min(selected()$decimalLatitude) + max(selected()$decimalLatitude)) / 2
  )
  meanLong <- reactive(
    (min(selected()$decimalLongitude) + max(selected()$decimalLongitude)) / 2
  )
  
  # automatically sets zoom level based on the distance between points - this lets the map
  # dynamically adjust its zoom level based on the characteristics of the data, being
  # more zoomed in when the data are highly clustered and less zoomed in when the data
  # are spread over a wide area
  zoomLevel <- reactive(
    # equation coefficients are somewhat arbitrary - selected based on wanting a zoom level
    # of 12 for the whole city and a zoom level of 13.5 for small neighborhoods
    13.95 - (15 * (max(selected()$decimalLongitude) - min(selected()$decimalLongitude)))
  )
  
 # define map
 output$main_map <- renderLeaflet({
   # leaflet for interactive map
   # no zoom control because it was getting in the way of the neighborhood selector
   leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
     # use cartoDB basemap
     # eventually want to find one with a decent hillshade
     addProviderTiles(providers$CartoDB.Positron,
                      options = providerTileOptions(noWrap = TRUE)) %>%
     # center map on Bellingham, roughly
     fitBounds(-122.425, 48.7, -122.475, 48.8) %>%
     # bellingham city outline
     addPolygons(data = bellingham_outline,
                 # hollow outline of the city
                 fillOpacity = 0,
                 color = "black",
                 weight = 1.5) %>%
     # neighborhood outlines
     addPolygons(data = neighborhoods,
                 # subtle hollow outlines for non-selected neighborhoods
                 fillOpacity = 0,
                 color = "black",
                 weight = 0.5) %>%
     addPolygons(data = neighborhoodSelect(),
                 # thick hollow outlines for selected neighborhoods
                 fillOpacity = 0,
                 color = "black",
                 weight = 3) %>%
     addPolygons(data = parkSelect(),
                 # thick hollow outlines for selected parks
                 fillOpacity = 0,
                 color = "black",
                 weight = 2) %>%
     # something for selected parks goes here
     # add wildlife observations to map in form of heatmap
     addHeatmap(lng = selected()$decimalLongitude,
                lat = selected()$decimalLatitude,
                # higher minimum opacity than default
                minOpacity = 0.2,
                # higher blur than default
                blur = 20,
                # lower max than default
                max = 0.2,
                # default radius
                radius = 25,
                # may give users control over symbology eventually
                gradient = "Spectral",
                # smooths the intensity of the heatmap to prevent it from being mostly low
                #    with a few intense dots
                # this produces a warning because the number of points is not evenly divisible
                #    by 4. that's fine, it doesn't seem to actually matter
                intensity = c(0, 0.5, 0.75, 1)) %>%
     # zooms in on selected neighborhood(s)/parks
     setView(lng = meanLong(), lat = meanLat(), zoom = zoomLevel())
 })
 
 # render plot
 output$main_plot <- renderPlot(
   selected() %>%
     # group by species, count number of occurrences among selected points only
     group_by(commonName) %>%
     summarize(count = n()) %>%
     # arrange from highest count to lowest count
     arrange(desc(count)) %>%
     # grab top 5 (reasonable number for a single chart)
     slice_head(n = 5) %>%
     # plot ordered by most occurrences to least (for 6 most common species)
     ggplot(aes(y = reorder(commonName, count), x = count)) %>%
     # columns colored by species
     + geom_col(aes(fill = commonName)) %>%
     # set palette; no legend
     + scale_fill_brewer(palette = "Set2", guide = "none") %>%
     # axis labels
     + labs(y = "Species",
            x = "Count") %>%
     # dark theme
     + dark_theme_gray() %>%
     # no space between columns and y-axis, slight space between columns and right side of chart
     + scale_x_continuous(expand = expansion(mult = c(0, 0.1)),
                          labels = scales::comma) %>%
     # change axis text size
     + theme(axis.text = element_text(size = 12))
 )
 
 # output table generated with reactable
 output$table <- renderReactable(
   selected() %>%
     # group by species, count number of occurrences among selected points only
     group_by(Species = commonName) %>%
     summarize(Count = n()) %>%
     # arrange from highest count to lowest count
     arrange(desc(Count)) %>%
     # displays 5 per page to match chart
     reactable(defaultPageSize = 5,
               # searchable in case someone is curious about a specific species/set of species
               searchable = TRUE,
               # set table colors
               theme = reactableTheme(backgroundColor = "#999999",
                                      color = "black",
                                      borderColor = "black",
                                      searchInputStyle = list(backgroundColor = "lightgray",
                                                              color = "black")))
 )
}

# ----- run app -----

shinyApp(ui, server)