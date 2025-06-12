# Emily Bradford
# ESCI 599: App Development
# June 11, 2025

# Assignment: Final Project

# ----------

# Bellingham Urban Wildlife Viewer v1.0.0

# ---------- packages ----------

library(Cairo)          # higher-resolution text on plots
library(ggdark)         # ggplot themes
library(leaflet)        # interactive map
library(leaflet.extras) # heatmap
library(plotly)         # interactive plots
library(RColorBrewer)   # palette for map legend
library(reactable)      # tables
library(sf)             # handling shapefiles
library(shiny)          # needed for a shiny app, unsurprisngly
library(shinybusy)      # busy indicator
library(shinythemes)    # theme
library(shinyWidgets)   # tree selector, background image, etc.
library(tidyverse)      # general data analysis, pipes, ggplot, etc.
library(vroom)          # fast file loading

# ---------- load data ----------

# wildlife sightings
# vroom is useful here because there are almost 225,000 observations
# NOTE: i ran an identity tool in arcgis to extract neighborhood and park values for each point
wildlifePoints <- vroom("Data//WildlifePoints.csv")

# shapefile of bellingham
bellingham_outline <- read_sf("Data//Bellingham_Project.shp")

# shapefile of bellingham neighborhoods
neighborhoods <- read_sf("Data//Neighborhoods_Project.shp")

# extract neighborhood area in sq km
# (the number converts sq ft to sq km)
NeighborhoodArea <- as.data.frame(neighborhoods) %>%
  select(NEIGHBORHO, Shape_Area) %>%
  mutate(Area = Shape_Area / 10764000) %>%
  # drop original area column
  select(NEIGHBORHO, Area)

# add neighborhood area to wildlifePoints; useful for calculating density later
wildlifePoints <- merge(wildlifePoints, NeighborhoodArea, by = "NEIGHBORHO", all.x = TRUE)

# shapefile of bellingham parks
parks <- read_sf("Data//Parks_Project.shp") %>%
  # removes parks with zero wildlife sightings
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
            suborderName  = first(suborderName),
            familyName    = first(familyName),
            subfamilyName = first(subfamilyName),
            genusName     = first(genusName)
            ) %>%
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
  # busy indicator. the app takes a second or so to think every time the selection is changed
  # light gray color to stand out from background
  # also the hex code represents my feelings about everything at the end of the quarter
  add_busy_spinner(spin = "fading-circle",
                   color = "#aaaaaa"
                   ),
  # title
  titlePanel(h1("Bellingham Urban Wildlife Viewer",
                # place in center
                align = "center",
                # CSS options
                style = "border-style: solid; border-color: #141518; background-color: #1C1E22;
                padding-left: 10px; padding-right: 10px; padding-bottom: 10px; padding-top: 10px;"
                ),
             # set browser window title - abbreviated
             windowTitle = "BUWV 1.0"
             ),
  # subtitle: version, date published, author
  div(h4("Version 1.0.0 (June 11, 2025)          |          By Emily Bradford",
         # place in center
         align = "center",
         # CSS options
         style = "border-style: solid; border-color: #141518; background-color: #1C1E22;
         padding-left: 10px; padding-right: 10px; padding-bottom: 10px; padding-top: 10px;
         white-space: pre;"
         )
      ),
  # information about the app
  div("This app allows users to view wildlife sightings within the city of Bellingham using data from GBIF.
      You can filter the data to specific neighborhoods or parks of interest using the selection input to the
      upper left side of the navigation menu below. To the upper right, you can filter to only include native
      species, only include introduced species, or include both. In the sidebar, you can choose which taxa to
      view. The sidebar also displays a chart and table showing the most common taxa in your area of interest.
      In addition to the map, you can also display a table showing the count of species or genera by neighborhood
      and a comparison of the most common species or genera across two neighborhoods.",
      # CSS options
      style = "border-style: solid; border-color: #141518; background-color: #1C1E22;
      padding-left: 10px; padding-right: 10px; padding-bottom: 10px; padding-top: 10px;"),
  br(),
  # row for various selection inputs
  fluidRow(
    # ensure that the select input displays above the navbar, not below
    # (99999 is probably overkill but it just works)
    tags$style(HTML(".vscomp-dropbox-container  {z-index:99999 !important;}")),
    # select input: allow user to select any combination of neighborhoods and parks
    column(width = 4,
           # virtualSelectInput from shinywidgets
           virtualSelectInput("RegionSelect",
                              "Select neighborhood or park to filter to:",
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
                              search = TRUE
                              )
      ),
    # second column: whether to view native vs introduced species
    column(width = 3,
           # allow user to select whether to view native species, introduced species, or both
           # prettyCheckboxGroup from shinywidgets
           prettyCheckboxGroup("NativeSelect",
                               "View native or introduced species?",
                               # multiple selections allowed
                               choices = c("Native", "Introduced"),
                               # default is both selected
                               selected = c("Native", "Introduced"),
                               # display horizontally
                               inline = TRUE
                               )
      )
  ),
  # to add: fun facts about various taxa, if i have time to
  #    update: i probably don't have time to before the end of the quarter
  #    but i might add this at some point later as app version 1.1 or something
  # main layout
  sidebarLayout(
    # map/neighborhood info panel
    mainPanel(
      # navbar layout within the main panel
      navbarPage(title = "Display",
                 # map - default when app is opened
                 # including the word "interactive" here to make it obvious to users that they can zoom, scroll, etc.
                 tabPanel("Interactive map",
                          leafletOutput("main_map",
                                        # default height is too short; want map to be square-ish
                                        # and more importantly line up with the sidebar's height
                                        height = 750
                                        )
                          ),
                 # table of count/density of selected taxa by neighborhood
                 tabPanel("Count of selected taxa by neighborhood",
                          reactableOutput("neighborhood_count")
                          ),
                 # tab containing two columns to allow comparing two neighborhoods
                 # each column has one select input, one chart, and one table
                 tabPanel("Compare neighborhoods",
                          # first row: selection inputs
                          fluidRow(
                            # first column: neighborhood 1
                            column(width = 6,
                                   # similar to overall neighborhood/park selector. hence few comments
                                   virtualSelectInput("NeighborhoodSelect1",
                                                      "Select neighborhood 1:",
                                                      choices = list(
                                                        "Neighborhoods" = neighborhoods$NEIGHBORHO
                                                      ),
                                                      # only allow selecting a single neighborhood
                                                      multiple = FALSE,
                                                      # allow searching
                                                      search = TRUE
                                                      )
                                   ),
                            # second column: neighborhood 2
                            column(width = 6,
                                   # as above but for the second neighborhood
                                   virtualSelectInput("NeighborhoodSelect2",
                                                      "Select neighborhood 2:",
                                                      choices = list(
                                                        "Neighborhoods" = neighborhoods$NEIGHBORHO
                                                        ),
                                                      multiple = FALSE,
                                                      search = TRUE
                                                      )
                                   )
                            ),
                          # second row: display one plot per neighborhood
                          fluidRow(
                            # first column: neighborhood 1
                            column(width = 6,
                                   plotlyOutput("neighborhood_plot1",
                                                # shorter to make sure everything at least comes close to fitting
                                                #    into a single page
                                                # and to make sure the bottom of the table lines up with
                                                #    the bottom of the sidebar
                                                height = 335
                                                )
                                   ),
                            # second column: neighborhood 2
                            column(width = 6,
                                   plotlyOutput("neighborhood_plot2",
                                                height = 335
                                                )
                                   )
                          ),
                          # third row: display one table per neighborhood
                          fluidRow(
                            # first column: neighborhood 1
                            column(width = 6,
                                   reactableOutput("neighborhood_table1")
                                   ),
                            # second column: neighborhood 2
                            column(width = 6,
                                   reactableOutput("neighborhood_table2")
                                   )
                          )
                          )
                 ),
      # main/navbar layout takes up 58% of layout's horizontal space
      width = 7
    ),
    # sidebar
    sidebarPanel(
      # tree selector using tree generated above
      # i would really like this to be searchable, but i'm not sure that's possible
      # one stopgap solution might be to let users manually search for and select individual species
      #    using a different selector? not sure how that would work
      # treeInput from shinywidgets
      treeInput("TaxonomySelect",
                "Select taxa to view:",
                # choices from tree defined earlier in script
                choices = tree,
                # default selection is Every Species
                selected = unique(wildlifePoints$commonName),
                # default is to show class only; classes can then be expanded to show orders, etc.
                closeDepth = 0
                ),
      br(),
      # allow selecting species or genus
      "Display species or genus in plots and tables?",
      # prettyToggle from shinywidgets
      prettyToggle("SpeciesGenus",
                   # species by default, genus if clicked
                   label_on = "Genus",
                   label_off = "Species"
                   ),
      # display plot below tree selector
      plotlyOutput("main_plot",
                   # significantly shorter to make sure everything at least comes close to fitting in a single page
                   height = 250
                   ),
      br(),
      # display table below plot
      reactableOutput("table"),
      # sidebar takes up 42% of layout's horizontal space
      width = 5
    )
  ),
  br(),
  # citations
  div("Wildlife sighting data from GBIF, 2025: GBIF.org (12 May 2025) GBIF Occurrence Download",
      # formats link as hyperlink
      a("https://doi.org/10.15468/dl.a9edgc",
        href = "https://doi.org/10.15468/dl.a9edgc"
        ),
      br(),
      "Bellingham neighborhood and parks data from City of Bellingham, 2025: ",
      a("https://cob.org/services/maps/gis",
        href = "https://cob.org/services/maps/gis"
        ),
      br(),
      "Background image from City of Bellingham, 2017: ",
      a("https://cob.org/services/environment/no-deer-feeding",
        href = "https://cob.org/services/environment/no-deer-feeding"
        ),
      # CSS
      style = "border-style: solid; border-color: #141518; background-color: #1C1E22;
      padding-left: 10px; padding-right: 10px; padding-bottom: 10px; padding-top: 10px;")
)

# ----- server -----

server <- function(input, output, session) {
  # reactive function to select only a subset of wildlife points
  selected <- reactive(
    # input: all wildlife observations
    wildlifePoints %>%
      # filter to points selected by taxonomy tree
      filter(commonName %in% input$TaxonomySelect &
               # filter to points in only selected neighborhood/park
               (NEIGHBORHO %in% input$RegionSelect | ParkName %in% input$RegionSelect) &
               # filter to user's choice of native species, introduced species, or both
               introduced %in% input$NativeSelect
             )
  )
  
  # reactive function to select only the neighborhoods selected by user
  neighborhoodSelect <- reactive(
    # input: all neighborhoods
    neighborhoods %>%
      # filter to selected neighborhood(s)
      filter(NEIGHBORHO %in% input$RegionSelect)
  )
  
  # reactive function to select only the parks selected by user
  parkSelect <- reactive(
    # input: all parks
    parks %>%
      # filter to selected park(s)
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
  
  # set palette for map legend
  pal <- brewer.pal(9, "Spectral")
  
 # define map
 output$main_map <- renderLeaflet({
   # leaflet for interactive map
   # no zoom control because it was getting in the way of the neighborhood selector
   leaflet(options = leafletOptions(zoomControl = FALSE)
           ) %>%
     # use esri natgeo-style world map
     addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
     # center map on Bellingham, roughly
     fitBounds(-122.425, 48.7, -122.475, 48.8) %>%
     # bellingham city outline
     addPolygons(data = bellingham_outline,
                 # hollow outline of the city
                 fillOpacity = 0,
                 color       = "black",
                 weight      = 1.5
                 ) %>%
     # non-selected neighborhood outlines
     addPolygons(data = neighborhoods,
                 # subtle hollow outlines for non-selected neighborhoods
                 fillOpacity = 0,
                 color       = "black",
                 weight      = 0.5
                 ) %>%
     # selected neighborhood outlines
     addPolygons(data = neighborhoodSelect(),
                 # thick hollow outlines for selected neighborhoods
                 fillOpacity = 0,
                 color       = "black",
                 weight      = 3
                 ) %>%
     # selected park outlines
     addPolygons(data = parkSelect(),
                 # thick hollow outlines for selected parks
                 fillOpacity = 0,
                 color       = "black",
                 weight      = 2
                 ) %>%
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
                # may give users control over symbology eventually. probably not soon
                gradient = "Spectral",
                # smooths the intensity of the heatmap to prevent it from being mostly low
                #    with a few intense dots
                # this produces a warning because the number of points is not evenly divisible
                #    by 4. that's fine, it doesn't seem to actually matter
                intensity = c(0, 0.5, 0.75, 1)
                ) %>%
     # add legend using colors defined above
     addLegend(colors = pal,
               # qualitative high-low legend is the only option that makes sense imo
               # number of blanks depends on number of colors in the palette
               labels = c("Low", "", "", "", "", "", "", "", "High"),
               # legend title
               title = "Number of observations"
               ) %>%
     # auto-zooms in on selected neighborhood(s)/parks
     setView(lng = meanLong(),
             lat = meanLat(),
             zoom = zoomLevel()
             )
 })
 
 # render plot
 output$main_plot <- renderPlotly(
   # if user wants to view species
   if (input$SpeciesGenus == FALSE) {
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
     + scale_fill_brewer(palette = "Set2",
                         guide = "none"
                         ) %>%
     # axis labels
     + labs(y = "Species",
            x = "Count"
            ) %>%
     # dark theme
     + dark_theme_gray() %>%
     # no space between columns and y-axis, slight space between columns and right side of chart
     + scale_x_continuous(expand = expansion(mult = c(0, 0.1)),
                          # thousands separators for my sanity
                          labels = scales::comma
                          ) %>%
     # change axis text size
     + theme(axis.text = element_text(size = 10),
             # ensure legend doesn't show up when converted to plotly object
             legend.position = "none"
             ) %>%
       # convert to plotly
       # tooltips only display count
       ggplotly(tooltip = "count")
     }
   # if user wants to view genus
   # code is pretty much same as previous chart, but with species swaped for genus
   # hence no comments; would be redundant.
   else {
     selected() %>%
       group_by(genusName) %>%
       summarize(count = n()) %>%
       arrange(desc(count)) %>%
       slice_head(n = 5) %>%
       ggplot(aes(y = reorder(genusName, count), x = count)) %>%
       + geom_col(aes(fill = genusName)) %>%
       + scale_fill_brewer(palette = "Set2",
                           guide = "none"
                           ) %>%
       + labs(y = "Genus",
              x = "Count"
              ) %>%
       + dark_theme_gray() %>%
       + scale_x_continuous(expand = expansion(mult = c(0, 0.1)),
                            labels = scales::comma
                            ) %>%
       + theme(axis.text = element_text(size = 10),
               legend.position = "none"
               ) %>%
       ggplotly(tooltip = "count")
   }
 )
 
 # set table theme
 tableTheme <- reactableTheme(backgroundColor = "#999999",
                              # color = text color. not super intuitive
                              color            = "black",
                              borderColor      = "black",
                              # sets visual options for search bar
                              searchInputStyle = list(backgroundColor = "lightgray",
                                                      color           = "black"
                                                      )
                              )
 
 # output table generated with reactable
 output$table <- renderReactable(
   # if user has chosen to display by species
   if (input$SpeciesGenus == FALSE) {
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
               theme = tableTheme,
               # reformat columns
               # add thousands separators to count
               columns = list(Count = colDef(format = colFormat(separators = TRUE)
                                             )
                              )
               )
     }
   # if user has chosen to display by genus
   else {
     # table identical to above, except with genus swapped for species
     # hence no comments. would be redundant
     selected() %>%
       group_by(Genus = genusName) %>%
       summarize(Count = n()) %>%
       arrange(desc(Count)) %>%
       reactable(defaultPageSize = 5,
                 searchable      = TRUE,
                 theme           = tableTheme,
                 columns         = list(Count = colDef(format = colFormat(separators = TRUE)
                                                       )
                                        )
                 )
   }
 )
 
 # count by neighborhood
 output$neighborhood_count <- renderReactable(
   selected() %>%
     group_by(Neighborhood = NEIGHBORHO) %>%
     summarize(Count = n(),
               # neighborhood area; needed for calculating density
               Area  = first(Area)
               ) %>%
     # calculate density
     mutate(Density = Count / Area) %>%
     # drop neighborhood area column
     select(!Area) %>%
     # arrange by count by default
     arrange(desc(Count)) %>%
     # table formatting
     reactable(defaultPageSize = 15,
               searchable      = TRUE,
               theme           = tableTheme,
               # reformat columns
               # add thousands separators to count
               columns = list(Count = colDef(format = colFormat(separators = TRUE)
                                             ),
                              # rename density
                              Density = colDef(name = "Density (per sq km)",
                                               # set decimals to 2 digits and add thousands separators
                                               format = colFormat(digits = 2,
                                                                  separators = TRUE
                                                                  )
                                               )
                              )
               )
 )
 
 # neighborhood comparison - select only those in neighborhood 1/neighborhood 2
 selected_n1 <- reactive(
   wildlifePoints %>%
     # still filter by taxonomy and native/introduced status
     filter(commonName %in% input$TaxonomySelect &
              NEIGHBORHO == input$NeighborhoodSelect1 &
              introduced %in% input$NativeSelect
            )
 )
 # code as above. hence no comments
 selected_n2 <- reactive(
   wildlifePoints %>%
     filter(commonName %in% input$TaxonomySelect &
              NEIGHBORHO == input$NeighborhoodSelect2 &
              introduced %in% input$NativeSelect
            )
 )
 
 # render plots - code extremely similar to main plot. hence no comments
 # this section of the code could be made more efficient via functions eventually, i think
 output$neighborhood_plot1 <- renderPlotly(
   if (input$SpeciesGenus == FALSE) {
     selected_n1() %>%
       group_by(commonName) %>%
       summarize(count = n()) %>%
       arrange(desc(count)) %>%
       slice_head(n = 5) %>%
       ggplot(aes(y = reorder(commonName, count), x = count)) %>%
       + geom_col(aes(fill = commonName)) %>%
       + scale_fill_brewer(palette = "Set2",
                           guide = "none"
                           ) %>%
       + labs(y = "Species",
              x = "Count"
              ) %>%
       + dark_theme_gray() %>%
       + scale_x_continuous(expand = expansion(mult = c(0, 0.1)),
                            labels = scales::comma
                            ) %>%
       + theme(axis.text = element_text(size = 8),
               legend.position = "none"
               ) %>%
       ggplotly(tooltip = "count")
     }
   else {
     selected_n1() %>%
       group_by(genusName) %>%
       summarize(count = n()) %>%
       arrange(desc(count)) %>%
       slice_head(n = 5) %>%
       ggplot(aes(y = reorder(genusName, count), x = count)) %>%
       + geom_col(aes(fill = genusName)) %>%
       + scale_fill_brewer(palette = "Set2",
                           guide = "none"
                           ) %>%
       + labs(y = "Genus",
              x = "Count"
              ) %>%
       + dark_theme_gray() %>%
       + scale_x_continuous(expand = expansion(mult = c(0, 0.1)),
                            labels = scales::comma
                            ) %>%
       + theme(axis.text = element_text(size = 8),
               legend.position = "none"
               ) %>%
       ggplotly(tooltip = "count")
   }
 )
 # code as above. hence no comments
 output$neighborhood_plot2 <- renderPlotly(
   if (input$SpeciesGenus == FALSE) {
     selected_n2() %>%
       group_by(commonName) %>%
       summarize(count = n()) %>%
       arrange(desc(count)) %>%
       slice_head(n = 5) %>%
       ggplot(aes(y = reorder(commonName, count), x = count)) %>%
       + geom_col(aes(fill = commonName)) %>%
       + scale_fill_brewer(palette = "Set2",
                           guide = "none"
                           ) %>%
       + labs(y = "Species",
              x = "Count"
              ) %>%
       + dark_theme_gray() %>%
       + scale_x_continuous(expand = expansion(mult = c(0, 0.1)),
                            labels = scales::comma
                            ) %>%
       + theme(axis.text = element_text(size = 8),
               legend.position = "none"
               ) %>%
       ggplotly(tooltip = "count")
     }
   else {
     selected_n2() %>%
       group_by(genusName) %>%
       summarize(count = n()) %>%
       arrange(desc(count)) %>%
       slice_head(n = 5) %>%
       ggplot(aes(y = reorder(genusName, count), x = count)) %>%
       + geom_col(aes(fill = genusName)) %>%
       + scale_fill_brewer(palette = "Set2",
                           guide = "none"
                           ) %>%
       + labs(y = "Genus",
              x = "Count"
              ) %>%
       + dark_theme_gray() %>%
       + scale_x_continuous(expand = expansion(mult = c(0, 0.1)),
                            labels = scales::comma
                            ) %>%
       + theme(axis.text = element_text(size = 8),
               legend.position = "none") %>%
       ggplotly(tooltip = "count")
   }
 )
 
 # output tables - very similar to main table. hence no comments
 # again could probably make this more efficient via functions etc.
 output$neighborhood_table1 <- renderReactable(
   if (input$SpeciesGenus == FALSE) {
     selected_n1() %>%
       group_by(Species = commonName) %>%
       summarize(Count = n()) %>%
       arrange(desc(Count)) %>%
       reactable(defaultPageSize = 5,
                 searchable      = TRUE,
                 theme           = tableTheme,
                 columns         = list(Count = colDef(format = colFormat(separators = TRUE)
                                                       )
                 )
                 )
     }
   else {
     selected_n1() %>%
       group_by(Genus = genusName) %>%
       summarize(Count = n()) %>%
       arrange(desc(Count)) %>%
       reactable(defaultPageSize = 5,
                 searchable      = TRUE,
                 theme           = tableTheme,
                 columns         = list(Count = colDef(format = colFormat(separators = TRUE)
                                                       )
                 )
                 )
   }
 )
 # code as above. hence no comments
 output$neighborhood_table2 <- renderReactable(
   if (input$SpeciesGenus == FALSE) {
     selected_n2() %>%
       group_by(Species = commonName) %>%
       summarize(Count = n()) %>%
       arrange(desc(Count)) %>%
       reactable(defaultPageSize = 5,
                 searchable      = TRUE,
                 theme           = tableTheme,
                 columns         = list(Count = colDef(format = colFormat(separators = TRUE)
                                                       )
                 )
                 )
     }
   else {
     selected_n2() %>%
       group_by(Genus = genusName) %>%
       summarize(Count = n()) %>%
       arrange(desc(Count)) %>%
       reactable(defaultPageSize = 5,
                 searchable      = TRUE,
                 theme           = tableTheme,
                 columns         = list(Count = colDef(format = colFormat(separators = TRUE)
                                                       )
                 )
                 )
   }
 )
}

# ----- run app -----

shinyApp(ui, server)