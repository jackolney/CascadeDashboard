LoadCountryMapData <- function() {
    world <- rgdal::readOGR(dsn = "server/data/leaflet-countries/ne_50m_admin_0_countries.dbf", layer = 'ne_50m_admin_0_countries', encoding = 'UTF-8')
    relevantCountries <- subset(world, name %in% LeafletCountryList)
    relevantCountries
}

observeEvent(input$countryMap_shape_click, {
    updateSelectInput(session, inputId = "selectCountry", selected = CountryList[which(LeafletCountryList == input$countryMap_shape_click[1])])
})

output$countryMap <- renderLeaflet({
    if (requireNamespace("rgdal", quietly = TRUE)) {
        mapData <- LoadCountryMapData()

        leaflet() %>%
        addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/jackolney/ciqrvfu7b000ic7nif534akgs/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiamFja29sbmV5IiwiYSI6ImNpcXJ2ZjVzejAwOTFoeW1hdWRhZ3R6bngifQ.qygvtBVW6dfo0bwAjVNgvg") %>%
        setView(lng = 0, lat = 30, zoom = 2) %>%
        addPolygons(
            data = mapData,
            layerId = mapData$name,
            weight = 2,
            stroke = TRUE,
            color = "white",
            opacity = 0.5,
            fill = TRUE,
            fillColor = "#4F8ABA",
            fillOpacity = 0.3,
            popup = mapData$name
        )
    }
})

# ui render for missing map (gives user some context)
output$mapWarning <- renderUI({
    if (!requireNamespace("rgdal", quietly = TRUE)) {
        tag1 <- tags$code("Package 'rgdal' not installed. Interactive map is disabled.")
        tag2 <- tags$p("")
        tag3 <- tags$p("To install 'rgdal' please run the following commands in a shell:")
        tag4 <- tags$pre("sudo -E apt-get -yq --no-install-suggests --no-install-recommends --force-yes install libgdal-dev libproj-dev")
        tag5 <- tags$p("Then in R:")
        tag6 <- tags$pre("install.packages('rgdal')")
        tag7 <- tags$p("Follow link for further instructions:")
        tag8 <- tags$a(href = "https://github.com/jackolney/CascadeDashboard/blob/master/README.md", "https://github.com/jackolney/CascadeDashboard/blob/master/README.md", target = "_blank")
        HTML(paste0(tag1, tag2, tag3, tag4, tag5, tag6, tag7, tag8))
    }
})
