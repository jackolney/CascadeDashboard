LoadCountryMapData <- function() {
    world <- rgdal::readOGR(dsn = "server/data/leaflet-countries/ne_50m_admin_0_countries.dbf", layer = 'ne_50m_admin_0_countries', encoding = 'UTF-8')
    relevantCountries <- subset(world, name %in% LeafletCountryList)
    relevantCountries
}

observeEvent(input$countryMap_shape_click, {
    updateSelectInput(session, inputId = "selectCountry", selected = CountryList[which(LeafletCountryList == input$countryMap_shape_click[1])])
})

output$countryMap <- renderLeaflet({
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
})
