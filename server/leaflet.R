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
    # addTiles() %>%                                # Base Map
    # addProviderTiles("Hydda.Base") %>%            # Dark Blue with Blank Countries
    addProviderTiles("MapQuestOpen.Aerial") %>%     # Satellite Imagery
    setView(lng = 0, lat = 30, zoom = 2) %>%
    addPolygons(
        data = mapData,
        layerId = mapData$name,
        weight = 3,
        stroke = TRUE,
        color = "white",
        popup = mapData$name
    )
})
