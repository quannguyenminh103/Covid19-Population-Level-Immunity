MakeMap <- function(vaccinatedType, ascertainmentBias){
  stateline <<- st_read("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/tl_2017_us_state.geojson")
  stateline <- subset(stateline,select = c('STUSPS','NAME',"geometry"))
  stateData <- DATA %>% filter(Date == vacc_data$date_vac[length(vacc_data$date_vac)]) %>%
    filter(ABias == ascertainmentBias) %>% filter(VaccType == vaccinatedType)
  USMAP <- stateline %>% inner_join(stateData, by = c('NAME' = 'State'))
  USMAP$ImmuLevel <- round(USMAP$ImmuLevel*100)
  #create risk-maps
    # continous color
    #legendlabs <<- c("< 50", "50-55", "55-60", "60-65", "> 70")
    pal <- colorNumeric("viridis", domain = c(0,100), na.color = 'grey')
    legendlabs = c("0%", "20%", "40%", "60%", "80%", "100%")
    
    JAM = leafletOptions(worldCopyJump=TRUE)
    
    labels <- sprintf(
      "<strong>State: %s</strong><br/>Population-level Immunity: %.0f%%<br/>Infected: %.0f%%<br/>Vaccinated (%s): %.0f%%<br/>Updated Date: %s<br/>",
      USMAP$NAME, USMAP$ImmuLevel, round(USMAP$CaseFrac*100), USMAP$VaccType, round(USMAP$VaccFrac*100),USMAP$Date
    ) %>% lapply(htmltools::HTML)
    map <- leaflet(USMAP,options=JAM) %>% addTiles() %>% setView(-120, 50, zoom = 3) %>% #including Alaska & Hawaii
      #setView(-98.35, 39.7, zoom = 4) %>% 
      #addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = USMAP,
                  fillColor = ~pal(ImmuLevel),
                  weight = 0.5,
                  opacity = 0.7,
                  color = "black",
                  dashArray = "2",
                  fillOpacity = 0.5,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.6,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, opacity = 1, title = 'Immunity Level (%)', na.label = 'NA', values = c(0,100), 
                #labels = legendlabs,
                position = "topright",
                labFormat = function(type, cuts, p) {
                  paste0(legendlabs)
                })
  return (map)
    #save as widget
  #saveWidget(MapTogether,"mypath.html",selfcontained=F) 
  
}
