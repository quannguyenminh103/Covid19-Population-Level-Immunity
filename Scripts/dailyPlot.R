dailyPlot <- function(stateInput, vaccinatedType, ascertainmentBias){
  base <- contourMap()
  for (i in 1:length(stateInput)){
    stateData <- DATA %>% filter(State == stateInput[i]) %>% filter(ABias == ascertainmentBias) %>% filter(VaccType == vaccinatedType)
    stateData$ImmuLevelLabels <- stateData$ImmuLevel/max(stateData$ImmuLevel, na.rm =TRUE)
    # get the first date of each month
    firstDateList <- seq(from = ceiling_date(stateData$Date[length(stateData$Date)], "month"), to = floor_date(stateData$Date[1], 'month'), by = "months")
    if (firstDateList[length(firstDateList)] == stateData$Date[1]) {
      firstDateList <- c(stateData$Date[length(stateData$Date)],firstDateList[1:length(firstDateList)-1])
    } else {
      firstDateList <- c(stateData$Date[length(stateData$Date)],firstDateList)
    }
    # draw all dates
    temp <- stateData[-is.element(stateData$Date,firstDateList),]
    len <- length(temp$CaseFrac)
    base <- base %>% add_trace(temp[2:len,], x = temp$CaseFrac[2:len], y = temp$VaccFrac[2:len],
                                       type = 'scatter', color = 'transparent',
                                       hovertemplate = paste0('<b>', temp$State[2:len], '</b><br>',
                                                             'Population-level Immunity: ', round(temp$ImmuLevel[2:len]*100), '%</br>',
                                                             'Infected: ', round(temp$CaseFrac[2:len]*100), '%</br>',
                                                             'Vaccinated (', temp$VaccType[2:len],'): ', round(temp$VaccFrac[2:len]*100), '%</br>',
                                                             'Updated Date: ', temp$Date[2:len],'<extra></extra>'),
                                       marker = list(color = colorSelected[i], opacity = 0.75,
                                                     size = temp$ImmuLevelLabels[2:len]*20, # zoom 20 times
                                                     sizemode = 'diameter', sizeref = stateData$ImmuLevelLabels[1],
                                                     line = list(width = 0)),
                                       showlegend = F, connectgaps = TRUE) %>%
      # connecting lines
      add_trace(stateData, x = stateData$CaseFrac, y = stateData$VaccFrac, type = 'scatter', mode = 'lines',line = list(width = 2, dash = 'line'), showlegend = F) #%>%
      # # line between the default point to the first date of vaccination
      # add_trace(x = c(0.3,stateData$CaseFrac[len]), y = c(0, stateData$VaccFrac[len]), type = 'scatter', mode = 'lines', 
      #           line = list(width = 2, dash = 'dash'), showlegend = F)
    # draw the first date of each month
    monthData <- stateData[is.element(stateData$Date,firstDateList),]
    base <- base %>% add_trace(monthData, x = monthData$CaseFrac, y = monthData$VaccFrac, type = 'scatter', mode = 'marker',
                      hovertemplate = paste0('<b>', monthData$State[1],'</b><br>',
                                            'Population-level Immunity: ', round(monthData$ImmuLevel*100), '%</br>',
                                            'Infected : ', round(monthData$CaseFrac*100), '%</br>',
                                            'Vaccinated (', monthData$VaccType,'): ', round(monthData$VaccFrac*100), '%</br>',
                                            'Updated Date: ', monthData$Date, "<extra></extra>"),
                       marker = list(size = monthData$ImmuLevelLabels*20,
                                     sizemode = 'diameter', sizeref = stateData$ImmuLevelLabels[1], opacity = 0.75,
                                     color = colorSelected[i],
                                     line = list(color = 'grey', width = 3, opacity = 5)),
                                     line = list(width = 0),
                       showlegend = F) %>%
      # the last date of the month
      add_trace(stateData[1], x = stateData$CaseFrac[1], y = stateData$VaccFrac[1], 
                type = "scatter", 
                hovertemplate = paste0('<b>', stateData$State[1],'</b><br>',
                                      'Population-level Immunity: ', round(stateData$ImmuLevel[1]*100), '%</br>',
                                      'Infected : ', round(stateData$CaseFrac[1]*100), '%</br>',
                                      'Vaccinated (', stateData$VaccType[1],'): ', round(stateData$VaccFrac[1]*100), '%</br>',
                                      'Updated Date: ', stateData$Date[1], "<extra></extra>"),
                color = 'black', 
                marker = list(size = stateData$ImmuLevelLabels[1]*20, 
                              #size = 15,
                              sizemode = 'diameter', sizeref = stateData$ImmuLevelLabels[1],
                              color = colorSelected[i], opacity = 1,
                              line = list(color = 'black', width = 2)), showlegend = F,
                texttemplate = stateData$Code[1], textpositon = 'inside',
                textfont = list(color = 'black', size = 10))
    }
  base
  return(base)
}

