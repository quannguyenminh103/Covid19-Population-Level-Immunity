immuStatePlot <- function(stateInput, vaccinatedType, ascertainmentBias){
  base <- contourMap()
  for (i in 1:length(stateInput)){
    temp <- DATA %>% filter(Date == updated_date) %>% filter(State == stateInput[i]) %>%
      filter(ABias == ascertainmentBias) %>% filter(VaccType == vaccinatedType)
    base <- base %>% add_trace(temp, x = temp$CaseFrac, y = temp$VaccFrac, type = "scatter",
                               hovertemplate = paste0('<b>', temp$State,'</b><br>',
                                            'Population-level Immunity: ', round(temp$ImmuLevel*100), '%</br>',
                                            'Infected: ', round(temp$CaseFrac*100), '%</br>',
                                            'Vaccinated (', temp$VaccType,'): ', round(temp$VaccFrac*100), '%</br>',
                                            'Updated Date: ', temp$Date, '<extra></extra>'),
                               marker = list(color= colorSelected[i], size = 20, 
                                             line = list(color = 'black', width = 2)),
                               line = list(width = 0), showlegend = F,
                               texttemplate = temp$Code, textpositon = 'center',
                               textfont = list(color = 'black', size = 10))
  }
  
  return(base)
}

