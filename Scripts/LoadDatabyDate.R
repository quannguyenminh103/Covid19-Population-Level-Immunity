LoadDatabyDate <- function(stateInput, aBias, vaccinatedType){
  final_data <- data.frame()
  for (i in 1:length(stateInput)){
    infected_data <- cases_data %>% filter(state == stateInput[i]) %>% filter(date <= updated_date - 14)
    vaccination_data <- vacc_data %>% filter(state == stateInput[i]) %>% filter(date_vac <= updated_date - 14) %>%
      mutate(date_cases = date_vac + 14)
    ##vaccination_data$date <- vaccination_data$date_vac - 14
    joinData <- inner_join(infected_data, vaccination_data, by = c('date' = 'date_vac','state')) %>% select(date = date_cases, state, cases,
                                                                      people_vaccinated, people_fully_vaccinated,population, Code)
    final_data <- rbind(final_data,joinData)
  }
  ## Ascertainment bias = 4
  final_data$CaseFrac <- final_data$cases*aBias/final_data$population
  if (vaccinatedType == "At Least One Dose"){
    final_data$VaccFrac <- final_data$people_vaccinated/final_data$population
  } else {
    final_data$VaccFrac <- final_data$people_fully_vaccinated/final_data$population
    
  }
  final_data$ImmuLevel <-   final_data$CaseFrac + (1-final_data$CaseFrac)*final_data$VaccFrac
  FDATA <- final_data %>% select(Date = date, State = state, Code,
                                 CaseFrac, VaccFrac, ImmuLevel)
  FDATA$ABias <- aBias
  FDATA$VaccType <- vaccinatedType
  return(FDATA)
}
  