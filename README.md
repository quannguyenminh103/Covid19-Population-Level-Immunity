# Covid19-Population-Level-Immunity
#Population Level Immunity Dashboard

Population immunity is the estimated percentage of the population expected to have (potentially, partial) immunity to Covid-19, where contours lines are equal to 100% x [1- (1-fraction_infected) x (1-fraction_immunized)]. These contours assume that immunization is independent of infection; if correlated then these will be over-estimates, if anti-correlated, then these will be under-estimates. Note that the dashboard makes a homogeneity assumption and does not break down infection and/or immunization rates by age. The vaccination type includes data on individuals with at least one dose or those considered fully vaccinated (2 doses for mRNA vaccines). The ascertainment bias is the ratio of actual cases to documented cases. Note that in both cases we use cumulative infection and vaccination data through two weeks ago, given expected delays before the onset of protective immunity.

**Data Sources:**
- [COVID-19 case data collated by The New York Times, based on reports from state and local health agencies](https://github.com/nytimes/covid-19-data).
- [Vaccination data collated by Our World in Data from United States Centers for Disease Control and Prevention](https://github.com/owid/covid-19-data/tree/master/public/data/vaccinations).
- Population data from the United States Census Bureau. 

**Developers:** [Quan M Nguyen](https://www.linkedin.com/in/king-nguyen-103/) , [Stephen J Beckett](http://sjbeckett.github.io/) and [Joshua S Weitz](https://weitzgroup.biosci.gatech.edu/). For more information contact Dr. Beckett (stephen.beckett@biology.gatech.edu) and Dr. Weitz (jsweitz@gatech.edu).
