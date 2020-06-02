setwd("C:/Users/Stanley/Desktop/Learning R/Projects/UnBDC COVID19 Project/Code")

library(COVID19)
library(tidyverse)
library(ggthemes)
library(ggrepel)

countries_to_investigate = SOUTHERN_EU

#AMERICA => AFRICA => ASIA => EUROPE

#================================================================================================#
#Get Data
density_dataset <- read.delim("Country Population Density 2018.txt", header=TRUE) %>%
  select(Country.Name, Country.Code, X2018..YR2018.) %>% filter(X2018..YR2018. != "..") %>% 
  mutate(Popu_Density= round(as.integer(X2018..YR2018.))) %>% 
  select(Country.Code, Popu_Density) %>%
  arrange(Country.Code)
names(density_dataset)[1] <- "id"

covid <- covid19() %>% 
  select(id, date, confirmed, recovered, deaths, population) %>%
  filter(id %in% density_dataset$id)

#Get Total Days Till 1000 Cases
min_cases = 2000

last_day <- covid %>% 
  group_by(id) %>% 
  filter(confirmed >= min_cases) %>%
  summarize(day_1000=max(date))

#What I did was use max() instead of min ^^^^^

first_day <- covid %>% 
  group_by(id) %>% 
  filter(confirmed != 0 & id %in% last_day$id) %>%
  summarize(day_1=min(date))

density_dataset <- density_dataset %>% filter(id %in% last_day$id)
covid <- covid %>% filter(id %in% last_day$id)

density_dataset <- density_dataset %>% mutate(days_to = as.integer(last_day$day_1000 - first_day$day_1))

#Variables
case_fatality <- vector()
survival_rate <- vector()
all_ids <- levels(as.factor(covid$id))




#Outer Function
compare_countries <- function(countries=countries_to_investigate){
  countries_of_interest <- NULL
  
  #Validate Input
  if(countries[1]=="all"){
    countries_of_interest = all_ids
  }else{
    for(possible_id in countries){
      if(possible_id %in% all_ids){
        countries_of_interest <- c(countries_of_interest, possible_id)
      }
    }
    stopifnot(!is.null(countries_of_interest))
  }
  
  #Sort Countries
  countries_of_interest <- sort(countries_of_interest)
  
  density_dataset <- density_dataset %>% filter(id %in% countries_of_interest)
  
  #Update Values
  lapply(countries_of_interest, cumulative_rates_per_country)
  create_plot(countries_of_interest)
}


#Get Fatality Rate and Survival Rate
cumulative_rates_per_country <- function(country){
  local_covid_data <- covid %>% filter(id == country)
  
  local_fatality <- local_covid_data %>% 
    filter(date == max(date)) %>%
    summarize(fatality= deaths/confirmed) %>%
    pull(fatality)
  case_fatality <<- c(case_fatality, local_fatality)
  
  local_survival <- local_covid_data %>% 
    filter(date == max(date)) %>%
    summarize(survival= recovered/confirmed) %>%
    pull(survival)
  survival_rate <<- c(survival_rate, local_survival)
}


#Create Plot
create_plot <- function(countries){
  region <- vector()
  
  for(country in countries){
    if(country %in% EASTERN_AF){
      region <- c(region, "Eastern Africa")
    }else if(country %in% MIDDLE_AF){
      region <- c(region, "Middle Africa")
    }else if(country %in% NORTHERN_AF){
      region <- c(region, "Northern Africa")
    }else if(country %in% SOUTHERN_AF){
      region <- c(region, "Southern Africa")
    }else if(country %in% WESTERN_AF){
      region <- c(region, "Western Africa")
    }else if(country %in% EASTERN_EU){
      region <- c(region, "Eastern Europe")
    }else if(country %in% NORTHERN_EU){
      region <- c(region, "Northern Europe")
    }else if(country %in% SOUTHERN_EU){
      region <- c(region, "Southern Europe")
    }else if(country %in% WESTERN_EU){
      region <- c(region, "Western Europe")
    }else if(country %in% CENTRAL_AS){
      region <- c(region, "Central Asia")
    }else if(country %in% EAST_AS){
      region <- c(region, "East Asia")
    }else if(country %in% SOUTH_AS){
      region <- c(region, "South Asia")
    }else if(country %in% SOUTHEAST_AS){
      region <- c(region, "Southeast Asia")
    }else if(country %in% WESTERN_AS){
      region <- c(region, "Western Asia")
    }else if(country %in% NORTHERN_AM){
      region <- c(region, "North America")
    }else if(country %in% CARRIBEAN_AM){
      region <- c(region, "Carribean")
    }else if(country %in% CENTRAL_AM){
      region <- c(region, "Central America")
    }else if(country %in% SOUTH_AM){
      region <- c(region, "South America")
    }else{
      region <- c(region, "Others")
    }
  }
  
  region <- as.factor(region)
  cases <- covid %>% 
    select(id, confirmed, date) %>% 
    filter(id %in% countries & date == max(date)) %>% 
    pull(confirmed)
  popu <- covid %>% 
    select(id, population, date) %>% 
    filter(id %in% countries & date == max(date)) %>% 
    pull(population)
  prop_infected = cases/popu
  
  
  compared_countries <- data.frame(id=countries,
                                   region=region,
                                   population=popu,
                                   population_infected=prop_infected,
                                   population_density=density_dataset %>%
                                     filter(id %in% countries) %>% pull(Popu_Density),
                                   survival_rate=survival_rate,
                                   case_fatality_rate=case_fatality,
                                   days_since_start= density_dataset %>%
                                     filter(id %in% countries) %>% pull(days_to),
                                   total_cases=cases)
  
  print(compared_countries %>% arrange(population))
  
  linear_reg <- compared_countries %>% lm(formula=case_fatality_rate~days_since_start)
  print(summary(linear_reg))
  
  cor_stat <- cor(compared_countries$case_fatality_rate, compared_countries$days_since_start, method="kendall")
  print(cor_stat)
  
  compared_countries %>%
    ggplot(aes(x=days_since_start, y=case_fatality_rate, label=id, color=id)) +
    geom_abline(intercept=-0.196542, slope=0.002685, color="black") +
    geom_point(size=5, show.legend = FALSE, alpha=0.5) +
    geom_text_repel(size=6, color="black", show.legend = FALSE) +
    geom_text(size=6, x=113, y=0.05, label="R-Squared: 0.8762\np-value: 0.0006223", hjust=0, color="black") +
    xlab(sprintf("\nDays since first case")) +
    ylab("Case Fatality Rate\n(Proportion of Infected Who Died\n") +
    ggtitle("Southern European Countries with 1000 or more cases") +
    theme_minimal() +
    theme(plot.title = element_text(size=18, vjust=0.5, hjust=0.5, face="bold"),
          axis.title.y = element_text(size=15, lineheight=1.2),
          axis.title.x = element_text(size=15, lineheight=1))
}



#x=days_till 1000 cases vs case_fatality
#Dependent Variables: survival_rate, case_fatality, days_till
#Independent Variables: days_till, population_density

##OBSERVATIONS  [Independent vs Dependent]

#[Days Since 1st Case] VS [Case Fatality]     #Direct Relationship
        #Western Asia       p-value: 0.04456  #w/ Turkey outlier removed
        #Southern Europe    
              #p-value: 0.0004644    
              #R2 Value: 0.8762
              #Kendall Corr: 0.5455447

#[Days till 1000 Cases] vs [Case Fatality]
        #Southeast Asia     p-value: 0.1739

#[Days till 1000 Cases] vs [Population Density]
        #Eastern Europe     p-value: 0.04649    #after removing BLR, BGR, RUS, UKR, ROU


# Proportion of Population Infected vs Case Fatality Rate
        #Western Africa
              #p-value = 0.05289
              #R2 value = 0.7632
              #Kendell Tau's Corr = -0.6

##TO STUDY
#Density Graph Per Region (i.e. AFRICA, EUROPE ,etc.) of days_till 1000 cases
    #Notice that Asia (not land-locked countries) tend to take longer to reach 1000 cases versus Europe

#Experiment with Days_Till n, where n is 100, 500, 1000, 1500 and 2000. Maybe make a grid plot?

#QQPLOT PLEASE to check if variables are normally distributed


#################################################################################################
ALL <- c("all")

#Africa Subregions
EASTERN_AF <- c("BDI", "COM", "DJI", "ERI", "KEN", "MDG", "MWI", "MUS", "MYT", "MOZ", "REU", "RWA", "SYC", "SOM", "TZA", "UGA", "ZMB", "ZWE") 
MIDDLE_AF <- c("AGO", "CMR", "CAF","TCD", "COG", "COD", "GNQ", "GAB", "STP")
NORTHERN_AF <- c("DZA", 'EGY', 'LBY', 'MAR', 'SSD', 'SDN', 'TUN', 'ESH')
SOUTHERN_AF <- c('BWA', 'SWZ', 'LSO', 'NAM', 'ZAF')
WESTERN_AF <- c('BEN', 'BFA', 'CPV', 'CIV', 'GMB', 'GHA', 'GIN', 'GNB', 'LBR', 'MLI', 'MRT', 'NER', 'NGA', 'SHN', 'SEN', 'SLE', 'TGO')

AFRICA <- c(EASTERN_AF, MIDDLE_AF, NORTHERN_AF, SOUTHERN_AF, WESTERN_AF)

#Asia Subregions
CENTRAL_AS <- c('TJK', 'UZB', 'KAZ', 'TKM', 'KGZ')
EAST_AS <- c('CHN', 'MNG', 'PRK', 'KOR', 'JPN', 'HKG', 'TWN', 'MAC')
SOUTH_AS <- c('LKA', 'BGD', 'IND', 'AFG', 'PAK', 'BHU', 'NPL', 'MDV')
SOUTHEAST_AS <- c('VNM', 'MYS', 'IDN', 'LAO', 'BRN', 'SGP', 'PHL', 'MMR', 'TLS', 'THA', 'KHM', "CXR", 'CCK')
WESTERN_AS <- c('GEO', 'ARM', 'AZE', 'TUR', 'CYP', 'SYR', 'LBN', 'ISR', 'PSE', 'JOR', 'IRQ', 'IRN', 'KWT', 'BHR', 'QAT', 'SAU')

ASIA <- c(CENTRAL_AS, EAST_AS, SOUTH_AS, SOUTHEAST_AS, WESTERN_AS)

#Europe Subregions
EASTERN_EU <- c('BLR', 'BGR', 'CZE', 'HUN', 'POL', 'MDA', 'ROU', 'RUS', 'SVK', 'UKR')
NORTHERN_EU <- c('ALA', 'DNK', 'EST', 'FRO', 'FIN', 'GGY', 'ISL', 'IRL', 'IMN', 'JEY', 'LVA', 'LTU', 'NOR', 'SJM', 'SWE', 'GBR')
SOUTHERN_EU <- c('ALB', 'AND', 'BIH', 'HRV', 'GIB', 'GRC', 'ITA', 'MKD', 'MLT', 'MNE', 'PRT', 'SMR', 'SRB', 'SVN', 'ESP', 'VAT')
WESTERN_EU <- c('AUT', 'BEL', 'FRA', 'DEU', 'LIE', 'LUX', 'MCO', 'NLD', 'CHE')

EUROPE <- c(NORTHERN_EU, SOUTHERN_EU, WESTERN_EU, EASTERN_EU)

#America subregions
NORTHERN_AM <- c('CAN', 'GRL', 'MEX', 'USA')
CARRIBEAN_AM <- c('AIA', 'ATG', 'ABW', 'BHS', 'BRB', 'BMU', 'VGB', 'CYM', 'CUB', 'CUW', 'DMA', 'DOM', 'GRD', 'GLP', 'HTI',
               'JAM', 'MTQ', 'MSR', 'PRI', 'KNA', 'LCA', 'VCT', 'TTO', 'VIR')
CENTRAL_AM <- c('BLZ', 'CRC', 'SLV', 'GTM', 'HND', 'NIC', 'PAN')
SOUTH_AM <- c('ARG', 'BOL', 'BRA', 'CHL', 'COL', 'ECU', 'GUF', 'GUY', 'PRY', 'PER', 'SUR', 'URY', 'VEN')

AMERICA <- c(NORTHERN_AM, CARRIBEAN_AM, CENTRAL_AM, SOUTH_AM)












