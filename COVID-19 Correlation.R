library(COVID19)
library(tidyverse)
library(ggthemes)
library(ggrepel)

countries_to_investigate = SOUTHERN_EU

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

first_day <- covid %>% 
  group_by(id) %>% 
  filter(confirmed != 0 & id %in% last_day$id) %>%
  summarize(day_1=min(date))

#Filtering datasets and updating
density_dataset <- density_dataset %>% filter(id %in% last_day$id)
covid <- covid %>% filter(id %in% last_day$id)

density_dataset <- density_dataset %>% mutate(days_to = as.integer(last_day$day_1000 - first_day$day_1))

#Variables
case_fatality <- vector()
survival_rate <- vector()
all_ids <- levels(as.factor(covid$id))



#================================================================================================#
#Outer Function
compare_countries <- function(countries=countries_to_investigate, prelim_plot=0){
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
  
  ##Preliminary Data Exploration   (OPTIONAL)
  
  if(prelim_plot == 1){
    #Get plots
    all_preliminary_plots <- lapply(countries_of_interest, preliminary_data_exploration)
    
    #Plot ggplots OR place in pdf if 5 countries or more compared
    if(length(countries_of_interest) <= 4){
      grid.arrange(grobs=all_preliminary_plots, nrow=length(countries_of_interest))
    }else{
      pdf("COVID-19 Exploratory Data Analysis.pdf", onefile=TRUE)
      for(i in seq(length(all_preliminary_plots))){
        do.call("grid.arrange", all_preliminary_plots[i])
      }
      dev.off()
    }
  }
  
  #Create Correlation Plot && Display Information in a Table
  create_corplot(countries_of_interest)
}
#================================================================================================#
###Preliminary Data Exploration
preliminary_data_exploration <- function(countries){
  #Obtain local data
  local_covid_data <- covid %>% filter(id == country)
  
  #Compute Estimate Case Fatality and Proportion who Recovered   &&  Replace NaN
  deaths_per_current_cases <- local_covid_data[["deaths"]] / 
    sympto_and_asympto(local_covid_data[["confirmed"]])
  recover_per_current_cases <- local_covid_data[["recovered"]] / 
    sympto_and_asympto(local_covid_data[["confirmed"]])
  
  deaths_per_current_cases <- replace_na(deaths_per_current_cases, 0)
  recover_per_current_cases <- replace_na(recover_per_current_cases, 0)
  
  #Aesthetics Variables
  legend_x_pos <- round(local_covid_data$date[length(local_covid_data$date)/2])
  cases_began <- local_covid_data %>% filter(confirmed > 0) %>%
    pull(date) 
  cases_thousand <- local_covid_data %>% filter(confirmed >= 1000) %>%
    pull(date) 
  label_began <- data.frame(my_text="first case", x = cases_began[1]-2, y = 0.41)
  label_thousand <- data.frame(my_text="first thousand cases", x = cases_thousand[1]-2, y = 0.41)
  
  #Plot
  local_covid_data %>% ggplot(aes(date)) +
    geom_line(aes(y=deaths_per_current_cases, color="who died"), size=0.75, alpha=0.9) +
    geom_line(aes(y=recover_per_current_cases, color="who recovered"), size=0.75, alpha=0.9) +
    geom_vline(xintercept=cases_began[1], linetype="dotted") +
    geom_vline(xintercept=cases_thousand[1], linetype="dotted") +
    geom_text(data=label_began, aes(x, y, label=my_text), size=2.5, angle=90) +
    geom_text(data=label_thousand, aes(x, y, label=my_text), size=2.5, angle=90) +
    ggtitle(sprintf("COVID-19 Case Fatality Rate & Proportion who Recovered in %s", country)) +
    xlab(NULL) +
    scale_y_continuous(name="Proportion of Infected\n", limits = c(0, 0.9)) + 
    scale_color_manual(values=c("red", "blue")) +
    theme_clean() +
    theme(plot.title = element_text(size=12, vjust=0.5, hjust=0.5, face="bold"),
          axis.title.y = element_text(vjust=1, size=10),
          legend.position = "bottom",
          legend.title=element_blank())
}
#================================================================================================#
###Update Global Fatality Rate and Proportion who Recovered
cumulative_rates_per_country <- function(country){
  local_covid_data <- covid %>% filter(id == country)
  
  local_fatality <- local_covid_data %>% 
    filter(date == max(date)) %>%
    summarize(fatality= deaths/confirmed) %>%
    pull(fatality)
  case_fatality <<- c(case_fatality, local_fatality)
  
  prop_recovered <- local_covid_data %>% 
    filter(date == max(date)) %>%
    summarize(survival= recovered/confirmed) %>%
    pull(survival)
  survival_rate <<- c(survival_rate, prop_recovered)
}

#================================================================================================#
###Helper FUNCTION: Provides estimate of true number of cases
sympto_and_asympto <- function(cases){
  avg <- 17.9                                         #Estimation by Mizumoto et al.
  sigma <- (avg - 15.5)/2
  
  #Taking a sample of possible proportion of asymptomatic cases
  prop <- sample(rnorm(1000, avg, sigma), 1, replace=TRUE) / 100
  (prop*cases) + cases
}
#================================================================================================#
###Create Correlationary Plots                            ##UPDATE THIS FOR DIFFERENT VARIABLES TO CORRELATE
create_corplot <- function(countries){
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
#================================================================================================#
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
#
#
#
#
#
#
#
#
#
#
#####################################################################################################
#####################################################################################################
###################################          INSTRUCTIONS          ##################################

##To compare two or more countries, simply type "compare_countries(c("country1_id", "country2_id", ...))

#To compare regions, refer to regions and sub-regions above.
    #e.g. compare_countries(EUROPE)
    #e.g. compare_countries(SOUTHERN_EU)

##OPTIONAL: Preliminary Data Exploration
#If you wish to include this, update additional parameter. 
    #e.g. compare_countries(EUROPE, prelim_plot=1), or simply compare_countries(EUROPE, 1)

##OPTIONAL: To change variables of interest
#Manually change variables in function create_corplot()
#####################################################################################################
#####################################################################################################



