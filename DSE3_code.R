#Clear the environment
rm(list=ls(all=TRUE))

#setting the working directory
setwd("/Users/jp/DataScienceExam3")

#load tidycensus
library(tidycensus)

#load my API Key
census_api_key("2a948c32a69d16b8ec8ecda67d3076cc9eb6126c")

#load the variables
v10 <- load_variables(year = 2010, dataset = "acs5")
#Used this to locate GINI data by searching "GINI" in view panel (GINI code: B19083_001)

#import state level data for both years
gini10 <- get_acs(geography = "state",
                 variables = c(poverty = c("B19083_001")),
                 year = 2010)

gini15 <- get_acs(geography = "state",
                  variables = c(poverty = c("B19083_001")),
                  year = 2015)

#add year column
gini10$year <- 2010
gini15$year <- 2015

#combine
#load package to deal with these big boys
library(bit64)

#put our data into one big boy panel
library(tidyverse)
inequality_panel = bind_rows(gini10, gini15)

#rename variables
library(data.table)
setnames(inequality_panel, "NAME","state" )
setnames(inequality_panel, "estimate","gini" )

#order
inequality_panel = inequality_panel[order(inequality_panel$state, 
                                          inequality_panel$year), ]

head(inequality_panel)

#make it wide by GINI
wide_by_gini <-
  inequality_panel %>%
  pivot_wider(id_cols = c("gini", "year"),
              names_from = c("state", "GEOID"),
              values_from = "gini")

#make it wide by GINI (2)
wide_by_gini <-
  inequality_panel %>%
  pivot_wider(id_cols = c("gini", "year"),
              names_from = "state",
              values_from = "gini")

head(wide_by_gini)


#make it long again
long_inequality <-
  wide_by_gini %>% 
  pivot_longer(cols =starts_with("year"),
               names_to = "year",
               values_to = "gini",
               values_drop_na = FALSE) %>%
  filter(!(current_amount==0))

#showing they are different
str(wide_by_gini)
str(long_inequality)

##############
#pivot the data wide by country
wide_by_country <-
  collapsed_data %>%
  pivot_wider(id_cols = c("country_name", "year"),
              names_from = "country_name",
              values_from = "current_amount")

#pivot to wide by year
wide_by_year <- 
  collapsed_data %>% 
  pivot_wider(id_cols =c("country_code", "country_name", "year"),
              names_from = "year",
              values_from = "current_amount",
              names_prefix = "year")

#pivot longer (reshape) (back to alldata)
long_data_frame <-
  wide_by_year %>% 
  pivot_longer(cols =starts_with("year"),
               names_to = "year",
               names_prefix = "year_",
               values_to = "current_amount",
               values_drop_na = FALSE) %>%
  filter(!(current_amount==0))

###################

collapsed_data <- 
  inequality_panel %>%
  group_by(GEOID, year, gini) %>%
  summarize(across(where(is.numeric), mean)) %>%
  select (-c("transaction_id"))

#pulling Mike's PDF
library(pdftools)
library(tidyr)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)

mytext=pdf_text(pdf = "https://pdf.usaid.gov/pdf_docs/PA00TNMG.pdf") 

#converting to df
armeniatext=as.data.frame(mytext)
armeniatext$page=c(1:65)
colnames(armeniatext)[which(names(armeniatext) == "mytext")] <- "text"

#tokenize/remove stop words
armeniatext=armeniatext %>%
  unnest_tokens(word, text)

armeniatext=armeniatext %>%
anti_join(stop_words)

#frequency analysis
arfreq <- armeniatext %>%
  count(word, sort = TRUE)

head(arfreq)



