## ----setup, include=FALSE---------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
library(tidyverse)
library(janitor)
library(readxl)
library(rworldmap)
library(leaflet)
#library(rstanarm)


## ----world_happiness_report_2019--------------------------------------------------------------

  happiness_2019 <- read_csv(file = "raw_data/2019 copy.csv",
                           col_types = cols(
  `Overall rank` = col_double(),
  `Country or region` = col_character(),
  Score = col_double(),
  `GDP per capita` = col_double(),
  `Social support` = col_double(),
  `Healthy life expectancy` = col_double(),
  `Freedom to make life choices` = col_double(),
  Generosity = col_double(),
  `Perceptions of corruption` = col_double())) %>% 
  clean_names()

happiness_2019


## ----WVS, cache=TRUE--------------------------------------------------------------------------
# should I add the raw data file to gitignore? website for reference:
# https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp I am using the
# reverse scale- so pay attention that for questions like - how strongly to you
# agree with a statement, 1 is disagree and 4 is agree. (the letter p- stands
# for positive, was added to variable names). An idea for a question- Are women
# more happy in an environment that is more liberal? interesting questions are-
# Q28- Q33 about the Status of Women. Q46 to Q56 are about happiness question
# about religious beliefs Q164 to Q175 Q260 is the respondent's sex (according
# to the interviewer's observation) Q261 is year of birth Q275- highest
# education level of respondent

WVS <- read_rds(file = "raw_data/World_Values_Survey_Wave_7_Inverted_R_v1_6 2") %>% 
  clean_names()

# variable code for the WVS

WVS_var <- read_xlsx(path = "raw_data/WVS-7_Variables_Report_Annex copy.xlsx") %>% 
  clean_names() %>% 
  rename("b_country" = "iso_3166_1_numeric_code")

WVS_countries <- WVS_var %>%
  select(country_territory, b_country)
  


## ---------------------------------------------------------------------------------------------
# plotting question 28

# q260 specifies the gender, it is either male of female (1/2) because I wanted
# to use fill = q260 in the plot I had to change it to a character so it won't
# be continues and I could use dodge later.

WVS$q260 <- as.character(WVS$q260)

plot_q28 <- WVS %>% 
  inner_join(WVS_countries, by = "b_country") %>%
  
  # for q260- 1 = Male, 2 = Female
  # q28- how much do you agree: When a mother works for pay, the children suffer
  
  select(b_country, country_territory, q261, q260, q28p) %>%
  
  # before I added drop_an, when I did the summarize I got many NA values.
  
  drop_na() %>%
  group_by(b_country,country_territory, q260) %>% 
  
  # calculating the average score q28 got- ranges from 1 (strongly disagree) to
  # 4 (strongly agree), according to gender and country.
  
  summarise(avg_q28 = sum(q28p)/n(), .groups = "drop") %>% 
  ggplot(mapping = aes(x = avg_q28,
                       y = fct_reorder(country_territory, avg_q28),
                       fill = q260)) +
  geom_col(position = "dodge") +
  labs(title = "Agree or Disagree:
       When a mother works for payment, her children suffer",
       subtitle = "Opinions are similar between men and women, but highly varied between countries",
       fill = "Gender",
       x = "Average value raging from 1- strongly disagree, to 4- strongly agree",
       y = NULL) +
  scale_fill_discrete(name = "Gender", labels = c("Male", "Female"))

plot_q28


## ---------------------------------------------------------------------------------------------
ggsave("plot_q28.png", plot_q28)


## ---------------------------------------------------------------------------------------------
# plotting question 29: Q32- Being a housewife is just as fulfilling as working
# for pay

plot_q29 <- WVS %>% 
  
  # joining WVS data with the countries names instead of number
  
  inner_join(WVS_countries, by = "b_country") %>%
  
  # for q260- 1 = Male, 2 = Female
  # q28- how much do you agree: When a mother works for pay, the children suffer
  
  select(b_country, country_territory, q261, q260, q29p) %>%
  
  # before I added drop_an, when I did the summarize I got many NA values.
  
  drop_na() %>% 
  group_by(b_country,country_territory, q260) %>% 
  
  # calculating the average score q28 got- ranges from 1 (strongly disagree) to
  # 4 (strongly agree), according to gender and country.
  
  summarise(avg_q29 = sum(q29p)/n(), .groups = "drop") %>% 
  ggplot(mapping = aes(x = avg_q29,
                       y = fct_reorder(country_territory, avg_q29),
                       fill = q260)) +
  geom_col(position = "dodge") +
  labs(title = "Agree or Disagree:\nBeing a housewife is just as fulfilling as working for pay",
       subtitle = "Ironically men tend to agree with this statement more than women do",
       fill = "Gender",
       x = "Average value raging from 1- strongly disagree, to 4- strongly agree",
       y = NULL) + 
  scale_fill_discrete(name = "Gender", labels = c("Male", "Female"))

plot_q29


## ---------------------------------------------------------------------------------------------
# plotting question 29: Q32- Being a housewife is just as fulfilling as working
# for pay. trying to do a posterior to check the causality between being a male
# and agreeing

plot_q29_new <- WVS %>% 
  
  # joining WVS data with the countries names instead of number
  
  inner_join(WVS_countries, by = "b_country") %>%
  
  # for q260- 1 = Male, 2 = Female

  select(b_country, country_territory, q260, q29p) %>%
  
  # male is 1 female is zero
  
  mutate(gender = ifelse(q260 == "1", 1, 0))
  
  
plot_q29_new

# answering the question of how gender affects agreement with q29

# fit_29 <- stan_glm(formula = q29p ~ gender,
#          data = plot_q29_new,
#          refresh = 0)
# 
# newobs <- tibble(gender = c(1,0))
# 
# posterior_epred(fit_29, newdata = newobs) %>% 
#   as_tibble() %>% 
#   mutate_all(as.numeric) %>% 
#   rowwise() %>% 
#   mutate(diff_male_minus_female = `1` - `2`) %>% 
#   ggplot(aes(x = diff_male_minus_female)) +
#     geom_histogram(aes(y = after_stat(count/sum(count))),
#                    bins = 100) +
#   scale_y_continuous(labels = scales::percent_format()) +
#     theme_classic()



## ---------------------------------------------------------------------------------------------
ggsave("plot_q29.png", plot_q29)


## ----freedom_happiness_2019--------------------------------------------------------------------
freedom <- happiness_2019 %>% 
  select(country_or_region, freedom_to_make_life_choices) %>% 
  mutate(freedom_to_make_life_choices_10 = freedom_to_make_life_choices*10)

# https://rpubs.com/emmavalme/rworldmap_vignette
#https://slcladal.github.io/maps.html#1_Getting_started_with_maps

joinData <- joinCountryData2Map(freedom,
                                joinCode = "NAME",
                                nameJoinColumn = "country_or_region")

theMap <- mapCountryData(joinData, nameColumnToPlot="freedom_to_make_life_choices", addLegend = FALSE, mapTitle = "Freedom To Make Life Choices", colourPalette = "heat")

#do.call(addMapLegend, c(theMap, legendWidth = 1, legendMar = 2))

max(freedom$freedom_to_make_life_choices)



## ----freedom_hapiness_2019_interactive--------------------------------------------------------
#  Freedom to make life choices is the national average of binary responses to
#  the GWP question “Are you satisfied or dissatisfied with your freedom to
#  choose what you do with your life?”

# the freedom data from 2019: 

freedom <- happiness_2019 %>% 
  select(country_or_region, freedom_to_make_life_choices) %>% 
  mutate(freedom_to_make_life_choices_10 = freedom_to_make_life_choices*10)

# joining freedom data with map data:

joinData <- joinCountryData2Map(freedom,
                                joinCode = "NAME",
                                nameJoinColumn = "country_or_region")

# defining color for the map:

qpal <- colorNumeric("magma",
                     joinData$freedom_to_make_life_choices_10, na.color = NA)


# the leaflet map:

freedom_interactive <- leaflet(joinData, 
                               options = leafletOptions(attributionControl = FALSE, minzoom=1.5)) %>%
  
  # I multiplied the score by 10 to create a scale of 1-10 because the original
  # value is from 0-1 to a boolean question which was averaged
  # defining the values read when hovering on a country:
  
  addPolygons(label= ~stringr::str_c(country_or_region, ' ',
                                     as.double(freedom_to_make_life_choices_10)),
              labelOptions= labelOptions(direction = 'auto'),
              weight=1, color='#333333', opacity=1,
              fillColor = ~qpal(freedom_to_make_life_choices_10), fillOpacity = 1,
              highlightOptions = highlightOptions(
                color='#000000', weight = 2,
                bringToFront = TRUE, sendToBack = TRUE)
  ) %>%
  addLegend(values = ~freedom_to_make_life_choices_10,
            opacity = 1, pal = qpal, title = htmltools::HTML("Freedom to Make Life Choices <br> 2019 World Happiness Report <h5>(from 1- lowest to 10- highest)</h5>"))  

# display visualization

freedom_interactive
## ----happiness--------------------------------------------------------------------------------
happiness_score <- happiness_2019 %>% 
  select(country_or_region, score)

happiness_score

# https://rpubs.com/emmavalme/rworldmap_vignette
#https://slcladal.github.io/maps.html#1_Getting_started_with_maps

joinDataHapinnes <- joinCountryData2Map(happiness_score,
                                joinCode = "NAME",
                                nameJoinColumn = "country_or_region")

#theMapHapiness <- mapCountryData(joinData, nameColumnToPlot="score", addLegend = FALSE, mapTitle = "Happiness", colourPalette = "heat")

#do.call(addMapLegend, c(theMap, legendWidth = 1, legendMar = 2))

max(happiness_score$score)


## ----happiness_interactive--------------------------------------------------------------------
# at first I used colorQuantile but it doesn't make sense for the data

qpal <- colorNumeric(rev(viridis::viridis(10)),
                      joinDataHapinnes$score, na.color = "gray")


happiness_interactive <- leaflet(joinDataHapinnes, 
             options = leafletOptions(attributionControl = FALSE, minzoom=1.5)) %>%
    addPolygons(label= ~stringr::str_c(country_or_region, ' ', as.double(score)),
    labelOptions= labelOptions(direction = 'auto'),
    weight=1, color='#333333', opacity=1,
    fillColor = ~qpal(score), fillOpacity = 1,
    highlightOptions = highlightOptions(
      color='#000000', weight = 2,
      bringToFront = TRUE, sendToBack = TRUE)
    ) %>%
  addLegend(values = ~score,
    opacity = 1, pal = qpal, title = htmltools::HTML("Happiness score from 1 lowest to 10 highest <br> 2019 World Happiness Report"))  

happiness_interactive

