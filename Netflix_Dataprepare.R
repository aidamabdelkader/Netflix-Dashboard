library(flexdashboard)
library(knitr)
library(DT)
library(rpivotTable)
library(ggplot2)
library(plotly)
library(dplyr)
library(openintro)
library(highcharter)
library(ggvis)
library(readxl)
library(readr)
library(DataExplorer)
library(reshape2)
library(tidyr)
library(purrr)
library(useful) ## imported for using the simple imputer function 
library(devtools)
library(flipTime) ## thelibrary of flipTime is installed from github
library(lubridate)
library(stringr)
library(forcats)
library(formattable)
library(ecodist)
library(janitor)


#################################################################################################################



### Reading the Netflix data 

netflix_data <- read_excel("..//data//netflix_data.xlsx")

## Examining the missing data 
## To display the missing values per each column 
netflix_data %>% 
  map_df(~ sum(is.na(.))) %>% 
  gather(key = "Feature", value = "Na_Count") %>% 
  arrange(desc(Na_Count))


## Calculating the percentage of the missing values out of the total netflix data 
netflix_data %>% 
  is.na() %>% 
  colSums() / nrow(netflix_data) * 100


## plotting missing data 

plot_missing(netflix_data)

#### creating the get mode function 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
  
}


country_mode <- getmode(netflix_data$country)
date_added_mode <- getmode(netflix_data$date_added)
rating_mode <- getmode(netflix_data$rating)


netflix_data <- netflix_data %>% 
  mutate(country = if_else(is.na(country), country_mode, country), 
         date_added = if_else(is.na(date_added), date_added_mode, date_added), 
         rating = if_else(is.na(rating), rating_mode, rating), 
         cast = if_else(is.na(cast), "No Data", cast), 
         director = if_else(is.na(director), "No Data", director), 
         date_added = as.Date(date_added, format = "%b %d, %y"), 
         date_added = simple.impute(date_added, fun = median))


Netflix <- netflix_data %>% 
  mutate(Added_Year = year(date_added), 
         Added_month= month(date_added), 
         Added_Day = day(date_added), 
         director = gsub("[[:punct:]]", "", director), 
         main_country = str_split_fixed(country,",",2)[,1], 
         Target_Ages = case_when(rating == "TV-PG" ~ "Older Kids", 
                                 rating == "TV-MA" ~ "Adults", 
                                 rating == "TV-Y7-FV" ~ "Older Kids", 
                                 rating == "TV-Y7" ~ "Older Kids", 
                                 rating == "TV-14" ~ "Teenagers", 
                                 rating == "R" ~ "Adults", 
                                 rating == "TV-Y" ~ "Kids", 
                                 rating == "NR" ~ "Adults", 
                                 rating == "PG-13" ~ "Teenagers", 
                                 rating == "TV-G" ~ "Kids", 
                                 rating == "PG" ~ "Older Kids", 
                                 rating == "G" ~ "Kids",
                                 rating == "UR" ~ "Adults", 
                                 rating == "NC-17" ~ "Adults")) 


#################################################################################################################



Movie_TV_Show_Country <- Netflix %>% 
  group_by(main_country) %>% 
  summarise(Number = n()) %>% 
  ungroup()%>%
  arrange(desc(Number))  %>% 
  slice(1:10) %>% 
  mutate(main_country = fct_reorder(main_country,desc(Number)))
fig <- plot_ly(Movie_TV_Show_Country, x = ~main_country, y = ~Number, type = 'bar',
               text = ~ Number,
               texttemplate = '%{y:,}', textposition = 'outside',
               marker = list(color = c("#b20710", "#b20710", "#b20710", 
                                       "#221f1f","#221f1f","#221f1f","#221f1f",
                                       "#221f1f","#221f1f","#221f1f")))
fig <- fig %>% layout(xaxis = list(title = "",showgrid = FALSE),
                      yaxis = list(title = "", showgrid = FALSE))

fig



#################################################################################################################

### Top 10 Countries By TV show and Movies


Movie_TV_Show_Country <- Netflix %>% 
  mutate(Total = as.character("Total")) %>%
  group_by(main_country,type,Total) %>% 
  summarise(Number = n()) %>% 
  ungroup() %>% 
  left_join(Netflix %>% 
              mutate(Total = as.character("Total")) %>%
              group_by(main_country,Total) %>%
              summarize(Total_Number = n()) %>% 
              ungroup(), by = c("main_country", "Total")) %>% 
  mutate(Movie_TV_show_percentage = round(Number/Total_Number * 100)) %>% 
  arrange(desc(Total_Number))  %>% 
  slice(1:20) %>%
  arrange(desc(Movie_TV_show_percentage)) %>% 
  ungroup()

ggplotly(ggplot(Movie_TV_Show_Country, aes(fill = type, y = Movie_TV_show_percentage, x= main_country)) +
           geom_bar(position = "stack", stat = "identity") + 
           coord_flip() + 
           geom_text(aes(label = paste0(Movie_TV_show_percentage, "%"), x= main_country, y = Movie_TV_show_percentage), 
                     cex = 4, colour = "white", position = position_stack(vjust = 0.5)) + 
           scale_fill_manual(values = c("#b20710", "#221f1f")) + 
           theme_minimal() + 
           ylab("") + 
           xlab("") + 
           theme(plot.background =  element_blank(), 
                 panel.grid.major =element_blank(),
                 panel.grid.minor =  element_blank(), 
                 panel.border = element_blank(),
                 axis.ticks.x = element_blank(), 
                 axis.title.y = element_blank(),
                 axis.text.x = element_blank(),
                 axis.ticks.y = element_blank(), 
                 legend.title = element_blank()))



#################################################################################################################


### Plotting the Target Audience by country

Tagreted_Audience_Country <- Netflix %>% 
  group_by(main_country,Target_Ages) %>% 
  summarize(Number = n()) %>% 
  ungroup() %>% 
  filter(main_country %in% c("United States", "India", 
                             "United Kingdom", "Canada", 
                             "Japan", "France", "South Korea", 
                             "Spain", "Mexico", 
                             "Tureky"))

palette <- colorRampPalette(c('#f5f5f1','#e50914','#b20710','#221f1f'))
fig <- plot_ly(Tagreted_Audience_Country) %>%
  add_trace(
    type='heatmap',
    x = ~main_country,
    y = ~Target_Ages,
    z = ~Number, 
    colors = palette(4))  %>%
  add_trace(
    mode = "text",
    text = ~Number,
    type = "scattergl",
    textfont = list(
      size = 20
    ) )  %>% 
  layout(xaxis = list(title = ""),
         yaxis = list(title = ""))


fig