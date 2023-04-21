#link to video turorial: https://www.youtube.com/watch?v=DiY8EqZDwoI&ab_channel=Dataslice
#link to tidyverse tutorial: https://www.youtube.com/watch?v=grB4aW41-Mw&ab_channel=AndrewCouch

#________________________________________________________________________________
# TIDY DATA:
# [1] every column is a variable
# [2] every row is an observation
# [3] every cell is a single variable
#________________________________________________________________________________

#install packages####
install.packages("tdyverse")
install.packages("lubridate")

#load packages####
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(tidyr)

#load data frames####

#INSERT read_csv(FILE_PATH)
getwd() #gets the current working directory
setwd("C:/prog/R_cheat-sheets/R_tidyverse_cheat-sheet") #needs to be set to current wd

#import csv files
mtv_1 <- read_csv("MTV 1.csv") #header = TRUE ensures that R knows about the column names
mtv_2 <- read_csv("MTV 2.csv")
mtv_3 <- read_csv("MTV 3.csv")
mtv_4 <- read_csv("MTV 4.csv")

mtv_df <- bind_rows( #combines the imported dfs to one
  mtv_1,
  mtv_2,
  mtv_3,
  mtv_4)
mtv_df_lower <- mtv_df %>%
  mutate(
    across(
      .cols = where(is.character), tolower))


#ESSENTIALS_________________________________________________________________####

#piping####

#passes what comes before %>% as first argument into the function
charts %>% head(10)
#pass it to specific point with .
10 %>% head(charts, .)

#select()####

#selects columns
charts %>%
  select(date, rank, song, artist, weeks.on.board) %>% View()
charts %>%
  select(date:artist, weeks.on.board) %>% View() #use : to select a range of columns
charts %>%
  select(date:artist, weeks_popular = weeks.on.board) %>% View() #use = to rename column
charts %>%
  select(-song, -artist) %>% View() #select all columns except
iris %>%
    mutate(
      sqrt_sepal_length = sqrt(Sepal.Length)) %>%
    select(sqrt_sepal_length,
           everything()) %>% View() #select everything
starwars %>%
  select(name, ends_with("color"))

#mutate()####

#comptue or append columns
charts_manipulated <- charts %>%
  select(song, artist) %>%
  mutate(is_collab = grepl('featuring', tolower(artist))) %>%
  mutate(is_justin = grepl('justin bieber', tolower(artist))) %>%
  select(is_justin, song, artist, is_collab) %>% View()
iris %>%
  mutate(sqrt_sepal = sqrt(Sepal.Length),
         length_larger_than_five = if_else(Sepal.Length > 5, "greather than 5", 
                                    if_else(Sepal.Length == 5, "equal 5", "less than 5"))) %>% 
  mutate(
    across(
      .cols = where(is.character), toupper)) %>% View()

#filter()####

#for selecting rows
charts_manipulated %>%
    select(is_justin, artist, song, is_collab) %>%
    filter(is_justin == TRUE, is_collab == TRUE) %>%
    unique() %>% View()
charts_taylor_or_drake <- charts %>%
  mutate(is_taylorswift = grepl('taylor swift', tolower(artist))) %>%
  mutate(is_drake = grepl('drake', tolower(artist))) %>%
  select(is_taylorswift, is_drake, weeks.on.board, artist, song) %>%
  filter(is_taylorswift == TRUE | is_drake == TRUE) %>% View()
iris %>%
  filter(Species != "setosa") %>% View() #==, !=, etc.
iris %>%
  filter(Species %in% c("setosa", "versicolor")) %>% View()
iris %>%
  filter(Species == "setosa" | Sepal.Length >= 5) %>% View() #| = or, & = and
starwars %>%
  select(name, ends_with("color")) %>%
  filter(hair_color %in% c("blond", "brown"))

#distinct()####
View(charts_taylor_or_drake)
charts_taylor_or_drake %>%
  distinct(song) %>%
  .$song #.$ creates vector

#grouping()/summarise()/arrange()####
charts_taylor_or_drake %>%
  filter(is_drake == TRUE) %>%
  group_by(song, artist) %>%
  summarise(total_weeks_popular = max(weeks.on.board)) %>% #needs to follow after a group_by
  arrange(desc(total_weeks_popular), song) %>% #per default in ascending order
  head(10) #getting top 10
iris %>%
  group_by(Species) %>%
  mutate(avg_sepal_length = mean(Sepal.Length)) %>% #summarise doesn't work here because we need the dropped columns later
  arrange(desc(avg_sepal_length)) %>%
  ungroup() %>% #ungrouping
  mutate(avg_total_sepal_length = mean(Sepal.Length), #averaging without grouping via mutate(), gives avg of whole column
         sd_sepal_length = sd(Sepal.Length)) %>% View()
iris %>%
  group_by(Species) %>%
  summarise(avg_sepal_width = mean(Sepal.Width),
            observations = n()) %>% View() #count it with on()

#count()####
charts %>%
  mutate(is_kanye = grepl('kanye west', tolower(artist))) %>%
  distinct(song, is_kanye) %>%
  count(is_kanye) %>%
  filter(is_kanye == TRUE) %>%
  select(is_kanye, number_of_songs = n) %>% View()
iris %>%
  mutate(sqrt_sepal = sqrt(Sepal.Length),
         length_larger_than_five = if_else(Sepal.Length > 5, "greater than 5", 
                                          if_else(Sepal.Length == 5, "equal 5", "less than 5"))) %>%
  count(Species, length_larger_than_five) %>% #counting
  pivot_wider(names_from = length_larger_than_five, values_from = n) %>% View() #pivot wider by species
iris %>%
  mutate(length_larger_than_five = if_else(Sepal.Length > 5, "greater than 5", 
                                           if_else(Sepal.Length == 5, "equal 5", "less than 5"))) %>%
  count(Species, length_larger_than_five, sort = TRUE, name = "count") %>% View() #sort & rename column while 
iris %>%
  add_count(Species) %>% View() #adds a column for count

#ADVANCED___________________________________________________________________####
#create new df with selected columns
music_df <-  charts %>%
  select(date:artist, weeks_popular = weeks.on.board)

#character vector to date####
music_df %>%
  mutate(date = ymd(date)) -> music_df #round down to get corresponding month
View(music_df)

#splitting up charachters####
music_df <- 
  music_df %>%
  mutate(
    primary_artist = ifelse(
      str_detect(artist, 'Featuring'),
      str_match(artist, '(.*)\\sFeaturing')[,2],
      artist),
    featured_artist = str_match(artist, 'Featuring\\s(.*)')[,2])
View(music_df)  
  

#drop_na()####
bill_df %>% 
  drop_na() %>% View()
iris %>%
  add_row(Species = "TESTER") %>% #adds row to a dataset, not specified column values lead to NA
  drop_na(c("Sepal.Width")) %>% View() #drops rows with NA in specific color

#seperate()####
bill_df %>%
  separate_rows(artist, sep = " ")

#case_when####
iris %>%
  mutate(test = case_when( #for multiple if_else statements
    Sepal.Length > 5 & Sepal.Width > 4 ~ "Large", 
    Sepal.Length < 4.5 & Sepal.Width < 3.5 ~ "Small",
    TRUE ~ "Standard")) %>% 
  group_by(test) %>%
  summarise(avg_length_by_group = mean(Sepal.Length),
            avg_width_by_group = mean(Sepal.Width),
            count = n()) %>% View()

#window functions####
economics %>%
  select(date, unemploy) %>%
  mutate(previous_unemploy = lag(unemploy, 
                                 n = 1,
                                 order_by = date, 
                                 default = mean(unemploy))) %>% View()

#ranking####
iris %>%
  arrange(desc(Sepal.Length)) %>% #first arrange in descending order
  mutate(s_length_rank_by_row = row_number()) %>% #ranks by row numbers, cannot handel ties
  mutate(s_length_rank_cume_dist = cume_dist(Sepal.Length)) %>%
  mutate(s_length_rank_buckets = ntile(Sepal.Length, 4)) %>%
  mutate(s_length_rank = dense_rank(Sepal.Length)) %>% View()

#slicing####
iris %>%
  slice_max(Sepal.Length) %>% View() #or slice_max()
iris %>%
  arrange(desc(Sepal.Width)) %>%
  slice(1:15) %>% View()

#merging dataframes####
d1 <- data.frame(
  id = 1:2,
  x1 = c("a1", "a2"),
  stringsAsFactors = FALSE)
d2 <- data.frame(
  id = 2:3,
  x2 = c("b1", "b2"),
  stringsAsFactors = FALSE)
#inner_join
d1 %>%
  inner_join(d2, by = "id") %>% View()

#left_join
d1 %>%
  left_join(d2) %>%
  View()
#right_join
d1 %>%
  right_join(d2) %>%
  View()
#full_join
d1 %>%
  full_join(d2) %>%
  View()

#pivotting####

#pivot_longer (column to row)
artist_breakdown <- 
  music_df %>%
  distinct(song, artist, primary_artist, featured_artist) %>%
  pivot_longer(primary_artist:featured_artist, #works also with the index ex. 2:3
               names_to = "artist_type",
               values_to = "artist_name")
View(artist_breakdown)

#pivot_wider (row to column)
music_df %>%
  filter(rank <= 3) %>%
  select(date, rank, song) %>%
  pivot_wider(
    names_from = "rank",
    values_from = "song") %>%
  arrange %>% View()

#both pivots
bill_df %>%
  pivot_longer(wk1:wk76, names_to = "week", values_to = "rank") %>% View() %>%
  drop_na() %>%
  pivot_wider(names_from = week, values_from = rank, values_fill = as.double(-999))

#missing data####

#NA = not available - missing in the data source
#NaN = not a number - created form computation in R
mean(starwars$height) #not available 
mean(starwars$height, na.rm = T) #replaces NAs wiht TRUE
starwars %>% #omit all NAs
  na.omit() 
starwars %>% #filter for only NAs
  select(name, gender, hair_color, height, name) %>%
  filter(!complete.cases(.))
starwars %>% #filter for only NAs
  select(name, gender, hair_color, height, name) %>%
  filter(!complete.cases(.)) %>%
  mutate(hair_color2 = replace_na(hair_color, "NONE"))

#LEVELS_____________________________________________________________________####

#switch levels
starwars$gender %>%
  levels() #shows the levels of the varible
starwars$gender %>%
  factor(levels = c("masculine", 
                    "feminine")) -> starwars$gender #switches levels around

#variable types####
glimpse(starwars) #shows variable types of df
class(starwars$gender) #tells class of variable
unique(starwars$gender) #tells the different values the varible has

#factor type####
starwars$gender <- as.factor(starwars$gender) #gender is now a factor variable
glimpse(starwars$gender)

#JOINS______________________________________________________________________####

#create example dfs, picture: join_dfs.png
data_1 <- data.frame(ID = 1:2,
                     x_1 = c("a_1", "a_2"),
                     stringsAsFactors = FALSE) #determines how character vectors should be treated. F = stay as characters and do not convert to factors
data_2 <- data.frame(ID = 2:3,
                     x_2 = c("b_1", "b_2"),
                     stringsAsFactors = FALSE)

#mutating joins####
#inner_join
inner_join(data_1, data_2, by = "ID") %>% #by = defines the join argument column. Similar to SQL
  View()
#left_join/right_join
#these joins can create NAs
left_join(data_1, data_2, by = "ID") %>% #joins with using all rows form data_1, and the matching from data_2
  View()
right_join(data_1, data_2, by = "ID") %>% #similar to left join
  View()
#full_join
#this join cna create NAs
full_join(data_1, data_2, by = "ID") %>% #joins all 
  View()

#filter joins####
#only retains certain rows
#semi_joins
semi_join(data_1, data_2, by = "ID") %>% #data_2 is used as filter. ID = 1 gets deleted since it's not in data_2
  View()
#anti_join
anti_join(data_1, data_2, by = "ID") %>% #similar to semi_joins but keeps the rows which are not in data_2
  View()