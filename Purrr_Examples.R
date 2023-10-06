library(purrr)
library(dplyr)
data(starwars)

# узнать число NAs
map(starwars, ~sum(is.na(.)))
map_int(starwars, ~sum(is.na(.)))

# узнать класс 
map_chr(starwars, class)

# число уникальных наблюдений
map_dbl(starwars, n_distinct)

# среднее
starwars %>% 
  select(mass, height) %>% 
  map(~mean(., na.rm = T))

# в датафрейм
starwars %>% 
  map_df(~(data.frame(n_distinct = n_distinct(.x),
                                  class = class(.x))),
         .id = "variable"
         )

# пользовательская функция
describe_vec <- function(vec){
  l = paste("Длина вектора: ", length(vec))
  c = paste("Класс вектора: ", class(vec))
  result = paste(l, c, sep = " | ")
  return(result)
}

# несколько векторов
text1 <- c("гнев", "богиня", "воспой")
text2 <- c("в", "мысли", "ему", "то", "вложила", "богиня", "державная", "гера")


# применить функцию к нескольким векторам
my_vecs <- list(text1, text2)
map_chr(my_vecs, describe_vec)

# с анонимной функцией
map_chr(my_vecs, function(x) paste("Длина вектора: ", length(x)))

# игрушечный корпус
corpus <- tibble(title = rep("text2", length(text2)),
                 word = text2)

# скользящее окно
library(slider)
windows <- slide(corpus, ~.x, .after = 1)

out <- map2_dfr(.x = windows, .y = 1:length(windows), 
                ~mutate(.x, window_id = .y)) 

# imap
imap(.x = windows, ~ mutate(.x, window_id = .y))


# чтение файлов 
library(readr)
library(stringr)

files <- list.files("./files/HP", pattern = ".csv", full.names = TRUE)
full <- map(files, read_csv, col_types = cols())
names(full) <- str_extract(files, "\\w+(?=.csv)")

# извлечь столбцы
bnb <- map(full, select, `BNB number`)

# извлечь ряды
bnb_full <- map(full, filter, !(is.na(`BNB number`)))

# уникальные названия
titles <- map(bnb_full, distinct, Title)

# найти общие столбцы
map(full, colnames) %>% 
  reduce(intersect)


# объединить 
data_joined <- full %>% 
  reduce(left_join)

# почиcтить и исследовать
library(ggplot2)
library(tidyr)
data_sum <- data_joined %>% 
  group_by(`Date of publication`, `Country of publication`) %>% 
  summarise(n = n()) %>% 
  separate(`Date of publication`, into = c("year", NA)) %>% 
  filter(!is.na(year)) %>% 
  rename(country = `Country of publication`) %>% 
  separate(country, into = c("country", NA), sep = ";") %>% 
  filter(!is.na(country)) %>% 
  mutate(country = str_squish(country))

# условное преобразование
data_tidy <- data_sum %>% 
  mutate(country = 
           case_when(country == "England" ~ "United Kingdom",
                     country == "Scotland" ~ "United Kingdom",
                   TRUE ~ country))

# график
library(ggplot2)
data_tidy %>% 
  ggplot(aes(year, n, fill = country)) + 
  geom_col() + 
  xlab(NULL) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_viridis_d()
  
# основные темы
library(tidyr)
data_topics <- data_joined %>% 
  filter(!is.na(Topics)) %>% 
  separate(Topics, into = c("topic")) %>% 
  mutate(topic = tolower(topic)) %>% 
  group_by(topic) %>% 
  summarise(n = n()) %>% 
  filter(!topic %in% c("harry", "rowling", "potter", "children", "literary"))


pal <- c("#f1c40f", "#34495e", 
         "#8e44ad", "#3498db",
         "#2ecc71")

library(wordcloud)
par(mar = c(1, 1, 1, 1))
wordcloud(data_topics$topic, 
          data_topics$n,
          min.freq = 3,
          #max.words = 50, 
          scale = c(3, 0.8),
          colors = pal, 
          random.color = T, 
          rot.per = .2,
          vfont=c("gothic italian","plain")
          )
        
devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)
wordcloud2(data_topics, 
           figPath = "./images/hat.png",
           size = 1.5,
           color='random-light', 
           backgroundColor="black")


# еще полезные функции из purrr
starwars %>% 
  keep(is.numeric)

starwars %>% 
  discard(is.numeric)

x <- 1:10
x %>% accumulate(sum)

