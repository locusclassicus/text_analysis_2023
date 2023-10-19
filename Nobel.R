library(tidyr)
library(dplyr)
library(purrr)
library(jsonlite)
library(ggplot2)
library(lubridate)

# прочитайте файлы json_laureates.json и json_laureates.json
# не передавайте функции fromJSON никакие аргументы, кроме пути к файлу

laureates <- fromJSON("archive/json_laureates.json")
awards <- fromJSON("archive/json_award.json")

# используйте jsonlite::flatten(), чтобы избавиться от вложенных датафреймов
# используйте unnest(), чтобы "распаковать" столбец laureates в awards
# используйте unnest(), чтобы "распаковать" столбец nobelPrizes в laureates
# для починки имен передайте unnest аргумент names_repair = "minimal"
# обратите внимание, как изменится количество столбцов
laureates <- laureates %>% 
  unnest(nobelPrizes, names_repair = "minimal") %>% 
  jsonlite::flatten() %>% 
  unnest(affiliations, names_repair = "minimal") %>% 
  jsonlite::flatten()

awards <- awards %>% 
  unnest(laureates, names_repair = "minimal") %>% 
  jsonlite::flatten()

# используйте reduce, чтобы узнать общие имена столбцов у двух датафреймов
# не забывайте, что функция принимает на входе список!
my_list <- list(colnames(laureates), colnames(awards))
reduce(my_list, intersect)

# удалите столбцы, которые начинаются с links, 
# используя комбинацию select() и starts_with()

laureates <- laureates %>% 
  select(!starts_with(("links")))
awards <- awards %>% 
  select(!starts_with(("links")))

# объедините тибблы; 
# удалите сведения на шведском и норвежском языке (se, no),
# используя комбинацию select() и ends_with();

nobel <- laureates %>% 
  left_join(awards) %>% 
  select(!ends_with("no") & !ends_with("se")) 

# удалите столбцы с NA > 0.5,
# используя комбинацию select() и where()
# удаляем лишние столбцы

nobel <- nobel %>% 
  as_tibble() %>% 
  discard(map_int(., ~sum(is.na(.))) > (nrow(nobel)/2)) %>%
  select(-c(givenName.en, familyName.en, fullName.en)) %>% 
  select(!ends_with("Now.en")) %>% 
  select(!ends_with("String.en")) %>% 
  select(!starts_with("death")) %>% 
  select(-c(prizeStatus, residences)) %>% 
  rename(university = name.en, 
         country = country.en, 
         category = category.en)
  
# в каких университетах больше всего ноб. лауреатов?
top_universities <- nobel %>% 
  group_by(university, country) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))  %>% 
  ungroup() %>% 
  top_n(15)

top_universities %>% 
  ggplot(aes(reorder(university, n), n, fill = country)) + 
  geom_bar(stat = "identity") +
  coord_flip()

# в каком возрасте получают ноб. премию?
age_data <- nobel %>% 
  select(awardYear, dateAwarded, birth.date, category, university) %>% 
  mutate(dateAwarded = case_when(
             is.na(dateAwarded) ~ paste(awardYear, "10", 10, sep = "-"),
             .default = dateAwarded
             )) %>% 
  mutate(birth.date = ymd(birth.date),
         dateAwarded = ymd(dateAwarded),
         age = interval(birth.date, dateAwarded) %/% years(1)) %>% 
  mutate(awardYear = as.integer(awardYear))
  
age_data %>% 
  ggplot(aes(awardYear, age)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth()

# в каком университете самые молодые лауреаты?

top_age <- age_data %>% 
  group_by(university) %>% 
  summarise(mean = mean(age, na.rm = T)) %>% 
  right_join(top_universities)
