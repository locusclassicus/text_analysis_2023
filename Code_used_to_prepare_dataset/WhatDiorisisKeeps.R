# ## write function
# path = "/Users/olga/R_Workflow/Big_collections_Greek_corpus/Diorisis_XML"
# setwd(dir = path)
# greek <- function(path = ".") {
#   file_names <- list.files(pattern = ".xml", path = path)
#   tbl <- tibble()
# 
#   for(i in 1:length(file_names)) {
#     message("processing file ",  i, " out of ", length(file_names))
#     doc <- xmlTreeParse(file_names[i], useInternalNodes = T)
# 
#     ## get name, title, date, genre, and subgenre
#     name <- xmlValue(getNodeSet(doc, "/TEI.2/teiHeader/fileDesc/titleStmt/author",
#                                 namespaces = c(tei = "http://www.tei-c.org/ns/1.0")))
#     title <- xmlValue(getNodeSet(doc, "/TEI.2/teiHeader/fileDesc/titleStmt/title",
#                                  namespaces = c(tei = "http://www.tei-c.org/ns/1.0")))
#     date <- xmlValue(getNodeSet(doc, "/TEI.2/teiHeader/profileDesc/creation/date",
#                                 namespaces = c(tei = "http://www.tei-c.org/ns/1.0")))
#     genre <- xmlValue(getNodeSet(doc, "/TEI.2/teiHeader/xenoData/genre",
#                                  namespaces = c(tei = "http://www.tei-c.org/ns/1.0")))
#     subgenre <- xmlValue(getNodeSet(doc, "/TEI.2/teiHeader/xenoData/subgenre",
#                                     namespaces = c(tei = "http://www.tei-c.org/ns/1.0")))
# 
#     ## get all nodes with lemma
#     lemma <- getNodeSet(doc, "/TEI.2/text/body/sentence/word/lemma",
#                            namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))
#     entries <- unlist(sapply(lemma, xmlGetAttr, "entry"))
# 
#     ## make tibble
#     obs <- tibble(name = name, title = title, date = date,
#                   genre = genre, subgenre = subgenre, entries = entries)
#     tbl <- bind_rows(tbl, obs)
#   }
#   tbl
# }
# 
# # run function and save
# library(tidyverse)
# library(XML)
# my.df <- greek()
# my.df <- my.df %>% mutate(date = as.numeric(date))
# setwd(dir = "/Users/olga/R_Workflow")
# save(my.df, file = "DiorisisTidy.Rdata")

# load
load("/Users/olga/R_Workflow/DiorisisTidy.Rdata")

# libraries
library(tidyverse)

# explore
dist.df <- my.df %>% select(-entries) %>% 
  distinct() 

# mutate groups 
dist.trm <- dist.df %>% 
  mutate(
  group = case_when(
    genre == c("Comedy") ~ "Poetry",
    genre == c("Tragedy") ~ "Poetry",
    subgenre == "Hymns" ~ "Poetry",
    genre %in% c("Narrative", "Letters", "Essays",
                 "Philosophy", "Oratory") ~ "Prose",
    TRUE ~ genre
  )) %>% filter(!genre %in% c("Religion", "Technical"))

dist.trm %>% group_by(group) %>% 
  summarise(n = n())

# transform
df.gr <- my.df %>% left_join(dist.trm, .by = c("name", "title"))

# count words per genre
total.gr <- df.gr  %>% 
  group_by(group) %>% 
  summarise(n = n())

# count words per work
total <- df.gr %>% 
  group_by(name, title) %>% 
  summarise(n = n(), .groups = "keep")

# select article
art.df <- df.gr %>% filter(entries == "ὁ") %>% 
  group_by(name, title, group) %>%
  summarise(n = n(), .groups = "keep")

# relative articles
rel.df <- total %>%
  left_join(art.df, by = c("name", "title")) %>%
  mutate(art = n.y/n.x) %>%
  select(-n.x, -n.y)

# boxplot
rel.df %>% filter(!is.na(group)) %>% 
  ggplot(aes(group, art, fill = group)) +
  geom_boxplot()

# date 
date.df <- dist.trm %>% 
  select(name, title, date) 
new.df <- rel.df %>% 
  left_join(date.df, .by = c(name, title)) %>% 
  filter(!is.na(group), !is.na(group))

# plot date
ggplot(aes(date, fill = factor(group)), data = new.df) + 
    geom_dotplot(binwidth = 10, stackdir = "centerwhole", binpositions = "all") +
    scale_y_continuous(NULL, breaks = NULL) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

# model
lm <- lm(art ~ group, data = new.df)
summary(lm)
plot(lm)

lm <- lm(art ~ group + date, data = new.df)
summary(lm)

# my.df %>% ggplot(aes(date, fill = factor(genre))) + 
#   geom_dotplot(binwidth = 10, stackdir = "centerwhole", binpositions = "all") +  
#   scale_y_continuous(NULL, breaks = NULL) + 
#   scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
# 
# my.df %>% ggplot(aes(date, fill = factor(genre))) + 
#   geom_dotplot(binwidth = 10, method = "histodot")
# 
# my.df %>% group_by(as.factor(genre)) %>% summarise(total = n()) %>%
#   ggplot(aes(total, genre, fill = `as.factor(genre)`, size = total)) + 
#   geom_point()
# 
