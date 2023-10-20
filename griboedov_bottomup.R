library(XML)
library(purrr)
library(dplyr)
library(stringr)

doc <- xmlTreeParse("files/Griboedov.xml", useInternalNodes = T)
rootnode <- xmlRoot(doc)

# пространство имен
ns <- xmlNamespace(rootnode)

# сначала извлекаем все стихи
line_nodes <- getNodeSet(rootnode, "//tei:l", namespaces = c(tei = ns))

# кто говорит
get_speaker <- function(node) {
  who <- xmlValue(xmlParent(xmlParent(node))[["speaker"]])
  return(who)
}

# узнать явление
get_scene <- function(node) {
  scene_name <- xmlValue(xmlParent(xmlParent(xmlParent(line_nodes[[1]])))[["head"]])
  return(scene_name)
}

# узнать действие
get_act <- function(node) {
  act_name <- xmlValue(xmlParent(xmlParent(xmlParent(xmlParent(line_nodes[[1]]))))[["head"]])
  return(act_name)
}

# достать текст
text <- map_chr(line_nodes, xmlValue)

# теперь на все узлы 
speaker_names <- map_chr(line_nodes, get_speaker)
scene_names <- map_chr(line_nodes, get_scene)
act_names <- map_chr(line_nodes, get_act)

# сшиваем
my_df <- tibble(act = act_names, scene = scene_names, 
                speaker = speaker_names, text = text)


# отбор по имени
famusov <- my_df %>% 
  filter(speaker == "Фамусов")

# а что дальше?
library(tidytext)
famusov %>% 
  unnest_tokens(output = "word", input = "text")
