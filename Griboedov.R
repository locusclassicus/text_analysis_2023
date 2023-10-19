library(XML)
library(purrr)
library(dplyr)

doc <- xmlTreeParse("files/Griboedov.xml", useInternalNodes = T)
rootnode <- xmlRoot(doc)

# сначала найдем все действия
act_nodes <- getNodeSet(rootnode, "/tei:TEI//tei:text//tei:body//tei:div[@type='act']", 
                   namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))
class(act_nodes) #"XMLNodeSet"

# потренируемся на первом действии, потом масштабируем
act1 <- act_nodes[[1]] 

class(act1) # XMLInternalElementNode" "XMLInternalNode"    
names(xmlChildren(act1))

# достанем название акта
act_name <- xmlValue(act1[["head"]]) # в df

# достанем названия явлений
# idx <- names(xmlChildren(act1)) == "div"
# names_list <- map(xmlChildren(act1)[idx], pluck, "head")
# scenes_names <- unname(map_chr(names_list, xmlValue))
# scenes_names

# как еще это можно сделать
scenes <- xmlElementsByTagName(act1, "div", recursive = FALSE)
class(scenes) # "list"

scenes_head_list <- map(scenes, pluck, "head")
scenes_names <- unname(map_chr(scenes_head_list, xmlValue)) # в df

# заглянем в первое явление и узнаем, кто там говорит
scene1 <- scenes[[1]]
class(scene1) # "XMLInternalElementNode" "XMLInternalNode"   

names(xmlChildren(scene1))

speakers <- xmlElementsByTagName(scene1, "sp")
class(speakers) # "list"

speakers_names_list <- map(speakers, pluck, "speaker")
speakers_names <- unname(map_chr(speakers_names_list, xmlValue)) # в df

# осталось для первого спикера извлечь его реплику
speaker1 <- speakers[[1]]
class(speaker1) # "XMLInternalElementNode" "XMLInternalNode"

names(xmlChildren(speaker1))

# вот здесь важно, что  recursive = T     
lines_list <- xmlElementsByTagName(speaker1, "l", recursive = T) 

lines_text <- unname(map_chr(lines_list, xmlValue)) # в df

# объединяем
speaker_df <- cbind(act_name, scenes_names[1], speakers_names[1], lines_text)
colnames(speaker_df)


# теперь пытаемся обобщить в виде цикла
library(tictoc)

tic()
act_nodes <- getNodeSet(rootnode, "/tei:TEI//tei:text//tei:body//tei:div[@type='act']", 
                        namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))
my_df <- tibble()

for (a in 1:length(act_nodes)) {
  act <- act_nodes[[a]]
  act_name <- xmlValue(act[["head"]])
  scenes <- xmlElementsByTagName(act, "div", recursive = FALSE)
  
  for (s in 1:length(scenes)) {
    scene <- scenes[[s]]
    scene_name <- xmlValue(scene[["head"]])
    speakers <- xmlElementsByTagName(scene, "sp")
    
    for (sp in 1:length(speakers)) {
      speaker <- speakers[[sp]]
      speaker_name <- xmlValue(pluck(speaker, "speaker"))
      lines_list <- xmlElementsByTagName(speaker, "l", recursive = T) 
      lines_text <- unname(map_chr(lines_list, xmlValue))
      text <- str_c(lines_text, collapse = " ")
      
      speaker_tbl <- tibble(act = act_name, scene = scene_name, 
                            speaker = speaker_name, text = text)
      my_df <- bind_rows(my_df, speaker_tbl)
    }
  }
}
toc()
# 0.87 sec elapsed

# можно ли обойтись без цикла?

tic()
sp_nodes <- getNodeSet(rootnode, 
                            "/tei:TEI//tei:text//tei:body//tei:div//tei:div//tei:sp",
                            namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))

speakers <- map(sp_nodes, function(x) xmlValue(xmlElementsByTagName(x, "speaker")))

# в двух узлах не прописан speaker
idx <- which(map_lgl(speakers, is.list))
# [1] 366 578

speakers <- map_chr(speakers[-idx], pluck, "speaker")


lg_nodes <- getNodeSet(rootnode, 
                       "/tei:TEI//tei:text//tei:body//tei:div//tei:div//tei:sp//tei:lg",
                       namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))


# достаем реплики (пока вместе с ремарками)
lines <- map(lg_nodes[-idx], xmlValue)

# осталось придумать, как для каждой реплики найти явление
parent_scene <- map(sp_nodes[-idx], function(x) xmlValue(xmlParent(x)[["head"]]))

# добавляем действия
parent_act <- map(sp_nodes[-idx], function(x) xmlValue(xmlParent(xmlParent(x))[["head"]]))

# соединяем
my_df2 <- tibble(act = parent_act, scene = parent_scene, 
                 speaker = speakers, text = lines)
toc()

# 0.112

# самостоятельно подумать: как убрать ремарки драматурга?
