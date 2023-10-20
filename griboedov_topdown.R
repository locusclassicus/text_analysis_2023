library(XML)
library(purrr)
library(dplyr)
library(stringr)

doc <- xmlTreeParse("files/Griboedov.xml", useInternalNodes = T)
rootnode <- xmlRoot(doc)

# пространство имен
ns <- xmlNamespace(rootnode)

# сначала извлекаем все действия
act_nodes <- getNodeSet(rootnode, "//tei:body//tei:div[@type='act']", 
                        namespaces = c(tei = ns))


my_df <- tibble()

# пишем цикл
for (a in 1:length(act_nodes)) {
  act <- act_nodes[[a]]
  act_name <- xmlValue(act[["head"]])
  scenes <- xmlElementsByTagName(act, "div") 
  
  for (s in 1:length(scenes)) {
    scene <- scenes[[s]]
    scene_name <- xmlValue(scene[["head"]])
    speeches <- xmlElementsByTagName(scene, "sp")
    
    for (sp in 1:length(speeches)) {
      speech <- speeches[[sp]]
      
      speaker_name <- xmlValue(speech[["speaker"]])
      
      lines <- xmlElementsByTagName(speech, "l", recursive = T)
      text <- map_chr(lines, xmlValue) %>% 
        unname() %>% 
        str_c(collapse = " ")
      
      speaker_df <- cbind(act_name, scene_name, speaker_name, text)
      my_df <- rbind(my_df, speaker_df)
    }
  }
}




