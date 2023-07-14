library(stylo)
my_corpus = load.corpus.and.parse(files = "all", 
                                  corpus.dir = "corpora/plato", 
                                  corpus.lang = "Other")
corp_freq <- make.frequency.list(my_corpus, value = TRUE, relative = F)
hapax_names <- names(corp_freq[corp_freq==1])
length(hapax_names) 
length(corp_freq)

corpus_size <- length(my_corpus)
corpus_names <- names(my_corpus)
d <- data.frame()

for (i in 1:corpus_size){
  my_text<- my_corpus[[i]]
  my_vec <- logical()
  for (z in my_text) {
    x <- any(hapax_names == z)
    my_vec <- c(my_vec, x)
    }
  hapax_sum <- sum(my_vec)
  text_size <- length(my_text)
  result <-(c(corpus_names[i], text_size, hapax_sum, round((hapax_sum/text_size), digits = 3)))
  d <- rbind(d, result)
  }

View(d)
names(d) <- c("dialogue", "words", "hapax", "ratio")
d$dialogue

group <- c(1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 2, 1, 2, 3, 1, 2, 3, 3, 1, 2, 3)

hapax_plato <- cbind(d, group)
save(hapax_plato, file = "datasets/HapaxPlato.Rdata")



