### instalación de paquetes ###

install.packages("tm")
install.packages("NLP")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("stringr")

# carga de librerias###
library(tm)
library(NLP)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(stringi)
library(stringr)
library(ggplot2)

### carga de datos###
cupo <- readLines("C:/Users/lucre/Desktop/debatecupo/Ley de cupo.txt")
cupo = iconv(cupo, to="ASCII//TRANSLIT")
cupo = Corpus(VectorSource(cupo))

str(cupo)
str_length(cupo)

### limpieza y corpus###

debate_cupo <- cupo %>% 
  str_replace_all(., "[[:cntrl:]]", " ") %>% 
  str_to_lower() %>% 
  removePunctuation() %>% 
  str_replace_all(., "—", " ")

debate_cupo <- removeWords(debate_cupo, words = stopwords("spanish"))

debate_cupo <- stripWhitespace(debate_cupo)

corpus_cupo <- debate_cupo %>%  ###corpus###
  VectorSource() %>%
  Corpus()

### Nube de palabras ###
wordcloud(corpus_cupo, 
          min.freq = 5,
          max.words = 80, 
          random.order = FALSE, 
          colors = brewer.pal(name = "Dark2", n = 8))

a_retirar <- c("muchas", "veces", "siempre", "sino", "asi", "mas", 
               "tambien")

debate_cupo <- removeWords(debate_cupo, words = a_retirar)

corpus_cupo <- debate_cupo %>% 
  VectorSource() %>%
  Corpus()

wordcloud(corpus_cupo, 
          min.freq = 5,
          max.words = 80, 
          random.order = FALSE, 
          colors = brewer.pal(name = "Dark2", n = 8)
)

###Frecuencia de palabras###

palabras <- corpus_cupo %>% 
  TermDocumentMatrix() %>% 
  as.matrix() %>% 
  rowSums() %>% 
  sort(decreasing = TRUE)

palabras %>% 
  head(20)

frecuencias <- data.frame(
  palabra = names(palabras),
  frecuencia = palabras
)

frecuencias[1:10,] %>% 
  ggplot() +
  aes(frecuencia, y = reorder(palabra, frecuencia)) +
  geom_bar(stat = "identity", color = "white", fill = "violet") +
  geom_text(aes(label = frecuencia, hjust = 1.5), color = "white") +
  labs(
    x = NULL,
    y = "Palabras más utilizadas en el debate"
  )


