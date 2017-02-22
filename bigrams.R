install.packages("ggraph", repo="https://cran.revolutionanalytics.com")

require(tidyr)
require(igraph)

austen_bigrams %>%
  count(bigram, sort = TRUE)

austen_books()
library(janeaustenr)


text_df <- data_frame(line = 1:4, text = text)
  l <- readLines("gita.txt")
 t <- readLines("tao.txt")
  
# austen_bigrams <- l %>%
 # unnest_tokens(bigram, text, token = "ngrams", n = 2)
text_df <- data_frame(l, book = "tao")

# text_df<-text_df %>%
  # unnest_tokens(word, l) %>%
  # mutate(linenumber = row_number())


austen_bigrams <- text_df %>%
  unnest_tokens(bigram, l, token = "ngrams", n = 2)

austen_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")


bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_graph <- bigram_counts %>%
  filter(n > 2) %>%
  graph_from_data_frame()


library(ggraph)
set.seed(2017)

plot(bigram_graph,layout =layout.fruchterman.reingold(bigram_graph))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


