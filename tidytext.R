install.packages("tidytext", repo="https://cran.revolutionanalytics.com")
install.packages("ggplot2", repo="https://cran.revolutionanalytics.com")

require(tidytext)
require(tm)
require(dplyr)
require(ggplot2)



tmpText <- c(tao.corpus, bible.corpus,gita.corpus,quran.corpus)
tdm<-TermDocumentMatrix(tmpText)
colnames(tdm)<- c("Tao","Bible","Gita","Quran")

tail(inspect(tdm))

df<-tidy(tdm)
str(df)


doc_words <- df %>% inner_join(get_sentiments("bing"), by = c(term = "word"))

total_words<- doc_words %>% 
  group_by(document) %>% 
  summarize(total = sum(count))

doc_words <- left_join(doc_words,total_words)

doc_words %>%
  #filter(document =="Bible") %>%
  count(document,sentiment, term, wt = count) %>%
  #ungroup() %>%
  group_by(document) %>%
  mutate(r = row_number(-n)) %>%
  filter(r <=25) %>%
  ungroup() %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip() +
  facet_wrap(~document,scales = "free")


ggplot(doc_words, aes(count/total, fill = document)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.09) +
  facet_wrap(~document, ncol = 2, scales = "free_y")


doc_wordstfidf <- df %>%
  bind_tf_idf(term, document, count)

doc_wordstfidf %>%
  #select(-total) %>%
  arrange(desc(doc_wordstfidf))

doc_wordstfidf <- anti_join(doc_wordstfidf , doc_wordstfidf [duplicated(doc_wordstfidf [1]),], by="term")


plot_docs <- doc_wordstfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(term, levels = rev(unique(term))))

ggplot(plot_docs[1:20,], aes(word, tf_idf, fill = document)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()


plot_docs2 <- plot_docs %>% 
  group_by(document) %>% 
  top_n(15) %>% 
  ungroup
  
ggplot(plot_docs2, aes(word, tf_idf, fill = document)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~document, scales="free") +
  coord_flip()

