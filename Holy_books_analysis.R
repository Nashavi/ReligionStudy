
setwd("~/Documents/UCD/BA Prac/ReligionStudy")

require(gutenbergr) # For downloads of The King James Version Bible (#10) and The Tao Te Ching (#216)

bible <- gutenberg_download(10)
names(bible) <- c("doc","text")
bible$doc <- "The King James Bible"

tao <- gutenberg_download(216)
names(tao) <- c("doc","text")
tao$doc<- "The Tao Te Ching"

system(paste('"/Users/Avi/Documents/UCD/BA Prac/ReligionStudy/pdftotext"','"gita.pdf"'), wait=FALSE)
text <- readLines("gita.txt")
gita <- data_frame(doc = "The Bhagavad Gita", text)

system(paste('"/Users/Avi/Documents/UCD/BA Prac/ReligionStudy/pdftotext"','"quran.pdf"'), wait=FALSE)
text <- readLines("quran.txt")
quran <- data_frame(doc = "The Quran", text)

holybooks <- rbind(bible,tao,gita,quran)
holybooks <- holybooks[holybooks$text !="",]
holybooks$text <- gsub('[[:punct:]]|[[:digit:]]','',holybooks$text)

rm(bible,gita,quran,tao)

save(list = ls(),file = "holybooks.Rdata")

# Start here if the dataset can be loaded -------------------------------

require(dplyr)
require(tidytext)
require(ggplot2)

dev.off()

load("holybooks.Rdata")

clean_hb <- holybooks %>%
  group_by(doc) %>%
  mutate(linenumber = row_number()) %>%
  ungroup()

wordwise <- clean_hb %>%
  unnest_tokens(word, text)

wordwise<- filter(wordwise, nchar(wordwise$word)>2)

wordwise<- wordwise %>%
  group_by(doc) %>%
  mutate(idx = round(100*(linenumber/max(linenumber)),0))

data(stop_words)

cleanwords <- wordwise %>%
  anti_join(stop_words)


cleanwords %>%
  count(word, sort = TRUE) %>%
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

# Sentiment flow ----------------------------------------------------------

require(tidyr)

hb_sentiments <- wordwise %>%
  inner_join(get_sentiments("bing")) %>%
  count(doc, index = idx, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

hb_sentiments<- hb_sentiments %>%
  group_by(doc) %>%
  mutate(centeredsentiment = as.numeric(scale(sentiment)))

ggplot(hb_sentiments, aes(index, sentiment, fill=as.factor(sentiment>0))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~doc, ncol = 1) +
  scale_x_continuous(label=function(x){return(paste0(x, "%"))})+  scale_fill_manual("",values = c("#ddaa00","#ff9933","#009000","brown")) +
  labs(x= "Time Trajectory of the document\n(Section Percentile)",
       y= "Sentiments\n(scales & centered)",
       title = "Flow of sentiments")

ggplot(hb_sentiments, aes(index, centeredsentiment, fill=as.factor(sentiment>0))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_segment(mapping=aes(x=0, y=0, xend=105, yend=0), arrow=arrow(angle = 30,length = unit(0.10, "inches"), ends = "last", type = "closed"), size=0.05, color="black") +
  facet_wrap(~doc, ncol = 1) +
  scale_x_continuous(label=function(x){return(paste0(x, "%"))})+  scale_fill_manual("",values = c("brown","#ddaa00")) +
  labs(x= expression("Time Trajectory of the document (Section in Percentile)" %->% ""),
       y= "Sentiments\n(scaled & centered)",
       title = "Flow of sentiments") +
  theme_bw() +
  theme(legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "#f9f9f9"),
        panel.grid.minor = element_blank(),
        plot.title=element_text(hjust=0.5,face="bold"),
        strip.text.x = element_text(size=11,hjust=0.05,face="bold"),
        strip.background = element_blank())


# Word Cloud drill down on sentiment flow ---------------------------------

require(wordcloud)
require(reshape2)

# Largest positive sentiment word cloud
cleanwords %>%
  filter(doc=="The King James Bible") %>%
  filter(idx=="53") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("brown","#ddaa00"),random.order=FALSE,rot.per=0,max.words = 100)

# Largest negative sentiment word cloud
cleanwords %>%
  filter(doc=="The King James Bible") %>%
  filter(idx=="67") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("brown","#ddaa00"),random.order=FALSE,rot.per=0,max.words = 100)

# Largest negative centered sentiment word cloud
cleanwords %>%
  filter(doc=="The Tao Te Ching") %>%
  filter(idx=="92") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("brown","#ddaa00"),random.order=FALSE,rot.per=0,max.words = 100)

# Largest positive centered sentiment word cloud
cleanwords %>%
  filter(doc=="The Bhagavad Gita") %>%
  filter(idx=="33") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("brown","#ddaa00"),random.order=FALSE,rot.per=0,max.words = 100)


# Percentage of negative and positive sentiments --------------------------


bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

bingpositive <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")

wordcounts <- cleanwords %>%
  group_by(doc) %>%
  summarize(words = n())

negwordcount<-cleanwords %>%
  semi_join(bingnegative) %>%
  group_by(doc) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("doc")) %>%
  mutate(neg_words_percent = round(100*negativewords/words,2)) %>%
  ungroup

poswordcount<-cleanwords %>%
  semi_join(bingpositive) %>%
  group_by(doc) %>%
  summarize(positivewords = n()) %>%
  left_join(wordcounts, by = c("doc")) %>%
  mutate(pos_words_percent = round(100*positivewords/words,2)) %>%
  ungroup

all_sentiments<-merge(negwordcount,poswordcount)

ggplot(melt(all_sentiments[,c(1,4,6)]),
        aes(x=doc, value, fill=variable,  width = 0.25)) +
  geom_bar(stat = "identity",position = position_dodge(width=0.3))+
  labs(x= "",y= "Percentage of words")+
  ggtitle("Percentage of Total Negative and Positive word sentiments") +
  scale_fill_manual("",values = c("brown","#ddaa00"),labels= c("Negative\nSentiments","Positive\nSentiments")) +
  scale_y_continuous(label=function(y){return(paste0(y, "%"))})+
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5,face="bold"),
        axis.text.x = element_text(face="bold",size=10),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Top 25 sentiments -------------------------------------------------------

doc_words <- wordwise %>%
  group_by(doc,word) %>%
  summarize(count=length(word)) %>%
  inner_join(get_sentiments("bing"), by = c(word = "word")) 

total_words<- doc_words %>% 
  group_by(doc) %>% 
  summarize(total = sum(count))

doc_words <- left_join(doc_words,total_words)

plotthis<-doc_words %>%
  #filter(document =="Bible") %>%
  count(doc,sentiment, word, wt = count, sort = TRUE) %>%
  #ungroup() %>%
  #ungroup() %>%
  #group_by(doc) %>%
  #top_n(n=25,wt=n) %>%
  #ungroup() %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  group_by(doc) %>%
  top_n(n=20,wt=abs(n))  %>%
  arrange(doc,n) %>%
  ungroup () %>%
  mutate(order = row_number())

ggplot(plotthis, aes(order, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ doc, scales = "free") +
  xlab("Words preceded by negation") +
  ylab("Sentiment score * # of occurrences") +
  theme_bw() +
  coord_flip() + 
  # Add categories to axis
  scale_x_continuous(
    breaks = plotthis$order,
    labels = plotthis$word,
    expand = c(0,0)) +
  labs(y="Contribution to sentiment",x=NULL,title = "Top 20 sentiments in each text") +
  coord_flip() +
  facet_wrap(~doc,scales = "free",ncol=1)+
  scale_fill_manual("",values = c("brown","#ddaa00"),labels= c("Negative\nSentiments","Positive\nSentiments"))+
  theme_bw() +
  theme(legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "#f9f9f9"),
        panel.grid.minor = element_blank(),
        plot.title=element_text(hjust=0.5,face="bold"),
        strip.text.x = element_text(size=11,hjust=0,face="bold"),
        strip.background = element_blank())

# Common Sentiments -------------------------------------------------------------
require(ggradar)
require(gridExtra)

commonnegwords<-doc_words %>%
  group_by(doc) %>%
  mutate(countpercent=count/sum(count)) %>%
  ungroup() %>%
  group_by(word) %>%
  mutate(wordimp= sum(countpercent)) %>%
  filter(sentiment=="negative") %>%
  group_by(doc) %>%
  top_n(n=10,wt=wordimp) %>%
  arrange(doc,wordimp) %>%
  select(doc,word,countpercent) %>%
  dcast(doc~word)

commonnegwords[is.na(commonnegwords)]<-0
x<-ggradar(commonnegwords, grid.min = 0,
           grid.mid = 0.015,
           grid.max = 0.03,
           axis.label.offset = 1.1,
           axis.label.size = 4,
           grid.label.size = 0,
           group.line.width = 0.8,
           group.point.size = 1.5,
           background.circle.colour = "#ffffff",
           legend.text.size = 9,
           plot.legend = FALSE,
           plot.title = "Common negative sentiments")+
  theme(legend.position = "bottom",
        plot.title=element_text(hjust=0.5,face = "bold"))+
  scale_colour_manual(values = rep(c("#ffbf00","darkkhaki","#009000","cadetblue3"), 100))

commonposwords<-doc_words %>%
  group_by(doc) %>%
  mutate(countpercent=count/sum(count)) %>%
  ungroup() %>%
  group_by(word) %>%
  mutate(wordimp= sum(countpercent)) %>%
  filter(sentiment=="positive") %>%
  group_by(doc) %>%
  top_n(n=10,wt=wordimp) %>%
  arrange(doc,wordimp) %>%
  select(doc,word,countpercent) %>%
  dcast(doc~word)

commonposwords[is.na(commonposwords)]<-0
y<-ggradar(commonposwords, grid.min = 0,
           grid.mid = 0.025,
           grid.max = 0.05,
           axis.label.offset = 1.1,
           axis.label.size = 4,
           grid.label.size = 0,
           group.line.width = 0.8,
           group.point.size = 1.5,
           background.circle.colour = "#ffffff",
           legend.text.size = 9,
           plot.legend = FALSE,
           plot.title = "Common positive sentiments")+
  theme(legend.position = "bottom",
        plot.title=element_text(hjust=0.5,face = "bold"),
        axis.title = element_text(face = "bold"))+
  scale_colour_manual(values = rep(c("#ffbf00","darkkhaki","#009000","cadetblue3"), 100))

tmp <- arrangeGrob(x + theme(legend.position = "none"), y + theme(legend.position = "none"), layout_matrix = matrix(c(1, 2), nrow = 2))

g <- ggplotGrob(y + theme(legend.position="right"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

grid.arrange(tmp, legend,ncol=2, widths=c(9,6))

grid.arrange(x,y,nrow=2)
# Deleted:-
# require(gridExtra)
# 
# p<-doc_words %>%
#   #inner_join(get_sentiments("bing")) %>%
#   #count(doc,word, sentiment, sort = TRUE) %>%
#   acast(word ~ doc, value.var = "count",sum) %>%
#   data.frame()
# 
# #p$total<-rowSums(p)
# p$word<-row.names(p)
# 
# 
# p$The.Bhagavad.Gita<- ifelse(p$The.Bhagavad.Gita!=0,p$The.Bhagavad.Gita/sum(p$The.Bhagavad.Gita),0)
# p$The.King.James.Bible<-ifelse(p$The.King.James.Bible!=0,p$The.King.James.Bible/sum(p$The.King.James.Bible),0)
# p$The.Quran<-ifelse(p$The.Quran!=0,p$The.Quran/sum(p$The.Quran),0)
# p$The.Tao.Te.Ching<-ifelse(p$The.Tao.Te.Ching!=0,p$The.Tao.Te.Ching/sum(p$The.Tao.Te.Ching),0)
# #p$totalratio<-p$total/sum(p$total)
# 
# #p$sumratio<-p$The.Bhagavad.Gita+p$The.King.James.Bible+p$The.Quran+p$The.Tao.Te.Ching
# 
# p<-melt(p)
# 
# p<-p %>%
#   inner_join(get_sentiments("bing"))
# 
# x<-p %>%
#   filter(sentiment=="negative") %>%
#   group_by(word) %>%
#   mutate(l=sum(value)) %>%
#   ungroup() %>%
#   top_n(n=50) %>%
#   mutate(word = reorder(word, l)) %>%
#   ggplot(aes(word,value,fill=variable)) +
#   geom_bar(stat="identity",position = position_dodge(width=0.8),show.legend = F) +
#   coord_flip()+
#   scale_fill_manual("",values = c("#ffbf00","darkkhaki","#009000","cadetblue3"),labels = c("The Bhagavad GIta","The King James Bible","The Quran","The Tao Te Ching"))+
#   labs(x="Word",y="Weight", title = "Common Negative sentiments")+
#   theme(plot.title=element_text(hjust=0.5,face = "bold"),
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         panel.grid.major = element_line(colour = "#f9f9f9"),
#         panel.grid.minor = element_blank(),
#         strip.text.x = element_text(size=11,hjust=0.05,face="bold"),
#         strip.background = element_blank(),
#         axis.title = element_text(face = "bold"))
# 
# y<-p %>%
#   filter(sentiment=="positive") %>%
#   group_by(word) %>%
#   mutate(l=sum(value)) %>%
#   ungroup() %>%
#   top_n(n=50) %>%
#   mutate(word = reorder(word, l)) %>%
#   ggplot(aes(word,value,fill=variable)) +
#   geom_bar(stat="identity",position = position_dodge(width=0.8)) +
#   coord_flip()+
#   scale_fill_manual("",values = c("#ffbf00","darkkhaki","#009000","cadetblue3"),labels = c("The Bhagavad GIta","The King James Bible","The Quran","The Tao Te Ching"))+
#   labs(x="",y="Weight", title = "Common Positive sentiments")+
#   theme(plot.title=element_text(hjust=0.5,face = "bold"),
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         panel.grid.major = element_line(colour = "#f9f9f9"),
#         panel.grid.minor = element_blank(),
#         strip.text.x = element_text(size=11,hjust=0.05,face="bold"),
#         strip.background = element_blank(),
#         axis.title = element_text(face = "bold"))
# 
# grid.arrange(x,y, widths=3:4)

# Top 20 Unqiue Words - TF-IDF --------------------------------------------

doc_words2<- cleanwords %>%
  count(doc, word, sort = TRUE) %>%
  ungroup()

doc_words2 <- left_join(doc_words2,
                       doc_words2 %>%
                         group_by(doc) %>% 
                         summarize(total = sum(n)))

ggplot(doc_words2, aes(n/total, fill = doc)) +
  geom_histogram(show.legend = FALSE,bins=70) +
  #xlim(NA, 0.03) +
  facet_wrap(~doc, ncol = 2, scales = "free_y")

doc_wordstfidf <- doc_words2 %>%
  bind_tf_idf(word, doc, n)

doc_wordstfidf <- anti_join(doc_wordstfidf , doc_wordstfidf [duplicated(doc_wordstfidf[2]),], by="word")

plot_tfidf <- doc_wordstfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

plot_this2 <- plot_tfidf %>% 
  group_by(doc) %>% 
  top_n(20) %>% 
  ungroup

ggplot(plot_this2, aes(word, tf_idf, fill = doc)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~doc, ncol = 2, scales = "free") +
  coord_flip()+
  scale_fill_manual("",values = c("#ffbf00","darkkhaki","#009000","cadetblue3")) +
  labs(y= "TF-IDF",title = "Top 20 unique words in each text")+
  theme(plot.title=element_text(hjust=0.5,face = "bold"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "#f9f9f9"),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size=11,hjust=0.05,face="bold"),
        strip.background = element_blank(),
        axis.title = element_text(face = "bold"))


# Bigram analysis ---------------------------------------------------------

require(igraph)
require(ggraph)
require(gridExtra)

hb_bigrams <- holybooks %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

hb_bigrams<- filter(hb_bigrams, nchar(hb_bigrams$bigram)>2)

bigrams_separated <- hb_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(doc,word1, word2, sort = TRUE)

bigram_counts<- bigram_counts[,c(2,3,4,1)]

bigram_counts_gr<- bigram_counts %>%
  group_by(doc) %>%
  top_n(50,wt=n)%>%
  filter (n>2) %>%
  ungroup()

set_graph_style()

printgraph<- function(df,doc,colorname){
  
  set.seed(123)
  
  bigram_graph <- df[df$doc==doc,] %>%
    graph_from_data_frame()
  
  gh<-df[df$doc==doc,]
  
  names(gh)<-c("word","word","n","doc")
  tt<- rbind(gh[,c(1,4)],gh[,c(2,4)])
  pk<-tt[match(unique(tt$word), tt$word),]
  
  V(bigram_graph)$class<-pk$doc
  
  a <- grid::arrow(type = "closed", length = unit(.10, "inches"))
  
  p<- ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n),arrow = a) +
    geom_node_point(size = 1.5,colour = colorname) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) + 
    ggtitle(doc) +
    #th_foreground(foreground = 'grey80', border = F)+
    theme(legend.position="none",
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin=unit(c(0.2,0.5,0.7,0.5), "cm"))
  
  return (p)
}


g2<-printgraph(bigram_counts_gr,"The King James Bible",colorname="darkkhaki")
g1<-printgraph(bigram_counts_gr,"The Bhagavad Gita",colorname="#ffbf00")
g3<-printgraph(bigram_counts_gr,"The Quran",colorname="#009000")
g4<-printgraph(bigram_counts_gr,"The Tao Te Ching",colorname="cadetblue3")


grid.arrange(g1,g2,g3,g4,ncol=1)

