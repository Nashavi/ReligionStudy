
library(NLP) 
library(openNLP)
require(plyr)

pick_POS_from_Corpus<- function(x,postag){
  
  #convert to string
  x<-as.String(x)
  
  #create and annotate sentence and word tokens
  x.word.sent<-annotate(x,list(Maxent_Sent_Token_Annotator(),Maxent_Word_Token_Annotator()))
  
  #create and annotate parts of speech tags
  x.pos<-annotate(x,Maxent_POS_Tag_Annotator(),x.word.sent)
  
  #pick only words (removing sentences)
  x.words<-subset(x.pos,type == "word")
  
  #extract pos tags
  x.tags<-sapply(x.words$features, '[[',"POS")
  
  #select required tags
  x.postag <- grep(postag, x.tags)
  
  x.wordtags<- sprintf("%s/%s", x[x.words][x.postag], x.tags[x.postag])
  
  x.wordtagscollapsed <- paste(x.wordtags, collapse = " ")
  
  postable<-strsplit(unlist(x.wordtagscollapsed[1]),' ')
  
  postagdf<-as.data.frame(table(postable))
  
  postagdf$postable<-sapply(strsplit(as.character(postagdf$postable),split="/",fixed=T),function (x)(x[1]))
  
  postagdf$postable<-gsub('"',"",postagdf$postable)
  
  postagdf<-ddply(postagdf,"postable",numcolwise(sum))
  
  return(postagdf)
}

taoverbs<-pick_POS_from_Corpus(tao.corpus,"VB")
save(taoverbs,file="taoverbs.Rdata")
gitaverbs<-pick_POS_from_Corpus(gita.corpus,"VB")
save(gitaverbs,file="gitaverbs.Rdata")
bibleverbs<-pick_POS_from_Corpus(bible.corpus,"VB")
save(bibleverbs,file="bibleverbs.Rdata")
quranverbs<-pick_POS_from_Corpus(quran.corpus,"VB")
save(quranverbs,file="quranverbs.Rdata")

wordcloud(words = taoverbs$postable,ordered.colors=T,rot.per=0,freq = taoverbs$Freq,random.order=F,min.freq=5)

wordcloud(words = gitaverbs$postable,freq = gitaverbs$Freq)
wordcloud(words = taoverbs$postable,freq = taoverbs$Freq)
wordcloud(words = taoverbs$postable,freq = taoverbs$Freq)
