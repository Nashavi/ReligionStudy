
##Conver PDF to text and read them as a Corpus. Installation of xpdftotext is required. For more details visit: http://stackoverflow.com/questions/17563896/readpdf-tm-package-in-r/19926301#19926301

# path.1<-'"//Users//Avi//Documents//UCD//BA Prac//ReligionStudy//pdftotext"'
# path.2<-'"//Users//Avi//Documents//UCD/BA Prac//ReligionStudy//"'
# 
# pdf2corpus<- function(file){
#   system(paste(path.1,paste(path.2,file,sep = "")))
# }
# 
# paste("perl -ane 'system(\"cat /auto/Sample_output/tmp.$F[0].vcf >> Sample_90061.vcf\");'")
# ,sep="")
# pdf2corpus(tao.pdf)
# 
# paste(
# }}}
# 
# system("\"/Users/Avi/Documents/UCD/BA Prac/ReligionStudy/pdftotext\" \"/Users/Avi/Documents/UCD/BA Prac/ReligionStudy/tao.pdf\"", wait=FALSE)

system(paste('"/Users/Avi/Documents/UCD/BA Prac/ReligionStudy/pdftotext"', 
             '"/Users/Avi/Documents/UCD/BA Prac/ReligionStudy/tao.pdf"'), wait=FALSE)
system(paste('"/Users/Avi/Documents/UCD/BA Prac/ReligionStudy/pdftotext"', 
             '"/Users/Avi/Documents/UCD/BA Prac/ReligionStudy/bible.pdf"'), wait=FALSE)
system(paste('"/Users/Avi/Documents/UCD/BA Prac/ReligionStudy/pdftotext"', 
             '"/Users/Avi/Documents/UCD/BA Prac/ReligionStudy/gita.pdf"'), wait=FALSE)
system(paste('"/Users/Avi/Documents/UCD/BA Prac/ReligionStudy/pdftotext"', 
             '"/Users/Avi/Documents/UCD/BA Prac/ReligionStudy/quran.pdf"'), wait=FALSE)
require(tm)
require(wordcloud)
require(RColorBrewer)
require(RTextTools)
require(SnowballC)
require(igraph)

tao.corpus <- Corpus(URISource("/Users/Avi/Documents/UCD/BA Prac/ReligionStudy/tao.txt"))
bible.corpus <- Corpus(URISource("/Users/Avi/Documents/UCD/BA Prac/ReligionStudy/bible.txt"))
gita.corpus <- Corpus(URISource("/Users/Avi/Documents/UCD/BA Prac/ReligionStudy/gita.txt"))
quran.corpus <- Corpus(URISource("/Users/Avi/Documents/UCD/BA Prac/ReligionStudy/quran.txt"))


#Clean Up Corpus
cleanup<- function(corpus){
  tmpcorpus<-tm_map(corpus,removePunctuation)
  tmpcorpus<-tm_map(tmpcorpus,removeNumbers)
  tmpcorpus<-tm_map(tmpcorpus,tolower)
  tmpcorpus<-tm_map(tmpcorpus,stripWhitespace)
  tmpcorpus<-tm_map(tmpcorpus,PlainTextDocument)
  #tmpcorpus<-tm_map(tmpcorpus,removeWords,words=as.character(stop.words))
  #tmpcorpus<-tm_map(tmpcorpus,removeWords,stopwords("english"))
  #tmpcorpus<-tm_map(tmpcorpus,stemDocument)
  return(tmpcorpus)
}

tao.corpus<-cleanup(tao.corpus)
bible.corpus<-cleanup(bible.corpus)
gita.corpus<-cleanup(gita.corpus)
quran.corpus<-cleanup(quran.corpus)

wordcloud(words = tao.corpus, scale=c(4,0.8), max.words=100, random.order=FALSE, 
          rot.per=0.1, use.r.layout=FALSE, colors="black")
wordcloud(words = bible.corpus, scale=c(4,0.8), max.words=100, random.order=FALSE, 
          rot.per=0.1, use.r.layout=FALSE, colors="chocolate4")
wordcloud(words = gita.corpus, scale=c(4,0.8), max.words=100, random.order=FALSE, 
              rot.per=0.1, use.r.layout=FALSE, colors="chocolate1")
wordcloud(words = quran.corpus, scale=c(4,0.8), max.words=100, random.order=FALSE, 
              rot.per=0.1, use.r.layout=FALSE, colors="darkgreen")

tmpText <- c(tao.corpus, bible.corpus,gita.corpus,quran.corpus)
tdm<-TermDocumentMatrix(tmpText)
tdm<-as.matrix(tdm)
colnames(tdm)<- c("Tao","Bible","Gita","Quran")

comparison.cloud(tdm, random.order=FALSE, 
                 colors = c("black", "chocolate4", "chocolate1", "darkgreen"),
                 title.size=1.5, max.words=150, rot.per = 0)

commonality.cloud(tdm, random.order=FALSE, 
                  colors = c("black", "chocolate4", "chocolate1", "darkgreen"),
                  max.words=150, rot.per = 0)


# create a term-term adjacency matrix

#gitatdm<-TermDocumentMatrix(gita.corpus)
#gitatdm<-as.matrix(gitatdm)

tdm[tdm>=1]<-1
termMatrix <- tdm %*% t(tdm)
termMatrix<-termMatrix[1:10,1:10]
termMatrix[termMatrix>=1]<-1
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
# remove loops
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
plot(g,layout =layout.fruchterman.reingold(g))

