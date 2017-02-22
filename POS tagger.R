#####wordnet for synonyms#####

tao.corpus<-as.String(tao.corpus)

wordann<-Maxent_Word_Token_Annotator()
sentann<-Maxent_Sent_Token_Annotator()
posann<-Maxent_POS_Tag_Annotator()



tao1<-annotate(tao.corpus,list(sentann,wordann))
tao.pos<-annotate(tao.corpus,posann,tao1)
tao.words<-subset(tao.pos,type == "word")
tao.tags<-sapply(tao.words$features, '[[',"POS")
table(tao.tags)
thisPOSindex <- grep("NN", tao.tags)
tokenizedAndTagged <- sprintf("%s/%s", tao.corpus[tao.words][thisPOSindex], tao.tags[thisPOSindex])
untokenizedAndTagged <- paste(tokenizedAndTagged, collapse = " ")
untokenizedAndTagged
vbtable<-strsplit(unlist(untokenizedAndTagged[1]),' ')
vbtable2<-strsplit(unlist(vbtable), "/")
vb<-as.data.frame(vbtable2)
vb<-vb[1,]
require(reshape)
pqr<-as.data.frame(table(vbtable))
pqr$vbtable<-sapply(strsplit(as.character(pqr$vbtable),split="/",fixed=T),function (x)(x[1]))
pqr$vbtable<-gsub('"',"",pqr$vbtable)
rrr<-ddply(pqr,"vbtable",numcolwise(sum))
