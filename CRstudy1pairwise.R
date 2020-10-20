##################################################
#
# Data Analysis Code for Conversational Receptiveness
#             Study 1
#           Mike Yeomans
#
###################################################

CRstudy1B$otherRating<-NA
tpb<-txtProgressBar(0,nrow(CRstudy1B))
for(.s in 1:nrow(CRstudy1B)){
  CRstudy1B[.s,]$otherRating<-CRstudy1B %>%
    filter((CRstudy1B$targetID==CRstudy1B[.s,]$targetID)
           &(ResponseId!=CRstudy1B[.s,]$ResponseId)) %>%
    select(ownRating) %>%
    unlist() %>%
    as.numeric() %>%
    mean()
  setTxtProgressBar(tpb,.s)
}


#################################################################################
# human pairwise accuracy
#################################################################################

judgePairs<-left_join(CRstudy1B %>%
            select(target, ownRating,ResponseId) %>%
            spread(target, ownRating) %>%
            rename(ownA="1", ownB="2"),
          CRstudy1B %>%
            select(target, otherRating,ResponseId) %>%
            spread(target, otherRating) %>%
            rename(otherA="1", otherB="2")) 

head(judgePairs)

judgePairs%>%
  mutate(ownChoose=1*(ownA>ownB),
         otherChoose=1*(otherA>otherB),
         ownDiff=(ownA-ownB),
         otherDiff=(otherA-otherB),
         acc=1*(ownChoose==otherChoose)) %>%
  summarize(m=mean(acc),se=sd(acc)/sqrt(n()),
            l=m-1.96*se,u=m+1.96*se)

#################################################################################
# Now everyone do it
#################################################################################

accData <- CRstudy1B %>%
  left_join(CRstudy1A %>%
              select(ResponseId,receptiveNLP,sentiment,wordcount,targetMFT),
            by=c("targetID"="ResponseId"))

left_join(accData %>%
            select(target, receptiveNLP,ResponseId) %>%
            spread(target, receptiveNLP) %>%
            rename(algoA="1", algoB="2"),
          accData %>%
            select(target, otherRating,ResponseId) %>%
            spread(target, otherRating) %>%
            rename(crowdA="1", crowdB="2"))%>%
  left_join(accData %>%
              select(target, ownRating,ResponseId) %>%
              spread(target, ownRating) %>%
              rename(humanA="1", humanB="2")) %>%
  left_join(accData %>%
              select(target, sentiment,ResponseId) %>%
              spread(target, sentiment) %>%
              rename(sentimentA="1", sentimentB="2")) %>%
  left_join(accData %>%
              select(target, wordcount,ResponseId) %>%
              spread(target, wordcount) %>%
              rename(wordcountA="1", wordcountB="2")) %>%
  left_join(accData %>%
              select(target, targetMFT,ResponseId) %>%
              spread(target, targetMFT) %>%
              rename(mftA="1", mftB="2"))%>%
  mutate(crowdChoose=(crowdA>crowdB),
         humanAcc=1*((humanA>humanB)==crowdChoose),
         sentimentAcc=1*((sentimentA>sentimentB)==crowdChoose),
         wordcountAcc=1*((wordcountA>wordcountB)==crowdChoose),
         mftAcc=1*((mftA>mftB)==crowdChoose),
         algoAcc=1*((algoA>algoB)==crowdChoose)
         #transferAcc=1*((transferA>transferB)==crowdChoose)
  ) %>%
  select(sentimentAcc,wordcountAcc,
         humanAcc,#transferAcc
         mftAcc,algoAcc) %>%
  gather(predictor,acc) %>%
  group_by(predictor) %>%
  summarize(m=mean(acc),l=m-1.96*sd(acc)/sqrt(n()),u=m+1.96*sd(acc)/sqrt(n())) %>%
  mutate(m=as.numeric(m),u=as.numeric(u),l=as.numeric(l),
         predictor=factor(predictor,ordered=TRUE,
                          levels=c("mftAcc","sentimentAcc","wordcountAcc",
                                   "transferAcc","algoAcc","humanAcc"),
                          labels=c("Moral Foundations","Sentiment","Word Counts",
                                   "Out-of-Topic Algorithm",
                                   "Cross-Validated Algorithm","Human Judges"))) %>%
  ggplot(aes(x=predictor,y=m,ymin=l,ymax=u,fill=predictor)) +
  geom_bar(stat="identity") +
  coord_flip(ylim=c(.49,.7))+
  geom_hline(yintercept=.5)+
  geom_errorbar(width=0.1) +
  scale_y_continuous(breaks = seq(0,1,.1),position = "right",
                     labels=paste0(seq(0,100,10),"%"))+
  scale_fill_manual(values=c("cornflowerblue","dodgerblue1","skyblue",
                             "forestgreen","sienna2")) +
  theme_bw() +
  geom_hline(yintercept=0)+
  labs(x="",y="Pairwise Match with Average Judges")+
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size=24),
        axis.text.x = element_text(size=16,face="bold"))


#ggsave("accPlot.png",units="cm",width=30,height=20)

