######################################################
#
#                    Study 1
#
# Communicating Warmth 
#     is Surprisingly Counterproductive
#     in Distributive Negotiations
#
######################################################


# Cross-Validation Prep

ngram.data<-CWstudy1$message %>%
  tokens(remove_punct = TRUE,
         remove_numbers = FALSE) %>%
  tokens_select(pattern = stopwords("en"), 
                selection = "remove") %>%
  dfm() %>% 
  as.matrix()

DV<-(CWstudy1$warm)

CV.models<-list(Ngrams=list(exes=ngram.data),
                Politeness=list(exes=polite.data),
                `Ngrams & Politeness`=list(exes=cbind(ngram.data,polite.data)))

####################################################################
#  Classifier Loop
####################################################################

cycles<-2 # repeats of the whole procedure to smooth out CV error

for(x in 1:length(CV.models)){
  CV.models[[x]][["guesses"]]<-array(NA,c(length(DV),cycles))
  for(cycle in 1:cycles){
    CV.models[[x]][["guesses"]][,cycle]<-politenessProjection(df_polite_train = CV.models[[x]]$exes,
                                                              covar=DV,
                                                              cv_folds=10)$train_proj
  }
  CV.models[[x]][["guess"]]<-rowMeans(CV.models[[x]][["guesses"]],na.rm=TRUE)
  CV.models[[x]][["acc"]]<-pROC::roc(DV,CV.models[[x]][["guess"]], 
                                     direction="<",ci=TRUE)
  CV.models[[x]][["plotSet"]]<-as.vector(CV.models[[x]][["acc"]]$ci)
}


####################################################################
# Evaluate Accuracy
####################################################################

plotSet<-lapply(CV.models,function(x) x$plotSet) %>%
  bind_cols()%>%
  t() %>%
  as.data.frame() %>%
  mutate_all(as.numeric) %>%
  mutate(feature=names(CV.models))

wordCountAcc<-pROC::roc(DV,CWstudy1$wordCount, direction="<",ci=TRUE)$ci
warmthAcc<-pROC::roc(DV,CWstudy1$wordCount, direction="<",ci=TRUE)$ci

plotSet<-plotSet %>%
  add_row(feature="Warmth",V1=warmthAcc[1],V2=warmthAcc[2],V3=warmthAcc[3]) %>%
  add_row(feature="Word Count",V1=wordCountAcc[1],V2=wordCountAcc[2],V3=wordCountAcc[3])


plotSet %>%
  mutate(feature=factor(feature,ordered=T,
                           levels=c("Ngrams & Politeness","Politeness","Ngrams","Warmth","Word Count"))) %>%
  ggplot(aes(x=V2,xmin=V1,xmax=V3,y=feature,fill=feature)) +
  geom_bar(stat="identity") +
  geom_errorbarh(height=.4)+
  theme_bw() +
  xlab("Accuracy")+
  ylab("Model")+
  scale_x_continuous(breaks=seq(0,1,.1),
                     labels=paste0(seq(0,100,10),"%"))+
  coord_cartesian(xlim = c(.5, 1))+
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=20),
        legend.position="none",
        panel.grid.minor = element_blank(),
        panel.grid.major  = element_blank())

ggsave("CWs1acc.png",units="cm",width=30,height=20)

pROC::roc.test(CV.models[["Ngrams"]][["acc"]],
               CV.models[["Politeness"]][["acc"]],
               paired=TRUE)

pROC::roc.test(CV.models[["Ngrams"]][["acc"]],
               CV.models[["Ngrams & Politeness"]][["acc"]],
               paired=TRUE)
