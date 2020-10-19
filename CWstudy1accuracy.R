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

ngram.data<-study1$message %>%
  tokens(remove_punct = TRUE,
         remove_numbers = FALSE) %>%
  tokens_select(pattern = stopwords("en"), 
                selection = "remove") %>%
  dfm() %>% 
  as.matrix()

DV<-(study1$warm)

CV.models<-list(ngrams=list(exes=ngram.data),
                politeness=list(exes=polite.data),
                all=list(exes=cbind(ngram.data,polite.data)))

####################################################################
#  Classifier Loop
####################################################################

cycles<-2 # repeats of the whole procedure to smooth out CV error

registerDoMC(cores=detectCores()) # parallel processing - much faster!

for(x in 1:length(CV.models)){
  CV.models[[x]][["guesses"]]<-array(NA,c(length(DV),cycles))
  for(cycle in 1:cycles){
    CV.models[[x]][["guesses"]][,cycle]<-politenessProjection(df_polite_train = CV.models[[x]]$exes,
                                                              covar=DV,
                                                              cv_folds=10,
                                                              parallel=TRUE)$train_proj
  }
  CV.models[[x]][["guess"]]<-rowMeans(CV.models[[x]][["guesses"]],na.rm=T)
  CV.models[[x]][["acc"]]<-pROC::roc(DV,CV.models[[x]][["guess"]], 
                                     direction="<",ci=T)
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

wordCountAcc<-pROC::roc(DV,study1$wordCount, direction="<",ci=T)$ci
warmthAcc<-pROC::roc(DV,study1$wordCount, direction="<",ci=T)$ci

plotSet<-plotSet %>%
  add_row(feature="Warmth",V1=warmthAcc[1],V2=warmthAcc[2],V3=warmthAcc[3]) %>%
  add_row(feature="Word Count",V1=wordCountAcc[1],V2=wordCountAcc[2],V3=wordCountAcc[3])


plotSet %>%
  ggplot(aes(x=V2,xmin=V1,xmax=V3,y=feature,color=feature)) +
  geom_point(stat="identity") +
  geom_errorbarh(height=.4)+
  xlim(.5,1) +
  theme_bw() +
  xlab("Accuracy")+
  ylab("Model")+
  scale_x_continuous(breaks=seq(0,1,.1),
                     labels=paste0(seq(0,100,10),"%"))+
  theme(axis.text=element_text(size=16),
        legend.position="none",
        panel.grid.minor = element_blank(),
        panel.grid.major  = element_blank())


pROC::roc.test(CV.models[["ngrams"]][["acc"]],
               CV.models[["politeness"]][["acc"]],
               paired=T)

pROC::roc.test(CV.models[["ngrams"]][["acc"]],
               CV.models[["all"]][["acc"]],
               paired=T)
