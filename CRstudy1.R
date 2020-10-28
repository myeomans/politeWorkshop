##################################################
#
# Data Analysis Code for Conversational Receptiveness
#             Study 1
#           Mike Yeomans
#
###################################################

# Demographics
mean(as.numeric(CRstudy1A$age),na.rm=TRUE)
mean(CRstudy1A$gender==1,na.rm=TRUE)

mean(CRstudy1B$age,na.rm=TRUE)
mean(CRstudy1B$gender==1,na.rm=TRUE)

# Rater count 
length(unique(CRstudy1B$ResponseId))
# Rating count  (two per rater)
nrow(CRstudy1B)/length(unique(CRstudy1B$targetID))

#################################################################################

# Drop non-opposers
CRstudy1A<-CRstudy1A %>%
  filter(text_agree==0 & issue_pos!=4)

# merge average ratings

CRstudy1A <- CRstudy1A %>%
  left_join(CRstudy1B %>%
              group_by(targetID) %>%
              summarize(receptiveAll=mean(as.numeric(ownRating),na.rm=TRUE)),
            by=c("ResponseId"="targetID"))


# Effect of (weak) treatment effect on average rating of their response

CRstudy1A %>%
  with(summary(lm(scale(receptiveAll)~receptive)))

CRstudy1A %>% 
  group_by(receptive) %>%
  summarize(mean=mean(receptiveAll),
            sd=sd(receptiveAll))


# median of treatment group is in the XX percentile of control group
.median<-CRstudy1A %>%
  filter(receptive==1) %>%
  with(median(receptiveAll))

CRstudy1A %>% 
  filter(receptive==0) %>%
  with(mean(receptiveAll<=.median))


###############################################
# Other Linguistic Baselines
###############################################

simple.polite<-politeness(CRstudy1A$text,parser="none")

CRstudy1A <- CRstudy1A %>%
  mutate(wordcount=str_count(text,"[[:alpha:]]+"),
         sentiment=(simple.polite$Positive.Emotion-simple.polite$Negative.Emotion)/wordcount,
         emotion=(simple.polite$Positive.Emotion+simple.polite$Negative.Emotion)/wordcount)

CRstudy1A %>%
  with(list(sentiment=cor.test(receptiveAll,sentiment),
            wordcount=cor.test(receptiveAll,wordcount)))

### Moral Foundations Theory Tests
morFoDict<-quanteda::dictionary(file="data/MFD2.0.dic")

CRstudy1A <- CRstudy1A %>%
  left_join(quanteda::dfm(CRstudy1A$text, dictionary=morFoDict)%>%
              as_tibble() %>%
              mutate(ResponseId=CRstudy1A$ResponseId),
            by="ResponseId")%>%
              mutate(writer_cons=factor(ifelse(issue=="blm",issue_agree,1-issue_agree),
                                        levels=c(0,1),labels=c("Liberal","Conservative")),
                     consLibMFT=scale(care.virtue+care.vice+fairness.virtue+fairness.vice
                                      -loyalty.virtue-loyalty.vice-authority.virtue-authority.vice
                                      -sanctity.virtue-sanctity.vice), 
                     targetMFT=ifelse(writer_cons=="Liberal",(-1)*consLibMFT,consLibMFT))

summary(lm(scale(receptiveAll)~scale(targetMFT),
           data=CRstudy1A))
confint(lm(scale(receptiveAll)~scale(targetMFT),
           data=CRstudy1A))

# Test of "communicated warmth" 

CRstudy1A$commWarmth<-politenessProjection(politeness(politeness::phone_offers$message,parser="spacy"),
                                 politeness::phone_offers$condition,
                                 CRstudy1A %>%
                                   select(text) %>%
                                   politeness(parser="spacy"))$test_proj
CRstudy1A %>%
  with(cor.test(commWarmth,receptiveAll))


###############################################
# Cross-validation Results
###############################################

polite.data<-politeness(CRstudy1A$text,parser="spacy")

# Cross-Validation Prep

ngram.data<-CRstudy1A$text %>%
  DTMtools::DTM(ngrams = 1:3, stop.words = TRUE)

DV<-(CRstudy1A$receptiveAll)

CV.models<-list(ngrams=list(exes=ngram.data),
                politeness=list(exes=polite.data))


cycles<-2 # repeats of the whole procedure to smooth out CV error

for(x in 1:length(CV.models)){
  CV.models[[x]][["guesses"]]<-array(NA,c(length(DV),cycles))
  CV.models[[x]][["coefs"]]<-NA
  for(cycle in 1:cycles){
    cycleModel<-politenessProjection(df_polite_train = CV.models[[x]]$exes,
                                     covar=DV,
                                     cv_folds=10)
    CV.models[[x]][["guesses"]][,cycle]<-cycleModel$train_proj
  }
  CV.models[[x]][["guess"]]<-rowMeans(CV.models[[x]][["guesses"]],na.rm=TRUE)
  CV.models[[x]][["coefs"]]<-cycleModel$train_coefs
}


CRstudy1A$receptiveNLP<-CV.models[["politeness"]][["guess"]]
CRstudy1A$receptiveNLPngrams<-CV.models[["ngrams"]][["guess"]]



# Similar accuracy
CRstudy1A %>%
  with(cor.test(receptiveAll,receptiveNLP))

CRstudy1A %>%
  with(cor.test(receptiveAll,receptiveNLPngrams))

# Very different models

sort(CV.models[["politeness"]][["coefs"]])

sort(CV.models[["ngrams"]][["coefs"]])


