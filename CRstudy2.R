##################################################
#
# Data Analysis Code for Conversational Receptiveness
#             Study 2
#           Mike Yeomans
#
###################################################



##################################################
# Function for clustering standard errors at the dyad level
dyadtest<-function(IV,DV,covars="",dataset,group="session.group",standardize=T){
  scaler=ifelse(standardize,"scale(","(")
  m1<-lm(formula(paste(scaler,IV,")~",scaler,DV,")",covars)),data=dataset)
  # Sandwich adjustment for SE
  vcov <- multiwayvcov::cluster.vcov(m1, dataset[,group]) 
  test.table<-lmtest::coeftest(m1, vcov, stata_fe_model_rank=TRUE) 
  print(paste("df=",m1$df))
  test.table<-round(test.table,4)
  return(test.table)
}
##################################################


# Data dictionary
# dispo_rcptv - day 1 dispositional receptiveness
# my_rcptv - day 2 self-rated receptiveness
# partn_rcptv - day 2 partner-rated receptiveness
# *_part - partner's version of * 
# round_1:round_5 - five separate turns of text
# text - all five turns together


# Demographics
mean(CRstudy2$gender=="m", na.rm=TRUE)
mean(CRstudy2$age,na.rm=TRUE)

# Check random assignment

dyadtest("dispo_rcptv_part","dispo_rcptv",dataset=CRstudy2)

# Within-judge stability

dyadtest("my_rcptv","dispo_rcptv",dataset=CRstudy2)

# Across judge contrasts

dyadtest("partn_rcptv_part","dispo_rcptv",dataset=CRstudy2)

dyadtest("partn_rcptv_part","my_rcptv",dataset=CRstudy2)

# Egocentrism
mean(CRstudy2$my_rcptv)
sd(CRstudy2$my_rcptv)

mean(CRstudy2$partn_rcptv)
sd(CRstudy2$partn_rcptv)

CRstudy2$egodiff_rcptv<-CRstudy2$my_rcptv-CRstudy2$partn_rcptv
dyadtest("egodiff_rcptv","1",dataset=CRstudy2,standardize = FALSE)


###############################################
# Receptiveness Detector
###############################################

trainData<-CRstudy1A %>%
  filter(text_agree==0 & issue_pos!=4)

train.polite<-trainData %>%
  select(text) %>%
  politeness::politeness(parser="spacy")

train.DV<-trainData %>% 
  select(receptiveAll) %>% # train on rater labels, not condition
  unlist() %>%
  as.numeric()


###############################################
# Example test dataset - state & local government data

test.polite<-CRstudy2 %>%
  select(text) %>%
  unlist() %>%
  politeness::politeness(parser="spacy")
###############################################
# Estimate the detector and test


for(.rr in 1:5){
  .convo.polite<-politeness::politeness(CRstudy2[,paste0("round_",.rr)],parser="spacy")
  CRstudy2[,paste0("r",.rr,"polite")]<-politenessProjection(train.polite,
                                                              train.DV,
                                                              .convo.polite)$test_proj
  print(.rr)
}

CRstudy2$allpolite<-politenessProjection(train.polite,
                                         train.DV,
                                         test.polite)$test_proj


CRstudy2 <-CRstudy2 %>%
  left_join(CRstudy2 %>%
              select(idcode_part="idcode",
                     allpolite_part="allpolite",
                     r1polite_part="r1polite",
                     r2polite_part="r2polite",
                     r3polite_part="r3polite",
                     r4polite_part="r4polite",
                     r5polite_part="r5polite"),
            by="idcode_part")


# Correlations with algorithm

dyadtest("allpolite","partn_rcptv_part",dataset=CRstudy2)

dyadtest("allpolite","my_rcptv",dataset=CRstudy2)

dyadtest("allpolite","dispo_rcptv",dataset=CRstudy2)

# Time Course Spot Checks
mean(CRstudy2$r1polite)
sd(CRstudy2$r1polite)

mean(CRstudy2$r5polite)
sd(CRstudy2$r5polite)

CRstudy2$diff1to5<-CRstudy2$r5polite-CRstudy2$r1polite
dyadtest("diff1to5","1",dataset=CRstudy2, standardize = FALSE)

# Works in every round!
dyadtest("partn_rcptv_part","r1polite",dataset=CRstudy2)
dyadtest("partn_rcptv_part","r2polite",dataset=CRstudy2)
dyadtest("partn_rcptv_part","r3polite",dataset=CRstudy2)
dyadtest("partn_rcptv_part","r4polite",dataset=CRstudy2)
dyadtest("partn_rcptv_part","r5polite",dataset=CRstudy2)

# No interaction with time course
CRstudy2 %>%
  select(r1polite,r2polite,r3polite,r4polite,r5polite,partn_rcptv_part) %>%
  gather(round,polite,-partn_rcptv_part) %>%
  mutate(round=gsub("polite","",gsub("r","",round))) %>%
  with(summary(lm(scale(partn_rcptv_part)~scale(polite)*as.numeric(round), data=.)))


# Convergence results

dyadtest("partn_rcptv_part","partn_rcptv",dataset=CRstudy2)

dyadtest("my_rcptv_part","my_rcptv",dataset=CRstudy2)


# First round - no convergence
dyadtest("r1polite_part","r1polite",dataset=CRstudy2)

# Similar story across all four rounds
dyadtest("r2polite_part","r2polite",dataset=CRstudy2)
dyadtest("r3polite_part","r3polite",dataset=CRstudy2)
dyadtest("r4polite_part","r4polite",dataset=CRstudy2)
dyadtest("r5polite_part","r5polite",dataset=CRstudy2)

# We collapsed to make this result succinct for the paper
CRstudy2$r25polite<-rowMeans(CRstudy2[,paste0("r",2:5,"polite")])
CRstudy2$r25polite_part<-rowMeans(CRstudy2[,paste0("r",2:5,"polite_part")])

dyadtest("r25polite_part","r25polite",dataset=CRstudy2)

bind_rows(dyadtest("r1polite_part","r1polite",dataset=CRstudy2)[2,1:2],
          dyadtest("r1polite_part","r2polite",dataset=CRstudy2)[2,1:2],
          dyadtest("r2polite_part","r3polite",dataset=CRstudy2)[2,1:2],
          dyadtest("r3polite_part","r4polite",dataset=CRstudy2)[2,1:2],
          dyadtest("r4polite_part","r5polite",dataset=CRstudy2)[2,1:2]) %>%
  mutate(round=1:5,
         u=Estimate+1.96*`Std. Error`,
         l=Estimate-1.96*`Std. Error`) %>%
  ggplot(aes(x=round,y=Estimate,ymin=l,ymax=u)) +
  geom_point(size=4, shape=15) +
  geom_hline(yintercept = 0)+
  geom_errorbar(width=.2) +
  theme_bw()+
  labs(x="Round of Conversation", 
       y="Correlation Between Partners") +
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.title = element_text(face="bold",size=20),
        legend.text = element_text(size=14),
        legend.title = element_text(face="bold",size=15),
        legend.position = "none",
        panel.grid=element_blank(),
        axis.title = element_text(size=20, face="bold"))

ggsave("convergeplot.png",units="cm",width=18,height=12) 

