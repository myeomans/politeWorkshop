##################################################
#
# Data cleaning Code for Conversational Receptiveness
#
# (authors blinded for review)
#
###################################################
library(tidyverse)
library(politeness)

##################################################

study2data <- read.csv("rawData/study2raw.csv", header=TRUE, stringsAsFactors = F)


###############calculate composites
###receptiveness


#reverse code recepts
.receptVars <- c(paste0("dispo_recept_",6:18), paste0("partn_recept_",6:18), paste0("my_recept_",6:18))

study2data[.receptVars] <-
  lapply(study2data[.receptVars],
         function(x) car::recode(x, recodes = "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))

#overall
study2data$partn_recept_overall <- (study2data$partn_recept_1+study2data$partn_recept_2+study2data$partn_recept_3+study2data$partn_recept_4+study2data$partn_recept_5+
                                      study2data$partn_recept_6+study2data$partn_recept_7+study2data$partn_recept_8+study2data$partn_recept_9+
                                      study2data$partn_recept_10+study2data$partn_recept_11+study2data$partn_recept_12+study2data$partn_recept_13+study2data$partn_recept_14+
                                      study2data$partn_recept_15+study2data$partn_recept_16+study2data$partn_recept_17+study2data$partn_recept_18)/18

study2data$my_recept_overall <- (study2data$my_recept_1+study2data$my_recept_2+study2data$my_recept_3+study2data$my_recept_4+study2data$my_recept_5+
                                   study2data$my_recept_6+study2data$my_recept_7+study2data$my_recept_8+study2data$my_recept_9+
                                   study2data$my_recept_10+study2data$my_recept_11+study2data$my_recept_12+study2data$my_recept_13+study2data$my_recept_14+
                                   study2data$my_recept_15+study2data$my_recept_16+study2data$my_recept_17+study2data$my_recept_18)/18

study2data$dispo_recept_overall <- (study2data$dispo_recept_1+study2data$dispo_recept_2+study2data$dispo_recept_3+study2data$dispo_recept_4+study2data$dispo_recept_5+
                                      study2data$dispo_recept_6+study2data$dispo_recept_7+study2data$dispo_recept_8+study2data$dispo_recept_9+
                                      study2data$dispo_recept_10+study2data$dispo_recept_11+study2data$dispo_recept_12+study2data$dispo_recept_13+study2data$dispo_recept_14+
                                      study2data$dispo_recept_15+study2data$dispo_recept_16+study2data$dispo_recept_17+study2data$dispo_recept_18)/18

#negative emotions subfactor
study2data$partn_recept_neg_emo <- (study2data$partn_recept_15+study2data$partn_recept_16+study2data$partn_recept_17+study2data$partn_recept_18)/4

study2data$my_recept_neg_emo <- (study2data$my_recept_15+study2data$my_recept_16+study2data$my_recept_17+study2data$my_recept_18)/4

study2data$dispo_recept_neg_emo <- (study2data$dispo_recept_15+study2data$dispo_recept_16+study2data$dispo_recept_17+study2data$dispo_recept_18)/4

#intellectual curiosity subfactor
study2data$partn_recept_cur <- (study2data$partn_recept_1+study2data$partn_recept_2+study2data$partn_recept_3+study2data$partn_recept_4+study2data$partn_recept_5)/5

study2data$my_recept_cur <- (study2data$my_recept_1+study2data$my_recept_2+study2data$my_recept_3+study2data$my_recept_4+study2data$my_recept_5)/5

study2data$dispo_recept_cur <- (study2data$dispo_recept_1+study2data$dispo_recept_2+study2data$dispo_recept_3+study2data$dispo_recept_4+study2data$dispo_recept_5)/5

#derogation of opponents subfactor
study2data$partn_recept_dero <- (study2data$partn_recept_6+study2data$partn_recept_7+study2data$partn_recept_8+study2data$partn_recept_13+study2data$dispo_recept_14)/5

study2data$my_recept_dero <- (study2data$my_recept_6+study2data$my_recept_7+study2data$my_recept_8+study2data$my_recept_13+study2data$dispo_recept_14)/5

study2data$dispo_recept_dero <- (study2data$dispo_recept_6+study2data$dispo_recept_7+study2data$dispo_recept_8+study2data$dispo_recept_13+study2data$dispo_recept_14)/5

#taboo issues
study2data$partn_recept_taboo <- (study2data$partn_recept_9+study2data$partn_recept_10+study2data$partn_recept_11+study2data$partn_recept_12)/4

study2data$my_recept_taboo <- (study2data$my_recept_9+ study2data$my_recept_10+study2data$my_recept_11+study2data$my_recept_12)/4

study2data$dispo_recept_taboo <- (study2data$dispo_recept_9+ study2data$dispo_recept_10+study2data$dispo_recept_11+study2data$dispo_recept_12)/4

###value of disagreement composite
study2data$value_of_disagreement <- (study2data$discuss_important+study2data$discuss_valuable+study2data$discuss_better_decisions)/3

###work with partner
study2data$work_with_partner <- (study2data$like_on_team + study2data$trust_partn_judgment + study2data$like_partn_represent)/3

# Clean-up
study2data[,paste0("my_recept_",1:18)]<-NULL
study2data[,paste0("partn_recept_",1:18)]<-NULL
study2data[,paste0("dispo_recept_",1:18)]<-NULL

################################################
################################################
for (.p in c("post_death","post_union","post_police")){
  study2data[,.p]<-fct_recode(study2data[,.p],
                              `1`="Strongly Disagree",
                              `2`="Moderately Disagree",
                              `3`="Slightly Disagree",
                              `4`="No Opinion",
                              `5`="Slightly Agree",
                              `6`="Moderately Agree",
                              `7`="Strongly Agree") %>%
    as.character() %>%
    as.numeric()
}


study2data$change_death<-study2data$post_death-study2data$X1_death_attitude
study2data$change_union<-study2data$post_union-study2data$X8_balance_attitude
study2data$change_police<-study2data$post_police-study2data$X9_police_attitude

study2data<-study2data %>%
  mutate(issue_pos=case_when(
    grepl("penalty",issue) ~ X1_death_attitude,
    grepl("unions",issue) ~ X8_balance_attitude,
    grepl("police",issue) ~ X9_police_attitude
  ),
  issue_impt=case_when(
    grepl("penalty",issue) ~ X1_death_impt,
    grepl("unions",issue) ~ X8_balance_impt,
    grepl("police",issue) ~ X9_police_impt
  ),
  issueID=case_when(
    grepl("penalty",issue) ~ "death",
    grepl("unions",issue) ~ "unions",
    grepl("police",issue) ~ "police"
  ),
  post_issue=case_when(
    grepl("penalty",issue) ~ post_death,
    grepl("unions",issue) ~ post_union,
    grepl("police",issue) ~ post_police
  ),
  issue_extrem=abs(issue_pos-4),
  post_extrem=abs(post_issue-4),
  
  post_change=case_when(
    issue_pos>4 ~ issue_pos-post_issue,
    issue_pos<4 ~ post_issue- issue_pos)
  )



study2data<-study2data %>%
  mutate(post_change=case_when(
    issue_pos>4 ~ issue_pos-post_issue,
    issue_pos<4 ~ post_issue- issue_pos)
  )


# Text

study2data$text<-apply(study2data[,paste0("round_",1:5)], 1, paste0, collapse=". ")
study2data$text<-gsub("\\n"," ", study2data$text, fixed=T)


study2data$text_wdct<-stringr::str_count(study2data$text,"[[:alpha:]]+")


################################################
################################################

# Pairing Up

study2data$session.group<-paste0(study2data$session, ".", study2data$group)

study2data$idcode_part<-NA
for(.s in 1:nrow(study2data)){
  study2data[.s,]$idcode_part<-study2data[(study2data$idcode!=study2data[.s,]$idcode)
                                          &(study2data$session.group==study2data[.s,]$session.group),]$idcode
}




names(study2data)<-gsub("_recept_overall","_rcptv",names(study2data),fixed=T)

.partVars<-c(paste0(c("partn","dispo","my"),"_rcptv"),
             "issue_pos","issue_impt",
             "value_of_disagreement","work_with_partner")

study2data <-study2data %>%
  left_join(study2data %>%
              select(c("idcode","text",.partVars)) %>%
              setNames(paste0(names(.),"_part"))) %>%
  mutate(issue_dist=abs(issue_pos-issue_pos_part)) %>%
  filter((!is.na(dispo_rcptv))
         &(!is.na(dispo_rcptv_part))
         &(!is.na(my_rcptv))
         &(!is.na(my_rcptv_part))
         &issue_dist>3)


study2data$dispo_split<-ifelse(study2data$dispo_rcptv>mean(study2data$dispo_rcptv),"High","Low") %>%
  factor(ordered=T, levels=c("Low","High"))
study2data$dispo_split_part<-ifelse(study2data$dispo_rcptv_part>mean(study2data$dispo_rcptv_part),"High","Low") %>%
  factor(ordered=T, levels=c("Low","High"))
study2data$partn_split_part<-ifelse(study2data$partn_rcptv_part>mean(study2data$partn_rcptv_part),"High","Low") %>%
  factor(ordered=T, levels=c("Low","High"))
study2data$my_split<-ifelse(study2data$my_rcptv>mean(study2data$my_rcptv),"High","Low") %>%
  factor(ordered=T, levels=c("Low","High"))


################################################
################################################

###############################################
# Receptiveness Detector
###############################################


.trainData<-study1Adata

.train.polite<-.trainData %>%
  filter(text_agree==0 & issue_pos!=4) %>% # Only people responding to disagreeing views
  select(text) %>%
  unlist() %>%
  politeness::politeness(drop_blank = F, metric="count", parser="spacy")

.train.DV<-.trainData %>% 
  filter(text_agree==0 & issue_pos!=4) %>% 
  select(receptiveAll) %>% # train on rater labels, not condition
  #select(receptive) %>%
  unlist() %>%
  as.numeric()


###############################################
# Example test dataset - state & local government data

.test.polite<-study2data %>%
  select(text) %>%
  unlist() %>%
  politeness::politeness(drop_blank = F, metric="count", parser="spacy")
###############################################
# Estimate the detector and test

study2data$allpolite<-politenessProjection(.train.polite,
                                           .train.DV,
                                           .test.polite)$test_proj


for(.rr in 1:5){
  .convo.polite<-politeness::politeness(study2data[,paste0("round_",.rr)],
                                        drop_blank=F, metric = "count",
                                        parser="spacy")
  study2data[,paste0("r",.rr,"polite")]<-politenessProjection(.train.polite,
                                                              .train.DV,
                                                              .convo.polite)$test_proj
  print(.rr)
}

study2data <-study2data %>%
  left_join(study2data %>%
              select(idcode_part="idcode",
                     allpolite_part="allpolite",
                     r1polite_part="r1polite",
                     r2polite_part="r2polite",
                     r3polite_part="r3polite",
                     r4polite_part="r4polite",
                     r5polite_part="r5polite"),
            by="idcode_part")


# 
# 
# .train.DV<-.trainData %>% 
#   filter(text_agree==0 & issue_pos!=4) %>% 
#   select(receptiveNoCur) %>%
#   unlist() %>%
#   as.numeric()
# 
# study2data$allpoliteNoCur<-politenessProjection(.train.polite,
#                                            .train.DV,
#                                            .test.polite)$test_proj
# 
# 
# 
# .train.DV<-.trainData %>% 
#   filter(text_agree==0 & issue_pos!=4) %>% 
#   select(receptiveCurious) %>% 
#   unlist() %>%
#   as.numeric()
# 
# study2data$allpoliteCurious<-politenessProjection(.train.polite,
#                                                 .train.DV,
#                                                 .test.polite)$test_proj
# 
# 
# .train.DV<-.trainData %>% 
#   filter(text_agree==0 & issue_pos!=4) %>% 
#   select(receptiveTaboo) %>% 
#   unlist() %>%
#   as.numeric()
# 
# study2data$allpoliteTaboo<-politenessProjection(.train.polite,
#                                                   .train.DV,
#                                                   .test.polite)$test_proj
# 
# 
# .train.DV<-.trainData %>% 
#   filter(text_agree==0 & issue_pos!=4) %>% 
#   select(receptiveNegemo) %>% 
#   unlist() %>%
#   as.numeric()
# 
# study2data$allpoliteNegemo<-politenessProjection(.train.polite,
#                                                   .train.DV,
#                                                   .test.polite)$test_proj
# 
# 
# .train.DV<-.trainData %>% 
#   filter(text_agree==0 & issue_pos!=4) %>% 
#   select(receptiveDerogate) %>% 
#   unlist() %>%
#   as.numeric()
# 
# study2data$allpoliteDerogate<-politenessProjection(.train.polite,
#                                                   .train.DV,
#                                                   .test.polite)$test_proj



# study2data <-study2data %>%
#   left_join(study2data %>%
#               select(idcode_part="idcode",
#                      allpolite_part="allpolite",
#                      allpoliteNegemo_part="allpoliteNegemo",
#                      allpoliteDerogate_part="allpoliteDerogate",
#                      allpoliteTaboo_part="allpoliteTaboo",
#                      allpoliteCurious_part="allpoliteCurious",
#                      allpoliteNoCur_part="allpoliteNoCur"),
#             by="idcode_part")
