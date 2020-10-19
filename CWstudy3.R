library(politeness)
library(tidyverse)
library(glmnet)

CWstudy1<-read.csv("CWstudy1.csv")

CWstudy3turns<-read.csv("CWstudy3turns.csv")
CWstudy3people<-read.csv("CWstudy3people.csv")

######################################################
# Data for pre-trained model from Study 1
######################################################
polite.train<-politeness::politeness(CWstudy1$message, parser="spacy")
DV.train<-(CWstudy1$warm)

##########################################################################################
# Merge text data over from chats
##########################################################################################

CWstudy3people <- CWstudy3people %>%
  left_join(CWstudy3turns %>%
              filter(turn<3) %>%
              select(id,first_turn_text="text"),
            by="id")

CWstudy3people <- CWstudy3people %>%
  left_join(CWstudy3turns %>%
              filter(turn>2) %>%
              group_by(id) %>% 
              summarize(all_chat_text = paste0(text, collapse = ". ")),
            by="id") 


##########################################################################################
# Initial Buyer Offer - Politeness
##########################################################################################
buyer.first.polite<-CWstudy3people %>%
  filter(seller==0) %>%
  select(first_turn_text) %>%
  unlist() %>%
  politeness(parser="spacy")

buyer.first.DV<-CWstudy3people %>%
  filter(seller==0) %>%
  select(tough)%>%
  unlist()

buyer.first.pred<-as.vector(politenessProjection(polite.train,DV.train,buyer.first.polite)$test_proj)

pROC::roc(buyer.first.DV, buyer.first.pred, ci=T)


politeness::politenessPlot(buyer.first.polite,
                           split=buyer.first.DV,
                           split_levels=c("Warm","Tough"),
                           split_name="Communication\n         Style",
                           split_cols=c("firebrick","darkslategray2"),
                           top_title="Study 3 Buyer Initial Offers")
#ggsave(filename="Figure_2a.png", width=1200, height=1200, res=200)

##########################################################################################
# All Buyer Chat- Politeness
##########################################################################################
buyer.all.polite<-CWstudy3people %>%
  filter(seller==0) %>%
  select(all_chat_text) %>%
  unlist() %>%
  politeness(parser="spacy")

buyer.all.DV<-CWstudy3people %>%
  filter(seller==0) %>%
  select(tough)%>%
  unlist()

buyer.all.pred<-as.vector(politenessProjection(polite.train,DV.train,buyer.all.polite)$test_proj)

pROC::roc(buyer.all.DV, buyer.all.pred, ci=T)


politeness::politenessPlot(buyer.all.polite,
                           split=buyer.all.DV,
                           split_levels=c("Warm","Tough"),
                           split_name="Communication\n         Style",
                           split_cols=c("firebrick","darkslategray2"),
                           top_title="Study 3 Buyer Later Chat Text")
##########################################################################################
# Initial Seller Response - Politeness
##########################################################################################
seller.first.polite<-CWstudy3people %>%
  filter(seller==1) %>%
  select(first_turn_text) %>%
  unlist() %>%
  politeness(parser="spacy")

seller.first.DV<-CWstudy3people %>%
  filter(seller==1) %>%
  select(partner_tough) %>%
  unlist()

seller.first.pred<-politenessProjection(polite.train,DV.train,seller.first.polite)$test_proj

pROC::roc(seller.first.DV, seller.first.pred, ci=T)


politeness::politenessPlot(seller.first.polite,
                           split=seller.first.DV,
                           split_levels=c("Warm","Tough"),
                           split_name="Communication\n         Style",
                           split_cols=c("firebrick","darkslategray2"),
                           top_title="Study 3 Seller Initial Offers")


#ggsave(filename="Figure_2b.png", units="cm", width=20, height=20, res=200)



##########################################################################################
# All Seller Chat - Politeness
##########################################################################################
seller.all.polite<-CWstudy3people %>%
  filter(seller==1) %>%
  select(all_chat_text) %>%
  politeness(parser="spacy")

seller.all.DV<-CWstudy3people %>%
  filter(seller==1) %>%
  select(partner_tough)%>%
  unlist()

seller.all.pred<-politenessProjection(polite.train,DV.train,seller.all.polite)$test_proj

pROC::roc(seller.all.DV, seller.all.pred, ci=T)


#seller.all.pred<-politenessProjection(buyer.first.polite,buyer.first.DV,seller.all.polite)$test_proj
#pROC::roc(seller.all.DV, seller.all.pred, ci=T)



politeness::politenessPlot(seller.all.polite,
                           split=seller.all.DV,
                           split_levels=c("Warm","Tough"),
                           split_name="Communication\n         Style",
                           split_cols=c("firebrick","darkslategray2"),
                           top_title="Study 3 Seller All Chat Text")
##########################################################################################
##########################################################################################

