##################################################
#
# Data Analysis Code for Conversational Receptiveness
#             Study 1
#           Mike Yeomans
#
###################################################

#######################################################
# Does receptiveness model transfer across topics?
#######################################################
blm.polite<-CRstudy1 %>%
  filter(issue=="blm") %>% 
  select(text) %>%
  politeness::politeness(parser="spacy")

blm.DV.rated<-CRstudy1 %>% 
  filter(issue=="blm") %>% 
  select(receptiveAll) %>%
  unlist() %>%
  as.numeric()

sa.polite<-CRstudy1 %>%
  filter(issue=="sa") %>% 
  select(text) %>%
  unlist() %>%
  politeness::politeness(parser="spacy")

sa.DV.rated<-CRstudy1 %>% 
  filter(issue=="sa") %>% 
  select(receptiveAll) %>%
  unlist() %>%
  as.numeric()



###############################################
#Estimate the transfer learning rate from annotations

print(cor.test(politenessProjection(blm.polite,
                                    blm.DV.rated,
                                    sa.polite)$test_proj,
               sa.DV.rated))

# both ways!
print(cor.test(politenessProjection(sa.polite,
                                    sa.DV.rated,
                                    blm.polite)$test_proj,
               blm.DV.rated))

##########################################
# bag-of-words transfer learning
##########################################

blm.DV.rated<-CRstudy1 %>% 
  filter(issue=="blm") %>% 
  select(receptiveAll) %>%
  unlist() %>%
  as.numeric()

sa.DV.rated<-CRstudy1 %>% 
  filter(issue=="sa") %>% 
  select(receptiveAll) %>%
  unlist() %>%
  as.numeric()

blm.ng<-CRstudy1 %>%
  filter(issue=="blm") %>% 
  select(text) %>%
  unlist() %>%
  DTMtools::DTM(ngrams = 1:3, stop.words = TRUE) %>%
  as.tibble()

sa.ng<-CRstudy1 %>%
  filter(issue=="sa") %>% 
  select(text) %>%
  unlist() %>%
  DTMtools::DTM(ngrams = 1:3, stop.words=TRUE, vocabmatch = blm.ng) %>%
  as.tibble()




print(cor.test(politenessProjection(blm.ng,
                                    blm.DV.rated,
                                    sa.ng)$test_proj,
               sa.DV.rated))


# both ways!

sa.ng<-CRstudy1 %>%
  filter(issue=="sa") %>% 
  select(text) %>%
  unlist() %>%
  DTMtools::DTM(ngrams = 1:3, stop.words = TRUE) %>%
  as.tibble()

blm.ng<-CRstudy1 %>%
  filter(issue=="blm") %>% 
  select(text) %>%
  unlist() %>%
  DTMtools::DTM(ngrams = 1:3, stop.words=TRUE, vocabmatch = sa.ng) %>%
  as.tibble()

print(cor.test(politenessProjection(sa.ng,
                                    sa.DV.rated,
                                    blm.ng)$test_proj,
               blm.DV.rated))
