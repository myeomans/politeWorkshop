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

blm.polite<-CRstudy1A %>%
  filter(issue=="blm") %>% 
  select(text) %>%
  politeness::politeness(parser="spacy")

blm.DV.rated<-CRstudy1A %>% 
  filter(issue=="blm") %>% 
  select(receptiveAll) %>%
  unlist() %>%
  as.numeric()

sa.polite<-CRstudy1A %>%
  filter(issue=="sa") %>% 
  select(text) %>%
  unlist() %>%
  politeness::politeness(parser="spacy")

sa.DV.rated<-CRstudy1A %>% 
  filter(issue=="sa") %>% 
  select(receptiveAll) %>%
  unlist() %>%
  as.numeric()



###############################################
#Estimate the transfer learning rate from BLM to SA

print(cor.test(politenessProjection(blm.polite,
                                    blm.DV.rated,
                                    sa.polite)$test_proj,
               sa.DV.rated))

#Estimate the transfer learning rate from SA to BLM

print(cor.test(politenessProjection(sa.polite,
                                    sa.DV.rated,
                                    blm.polite)$test_proj,
               blm.DV.rated))

##########################################
# bag-of-words transfer learning
##########################################

blm.DV.rated<-CRstudy1A %>% 
  filter(issue=="blm") %>% 
  select(receptiveAll) %>%
  unlist() %>%
  as.numeric()

sa.DV.rated<-CRstudy1A %>% 
  filter(issue=="sa") %>% 
  select(receptiveAll) %>%
  unlist() %>%
  as.numeric()

# Train on BLM
blm.ng<-CRstudy1A %>%
  filter(issue=="blm") %>% 
  select(text) %>%
  unlist() %>%
  DTMtools::DTM(ngrams = 1:3, stop.words = TRUE) %>%
  as_tibble()

# Test on SA (using ngrams from BLM training)

sa.ng<-CRstudy1A %>%
  filter(issue=="sa") %>% 
  select(text) %>%
  unlist() %>%
  DTMtools::DTM(ngrams = 1:3, stop.words=TRUE, vocabmatch = blm.ng) %>%
  as_tibble()

# Estimate transfer learning rate
print(cor.test(politenessProjection(blm.ng,
                                    blm.DV.rated,
                                    sa.ng)$test_proj,
               sa.DV.rated))


# Train on SA

sa.ng<-CRstudy1A %>%
  filter(issue=="sa") %>% 
  select(text) %>%
  unlist() %>%
  DTMtools::DTM(ngrams = 1:3, stop.words = TRUE) %>%
  as_tibble()

# Test on BLM (using ngrams from SA training)

blm.ng<-CRstudy1A %>%
  filter(issue=="blm") %>% 
  select(text) %>%
  unlist() %>%
  DTMtools::DTM(ngrams = 1:3, stop.words=TRUE, vocabmatch = sa.ng) %>%
  as_tibble()

# Estimate transfer learning rate
print(cor.test(politenessProjection(sa.ng,
                                    sa.DV.rated,
                                    blm.ng)$test_proj,
               blm.DV.rated))
