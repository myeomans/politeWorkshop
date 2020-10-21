######################################################
#
#                    Study 1
#
# Communicating Warmth 
#     is Surprisingly Counterproductive
#     in Distributive Negotiations
#
######################################################

# Writing Time

CWstudy1 %>%
  group_by(warm) %>%
  summarize(m=mean(write.time),
            sd=sd(write.time))

CWstudy1 %>%
  with(t.test(write.time~warm, var.equal=TRUE))

# Word Count

CWstudy1 <-CWstudy1 %>%
  mutate(wordCount=str_count(message,"[[:alpha:]]+")) 

CWstudy1 %>%
  group_by(warm) %>%
  summarize(m=mean(wordCount),
            sd=sd(wordCount))

CWstudy1 %>%
  with(t.test(wordCount~warm, var.equal=TRUE))


####################################################
# POLITENESS!
####################################################

polite.data<-politeness(CWstudy1$message, parser="spacy")

# Examples
findPoliteTexts(CWstudy1$message,polite.data,CWstudy1$warm,type = "most") %>%
  as_tibble()
findPoliteTexts(CWstudy1$message,polite.data,CWstudy1$warm,type = "least") %>%
  as_tibble()

# Plot
politenessPlot(polite.data,
               middle_out=.01,
               split=(CWstudy1$warm==1),
               split_levels=c("Tough","Warm"),
               split_name="Communication Style",
               split_cols=c("darkslategray2","firebrick"))

#ggsave("fig1.png",width=20,height=15,units="cm",res=200)

####################################################################
# LIWC warmth dictionary
####################################################################

LIWCwarm<-quanteda::dictionary(list(warmth=c("affectionate","child*","cheer*","commit*","communal",
                                             "compassion*","connect*","considerate","cooperat*",
                                             "depend*","emotiona*","empath*","feminine","flatterable","gentle",
                                             "honest","interpersonal","interdependen*","interpersona*","kind",
                                             "kinship","loyal*","modesty","nag","nurtur*","pleasant*","polite",
                                             "quiet*","respon*","sensitiv*","submissive","supporti*","sympath*",
                                             "tender*","together*","trust*","understand*","warm*","whin*","yield*")))

CWstudy1$warmth<-as.vector(dfm(CWstudy1$message,dictionary=LIWCwarm))/CWstudy1$wordCount

hist(CWstudy1$warmth) # not many hits... unlikely to do well
