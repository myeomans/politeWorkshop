######################################################
#
#                    Study 1
#
# Communicating Warmth 
#     is Surprisingly Counterproductive
#     in Distributive Negotiations
#
######################################################


# Structured Responses

study1 %>%
  group_by(warm) %>%
  summarize(m=mean(OwnMessage),
            sd=sd(OwnMessage))

study1 %>%
  with(t.test(OwnMessage~warm, var.equal=T))

study1 <-study1 %>%
  mutate(about.the.same=1*(OwnMessage==3)) 

study1%>%
  group_by(warm) %>%
  summarize(m=mean(about.the.same))

study1 %>%
  with(chisq.test(table(about.the.same,warm)))

# Writing Time

study1 %>%
  group_by(warm) %>%
  summarize(m=mean(write.time),
            sd=sd(write.time))

study1 %>%
  with(t.test(write.time~warm, var.equal=T))


# Word Count

study1 <-study1 %>%
  mutate(wordCount=str_count(message,"[[alpha:]]+")) 

study1 %>%
  group_by(warm) %>%
  summarize(m=mean(write.time),
            sd=sd(write.time))

study1 %>%
  with(t.test(wordCount~warm, var.equal=T))


####################################################
# POLITENESS!
####################################################

#spacyr::spacy_initialize()

polite.data<-politeness(study1$message, parser="spacy",drop_blank=FALSE)

# Examples
findPoliteTexts(study1$message,polite.data,study1$warm,type = "most")
findPoliteTexts(study1$message,polite.data,study1$warm,type = "least")

# Plot
politenessPlot(polite.data,
               middle_out=.01,
               split=(study1$warm==1),
               split_levels=c("Warm","Tough"),
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

study1$warmth<-as.vector(dfm(study1$message,dictionary=LIWCwarm))/study1$wordCount

hist(study1$warmth)
