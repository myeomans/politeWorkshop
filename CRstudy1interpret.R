
###############################################
# Grab some slide examples
###############################################

CRstudy1A[grepl("The public reaction has not been overblown",CRstudy1A$seedtext),]$textID

CRstudy1A %>%
  filter(grepl("The public reaction has not been overblown",seedtext)) %>%
  with(findPoliteTexts(text, politeness(text,parser="spacy"),receptiveNLP,num_docs = 10,type="most"))

CRstudy1A %>%
  filter(grepl("The public reaction has not been overblown",seedtext)) %>%
  with(findPoliteTexts(text, politeness(text,parser="spacy"),receptiveNLP,type = "both",num_docs = 20))

###############################################
# Get a plot
###############################################

train.polite<-CRstudy1A %>%
  select(text) %>%
  unlist() %>%
  politeness::politeness(parser="spacy")

train.DV<-CRstudy1A %>% 
  select(receptiveAll) %>%
  unlist() %>%
  as.numeric()

politenessPlot(train.polite,train.DV,
               split_levels=c("Unreceptive","Receptive"),
               split_cols = c("firebrick","skyblue"),
               split_name="Rater Consensus",
               middle_out=.05) +
  guides(fill = guide_legend(title.position = "left",nrow = 2,
                             title.hjust = .5)) +
  theme(text=element_text(family=""),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("figures/figure1.png",units="cm",res=300,width=20,height=14)

