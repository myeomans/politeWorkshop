
# Measures of Consequences

CRstudy2 %>%
  select(like_on_team,trust_partn_judgment,like_partn_represent) %>%
  psych::alpha()

CRstudy2 %>%  
  select(discuss_important,discuss_valuable,discuss_better_decisions) %>%
  psych::alpha()

# Behavioral consequences and dispositional receptiveness

dyadtest("dispo_rcptv","work_with_partner",dataset=CRstudy2)

dyadtest("dispo_rcptv","value_of_disagreement",dataset=CRstudy2)

# Linguistic Receptiveness and interpersonal preferences

dyadtest("work_with_partner","allpolite_part",dataset=CRstudy2)

dyadtest("work_with_partner","allpolite_part","+scale(dispo_rcptv)",dataset=CRstudy2)

# Partner self ratings and interpersonal preferences

dyadtest("work_with_partner","my_rcptv_part",dataset=CRstudy2)

dyadtest("work_with_partner","dispo_rcptv_part",dataset=CRstudy2)

dyadtest("work_with_partner","my_rcptv_part","+scale(dispo_rcptv)",dataset=CRstudy2)
dyadtest("work_with_partner","dispo_rcptv_part","+scale(dispo_rcptv)",dataset=CRstudy2)


# Linguistic Receptiveness and value of disagreement
dyadtest("value_of_disagreement","allpolite_part",dataset=CRstudy2)

dyadtest("value_of_disagreement","allpolite_part","+scale(dispo_rcptv)",dataset=CRstudy2)


# Outcome convergence

dyadtest("work_with_partner_part","work_with_partner",dataset=CRstudy2)

dyadtest("value_of_disagreement_part","value_of_disagreement",dataset=CRstudy2)


preds<-rev(c("partn_rcptv",
             "allpolite_part",
             "dispo_rcptv",
             "dispo_rcptv_part",
             "my_rcptv_part"))

predLabs<-rev(c("Own Day 2\nRating of Partner",
                "Algorithm's\nRating of Partner",
                "Own Day 1\nSelf-Rating",
                "Partner's Day 1\nSelf-Rating",
                "Partner's Day 2\nSelf-Rating"))

outs<-c("work_with_partner",
        "value_of_disagreement")

outLabs<-c(
  "Collaboration Intentions",
  "Values Disagreement in General"
)

CRstudy2 %>%
  select(c(preds,outs)) %>%
  gather(Outcome,rating,-dispo_rcptv_part,
         -partn_rcptv,-dispo_rcptv,
         -my_rcptv_part,-allpolite_part)%>% 
  gather(Predictor,label,-Outcome,-rating) %>%
  mutate(Outcome=factor(Outcome, 
                        ordered=TRUE,
                        labels=outLabs,
                        levels=outs))%>%
  group_by(Outcome,Predictor)%>%
  summarize(e=cor.test(rating,label)$estimate,
            l=cor.test(rating,label)$conf.int[1],
            u=cor.test(rating,label)$conf.int[2])%>%
  mutate(Predictor=factor(Predictor, 
                          ordered=TRUE,
                          levels=preds,
                          labels=predLabs)) %>%
  ggplot(aes(x=Predictor,y=e,ymin=l,ymax=u,
             group=Outcome,
             pch=Outcome,
             color=Outcome)) +
  coord_flip() +
  geom_hline(yintercept=0) +
  geom_point(stat="identity", size=3, position=position_dodge(width=.3)) +
  geom_errorbar(width=.4, position=position_dodge(width=.3)) +
  theme_bw() +
  ylab("Correlation") +
  xlab("")+
  ggtitle("Prediction Method") +
  scale_color_manual(values=c("forestgreen","goldenrod3")) +
  guides(shape=guide_legend(ncol=1,title.position = "top",reverse=TRUE)) +
  guides(color=guide_legend(ncol=1,title.position = "top",reverse=TRUE)) +
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.grid = element_blank(),
        plot.title = element_text(face="bold",size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(face="bold",size=15),
        legend.position = "bottom",
        axis.title = element_text(size=20, face="bold"))

ggsave("CRstudy2outcomes.png",units="cm",width=18,height=12)
