
s2.polite<-CRstudy2 %>%
  select(text) %>%
  politeness::politeness(parser="spacy")



politenessPlot(s2.polite,
               ifelse(CRstudy2$my_split=="High",
                      "Low","High"),
               split_levels=c("Unreceptive","Receptive"),
               split_cols = c("firebrick1","navy"),
               drop_blank=0,
               middle_out=.2) +
  theme(text = element_text(family=""))

#ggsave("bmm1.png",units="cm",width=16,height=12)

politenessPlot(s2.polite,
               # ifelse(CRstudy2$partn_split_part=="High",
               #        "Low","High"),
               CRstudy2$work_with_partner,
               split_levels=c("Unreceptive","Receptive"),
               split_cols = c("firebrick1","navy"),
               middle_out=.2)+
  theme(text = element_text(family=""))

#ggsave("bmm2.png",units="cm",width=16,height=12)




data.frame(count=colMeans(s2.polite),
           self=apply(s2.polite,2,function(x) cor(x,CRstudy2$my_rcptv)),
           other=apply(s2.polite,2,function(x) cor(x,CRstudy2$partn_rcptv_part)),
           name=gsub("."," ",names(s2.polite),fixed=T)) %>%
  ggplot(aes(x=self,y=other,label=name)) +
  #geom_abline(slope = 1,intercept = 0,color="firebrick1")+
  geom_hline(yintercept = 0,color="firebrick1")+
  geom_vline(xintercept = 0,color="firebrick1")+
  ggrepel::geom_text_repel() +
  geom_point()+
  xlim(-.25,.25)+
  ylim(-.25,.25)+
  geom_abline(a=0,b=1,lty=2,color="gray")+
  xlab("Self-Rated Receptiveness")+
  ylab("Other-Rated Receptiveness")+
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14))

#ggsave("bmm3.png",units="cm",width=16,height=12)
