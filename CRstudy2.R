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
  test.table<-lmtest::coeftest(m1, vcov, stata_fe_model_rank=T) 
  print(paste("df=",m1$df))
  test.table<-round(test.table,4)
  return(test.table)
}
##################################################

# Demographics
mean(CRstudy2$gender=="m", na.rm=T)
mean(CRstudy2$age,na.rm=T)

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
