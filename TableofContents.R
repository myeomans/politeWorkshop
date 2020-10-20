#######################################################
# 
#     Workshop on Politeness in Social Interaction
#
#                 Michael Yeomans
#
#######################################################

library(politeness)  # what we're here for
library(quanteda)    # generic text analysis
library(tidyverse)   # useful and ubiquitous
library(glmnet)      # machine learning algorithm 
library(pROC)        # non-parametric accuracy using ROC 

#######################################################

# -------- Workflow Example 1 --------
# Communicating Warmth in Distributive Negotiations
#     is Surprisingly Counterproductive


CWstudy1<-read.csv("data/CWstudy1.csv")
source("CWstudy1.R")
source("CWstudy1accuracy.R")


CWstudy3turns<-read.csv("data/CWstudy3turns.csv")
CWstudy3people<-read.csv("data/CWstudy3people.csv")

source("CWstudy3.R")

# -------- Workflow Example 2 --------
# Conversational Receptiveness:
#     Improving Engagement with Opposing Views

library(DTMtools) # Mike Y's special ngram extractor
library(multiwayvcov) # cluster-robust standard errors
library(lmtest) # regression models
##################################################



CRstudy1A<-read.csv("data/CRstudy1A.csv")
CRstudy1B<-read.csv("data/CRstudy1B.csv")
source("CRstudy1.R")
source("CRstudy1pairwise.R")
source("CRstudy1transfer.R")
source("CRstudy1interpret.R")

CRstudy2<-read.csv("data/CRstudy2.csv")

source("CRstudy2.R")
source("CRstudy2outcomes.R")
source("CRstudy2bmm.R")


#######################################################