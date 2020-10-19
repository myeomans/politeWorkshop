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
library(doMC)        # parallel processing (speeds up glmnet)
library(pROC)        # non-parametric accuracy using ROC 

options(dplyr.summarise.inform=FALSE)

#######################################################

# -------- Workflow Example 1 --------
# Communicating Warmth in Distributive Negotiations
#     is Surprisingly Counterproductive

study1<-read.csv("data/CWstudy1.csv")
source("CWstudy1.R")
source("CWstudy1accuracy.R")

study2<-read.csv("data/CWstudy2.csv")
source("CWstudy2.R")

study3<-read.csv("data/CWstudy3.csv")
source("CWstudy3.R")

# -------- Workflow Example 2 --------
# Conversational Receptiveness:
#     Improving Engagement with Opposing Views

study1A<-read.csv("data/CRstudy1.csv")
source("CRstudy1.R")
source("CRstudy1plots.R")
source("CRstudy2.R")



#######################################################