#ACE Analysis for Processing Speed Study
library(doBy)
library(aceR)
library(readr)
library(ggplot2)
library(tidyverse)

#Load data
path=("~/Documents/CMI/bks_ace_analysis/data")
data=load_ace_bulk(path, pulvinar=TRUE, exclude="Demographics")

#Write out trial level data
trial_level_data=unnest(data)
write.csv(trial_level_data, file=paste("~/Documents/CMI/bks_ace_analysis/trial_data/ACE_trialleveldata", ".csv", sep=""),row.names=FALSE)

##############################################################################################################################
#BRT Analysis

BRT = data$data$BRT

BRT_rt <- function(p_id, hand) {
  
  # p_id of {001, 002, 004, 010, 012, 013}
  # Hand = Left or Right
  
  BRT = data$data$BRT
  p_id_session1 <- paste('ADMIN-UCSF-BK', p_id, sep="")
  p_id_session2 <- paste('ADMIN-UCSF-BS', p_id, sep="")
  
  # Session 1 dataframe
  pid_BRT_session1 = subset(BRT, pid == p_id_session1)
  pid_BRT_session1 = subset(pid_BRT_session1, condition == hand)
  
  pid_BRT_session1$condition <- as.factor(pid_BRT_session1$condition)
  pid_BRT_session1$correct_button <- as.factor(pid_BRT_session1$correct_button)
  levels(pid_BRT_session1$correct_button)[levels(pid_BRT_session1$correct_button)=="correct"] <- "Correct"
  levels(pid_BRT_session1$correct_button)[levels(pid_BRT_session1$correct_button)=="incorrect"] <- "Incorrect"
  levels(pid_BRT_session1$correct_button)[levels(pid_BRT_session1$correct_button)=="no_response"] <- "No Response"
  names(pid_BRT_session1)[names(pid_BRT_session1)=="correct_button"]  <- "Button"
  names(pid_BRT_session1)[names(pid_BRT_session1)=="condition"]  <- "Condition"
  pid_BRT_session1 <- subset(pid_BRT_session1, Button != 'No Response')
  pid_BRT_session1$session <- 1
  pid_BRT_session1$session <- as.factor(pid_BRT_session1$session)
  
  # Session 2 dataframe
  pid_BRT_session2 = subset(BRT, pid == p_id_session2)
  pid_BRT_session2 = subset(pid_BRT_session2, condition == hand)
  
  pid_BRT_session2$condition <- as.factor(pid_BRT_session2$condition)
  pid_BRT_session2$correct_button <- as.factor(pid_BRT_session2$correct_button)
  levels(pid_BRT_session2$correct_button)[levels(pid_BRT_session2$correct_button)=="correct"] <- "Correct"
  levels(pid_BRT_session2$correct_button)[levels(pid_BRT_session2$correct_button)=="incorrect"] <- "Incorrect"
  levels(pid_BRT_session2$correct_button)[levels(pid_BRT_session2$correct_button)=="no_response"] <- "No Response"
  names(pid_BRT_session2)[names(pid_BRT_session2)=="correct_button"]  <- "Button"
  names(pid_BRT_session2)[names(pid_BRT_session2)=="condition"]  <- "Condition"
  pid_BRT_session2 <- subset(pid_BRT_session2, Button != 'No Response')
  pid_BRT_session2$session <- 2
  pid_BRT_session2$session <- as.factor(pid_BRT_session2$session)
  
  dframe = rbind(pid_BRT_session1, pid_BRT_session2)
  
  savename = paste("~/Documents/CMI/bks_ace_analysis/results/BRT_", p_id, "_", hand, ".pdf", sep="")
  
  pdf(savename)
  print(ggplot(dframe, aes(x=factor(Button), fill=factor(session), y=dframe$rt)) +
    
    scale_y_continuous(limits = c(0,700)) +
    stat_summary(fun.data=mean_sdl, width=0.5, fun.args = list(mult=1),
                 geom="errorbar", position=position_dodge(width=1), color="grey70") +
    stat_summary(fun.y=mean, geom="point", position=position_dodge(width=1), size=2.7, alpha = 0.8, aes(color=session)) +
    geom_point(aes(group=factor(session), colour = factor(session), x=Button, y=dframe$rt),
               position= position_dodge(width=1), alpha= 0.4, size=1.2) +
    
    theme_classic()+
    theme(axis.title.x = element_text(face="bold", size=16),
          axis.title.y = element_text(face="bold", size=16),
          axis.text.x  = element_text(size=16, angle=65, vjust=0.6),
          axis.text.y  = element_text(size=16),
          plot.title   = element_text(vjust=2, hjust=0.5, face="bold", size=20),
          legend.title = element_text(size=16, face="bold"))+
    labs(title="BKS Training Results for BRT",
         x="Button Press Condition",
         y="Response Time") +
    scale_color_discrete(name="Session")+
    scale_fill_discrete(name="", guide='none'))
  
  dev.off()
  
}

BRT_rt('013', 'Left')

##############################################################################################################################
#SAAT Analysis

SAAT = data$data$SAAT

SAAT_RT <- function (p_id, attention) {
  
  # p_id of {001, 002, 004, 010, 012, 013}
  # attention = Impulsive, Sustained
  
  SAAT = data$data$SAAT

  p_id_session1 <- paste('ADMIN-UCSF-BK', p_id, sep="")
  p_id_session2 <- paste('ADMIN-UCSF-BS', p_id, sep="")
  
  # Session 1 dataframe
  pid_SAAT_session1 = subset(SAAT, pid == p_id_session1)
  pid_SAAT_session1$condition <- as.factor(pid_SAAT_session1$condition)
  pid_SAAT_session1$correct_button <- as.factor(pid_SAAT_session1$correct_button)
  levels(pid_SAAT_session1$condition)[levels(pid_SAAT_session1$condition)=="impulsive"] <- "Impulsive"
  levels(pid_SAAT_session1$condition)[levels(pid_SAAT_session1$condition)=="sustained"] <- "Sustained"
  pid_SAAT_session1 = subset(pid_SAAT_session1, condition == attention)
  levels(pid_SAAT_session1$correct_button)[levels(pid_SAAT_session1$correct_button)=="correct"] <- "Correct"
  levels(pid_SAAT_session1$correct_button)[levels(pid_SAAT_session1$correct_button)=="incorrect"] <- "Incorrect"
  levels(pid_SAAT_session1$correct_button)[levels(pid_SAAT_session1$correct_button)=="no_response"] <- "No Response"
  names(pid_SAAT_session1)[names(pid_SAAT_session1)=="correct_button"]  <- "Button"
  names(pid_SAAT_session1)[names(pid_SAAT_session1)=="condition"]  <- "Condition"
  levels(pid_SAAT_session1$Condition)[levels(pid_SAAT_session1$Condition)=="impulsive"] <- "Impulsive"
  levels(pid_SAAT_session1$Condition)[levels(pid_SAAT_session1$Condition)=="sustained"] <- "Sustained"
  pid_SAAT_session1$session <- 1
  pid_SAAT_session1$session <- as.factor(pid_SAAT_session1$session)
  
  # Session 2 dataframe
  pid_SAAT_session2 = subset(SAAT, pid == p_id_session2)
  pid_SAAT_session2$condition <- as.factor(pid_SAAT_session2$condition)
  pid_SAAT_session2$correct_button <- as.factor(pid_SAAT_session2$correct_button)
  levels(pid_SAAT_session2$condition)[levels(pid_SAAT_session2$condition)=="impulsive"] <- "Impulsive"
  levels(pid_SAAT_session2$condition)[levels(pid_SAAT_session2$condition)=="sustained"] <- "Sustained"
  pid_SAAT_session2 = subset(pid_SAAT_session2, condition == attention)
  levels(pid_SAAT_session2$correct_button)[levels(pid_SAAT_session2$correct_button)=="correct"] <- "Correct"
  levels(pid_SAAT_session2$correct_button)[levels(pid_SAAT_session2$correct_button)=="incorrect"] <- "Incorrect"
  levels(pid_SAAT_session2$correct_button)[levels(pid_SAAT_session2$correct_button)=="no_response"] <- "No Response"
  names(pid_SAAT_session2)[names(pid_SAAT_session2)=="correct_button"]  <- "Button"
  names(pid_SAAT_session2)[names(pid_SAAT_session2)=="condition"]  <- "Condition"
  levels(pid_SAAT_session2$Condition)[levels(pid_SAAT_session2$Condition)=="impulsive"] <- "Impulsive"
  levels(pid_SAAT_session2$Condition)[levels(pid_SAAT_session2$Condition)=="sustained"] <- "Sustained"
  pid_SAAT_session2$session <- 2
  pid_SAAT_session2$session <- as.factor(pid_SAAT_session2$session)
  
  dframe = rbind(pid_SAAT_session1, pid_SAAT_session2)
  
  dframe <- subset(dframe, Condition == attention)
  
  savename = paste("~/Documents/CMI/bks_ace_analysis/results/SAAT_", p_id, "_", attention, ".pdf", sep="")
  
  pdf(savename)
  print(ggplot(dframe, aes(x=factor(Button), fill=factor(session), y=dframe$rt)) +
          
          scale_y_continuous(limits = c(0,700)) +
          stat_summary(fun.data=mean_sdl, width=0.5, fun.args = list(mult=1),
                       geom="errorbar", position=position_dodge(width=1), color="grey70") +
          stat_summary(fun.y=mean, geom="point", position=position_dodge(width=1), size=2.7, alpha = 0.8, aes(color=session)) +
          geom_point(aes(group=factor(session), colour = factor(session), x=Button, y=dframe$rt),
                     position= position_dodge(width=1), alpha= 0.4, size=1.2) +
          
          theme_classic()+
          theme(axis.title.x = element_text(face="bold", size=16),
                axis.title.y = element_text(face="bold", size=16),
                axis.text.x  = element_text(size=16, angle=65, vjust=0.6),
                axis.text.y  = element_text(size=16),
                plot.title   = element_text(vjust=2, hjust=0.5, face="bold", size=20),
                legend.title = element_text(size=16, face="bold"))+
          labs(title=paste("BKS Training Results for SAAT ", attention, sep=""),
               x="Button Press Condition",
               y="Response Time") +
          scale_color_discrete(name="Session")+
          scale_fill_discrete(name="", guide='none'))
  
  dev.off()
  
}

SAAT_RT('013', 'Impulsive')

##############################################################################################################################
#FLANKER Analysis




################################################################################################################

group_level_reproducibility <- read_csv("/Users/aki.nikolaidis/git_repo/BASC_Tech_paper/Data/similarity_50min/group_level_reproducibility.csv")

Data<-group_level_reproducibility
datavar=Data$gsm_repref_corr
Data$bootstrap<- as.factor(Data$bootstrap)
Data$scantime <- as.factor(Data$scantime)
Data$scantime <- factor(Data$scantime,levels(Data$scantime)[c(4,5,7,3,6,1,9,8,2)])

levels(Data$bootstrap)[levels(Data$bootstrap)=="1"] <- "0"


levels(Data$scantime)[levels(Data$scantime)=="halfmintest"] <- "Half Minute"
levels(Data$scantime)[levels(Data$scantime)=="onemintest"] <- "One Minute"
levels(Data$scantime)[levels(Data$scantime)=="threemintest"] <- "Three Minutes"
levels(Data$scantime)[levels(Data$scantime)=="fivemintest"] <- "Five Minutes"
levels(Data$scantime)[levels(Data$scantime)=="tenmintest"] <- "Ten Minutes"
levels(Data$scantime)[levels(Data$scantime)=="fifteenmintest"] <- "Fifteen Minutes"
levels(Data$scantime)[levels(Data$scantime)=="twentymintest"] <- "Twenty Minutes"
levels(Data$scantime)[levels(Data$scantime)=="twentyfivemintest"] <- "Twenty Five Minutes"
levels(Data$scantime)[levels(Data$scantime)=="fiftymintest"] <- "Fifty Minutes"

names(Data)[names(Data)=="scantime"]  <- "Time"

pdf("Group_Spatial_Correlation_Reliability.pdf")
ggplot(Data, aes(x=factor(bootstrap), fill=factor(Time), y=datavar)) +
  #CONTENTS
  scale_y_continuous(limits = c(0,1)) +
  stat_summary(fun.data=mean_sdl, width=0.5, fun.args = list(mult=1),
               geom="errorbar", position=position_dodge(width=1), color="grey70") +
  stat_summary(fun.y=mean, geom="point", position=position_dodge(width=1), size=2.7, alpha = 0.8, aes(color=Time)) +
  geom_point(aes(group=factor(Time), colour = factor(Time), x=bootstrap, y=datavar),
             position= position_dodge(width=1), alpha= 0.4, size=1.2) +
  #LAYOUT
  theme_classic()+
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.title.y = element_text(face="bold", size=16),
        axis.text.x  = element_text(size=16, angle=65, vjust=0.6),
        axis.text.y  = element_text(size=16),
        plot.title   = element_text(vjust=2, hjust=0.5, face="bold", size=20),
        legend.title = element_text(size=16, face="bold"))+
  labs(title="Group Test-Retest Reliability",
       x="Bootstrap Aggregates",
       y="Spatial Correlation") +
  scale_color_discrete(name="Scan Time")+
  scale_fill_discrete(name="", guide='none')
dev.off()