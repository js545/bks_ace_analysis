#OHBM Bootstrap Testing
library(readr)
library(ggplot2)


#GROUP LEVEL SPATIAL CORRELATION
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