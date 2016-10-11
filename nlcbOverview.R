##############################
### Figurer til mandagsmøtet
##############################

## LIBRARY
library(lubridate)
library(ggplot2)
########

## DATA
df <- read.table("/Users/anne_heidi/PhD_mac/lungcancer/data/TEMP_nlcbPlusSpss.txt", header =TRUE, stringsAsFactors = FALSE)
########

## RESULTS
mainDir <- "/Users/anne_heidi/PhD_mac/lungcancer/results/figs/"
subDir <- "overview/temp/"
path <- as.character(paste0(mainDir, subDir))

ggsave(paste0(path, "stadium", ".png"), Stadium, width=2, height=2)
ggsave(paste0(path, "status", ".png"), status, width=2, height=2)
ggsave(paste0(path, "ageVSinc", ".png"), Age_inc, width=2, height=2)
ggsave(paste0(path, "start", ".png"), start, width=2, height=2)
ggsave(paste0(path, "stop", ".png"), stop, width=2, height=2)
ggsave(paste0(path, "time", ".png"), time, width=2, height=2)
ggsave(paste0(path, "pack", ".png"), pack, width=3, height=2)
ggsave(paste0(path, "bornVSinc", ".png"), bornInc, width=2, height=2)
ggsave(paste0(path, "noCigVStime", ".png"), noCigVStime, width=2, height=2)
ggsave(paste0(path, "stadiumVStime", ".png"), stadiumVStime, width=3, height=2)
ggsave(paste0(path, "startVSpackY", ".png"), start_py, width=3, height=2)
ggsave(paste0(path, "subsVStime", ".png"), subsVStime, width=3, height=3)
ggsave(paste0(path, "mainVStime", ".png"), mainVStime, width=2, height=2)
ggsave(paste0(path, "subsVSage", ".png"), subsAge, width=3, height=3)
ggsave(paste0(path, "main", ".png"), main, width=2, height=2)
ggsave(paste0(path, "subs", ".png"), subs, width=4, height=3)
ggsave(paste0(path, "subsVSstatus", ".png"), subsVSstatus, width=4, height=3)
ggsave(paste0(path, "stopVSinc", ".png"), stopInc, width=2, height=2)
ggsave(paste0(path, "startY", ".png"), startY, width=2, height=2)
ggsave(paste0(path, "statusVStime", ".png"), statusVStime, width=2, height=2)
ggsave(paste0(path, "subsVSduration", ".png"), subsVSduration, width=3, height=3)
ggsave(paste0(path, "mainVSduration", ".png"), mainVSduration, width=2, height=2)
ggsave(paste0(path, "stadiumVSduration", ".png"), stadiumVSduration, width=3, height=2)
ggsave(paste0(path, "duration", ".png"), duration, width=2, height=2)
########

## Dataprep:
df$born <- as.Date(df$born)
df$inclusion <- as.Date(df$inclusion)
df$died <- as.Date(df$died)

# Får ikke til å regne ut år fra days i R så jeg gjør det enkelt
df$age_inclusion <- as.numeric(df$inclusion - df$born)
df$age_incl <- as.numeric(df$age_inclusion/365.2598)
df$age_death <- as.numeric(df$died - df$born)
df$age_d <- as.numeric(df$age_death/365.2598)
df$time <- as.numeric(df$age_death - df$age_inclusion)
df$years <- as.numeric(df$time/365.2598)

df$gender <- as.factor(df$gender)
df$histology <- as.factor(df$histology)
df$smoking_status <- as.factor(df$smoking_status)

df$Stadium <- gsub("3b", "IIIB", df$Stadium)
df$Stadium[is.na(df$Stadium)]<-"NA"
df$Stadium <- as.factor(df$Stadium)

df$histology <- gsub("Adeno_Ca", "AdenoCarcinoma", df$histology)
df$histology <- gsub("Squamous_Cell_Ca", "SquamousCellCarcinoma", df$histology)
df$histology <- gsub("Mixed_SCLC/NSCLC", "mixed", df$histology)
df$histology <- gsub("Undiff_Ca", "LargeCellCarcinoma", df$histology)
df$histology <- gsub("Large_Cell_NEC", "LargeCellNeuroendocrine", df$histology)
df$histology <- gsub("_", "Unknown", df$histology)
df$histology <- gsub("NotUnknownconfirmed", "Unknown", df$histology)

df[grep("SCLC", df$histology), "NSCLCvsSCLC"] <- "SCLC"
df[grep("mixed", df$histology), "NSCLCvsSCLC"] <- "mixed"
df[df$histology != "SCLC", "NSCLCvsSCLC"] <- "NSCLC"


df2 <- df[df$smoking_status == "Earlier_smoker",]
df2$age_incl2 <- signif(df2$age_incl, digits=2)
ifelse(df2$age_incl2 == df2$smoke_stop, "yes", "NO") #OK
summary(ifelse(df2$age_incl2 == df2$smoke_stop, "yes", "NO") == "yes")

df$age_incl2 <- signif(df$age_incl, digits=2)
ifelse(df$age_incl2 == df$smoke_stop, "yes", "NO") #OK
summary(ifelse(df$age_incl2 == df$smoke_stop, "yes", "NO") == "yes")






df$pack_year_lvl[is.na(df$pack_year_lvl)]<-"NA"




ggplot(df, aes(Stadium)) + geom_bar(aes(fill=gender))

Stadium <- ggplot(df) + geom_bar(aes(Stadium, fill=gender)) +
				theme_bw() +
				labs(fill="Gender") +
				scale_fill_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(fill = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 

status <- ggplot(df) + geom_bar(aes(smoking_status, fill=gender)) +
				theme_bw() +
				labs(x="Smoking status", fill="Gender") +
				scale_fill_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(fill = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 			    

Age_inc <- ggplot(df) + geom_density(aes(age_incl, fill=gender), alpha=0.5) +
				theme_bw() +
				labs(x="Age at diagnosis", fill="Gender") +
				scale_fill_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(fill = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 

start <- ggplot(df) + geom_density(aes(smoke_start, fill=gender), alpha=0.5) +
				theme_bw() +
				labs(x="Smoke start", fill="Gender") +
				scale_fill_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(fill = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 

stop <- ggplot(df) + geom_density(aes(smoke_stop, fill=gender), alpha=0.5) +
				theme_bw() +
				labs(x="Smoke stop", fill="Gender") +
				scale_fill_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(fill = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 

time <- ggplot(df) + geom_density(aes(years, fill=gender), alpha=0.5) +
				theme_bw() +
				labs(x="Y", fill="Gender") +
				scale_fill_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(fill = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 


pack <- ggplot(df) + geom_bar(aes(pack_year_lvl, fill=gender), alpha=0.5) +
				theme_bw() +
				labs(x="Pack_years", fill="Gender") +
				scale_fill_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(fill = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 

bornInc <- ggplot(df) + geom_point(aes(born, age_incl, color=gender), alpha=0.5) +
				theme_bw() +
				labs(x="Born", y="Age diagnosed", color="Gender") +
				scale_color_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(color = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 

noCigVStime <- ggplot(df) + geom_point(aes(no_of_cigarettes, years, color=gender), alpha=0.5) +
				theme_bw() +
				#labs(x="", color="Gender") +
				scale_color_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(color = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 

stadiumVStime <- ggplot(df) + geom_boxplot(aes(Stadium, years, color=gender), alpha=0.5) +
				theme_bw() +
				labs(y="years", color="Gender") +
				scale_color_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(color = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 


start_py <- ggplot(df) + geom_boxplot(aes(pack_year_lvl,smoke_start, color=gender), alpha=0.5) +
				theme_bw() +
				labs(x="Pack years", color="Gender") +
				scale_color_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(color = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8, angle=45,hjust=1), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 


subsVStime <- ggplot(df) + geom_boxplot(aes(histology, years, color=gender), alpha=0.5) +
				theme_bw() +
				labs(x="Subtypes of lung cancer", color="Gender") +
				scale_color_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(color = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 

mainVStime <- ggplot(df) + geom_boxplot(aes(NSCLCvsSCLC, years, color=gender), alpha=0.5) +
				theme_bw() +
				labs(x="Main types", color="Gender") +
				scale_color_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(color = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 

statusVStime <- ggplot(df) + geom_boxplot(aes(smoking_status, years, color=gender), alpha=0.5) +
				theme_bw() +
				labs(x="Smoking status", color="Gender") +
				scale_color_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(color = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 


subsAge <- ggplot(df) + geom_boxplot(aes(histology, age_d, color=gender), alpha=0.5) +
				theme_bw() +
				labs(x="Subtypes of lung cancer", color="Gender") +
				scale_color_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(color = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 				


main <- ggplot(df) + geom_bar(aes(NSCLCvsSCLC, fill=gender), alpha=0.5) +
				theme_bw() +
				labs(x="main lung cancer types", fill="Gender") +
				scale_fill_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(fill = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8),
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 


subs <- ggplot(df) + geom_bar(aes(histology, fill=gender), alpha=0.5) +
				theme_bw() +
				labs(x="Lung cancer sub type", fill="Gender") +
				scale_fill_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(color = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 


subsVSstatus <- ggplot(df) + geom_bar(aes(histology, fill=gender), alpha=0.5) +
				theme_bw() +
				labs(x="Lung cancer sub type", fill="Gender") +
				facet_wrap(~smoking_status) +
				scale_fill_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(color = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 
# ggplot(df) + geom_point(aes(smoke_stop, smoke_start, color=gender), alpha=0.5) +
# 				theme_bw() +
# 				#labs(x="Born", color="Gender") +
# 				scale_color_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
# 				guides(color = guide_legend(reverse=TRUE)) +
# 				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8), 
# 			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
# 			    legend.title =  element_text(size=8),
# 			    legend.text = element_text(size=8),
# 			    legend.margin = unit(0.1, "cm"),
# 			    legend.key = element_rect(fill = NULL, colour = "white"), 
# 			    legend.key.size=unit(0.8, "line")) 

stopInc <- ggplot(df) + geom_point(aes(smoke_stop, age_incl, color=gender), alpha=0.5) +
				theme_bw() +
				#labs(x="", color="Gender") +
				scale_color_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(color = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 

startY <- ggplot(df) + geom_point(aes(smoke_start, years, color=gender), alpha=0.5) +
				theme_bw() +
				#labs(x="", color="Gender") +
				scale_color_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(color = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 

duration <- ggplot(df) + geom_bar(aes(duration, fill=gender), alpha=0.5) +
				theme_bw() +
				labs(x="No. of years smoking", fill="Gender") +
				scale_fill_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(color = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 


mainVSduration <- ggplot(df) + geom_boxplot(aes(NSCLCvsSCLC, duration, color=gender), alpha=0.5) +
				theme_bw() +
				labs(x="", color="Gender") +
				scale_color_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(color = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 

subsVSduration <- ggplot(df) + geom_boxplot(aes(histology, duration, color=gender), alpha=0.5) +
				theme_bw() +
				labs(x="", color="Gender") +
				scale_color_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(color = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 

stadiumVSduration <- ggplot(df) + geom_boxplot(aes(Stadium, duration, color=gender), alpha=0.5) +
				theme_bw() +
				labs(x="", color="Gender") +
				scale_color_manual(values=c("#E76BF3", "#00B0F6", "#00BA38"))+
				guides(color = guide_legend(reverse=TRUE)) +
				theme(axis.text.y=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1), 
			    axis.title.y=element_text(size=8), axis.title.x=element_text(size=8),
			    legend.title =  element_text(size=8),
			    legend.text = element_text(size=8),
			    legend.margin = unit(0.1, "cm"),
			    legend.key = element_rect(fill = NULL, colour = "white"), 
			    legend.key.size=unit(0.8, "line")) 


