######JVP Reproducibility assessment script
######Authors: Daniel & Pedro
######February, 2026

#libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(ggpubr)

#variable names
labels<-c("Can we run the analyses with the submitted files/info?",
		  "Were all the necessary files submitted?",
		  "Were character and state names included in the matrix?",
		  "Was enough information provided about the search procedure?",
		  "Were the employed parameters adequate for the analysis?",
		  "Did we obtain the same strict consensus topology?",
		  "Did we obtain the same number of MPTs?",
		  "Did the authors provide the MPTs of their analyses?")


#read and reshape data
dat<-read.csv("JVP_data.csv", row.names = 1, sep=";")
colnames(dat)[-c(1:3)]<-labels
dat2<-dat[,-c(1:3)]
numbers<-data.frame(number=rownames(dat2))
sep<-separate(numbers,"number",c("number","round"),sep="\\.")
sep[is.na(sep$round),]$round<-"R0"
dat2x<-cbind(sep$round,dat2)

##PLOT_1 
	#all mansucripts
	dat3 <- dat2 %>%
	pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
	group_by(Variable, Value) %>%
	summarise(Count = n(), .groups = "drop") %>%
	group_by(Variable) %>%
	mutate(Proportion = Count / sum(Count))
	dat3$Variable <- factor(dat3$Variable, levels = labels)
	dat3$Value<-rep(c("Yes","No"),length(dat3$Value)/2)
	
	#initial
	dat4 <- dat2x[dat2x$`sep$round`=="R0",][,-1] %>%
	pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
	group_by(Variable, Value) %>%
	summarise(Count = n(), .groups = "drop") %>%
	group_by(Variable) %>%
	mutate(Proportion = Count / sum(Count))
	dat4$Variable <- factor(dat4$Variable, levels = labels)
	dat4$Value<-rep(c("Yes","No"),length(dat4$Value)/2)
	
	#resubs
	dat5 <- dat2x[dat2x$`sep$round`!="R0",][,-1] %>%
	pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
	group_by(Variable, Value) %>%
	summarise(Count = n(), .groups = "drop") %>%
	group_by(Variable) %>%
	mutate(Proportion = Count / sum(Count))
	dat5$Variable <- factor(dat5$Variable, levels = labels)
	dat5$Value<-rep(c("Yes","No"),length(dat5$Value)/2)
	
	#prepare to plot
	N<-c(sum(dat3[1:2,]$Count),sum(dat4[1:2,]$Count),sum(dat5[1:2,]$Count))
	dat3 <- dat3 |> mutate(dataset = paste0("All submissions"," (N = ",N[1],")"))
	dat4 <- dat4 |> mutate(dataset = paste0("Initial submissions"," (N = ",N[2],")"))
	dat5 <- dat5 |> mutate(dataset = paste0("Resubmissions"," (N = ",N[3],")"))
	all_tabs <- bind_rows(dat3, dat4, dat5)
	
	#make plot
	ggplot(all_tabs, aes(y = Variable, x = Proportion, fill = as.factor(Value))) +
	geom_bar(stat = "identity", position = "stack", color = "white", width = 0.90) +
	geom_text(aes(label = Count), position = position_stack(vjust = 0.5), color = "black", size = 3) +
	scale_fill_manual(values = c("Yes" = "gray90", "No" = "#DE8181"), name = "Value") +
	theme_bw() +  labs(y = "", x = paste0("Proportion"), fill="") +
	theme(legend.title = element_blank(),legend.position="top", axis.text.x = element_text(angle = 0, hjust = 0.5, size=7),
	panel.spacing = unit(1, "lines")) +
 scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1), expand=FALSE) +
 scale_y_discrete(limits = rev(levels(factor(dat3$Variable)))) +
 guides(fill = guide_legend(reverse = TRUE)) + facet_wrap(~ dataset, nrow = 1)
 ggsave("JVP_barH.pdf", width=10, height=4.5)  	


##PLOT_2
	prov_dat<-separate(dat,Submission.date,into=c("D","M","Y"))
	prov_dat$M<-as.numeric(prov_dat$M)
	prov_dat$Y<-as.numeric(prov_dat$Y)
	prov_dat$S<-ifelse(prov_dat$M <= 6, 1, 2)
	prov_dat$T <- paste(prov_dat$Y, prov_dat$S, sep = "-")
	
	prov_long <- prov_dat %>%
	filter(T != "2022-1") %>%
	filter(T != "2026-1") %>%
	pivot_longer(cols = 6:13, names_to = "variable", values_to = "X")  
	prov_long$variable <- factor(prov_long$variable, levels = labels)
	
	ggplot(prov_long, aes(x = T, fill = factor(X))) +
	geom_bar(position = "fill") +
	facet_wrap(~ variable, ncol = 2, nrow = 4) +
	scale_y_continuous() +
	labs(x = "Year-Semester", y = "Proportion", fill = "Value") +
	theme_bw() +
	scale_fill_manual(values = c("0" = "gray90", "1" = "#DE8181"),  labels = c("0" = "Yes", "1" = "No"),  name = "")+
	theme(axis.text.x = element_text(angle = 45, hjust = 1))
	ggsave("JVP_time.pdf", width=9, height=6)  
