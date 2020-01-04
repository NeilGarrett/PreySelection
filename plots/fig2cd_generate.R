#this code to plot out acceptance rates
#N.Garrett, sept 2018

#Clear workspace
rm(list = ls())

#Get rid of exisiting plots
dev.off()

#Load packages - may need to install if not already
library(ggplot2)
library(data.table)
library(cowplot)
library(Hmisc)
library(plyr)

exp = 1 #change this according to which plot you want to do 

#load in data
if (exp==1){
  dat = fread("/Users/neil/GitHubRepo/Projects/PreySelection/v103/data/acceptance_rates_103.csv")
} else if (exp ==2){
  dat = fread("/Users/neil/GitHubRepo/Projects/PreySelection/v104/data/acceptance_rates_104.csv")
}

data_individual = melt(dat[, .(best = accept_AB_prev1, intermediate = accept_AB_prev23, worst = accept_AB_prev4)], 
                       measure.vars = c("best", "intermediate", "worst"), 
              variable.name = c("previous_option"), 
              value.name = c("acceptance_rate"))


#get summary stats for within block effects
within_AB_means = melt(dat[, .(best = mean(accept_AB_prev1), intermediate = mean(accept_AB_prev23), worst = mean(accept_AB_prev4))],
                       measure.vars = c("best", "intermediate", "worst"),
                       variable.name = c("previous_option"), 
                       value.name = c("acceptance_rate"))

#within_AB_means = dat[, .(best = mean(accept_AB_prev1), intermediate = mean(accept_AB_prev23), worst = mean(accept_AB_prev4))]

within_AB_sem = melt(dat[, .(best = sd(accept_AB_prev1)/(sqrt(.N)), intermediate = sd(accept_AB_prev23)/(sqrt(.N)), worst = sd(accept_AB_prev4)/(sqrt(.N)))],
                       measure.vars = c("best", "intermediate", "worst"),
                       variable.name = c("previous_option"), 
                       value.name = c("sem"))

within_AB = merge(within_AB_means, within_AB_sem, all=TRUE)

if (exp==1){
  title_text = "Within Block Effects: Experiment 1"
} else if (exp==2){
  title_text = "Within Block Effects: Experiment 2"
}

p1 <- ggplot(data = within_AB, aes(x = previous_option, y = acceptance_rate)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = within_AB, aes(ymin = acceptance_rate-sem, ymax = acceptance_rate+sem), position = position_dodge(width = .5), width=0.25) + 
  geom_point(data = data_individual, aes(x = previous_option, y = acceptance_rate, fill="grey"), alpha = 0.2, size = 4, position = position_jitterdodge(dodge.width=0.5)) +
  ylab("accept % \n (trial t)") +
  xlab("option \n (trial t-1)") +
  theme_cowplot()+
  ggtitle(title_text)+
  scale_fill_brewer(palette="Set1", direction=-1)+
  ylim(0.0,1.0)+ 
  theme(legend.position="none")+
  theme(axis.text=element_text(size=15), axis.title=element_text(size=15, face="bold"), plot.title = element_text(size = 10, face = "bold"))

p1
