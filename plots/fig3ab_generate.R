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

#code order_condition as factor
dat$order_condition = as.factor(dat$order_condition)  

#label these
levels(dat$order_condition) <- c("richpoor", "poorrich")

dat$acceptance_change = (dat$percent_accept_B1_min_A1+dat$percent_accept_B2_min_A2+dat$percent_accept_B3_min_A3+dat$percent_accept_B4_min_A4)/4

dat[order_condition=="richpoor", mean(acceptance_change)]
dat[order_condition=="poorrich", mean(acceptance_change)]

#combine the summary stats into one
data_summary = dat[, .(means = mean(acceptance_change), sem = sd(acceptance_change)/.N), by=order_condition]


if (exp==1){
  title_text = "Order Effect: Experiment 1"
  min_y = -0.2
  max_y = 0.4
} else if (exp==2){
  title_text = "Order Effect: Experiment 2"
  min_y = -0.3
  max_y = 0.6
}

#plot
p1 <- ggplot(data = data_summary, aes(x = order_condition, y = means)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = data_summary, aes(ymin = means-sem, ymax = means+sem), position = position_dodge(width = .5), width=0.25) + 
  geom_jitter(data = dat, aes(x = order_condition, y = acceptance_change), alpha = 0.2, size = 3, width=0.075) +
  ylab("Change in acceptance rates \n (Poor - Rich ") +
  theme_cowplot()+
  theme(legend.position="none")+
  ggtitle(title_text)+
  ylim(min_y, max_y)+
  scale_fill_brewer(palette="Set1", direction=-1)+
  theme(axis.text=element_text(size=8), axis.title=element_text(size=15, face="bold"), plot.title = element_text(size = 10, face = "bold"))

p1

