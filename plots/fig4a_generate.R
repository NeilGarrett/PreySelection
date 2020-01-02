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

#load in data
#block=0=rich
#block=1=poor
dat = fread("/Users/neil/GitHubRepo/Projects/PreySelection/v103/models/model_asymmetric/reward_rate_extract.csv")

dat$block = as.factor(dat$block)
dat[block==0, ]$block = "rich"
dat[block==1, ]$block = "poor"

dat$order_condition = as.factor(NA)

subs = unique(dat$sub)

for (i in 1:length(subs)){
  
  dat_tmp = dat[sub == subs[i], ]
  
  if (dat_tmp[1, block]=="rich"){
    dat[sub == subs[i], order_condition := "richpoor"]
  } else if (dat_tmp[1, block]=="poor"){
    dat[sub == subs[i], order_condition := "poorrich"]
  }
  
}

#calculate means for condition and environment
data_individual = dat[, mean(reward_rate), by = .(block, order_condition, sub)]
data_summary = data_individual[, .(means = mean(V1), sem = sd(V1)/sqrt(.N)), by=.(order_condition, block)]

#reorder factors
data_individual$block = relevel(data_summary$block, "rich")
data_individual$order_condition = relevel(data_summary$order_condition, "richpoor")

data_summary$block = relevel(data_summary$block, "rich")
data_summary$order_condition = relevel(data_summary$order_condition, "richpoor")

#plot
p1 <- ggplot(data = data_summary, aes(x = order_condition, y = means, fill=block)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = data_summary, aes(ymin = means-sem, ymax = means+sem), position = position_dodge(width = .5), width=0.25)+
  geom_point(data = data_individual, aes(x = order_condition, y = V1, fill=block), alpha = 0.2, size = 3, position = position_dodge(width=0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 60))+
  ylab("Reward Rate Estimate \n (Asymmetric Model)") +
  xlab("order condition") +
  theme_cowplot()+
  theme(legend.position="none")+
  ggtitle("reward rate estimate \n (time of choice)")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  theme(axis.text=element_text(size=15), axis.title=element_text(size=15, face="bold"), plot.title = element_text(size = 15, face = "bold"))

p1

