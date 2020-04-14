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

dat = dat[order_condition=="poorrich", ]

#split these up into two data frames - one for each block - only extracting the relevant fields
data_A = dat[, .(percent_accept_A1, percent_accept_A2, percent_accept_A3, percent_accept_A2_A3, percent_accept_A4, order_condition)]
data_B = dat[, .(percent_accept_B1, percent_accept_B2, percent_accept_B3,  percent_accept_B2_B3, percent_accept_B4, order_condition)]

#for block A, 
#convert to long format for plotting so have all acceptance rates in one column 
#and a second column indicating which options this applies to
data_A = melt(data_A, measure.vars = c("percent_accept_A1", "percent_accept_A2", "percent_accept_A3", "percent_accept_A4"), 
              variable.name = c("rank"), 
              value.name = c("percent_accept"))

#code the options as factors
data_A$rank = as.factor(data_A$rank)

#rename them to best, intermediate, worst
data_A$rank = revalue(data_A$rank, c("percent_accept_A1"="HRLD", "percent_accept_A2"="LRLD", "percent_accept_A3"="HRHD", "percent_accept_A4"="LRHD"))

data_A$block = as.factor(c(rep("rich",nrow(data_A))))

#now do the same for block B
data_B = melt(data_B, measure.vars = c("percent_accept_B1", "percent_accept_B2", "percent_accept_B3", "percent_accept_B4"), 
              variable.name = c("rank"), 
              value.name = c("percent_accept"))

data_B$rank = as.factor(data_B$rank)

#rename them to best, intermediate, worst
data_B$rank = revalue(data_B$rank, c("percent_accept_B1"="HRLD", "percent_accept_B2"="LRLD", "percent_accept_B3"="HRHD", "percent_accept_B4"="LRHD"))

data_B$block = as.factor(c(rep("poor",nrow(data_B))))

data_individual = merge(data_A, data_B, all=TRUE)

#now calculate the summary stats - means and sem - for each option for each block

#note that for the initial variable (as this table hasn't been created yet), 
#you have to do a long winded code to get the varaible name
data_summaryallA = data_A[, .(mean(percent_accept)), by=c("rank")][,c("acceptance_rates"):=.(V1)][,c("V1"):=NULL]
data_summaryallA$sem = data_A[, .(sd(percent_accept)/sqrt(.N)), by=c("rank")][,V1]
data_summaryallA$block = as.factor(c(rep("rich",nrow(data_summaryallA))))
data_summaryallA$order_condition = as.factor(c(rep("all",nrow(data_summaryallA))))

data_summaryallB = data_B[, .(mean(percent_accept)), by=c("rank")][,c("acceptance_rates"):=.(V1)][,c("V1"):=NULL]
data_summaryallB$sem = data_B[, .(sd(percent_accept)/sqrt(.N)), by=c("rank")][,V1]
data_summaryallB$block = as.factor(c(rep("poor", nrow(data_summaryallB))))
data_summaryallB$order_condition = as.factor(c(rep("all",nrow(data_summaryallB))))

#combine the summary stats into one
data_summary = merge(data_summaryallA, data_summaryallB, all=TRUE)

#order by block then by rank
setkey(data_summary, block, rank)

#get rid of various data frames to tidy up
rm("data_summaryallA", "data_summaryallB")

if (exp==1){
  title_text = "RichPoor Participants: Experiment 1"
} else if (exp==2){
  title_text = "RichPoor Participants: Experiment 2"
}

#plot
p1 <- ggplot(data = data_summary, aes(x = rank, y = acceptance_rates, fill=block)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = data_summary, aes(ymin = acceptance_rates-sem, ymax = acceptance_rates+sem), position = position_dodge(width = .5), width=0.25) + 
  geom_point(data = data_individual, aes(x = rank, y = percent_accept, colour = block), stroke = 1, colour = "black", alpha = 0.2, size = 1.5, pch=21, position = position_jitterdodge(dodge.width=0.5)) +
  scale_color_manual(values = c('rich' = 'blue', 'poor' = 'red'))+
  ylab("accept %") +
  xlab("option") +
  theme_cowplot()+
  theme(legend.position="none")+
  #ggtitle(title_text)+
  scale_fill_brewer(palette="Set1", direction=-1)+
  scale_y_continuous(limits = c(0, 1))+
  theme(axis.text=element_text(size=15), axis.title=element_text(size=15, face="bold"), plot.title = element_text(size = 10, face = "bold"))

p1
