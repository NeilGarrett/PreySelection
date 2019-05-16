#this code to plot out data plus simulations for 1lr and 2lr models
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

# here put path to  the data you want to generate plots for 
# data (needs to have group assigments in 1st column labelled "group" 
#and individual observations in 2nd column labelled "score")

model_folder = c("v103")

#load in data
dat = fread(paste("~/GitHubRepo/Projects/PreySelection/", model_folder, "/models/model_asymmetry/summary_stats2.csv", sep =""))

#path and filename for saving
output_path = paste("~/GitHubRepo/Projects/PreySelection/plots_",model_folder, ".jpg", sep="")

#code order_condition as factor
dat$order_condition = as.factor(dat$order_condition)  

#add LRLD vs HRHD preference
dat$LRLD_vs_HRHD = dat$percent_accept_AB2 - dat$percent_accept_AB3

#add lr difference
dat$eta_r_vs_eta_d = dat$learning_rate_reward_raw - dat$learning_rate_delay_raw

dat = melt(dat, measure.vars = c("av_reward_est_A", "av_reward_est_B"),
    variable.name = c("av_reward_block"),
     value.name = c("av_reward"))

#label these
levels(dat$order_condition) <- c("richpoor", "poorrich")

#split these up into two data frames - one for each block - only extracting the relevant fields
data_A = dat[, .(percent_accept_A1, percent_accept_A2, percent_accept_A3, percent_accept_A2_A3, percent_accept_A4, order_condition)]
data_B = dat[, .(percent_accept_B1, percent_accept_B2, percent_accept_B3,  percent_accept_B2_B3, percent_accept_B4, order_condition)]

#for block A, 
#convert to long format for plotting so have all acceptance rates in one column 
#and a second column indicating which options this applies to
data_A = melt(data_A, measure.vars = c("percent_accept_A1", "percent_accept_A2", "percent_accept_A3", "percent_accept_A2_A3", "percent_accept_A4"), 
                 variable.name = c("rank"), 
                 value.name = c("percent_accept"))

#code the options as factors
data_A$rank = as.factor(data_A$rank)

#rename them to best, intermediate, worst
data_A$rank = revalue(data_A$rank, c("percent_accept_A1"="HRLD", "percent_accept_A2"="LRLD", "percent_accept_A3"="HRHD", "percent_accept_A2_A3"="intermediate", "percent_accept_A4"="LRHD"))

data_A$block = as.factor(c(rep("rich",nrow(data_A))))
  
#now do the same for block B
data_B = melt(data_B, measure.vars = c("percent_accept_B1", "percent_accept_B2", "percent_accept_B3", "percent_accept_B2_B3", "percent_accept_B4"), 
              variable.name = c("rank"), 
              value.name = c("percent_accept"))

data_B$rank = as.factor(data_B$rank)

#rename them to best, intermediate, worst
data_B$rank = revalue(data_B$rank, c("percent_accept_B1"="HRLD", "percent_accept_B2"="LRLD", "percent_accept_B3"="HRHD", "percent_accept_B2_B3"="intermediate", "percent_accept_B4"="LRHD"))

data_B$block = as.factor(c(rep("poor",nrow(data_B))))

data_individual = merge(data_A, data_B, all=TRUE)

#now calculate the summary stats - means and sem - for each option for each block

#note that for the initial variable (as this table hasn't been created yet), 
#you have to do a long winded code to get the varaible name
data_summaryallA = data_A[, .(mean(percent_accept)), by=c("rank")][,c("acceptance_rates"):=.(V1)][,c("V1"):=NULL]
data_summaryallA$sem = data_A[, .(sd(percent_accept)/sqrt(.N)), by=c("rank")][,V1]
data_summaryallA$block = as.factor(c(rep("rich",nrow(data_summaryallA))))
data_summaryallA$order_condition = as.factor(c(rep("all",nrow(data_summaryallA))))

data_summaryorderA = data_A[, .(mean(percent_accept)), by=c("rank","order_condition")][,c("acceptance_rates"):=.(V1)][,c("V1"):=NULL]
data_summaryorderA$sem = data_A[, .(sd(percent_accept)/sqrt(.N)), by=c("rank","order_condition")][,V1]
data_summaryorderA$block = as.factor(c(rep("rich",nrow(data_summaryorderA))))

data_summaryallB = data_B[, .(mean(percent_accept)), by=c("rank")][,c("acceptance_rates"):=.(V1)][,c("V1"):=NULL]
data_summaryallB$sem = data_B[, .(sd(percent_accept)/sqrt(.N)), by=c("rank")][,V1]
data_summaryallB$block = as.factor(c(rep("poor", nrow(data_summaryallB))))
data_summaryallB$order_condition = as.factor(c(rep("all",nrow(data_summaryallB))))

data_summaryorderB = data_B[, .(mean(percent_accept)), by=c("rank","order_condition")][,c("acceptance_rates"):=.(V1)][,c("V1"):=NULL]
data_summaryorderB$sem = data_B[, .(sd(percent_accept)/sqrt(.N)), by=c("rank","order_condition")][,V1]
data_summaryorderB$block = as.factor(c(rep("poor",nrow(data_summaryorderB))))

#combine the summary stats into one
data_summaryall = merge(data_summaryallA, data_summaryallB, all=TRUE)

#combine the summary stats into one
data_summaryorder = merge(data_summaryorderA, data_summaryorderB, all=TRUE)

data_summary = merge(data_summaryall, data_summaryorder, all=TRUE)
  
#order by block then by rank
setkey(data_summary, block, rank)

#get rid of various data frames to tidy up
rm("data_summaryall", "data_summaryorder", "data_summaryallA", "data_summaryallB", "data_summaryorderA", "data_summaryorderB")

#plot 1 - acceptance rates for each option in each environment (bar plot with error bars) over all subs, intermediate options combined
p1a <- ggplot(data = data_summary[order_condition=="all" & rank != "HRHD" & rank != "LRLD"], aes(x = rank, y = acceptance_rates, fill=block)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = data_summary[order_condition=="all" & rank != "HRHD" & rank != "LRLD"], aes(ymin = acceptance_rates-sem, ymax = acceptance_rates+sem), position = position_dodge(width = .5), width=0.25) + 
  ylab("accept %") +
  xlab("option") +
  theme_cowplot()+
  ggtitle("All participants")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))

#plot 1b - as 1a but with indie data points
p1b <- ggplot(data = data_summary[order_condition=="all" & rank != "HRHD" & rank != "LRLD"], aes(x = rank, y = acceptance_rates, fill=block)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = data_summary[order_condition=="all" & rank != "HRHD" & rank != "LRLD"], aes(ymin = acceptance_rates-sem, ymax = acceptance_rates+sem), position = position_dodge(width = .5), width=0.25) + 
  geom_point(data = data_individual[rank != "HRHD" & rank != "LRLD"], aes(x = rank, y = percent_accept, colour = block), alpha = 0.4, position = position_dodge(width = .5), size = .75) +
  ylab("accept %") +
  xlab("option") +
  theme_cowplot()+
  ggtitle("All participants")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))

p1b

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point() +
  scale_color_manual(values = rev(cols))

#plot 1c - acceptance rates for each option in each environment (bar plot with error bars) over all subs, intermediate options seperate
p1c <- ggplot(data = data_summary[order_condition=="all" & rank != "intermediate"], aes(x = rank, y = acceptance_rates, fill=block)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = data_summary[order_condition=="all" & rank != "intermediate"], aes(ymin = acceptance_rates-sem, ymax = acceptance_rates+sem), position = position_dodge(width = .5), width=0.25) + 
  ylab("accept %") +
  xlab("option") +
  theme_cowplot()+
  ggtitle("All participants")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))

#plot 2a - acceptance rates for each option in each environment (bar plot with error bars) rich=>poor subs only, intermediate options combined
p2a <- ggplot(data = data_summary[order_condition=="richpoor" & rank != "HRHD" & rank != "LRLD"], aes(x = rank, y = acceptance_rates, fill=block)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = data_summary[order_condition=="richpoor" & rank != "HRHD" & rank != "LRLD"], aes(ymin = acceptance_rates-sem, ymax = acceptance_rates+sem), position = position_dodge(width = .5), width=0.25) + 
  ylab("accept %") +
  xlab("option") +
  theme_cowplot()+
  ggtitle("Rich=>Poor participants")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))

#plot 2b - as 2b but with indie data points
p2b <- ggplot(data = data_summary[order_condition=="richpoor" & rank != "HRHD" & rank != "LRLD"], aes(x = rank, y = acceptance_rates, fill=block)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = data_summary[order_condition=="richpoor" & rank != "HRHD" & rank != "LRLD"], aes(ymin = acceptance_rates-sem, ymax = acceptance_rates+sem), position = position_dodge(width = .5), width=0.25) + 
  geom_point(data = data_individual[order_condition=="richpoor" & rank != "HRHD" & rank != "LRLD"], aes(x = rank, y = percent_accept, fill=block), alpha = 0.4, position = position_dodge(width = .5), size = .25) +
  ylab("accept %") +
  xlab("option") +
  theme_cowplot()+
  ggtitle("Rich=>Poor participants")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))

#plot 2c - acceptance rates for each option in each environment (bar plot with error bars) rich=>poor subs only, intermediate options seperated out
p2c <- ggplot(data = data_summary[order_condition=="richpoor" & rank != "intermediate"], aes(x = rank, y = acceptance_rates, fill=block)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = data_summary[order_condition=="richpoor" & rank != "intermediate"], aes(ymin = acceptance_rates-sem, ymax = acceptance_rates+sem), position = position_dodge(width = .5), width=0.25) + 
  ylab("accept %") +
  xlab("option") +
  theme_cowplot()+
  ggtitle("Rich=>Poor participants")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))

#plot 3a - acceptance rates for each option in each environment (bar plot with error bars) poor=>rich subs only, intermediate options combined
p3a <- ggplot(data = data_summary[order_condition=="poorrich" & rank != "HRHD" & rank != "LRLD"], aes(x = rank, y = acceptance_rates, fill=block)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = data_summary[order_condition=="poorrich" & rank != "HRHD" & rank != "LRLD"], aes(ymin = acceptance_rates-sem, ymax = acceptance_rates+sem), position = position_dodge(width = .5), width=0.25) + 
  ylab("accept %") +
  xlab("option") +
  theme_cowplot()+
  ggtitle("Poor=>Rich participants")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))

#plot 3b - as 3b but with indie data points
p3b <- ggplot(data = data_summary[order_condition=="poorrich" & rank != "HRHD" & rank != "LRLD"], aes(x = rank, y = acceptance_rates, fill=block)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = data_summary[order_condition=="poorrich" & rank != "HRHD" & rank != "LRLD"], aes(ymin = acceptance_rates-sem, ymax = acceptance_rates+sem), position = position_dodge(width = .5), width=0.25) + 
  geom_point(data = data_individual[order_condition=="poorrich" & rank != "HRHD" & rank != "LRLD"], aes(x = rank, y = percent_accept, fill=block), alpha = 0.4, position = position_dodge(width = .5), size = .25) +
  ylab("accept %") +
  xlab("option") +
  theme_cowplot()+
  ggtitle("Poor=>Rich participants")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))

#plot 3c - acceptance rates for each option in each environment (bar plot with error bars) poor=>rich subs only, intermediate options seperated out
p3c <- ggplot(data = data_summary[order_condition=="poorrich" & rank != "intermediate"], aes(x = rank, y = acceptance_rates, fill=block)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = data_summary[order_condition=="poorrich" & rank != "intermediate"], aes(ymin = acceptance_rates-sem, ymax = acceptance_rates+sem), position = position_dodge(width = .5), width=0.25) + 
  ylab("accept %") +
  xlab("option") +
  theme_cowplot()+
  ggtitle("Poor=>Rich participants")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))

#plot correlation of learning asymetry with degree of preference for LRLD option over HRHD option
p4 <- ggplot(data = dat, aes(x=eta_r_vs_eta_d, y=LRLD_vs_HRHD)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("learning bias \n (α reward - α delay)") +
  ylab("intermediate option choice preference \n (LRLD  - HRHD)") +
  theme_cowplot()+
  #ggtitle("learning asymetry predicts preference for intermediate options")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))


av_reward_plot = dat[, .(means = mean(av_reward)), by=c("av_reward_block", "order_condition")]
av_reward_plot$sem = dat[,sd(av_reward)/sqrt(.N), by=c("av_reward_block", "order_condition")][,V1]

#rename levels 
levels(av_reward_plot$av_reward_block) = c("rich", "poor")
levels(av_reward_plot$order_condition) = c("RichPoor", "PoorRich")

#rename column
setnames(av_reward_plot, old=c("av_reward_block"), new=c("environment"))

p5 <- ggplot(data = av_reward_plot, aes(x=order_condition, y=means, fill=environment)) + 
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = av_reward_plot, aes(ymin = means-sem, ymax = means+sem), position = position_dodge(width = .5), width=0.25) + 
  ylab("Average Reward Estimate \n (2lr model)") +
  xlab("Order Condition") +
  theme_cowplot()+
  ggtitle("mean reward rate estimates \n (time of choice)")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  annotate("segment", x=c(0.9,0.9,1.1),xend=c(0.9,1.1,1.1), y= c(22.5,23,23), yend=c(23,23,22.5))+
  annotate("text",x=1,y=24,label="n.s.") + 
  annotate("segment", x=c(1.9,1.9,2.1),xend=c(1.9,2.1,2.1), y= c(20.5,21,21), yend=c(21,21,20.5))+
  annotate("text",x=2,y=22,label="*") + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))

#get summary stats for within block effects
#there ought to be a cleaner means of pulling out these numbers but anyway
within_A_means = melt(dat[, .(HRLD=mean(accept_A_prev1), intermediate = mean(accept_A_prev23), LRHD = mean(accept_A_prev4))], variable.name = c("previous_option"), value.name = c("acceptance_rate"))
within_A_sem = melt(dat[, .(HRLD=sd(accept_A_prev1)/(sqrt(.N)), intermediate=sd(accept_A_prev23)/(sqrt(.N)), LRHD=sd(accept_A_prev4)/(sqrt(.N))), ], variable.name = c("previous_option"), value.name = c("sem"))
within_A = merge(within_A_means, within_A_sem, all=TRUE)

within_B_means = melt(dat[, .(HRLD=mean(accept_B_prev1), intermediate = mean(accept_B_prev23), LRHD = mean(accept_B_prev4))], variable.name = c("previous_option"), value.name = c("acceptance_rate"))
within_B_sem = melt(dat[, .(HRLD=sd(accept_B_prev1)/(sqrt(.N)), intermediate=sd(accept_B_prev23)/(sqrt(.N)), LRHD=sd(accept_B_prev4)/(sqrt(.N))), ], variable.name = c("previous_option"), value.name = c("sem"))
within_B = merge(within_B_means, within_B_sem, all=TRUE)

within_AB_means = melt(dat[, .(HRLD=mean(accept_AB_prev1), intermediate = mean(accept_AB_prev23), LRHD = mean(accept_AB_prev4))], variable.name = c("previous_option"), value.name = c("acceptance_rate"))
within_AB_sem = melt(dat[, .(HRLD=sd(accept_AB_prev1)/(sqrt(.N)), intermediate=sd(accept_AB_prev23)/(sqrt(.N)), LRHD=sd(accept_AB_prev4)/(sqrt(.N))), ], variable.name = c("previous_option"), value.name = c("sem"))
within_AB = merge(within_AB_means, within_AB_sem, all=TRUE)

rm(within_A_means, within_A_sem, within_B_means, within_B_sem, within_AB_means, within_AB_sem)

within_A$environment = rep(c("rich"),3)
within_B$environment = rep(c("poor"),3)

within_combined = rbind(within_A, within_B)
within_combined$environment = as.factor(within_combined$environment)

within_combined$environment = factor(within_combined$environment,levels(within_combined$environment)[c(2,1)])

p6 <- ggplot(data = within_A, aes(x = previous_option, y = acceptance_rate)) +
  geom_bar(stat = "identity", width = .5, position="dodge", fill = "blue") +
  geom_errorbar(data = within_A, aes(ymin = acceptance_rate-sem, ymax = acceptance_rate+sem), position = position_dodge(width = .5), width=0.25) + 
  ylab("accept % trial t \n (all options)") +
  xlab("previous option trial t-1") +
  theme_cowplot()+
  ggtitle("Within Block Effects: \n Rich Enironment")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  ylim(0.0,1.0)+
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))

p7 <- ggplot(data = within_B, aes(x = previous_option, y = acceptance_rate)) +
  geom_bar(stat = "identity", width = .5, position="dodge", fill = "red") +
  geom_errorbar(data = within_B, aes(ymin = acceptance_rate-sem, ymax = acceptance_rate+sem), position = position_dodge(width = .5), width=0.25) + 
  ylab("accept % trial t \n (all options)") +
  xlab("previous option trial t-1") +
  theme_cowplot()+
  ggtitle("Within Block Effects: \n Poor Enironment")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  ylim(0.0,1.0)+ 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))

p8 <- ggplot(data = within_AB, aes(x = previous_option, y = acceptance_rate)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = within_AB, aes(ymin = acceptance_rate-sem, ymax = acceptance_rate+sem), position = position_dodge(width = .5), width=0.25) + 
  ylab("accept % trial t \n (all options)") +
  xlab("previous option trial t-1") +
  theme_cowplot()+
  ggtitle("Within Block Effects: \n Both Enironments combined")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  ylim(0.0,1.0)+ 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))

p8b <- ggplot(data = within_AB, aes(x = previous_option, y = acceptance_rate)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = within_AB, aes(ymin = acceptance_rate-sem, ymax = acceptance_rate+sem), position = position_dodge(width = .5), width=0.25) + 
  ylab("accept % trial t \n (all options)") +
  xlab("previous option trial t-1") +
  theme_cowplot()+
  ggtitle("Within Block Effects: \n Both Enironments combined")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  ylim(0.0,1.0)+ 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))


p9 <- ggplot(data = within_combined, aes(x = previous_option, y = acceptance_rate, fill=environment)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = within_combined, aes(ymin = acceptance_rate-sem, ymax = acceptance_rate+sem), position = position_dodge(width = .5), width=0.25) + 
  ylab("accept % trial t \n (all options)") +
  xlab("previous option trial t-1") +
  theme_cowplot()+
  ggtitle("Within Block Effects: \n Both Enironments combined")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  ylim(0.0,1.0)+ 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))

#load in simulation data for 2lr model
sim_1lr = fread(paste("~/GitHubRepo/Projects/PreySelection/", model_folder, "/models/model_subjective1beta1lr_delayreward/simulation_dat_1lr.csv", sep =""))
sim_2lr = fread(paste("~/GitHubRepo/Projects/PreySelection/", model_folder, "/models/model_subjective1beta2lr_delayreward/simulation_dat_2lr.csv", sep =""))

data_summary = merge(data_summary, sim_1lr, by=c("rank", "block", "order_condition"))
data_summary = merge(data_summary, sim_2lr, by=c("rank", "block", "order_condition"))

setnames(data_summary, old=c("acceptance_rates.x", "acceptance_rates.y", "acceptance_rates", "sem.x", "sem.y", "sem"), new=c("acceptance_rates_data", "acceptance_rates_sim1lr", "acceptance_rates_sim2lr", "sem_data", "sem_sim1lr", "sem_sim2lr"))

data_summary$rank = as.factor(data_summary$rank)
data_summary$block = as.factor(data_summary$block)
data_summary$order_condition = as.factor(data_summary$order_condition)

#order by block then by rank
setkey(data_summary, block, rank)

data_summary$block <- factor(data_summary$block, levels=rev(levels(data_summary$block)))

data_summary$rank = factor(data_summary$rank, levels(data_summary$rank)[c(2,5,1,3,4)])

#plot 10a - acceptance rates for each option in each environment (bar plot with error bars) with simulation fits
#rich=>poor subs only, intermediate options combined
p10a <- ggplot(data = data_summary[order_condition=="richpoor" & rank != "HRHD" & rank != "LRLD"], aes(x = rank, y = acceptance_rates_data, fill=block)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = data_summary[order_condition=="richpoor" & rank != "HRHD" & rank != "LRLD"], aes(ymin = acceptance_rates_data-sem_data, ymax = acceptance_rates_data+sem_data), position = position_dodge(width = .5), width=0.25) + 
  #geom_point(data = data_summary[order_condition=="richpoor" & rank != "HRHD" & rank != "LRLD"], aes(x = rank, y=acceptance_rates_sim1lr)) +
  #geom_point(data = data_summary[order_condition=="richpoor" & rank != "HRHD" & rank != "LRLD"], aes(x = rank, y=acceptance_rates_sim1lr), position = "identity", stat = "identity", fill="white", colour = "black", pch=21, size=4) +
  #geom_point(data = data_summary[order_condition=="richpoor" & rank != "HRHD" & rank != "LRLD"], aes(x = rank, y=acceptance_rates_sim1lr), stat = "identity", position = position_dodge(width = .1)) +
  #geom_pointrange(data = data_summary[order_condition=="richpoor" & rank != "HRHD" & rank != "LRLD"], aes(x = rank, y=acceptance_rates_sim1lr, ymin = acceptance_rates_sim1lr, ymax = acceptance_rates_sim1lr), position = position_dodge(width = .5)) +
  geom_point(aes(x = rank, y=acceptance_rates_sim1lr), position = position_dodge(width = .2), colour = "black", show.legend = FALSE, size=3) +
  geom_point(aes(x = rank, y=acceptance_rates_sim2lr), position = position_dodge(width = .8), colour = "grey", show.legend = FALSE, size=3) +
  geom_errorbar(data = data_summary[order_condition=="richpoor" & rank != "HRHD" & rank != "LRLD"], aes(ymin = acceptance_rates_sim1lr-sem_sim1lr, ymax = acceptance_rates_sim1lr+sem_sim1lr), position = position_dodge(width = .2), width=0.25, colour = "black") + 
  geom_errorbar(data = data_summary[order_condition=="richpoor" & rank != "HRHD" & rank != "LRLD"], aes(ymin = acceptance_rates_sim2lr-sem_sim2lr, ymax = acceptance_rates_sim2lr+sem_sim2lr), position = position_dodge(width = .8), width=0.25, colour = "grey") + 
  ylab("accept %") +
  xlab("option") +
  theme_cowplot()+
  ggtitle("Data and simulations \n Rich=>Poor participants")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))


#plot 10b - acceptance rates for each option in each environment (bar plot with error bars) with simulation fits
#rich=>poor subs only, intermediate options combined
p10b <- ggplot(data = data_summary[order_condition=="richpoor" & rank != "intermediate"], aes(x = rank, y = acceptance_rates_data, fill=block)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = data_summary[order_condition=="richpoor" & rank != "intermediate"], aes(ymin = acceptance_rates_data-sem_data, ymax = acceptance_rates_data+sem_data), position = position_dodge(width = .5), width=0.25) + 
  #geom_point(data = data_summary[order_condition=="richpoor" & rank != "HRHD" & rank != "LRLD"], aes(x = rank, y=acceptance_rates_sim1lr)) +
  #geom_point(data = data_summary[order_condition=="richpoor" & rank != "HRHD" & rank != "LRLD"], aes(x = rank, y=acceptance_rates_sim1lr), position = "identity", stat = "identity", fill="white", colour = "black", pch=21, size=4) +
  #geom_point(data = data_summary[order_condition=="richpoor" & rank != "HRHD" & rank != "LRLD"], aes(x = rank, y=acceptance_rates_sim1lr), stat = "identity", position = position_dodge(width = .1)) +
  #geom_pointrange(data = data_summary[order_condition=="richpoor" & rank != "HRHD" & rank != "LRLD"], aes(x = rank, y=acceptance_rates_sim1lr, ymin = acceptance_rates_sim1lr, ymax = acceptance_rates_sim1lr), position = position_dodge(width = .5)) +
  geom_point(aes(x = rank, y=acceptance_rates_sim1lr), position = position_dodge(width = .2), colour = "black", show.legend = FALSE, size=3) +
  geom_point(aes(x = rank, y=acceptance_rates_sim2lr), position = position_dodge(width = .8), colour = "grey", show.legend = FALSE, size=3) +
  geom_errorbar(data = data_summary[order_condition=="richpoor" & rank != "intermediate"], aes(ymin = acceptance_rates_sim1lr-sem_sim1lr, ymax = acceptance_rates_sim1lr+sem_sim1lr), position = position_dodge(width = .2), width=0.25, colour = "black") + 
  geom_errorbar(data = data_summary[order_condition=="richpoor" & rank != "intermediate"], aes(ymin = acceptance_rates_sim2lr-sem_sim2lr, ymax = acceptance_rates_sim2lr+sem_sim2lr), position = position_dodge(width = .8), width=0.25, colour = "grey") + 
  ylab("accept %") +
  xlab("option") +
  theme_cowplot()+
  ggtitle("Data and simulations \n Rich=>Poor participants")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))


#plot 11a - acceptance rates for each option in each environment (bar plot with error bars) with simulation fits
#poor=>rich subs only, intermediate options combined
p11a <- ggplot(data = data_summary[order_condition=="poorrich" & rank != "HRHD" & rank != "LRLD"], aes(x = rank, y = acceptance_rates_data, fill=block)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = data_summary[order_condition=="poorrich" & rank != "HRHD" & rank != "LRLD"], aes(ymin = acceptance_rates_data-sem_data, ymax = acceptance_rates_data+sem_data), position = position_dodge(width = .5), width=0.25) + 
  geom_point(aes(x = rank, y=acceptance_rates_sim1lr), position = position_dodge(width = .2), colour = "black", show.legend = FALSE, size=2) +
  geom_point(aes(x = rank, y=acceptance_rates_sim2lr), position = position_dodge(width = .8), colour = "grey", show.legend = FALSE, size=2) +
  geom_errorbar(data = data_summary[order_condition=="poorrich" & rank != "HRHD" & rank != "LRLD"], aes(ymin = acceptance_rates_sim1lr-sem_sim1lr, ymax = acceptance_rates_sim1lr+sem_sim1lr), position = position_dodge(width = .2), width=0.25, colour = "black") + 
  geom_errorbar(data = data_summary[order_condition=="poorrich" & rank != "HRHD" & rank != "LRLD"], aes(ymin = acceptance_rates_sim2lr-sem_sim2lr, ymax = acceptance_rates_sim2lr+sem_sim2lr), position = position_dodge(width = .8), width=0.25, colour = "grey") + 
  ylab("accept %") +
  xlab("option") +
  theme_cowplot()+
  ggtitle("Data and simulations \n Poor=>Rich participants")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))

#plot 11b - acceptance rates for each option in each environment (bar plot with error bars) with simulation fits
#poor=>rich subs only, intermediate options combined
p11b <- ggplot(data = data_summary[order_condition=="poorrich" & rank != "intermediate"], aes(x = rank, y = acceptance_rates_data, fill=block)) +
  geom_bar(stat = "identity", width = .5, position="dodge") +
  geom_errorbar(data = data_summary[order_condition=="poorrich" & rank != "intermediate" ], aes(ymin = acceptance_rates_data-sem_data, ymax = acceptance_rates_data+sem_data), position = position_dodge(width = .5), width=0.25) + 
  geom_point(aes(x = rank, y=acceptance_rates_sim1lr), position = position_dodge(width = .2), colour = "black", show.legend = FALSE, size=2) +
  geom_point(aes(x = rank, y=acceptance_rates_sim2lr), position = position_dodge(width = .8), colour = "grey", show.legend = FALSE, size=2) +
  geom_errorbar(data = data_summary[order_condition=="poorrich" & rank != "intermediate"], aes(ymin = acceptance_rates_sim1lr-sem_sim1lr, ymax = acceptance_rates_sim1lr+sem_sim1lr), position = position_dodge(width = .2), width=0.25, colour = "black") + 
  geom_errorbar(data = data_summary[order_condition=="poorrich" & rank != "intermediate"], aes(ymin = acceptance_rates_sim2lr-sem_sim2lr, ymax = acceptance_rates_sim2lr+sem_sim2lr), position = position_dodge(width = .8), width=0.25, colour = "grey") + 
  ylab("accept %") +
  xlab("option") +
  theme_cowplot()+
  ggtitle("Data and simulations \n Poor=>Rich participants")+
  scale_fill_brewer(palette="Set1", direction=-1)+
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))


p12a <- ggplot(data = new_sum, aes(x=rank, y=data, fill = order_condition)) + geom_bar(stat="identity")
#;put all plots into 1 figure using plot_grid function 
all_plot <- plot_grid(p1a, p1c, p2a, p2c, p3a, p3c, p4, p5, p6, p7, p8, p9, p10a, p11a, p10b, p11b, labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P"), nrow = 9) +
  ggtitle("Various Plots")

# save plots according to path and filename at top of script
save_plot(
  output_path, all_plot,
  ncol = 2, # we're saving a grid plot of x columns
  nrow = 9, # and x rows
  base_asp = 1.35 # aspect ratio of 1.35 for individual figures in the grid 
)


# 
# 
# 


