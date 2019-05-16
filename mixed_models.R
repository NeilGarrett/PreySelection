# Clear workspace
rm(list = ls())

#load in lme4 library
library(lme4)
#and lmerTest
#library(lmerTest)
library(data.table)

model_folder = c("v103")

setwd(paste("~/GitHubRepo/Projects/PreySelection/", model_folder, "/models/model_asymmetry/", sep =""))

#load in data
dat = fread(paste("data_for_lme4.csv", sep =""))

dat_summary_stats = fread(paste("summary_stats2.csv", sep =""))

#path and filename for saving
output_models = paste("mixedmodels_", model_folder, ".txt", sep="")

 dat$block = as.factor(dat$block)
 dat$order_condition = as.factor(dat$order_condition)
 
 #dat$stim_rank_factor = as.factor(dat$stim_rank)
 
 dat$profitability = as.factor(dat$profitability)
 dat[, stim_rank_factor_z := scale(as.numeric(stim_rank_factor))]
 dat[, stim_rank_factor_1back_z := scale(as.numeric(stim_rank_factor_1back))]
 
 #scale this so that in same units as delays - easier for interpreation of betas
 dat$change_reward = dat$change_reward/10 
 
 dat$choice_log_reg_1back = as.factor(dat$choice_log_reg_1back)
 #dat$stim_rank_factor_1back = as.factor(dat$stim_rank_factor_1back)
 
 #mean(dat[choice_log_reg==1 & block==-1, .N, by=.(subj)][,N]/dat[block==-1, .N, by=.(subj)][,N])
 #sd(dat[choice_log_reg==1 & block==-1, .N, by=.(subj)][,N]/dat[block==-1, .N, by=.(subj)][,N])
 #mean(dat[choice_log_reg==1 & block==1, .N, by=.(subj)][,N]/dat[block==1, .N, by=.(subj)][,N])
 #sd(dat[choice_log_reg==1 & block==1, .N, by=.(subj)][,N]/dat[block==1, .N, by=.(subj)][,N])
  
#sink - prints subsequent output to text file 
#sink(output_models)
# 
#m1a = lmer('choice_log_reg ~ stim_rank_factor*block + (1 + stim_rank_factor*block | subj)', data = dat, family = binomial)
#  summary(m1a)
  
m1b = lmer('choice_log_reg ~ stim_rank_factor + stim_rank_factor_1back + block + (1 + stim_rank_factor + stim_rank_factor_1back + block | subj)', data = dat, family = binomial)
#m1b = lmer('choice_log_reg ~ profitability + block + stim_rank_factor_1back + (1 + profitability + block + stim_rank_factor_1back | subj)', data = dat, family = binomial)
summary(m1b)
# 
# stats_m1a = data.frame(coef(summary(m1a)))
# write.csv(stats_m1a, file="stats_m1a.csv")

m2a = glmer('choice_log_reg ~ stim_rank_factor_1back*choice_log_reg_1back + (1 + stim_rank_factor_1back*choice_log_reg_1back| subj)', data = dat[choice_log_reg_1back!="NaN", ], family = binomial)
summary(m2a)

m2a_z = glmer('choice_log_reg ~ stim_rank_factor_1back_z*choice_log_reg_1back + (1 + stim_rank_factor_1back_z*choice_log_reg_1back| subj)', data = dat[choice_log_reg_1back!="NaN", ], family = binomial)
summary(m2a_z)
 
m2a_z = glmer('choice_log_reg ~ stim_rank_factor_1back_z + (1 + stim_rank_factor_1back_z | subj)', data = dat[choice_log_reg_1back!="NaN", ], family = binomial)

#check if within trial fluctuationas are just the result of autocorrelation by putting in both current option and previous option
m2b = glmer('choice_log_reg ~ stim_rank_factor + stim_rank_factor_1back + (1 + stim_rank_factor + stim_rank_factor_1back| subj)', data = dat[choice_log_reg_1back!="NaN", ], family = binomial)
summary(m2b)


#z score?
m2b_z = glmer('choice_log_reg ~ stim_rank_factor_z + stim_rank_factor_1back_z + (1 + stim_rank_factor_z + stim_rank_factor_1back_z | subj)', data = dat[choice_log_reg_1back!="NaN", ], family = binomial)
summary(m2b_z)

m2c = glmer('choice_log_reg ~ stim_rank_factor*stim_rank_factor_1back + (1 + stim_rank_factor*stim_rank_factor_1back| subj)', data = dat[choice_log_reg_1back!="NaN", ], family = binomial)
summary(m2c)

m2c_z = glmer('choice_log_reg ~ stim_rank_factor_z*stim_rank_factor_1back_z + (1 + stim_rank_factor_z*stim_rank_factor_1back_z| subj)', data = dat[choice_log_reg_1back!="NaN", ], family = binomial)
summary(m2c_z)


#controling for prev choice
m2d = glmer('choice_log_reg ~ stim_rank_factor + stim_rank_factor_1back + choice_log_reg_1back + (1 + stim_rank_factor + stim_rank_factor_1back + choice_log_reg_1back| subj)', data = dat[choice_log_reg_1back!="NaN", ], family = binomial)
summary(m2d)

m2d_z = glmer('choice_log_reg ~ stim_rank_factor_z + stim_rank_factor_1back_z + choice_log_reg_1back + (1 + stim_rank_factor_z + stim_rank_factor_1back_z + choice_log_reg_1back| subj)', data = dat[choice_log_reg_1back!="NaN", ], family = binomial)
summary(m2d_z)


#interact prev choice with previous option 
m2e = glmer('choice_log_reg ~ stim_rank_factor + stim_rank_factor_1back*choice_log_reg_1back + (1 + stim_rank_factor + stim_rank_factor_1back*choice_log_reg_1back| subj)', data = dat[choice_log_reg_1back!="NaN", ], family = binomial)
summary(m2e)

m2e_z = glmer('choice_log_reg ~ stim_rank_factor_z + stim_rank_factor_1back_z*choice_log_reg_1back + (1 + stim_rank_factor_z + stim_rank_factor_1back_z*choice_log_reg_1back| subj)', data = dat[choice_log_reg_1back!="NaN", ], family = binomial)
summary(m2e_z)


#control for environment
m2f = glmer('choice_log_reg ~ stim_rank_factor + stim_rank_factor_1back*choice_log_reg_1back + block + (1 + stim_rank_factor + stim_rank_factor_1back*choice_log_reg_1back + block | subj)', data = dat[choice_log_reg_1back!="NaN", ], family = binomial)
summary(m2f)

m2f_z = glmer('choice_log_reg ~ stim_rank_factor_z + stim_rank_factor_1back_z*choice_log_reg_1back + block + (1 + stim_rank_factor_z + stim_rank_factor_1back_z*choice_log_reg_1back + block | subj)', data = dat[choice_log_reg_1back!="NaN", ], family = binomial)
summary(m2f_z)

# 
#m4a = glmer('choice_log_reg ~ change_reward + change_delay + block + (1 + change_reward + change_delay + block| subj)', data = dat, family = binomial)
#summary(m4a)

#m4b = glmer('choice_log_reg ~ change_reward + change_delay + block + choice_log_reg_1back + (1 + change_reward + change_delay + block + choice_log_reg_1back | subj)', data = dat[choice_log_reg_1back!="NaN", ], family = binomial)
#summary(m4b)

m4bb = glmer('choice_log_reg ~ profitability + change_reward + change_delay + block + choice_log_reg_1back + (1 + profitability + change_reward + change_delay + block + choice_log_reg_1back | subj)', data = dat[choice_log_reg_1back!="NaN", ], family = binomial)
summary(m4bb)



m4c = glmer('choice_log_reg ~ reward_percent + delay_s + change_reward + change_delay + block + choice_log_reg_1back + (1 + reward_percent + delay_s + change_reward + change_delay + block + choice_log_reg_1back | subj)', data = dat, family = binomial)
summary(m4c)

m4d = glmer('choice_log_reg ~ profitability + change_reward + change_delay + block + choice_log_reg_1back + (1 + profitability + change_reward + change_delay + block + choice_log_reg_1back | subj)', data = dat, family = binomial)
summary(m4d)

m4e = glmer('choice_log_reg ~ profitability + change_reward*change_delay + block + choice_log_reg_1back + (1 + profitability + change_reward*change_delay + block + choice_log_reg_1back | subj)', data = dat, family = binomial)
summary(m4e)


##crossed effect models

m5a = glmer('choice_log_reg ~ change_reward*choice_log_reg_1back + change_delay*choice_log_reg_1back + (1 + change_reward*choice_log_reg_1back + change_delay*choice_log_reg_1back| subj) + (1 | stim_rank_factor)', data = dat[choice_log_reg_1back!="NaN", ], family = binomial)
m5b = glmer('choice_log_reg ~ change_reward*choice_log_reg_1back + change_delay*choice_log_reg_1back + block + (1 + change_reward*choice_log_reg_1back + change_delay*choice_log_reg_1back+ block| subj) + (1 | stim_rank_factor)', data = dat[choice_log_reg_1back!="NaN", ], family = binomial)

m5c = glmer('choice_log_reg ~ change_reward*choice_log_reg_1back + change_delay*choice_log_reg_1back + (1 + change_reward*choice_log_reg_1back + change_delay*choice_log_reg_1back | subj) + (1 | stim_rank_factor)', data = dat[choice_log_reg_1back!="NaN" & block==-1, ], family = binomial)
m5d = glmer('choice_log_reg ~ change_reward + change_delay + choice_log_reg_1back + (1 + change_reward + change_delay + choice_log_reg_1back | subj) + (1 | stim_rank_factor)', data = dat[choice_log_reg_1back!="NaN" & block==-1, ], family = binomial)

m5e = glmer('choice_log_reg ~ change_reward*choice_log_reg_1back + change_delay*choice_log_reg_1back + (1 + change_reward*choice_log_reg_1back + change_delay*choice_log_reg_1back | subj) + (1 | stim_rank_factor)', data = dat[choice_log_reg_1back!="NaN" & block==1, ], family = binomial)
m5f = glmer('choice_log_reg ~ change_reward + change_delay + choice_log_reg_1back + (1 + change_reward + change_delay + choice_log_reg_1back | subj) + (1 | stim_rank_factor)', data = dat[choice_log_reg_1back!="NaN" & block==1, ], family = binomial)


# # 
#  m4b = lmer('choice_log_reg ~ change_reward*block + change_delay*block + (1 + change_reward*block + change_delay*block | subj)', data = dat, family = binomial)
#    summary(m4b)

    #stats_m4a = data.frame(coef(summary(m4a)))
    #write.csv(stats_m4a, file="stats_m4a.csv")

 # m4c = lmer('choice_log_reg ~ stim_rank_factor_z + change_reward + change_delay + (1 + stim_rank_factor_z + change_reward + change_delay | subj)', data = dat, family = binomial)
 # summary(m4c)
 # 
 # m4d = lmer('choice_log_reg ~ stim_rank_factor_z + change_reward + change_delay + block + (1 + stim_rank_factor_z + change_reward + change_delay + block | subj)', data = dat, family = binomial)
 # summary(m4d)
 # 
 
# m4e = lmer('choice_log_reg ~ stim_rank_factor_z + change_reward*block + change_delay*block + (1 + stim_rank_factor_z + change_reward*block + change_delay*block | subj)', data = dat, family = binomial)
# summary(m4e)
   
# m5a = lmer('choice_log_reg ~ stim_rank_factor_z + block*order_condition + (1 + stim_rank_factor_z + block| subj)', data = dat, family = binomial)
# summary(m5a)
#   
# m5b = lmer('choice_log_reg ~ stim_rank_factor_z*block*order_condition + (1 + stim_rank_factor_z*block| subj)', data = dat, family = binomial)
# summary(m5b)

# m6 = lmer('choice_log_reg ~ stim_rank_factor_z*block + stim_rank_factor_1back_z*block + (1 + stim_rank_factor_z*block+ stim_rank_factor_1back_z*block  | subj)', data = dat, family = binomial)
# summary(m6)

# 
# t.test(dat_summary_stats$percent_accept_A1,dat_summary_stats$percent_accept_B1,paired=TRUE) 
# t.test(dat_summary_stats$percent_accept_A2_A3,dat_summary_stats$percent_accept_B2_B3,paired=TRUE) 
# t.test(dat_summary_stats$percent_accept_A4,dat_summary_stats$percent_accept_B4,paired=TRUE) 
# 
# # test change in acceptance rates for participants in seperate conditions
# t.test(dat_summary_stats$percent_accept_B2_B3_min_A2_A3~dat_summary_stats$order_condition)

#stop record
#sink()



# index_numeric=unlist(lapply(dat, is.numeric))
# a = cor(dat[, index_numeric, with=FALSE], use = "pairwise.complete.obs")
# b = correlate(dat[, index_numeric, with=FALSE])
# c = stretch(b)
# 
# dat[, index_numeric, with=FALSE]
# 
# 
# correlate(dat[, lapply(SD. is.numeric())])
# 
# lapply(.SD, mean)


#plot 1 - acceptance rates for each option in each environment (bar plot with error bars) over all subs, intermediate options combined

table_estimates = summary(m4b)
dat_plot = data.frame(table_estimates$coefficients)
dat_plot$coefficent <- rownames(dat_plot)

p1a <- ggplot(data = dat_plot, aes(x = coefficent, y = Estimate)) +
  geom_bar(stat = "identity", width = .5, position="dodge")
  geom_errorbar(data = data_summary[order_condition=="all" & rank != "HRHD" & rank != "LRLD"], aes(ymin = acceptance_rates-sem, ymax = acceptance_rates+sem), position = position_dodge(width = .5), width=0.25) + 
  #ylab("accept %") +
  #xlab("option") +
  #theme_cowplot()+
  #ggtitle("All participants")+
  #scale_fill_brewer(palette="Set1", direction=-1)+
  #theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face="bold"), plot.title = element_text(size = 10, face = "bold"), legend.text=element_text(size=5), legend.title=element_text(size=5))

