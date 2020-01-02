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

exp = 2 #change this according to which plot you want to do 

#load in data
if (exp==1){
  dat_SYM = fread("/Users/neil/GitHubRepo/Projects/PreySelection/v103/models/model_symmetric/loocv_scores.csv")
  dat_ASYM = fread("/Users/neil/GitHubRepo/Projects/PreySelection/v103/models/model_asymmetric/loocv_scores.csv")
} else if (exp ==2){
  dat_SYM = fread("/Users/neil/GitHubRepo/Projects/PreySelection/v104/models/model_symmetric/loocv_scores.csv")
  dat_ASYM = fread("/Users/neil/GitHubRepo/Projects/PreySelection/v104/models/model_asymmetric/loocv_scores.csv")
}

N_subs = nrow(dat_SYM)
data_summary = data.table(model = as.factor(c("Symmetric Model", "Asymmetric Model")), means = c(mean(dat_SYM$liks), mean(dat_ASYM$liks)), sem = c(sd(dat_SYM$liks)/(sqrt(N_subs)), sd(dat_ASYM$liks)/(sqrt(N_subs))))

levels(data_summary$model)

data_summary$model = relevel(data_summary$model, "Symmetric Model")

levels(data_summary$model)

data_individual = data.table(liks = c(dat_SYM$liks, dat_ASYM$liks), model = as.factor(c(rep("Symmetric Model", N_subs), rep("Asymmetric Model", N_subs))))

levels(data_individual$model)

data_individual$model = relevel(data_individual$model, "Symmetric Model")

levels(data_individual$model)


if (exp==1){
  title_text = "Model Comparison: Experiment 1"
  maxy = 150
} else if (exp==2){
  title_text = "Model Comparison: Experiment 2"
  maxy = 125
  
}


col_palette = c("#D4EFDF", "#D6EAF8")

p1 <- ggplot(data = data_summary, aes(x = model, y = means, fill=model)) +
  geom_bar(stat = "identity", width = .5, position="dodge", colour="black") +
  geom_errorbar(data = data_summary, aes(ymin = means-sem, ymax = means+sem), position = position_dodge(width = .5), width=0.25)+
  geom_point(data = data_individual, aes(x = model, y = liks), alpha = 0.2, size = 3, position = position_dodge(width=0.5)) +
  geom_point(data = data_individual, aes(x = model, y = liks, fill = model), colour = "black", alpha = 0.2, size = 3, position = position_dodge(width=0.5)) +
  coord_flip()+
  ylab("Model Scores \n (Leave one out cross validation)")+
  theme_cowplot()+
  theme(legend.position="none")+
  ggtitle(title_text)+
  scale_fill_manual(values=col_palette)+
  scale_y_continuous(expand = c(0,0), limits = c(0, maxy))+
  theme(axis.text=element_text(size=8), axis.title=element_text(size=15, face="bold"), plot.title = element_text(size = 10, face = "bold"))

p1
