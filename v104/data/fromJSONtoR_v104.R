# Clear workspace
rm(list = ls())

# Load packages
library(jsonlite)

# Set working directory (change this to yours) 
setwd('/Users/neil/Desktop/')

# Set output paths  
outCSV_data <- c('/Users/neil/Dropbox/Daw_Lab/PreySelection/v104/data/trialdata_104_processed.csv')
outCSV_stats <- c('/Users/neil/Dropbox/Daw_Lab/PreySelection/v104/data/subdata_104.csv')

# Load data (this is the output of download_datafiles)
datDump = read.csv('/Users/neil/Dropbox/Daw_Lab/PreySelection/v104/data/trialdata_104_raw.csv', header = TRUE)

colnames(datDump) <- c("col1", "col2", "col3", "col4") 

# Get subject IDs
subjIDs<-unique(datDump$col1)

# Initialize data frames
dodf<-data.frame(subj = numeric(),
                 trial_index_actual=numeric(),
                 block=numeric(),
                 stimulus=character(),
                 stim_rank=numeric(),
                 reward_percent=numeric(),
                 delay_s = numeric(),
                 profitability = numeric(),
                 stim_left_right=character(),
                 key_press=numeric(),
                 approach_avoid=numeric(),
                 rt=numeric(),
                 rt_z=numeric(),
                 force_trial=numeric(),
                 missed=numeric(),
                 order_condition=numeric(),
                 exclude=numeric(),
                 exclude_reason=character(),
                 stringsAsFactors=FALSE)

complete_count = 0; #acts as subject number

stats_compile= data.frame(MturkID = character(),
                          AssignID = character(),
                          sub_no = numeric(),
                          age = numeric(),
                          gender = character(),
                          n_trials = numeric(),
                          n_force_wrong = numeric(),
                          n_missed = numeric(),
                          bonus_payment = numeric(),
                          percent_accept_A1=numeric(),
                          percent_accept_A2=numeric(),
                          percent_accept_A3=numeric(),
                          percent_accept_A4=numeric(),
                          percent_accept_A2_A3=numeric(), 
                          percent_accept_B1=numeric(), 
                          percent_accept_B2=numeric(), 
                          percent_accept_B3=numeric(), 
                          percent_accept_B4=numeric(), 
                          percent_accept_B2_B3=numeric(), 
                          percent_accept_B1_min_A1 = numeric(),
                          percent_accept_B2_min_A2 = numeric(),
                          percent_accept_B3_min_A3 = numeric(),
                          percent_accept_B4_min_A4 = numeric(),
                          percent_accept_B2_B3_min_A2_A3 = numeric(),
                          percent_accept_AB1 = numeric(),
                          percent_accept_AB2 = numeric(),
                          percent_accept_AB3 = numeric(),
                          percent_accept_AB4 = numeric(),
                          percent_accept_AB2_AB3 = numeric(),
                          exclude=numeric(),
                          exclude_reason=character(),
                          order_condition = numeric(),
                          comment=character(),
                          stringsAsFactors=FALSE)
                              
# loop through subjects
for (i in 1:length(subjIDs) ) {
  
  print(i)
  
  id = subjIDs[i] # grab ID
  subjDump = subset(datDump, col1==id)
  
  AssignID = toString(subjIDs[i])
  AssignID = gsub("..*:", "", AssignID)
  MturkID = toString(subjIDs[i])
  MturkID = gsub(":..*", "", MturkID)
  
  # grab trial level JSON, append brackets 
  # see: https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
  json<-toString(subjDump$col4) 
  json<-gsub("}$", "}]", json) 
  json<-paste("[", json, sep = "")
  
  # turn into dataframe
  subjDF<-fromJSON(json)
  
  # extract age and gender responses from first page and comments at the end (putinto dataframe)
  dodf_text.subj<-subset(subjDF, trial_type=="survey-text")
  dodf_text.subj = toString(dodf_text.subj$responses) 
  dodf_text.subj = gsub("}$", "}]", dodf_text.subj)
  dodf_text.subj<-paste("[", dodf_text.subj, sep = "")
  dodf_text.subj = fromJSON(dodf_text.subj)
  
  age = dodf_text.subj[1,2]
  gender = dodf_text.subj[1,3]
  
  # pull out all "stim-enters" 
  dodf.subj<-subset(subjDF, trial_type=="stim-enter")
  
  # pull out "text" for bonus payment and store to variable bonus_payment
  dodf_bonus.subj<-subset(subjDF, trial_type=="text")
  bonus_payment = dodf_bonus.subj$bonus_payment[length(dodf_bonus.subj$bonus_payment)]
  
  # pull out "text" for bonus payment and store to variable bonus_payment
  if (!is.null(bonus_payment)) {
    comment = dodf_text.subj[2,1] 
  } else {
    comment = NaN
  }
    
  # pull out all missed responses
  dodf_missed.subj<-subset(subjDF, trial_type=="missed-response")
  
  # get rid of training trials
  dodf_missed.subj = dodf_missed.subj[dodf_missed.subj$missed_response_training==0, ]
  
  # now store missed trials index
  missed_trials = dodf_missed.subj$trial_index_actual
  
  # get rid of training trials
  dodf.subj = dodf.subj[dodf.subj$reward_mean_stim<1.0 & dodf.subj$reward_mean_stim>0.1,]
    
  if (nrow(dodf.subj) > 0) { 
    complete_count = complete_count+1; #iterates through subject number
  }
  
  # shlp is to be the subject number
  shlp <- rep(complete_count, each=length(dodf.subj$trial_index))
  
  # calculate whether approach / avoid
  approach_avoid = rep(0, each=length(dodf.subj$trial_index))
  
  for (j in 1:length(approach_avoid)) {
      if (dodf.subj$key_press[j]==70 & dodf.subj$stim_left_right[j]=="left"){
      approach_avoid[j]=1
      } else if (dodf.subj$key_press[j]==74 & dodf.subj$stim_left_right[j]=="right") {
      approach_avoid[j]=1
      } else if (dodf.subj$key_press[j]==70 & dodf.subj$stim_left_right[j]=="right") {
      approach_avoid[j]=-1
      } else if (dodf.subj$key_press[j]==74 & dodf.subj$stim_left_right[j]=="left") {
      approach_avoid[j]=-1
      }
  }
  
  # calculate whether missed
  missed = rep(0, each=length(dodf.subj$trial_index))

  missed[dodf.subj$trial_index_actual %in% missed_trials] = 1
  
  rt_z = rep(NaN, each=length(dodf.subj$trial_index))
    
  rt_z[missed==0] = scale(dodf.subj$rt[missed==0])
  
  # put NaN for missed responses for rt
  dodf.subj$rt[missed==1] = NaN
  
  # and approach avoid
  approach_avoid[missed==1] = NaN
  
  # order condition calculate
  if (dodf.subj$block_index[1] == 0){
  order_condition = rep(1, each=length(dodf.subj$trial_index))
  } else if (dodf.subj$block_index[1] == 1){
  order_condition = rep(2, each=length(dodf.subj$trial_index))
  }
  
  # calculate profitability. note need to add a second for the confirmation screen
  profitability = dodf.subj$reward_mean_stim*100/((dodf.subj$time_delay_mean_stim + 10)/10)
  
  # calculate acceptance rates
  percent_accept_A1 = length(which((dodf.subj$rank==1 & approach_avoid==1 & dodf.subj$block_index==0 & dodf.subj$force_trial==0 & missed==0)))/length(which((dodf.subj$rank==1 & dodf.subj$block_index==0 & dodf.subj$force_trial==0 & missed==0)))
  percent_accept_A2 = length(which((dodf.subj$rank==2 & approach_avoid==1 & dodf.subj$block_index==0 & dodf.subj$force_trial==0 & missed==0)))/length(which((dodf.subj$rank==2 & dodf.subj$block_index==0 & dodf.subj$force_trial==0 & missed==0)))
  percent_accept_A3 = length(which((dodf.subj$rank==3 & approach_avoid==1 & dodf.subj$block_index==0 & dodf.subj$force_trial==0 & missed==0)))/length(which((dodf.subj$rank==3 & dodf.subj$block_index==0 & dodf.subj$force_trial==0 & missed==0)))
  percent_accept_A4 = length(which((dodf.subj$rank==4 & approach_avoid==1 & dodf.subj$block_index==0 & dodf.subj$force_trial==0 & missed==0)))/length(which((dodf.subj$rank==4 & dodf.subj$block_index==0 & dodf.subj$force_trial==0 & missed==0)))
  percent_accept_A2_A3 = length(which((dodf.subj$rank>1 & dodf.subj$rank<4 & approach_avoid==1 & dodf.subj$block_index==0 & dodf.subj$force_trial==0 & missed==0)))/length(which((dodf.subj$rank>1 & dodf.subj$rank<4  & dodf.subj$block_index==0 & dodf.subj$force_trial==0 & missed==0)))
  
  percent_accept_B1 = length(which((dodf.subj$rank==1 & approach_avoid==1 & dodf.subj$block_index==1 & dodf.subj$force_trial==0 & missed==0)))/length(which((dodf.subj$rank==1 & dodf.subj$block_index==1 & dodf.subj$force_trial==0 & missed==0)))
  percent_accept_B2 = length(which((dodf.subj$rank==2 & approach_avoid==1 & dodf.subj$block_index==1 & dodf.subj$force_trial==0 & missed==0)))/length(which((dodf.subj$rank==2 & dodf.subj$block_index==1 & dodf.subj$force_trial==0 & missed==0)))
  percent_accept_B3 = length(which((dodf.subj$rank==3 & approach_avoid==1 & dodf.subj$block_index==1 & dodf.subj$force_trial==0 & missed==0)))/length(which((dodf.subj$rank==3 & dodf.subj$block_index==1 & dodf.subj$force_trial==0 & missed==0)))
  percent_accept_B4 = length(which((dodf.subj$rank==4 & approach_avoid==1 & dodf.subj$block_index==1 & dodf.subj$force_trial==0 & missed==0)))/length(which((dodf.subj$rank==4 & dodf.subj$block_index==1 & dodf.subj$force_trial==0 & missed==0)))
  percent_accept_B2_B3 = length(which((dodf.subj$rank>1 & dodf.subj$rank<4 & approach_avoid==1 & dodf.subj$block_index==1 & dodf.subj$force_trial==0 & missed==0)))/length(which((dodf.subj$rank>1 & dodf.subj$rank<4 & dodf.subj$block_index==1 & dodf.subj$force_trial==0 & missed==0)))
  
  percent_accept_B1_min_A1 = percent_accept_B1 - percent_accept_A1
  percent_accept_B2_min_A2 = percent_accept_B2 - percent_accept_A2
  percent_accept_B3_min_A3 = percent_accept_B3 - percent_accept_A3
  percent_accept_B4_min_A4 = percent_accept_B4 - percent_accept_A4
  percent_accept_B2_B3_min_A2_A3 = percent_accept_B2_B3 - percent_accept_A2_A3
  
  percent_accept_AB1 = length(which((dodf.subj$rank==1 & approach_avoid==1 & dodf.subj$force_trial==0 & missed==0)))/length(which((dodf.subj$rank==1 & dodf.subj$force_trial==0 & missed==0)))
  percent_accept_AB2 = length(which((dodf.subj$rank==2 & approach_avoid==1 & dodf.subj$force_trial==0 & missed==0)))/length(which((dodf.subj$rank==2 & dodf.subj$force_trial==0 & missed==0)))
  percent_accept_AB3 = length(which((dodf.subj$rank==3 & approach_avoid==1 & dodf.subj$force_trial==0 & missed==0)))/length(which((dodf.subj$rank==3 & dodf.subj$force_trial==0 & missed==0)))
  percent_accept_AB4 = length(which((dodf.subj$rank==4 & approach_avoid==1 & dodf.subj$force_trial==0 & missed==0)))/length(which((dodf.subj$rank==4 & dodf.subj$force_trial==0 & missed==0)))
  percent_accept_AB2_AB3 = length(which((dodf.subj$rank>1 & dodf.subj$rank<4 & approach_avoid==1 & dodf.subj$force_trial==0 & missed==0)))/length(which((dodf.subj$rank>1 & dodf.subj$rank<4 & dodf.subj$force_trial==0 & missed==0)))
  
  # calculate how many force trials wrong
  n_force_wrong = length(which(approach_avoid==-1 & dodf.subj$force_trial==1)) + length(which(approach_avoid==1 & dodf.subj$force_trial==2)) + length(which(approach_avoid=="NaN" & dodf.subj$force_trial>0))

  # and how many missed
  n_missed = length(which(missed==1))
  
  odd_acceptance = vector(length=6)

  # accept worst option more than the best option
  odd_acceptance[1] = percent_accept_A4 >= percent_accept_A1
  odd_acceptance[2] = percent_accept_B4 >= percent_accept_B1
  
  # accept every option all the time
  odd_acceptance[3] = (percent_accept_A1 == 1 & percent_accept_A2 == 1 & percent_accept_A3 == 1 &  percent_accept_A4 == 1)
  odd_acceptance[4] = (percent_accept_B1 == 1 & percent_accept_B2 == 1 & percent_accept_B3 == 1 &  percent_accept_B4 == 1)
  
  # reject every option all the time
  odd_acceptance[5] = (percent_accept_A1 == 0 & percent_accept_A2 == 0 & percent_accept_A3 == 0 &  percent_accept_A4 == 0)
  odd_acceptance[6] = (percent_accept_B1 == 0 & percent_accept_B2 == 0 & percent_accept_B3 == 0 &  percent_accept_B4 == 0)
  
  # exclude if more than 20 responses missed
  # or if more than 10 force trials missed
  # or if did not finish
  
  # initalise
  exclude = rep(0, each=length(dodf.subj$trial_index))
  exclude_reason = rep("do not exclude", each=length(dodf.subj$trial_index)) 
    
  if (n_missed > 19){
    exclude = rep(1, each=length(dodf.subj$trial_index))
    exclude_reason = rep(">19 missed", each=length(dodf.subj$trial_index)) 
  } else if (n_force_wrong > 9){
    exclude = rep(1, each=length(dodf.subj$trial_index))
    exclude_reason = rep(">9 wrong force", each=length(dodf.subj$trial_index)) 
  } else if (is.null(bonus_payment)){
    exclude = rep(1, each=length(dodf.subj$trial_index))
    exclude_reason = rep("failed to finish", each=length(dodf.subj$trial_index)) 
  } else if (length(which(odd_acceptance) > 0)){
    exclude = rep(1, each=length(dodf.subj$trial_index))
    exclude_reason = rep("poor behaviour", each=length(dodf.subj$trial_index)) 
  }
  
  dodf.subj<-cbind(shlp,
                   dodf.subj$trial_index_actual,
                   dodf.subj$block_index,
                   dodf.subj$stimulus,
                   dodf.subj$rank,
                   dodf.subj$reward_mean_stim*100,
                   (dodf.subj$time_delay_mean_stim + 10)/10,
                   profitability,
                   dodf.subj$stim_left_right,
                   dodf.subj$key_press,
                   approach_avoid,
                   dodf.subj$rt,
                   rt_z,
                   dodf.subj$force_trial,
                   missed,
                   order_condition,
                   exclude,
                   exclude_reason)
  
  dodf<-rbind(dodf,dodf.subj)

  stats_compile[i,1] = MturkID
  stats_compile[i,2] = AssignID
  stats_compile[i,3] = complete_count
  stats_compile[i,4] = age
  stats_compile[i,5] = gender
  stats_compile[i,6] = length(profitability)
  stats_compile[i,7] = n_force_wrong
  stats_compile[i,8] = n_missed

  if (!is.null(bonus_payment)){
  stats_compile[i,9] = bonus_payment
  } else {
    stats_compile[i,9] = NaN
  }
  
  stats_compile[i,10]= percent_accept_A1
  stats_compile[i,11]= percent_accept_A2
  stats_compile[i,12]= percent_accept_A3
  stats_compile[i,13]= percent_accept_A4
  stats_compile[i,14]= percent_accept_A2_A3
  stats_compile[i,15]= percent_accept_B1
  stats_compile[i,16]= percent_accept_B2
  stats_compile[i,17]= percent_accept_B3
  stats_compile[i,18]= percent_accept_B4
  stats_compile[i,19]= percent_accept_B2_B3
  stats_compile[i,20] = percent_accept_B1_min_A1
  stats_compile[i,21] = percent_accept_B2_min_A2
  stats_compile[i,22] = percent_accept_B3_min_A3
  stats_compile[i,23] = percent_accept_B4_min_A4
  stats_compile[i,24] = percent_accept_B2_B3_min_A2_A3
  stats_compile[i,25] = percent_accept_AB1
  stats_compile[i,26] = percent_accept_AB2
  stats_compile[i,27] = percent_accept_AB3
  stats_compile[i,28] = percent_accept_AB4
  stats_compile[i,29] = percent_accept_AB2_AB3
  stats_compile[i,30] = exclude[1]
  stats_compile[i,31] = exclude_reason[1]
  stats_compile[i,32] = order_condition[1]
  stats_compile[i,33] = comment
}

# Rename columns - need?
colnames(dodf) <- c("subj", "trial_index_actual", "block", "stimulus", "stim_rank", "reward_percent", "delay_s", "profitability", "stim_left_right", "key_press", "approach_avoid", "rt", "rt_z", "force_trial", "missed", "order_condition", "exclude", "exclude_reason") 

# Save dataframes
write.csv(dodf, outCSV_data[1], row.names=F)
write.csv(stats_compile, outCSV_stats[1],row.names=F)

