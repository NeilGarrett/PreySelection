%% analysis script for prey selection task v103
% N GARRETT, June 2018

%% clear everything, screen etc.
clear all;  
close all;
clc;

%change this to run the relevant experiment (103/104/105)
experiment_v = 105

%% load in data
fs = filesep;

experiment_folder = ['v' num2str(experiment_v)];

%paths to where folders are
experiment_path = ['/Users/neil/GitHubRepo/Projects/PreySelection/' experiment_folder fs 'models/']

%model_path = 'model_MVT'
model_path = 'model_asymmetry'

%cd to folder where data is (should be in relevant model)
cd([experiment_path fs model_path fs])

%load in data (generated from model in julia). note julia creates csv - need to save as excel file as matlab finds it easier to read this format
data = readtable('trial_by_trial_values.csv');

summary_stats = readtable('summary_stats.csv');

%% data prep %%

%difference in value (signed)
dv = data.reward_percent - data.opp_cost_estimate;

%unsigned
dv_abs = abs(dv);

%add variable coding choice as 1s (accept) and 0s (reject)
choice_log_reg = data.approach_avoid - 1;

%add variables coding reward, delays and rank as factors (1/-1 and 1/0/-1)
reward_factor = NaN*ones(length(data.block),1);
delay_factor = NaN*ones(length(data.block),1);

reward_factor(find(data.reward_percent==80)) = 1;
reward_factor(find(data.reward_percent==20)) = -1;

delay_factor(find(data.delay_s==8)) = -1;
delay_factor(find(data.delay_s==2)) = 1;

stim_rank_factor = data.stim_rank;
stim_rank_factor(find(data.stim_rank==2)) = 0;
stim_rank_factor(find(data.stim_rank==3)) = 0;
stim_rank_factor(find(data.stim_rank==4)) = -1;

%list of subject numbers
sub_n = unique(data.subj);

% store reward and delay of options 1/2/3 back
% and acceptance of previous choice
% do this before excluding force trials

%initalise 
reward_factor_1back = []; delay_factor_1back = [];
reward_factor_2back = []; delay_factor_2back = [];
reward_factor_3back = []; delay_factor_3back = [];

choice_log_reg_1back = [];

change_delay = []; change_reward = [];

stim_rank_factor_1back = [];


for j = 1:length(sub_n)
    
    this_sub_data = data(find(data.subj==sub_n(j)),:);
    this_sub_stim_rank_factor = stim_rank_factor(find(data.subj==sub_n(j)),:);
    
    reward_factor_1back = [reward_factor_1back; NaN; this_sub_data.reward_percent(1:end-1)];
    delay_factor_1back = [delay_factor_1back; NaN; this_sub_data.delay_s(1:end-1)];
    reward_factor_2back = [reward_factor_2back; NaN; NaN; this_sub_data.reward_percent(1:end-2)];
    delay_factor_2back = [delay_factor_2back; NaN; NaN; this_sub_data.delay_s(1:end-2)];
    reward_factor_3back = [reward_factor_3back; NaN; NaN; NaN; this_sub_data.reward_percent(1:end-3)];
    delay_factor_3back = [delay_factor_3back; NaN; NaN; NaN; this_sub_data.delay_s(1:end-3)];   
    
    choice_log_reg_1back = [choice_log_reg_1back; NaN; this_sub_data.approach_avoid(1:end-1)];
     
    stim_rank_factor_1back = [stim_rank_factor_1back; NaN; this_sub_stim_rank_factor(1:end-1)];

    change_delay = [change_delay; 0; this_sub_data.delay_sum(1); this_sub_data.delay_sum(2:end-1) - this_sub_data.delay_sum(1:end-2)];
    change_reward = [change_reward; 0; this_sub_data.reward_sum(1); this_sub_data.reward_sum(2:end-1) - this_sub_data.reward_sum(1:end-2)];
    
end

reward_factor_1back(find(reward_factor_1back==80)) = 1;
reward_factor_1back(find(reward_factor_1back==20)) = -1;
delay_factor_1back(find(delay_factor_1back==8)) = -1;
delay_factor_1back(find(delay_factor_1back==2)) = 1;

reward_factor_2back(find(reward_factor_2back==80)) = 1;
reward_factor_2back(find(reward_factor_2back==20)) = -1;
delay_factor_2back(find(delay_factor_2back==8)) = -1;
delay_factor_2back(find(delay_factor_2back==2)) = 1;

reward_factor_3back(find(reward_factor_3back==80)) = 1;
reward_factor_3back(find(reward_factor_3back==20)) = -1;
delay_factor_3back(find(delay_factor_3back==8)) = -1;
delay_factor_3back(find(delay_factor_3back==2)) = 1;

choice_log_reg_1back = choice_log_reg_1back - 1;

%missed responses are deleted (below) but for 1 back need to mark as NaN
%(these will currently be coded as -1)
choice_log_reg_1back(find(choice_log_reg_1back==-1)) = NaN;

%so these are coded 0 for when rejected on previous trial, 1 for when
%accepted and the reward/delay was high, -1 when accepted and the reward/delay was low
choice_reward_1back_interaction = choice_log_reg_1back.*reward_factor_1back;
choice_delay_1back_interaction = choice_log_reg_1back.*delay_factor_1back;

%add variables to table
dat = table(dv, dv_abs, choice_log_reg, reward_factor, delay_factor, stim_rank_factor, reward_factor_1back, delay_factor_1back, reward_factor_2back, delay_factor_2back, reward_factor_3back, delay_factor_3back, choice_log_reg_1back, choice_reward_1back_interaction, choice_delay_1back_interaction, change_delay, change_reward, stim_rank_factor_1back);
data = [data, dat];

%clear this table as everything is in data now
clear dat

%get rid of missed trials
data = data(find(data.missed==0),:);

%now get rid of forced trials - don't want to use these for predicting
%accept/reject
data = data(find(data.force_trial==0),:);

%code block as -1s (rich block where ought to be more selective, acceptance rates down) and 1s
%(poor block where ought to be less selective, acceptance rates up)
data.block(find(data.block==0)) = -1;  

%if running version 105 (which has intervention block), remove this
data = data(find(data.block<2),:);

%code condition as -1s (AB) and 1s (BA) 
data.order_condition(find(data.order_condition==1)) = -1; 
data.order_condition(find(data.order_condition==2)) = 1;

%% extract average reward estimate for each block %%

for j = 1:length(sub_n)
    
    this_sub_data_A = data(find(data.subj==sub_n(j) & data.block==-1),:);
    this_sub_data_B = data(find(data.subj==sub_n(j) & data.block==1),:);
    
    this_sub_data_ALL = data(find(data.subj==sub_n(j)),:); 
    
    av_reward_compile_single(j ,1) = mean(this_sub_data_A.Q_estimate);
    av_reward_compile_single(j ,2) = mean(this_sub_data_B.Q_estimate);
    av_reward_compile_single(j ,3) = mean(this_sub_data_ALL.Q_estimate);
    
    av_reward_arithmetic(j, 1) = mean(this_sub_data_A.Q_arithmetic);
    av_reward_arithmetic(j, 2) = mean(this_sub_data_B.Q_arithmetic);
    av_reward_arithmetic(j, 3) = mean(this_sub_data_ALL.Q_arithmetic);
    
    comp(j,1) = length(find(this_sub_data_A.reward_percent(find(this_sub_data_A.choice_log_reg==1)) > this_sub_data_A.Q_estimate(find(this_sub_data_A.choice_log_reg==1))))/length(this_sub_data_A.reward_percent(find(this_sub_data_A.choice_log_reg==1)));
    comp(j,2) = length(find(this_sub_data_B.reward_percent(find(this_sub_data_B.choice_log_reg==1)) > this_sub_data_B.Q_estimate(find(this_sub_data_B.choice_log_reg==1))))/length(this_sub_data_B.reward_percent(find(this_sub_data_B.choice_log_reg==1)));
    comp(j,3) = length(find(this_sub_data_ALL.reward_percent(find(this_sub_data_ALL.choice_log_reg==1)) > this_sub_data_ALL.Q_estimate(find(this_sub_data_ALL.choice_log_reg==1))))/length(this_sub_data_ALL.reward_percent(find(this_sub_data_ALL.choice_log_reg==1)));

    order_condition(j, 1) = this_sub_data_ALL.order_condition(1);
    
end

%% calculate acceptance rates based on previous option

for s = 1:length(sub_n)

    %subset data
    this_sub = sub_n(s);
    subset_data = data(find(data.subj==this_sub),:);
    subset_data_A = data(find(data.subj==this_sub & data.block==-1),:);
    subset_data_B = data(find(data.subj==this_sub & data.block==1),:);
    
    %index the options over both blocks
    %index_best = find(subset_data.stim_rank==1);
    %index_middle = find(subset_data.stim_rank==2 | subset_data.stim_rank==3);
    %index_worst = find(subset_data.stim_rank==4);

    %remove final entry as won't be a subsequent trial for this one
    %if ((max(index_best) > max(index_middle)) & (max(index_best) > max(index_worst)))
    %    index_best(end)=[];
    %elseif ((max(index_middle) > max(index_best)) & (max(index_middle) > max(index_worst)))
    %    index_middle(end)=[];
    %elseif ((max(index_worst) > max(index_best)) & (max(index_worst) > max(index_middle)))    
    %    index_worst(end)=[];
    %end

    %now calculate acceptance rates of the proceeding trial - comes out odd
    %because of inbalance of trials, better to take average of each
    %seperately then averag those
    %accept_precedeAB_1back(s,1) = length(find(subset_data(index_best+1,:).choice_log_reg==1))/length(subset_data(index_best+1,:).choice_log_reg);
    %accept_precedeAB_1back(s,2) = length(find(subset_data(index_middle+1,:).choice_log_reg==1))/length(subset_data(index_middle+1,:).choice_log_reg);
    %accept_precedeAB_1back(s,3) = length(find(subset_data(index_worst+1,:).choice_log_reg==1))/length(subset_data(index_worst+1,:).choice_log_reg);
    
    %index the best options in block A - bit sloppy as use the same
    %variables above
    index_best = find(subset_data_A.stim_rank==1);
    index_middle = find(subset_data_A.stim_rank==2 | subset_data_A.stim_rank==3);
    index_worst = find(subset_data_A.stim_rank==4);

    %remove final entry as won't be a subsequent trial for this one
    if ((max(index_best) > max(index_middle)) & (max(index_best) > max(index_worst)))
        index_best(end)=[];
    elseif ((max(index_middle) > max(index_best)) & (max(index_middle) > max(index_worst)))
        index_middle(end)=[];
    elseif ((max(index_worst) > max(index_best)) & (max(index_worst) > max(index_middle)))    
        index_worst(end)=[];
    end

    %now calculate acceptance rates of the proceeding trial
    accept_precedeA_1back(s,1) = length(find(subset_data_A(index_best+1,:).choice_log_reg==1))/length(subset_data_A(index_best+1,:).choice_log_reg);
    accept_precedeA_1back(s,2) = length(find(subset_data_A(index_middle+1,:).choice_log_reg==1))/length(subset_data_A(index_middle+1,:).choice_log_reg);
    accept_precedeA_1back(s,3) = length(find(subset_data_A(index_worst+1,:).choice_log_reg==1))/length(subset_data_A(index_worst+1,:).choice_log_reg);

    %now calculate acceptance rates of the proceeding trial according to
    %what the current option is (e.g. how likely accept the best option based on whether
    %previous option was/was the best, etc.) 
    accept_precedeA_1back_best(s,1) = length(find(subset_data_A(index_best+1,:).choice_log_reg==1 & subset_data_A(index_best+1,:).stim_rank_factor==1))/length(find(subset_data_A(index_best+1,:).stim_rank_factor==1));
    accept_precedeA_1back_best(s,2) = length(find(subset_data_A(index_middle+1,:).choice_log_reg==1 & subset_data_A(index_middle+1,:).stim_rank_factor==1))/length(find(subset_data_A(index_middle+1,:).stim_rank_factor==1));
    accept_precedeA_1back_best(s,3) = length(find(subset_data_A(index_worst+1,:).choice_log_reg==1 & subset_data_A(index_worst+1,:).stim_rank_factor==1))/length(find(subset_data_A(index_worst+1,:).stim_rank_factor==1));

    accept_precedeA_1back_intermediate(s,1) = length(find(subset_data_A(index_best+1,:).choice_log_reg==1 & subset_data_A(index_best+1,:).stim_rank_factor==0))/length(find(subset_data_A(index_best+1,:).stim_rank_factor==0));
    accept_precedeA_1back_intermediate(s,2) = length(find(subset_data_A(index_middle+1,:).choice_log_reg==1 & subset_data_A(index_middle+1,:).stim_rank_factor==0))/length(find(subset_data_A(index_middle+1,:).stim_rank_factor==0));
    accept_precedeA_1back_intermediate(s,3) = length(find(subset_data_A(index_worst+1,:).choice_log_reg==1 & subset_data_A(index_worst+1,:).stim_rank_factor==0))/length(find(subset_data_A(index_worst+1,:).stim_rank_factor==0));

    accept_precedeA_1back_worst(s,1) = length(find(subset_data_A(index_best+1,:).choice_log_reg==1 & subset_data_A(index_best+1,:).stim_rank_factor==-1))/length(find(subset_data_A(index_best+1,:).stim_rank_factor==-1));
    accept_precedeA_1back_worst(s,2) = length(find(subset_data_A(index_middle+1,:).choice_log_reg==1 & subset_data_A(index_middle+1,:).stim_rank_factor==-1))/length(find(subset_data_A(index_best+1,:).stim_rank_factor==-1));
    accept_precedeA_1back_worst(s,3) = length(find(subset_data_A(index_worst+1,:).choice_log_reg==1 & subset_data_A(index_worst+1,:).stim_rank_factor==-1))/length(find(subset_data_A(index_best+1,:).stim_rank_factor==-1));

    %remove first entry as won't be a previous trial for this one
    if ((min(index_best) < min(index_middle)) & (min(index_best) < min(index_worst)))
        index_best(1)=[];
    elseif ((min(index_middle) < min(index_best)) & (min(index_middle) < min(index_worst)))
        index_middle(1)=[];
    elseif ((min(index_worst) < min(index_best)) & (min(index_worst) < min(index_middle)))    
        index_worst(1)=[];
    end

    %now calculate acceptance rates of the proceeding trial based
    %on past two options
    accept_precedeA_2back(s,1) = subset_data_A.order_condition(1);

    accept_precedeA_2back(s,2) = length(find(subset_data_A(index_best+1,:).choice_log_reg==1 & subset_data_A(index_best-1,:).stim_rank_factor==1))/length(find(subset_data_A(index_best-1,:).stim_rank_factor==1));
    accept_precedeA_2back(s,3) = length(find(subset_data_A(index_best+1,:).choice_log_reg==1 & subset_data_A(index_best-1,:).stim_rank_factor==0))/length(find(subset_data_A(index_best-1,:).stim_rank_factor==0));
    accept_precedeA_2back(s,4) = length(find(subset_data_A(index_best+1,:).choice_log_reg==1 & subset_data_A(index_best-1,:).stim_rank_factor==-1))/length(find(subset_data_A(index_best-1,:).stim_rank_factor==-1));

    accept_precedeA_2back(s,5) = length(find(subset_data_A(index_middle+1,:).choice_log_reg==1 & subset_data_A(index_middle-1,:).stim_rank_factor==1))/length(find(subset_data_A(index_middle-1,:).stim_rank_factor==1));
    accept_precedeA_2back(s,6) = length(find(subset_data_A(index_middle+1,:).choice_log_reg==1 & subset_data_A(index_middle-1,:).stim_rank_factor==0))/length(find(subset_data_A(index_middle-1,:).stim_rank_factor==0));
    accept_precedeA_2back(s,7) = length(find(subset_data_A(index_middle+1,:).choice_log_reg==1 & subset_data_A(index_middle-1,:).stim_rank_factor==-1))/length(find(subset_data_A(index_middle-1,:).stim_rank_factor==-1));

    accept_precedeA_2back(s,8) = length(find(subset_data_A(index_worst+1,:).choice_log_reg==1 & subset_data_A(index_worst-1,:).stim_rank_factor==1))/length(find(subset_data_A(index_worst-1,:).stim_rank_factor==1));
    accept_precedeA_2back(s,9) = length(find(subset_data_A(index_worst+1,:).choice_log_reg==1 & subset_data_A(index_worst-1,:).stim_rank_factor==0))/length(find(subset_data_A(index_worst-1,:).stim_rank_factor==0));
    accept_precedeA_2back(s,10) = length(find(subset_data_A(index_worst+1,:).choice_log_reg==1 & subset_data_A(index_worst-1,:).stim_rank_factor==-1))/length(find(subset_data_A(index_worst-1,:).stim_rank_factor==-1));

    %bit lazy as you're overwriting variables above
    index_best = find(subset_data_B.stim_rank==1);
    index_middle = find(subset_data_B.stim_rank==2 | subset_data_B.stim_rank==3);
    index_worst = find(subset_data_B.stim_rank==4);

    %remove final entry as won't be a subsequent trial for this one
    if ((max(index_best) > max(index_middle)) & (max(index_best) > max(index_worst)))
        index_best(end)=[];
    elseif ((max(index_middle) > max(index_best)) & (max(index_middle) > max(index_worst)))
        index_middle(end)=[];
    elseif ((max(index_worst) > max(index_best)) & (max(index_worst) > max(index_middle)))    
        index_worst(end)=[];
    end

    accept_precedeB_1back(s,1) = length(find(subset_data_B(index_best+1,:).choice_log_reg==1))/length(subset_data_B(index_best+1,:).choice_log_reg);
    accept_precedeB_1back(s,2) = length(find(subset_data_B(index_middle+1,:).choice_log_reg==1))/length(subset_data_B(index_middle+1,:).choice_log_reg);
    accept_precedeB_1back(s,3) = length(find(subset_data_B(index_worst+1,:).choice_log_reg==1))/length(subset_data_B(index_worst+1,:).choice_log_reg);

    %now calculate acceptance rates of the proceeding trial
    accept_precedeB_1back_best(s,1) = length(find(subset_data_B(index_best+1,:).choice_log_reg==1 & subset_data_B(index_best+1,:).stim_rank_factor==1))/length(find(subset_data_B(index_best+1,:).stim_rank_factor==1));
    accept_precedeB_1back_best(s,2) = length(find(subset_data_B(index_middle+1,:).choice_log_reg==1 & subset_data_B(index_middle+1,:).stim_rank_factor==1))/length(find(subset_data_B(index_middle+1,:).stim_rank_factor==1));
    accept_precedeB_1back_best(s,3) = length(find(subset_data_B(index_worst+1,:).choice_log_reg==1 & subset_data_B(index_worst+1,:).stim_rank_factor==1))/length(find(subset_data_B(index_worst+1,:).stim_rank_factor==1));

    accept_precedeB_1back_intermediate(s,1) = length(find(subset_data_B(index_best+1,:).choice_log_reg==1 & subset_data_B(index_best+1,:).stim_rank_factor==0))/length(find(subset_data_B(index_best+1,:).stim_rank_factor==0));
    accept_precedeB_1back_intermediate(s,2) = length(find(subset_data_B(index_middle+1,:).choice_log_reg==1 & subset_data_B(index_middle+1,:).stim_rank_factor==0))/length(find(subset_data_B(index_middle+1,:).stim_rank_factor==0));
    accept_precedeB_1back_intermediate(s,3) = length(find(subset_data_B(index_worst+1,:).choice_log_reg==1 & subset_data_B(index_worst+1,:).stim_rank_factor==0))/length(find(subset_data_B(index_worst+1,:).stim_rank_factor==0));

    accept_precedeB_1back_worst(s,1) = length(find(subset_data_B(index_best+1,:).choice_log_reg==1 & subset_data_B(index_best+1,:).stim_rank_factor==-1))/length(find(subset_data_B(index_best+1,:).stim_rank_factor==-1));
    accept_precedeB_1back_worst(s,2) = length(find(subset_data_B(index_middle+1,:).choice_log_reg==1 & subset_data_B(index_middle+1,:).stim_rank_factor==-1))/length(find(subset_data_B(index_best+1,:).stim_rank_factor==-1));
    accept_precedeB_1back_worst(s,3) = length(find(subset_data_B(index_worst+1,:).choice_log_reg==1 & subset_data_B(index_worst+1,:).stim_rank_factor==-1))/length(find(subset_data_B(index_best+1,:).stim_rank_factor==-1));
    
    %remove first entry as won't be a previous trial for this one
    if ((min(index_best) < min(index_middle)) & (min(index_best) < min(index_worst)))
        index_best(1)=[];
    elseif ((min(index_middle) < min(index_best)) & (min(index_middle) < min(index_worst)))
        index_middle(1)=[];
    elseif ((min(index_worst) < min(index_best)) & (min(index_worst) < min(index_middle)))    
        index_worst(1)=[];
    end

    %now calculate acceptance rates of the proceeding trial based
    %on past two options
    accept_precedeB_2back(s,1) = subset_data_B.order_condition(1);

    accept_precedeB_2back(s,2) = length(find(subset_data_B(index_best+1,:).choice_log_reg==1 & subset_data_B(index_best-1,:).stim_rank_factor==1))/length(find(subset_data_B(index_best-1,:).stim_rank_factor==1));
    accept_precedeB_2back(s,3) = length(find(subset_data_B(index_best+1,:).choice_log_reg==1 & subset_data_B(index_best-1,:).stim_rank_factor==0))/length(find(subset_data_B(index_best-1,:).stim_rank_factor==0));
    accept_precedeB_2back(s,4) = length(find(subset_data_B(index_best+1,:).choice_log_reg==1 & subset_data_B(index_best-1,:).stim_rank_factor==-1))/length(find(subset_data_B(index_best-1,:).stim_rank_factor==-1));

    accept_precedeB_2back(s,5) = length(find(subset_data_B(index_middle+1,:).choice_log_reg==1 & subset_data_B(index_middle-1,:).stim_rank_factor==1))/length(find(subset_data_B(index_middle-1,:).stim_rank_factor==1));
    accept_precedeB_2back(s,6) = length(find(subset_data_B(index_middle+1,:).choice_log_reg==1 & subset_data_B(index_middle-1,:).stim_rank_factor==0))/length(find(subset_data_B(index_middle-1,:).stim_rank_factor==0));
    accept_precedeB_2back(s,7) = length(find(subset_data_B(index_middle+1,:).choice_log_reg==1 & subset_data_B(index_middle-1,:).stim_rank_factor==-1))/length(find(subset_data_B(index_middle-1,:).stim_rank_factor==-1));

    accept_precedeB_2back(s,8) = length(find(subset_data_B(index_worst+1,:).choice_log_reg==1 & subset_data_B(index_worst-1,:).stim_rank_factor==1))/length(find(subset_data_B(index_worst-1,:).stim_rank_factor==1));
    accept_precedeB_2back(s,9) = length(find(subset_data_B(index_worst+1,:).choice_log_reg==1 & subset_data_B(index_worst-1,:).stim_rank_factor==0))/length(find(subset_data_B(index_worst-1,:).stim_rank_factor==0));
    accept_precedeB_2back(s,10) = length(find(subset_data_B(index_worst+1,:).choice_log_reg==1 & subset_data_B(index_worst-1,:).stim_rank_factor==-1))/length(find(subset_data_B(index_worst-1,:).stim_rank_factor==-1));

end

%% save trial by trial data to run in R lme4
writetable(data, 'data_for_lme4.csv')

%% augment summary stats and resave as csv

%split out variables you want to add
%accept_AB_prev1 = accept_precedeAB_1back(:,1);
%accept_AB_prev23 = accept_precedeAB_1back(:,2);
%accept_AB_prev4 = accept_precedeAB_1back(:,3);

accept_AB_prev1 = (accept_precedeA_1back(:,1) + accept_precedeB_1back(:,1))/2;
accept_AB_prev23 = (accept_precedeA_1back(:,2) + accept_precedeB_1back(:,2))/2;
accept_AB_prev4 =  (accept_precedeA_1back(:,3) + accept_precedeB_1back(:,3))/2;

% in general accept cuurent option (whatever it is) based on what the previous option was 
accept_A_prev1 = accept_precedeA_1back(:,1);
accept_A_prev23 = accept_precedeA_1back(:,2);
accept_A_prev4 = accept_precedeA_1back(:,3);

% accept best option only based on what the previous option was 
accept_A1_prev1 = accept_precedeA_1back_best(:,1);
accept_A1_prev23 = accept_precedeA_1back_best(:,2);
accept_A1_prev4 = accept_precedeA_1back_best(:,3);

% accept intermediate options only based on what the previous option was 
accept_A23_prev1 = accept_precedeA_1back_intermediate(:,1);
accept_A23_prev23 = accept_precedeA_1back_intermediate(:,2);
accept_A23_prev4 = accept_precedeA_1back_intermediate(:,3);

% accept worst option only based on what the previous option was 
accept_A4_prev1 = accept_precedeA_1back_worst(:,1);
accept_A4_prev23 = accept_precedeA_1back_worst(:,2);
accept_A4_prev4 = accept_precedeA_1back_worst(:,3);

accept_B_prev1 = accept_precedeB_1back(:,1);
accept_B_prev23 = accept_precedeB_1back(:,2);
accept_B_prev4 = accept_precedeB_1back(:,3);

accept_B1_prev1 = accept_precedeB_1back_best(:,1);
accept_B1_prev23 = accept_precedeB_1back_best(:,2);
accept_B1_prev4 = accept_precedeB_1back_best(:,3);

accept_B23_prev1 = accept_precedeB_1back_intermediate(:,1);
accept_B23_prev23 = accept_precedeB_1back_intermediate(:,2);
accept_B23_prev4 = accept_precedeB_1back_intermediate(:,3);

accept_B4_prev1 = accept_precedeB_1back_worst(:,1);
accept_B4_prev23 = accept_precedeB_1back_worst(:,2);
accept_B4_prev4 = accept_precedeB_1back_worst(:,3);

%average reward estimates
av_reward_est_A = av_reward_compile_single(:,1);
av_reward_est_B = av_reward_compile_single(:,2);
av_reward_est_both = av_reward_compile_single(:,3);

av_reward_arithmetic_A = av_reward_arithmetic(:,1);
av_reward_arithmetic_B = av_reward_arithmetic(:,2);
av_reward_arithmetic_both = av_reward_arithmetic(:,3);
    
%put into tables (names will be the names of the variables)
csv_summary = table(accept_AB_prev1, accept_AB_prev23, accept_AB_prev4,...
    accept_A_prev1, accept_A_prev23, accept_A_prev4,...
    accept_B_prev1, accept_B_prev23, accept_B_prev4,...
    accept_A1_prev1, accept_A1_prev23, accept_A1_prev4,...
    accept_A23_prev1, accept_A23_prev23, accept_A23_prev4,...
    accept_A4_prev1, accept_A4_prev23, accept_A4_prev4,...
    accept_B1_prev1, accept_B1_prev23, accept_B1_prev4,...
    accept_B23_prev1, accept_B23_prev23, accept_B23_prev4,...
    accept_B4_prev1, accept_B4_prev23, accept_B4_prev4,...    
    av_reward_est_A, av_reward_est_B, av_reward_est_both,...
    av_reward_arithmetic_A, av_reward_arithmetic_B, av_reward_arithmetic_both);

%add to existing summary stats
csv_summary = [summary_stats csv_summary];

%save
writetable(csv_summary, 'summary_stats2.csv')

%% clear variables 
clearvars -except csv_summary data

