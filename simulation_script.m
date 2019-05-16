%% simulations for prey selection task v103
% N GARRETT, June 2018

%% clear everything, screen etc.
clear all;
close all;
clc;

%% variables to check/change before running
%change this to run the relevant experiment (103/104/105)
experiment_v = 103

%number of learning rates
n_eta = 1

%number of subs per run
n_iterations = 40;

%number of runs
n_sims = 1000;
%n_sims = 11;

%initial Q

%time (in seconds) for each block 
if experiment_v==103
    inital_Q = 8.21; %exp 1else
elseif experiment_v==104
    inital_Q = 7.77; %exp 2
elseif experiment_v==105
    inital_Q = 7.02; %exp 3
end

%rank, reward, delay of each of the 4 options in each block
block_A_options = [1, 80, 2; 1, 80, 2; 1, 80, 2; 1, 80, 2; 2, 20, 2; 3, 80, 8; 4, 20, 8];
block_B_options = [1, 80, 2; 2, 20, 2; 3, 80, 8; 4, 20, 8; 4, 20, 8; 4, 20, 8; 4, 20, 8];
block_C_options = [4, 20, 8; 4, 20, 8; 4, 20, 8; 4, 20, 8; 4, 20, 8; 4, 20, 8; 4, 20, 8];

%time (in seconds) for each block 
if experiment_v==103
    time_per_block = 900;
else
    time_per_block = 600; %check this
end


%%
fs = filesep;

experiment_folder = ['v' num2str(experiment_v)];

%paths to where folders are
prey_selection_path = ['/Users/neil/GitHubRepo/Projects/PreySelection']

%cd to current folder where must have function that runs simulation
cd([prey_selection_path])

experiment_path = [prey_selection_path fs experiment_folder fs 'models'] 

if n_eta ==1
    model_path = [experiment_path fs 'model_MVT'];
    %what to save csv file as
    output_filename = [model_path fs 'simulation_dat_MVT.csv'];
    output_filename_ttest = [model_path fs 'simulation_MVT_ttest.csv'];
    
elseif n_eta ==2
    %change this to run 1 or 2lr model
    model_path = [experiment_path fs 'model_asymmetry'];
    %what to save csv file as
    output_filename = [model_path fs 'simulation_dat_asymmetry.csv'];
    output_filename_ttest = [model_path fs 'simulation_asymmetry_ttest.csv'];

end

%% load in data

%load in parameters fit by model
params = readtable([model_path fs 'subject_params.csv'],'TreatAsEmpty', {'NA', '.', ''});

n_subs = length(params.intercept);

for sims = 1:n_sims
    
    %% run for set of subs
    for iterations = 1:n_iterations

        %initalise Q
        Q = inital_Q;

        %pick random sub
        sub = randperm(n_subs,1);

        %pick AB/BA condition (1=AB; 2=BA)
        condition = randperm(2,1);
        
        %pick out parameters for this sub
        intercept = params(sub,:).intercept;
        beta = params(sub,:).beta;
        
        if n_eta ==1
            lr_reward = params(sub,:).learning_rate_transformed;
            lr_delay = params(sub,:).learning_rate_transformed;
        elseif n_eta ==2
            lr_reward = params(sub,:).learning_rate_reward_transformed;
            lr_delay = params(sub,:).learning_rate_delay_transformed;
        end
        
        % run experiment in order contignent on condition
        if (condition == 1)
            
            [comp_A, percentages_A, arithmetic_A] = model_run(block_A_options, time_per_block, Q, intercept, beta, lr_reward, lr_delay);
               
            % take Q from last trial of previous block (A for these subs)
            Q = comp_A(end,5);
            
            if experiment_v == 105
                [comp_C, percentages_C, arithmetic_C] = model_run(block_C_options, time_per_block, Q, intercept, beta, lr_reward, lr_delay);
            Q = comp_C(end,5);
            else
            end

            
            
            [comp_B, percentages_B, arithmetic_B] = model_run(block_B_options, time_per_block, Q, intercept, beta, lr_reward, lr_delay);

        elseif (condition == 2)
            
            [comp_B, percentages_B, arithmetic_B] = model_run(block_B_options, time_per_block, Q, intercept, beta, lr_reward, lr_delay);

            % take Q from last trial of previous block (B for these subs)
            Q = comp_B(end,5);

            if experiment_v == 105
                [comp_C, percentages_C, arithmetic_C] = model_run(block_C_options, time_per_block, Q, intercept, beta, lr_reward, lr_delay);
            Q = comp_C(end,5);
            else
            end

            
            
            [comp_A, percentages_A, arithmetic_A] = model_run(block_A_options, time_per_block, Q, intercept, beta, lr_reward, lr_delay);

        end
        
        %compile percentage acceptance rates for each block also store
        %condition
        blockA(iterations, 1) = condition;
        blockA(iterations, 2:6) = percentages_A;
        
        blockB(iterations, 1) = condition;
        blockB(iterations, 2:6) = percentages_B;

    end

%now compile mean acceptance rates over the subs...

%for all subs combined
allsubs_blockA_means(sims,1:5) = nanmean(blockA(:,2:6));
allsubs_blockB_means(sims,1:5) = nanmean(blockB(:,2:6));

%for AB subs
ABsubs_blockA_means(sims,1:5) = nanmean(blockA(find(blockA(:,1)==1),2:6));
ABsubs_blockB_means(sims,1:5) = nanmean(blockB(find(blockB(:,1)==1),2:6));

%for BA subs
BAsubs_blockA_means(sims,1:5) = nanmean(blockA(find(blockA(:,1)==2),2:6));
BAsubs_blockB_means(sims,1:5) = nanmean(blockB(find(blockB(:,1)==2),2:6));

%ttests comparing the difference in acceptance rates
ttest_comp_g1 = mean((blockB(find(blockB(:,1)==1),2:5) - blockA(find(blockA(:,1)==1),2:5))')';   
ttest_comp_g2 = mean((blockB(find(blockB(:,1)==2),2:5) - blockA(find(blockA(:,1)==2),2:5))')'; 

%store the result of this test
ttest_result(sims, 1) = ttest2(ttest_comp_g1, ttest_comp_g2);

end

rank = repmat({'HRLD'; 'LRLD'; 'HRHD'; 'LRHD'; 'intermediate'}, 6, 1);
   
block = repmat({'rich'; 'rich'; 'rich'; 'rich'; 'rich'; 'poor'; 'poor'; 'poor'; 'poor'; 'poor'}, 3, 1);

order_condition = {'all'; 'all'; 'all'; 'all'; 'all'; 'all'; 'all'; 'all'; 'all'; 'all'; 
    'richpoor'; 'richpoor'; 'richpoor'; 'richpoor'; 'richpoor'; 'richpoor'; 'richpoor'; 'richpoor'; 'richpoor'; 'richpoor';
    'poorrich'; 'poorrich'; 'poorrich'; 'poorrich'; 'poorrich'; 'poorrich'; 'poorrich'; 'poorrich'; 'poorrich'; 'poorrich'}

acceptance_rates = [mean(allsubs_blockA_means)'; mean(allsubs_blockB_means)';
    mean(ABsubs_blockA_means)'; mean(ABsubs_blockB_means)';
    mean(BAsubs_blockA_means)'; mean(BAsubs_blockB_means)']

sem = [(std(allsubs_blockA_means)/sqrt(n_sims))'; (std(allsubs_blockB_means)/sqrt(n_sims)')';
    (std(ABsubs_blockA_means)/sqrt(n_sims))'; (std(ABsubs_blockB_means)/sqrt(n_sims))';
    (std(BAsubs_blockA_means)/sqrt(n_sims))'; (std(BAsubs_blockB_means)/sqrt(n_sims))']
 
AB_sims = nanmean((ABsubs_blockB_means(:,1:4) - ABsubs_blockA_means(:,1:4))')';
AB_sims_mean = mean(AB_sims)
AB_sims_sem = std(AB_sims)/sqrt(n_sims)

BA_sims = mean((BAsubs_blockB_means(:,1:4) - BAsubs_blockA_means(:,1:4))')';
BA_sims_mean = mean(BA_sims)
BA_sims_sem = std(BA_sims)/sqrt(n_sims)

%compile into table
%sims = table(rank, block, order_condition, acceptance_rates, sem)

ttest_result = table(ttest_result)

%save as csv
%writetable(sims, output_filename);
writetable(ttest_result, output_filename_ttest);


