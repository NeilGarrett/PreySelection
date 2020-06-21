%% simulations for prey selection task
% N GARRETT, June 2018

%% clear everything, screen etc.
clear all;
close all;
clc;

%% variables to check/change before running
%change this to run the relevant experiment (103/104/105)
experiment_v = input('experiment version (1=103, 2=104): ');

%number of learning rates
n_eta = input('n learning rates (1=symmetric model; 2=asymmetric model): ');

%number of subs per run
n_iterations = 40;

%number of runs
n_sims = 100;

%initial Q
if experiment_v==103
    inital_Q = 8.21; %exp 1else

elseif experiment_v==104
    inital_Q = 7.77; %exp 2
end

%rank, reward, delay of each of the 4 options in each block
block_A_options = [1, 80, 2; 1, 80, 2; 1, 80, 2; 1, 80, 2; 2, 20, 2; 3, 80, 8; 4, 20, 8];
block_B_options = [1, 80, 2; 2, 20, 2; 3, 80, 8; 4, 20, 8; 4, 20, 8; 4, 20, 8; 4, 20, 8];

%time (in seconds) for each block 
if experiment_v==103
    time_per_block = 900;
else
    time_per_block = 600; 
end

%%
fs = filesep;

experiment_folder = ['v' num2str(experiment_v)];

%paths to where folders are
prey_selection_path = ['/Users/neil/GitHubRepo/Projects/PreySelection/' experiment_folder '/models'];

if n_eta ==1
    model_path = [prey_selection_path fs 'model_symmetric'];
    %what to save mat file as
    output_filename = [model_path fs 'simulation_dat_symmetric.mat'];
elseif n_eta ==2
    model_path = [prey_selection_path fs 'model_asymmetric'];
    output_filename = [model_path fs 'simulation_dat_asymmetric.mat'];
end

%% load in data

%load in parameters fit by model
params = readtable([model_path fs 'subject_params.csv'],'TreatAsEmpty', {'NA', '.', ''});

n_subs = length(params.intercept);

%initalise table for storing 
richpoor = NaN*ones(n_sims, 1);
poorrich = NaN*ones(n_sims, 1);

acceptance_change_comp = table(richpoor, poorrich);

for sims = 1:n_sims
    
    %% run for set of subs
    for iterations = 1:n_iterations

        %initalise Q
        Q = inital_Q;

        %pick random set of parameters
        sub = randperm(n_subs,1);

        %pick AB/BA condition (1=AB; 2=BA)
        condition = randperm(2,1);
        
        %pick out parameters for this sub
        intercept = params(sub,:).intercept;
        beta = params(sub,:).beta;
        
        if n_eta ==1
            lr_reward = 0.5 + 0.5*erf(params(sub,:).learning_rate_raw/sqrt(2));
            lr_delay = 0.5 + 0.5*erf(params(sub,:).learning_rate_raw/sqrt(2));
        elseif n_eta ==2
            lr_reward = 0.5 + 0.5*erf(params(sub,:).learning_rate_raw_pos/sqrt(2)); 
            lr_delay = 0.5 + 0.5*erf(params(sub,:).learning_rate_raw_neg/sqrt(2));
        end
                
        % run experiment in order contignent on condition
        if (condition == 1)
            
            [comp_A, percentages_A, arithmetic_A] = model_run(block_A_options, time_per_block, Q, intercept, beta, lr_reward, lr_delay);
               
            % take Q from last trial of previous block (A for these subs)
            Q = comp_A(end,5);
            

            [comp_B, percentages_B, arithmetic_B] = model_run(block_B_options, time_per_block, Q, intercept, beta, lr_reward, lr_delay);

        elseif (condition == 2)
            
            [comp_B, percentages_B, arithmetic_B] = model_run(block_B_options, time_per_block, Q, intercept, beta, lr_reward, lr_delay);

            % take Q from last trial of previous block (B for these subs)
            Q = comp_B(end,5);

            [comp_A, percentages_A, arithmetic_A] = model_run(block_A_options, time_per_block, Q, intercept, beta, lr_reward, lr_delay);

        end
        
        acceptance_change(iterations, 1) = condition;        
        acceptance_change(iterations, 2) = mean(percentages_B(1:4, 1)) - mean(percentages_A(1:4, 1));

    end


% %store
acceptance_change_comp.richpoor(sims) = mean(acceptance_change(find(acceptance_change==1), 2));
acceptance_change_comp.poorrich(sims) = mean(acceptance_change(find(acceptance_change==2), 2));

end

%save simulations dat to mat file
%save(output_filename, 'acceptance_change_comp');
mean(acceptance_change_comp.poorrich)
mean(acceptance_change_comp.richpoor)

