function [comp, percentages, arithmetic_r] = model_run(options, time_per_block, inital_Q, intercept, beta, learning_rate_reward, learning_rate_delay)

    time_counter = 1;
    n_trials_per_set = length(options);
    Q = inital_Q;
    comp = [];
    
    sum_reward = 0;
    arithmetic_r = 0;

while time_counter < time_per_block
    
    %initalise
    options_rand = [];
    
    %determine ordering of options in this set
    trial_order = randperm(n_trials_per_set);
    options_rand = options(trial_order, :);
        
    %loop over trials for this set
    for trials = 1:n_trials_per_set
        %Q_store = Q;
        %arithmetic_r = [arithmetic_r; sum_reward/time_counter];

        %update Q based on 
        Q = (1-learning_rate_delay)*Q + 0;
        Q = (1-learning_rate_delay)*Q + 0;

        %update time counter by 2 seconds
        time_counter = time_counter + 2;
            
        option_rank = options_rand(trials, 1);
        option_reward = options_rand(trials, 2);
        option_delay = options_rand(trials, 3);

        %calculate opportunity cost of accepting
        opp_cost = option_delay*Q;
        
        %probability of accepting
        prob_accept = exp(beta*option_reward)/(exp(beta*option_reward) + exp(intercept+beta*opp_cost));
        
        %force trial?
        force = 0.25>rand;
        
        %if force trial, 50% chance could be force accept, 50% chance could
        %be force reject
        if force  
           accept = 0.5>rand;
        else
           accept = prob_accept>rand;
        end
        
        arithmetic_r = [arithmetic_r; sum_reward/time_counter];
        
        %store all this
        comp =  [comp; [option_rank, force, option_reward, option_delay, Q, opp_cost, accept, prob_accept]];
        
        %update Q if accept option (regardless of whether force trial or
        %not)
        if accept
            
            for d = 1:option_delay
                
                Q = (1-learning_rate_delay)*Q + 0;
                
            end
            
            %update time counter by n seconds taken to collect the reward
            time_counter = time_counter + option_delay;
            
            % update Q for reward recieved
            Q = (1-learning_rate_reward)*Q + learning_rate_reward*option_reward;
            
            sum_reward = sum_reward + option_reward;

        else
        end
        
    end

    
end

for rank = 1:4
    
    accept = comp(find(comp(:, 1)==rank & comp(:, 2)==0), 7);
    percentages(rank, 1) = sum(accept)/length(accept);
    
    
end

accept = comp(find((comp(:, 1)==2 | comp(:, 1)==3) & comp(:, 2)==0),7);
percentages(5, 1) = sum(accept)/length(accept);

end
