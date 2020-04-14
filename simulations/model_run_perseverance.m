function [comp, percentages] = model_run(options, time_per_block, inital_Q, intercept, beta, learning_rate_reward, learning_rate_delay, persev)

    time_counter = 1;
    n_trials_per_set = length(options);
    Q = inital_Q;
    comp = [];
    
    c_prev = NaN;
    
while time_counter < time_per_block
    
    %initalise
    options_rand = [];
    
    %determine ordering of options in this set
    trial_order = randperm(n_trials_per_set);
    options_rand = options(trial_order, :);
        
    %loop over trials for this set
    for trials = 1:n_trials_per_set

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
        
        if ~isnan(c_prev)
            if (c_prev==1)
                %probability of accepting
                prob_accept = exp(beta*option_reward+persev)/(exp(beta*option_reward+persev) + exp(intercept+beta*opp_cost));
            elseif (c_prev==2)
                prob_accept = exp(beta*option_reward)/(exp(beta*option_reward) + exp(intercept+beta*opp_cost+persev));            
            end
         else
                prob_accept = exp(beta*option_reward)/(exp(beta*option_reward) + exp(intercept+beta*opp_cost));
         end
            
        %force trial?
        force = 0.25>rand;
        
        %if force trial, 50% chance could be force accept, 50% chance could
        %be force reject
        if force  
           accept = 0.5>rand;
        else
           accept = prob_accept>rand;
        end
                
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
            
            c_prev = 1;
            
        else
            
            c_prev = 2;
            
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
