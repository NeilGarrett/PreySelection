# julia genVars function


function genVars(df,np)
    data = df;
    subs = unique(data[:sub]);
    NS = length(subs);
    try
       if isdefined(data[:step2state])
          if maximum(data[:step2state]).==3
              data[:step2state]+=-1;
          end
          data[:c2] = data[:step2resp];
          data[:s] = data[:step2state];
          data[:r] = (data[:reward].*2)-1;
      end
    end

    if np ==4
        try
            X = designmatrix(Any[[ones(NS)] [ones(NS)] [ones(NS)] [ones(NS)]]);
            betas = flatten(Any[[0.] [0.] [0.] [0.]]);
            sigma = [1.,1.,1.,1.];
        catch
            X = designmatrix([ones(NS), ones(NS), ones(NS), ones(NS)]);;
            betas = flatten([0. 0. 0. 0.]);
            sigma = [1.,1.,1.,1.];
        end
    elseif np==3
        X = designmatrix(Any[[ones(NS)] [ones(NS)] [ones(NS)]]);
        betas = flatten(Any[[0.] [0.] [0.]]);
        sigma = [1.,1.,1.];
    elseif np==2
        X = designmatrix(Any[[ones(NS)] [ones(NS)]]);
        betas = flatten(Any[[0.] [0.]]);
        sigma = [1.,1.];
    elseif np==1
        X = designmatrix(Any[ones(NS)]);
        betas = flatten(Any[0.]);
        sigma = [1.];
    elseif np ==5
        try
            X = designmatrix(Any[[ones(NS)] [ones(NS)] [ones(NS)] [ones(NS)] [ones(NS)]]);
            betas = flatten(Any[[0.] [0.] [0.] [0.] [0.]]);
            sigma = [1.,1.,1.,1.,1.];
        catch
            X = designmatrix([ones(NS), ones(NS), ones(NS), ones(NS), ones(NS)]);;
            betas = flatten([0. 0. 0. 0. 0.]);
            sigma = [1.,1.,1.,1.,1.];
        end
    elseif np ==6
        try
            X = designmatrix(Any[[ones(NS)] [ones(NS)] [ones(NS)] [ones(NS)] [ones(NS)] [ones(NS)]]);
            betas = flatten(Any[[0.] [0.] [0.] [0.] [0.] [0.]]);
            sigma = [1.,1.,1.,1.,1.,1.];
        catch
            X = designmatrix([ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS)]);;
            betas = flatten([0. 0. 0. 0. 0. 0.]);
            sigma = [1.,1.,1.,1.,1.,1.];
        end
    elseif np ==7
        try
            X = designmatrix(Any[[ones(NS)] [ones(NS)] [ones(NS)] [ones(NS)] [ones(NS)] [ones(NS)] [ones(NS)]]);
            betas = flatten(Any[[0.] [0.] [0.] [0.] [0.] [0.] [0.]]);
            sigma = [1.,1.,1.,1.,1.,1.,1.];
        catch
            X = designmatrix([ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS) , ones(NS)]);;
            betas = flatten([0. 0. 0. 0. 0. 0. 0.]);
            sigma = [1.,1.,1.,1.,1.,1.,1.];
        end
    elseif np ==8
        try
            X = designmatrix(Any[ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS)]);
            betas =  flatten(Any[[0.] [0.] [0.] [0.] [0.] [0.] [0.][0.]]);
            sigma = [1.,1.,1.,1.,1.,1.,1.,1.];
        catch
            X = designmatrix([ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS)]);
            betas =  flatten([0. 0. 0. 0. 0. 0. 0. 0.]);;
            sigma = [1.,1.,1.,1.,1.,1.,1.,1.];
        end
    elseif np ==9
        try
            X = designmatrix(Any[ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS)]);
            betas =  flatten(Any[[0.] [0.] [0.] [0.] [0.] [0.] [0.] [0.] [0.]]);
            sigma = [1.,1.,1.,1.,1.,1.,1.,1.,.1];
        catch
            X = designmatrix([ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS)]);
            betas =  flatten([0. 0. 0. 0. 0. 0. 0. 0. 0.]);
            sigma = [1.,1.,1.,1.,1.,1.,1.,1.,.1];
        end
    elseif np ==10    
        try
            X = designmatrix(Any[ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS)]);
            betas =  flatten(Any[[0.] [0.] [0.] [0.] [0.] [0.] [0.] [0.] [0.] [0.]]);
            sigma = [1.,1.,1.,1.,1.,1.,1.,1.,.1,.1];
        catch
            X = designmatrix([ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS), ones(NS)]);
            betas =  flatten([0. 0. 0. 0. 0. 0. 0. 0. 0. 0.]);;
            sigma = [1.,1.,1.,1.,1.,1.,1.,1.,.1,.1];
        end
        
    end
    return (data,subs,X,betas,sigma)

end