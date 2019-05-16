
%useful:
%http://genomicsclass.github.io/book/pages/interactions_and_contrasts.html

cov_mat = [0.1434301684,	0.0000238565,	-0.0096344706,	0.0104918055;
0.0000238565,	0.0000204126,	-0.0001056018,	-0.0000043363;
-0.0096344706,	-0.0001056018,	0.0085739069,	0.0074753530;
0.0104918055,	-0.0000043363,	0.0074753530,	0.0113054426]
    
%beta_mat = [-1.787259432, 0.07594555,  -2.58138733, -2.979877288];

con_vec = [0 0 1 -1];
%con_vec = [0 1 0 0];
%con_vec = [0 0 1 0];

N=41

%std_error calculate - from this can then get a z statistic 
std_error = sqrt(con_vec*cov_mat*con_vec')

%convert std_error to deviation
std_dev = std_error*sqrt(N)

intercept_av = -1.787259432
beta_av = 0.07594555

%mean estimates
lreward_av = -2.58138733
%lreward_av = 0.009730807

ldelay_av = -2.979877288
%ldelay_av = 0.003597816

%difference in lr reward and lr delay
diff = lreward_av - ldelay_av;

%covert to t stat (estimate over standard error)
t_stat = diff./std_error

%can then do (in r):
%2*(1-pnorm(t_stat))
 
%or use an online tool like this:
%https://www.medcalc.org/calc/test_one_mean.php
