* Encoding: UTF-8.
*first look at expeirment 1 - select just this exp

DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(study = 103).
VARIABLE LABELS filter_$ 'study = 103 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

* look at acceptance rates between blocks (intermediate options combined)

GLM percent_accept_A1 percent_accept_A2_A3 percent_accept_A4 percent_accept_B1_min_A1 
    percent_accept_B2_B3_min_A2_A3 percent_accept_B4_min_A4
  /WSFACTOR=block 2 Polynomial rank 3 Polynomial 
  /METHOD=SSTYPE(3)
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=block rank block*rank.

*ttests between blocks for each rank

T-TEST PAIRS=percent_accept_A1 percent_accept_A2_A3 percent_accept_A4 WITH percent_accept_B1 
    percent_accept_B2_B3 percent_accept_B4 (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

*compare change in acceptance rates between each block for each rank

T-TEST PAIRS=percent_accept_B2_B3_min_A2_A3 percent_accept_B2_B3_min_A2_A3 WITH 
    percent_accept_B1_min_A1 percent_accept_B4_min_A4 (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

*check same result ensues when put in rank as 4 levels (not collapsing the 2 intermediate options)

GLM percent_accept_A1 percent_accept_A2 percent_accept_A3 percent_accept_A4 
    percent_accept_B1_min_A1 percent_accept_B2_min_A2 percent_accept_B3_min_A3 percent_accept_B4_min_A4
  /WSFACTOR=block 2 Polynomial rank 4 Polynomial 
  /METHOD=SSTYPE(3)
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=block rank block*rank.

* examine whether odd result for change in acceptance rates being higher for best and worst options in the rich environment by comparing participants in the two order conditions

*compare difference in acceptance rates between participants

T-TEST GROUPS=order_condition(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=percent_accept_B1_min_A1 percent_accept_B4_min_A4
  /CRITERIA=CI(.95).

*see if significant for each group on their own

USE ALL.
COMPUTE filter_$=(study = 103  &  order_condition = 1).
VARIABLE LABELS filter_$ 'study = 103  &  order_condition = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST PAIRS=percent_accept_A1 percent_accept_A2_A3 percent_accept_A4 WITH percent_accept_B1 
    percent_accept_B2_B3 percent_accept_B4 (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

USE ALL.
COMPUTE filter_$=(study = 103  &  order_condition = 2).
VARIABLE LABELS filter_$ 'study = 103  &  order_condition = 2 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST PAIRS=percent_accept_A1 percent_accept_A2_A3 percent_accept_A4 WITH percent_accept_B1 
    percent_accept_B2_B3 percent_accept_B4 (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(study = 103).
VARIABLE LABELS filter_$ 'study = 103 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


*within trial effects

GLM accept_AB_prev1 accept_AB_prev23 accept_AB_prev4
  /WSFACTOR=rank_prev 3 Polynomial 
  /METHOD=SSTYPE(3)
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=rank_prev.

* ttests comparing each bin 

T-TEST PAIRS=accept_AB_prev1 accept_AB_prev23 accept_AB_prev1 WITH accept_AB_prev23 accept_AB_prev4 
    accept_AB_prev4 (PAIRED)
  /CRITERIA=CI(.9500)
  

T-TEST GROUPS=order_condition(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=average_change_B_min_A_rank_1_intermediate_4
  /CRITERIA=CI(.95).

USE ALL.
COMPUTE filter_$=(study = 103  &  order_condition = 1).
VARIABLE LABELS filter_$ 'study = 103  &  order_condition = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST
  /TESTVAL=0
  /MISSING=ANALYSIS
  /VARIABLES=average_change_B_min_A_rank_1_intermediate_4
  /CRITERIA=CI(.95).

USE ALL.
COMPUTE filter_$=(study = 103  &  order_condition = 2).
VARIABLE LABELS filter_$ 'study = 103  &  order_condition = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST
  /TESTVAL=0
  /MISSING=ANALYSIS
  /VARIABLES=average_change_B_min_A_rank_1_intermediate_4
  /CRITERIA=CI(.95).

USE ALL.
COMPUTE filter_$=(study = 103).
VARIABLE LABELS filter_$ 'study = 103  &  order_condition = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


T-TEST GROUPS=order_condition(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=average_change_B_min_A_rank_1_2_3_4
  /CRITERIA=CI(.95).

USE ALL.
COMPUTE filter_$=(study = 103  &  order_condition = 1).
VARIABLE LABELS filter_$ 'study = 103  &  order_condition = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST
  /TESTVAL=0
  /MISSING=ANALYSIS
  /VARIABLES=average_change_B_min_A_rank_1_2_3_4
  /CRITERIA=CI(.95).

USE ALL.
COMPUTE filter_$=(study = 103  &  order_condition = 2).
VARIABLE LABELS filter_$ 'study = 103  &  order_condition = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST
  /TESTVAL=0
  /MISSING=ANALYSIS
  /VARIABLES=average_change_B_min_A_rank_1_2_3_4
  /CRITERIA=CI(.95).


** -----------------------------------------------------------------------------------------------------------










** now run this analysis for 2nd Experiment

USE ALL.
COMPUTE filter_$=(study = 104).
VARIABLE LABELS filter_$ 'study = 104 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


* look at acceptance rates between blocks (intermediate options combined)

GLM percent_accept_A1 percent_accept_A2_A3 percent_accept_A4 percent_accept_B1_min_A1 
    percent_accept_B2_B3_min_A2_A3 percent_accept_B4_min_A4
  /WSFACTOR=block 2 Polynomial rank 3 Polynomial 
  /METHOD=SSTYPE(3)
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=block rank block*rank.

*ttests between blocks for each rank

T-TEST PAIRS=percent_accept_A1 percent_accept_A2_A3 percent_accept_A4 WITH percent_accept_B1 
    percent_accept_B2_B3 percent_accept_B4 (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

*compare change in acceptance rates between each block for each rank

T-TEST PAIRS=percent_accept_B2_B3_min_A2_A3 percent_accept_B2_B3_min_A2_A3 WITH 
    percent_accept_B1_min_A1 percent_accept_B4_min_A4 (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

*check same result ensues when put in rank as 4 levels (not collapsing the 2 intermediate options)

GLM percent_accept_A1 percent_accept_A2 percent_accept_A3 percent_accept_A4 
    percent_accept_B1_min_A1 percent_accept_B2_min_A2 percent_accept_B3_min_A3 percent_accept_B4_min_A4
  /WSFACTOR=block 2 Polynomial rank 4 Polynomial 
  /METHOD=SSTYPE(3)
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=block rank block*rank.

T-TEST GROUPS=order_condition(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=percent_accept_B1_min_A1 percent_accept_B4_min_A4
  /CRITERIA=CI(.95).

*see if significant for each group on their own

USE ALL.
COMPUTE filter_$=(study = 104  &  order_condition = 1).
VARIABLE LABELS filter_$ 'study = 104  &  order_condition = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST PAIRS=percent_accept_A1 percent_accept_A2_A3 percent_accept_A4 WITH percent_accept_B1 
    percent_accept_B2_B3 percent_accept_B4 (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

USE ALL.
COMPUTE filter_$=(study = 104  &  order_condition = 2).
VARIABLE LABELS filter_$ 'study = 104  &  order_condition = 2 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST PAIRS=percent_accept_A1 percent_accept_A2_A3 percent_accept_A4 WITH percent_accept_B1 
    percent_accept_B2_B3 percent_accept_B4 (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(study = 104).
VARIABLE LABELS filter_$ 'study = 104 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

*within trial effects

GLM accept_AB_prev1 accept_AB_prev23 accept_AB_prev4
  /WSFACTOR=rank_prev 3 Polynomial 
  /METHOD=SSTYPE(3)
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=rank_prev.

* ttests comparing each bin 

T-TEST PAIRS=accept_AB_prev1 accept_AB_prev23 accept_AB_prev1 WITH accept_AB_prev23 accept_AB_prev4 
    accept_AB_prev4 (PAIRED)
  /CRITERIA=CI(.9500)
  

T-TEST GROUPS=order_condition(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=average_change_B_min_A_rank_1_intermediate_4
  /CRITERIA=CI(.95).

USE ALL.
COMPUTE filter_$=(study = 104  &  order_condition = 1).
VARIABLE LABELS filter_$ 'study = 104  &  order_condition = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST
  /TESTVAL=0
  /MISSING=ANALYSIS
  /VARIABLES=average_change_B_min_A_rank_1_intermediate_4
  /CRITERIA=CI(.95).

USE ALL.
COMPUTE filter_$=(study = 104  &  order_condition = 2).
VARIABLE LABELS filter_$ 'study = 104  &  order_condition = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST
  /TESTVAL=0
  /MISSING=ANALYSIS
  /VARIABLES=average_change_B_min_A_rank_1_intermediate_4
  /CRITERIA=CI(.95).

USE ALL.
COMPUTE filter_$=(study = 104).
VARIABLE LABELS filter_$ 'study = 104  &  order_condition = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


T-TEST GROUPS=order_condition(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=average_change_B_min_A_rank_1_2_3_4
  /CRITERIA=CI(.95).

USE ALL.
COMPUTE filter_$=(study = 104  &  order_condition = 1).
VARIABLE LABELS filter_$ 'study = 104  &  order_condition = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST
  /TESTVAL=0
  /MISSING=ANALYSIS
  /VARIABLES=average_change_B_min_A_rank_1_2_3_4
  /CRITERIA=CI(.95).

USE ALL.
COMPUTE filter_$=(study = 104  &  order_condition = 2).
VARIABLE LABELS filter_$ 'study = 104  &  order_condition = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST
  /TESTVAL=0
  /MISSING=ANALYSIS
  /VARIABLES=average_change_B_min_A_rank_1_2_3_4
  /CRITERIA=CI(.95).



* ------------------------------------------------------------------------







USE ALL.
COMPUTE filter_$=(study = 105 ).
VARIABLE LABELS filter_$ 'study = 105 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


T-TEST GROUPS=order_condition(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=average_change_B_min_A_rank_1_intermediate_4
  /CRITERIA=CI(.95).

USE ALL.
COMPUTE filter_$=(study = 105  &  order_condition = 1).
VARIABLE LABELS filter_$ 'study = 105  &  order_condition = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST
  /TESTVAL=0
  /MISSING=ANALYSIS
  /VARIABLES=average_change_B_min_A_rank_1_intermediate_4
  /CRITERIA=CI(.95).

USE ALL.
COMPUTE filter_$=(study = 105  &  order_condition = 2).
VARIABLE LABELS filter_$ 'study = 105  &  order_condition = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST
  /TESTVAL=0
  /MISSING=ANALYSIS
  /VARIABLES=average_change_B_min_A_rank_1_intermediate_4
  /CRITERIA=CI(.95).

USE ALL.
COMPUTE filter_$=(study = 105).
VARIABLE LABELS filter_$ 'study = 105 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.




T-TEST GROUPS=order_condition(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=average_change_B_min_A_rank_1_2_3_4
  /CRITERIA=CI(.95).

USE ALL.
COMPUTE filter_$=(study = 105  &  order_condition = 1).
VARIABLE LABELS filter_$ 'study = 105  &  order_condition = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST
  /TESTVAL=0
  /MISSING=ANALYSIS
  /VARIABLES=average_change_B_min_A_rank_1_2_3_4
  /CRITERIA=CI(.95).

USE ALL.
COMPUTE filter_$=(study = 105  &  order_condition = 2).
VARIABLE LABELS filter_$ 'study = 105  &  order_condition = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST
  /TESTVAL=0
  /MISSING=ANALYSIS
  /VARIABLES=average_change_B_min_A_rank_1_2_3_4
  /CRITERIA=CI(.95).

*--------
