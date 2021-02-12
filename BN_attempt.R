#test of the hill climbing 'greedy' bn with bnlearn
#Tsamardinos, Ioannis, Laura E. Brown, and Constantin F. Aliferis. “The max-min hill-climbing Bayesian network structure learning algorithm.” Machine learning 65.1 (2006): 31-78.

library(bnlearn)

ghg_variables<- read.csv("Kenya_baseline_individual_cow_wNotes.csv")

# all functions in bnlearn require
# complete data so that error message is expected. However, you can
# estimate the CPTs from incomplete data using table() and prop.table()
# and assemble them in a fitted BN with custom.fit(). On the other hand,
# maybe it would be better to write an EM wrapper around bn.fit() to
# make the best of the dependence structure of the data?
table(ghg_variables)
prop.table(ghg_variables)
# overcome the Error in check.data(x) : 
# the data set contains NaN/NA values.
