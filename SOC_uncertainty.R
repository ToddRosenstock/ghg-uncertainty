# Uncertainty analysis for estimating soil organic carbon stocks

# call data manipulation functions
library(dplyr)
library(magrittr)

# create work station
soil<- read.csv("C:/Users/jwafula/Dropbox/JFPE submission/GHG uncertainity/data/data.csv")
soil<- dplyr::select(soil,soc,depth,bdensity,cfragments)

# calculate soil carbon stocks (soc x depth x bdensity*(1-cfragments/100)
socstock=(soil$soc*soil$depth*soil$bdensity*(1-soil$cfragments/100))
socstock_mean<-mean(socstock)

#a) Uncertainty analysis using error propagation method

# calculate absolute uncertainty
z_score<-1.65 # desired confidence level is 90% 
soc_abs_unc<-z_score*(sd(soil$soc)/sqrt(75))
dep_abs_unc<-z_score*(sd(soil$depth)/sqrt(75))
bdensity_abs_unc<-z_score*(sd(soil$bdensity)/sqrt(75))
cfr_abs_unc<-z_score*(sd(soil$cfragments)/sqrt(75))

# calculate relative uncertainty
soc_rel_unc<-100*soc_abs_unc/mean(soil$soc)
dep_rel_unc<-100*dep_abs_unc/mean(soil$depth)
bdensity_rel_unc<-100*bdensity_abs_unc/mean(soil$bdensity)
cfr_rel_unc<-100*cfr_abs_unc/mean(soil$cfragments)

# uncertainty propagation
socstock_rel_unc<-sqrt(soc_rel_unc^2+dep_rel_unc^2+bdensity_rel_unc^2+cfr_rel_unc^2)
socstock_abs_unc<-socstock_mean*socstock_rel_unc/100

# (b) MC simulation approach using decisionSupport package

library(decisionSupport)

mean_soc<-mean(soil$soc)
mean_depth<-mean(soil$depth)
mean_bdensity<-mean(soil$bdensity)
mean_cfragments<-mean(soil$cfragments)

input_table <- "MC simulation data.csv"
results_folder <- "MC results"

# Generate a function for testing the model function 'line by line' 
# by taking a single random sample of the provided estimates
make_variables <- function(est,n=1){ x <- random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}
make_variables(estimate_read_csv(input_table))

soil_carbon_stocks<-function(x, varnames){
  
# calculate soil carbon stocks (soc x depth x bdensity*(1-cfragments/100)  
carbon_stocks<-(sampling_depth*soil_organic_cab_conc*bulk_density*(1-prop_rock_frag_cont/100))
  return(carbon_stocks)
}

# Run the Monte Carlo using the decisionSupport function ####
decisionSupport(inputFilePath = input_table, #input file with estimates
                outputPath = results_folder, #output folder
                welfareFunction = soil_carbon_stocks, #the function created above
                numberOfModelRuns = 10000,
                functionSyntax = "plainNames", 
                write_table = TRUE,)



