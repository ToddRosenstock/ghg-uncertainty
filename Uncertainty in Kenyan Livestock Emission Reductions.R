# Estimating Uncertainty in Kenyan Livestock Emission Reductions

# Load data
# CW Note ####
# We need this file
# Also, no need for the whole directory here. 
# The .Rproj will find the .csv in a relative location (i.e. subfolder)

library(decisionSupport)
#CW Note ####
# remove all the 'C:/Users/jwafula/Dropbox/UNIQUE_CCAFS/analysis/data/' business
# This project will look in relative file paths (i.e. same or lower folders)
input_table <- "livestock_ghg_input_table.csv"
results_folder <- "MC results"

# Generate a function for testing the model function 'line by line' 
# by taking a single random sample of the provided estimates
make_variables <- function(est,n=1){ x <- random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}
# run the function on the input_table
make_variables(estimate_read_csv(input_table))

# Generate the GHG emissions function 
ghg_emissions<-function(x, varnames){

# Enteric fermentation emissions ($CH_4$)
  #CW Note ####
  #in these code sections I am missing the overview. 
  # it is not immediately clear what this is meant to do... 
  # from a specific publication? 
nem<-maintenaince_coefficient_Cfi*(live_weight_LW^0.75)
nea<-activity_coefficient_Ca*nem
nel<-milk_yield*(1.47+0.4*milk_fat) 
nep<-pregnancy_coefficient_Cp*nem
neg<-22.02*(((live_weight_LW/(growth_coefficient_C*mature_weight_MW))^0.75)*(weight_gain_WG^1.097))
rem<-(1.123-(4.092*(10^-3)*dig_energy)+(1.126*(10^-5)*(dig_energy^2))-(25.4/dig_energy))
reg<-(1.164-(5.16*(10^-3)*dig_energy)+(1.308*(10^-5)*(dig_energy^2))-(37.4/dig_energy))
gross_energy<-((((nem+nea+nel+nep)/rem)+(neg/reg))/dig_energy/100)
enteric_CH4<-(gross_energy*(6.5/100)*365)/55.65 

# Compute the CO2 equation for enteric methane
enteric_CO2=enteric_CH4*25*365

# Manure management methane ($CH_4$) and nitrous oxide ($N_2O$)
volsol<-(gross_energy*(1-(dig_energy/100))+(0.04*gross_energy))*((1-0.08)/18.45) 
mm_ch4<-(volsol*365)*(bo*0.67*((mms_pasture*0.015)+(mms_spread*0.005)+(mms_drylot*0.015)+(mms_solidstore*0.04)+
        (mms_composted*0.005)+(mms_liquid*0.2)+(mms_biogas*0.1)+(mms_burn*0.1)))

# Estimate CO2eq for manure methane
mm_CO2<-mm_ch4*25*365

# Calculate Nitrous oxide emissions from manure and urine on pasture
n_intake<-(gross_energy/18.45)*((pregnancy_coefficient_Cp/100)/6.25)
n_retension<-((milk_yield*(3.3/100))/6.38)
n_excretion<-n_intake*(1-n_retension)*365
mm_direct_n2o<-n_excretion*((mms_pasture*0.02)+(mms_spread*0.005)+(mms_drylot*0.02)+(mms_solidstore*0.005)+
               (mms_composted*0.006)+(mms_liquid*0.005)+(mms_biogas*0)+(mms_burn*0))*0.01*(44/28)

# Nitrous oxide (N2O) from volatization 
mm_vol_n2o<-(n_excretion*((mms_pasture*0.02)+(mms_spread*0.07)+(mms_drylot*0.2)+(mms_solidstore*0.3)+
            (mms_composted*0.3)+(mms_liquid*0.4)+(mms_biogas*0)+(mms_burn*0))*0.01*(44/28))

# Nitrous oxide (N2O) from leaching 
mm_leach_n2o<-n_excretion*((mms_pasture*0.3)+(mms_spread*0.3)+(mms_drylot*0.3)+(mms_solidstore*0.3)+
              (mms_composted*0.3)+(mms_liquid*0.3)+(mms_biogas*0)+(mms_burn*0))*0.0075*(44/28)

# Estimate CO2eq for manure nitrous
mm_CO2_N2Od<-mm_direct_n2o*298*365
mm_CO2_N2Ovol<-mm_vol_n2o*298*365
mm_CO2_N2Oleach<-mm_leach_n2o*298*365
mm_CO2_N2O<-mm_CO2_N2Od+mm_CO2_N2Ovol+mm_CO2_N2Oleach

# Emissions from feed production and transport. 
feed_CO2<-feed_prod_CO2*feed_trans_CO2

# Total emissions
on_farm<-enteric_CH4+enteric_CO2+mm_CO2+mm_CO2_N2O+feed_CO2

#CW Note ####
# Not clear what this part does (through to the ggplot)

# Relationship between greenhouse gas intensity and milk yields. 
# Fat adjusts
ann_adj <-kg_FPCM_year*365 
df <- data.frame(
  GHGi = on_farm/ann_adj, 
  FPCM = ann_adj)

# plot

library(ggplot2)
ggplot(df, aes(x = FPCM, y = GHGi)) +geom_point() + ylim(0,50) 

return(list(enteric_CH4=enteric_CH4,
            #CW NOTE ####
            #working backwards from 'enteric_CO2' I see that 
            # this is the product of something twice multiplied by 365
            enteric_CO2=enteric_CO2,
            mm_CO2=mm_CO2,
            mm_CO2_N2O=mm_CO2_N2O,
            feed_CO2=feed_CO2,
            on_farm=on_farm))
}

# Run the Monte Carlo using the decisionSupport function ####
decisionSupport(inputFilePath = input_table, #input file with estimates
                outputPath = results_folder, #output folder
                welfareFunction = ghg_emissions, #the function created above
                numberOfModelRuns = 100, #make 100 for now
                functionSyntax = "plainNames", 
                write_table = TRUE,)

