# Estimating Uncertainty in Kenyan Livestock Emission Reductions

# Load data
ghg_variables<- read.csv("C:/Users/jwafula/ghg-uncertainty/Kenya_baseline_individual_cow_wNotes.csv")

# (a) MC simulation approach using decisionSupport package

# Calculate mean values for the variables to help generate the MC simulation table
mean_Cfi<-mean(ghg_variables$Cfi)
mean_LW<-mean(ghg_variables$live_weight_LW)
mean_Ca<-mean(ghg_variables$Ca)
mean_C<-mean(ghg_variables$C)
mean_milk_yield<-mean(ghg_variables$milk_yield)
mean_milk_fat<-mean(ghg_variables$milk_fat)
mean_Cp<-mean(ghg_variables$Cp)
mean_MW<-mean(ghg_variables$mature_weight_MW)
mean_WG<-mean(ghg_variables$weight_gain_WG)
mean_dig_energy<-mean(ghg_variables$dig_energy)
mean_bo<-mean(ghg_variables$bo)
mean_mms_pasture<-mean(ghg_variables$mms_pasture)
mean_mms_spread<-mean(ghg_variables$mms_spread)
mean_mms_drylot<-mean(ghg_variables$mms_drylot)
mean_mms_solidstore<-mean(ghg_variables$mms_solidstore)
mean_mms_composted<-mean(ghg_variables$mms_composted)
mean_mms_liquid<-mean(ghg_variables$mms_liquid)
mean_mms_biogas<-mean(ghg_variables$mms_biogas)
mean_mms_burn<-mean(ghg_variables$mms_burn)
mean_feed_kgCO2<-mean(ghg_variables$feed_kgCO2)
mean_feed_transport_kgCO2<-mean(ghg_variables$feed_transport_kgCO2)
mean_fpcm<-mean(ghg_variables$kg_FPCM_year)
  
# Calculate absolute uncertainty to help get upper and lower bounds of variables in the MC input table
z_score<-1.65 # desired confidence level is 90% 
Cfi_abs_unc<-z_score*(sd(ghg_variables$Cfi)/sqrt(1348))
LW_abs_unc<-z_score*(sd(ghg_variables$live_weight_LW)/sqrt(1348))
Ca_abs_unc<-z_score*(sd(ghg_variables$Ca)/sqrt(1348))
C_abs_unc<-z_score*(sd(ghg_variables$C)/sqrt(1348))
Milk_yield_abs_unc<-z_score*(sd(ghg_variables$milk_yield)/sqrt(1348))
Milk_fat_abs_unc<-z_score*(sd(ghg_variables$milk_fat)/sqrt(1348))
Cp_abs_unc<-z_score*(sd(ghg_variables$Cp)/sqrt(1348))
MW_abs_unc<-z_score*(sd(ghg_variables$mature_weight_MW)/sqrt(1348))
WG_abs_unc<-z_score*(sd(ghg_variables$weight_gain_WG)/sqrt(1348))
Dig_energy_abs_unc<-z_score*(sd(ghg_variables$dig_energy)/sqrt(1348))
Bo_abs_unc<-z_score*(sd(ghg_variables$bo)/sqrt(1348))
Mms_pasture_abs_unc<-z_score*(sd(ghg_variables$mms_pasture)/sqrt(1348))
Mms_spread_abs_unc<-z_score*(sd(ghg_variables$mms_spread)/sqrt(1348))
Mms_drylot_abs_unc<-z_score*(sd(ghg_variables$mms_drylot)/sqrt(1348))
Mms_solidstore_abs_unc<-z_score*(sd(ghg_variables$mms_solidstore)/sqrt(1348))
Mms_composted_abs_unc<-z_score*(sd(ghg_variables$mms_composted)/sqrt(1348))
Mms_liquid_abs_unc<-z_score*(sd(ghg_variables$mms_liquid)/sqrt(1348))
Mms_biogas_abs_unc<-z_score*(sd(ghg_variables$mms_biogas)/sqrt(1348))
Mms_burn_abs_unc<-z_score*(sd(ghg_variables$mms_burn)/sqrt(1348))
Feed_kgCO2_abs_unc<-z_score*(sd(ghg_variables$feed_kgCO2)/sqrt(1348))
Feed_transport_kgCO2_abs_unc<-z_score*(sd(ghg_variables$feed_transport_kgCO2)/sqrt(1348))
fpcm_abs_unc<-z_score*(sd(ghg_variables$kg_FPCM_year)/sqrt(1348))

library(decisionSupport)

input_table <- "C:/Users/jwafula/ghg-uncertainty/livestock_ghg_input_table.csv"
results_folder <- "MC results"

# Generate a function for testing the model function 'line by line' 
# by taking a single random sample of the provided estimates
make_variables <- function(est,n=1){ x <- random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}
make_variables(estimate_read_csv(input_table))

# Generate the GHG emissions function 
ghg_emissions<-function(x, varnames){

# Enteric fermentation emissions ($CH_4$)
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

# Relationship between greenhouse gas intensity and milk yields. 
# Fat adjusts
ann_adj<-kg_FPCM_year*365 
GHGi = on_farm/ann_adj 

return(list(enteric_CH4=enteric_CH4,
            enteric_CO2=enteric_CO2,
            mm_CO2=mm_CO2,
            mm_CO2_N2O=mm_CO2_N2O,
            feed_CO2=feed_CO2,
            on_farm=on_farm,
            GHGi=GHGi))
}

# Run the Monte Carlo using the decisionSupport function ####
decisionSupport(inputFilePath = input_table, #input file with estimates
                outputPath = results_folder, #output folder
                welfareFunction = ghg_emissions, #the function created above
                numberOfModelRuns = 10000,
                functionSyntax = "plainNames", 
                write_table = TRUE,)


# b) Uncertainty analysis using error propagation method


































































































































































































































































































































































































































































































































































