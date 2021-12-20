# Generate the GHG emissions function ####
# Cory Whitney and Eike Luedeling

# # install packages ####
library(decisionSupport)
library(ggplot2)
library(plyr)

# load data ####
input_table <- "data/livestock_ghg_input_table.csv"

# make variables function ####
# Here we generate a `make_variables` function 
# for testing the model function 'line by line'. 
# We use it to take a single random sample of the provided estimates 
# (this isn't required for running the model later, 
# but it helps when developing the code).
make_variables <- function(est,n=1){ x <- random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

# run the function on the input_table
# make_variables(estimate_read_csv(input_table))

# Generate the GHG emissions function ####
ghg_emissions<-function(x, varnames){
  
  # Enteric fermentation emissions ($CH_4$)
  
  # The main source of all this is Chapter 10 of the *2006 IPCC Guidelines 
  # for National Greenhouse Gas Inventories* 
  # (Volume 4 - Agriculture, Forestry and Other Land Use). 
  # The document can be found on the IPCC web 
  # https://www.ipcc-nggip.iges.or.jp/public/2006gl/pdf/4_Volume4/V4_10_Ch10_Livestock.pdf
  
  # We start with the net energy for maintenance (NE_m; in MJ/day)
  # This includes a maintenance coefficient, 
  # for which different values are recommended depending on animal type
  
  # The following animal types were used in the example:
  
  # animal type 1 - 'intact' grown-up males >=36 months
  # animal type 2 - castrated grown-up males >=36 months
  # animal type 3 - males 12-36 months
  # animal type 4 - lactating cows
  # animal type 5 - lactating cows
  # animal type 6 - lactating cows
  # animal type 7 - weaned young females <= 12 months
  # animal type 8 - weaned young males <= 12 months
  # animal type 9 - females 12-36 months
  # animal type 10 - pre-weaned female calves (<12 months)
  # animal type 11 - pre-weaned male calves (<12 months)
  # animal type 12 - first-time pregnant cows
  
  # animal types 4, 5 and 6 are all lactating cows - 
  # important to note here that the original data table 
  # appears to have worked with the actual number of months 
  # that cows were pregnant. This resulted in a weighted mean 
  # of the various maintenance coefficients. 
  # We're not sure where we could get such data. 
  # In the original dataset, types 4, 5 and 6 
  # referred to different pregnancy/lactation states, 
  # but it doesn't seem like this scheme was rigorously followed.
  
  # We make placeholder vectors to collect emissions data 
  # for all animal types
  mm_N2O_CO2eq<-c()
  mm_CH4_CO2eq<-c()
  enteric_CH4_CO2eq<-c()
  total_milk<-c()
  
  # Now we start a long loop, 
  # where emissions are calculated for each animal type
  
  for (animal_type in 1:12)
  {
    # define C and Cfi based on sex (Page 10.17, below Equation 10.6)
    if(animal_type %in% c(1,3,8,11)) growth_coefficient_C<-C_intact_males # males (1.2)
    if(animal_type %in% c(2)) growth_coefficient_C<-C_castrates # castrates (1.0)
    if(animal_type %in% c(4,5,6,7,9,10,12)) growth_coefficient_C<-C_females # females (0.8)
    
    if(animal_type %in% c(4,5,6)) Cfi<-maintenance_coefficient_Cfi_lact_cow # lactating cows (0.386)
    if(animal_type %in% c(1)) Cfi<-maintenance_coefficient_Cfi_bulls # bulls (0.37)
    if(animal_type %in% c(2,3,7,8,9,10,11,12)) Cfi<-maintenance_coefficient_Cfi_other # others (0.322)
    
    # define mature body weight (assuming this needs to be disaggregated by sex, for Eqation 10.6)
    if(animal_type %in% c(1,2,3,8,11)) mature_weight_MW<-mature_weight_MW_male
    if(animal_type %in% c(4,5,6,7,9,10,12)) mature_weight_MW<-mature_weight_MW_female
    
    # define milk yield
    if(animal_type %in% c(1,2,3,7,8,9,10,11,12)) milk_yield<-0
    if(animal_type %in% c(4,5,6)) milk_yield<-eval(parse(text=paste0("milk_yield_",animal_type)))
    
    #define pregnancy coefficient (full Cp for first-time pregnant, lower for later in most cases)
    if(animal_type==12) Cp<-Cp_0
    if(animal_type==4) Cp<-Cp_0*Cp_rate_4
    if(animal_type==5) Cp<-Cp_0*Cp_rate_5
    if(animal_type==6) Cp<-Cp_0*Cp_rate_6
    if(animal_type %in% c(1,2,3,7,8,9,10,11)) Cp<-0
    
    N_animal_type<-eval(parse(text=paste0("N_animal_type_",animal_type)))
    
    # define activity coefficient Ca (Table 10.5)
    # Stall 0.00 - Animals are confined to a small area 
    #     (i.e., tethered, pen, barn) with the result that they expend 
    #     very little or no energy to acquire feed.
    # Pasture 0.17 - Animals are confined in areas with sufficient forage 
    #     requiring modest energy expense to acquire feed.
    # Grazing large areas 0.36 - Animals graze in open range land 
    #     or hilly terrain and expend significant energy to acquire feed.
    

    if(feeding_system==1) C_activity<-Ca_stall # Stall (0.0)
    if(feeding_system==2) C_activity<-Ca_pasture # Stall (0.17)
    if(feeding_system==3) C_activity<-Ca_large_grazing # Stall (0.36)
    
    # Mean live weight (kg) of animals surely varies by animal type, 
    # so we need separate estimates for them
    live_weight_LW<-eval(parse(text=paste0("live_weight_LW_",animal_type)))
    
    # Mean weight gain of animal type (kg/day)
    weight_gain_WG<-eval(parse(text=paste0("weight_gain_WG_",animal_type)))
    
    # Calculate the Net Energy for Maintenance (NE_m) (MJ/day) (Equation 10.3)
    NE_m<-Cfi*(live_weight_LW^0.75)
    
    # Calculate the Net Energy for Activity (NE_activity) (MJ/day) (Equation 10.4)
    NE_activity<-C_activity*NE_m
    
    #Net energy for growth (NE_growth) (MJ/day) (Equation 10.6)
    NE_growth<-22.02*
      ((live_weight_LW/(growth_coefficient_C*mature_weight_MW))^0.75)*
      (weight_gain_WG^1.097)
    
    # Net energy for lactation
    NE_lactation<-milk_yield*(1.47+0.40*milk_fat)
    
    #Energy need during pregnancy (Equation 10.13)
    NE_pregnancy<-Cp*NE_m
    
    # Ratio of net energy available in a diet for maintenance 
    # to digestible energy consumed (Equation 10.14)
    REM<-1.123-(4.092/1000*DE_perc)+(1.126/100000*DE_perc^2)-(25.4/DE_perc)
    
    # Ratio of net energy available for growth in a diet 
    # to digestible energy consumed (Equation 10.15)
    REG<-1.164-(5.160/1000*DE_perc)+(1.308/100000*DE_perc^2)-(37.4/DE_perc)
    
    # Gross Energy (GE) MJ/day
    NE_work<-0 # work animals aren't covered here, so we set this to zero
    GE<-(((NE_m+NE_activity+NE_lactation+NE_work+NE_pregnancy)/REM)+
           (NE_growth/REG))/(DE_perc/100)
    
    # CH4 emission factors for enteric fermentation from a livestock category 
    # (Equation 10.21)
    enteric_CH4<-(GE*Y_m*365)/55.65 # kg CH4 per animal per year
    
    # Compute the CO2-equivalent of methane emissions for enteric methane
    # This converts methane emissions to their Global Warming Potential 
    # (CO2-equivalent of CH4 is 25)
    enteric_CH4_CO2eq[animal_type]<-N_animal_type*enteric_CH4*25
    
    # Manure management methane ($CH_4$) and nitrous oxide ($N_2O$)
    # daily volatile solid excreted (Equation 10.24)
    
    volsol<-(GE*
               (1-(DE_perc/100))+
               (urinary_energy*GE))*
      ((1-ash_content)/18.45) 
    
    # The maximum methane emission from manure (Bo) depends on the animal type. 
    # In the input table, we treat these as constants for each animal type. 
    # The following line assigns the corresponding Bo value
    
    Bo<-eval(parse(text=paste0("Bo_animaltype_",animal_type)))
    
    # CH4 emission factor from manure management (Equation 10.23)
    mm_ch4<-(volsol*365)*
      (Bo*0.67*
         ((mms_pasture*MCFpasture)+
            (mms_spread*MCFspread)+
            (mms_drylot*MCFdrylot)+
            (mms_solidstore*MCFsolidstore)+
            (mms_composted*MCFcomposted)+
            (mms_liquid*MCFliquid)+
            (mms_biogas*MCFbiogas)+
            (mms_burn*MCFburn)+
          # the original code was lacking the 'sold' fraction - 
          # we added this for completeness
          (mms_sold*MCFsold)))
    
    # Estimate CO2eq for manure methane
    mm_CH4_CO2eq[animal_type]<-N_animal_type*mm_ch4*25
    
    # Calculate Nitrous oxide emissions from manure and urine on pasture
    
    # estimate N intake (kg/animal/day) (Equation 10.32)
    N_intake<-(GE/18.45)*((perc_crude_protein_diet/100)/6.25)
    
    # estimate N retention (kg/animal/day) (Equation 10.33)
    
    if(weight_gain_WG==0)
      N_retention <- ((milk_yield*(1.9+0.4*milk_fat)/100)/6.38)
        
    if(!weight_gain_WG==0)
      N_retention<-((milk_yield*(1.9+0.4*milk_fat)/100)/6.38)+
      ((weight_gain_WG*(268-(7.03*NE_growth/weight_gain_WG)))/(1000*6.25))
    
    # estimate N excretion 
    # (kg N/animal/yr) (Equation 10.31)
    N_excretion<-N_intake*(1-N_retention)*365
    
    # Direct nitrous oxide emissions 
    # (kg N2O/animal/yr) (Eq 10.25 from IPCC guidelines)
    mm_direct_n2o<-N_excretion*((mms_pasture*mndec_pasture)+
                                  (mms_spread*mndec_spread)+
                                  (mms_drylot*mndec_drylot)+
                                  (mms_solidstore*mndec_solidstore)+
                                  (mms_composted*mndec_composted)+
                                  (mms_liquid*mndec_liquid)+
                                  (mms_biogas*mndec_biogas)+
                                  (mms_burn*mndec_burn)+
                                  (mms_sold*mndec_sold))*(44/28)
    
    # N losses from volatilization 
    # (kg N/animal/yr) (Eq 10.26 from IPCC guidelines)
    mm_vol_n<-N_excretion*((mms_pasture*mnvc_pasture)+
                             (mms_spread*mnvc_spread)+
                             (mms_drylot*mnvc_drylot)+
                             (mms_solidstore*mnvc_solidstore)+
                             (mms_composted*mnvc_composted)+
                             (mms_liquid*mnvc_liquid)+
                             (mms_biogas*mnvc_biogas)+
                             (mms_burn*mnvc_burn)+
                             (mms_sold*mnvc_sold))
    
    # indirect N2O emissions due to volatilization of N from manure management 
    # (kg N2O/animal/year) (Equation 10.27)
    mm_vol_n2o<-mm_vol_n*EF4*(44/28)
    
    # Nitrous oxide (N2O) from leaching (kg N/animal/year) (equation 10.28)
    mm_leach_n<-N_excretion*((mms_pasture*frac_leach_pasture)+
                               (mms_spread*frac_leach_spread)+
                               (mms_drylot*frac_leach_drylot)+
                               (mms_solidstore*frac_leach_solidstore)+
                               (mms_composted*frac_leach_composted)+
                               (mms_liquid*frac_leach_liquid)+
                               (mms_biogas*frac_leach_biogas)+
                               (mms_burn*frac_leach_burn)+
                               (mms_sold*frac_leach_sold))
    
    # indirect N2O emissions due to leaching of N from manure management 
    # (kg N2O/animal/year) (Equation 10.29)
    mm_leach_n2o<-mm_leach_n*EF5*(44/28)
    
    # CW EL Comment ####
    # # comment: we removed this, it was probably wrong, 
    # since leached nitrogen doesn't emit (much) nitrous oxide
    #mm_leach_n2o<-n_excretion*((mms_pasture*0.3)+(mms_spread*0.3)+(mms_drylot*0.3)+(mms_solidstore*0.3)+
    #              (mms_composted*0.3)+(mms_liquid*0.3)+(mms_biogas*0)+(mms_burn*0))*0.0075*(44/28)
    
    # Estimate CO2eq for manure nitrous
    mm_N2O_CO2eq[animal_type]<-N_animal_type*(mm_direct_n2o+mm_vol_n2o+mm_leach_n2o)*298 # (298 is the CO2-equivalent of N2O)
    
    total_milk[animal_type]<-N_animal_type*milk_yield*365
    
  }
  
  # Emissions from feed production and transport. 

  feed_CO2<-feed_prod_CO2+feed_trans_CO2
  
  # Total emissions 

  on_farm<-sum(enteric_CH4_CO2eq+mm_CH4_CO2eq+mm_N2O_CO2eq)+feed_CO2
  
  total_animals<-sum(sapply(1:12,function(x) eval(parse(text=paste0("N_animal_type_",x)))))

  return(list(enteric_CH4=enteric_CH4,
              milk_yield=sum(total_milk),
              enteric_CH4_CO2eq=sum(enteric_CH4_CO2eq),
              mm_CH4_CO2eq=sum(mm_CH4_CO2eq),
              mm_N2O_CO2eq=sum(mm_N2O_CO2eq),
              feed_CO2=feed_CO2,
              on_farm=on_farm,
              pc_enteric_CH4=enteric_CH4/total_animals,
              pc_milk_yield=sum(total_milk)/total_animals,
              pc_enteric_CH4_CO2eq=sum(enteric_CH4_CO2eq)/total_animals,
              pc_mm_CH4_CO2eq=sum(mm_CH4_CO2eq)/total_animals,
              pc_mm_N2O_CO2eq=sum(mm_N2O_CO2eq)/total_animals,
              pc_feed_CO2=feed_CO2/total_animals,
              pc_on_farm=on_farm/total_animals
              
              
              ))
}
