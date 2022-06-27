---
title: "Uncertainty analysis for estimating soil organic carbon stocks"
author: "Joshua Wafula, Ermias Betemariam"
date: "10/22/2020"
output:
  html_document: default
  word_document: default
---



## Introduction

Uncertainties in soil organic carbon (SOC) stock assessments are rarely quantified even though they are critical in determining the significance of the results. Most studies generally focus on a single variable involved in the SOC stock calculation (SOC concentration, sampling depth, bulk density and rock fragment content) or on a single scale, rather than using an integrated approach (i.e. taking into account interactions between variables)(Goidts et al. 2009). We used the error propagation method to quantify the relative contribution of each variable and interaction involved to the final SOC stock variability. Monte Carlo simulations were used to cross-check the results. We used a data set for a study that was conducted around Serowe township in Botswana. Soil composite samples were collected from selected plot centers and at four different corners of each plot at 30 cm depth. The carbon content (by weight %) was determined using the Walkley-Black method (van Reeuwijk, 2002) in the soil laboratory of the Institute for Geo-information Science and Earth Observation (ITC) in the Netherlands. The air-dried soil samples were crushed and then passed through a 2 mm sieve prior to laboratory analysis. Bulk density was determined using the core sampling technique (Stone, 1991).   

## Methods

Soil organic C stocks (Mg C ha-1) was calculated as: SOC stock = C/100* Bulk Density (BD)* 100, where SOC stock is given in Mg C ha-1; C is soil organic carbon concentration of fine soil (fraction < 2 mm) determined in the laboratory (g kg-1). BD is in g m-3 and 100 is used to convert the units to Mg C ha-1.


```r
# calculate soil carbon stocks (soc x depth x bdensity*(1-cfragments/100)
socstock=(soil$soc*soil$depth*soil$bdensity*(1-soil$cfragments/100))
socstock_mean<-mean(socstock)
```

### a) Error propagation

To propagate the errors from the variables used to calculate SOC stocks, we first determined the the absolute error for each of the variables. We did this by calculating the margins of error with a desired confidence level of 90% (Z score).    


```r
# calculate absolute uncertainty. we have 75 data points
z_score<-1.65 # desired confidence level is 90% 
soc_abs_unc<-z_score*(sd(soil$soc)/sqrt(75))
dep_abs_unc<-z_score*(sd(soil$depth)/sqrt(75))
bdensity_abs_unc<-z_score*(sd(soil$bdensity)/sqrt(75))
cfr_abs_unc<-z_score*(sd(soil$cfragments)/sqrt(75))
```

We converted the absolute errors into relative errors (%) to enable propagation of the errors when multiplying/dividing variables.  


```r
# calculate relative uncertainty
soc_rel_unc<-100*soc_abs_unc/soil$soc
dep_rel_unc<-100*dep_abs_unc/soil$depth
bdensity_rel_unc<-100*bdensity_abs_unc/soil$bdensity
cfr_rel_unc<-100*cfr_abs_unc/soil$cfragments
```

We then computed the square root of the sum of the square of each of the SOC stock input variables to get the propagated relative error (Palmer, 2003). We converted the resultant relative error to absolute error. 


```r
# uncertainty propagation
socstock_rel_unc<-sqrt(soc_rel_unc^2+dep_rel_unc^2+bdensity_rel_unc^2+cfr_rel_unc^2)
socstock_abs_unc<-socstock*socstock_rel_unc/100
socstock_abs_unc_mean<-mean(socstock_abs_unc)
```

### b) Monte Carlo simulations using decisionSupport package

We used the `decisionSupport` function in the R package to run a Monte Carlo simulation of SOC stocks.The function requires two inputs:

1. An `input_table` (in .csv format) specifying the names and probability distributions for all variables used in the analysis. The variable distributions are described by a 90% confidence interval, which are specified by lower (5% quantile) and upper (95% quantile) bounds, as well as the shape of the distribution. `const` describe variables that are constant throughout, `norm` describe variables with normal distribution, `tnorm_0_1` describe variables with a truncated normal distribution that can only have values between 0 and 1 (useful for probabilities) and `posnorm` describe variables with normal distribution truncated at 0 (only positive values allowed). For this analysis, we used the calculated margins of error to estimate the lower and upper bounds for the mean values of SOC stocks input variables. This enabled us to generate the `input_table` below.


Table: Monte Carlo input data

|Description                                   | lower| upper|distribution |variable              |
|:---------------------------------------------|-----:|-----:|:------------|:---------------------|
|Sampling depth (m)                            | 30.00| 30.00|const        |sampling_depth        |
|Soil organic carbon concentration  (g C kg-1) |  0.45|  0.54|posnorm      |soil_organic_cab_conc |
|Bulk density (kg m-3)                         |  1.48|  1.54|posnorm      |bulk_density          |
|Mass proportion of rock fragment content (%)  |  2.65|  3.37|posnorm      |prop_rock_frag_cont   |

2. An `R_function` that calculates SOC stocks based on the variables named in `input_table`. For this analysis, the function `soil_carbon_stocks` describing interactions between SOC stock variables (SOC concentration, sampling depth, bulk density and rock fragment content) was used. 


```r
soil_carbon_stocks<-function(x, varnames){
# calculate soil carbon stocks (soc x depth x bdensity*(1-cfragments/100)  
carbon_stocks.1<-(sampling_depth*soil_organic_cab_conc.1*bulk_density.1*(1-prop_rock_frag_cont.1/100))
  return(carbon_stocks)
}
```

To run the simulation, the `soil_carbon_stocks` function, along with the data from the `input_table`, were fed into the Monte Carlo simulation (MC) function (Luedeling and Whitney, 2018) to conduct the full analysis. Below is the code we used to perform the Monte Carlo simulation with 10,000 model runs. We also applied Partial Least Squares (PLS) regression analysis to the MC simulation results to generate the Variable-Importance-in-the-Projection (VIP) statistic for input parameters (Luedeling & Gassner, 2012).The VIP statistic represents the direction and strength of each input variables relationship with the output variable (Wold et al. 2001). This helped us to identify the main sources of error when calculating SOC stocks.


```r
soil_carbon_stocks<-function(x, varnames){
make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}
make_variables(estimate_read_csv(input_table))
decisionSupport(inputFilePath = input_table, #input file with estimates
                outputPath = results_folder, #output folder
                welfareFunction = soil_carbon_stocks, #the function created above
                numberOfModelRuns = 10000,
                functionSyntax = "plainNames", 
                write_table = TRUE,)}
```

## Results and Discusion

The error propagation method projected a mean SOC stock of 21.26 Mg C ha-1 with a computed margin of error of 3.63 Mg C ha-1. MC simulation approach had a mean of 22 Mg C ha-1 with 90% confidence that the actual value was between the range of 20 to 24 Mg C ha-1 (i.e about 2 Mg C ha-1 margin of error). This outcome points to an overestimation of about 8% when using the error propagation method as compared to MC simulation method. This trend has also been reported by Jones (1989) (overestimation range of between 1.8 to 16.9%). Despite this overestimation, the error propagation method is useful to identify the contribution of each term of various types of equations to the final variability observed on the result, provided that uncertainties in individual terms and interaction can be estimated. The main source of uncertainty in the SOC stock was the variability of SOC concentration (due to errors from the laboratory and to the high SOC spatial variability) (Fig.1). This outcome shows that When assessing SOC stock at the landscape scale, one should focus on the precision of SOC analyses from the laboratory. 

![Figure 1: Important variables (determined by VIP analysis of PLS regression models) prejections, depicting the main sources of errors. The results were produced through MC simulation (10,000 model runs) of SOC stock calculations.](MC results/output_1_PLS_VIP.png)

## Conclusion

Uncertainties are difficult to quantify and to identify because they stem from complex interactions between the variables involved in SOC stocks (i.e. SOC concentration, bulk density, sampling depth and rock fragment content). However, the concept of error propagation can be directly applied to the mathematical expression of SOC stock, since the random error in the SOC stock comes from the propagation of the random errors in each variable used in the SOC stock equation. Given that these individual random errors and interaction are first estimated, their propagation can be assessed by different methods, including the Monte Carlo simulation method (MC), which generates numerous SOC stocks using random values for each individual variable (stochastic approach). The advantage of the error propagation method is to give an explicit equation for the final SOC stock variability (i.e. the solution comes in an analytical form), which allows one to quantify the relative contribution of the individual sources of uncertainties as well as their interaction (Heuvelink, 1998). However, when more complex functions than the SOC stock are considered, the MC method is easier to implement even if a large number of runs might be needed.

## References

Goidts, E., Van Wesemael, B., & Crucifix, M. (2009). Magnitude and sources of uncertainties in soil organic carbon (SOC) stock assessments at various scales. European Journal of Soil Science. https://doi.org/10.1111/j.1365-2389.2009.01157.x

Heuvelink, G.B.M. 1998. Uncertainty analysis in environmental modeling under a change of spatial scale. Nutrient Cycling in Agroecosystems, 50, 255-264.

Jones, L. 1989. Some results comparing Monte Carlo simulation and ???rst order Taylor series approximation for steady groundwater ???ow. Stochastic Hydrology & Hydraulics, 3, 179-190

Luedeling, E., Whitney, C.W., 2018. R package decisionSupport: Controlled burns in conifer forests. Retrieved from https://cran.r-project.org/web/packages/decisionSupport/vignettes/wildfire_example.html

Luedeling, E., Gassner, A., 2012. Partial Least Squares Regression for analyzing walnut phenology in California. Agric. For. Meteorol. https://doi.org/10.1016/j.agrformet.2011.10.020

van Reeuwijk, L.P., 2002. Procedures for Soil Analysis. Wageningen, The Netherlands

Palmer, M. (2003). Propagation of Uncertainty through Mathematical Operations. MIT Module.

Stone, J. A. (1991). Core sampling technique for bulk density and porosity determination on a clay loam soil. Soil and Tillage Research. https://doi.org/10.1016/0167-1987(91)90032-S

Wold, S., Sj??str??m, M., Eriksson, L., 2001. PLS-regression: A basic tool of chemometrics, in: Chemometrics and Intelligent Laboratory Systems. pp. 109130. https://doi.org/10.1016/S0169-7439(01)00155-1




