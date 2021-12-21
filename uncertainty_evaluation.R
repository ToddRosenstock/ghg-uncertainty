# uncertainty package data processing functions

data_varkernelslicerange<-
function (in_var, out_var, min_in_var, max_in_var, scenario="Scenario1",
          xlab_vars = "Outcome variable dist. given influence variable",
          show_plot = FALSE) 
{
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.", 
         call. = FALSE)
  }
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.", 
         call. = FALSE)
  }
  in_outdata <- in_out <- NULL
  assertthat::validate_that(length(in_var) == length(out_var), 
                            msg = "\"in_var\" and \"out_var\" are not equal lengths.")
  assertthat::validate_that(is.numeric(in_var), msg = "\"in_var\" is not numeric.")
  assertthat::validate_that(is.numeric(min_in_var), msg = "\"min_in_var\" is not numeric.")
  assertthat::validate_that(is.numeric(max_in_var), msg = "\"max_in_var\" is not numeric.")
  assertthat::validate_that(is.numeric(out_var), msg = "\"out_var\" is not numeric.")
  in_out <- as.data.frame(cbind(in_var, out_var))
  in_outdata <- in_out[stats::complete.cases(in_out), ]
  assertthat::see_if(length(in_out) == length(in_outdata), 
                     msg = "Rows with NA were removed.")
  in_outkernel <- MASS::kde2d(in_outdata$in_var, in_outdata$out_var, 
                              n = 100)
  lbound <- which(in_outkernel$x == min(in_outkernel$x[which(in_outkernel$x > 
                                                               min_in_var)]))
  rbound <- which(in_outkernel$x == max(in_outkernel$x[which(in_outkernel$x <= 
                                                               max_in_var)]))
  if (show_plot) {graphics::plot(in_outkernel$y, rowMeans(in_outkernel$z[, 
                                                         lbound:rbound]), type = "l", col = "seagreen", 
                 lwd = 2, xlab = paste(xlab_vars, as.character(min_in_var), 
                                       "to", as.character(max_in_var)), ylab = "Relative probability")
  print("Relative probability (y) of the outcome variable for the given values of the influencing variable (x).")}
  
  return(list(
    slice_values = data.frame(
      scenario = scenario,
      y = in_outkernel$y,
      z = rowMeans(in_outkernel$z[, lbound:rbound])
    ),
    x_range = c(min_in_var, max_in_var)
  ))
  
}

multi_data_varkernelslicerange<-function(scenarios,min_in_var=NA, max_in_var=NA) # scenarios is a list of scenario (string),
  #in_var and out_var (numeric vectors of same length)
{
  
  if(is.na(min_in_var))
    min_in_var<-min(sapply(1:length(scenarios), function(x) min(scenarios[[x]]$in_var,na.rm=TRUE)))
 
  if(is.na(max_in_var))
    max_in_var<-max(sapply(1:length(scenarios), function(x) max(scenarios[[x]]$in_var,na.rm=TRUE)))
  
  for (ll in 1:length(scenarios))
  {
    scenario<-scenarios[[ll]]$scenario
    in_var<-scenarios[[ll]]$in_var
    out_var<-scenarios[[ll]]$out_var
    out<-data_varkernelslicerange(in_var, out_var, min_in_var, max_in_var, scenario=scenario,
                             show_plot = FALSE)
    if(ll==1) result<-out$slice_values else result<-rbind(result,out$slice_values)
  }
  
  return(list(slice_values=result, x_range=c(min_in_var, max_in_var)))

}

# scenarios<-list(list(scenario="baseline",
#                      in_var=ghg_simulation_scenarios$y$pc_milk_yield,
#                      out_var=ghg_simulation_scenarios$y$pc_on_farm),
#                 list(scenario="less male replacement",
#                      in_var=lessreplace_res$y$pc_milk_yield,
#                      out_var=lessreplace_res$y$pc_on_farm),
#                 list(scenario="no bulls",
#                      in_var=nobull_res$y$pc_milk_yield,
#                      out_var=nobull_res$y$pc_on_farm),
#                 list(scenario="breed upgrade",
#                      in_var=breed_res$y$pc_milk_yield,
#                      out_var=breed_res$y$pc_on_farm))
#                 
# 
# 
# mm<-multi_data_varkernelslicerange(scenarios)               
#                            
#        
# library(ggplot2)
# 
# 
# 
# ggplot(data = mm$slice_values, aes(x = y, y = z, color = scenario)) +
#   geom_line() + theme_bw(base_size = 12) +
#   xlab(expression(per~capita~emissions~(kg~co[2]-eq~year^-1))) +
#   ylab("density") +
#   ggtitle("per capita emissions") +
#   scale_color_discrete(name = "scenario")
# 
