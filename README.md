# synthpanel

The synthpanel package offers three main functionalities:
- Simulation of panel data with causal treatment effects.
- Calculation of regression weights in order to estimate varying single-measure treatment effect aggregations.
- Wrapper around the packages did, did2s, DIDmultiplegtDYN, didimputation, and fixest(+sunab), allowing to retrieve treatment effect estimats under unified syntax.

## Installation
I recommend to install via devtools:
```
devtools::install_github("fewonett/synthpanel")
```
As of right now, the package depends on the [DIDmultiplegtDYN]([url](https://github.com/chaisemartinPackages/did_multiplegt_dyn)) package which in turn depends on rjava. This dependency may cause issues for some users. If this is the case for you, consider installing the alternative noxlsx version of DIDmultiplegtDYN via:
```
# Remove "normal" version, if installed:
remove.packages("DIDmultiplegtDYN")
# Install alternate version to avoid rjava dependency
devtools::install_github("chaisemartinPackages/did_multiplegt_dyn/no_xlsx/DIDmultiplegtDYN")
```
## Quick guide to the package
Let's simulate a dataset with heterogeneus treatment effects across units and within units over time:
First, we need to define the necessary parameters:
```
library(synthpanel)
set.seed(12)

# Define number of units, time periods, treatment cohorts, and time of treatment
# for each cohort
num_units <- 500
time_periods <- 10
group_number <-6
time_of_treatment <- c(-1,2,4,6,8,10)
# Define group level at t=1, and observation level noise:
group_level <- c(12,3,17,8,0,2)
s_d <- 3
```
Next, we define a joint time trend for all groups. This ensures that the parallel trend assumption holds. 
Alternatively, separate vectors for each group could be passed in a list to define varying time trends.
```
# Use trend brownian motion function to simulate time trend.
trend_vector <- trend_brownian_motion(time_periods,2,1)
```

The treatment effect heterogeneity is modelled via two inputs, group _te and exposure_te. 
Group_te defines the group treatment effect in period 1 for each group.
Exposure_te defines the development of treatment effects over time relative to the initial group_te. 
In this case effects are increasing by 20% each period. Once again a list of vectors could be passed,
to define differing developments of treatment effects over time for each group.
```
group_te <- c(0,4,8,10,36,-5)
exposure_te <- seq(1,2.6, 0.2)
```
Now we are ready to simulate and plot the data
```
# Simulate data:
data <- DGP_full(num_units,
                      time_periods,
                      group_number,
                      group_te,
                      time_of_treatment,
                      trend_vectors = trend_vector,
                      group_level,
                      s_d = s_d,
                      exposure_list = exposure_te)

# Plot the data 
time_series_plot(data)
```
and to estimate the treatment effects with each included estimator:
```
# Use get_all_ests function to retrieve TE estimates with different heterogeneity
# robust estimators:
results_simple <- get_all_ests(data)
results_simple
att_simple(data$treatment_effect, data$treat)
# The default aggregation is the ATT_simple

# Alternatively, retrieve estimates of the ATT_group, via all estimators. The necessary
# regression weights are calculated and passed internally:
results_group <- get_all_ests(data,ests =c("borus", "chaise", "cal_sa", "sunab", "gardner"),  aggregation = "group")
results_group
att_group(data$treatment_effect, data$group, data$time)
```
It is also possible to access the regression weights manually:
```
# For the ATT_group
group_weights <- weights_group(data, "unit", "group", "time", "treat")
group_weights
# For the ATT_dynamic
dynamic_weights <- weights_dyn(data, "group", "time", "treat")
dynamic_weights
```

