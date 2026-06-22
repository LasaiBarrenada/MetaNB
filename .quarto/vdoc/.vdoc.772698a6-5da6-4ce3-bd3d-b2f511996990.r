#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# remotes::install_github("zhipeiwang/MetaNB")
library(MetaNB)

# load other packages
library(dplyr)
library(coda)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
knitr::kable(head(data_ADNEXCA125))
#
#
#
#
#
#
#
#
#
# download the package from GitHub
pak::pkg_install("zhipeiwang/MetaNB")

# load the package
library(MetaNB)

# load other packages
library(dplyr)
library(coda)
#
#
#
# set options for better display of tables and numbers
options(
  pillar.width = Inf,
  tibble.width = Inf,
  tibble.print_max = Inf,
  scipen = 999
)

fmt <- function(x, digits = 4) {
  formatC(x, format = "f", digits = digits)
}

fmt_int <- function(x) {
  formatC(round(x), format = "d", big.mark = ",")
}
#
#
#
#
#
#
#
#
#
#
#
# basic fit without VOI
MA_NB_tri(data = data_ADNEXCA125, # data frame containing the data
          tp = TP, tn = TN, n_event = n_event, n_nonevent = n_nonevent, # column names in the data frame for the input data
          prior_type = "weak", # type of prior to use, either "weak" or "wishart"
          t = 0.1, # threshold for defining true positives and true negatives
          return_vars = c("NB", "RU", "probuseful", "sens", "spec"), # we additionally request relative utility (RU), sensitivity (sens), and specificity (spec) because these quantities will be used later in the vignette
          seed = 123, # random seed for reproducibility
          prev_known = 0.5, # this is to calculate conditional estimates of net benefit assuming a known prevalence
          return_known = TRUE # return the conditional estimates of net benefit assuming a known prevalence
          )
#
#
#
#| cache: false
#| class-output: output
# cache is for avoiding re-running the code that's time-consuming every time we knit this file
# The two identical code chunks are for first displaying the code without running, and second actually running the code and showing the output but without showing the code. This is because otherwise the scrollable feature would apply to both code and output, which is inconvenient for viewing.
MA_NB_tri(data = data_ADNEXCA125, # data frame containing the data
          tp = TP, tn = TN, n_event = n_event, n_nonevent = n_nonevent, # column names in the data frame for the input data
          prior_type = "weak", # type of prior to use, either "weak" or "wishart"
          t = 0.1, # threshold for defining true positives and true negatives
          return_vars = c("NB", "RU", "probuseful", "sens", "spec"), # variables to return in the output
          seed = 123, # random seed for reproducibility
          prev_known = 0.5, # this is to calculate conditional estimates of net benefit assuming a known prevalence
          return_known = TRUE # return the conditional estimates of net benefit assuming a known prevalence
          )
#
#
#
#
#
#
#
# fit with VOI enabled

fit_voi <- MA_NB_tri(data = data_ADNEXCA125,
                     tp = TP, tn = TN, n_event = n_event, n_nonevent = n_nonevent,
                     prior_type = "weak",
                     t = 0.1,
                     return_vars = c("NB", "RU", "probuseful", "sens", "spec", "pooledprev"),
                     seed = 123,
                     prev_known = 0.5,
                     return_known = TRUE,
                     
                     # VOI specific arguments
                     compute_EVPI = TRUE, # enable the computation of VOI quantities
                     auto_resample = FALSE, # whether to automatically draw additional samples until diagnostics indicate that the EVPI estimates are stable
                     center_rows = 1:36, # which rows (centers) to calculate the center-specific EVPI calculation
                     center_label_cols = c("Publication", "Country") # which columns to use for labeling the centers in the output of center-specific EVPI
                     )

#
#
#
#
#| cache: false
#| class-output: output
# fit with VOI enabled

fit_voi <- MA_NB_tri(data = data_ADNEXCA125,
                     tp = TP, tn = TN, n_event = n_event, n_nonevent = n_nonevent,
                     prior_type = "weak",
                     t = 0.1,
                     return_vars = c("NB", "RU", "probuseful", "sens", "spec", "pooledprev"),
                     seed = 123,
                     prev_known = 0.5,
                     return_known = TRUE,
                     compute_EVPI = TRUE, # enable the computation of VOI quantities
                     auto_resample = FALSE, # whether to automatically draw additional samples until diagnostics indicate that the EVPI estimates are stable
                     center_rows = 1:36, # which rows (centers) to calculate the center-specific EVPI calculation
                     center_label_cols = c("Publication", "Country") # which columns to use for labeling the centers in the output of center-specific EVPI
                     )

```
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
MA_NB_tri(data = data_ADNEXCA125,
          tp = TP, tn = TN, n_event = n_event, n_nonevent = n_nonevent,
          prior_type = "weak",
          t = 0.1,
          return_vars = c("NB", "RU", "probuseful", "sens", "spec"),
          seed = 123,
          weak_priors = list(mu_etap = -0.9) # example of overriding the prior mean for etap
          )
#
#
#
#
#
#
#
fit_voi$meta$prior_type
fit_voi$priors_used
#
#
#
#| class-output: output
fit_voi$meta$prior_type
fit_voi$priors_used
#
#
#
#
#
fit_voi$meta
#
#
#
#| class-output: output
fit_voi$meta
#
#
#
#
#
#
#
#
#
#
#
traceplot(fit_voi$samples[, c("pooledsens", "pooledspec", "pooledNB", "pooledNB_TA", "pooledRU")])
#
#
#
#
#| cache: false
#| class-output: output
traceplot(fit_voi$samples[, c("pooledsens", "pooledspec", "pooledNB", "pooledNB_TA", "pooledRU")])
```
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
summarize_tri_ma(
  fit_voi$samples,
  data = data_ADNEXCA125,
  label_cols = c("Publication", "Country", "N", "Prev"),
  metrics = c("NB", "RU", "probuseful", "sens", "spec"),
  per_study_metrics = c("NB", "RU", "sens", "spec"),
  return_known = FALSE, # asks to not return summaries for conditional estimates of net benefit assuming a known prevalence
  include_per_study = FALSE) # asks to not return per-study estimates
#
#
#
#| cache: false
#| class-output: output
summarize_tri_ma(
  fit_voi$samples,
  data = data_ADNEXCA125,
  label_cols = c("Publication", "Country", "N", "Prev"),
  metrics = c("NB", "RU", "probuseful", "sens", "spec"),
  per_study_metrics = c("NB", "RU", "sens", "spec"),
  return_known = FALSE,
  include_per_study = FALSE)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
summarize_tri_ma(
  fit_voi$samples,
  data = data_ADNEXCA125,
  label_cols = c("Publication", "Country", "N", "Prev"),
  metrics = c("NB", "sens"),
  per_study_metrics = c("NB", "sens"),
  return_known = FALSE,
  include_per_study = TRUE)$NB$per_study
#
#
#
#| cache: false
#| class-output: output
summarize_tri_ma(
  fit_voi$samples,
  data = data_ADNEXCA125,
  label_cols = c("Publication", "Country", "N", "Prev"),
  metrics = c("NB", "sens"),
  per_study_metrics = c("NB", "sens"),
  return_known = FALSE,
  include_per_study = TRUE)$NB$per_study
#
#
#
#
#
summarize_tri_ma(
  fit_voi$samples,
  data = data_ADNEXCA125,
  metrics = c("pooledprev"))
#
#
#
#| cache: false
#| class-output: output
summarize_tri_ma(
  fit_voi$samples,
  data = data_ADNEXCA125,
  metrics = c("pooledprev"))
#
#
#
#
#
#
#
#
#
# fit the model with a known prevalence for the target setting
fit_prev05 <- MA_NB_tri(data = data_ADNEXCA125,
                 tp = TP, tn = TN, n_event = n_event, n_nonevent = n_nonevent,
                 prior_type = "weak",
                 t = 0.1,
                 prev_known = 0.5, # example known prevalence for the target setting
                 return_vars = c("NB", "RU", "probuseful", "sens", "spec"),
                 return_known = TRUE,
                 seed = 123
                 )

# summarize the results, including the quantities conditional on the known prevalence
summarize_tri_ma(
  fit_prev05,
  data = data_ADNEXCA125,
  metrics = c("NB"),
  per_study_metrics = c("NB"),
  return_known = TRUE)
#
#
#
#
#| cache: false
#| class-output: output
# fit the model with a known prevalence for the target setting
fit_prev05 <- MA_NB_tri(data = data_ADNEXCA125,
                 tp = TP, tn = TN, n_event = n_event, n_nonevent = n_nonevent,
                 prior_type = "weak",
                 t = 0.1,
                 prev_known = 0.5, # example known prevalence for the target setting
                 return_vars = c("NB", "RU", "probuseful", "sens", "spec"),
                 return_known = TRUE,
                 seed = 123
                 )

# summarize the results, including the quantities conditional on the known prevalence
summarize_tri_ma(
  fit_prev05,
  data = data_ADNEXCA125,
  metrics = c("NB"),
  include_per_study = FALSE,
  return_known = TRUE)
```
#
#
#
#
#
#
#
#
#
#
#
#
#
#| cache: false
#| out-width: 100%
#| fig-width: 12
#| fig-height: 10
plot_forest(
  fit_voi$samples,
  data = data_ADNEXCA125 %>%
    mutate(Prev = paste0(round(Prev * 100), "%")),
  label_cols = c("Publication", "Country", "N", "Prev"),
  study_label_col = "Publication",
  metric = "NB",
  t = 0.1,
  xlim = c(-0.1, 0.7),
  mark_imputed = FALSE
)

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# fit with VOI enabled

fit_voi <- MA_NB_tri(data = data_ADNEXCA125,
                     tp = TP, tn = TN, n_event = n_event, n_nonevent = n_nonevent,
                     prior_type = "weak",
                     t = 0.1,
                     return_vars = c("NB", "RU", "probuseful", "sens", "spec"),
                     seed = 123,
                     prev_known = 0.5,
                     return_known = TRUE,
                     compute_EVPI = TRUE, # enable the computation of VOI quantities
                     auto_resample = FALSE, # whether to automatically draw additional samples until diagnostics indicate that the EVPI estimates are stable
                     center_rows = 1:36, # which rows (centers) to calculate the center-specific EVPI calculation
                     center_label_cols = c("Publication", "Country") # which columns to use for labeling the centers in the output of center-specific EVPI
                     )

#
#
#
#
#
#
#
fit_voi$voi_diagnostics
#
#
#
#| class-output: output
print(fit_voi$voi_diagnostics, width = Inf)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
options(scipen = 999)
as.data.frame(fit_voi$voi_metrics)
#
#
#
#| cache: false
#| class-output: output
options(scipen = 999)
as.data.frame(fit_voi$voi_metrics)
#
#
#
#
#
#
#
fit_voi$voi_center_meta
#
#
#
#| cache: false
#| class-output: output
options(scipen = 999)
fit_voi$voi_center_meta
#
#
#
#
#
fit_voi$voi_center_metrics
#
#
#
#| cache: false
#| class-output: output
options(scipen = 999)
print(fit_voi$voi_center_metrics, width = Inf)
#
#
#
#
#
#
#
#
#
# fit with VOI enabled

fit_voi_wishart <- MA_NB_tri(data = data_ADNEXCA125,
                     tp = TP, tn = TN, n_event = n_event, n_nonevent = n_nonevent,
                     prior_type = "wishart",
                     t = 0.1,
                     return_vars = c("NB", "RU", "probuseful", "sens", "spec"),
                     seed = 123,
                     prev_known = 0.5,
                     return_known = TRUE,
                     compute_EVPI = TRUE,
                     auto_resample = FALSE,
                     center_rows = 1:36,
                     center_label_cols = c("Publication", "Country")
                     )

#
#
#
