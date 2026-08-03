# blair_cooper_coppock_humphreys_2019/maintained/bs_design.R
# Output: none
# Depends on: helpers.R, and a seed set by the sourcing script
# Description: Declaration of the Bjorkman and Svensson (2009) replication design
#   of appendix section 4. Three scripts diagnose it (Table 4, the sample size
#   comparison and the covariate adjustment comparison), so the declaration is
#   written once here rather than copied into each of them. The fixed population
#   is drawn at declaration time, so the sourcing script must set the seed first.

N_catchment_areas <- 50

infant_prob <- (1135 / (1 - 0.1205)) / 5000

pop <- declare_model(
  catchment_area = add_level(N = 300, area_health = rnorm(N)),
  households = add_level(
    N = 2500,
    infant = rbinom(n = N, 1, prob = infant_prob),
    family_health = rnorm(N)
  )
)

# The population of catchment areas and households is drawn once and held fixed
# across simulations; only the potential outcomes, the sample and the assignment
# are redrawn. Drawing it into an object rather than calling pop() inside
# declare_model() is what makes it genuinely fixed.
pop_data <- pop()

fixed_pop <- declare_model(data = pop_data)

alive_po_function <- function(Z, family_health, area_health, infant) {
  # Pre-1.0 DeclareDesign handed a potential outcomes function a full length Z;
  # it now hands it the single condition value. Z is expanded so that the draw
  # has one entry per household, as the number of trials below assumes.
  Z <- rep(Z, length.out = length(infant))
  alive <- rbinom(
    n = length(Z), size = 1,
    prob = plogis(qlogis(0.86) + 0.5 * Z + family_health + area_health)
  )
  alive[infant == 0] <- NA
  alive
}

pos_alive <- declare_potential_outcomes(
  infant_alive ~ alive_po_function(Z, family_health, area_health, infant)
)

weight_po_function <- function(Z, infant_alive_Z_0, infant_alive_Z_1,
                               family_health, area_health) {
  weight <- 0.14 * Z + family_health + area_health
  masked <- infant_alive_Z_1 * Z + infant_alive_Z_0 * (1 - Z)
  weight[(masked) == 0 | is.na(masked)] <- NA
  weight
}

pos_weight <- declare_potential_outcomes(
  weight_for_age ~ weight_po_function(
    Z, infant_alive_Z_0, infant_alive_Z_1, family_health, area_health
  )
)

cl_mortality_inquiry <- declare_inquiry(
  Mortality = (1 - mean(infant_alive_Z_1)) - (1 - mean(infant_alive_Z_0))
)

hh_weight_inquiry <- declare_inquiry(
  Weight = mean(weight_for_age_Z_1 - weight_for_age_Z_0),
  subset = infant == TRUE & infant_alive_Z_0 == 1 & infant_alive_Z_1 == 1
)

cl_sampling <- declare_sampling(
  S = cluster_rs(clusters = catchment_area, n = N_catchment_areas), filter = S == 1
)

hh_sampling <- declare_sampling(
  S = strata_rs(strata = catchment_area, prob = 100 / 2500), filter = S == 1
)

assignment <- declare_assignment(Z = cluster_ra(clusters = catchment_area))

reveal_outcomes_step <- declare_measurement(
  infant_alive = reveal_outcomes(infant_alive ~ Z),
  weight_for_age = reveal_outcomes(weight_for_age ~ Z)
)

# The mortality inquiry is defined at the catchment area level, so the data are
# collapsed to that level before the mortality estimator runs.
aggregate_data <- declare_step(handler = function(data) {
  aggregate(
    cbind(infant_alive_Z_0, infant_alive_Z_1, infant_alive, Z) ~ catchment_area,
    FUN = mean, na.rm = TRUE, data = data
  )
})

cl_mortality_estimator <- declare_estimator(
  (1 - infant_alive) ~ Z,
  .method = lm_robust, inquiry = cl_mortality_inquiry, label = "Mortality estimate"
)

hh_weight_estimator <- declare_estimator(
  weight_for_age ~ Z,
  .method = lm_robust, clusters = catchment_area,
  inquiry = hh_weight_inquiry, label = "Weight estimate"
)

bjorkman_svensson_design <-
  fixed_pop + pos_alive + pos_weight + cl_sampling + hh_sampling + assignment +
  reveal_outcomes_step + hh_weight_estimator + hh_weight_inquiry +
  aggregate_data + cl_mortality_inquiry + cl_mortality_estimator
