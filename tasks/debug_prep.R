devtools::load_all()
library(posterior)

fit1 <- readRDS("tasks/fixtures/fit1.rds")

cat("=== Checking obs_model structure ===\n")
cat("obs_model is NULL:", is.null(fit1$obs_model), "\n")
cat("obs_model class:", class(fit1$obs_model), "\n")
cat("obs_model has formula:", !is.null(fit1$obs_model$formula), "\n")
cat("obs_model has family:", !is.null(fit1$obs_model$family), "\n")
cat("obs_model has data:", !is.null(fit1$obs_model$data), "\n\n")

cat("=== Creating mock stanfit ===\n")
obs_params <- extract_obs_parameters(fit1)
cat("Observation parameters:", obs_params, "\n")

full_draws <- posterior::as_draws_matrix(fit1$fit)
obs_draws <- posterior::subset_draws(full_draws, variable = obs_params)
cat("obs_draws class:", class(obs_draws), "\n")
cat("obs_draws dimensions:", dim(obs_draws), "\n\n")

mock_obs <- create_mock_stanfit(obs_draws)
cat("mock_obs class:", class(mock_obs), "\n\n")

cat("=== Testing brms::make_standata directly ===\n")
newdata <- data.frame(
  time = 25:27,
  series = factor(rep("series1", 3)),
  x = c(0.5, -0.3, 0.8)
)

tryCatch({
  standata_test <- brms::make_standata(
    fit1$obs_model,
    newdata = newdata
  )
  cat("make_standata succeeded!\n")
  cat("standata names:\n")
  print(names(standata_test))
  cat("\nKey standata components:\n")
  cat("- Has X:", "X" %in% names(standata_test), "\n")
  cat("- Has N:", "N" %in% names(standata_test), "\n")
}, error = function(e) {
  cat("make_standata ERROR:\n")
  cat(conditionMessage(e), "\n")
})

cat("\n=== Testing brms::standata with internal=TRUE ===\n")

# Add dummy response variable (values don't matter for prediction)
newdata_for_standata <- newdata
resp_var <- as.character(fit1$obs_model$formula$formula[[2]])
cat("Response variable name:", resp_var, "\n")
if (!resp_var %in% names(newdata_for_standata)) {
  newdata_for_standata[[resp_var]] <- rep(0, nrow(newdata))
}

tryCatch({
  sdata <- brms::standata(
    fit1$obs_model,
    newdata = newdata_for_standata,
    internal = TRUE
  )
  cat("SUCCESS: brms::standata with internal=TRUE worked!\n")
  cat("sdata names:\n")
  print(names(sdata))
  cat("\nsdata structure (first level):\n")
  str(sdata, max.level = 1)
  cat("\n")

  # Check key components
  cat("Key components:\n")
  cat("- Has X:", "X" %in% names(sdata), "\n")
  cat("- Has N:", "N" %in% names(sdata), "\n")
  if ("X" %in% names(sdata)) {
    cat("- X dimensions:", dim(sdata$X), "\n")
    cat("- X column names:", colnames(sdata$X), "\n")
  }
  cat("\n")

}, error = function(e) {
  cat("ERROR with brms::standata internal=TRUE:\n")
  cat(conditionMessage(e), "\n\n")
})

cat("=== Testing minimal prepare_predictions.mock_stanfit approach ===\n")

# Define the minimal function inline for testing
prepare_predictions_test <- function(object, brmsfit, newdata,
                                     re_formula = NULL,
                                     allow_new_levels = FALSE,
                                     ...) {
  cat("Inside prepare_predictions_test...\n")

  # Add dummy response variable (standata needs it even with internal=TRUE)
  newdata_with_resp <- newdata

  # Extract response variable(s) - handle both univariate and multivariate
  if (brms::is.mvbrmsformula(brmsfit$formula)) {
    # Multivariate: get all response variables
    resp_vars <- names(brmsfit$formula$forms)
  } else {
    # Univariate: get single response
    resp_vars <- as.character(brmsfit$formula$formula[[2]])
  }

  # Add dummy values for any missing response variables
  for (rv in resp_vars) {
    if (!rv %in% names(newdata_with_resp)) {
      newdata_with_resp[[rv]] <- rep(0, nrow(newdata))
    }
  }

  # Use EXPORTED standata with internal=TRUE
  sdata <- brms::standata(
    brmsfit,
    newdata = newdata_with_resp,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    internal = TRUE,
    ...
  )

  cat("Got sdata\n")

  # Extract draws from mock - use our method directly
  draws <- object$draws_cache

  cat("Got draws, class:", class(draws), "\n")

  # Return minimal prep
  prep <- structure(
    list(
      sdata = sdata,
      draws = draws,
      formula = brmsfit$formula,
      family = brmsfit$family,
      newdata = newdata,
      nobs = nrow(newdata)
    ),
    class = c("brmsprep", "mvgam_prep")
  )

  cat("Built prep structure\n")
  return(prep)
}

tryCatch({
  prep_obs_1 <- prepare_predictions_test(
    mock_obs,
    brmsfit = fit1$obs_model,
    newdata = newdata
  )

  cat("SUCCESS: fit1 obs_model prep returned!\n")
  cat("prep class:", class(prep_obs_1), "\n")
  cat("prep names:\n")
  print(names(prep_obs_1))
  cat("\nprep$sdata has X:", "X" %in% names(prep_obs_1$sdata), "\n")
  cat("prep$sdata$X dimensions:", dim(prep_obs_1$sdata$X), "\n")
  cat("prep$draws dimensions:", dim(prep_obs_1$draws), "\n\n")

}, error = function(e) {
  cat("ERROR in fit1 obs_model test:\n")
  cat(conditionMessage(e), "\n\n")
})

# Test fit1 trend_model
cat("=== Testing fit1 trend_model ===\n")

if (!is.null(fit1$trend_model)) {
  cat("Extracting trend parameters...\n")
  trend_params_1 <- extract_trend_parameters(fit1)
  cat("Trend parameters:", head(trend_params_1, 10), "\n")

  trend_draws_1 <- posterior::subset_draws(full_draws, variable = trend_params_1)
  mock_trend_1 <- create_mock_stanfit(trend_draws_1)

  tryCatch({
    prep_trend_1 <- prepare_predictions_test(
      mock_trend_1,
      brmsfit = fit1$trend_model,
      newdata = newdata
    )

    cat("SUCCESS: fit1 trend_model prep returned!\n")
    cat("prep$sdata names:\n")
    print(names(prep_trend_1$sdata))
    cat("prep$draws dimensions:", dim(prep_trend_1$draws), "\n\n")

  }, error = function(e) {
    cat("ERROR in fit1 trend_model test:\n")
    cat(conditionMessage(e), "\n\n")
  })
} else {
  cat("fit1$trend_model is NULL - skipping\n\n")
}

# Load and test fit2 (multivariate)
cat(strrep("=", 78), "\n")
cat("TESTING FIT2: Multivariate Shared RW\n")
cat(strrep("=", 78), "\n\n")

fit2 <- readRDS("tasks/fixtures/fit2.rds")
cat("Response names:", fit2$response_names, "\n\n")

# Test fit2 obs_model
cat("=== Testing fit2 obs_model ===\n")

obs_params_2 <- extract_obs_parameters(fit2)
full_draws_2 <- posterior::as_draws_matrix(fit2$fit)
obs_draws_2 <- posterior::subset_draws(full_draws_2, variable = obs_params_2)
mock_obs_2 <- create_mock_stanfit(obs_draws_2)

newdata_mv <- data.frame(
  time = rep(25:27, each = 2),
  series = factor(rep(c("series1", "series2"), 3)),
  x = rnorm(6),
  presence = rep(1, 6)  # Add for fit3 trend model
)

tryCatch({
  prep_obs_2 <- prepare_predictions_test(
    mock_obs_2,
    brmsfit = fit2$obs_model,
    newdata = newdata_mv
  )

  cat("SUCCESS: fit2 obs_model prep returned!\n")
  cat("prep$sdata has X:", "X" %in% names(prep_obs_2$sdata), "\n")
  if ("X" %in% names(prep_obs_2$sdata)) {
    cat("prep$sdata$X dimensions:", dim(prep_obs_2$sdata$X), "\n")
  }
  cat("prep$draws dimensions:", dim(prep_obs_2$draws), "\n\n")

}, error = function(e) {
  cat("ERROR in fit2 obs_model test:\n")
  cat(conditionMessage(e), "\n\n")
})

# Test fit2 trend_model
cat("=== Testing fit2 trend_model ===\n")

if (!is.null(fit2$trend_model)) {
  trend_params_2 <- extract_trend_parameters(fit2)
  trend_draws_2 <- posterior::subset_draws(full_draws_2, variable = trend_params_2)
  mock_trend_2 <- create_mock_stanfit(trend_draws_2)

  tryCatch({
    prep_trend_2 <- prepare_predictions_test(
      mock_trend_2,
      brmsfit = fit2$trend_model,
      newdata = newdata_mv
    )

    cat("SUCCESS: fit2 trend_model prep returned!\n")
    cat("prep$sdata names:\n")
    print(names(prep_trend_2$sdata))
    cat("prep$draws dimensions:", dim(prep_trend_2$draws), "\n\n")

  }, error = function(e) {
    cat("ERROR in fit2 trend_model test:\n")
    cat(conditionMessage(e), "\n\n")
  })
} else {
  cat("fit2$trend_model is NULL - skipping\n\n")
}

# Load and test fit3 (VARMA with smooths)
cat(strrep("=", 78), "\n")
cat("TESTING FIT3: VARMA with Smooths\n")
cat(strrep("=", 78), "\n\n")

fit3 <- readRDS("tasks/fixtures/fit3.rds")
cat("Response names:", fit3$response_names, "\n\n")

# Test fit3 obs_model
cat("=== Testing fit3 obs_model ===\n")

obs_params_3 <- extract_obs_parameters(fit3)
full_draws_3 <- posterior::as_draws_matrix(fit3$fit)
obs_draws_3 <- posterior::subset_draws(full_draws_3, variable = obs_params_3)
mock_obs_3 <- create_mock_stanfit(obs_draws_3)

tryCatch({
  prep_obs_3 <- prepare_predictions_test(
    mock_obs_3,
    brmsfit = fit3$obs_model,
    newdata = newdata_mv
  )

  cat("SUCCESS: fit3 obs_model prep returned!\n")
  cat("prep$sdata names:\n")
  print(names(prep_obs_3$sdata))

  # Check for smooth-specific components
  cat("\nSmooth-specific checks:\n")
  cat("- Has Xs:", "Xs" %in% names(prep_obs_3$sdata), "\n")
  if ("Xs" %in% names(prep_obs_3$sdata)) {
    cat("- Xs length:", length(prep_obs_3$sdata$Xs), "\n")
  }
  cat("prep$draws dimensions:", dim(prep_obs_3$draws), "\n\n")

}, error = function(e) {
  cat("ERROR in fit3 obs_model test:\n")
  cat(conditionMessage(e), "\n\n")
})

# Test fit3 trend_model
cat("=== Testing fit3 trend_model ===\n")

if (!is.null(fit3$trend_model)) {
  trend_params_3 <- extract_trend_parameters(fit3)
  trend_draws_3 <- posterior::subset_draws(full_draws_3, variable = trend_params_3)
  mock_trend_3 <- create_mock_stanfit(trend_draws_3)

  tryCatch({
    prep_trend_3 <- prepare_predictions_test(
      mock_trend_3,
      brmsfit = fit3$trend_model,
      newdata = newdata_mv
    )

    cat("SUCCESS: fit3 trend_model prep returned!\n")
    cat("prep$sdata names:\n")
    print(names(prep_trend_3$sdata))
    cat("prep$draws dimensions:", dim(prep_trend_3$draws), "\n\n")

  }, error = function(e) {
    cat("ERROR in fit3 trend_model test:\n")
    cat(conditionMessage(e), "\n\n")
  })
} else {
  cat("fit3$trend_model is NULL - skipping\n\n")
}

cat(strrep("=", 78), "\n")
cat("COMPREHENSIVE TESTING COMPLETE\n")
cat(strrep("=", 78), "\n")
