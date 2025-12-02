# Debug script to trace the "non-conformable arrays" error
devtools::load_all()

cat("=== DEBUGGING SMOOTH ARRAYS ERROR ===\n\n")

# Test fit3
cat("Testing fit3...\n")
model <- readRDS('tasks/fixtures/fit3.rds')
newdata <- model$data[1:3,]
obs_params <- extract_obs_parameters(model)
full_draws <- posterior::as_draws_matrix(model$fit)
obs_draws <- full_draws[, obs_params, drop = FALSE]
mock_fit <- create_mock_stanfit(obs_draws)
prep <- prepare_predictions.mock_stanfit(mock_fit, model$obs_model, newdata)

cat("\nAttempting linear predictor extraction...\n")
tryCatch({
  linpred <- extract_linpred_from_prep(prep)
  cat("SUCCESS: Linear predictor extracted\n")
  cat("Result structure:\n")
  if (is.list(linpred)) {
    for (nm in names(linpred)) {
      cat("  ", nm, ": ", paste(dim(linpred[[nm]]), collapse="×"), "\n")
    }
  } else {
    cat("  Dims: ", paste(dim(linpred), collapse="×"), "\n")
  }
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
})

cat("\n=== Testing fit13 ===\n")
model13 <- readRDS('tasks/fixtures/fit13.rds')
newdata13 <- model13$data[1:3,]
obs_params13 <- extract_obs_parameters(model13)
full_draws13 <- posterior::as_draws_matrix(model13$fit)
obs_draws13 <- full_draws13[, obs_params13, drop = FALSE]
mock_fit13 <- create_mock_stanfit(obs_draws13)
prep13 <- prepare_predictions.mock_stanfit(mock_fit13, model13$obs_model, newdata13)

cat("\nAttempting linear predictor extraction for fit13...\n")
tryCatch({
  linpred13 <- extract_linpred_from_prep(prep13)
  cat("SUCCESS: Linear predictor extracted\n")
  cat("Result dims: ", paste(dim(linpred13), collapse="×"), "\n")
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
})

cat("\n=== DEBUG COMPLETE ===\n")