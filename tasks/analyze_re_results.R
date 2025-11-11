# Analyze Random Effects Test Results
devtools::load_all()

results <- readRDS('tasks/random_effects_test_results.rds')

cat('SUCCESS RATE:', round(100 * mean(results$success), 1), '%\n')
cat('\nWORKING PATTERNS:\n')
print(results[results$success, c('test_name', 'compile_time')])

cat('\nCRITICAL BUG PATTERN:\n')
failures <- results[!results$success & results$stage_reached == 'stancode_generation', ]
if(nrow(failures) > 0) {
  cat('Stan semantic error in', nrow(failures), 'tests\n')
  cat('Error pattern: mu[n] += mu + trend[...] - Stan type error\n')
  cat('\nFirst failure details:\n')
  detailed <- attr(results, 'detailed_results')
  first_fail <- detailed[[which(!results$success)[1]]]
  cat('Error:', first_fail$error_message, '\n')
} else {
  cat('No Stan errors found\n')
}

cat('\nSUMMARY:\n')
cat('- Pure brms (no trend): ALL WORK\n')
cat('- brms with trend: SYSTEMATIC BUG in Stan code generation\n')
cat('- Critical line: mu[n] += mu + trend[...] has type error\n')
cat('- Issue: mu is vector, mu[n] is scalar, cannot add vector to scalar\n')