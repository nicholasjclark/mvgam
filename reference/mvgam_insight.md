# insight S3 methods for mvgam objects

Minimum surface that `marginaleffects` and `insight` need to build
prediction grids and walk formula structure. mvgam keeps its observation
submodel formula in `x$formula` (a `brmsformula`) and its trend submodel
formula in `x$trend_formula`. These methods combine both into the
structures insight expects. The default formula walker fails on the
combined observation and trend representation.
