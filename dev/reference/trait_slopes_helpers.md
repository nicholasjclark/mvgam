# Trait-mediated responses for `jsdgam()`

Internal helpers that expand `formula` and `trait_slopes` into the
fourth-corner model

## Details

\$\$\beta\_{s,k} = \gamma\_{0,k} + \sum_l T\_{s,l}\\ \gamma\_{l,k} +
\epsilon\_{s,k}\$\$

where each term `k` of `formula` takes a shared effect, an effect of
each trait `l` and a species deviation. The expansion adds terms to
`formula` and leaves its own terms as written:

`formula = y ~ env + s(x)` `trait_slopes = ~ size`

becomes

`y ~ env + s(x) + size + env:size + s(x, by = size) + (1 + env | sp | series) + s(x, by = series, m = 1)`

The `|sp|` tag correlates the species deviations of the intercept and
the linear terms under one LKJ prior. A smooth's species deviation is a
smooth per species penalised towards a flat line (the "GI" model of
Pedersen et al. 2019).
[`species_effects()`](https://nicholasjclark.github.io/mvgam/dev/reference/species_effects.md)
reports the species' responses and the trait effects.

## References

Pedersen, E. J., Miller, D. L., Simpson, G. L. and Ross, N. (2019).
Hierarchical generalized additive models in ecology: an introduction
with mgcv. *PeerJ* 7:e6876.
[doi:10.7717/peerj.6876](https://doi.org/10.7717/peerj.6876)
