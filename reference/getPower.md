# Calculate power for given success rates

Calculate power for given success rates

## Usage

``` r
getPower(
  piTreatment,
  piControl,
  nTreatment,
  nControl,
  alpha,
  piNull = c(piControl, piControl)
)
```

## Arguments

- piTreatment:

  Treatment success rate.

- piControl:

  Control success rate.

- nTreatment:

  Number of subjects in treatment group.

- nControl:

  Number of subjects in control group.

- alpha:

  One-sided significance level.

- piNull:

  Null hypothesis success rates. Should consist of two entries, the
  first for the treatment success rate under the null hypothesis, the
  second for the control success rate under the null hypothesis.
  Defaults to `piControl` for both entries.

## Value

The power for the given scenario.
