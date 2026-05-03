# Identify the critical value for the exact risk difference test

Identify the critical value for the exact risk difference test

## Usage

``` r
getCriticalValue(
  alpha,
  piTreatment,
  piControl,
  nTreatment,
  nControl,
  mustExceed = TRUE
)
```

## Arguments

- alpha:

  One-sided significance level.

- piTreatment:

  Event rate in treatment group.

- piControl:

  Event rate in control group.

- nTreatment:

  Number of subjects in treatment group.

- nControl:

  Number of subjects in control group.

- mustExceed:

  Logical indicating whether the observed risk difference must exceed
  the critical value for rejection (default `TRUE`). If `FALSE`, the
  observed risk difference must be equal to or greater than the critical
  value.

## Value

Critical value.
