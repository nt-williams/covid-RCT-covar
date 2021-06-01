Optimizing Precision and Power by Covariate Selection in Randomized
Trials with an Application to COVID-19
================

Nicholas Williams, Michael Rosenblum, Iván Díaz

------------------------------------------------------------------------

This repository contains the R code for the simulation studies in the
paper “Optimizing Precision and Power by Covariate Selection in
Randomized Trials with an Application to COVID-19.”

## Directory guide

The `R/` directory contains the source code for functions used in the
simulations. The `scripts/` directory contains R and bash scripts for
cleaning the raw data, calculating true values, running the simulations
on a computing cluster, and turning simulation results into LaTeX
tables.

## Simulation data

The COVID-19 data used in the paper will not be shared. However,
synthetic versions of the data have been made available in the
`data/public` directory.

## adjrct

The estimators described in the paper are implemented with a special
version of the *adjrct* package. The source code for the package is
included with this repository as a .tar.gz file. The package can be
installed with
`install.packages("adjrct_0.1.0.9000.tar.gz", type = "source", repos = NULL)`.

## Running simulations

Simulations are intended to be run on a high performance computing
cluster using Slurm. `scripts/simulate.sh` establishes the computing
parameters (this file contains code specific to the authors machines and
will need to be modified accordingly by other users) and runs
`scripts/simulate.R`. `scripts/simulate.R` uses configuration parameters
defined in `scripts/config.yml` to execute different simulation
scenarios.

## Tutorial

#### Loading data

``` r
box::use(dgm = ./R/data)

synthetic <- dgm$covid("ordinal", private = FALSE)
```

### Generating RCT data

``` r
tmp <- dgm$generate_data(synthetic, "ordinal", prognostic = TRUE, seed = 123, n = 500, effect_size = 3)
head(tmp)
```

    ##         age    sex   bmi         smoke  o2 num_comorbid num_symptoms bilat
    ## 1: 54.93498   Male 25.70            No Yes            0            8   Yes
    ## 2: 87.91513   Male 29.63 Former Smoker Yes            3            4   Yes
    ## 3: 79.86037 Female 29.12            No  No            4            1   Yes
    ## 4: 35.35661   Male 29.50            No Yes            0            4   Yes
    ## 5: 59.33744   Male 24.94            No  No            1            8    No
    ## 6: 61.44011 Female 30.40            No Yes            1            2    No
    ##    dyspnea hyper            time state_ordinal A
    ## 1:       1     0  2.8812500 days             0 0
    ## 2:       0     1  0.1166667 days             4 0
    ## 3:       0     1  3.9277778 days             0 1
    ## 4:       1     0 13.7076389 days             0 0
    ## 5:       1     0  4.7506944 days             0 0
    ## 6:       1     0 14.7111111 days             1 0

### Running a simulation

The `simulate()` function runs a single iteration of a scenario. In the
following example, we run an instance for an ordinal outcome in an RCT
with 500 observations where we adjust for age, sex, dyspnea, and
supplemental o2 use. These covariates are prognostic of the outcome;
variable selection is performed using ℓ1 regularization.

``` r
box::use(sim = ./R/simulate)

sim$simulate(synthetic, type = "ordinal", covar = c("age", "sex", "dyspnea", "o2"), 
             prognostic = TRUE, seed = 123, n = 500, effect_size = 3, algo = "lasso", 
             crossfit = FALSE)
```

    ## $log_or
    ## [1] 0.667766
    ## 
    ## $log_or.std.error
    ## [1] 0.2181065
    ## 
    ## $mannwhit
    ## [1] 0.4563578
    ## 
    ## $mannwhit.std.error
    ## [1] 0.02082584

The `partition()` function takes a data frame of simulation parameters
where each row corresponds to a single iteration and dispatches these
scenarios to the `simulate()` function.
