# The `dshare` R package
![R CMD check](https://github.com/ceopinio/dshare/actions/workflows/r.yml/badge.svg)

`dshare` is an R package from the Centre d'Estudis d'Opinió to
estimate vote shares at the district level. 

The package provides one function, `dshare`, which estimates a
multivariate (i.e., with several outcomes) Bayesian linear regression
through [Stan](https://mc-stan.org). The left-hand side of the model
is a (factor) variable with the party choice for a respondent. The
right-hand side is a (factor) variable with the district of the
respondent. The coefficients of the regression are constrained to add
up to 1. The user can specify priors for the vote share in each
district.

The package can be used to estimate district-level vote shares when
the number of observations in each district is small and the analyst
has access to sensible priors (like previous electoral results). In
essence, the model will calculate a weighted average between the
priors and the observed vote shares in the data -- ensuring that the
resulting shares are valid proportions.

# An example 

Consider, for instance, the following cross tabulation of reported
vote choice by district:

```r
R> xtabs(~ intention + provincia, data=bop)
                         provincia
intention                 Barcelona Girona Lleida Tarragona
  PPC                            31      1      0        11
  ERC                           289     47     16        70
  PSC                           216     12      9        24
  Cs                             18      2      1         4
  CUP                            76     18      3         9
  Junts.per.Catalunya            89     22     18        24
  Catalunya.en.Comu.Podem        95      4      0         7
  Vox                            32      6      1         6
  Altres.partits                 21      6      1         4
```

The sample size for most of the districts is too small to make
reasonable inferences about vote choice using the data in the survey
alone.

However, suppose that the analyst has some additional information
about the support for each party in each district. This additional
information can be expressed in the form of the following priors:

```r
R> P
                         provincia
party                     Barcelona Girona Lleida Tarragona
  PPC                        0.0556 0.0285 0.0500    0.0591
  ERC                        0.2313 0.2577 0.3098    0.2773
  PSC                        0.2719 0.1719 0.1678    0.2176
  Cs                         0.0435 0.0242 0.0236    0.0373
  CUP                        0.0682 0.1018 0.0822    0.0734
  Junts.per.Catalunya        0.1411 0.2682 0.2271    0.1525
  Catalunya.en.Comu.Podem    0.0816 0.0444 0.0349    0.0517
  Vox                        0.0779 0.0640 0.0567    0.0937
  Altres.partits             0.0291 0.0393 0.0477    0.0373
```

The `dshare` estimates a model that combines the two pieces of
information. In this case, it estimates a Bayesian linear regression
between the declared `intention` of each respondent and their
`provincia` with priors centered around the analyst expectations with
a standard deviation (for each party in each district) of 0.025
points. The `weights` argument consumes the survey case weights. All
other arguments are passed directly to `Stan` and can be
used to control the sampling.

```r
fit <- dshare(intention ~ provincia,
              weights=weight,
              data=bop,
              priors=P,
              sd=.025,
              chains=3)
```

The result from the model is 

```r
R> pestimates <- rstan::extract(fit) # Extracts samples
R> pestimates <- apply(pestimates$beta, c(2, 3), mean) # Averages over samples
R> dimnames(pestimates) <- dimnames(t(cfactors)) # Assigns dim names
R> t(pestimates)
                         provincia
party                     Barcelona Girona Lleida Tarragona
  PPC                        0.0523 0.0175 0.0358    0.0889
  ERC                        0.2304 0.2653 0.3020    0.2851
  PSC                        0.2828 0.1633 0.1734    0.2057
  Cs                         0.0383 0.0216 0.0266    0.0465
  CUP                        0.0705 0.1097 0.0753    0.0602
  Junts.per.Catalunya        0.1319 0.2578 0.2684    0.1638
  Catalunya.en.Comu.Podem    0.0889 0.0358 0.0256    0.0393
  Vox                        0.0774 0.0794 0.0550    0.0790
  Altres.partits             0.0276 0.0496 0.0378    0.0315
```
# Installation

The package is not available on CRAN and must be installed from the
GitHub repository directly. 

```R
install.packages("remotes")
remotes::install_github("ceopinio/dshare", subdir="dshare")
```
