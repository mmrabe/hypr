# hypr

[![CRAN](https://www.r-pkg.org/badges/version/hypr)](https://cran.r-project.org/package=hypr)
[![downloads](https://cranlogs.r-pkg.org/badges/hypr)](https://cran.r-project.org/package=hypr)
[![JOSS publication
status](https://joss.theoj.org/papers/10.21105/joss.02134/status.svg)](https://joss.theoj.org/papers/10.21105/joss.02134)
[![DOI](https://zenodo.org/badge/208564895.svg)](https://zenodo.org/badge/latestdoi/208564895)
[![Package
status](https://github.com/mmrabe/hypr/actions/workflows/r.yml/badge.svg)](https://github.com/mmrabe/hypr/actions/workflows/r.yml)

hypr is an R package for easy translation between experimental (null)
hypotheses and contrast matrices as used for linear regression. For an
extensive overview of the package functions, see the `hypr-intro`
vignette, e.g. by running `vignette("hypr-intro")` after installing the
package.

## Installation

Install from CRAN within R using:

``` r
install.packages("hypr")
```

Install the development version in R using `devtools`:

``` r
devtools::install_github("mmrabe/hypr", build_vignettes = TRUE)
```

## Deriving contrast matrices

For a treatment contrast with a baseline and three treatment conditions:

![](https://render.githubusercontent.com/render/math?math=H_%7B0_1%7D:%20%5Cmu_1%20=%200)

![](https://render.githubusercontent.com/render/math?math=H_%7B0_2%7D:%20%5Cmu_2%20=%20%5Cmu_1)

![](https://render.githubusercontent.com/render/math?math=H_%7B0_3%7D:%20%5Cmu_3%20=%20%5Cmu_1)

![](https://render.githubusercontent.com/render/math?math=H_%7B0_4%7D:%20%5Cmu_4%20=%20%5Cmu_1)

``` r
trtC <- hypr(mu1~0, mu2~mu1, mu3~mu1, mu4~mu1)
trtC
```

    ## hypr object containing 4 null hypotheses:
    ## H0.1: 0 = mu1        (Intercept)
    ## H0.2: 0 = mu2 - mu1
    ## H0.3: 0 = mu3 - mu1
    ## H0.4: 0 = mu4 - mu1
    ## 
    ## Call:
    ## hypr(mu1 ~ 0, mu2 - mu1 ~ 0, mu3 - mu1 ~ 0, mu4 - mu1 ~ 0, levels = c("mu1", 
    ## "mu2", "mu3", "mu4"))
    ## 
    ## Hypothesis matrix (transposed):
    ##     [,1] [,2] [,3] [,4]
    ## mu1  1   -1   -1   -1  
    ## mu2  0    1    0    0  
    ## mu3  0    0    1    0  
    ## mu4  0    0    0    1  
    ## 
    ## Contrast matrix:
    ##     [,1] [,2] [,3] [,4]
    ## mu1 1    0    0    0   
    ## mu2 1    1    0    0   
    ## mu3 1    0    1    0   
    ## mu4 1    0    0    1

To assign the contrast matrix to a factor `fac` with an intermediate
hypr object:

``` r
contrasts(fac) <- contr.hypothesis(trtC)
```

… or without an intermediate object:

``` r
contrasts(fac) <- contr.hypothesis(mu1~0, mu2~mu1, mu3~mu1, mu4~mu1) 
```

For more information, see `vignette("hypr-regression")`.

## Deriving research hypotheses

To check which reserach (null) hypotheses a given contrast matrix is
testing, we can create an empty hypr object.

``` r
testC <- hypr() # create an empty hypr object
```

A treatment contrast with 4 levels (incl. the baseline) can look as
follows:

``` r
contr.treatment(4)
```

    ##   2 3 4
    ## 1 0 0 0
    ## 2 1 0 0
    ## 3 0 1 0
    ## 4 0 0 1

We can now populate the hypr object by setting its contrast matrix. Note
that the treatment contrast does not have an intercept. We thus have to
add it when populating the hypr object:

``` r
cmat(testC, add_intercept = TRUE) <- contr.treatment(4) # populate object via contrast matrix
```

Now, the hypr object contains 4 hypotheses, a hypothesis matrix and the
contrast matrix identical to the treatment contrast with an intercept
added:

``` r
testC
```

    ## hypr object containing 4 null hypotheses:
    ## H0.Intercept: 0 = X1        (Intercept)
    ##         H0.2: 0 = -X1 + X2
    ##         H0.3: 0 = -X1 + X3
    ##         H0.4: 0 = -X1 + X4
    ## 
    ## Call:
    ## hypr(Intercept = X1 ~ 0, `2` = -X1 + X2 ~ 0, `3` = -X1 + X3 ~ 
    ##     0, `4` = -X1 + X4 ~ 0, levels = c("X1", "X2", "X3", "X4"))
    ## 
    ## Hypothesis matrix (transposed):
    ##    Intercept 2  3  4 
    ## X1  1        -1 -1 -1
    ## X2  0         1  0  0
    ## X3  0         0  1  0
    ## X4  0         0  0  1
    ## 
    ## Contrast matrix:
    ##    Intercept 2 3 4
    ## X1 1         0 0 0
    ## X2 1         1 0 0
    ## X3 1         0 1 0
    ## X4 1         0 0 1

The derived hypotheses can be rewritten as:

![](https://render.githubusercontent.com/render/math?math=H_%7B0_1%7D:%20%5Cmu_1%20=%200)

![](https://render.githubusercontent.com/render/math?math=H_%7B0_2%7D:%20%5Cmu_2%20=%20%5Cmu_1)

![](https://render.githubusercontent.com/render/math?math=H_%7B0_3%7D:%20%5Cmu_3%20=%20%5Cmu_1)

![](https://render.githubusercontent.com/render/math?math=H_%7B0_4%7D:%20%5Cmu_4%20=%20%5Cmu_1)

For more information, see `vignette("hypr-contrasts")`.

## Community guidelines

If you want to report a bug, are having technical difficulties or want
to recommend features, it’s best to open a [Github
Issue](https://github.com/mmrabe/hypr/issues/new/choose). If you want to
suggest a specific implementation of a feature or bug fix, you’re
welcome to fork the repository and submit a pull request! Alternatively,
if you are having problems or questions, you can also send an e-mail
(<maximilian.rabe@uni-potsdam.de>).
