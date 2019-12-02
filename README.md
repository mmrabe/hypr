
# hypr

hypr is an R package for easy translation between experimental (null) hypotheses and contrast matrices as used for linear regression. For an extensive overview of the package functions, see the `hypr-intro` vignette, e.g. by running `vignette("hypr-intro")` after installing the package.

## Installation

Install from CRAN within R using:


```r
install.packages("hypr")
```

Install the development version in R using `devtools`:


```r
devtools::install_github("mmrabe/hypr", build_vignettes = TRUE)
```

## Deriving contrast matrices

For a treatment contrast with a baseline and three treatment conditions:

$$
\begin{align}
H_{0_1}:& \; \mu_1 = 0 \\
H_{0_2}:& \; \mu_2 = \mu_1 \\
H_{0_3}:& \; \mu_3 = \mu_1 \\
H_{0_4}:& \; \mu_4 = \mu_1
\end{align}
$$


```r
trtC <- hypr(mu1~0, mu2~mu1, mu3~mu1, mu4~mu1)
trtC
```

```
## hypr object containing 4 null hypotheses:
## H0.1: 0 = mu1
## H0.2: 0 = mu2 - mu1
## H0.3: 0 = mu3 - mu1
## H0.4: 0 = mu4 - mu1
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
```

To assign the contrast matrix to a factor `fac` with an intermediate hypr object:

```r
contrasts(fac) <- contr.hypothesis(trtC)
```

... or without an intermediate object:

```r
contrasts(fac) <- contr.hypothesis(mu1~0, mu2~mu1, mu3~mu1, mu4~mu1) 
```

For more information, see `vignette("hypr-regression")`.


## Deriving research hypotheses

To check which reserach (null) hypotheses a given contrast matrix is testing, we can create an empty hypr object.


```r
testC <- hypr() # create an empty hypr object
```

A treatment contrast with 4 levels (incl. the baseline) can look as follows:


```r
contr.treatment(4)
```

```
##   2 3 4
## 1 0 0 0
## 2 1 0 0
## 3 0 1 0
## 4 0 0 1
```

We can now populate the hypr object by setting its contrast matrix. Note that the treatment contrast does not have an intercept. We thus have to add it when populating the hypr object:


```r
cmat(testC, add_intercept = TRUE) <- contr.treatment(4) # populate object via contrast matrix
```

Now, the hypr object contains 4 hypotheses, a hypothesis matrix and the contrast matrix identical to the treatment contrast with an intercept added:


```r
testC
```

```
## hypr object containing 4 null hypotheses:
## H0.Intercept: 0 = X1
##         H0.2: 0 = -X1 + X2
##         H0.3: 0 = -X1 + X3
##         H0.4: 0 = -X1 + X4
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
```

The derived hypotheses can be rewritten as:

$$
\begin{align}
H_{0_1}:& \; \mu_1 = 0 \\
H_{0_2}:& \; \mu_2 = \mu_1 \\
H_{0_3}:& \; \mu_3 = \mu_1 \\
H_{0_4}:& \; \mu_4 = \mu_1
\end{align}
$$

For more information, see `vignette("hypr-contrasts")`.


