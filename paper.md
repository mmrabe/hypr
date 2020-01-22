---
title: 'hypr: An R package for hypothesis-driven contrast coding'
authors:
- affiliation: 1
  name: Maximilian M. Rabe
  orcid: 0000-0002-2556-5644
- affiliation: 1
  name: Shravan Vasishth
  orcid: 0000-0003-2027-1994
- affiliation: 1
  name: Sven Hohenstein
  orcid: 0000-0002-9708-1593
- affiliation: 1
  name: Reinhold Kliegl
  orcid: 0000-0002-0180-8488
- affiliation: 1, 2
  name: Daniel J. Schad
  orcid: 0000-0003-2586-6823
date: "21 January 2020"
output:
  html_document:
    df_print: paged
bibliography: paper.bib
tags:
- R
- psychology
- linguistics
- linear regression
- linear model
- statistics
- research methods
- research hypotheses
affiliations:
- index: 1
  name: University of Potsdam
- index: 2
  name: Tilburg University
---



# Summary

The `hypr` package in R provides the researcher with a straightforward interface to generate contrast matrices from research hypotheses and the reverse. It can be used to derive contrast matrices for custom research hypotheses and as an instructional tool to understand what research hypotheses a given contrast matrix tests.

# Contrast coding in R

In many empirical sciences, the linear model and variants are used to investigate the statistical relationship between a continuous *dependent (outcome) variable*, such as response times or percent correct in cognitive research, and one or more *independent (explanatory) variables*. If independent variables are categorical (or, `factor` instances in R), such as the assignment to a baseline and a treatment group, or other categorical experimental conditions, then the researcher must decide how to code the contrasts between the levels of that independent variable, typically termed *contrast coding*.

For a set of standard contrasts (e.g., treatment contrast, sum contrast, Helmert contrast), coding schemes are readily available as part of the base R `stats` package. One such example is the function `contr.treatment()` for treatment contrasts. Each of these functions generates a *contrast matrix*, e.g.:


```r
contr.treatment(c("baseline","trt1","trt2"))
```

```
##          trt1 trt2
## baseline    0    0
## trt1        1    0
## trt2        0    1
```

A contrast matrix can be easily assigned to a `factor`:


```r
contrasts(some_factor) <- contr.treatment(c("baseline","trt1","trt2"))
```

When fitting a linear model, R will transform the factor to as many contrasts as there are columns in the contrast matrix. For each factor level, it will assign the respective contrast coefficient as a numeric value to the contrast for each observation, and it will use these numeric contrasts as covariates for fitting the linear model.

# Contrast coding with hypr

While for simpler contrast coding schemes, contrast matrices may be easily interpretable, when custom research hypotheses are tested, contrast coding schemes can be more challenging to understand and to correctly specify. The `hypr` package in R provides a set of functions to aid in this process. It allows translating an arbitrary set of research hypotheses into a contrast matrix, and in reverse allows translating arbitrary contrast matrices into the hypotheses that they test. We reviewed this statistical methodolgy in a tutorial on contrast coding based on the generalized matrix inverse [@Schad2020].

Most importantly, the package provides the `hypr()` function which constructs a `hypr` object. An arbitrary set of linear research (null) hypotheses can be passed as arguments. For example, to create a `hypr` object that tests a baseline condition against zero and two treatment conditions against the baseline (i.e., a treatment contrast with one baseline and two treatments), can be created as follows:


```r
trtC <- hypr(baseline~0, trt1~baseline, trt2~baseline)
trtC
```

```
## hypr object containing 3 null hypotheses:
## H0.1: 0 = baseline
## H0.2: 0 = trt1 - baseline
## H0.3: 0 = trt2 - baseline
## 
## Hypothesis matrix (transposed):
##          [,1] [,2] [,3]
## baseline  1   -1   -1  
## trt1      0    1    0  
## trt2      0    0    1  
## 
## Contrast matrix:
##          [,1] [,2] [,3]
## baseline 1    0    0   
## trt1     1    1    0   
## trt2     1    0    1
```

The term `baseline` or $\mu_1$ represents the mean response in the baseline condition, while `trt1` and `trt2`, or $\mu_2$ and $\mu_3$, respectively, represent the means of the response in the two treatment conditions. As can be seen above, the object contains three research (null) hypotheses, which are represented as the three columns of the "Hypothesis matrix (transposed)". Hypothesis $H_{0_1}$ (i.e., the first column of the transposed hypothesis matrix) tests whether the baseline condition is significantly different from zero, while $H_{0_2}$ and $H_{0_3}$ (i.e., columns two and three) each test whether one of the two treatment conditions are significantly different from the baseline condition. These hypotheses can be formally specified as:

\begin{align*}
H_{0_1}: && \mu_1 & = 0 && \\
H_{0_2}: && \mu_2 & = \mu_1 && \\
H_{0_3}: && \mu_3 & = \mu_1 &&
\end{align*}

The *hypothesis matrix* contains the coefficient or weight of each of the group means in each of the hypotheses. Groups that are not part of a particular hypothesis have a weight of zero. The generalized inverse of the hypothesis matrix yields the desired *contrast matrix*.

With the provided `contr.hypothesis()` function, the resulting contrast matrix (as seen in the lower third of the output above) can be derived and -- as expected -- it equals the output of the `contr.treatment(3)` function call:


```r
contr.hypothesis(trtC)
```

```
##          [,1] [,2]
## baseline    0    0
## trt1        1    0
## trt2        0    1
```

This matrix can be assigned to a factor in the same way as the matrices generated by the `contr.*()` functions in R:


```r
contrasts(some_factor) <- contr.hypothesis(trtC)
```

The package provides more useful functions to retrieve and modify a `hypr` object's contrast matrix or (transposed) hypothesis matrix, such as `cmat()`, `thmat()` and `hmat()` for example.

# Contrast validation with hypr

As previously mentioned, `hypr` can also be used to derive a set of tested research (null) hypotheses from a given contrast matrix. This can facilitate verifying what a given contrast matrix is testing. To do so, one can create an empty `hypr` object and set its contrast matrix to whatever matrix is to be inspected.

We here consider a successive difference contrast, which is designed to test the differences between each successive pair of ordered groups [@Venables2002]. For example, with four levels, the contrast should test three hypotheses: the difference between the second and the first factor level, between the third and second factor level, as well as between the fourth and third level. This is not intuitive by looking at the contrast matrix alone:


```r
MASS::contr.sdif(4)
```

```
##     2-1  3-2   4-3
## 1 -0.75 -0.5 -0.25
## 2  0.25 -0.5 -0.25
## 3  0.25  0.5 -0.25
## 4  0.25  0.5  0.75
```

After initializing the `hypr` object, its contrast matrix can be set to a successive difference contrast as follows:


```r
sdifC <- hypr()
cmat(sdifC) <- MASS::contr.sdif(4)
sdifC
```

```
## hypr object containing 3 null hypotheses:
## H0.2-1: 0 = -X1 + X2
## H0.3-2: 0 = -X2 + X3
## H0.4-3: 0 = -X3 + X4
## 
## Hypothesis matrix (transposed):
##    2-1 3-2 4-3
## X1 -1   0   0 
## X2  1  -1   0 
## X3  0   1  -1 
## X4  0   0   1 
## 
## Contrast matrix:
##    2-1  3-2  4-3 
## X1 -3/4 -1/2 -1/4
## X2  1/4 -1/2 -1/4
## X3  1/4  1/2 -1/4
## X4  1/4  1/2  3/4
```

When evaluating the output, it can be seen that the "hypothesis matrix (transposed)" (i.e., the transposed generalized inverse of the contrast matrix) now shows much more meaningful information (compared to the contrast matrix): it shows the hypotheses that the contrast matrix tests, and can thus be used to interpret the results.

The derived research hypotheses can be rewritten as:

\begin{align*}
H_{0_1}:&& \mu_2 & = \mu_1 && \\
H_{0_2}:&& \mu_3 & = \mu_2 && \\
H_{0_3}:&& \mu_4 & = \mu_3 &&
\end{align*}

As expected, the contrasts test each of the successive differences between $\mu_1$, $\mu_2$, $\mu_3$, and $\mu_4$ as outlined above. In a case, where we would be unsure about what a given contrast matrix tests, this hypothesis matrix could thus show us the hypotheses that are tested by the contrast matrix. In order to learn what the intercept term tests in this example case, we can add an intercept to the contrast matrix:


```r
cmat(sdifC) <- cbind(int = 1, MASS::contr.sdif(4))
sdifC
```

```
## hypr object containing 4 null hypotheses:
## H0.int: 0 = 1/4*X1 + 1/4*X2 + 1/4*X3 + 1/4*X4
## H0.2-1: 0 = -X1 + X2
## H0.3-2: 0 = -X2 + X3
## H0.4-3: 0 = -X3 + X4
## 
## Hypothesis matrix (transposed):
##    int 2-1 3-2 4-3
## X1 1/4  -1   0   0
## X2 1/4   1  -1   0
## X3 1/4   0   1  -1
## X4 1/4   0   0   1
## 
## Contrast matrix:
##    int  2-1  3-2  4-3 
## X1    1 -3/4 -1/2 -1/4
## X2    1  1/4 -1/2 -1/4
## X3    1  1/4  1/2 -1/4
## X4    1  1/4  1/2  3/4
```

In the first column of the transposed hypothesis matrix, we can see that the intercept term -- as is typically the case for centered contrasts -- is the grand mean, i.e. the mean of means:

\begin{align*}
H_{0_{Int}}:&& 0 & = \frac{\mu_1+\mu_2+\mu_3+\mu_4}{4} &&
\end{align*}

# Acknowledgements

The development of this package was supported by German Research Foundation (DFG)/SFB 1287 _Limits of variability in language_ and Center for Interdisciplinary Research, Bielefeld (ZiF)/Cooperation Group _Statistical models for psychological and linguistic data_.

# References
