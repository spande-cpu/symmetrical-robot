---
title: Regression and the Omitted Variable Bias
author: "Shashwat M. Pande"
date: '2023-08-01'
slug: regression-and-the-omitted-variable-bias
categories:
  - Data Science
tags:
  - Data Science
subtitle: ''
summary: ''
authors: []
lastmod: '2023-08-01T15:03:31+01:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

## Introduction

When estimating a regression model, we are often interested in how a change in level of a particular variable (or treatment) affects our outcome of interest.

We are looking to estimate the distribution of some random variable `\(Y\)` as some function, `\(\textit{f}(\bf{X})\)` using some set of linear predictors, `\(\textbf{X}\)` that we suspect might cause `\(Y\)`. The core of the the estimation problem is the identification of which subset of all `\(p\)`, possible predictors from `\(X = (x_1,x_2,...,x_p)\)` to include in our model. Often, the specific predictors we choose are based on substantive considerations made during the definition of our research problem and some form of literature review and together, these aspects tend to define the design an effective experiment to test our proposition against a null-hypothesis ($H_0$).

However, in a world of many possibilities, one of the challenges that a researcher must beware is the omission of variables that might be (causally) related to both the outcome and focal predictor in our analysis which can seriously confound the conclusions drawn from a linear-model.

## Correlation != Causation: Do storks deliver babies?

Suppose we are looking to question the age old folk-tale of whether storks bring new-born babies to their doting parents -- presumably, because we are a little bit bored and feeling somewhat cynical or like behavioural maximizers, prefer to be data-driven having been given such a tantalizingly testable proposition. Perhaps from a similar starting point, although probably a more pedagogically grounded motivation, [Matthews (2000)](http://www.brixtonhealth.com/storksBabies.pdf) presents an analysis of some real statistical data across a sample of European countries where large stork-populations mean that agencies such as the [Royal Society for the Protection of Birds]() painstakingly maintain records on their numbers.

Matthew's analysis shows that in fact, the number of breeding stork-pairs that are found in these countries does indeed relate to human birth-rates and while the correlation is moderate ($\rho = .62$), it is statistically significant ($p = .008$) at the `\(\alpha = 0.05\)` or `\(95\)`% confidence-level, meeting the minimal convention for (sufficient) statistical evidence in most published academic research.

Let's try and reproduce the results in Matthew's paper.


```r
# For tibbles, %>% and everything nice.
library(tidyverse)

# Stork data from Matthews (2000)
storks <- tribble(
 ~Country, ~Area, ~Storks, ~Humans, ~BirthRate,
 "ALB",    28750,   100,     3.2,     83,
 "AUT",    83860,   300,     7.6,     87,
 "BEL",    30520,     1,     9.9,    118,
 "BGR",   111000,  5000,     9.0,    117,
 "DNK",    43100,     9,     5.1,     59,
 "FRA",   544000,   140,    56.0,    774,
 "DEU",   357000,  3300,    78.0,    901,
 "GRC",   132000,  2500,    10.0,    106,
 "NLD",    41900,     4,    15.0,    188,
 "HUN",    93000,  5000,    11.0,    124,
 "ITA",   301280,     5,    57.0,    551,
 "POL",   312680, 30000,    38.0,    610,
 "PRT",    92390,  1500,    10.0,    120,
 "ROU",   237500,  5000,    23.0,    367,
 "ESP",   504750,  8000,    39.0,    439,
 "CHE",    41290,   150,     6.7,     82,
 "TUR",   779450, 25000,    56.0,   1576
)

# Plot Pair-wise Correlations
storks %>%
  select_if(is.numeric) %>%
  GGally::ggpairs() +
  theme_bw() +
  theme(plot.title = element_text(face = "bold")) + 
  ggtitle("Pairwise Correlations in Matthew's (2000) Stork Data") 
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/data-1.png" width="672" />

-   The no. of breeding stork-pairs appear to moderately correlate with human birthrates -- and this relationship is significant at `\(\alpha=.05\)`, very surprising indeed.
-   But this chart is information dense, it tells us a number of additional things.
    -   Birth rates seem to correlate positively with both, the Human population and land area of these countries -- i.e. in addition to the positive pair-wise relationship between the no. of breeding stork-pairs and human birth-rates.
    -   Perhaps not so surprisingly, the no. of stork-pairs is also positively correlated with land area.
    -   The distributions of the variables in our data are positively skewed again, not surprising given the limited sample-size at our disposal.
-   Here's what the linear relationship between `Area`, `Humans`, `Storks` -- i.e. our `\(X\)` variables and human birth-rates looks like).


```r
# Faceted GG-Plot 
storks %>%
  pivot_longer(Area:Humans, names_to = "x_p", values_to = ".measurement") %>%
  ggplot(aes(.measurement, BirthRate)) + 
  geom_point() +
  facet_wrap(vars(x_p), scales = "free_x") +
  theme_minimal() +
  geom_smooth(col = "black", method = "lm") +
  theme(plot.title = element_text(face = "bold")) + 
  xlab(expression(".measurement")) +
  ylab("BirthRate") +
  ggtitle("Linear Relationsip between predictors and human birth-rates") 
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/linear-1.png" width="672" />

## Let's Get Hypothetical: Do storks actually deliver babies?

What if we had defined our alternative hypothesis as follows:

`\(\bf{H_A}\)`: *The no. of breeding stork-pairs in country, i, is positively related to birth-rates*.

We would probably estimate the following model:

`$$\widehat{BirthRate_i} = \beta_0 + \beta_1.Storks_i + \beta_2.Humans_i + \beta_3.Area_i +\epsilon_i$$`

Suppose we collected some data to test this proposition against a null hypothesis but for whatever reason, we only collect data on 2-variables, `Storks` and `BirthRate`. We would like to estimate this relationship using a linear model of the form:

`$$\widehat{BirthRate_i} = \beta_0 + \beta_1Storks_i + \epsilon_i$$`

Since we have the privilege of sufficient data to estimate both models, let's fit and compare their results.


```r
# Fit model 1 and model 2
model1 <- lm(BirthRate ~ Storks, data = storks)
summary(model1)
```

```
## 
## Call:
## lm(formula = BirthRate ~ Storks, data = storks)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -478.8 -166.3 -144.9   -2.0  631.1 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)   
## (Intercept) 2.250e+02  9.356e+01   2.405   0.0295 * 
## Storks      2.879e-02  9.402e-03   3.063   0.0079 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 332.2 on 15 degrees of freedom
## Multiple R-squared:  0.3847,	Adjusted R-squared:  0.3437 
## F-statistic:  9.38 on 1 and 15 DF,  p-value: 0.007898
```

```r
model2 <- lm(BirthRate ~ Storks + Humans + Area, data = storks)
summary(model2)
```

```
## 
## Call:
## lm(formula = BirthRate ~ Storks + Humans + Area, data = storks)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -317.24  -52.95    2.44   73.89  295.48 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -4.824e+01  5.172e+01  -0.933   0.3680  
## Storks       8.965e-03  5.024e-03   1.784   0.0977 .
## Humans       6.369e+00  2.635e+00   2.417   0.0311 *
## Area         9.596e-04  3.240e-04   2.962   0.0110 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 140.3 on 13 degrees of freedom
## Multiple R-squared:  0.9049,	Adjusted R-squared:  0.8829 
## F-statistic: 41.23 on 3 and 13 DF,  p-value: 6.644e-07
```

Interesting. In both models, the estimate for the impact of the no. of breeding stork-pairs and human birth-rates is wildly different. In fact, when we control for `Area` and the no. of `Humans` in these countries, the estimate is nearly 99.1% smaller. Wow, that's quite a difference.

Moreover, when examined independently, the correlation between the number of breeding stork pairs and human birth rates is also 'statistically significant', in that it attains a p-value of `\(0.0295\)`. Does this mean that the relationship between `Storks` and `BirthRate` truly significant, can it be that for every 1000 stork-pairs the human birth rate grows by 28.79 per 1000 people?

## Reproducing the Omitted Variable Bias with Simulated Data


```r
# Reproduce
set.seed(123)
N <- 10000

# Fixed Parameters
c <- 1.5
e0 <- rnorm(n = N, mean = 0, sd = 1)

# Pairwise-correlations between x1 and Z = (z0, z1, ..., zp)
rho <- seq(from = -.99, to = .99, length.out = 50)

# Focal predictor
x1 <- rnorm(N, 0, 1)

# Covariates
Z <- tibble()
for (i in 1:length(rho)) {
  t <- tibble(
    z = (rho[i] * x1) + sqrt(1 - rho[i]*rho[i]) * rnorm(n = N, mean = 0, sd = 1),
    rho = rho[i]
  )
  Z <- bind_rows(Z, t)
} 

# Predictors 
X <- tibble(
  cbind(
    x1 = rep(x1, length(rho)), Z
    )
  )

# Data Frame
df <- X %>%
  group_by(rho) %>%
  mutate(
    # Outcome
    y = c + 1.5*x1 + 0*z + e0
  )

# Model Coefficients Across rho's
df %>%
  split(df$rho) %>%
  map(\(df) lm(y ~ z, data = df)) %>%
  map(\(mod) as.data.frame(t(as.matrix(coef(mod))))) %>%
  list_rbind() %>%
  mutate(rho = rho) %>%
  ggplot(aes(rho, z)) +
  geom_point() +
  geom_hline(aes(yintercept = 0, linetype = "True Value"))
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-1-1.png" width="672" />
