FA10 - Two-Way Mixed ANOVA - Report on the Effects of Margarine Type and
Time on Cholesterol Levels
================
Cuerdo, Naomi Hannah A., Percia, Kyte Daiter M.
2024-11-26

# Introduction

This report examines the effect of margarine brand and time on
cholesterol levels. Cholesterol levels were recorded on a continuous
scale, with higher values indicating greater cholesterol concentrations.
The margarine brand (“A” and “B”) and time of measurement (“Before”,
“After 4 Weeks”, and “After 8 Weeks”) were used as independent variables
to determine their effects on cholesterol levels. Furthermore, this
report also discusses the interaction between margarine brand and time
to understand how these factors influence changes in cholesterol over
time.

# Method

## Participants

Participants in the study consisted of 18 individuals, categorized based
on the type of margarine they consumed:

- Margarine:
  - Brand A: 9 Participants
  - Brand B: 9 Participants

On the other hand, Cholesterol levels, the dependent variable, were
measured at three different time points: **Before, After 4 weeks, and
After 8 weeks**. These are the independent variables, measured at a
continuous scale.

This dataset provides a basis for examining how margarine type and time
influence changes in cholesterol levels.

## Procedure

The data was analyzed using a Two-Way Mixed ANOVA with one
between-subjects factor (margarine brand) and one within subjects
(time). This statistical method was chosen to assess the main effects of
margarine type and time, as well as their potential interaction, on
cholesterol levels.

Below are the assumptions:

***Assumption 1.*** There is a continuous dependent variable.
(Cholesterol Level)

***Assumption 2.*** There are between-subjects factor (*Margarine*) that
is categorical with two or more categories.

***Assumption 3.*** There are one within-subjects factor (*Time*) that
is categorical with two or more categories.

***Assumption 4.*** There should be no significant outliers in any cell
of the design.

***Assumption 5.*** The dependent variable should be approximately
normally distributed for each cell of the design.

***Assumption 6.*** The variance of your dependent variable should be
equal between the groups of the between-subjects factor, referred to as
the assumption of homogeneity of variances.

***Assumption 7.*** There should be homogeneity of covariances.

***Assumption 8.*** The variance of differences between groups should be
equal, referred to as the assumption of sphericity.

# Hypotheses

**1.Main Effect of Margarine Type:** H₀:There is no significant effect
of margarine type on cholesterol levels. H₁: There is a significant
effect of margarine type on cholesterol levels.

**2.Main Effect of Margarine Type:** H₀:There is no significant effect
of time on cholesterol levels: H₁: There is a significant effect of time
on cholesterol levels.

**3.Interaction Effect (Margarine Type x Time):** H₀:There is no
significant effect between  
margarine type and time on cholesterol levels. H₁: There is a
significant effect of between margarine type and time on cholesterol
levels.

# Descriptive Analysis

``` r
descriptive_stats <- data %>%
  gather(key = "Time", value = "Cholesterol", Before, After4weeks, After8weeks) %>%
  group_by(Margarine, Time) %>%
  summarise(
    `Mean Cholesterol Level` = mean(Cholesterol, na.rm = TRUE),
    `Standard Deviation` = sd(Cholesterol, na.rm = TRUE),
    .groups = "drop"
  )
```

``` r
print(descriptive_stats)
```

    ## # A tibble: 6 × 4
    ##   Margarine Time        `Mean Cholesterol Level` `Standard Deviation`
    ##   <chr>     <chr>                          <dbl>                <dbl>
    ## 1 A         After4weeks                     5.47                1.39 
    ## 2 A         After8weeks                     5.41                1.37 
    ## 3 A         Before                          5.94                1.43 
    ## 4 B         After4weeks                     6.14                0.815
    ## 5 B         After8weeks                     6.08                0.779
    ## 6 B         Before                          6.78                0.866

``` r
str(data_long$Cholesterol)
```

    ##  num [1:54] 6.42 5.83 5.75 6.76 6.2 6.13 6.56 5.83 5.71 4.8 ...

The dataset included cholesterol levels of participants measured across three time points: **Before**, **After 4 Weeks**, **After 8 Weeks**, with two treatment groups (**Margarine A** and **Margarine B**). Table 1 summarizes the means and standard deviations for each group at each time point.

The dependent variable (*cholesterol levels*) was measured on a continuous scale, with the range as numeric.

*Assumption 1 was achieved*.

``` r
table(data_long$Margarine)
```

    ## 
    ##  A  B 
    ## 24 30

The independent variables consist of between-subject factors. The between-subjects factor, *Margarine*, consists of two categorical levels: **Margarine A** (n = 24) and **Margarine B** (n = 30), as confirmed by the frequency table above. 

*Assumption 2 was achieved*.

``` r
levels(data_long$Time)
```

    ## [1] "Before"      "After4weeks" "After8weeks"

The independent variables consist of within-subject factors. The within-subjects factor, *Time*, consists of three repeated measures: **Before**, **After 4 Weeks**, **After 8 Weeks**.

*Assumption 3 was achieved*

Checking for Outliers:

``` r
boxplot(ID ~ Margarine * Time, data = data_long, 
        main = "Boxplot of Cholesterol Levels by Margarine and Time")
```

![](SEC1_FA10-Group-1-Cuerdo_Percia_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

The boxplot above shows no significant outliers in cholesterol levels across the groups. This indicates that the data distribution for each group is consistent and not unduly influence by extreme values.

*Assumption 4 was achieved.*

``` r
by(data_long$Cholesterol, list(data_long$Time, data_long$Margarine), shapiro.test)
```

    ## : Before
    ## : A
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  dd[x, ]
    ## W = 0.90053, p-value = 0.2922
    ## 
    ## ------------------------------------------------------------ 
    ## : After4weeks
    ## : A
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  dd[x, ]
    ## W = 0.87106, p-value = 0.1544
    ## 
    ## ------------------------------------------------------------ 
    ## : After8weeks
    ## : A
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  dd[x, ]
    ## W = 0.87638, p-value = 0.1738
    ## 
    ## ------------------------------------------------------------ 
    ## : Before
    ## : B
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  dd[x, ]
    ## W = 0.88095, p-value = 0.1338
    ## 
    ## ------------------------------------------------------------ 
    ## : After4weeks
    ## : B
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  dd[x, ]
    ## W = 0.92494, p-value = 0.4
    ## 
    ## ------------------------------------------------------------ 
    ## : After8weeks
    ## : B
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  dd[x, ]
    ## W = 0.89941, p-value = 0.2159

Assessing the normality of cholesterol levels, through Shapiro-Wilk test, across the combinations of the within-subjects factor (*Time*: Before, After 4 Weeks, After 8 Weeks) and the between-subject factor (*Margarine*: A, B). 

All p-values were **greater than 0.05**, indicateding that the assumption of normality was met for each group.

*Assumption 5 was achieved.*

``` r
levene_test <- leveneTest(Cholesterol ~ Margarine * Time, data = data_long)
print("Levene's Test for Homogeneity of Variance:")
```

    ## [1] "Levene's Test for Homogeneity of Variance:"

``` r
print(levene_test)
```

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##       Df F value  Pr(>F)  
    ## group  5  2.6669 0.03315 *
    ##       48                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Levene's Test for Homogeneity of Variance was conducted to evaluate whether the variances of cholesterol levels were equal across the combiniations of the within-subjects factor and the between-subjects factor.

The test revealed a statistically significant reuslt, *F*(5, 48) = 2.67, *p* = 0.33, indicating the assumption of homogeneity of variance was violated.

Since this assumption is essential for the validity of parametric tests for the two-way mixed ANOVA, the analysis cannot procced with the violation. A non-paramatric test alternative will be conducted to ensure the robustness of the findings.

*Assumption 6 was NOT achieved.*.

``` r
friedman_test <- friedman.test(Cholesterol ~ Time | ID, data = data_long)
print(friedman_test)
```

    ## 
    ##  Friedman rank sum test
    ## 
    ## data:  Cholesterol and Time and ID
    ## Friedman chi-squared = 29.778, df = 2, p-value = 3.419e-07

Given the violation of the homogeneity of variance assumption, a Friendman test was conducted as a non-parametric alternative to evaluate differences in cholesterol levels across the three time points.

The results of the Friedman test indicated a statistically significant differencein cholesterol levels across time points, χ² (2, *N* = 54) = 29.78, *p* < .001.

*Assumption 6 was still NOT achieved.*.

``` r
gls_model <- gls(
  Cholesterol ~ Margarine * Time, 
  data = data_long,
  weights = varIdent(form = ~ 1 | Margarine)
)
summary(gls_model)
```

    ## Generalized least squares fit by REML
    ##   Model: Cholesterol ~ Margarine * Time 
    ##   Data: data_long 
    ##        AIC      BIC    logLik
    ##   168.7293 183.6989 -76.36463
    ## 
    ## Variance function:
    ##  Structure: Different standard deviations per stratum
    ##  Formula: ~1 | Margarine 
    ##  Parameter estimates:
    ##        B        A 
    ## 1.000000 1.701692 
    ## 
    ## Coefficients:
    ##                               Value Std.Error   t-value p-value
    ## (Intercept)                 5.94500 0.4937974 12.039352  0.0000
    ## MargarineB                  0.83300 0.5578526  1.493226  0.1419
    ## TimeAfter4weeks            -0.47625 0.6983349 -0.681979  0.4985
    ## TimeAfter8weeks            -0.53625 0.6983349 -0.767898  0.4463
    ## MargarineB:TimeAfter4weeks -0.16175 0.7889227 -0.205026  0.8384
    ## MargarineB:TimeAfter8weeks -0.16675 0.7889227 -0.211364  0.8335
    ## 
    ##  Correlation: 
    ##                            (Intr) MrgrnB TmAft4 TmAft8 MB:TA4
    ## MargarineB                 -0.885                            
    ## TimeAfter4weeks            -0.707  0.626                     
    ## TimeAfter8weeks            -0.707  0.626  0.500              
    ## MargarineB:TimeAfter4weeks  0.626 -0.707 -0.885 -0.443       
    ## MargarineB:TimeAfter8weeks  0.626 -0.707 -0.443 -0.885  0.500
    ## 
    ## Standardized residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -1.4570373 -0.6769997 -0.2028623  0.9945532  2.0127838 
    ## 
    ## Residual standard error: 0.8207538 
    ## Degrees of freedom: 54 total; 48 residual

Given again the violation of the homogeneity of variance assumption, a generalized least squares (GLS) model was employed to account for its observed range between data values. 

The results **still** indicate no statistically significant main effects of *Margarine* or *Time*, and no significnat interaction between theese factors on cholesterol levels (*p* > .05 for all predictors).

*Assumption 6 was still NOT achieved.*.

Kruskal-Wallis Test

``` r
kruskal_test <- kruskal.test(Cholesterol ~ Time, data = data_long)
print(kruskal_test)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  Cholesterol by Time
    ## Kruskal-Wallis chi-squared = 2.9771, df = 2, p-value = 0.2257

``` r
kruskal_test <- kruskal.test(Cholesterol ~ Margarine, data = data_long)
print(kruskal_test)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  Cholesterol by Margarine
    ## Kruskal-Wallis chi-squared = 3.9386, df = 1, p-value = 0.04719

``` r
aov_results <- aov(Cholesterol ~ Margarine * Time + Error(ID / Time), data = data_long)
summary(aov_results)
```

    ## 
    ## Error: ID
    ##           Df Sum Sq Mean Sq
    ## Margarine  1 0.8144  0.8144
    ## 
    ## Error: ID:Time
    ##      Df Sum Sq Mean Sq
    ## Time  2  3.207   1.604
    ## 
    ## Error: Within
    ##                Df Sum Sq Mean Sq F value Pr(>F)  
    ## Margarine       1   6.25   6.246   4.758 0.0344 *
    ## Time            2   1.12   0.559   0.426 0.6557  
    ## Margarine:Time  2   0.08   0.038   0.029 0.9716  
    ## Residuals      45  59.07   1.313                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The Kruskal-Wallis test corroborated the mixed ANOVA findings, highlighitng significant difference in cholesterol levels between Margarine A and B but no significant variations over time. Additionally, no evidence of *Margarine* x *Time* ineraction was found, suggesting that the type of margarine did not diffrentially affect cholesterol levels across the three time points.

## Discussion

A series of statistical tests were conducted to examine the effects of margarine type (*Margarine*: A vs. B) and time (*Time*: Before, After 4 Weeks, After 8 Weeks) on cholesterol levels. The assumptions for conducting these tests were evaluated.

The assumption of normality was met based on the Shapiro-Wilk test, where all combinations of *Margarine* and *Time* yielded non-significant results (all *p* > .05). This indicates that the residuals were approximately normally distributed. 

However, the sixth assumption, homogeneity of variances, was assessed using Levene's test, which revealed a significant violation (*F*(5,48) = 2.67, *p* = .033). To address this, non-parametric tests and robust methods were employed. The Kruskal-Wallis test found a significant difference in cholesterol levels between margarine types (χ²(1) = 3.94, *p* = .047) but not across time points (χ²(2) = 2.98, *p* = .226). A mixed ANOVA further indicated a significant main effect of margarine type (*F*(1,45) = 4.76, *p* = .034), while the effects of time (*F*(2,45) = 0.43, *p* = .656) and the margarine-by-time interaction (*F*(2,45) = 0.03, *p* = .972) were non-significant. To confirm these findings, a generalized least squares (GLS) model was conducted, which upheld the non-significance of the interaction between margarine type and time (all *p* > .05). Collectively, these analyses highlight that while margarine type significantly affects cholesterol levels, time and the interaction between margarine and time do not have significant effects, even when adjusting for variance heterogeneity.

In summary, cholesterol levels were significantly influenced by the type of margarine but not by time or the interaction between margarine type and time. These findings suggest that margarine type plays a more critical role than duration of consumption in affecting cholesterol levels, emphasizing the importance of dietary choices on health outcomes.
