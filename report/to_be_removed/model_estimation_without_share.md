Model Estimation without Share Data
================
Katsuhiro Komatsu
2021-04-04

### Parameter Estimates

#### 1\. Estimating \(\alpha, \lambda^W, \lambda^F\) and other linear parameters

<table class=" lightable-paper lightable-hover" style='font-family: "Arial Narrow", arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>

<caption>

Non-linear Parameters

</caption>

<thead>

<tr>

<th style="text-align:left;">

Parameter

</th>

<th style="text-align:right;">

Est.

</th>

<th style="text-align:right;">

S.E.

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

alpha

</td>

<td style="text-align:right;">

0.511

</td>

<td style="text-align:right;">

51.943

</td>

</tr>

<tr>

<td style="text-align:left;">

lambda\_W

</td>

<td style="text-align:right;">

0.112

</td>

<td style="text-align:right;">

11.421

</td>

</tr>

<tr>

<td style="text-align:left;">

lambda\_F

</td>

<td style="text-align:right;">

0.107

</td>

<td style="text-align:right;">

11.408

</td>

</tr>

</tbody>

</table>

<table class=" lightable-paper lightable-hover" style='font-family: "Arial Narrow", arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>

<caption>

Linear Parameters

</caption>

<thead>

<tr>

<th style="text-align:left;">

Variables

</th>

<th style="text-align:right;">

Coef.

</th>

<th style="text-align:right;">

S.E.

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

daily

</td>

<td style="text-align:right;">

0.122

</td>

<td style="text-align:right;">

0.019

</td>

</tr>

<tr>

<td style="text-align:left;">

shokai

</td>

<td style="text-align:right;">

0.288

</td>

<td style="text-align:right;">

0.018

</td>

</tr>

<tr>

<td style="text-align:left;">

oversea

</td>

<td style="text-align:right;">

0.731

</td>

<td style="text-align:right;">

0.085

</td>

</tr>

<tr>

<td style="text-align:left;">

cocurrent

</td>

<td style="text-align:right;">

0.216

</td>

<td style="text-align:right;">

0.020

</td>

</tr>

</tbody>

</table>

<table class=" lightable-paper lightable-hover" style='font-family: "Arial Narrow", arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>

<caption>

Implied Price Impact

</caption>

<thead>

<tr>

<th style="text-align:left;">

Variables

</th>

<th style="text-align:right;">

Coef.

</th>

<th style="text-align:right;">

Elasticity

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Net Wage

</td>

<td style="text-align:right;">

4.557

</td>

<td style="text-align:right;">

1.151

</td>

</tr>

<tr>

<td style="text-align:left;">

Net Fee

</td>

<td style="text-align:right;">

\-4.549

</td>

<td style="text-align:right;">

\-3.553

</td>

</tr>

</tbody>

</table>

#### 2\. Excluding \(\alpha\) from the estimation

<table class=" lightable-paper lightable-hover" style='font-family: "Arial Narrow", arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>

<caption>

Non-linear Parameters

</caption>

<thead>

<tr>

<th style="text-align:left;">

Parameter

</th>

<th style="text-align:right;">

Est.

</th>

<th style="text-align:right;">

S.E.

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

lambda\_W

</td>

<td style="text-align:right;">

0.220

</td>

<td style="text-align:right;">

0.028

</td>

</tr>

<tr>

<td style="text-align:left;">

lambda\_F

</td>

<td style="text-align:right;">

0.067

</td>

<td style="text-align:right;">

0.008

</td>

</tr>

</tbody>

</table>

<table class=" lightable-paper lightable-hover" style='font-family: "Arial Narrow", arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>

<caption>

Linear Parameters

</caption>

<thead>

<tr>

<th style="text-align:left;">

Variables

</th>

<th style="text-align:right;">

Coef.

</th>

<th style="text-align:right;">

S.E.

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

daily

</td>

<td style="text-align:right;">

0.230

</td>

<td style="text-align:right;">

0.016

</td>

</tr>

<tr>

<td style="text-align:left;">

shokai

</td>

<td style="text-align:right;">

0.327

</td>

<td style="text-align:right;">

0.015

</td>

</tr>

<tr>

<td style="text-align:left;">

oversea

</td>

<td style="text-align:right;">

0.516

</td>

<td style="text-align:right;">

0.057

</td>

</tr>

<tr>

<td style="text-align:left;">

cocurrent

</td>

<td style="text-align:right;">

0.340

</td>

<td style="text-align:right;">

0.014

</td>

</tr>

</tbody>

</table>

### Decomposing (Semi) Elasticities

<table class=" lightable-paper lightable-hover" style='font-family: "Arial Narrow", arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>

<caption>

Application Semi-Elasticities

</caption>

<thead>

<tr>

<th style="text-align:left;">

Parameter

</th>

<th style="text-align:right;">

Estimates

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Direct (Wage)

</td>

<td style="text-align:right;">

4.550

</td>

</tr>

<tr>

<td style="text-align:left;">

Network (Wage)

</td>

<td style="text-align:right;">

\-2.171

</td>

</tr>

<tr>

<td style="text-align:left;">

Application (Fee)

</td>

<td style="text-align:right;">

14.851

</td>

</tr>

<tr>

<td style="text-align:left;">

Network (Fee)

</td>

<td style="text-align:right;">

\-4.230

</td>

</tr>

</tbody>

</table>

<table class=" lightable-paper lightable-hover" style='font-family: "Arial Narrow", arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>

<caption>

Match Semi-Elasticities

</caption>

<thead>

<tr>

<th style="text-align:left;">

Parameter

</th>

<th style="text-align:right;">

Estimates

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Direct (Wage)

</td>

<td style="text-align:right;">

2.794

</td>

</tr>

<tr>

<td style="text-align:left;">

Network (Wage)

</td>

<td style="text-align:right;">

1.764

</td>

</tr>

<tr>

<td style="text-align:left;">

Application (Fee)

</td>

<td style="text-align:right;">

2.792

</td>

</tr>

<tr>

<td style="text-align:left;">

Network (Fee)

</td>

<td style="text-align:right;">

1.757

</td>

</tr>

</tbody>

</table>

# Distribution of Platform Heterogeneity \(a_i\)

  - Unit: 10K Yen
      - Normalization: Direct employment \(a_0=0\)
      - This does not depend on the assumption on the matching
        parameters.

### Distribution of \(a_i\)

<img src="../figuretable/gmm_model_estimation/heterogeneity_hist.png" width="50%" />

### Distribution of Market FE

<img src="../figuretable/gmm_model_estimation/market_fe_hist.png" width="50%" />

### Distribution of Firm FE

<img src="../figuretable/gmm_model_estimation/firm_fe_hist.png" width="50%" />

### Market FE vs Market Size

<img src="../figuretable/gmm_model_estimation/market_fe_vs_market_size.png" width="50%" />

## Marginal Cost

<img src="../figuretable/gmm_model_estimation/mc_cdf.png" width="50%" /><img src="../figuretable/gmm_model_estimation/mc_hist.png" width="50%" />

# Model Fit

### Wage

  - \(R^2\) of regressing actual data on simulated data: 0.013

<img src="../figuretable/model_fit/wage.png" width="50%" />

<img src="../figuretable/model_fit/wage_hist.png" width="50%" />

### Fee

  - \(R^2\) of regressing actual data on simulated data: 0.326

<img src="../figuretable/model_fit/fee.png" width="50%" />

<img src="../figuretable/model_fit/fee_hist.png" width="50%" />

### Margin

  - margin = \(log(fee)-log(wage)\)
  - \(R^2\) of regressing actual data on simulated data: 0.228

<img src="../figuretable/model_fit/margin.png" width="50%" />

### Match

  - Log of the number of matches relative to outside option
    \(log{(q/q_0)}\)
  - \(R^2\) of regressing actual data on simulated data: 0.44

<img src="../figuretable/model_fit/match.png" width="50%" />

## Aggregate

<img src="../figuretable/model_fit/wage_aggregate.png" width="50%" />

<img src="../figuretable/model_fit/fee_aggregate.png" width="50%" />

<img src="../figuretable/model_fit/margin_aggregate.png" width="50%" />

<img src="../figuretable/model_fit/match_aggregate.png" width="50%" />

## Imposing Minimum Wage

![](model_estimation_without_share_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

### Distribution of Actual Wage - Minimum Wage

![](model_estimation_without_share_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

### Wage

  - \(R^2\) of regressing actual data on simulated data: 0.013
  - \(R^2\) of regressing actual data on simulated data with minimum
    wage constraint: 0.01

<img src="../figuretable/model_fit/wage_both.png" width="50%" />

### Fee

  - \(R^2\) of regressing actual data on simulated data: 0.326
  - \(R^2\) of regressing actual data on simulated data with minimum
    wage constraint: 0.396

<img src="../figuretable/model_fit/fee_both.png" width="50%" />

### Margin

  - margin = \(log(fee)-log(wage)\)
  - \(R^2\) of regressing actual data on simulated data: 0.228
  - \(R^2\) of regressing actual data on simulated data with minimum
    wage constraint: 0.4

<img src="../figuretable/model_fit/margin_both.png" width="50%" />

### Match

  - Log of the number of matches relative to outside option
    \(log{(q/q_0)}\)
  - \(R^2\) of regressing actual data on simulated data: 0.44
  - \(R^2\) of regressing actual data on simulated data with minimum
    wage constraint: 0.459

<img src="../figuretable/model_fit/match_both.png" width="50%" />

## Aggregate

<img src="../figuretable/model_fit/wage_aggregate_both.png" width="50%" />

<img src="../figuretable/model_fit/fee_aggregate_both.png" width="50%" />

<img src="../figuretable/model_fit/margin_aggregate_both.png" width="50%" />

<img src="../figuretable/model_fit/match_aggregate_both.png" width="50%" />

## Taking logs in indirect utilities

<img src="../figuretable/model_fit/wage_logutil.png" width="50%" />

<img src="../figuretable/model_fit/wage_hist_logutil.png" width="50%" />
<!-- # Subsampling --> <!-- ```{r} -->
<!-- load("../output/gmm_result_without_share_each_type.Rmd") -->

<!-- lambda_W <- 1:26 -->

<!-- lambda_F <- 1:26 -->

<!-- for (i in 1:26){ -->

<!--   result_i <- result_list[[i]] -->

<!--   if (!is.na(result_i)){ -->

<!--     lambda_W[i] <- min(result_i$params_nonlinear[2], 3) -->

<!--     lambda_F[i] <- min(result_i$params_nonlinear[3], 3) -->

<!--   } else{ -->

<!--     lambda_W[i] <- NA -->

<!--     lambda_F[i] <- NA -->

<!--   } -->

<!-- } -->

<!-- data_lambda <- data.frame(type = worktype_description$description_jp, -->

<!--                         lambda_W = lambda_W, -->

<!--                         lambda_F = lambda_F) -->

<!-- g1 <- data_lambda %>%  -->

<!--   ggplot(aes(x = type, y = lambda_W)) + geom_bar(stat = "identity") + coord_flip() + -->

<!--   ylab("lambda_W") + xlab("Work Type") -->

<!-- g2 <- data_lambda %>%  -->

<!--   ggplot(aes(x = type, y = lambda_F)) + geom_bar(stat = "identity") + coord_flip() + -->

<!--   ylab("lambda_F") + xlab("Work Type") -->

<!-- gridExtra::grid.arrange(g1, g2, ncol = 2) -->

<!-- ``` -->

<!-- ## Impact of $\alpha$ given $\beta = 0.5$ -->

<!-- ```{r} -->

<!-- mc_1_avg <- 1:9 -->

<!-- mc_2_avg <- 1:9 -->

<!-- s_w <- 1:9 -->

<!-- s_f <- 1:9 -->

<!-- semi_els_1 <- 1:9 -->

<!-- semi_els_2 <- 1:9 -->

<!-- ``` -->

<!-- ```{r} -->

<!-- for (i in 1:9){ -->

<!--   alpha <- i * 0.1 -->

<!--   beta <- 0.5 -->

<!--   mc <- mc_func(alpha, beta) -->

<!--   mc_1_avg[i] <- mean(mc$mc_1) -->

<!--   mc_2_avg[i] <- mean(mc$mc_2) -->

<!--   s_w[i] <- mean(mc$s_w) -->

<!--   s_f[i] <- mean(mc$s_f) -->

<!--   semi_els_1[i] <- mean(mc$semi_els_1) -->

<!--   semi_els_2[i] <- mean(mc$semi_els_2) -->

<!-- } -->

<!-- temp <- data.frame(alpha = 1:9 * 0.1, -->

<!--            mc_1_avg = mc_1_avg, -->

<!--            mc_2_avg = mc_2_avg, -->

<!--            s_w = s_w, -->

<!--            s_f = s_f, -->

<!--            semi_els_1 = semi_els_1, -->

<!--            semi_els_2 = semi_els_2) -->

<!-- g1 <- temp %>%  -->

<!--   ggplot(aes(x = alpha, y = s_w)) + geom_line() + xlab("alpha") + ylab("Share") + -->

<!--   ggtitle("Model Implied Worker Share") -->

<!-- g2 <- temp %>%  -->

<!--   ggplot(aes(x = alpha, y = s_f)) + geom_line() + xlab("alpha") + ylab("Share") + -->

<!--   ggtitle("Model Implied Firm Share") -->

<!-- g3 <- temp %>%  -->

<!--   ggplot(aes(x = alpha, y = mc_1_avg)) + geom_line() + xlab("alpha") + ylab("Marginal Cost (10K Yen)") + -->

<!--   ggtitle("Marginal Cost Implied by FOC for Wages") -->

<!-- g4 <- temp %>%  -->

<!--   ggplot(aes(x = alpha, y = mc_2_avg)) + geom_line() + xlab("alpha") + ylab("Marginal Cost (10K Yen)")+ -->

<!--   ggtitle("Marginal Cost Implied by FOC for Fees") -->

<!-- g5 <- temp %>%  -->

<!--   ggplot(aes(x = alpha, y = semi_els_1)) + geom_line() + xlab("alpha") + ylab("") + -->

<!--   ggtitle("Worker Side Semi-elasticity") -->

<!-- g6 <- temp %>%  -->

<!--   ggplot(aes(x = alpha, y = semi_els_2)) + geom_line() + xlab("alpha") + ylab("")+ -->

<!--   ggtitle("Firm Side Semi-elasticity") -->

<!-- gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, ncol = 2) -->

<!-- ``` -->

<!-- ## Impact of $\beta$ given $\alpha = 0.5$ -->

<!-- ```{r} -->

<!-- for (i in 1:9){ -->

<!--   alpha <- 0.5 -->

<!--   beta <- i * 0.1 -->

<!--   mc <- mc_func(alpha, beta) -->

<!--   mc_1_avg[i] <- mean(mc$mc_1) -->

<!--   mc_2_avg[i] <- mean(mc$mc_2) -->

<!--   s_w[i] <- mean(mc$s_w) -->

<!--   s_f[i] <- mean(mc$s_f) -->

<!--   semi_els_1[i] <- mean(mc$semi_els_1) -->

<!--   semi_els_2[i] <- mean(mc$semi_els_2) -->

<!-- } -->

<!-- temp <- data.frame(beta = 1:9 * 0.1, -->

<!--            mc_1_avg = mc_1_avg, -->

<!--            mc_2_avg = mc_2_avg, -->

<!--            s_w = s_w, -->

<!--            s_f = s_f, -->

<!--            semi_els_1 = semi_els_1, -->

<!--            semi_els_2 = semi_els_2) -->

<!-- g1 <- temp %>%  -->

<!--   ggplot(aes(x = beta, y = s_w)) + geom_line() + xlab("beta") + ylab("Share") + -->

<!--   ggtitle("Model Implied Worker Share") -->

<!-- g2 <- temp %>%  -->

<!--   ggplot(aes(x = beta, y = s_f)) + geom_line() + xlab("beta") + ylab("Share") + -->

<!--   ggtitle("Model Implied Firm Share") -->

<!-- g3 <- temp %>%  -->

<!--   ggplot(aes(x = beta, y = mc_1_avg)) + geom_line() + xlab("beta") + ylab("Marginal Cost (10K Yen)") + -->

<!--   ggtitle("Marginal Cost Implied by FOC for Wages") -->

<!-- g4 <- temp %>%  -->

<!--   ggplot(aes(x = beta, y = mc_2_avg)) + geom_line() + xlab("beta") + ylab("Marginal Cost (10K Yen)")+ -->

<!--   ggtitle("Marginal Cost Implied by FOC for Fees") -->

<!-- g5 <- temp %>%  -->

<!--   ggplot(aes(x = beta, y = semi_els_1)) + geom_line() + xlab("beta") + ylab("") + -->

<!--   ggtitle("Worker Side Semi-elasticity") -->

<!-- g6 <- temp %>%  -->

<!--   ggplot(aes(x = beta, y = semi_els_2)) + geom_line() + xlab("beta") + ylab("")+ -->

<!--   ggtitle("Firm Side Semi-elasticity") -->

<!-- gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, ncol = 2) -->

<!-- ``` -->

<!-- ## Impact of $\alpha$ given $\alpha + \beta = 1$ -->

<!-- ```{r} -->

<!-- for (i in 1:9){ -->

<!--   alpha <- i * 0.1 -->

<!--   beta <- 1 - alpha -->

<!--   mc <- mc_func(alpha, beta) -->

<!--   mc_1_avg[i] <- mean(mc$mc_1) -->

<!--   mc_2_avg[i] <- mean(mc$mc_2) -->

<!--   s_w[i] <- mean(mc$s_w) -->

<!--   s_f[i] <- mean(mc$s_f) -->

<!--   semi_els_1[i] <- mean(mc$semi_els_1) -->

<!--   semi_els_2[i] <- mean(mc$semi_els_2) -->

<!-- } -->

<!-- temp <- data.frame(alpha = 1:9 * 0.1, -->

<!--            mc_1_avg = mc_1_avg, -->

<!--            mc_2_avg = mc_2_avg, -->

<!--            s_w = s_w, -->

<!--            s_f = s_f, -->

<!--            semi_els_1 = semi_els_1, -->

<!--            semi_els_2 = semi_els_2) -->

<!-- g1 <- temp %>%  -->

<!--   ggplot(aes(x = alpha, y = s_w)) + geom_line() + xlab("alpha") + ylab("Share") + -->

<!--   ggtitle("Model Implied Worker Share") -->

<!-- g2 <- temp %>%  -->

<!--   ggplot(aes(x = alpha, y = s_f)) + geom_line() + xlab("alpha") + ylab("Share") + -->

<!--   ggtitle("Model Implied Firm Share") -->

<!-- g3 <- temp %>%  -->

<!--   ggplot(aes(x = alpha, y = mc_1_avg)) + geom_line() + xlab("alpha") + ylab("Marginal Cost (10K Yen)") + -->

<!--   ggtitle("Marginal Cost Implied by FOC for Wages") -->

<!-- g4 <- temp %>%  -->

<!--   ggplot(aes(x = alpha, y = mc_2_avg)) + geom_line() + xlab("alpha") + ylab("Marginal Cost (10K Yen)")+ -->

<!--   ggtitle("Marginal Cost Implied by FOC for Fees") -->

<!-- g5 <- temp %>%  -->

<!--   ggplot(aes(x = alpha, y = semi_els_1)) + geom_line() + xlab("alpha") + ylab("") + -->

<!--   ggtitle("Worker Side Semi-elasticity") -->

<!-- g6 <- temp %>%  -->

<!--   ggplot(aes(x = alpha, y = semi_els_2)) + geom_line() + xlab("alpha") + ylab("")+ -->

<!--   ggtitle("Firm Side Semi-elasticity") -->

<!-- gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, ncol = 2) -->

<!-- ``` -->
