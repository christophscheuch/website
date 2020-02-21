---
title: 'Tidy Asset Pricing - Part II: Beta and Stock Returns'
subtitle: 
summary: 'On the estimation on market betas and their relation to stock returns'
authors:
- admin
tags:
- Academic
categories:
- Asset Pricing
date: "2020-02-13T00:00:00Z"
lastmod: "2020-02-13T00:00:00Z"
featured: true
draft: false

# Featured image
# To use, add an image named `featured.jpg/png` to your page's folder.
# Placement options: 1 = Full column width, 2 = Out-set, 3 = Screen-width
# Focal point options: Smart, Center, TopLeft, Top, TopRight, Left, Right, BottomLeft, Bottom, BottomRight
image:
  placement: 2
  caption: ''
  focal_point: ""
  preview_only: false

# Projects (optional).
#   Associate this post with one or more of your projects.
#   Simply enter your project's folder or file name without extension.
#   E.g. `projects = ["internal-project"]` references `content/project/deep-learning/index.md`.
#   Otherwise, set `projects = []`.
projects: []
---

<dl>
<p>This note proposes an estimation and evaluation procedure for market betas following <a href="https://www.wiley.com/en-us/Empirical+Asset+Pricing%3A+The+Cross+Section+of+Stock+Returns-p-9781118095041">Bali, Engle and Murray</a>. I do not go into details about the foundations of market beta, but simply refer to any treatment of the Capital Asset Pricing Model (<a href="https://en.wikipedia.org/wiki/Capital_asset_pricing_model">CAPM</a>). This note has two objectives: the first one is to implement several approaches to estimate a stock’s beta in a tidy manner and to empirically examine these measures. The second objective is to analyze the cross-sectional relation between stock returns and market beta. The text below references an opinion and is for information purposes only. I do not intend to provide any investment advice.</p>
<p>I mainly use the following packages throughout this note:</p>
<pre class="r"><code>library(tidyverse)  
library(slider)     # for rolling window operations (https://github.com/DavisVaughan/slider)
library(kableExtra) # for nicer html tables</code></pre>
<p>First, I load the monthly return data that I prepared in an <a href="https://christophscheuch.github.io/post/asst-pricing/crsp-sample/">earlier post</a> and compute market excess returns required for the estimation procedure.</p>
<pre class="r"><code>crsp &lt;- read_rds(&quot;data/crsp.rds&quot;)

crsp &lt;- crsp %&gt;%
  mutate(ret_excess = ret_adj - rf_ff,
         mkt_ff_excess = mkt_ff - rf_ff) %&gt;%
  select(permno, date_start, ret_excess, mkt_ff_excess)</code></pre>
<div id="estimation" class="section level2">
<h2>Estimation</h2>
<p>According to the CAPM, cross-sectional variation in expected asset returns should be a function of the covariance between the return of the asset and the return on the market portfolio – the beta. To estimate stock-specific betas, I have to regress excess stock returns on excess returns of the market portfolio. Throughout the note, I use the market excess return and risk-free rate provided in <a href="https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html">Ken French’s data library</a>. The estimation procedure is based on a rolling window estimation where I may use either monthly or daily returns and different window lengths. I follow Bali et al. and examine nine different combinations of estimation periods and data frequencies. For monthly return data, I use excess return observations over the past 1, 2, 3 and 5 years, requiring at least 10, 20, 24 and 24 valid return observations, respectively. For daily data measures, I use period lengths of 1, 3, 6, 12 and 24 months and require 15, 50, 100, 200 and 450 days of valid return data. The function below implements these requirements for a given estimation window and data frequency and returns the estimated beta.</p>
<pre class="r"><code>capm_regression &lt;- function(x, window, freq) {
  # drop missing values
  x &lt;- na.omit(x)
  
  # determine minimum number of observations depending on window size and data frequency
  if (freq == &quot;monthly&quot;) {
    if (window == 12) {check &lt;- 10}
    if (window == 24) {check &lt;- 20}
    if (window == 36) {check &lt;- 24}
    if (window == 60) {check &lt;- 24}
  }
  if (freq == &quot;daily&quot;) {
    if (window == 1) {check &lt;- 15}
    if (window == 3) {check &lt;- 50}
    if (window == 6) {check &lt;- 100}
    if (window == 12) {check &lt;- 200}
    if (window == 24) {check &lt;- 450}
  }
  
  # check if minimum number of obervations is satisfied
  if (nrow(x) &lt; check) {
    return(tibble(beta = NA))
  } else {
    reg &lt;- lm(ret ~ mkt, data = x)
    return(tibble(beta = reg$coefficients[2]))
  }
}</code></pre>
<p>To perform the rolling estimation, I employ the new <code>slider</code> package of <a href="https://github.com/DavisVaughan/slider">Davis Vaughan</a> which provides a family of sliding window functions similar to <code>purrr::map()</code>. Most importantly, the <code>slide_index</code> function is able to handle months in its window input. I thus avoid using any time-series package (e.g., <code>zoo</code>) and converting the data to fit the package functions, but rather stay in the <code>tibble</code> world.</p>
<pre class="r"><code>rolling_capm_regression &lt;- function(data, window, freq = &quot;monthly&quot;, col_name, ...) {
  # prepare data
  x &lt;- data %&gt;%
    select(date = date_start, ret = ret_excess, mkt = mkt_ff_excess) %&gt;%
    arrange(date)

  # slide across dates
  betas &lt;- slide_index(x, x$date, ~capm_regression(.x, window, freq),
                       .before = months(window), .complete = FALSE) %&gt;%
    bind_rows()

  # collect output
  col_name &lt;- quo_name(col_name)
  mutate(data, !! col_name := betas$beta)
}</code></pre>
<p>I use the above function to compute several different variants of the beta estimator. However, the estimation takes up a considerable amount of time (around 5 hours). In principle, I could considerably speed up the estimation procedure by parallelizing it across stocks. However, for the sake of exposition, I just provide the code for the joint estimation below.</p>
<pre class="r"><code>betas_monthly &lt;- crsp %&gt;%
  group_by(permno) %&gt;%
  group_modify(~rolling_capm_regression(.x, window = 12, freq = &quot;monthly&quot;, col_name = &quot;beta_1y&quot;)) %&gt;%
  group_modify(~rolling_capm_regression(.x, window = 24, freq = &quot;monthly&quot;, col_name = &quot;beta_2y&quot;)) %&gt;%
  group_modify(~rolling_capm_regression(.x, window = 36, freq = &quot;monthly&quot;, col_name = &quot;beta_3y&quot;)) %&gt;%
  group_modify(~rolling_capm_regression(.x, window = 60, freq = &quot;monthly&quot;, col_name = &quot;beta_5y&quot;)) %&gt;%
  select(-c(ret_excess, mkt_ff_excess))</code></pre>
<p>Next, I use the same function to perform rolling window estimation on the daily return data. Note that this sample has about 3 GB and that the computation takes about 6 hours (but is again in principle parallelizable).</p>
<pre class="r"><code>crsp_daily &lt;- read_rds(&quot;data/crsp_daily.rds&quot;)

crsp_daily &lt;- crsp_daily %&gt;%
  mutate(ret_excess = ret - rf_ff,
         mkt_ff_excess = mkt_ff - rf_ff) %&gt;%
  select(permno, date_start, ret_excess, mkt_ff_excess)

betas_daily &lt;- crsp_daily %&gt;%
  group_by(permno) %&gt;%
  group_modify(~rolling_capm_regression(.x, window = 1, freq = &quot;daily&quot;, col_name = &quot;beta_1m&quot;)) %&gt;%
  group_modify(~rolling_capm_regression(.x, window = 3, freq = &quot;daily&quot;, col_name = &quot;beta_3m&quot;)) %&gt;%
  group_modify(~rolling_capm_regression(.x, window = 6, freq = &quot;daily&quot;, col_name = &quot;beta_6m&quot;)) %&gt;%
  group_modify(~rolling_capm_regression(.x, window = 12, freq = &quot;daily&quot;, col_name = &quot;beta_12m&quot;)) %&gt;%
  group_modify(~rolling_capm_regression(.x, window = 24, freq = &quot;daily&quot;, col_name = &quot;beta_24m&quot;)) %&gt;%
  distinct(permno, date_start, beta_1m, beta_3m, beta_6m, beta_12m, beta_24m)</code></pre>
</div>
<div id="summary-statistics" class="section level2">
<h2>Summary Statistics</h2>
<p>Let us add the beta estimates to the monthly crsp sample. As a first evaluation, I compute average summary statistics across all months.</p>
<pre class="r"><code>crsp &lt;- crsp %&gt;%
  left_join(betas_monthly, by = c(&quot;permno&quot;, &quot;date_start&quot;)) %&gt;%
  left_join(betas_daily, by = c(&quot;permno&quot;, &quot;date_start&quot;))

crsp_ts &lt;- crsp %&gt;%
  select(date, beta_1y:beta_24m) %&gt;%
  pivot_longer(cols = beta_1y:beta_24m, names_to = &quot;measure&quot;, values_drop_na = TRUE) %&gt;%
  group_by(measure, date) %&gt;%
  summarize(mean = mean(value),
            sd = sd(value),
            skew = moments::skewness(value),
            kurt = moments::kurtosis(value),
            min = min(value),
            q05 = quantile(value, 0.05),
            q25 = quantile(value, 0.25),
            q50 = quantile(value, 0.50),
            q75 = quantile(value, 0.75),
            q95 = quantile(value, 0.95),
            max = max(value),
            n = n()) %&gt;%
  ungroup()

crsp_ts %&gt;%
  select(-date) %&gt;%
  group_by(measure) %&gt;%
  summarize_all(list(mean)) %&gt;%
  mutate(measure = factor(measure, 
                          levels = c(&quot;beta_1y&quot;, &quot;beta_2y&quot;, &quot;beta_3y&quot;, &quot;beta_5y&quot;,
                                     &quot;beta_1m&quot;, &quot;beta_3m&quot;, &quot;beta_6m&quot;, &quot;beta_12m&quot;, &quot;beta_24m&quot;))) %&gt;%
  arrange(measure) %&gt;%
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;))</code></pre>
<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
measure
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:right;">
sd
</th>
<th style="text-align:right;">
skew
</th>
<th style="text-align:right;">
kurt
</th>
<th style="text-align:right;">
min
</th>
<th style="text-align:right;">
q05
</th>
<th style="text-align:right;">
q25
</th>
<th style="text-align:right;">
q50
</th>
<th style="text-align:right;">
q75
</th>
<th style="text-align:right;">
q95
</th>
<th style="text-align:right;">
max
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
beta_1y
</td>
<td style="text-align:right;">
1.16
</td>
<td style="text-align:right;">
1.12
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
17.72
</td>
<td style="text-align:right;">
-7.10
</td>
<td style="text-align:right;">
-0.35
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
1.06
</td>
<td style="text-align:right;">
1.72
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
11.55
</td>
<td style="text-align:right;">
2969.62
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_2y
</td>
<td style="text-align:right;">
1.16
</td>
<td style="text-align:right;">
0.82
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
9.14
</td>
<td style="text-align:right;">
-3.74
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
1.08
</td>
<td style="text-align:right;">
1.60
</td>
<td style="text-align:right;">
2.57
</td>
<td style="text-align:right;">
7.10
</td>
<td style="text-align:right;">
2776.02
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_3y
</td>
<td style="text-align:right;">
1.17
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.55
</td>
<td style="text-align:right;">
7.12
</td>
<td style="text-align:right;">
-2.71
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
1.09
</td>
<td style="text-align:right;">
1.56
</td>
<td style="text-align:right;">
2.41
</td>
<td style="text-align:right;">
5.83
</td>
<td style="text-align:right;">
2713.06
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_5y
</td>
<td style="text-align:right;">
1.17
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
0.43
</td>
<td style="text-align:right;">
8.56
</td>
<td style="text-align:right;">
-2.47
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.73
</td>
<td style="text-align:right;">
1.10
</td>
<td style="text-align:right;">
1.53
</td>
<td style="text-align:right;">
2.29
</td>
<td style="text-align:right;">
5.19
</td>
<td style="text-align:right;">
2733.32
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_1m
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
22.23
</td>
<td style="text-align:right;">
-7.03
</td>
<td style="text-align:right;">
-0.40
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
1.37
</td>
<td style="text-align:right;">
2.42
</td>
<td style="text-align:right;">
9.06
</td>
<td style="text-align:right;">
3158.70
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_3m
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
10.12
</td>
<td style="text-align:right;">
-4.03
</td>
<td style="text-align:right;">
-0.15
</td>
<td style="text-align:right;">
0.39
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
1.32
</td>
<td style="text-align:right;">
2.17
</td>
<td style="text-align:right;">
5.99
</td>
<td style="text-align:right;">
3120.56
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_6m
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.40
</td>
<td style="text-align:right;">
6.23
</td>
<td style="text-align:right;">
-2.52
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
0.43
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
1.29
</td>
<td style="text-align:right;">
2.05
</td>
<td style="text-align:right;">
4.58
</td>
<td style="text-align:right;">
3070.97
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_12m
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.59
</td>
<td style="text-align:right;">
0.44
</td>
<td style="text-align:right;">
4.10
</td>
<td style="text-align:right;">
-1.53
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.47
</td>
<td style="text-align:right;">
0.84
</td>
<td style="text-align:right;">
1.27
</td>
<td style="text-align:right;">
1.95
</td>
<td style="text-align:right;">
3.58
</td>
<td style="text-align:right;">
2973.85
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_24m
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
3.37
</td>
<td style="text-align:right;">
-0.88
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
1.25
</td>
<td style="text-align:right;">
1.87
</td>
<td style="text-align:right;">
3.01
</td>
<td style="text-align:right;">
2736.83
</td>
</tr>
</tbody>
</table>
<p>There appears to be more stability in the distribution of estimated betas as I extend the estimation period. This might indicate that longer estimation periods yield more precise beta estimates. However, the average number of observations mechanically decreases as the estimation windows increase. Moreover, estimates based on monthly observations are on average higher than estimates based on daily returns.</p>
</div>
<div id="correlations" class="section level2">
<h2>Correlations</h2>
<p>The next table presents the time-series averages of the monthly cross-sectional (Pearson product-moment) correlations between different measures of market beta.</p>
<pre class="r"><code>cor_matrix &lt;- crsp %&gt;%
  select(contains(&quot;beta&quot;)) %&gt;%
  cor(use = &quot;complete.obs&quot;, method = &quot;pearson&quot;)

cor_matrix[upper.tri(cor_matrix, diag = FALSE)] &lt;- NA

cor_matrix[, ] %&gt;%
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;))</code></pre>
<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
beta_1y
</th>
<th style="text-align:right;">
beta_2y
</th>
<th style="text-align:right;">
beta_3y
</th>
<th style="text-align:right;">
beta_5y
</th>
<th style="text-align:right;">
beta_1m
</th>
<th style="text-align:right;">
beta_3m
</th>
<th style="text-align:right;">
beta_6m
</th>
<th style="text-align:right;">
beta_12m
</th>
<th style="text-align:right;">
beta_24m
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
beta_1y
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_2y
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_3y
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_5y
</td>
<td style="text-align:right;">
0.53
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_1m
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_3m
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_6m
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.39
</td>
<td style="text-align:right;">
0.41
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_12m
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
0.47
</td>
<td style="text-align:right;">
0.48
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
1.0
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_24m
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.53
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
0.9
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>
<p>Correlations range from as low as 0.21 between the one-year measure based on monthly data and the one-month measure based on daily data to as high as 0.9 between the one-year and two-year measure based on daily data. Again, correlations seem to increase as the length of the measurement period increases.</p>
</div>
<div id="persistence" class="section level2">
<h2>Persistence</h2>
<p>If I want to use a measure of a stock’s beta in fruther analyses, I want the beta to be fairly stable over time. To examine whether this is the case, I calculate the persistence of each measure as the correlation between month <span class="math inline">\(t\)</span> and month <span class="math inline">\(t+\tau\)</span>. The table below presents the persistence measures for lags one, three, six, 12, 24, 36, 48, 60 and 120 months.</p>
<pre class="r"><code>compute_persistence &lt;- function(data, col, tau) {
  dates &lt;- data %&gt;%
    distinct(date) %&gt;%
    arrange(date) %&gt;%
    mutate(date_lag = lag(date, tau))

  col &lt;- enquo(col)
  correlation &lt;- data %&gt;%
    select(permno, date, !!col) %&gt;%
    left_join(dates, by = &quot;date&quot;) %&gt;%
    left_join(data %&gt;% select(permno, date, !! col),
              by = c(&quot;permno&quot;, &quot;date_lag&quot;=&quot;date&quot;)) %&gt;%
    select(-c(permno, date, date_lag)) %&gt;%
    cor(use = &quot;pairwise.complete.obs&quot;, method = &quot;pearson&quot;)

  return(correlation[1, 2])
}

tau &lt;- c(1, 3, 6, 12, 24, 36, 48, 60, 120)
beta_1y &lt;- map_dbl(tau, ~compute_persistence(crsp, beta_1y, .x))
beta_2y &lt;- map_dbl(tau, ~compute_persistence(crsp, beta_2y, .x))
beta_3y &lt;- map_dbl(tau, ~compute_persistence(crsp, beta_3y, .x))
beta_5y &lt;- map_dbl(tau, ~compute_persistence(crsp, beta_5y, .x))
beta_1m &lt;- map_dbl(tau, ~compute_persistence(crsp, beta_1m, .x))
beta_3m &lt;- map_dbl(tau, ~compute_persistence(crsp, beta_3m, .x))
beta_6m &lt;- map_dbl(tau, ~compute_persistence(crsp, beta_6m, .x))
beta_12m &lt;- map_dbl(tau, ~compute_persistence(crsp, beta_12m, .x))
beta_24m &lt;- map_dbl(tau, ~compute_persistence(crsp, beta_24m, .x))

tibble(tau = tau, 
       beta_1y = beta_1y, beta_2y = beta_2y, beta_3y = beta_3y, beta_5y = beta_5y,
       beta_1m = beta_1m, beta_3m = beta_3m, beta_6m = beta_6m, beta_12m = beta_12m, beta_24m = beta_24m) %&gt;%
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;))</code></pre>
<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
tau
</th>
<th style="text-align:right;">
beta_1y
</th>
<th style="text-align:right;">
beta_2y
</th>
<th style="text-align:right;">
beta_3y
</th>
<th style="text-align:right;">
beta_5y
</th>
<th style="text-align:right;">
beta_1m
</th>
<th style="text-align:right;">
beta_3m
</th>
<th style="text-align:right;">
beta_6m
</th>
<th style="text-align:right;">
beta_12m
</th>
<th style="text-align:right;">
beta_24m
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
0.99
</td>
</tr>
<tr>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.56
</td>
<td style="text-align:right;">
0.80
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.97
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.41
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.84
</td>
<td style="text-align:right;">
0.94
</td>
</tr>
<tr>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.86
</td>
</tr>
<tr>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.55
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.46
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
0.71
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.37
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
0.43
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0.65
</td>
</tr>
<tr>
<td style="text-align:right;">
48
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.41
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
0.61
</td>
</tr>
<tr>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.32
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.39
</td>
<td style="text-align:right;">
0.48
</td>
<td style="text-align:right;">
0.57
</td>
</tr>
<tr>
<td style="text-align:right;">
120
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.37
</td>
<td style="text-align:right;">
0.44
</td>
</tr>
</tbody>
</table>
<p>Of course, entries that correspond to lags for which the measurement periods overlap mechanically exhibit high persistence. The results indicate that the calculation of beta using short measurement periods is very noisy. Moreoever, as the length of the measurement window increases, so does the persistence of beta calculated from daily return data. The measures based on monthly return data exhibit similar persistence patterns. In summary, the results indicate that the use of longer measurement periods results in more accurate measures of beta and that beta seems to be more accurately measured using daily return data.</p>
</div>
<div id="portfolio-analysis" class="section level2">
<h2>Portfolio Analysis</h2>
<p>Next, I turn to the relation of beta and stock returns. The fundamental prediction of the CAPM is that there is a positive relation between market beta and expected stock returns where the slope of this relation is the market risk premium. We would expect to find a positive cross-sectional association between beta and future excess returns. However, as I show below, contrary to this prediction, analyses fail to detect any strong relation or even exhibit a negative relation. This result is typically viewed as one of the most persistent empirical anomalies in all of empirical asset pricing.</p>
<p>To perform the portfolio analysis, I sort stocks into 10 portfolios ranked by the value of the beta estimate. The breakpoints for each portfolio are based on deciles constructed using only NYSE stocks, while the portfolios are formed using stocks from all exchanges. The rationale behind this procedure is that firms with small market capitalization make up a large chunk of the investable universe by number, but only a small portion of the total market value. Since NYSE stocks exhibit on average larger market capitalization, sorting on all firms would basically separate NYSE stocks from all others. Moreover, the CRSP data covers NYSE stocks since 1926, while AMEX and NASDAQ stocks enter later. Another reason for using NYSE breakpoints is consistency over an extended period of time.</p>
<p>In addition to the 10 decile portfolios, the function below also adds the 10-1 portfolio.</p>
<pre class="r"><code>construct_portfolios &lt;- function(data, variable) {
  variable &lt;- enquo(variable)
  
  # determine breakpoints
  data_nyse &lt;- data %&gt;%
    filter(exchange == &quot;NYSE&quot;) %&gt;%
    select(date, !!variable)
  
  percentiles &lt;- seq(0.1, 0.9, 0.1)
  percentiles_names &lt;- map_chr(percentiles, ~paste0(&quot;q&quot;, .x*100))
  percentiles_funs &lt;- map(percentiles, ~partial(quantile, probs = .x, na.rm = TRUE)) %&gt;% 
    set_names(nm = percentiles_names)
  
  quantiles &lt;- data_nyse %&gt;%
    group_by(date) %&gt;%
    summarize_at(vars(!!variable), lst(!!!percentiles_funs)) 
  
  # sort all stocks into decile portfolios
  portfolios &lt;- data %&gt;%
    left_join(quantiles, by = &quot;date&quot;) %&gt;%
    mutate(portfolio = case_when(!!variable &gt; 0 &amp; !!variable &lt;= q10 ~ 1L,
                                 !!variable &gt; q10 &amp; !!variable &lt;= q20 ~ 2L,
                                 !!variable &gt; q20 &amp; !!variable &lt;= q30 ~ 3L,
                                 !!variable &gt; q30 &amp; !!variable &lt;= q40 ~ 4L,
                                 !!variable &gt; q40 &amp; !!variable &lt;= q50 ~ 5L,
                                 !!variable &gt; q50 &amp; !!variable &lt;= q60 ~ 6L,
                                 !!variable &gt; q60 &amp; !!variable &lt;= q70 ~ 7L,
                                 !!variable &gt; q70 &amp; !!variable &lt;= q80 ~ 8L,
                                 !!variable &gt; q80 &amp; !!variable &lt;= q90 ~ 9L,
                                 !!variable &gt; q90 ~ 10L))
  
  portfolios_ts &lt;- portfolios %&gt;%
    mutate(portfolio = as.character(portfolio)) %&gt;%
    group_by(portfolio, date) %&gt;%
    summarize(ret_ew = mean(ret_adj_excess_f1, na.rm = TRUE),
              ret_vw = weighted.mean(ret_adj_excess_f1, mktcap, na.rm = TRUE),
              ret_mkt = mean(mkt_ff_excess_f1, na.rm = TRUE)) %&gt;%
    na.omit() %&gt;%
    ungroup()
  
  # 10-1 portfolio
  portfolios_ts_101 &lt;- portfolios_ts %&gt;%
    filter(portfolio %in% c(&quot;1&quot;, &quot;10&quot;)) %&gt;%
    pivot_wider(names_from = portfolio, values_from = c(ret_ew, ret_vw)) %&gt;%
    mutate(ret_ew = ret_ew_10 - ret_ew_1,
           ret_vw = ret_vw_10 - ret_vw_1,
           portfolio = &quot;10-1&quot;) %&gt;%
    select(portfolio, date, ret_ew, ret_vw, ret_mkt)
  
  # combine everything
  out &lt;- bind_rows(portfolios_ts, portfolios_ts_101) %&gt;%
    mutate(portfolio = factor(portfolio, levels = c(as.character(seq(1, 10, 1)), &quot;10-1&quot;)))
  
  return(out)
}

portfolios_beta_1y &lt;- construct_portfolios(crsp, beta_1y)
portfolios_beta_2y &lt;- construct_portfolios(crsp, beta_2y)
portfolios_beta_3y &lt;- construct_portfolios(crsp, beta_3y)
portfolios_beta_5y &lt;- construct_portfolios(crsp, beta_5y)
portfolios_beta_1m &lt;- construct_portfolios(crsp, beta_1m)
portfolios_beta_3m &lt;- construct_portfolios(crsp, beta_3m)
portfolios_beta_6m &lt;- construct_portfolios(crsp, beta_6m)
portfolios_beta_12m &lt;- construct_portfolios(crsp, beta_12m)
portfolios_beta_24m &lt;- construct_portfolios(crsp, beta_24m)</code></pre>
<p>For illustrative purposes, I plot the cumulative value-weighted excess returns over time to get a first feeling for the data.</p>
<pre class="r"><code>portfolios_beta_12m %&gt;%
  group_by(portfolio) %&gt;%
  arrange(date) %&gt;%
  na.omit() %&gt;%
  mutate(cum_ret = cumsum(ret_vw)) %&gt;%
  ungroup() %&gt;%
  ggplot(aes(x = date, y = cum_ret, group = as.factor(portfolio))) +
  geom_line(aes(color = as.factor(portfolio), linetype = as.factor(portfolio))) +
  labs(x = &quot;&quot;, y = &quot;Cumulative Log Return (in %)&quot;, 
       color = &quot;Portfolio&quot;, linetype = &quot;Portfolio&quot;) + 
  scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks()) + 
  scale_x_date(expand = c(0, 0), date_breaks = &quot;10 years&quot;, date_labels = &quot;%Y&quot;) +
  theme_classic()</code></pre>
<p><img src="data:image/png;base64,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" width="672" style="display: block; margin: auto;" /></p>
<p>Given the portfolio returns, I want to evaluate whether the portfolio exhibit on average positive or negative excess returns. The following function estimates the mean excess return and CAPM alpha for each portfolio and computes corresponding <a href="https://www.jstor.org/stable/1913610?seq=1">Newey and West (1987)</a> <span class="math inline">\(t\)</span>-statistics (using six lags as common in the literature) testing the null hypothesis that the average portfolio excess return or CAPM alpha is zero.</p>
<pre class="r"><code>estimate_portfolio_returns &lt;- function(data, ret) {
  
  # estimate average returns per portfolio
  average_ret &lt;- data %&gt;%
    group_by(portfolio) %&gt;%
    arrange(date) %&gt;%
    do(model = lm(paste0(ret,&quot; ~ 1&quot;), data = .)) %&gt;% 
    mutate(nw_stderror = sqrt(diag(sandwich::NeweyWest(model, lag = 6)))) %&gt;%
    broom::tidy(model) %&gt;%
    ungroup() %&gt;%
    mutate(nw_tstat = estimate / nw_stderror) %&gt;%
    select(estimate, nw_tstat) %&gt;%
    t()
  
  # estimate capm alpha per portfolio
  average_capm_alpha &lt;- data %&gt;%
    group_by(portfolio) %&gt;%
    arrange(date) %&gt;%
    do(model = lm(paste0(ret,&quot; ~ 1 + ret_mkt&quot;), data = .)) %&gt;% 
    mutate(nw_stderror = sqrt(diag(sandwich::NeweyWest(model, lag = 6))[1])) %&gt;%
    broom::tidy(model)%&gt;%
    ungroup() %&gt;%
    filter(term == &quot;(Intercept)&quot;) %&gt;%
    mutate(nw_tstat = estimate / nw_stderror) %&gt;%
    select(estimate, nw_tstat) %&gt;%
    t()
  
  # construct output table
  out &lt;- rbind(average_ret, average_capm_alpha)
  colnames(out) &lt;-c(as.character(seq(1, 10, 1)), &quot;10-1&quot;)
  rownames(out) &lt;- c(&quot;Excess Return&quot;, &quot;t-Stat&quot; , &quot;CAPM Alpha&quot;, &quot;t-Stat&quot;)
  
  return(out)
}</code></pre>
<p>Let us first apply the function to equal-weighted porfolios.</p>
<pre class="r"><code>rbind(estimate_portfolio_returns(portfolios_beta_1y, &quot;ret_ew&quot;),
      estimate_portfolio_returns(portfolios_beta_2y, &quot;ret_ew&quot;),
      estimate_portfolio_returns(portfolios_beta_3y, &quot;ret_ew&quot;),
      estimate_portfolio_returns(portfolios_beta_5y, &quot;ret_ew&quot;),
      estimate_portfolio_returns(portfolios_beta_1m, &quot;ret_ew&quot;),
      estimate_portfolio_returns(portfolios_beta_3m, &quot;ret_ew&quot;),
      estimate_portfolio_returns(portfolios_beta_6m, &quot;ret_ew&quot;),
      estimate_portfolio_returns(portfolios_beta_12m, &quot;ret_ew&quot;),
      estimate_portfolio_returns(portfolios_beta_24m, &quot;ret_ew&quot;)) %&gt;%
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;)) %&gt;%
  pack_rows(&quot;Estimation Period: 1 Year (Monthly Data) - Equal-Weighted Portfolio Returns&quot;, 1, 4) %&gt;%
  pack_rows(&quot;Estimation Period: 2 Years (Monthly Data) - Equal-Weighted Portfolio Returns&quot;, 5, 8) %&gt;%
  pack_rows(&quot;Estimation Period: 3 Years (Monthly Data) - Equal-Weighted Portfolio Returns&quot;, 9, 12) %&gt;%
  pack_rows(&quot;Estimation Period: 5 Years (Monthly Data) - Equal-Weighted Portfolio Returns&quot;, 13, 16) %&gt;%
  pack_rows(&quot;Estimation Period: 1 Month (Daily Data) - Equal-Weighted Portfolio Returns&quot;, 17, 20) %&gt;%
  pack_rows(&quot;Estimation Period: 3 Months (Daily Data) - Equal-Weighted Portfolio Returns&quot;, 21, 24) %&gt;%
  pack_rows(&quot;Estimation Period: 6 Months (Daily Data) - Equal-Weighted Portfolio Returns&quot;, 25, 28) %&gt;%
  pack_rows(&quot;Estimation Period: 12 Months (Daily Data)- Equal-Weighted Portfolio Returns&quot;, 29, 32) %&gt;%
  pack_rows(&quot;Estimation Period: 24 Months (Daily Data) - Equal-Weighted Portfolio Returns&quot;, 33, 36)</code></pre>
<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
1
</th>
<th style="text-align:right;">
2
</th>
<th style="text-align:right;">
3
</th>
<th style="text-align:right;">
4
</th>
<th style="text-align:right;">
5
</th>
<th style="text-align:right;">
6
</th>
<th style="text-align:right;">
7
</th>
<th style="text-align:right;">
8
</th>
<th style="text-align:right;">
9
</th>
<th style="text-align:right;">
10
</th>
<th style="text-align:right;">
10-1
</th>
</tr>
</thead>
<tbody>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 1 Year (Monthly Data) - Equal-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.82
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
1.03
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
1.03
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.34
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.93
</td>
<td style="text-align:right;">
3.12
</td>
<td style="text-align:right;">
3.37
</td>
<td style="text-align:right;">
3.59
</td>
<td style="text-align:right;">
3.58
</td>
<td style="text-align:right;">
3.28
</td>
<td style="text-align:right;">
3.51
</td>
<td style="text-align:right;">
3.28
</td>
<td style="text-align:right;">
3.20
</td>
<td style="text-align:right;">
2.47
</td>
<td style="text-align:right;">
1.42
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
-0.36
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.92
</td>
<td style="text-align:right;">
1.06
</td>
<td style="text-align:right;">
0.82
</td>
<td style="text-align:right;">
1.41
</td>
<td style="text-align:right;">
1.19
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
-1.13
</td>
<td style="text-align:right;">
-1.84
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 2 Years (Monthly Data) - Equal-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
1.03
</td>
<td style="text-align:right;">
1.04
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.28
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.91
</td>
<td style="text-align:right;">
3.12
</td>
<td style="text-align:right;">
3.58
</td>
<td style="text-align:right;">
3.77
</td>
<td style="text-align:right;">
3.29
</td>
<td style="text-align:right;">
3.54
</td>
<td style="text-align:right;">
3.39
</td>
<td style="text-align:right;">
3.20
</td>
<td style="text-align:right;">
2.92
</td>
<td style="text-align:right;">
2.42
</td>
<td style="text-align:right;">
1.29
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
-0.45
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
1.15
</td>
<td style="text-align:right;">
1.47
</td>
<td style="text-align:right;">
1.30
</td>
<td style="text-align:right;">
1.63
</td>
<td style="text-align:right;">
1.07
</td>
<td style="text-align:right;">
1.29
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0.59
</td>
<td style="text-align:right;">
-0.21
</td>
<td style="text-align:right;">
-1.08
</td>
<td style="text-align:right;">
-2.52
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 3 Years (Monthly Data) - Equal-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
0.84
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.09
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.29
</td>
<td style="text-align:right;">
3.48
</td>
<td style="text-align:right;">
3.75
</td>
<td style="text-align:right;">
3.49
</td>
<td style="text-align:right;">
3.33
</td>
<td style="text-align:right;">
3.33
</td>
<td style="text-align:right;">
3.13
</td>
<td style="text-align:right;">
3.29
</td>
<td style="text-align:right;">
2.83
</td>
<td style="text-align:right;">
2.47
</td>
<td style="text-align:right;">
0.40
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
-0.01
</td>
<td style="text-align:right;">
-0.20
</td>
<td style="text-align:right;">
-0.56
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
1.37
</td>
<td style="text-align:right;">
1.92
</td>
<td style="text-align:right;">
1.92
</td>
<td style="text-align:right;">
1.46
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
1.29
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
-0.06
</td>
<td style="text-align:right;">
-0.93
</td>
<td style="text-align:right;">
-3.22
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 5 Years (Monthly Data) - Equal-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
1.06
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.60
</td>
<td style="text-align:right;">
3.61
</td>
<td style="text-align:right;">
3.56
</td>
<td style="text-align:right;">
3.57
</td>
<td style="text-align:right;">
3.61
</td>
<td style="text-align:right;">
3.34
</td>
<td style="text-align:right;">
3.18
</td>
<td style="text-align:right;">
3.27
</td>
<td style="text-align:right;">
2.74
</td>
<td style="text-align:right;">
2.26
</td>
<td style="text-align:right;">
0.16
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
-0.07
</td>
<td style="text-align:right;">
-0.21
</td>
<td style="text-align:right;">
-0.62
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
1.90
</td>
<td style="text-align:right;">
1.92
</td>
<td style="text-align:right;">
1.83
</td>
<td style="text-align:right;">
1.52
</td>
<td style="text-align:right;">
1.75
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
0.44
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
-0.38
</td>
<td style="text-align:right;">
-0.97
</td>
<td style="text-align:right;">
-3.51
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 1 Month (Daily Data) - Equal-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.82
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
-0.19
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.93
</td>
<td style="text-align:right;">
3.47
</td>
<td style="text-align:right;">
3.84
</td>
<td style="text-align:right;">
3.45
</td>
<td style="text-align:right;">
3.69
</td>
<td style="text-align:right;">
3.39
</td>
<td style="text-align:right;">
3.16
</td>
<td style="text-align:right;">
3.01
</td>
<td style="text-align:right;">
2.69
</td>
<td style="text-align:right;">
1.77
</td>
<td style="text-align:right;">
-0.87
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
-0.06
</td>
<td style="text-align:right;">
-0.18
</td>
<td style="text-align:right;">
-0.53
</td>
<td style="text-align:right;">
-0.91
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
1.10
</td>
<td style="text-align:right;">
1.64
</td>
<td style="text-align:right;">
1.84
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
1.26
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
-0.45
</td>
<td style="text-align:right;">
-1.13
</td>
<td style="text-align:right;">
-2.78
</td>
<td style="text-align:right;">
-5.56
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 3 Months (Daily Data) - Equal-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
-0.19
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.15
</td>
<td style="text-align:right;">
3.51
</td>
<td style="text-align:right;">
3.59
</td>
<td style="text-align:right;">
3.55
</td>
<td style="text-align:right;">
3.42
</td>
<td style="text-align:right;">
3.38
</td>
<td style="text-align:right;">
3.19
</td>
<td style="text-align:right;">
3.01
</td>
<td style="text-align:right;">
2.55
</td>
<td style="text-align:right;">
1.76
</td>
<td style="text-align:right;">
-0.94
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
-0.53
</td>
<td style="text-align:right;">
-0.86
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
1.72
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
1.56
</td>
<td style="text-align:right;">
1.19
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
-0.29
</td>
<td style="text-align:right;">
-1.44
</td>
<td style="text-align:right;">
-2.74
</td>
<td style="text-align:right;">
-5.71
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 6 Months (Daily Data) - Equal-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.82
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
-0.27
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.65
</td>
<td style="text-align:right;">
3.60
</td>
<td style="text-align:right;">
3.47
</td>
<td style="text-align:right;">
3.38
</td>
<td style="text-align:right;">
3.56
</td>
<td style="text-align:right;">
3.43
</td>
<td style="text-align:right;">
3.20
</td>
<td style="text-align:right;">
2.86
</td>
<td style="text-align:right;">
2.59
</td>
<td style="text-align:right;">
1.77
</td>
<td style="text-align:right;">
-1.37
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
-0.09
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
-0.56
</td>
<td style="text-align:right;">
-0.95
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.17
</td>
<td style="text-align:right;">
2.07
</td>
<td style="text-align:right;">
1.42
</td>
<td style="text-align:right;">
1.04
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
-0.57
</td>
<td style="text-align:right;">
-1.40
</td>
<td style="text-align:right;">
-2.95
</td>
<td style="text-align:right;">
-5.90
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 12 Months (Daily Data)- Equal-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
-0.29
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.38
</td>
<td style="text-align:right;">
3.60
</td>
<td style="text-align:right;">
3.62
</td>
<td style="text-align:right;">
3.50
</td>
<td style="text-align:right;">
3.34
</td>
<td style="text-align:right;">
3.33
</td>
<td style="text-align:right;">
3.23
</td>
<td style="text-align:right;">
2.88
</td>
<td style="text-align:right;">
2.60
</td>
<td style="text-align:right;">
1.73
</td>
<td style="text-align:right;">
-1.42
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
-0.08
</td>
<td style="text-align:right;">
-0.20
</td>
<td style="text-align:right;">
-0.57
</td>
<td style="text-align:right;">
-0.96
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.47
</td>
<td style="text-align:right;">
2.06
</td>
<td style="text-align:right;">
1.56
</td>
<td style="text-align:right;">
1.26
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
-0.49
</td>
<td style="text-align:right;">
-1.18
</td>
<td style="text-align:right;">
-3.05
</td>
<td style="text-align:right;">
-5.94
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 24 Months (Daily Data) - Equal-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
-0.32
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.54
</td>
<td style="text-align:right;">
3.59
</td>
<td style="text-align:right;">
3.84
</td>
<td style="text-align:right;">
3.47
</td>
<td style="text-align:right;">
3.37
</td>
<td style="text-align:right;">
3.29
</td>
<td style="text-align:right;">
3.32
</td>
<td style="text-align:right;">
2.90
</td>
<td style="text-align:right;">
2.74
</td>
<td style="text-align:right;">
1.72
</td>
<td style="text-align:right;">
-1.60
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
-0.06
</td>
<td style="text-align:right;">
-0.12
</td>
<td style="text-align:right;">
-0.55
</td>
<td style="text-align:right;">
-0.97
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.66
</td>
<td style="text-align:right;">
2.06
</td>
<td style="text-align:right;">
2.23
</td>
<td style="text-align:right;">
1.30
</td>
<td style="text-align:right;">
1.08
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.44
</td>
<td style="text-align:right;">
-0.35
</td>
<td style="text-align:right;">
-0.73
</td>
<td style="text-align:right;">
-2.95
</td>
<td style="text-align:right;">
-5.89
</td>
</tr>
</tbody>
</table>
<p>There is neither a clear pattern across the portfolios in average excess returns, nor does the 10-1 portfolio yield statistically significant excess returns. However, the CAPM alphas exhibit a decreasing pattern across portfolios and a statistically significant negative coefficient on the 10-1 portfolio for all beta estimates. This result is actually not surprising since the risk-adjustment using the CAPM model adjusts returns for the sensitivity to the market factor which is the same factor used to calculate beta. Stocks in the highest decile hence mechanically exhibit a higher sensitivity than stocks in the low decile. As a result, the 10-1 portfolio has a high sensitivity to the market portfolio. Since the market factor generates positive returns in the long run and the 10-1 portfolio has a positive sensitivity, the effect of the risk-adjustment is negative.</p>
<p>Next, let us repeat the analysis using value-weighted returns for each portfolio.</p>
<pre class="r"><code>rbind(estimate_portfolio_returns(portfolios_beta_1y, &quot;ret_vw&quot;),
      estimate_portfolio_returns(portfolios_beta_2y, &quot;ret_vw&quot;),
      estimate_portfolio_returns(portfolios_beta_3y, &quot;ret_vw&quot;),
      estimate_portfolio_returns(portfolios_beta_5y, &quot;ret_vw&quot;),
      estimate_portfolio_returns(portfolios_beta_1m, &quot;ret_vw&quot;),
      estimate_portfolio_returns(portfolios_beta_3m, &quot;ret_ew&quot;),
      estimate_portfolio_returns(portfolios_beta_6m, &quot;ret_vw&quot;),
      estimate_portfolio_returns(portfolios_beta_12m, &quot;ret_vw&quot;),
      estimate_portfolio_returns(portfolios_beta_24m, &quot;ret_vw&quot;)) %&gt;%
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;)) %&gt;%
  pack_rows(&quot;Estimation Period: 1 Year (Monthly Data) - Value-Weighted Portfolio Returns&quot;, 1, 4) %&gt;%
  pack_rows(&quot;Estimation Period: 2 Years (Monthly Data) - Value-Weighted Portfolio Returns&quot;, 5, 8) %&gt;%
  pack_rows(&quot;Estimation Period: 3 Years (Monthly Data) - Value-Weighted Portfolio Returns&quot;, 9, 12) %&gt;%
  pack_rows(&quot;Estimation Period: 5 Years (Monthly Data) - Value-Weighted Portfolio Returns&quot;, 13, 16) %&gt;%
  pack_rows(&quot;Estimation Period: 1 Month (Daily Data) - Value-Weighted Portfolio Returns&quot;, 17, 20) %&gt;%
  pack_rows(&quot;Estimation Period: 3 Months (Daily Data) - Value-Weighted Portfolio Returns&quot;, 21, 24) %&gt;%
  pack_rows(&quot;Estimation Period: 6 Months (Daily Data) - Value-Weighted Portfolio Returns&quot;, 25, 28) %&gt;%
  pack_rows(&quot;Estimation Period: 12 Months (Daily Data)- Value-Weighted Portfolio Returns&quot;, 29, 32) %&gt;%
  pack_rows(&quot;Estimation Period: 24 Months (Daily Data) - Value-Weighted Portfolio Returns&quot;, 33, 36)</code></pre>
<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
1
</th>
<th style="text-align:right;">
2
</th>
<th style="text-align:right;">
3
</th>
<th style="text-align:right;">
4
</th>
<th style="text-align:right;">
5
</th>
<th style="text-align:right;">
6
</th>
<th style="text-align:right;">
7
</th>
<th style="text-align:right;">
8
</th>
<th style="text-align:right;">
9
</th>
<th style="text-align:right;">
10
</th>
<th style="text-align:right;">
10-1
</th>
</tr>
</thead>
<tbody>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 1 Year (Monthly Data) - Value-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.53
</td>
<td style="text-align:right;">
0.46
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
0.50
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.90
</td>
<td style="text-align:right;">
2.54
</td>
<td style="text-align:right;">
3.57
</td>
<td style="text-align:right;">
3.65
</td>
<td style="text-align:right;">
3.20
</td>
<td style="text-align:right;">
2.85
</td>
<td style="text-align:right;">
3.34
</td>
<td style="text-align:right;">
3.13
</td>
<td style="text-align:right;">
2.83
</td>
<td style="text-align:right;">
2.01
</td>
<td style="text-align:right;">
1.88
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
-0.12
</td>
<td style="text-align:right;">
-0.06
</td>
<td style="text-align:right;">
-0.14
</td>
<td style="text-align:right;">
-0.16
</td>
<td style="text-align:right;">
-0.48
</td>
<td style="text-align:right;">
-0.31
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
-0.37
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
-0.88
</td>
<td style="text-align:right;">
-0.50
</td>
<td style="text-align:right;">
-1.14
</td>
<td style="text-align:right;">
-1.11
</td>
<td style="text-align:right;">
-3.04
</td>
<td style="text-align:right;">
-1.49
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 2 Years (Monthly Data) - Value-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.60
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.34
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.73
</td>
<td style="text-align:right;">
2.89
</td>
<td style="text-align:right;">
3.19
</td>
<td style="text-align:right;">
3.50
</td>
<td style="text-align:right;">
2.87
</td>
<td style="text-align:right;">
3.42
</td>
<td style="text-align:right;">
2.80
</td>
<td style="text-align:right;">
2.88
</td>
<td style="text-align:right;">
2.35
</td>
<td style="text-align:right;">
2.26
</td>
<td style="text-align:right;">
1.50
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
-0.16
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.30
</td>
<td style="text-align:right;">
-0.39
</td>
<td style="text-align:right;">
-0.46
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
-0.26
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
-1.17
</td>
<td style="text-align:right;">
-0.79
</td>
<td style="text-align:right;">
-2.26
</td>
<td style="text-align:right;">
-2.30
</td>
<td style="text-align:right;">
-2.57
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 3 Years (Monthly Data) - Value-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.48
</td>
<td style="text-align:right;">
0.55
</td>
<td style="text-align:right;">
0.57
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.16
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.63
</td>
<td style="text-align:right;">
3.04
</td>
<td style="text-align:right;">
3.19
</td>
<td style="text-align:right;">
3.34
</td>
<td style="text-align:right;">
2.80
</td>
<td style="text-align:right;">
2.97
</td>
<td style="text-align:right;">
2.62
</td>
<td style="text-align:right;">
2.66
</td>
<td style="text-align:right;">
2.27
</td>
<td style="text-align:right;">
2.09
</td>
<td style="text-align:right;">
0.73
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
-0.17
</td>
<td style="text-align:right;">
-0.16
</td>
<td style="text-align:right;">
-0.28
</td>
<td style="text-align:right;">
-0.40
</td>
<td style="text-align:right;">
-0.54
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
0.32
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
-0.37
</td>
<td style="text-align:right;">
-0.27
</td>
<td style="text-align:right;">
-1.29
</td>
<td style="text-align:right;">
-1.21
</td>
<td style="text-align:right;">
-1.95
</td>
<td style="text-align:right;">
-2.36
</td>
<td style="text-align:right;">
-3.05
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 5 Years (Monthly Data) - Value-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.57
</td>
<td style="text-align:right;">
0.13
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.90
</td>
<td style="text-align:right;">
3.02
</td>
<td style="text-align:right;">
3.16
</td>
<td style="text-align:right;">
3.25
</td>
<td style="text-align:right;">
3.04
</td>
<td style="text-align:right;">
2.91
</td>
<td style="text-align:right;">
2.73
</td>
<td style="text-align:right;">
2.65
</td>
<td style="text-align:right;">
2.23
</td>
<td style="text-align:right;">
1.82
</td>
<td style="text-align:right;">
0.58
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
-0.07
</td>
<td style="text-align:right;">
-0.14
</td>
<td style="text-align:right;">
-0.18
</td>
<td style="text-align:right;">
-0.32
</td>
<td style="text-align:right;">
-0.41
</td>
<td style="text-align:right;">
-0.57
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.44
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
-0.45
</td>
<td style="text-align:right;">
-1.09
</td>
<td style="text-align:right;">
-1.33
</td>
<td style="text-align:right;">
-2.08
</td>
<td style="text-align:right;">
-2.54
</td>
<td style="text-align:right;">
-3.22
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 1 Month (Daily Data) - Value-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.52
</td>
<td style="text-align:right;">
0.53
</td>
<td style="text-align:right;">
0.55
</td>
<td style="text-align:right;">
0.59
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
0.73
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.47
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.35
</td>
<td style="text-align:right;">
3.01
</td>
<td style="text-align:right;">
3.21
</td>
<td style="text-align:right;">
3.27
</td>
<td style="text-align:right;">
3.15
</td>
<td style="text-align:right;">
3.62
</td>
<td style="text-align:right;">
2.87
</td>
<td style="text-align:right;">
3.45
</td>
<td style="text-align:right;">
2.58
</td>
<td style="text-align:right;">
1.52
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
-0.11
</td>
<td style="text-align:right;">
-0.01
</td>
<td style="text-align:right;">
-0.21
</td>
<td style="text-align:right;">
-0.63
</td>
<td style="text-align:right;">
-0.73
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.73
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
-0.37
</td>
<td style="text-align:right;">
0.55
</td>
<td style="text-align:right;">
-0.82
</td>
<td style="text-align:right;">
-0.07
</td>
<td style="text-align:right;">
-1.56
</td>
<td style="text-align:right;">
-4.18
</td>
<td style="text-align:right;">
-3.91
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 3 Months (Daily Data) - Value-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
-0.19
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.15
</td>
<td style="text-align:right;">
3.51
</td>
<td style="text-align:right;">
3.59
</td>
<td style="text-align:right;">
3.55
</td>
<td style="text-align:right;">
3.42
</td>
<td style="text-align:right;">
3.38
</td>
<td style="text-align:right;">
3.19
</td>
<td style="text-align:right;">
3.01
</td>
<td style="text-align:right;">
2.55
</td>
<td style="text-align:right;">
1.76
</td>
<td style="text-align:right;">
-0.94
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
-0.53
</td>
<td style="text-align:right;">
-0.86
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
1.72
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
1.56
</td>
<td style="text-align:right;">
1.19
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
-0.29
</td>
<td style="text-align:right;">
-1.44
</td>
<td style="text-align:right;">
-2.74
</td>
<td style="text-align:right;">
-5.71
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 6 Months (Daily Data) - Value-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.55
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
0.60
</td>
<td style="text-align:right;">
0.16
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.95
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
2.93
</td>
<td style="text-align:right;">
3.43
</td>
<td style="text-align:right;">
2.96
</td>
<td style="text-align:right;">
3.46
</td>
<td style="text-align:right;">
3.24
</td>
<td style="text-align:right;">
2.94
</td>
<td style="text-align:right;">
2.47
</td>
<td style="text-align:right;">
1.91
</td>
<td style="text-align:right;">
0.68
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
-0.06
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
-0.09
</td>
<td style="text-align:right;">
-0.26
</td>
<td style="text-align:right;">
-0.50
</td>
<td style="text-align:right;">
-0.64
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
-0.50
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
-0.13
</td>
<td style="text-align:right;">
-0.78
</td>
<td style="text-align:right;">
-1.91
</td>
<td style="text-align:right;">
-3.37
</td>
<td style="text-align:right;">
-3.89
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 12 Months (Daily Data)- Value-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.53
</td>
<td style="text-align:right;">
0.59
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.56
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.56
</td>
<td style="text-align:right;">
0.09
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.79
</td>
<td style="text-align:right;">
3.04
</td>
<td style="text-align:right;">
3.40
</td>
<td style="text-align:right;">
3.29
</td>
<td style="text-align:right;">
3.04
</td>
<td style="text-align:right;">
3.46
</td>
<td style="text-align:right;">
3.22
</td>
<td style="text-align:right;">
2.97
</td>
<td style="text-align:right;">
2.33
</td>
<td style="text-align:right;">
1.79
</td>
<td style="text-align:right;">
0.42
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
-0.01
</td>
<td style="text-align:right;">
-0.08
</td>
<td style="text-align:right;">
-0.27
</td>
<td style="text-align:right;">
-0.53
</td>
<td style="text-align:right;">
-0.66
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.46
</td>
<td style="text-align:right;">
-0.19
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
-0.11
</td>
<td style="text-align:right;">
-0.61
</td>
<td style="text-align:right;">
-2.04
</td>
<td style="text-align:right;">
-3.59
</td>
<td style="text-align:right;">
-4.04
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 24 Months (Daily Data) - Value-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.55
</td>
<td style="text-align:right;">
0.59
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.59
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
0.52
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.80
</td>
<td style="text-align:right;">
3.11
</td>
<td style="text-align:right;">
3.34
</td>
<td style="text-align:right;">
3.26
</td>
<td style="text-align:right;">
3.03
</td>
<td style="text-align:right;">
3.12
</td>
<td style="text-align:right;">
3.30
</td>
<td style="text-align:right;">
2.70
</td>
<td style="text-align:right;">
2.19
</td>
<td style="text-align:right;">
1.64
</td>
<td style="text-align:right;">
0.14
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
-0.11
</td>
<td style="text-align:right;">
-0.29
</td>
<td style="text-align:right;">
-0.54
</td>
<td style="text-align:right;">
-0.69
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
1.15
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
-0.79
</td>
<td style="text-align:right;">
-2.13
</td>
<td style="text-align:right;">
-3.44
</td>
<td style="text-align:right;">
-4.16
</td>
</tr>
</tbody>
</table>
<p>The results show that the negative relation between beta and future stock returns detected using equal-weighted portfolios is weaker using value-weighted portfolios. Taken together, the portfolio analyses provide evidence that contradicts the predictions of the CAPM. According to the CAPM, returns should increase across the decile portfolios and risk-adjusted returns should be statistically indistinguishable from zero.</p>
</div>
<div id="regression-analysis" class="section level2">
<h2>Regression Analysis</h2>
<p>As a last step, I analyze the relation between beta and stock returns using <a href="https://www.jstor.org/stable/1831028?seq=1">Fama and MacBeth (1973)</a> regression analysis. Each month, I perform a cross-sectional regression of one-month-ahead excess stock returns on the given measure of beta. Time-series averages over these cross-sectional regressions then provide the desired results.</p>
<p>Following the literature, I winsorize each measure of beta on a monthly basis to mitigate the impact of outliers or estimation errors. I define the following function to winsorize any given variable.</p>
<pre class="r"><code>winsorize &lt;- function(x, cut = 0.005){
  cut_point_top &lt;- quantile(x, 1 - cut, na.rm = TRUE)
  cut_point_bottom &lt;- quantile(x, cut, na.rm = TRUE)
  i &lt;- which(x &gt;= cut_point_top) 
  x[i] &lt;- cut_point_top
  j &lt;- which(x &lt;= cut_point_bottom) 
  x[j] &lt;- cut_point_bottom
  return(x)
}</code></pre>
<p>The next function performs Fama-MacBeth regressions for a given variable and returns the time-series average of monthly regression coefficients and corresponding <span class="math inline">\(t\)</span>-statistics.</p>
<pre class="r"><code>fama_macbeth_regression &lt;- function(data, variable, cut = 0.005) {
  variable &lt;- enquo(variable)
  
  # prepare and winsorize data
  data_nested &lt;- data %&gt;%
    filter(!is.na(ret_adj_f1) &amp; !is.na(!!variable)) %&gt;%
    group_by(date) %&gt;%
    mutate(beta = winsorize(!!variable, cut = cut)) %&gt;%
    nest() 
  
  # perform cross-sectional regressions for each month
  cross_sectional_regs &lt;- data_nested %&gt;%
    mutate(model = map(data, ~lm(ret_adj_f1 ~ beta, data = .x))) %&gt;%
    mutate(tidy = map(model, broom::tidy),
           glance = map(model, broom::glance),
           n = map(model, stats::nobs))
  
  # extract average coefficient estimates
  fama_macbeth_coefs &lt;- cross_sectional_regs %&gt;%
    unnest(tidy) %&gt;%
    group_by(term) %&gt;%
    summarize(coefficient = mean(estimate))
  
  # compute newey-west standard errors of average coefficient estimates
  newey_west_std_errors &lt;- cross_sectional_regs %&gt;%
    unnest(tidy) %&gt;%
    group_by(term) %&gt;%
    arrange(date) %&gt;%
    group_modify(~enframe(sqrt(diag(sandwich::NeweyWest(lm(estimate ~ 1, data = .x), lag = 6))))) %&gt;%
    select(term, nw_std_error = value)
  
  # put coefficient estimates and standard errors together and compute t-statistics
  fama_macbeth_coefs &lt;- fama_macbeth_coefs %&gt;%
    left_join(newey_west_std_errors, by = &quot;term&quot;) %&gt;%
    mutate(nw_t_stat = coefficient / nw_std_error) %&gt;%
    select(term, coefficient, nw_t_stat) %&gt;%
    pivot_longer(cols = c(coefficient, nw_t_stat), names_to = &quot;statistic&quot;) %&gt;%
    mutate(statistic = paste(term, statistic, sep = &quot; &quot;)) %&gt;%
    select(-term)
  
  # extract average r-squared and average number of observations
  fama_macbeth_stats &lt;- cross_sectional_regs %&gt;% 
    unnest(c(glance, n)) %&gt;%
    ungroup() %&gt;%
    summarize(adj_r_squared = mean(adj.r.squared),
              n = mean(n)) %&gt;%
    pivot_longer(c(adj_r_squared, n), names_to = &quot;statistic&quot;)
  
  # combine desired output and return results
  out &lt;- rbind(fama_macbeth_coefs, fama_macbeth_stats)
  return(out)
}</code></pre>
<p>The following table provides the regression results for each beta measure.</p>
<pre class="r"><code>beta_1y_reg &lt;- fama_macbeth_regression(crsp, beta_1y) %&gt;% rename(beta_1y = value)
beta_2y_reg &lt;- fama_macbeth_regression(crsp, beta_2y) %&gt;% rename(beta_2y = value)
beta_3y_reg &lt;- fama_macbeth_regression(crsp, beta_3y) %&gt;% rename(beta_3y = value)
beta_5y_reg &lt;- fama_macbeth_regression(crsp, beta_5y) %&gt;% rename(beta_5y = value)
beta_1m_reg &lt;- fama_macbeth_regression(crsp, beta_1m) %&gt;% rename(beta_1m = value)
beta_3m_reg &lt;- fama_macbeth_regression(crsp, beta_3m) %&gt;% rename(beta_3m = value)
beta_6m_reg &lt;- fama_macbeth_regression(crsp, beta_6m) %&gt;% rename(beta_6m = value)
beta_12m_reg &lt;- fama_macbeth_regression(crsp, beta_12m) %&gt;% rename(beta_12m = value)
beta_24m_reg &lt;- fama_macbeth_regression(crsp, beta_24m) %&gt;% rename(beta_24m = value)

regression_table &lt;- beta_1y_reg %&gt;%
  left_join(beta_2y_reg, by = &quot;statistic&quot;) %&gt;%
  left_join(beta_3y_reg, by = &quot;statistic&quot;) %&gt;%
  left_join(beta_5y_reg, by = &quot;statistic&quot;) %&gt;%
  left_join(beta_1m_reg, by = &quot;statistic&quot;) %&gt;%
  left_join(beta_3m_reg, by = &quot;statistic&quot;) %&gt;%
  left_join(beta_6m_reg, by = &quot;statistic&quot;) %&gt;%
  left_join(beta_12m_reg, by = &quot;statistic&quot;) %&gt;%
  left_join(beta_24m_reg, by = &quot;statistic&quot;)

regression_table$statistic &lt;- c(&quot;intercept&quot;, &quot;t-stat&quot;, &quot;beta&quot;, &quot;t-stat&quot;, &quot;adj. R-squared&quot;, &quot;n&quot;)
regression_table %&gt;%
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;))</code></pre>
<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
statistic
</th>
<th style="text-align:right;">
beta_1y
</th>
<th style="text-align:right;">
beta_2y
</th>
<th style="text-align:right;">
beta_3y
</th>
<th style="text-align:right;">
beta_5y
</th>
<th style="text-align:right;">
beta_1m
</th>
<th style="text-align:right;">
beta_3m
</th>
<th style="text-align:right;">
beta_6m
</th>
<th style="text-align:right;">
beta_12m
</th>
<th style="text-align:right;">
beta_24m
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
intercept
</td>
<td style="text-align:right;">
1.09
</td>
<td style="text-align:right;">
1.07
</td>
<td style="text-align:right;">
1.08
</td>
<td style="text-align:right;">
1.06
</td>
<td style="text-align:right;">
1.20
</td>
<td style="text-align:right;">
1.22
</td>
<td style="text-align:right;">
1.28
</td>
<td style="text-align:right;">
1.32
</td>
<td style="text-align:right;">
1.36
</td>
</tr>
<tr>
<td style="text-align:left;">
t-stat
</td>
<td style="text-align:right;">
4.43
</td>
<td style="text-align:right;">
4.67
</td>
<td style="text-align:right;">
4.90
</td>
<td style="text-align:right;">
5.01
</td>
<td style="text-align:right;">
4.61
</td>
<td style="text-align:right;">
4.73
</td>
<td style="text-align:right;">
4.95
</td>
<td style="text-align:right;">
5.13
</td>
<td style="text-align:right;">
5.30
</td>
</tr>
<tr>
<td style="text-align:left;">
beta
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.12
</td>
<td style="text-align:right;">
-0.17
</td>
<td style="text-align:right;">
-0.21
</td>
<td style="text-align:right;">
-0.23
</td>
</tr>
<tr>
<td style="text-align:left;">
t-stat
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
-1.57
</td>
<td style="text-align:right;">
-1.57
</td>
<td style="text-align:right;">
-1.82
</td>
<td style="text-align:right;">
-1.99
</td>
<td style="text-align:right;">
-2.00
</td>
</tr>
<tr>
<td style="text-align:left;">
adj. R-squared
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left;">
n
</td>
<td style="text-align:right;">
2940.43
</td>
<td style="text-align:right;">
2747.58
</td>
<td style="text-align:right;">
2679.78
</td>
<td style="text-align:right;">
2687.25
</td>
<td style="text-align:right;">
3132.94
</td>
<td style="text-align:right;">
3095.20
</td>
<td style="text-align:right;">
3045.97
</td>
<td style="text-align:right;">
2949.61
</td>
<td style="text-align:right;">
2715.26
</td>
</tr>
</tbody>
</table>
<p>For all measures based on monthly data, I do not find any statistically significant relation between beta and stock returns, while the measures based on daily returns do exhibit a statistically significant relation for longer estimation periods. Moreover, the intercepts are positive and statistically significant across all beta measures, although, controlling for the effect of beta, the average excess stock returns should be zero according to the CAPM. The results thus provide no evidence in support of the CAPM.</p>
<p>Finally, it is worth noting that the average adjusted R-squared ranges only from 2 to 3 percent. Low values of R-squared are common in empirical asset pricing studies. The main reason put forward in the literature is that predicting stock returns is that realized stock returns are just a very noisy measure for expected stock returns (e.g., <a href="https://onlinelibrary.wiley.com/doi/abs/10.1111/0022-1082.00144">Elton, 1999</a>).</p>

</dl>
