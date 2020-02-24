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

cor_matrix %&gt;%
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
<p>If I want to use a measure of a stock’s beta in further analyses, I want the beta to be fairly stable over time. To examine whether this is the case, I calculate the persistence of each measure as the correlation between month <span class="math inline">\(t\)</span> and month <span class="math inline">\(t+\tau\)</span>. The table below presents the persistence measures for lags one, three, six, 12, 24, 36, 48, 60 and 120 months.</p>
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
<p>To perform the portfolio analysis, I sort stocks into 10 portfolios ranked by the value of the beta estimate. The breakpoints for each portfolio are based on deciles constructed using all stocks. In addition to the 10 decile portfolios, the function below also adds the 10-1 portfolio.</p>
<pre class="r"><code>construct_portfolios &lt;- function(data, var) {
  var &lt;- enquo(var)
  
  # keep only observations where the sorting variable is defined
  data &lt;- data %&gt;%
    filter(!is.na(!!var))
  
  # calculate quantiles
  percentiles &lt;- seq(0.1, 0.9, 0.1)
  percentiles_names &lt;- map_chr(percentiles, ~paste0(&quot;q&quot;, .x*100))
  percentiles_funs &lt;- map(percentiles, ~partial(quantile, probs = .x, na.rm = TRUE)) %&gt;% 
    set_names(nm = percentiles_names)
  
  quantiles &lt;- data %&gt;%
    group_by(date) %&gt;%
    summarize_at(vars(!!var), lst(!!!percentiles_funs)) 
  
  # sort all stocks into decile portfolios
  portfolios &lt;- data %&gt;%
    left_join(quantiles, by = &quot;date&quot;) %&gt;%
    mutate(portfolio = case_when(!!var &lt;= q10 ~ 1L,
                                 !!var &gt; q10 &amp; !!var &lt;= q20 ~ 2L,
                                 !!var &gt; q20 &amp; !!var &lt;= q30 ~ 3L,
                                 !!var &gt; q30 &amp; !!var &lt;= q40 ~ 4L,
                                 !!var &gt; q40 &amp; !!var &lt;= q50 ~ 5L,
                                 !!var &gt; q50 &amp; !!var &lt;= q60 ~ 6L,
                                 !!var &gt; q60 &amp; !!var &lt;= q70 ~ 7L,
                                 !!var &gt; q70 &amp; !!var &lt;= q80 ~ 8L,
                                 !!var &gt; q80 &amp; !!var &lt;= q90 ~ 9L,
                                 !!var &gt; q90 ~ 10L))
  
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
<pre class="r"><code>## plot cumulative value-weighted excess returns over time
portfolios_beta_12m %&gt;%
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
0.84
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
1.06
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.84
</td>
<td style="text-align:right;">
0.07
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.88
</td>
<td style="text-align:right;">
2.99
</td>
<td style="text-align:right;">
3.34
</td>
<td style="text-align:right;">
3.44
</td>
<td style="text-align:right;">
3.45
</td>
<td style="text-align:right;">
3.40
</td>
<td style="text-align:right;">
3.31
</td>
<td style="text-align:right;">
3.37
</td>
<td style="text-align:right;">
2.83
</td>
<td style="text-align:right;">
2.24
</td>
<td style="text-align:right;">
0.38
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
0.14
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.32
</td>
<td style="text-align:right;">
-0.55
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
1.08
</td>
<td style="text-align:right;">
1.17
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.56
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
-0.57
</td>
<td style="text-align:right;">
-1.48
</td>
<td style="text-align:right;">
-3.09
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
0.81
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
0.11
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.16
</td>
<td style="text-align:right;">
2.93
</td>
<td style="text-align:right;">
3.21
</td>
<td style="text-align:right;">
3.73
</td>
<td style="text-align:right;">
3.31
</td>
<td style="text-align:right;">
3.52
</td>
<td style="text-align:right;">
3.25
</td>
<td style="text-align:right;">
3.07
</td>
<td style="text-align:right;">
2.86
</td>
<td style="text-align:right;">
2.17
</td>
<td style="text-align:right;">
0.51
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
0.18
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
-0.09
</td>
<td style="text-align:right;">
-0.31
</td>
<td style="text-align:right;">
-0.60
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
1.60
</td>
<td style="text-align:right;">
1.25
</td>
<td style="text-align:right;">
1.20
</td>
<td style="text-align:right;">
1.53
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
1.17
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.32
</td>
<td style="text-align:right;">
-0.49
</td>
<td style="text-align:right;">
-1.41
</td>
<td style="text-align:right;">
-3.49
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
0.75
</td>
<td style="text-align:right;">
0.80
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
-0.06
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.14
</td>
<td style="text-align:right;">
3.33
</td>
<td style="text-align:right;">
3.73
</td>
<td style="text-align:right;">
3.53
</td>
<td style="text-align:right;">
3.19
</td>
<td style="text-align:right;">
3.30
</td>
<td style="text-align:right;">
3.13
</td>
<td style="text-align:right;">
3.07
</td>
<td style="text-align:right;">
2.84
</td>
<td style="text-align:right;">
2.22
</td>
<td style="text-align:right;">
-0.25
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
0.24
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
-0.28
</td>
<td style="text-align:right;">
-0.70
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
1.44
</td>
<td style="text-align:right;">
1.71
</td>
<td style="text-align:right;">
1.85
</td>
<td style="text-align:right;">
1.64
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.53
</td>
<td style="text-align:right;">
-0.26
</td>
<td style="text-align:right;">
-1.28
</td>
<td style="text-align:right;">
-3.70
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
0.76
</td>
<td style="text-align:right;">
0.82
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
-0.01
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
3.60
</td>
<td style="text-align:right;">
3.56
</td>
<td style="text-align:right;">
3.55
</td>
<td style="text-align:right;">
3.36
</td>
<td style="text-align:right;">
3.41
</td>
<td style="text-align:right;">
3.15
</td>
<td style="text-align:right;">
2.95
</td>
<td style="text-align:right;">
2.76
</td>
<td style="text-align:right;">
2.12
</td>
<td style="text-align:right;">
-0.06
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
0.27
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
-0.11
</td>
<td style="text-align:right;">
-0.26
</td>
<td style="text-align:right;">
-0.68
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
1.55
</td>
<td style="text-align:right;">
1.93
</td>
<td style="text-align:right;">
1.72
</td>
<td style="text-align:right;">
1.60
</td>
<td style="text-align:right;">
1.34
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
0.37
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
-0.61
</td>
<td style="text-align:right;">
-1.17
</td>
<td style="text-align:right;">
-3.58
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
1.01
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
0.90
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
0.62
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
3.20
</td>
<td style="text-align:right;">
3.51
</td>
<td style="text-align:right;">
3.61
</td>
<td style="text-align:right;">
3.58
</td>
<td style="text-align:right;">
3.65
</td>
<td style="text-align:right;">
3.28
</td>
<td style="text-align:right;">
3.29
</td>
<td style="text-align:right;">
2.98
</td>
<td style="text-align:right;">
2.57
</td>
<td style="text-align:right;">
1.67
</td>
<td style="text-align:right;">
-2.25
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
0.27
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
-0.07
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
-0.56
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
1.41
</td>
<td style="text-align:right;">
1.74
</td>
<td style="text-align:right;">
1.82
</td>
<td style="text-align:right;">
1.37
</td>
<td style="text-align:right;">
1.34
</td>
<td style="text-align:right;">
0.55
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
-0.53
</td>
<td style="text-align:right;">
-1.43
</td>
<td style="text-align:right;">
-2.86
</td>
<td style="text-align:right;">
-6.33
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
1.01
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
-0.34
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.33
</td>
<td style="text-align:right;">
3.41
</td>
<td style="text-align:right;">
3.58
</td>
<td style="text-align:right;">
3.68
</td>
<td style="text-align:right;">
3.53
</td>
<td style="text-align:right;">
3.30
</td>
<td style="text-align:right;">
3.18
</td>
<td style="text-align:right;">
2.91
</td>
<td style="text-align:right;">
2.42
</td>
<td style="text-align:right;">
1.64
</td>
<td style="text-align:right;">
-1.92
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.14
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
-0.27
</td>
<td style="text-align:right;">
-0.58
</td>
<td style="text-align:right;">
-0.94
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
1.71
</td>
<td style="text-align:right;">
2.11
</td>
<td style="text-align:right;">
1.71
</td>
<td style="text-align:right;">
1.64
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
-0.52
</td>
<td style="text-align:right;">
-1.72
</td>
<td style="text-align:right;">
-2.91
</td>
<td style="text-align:right;">
-6.08
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
1.05
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
-0.42
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.56
</td>
<td style="text-align:right;">
3.46
</td>
<td style="text-align:right;">
3.59
</td>
<td style="text-align:right;">
3.46
</td>
<td style="text-align:right;">
3.46
</td>
<td style="text-align:right;">
3.46
</td>
<td style="text-align:right;">
3.18
</td>
<td style="text-align:right;">
2.83
</td>
<td style="text-align:right;">
2.38
</td>
<td style="text-align:right;">
1.71
</td>
<td style="text-align:right;">
-2.24
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.44
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.30
</td>
<td style="text-align:right;">
-0.58
</td>
<td style="text-align:right;">
-1.05
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.42
</td>
<td style="text-align:right;">
2.03
</td>
<td style="text-align:right;">
1.77
</td>
<td style="text-align:right;">
1.42
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
-0.67
</td>
<td style="text-align:right;">
-1.84
</td>
<td style="text-align:right;">
-2.98
</td>
<td style="text-align:right;">
-6.53
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
1.07
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
0.82
</td>
<td style="text-align:right;">
0.61
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
3.84
</td>
<td style="text-align:right;">
3.68
</td>
<td style="text-align:right;">
3.49
</td>
<td style="text-align:right;">
3.54
</td>
<td style="text-align:right;">
3.52
</td>
<td style="text-align:right;">
3.47
</td>
<td style="text-align:right;">
3.06
</td>
<td style="text-align:right;">
2.80
</td>
<td style="text-align:right;">
2.49
</td>
<td style="text-align:right;">
1.66
</td>
<td style="text-align:right;">
-2.25
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.24
</td>
<td style="text-align:right;">
-0.60
</td>
<td style="text-align:right;">
-1.13
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.98
</td>
<td style="text-align:right;">
2.45
</td>
<td style="text-align:right;">
1.67
</td>
<td style="text-align:right;">
1.58
</td>
<td style="text-align:right;">
1.18
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
-0.70
</td>
<td style="text-align:right;">
-1.46
</td>
<td style="text-align:right;">
-3.15
</td>
<td style="text-align:right;">
-6.89
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
1.04
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
-0.43
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.82
</td>
<td style="text-align:right;">
3.57
</td>
<td style="text-align:right;">
3.74
</td>
<td style="text-align:right;">
3.65
</td>
<td style="text-align:right;">
3.44
</td>
<td style="text-align:right;">
3.32
</td>
<td style="text-align:right;">
3.28
</td>
<td style="text-align:right;">
2.92
</td>
<td style="text-align:right;">
2.57
</td>
<td style="text-align:right;">
1.68
</td>
<td style="text-align:right;">
-2.08
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.53
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
-0.18
</td>
<td style="text-align:right;">
-0.57
</td>
<td style="text-align:right;">
-1.10
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.27
</td>
<td style="text-align:right;">
2.35
</td>
<td style="text-align:right;">
2.28
</td>
<td style="text-align:right;">
1.85
</td>
<td style="text-align:right;">
1.36
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.47
</td>
<td style="text-align:right;">
-0.27
</td>
<td style="text-align:right;">
-1.12
</td>
<td style="text-align:right;">
-3.02
</td>
<td style="text-align:right;">
-6.48
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
0.39
</td>
<td style="text-align:right;">
0.48
</td>
<td style="text-align:right;">
0.59
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
0.26
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
1.95
</td>
<td style="text-align:right;">
2.60
</td>
<td style="text-align:right;">
3.22
</td>
<td style="text-align:right;">
3.55
</td>
<td style="text-align:right;">
3.01
</td>
<td style="text-align:right;">
3.13
</td>
<td style="text-align:right;">
3.10
</td>
<td style="text-align:right;">
2.81
</td>
<td style="text-align:right;">
2.64
</td>
<td style="text-align:right;">
1.72
</td>
<td style="text-align:right;">
1.06
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
-0.11
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
-0.57
</td>
<td style="text-align:right;">
-0.50
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
-0.71
</td>
<td style="text-align:right;">
-0.29
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
-0.18
</td>
<td style="text-align:right;">
-0.80
</td>
<td style="text-align:right;">
-0.79
</td>
<td style="text-align:right;">
-1.87
</td>
<td style="text-align:right;">
-1.59
</td>
<td style="text-align:right;">
-3.09
</td>
<td style="text-align:right;">
-2.33
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
0.40
</td>
<td style="text-align:right;">
0.43
</td>
<td style="text-align:right;">
0.56
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
0.29
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
2.38
</td>
<td style="text-align:right;">
2.99
</td>
<td style="text-align:right;">
3.73
</td>
<td style="text-align:right;">
2.89
</td>
<td style="text-align:right;">
3.14
</td>
<td style="text-align:right;">
2.75
</td>
<td style="text-align:right;">
2.49
</td>
<td style="text-align:right;">
2.61
</td>
<td style="text-align:right;">
1.89
</td>
<td style="text-align:right;">
1.14
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
-0.07
</td>
<td style="text-align:right;">
-0.15
</td>
<td style="text-align:right;">
-0.20
</td>
<td style="text-align:right;">
-0.25
</td>
<td style="text-align:right;">
-0.50
</td>
<td style="text-align:right;">
-0.52
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
-0.16
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
-0.26
</td>
<td style="text-align:right;">
-0.47
</td>
<td style="text-align:right;">
-1.11
</td>
<td style="text-align:right;">
-1.65
</td>
<td style="text-align:right;">
-1.73
</td>
<td style="text-align:right;">
-2.78
</td>
<td style="text-align:right;">
-2.74
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
0.41
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.56
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.57
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
0.57
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
0.12
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.28
</td>
<td style="text-align:right;">
2.70
</td>
<td style="text-align:right;">
3.21
</td>
<td style="text-align:right;">
3.35
</td>
<td style="text-align:right;">
2.58
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
2.57
</td>
<td style="text-align:right;">
2.12
</td>
<td style="text-align:right;">
2.51
</td>
<td style="text-align:right;">
1.73
</td>
<td style="text-align:right;">
0.47
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
-0.08
</td>
<td style="text-align:right;">
-0.07
</td>
<td style="text-align:right;">
-0.18
</td>
<td style="text-align:right;">
-0.27
</td>
<td style="text-align:right;">
-0.24
</td>
<td style="text-align:right;">
-0.51
</td>
<td style="text-align:right;">
-0.60
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.47
</td>
<td style="text-align:right;">
-0.64
</td>
<td style="text-align:right;">
-0.44
</td>
<td style="text-align:right;">
-1.35
</td>
<td style="text-align:right;">
-2.10
</td>
<td style="text-align:right;">
-1.69
</td>
<td style="text-align:right;">
-2.73
</td>
<td style="text-align:right;">
-2.95
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
0.51
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.56
</td>
<td style="text-align:right;">
0.07
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.88
</td>
<td style="text-align:right;">
2.82
</td>
<td style="text-align:right;">
3.44
</td>
<td style="text-align:right;">
3.18
</td>
<td style="text-align:right;">
2.85
</td>
<td style="text-align:right;">
2.93
</td>
<td style="text-align:right;">
2.57
</td>
<td style="text-align:right;">
2.23
</td>
<td style="text-align:right;">
2.24
</td>
<td style="text-align:right;">
1.71
</td>
<td style="text-align:right;">
0.27
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
0.04
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.18
</td>
<td style="text-align:right;">
-0.25
</td>
<td style="text-align:right;">
-0.36
</td>
<td style="text-align:right;">
-0.45
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
0.72
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.46
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
-0.16
</td>
<td style="text-align:right;">
-0.67
</td>
<td style="text-align:right;">
-1.42
</td>
<td style="text-align:right;">
-1.85
</td>
<td style="text-align:right;">
-2.33
</td>
<td style="text-align:right;">
-2.55
</td>
<td style="text-align:right;">
-3.23
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
0.40
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.55
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
0.43
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
1.72
</td>
<td style="text-align:right;">
3.52
</td>
<td style="text-align:right;">
2.89
</td>
<td style="text-align:right;">
3.42
</td>
<td style="text-align:right;">
2.99
</td>
<td style="text-align:right;">
3.39
</td>
<td style="text-align:right;">
3.24
</td>
<td style="text-align:right;">
3.14
</td>
<td style="text-align:right;">
2.40
</td>
<td style="text-align:right;">
1.37
</td>
<td style="text-align:right;">
0.17
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
-0.15
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
-0.07
</td>
<td style="text-align:right;">
-0.27
</td>
<td style="text-align:right;">
-0.67
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
-1.03
</td>
<td style="text-align:right;">
1.41
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
-0.48
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
-0.40
</td>
<td style="text-align:right;">
-0.52
</td>
<td style="text-align:right;">
-1.91
</td>
<td style="text-align:right;">
-4.14
</td>
<td style="text-align:right;">
-3.24
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
1.01
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
-0.34
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.33
</td>
<td style="text-align:right;">
3.41
</td>
<td style="text-align:right;">
3.58
</td>
<td style="text-align:right;">
3.68
</td>
<td style="text-align:right;">
3.53
</td>
<td style="text-align:right;">
3.30
</td>
<td style="text-align:right;">
3.18
</td>
<td style="text-align:right;">
2.91
</td>
<td style="text-align:right;">
2.42
</td>
<td style="text-align:right;">
1.64
</td>
<td style="text-align:right;">
-1.92
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.14
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
-0.27
</td>
<td style="text-align:right;">
-0.58
</td>
<td style="text-align:right;">
-0.94
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
1.71
</td>
<td style="text-align:right;">
2.11
</td>
<td style="text-align:right;">
1.71
</td>
<td style="text-align:right;">
1.64
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
-0.52
</td>
<td style="text-align:right;">
-1.72
</td>
<td style="text-align:right;">
-2.91
</td>
<td style="text-align:right;">
-6.08
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
0.56
</td>
<td style="text-align:right;">
0.57
</td>
<td style="text-align:right;">
0.52
</td>
<td style="text-align:right;">
0.53
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.59
</td>
<td style="text-align:right;">
0.56
</td>
<td style="text-align:right;">
0.02
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
3.11
</td>
<td style="text-align:right;">
2.96
</td>
<td style="text-align:right;">
2.98
</td>
<td style="text-align:right;">
3.49
</td>
<td style="text-align:right;">
3.31
</td>
<td style="text-align:right;">
2.90
</td>
<td style="text-align:right;">
2.92
</td>
<td style="text-align:right;">
2.24
</td>
<td style="text-align:right;">
1.74
</td>
<td style="text-align:right;">
0.07
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
0.14
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.33
</td>
<td style="text-align:right;">
-0.55
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
0.86
</td>
<td style="text-align:right;">
1.09
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
-0.72
</td>
<td style="text-align:right;">
-0.91
</td>
<td style="text-align:right;">
-2.44
</td>
<td style="text-align:right;">
-3.37
</td>
<td style="text-align:right;">
-3.83
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
0.51
</td>
<td style="text-align:right;">
0.60
</td>
<td style="text-align:right;">
0.53
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
0.60
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
0.52
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
2.69
</td>
<td style="text-align:right;">
3.31
</td>
<td style="text-align:right;">
3.05
</td>
<td style="text-align:right;">
3.14
</td>
<td style="text-align:right;">
3.42
</td>
<td style="text-align:right;">
3.40
</td>
<td style="text-align:right;">
3.33
</td>
<td style="text-align:right;">
2.79
</td>
<td style="text-align:right;">
2.19
</td>
<td style="text-align:right;">
1.65
</td>
<td style="text-align:right;">
0.18
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
0.17
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
-0.12
</td>
<td style="text-align:right;">
-0.31
</td>
<td style="text-align:right;">
-0.57
</td>
<td style="text-align:right;">
-0.71
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
1.25
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.48
</td>
<td style="text-align:right;">
0.37
</td>
<td style="text-align:right;">
0.57
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
-1.02
</td>
<td style="text-align:right;">
-2.33
</td>
<td style="text-align:right;">
-3.61
</td>
<td style="text-align:right;">
-3.96
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
0.45
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.55
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.59
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.53
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.05
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.43
</td>
<td style="text-align:right;">
3.38
</td>
<td style="text-align:right;">
3.04
</td>
<td style="text-align:right;">
3.35
</td>
<td style="text-align:right;">
3.08
</td>
<td style="text-align:right;">
3.19
</td>
<td style="text-align:right;">
3.25
</td>
<td style="text-align:right;">
2.84
</td>
<td style="text-align:right;">
1.99
</td>
<td style="text-align:right;">
1.57
</td>
<td style="text-align:right;">
0.23
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
-0.06
</td>
<td style="text-align:right;">
-0.34
</td>
<td style="text-align:right;">
-0.56
</td>
<td style="text-align:right;">
-0.67
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
1.63
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
-0.47
</td>
<td style="text-align:right;">
-2.61
</td>
<td style="text-align:right;">
-3.60
</td>
<td style="text-align:right;">
-3.94
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
