---
title: 'Tidy Asset Pricing - Part II: Beta and Stock Returns'
subtitle: 
summary: 'On the estimation on market betas and their relation to stock returns'
authors:
- admin
tags:
- Asset Pricing
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
2969.59
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
3158.63
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
3120.50
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
3070.92
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
2973.81
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
0.94
</td>
<td style="text-align:right;">
0.84
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
1.06
</td>
<td style="text-align:right;">
1.15
</td>
<td style="text-align:right;">
1.03
</td>
<td style="text-align:right;">
0.93
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
3.73
</td>
<td style="text-align:right;">
4.04
</td>
<td style="text-align:right;">
4.32
</td>
<td style="text-align:right;">
4.37
</td>
<td style="text-align:right;">
4.26
</td>
<td style="text-align:right;">
4.14
</td>
<td style="text-align:right;">
3.99
</td>
<td style="text-align:right;">
4.00
</td>
<td style="text-align:right;">
3.33
</td>
<td style="text-align:right;">
2.63
</td>
<td style="text-align:right;">
0.34
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
0.25
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
-0.22
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
2.13
</td>
<td style="text-align:right;">
2.68
</td>
<td style="text-align:right;">
2.80
</td>
<td style="text-align:right;">
2.91
</td>
<td style="text-align:right;">
2.91
</td>
<td style="text-align:right;">
2.08
</td>
<td style="text-align:right;">
1.81
</td>
<td style="text-align:right;">
1.19
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
-1.25
</td>
<td style="text-align:right;">
-3.16
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
0.91
</td>
<td style="text-align:right;">
0.82
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
1.11
</td>
<td style="text-align:right;">
1.09
</td>
<td style="text-align:right;">
1.08
</td>
<td style="text-align:right;">
1.06
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.10
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
4.34
</td>
<td style="text-align:right;">
4.02
</td>
<td style="text-align:right;">
4.22
</td>
<td style="text-align:right;">
4.70
</td>
<td style="text-align:right;">
4.08
</td>
<td style="text-align:right;">
4.28
</td>
<td style="text-align:right;">
3.89
</td>
<td style="text-align:right;">
3.64
</td>
<td style="text-align:right;">
3.36
</td>
<td style="text-align:right;">
2.56
</td>
<td style="text-align:right;">
0.49
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.41
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.25
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
0.01
</td>
<td style="text-align:right;">
-0.22
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
3.39
</td>
<td style="text-align:right;">
3.28
</td>
<td style="text-align:right;">
3.34
</td>
<td style="text-align:right;">
3.77
</td>
<td style="text-align:right;">
2.63
</td>
<td style="text-align:right;">
2.80
</td>
<td style="text-align:right;">
1.88
</td>
<td style="text-align:right;">
1.27
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
-1.18
</td>
<td style="text-align:right;">
-3.57
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
0.86
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
1.03
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
1.10
</td>
<td style="text-align:right;">
1.04
</td>
<td style="text-align:right;">
1.09
</td>
<td style="text-align:right;">
1.07
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
-0.07
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
4.40
</td>
<td style="text-align:right;">
4.53
</td>
<td style="text-align:right;">
4.88
</td>
<td style="text-align:right;">
4.42
</td>
<td style="text-align:right;">
3.97
</td>
<td style="text-align:right;">
3.98
</td>
<td style="text-align:right;">
3.77
</td>
<td style="text-align:right;">
3.63
</td>
<td style="text-align:right;">
3.34
</td>
<td style="text-align:right;">
2.62
</td>
<td style="text-align:right;">
-0.29
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
0.35
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
-0.19
</td>
<td style="text-align:right;">
-0.72
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.48
</td>
<td style="text-align:right;">
4.14
</td>
<td style="text-align:right;">
4.36
</td>
<td style="text-align:right;">
3.85
</td>
<td style="text-align:right;">
2.59
</td>
<td style="text-align:right;">
2.35
</td>
<td style="text-align:right;">
1.39
</td>
<td style="text-align:right;">
1.53
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
-1.03
</td>
<td style="text-align:right;">
-3.78
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
0.85
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.03
</td>
<td style="text-align:right;">
1.11
</td>
<td style="text-align:right;">
1.06
</td>
<td style="text-align:right;">
1.07
</td>
<td style="text-align:right;">
1.08
</td>
<td style="text-align:right;">
0.85
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
4.61
</td>
<td style="text-align:right;">
4.89
</td>
<td style="text-align:right;">
4.65
</td>
<td style="text-align:right;">
4.51
</td>
<td style="text-align:right;">
4.13
</td>
<td style="text-align:right;">
4.10
</td>
<td style="text-align:right;">
3.78
</td>
<td style="text-align:right;">
3.49
</td>
<td style="text-align:right;">
3.24
</td>
<td style="text-align:right;">
2.51
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
0.38
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
-0.01
</td>
<td style="text-align:right;">
-0.17
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
3.89
</td>
<td style="text-align:right;">
4.59
</td>
<td style="text-align:right;">
4.16
</td>
<td style="text-align:right;">
3.94
</td>
<td style="text-align:right;">
3.02
</td>
<td style="text-align:right;">
2.54
</td>
<td style="text-align:right;">
1.40
</td>
<td style="text-align:right;">
1.22
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.91
</td>
<td style="text-align:right;">
-3.64
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
1.11
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
1.04
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.71
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
3.89
</td>
<td style="text-align:right;">
4.61
</td>
<td style="text-align:right;">
4.64
</td>
<td style="text-align:right;">
4.55
</td>
<td style="text-align:right;">
4.52
</td>
<td style="text-align:right;">
4.05
</td>
<td style="text-align:right;">
3.98
</td>
<td style="text-align:right;">
3.58
</td>
<td style="text-align:right;">
3.06
</td>
<td style="text-align:right;">
2.02
</td>
<td style="text-align:right;">
-2.26
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.37
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
-0.13
</td>
<td style="text-align:right;">
-0.47
</td>
<td style="text-align:right;">
-0.87
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.52
</td>
<td style="text-align:right;">
3.78
</td>
<td style="text-align:right;">
3.96
</td>
<td style="text-align:right;">
3.40
</td>
<td style="text-align:right;">
3.16
</td>
<td style="text-align:right;">
1.99
</td>
<td style="text-align:right;">
1.48
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
-1.15
</td>
<td style="text-align:right;">
-2.84
</td>
<td style="text-align:right;">
-6.40
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
1.11
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
0.71
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
4.07
</td>
<td style="text-align:right;">
4.50
</td>
<td style="text-align:right;">
4.60
</td>
<td style="text-align:right;">
4.64
</td>
<td style="text-align:right;">
4.40
</td>
<td style="text-align:right;">
4.06
</td>
<td style="text-align:right;">
3.85
</td>
<td style="text-align:right;">
3.49
</td>
<td style="text-align:right;">
2.91
</td>
<td style="text-align:right;">
1.99
</td>
<td style="text-align:right;">
-1.93
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
0.42
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.32
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
-0.18
</td>
<td style="text-align:right;">
-0.48
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
3.01
</td>
<td style="text-align:right;">
4.11
</td>
<td style="text-align:right;">
3.79
</td>
<td style="text-align:right;">
3.55
</td>
<td style="text-align:right;">
2.73
</td>
<td style="text-align:right;">
2.07
</td>
<td style="text-align:right;">
1.25
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
-1.56
</td>
<td style="text-align:right;">
-2.91
</td>
<td style="text-align:right;">
-6.13
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
1.15
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
0.72
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
4.39
</td>
<td style="text-align:right;">
4.55
</td>
<td style="text-align:right;">
4.58
</td>
<td style="text-align:right;">
4.35
</td>
<td style="text-align:right;">
4.30
</td>
<td style="text-align:right;">
4.20
</td>
<td style="text-align:right;">
3.82
</td>
<td style="text-align:right;">
3.41
</td>
<td style="text-align:right;">
2.86
</td>
<td style="text-align:right;">
2.08
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
0.55
</td>
<td style="text-align:right;">
0.41
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
-0.20
</td>
<td style="text-align:right;">
-0.49
</td>
<td style="text-align:right;">
-1.06
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.97
</td>
<td style="text-align:right;">
4.09
</td>
<td style="text-align:right;">
3.71
</td>
<td style="text-align:right;">
3.25
</td>
<td style="text-align:right;">
2.64
</td>
<td style="text-align:right;">
2.39
</td>
<td style="text-align:right;">
1.12
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
-1.73
</td>
<td style="text-align:right;">
-3.03
</td>
<td style="text-align:right;">
-6.59
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
1.17
</td>
<td style="text-align:right;">
1.06
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
1.03
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.70
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
4.89
</td>
<td style="text-align:right;">
4.79
</td>
<td style="text-align:right;">
4.48
</td>
<td style="text-align:right;">
4.41
</td>
<td style="text-align:right;">
4.32
</td>
<td style="text-align:right;">
4.21
</td>
<td style="text-align:right;">
3.70
</td>
<td style="text-align:right;">
3.37
</td>
<td style="text-align:right;">
2.96
</td>
<td style="text-align:right;">
2.02
</td>
<td style="text-align:right;">
-2.26
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.48
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
-0.01
</td>
<td style="text-align:right;">
-0.15
</td>
<td style="text-align:right;">
-0.50
</td>
<td style="text-align:right;">
-1.14
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
5.04
</td>
<td style="text-align:right;">
4.75
</td>
<td style="text-align:right;">
3.58
</td>
<td style="text-align:right;">
3.43
</td>
<td style="text-align:right;">
2.76
</td>
<td style="text-align:right;">
2.48
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
-0.06
</td>
<td style="text-align:right;">
-1.21
</td>
<td style="text-align:right;">
-3.25
</td>
<td style="text-align:right;">
-6.96
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
1.13
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
1.08
</td>
<td style="text-align:right;">
1.07
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
-0.44
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
4.97
</td>
<td style="text-align:right;">
4.66
</td>
<td style="text-align:right;">
4.71
</td>
<td style="text-align:right;">
4.55
</td>
<td style="text-align:right;">
4.20
</td>
<td style="text-align:right;">
4.05
</td>
<td style="text-align:right;">
3.97
</td>
<td style="text-align:right;">
3.50
</td>
<td style="text-align:right;">
3.06
</td>
<td style="text-align:right;">
2.05
</td>
<td style="text-align:right;">
-2.10
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
0.47
</td>
<td style="text-align:right;">
0.46
</td>
<td style="text-align:right;">
0.37
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
-0.09
</td>
<td style="text-align:right;">
-0.47
</td>
<td style="text-align:right;">
-1.11
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
5.63
</td>
<td style="text-align:right;">
4.63
</td>
<td style="text-align:right;">
4.21
</td>
<td style="text-align:right;">
3.86
</td>
<td style="text-align:right;">
2.90
</td>
<td style="text-align:right;">
2.19
</td>
<td style="text-align:right;">
1.69
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
-0.74
</td>
<td style="text-align:right;">
-3.11
</td>
<td style="text-align:right;">
-6.54
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
0.48
</td>
<td style="text-align:right;">
0.57
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0.84
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.27
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
3.77
</td>
<td style="text-align:right;">
4.40
</td>
<td style="text-align:right;">
4.75
</td>
<td style="text-align:right;">
3.96
</td>
<td style="text-align:right;">
4.04
</td>
<td style="text-align:right;">
3.94
</td>
<td style="text-align:right;">
3.47
</td>
<td style="text-align:right;">
3.19
</td>
<td style="text-align:right;">
2.13
</td>
<td style="text-align:right;">
1.09
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
-0.01
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
-0.13
</td>
<td style="text-align:right;">
-0.13
</td>
<td style="text-align:right;">
-0.47
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
-0.06
</td>
<td style="text-align:right;">
1.04
</td>
<td style="text-align:right;">
1.57
</td>
<td style="text-align:right;">
2.54
</td>
<td style="text-align:right;">
1.50
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
-1.78
</td>
<td style="text-align:right;">
-1.34
</td>
<td style="text-align:right;">
-3.13
</td>
<td style="text-align:right;">
-2.34
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
0.50
</td>
<td style="text-align:right;">
0.52
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.84
</td>
<td style="text-align:right;">
0.74
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
3.38
</td>
<td style="text-align:right;">
3.50
</td>
<td style="text-align:right;">
4.24
</td>
<td style="text-align:right;">
4.92
</td>
<td style="text-align:right;">
3.77
</td>
<td style="text-align:right;">
4.01
</td>
<td style="text-align:right;">
3.50
</td>
<td style="text-align:right;">
3.09
</td>
<td style="text-align:right;">
3.16
</td>
<td style="text-align:right;">
2.30
</td>
<td style="text-align:right;">
1.15
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
0.07
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.15
</td>
<td style="text-align:right;">
-0.41
</td>
<td style="text-align:right;">
-0.53
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
0.93
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
3.86
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
-0.80
</td>
<td style="text-align:right;">
-1.43
</td>
<td style="text-align:right;">
-1.54
</td>
<td style="text-align:right;">
-2.96
</td>
<td style="text-align:right;">
-2.78
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
0.50
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.84
</td>
<td style="text-align:right;">
0.68
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
3.57
</td>
<td style="text-align:right;">
3.86
</td>
<td style="text-align:right;">
4.50
</td>
<td style="text-align:right;">
4.41
</td>
<td style="text-align:right;">
3.40
</td>
<td style="text-align:right;">
3.86
</td>
<td style="text-align:right;">
3.28
</td>
<td style="text-align:right;">
2.69
</td>
<td style="text-align:right;">
3.06
</td>
<td style="text-align:right;">
2.16
</td>
<td style="text-align:right;">
0.48
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
0.11
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
-0.07
</td>
<td style="text-align:right;">
-0.17
</td>
<td style="text-align:right;">
-0.14
</td>
<td style="text-align:right;">
-0.41
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
1.34
</td>
<td style="text-align:right;">
1.49
</td>
<td style="text-align:right;">
2.02
</td>
<td style="text-align:right;">
2.80
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.55
</td>
<td style="text-align:right;">
-1.02
</td>
<td style="text-align:right;">
-2.34
</td>
<td style="text-align:right;">
-1.48
</td>
<td style="text-align:right;">
-2.96
</td>
<td style="text-align:right;">
-2.98
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
0.61
</td>
<td style="text-align:right;">
0.60
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
0.08
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
4.31
</td>
<td style="text-align:right;">
4.03
</td>
<td style="text-align:right;">
4.73
</td>
<td style="text-align:right;">
4.24
</td>
<td style="text-align:right;">
3.70
</td>
<td style="text-align:right;">
3.76
</td>
<td style="text-align:right;">
3.26
</td>
<td style="text-align:right;">
2.82
</td>
<td style="text-align:right;">
2.77
</td>
<td style="text-align:right;">
2.14
</td>
<td style="text-align:right;">
0.30
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
-0.08
</td>
<td style="text-align:right;">
-0.15
</td>
<td style="text-align:right;">
-0.26
</td>
<td style="text-align:right;">
-0.35
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
2.39
</td>
<td style="text-align:right;">
1.98
</td>
<td style="text-align:right;">
2.75
</td>
<td style="text-align:right;">
2.07
</td>
<td style="text-align:right;">
1.27
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
-1.20
</td>
<td style="text-align:right;">
-1.97
</td>
<td style="text-align:right;">
-2.48
</td>
<td style="text-align:right;">
-2.65
</td>
<td style="text-align:right;">
-3.26
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
0.49
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.60
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.80
</td>
<td style="text-align:right;">
0.74
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
2.46
</td>
<td style="text-align:right;">
4.91
</td>
<td style="text-align:right;">
4.16
</td>
<td style="text-align:right;">
4.73
</td>
<td style="text-align:right;">
4.10
</td>
<td style="text-align:right;">
4.40
</td>
<td style="text-align:right;">
4.18
</td>
<td style="text-align:right;">
4.02
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
1.77
</td>
<td style="text-align:right;">
0.20
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
-0.17
</td>
<td style="text-align:right;">
-0.57
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
-0.45
</td>
<td style="text-align:right;">
3.55
</td>
<td style="text-align:right;">
2.24
</td>
<td style="text-align:right;">
3.16
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.95
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
0.53
</td>
<td style="text-align:right;">
-2.04
</td>
<td style="text-align:right;">
-4.65
</td>
<td style="text-align:right;">
-3.25
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
1.11
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
0.71
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
4.07
</td>
<td style="text-align:right;">
4.50
</td>
<td style="text-align:right;">
4.60
</td>
<td style="text-align:right;">
4.64
</td>
<td style="text-align:right;">
4.40
</td>
<td style="text-align:right;">
4.06
</td>
<td style="text-align:right;">
3.85
</td>
<td style="text-align:right;">
3.49
</td>
<td style="text-align:right;">
2.91
</td>
<td style="text-align:right;">
1.99
</td>
<td style="text-align:right;">
-1.93
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
0.42
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.32
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
-0.18
</td>
<td style="text-align:right;">
-0.48
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
3.01
</td>
<td style="text-align:right;">
4.11
</td>
<td style="text-align:right;">
3.79
</td>
<td style="text-align:right;">
3.55
</td>
<td style="text-align:right;">
2.73
</td>
<td style="text-align:right;">
2.07
</td>
<td style="text-align:right;">
1.25
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
-1.56
</td>
<td style="text-align:right;">
-2.91
</td>
<td style="text-align:right;">
-6.13
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
0.65
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.60
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.65
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
3.76
</td>
<td style="text-align:right;">
4.51
</td>
<td style="text-align:right;">
4.28
</td>
<td style="text-align:right;">
4.14
</td>
<td style="text-align:right;">
4.71
</td>
<td style="text-align:right;">
4.36
</td>
<td style="text-align:right;">
3.81
</td>
<td style="text-align:right;">
3.68
</td>
<td style="text-align:right;">
2.80
</td>
<td style="text-align:right;">
2.16
</td>
<td style="text-align:right;">
0.09
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
-0.45
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
2.21
</td>
<td style="text-align:right;">
3.10
</td>
<td style="text-align:right;">
2.21
</td>
<td style="text-align:right;">
2.14
</td>
<td style="text-align:right;">
2.72
</td>
<td style="text-align:right;">
1.63
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
-0.01
</td>
<td style="text-align:right;">
-2.85
</td>
<td style="text-align:right;">
-3.60
</td>
<td style="text-align:right;">
-3.85
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
0.60
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.62
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
3.87
</td>
<td style="text-align:right;">
4.87
</td>
<td style="text-align:right;">
4.28
</td>
<td style="text-align:right;">
4.30
</td>
<td style="text-align:right;">
4.62
</td>
<td style="text-align:right;">
4.42
</td>
<td style="text-align:right;">
4.27
</td>
<td style="text-align:right;">
3.55
</td>
<td style="text-align:right;">
2.75
</td>
<td style="text-align:right;">
2.05
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
0.24
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
-0.21
</td>
<td style="text-align:right;">
-0.47
</td>
<td style="text-align:right;">
-0.72
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.45
</td>
<td style="text-align:right;">
3.53
</td>
<td style="text-align:right;">
2.58
</td>
<td style="text-align:right;">
2.31
</td>
<td style="text-align:right;">
2.59
</td>
<td style="text-align:right;">
2.69
</td>
<td style="text-align:right;">
1.73
</td>
<td style="text-align:right;">
-0.38
</td>
<td style="text-align:right;">
-2.66
</td>
<td style="text-align:right;">
-3.87
</td>
<td style="text-align:right;">
-4.02
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
0.54
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.59
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
3.64
</td>
<td style="text-align:right;">
4.81
</td>
<td style="text-align:right;">
4.37
</td>
<td style="text-align:right;">
4.57
</td>
<td style="text-align:right;">
4.19
</td>
<td style="text-align:right;">
4.22
</td>
<td style="text-align:right;">
4.24
</td>
<td style="text-align:right;">
3.63
</td>
<td style="text-align:right;">
2.55
</td>
<td style="text-align:right;">
2.00
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
0.22
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
-0.24
</td>
<td style="text-align:right;">
-0.46
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
2.27
</td>
<td style="text-align:right;">
3.70
</td>
<td style="text-align:right;">
2.72
</td>
<td style="text-align:right;">
3.20
</td>
<td style="text-align:right;">
2.20
</td>
<td style="text-align:right;">
1.85
</td>
<td style="text-align:right;">
1.96
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
-3.11
</td>
<td style="text-align:right;">
-4.19
</td>
<td style="text-align:right;">
-3.98
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
1.17
</td>
<td style="text-align:right;">
1.16
</td>
<td style="text-align:right;">
1.16
</td>
<td style="text-align:right;">
1.15
</td>
<td style="text-align:right;">
1.29
</td>
<td style="text-align:right;">
1.31
</td>
<td style="text-align:right;">
1.37
</td>
<td style="text-align:right;">
1.41
</td>
<td style="text-align:right;">
1.45
</td>
</tr>
<tr>
<td style="text-align:left;">
t-stat
</td>
<td style="text-align:right;">
5.80
</td>
<td style="text-align:right;">
6.38
</td>
<td style="text-align:right;">
6.85
</td>
<td style="text-align:right;">
7.10
</td>
<td style="text-align:right;">
5.78
</td>
<td style="text-align:right;">
6.02
</td>
<td style="text-align:right;">
6.32
</td>
<td style="text-align:right;">
6.66
</td>
<td style="text-align:right;">
6.92
</td>
</tr>
<tr>
<td style="text-align:left;">
beta
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
0.79
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
-1.57
</td>
<td style="text-align:right;">
-1.56
</td>
<td style="text-align:right;">
-1.82
</td>
<td style="text-align:right;">
-2.00
</td>
<td style="text-align:right;">
-2.01
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
2940.78
</td>
<td style="text-align:right;">
2747.98
</td>
<td style="text-align:right;">
2680.18
</td>
<td style="text-align:right;">
2687.65
</td>
<td style="text-align:right;">
3133.25
</td>
<td style="text-align:right;">
3095.53
</td>
<td style="text-align:right;">
3046.31
</td>
<td style="text-align:right;">
2949.95
</td>
<td style="text-align:right;">
2715.65
</td>
</tr>
</tbody>
</table>
<p>For all measures based on monthly data, I do not find any statistically significant relation between beta and stock returns, while the measures based on daily returns do exhibit a statistically significant relation for longer estimation periods. Moreover, the intercepts are positive and statistically significant across all beta measures, although, controlling for the effect of beta, the average excess stock returns should be zero according to the CAPM. The results thus provide no evidence in support of the CAPM.</p>
<p>Finally, it is worth noting that the average adjusted R-squared ranges only from 2 to 3 percent. Low values of R-squared are common in empirical asset pricing studies. The main reason put forward in the literature is that predicting stock returns is that realized stock returns are just a very noisy measure for expected stock returns (e.g., <a href="https://onlinelibrary.wiley.com/doi/abs/10.1111/0022-1082.00144">Elton, 1999</a>).</p>

</dl>
