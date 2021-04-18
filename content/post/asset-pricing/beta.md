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
lastmod: "2021-04-18T00:00:00Z"
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
<p>This note proposes an estimation and evaluation procedure for market betas following <a href="https://www.wiley.com/en-us/Empirical+Asset+Pricing%3A+The+Cross+Section+of+Stock+Returns-p-9781118095041">Bali, Engle and Murray</a> (BEM). I do not go into details about the foundations of market beta, but simply refer to any treatment of the Capital Asset Pricing Model (<a href="https://en.wikipedia.org/wiki/Capital_asset_pricing_model">CAPM</a>). However, I provide details about all the functions that I use to compute my results. As the post introduces a number of approaches of BEM, there are also many functions to discuss. Subsequent blog posts then recycle most of the functions.</p>
<p>This note has two objectives: the first one is to implement several approaches to estimate a stock’s beta in a tidy manner and to empirically examine these measures. The second objective is to analyze the cross-sectional relation between stock returns and market beta. The following disclaimer applies: the text below references an opinion and is for information purposes only and I do not intend to provide any investment advice.</p>
<p>I mainly use the following packages throughout this note:</p>
<pre class="r"><code>library(tidyverse)  # for grammar
library(scales)     # for nicer axes of figures
library(viridis)    # for nicer colors in figures
library(slider)     # for rolling window operations
library(moments)    # for skewness and kurtosis 
library(corrr)      # for correlations in table format
library(kableExtra) # for nicer html tables
library(broom)      # for converting statistical objects to tidy tibbles
library(sandwich)   # for Newey-West standard errors</code></pre>
<p>First, I load the monthly and daily return data that I prepared in an <a href="https://christophscheuch.github.io/post/asset-pricing/crsp-sample/">earlier post</a> and compute the stock and market excess returns that are required for the beta estimation procedure.</p>
<pre class="r"><code>tbl.crsp_monthly &lt;- read_rds(&quot;data/crsp_monthly.rds&quot;) %&gt;%
  mutate(ret = ret_adj - rf_ff,
         mkt = mkt_ff - rf_ff) %&gt;%
  select(permno, month, mkt, ret)

tbl.crsp_daily &lt;- read_rds(&quot;data/crsp_daily.rds&quot;) %&gt;%
  mutate(ret = ret - rf_ff,
         mkt = mkt_ff - rf_ff) %&gt;%
  select(permno, month, mkt, ret)</code></pre>
<div id="estimation" class="section level1">
<h1>Estimation</h1>
<p>According to the CAPM, cross-sectional variation in expected asset returns should be a function of the covariance between the return of the asset and the return on the market portfolio – the beta. To estimate stock-specific betas, I have to regress excess stock returns on excess returns of the market portfolio. Throughout the note, I use the market excess return and risk-free rate provided in <a href="https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html">Ken French’s data library</a>. The estimation procedure is based on a rolling window estimation where I may use either monthly or daily returns and different window lengths. I follow <a href="https://www.wiley.com/en-us/Empirical+Asset+Pricing%3A+The+Cross+Section+of+Stock+Returns-p-9781118095041">BEM</a> and examine nine different combinations of estimation periods and data frequencies. For monthly return data, I use excess return observations over the past 1, 2, 3 and 5 years, requiring at least 10, 20, 24 and 24 valid return observations, respectively. For daily data measures, I use period lengths of 1, 3, 6, 12 and 24 months and require 15, 50, 100, 200 and 450 days of valid return data. The function below implements these requirements for a given estimation window and data frequency and returns the estimated beta.</p>
<pre class="r"><code>fun.capm_regression &lt;- function(x, window, freq) {
  # Drop missing values
  x &lt;- na.omit(x)
  
  # Determine minimum number of observations depending on window size and data frequency
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
  
  # Check if minimum number of observations is satisfied
  if (nrow(x) &lt; check) {
    return(as.numeric(NA))
  } else {
    reg &lt;- lm(ret ~ mkt, data = x)
    return(as.numeric(reg$coefficients[2]))
  }
}</code></pre>
<p>To perform the rolling window estimation, I use the <code>slider</code> package of <a href="https://github.com/DavisVaughan/slider">Davis Vaughan</a>, which provides a family of sliding window functions similar to <code>purrr::map()</code>. Most importantly, the <code>slide_period</code> function is able to handle months in its window input in a straight-forward manner. I thus avoid using any time-series package (e.g., <code>zoo</code>) and converting the data to fit the package functions, but rather stay in the <code>tibble</code> world.</p>
<pre class="r"><code>fun.rolling_capm_regression &lt;- function(x, window, freq) {
  # Nested tibbles of daily data need to be binded
  if (freq == &quot;daily&quot;) {
    x &lt;- bind_rows(x)
  }
  out &lt;- slide_period_vec(.x = x, .i = x$month, .period = &quot;month&quot;,
                   .f = function(x) {fun.capm_regression(x, window, freq)},
                   .before = (window-1), .complete = FALSE)
  return(out)
}</code></pre>
<p>I use the above function to compute several different variants of the beta estimator put forward by <a href="https://www.wiley.com/en-us/Empirical+Asset+Pricing%3A+The+Cross+Section+of+Stock+Returns-p-9781118095041">BEM</a>. First, I use monthly data to estimate betas with different years as input. However, the estimation below takes up a considerable amount of time when I use the whole CRSP sample (around 2.5 hours). In principle, I could considerably speed up the estimation procedure by parallelizing it across stocks (e.g. using <code>multidplyr</code> or putting the data into some SQL table). However, for the sake of exposition, I just provide the code for the joint estimation below.</p>
<pre class="r"><code>tbl.betas_monthly &lt;- tbl.crsp_monthly %&gt;%
  group_by(permno) %&gt;%
  arrange(month) %&gt;%
  mutate(beta_1y = fun.rolling_capm_regression(cur_data(), window = 12, freq = &quot;monthly&quot;),
         beta_2y = fun.rolling_capm_regression(cur_data(), window = 24, freq = &quot;monthly&quot;),
         beta_3y = fun.rolling_capm_regression(cur_data(), window = 36, freq = &quot;monthly&quot;),
         beta_5y = fun.rolling_capm_regression(cur_data(), window = 60, freq = &quot;monthly&quot;)) %&gt;%
  ungroup() %&gt;%
  select(-c(mkt, ret)) %&gt;%
  arrange(permno, month)</code></pre>
<p>Next, I use the same function to perform rolling window estimation on the daily return data. To use the above function, I first nest the data by month. the function then loads the relevant months of daily data and binds them together. Note that that the computation now takes about 3.5 hours for the whole CRSP sample, but is again in principle parallelizable.</p>
<pre class="r"><code>tbl.crsp_daily_nested &lt;- tbl.crsp_daily %&gt;%
  group_by(permno, month_group = month) %&gt;%
  nest(daily_data = c(month, mkt, ret)) %&gt;% 
  ungroup()

tbl.betas_daily &lt;- tbl.crsp_daily_nested %&gt;% 
  group_by(permno) %&gt;% 
  arrange(month_group) %&gt;%
  mutate(beta_1m = fun.rolling_capm_regression(daily_data, window = 1, freq = &quot;daily&quot;),
         beta_3m = fun.rolling_capm_regression(daily_data, window = 3, freq = &quot;daily&quot;),
         beta_6m = fun.rolling_capm_regression(daily_data, window = 6, freq = &quot;daily&quot;),
         beta_12m = fun.rolling_capm_regression(daily_data, window = 12, freq = &quot;daily&quot;),
         beta_24m = fun.rolling_capm_regression(daily_data, window = 24, freq = &quot;daily&quot;)
  ) %&gt;%
  ungroup() %&gt;% 
  select(permno, month = month_group, beta_1m, beta_3m, beta_6m, beta_12m, beta_24m) %&gt;% 
  arrange(permno, month)</code></pre>
</div>
<div id="descriptives" class="section level1">
<h1>Descriptives</h1>
<p>Let us add the beta estimates to the monthly CRSP sample and look at some descriptives.</p>
<pre class="r"><code>tbl.betas &lt;- read_rds(&quot;data/crsp_monthly.rds&quot;) %&gt;%
  select(permno, month, ret_adj_excess_f1, mkt_ff_excess_f1, mktcap, mktcap_cpi, exchange) %&gt;% 
  left_join(tbl.betas_monthly, by = c(&quot;permno&quot;, &quot;month&quot;)) %&gt;%
  left_join(tbl.betas_daily, by = c(&quot;permno&quot;, &quot;month&quot;))

# Define vector of betas for convenience (e.g. factor labels and maps)
vec.betas &lt;- c(&quot;beta_1y&quot;, &quot;beta_2y&quot;, &quot;beta_3y&quot;, &quot;beta_5y&quot;,
               &quot;beta_1m&quot;, &quot;beta_3m&quot;, &quot;beta_6m&quot;, &quot;beta_12m&quot;, &quot;beta_24m&quot;)</code></pre>
<p>I first check whether our estimation procedure actually produce beta estimates. The figure below shows the share of stocks with a corresponding beta estimate for each month since 1927. I can see drops in 1962 and 1972 because that is when AMEX and NASDAQ enter the sample, respectively. As a new batch of stocks enters the sample, I cannot compute beta estimates until there is sufficient data available for these stocks. Finally, note that the estimates based on daily data drop in 2019 since I currently don’t have access to more recent daily CRSP data.</p>
<pre class="r"><code>tbl.coverage &lt;- tbl.betas %&gt;% 
  select(month, permno, contains(&quot;beta&quot;)) %&gt;% 
  pivot_longer(cols = contains(&quot;beta&quot;), names_to = &quot;measure&quot;, values_to = &quot;estimate&quot;) %&gt;% 
  group_by(month, measure) %&gt;% 
  summarize(share = n_distinct(permno[!is.na(estimate)]) / n_distinct(permno)) %&gt;% 
  ungroup()

fig.coverage &lt;- tbl.coverage %&gt;% 
  mutate(measure = factor(measure, levels = vec.betas)) %&gt;% 
  ggplot(aes(x = month, y = share, color = measure, linetype = measure)) +
  geom_line() +
  labs(x = NULL, y = NULL, color = NULL, linetype = NULL,
       title = &quot;Monthly share of stocks with beta estimate&quot;) + 
  scale_x_date(expand = c(0, 0), date_breaks = &quot;10 years&quot;, date_labels = &quot;%Y&quot;) +
  scale_y_continuous(labels = percent, breaks = pretty_breaks()) + 
  theme_classic() +
  scale_color_viridis_d()</code></pre>
<center>
<img src="data:image/png;base64,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" width="768" />
</center>
<p>As a next evaluation, I compute average summary statistics across all months, just like <a href="https://www.wiley.com/en-us/Empirical+Asset+Pricing%3A+The+Cross+Section+of+Stock+Returns-p-9781118095041">BEM</a>.</p>
<pre class="r"><code># Compute summary statistics for each month
tbl.betas_ts &lt;- tbl.betas %&gt;%
  select(month, contains(&quot;beta&quot;)) %&gt;%
  pivot_longer(cols = contains(&quot;beta&quot;), names_to = &quot;measure&quot;, values_drop_na = TRUE) %&gt;%
  group_by(measure, month) %&gt;%
  summarize(mean = mean(value),
            sd = sd(value),
            skew = skewness(value),
            kurt = kurtosis(value),
            min = min(value),
            q05 = quantile(value, 0.05),
            q25 = quantile(value, 0.25),
            q50 = quantile(value, 0.50),
            q75 = quantile(value, 0.75),
            q95 = quantile(value, 0.95),
            max = max(value),
            n = n()) %&gt;%
  ungroup()

# Compute average summary statistics across months
tab.betas_summary &lt;- tbl.betas_ts %&gt;%
  select(-month) %&gt;%
  group_by(measure) %&gt;%
  summarize_all(list(~mean(., na.rm = TRUE))) %&gt;%
  mutate(measure = factor(measure, levels = vec.betas)) %&gt;%
  arrange(measure) %&gt;% 
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;))
tab.betas_summary</code></pre>
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
1.15
</td>
<td style="text-align:right;">
1.17
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
21.40
</td>
<td style="text-align:right;">
-7.60
</td>
<td style="text-align:right;">
-0.41
</td>
<td style="text-align:right;">
0.48
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
1.73
</td>
<td style="text-align:right;">
3.06
</td>
<td style="text-align:right;">
12.36
</td>
<td style="text-align:right;">
2972.14
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
0.63
</td>
<td style="text-align:right;">
9.21
</td>
<td style="text-align:right;">
-3.62
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
1.08
</td>
<td style="text-align:right;">
1.60
</td>
<td style="text-align:right;">
2.56
</td>
<td style="text-align:right;">
7.29
</td>
<td style="text-align:right;">
2611.63
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_3y
</td>
<td style="text-align:right;">
1.16
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
6.31
</td>
<td style="text-align:right;">
-2.09
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
1.09
</td>
<td style="text-align:right;">
1.54
</td>
<td style="text-align:right;">
2.37
</td>
<td style="text-align:right;">
5.71
</td>
<td style="text-align:right;">
2255.79
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_5y
</td>
<td style="text-align:right;">
1.15
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.56
</td>
<td style="text-align:right;">
6.91
</td>
<td style="text-align:right;">
-1.81
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.73
</td>
<td style="text-align:right;">
1.09
</td>
<td style="text-align:right;">
1.50
</td>
<td style="text-align:right;">
2.21
</td>
<td style="text-align:right;">
5.01
</td>
<td style="text-align:right;">
1975.01
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_1m
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
1.26
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
27.77
</td>
<td style="text-align:right;">
-11.01
</td>
<td style="text-align:right;">
-0.81
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
1.47
</td>
<td style="text-align:right;">
2.83
</td>
<td style="text-align:right;">
12.76
</td>
<td style="text-align:right;">
3144.43
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
0.82
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
12.71
</td>
<td style="text-align:right;">
-4.92
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
1.34
</td>
<td style="text-align:right;">
2.26
</td>
<td style="text-align:right;">
6.83
</td>
<td style="text-align:right;">
3112.60
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
0.68
</td>
<td style="text-align:right;">
0.39
</td>
<td style="text-align:right;">
7.08
</td>
<td style="text-align:right;">
-2.84
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
0.43
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
1.30
</td>
<td style="text-align:right;">
2.08
</td>
<td style="text-align:right;">
4.89
</td>
<td style="text-align:right;">
3069.49
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
0.60
</td>
<td style="text-align:right;">
0.44
</td>
<td style="text-align:right;">
4.22
</td>
<td style="text-align:right;">
-1.61
</td>
<td style="text-align:right;">
0.06
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
1.96
</td>
<td style="text-align:right;">
3.68
</td>
<td style="text-align:right;">
2972.62
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
3.40
</td>
<td style="text-align:right;">
-0.91
</td>
<td style="text-align:right;">
0.13
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
3.03
</td>
<td style="text-align:right;">
2734.99
</td>
</tr>
</tbody>
</table>
<p>There appears to be more stability in the distribution of estimated betas as I extend the estimation period. This might indicate that longer estimation periods yield more precise beta estimates. However, the average number of observations mechanically decreases as the estimation windows increase. Moreover, estimates based on monthly observations are on average higher than estimates based on daily returns.</p>
</div>
<div id="correlations" class="section level1">
<h1>Correlations</h1>
<p>The next table presents the time-series averages of the monthly cross-sectional (Pearson product-moment) correlations between different measures of market beta.</p>
<pre class="r"><code>fun.compute_correlations &lt;- function(data, use = &quot;complete.obs&quot;, method = &quot;pearson&quot;,
                                     diagonal = 1, quiet = TRUE, shave_upper = TRUE) {
  data %&gt;% 
    correlate(use = use, method = method, diagonal = diagonal, quiet = quiet) %&gt;% 
    shave(., upper = shave_upper)
}

tbl.correlations &lt;- tbl.betas %&gt;%
  na.omit() %&gt;% 
  select(month, contains(&quot;beta&quot;)) %&gt;% 
  group_by(month) %&gt;% 
  nest(betas = contains(&quot;beta&quot;)) %&gt;% 
  mutate(correlations = map(betas, fun.compute_correlations)) %&gt;% 
  select(-betas) %&gt;% 
  unnest(correlations) %&gt;% 
  rename(measure = term) %&gt;% 
  mutate(measure = factor(measure, levels = vec.betas)) %&gt;% 
  group_by(measure) %&gt;% 
  summarize_at(vars(contains(&quot;beta&quot;)), list(~mean(., na.rm = TRUE)))

tab.correlations &lt;- tbl.correlations %&gt;%
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;))
tab.correlations</code></pre>
<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
measure
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
0.76
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
0.66
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
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_5y
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
0.88
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
0.23
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.29
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
0.36
</td>
<td style="text-align:right;">
0.41
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.43
</td>
<td style="text-align:right;">
0.69
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
0.42
</td>
<td style="text-align:right;">
0.48
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.59
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
</tr>
<tr>
<td style="text-align:left;">
beta_12m
</td>
<td style="text-align:right;">
0.46
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0.56
</td>
<td style="text-align:right;">
0.56
</td>
<td style="text-align:right;">
0.53
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
beta_24m
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.57
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.48
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.80
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>
<p>Correlations range from as low as 0.22 between the one-year measure based on monthly data and the one-month measure based on daily data to as high as 0.91 between the one-year and two-year measure based on daily data. Again, correlations seem to increase as the length of the measurement period increases.</p>
</div>
<div id="persistence" class="section level1">
<h1>Persistence</h1>
<p>If I use a measure of a stock’s beta in further analyses, I want it to be fairly stable over time. To examine whether this is the case, I calculate the persistence of each measure as the time-series averages of monthly cross-sectional correlations between month <span class="math inline">\(t\)</span> and month <span class="math inline">\(t+\tau\)</span>. Following <a href="https://www.wiley.com/en-us/Empirical+Asset+Pricing%3A+The+Cross+Section+of+Stock+Returns-p-9781118095041">BEM</a>, I winsorize each measure at the 0.5% level on a monthly basis to mitigate the impact of outliers. The table below presents the persistence measures for lags one, three, six, 12, 24, 36, 48, 60 and 120 months.</p>
<pre class="r"><code>fun.winsorize &lt;- function(x, cut = 0.005){
  cut_point_top &lt;- quantile(x, 1 - cut, na.rm = TRUE)
  cut_point_bottom &lt;- quantile(x, cut, na.rm = TRUE)
  i &lt;- which(x &gt;= cut_point_top)
  x[i] &lt;- cut_point_top
  j &lt;- which(x &lt;= cut_point_bottom)
  x[j] &lt;- cut_point_bottom
  return(x)
}

fun.compute_persistence &lt;- function(data, var, tau, cut = 0.005){
  # Initialize output
  out &lt;- list()
  for (t in 1:length(tau)) {
    # Create a table with lagged dates
    dates &lt;- data %&gt;%
      distinct(month) %&gt;%
      mutate(month_lag = lag(month, tau[t]))
    
    # Winsorize the data
    data_winsorized &lt;- data %&gt;%
      filter(!is.na({{var}})) %&gt;%
      group_by(month) %&gt;%
      mutate(beta = fun.winsorize({{var}}, cut = cut)) %&gt;%
      ungroup() %&gt;% 
      select(permno, month, beta)
    
    # Compute correlation to lagged value
    correlation &lt;- data_winsorized %&gt;%
      left_join(dates, by = &quot;month&quot;) %&gt;%
      left_join(data_winsorized %&gt;% 
                  select(permno, month, beta_lag = beta),
                by = c(&quot;permno&quot;, &quot;month_lag&quot; = &quot;month&quot;)) %&gt;%
      select(month, beta, beta_lag) %&gt;%
      na.omit() %&gt;% 
      nest(betas = c(beta, beta_lag)) %&gt;%
      mutate(correlations = map(betas, fun.compute_correlations)) %&gt;%
      unnest(correlations) %&gt;%
      filter(term == &quot;beta_lag&quot;) %&gt;% 
      summarise(mean = mean(beta, na.rm = TRUE))
    
    out[[t]] &lt;- tibble(tau = tau[t],
                       {{var}} := as.numeric(correlation))
  }
  return(bind_rows(out))
}

vec.tau &lt;- c(1, 3, 6, 12, 24, 36, 48, 60, 120)

fun.compute_persistence(tbl.betas, beta_1y, vec.tau)

lst.persistence &lt;- vec.betas %&gt;%
  map(~fun.compute_persistence(tbl.betas, !!sym(.), vec.tau)) %&gt;%
  set_names(vec.betas)</code></pre>
<pre class="r"><code>tab.persistence &lt;- lst.persistence %&gt;% 
  reduce(left_join) %&gt;%
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;))
tab.persistence</code></pre>
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
0.92
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.82
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
0.78
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.48
</td>
<td style="text-align:right;">
0.79
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
0.58
</td>
<td style="text-align:right;">
0.82
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
0.60
</td>
<td style="text-align:right;">
0.84
</td>
<td style="text-align:right;">
0.93
</td>
</tr>
<tr>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.80
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.55
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
0.22
</td>
<td style="text-align:right;">
0.37
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.80
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.37
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.60
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
0.20
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.44
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
0.55
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
0.19
</td>
<td style="text-align:right;">
0.32
</td>
<td style="text-align:right;">
0.41
</td>
<td style="text-align:right;">
0.60
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.52
</td>
<td style="text-align:right;">
0.60
</td>
</tr>
<tr>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.39
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.40
</td>
<td style="text-align:right;">
0.49
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
0.14
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.32
</td>
<td style="text-align:right;">
0.39
</td>
<td style="text-align:right;">
0.44
</td>
</tr>
</tbody>
</table>
<p>Of course, entries that correspond to lags for which the measurement periods overlap mechanically exhibit high persistence. The results indicate that the calculation of beta using short measurement periods is very noisy. Moreover, as the length of the measurement window increases, so does the persistence of beta calculated from daily return data. The measures based on monthly return data exhibit similar persistence patterns. In summary, the results indicate that the use of longer measurement periods results in more accurate measures of beta and that beta seems to be more accurately measured using daily return data.</p>
</div>
<div id="portfolio-analysis" class="section level1">
<h1>Portfolio Analysis</h1>
<p>Next, I turn to the relation of beta and stock returns. The fundamental prediction of the CAPM is that there is a positive relation between market beta and expected stock returns where the slope of this relation is the market risk premium. I should hence expect to find a positive cross-sectional association between beta and future excess returns. However, as I show below, contrary to this prediction, analyses fail to detect any strong relation or even exhibit a negative relation. This result is typically viewed as one of the most persistent empirical anomalies in all of empirical asset pricing.</p>
<p>The construction of univariate portfolios relies on a couple of helper functions. First, I need to introduce a wrapper for the basic weighted mean function to also exclude observations with missing weights.</p>
<pre class="r"><code>fun.weighted_mean &lt;- function(x, w, ..., na.rm = FALSE){
  if(na.rm){
    x1 &lt;- x[!is.na(x) &amp; !is.na(w)]
    w &lt;- w[!is.na(x) &amp; !is.na(w)]
    x &lt;- x1
  }
  weighted.mean(x, w, ..., na.rm = FALSE)
}</code></pre>
<p>The next function I introduce is a helper function that returns a list of quantile functions that I can throw into the summarize command to get a list of breakpoints for any given number of target portfolios. The code snippet below is a modified version of the code found <a href="https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/">here</a>. Most importantly, the function uses <code>purrr:partial</code> to fill in some arguments to the quantile functions.</p>
<pre class="r"><code>fun.get_breakpoints_functions &lt;- function(var, n_portfolios = 10) {
  # Get relevant percentiles
  percentiles &lt;- seq(0, 1, length.out = (n_portfolios + 1))
  percentiles &lt;- percentiles[percentiles &gt; 0 &amp; percentiles &lt; 1]
  
  # Construct set of named quantile functions
  percentiles_names &lt;- map_chr(percentiles, ~str_c(rlang::quo_text(enquo(var)), &quot;_q&quot;, .x*100))
  percentiles_funs &lt;- map(percentiles, ~partial(quantile, probs = .x, na.rm = TRUE)) %&gt;% 
    set_names(nm = percentiles_names)
  
  return(percentiles_funs)
}</code></pre>
<p>Given a list of breakpoints, I want to sort stocks into the corresponding portfolio. This turns out to be a bit tricky if I want to write a function that does the sorting for a flexible number of portfolios. Fortunately, my brilliant colleague Stefan Voigt pointed out the <code>findInterval</code> function to me.</p>
<pre class="r"><code>fun.get_portfolio &lt;- function(x, breakpoints) {
  portfolio &lt;- as.integer(1 + findInterval(x, unlist(breakpoints)))
  
  return(portfolio)
}</code></pre>
<p>The next function finally constructs univariate portfolios for any sorting variable, an abitrary number of portfolios and using all stocks or only NYSE stocks to compute breakpoints. The rationale behind using only NYSE stocks to calculate quantiles is that for a large portion of the sample period, these stocks tend to be much larger than stocks listed on AMEX or NASDAQ. If the breakpoints are calculated using all stocks in the CRSP sample, the breakpoints would effectively just separate NYSE stocks from all other stocks. However, it does not ensure an equal number of NYSE stocks in each portfolios, as I demonstrate in later posts.</p>
<p>Followin <a href="https://www.wiley.com/en-us/Empirical+Asset+Pricing%3A+The+Cross+Section+of+Stock+Returns-p-9781118095041">BEM</a>, I sort stocks into 10 portfolios ranked by the value of the beta estimate. The breakpoints for each portfolio are based on deciles constructed using all stocks. In addition to the 10 decile portfolios, the function below also adds the 10-1 portfolio. The function also computes a set of average stock characteristics for each portfolio, which becomes relevant in the other blog posts.</p>
<pre class="r"><code>fun.construct_univariate_portfolios &lt;- function(data, var, n_portfolios = 10, nyse_breakpoints = FALSE) {
  # Keep only observations where sorting variable is defined
  data &lt;- data %&gt;%
    filter(!is.na({{var}}))
  
  # Determine breakpoints based on NYSE stocks only or on all stocks
  if (nyse_breakpoints == TRUE) {
    data_quantiles &lt;- data %&gt;%
      filter(exchange == &quot;NYSE&quot;) %&gt;%
      select(month, {{var}})
  } else {
    data_quantiles &lt;- data %&gt;%
      select(month, {{var}}) 
  }
  
  # Compute quantiles
  var_funs &lt;- fun.get_breakpoints_functions({{var}}, n_portfolios)
  quantiles &lt;- data_quantiles %&gt;%
    group_by(month) %&gt;%
    summarize_at(vars({{var}}), lst(!!!var_funs)) %&gt;%
    group_by(month) %&gt;%
    nest(quantiles = -month) 
  
  # Independently sort all stocks into portfolios based on breakpoints
  portfolios &lt;- data %&gt;%
    left_join(quantiles, by = &quot;month&quot;) %&gt;%
    mutate(portfolio = map2_dbl({{var}}, quantiles, fun.get_portfolio)) %&gt;%
    select(-quantiles)
  
  # Compute average portfolio characteristics
  portfolios_ts &lt;- portfolios %&gt;%
    group_by(portfolio, month) %&gt;%
    summarize(ret_ew = mean(ret_adj_excess_f1, na.rm = TRUE),
              ret_vw = fun.weighted_mean(ret_adj_excess_f1, mktcap, na.rm = TRUE),
              ret_mkt = mean(mkt_ff_excess_f1, na.rm = TRUE),
              {{var}} := mean({{var}}, na.rm = TRUE),
              mktcap = mean(mktcap, na.rm = TRUE),
              mktcap_cpi_all = mean(mktcap_cpi, na.rm = TRUE),
              mktcap_cpi_nyse = mean(mktcap_cpi[exchange == &quot;NYSE&quot;], na.rm = TRUE),
              n_stocks = n(),
              n_stocks_nyse = sum(exchange == &quot;NYSE&quot;, na.rm = TRUE)) %&gt;%
    ungroup()
  
  # Construct long-short portfolio
  portfolios_ts_ls &lt;- portfolios_ts %&gt;%
    select(portfolio, month, ret_ew, ret_vw, ret_mkt) %&gt;%
    filter(portfolio %in% c(max(portfolio), min(portfolio))) %&gt;%
    pivot_wider(names_from = portfolio, values_from = c(ret_ew, ret_vw)) %&gt;%
    mutate(ret_ew = .[[4]] - .[[3]],
           ret_vw = .[[6]] - .[[5]],
           portfolio = paste0(n_portfolios, &quot;-1&quot;)) %&gt;%
    select(portfolio, month, ret_ew, ret_vw, ret_mkt)
  
  # Combine everything
  out &lt;- portfolios_ts %&gt;%
    mutate(portfolio = as.character(portfolio)) %&gt;%
    bind_rows(portfolios_ts_ls) %&gt;%
    mutate(portfolio = factor(portfolio, levels = c(as.character(seq(1, n_portfolios, 1)), str_c(n_portfolios, &quot;-1&quot;))))
  
  return(out)
}

lst.portfolios &lt;- vec.betas %&gt;%
  map(~fun.construct_univariate_portfolios(tbl.betas, !!sym(.))) %&gt;%
  set_names(vec.betas)</code></pre>
<p>Given the portfolio returns, I want to evaluate whether the portfolio exhibit on average positive or negative excess returns. The following function estimates the mean excess return and CAPM alpha for each portfolio and computes corresponding <a href="https://www.jstor.org/stable/1913610?seq=1">Newey and West (1987)</a> <span class="math inline">\(t\)</span>-statistics (using six lags as common in the literature) testing the null hypothesis that the average portfolio excess return or CAPM alpha is zero.</p>
<pre class="r"><code>fun.estimate_portfolio_returns &lt;- function(data, ret, n_portfolios = 10, lag = 6) {
  # Compute average returns per portfolio
  average_ret &lt;- data %&gt;%
    select(portfolio, month, ret = {{ret}}) %&gt;% 
    group_by(portfolio) %&gt;%
    nest(data = c(month, ret)) %&gt;% 
    mutate(model = map(data, ~lm(&quot;ret ~ 1&quot;, data = .)),
           nw_stderror = map_dbl(model, ~sqrt(diag(sandwich::NeweyWest(., lag = lag)))),
           model = map(model, tidy)) %&gt;% 
    unnest(model) %&gt;%
    ungroup() %&gt;% 
    mutate(nw_tstat = estimate / nw_stderror) %&gt;%
    select(estimate, nw_tstat) %&gt;%
    t()
  
  # Estimate capm alpha per portfolio
  average_capm_alpha &lt;- data %&gt;%
    select(portfolio, month, ret = {{ret}}, ret_mkt) %&gt;% 
    group_by(portfolio) %&gt;%
    nest(data = c(month, ret, ret_mkt)) %&gt;% 
    mutate(model = map(data, ~lm(&quot;ret ~ 1 + ret_mkt&quot;, data = .)),
           nw_stderror = map_dbl(model, ~sqrt(diag(sandwich::NeweyWest(., lag = lag))[1])),
           model = map(model, tidy)) %&gt;% 
    unnest(model) %&gt;%
    ungroup() %&gt;% 
    filter(term == &quot;(Intercept)&quot;) %&gt;% 
    mutate(nw_tstat = estimate / nw_stderror) %&gt;%
    select(estimate, nw_tstat) %&gt;%
    t()
  
  # Construct output table
  out &lt;- rbind(average_ret, average_capm_alpha)
  colnames(out) &lt;-c(as.character(seq(1, n_portfolios, 1)), str_c(n_portfolios, &quot;-1&quot;))
  rownames(out) &lt;- c(&quot;Excess Return&quot;, &quot;t-Stat&quot;, &quot;CAPM Alpha&quot;, &quot;t-Stat&quot;)
  
  return(out)
}</code></pre>
<p>Let us first apply the function to equal-weighted porfolios.</p>
<pre class="r"><code>tab.portfolio_returns_ew &lt;- lst.portfolios %&gt;% 
  map(~fun.estimate_portfolio_returns(., ret_ew)) %&gt;% 
  reduce(rbind) %&gt;%
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;)) %&gt;%
  pack_rows(&quot;Estimation Period: 1 Year (Monthly Data) - Equal-Weighted Portfolio Returns&quot;, 1, 4) %&gt;%
  pack_rows(&quot;Estimation Period: 2 Years (Monthly Data) - Equal-Weighted Portfolio Returns&quot;, 5, 8) %&gt;%
  pack_rows(&quot;Estimation Period: 3 Years (Monthly Data) - Equal-Weighted Portfolio Returns&quot;, 9, 12) %&gt;%
  pack_rows(&quot;Estimation Period: 5 Years (Monthly Data) - Equal-Weighted Portfolio Returns&quot;, 13, 16) %&gt;%
  pack_rows(&quot;Estimation Period: 1 Month (Daily Data) - Equal-Weighted Portfolio Returns&quot;, 17, 20) %&gt;%
  pack_rows(&quot;Estimation Period: 3 Months (Daily Data) - Equal-Weighted Portfolio Returns&quot;, 21, 24) %&gt;%
  pack_rows(&quot;Estimation Period: 6 Months (Daily Data) - Equal-Weighted Portfolio Returns&quot;, 25, 28) %&gt;%
  pack_rows(&quot;Estimation Period: 12 Months (Daily Data) - Equal-Weighted Portfolio Returns&quot;, 29, 32) %&gt;%
  pack_rows(&quot;Estimation Period: 24 Months (Daily Data) - Equal-Weighted Portfolio Returns&quot;, 33, 36)
tab.portfolio_returns_ew</code></pre>
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
0.73
</td>
<td style="text-align:right;">
0.80
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
-0.03
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
3.08
</td>
<td style="text-align:right;">
3.32
</td>
<td style="text-align:right;">
3.63
</td>
<td style="text-align:right;">
3.65
</td>
<td style="text-align:right;">
3.28
</td>
<td style="text-align:right;">
3.45
</td>
<td style="text-align:right;">
3.32
</td>
<td style="text-align:right;">
2.93
</td>
<td style="text-align:right;">
2.24
</td>
<td style="text-align:right;">
-0.18
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
0.15
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.32
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
1.07
</td>
<td style="text-align:right;">
1.13
</td>
<td style="text-align:right;">
1.52
</td>
<td style="text-align:right;">
1.34
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
-0.62
</td>
<td style="text-align:right;">
-1.57
</td>
<td style="text-align:right;">
-3.63
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
0.77
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.84
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
3.19
</td>
<td style="text-align:right;">
3.27
</td>
<td style="text-align:right;">
3.33
</td>
<td style="text-align:right;">
3.68
</td>
<td style="text-align:right;">
3.47
</td>
<td style="text-align:right;">
3.39
</td>
<td style="text-align:right;">
3.64
</td>
<td style="text-align:right;">
3.15
</td>
<td style="text-align:right;">
2.78
</td>
<td style="text-align:right;">
2.25
</td>
<td style="text-align:right;">
0.35
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
0.24
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
-0.07
</td>
<td style="text-align:right;">
-0.25
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
1.88
</td>
<td style="text-align:right;">
1.83
</td>
<td style="text-align:right;">
1.60
</td>
<td style="text-align:right;">
1.82
</td>
<td style="text-align:right;">
1.42
</td>
<td style="text-align:right;">
1.27
</td>
<td style="text-align:right;">
1.29
</td>
<td style="text-align:right;">
0.32
</td>
<td style="text-align:right;">
-0.43
</td>
<td style="text-align:right;">
-1.19
</td>
<td style="text-align:right;">
-3.15
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
0.81
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
1.01
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
1.10
</td>
<td style="text-align:right;">
1.09
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
0.97
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
3.57
</td>
<td style="text-align:right;">
3.89
</td>
<td style="text-align:right;">
4.01
</td>
<td style="text-align:right;">
3.99
</td>
<td style="text-align:right;">
3.83
</td>
<td style="text-align:right;">
3.72
</td>
<td style="text-align:right;">
3.88
</td>
<td style="text-align:right;">
3.47
</td>
<td style="text-align:right;">
3.07
</td>
<td style="text-align:right;">
2.60
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
0.33
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
-0.06
</td>
<td style="text-align:right;">
-0.23
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
2.14
</td>
<td style="text-align:right;">
2.45
</td>
<td style="text-align:right;">
2.21
</td>
<td style="text-align:right;">
2.05
</td>
<td style="text-align:right;">
1.58
</td>
<td style="text-align:right;">
1.29
</td>
<td style="text-align:right;">
1.20
</td>
<td style="text-align:right;">
0.48
</td>
<td style="text-align:right;">
-0.32
</td>
<td style="text-align:right;">
-1.09
</td>
<td style="text-align:right;">
-3.13
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
0.78
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
1.13
</td>
<td style="text-align:right;">
1.03
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.15
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
4.41
</td>
<td style="text-align:right;">
4.20
</td>
<td style="text-align:right;">
4.30
</td>
<td style="text-align:right;">
4.20
</td>
<td style="text-align:right;">
4.14
</td>
<td style="text-align:right;">
3.79
</td>
<td style="text-align:right;">
3.54
</td>
<td style="text-align:right;">
3.20
</td>
<td style="text-align:right;">
2.57
</td>
<td style="text-align:right;">
0.67
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
-0.01
</td>
<td style="text-align:right;">
-0.13
</td>
<td style="text-align:right;">
-0.36
</td>
<td style="text-align:right;">
-0.65
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.02
</td>
<td style="text-align:right;">
2.51
</td>
<td style="text-align:right;">
2.08
</td>
<td style="text-align:right;">
1.63
</td>
<td style="text-align:right;">
1.61
</td>
<td style="text-align:right;">
1.44
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
-0.77
</td>
<td style="text-align:right;">
-1.86
</td>
<td style="text-align:right;">
-3.94
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
1.04
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
1.02
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
0.96
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
-0.30
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.52
</td>
<td style="text-align:right;">
4.31
</td>
<td style="text-align:right;">
4.38
</td>
<td style="text-align:right;">
4.73
</td>
<td style="text-align:right;">
4.34
</td>
<td style="text-align:right;">
4.19
</td>
<td style="text-align:right;">
3.97
</td>
<td style="text-align:right;">
3.63
</td>
<td style="text-align:right;">
3.11
</td>
<td style="text-align:right;">
2.14
</td>
<td style="text-align:right;">
-2.17
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
0.31
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.38
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
1.90
</td>
<td style="text-align:right;">
3.25
</td>
<td style="text-align:right;">
3.39
</td>
<td style="text-align:right;">
3.98
</td>
<td style="text-align:right;">
2.80
</td>
<td style="text-align:right;">
2.46
</td>
<td style="text-align:right;">
1.58
</td>
<td style="text-align:right;">
0.48
</td>
<td style="text-align:right;">
-0.86
</td>
<td style="text-align:right;">
-2.34
</td>
<td style="text-align:right;">
-5.40
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
1.06
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
1.03
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
-0.35
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.74
</td>
<td style="text-align:right;">
4.52
</td>
<td style="text-align:right;">
4.64
</td>
<td style="text-align:right;">
4.37
</td>
<td style="text-align:right;">
4.52
</td>
<td style="text-align:right;">
4.04
</td>
<td style="text-align:right;">
3.92
</td>
<td style="text-align:right;">
3.48
</td>
<td style="text-align:right;">
3.01
</td>
<td style="text-align:right;">
1.99
</td>
<td style="text-align:right;">
-2.07
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.39
</td>
<td style="text-align:right;">
0.39
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
0.26
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
0.01
</td>
<td style="text-align:right;">
-0.14
</td>
<td style="text-align:right;">
-0.48
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
2.71
</td>
<td style="text-align:right;">
3.88
</td>
<td style="text-align:right;">
4.10
</td>
<td style="text-align:right;">
2.90
</td>
<td style="text-align:right;">
3.35
</td>
<td style="text-align:right;">
2.03
</td>
<td style="text-align:right;">
1.45
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
-1.20
</td>
<td style="text-align:right;">
-2.91
</td>
<td style="text-align:right;">
-5.85
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
1.13
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.73
</td>
<td style="text-align:right;">
-0.40
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
4.20
</td>
<td style="text-align:right;">
4.61
</td>
<td style="text-align:right;">
4.45
</td>
<td style="text-align:right;">
4.54
</td>
<td style="text-align:right;">
4.49
</td>
<td style="text-align:right;">
4.16
</td>
<td style="text-align:right;">
3.73
</td>
<td style="text-align:right;">
3.38
</td>
<td style="text-align:right;">
2.92
</td>
<td style="text-align:right;">
2.07
</td>
<td style="text-align:right;">
-2.22
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.52
</td>
<td style="text-align:right;">
0.41
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
-0.17
</td>
<td style="text-align:right;">
-0.48
</td>
<td style="text-align:right;">
-1.00
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.74
</td>
<td style="text-align:right;">
4.20
</td>
<td style="text-align:right;">
3.58
</td>
<td style="text-align:right;">
3.31
</td>
<td style="text-align:right;">
3.15
</td>
<td style="text-align:right;">
2.14
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
-0.15
</td>
<td style="text-align:right;">
-1.47
</td>
<td style="text-align:right;">
-2.96
</td>
<td style="text-align:right;">
-6.46
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 12 Months (Daily Data) - Equal-Weighted Portfolio Returns</strong>
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
1.03
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
1.04
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.87
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
4.67
</td>
<td style="text-align:right;">
4.79
</td>
<td style="text-align:right;">
4.71
</td>
<td style="text-align:right;">
4.34
</td>
<td style="text-align:right;">
4.35
</td>
<td style="text-align:right;">
4.13
</td>
<td style="text-align:right;">
3.85
</td>
<td style="text-align:right;">
3.27
</td>
<td style="text-align:right;">
2.87
</td>
<td style="text-align:right;">
2.09
</td>
<td style="text-align:right;">
-2.15
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.60
</td>
<td style="text-align:right;">
0.47
</td>
<td style="text-align:right;">
0.40
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
-0.18
</td>
<td style="text-align:right;">
-0.48
</td>
<td style="text-align:right;">
-1.08
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
4.77
</td>
<td style="text-align:right;">
4.60
</td>
<td style="text-align:right;">
4.03
</td>
<td style="text-align:right;">
3.39
</td>
<td style="text-align:right;">
2.86
</td>
<td style="text-align:right;">
2.32
</td>
<td style="text-align:right;">
1.27
</td>
<td style="text-align:right;">
-0.21
</td>
<td style="text-align:right;">
-1.46
</td>
<td style="text-align:right;">
-3.15
</td>
<td style="text-align:right;">
-6.64
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
1.14
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
1.08
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
1.08
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
1.03
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.69
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
4.96
</td>
<td style="text-align:right;">
4.69
</td>
<td style="text-align:right;">
4.72
</td>
<td style="text-align:right;">
4.49
</td>
<td style="text-align:right;">
4.29
</td>
<td style="text-align:right;">
4.05
</td>
<td style="text-align:right;">
3.97
</td>
<td style="text-align:right;">
3.56
</td>
<td style="text-align:right;">
3.02
</td>
<td style="text-align:right;">
2.04
</td>
<td style="text-align:right;">
-2.17
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
0.45
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
0.32
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.48
</td>
<td style="text-align:right;">
-1.12
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
5.62
</td>
<td style="text-align:right;">
4.68
</td>
<td style="text-align:right;">
4.20
</td>
<td style="text-align:right;">
3.73
</td>
<td style="text-align:right;">
3.14
</td>
<td style="text-align:right;">
2.19
</td>
<td style="text-align:right;">
1.76
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
-0.83
</td>
<td style="text-align:right;">
-3.15
</td>
<td style="text-align:right;">
-6.60
</td>
</tr>
</tbody>
</table>
<p>There is neither a clear pattern across the portfolios in average excess returns, nor does the 10-1 portfolio yield statistically significant excess returns. However, the CAPM alphas exhibit a decreasing pattern across portfolios and a statistically significant negative coefficient on the 10-1 portfolio for all beta estimates. This result is actually not surprising since the risk-adjustment using the CAPM model adjusts returns for the sensitivity to the market factor which is the same factor used to calculate beta. Stocks in the highest decile hence mechanically exhibit a higher sensitivity than stocks in the low decile. As a result, the 10-1 portfolio has a high sensitivity to the market portfolio. Since the market factor generates positive returns in the long run and the 10-1 portfolio has a positive sensitivity, the effect of the risk-adjustment is negative.</p>
<p>Next, let us repeat the analysis using value-weighted returns for each portfolio.</p>
<pre class="r"><code>tab.portfolio_returns_vw &lt;- lst.portfolios %&gt;% 
  map(~fun.estimate_portfolio_returns(., ret_vw)) %&gt;% 
  reduce(rbind) %&gt;%
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;)) %&gt;%
  pack_rows(&quot;Estimation Period: 1 Year (Monthly Data) - Value-Weighted Portfolio Returns&quot;, 1, 4) %&gt;%
  pack_rows(&quot;Estimation Period: 2 Years (Monthly Data) - Value-Weighted Portfolio Returns&quot;, 5, 8) %&gt;%
  pack_rows(&quot;Estimation Period: 3 Years (Monthly Data) - Value-Weighted Portfolio Returns&quot;, 9, 12) %&gt;%
  pack_rows(&quot;Estimation Period: 5 Years (Monthly Data) - Value-Weighted Portfolio Returns&quot;, 13, 16) %&gt;%
  pack_rows(&quot;Estimation Period: 1 Month (Daily Data) - Value-Weighted Portfolio Returns&quot;, 17, 20) %&gt;%
  pack_rows(&quot;Estimation Period: 3 Months (Daily Data) - Value-Weighted Portfolio Returns&quot;, 21, 24) %&gt;%
  pack_rows(&quot;Estimation Period: 6 Months (Daily Data) - Value-Weighted Portfolio Returns&quot;, 25, 28) %&gt;%
  pack_rows(&quot;Estimation Period: 12 Months (Daily Data) - Value-Weighted Portfolio Returns&quot;, 29, 32) %&gt;%
  pack_rows(&quot;Estimation Period: 24 Months (Daily Data) - Value-Weighted Portfolio Returns&quot;, 33, 36)
tab.portfolio_returns_vw</code></pre>
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
0.36
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.30
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
2.40
</td>
<td style="text-align:right;">
3.04
</td>
<td style="text-align:right;">
3.59
</td>
<td style="text-align:right;">
3.52
</td>
<td style="text-align:right;">
3.14
</td>
<td style="text-align:right;">
2.84
</td>
<td style="text-align:right;">
2.73
</td>
<td style="text-align:right;">
2.74
</td>
<td style="text-align:right;">
2.06
</td>
<td style="text-align:right;">
1.36
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
-0.01
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
-0.07
</td>
<td style="text-align:right;">
-0.15
</td>
<td style="text-align:right;">
-0.22
</td>
<td style="text-align:right;">
-0.22
</td>
<td style="text-align:right;">
-0.45
</td>
<td style="text-align:right;">
-0.35
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
-0.80
</td>
<td style="text-align:right;">
-0.35
</td>
<td style="text-align:right;">
-0.06
</td>
<td style="text-align:right;">
0.55
</td>
<td style="text-align:right;">
0.39
</td>
<td style="text-align:right;">
-0.67
</td>
<td style="text-align:right;">
-1.42
</td>
<td style="text-align:right;">
-1.92
</td>
<td style="text-align:right;">
-1.64
</td>
<td style="text-align:right;">
-2.59
</td>
<td style="text-align:right;">
-1.86
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
0.43
</td>
<td style="text-align:right;">
0.43
</td>
<td style="text-align:right;">
0.56
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.23
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.40
</td>
<td style="text-align:right;">
2.42
</td>
<td style="text-align:right;">
3.11
</td>
<td style="text-align:right;">
3.77
</td>
<td style="text-align:right;">
3.03
</td>
<td style="text-align:right;">
3.16
</td>
<td style="text-align:right;">
2.89
</td>
<td style="text-align:right;">
2.50
</td>
<td style="text-align:right;">
2.40
</td>
<td style="text-align:right;">
1.98
</td>
<td style="text-align:right;">
0.92
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
-0.01
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
-0.12
</td>
<td style="text-align:right;">
-0.22
</td>
<td style="text-align:right;">
-0.27
</td>
<td style="text-align:right;">
-0.44
</td>
<td style="text-align:right;">
-0.49
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
-0.09
</td>
<td style="text-align:right;">
0.41
</td>
<td style="text-align:right;">
1.32
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
-0.18
</td>
<td style="text-align:right;">
-1.04
</td>
<td style="text-align:right;">
-1.86
</td>
<td style="text-align:right;">
-1.97
</td>
<td style="text-align:right;">
-2.69
</td>
<td style="text-align:right;">
-2.59
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
0.42
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.58
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
0.70
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.33
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
2.78
</td>
<td style="text-align:right;">
3.35
</td>
<td style="text-align:right;">
3.54
</td>
<td style="text-align:right;">
3.18
</td>
<td style="text-align:right;">
3.02
</td>
<td style="text-align:right;">
2.90
</td>
<td style="text-align:right;">
2.69
</td>
<td style="text-align:right;">
2.44
</td>
<td style="text-align:right;">
2.23
</td>
<td style="text-align:right;">
1.26
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
-0.12
</td>
<td style="text-align:right;">
-0.15
</td>
<td style="text-align:right;">
-0.24
</td>
<td style="text-align:right;">
-0.33
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
0.26
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.53
</td>
<td style="text-align:right;">
-0.38
</td>
<td style="text-align:right;">
-1.04
</td>
<td style="text-align:right;">
-1.23
</td>
<td style="text-align:right;">
-2.03
</td>
<td style="text-align:right;">
-2.44
</td>
<td style="text-align:right;">
-2.91
</td>
<td style="text-align:right;">
-2.69
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
0.52
</td>
<td style="text-align:right;">
0.59
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
0.78
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
3.21
</td>
<td style="text-align:right;">
3.57
</td>
<td style="text-align:right;">
4.27
</td>
<td style="text-align:right;">
3.52
</td>
<td style="text-align:right;">
3.67
</td>
<td style="text-align:right;">
3.30
</td>
<td style="text-align:right;">
3.14
</td>
<td style="text-align:right;">
3.13
</td>
<td style="text-align:right;">
2.34
</td>
<td style="text-align:right;">
2.49
</td>
<td style="text-align:right;">
1.03
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
-0.14
</td>
<td style="text-align:right;">
-0.22
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
-0.45
</td>
<td style="text-align:right;">
-0.47
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
1.00
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
1.04
</td>
<td style="text-align:right;">
-0.25
</td>
<td style="text-align:right;">
-0.17
</td>
<td style="text-align:right;">
-1.22
</td>
<td style="text-align:right;">
-1.80
</td>
<td style="text-align:right;">
-1.85
</td>
<td style="text-align:right;">
-3.31
</td>
<td style="text-align:right;">
-3.01
</td>
<td style="text-align:right;">
-3.30
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
0.58
</td>
<td style="text-align:right;">
0.56
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
0.70
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
-0.05
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
3.62
</td>
<td style="text-align:right;">
4.24
</td>
<td style="text-align:right;">
4.55
</td>
<td style="text-align:right;">
4.29
</td>
<td style="text-align:right;">
4.12
</td>
<td style="text-align:right;">
4.22
</td>
<td style="text-align:right;">
3.95
</td>
<td style="text-align:right;">
3.19
</td>
<td style="text-align:right;">
1.78
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
0.01
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
-0.15
</td>
<td style="text-align:right;">
-0.52
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
0.12
</td>
<td style="text-align:right;">
1.64
</td>
<td style="text-align:right;">
2.30
</td>
<td style="text-align:right;">
3.22
</td>
<td style="text-align:right;">
2.23
</td>
<td style="text-align:right;">
1.35
</td>
<td style="text-align:right;">
1.45
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
-2.19
</td>
<td style="text-align:right;">
-3.92
</td>
<td style="text-align:right;">
-3.22
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
0.47
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
0.60
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.55
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
2.31
</td>
<td style="text-align:right;">
4.35
</td>
<td style="text-align:right;">
4.17
</td>
<td style="text-align:right;">
4.46
</td>
<td style="text-align:right;">
4.42
</td>
<td style="text-align:right;">
4.25
</td>
<td style="text-align:right;">
4.17
</td>
<td style="text-align:right;">
3.87
</td>
<td style="text-align:right;">
2.89
</td>
<td style="text-align:right;">
1.84
</td>
<td style="text-align:right;">
0.37
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
0.22
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
-0.19
</td>
<td style="text-align:right;">
-0.53
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
0.07
</td>
<td style="text-align:right;">
2.94
</td>
<td style="text-align:right;">
2.38
</td>
<td style="text-align:right;">
2.36
</td>
<td style="text-align:right;">
2.36
</td>
<td style="text-align:right;">
1.95
</td>
<td style="text-align:right;">
1.56
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
-2.40
</td>
<td style="text-align:right;">
-4.18
</td>
<td style="text-align:right;">
-2.82
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
0.59
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.64
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
3.35
</td>
<td style="text-align:right;">
4.71
</td>
<td style="text-align:right;">
4.07
</td>
<td style="text-align:right;">
4.30
</td>
<td style="text-align:right;">
4.66
</td>
<td style="text-align:right;">
4.10
</td>
<td style="text-align:right;">
4.31
</td>
<td style="text-align:right;">
3.57
</td>
<td style="text-align:right;">
2.80
</td>
<td style="text-align:right;">
2.15
</td>
<td style="text-align:right;">
0.25
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
-0.22
</td>
<td style="text-align:right;">
-0.46
</td>
<td style="text-align:right;">
-0.63
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
1.58
</td>
<td style="text-align:right;">
3.37
</td>
<td style="text-align:right;">
2.13
</td>
<td style="text-align:right;">
2.22
</td>
<td style="text-align:right;">
2.76
</td>
<td style="text-align:right;">
1.25
</td>
<td style="text-align:right;">
1.38
</td>
<td style="text-align:right;">
-0.28
</td>
<td style="text-align:right;">
-2.75
</td>
<td style="text-align:right;">
-3.80
</td>
<td style="text-align:right;">
-3.53
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Estimation Period: 12 Months (Daily Data) - Value-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
0.80
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
-0.02
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
4.58
</td>
<td style="text-align:right;">
4.78
</td>
<td style="text-align:right;">
4.24
</td>
<td style="text-align:right;">
4.50
</td>
<td style="text-align:right;">
4.28
</td>
<td style="text-align:right;">
4.29
</td>
<td style="text-align:right;">
3.48
</td>
<td style="text-align:right;">
2.81
</td>
<td style="text-align:right;">
2.07
</td>
<td style="text-align:right;">
-0.10
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
-0.20
</td>
<td style="text-align:right;">
-0.47
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
2.69
</td>
<td style="text-align:right;">
3.09
</td>
<td style="text-align:right;">
3.50
</td>
<td style="text-align:right;">
2.22
</td>
<td style="text-align:right;">
2.35
</td>
<td style="text-align:right;">
2.30
</td>
<td style="text-align:right;">
1.86
</td>
<td style="text-align:right;">
-0.72
</td>
<td style="text-align:right;">
-2.53
</td>
<td style="text-align:right;">
-3.90
</td>
<td style="text-align:right;">
-4.19
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
0.69
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.58
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
4.69
</td>
<td style="text-align:right;">
4.49
</td>
<td style="text-align:right;">
4.42
</td>
<td style="text-align:right;">
4.34
</td>
<td style="text-align:right;">
4.29
</td>
<td style="text-align:right;">
4.24
</td>
<td style="text-align:right;">
3.60
</td>
<td style="text-align:right;">
2.57
</td>
<td style="text-align:right;">
1.97
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
0.21
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
-0.48
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
2.25
</td>
<td style="text-align:right;">
3.62
</td>
<td style="text-align:right;">
2.74
</td>
<td style="text-align:right;">
2.92
</td>
<td style="text-align:right;">
2.44
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
1.95
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
-3.07
</td>
<td style="text-align:right;">
-4.28
</td>
<td style="text-align:right;">
-4.04
</td>
</tr>
</tbody>
</table>
<p>The results show that the negative relation between beta and future stock returns detected using equal-weighted portfolios is weaker using value-weighted portfolios. Taken together, the portfolio analyses provide evidence that contradicts the predictions of the CAPM. According to the CAPM, returns should increase across the decile portfolios and risk-adjusted returns should be statistically indistinguishable from zero.</p>
</div>
<div id="regression-analysis" class="section level1">
<h1>Regression Analysis</h1>
<p>As a last step, I analyze the relation between beta and stock returns using <a href="https://www.jstor.org/stable/1831028?seq=1">Fama and MacBeth (1973)</a> regression analysis. Each month, I perform a cross-sectional regression of one-month-ahead excess stock returns on the given measure of beta. Time-series averages over these cross-sectional regressions then provide the desired results.</p>
<p>The next function performs Fama-MacBeth regressions for a given variable and returns the time-series average of monthly regression coefficients and corresponding <span class="math inline">\(t\)</span>-statistics.</p>
<pre class="r"><code>fun.fama_macbeth_regression &lt;- function(data, model, model_name = &quot;value&quot;) {
  # Prepare and winsorize data
  data_nested &lt;- data %&gt;%
    group_by(month) %&gt;% 
    nest(data = -month) 
  
  # Perform cross-sectional regressions for each month
  cross_sectional_regs &lt;- data_nested %&gt;%
    mutate(model = map(data, ~lm(model, data = .x)),
           tidy = map(model, tidy),
           glance = map(model, glance),
           n = map_int(model, nobs))
  
  # Extract average coefficient estimates
  fama_macbeth_coefs &lt;- cross_sectional_regs %&gt;%
    unnest(tidy) %&gt;%
    group_by(term) %&gt;%
    summarize(coefficient = mean(estimate))
  
  # Compute newey-west standard errors of average coefficient estimates
  newey_west_std_errors &lt;- cross_sectional_regs %&gt;%
    unnest(tidy) %&gt;%
    select(month, term, estimate) %&gt;% 
    group_by(term) %&gt;%
    arrange(month) %&gt;%
    nest(data_nw = c(month, estimate)) %&gt;% 
    mutate(nw_std_error = map_dbl(data_nw, function(x) sqrt(diag(NeweyWest(lm(estimate ~ 1, data = x), lag = 6))))) %&gt;%
    select(term, nw_std_error)
  
  # Put coefficient estimates and standard errors together and compute t-statistics
  fama_macbeth_coefs &lt;- fama_macbeth_coefs %&gt;%
    left_join(newey_west_std_errors, by = &quot;term&quot;) %&gt;%
    mutate(nw_t_stat = coefficient / nw_std_error) %&gt;%
    select(term, coefficient, nw_t_stat) %&gt;%
    pivot_longer(cols = c(coefficient, nw_t_stat), 
                 names_to = &quot;statistic&quot;, values_to = model_name) %&gt;%
    mutate(statistic = paste(term, statistic, sep = &quot; &quot;)) %&gt;%
    select(-term)

  # Extract average r-squared and average number of observations
  fama_macbeth_stats &lt;- cross_sectional_regs %&gt;% 
    unnest(c(glance, n)) %&gt;%
    ungroup() %&gt;%
    summarize(adj_r_squared = mean(adj.r.squared),
              n = mean(n)) %&gt;%
    pivot_longer(c(adj_r_squared, n), names_to = &quot;statistic&quot;, values_to = model_name)
  
  # Combine desired output and return results
  out &lt;- rbind(fama_macbeth_coefs, fama_macbeth_stats)
  return(out)
}</code></pre>
<p>Before I apply the function, I winsorize all beta measures at 0.05% on a monthly basis to mitigate the impact of outliers or estimation errors.</p>
<pre class="r"><code>tbl.betas_regression &lt;- tbl.betas %&gt;% 
  filter(!is.na(ret_adj_excess_f1)) %&gt;%
  group_by(month) %&gt;%
  mutate_at(vars(contains(&quot;beta&quot;)), ~fun.winsorize(., cut = 0.005)) %&gt;%
  ungroup()

lst.regressions &lt;- vec.betas %&gt;%
  map(function(x) {tbl.betas_regression %&gt;% 
      filter(!is.na(!!sym(x))) %&gt;% 
      fun.fama_macbeth_regression(str_c(&quot;ret_adj_excess_f1 ~ &quot;, x), x)}) %&gt;%
  set_names(vec.betas)</code></pre>
<p>The following table provides the regression results for each beta measure.</p>
<pre class="r"><code>tbl.regressions &lt;- lst.regressions %&gt;%
  map(~mutate(., statistic = str_replace_all(statistic, str_c(vec.betas, collapse = &quot;|&quot;), &quot;beta&quot;))) %&gt;% 
  reduce(full_join)
tbl.regressions$statistic &lt;- c(&quot;intercept&quot;, &quot;t-stat&quot;, &quot;beta&quot;, &quot;t-stat&quot;, &quot;adj. R-squared&quot;, &quot;n&quot;)

tab.regressions &lt;- tbl.regressions %&gt;%
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;))
tab.regressions</code></pre>
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
0.82
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
0.84
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
1.04
</td>
<td style="text-align:right;">
1.08
</td>
<td style="text-align:right;">
1.14
</td>
<td style="text-align:right;">
1.18
</td>
</tr>
<tr>
<td style="text-align:left;">
t-stat
</td>
<td style="text-align:right;">
3.39
</td>
<td style="text-align:right;">
3.59
</td>
<td style="text-align:right;">
4.16
</td>
<td style="text-align:right;">
4.48
</td>
<td style="text-align:right;">
4.26
</td>
<td style="text-align:right;">
4.60
</td>
<td style="text-align:right;">
4.91
</td>
<td style="text-align:right;">
5.30
</td>
<td style="text-align:right;">
5.59
</td>
</tr>
<tr>
<td style="text-align:left;">
beta
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
-0.06
</td>
<td style="text-align:right;">
-0.12
</td>
<td style="text-align:right;">
-0.15
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
0.69
</td>
<td style="text-align:right;">
0.73
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
-1.28
</td>
<td style="text-align:right;">
-1.61
</td>
<td style="text-align:right;">
-1.66
</td>
<td style="text-align:right;">
-2.02
</td>
<td style="text-align:right;">
-2.05
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
0.03
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
2944.37
</td>
<td style="text-align:right;">
2587.02
</td>
<td style="text-align:right;">
2232.09
</td>
<td style="text-align:right;">
1949.31
</td>
<td style="text-align:right;">
3131.99
</td>
<td style="text-align:right;">
3094.10
</td>
<td style="text-align:right;">
3045.12
</td>
<td style="text-align:right;">
2948.86
</td>
<td style="text-align:right;">
2713.91
</td>
</tr>
</tbody>
</table>
<p>For all measures based on monthly data, I do not find any statistically significant relation between beta and stock returns, while the measures based on daily returns do exhibit a statistically significant relation for longer estimation periods. Moreover, the intercepts are positive and statistically significant across all beta measures, although, controlling for the effect of beta, the average excess stock returns should be zero according to the CAPM. The results thus provide no evidence in support of the CAPM.</p>
<p>It is worth noting that the average adjusted R-squared ranges only from 2 to 3 percent. Low values of R-squared are common in empirical asset pricing studies. The main reason put forward in the literature is that predicting stock returns is that realized stock returns are just a very noisy measure for expected stock returns (e.g., <a href="https://onlinelibrary.wiley.com/doi/abs/10.1111/0022-1082.00144">Elton, 1999</a>).</p>
<p>In all future blog posts, I use the 12-month beta estimate based on daily data just as<a href="https://www.wiley.com/en-us/Empirical+Asset+Pricing%3A+The+Cross+Section+of+Stock+Returns-p-9781118095041">BEM</a> do. The figure below shows the cumulative log returns of the corresponding value-weighted beta portfolios. It clearly demonstrates that high-beta stocks underperform low-beta stocks over the long-run, at least in the CRSP data.</p>
<pre class="r"><code>fig.cum_returns &lt;- lst.portfolios$beta_12m %&gt;%
  group_by(portfolio) %&gt;%
  arrange(month) %&gt;%
  mutate(cum_return = cumsum(log(1+ret_vw/100))) %&gt;%
  ungroup() %&gt;%
  ggplot(aes(x = month, y = cum_return, color = portfolio, linetype = portfolio)) +
  geom_line() +
  labs(x = NULL, y = NULL, color = NULL, linetype = NULL,
       title = &quot;Cumulative log returns of value-weighted beta portfolios&quot;) + 
  scale_x_date(expand = c(0, 0), date_breaks = &quot;10 years&quot;, date_labels = &quot;%Y&quot;) +
  scale_y_continuous(labels = percent, breaks = pretty_breaks()) + 
  theme_classic() +
  scale_color_viridis_d()</code></pre>
<center>
<img src="data:image/png;base64,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" width="768" />
</center>
</dl>
