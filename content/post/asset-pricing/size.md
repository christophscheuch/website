---
title: 'Tidy Asset Pricing - Part III: Size and Stock Returns'
subtitle: 
summary: 'On the relation of firm size and future stock returns'
authors:
- admin
tags:
- Asset Pricing
categories:
- Asset Pricing
date: "2020-02-21T00:00:00Z"
lastmod: "2020-02-21T00:00:00Z"
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
<p>In this note, I explore the relation between firm size and stock returns following the methodology outlined in <a href="https://www.wiley.com/en-us/Empirical+Asset+Pricing%3A+The+Cross+Section+of+Stock+Returns-p-9781118095041">Bali, Engle and Murray</a>. In two earlier notes, I first <a href="https://christophscheuch.github.io/post/asset-pricing/crsp-sample/">prepared the CRSP sample</a> in R and then <a href="https://christophscheuch.github.io/post/asset-pricing/beta/">estimated and analyzed market betas</a> using the same data. I do not go into the theoretical foundations of the size effect, but rather focus on its estimation in a tidy manner. I just want to mention that the size effect refers to the observation that stocks with large market capitalization tend to have lower returns than stocks with small market capitalizations (e.g., <a href="https://onlinelibrary.wiley.com/doi/full/10.1111/j.1540-6261.1992.tb04398.x">Fama and French, 1992</a>). The usual disclaimer applies, so the text below references an opinion and is for information purposes only and I do not intend to provide any investment advice. The code below replicates most of the results of Bali et al. up to a few basis points if I restrict my data to their sample period. If you spot any mistakes or want to share any suggestions for better implementation, just drop a comment below.</p>
<p>I mainly use the following packages throughout this note:</p>
<pre class="r"><code>library(tidyverse)
library(lubridate)  # for working with dates
library(broom)      # for tidying up estimation results
library(kableExtra) # for nicer html tables</code></pre>
<div id="calculating-firm-size" class="section level2">
<h2>Calculating Firm Size</h2>
<p>First, I load the data that I prepared earlier and add the market betas. I follow Bali et al. and use the betas estimated using daily return data and a twelve month estimation window as the measure for a stock’s covariance with the market.</p>
<pre class="r"><code>crsp &lt;- read_rds(&quot;data/crsp.rds&quot;)
betas &lt;- read_rds(&quot;data/betas_daily.rds&quot;)

crsp &lt;- crsp %&gt;%
  left_join(betas %&gt;%
              select(permno, date_start, beta = beta_12m), 
            by = c(&quot;permno&quot;, &quot;date_start&quot;)) # note: date_start refers to beginning of month</code></pre>
<p>Therer are two approaches to measure the market capitalization of a stock. The simplest approach is to take the share price and number of shares outstanding as of the end of each month for which the market capitalization is measured. This is the approach I used in the sample construction post to define the variable <code>mktcap</code>. Alternatively, Fama and French calculate market capitalization as of the last trading day of June in each year and hold the value constant for the months from June of the same year until May of the next year. The benefit of this approach is that the market capitalization measure does not vary with short-term movements in the stock price which may cause unwanted correlation with the stock returns. I implement the Fama-French approach below by defining a reference date for each month and then adding the June market cap as an additional column.</p>
<pre class="r"><code>crsp &lt;- crsp %&gt;%
  mutate(year = year(date),
         month = month(date),
         reference_date = if_else(month &lt; 6, 
                                  paste(year-1, 6, 30, sep = &quot;-&quot;), 
                                  paste(year, 6, 30, sep = &quot;-&quot;))) 

crsp_mktcap_ff &lt;- crsp %&gt;%
  filter(month == 6) %&gt;%
  select(permno, reference_date, mktcap_ff = mktcap)

crsp &lt;- crsp %&gt;%
  left_join(crsp_mktcap_ff, by = c(&quot;permno&quot;, &quot;reference_date&quot;))</code></pre>
<p>As it turns out, which approach I use has little impact on the results of the empirical analyses as the measures are highly correlated. While the timing of the calculation has a small effect, the distribution of measures might have a profound impact. As I show below, the distribution of market capitalization is highly skewed with a small number of stocks whose market capitalization is very large. For regression analyses, I hence use the log of market capitalization which I denote as size. For more meaningful interpretation of the market capitalization measures, I also calculate inflation-adjusted values (in terms of end of 2018 dollars).</p>
<pre class="r"><code>crsp &lt;- crsp %&gt;%
  mutate(mktcap_cpi = mktcap / cpi,
         mktcap_ff_cpi = mktcap_ff / cpi,
         size = log(mktcap),
         size_cpi = log(mktcap_cpi),
         size_ff = log(mktcap_ff),
         size_ff_cpi = log(mktcap_ff_cpi))</code></pre>
</div>
<div id="summary-statistics" class="section level2">
<h2>Summary Statistics</h2>
<p>I proceed to present summary statistics for the different measures of stock size using the full sample period from December 1925 until December 2018. I first compute corresponding summary statistics for each month and then average each summary statistics across all months. Note that I call the <code>moments</code> package to compute skewness and kurtosis.</p>
<pre class="r"><code># compute summary statistics for each month
crsp_ts &lt;- crsp %&gt;% 
  select(date, mktcap, mktcap_ff:size_ff_cpi) %&gt;%
  pivot_longer(cols = mktcap:size_ff_cpi, names_to = &quot;measure&quot;) %&gt;%
  na.omit() %&gt;%
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

# average summary statistics across all months
crsp_ts %&gt;%
  select(-date) %&gt;%
  group_by(measure) %&gt;%
  summarize_all(list(mean)) %&gt;% 
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
mktcap
</td>
<td style="text-align:right;">
1099.38
</td>
<td style="text-align:right;">
5186.43
</td>
<td style="text-align:right;">
14.57
</td>
<td style="text-align:right;">
323.08
</td>
<td style="text-align:right;">
0.52
</td>
<td style="text-align:right;">
5.76
</td>
<td style="text-align:right;">
29.35
</td>
<td style="text-align:right;">
114.91
</td>
<td style="text-align:right;">
476.70
</td>
<td style="text-align:right;">
4169.19
</td>
<td style="text-align:right;">
140802.80
</td>
<td style="text-align:right;">
3164.76
</td>
</tr>
<tr>
<td style="text-align:left;">
mktcap_cpi
</td>
<td style="text-align:right;">
1974.27
</td>
<td style="text-align:right;">
8993.83
</td>
<td style="text-align:right;">
14.56
</td>
<td style="text-align:right;">
323.01
</td>
<td style="text-align:right;">
2.86
</td>
<td style="text-align:right;">
20.31
</td>
<td style="text-align:right;">
82.73
</td>
<td style="text-align:right;">
267.93
</td>
<td style="text-align:right;">
982.34
</td>
<td style="text-align:right;">
7382.44
</td>
<td style="text-align:right;">
238170.44
</td>
<td style="text-align:right;">
3164.34
</td>
</tr>
<tr>
<td style="text-align:left;">
mktcap_ff
</td>
<td style="text-align:right;">
1092.84
</td>
<td style="text-align:right;">
5091.25
</td>
<td style="text-align:right;">
14.34
</td>
<td style="text-align:right;">
311.15
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
6.17
</td>
<td style="text-align:right;">
30.18
</td>
<td style="text-align:right;">
116.82
</td>
<td style="text-align:right;">
479.06
</td>
<td style="text-align:right;">
4170.68
</td>
<td style="text-align:right;">
135991.85
</td>
<td style="text-align:right;">
3057.32
</td>
</tr>
<tr>
<td style="text-align:left;">
mktcap_ff_cpi
</td>
<td style="text-align:right;">
1964.99
</td>
<td style="text-align:right;">
8857.12
</td>
<td style="text-align:right;">
14.34
</td>
<td style="text-align:right;">
311.16
</td>
<td style="text-align:right;">
3.35
</td>
<td style="text-align:right;">
20.99
</td>
<td style="text-align:right;">
83.71
</td>
<td style="text-align:right;">
270.10
</td>
<td style="text-align:right;">
983.78
</td>
<td style="text-align:right;">
7377.41
</td>
<td style="text-align:right;">
231163.32
</td>
<td style="text-align:right;">
3056.89
</td>
</tr>
<tr>
<td style="text-align:left;">
size
</td>
<td style="text-align:right;">
3.94
</td>
<td style="text-align:right;">
1.81
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
2.99
</td>
<td style="text-align:right;">
-1.34
</td>
<td style="text-align:right;">
1.18
</td>
<td style="text-align:right;">
2.66
</td>
<td style="text-align:right;">
3.82
</td>
<td style="text-align:right;">
5.12
</td>
<td style="text-align:right;">
7.10
</td>
<td style="text-align:right;">
10.40
</td>
<td style="text-align:right;">
3164.76
</td>
</tr>
<tr>
<td style="text-align:left;">
size_cpi
</td>
<td style="text-align:right;">
5.47
</td>
<td style="text-align:right;">
1.81
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
2.99
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
2.71
</td>
<td style="text-align:right;">
4.19
</td>
<td style="text-align:right;">
5.35
</td>
<td style="text-align:right;">
6.65
</td>
<td style="text-align:right;">
8.63
</td>
<td style="text-align:right;">
11.93
</td>
<td style="text-align:right;">
3164.34
</td>
</tr>
<tr>
<td style="text-align:left;">
size_ff
</td>
<td style="text-align:right;">
3.97
</td>
<td style="text-align:right;">
1.79
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
2.98
</td>
<td style="text-align:right;">
-1.02
</td>
<td style="text-align:right;">
1.24
</td>
<td style="text-align:right;">
2.69
</td>
<td style="text-align:right;">
3.84
</td>
<td style="text-align:right;">
5.13
</td>
<td style="text-align:right;">
7.11
</td>
<td style="text-align:right;">
10.39
</td>
<td style="text-align:right;">
3057.32
</td>
</tr>
<tr>
<td style="text-align:left;">
size_ff_cpi
</td>
<td style="text-align:right;">
5.49
</td>
<td style="text-align:right;">
1.79
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
2.98
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
2.77
</td>
<td style="text-align:right;">
4.21
</td>
<td style="text-align:right;">
5.37
</td>
<td style="text-align:right;">
6.65
</td>
<td style="text-align:right;">
8.63
</td>
<td style="text-align:right;">
11.91
</td>
<td style="text-align:right;">
3056.89
</td>
</tr>
</tbody>
</table>
<p>To further investigate the distribution of market capitalization, I examine the percentage of total market cap that comprised very large stocks. The figure below plots the percentage of total stock market capitalization that is captured by the larges <span class="math inline">\(x\)</span>% of stocks over the full sample period.</p>
<pre class="r"><code>crsp_top &lt;- crsp %&gt;%
  select(permno, date, mktcap) %&gt;%
  na.omit() %&gt;%
  group_by(date) %&gt;%
  mutate(top01 = if_else(mktcap &gt;= quantile(mktcap, 0.99), 1L, 0L),
         top05 = if_else(mktcap &gt;= quantile(mktcap, 0.95), 1L, 0L),
         top10 = if_else(mktcap &gt;= quantile(mktcap, 0.90), 1L, 0L),
         top25 = if_else(mktcap &gt;= quantile(mktcap, 0.75), 1L, 0L)) %&gt;%
  summarize(total = sum(mktcap),
            top01 = sum(mktcap[top01 == 1]) / total,
            top05 = sum(mktcap[top05 == 1]) / total,
            top10 = sum(mktcap[top10 == 1]) / total,
            top25 = sum(mktcap[top25 == 1]) / total) %&gt;%
  select(-total) %&gt;%
  pivot_longer(cols = top01:top25, names_to = &quot;group&quot;)

crsp_top %&gt;%
  mutate(group = case_when(group == &quot;top01&quot; ~ &quot;Largest 1% of Stocks&quot;,
                           group == &quot;top05&quot; ~ &quot;Largest 5% of Stocks&quot;,
                           group == &quot;top10&quot; ~ &quot;Largest 10% of Stocks&quot;,
                           group == &quot;top25&quot; ~ &quot;Largest 25% of Stocks&quot;),
         group = factor(group, levels = c(&quot;Largest 1% of Stocks&quot;, &quot;Largest 5% of Stocks&quot;, 
                                          &quot;Largest 10% of Stocks&quot;, &quot;Largest 25% of Stocks&quot;))) %&gt;%
  ggplot(aes(x = date, y = value, group = group)) +
  geom_line(aes(color = group, linetype = group)) +
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(), 
                     limits = c(0, 1)) + 
  labs(x = &quot;&quot;, y = &quot;Percentage of Total Market Capitalization&quot;,
       fill = &quot;&quot;, color = &quot;&quot;, linetype = &quot;&quot;) +
  scale_x_date(expand = c(0, 0), date_breaks = &quot;10 years&quot;, date_labels = &quot;%Y&quot;) +
  theme_classic()</code></pre>
<p><img src="data:image/png;base64,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" width="672" style="display: block; margin: auto;" /></p>
</div>
<div id="correlations" class="section level2">
<h2>Correlations</h2>
<p>The next table presents the time-series averages of the monthly cross-sectional (Pearson product-moment) correlations between different measures of market capitalization.</p>
<pre class="r"><code>cor_matrix &lt;- crsp %&gt;%
  select(mktcap, size, mktcap_ff, size_ff, beta) %&gt;%
  cor(use = &quot;complete.obs&quot;, method = &quot;pearson&quot;) 

cor_matrix[upper.tri(cor_matrix, diag = TRUE)] &lt;- NA

cor_matrix[-1, -5] %&gt;% 
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;))</code></pre>
<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
mktcap
</th>
<th style="text-align:right;">
size
</th>
<th style="text-align:right;">
mktcap_ff
</th>
<th style="text-align:right;">
size_ff
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
size
</td>
<td style="text-align:right;">
0.34
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
mktcap_ff
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
size_ff
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
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
0.28
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.28
</td>
</tr>
</tbody>
</table>
<p>Indeed, the different approaches are highly correlated, mostly because the total market capitalization of a firm in most cases changes very little over the course of a year (see next section). The correlation between beta and size indicates that larger stocks tend to have higher market betas.</p>
</div>
<div id="persistence" class="section level2">
<h2>Persistence</h2>
<p>Next, I turn to the persistence analysis, i.e. the correlation of each variable with its lagged values over time. To compute these correlations, I define the following function where I use the fairly new <a href="https://www.tidyverse.org/blog/2020/02/glue-strings-and-tidy-eval/">curly-curly operator</a> which simplifies writing functions around tidyverse pipelines.</p>
<pre class="r"><code>compute_persistence &lt;- function(var, tau) {
  dates &lt;- crsp %&gt;%
    distinct(date) %&gt;%
    arrange(date) %&gt;%
    mutate(date_lag = lag(date, tau))

  correlation &lt;- crsp %&gt;%
    select(permno, date, {{ var }}) %&gt;%
    left_join(dates, by = &quot;date&quot;) %&gt;%
    left_join(crsp %&gt;% 
                select(permno, date, {{ var }}) %&gt;%
                rename(&quot;{{ var }}_lag&quot; := {{ var }}),
              by = c(&quot;permno&quot;, &quot;date_lag&quot;=&quot;date&quot;)) %&gt;%
    select(contains(rlang::quo_text(enquo(var)))) %&gt;%
    cor(use = &quot;pairwise.complete.obs&quot;, method = &quot;pearson&quot;)

  return(correlation[1, 2])
}

persistence &lt;- tribble(~tau, ~size, ~size_ff,
                       1, compute_persistence(size, 1), compute_persistence(size_ff, 1),
                       3, compute_persistence(size, 3), compute_persistence(size_ff, 3),
                       6, compute_persistence(size, 6), compute_persistence(size_ff, 6),
                       12, compute_persistence(size, 12), compute_persistence(size_ff, 12),
                       24, compute_persistence(size, 24), compute_persistence(size_ff, 24),
                       36, compute_persistence(size, 36), compute_persistence(size_ff, 36),
                       48, compute_persistence(size, 48), compute_persistence(size_ff, 48),
                       60, compute_persistence(size, 60), compute_persistence(size_ff, 60),
                       120, compute_persistence(size, 120), compute_persistence(size_ff, 120))</code></pre>
<p>The following table provides the results of the persistence analysis. Indeed, the size measures exhibit a high persistence even up to 10 years. As the Fama-French measure is only updated every June, it mechanically exhibits a slightly higher persistence, but the impact is in fact minimal compared to the monthly updated variable.</p>
<pre class="r"><code>persistence %&gt;%
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;))</code></pre>
<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
tau
</th>
<th style="text-align:right;">
size
</th>
<th style="text-align:right;">
size_ff
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
</tr>
<tr>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.99
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
0.98
</td>
</tr>
<tr>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.97
</td>
</tr>
<tr>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.94
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.92
</td>
</tr>
<tr>
<td style="text-align:right;">
48
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.90
</td>
</tr>
<tr>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.89
</td>
</tr>
<tr>
<td style="text-align:right;">
120
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
0.84
</td>
</tr>
</tbody>
</table>
</div>
<div id="portfolio-analysis" class="section level2">
<h2>Portfolio Analysis</h2>
<p>To analyze the cross-sectional relation between market capitalization and future stock returns, I start with univariate portfolio analysis. The idea is to sort all stocks into portfolios based on quantiles calculated using NYSE stocks only and then calculate equal-weighted and value-weighted average returns for each portfolio.</p>
<p>Turns out that I need to adjust the basic <code>weighted.mean</code> function to take out observations with missing weights because it would <a href="https://stackoverflow.com/questions/40269022/weighted-average-in-r-using-na-weights">otherwise return missing values</a>. This is important whenever a stock’s return is defined while the market capitalization is not (e.g. due to missing number of shares outstanding).</p>
<pre class="r"><code>weighted_mean &lt;- function(x, w, ..., na.rm = FALSE){
  if(na.rm){
    x1 &lt;- x[!is.na(x) &amp; !is.na(w)]
    w &lt;- w[!is.na(x) &amp; !is.na(w)]
    x &lt;- x1
  }
  weighted.mean(x, w, ..., na.rm = FALSE)
}</code></pre>
<p>The next function I use is a helper function that returns a list of quantile functions that I can throw into the <code>summarize</code> command to get a list of breakpoints for any given number of portfolios. The code snippet below is a modified version of the code found <a href="https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/">here</a>. Most importantly, the function uses <code>purrr:partial</code> to fil in some arguments to the quantile functions. I apply the function to return breakpoints for 3 market cap portfolios as an example.</p>
<pre class="r"><code>get_breakpoints_funs &lt;- function(var, n_portfolios = 10) {
  # get relevant percentiles
  percentiles &lt;- seq(0, 1, length.out = (n_portfolios + 1))
  percentiles &lt;- percentiles[percentiles &gt; 0 &amp; percentiles &lt; 1]
  
  # construct set of named quantile functions
  percentiles_names &lt;- map_chr(percentiles, ~paste0(rlang::quo_text(enquo(var)), &quot;_q&quot;, .x*100))
  percentiles_funs &lt;- map(percentiles, ~partial(quantile, probs = .x, na.rm = TRUE)) %&gt;% 
    set_names(nm = percentiles_names)
  return(percentiles_funs)
}
get_breakpoints_funs(mktcap, n_portfolios = 3)</code></pre>
<pre><code>## $mktcap_q33.3333333333333
## &lt;partialised&gt;
## function (...) 
## quantile(probs = .x, na.rm = TRUE, ...)
## 
## $mktcap_q66.6666666666667
## &lt;partialised&gt;
## function (...) 
## quantile(probs = .x, na.rm = TRUE, ...)</code></pre>
<p>Given a list of breakpoints, I want to sort stocks into the corresponding portfolio. This turns out to be a bit tricky if I want to write a function that does the sorting for a flexible number of portfolios. Fortunately, my brilliant colleague <a href="http://www.voigtstefan.me/">Stefan Voigt</a> pointed out the <code>findInterval</code> function to me.</p>
<pre class="r"><code>get_portfolio &lt;- function(x, breakpoints) {
  portfolio &lt;- as.integer(1 + findInterval(x, unlist(breakpoints)))
  return(portfolio)
}</code></pre>
<p>The next function finally constructs univariate portfolios for any sorting variable using NYSE breakpoints. Note that the rationale behind using only NYSE stocks to calculate quantiles is that for a large portio of the sample period, NYSE stocks tended to be much larger than stocks listed on AMEX or NASDAQ. If the breakpoints were calculated using all stocks in the CRSP sample, the breakpoints would effectively just separate NYSE stocks from all other stocks. However, it does not ensure an equal number of NYSE stocks in each portfolios, as I show below. Again, I leverage the new curly-curly operator and associated naming capabilities.</p>
<pre class="r"><code>construct_univariate_portfolios &lt;- function(data, var, n_portfolios = 10) {
  # keep only observations where the sorting variable is defined
  data &lt;- data %&gt;%
    filter(!is.na({{ var }}))
  
  # determine breakpoints based on NYSE stocks only
  data_nyse &lt;- data %&gt;%
    filter(exchange == &quot;NYSE&quot;) %&gt;%
    select(date, {{ var }})

  var_funs &lt;- get_breakpoints_funs({{ var }}, n_portfolios)
  quantiles &lt;- data_nyse %&gt;%
    group_by(date) %&gt;%
    summarize_at(vars({{ var }}), lst(!!! var_funs)) %&gt;%
    group_by(date) %&gt;%
    nest() %&gt;%
    rename(quantiles = data)
  
  # independently sort all stocks into portfolios based on NYSE breakpoints
    portfolios &lt;- data %&gt;%
      left_join(quantiles, by = &quot;date&quot;) %&gt;%
      mutate(portfolio = map2_dbl({{ var }}, quantiles, get_portfolio)) %&gt;%
      select(-quantiles)
  
  # compute average portfolio characteristics
  portfolios_ts &lt;- portfolios %&gt;%
    group_by(portfolio, date) %&gt;%
    summarize(ret_ew = mean(ret_adj_excess_f1, na.rm = TRUE),
              ret_vw = weighted_mean(ret_adj_excess_f1, mktcap, na.rm = TRUE),
              ret_mkt = mean(mkt_ff_excess_f1, na.rm = TRUE),
              &quot;{{ var }}_mean&quot; := mean({{ var }}, na.rm = TRUE),
              &quot;{{ var }}_sum&quot; := sum({{ var }}, na.rm = TRUE),
              beta = mean(beta, na.rm = TRUE),
              n_stocks = n(),
              n_stocks_nyse = sum(exchange == &quot;NYSE&quot;, na.rm = TRUE)) %&gt;%
    na.omit() %&gt;%
    ungroup()
  
  ## long-short portfolio
  portfolios_ts_ls &lt;- portfolios_ts %&gt;%
     select(portfolio, date, ret_ew, ret_vw, ret_mkt) %&gt;%
    filter(portfolio %in% c(max(portfolio), min(portfolio))) %&gt;%
    pivot_wider(names_from = portfolio, values_from = c(ret_ew, ret_vw)) %&gt;%
    mutate(ret_ew = .[[4]] - .[[3]],
           ret_vw = .[[6]] - .[[5]],
           portfolio = paste0(n_portfolios, &quot;-1&quot;)) %&gt;%
    select(portfolio, date, ret_ew, ret_vw, ret_mkt)
  
  ## combine everything
  out &lt;- portfolios_ts %&gt;%
    mutate(portfolio = as.character(portfolio)) %&gt;%
    bind_rows(portfolios_ts_ls) %&gt;%
    mutate(portfolio = factor(portfolio, levels = c(as.character(seq(1, 10, 1)), paste0(n_portfolios, &quot;-1&quot;))))
  
  return(out)
}

portfolios_mktcap &lt;- construct_univariate_portfolios(crsp, mktcap)
portfolios_mktcap_ff &lt;- construct_univariate_portfolios(crsp, mktcap_ff)</code></pre>
<p>The table below provides the summary statistics for each portfolio.</p>
<pre class="r"><code># compute market cap totals
mktcap_totals &lt;- crsp %&gt;%
  group_by(date) %&gt;%
  summarize(mktcap_total = sum(mktcap, na.rm = TRUE),
            mktcap_ff_total = sum(mktcap_ff, na.rm = TRUE))

# portfolio characteristics
portfolios_mktcap_table &lt;- portfolios_mktcap %&gt;%
  filter(portfolio != &quot;10-1&quot;) %&gt;%
  left_join(mktcap_totals, by = &quot;date&quot;) %&gt;%
  group_by(portfolio) %&gt;%
  summarize(mktcap_mean = mean(mktcap_mean),
            mktcap_share = mean(mktcap_sum / mktcap_total) * 100,
            n = mean(n_stocks),
            n_nyse = mean(n_stocks_nyse / n_stocks) * 100,
            beta = mean(beta)) %&gt;%
  t()

portfolios_mktcap_table &lt;- portfolios_mktcap_table[-1, ] %&gt;%
  as.numeric() %&gt;%
  matrix(., ncol = 10)
colnames(portfolios_mktcap_table) &lt;- seq(1, 10, 1)
rownames(portfolios_mktcap_table) &lt;- c(&quot;Market Cap (in $B)&quot;, &quot;% of Total Market Cap&quot;, &quot;Number of Stocks&quot;, &quot;% NYSE Stocks&quot; , &quot;Beta&quot;)

# repeat for fama-french measure
portfolios_mktcap_ff_table &lt;- portfolios_mktcap_ff %&gt;%
  filter(portfolio != &quot;10-1&quot;) %&gt;%
  left_join(mktcap_totals, by = &quot;date&quot;) %&gt;%
  group_by(portfolio) %&gt;%
  summarize(mktcap_ff_mean = mean(mktcap_ff_mean),
            mktcap_ff_share = mean(mktcap_ff_sum / mktcap_ff_total) * 100,
            n = mean(n_stocks),
            n_nyse = mean(n_stocks_nyse / n_stocks) * 100,
            beta = mean(beta)) %&gt;%
  t()

portfolios_mktcap_ff_table &lt;- portfolios_mktcap_ff_table[-1, ] %&gt;%
  as.numeric() %&gt;%
  matrix(., ncol = 10)
colnames(portfolios_mktcap_ff_table) &lt;- seq(1, 10, 1)
rownames(portfolios_mktcap_ff_table) &lt;- c(&quot;Market Cap (in $B)&quot;, &quot;% of Total Market Cap&quot;, &quot;Number of Stocks&quot;, &quot;% NYSE Stocks&quot; , &quot;Beta&quot;)

# combine to table and print html
rbind(portfolios_mktcap_table, portfolios_mktcap_ff_table) %&gt;%
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;)) %&gt;%
  pack_rows(&quot;Market Cap (Turan-Engle-Murray)&quot;, 1, 5) %&gt;%
  pack_rows(&quot;Market Cap (Fama-French)&quot;, 6, 10)</code></pre>
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
</tr>
</thead>
<tbody>
<tr grouplength="5">
<td colspan="11" style="border-bottom: 1px solid;">
<strong>Market Cap (Turan-Engle-Murray)</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Market Cap (in $B)
</td>
<td style="text-align:right;">
25.43
</td>
<td style="text-align:right;">
100.24
</td>
<td style="text-align:right;">
181.84
</td>
<td style="text-align:right;">
288.66
</td>
<td style="text-align:right;">
439.11
</td>
<td style="text-align:right;">
656.88
</td>
<td style="text-align:right;">
1000.36
</td>
<td style="text-align:right;">
1678.64
</td>
<td style="text-align:right;">
3353.75
</td>
<td style="text-align:right;">
16646.88
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
% of Total Market Cap
</td>
<td style="text-align:right;">
1.13
</td>
<td style="text-align:right;">
1.16
</td>
<td style="text-align:right;">
1.44
</td>
<td style="text-align:right;">
1.89
</td>
<td style="text-align:right;">
2.51
</td>
<td style="text-align:right;">
3.33
</td>
<td style="text-align:right;">
4.77
</td>
<td style="text-align:right;">
7.52
</td>
<td style="text-align:right;">
13.76
</td>
<td style="text-align:right;">
62.48
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Number of Stocks
</td>
<td style="text-align:right;">
1417.84
</td>
<td style="text-align:right;">
387.86
</td>
<td style="text-align:right;">
266.60
</td>
<td style="text-align:right;">
217.65
</td>
<td style="text-align:right;">
186.92
</td>
<td style="text-align:right;">
162.65
</td>
<td style="text-align:right;">
150.42
</td>
<td style="text-align:right;">
141.72
</td>
<td style="text-align:right;">
134.15
</td>
<td style="text-align:right;">
129.71
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
% NYSE Stocks
</td>
<td style="text-align:right;">
43.28
</td>
<td style="text-align:right;">
56.31
</td>
<td style="text-align:right;">
64.16
</td>
<td style="text-align:right;">
69.63
</td>
<td style="text-align:right;">
74.92
</td>
<td style="text-align:right;">
81.14
</td>
<td style="text-align:right;">
85.10
</td>
<td style="text-align:right;">
88.52
</td>
<td style="text-align:right;">
92.13
</td>
<td style="text-align:right;">
94.54
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Beta
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.01
</td>
</tr>
<tr grouplength="5">
<td colspan="11" style="border-bottom: 1px solid;">
<strong>Market Cap (Fama-French)</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Market Cap (in $B)
</td>
<td style="text-align:right;">
26.30
</td>
<td style="text-align:right;">
102.10
</td>
<td style="text-align:right;">
182.39
</td>
<td style="text-align:right;">
288.65
</td>
<td style="text-align:right;">
435.70
</td>
<td style="text-align:right;">
648.76
</td>
<td style="text-align:right;">
985.41
</td>
<td style="text-align:right;">
1654.13
</td>
<td style="text-align:right;">
3306.93
</td>
<td style="text-align:right;">
16286.20
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
% of Total Market Cap
</td>
<td style="text-align:right;">
1.14
</td>
<td style="text-align:right;">
1.15
</td>
<td style="text-align:right;">
1.43
</td>
<td style="text-align:right;">
1.88
</td>
<td style="text-align:right;">
2.48
</td>
<td style="text-align:right;">
3.31
</td>
<td style="text-align:right;">
4.75
</td>
<td style="text-align:right;">
7.54
</td>
<td style="text-align:right;">
13.81
</td>
<td style="text-align:right;">
62.52
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Number of Stocks
</td>
<td style="text-align:right;">
1360.21
</td>
<td style="text-align:right;">
363.88
</td>
<td style="text-align:right;">
255.62
</td>
<td style="text-align:right;">
209.18
</td>
<td style="text-align:right;">
180.04
</td>
<td style="text-align:right;">
158.78
</td>
<td style="text-align:right;">
146.58
</td>
<td style="text-align:right;">
139.24
</td>
<td style="text-align:right;">
131.72
</td>
<td style="text-align:right;">
127.46
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
% NYSE Stocks
</td>
<td style="text-align:right;">
44.19
</td>
<td style="text-align:right;">
57.60
</td>
<td style="text-align:right;">
65.18
</td>
<td style="text-align:right;">
70.70
</td>
<td style="text-align:right;">
76.04
</td>
<td style="text-align:right;">
81.80
</td>
<td style="text-align:right;">
85.78
</td>
<td style="text-align:right;">
88.71
</td>
<td style="text-align:right;">
92.37
</td>
<td style="text-align:right;">
94.76
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Beta
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
1.03
</td>
<td style="text-align:right;">
1.03
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.01
</td>
</tr>
</tbody>
</table>
<p>The numbers are again very similar for both measures. The table illustrates the usage of NYSE breakpoints as the share of NYSE stocks increases across portfolios. The table also shows that the portfolio of largest firms takes up more than 60% of total market capitalization although it contains on average only about 130 firms.</p>
<p>Given the portfolio returns, I want to evaluate whether a portfolio exhibits on average positive or negative excess returns. The following function estimates the mean excess return and CAPM alpha for each portfolio and computes corresponding <a href="https://www.jstor.org/stable/1913610?seq=1">Newey and West (1987)</a> <span class="math inline">\(t\)</span>-statistics (using six lags as common in the literature) testing the null hypothesis that the average portfolio excess return or CAPM alpha is zero. I just recycle the function I wrote in <a href="https://christophscheuch.github.io/post/asset-pricing/beta/">this post</a>.</p>
<pre class="r"><code>estimate_portfolio_returns &lt;- function(data, ret) {
  # estimate average returns per portfolio
  average_ret &lt;- data %&gt;%
    group_by(portfolio) %&gt;%
    arrange(date) %&gt;%
    do(model = lm(paste0(rlang::as_name(enquo(ret)),&quot; ~ 1&quot;), data = .)) %&gt;% 
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
    do(model = lm(paste0(rlang::as_name(enquo(ret)),&quot; ~ 1 + ret_mkt&quot;), data = .)) %&gt;% 
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
}

rbind(estimate_portfolio_returns(portfolios_mktcap, ret_ew),
      estimate_portfolio_returns(portfolios_mktcap_ff, ret_ew),
      estimate_portfolio_returns(portfolios_mktcap, ret_vw),
      estimate_portfolio_returns(portfolios_mktcap_ff, ret_vw)) %&gt;%
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;)) %&gt;%
  pack_rows(&quot;Market Cap (Turan-Engle-Murray) - Equal-Weighted Portfolio Returns&quot;, 1, 4) %&gt;%
  pack_rows(&quot;Market Cap (Fama-French) - Equal-Weighted Portfolio Returns&quot;, 5, 8)%&gt;%
  pack_rows(&quot;Market Cap (Turan-Engle-Murray) - Value-Weighted Portfolio Returns&quot;, 9, 12) %&gt;%
  pack_rows(&quot;Market Cap (Fama-French) - Value-Weighted Portfolio Returns&quot;, 13, 16)</code></pre>
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
<strong>Market Cap (Turan-Engle-Murray) - Equal-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
1.62
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.84
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.60
</td>
<td style="text-align:right;">
-1.01
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.90
</td>
<td style="text-align:right;">
3.41
</td>
<td style="text-align:right;">
3.35
</td>
<td style="text-align:right;">
3.72
</td>
<td style="text-align:right;">
3.73
</td>
<td style="text-align:right;">
3.81
</td>
<td style="text-align:right;">
3.69
</td>
<td style="text-align:right;">
4.03
</td>
<td style="text-align:right;">
3.84
</td>
<td style="text-align:right;">
3.62
</td>
<td style="text-align:right;">
-3.25
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
-0.04
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
2.89
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
0.40
</td>
<td style="text-align:right;">
0.84
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
1.59
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
-1.67
</td>
<td style="text-align:right;">
-2.90
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Market Cap (Fama-French) - Equal-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
1.46
</td>
<td style="text-align:right;">
1.04
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.82
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.73
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
-0.85
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.90
</td>
<td style="text-align:right;">
3.32
</td>
<td style="text-align:right;">
3.62
</td>
<td style="text-align:right;">
3.62
</td>
<td style="text-align:right;">
3.75
</td>
<td style="text-align:right;">
3.85
</td>
<td style="text-align:right;">
3.79
</td>
<td style="text-align:right;">
3.90
</td>
<td style="text-align:right;">
3.88
</td>
<td style="text-align:right;">
3.59
</td>
<td style="text-align:right;">
-3.16
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
0.10
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.05
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
-0.56
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.65
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.82
</td>
<td style="text-align:right;">
1.04
</td>
<td style="text-align:right;">
0.82
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
-1.45
</td>
<td style="text-align:right;">
-2.65
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Market Cap (Turan-Engle-Murray) - Value-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
1.34
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.84
</td>
<td style="text-align:right;">
0.80
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.60
</td>
<td style="text-align:right;">
-0.74
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.40
</td>
<td style="text-align:right;">
3.41
</td>
<td style="text-align:right;">
3.38
</td>
<td style="text-align:right;">
3.73
</td>
<td style="text-align:right;">
3.74
</td>
<td style="text-align:right;">
3.80
</td>
<td style="text-align:right;">
3.72
</td>
<td style="text-align:right;">
4.08
</td>
<td style="text-align:right;">
3.83
</td>
<td style="text-align:right;">
3.73
</td>
<td style="text-align:right;">
-2.50
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
-0.01
</td>
<td style="text-align:right;">
-0.37
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
1.83
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
1.69
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
-0.41
</td>
<td style="text-align:right;">
-1.73
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>Market Cap (Fama-French) - Value-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
1.08
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.60
</td>
<td style="text-align:right;">
-0.48
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
3.25
</td>
<td style="text-align:right;">
3.21
</td>
<td style="text-align:right;">
3.62
</td>
<td style="text-align:right;">
3.70
</td>
<td style="text-align:right;">
3.79
</td>
<td style="text-align:right;">
3.91
</td>
<td style="text-align:right;">
3.85
</td>
<td style="text-align:right;">
4.01
</td>
<td style="text-align:right;">
3.90
</td>
<td style="text-align:right;">
3.75
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
0.15
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
-0.01
</td>
<td style="text-align:right;">
-0.16
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
1.23
</td>
<td style="text-align:right;">
1.05
</td>
<td style="text-align:right;">
1.44
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
-0.34
</td>
<td style="text-align:right;">
-0.88
</td>
</tr>
</tbody>
</table>
<p>The results show that all porfolios deliver positive excess returns, but the returns decrease as firm size increases. As a consequence, the portfolio that is long the largest firms and short the smallest firms yields statistically significant negative excess returns. Even though the CAPM alpha of each portfolio is statistically indistinguishable from zero, the long-short portfolio still yields negative alphas (except for the Fama-French value-weighted portfolios). Taken together, the results of the univariate portfolio analysis indicate a negative relation between firm market capitalization and future stock returns.</p>
</div>
<div id="regression-analysis" class="section level2">
<h2>Regression Analysis</h2>
<p>As a last step, I analyze the relation between firm size and stock returns using <a href="https://www.jstor.org/stable/1831028?seq=1">Fama and MacBeth (1973)</a> regression analysis. Each month, I perform a cross-sectional regression of one-month-ahead excess stock returns on the given measure of firm size. Time-series averages over these cross-sectional regressions then provide the desired results.</p>
<p>Again, I essentially recylce functions that I developed in <a href="https://christophscheuch.github.io/post/asset-pricing/beta/">an earlier post</a>. Note that I depart from Bali et al. and condition on all three explanatory variables being defined in the data, regardless of which measure I am using. I do this to ensure comparability across results, i.e. I want to run the same set of regressions across different specifications. Otherwise I cannot evaluate whether adding a covariate, using a different measure or changes in sample composition lead to different results (or all of them).</p>
<pre class="r"><code>winsorize &lt;- function(x, cut = 0.005){
  cut_point_top &lt;- quantile(x, 1 - cut, na.rm = TRUE)
  cut_point_bottom &lt;- quantile(x, cut, na.rm = TRUE)
  i &lt;- which(x &gt;= cut_point_top) 
  x[i] &lt;- cut_point_top
  j &lt;- which(x &lt;= cut_point_bottom) 
  x[j] &lt;- cut_point_bottom
  return(x)
}

fama_macbeth_regression &lt;- function(data, model, cut = 0.005) {
  # prepare and winsorize data
  data_nested &lt;- data %&gt;%
    filter(!is.na(ret_adj_f1) &amp; !is.na(size) &amp; !is.na(size_ff) &amp; !is.na(beta)) %&gt;%
    group_by(date) %&gt;%
    mutate_at(vars(size, size_ff, beta), ~winsorize(., cut = cut)) %&gt;%
    nest() 
  
  # perform cross-sectional regressions for each month
  cross_sectional_regs &lt;- data_nested %&gt;%
    mutate(model = map(data, ~lm(enexpr(model), data = .x))) %&gt;%
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
}

m1 &lt;- fama_macbeth_regression(crsp, ret_adj_f1 ~ size) %&gt;% rename(m1 = value)
m2 &lt;- fama_macbeth_regression(crsp, ret_adj_f1 ~ size + beta) %&gt;% rename(m2 = value)
m3 &lt;- fama_macbeth_regression(crsp, ret_adj_f1 ~ size_ff) %&gt;% rename(m3 = value)
m4 &lt;- fama_macbeth_regression(crsp, ret_adj_f1 ~ size_ff + beta) %&gt;% rename(m4 = value)</code></pre>
<p>The following table provides the regression results for various specifications.</p>
<pre class="r"><code>regression_table &lt;- m1 %&gt;%
  full_join(m2, by = &quot;statistic&quot;) %&gt;%
  full_join(m3, by = &quot;statistic&quot;) %&gt;%
  full_join(m4, by = &quot;statistic&quot;) %&gt;%
  right_join(tibble(statistic = c(&quot;(Intercept) coefficient&quot;, &quot;(Intercept) nw_t_stat&quot;, 
                                  &quot;size coefficient&quot;,  &quot;size nw_t_stat&quot;,
                                  &quot;size_ff coefficient&quot;, &quot;size_ff nw_t_stat&quot;,
                                  &quot;beta coefficient&quot;, &quot;beta nw_t_stat&quot;,
                                  &quot;adj_r_squared&quot;, &quot;n&quot;)), by = &quot;statistic&quot;)

colnames(regression_table) &lt;- c(&quot;statistic&quot;, &quot;m1&quot;, &quot;m2&quot;, &quot;m3&quot;, &quot;m4&quot;)
regression_table[, 1] &lt;- c(&quot;intercept&quot;, &quot;t-stat&quot;, 
                         &quot;size&quot;, &quot;t-stat&quot;, &quot;size_ff&quot;, &quot;t-stat&quot;, 
                         &quot;beta&quot;, &quot;t-stat&quot;,
                         &quot;adj_r_squared&quot;, &quot;n&quot;)

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
m1
</th>
<th style="text-align:right;">
m2
</th>
<th style="text-align:right;">
m3
</th>
<th style="text-align:right;">
m4
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
intercept
</td>
<td style="text-align:right;">
1.73
</td>
<td style="text-align:right;">
1.76
</td>
<td style="text-align:right;">
1.64
</td>
<td style="text-align:right;">
1.70
</td>
</tr>
<tr>
<td style="text-align:left;">
t-stat
</td>
<td style="text-align:right;">
4.74
</td>
<td style="text-align:right;">
5.64
</td>
<td style="text-align:right;">
4.68
</td>
<td style="text-align:right;">
5.59
</td>
</tr>
<tr>
<td style="text-align:left;">
size
</td>
<td style="text-align:right;">
-0.17
</td>
<td style="text-align:right;">
-0.17
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
t-stat
</td>
<td style="text-align:right;">
-3.71
</td>
<td style="text-align:right;">
-3.17
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
size_ff
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.15
</td>
<td style="text-align:right;">
-0.14
</td>
</tr>
<tr>
<td style="text-align:left;">
t-stat
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-3.41
</td>
<td style="text-align:right;">
-2.86
</td>
</tr>
<tr>
<td style="text-align:left;">
beta
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.09
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.14
</td>
</tr>
<tr>
<td style="text-align:left;">
t-stat
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.67
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-1.05
</td>
</tr>
<tr>
<td style="text-align:left;">
adj_r_squared
</td>
<td style="text-align:right;">
0.02
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
</tr>
<tr>
<td style="text-align:left;">
n
</td>
<td style="text-align:right;">
2944.50
</td>
<td style="text-align:right;">
2944.50
</td>
<td style="text-align:right;">
2944.50
</td>
<td style="text-align:right;">
2944.50
</td>
</tr>
</tbody>
</table>
<p>The regression results show a statisticall significant negative relation between firm size and abnormal stock returns. The results also show that controling for market beta neither yields a statistically significant relation to stock returns, nor does it impact the coefficient estimate on firm size. This result contradicts the fundamental prediction of the CAPM that market beta is the only determinant of cross-sectional variation in expected returns. Also note that the explanatory power of size and beta is still very low (see, e.g., <a href="https://onlinelibrary.wiley.com/doi/abs/10.1111/0022-1082.00144">Elton, 1999</a>).</p>

</dl>
