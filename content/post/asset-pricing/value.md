---
title: 'Tidy Asset Pricing - Part IV: The Value Premium'
subtitle: 
summary: 'On the relation of book-to-market ratio and future stock returns'
authors:
- admin
tags:
- Academic
categories:
- Asset Pricing
date: "2020-02-26T00:00:00Z"
lastmod: "2020-02-26T00:00:00Z"
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
<p>In this note, I explore the positive relation between the book-to-market (BM) ratio and expected stock returns – called the value premium – following <a href="https://www.wiley.com/en-us/Empirical+Asset+Pricing%3A+The+Cross+Section+of+Stock+Returns-p-9781118095041">Bali, Engle and Murray</a>. In three earlier notes, I first <a href="https://christophscheuch.github.io/post/asset-pricing/crsp-sample/">prepared the CRSP sample</a> in R, <a href="https://christophscheuch.github.io/post/asset-pricing/beta/">estimated and analyzed market betas</a> and <a href="https://christophscheuch.github.io/post/asset-pricing/size/">analyzed the relation between firm size and stock returns</a> using the same data. I do not go into the theoretical foundations of the value premium, but rather focus on its estimation in a tidy manner. The usual disclaimer applies, so the text below references an opinion and is for information purposes only and I do not intend to provide any investment advice. The code below replicates most of the results of Bali et al. up to a few basis points if I restrict my data to their sample period. If you spot any mistakes or want to share any suggestions for better implementation, just drop a comment below.</p>
<p>I mainly use the following packages throughout this note:</p>
<pre class="r"><code>library(tidyverse)
library(lubridate)  # for working with dates
library(broom)      # for tidying up estimation results
library(kableExtra) # for nicer html tables</code></pre>
<div id="calculating-the-book-to-market-ratio" class="section level2">
<h2>Calculating the Book-to-Market Ratio</h2>
<p>The BM ratio is defined as the book value of a firm’s common equity (BE) divided by the market value of the firm’s equity (ME), where the book value comes from the firm’s balance sheet and the market value is equal to the market capitalization of the firm as provided in the CRSP data. To get the BE data, I download the <a href="https://wrds-www.wharton.upenn.edu/pages/support/data-overview/wrds-overview-crspcompustat-merged-ccm/">CRSP/Compustat Merged (CCM)</a> data from WRDS. Importantly, this data already contains links of balance sheet information to the stock identifiers of CRSP.</p>
<p>First, I extract the following variables from the CCM sample:</p>
<pre class="r"><code>ccmfunda &lt;- read_csv(&quot;raw/ccmfunda.csv&quot;)
colnames(ccmfunda) &lt;- tolower(colnames(ccmfunda))

# select and parse relevant variables
compustat &lt;- ccmfunda %&gt;%
  transmute(
    permno = as.integer(lpermno),      # stock identifier
    datadate = ymd(datadate),          # date of report
    linktype = as.character(linktype), # link type
    linkenddt = ymd(linkenddt),        # date when link ends to be valid
    seq = as.numeric(seq),             # stockholders&#39; equity
    txdb = as.numeric(txdb),           # deferred taxes
    itcb = as.numeric(itcb),           # investment tax credit
    pstkrv = as.numeric(pstkrv),       # preferred stock redemption value
    pstkl = as.numeric(pstkl),         # preferred stock liquidating value
    pstk = as.numeric(pstk),           # preferred stock par value
    indfmt = as.character(indfmt),     # industry format
    datafmt = as.character(datafmt)    # data format
  ) </code></pre>
<p>Then, I apply a number of consistency checks and filters to the data. First, I need to make sure that only industry formats equal to <code>&quot;INDL&quot;</code> and data formats equal to <code>&quot;STD&quot;</code> are used.</p>
<pre class="r"><code>compustat &lt;- compustat %&gt;%
  filter(indfmt == &quot;INDL&quot; &amp; datafmt == &quot;STD&quot;) %&gt;%
  select(-c(indfmt, datafmt))</code></pre>
<p>Second, I should only use links that are established by comparing CUSIP values in the Compustat and CRSP database (<code>&quot;LU&quot;</code>) or links that have been researched and verified (<code>&quot;LC&quot;</code>).</p>
<pre class="r"><code>compustat &lt;- compustat %&gt;%
  filter(linktype %in% c(&quot;LU&quot;, &quot;LC&quot;))</code></pre>
<p>Third, I verify that links are still active at the time a firm’s accounting information is computed.</p>
<pre class="r"><code>compustat &lt;- compustat %&gt;%
  filter(datadate &lt;= linkenddt | is.na(linkenddt))</code></pre>
<p>Fourth, I make sure that there are no duplicate observations.</p>
<pre class="r"><code>compustat &lt;- compustat %&gt;%
  distinct()</code></pre>
<p>Fifth, I check that there is only one observation for each stock-date pair.</p>
<pre class="r"><code>compustat %&gt;%
  group_by(permno, datadate) %&gt;%
  filter(n() &gt; 1) %&gt;%
  nrow() == 0</code></pre>
<p>After these checks, I can proceed to calculate BE. The calculation is described in Bali et al. and translates to the following code where <code>bvps</code> refers to the book value of preferred stock.</p>
<pre class="r"><code>compustat &lt;- compustat %&gt;%
  mutate(bvps = case_when(!is.na(pstkrv) ~ pstkrv,
                          is.na(pstkrv) &amp; !is.na(pstkl) ~ pstkl,
                          is.na(pstkrv) &amp; is.na(pstkl) &amp; !is.na(pstk) ~ pstk,
                          TRUE ~ as.numeric(0)),
         be = seq + txdb + itcb - bvps,
         be = if_else(be &lt; 0, as.numeric(NA), be)) %&gt;%
  select(permno, datadate, be) </code></pre>
<p>To match the BE variable to the CRSP data and compute the BM ratio, I follow the procedure of <a href="https://onlinelibrary.wiley.com/doi/full/10.1111/j.1540-6261.1992.tb04398.x">Fama and French (1992)</a>, which is the most standard approach to doing this. Fama and French compute BM at the end of each calendar year and use that information starting in June of the next calendar year until the following May.</p>
<p>Firms have different fiscal year ends that indicate the timing of the accounting information as reported in the column <code>datadate</code>. However, this date does not indicate when the accounting information was publicly available. Moreoever, in some cases, firms change the month in which their fiscal year ends, resulting in multiple entries in the Compustat data per calendar year <span class="math inline">\(y\)</span>. The Fama-French procedure first ensures that there is only one entry per calendar year <span class="math inline">\(y\)</span> by taking the last available information.</p>
<pre class="r"><code>compustat &lt;- compustat %&gt;%
  mutate(year = year(datadate)) %&gt;%
  group_by(permno, year) %&gt;%
  filter(datadate == max(datadate)) %&gt;%
  ungroup()</code></pre>
<p>At this stage, I also kick out unnecessary rows with missing values in the BE column. These observations would lead to missing values in the upcoming matching procedure anyway.</p>
<pre class="r"><code>compustat &lt;- compustat %&gt;%
  na.omit()
nrow(compustat)</code></pre>
<p>Fama and French assume that data from the calendar year <span class="math inline">\(y\)</span> is not known until the end of June of year <span class="math inline">\(y+1\)</span>. I hence define the corresponding reference date for each observation.</p>
<pre class="r"><code>compustat &lt;- compustat %&gt;%
  mutate(reference_date = ymd(paste0(year + 1, &quot;-06-01&quot;))) %&gt;%
  select(-year)</code></pre>
<p>For each return observation in year <span class="math inline">\(y+1\)</span>, the relevant accounting information is either still from the end of year <span class="math inline">\(y-1\)</span> until May, or computed at the end of year <span class="math inline">\(y\)</span> starting from June. I match the BE variable following this rationale in the next code chunk.</p>
<pre class="r"><code>crsp &lt;- read_rds(&quot;data/crsp.rds&quot;)

# add book equity data
crsp &lt;- crsp %&gt;%
  mutate(year = year(date),
         month = month(date),
         reference_date = ymd(if_else(month &lt; 6, 
                                      paste0(year - 1, &quot;-06-01&quot;), 
                                      paste0(year, &quot;-06-01&quot;)))) 

crsp &lt;- crsp %&gt;%
  left_join(compustat, by = c(&quot;permno&quot;, &quot;reference_date&quot;))</code></pre>
<p>As mentioned above, the ME variable is determined each year at the end of a calendar year, but it is used to calculate BM starting in June of the following year.</p>
<pre class="r"><code>crsp_me &lt;- crsp %&gt;%
  filter(month(date_start) == 12) %&gt;%
  mutate(reference_date = ymd(paste0(year(date_start) + 1, &quot;-06-01&quot;))) %&gt;%
  select(permno, reference_date, me = mktcap)

crsp &lt;- crsp %&gt;%
  left_join(crsp_me, by = c(&quot;permno&quot;, &quot;reference_date&quot;))</code></pre>
<p>Next, I also add an estimate for a stock’s beta which I calculated in <a href="https://christophscheuch.github.io/post/asset-pricing/beta/">this note</a>. I use the beta based on daily data using a 12 month estimation window, as common in the literature according to Bali et al. </p>
<pre class="r"><code>betas &lt;- read_rds(&quot;data/betas_daily.rds&quot;)
crsp &lt;- crsp %&gt;%
  left_join(betas %&gt;%
              select(permno, date_start, beta = beta_12m), 
            by = c(&quot;permno&quot;, &quot;date_start&quot;))</code></pre>
<p>Finally, I can calculate the BM ratio and define additional variables that later enter the regression analysis.</p>
<pre class="r"><code>crsp &lt;- crsp %&gt;%
  mutate(bm = be / me,
         log_bm = if_else(bm == 0, as.numeric(NA), log(bm)),
         size = log(mktcap))</code></pre>
<p>The earliest date in my Compustat sample is in October 1960, while CRSP already starts in 1926. It might be thus interesting to look at the coverage of the variables of interest over the full sample period.</p>
<pre class="r"><code>crsp %&gt;%
  group_by(date) %&gt;%
  summarize(share_with_me = sum(!is.na(me)) / n() * 100,
            share_with_be = sum(!is.na(be)) / n() * 100,
            share_with_bm = sum(!is.na(bm)) / n() * 100) %&gt;%
  ggplot(aes(x = date)) +
  geom_line(aes(y = share_with_me, color = &quot;Market Equity&quot;)) +
  geom_line(aes(y = share_with_be, color = &quot;Book Equity&quot;)) +
  geom_line(aes(y = share_with_bm, color = &quot;Book-to-Market&quot;)) +
  labs(x = &quot;&quot;, y = &quot;Share of Stocks with Information (in %)&quot;, color = &quot;Variable&quot;) + 
  scale_x_date(expand = c(0, 0), date_breaks = &quot;10 years&quot;, date_labels = &quot;%Y&quot;) +
  theme_classic()</code></pre>
<p><img src="data:image/png;base64,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" width="672" style="display: block; margin: auto;" /></p>
<p>There seem to be a few periods in which market and book equity data is temporarily not available anymore for a huge chunk of firms. However, I currently have no idea why this is the case.</p>
<p>I follow the convention outlined in Bali et al. and start the sample in June 1963. Apart from the lack of accounting information, Bali et al. also mention that AMEX stocks start to be included in CRSP in July 1962 as a further reason (see <a href="https://christophscheuch.github.io/post/asset-pricing/crsp-sample/">this post</a>).</p>
<pre class="r"><code>crsp &lt;- crsp %&gt;%
  filter(date &gt;= &quot;1963-06-01&quot;)</code></pre>
</div>
<div id="summary-statistics" class="section level2">
<h2>Summary Statistics</h2>
<p>I proceed to present summary statistics for the different measures I use in the following analyses. I first compute corresponding summary statistics for each month and before I average each summary statistics across all months. Note that I call the <code>moments</code> package to compute skewness and kurtosis. Since I repeatedly use a similar procedure, I define the following function which uses the <a href="https://adv-r.hadley.nz/functions.html#fun-dot-dot-dot">dot-dot-dot</a> argument and the curly-curly operator to compute summary statistics for aribtrary columns grouped by some variable.</p>
<pre class="r"><code>summary_statistics &lt;- function(data, ..., by) {
    data %&gt;% 
    select({{ by }}, ...) %&gt;%
    pivot_longer(cols = -{{ by }}, names_to = &quot;measure&quot;) %&gt;%
    na.omit() %&gt;%
    group_by(measure, {{ by }}) %&gt;%
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
}</code></pre>
<p>The following table provides the time series averages of summary statistics for all observations with a non-missing BM.</p>
<pre class="r"><code>crsp_ts &lt;- crsp %&gt;%
  filter(!is.na(bm)) %&gt;%
  summary_statistics(bm, log_bm, be, me, mktcap, by = date)

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
be
</td>
<td style="text-align:right;">
783.72
</td>
<td style="text-align:right;">
3719.33
</td>
<td style="text-align:right;">
16.31
</td>
<td style="text-align:right;">
420.89
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
4.52
</td>
<td style="text-align:right;">
25.91
</td>
<td style="text-align:right;">
95.50
</td>
<td style="text-align:right;">
357.89
</td>
<td style="text-align:right;">
2947.87
</td>
<td style="text-align:right;">
90269.14
</td>
<td style="text-align:right;">
3201.61
</td>
</tr>
<tr>
<td style="text-align:left;">
bm
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
1.28
</td>
<td style="text-align:right;">
11.13
</td>
<td style="text-align:right;">
317.52
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.39
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
1.11
</td>
<td style="text-align:right;">
2.28
</td>
<td style="text-align:right;">
37.68
</td>
<td style="text-align:right;">
3201.61
</td>
</tr>
<tr>
<td style="text-align:left;">
log_bm
</td>
<td style="text-align:right;">
-0.52
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
-0.62
</td>
<td style="text-align:right;">
5.49
</td>
<td style="text-align:right;">
-6.31
</td>
<td style="text-align:right;">
-2.06
</td>
<td style="text-align:right;">
-1.02
</td>
<td style="text-align:right;">
-0.45
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
3.18
</td>
<td style="text-align:right;">
3201.57
</td>
</tr>
<tr>
<td style="text-align:left;">
me
</td>
<td style="text-align:right;">
1692.53
</td>
<td style="text-align:right;">
7801.00
</td>
<td style="text-align:right;">
14.38
</td>
<td style="text-align:right;">
297.86
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
8.18
</td>
<td style="text-align:right;">
47.36
</td>
<td style="text-align:right;">
191.41
</td>
<td style="text-align:right;">
762.77
</td>
<td style="text-align:right;">
6447.00
</td>
<td style="text-align:right;">
195302.48
</td>
<td style="text-align:right;">
3201.61
</td>
</tr>
<tr>
<td style="text-align:left;">
mktcap
</td>
<td style="text-align:right;">
1815.19
</td>
<td style="text-align:right;">
8327.60
</td>
<td style="text-align:right;">
14.27
</td>
<td style="text-align:right;">
293.05
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
7.81
</td>
<td style="text-align:right;">
47.60
</td>
<td style="text-align:right;">
202.99
</td>
<td style="text-align:right;">
828.82
</td>
<td style="text-align:right;">
6995.65
</td>
<td style="text-align:right;">
209416.19
</td>
<td style="text-align:right;">
3189.10
</td>
</tr>
</tbody>
</table>
<p>BM, BE and ME all exhibit a high positive skewness which justifies the log transformation in the regression analysis later. The average and median of BM are below one which indicates that the book equity values tend to be smaller than market equity valuations.</p>
</div>
<div id="correlations" class="section level2">
<h2>Correlations</h2>
<p>I now proceed to examine the cross-sectional correlations between the variables of interest. To do so, I define the following function which takes an arbitrary number of columns as input.</p>
<pre class="r"><code>compute_correlations &lt;- function(data, ..., upper = TRUE) {
  cor_matrix &lt;- data %&gt;%
    select(...) %&gt;%
    cor(use = &quot;complete.obs&quot;, method = &quot;pearson&quot;) 
  if (upper == TRUE) {
    cor_matrix[upper.tri(cor_matrix, diag = FALSE)] &lt;- NA
  }
  return(cor_matrix)
}</code></pre>
<p>The table below provides the results.</p>
<pre class="r"><code>correlations &lt;- crsp %&gt;%
  filter(!is.na(bm)) %&gt;%
  compute_correlations(bm, log_bm, be, me, mktcap, size, beta)

correlations %&gt;% 
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;))</code></pre>
<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
bm
</th>
<th style="text-align:right;">
log_bm
</th>
<th style="text-align:right;">
be
</th>
<th style="text-align:right;">
me
</th>
<th style="text-align:right;">
mktcap
</th>
<th style="text-align:right;">
size
</th>
<th style="text-align:right;">
beta
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
bm
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
log_bm
</td>
<td style="text-align:right;">
0.54
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
be
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
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
me
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
-0.09
</td>
<td style="text-align:right;">
0.74
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
mktcap
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
-0.09
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.97
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
size
</td>
<td style="text-align:right;">
-0.19
</td>
<td style="text-align:right;">
-0.29
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
beta
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>
<p>As expected, the cross-sectional correlation between BM and its log transformation is quite high. Moreover, BM is negatively correlated with each of market beta and size. The negative correlation between size and BM is somewhat mechnical als market capitalization is the denominator of BM.</p>
</div>
<div id="persistence" class="section level2">
<h2>Persistence</h2>
<p>In this section, I exmanie the persistence of BM and its log transformation over time via the following function.</p>
<pre class="r"><code>compute_persistence &lt;- function(data, var, tau) {
  dates &lt;- data %&gt;%
    distinct(date) %&gt;%
    arrange(date) %&gt;%
    mutate(date_lag = lag(date, tau))
  
  correlation &lt;- data %&gt;%
    select(permno, date, {{ var }}) %&gt;%
    left_join(dates, by = &quot;date&quot;) %&gt;%
    left_join(data %&gt;% 
                select(permno, date, {{ var }}) %&gt;%
                rename(&quot;{{ var }}_lag&quot; := {{ var }}),
              by = c(&quot;permno&quot;, &quot;date_lag&quot;=&quot;date&quot;)) %&gt;%
    select(contains(rlang::quo_text(enquo(var)))) %&gt;%
    cor(use = &quot;pairwise.complete.obs&quot;, method = &quot;pearson&quot;)
  
  return(correlation[1, 2])
}

persistence &lt;- tribble(~tau, ~bm, ~log_bm,
                       12, compute_persistence(crsp, bm, 12), compute_persistence(crsp, log_bm, 12),
                       24, compute_persistence(crsp, bm, 24), compute_persistence(crsp, log_bm, 24),
                       36, compute_persistence(crsp, bm, 36), compute_persistence(crsp, log_bm, 36),
                       48, compute_persistence(crsp, bm, 48), compute_persistence(crsp, log_bm, 48),
                       60, compute_persistence(crsp, bm, 60), compute_persistence(crsp, log_bm, 60),
                       120, compute_persistence(crsp, bm, 120), compute_persistence(crsp, log_bm, 120))</code></pre>
<p>The next table provides the persistence results.</p>
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
bm
</th>
<th style="text-align:right;">
log_bm
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.79
</td>
</tr>
<tr>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
0.66
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.59
</td>
</tr>
<tr>
<td style="text-align:right;">
48
</td>
<td style="text-align:right;">
0.48
</td>
<td style="text-align:right;">
0.53
</td>
</tr>
<tr>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
0.48
</td>
</tr>
<tr>
<td style="text-align:right;">
120
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.38
</td>
</tr>
</tbody>
</table>
<p>The table indicates thath both BM and log(BM) exhibit substantial persistence, but with decaying intensity over time. Regardless whether BM measures factor sensitivity or some sort of mispricing, it seems to persist for an extended period of time for the average stock.</p>
</div>
<div id="portfolio-analysis" class="section level2">
<h2>Portfolio Analysis</h2>
<p>Now, I turn to the cross-sectional relation between the book-to-market ratio and future stock returns. To conduct the univariate portfolio analysis, I need a couple of functions that I introduced in an <a href="https://christophscheuch.github.io/post/asset-pricing/size/">earlier post</a>.</p>
<pre class="r"><code>weighted_mean &lt;- function(x, w, ..., na.rm = FALSE){
  if(na.rm){
    x1 &lt;- x[!is.na(x) &amp; !is.na(w)]
    w &lt;- w[!is.na(x) &amp; !is.na(w)]
    x &lt;- x1
  }
  weighted.mean(x, w, ..., na.rm = FALSE)
}

get_breakpoints_funs &lt;- function(var, n_portfolios = 10) {
  # get relevant percentiles
  percentiles &lt;- seq(0, 1, length.out = (n_portfolios + 1))
  percentiles &lt;- percentiles[percentiles &gt; 0 &amp; percentiles &lt; 1]
  
  # construct set of named quantile functions
  percentiles_names &lt;- map_chr(percentiles, ~paste0(rlang::quo_text(enquo(var)), &quot;_q&quot;, .x*100))
  percentiles_funs &lt;- map(percentiles, ~partial(quantile, probs = .x, na.rm = TRUE)) %&gt;% 
    set_names(nm = percentiles_names)
  return(percentiles_funs)
}

get_portfolio &lt;- function(x, breakpoints) {
  portfolio &lt;- as.integer(1 + findInterval(x, unlist(breakpoints)))
  return(portfolio)
}</code></pre>
<p>The next function is an adapted version from the size post as it features the additional option of whether to use only NYSE stocks or all stocks to determine the breakpoints for portfolio sorts.</p>
<pre class="r"><code>construct_univariate_portfolios &lt;- function(data, var, n_portfolios = 10, nyse_breakpoints = TRUE) {
  data &lt;- data %&gt;%
    filter(!is.na({{ var }}))
  
  # determine breakpoints based on NYSE stocks only or on all stocks
  if (nyse_breakpoints == TRUE) {
    data_quantiles &lt;- data %&gt;%
      filter(exchange == &quot;NYSE&quot;) %&gt;%
      select(date, {{ var }})
  } else {
    data_quantiles &lt;- data %&gt;%
      select(date, {{ var }})
  }
  
  var_funs &lt;- get_breakpoints_funs({{ var }}, n_portfolios)
  quantiles &lt;- data_quantiles %&gt;%
    group_by(date) %&gt;%
    summarize_at(vars({{ var }}), lst(!!! var_funs)) %&gt;%
    group_by(date) %&gt;%
    nest() %&gt;%
    rename(quantiles = data)
  
  # independently sort all stocks into portfolios based on breakpoints
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
              {{ var }} := mean({{ var }}, na.rm = TRUE),
              log_bm = mean(log_bm, na.rm = TRUE),
              mktcap = mean(mktcap, na.rm = TRUE),
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
}</code></pre>
<p>The table below provides the summary statistics for each portfolio using the above functions.</p>
<pre class="r"><code>portfolios_bm_nyse &lt;- construct_univariate_portfolios(crsp, bm)
portfolios_bm_all &lt;- construct_univariate_portfolios(crsp, bm, nyse_breakpoints = FALSE)

portfolios_table_nyse &lt;- portfolios_bm_nyse %&gt;%
  filter(portfolio != &quot;10-1&quot;) %&gt;%
  group_by(portfolio) %&gt;%
  summarize(bm_mean = mean(bm),
            log_bm_mean = mean(log_bm),
            mktcap_mean = mean(mktcap),
            n = mean(n_stocks),
            n_nyse = mean(n_stocks_nyse / n_stocks) * 100,
            beta = mean(beta)) %&gt;%
  t()

portfolios_table_nyse &lt;- portfolios_table_nyse[-1, ] %&gt;%
  as.numeric() %&gt;%
  matrix(., ncol = 10)
colnames(portfolios_table_nyse) &lt;- seq(1, 10, 1)
rownames(portfolios_table_nyse) &lt;- c(&quot;BM&quot;, &quot;Log(BM)&quot;, &quot;MktCap&quot;, &quot;Number of Stocks&quot;, &quot;% NYSE Stocks&quot; , &quot;Beta&quot;)

portfolios_table_all &lt;- portfolios_bm_all %&gt;%
  filter(portfolio != &quot;10-1&quot;) %&gt;%
  group_by(portfolio) %&gt;%
  summarize(bm_mean = mean(bm),
            log_bm_mean = mean(log_bm),
            mktcap_mean = mean(mktcap),
            n = mean(n_stocks),
            n_nyse = mean(n_stocks_nyse / n_stocks) * 100,
            beta = mean(beta)) %&gt;%
  t()

portfolios_table_all &lt;- portfolios_table_all[-1, ] %&gt;%
  as.numeric() %&gt;%
  matrix(., ncol = 10)
colnames(portfolios_table_all) &lt;- seq(1, 10, 1)
rownames(portfolios_table_all) &lt;- c(&quot;BM&quot;, &quot;Log(BM)&quot;, &quot;MktCap&quot;, &quot;Number of Stocks&quot;, &quot;% NYSE Stocks&quot; , &quot;Beta&quot;)

# combine to table and print html
rbind(portfolios_table_nyse, portfolios_table_all) %&gt;%
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;)) %&gt;%
  pack_rows(&quot;NYSE Breakpoints&quot;, 1, 6) %&gt;%
  pack_rows(&quot;All Stocks Breakpoints&quot;, 7, 12)</code></pre>
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
<tr grouplength="6">
<td colspan="11" style="border-bottom: 1px solid;">
<strong>NYSE Breakpoints</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
BM
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
0.56
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
1.07
</td>
<td style="text-align:right;">
1.34
</td>
<td style="text-align:right;">
2.75
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Log(BM)
</td>
<td style="text-align:right;">
-2.01
</td>
<td style="text-align:right;">
-1.17
</td>
<td style="text-align:right;">
-0.86
</td>
<td style="text-align:right;">
-0.65
</td>
<td style="text-align:right;">
-0.47
</td>
<td style="text-align:right;">
-0.31
</td>
<td style="text-align:right;">
-0.16
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.81
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
MktCap
</td>
<td style="text-align:right;">
3067.17
</td>
<td style="text-align:right;">
2803.46
</td>
<td style="text-align:right;">
2521.26
</td>
<td style="text-align:right;">
1918.89
</td>
<td style="text-align:right;">
1805.70
</td>
<td style="text-align:right;">
1603.44
</td>
<td style="text-align:right;">
1213.99
</td>
<td style="text-align:right;">
1184.86
</td>
<td style="text-align:right;">
858.61
</td>
<td style="text-align:right;">
627.10
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Number of Stocks
</td>
<td style="text-align:right;">
509.70
</td>
<td style="text-align:right;">
344.79
</td>
<td style="text-align:right;">
301.26
</td>
<td style="text-align:right;">
279.80
</td>
<td style="text-align:right;">
265.91
</td>
<td style="text-align:right;">
261.67
</td>
<td style="text-align:right;">
260.85
</td>
<td style="text-align:right;">
271.33
</td>
<td style="text-align:right;">
301.35
</td>
<td style="text-align:right;">
405.77
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
% NYSE Stocks
</td>
<td style="text-align:right;">
30.63
</td>
<td style="text-align:right;">
37.60
</td>
<td style="text-align:right;">
42.49
</td>
<td style="text-align:right;">
44.03
</td>
<td style="text-align:right;">
46.55
</td>
<td style="text-align:right;">
47.39
</td>
<td style="text-align:right;">
47.15
</td>
<td style="text-align:right;">
44.77
</td>
<td style="text-align:right;">
41.26
</td>
<td style="text-align:right;">
33.51
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Beta
</td>
<td style="text-align:right;">
1.06
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
0.82
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.73
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.64
</td>
</tr>
<tr grouplength="6">
<td colspan="11" style="border-bottom: 1px solid;">
<strong>All Stocks Breakpoints</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
BM
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.39
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
1.11
</td>
<td style="text-align:right;">
1.45
</td>
<td style="text-align:right;">
3.02
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Log(BM)
</td>
<td style="text-align:right;">
-2.26
</td>
<td style="text-align:right;">
-1.38
</td>
<td style="text-align:right;">
-1.02
</td>
<td style="text-align:right;">
-0.76
</td>
<td style="text-align:right;">
-0.54
</td>
<td style="text-align:right;">
-0.35
</td>
<td style="text-align:right;">
-0.16
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.89
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
MktCap
</td>
<td style="text-align:right;">
2931.34
</td>
<td style="text-align:right;">
2937.76
</td>
<td style="text-align:right;">
2717.89
</td>
<td style="text-align:right;">
2142.78
</td>
<td style="text-align:right;">
1919.03
</td>
<td style="text-align:right;">
1601.10
</td>
<td style="text-align:right;">
1228.94
</td>
<td style="text-align:right;">
1136.03
</td>
<td style="text-align:right;">
831.41
</td>
<td style="text-align:right;">
611.28
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Number of Stocks
</td>
<td style="text-align:right;">
320.59
</td>
<td style="text-align:right;">
320.10
</td>
<td style="text-align:right;">
320.24
</td>
<td style="text-align:right;">
320.05
</td>
<td style="text-align:right;">
319.99
</td>
<td style="text-align:right;">
320.41
</td>
<td style="text-align:right;">
320.12
</td>
<td style="text-align:right;">
320.01
</td>
<td style="text-align:right;">
320.21
</td>
<td style="text-align:right;">
320.70
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
% NYSE Stocks
</td>
<td style="text-align:right;">
28.25
</td>
<td style="text-align:right;">
34.07
</td>
<td style="text-align:right;">
39.55
</td>
<td style="text-align:right;">
42.64
</td>
<td style="text-align:right;">
45.25
</td>
<td style="text-align:right;">
46.64
</td>
<td style="text-align:right;">
45.37
</td>
<td style="text-align:right;">
43.22
</td>
<td style="text-align:right;">
39.69
</td>
<td style="text-align:right;">
32.35
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Beta
</td>
<td style="text-align:right;">
1.06
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.84
</td>
<td style="text-align:right;">
0.80
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
0.73
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.64
</td>
</tr>
</tbody>
</table>
<p>By construction, the average BM ratio increases across portfolios. Market capitalization, betas and the number of stocks in each portfolio decrease across portfolios. These patterns are the same regardless of which breakpoints are used.</p>
<p>For illustrative purposes, I also plot the cumulative value-weighted excess returns for different portfolios over time to get a better feeling for the data.</p>
<pre class="r"><code>portfolios_bm_nyse %&gt;%
  group_by(portfolio) %&gt;%
  select(portfolio, date, ret_vw) %&gt;%
  arrange(date) %&gt;%
  mutate(cum_ret = cumsum(ret_vw)) %&gt;%
  ungroup() %&gt;%
  ggplot(aes(x = date, y = cum_ret, group = as.factor(portfolio))) +
  geom_line(aes(color = as.factor(portfolio), linetype = as.factor(portfolio))) +
  labs(x = &quot;&quot;, y = &quot;Cumulative Log Return (in %)&quot;, 
       color = &quot;Size Portfolio&quot;, linetype = &quot;Size Portfolio&quot;) + 
  scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks()) + 
  scale_x_date(expand = c(0, 0), date_breaks = &quot;10 years&quot;, date_labels = &quot;%Y&quot;) +
  theme_classic()</code></pre>
<p><img src="data:image/png;base64,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" width="672" style="display: block; margin: auto;" /></p>
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
}</code></pre>
<p>The following table provides the results of the univariate portfolio sorts using NYSE breakpoints.</p>
<pre class="r"><code>rbind(estimate_portfolio_returns(portfolios_bm_nyse, ret_ew),
      estimate_portfolio_returns(portfolios_bm_nyse, ret_vw)) %&gt;%
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;)) %&gt;%
  pack_rows(&quot;NYSE Breakpoints - Equal-Weighted Portfolio Returns&quot;, 1, 4) %&gt;%
  pack_rows(&quot;NYSE Breakpoints - Value-Weighted Portfolio Returns&quot;, 5, 8) </code></pre>
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
<strong>NYSE Breakpoints - Equal-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.43
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
1.13
</td>
<td style="text-align:right;">
1.01
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
1.26
</td>
<td style="text-align:right;">
1.53
</td>
<td style="text-align:right;">
1.90
</td>
<td style="text-align:right;">
2.12
</td>
<td style="text-align:right;">
2.35
</td>
<td style="text-align:right;">
2.19
</td>
<td style="text-align:right;">
2.57
</td>
<td style="text-align:right;">
2.84
</td>
<td style="text-align:right;">
2.86
</td>
<td style="text-align:right;">
5.33
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
-0.61
</td>
<td style="text-align:right;">
-0.26
</td>
<td style="text-align:right;">
-0.15
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.41
</td>
<td style="text-align:right;">
0.56
</td>
<td style="text-align:right;">
1.17
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
-2.11
</td>
<td style="text-align:right;">
-1.04
</td>
<td style="text-align:right;">
-0.63
</td>
<td style="text-align:right;">
-0.09
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
1.17
</td>
<td style="text-align:right;">
1.58
</td>
<td style="text-align:right;">
1.90
</td>
<td style="text-align:right;">
5.91
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>NYSE Breakpoints - Value-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.32
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
0.43
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.52
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.47
</td>
<td style="text-align:right;">
0.57
</td>
<td style="text-align:right;">
0.67
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
1.18
</td>
<td style="text-align:right;">
1.65
</td>
<td style="text-align:right;">
1.50
</td>
<td style="text-align:right;">
1.70
</td>
<td style="text-align:right;">
1.35
</td>
<td style="text-align:right;">
2.21
</td>
<td style="text-align:right;">
2.04
</td>
<td style="text-align:right;">
1.94
</td>
<td style="text-align:right;">
2.25
</td>
<td style="text-align:right;">
2.49
</td>
<td style="text-align:right;">
1.81
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
-0.26
</td>
<td style="text-align:right;">
-0.14
</td>
<td style="text-align:right;">
-0.19
</td>
<td style="text-align:right;">
-0.12
</td>
<td style="text-align:right;">
-0.18
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
-0.01
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.40
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
-1.21
</td>
<td style="text-align:right;">
-0.55
</td>
<td style="text-align:right;">
-0.96
</td>
<td style="text-align:right;">
-0.56
</td>
<td style="text-align:right;">
-0.69
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
2.04
</td>
</tr>
</tbody>
</table>
<p>Excess returns are monotonically increasing across BM portfolios, where stocks with high BM ratios seem to exhibit statistically significant excess returns. It is hence not surprising that the difference portfolio yields statistically significant positive excess returns. Adjusting the returns using the CAPM risk model has little effect on this result. Using value-weighted returns leads to a weaker relation between BM and expected returns, but the overall patterns are the same.</p>
<p>The next table provides the results of the univariate portfolio sorts using all stocks to calculate breakpoints.</p>
<pre class="r"><code>rbind(estimate_portfolio_returns(portfolios_bm_all, ret_ew),
      estimate_portfolio_returns(portfolios_bm_all, ret_vw)) %&gt;%
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;)) %&gt;%
  pack_rows(&quot;All Stocks Breakpoints - Equal-Weighted Portfolio Returns&quot;, 1, 4) %&gt;%
  pack_rows(&quot;All Stocks Breakpoints - Value-Weighted Portfolio Returns&quot;, 5, 8) </code></pre>
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
<strong>All Stocks Breakpoints - Equal-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.37
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
0.80
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.21
</td>
<td style="text-align:right;">
1.21
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
0.76
</td>
<td style="text-align:right;">
1.13
</td>
<td style="text-align:right;">
1.81
</td>
<td style="text-align:right;">
1.94
</td>
<td style="text-align:right;">
2.31
</td>
<td style="text-align:right;">
2.43
</td>
<td style="text-align:right;">
2.61
</td>
<td style="text-align:right;">
2.86
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
5.77
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
-0.75
</td>
<td style="text-align:right;">
-0.44
</td>
<td style="text-align:right;">
-0.30
</td>
<td style="text-align:right;">
-0.07
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.43
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
1.39
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
-2.52
</td>
<td style="text-align:right;">
-1.64
</td>
<td style="text-align:right;">
-1.18
</td>
<td style="text-align:right;">
-0.29
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
1.22
</td>
<td style="text-align:right;">
1.63
</td>
<td style="text-align:right;">
2.11
</td>
<td style="text-align:right;">
6.30
</td>
</tr>
<tr grouplength="4">
<td colspan="12" style="border-bottom: 1px solid;">
<strong>All Stocks Breakpoints - Value-Weighted Portfolio Returns</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Excess Return
</td>
<td style="text-align:right;">
0.32
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.43
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
0.57
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.44
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
1.08
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
1.71
</td>
<td style="text-align:right;">
1.36
</td>
<td style="text-align:right;">
1.76
</td>
<td style="text-align:right;">
1.86
</td>
<td style="text-align:right;">
2.03
</td>
<td style="text-align:right;">
2.03
</td>
<td style="text-align:right;">
2.27
</td>
<td style="text-align:right;">
2.72
</td>
<td style="text-align:right;">
1.97
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
-0.29
</td>
<td style="text-align:right;">
-0.32
</td>
<td style="text-align:right;">
-0.12
</td>
<td style="text-align:right;">
-0.21
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.22
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
-1.16
</td>
<td style="text-align:right;">
-1.67
</td>
<td style="text-align:right;">
-0.54
</td>
<td style="text-align:right;">
-0.95
</td>
<td style="text-align:right;">
-0.47
</td>
<td style="text-align:right;">
-0.24
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
2.21
</td>
</tr>
</tbody>
</table>
<p>Perhaps unsurprisingly, the patterns are essentially the same as in the case of using only NYSE stocks to calculate breakpoints.</p>
<p>This positive relation between the book-to-market ratio and expected stock returns is known as the value premium as stocks with high BM ratios (value stocks) tend to outperform stocks with low BM ratios (growth stocks).</p>
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
    filter(!is.na(ret_adj_f1) &amp; !is.na(size) &amp; !is.na(beta) &amp; !is.na(bm) &amp; !is.na(log_bm)) %&gt;%
    group_by(date) %&gt;%
    mutate_at(vars(size, beta, bm, log_bm), ~winsorize(., cut = cut)) %&gt;%
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
}</code></pre>
<p>The following table provides the regression results for various specifications.</p>
<pre class="r"><code>m1 &lt;- fama_macbeth_regression(crsp, ret_adj_f1 ~ bm) %&gt;% rename(m1 = value)
m2 &lt;- fama_macbeth_regression(crsp, ret_adj_f1 ~ bm + beta) %&gt;% rename(m2 = value)
m3 &lt;- fama_macbeth_regression(crsp, ret_adj_f1 ~ bm + size) %&gt;% rename(m3 = value)
m4 &lt;- fama_macbeth_regression(crsp, ret_adj_f1 ~ bm + beta + size) %&gt;% rename(m4 = value)
m5 &lt;- fama_macbeth_regression(crsp, ret_adj_f1 ~ log_bm) %&gt;% rename(m5 = value)
m6 &lt;- fama_macbeth_regression(crsp, ret_adj_f1 ~ log_bm + beta) %&gt;% rename(m6 = value)
m7 &lt;- fama_macbeth_regression(crsp, ret_adj_f1 ~ log_bm + size) %&gt;% rename(m7 = value)
m8 &lt;- fama_macbeth_regression(crsp, ret_adj_f1 ~ log_bm + beta + size) %&gt;% rename(m8 = value)

regression_table &lt;- list(m1, m2, m3, m4, m5, m6, m7, m8) %&gt;% 
  reduce(full_join, by = &quot;statistic&quot;) %&gt;%
  right_join(tibble(statistic = c(&quot;bm coefficient&quot;, &quot;bm nw_t_stat&quot;,
                                  &quot;log_bm coefficient&quot;, &quot;log_bm nw_t_stat&quot;,
                                  &quot;beta coefficient&quot;, &quot;beta nw_t_stat&quot;,
                                  &quot;size coefficient&quot;,  &quot;size nw_t_stat&quot;,
                                  &quot;(Intercept) coefficient&quot;, &quot;(Intercept) nw_t_stat&quot;, 
                                  &quot;adj_r_squared&quot;, &quot;n&quot;)), by = &quot;statistic&quot;)

regression_table %&gt;%
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;)) %&gt;%
  pack_rows(&quot;coefficient estimates&quot;, 1, 10) %&gt;%
  pack_rows(&quot;summary statistics&quot;, 11, 12) </code></pre>
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
<th style="text-align:right;">
m5
</th>
<th style="text-align:right;">
m6
</th>
<th style="text-align:right;">
m7
</th>
<th style="text-align:right;">
m8
</th>
</tr>
</thead>
<tbody>
<tr grouplength="10">
<td colspan="9" style="border-bottom: 1px solid;">
<strong>coefficient estimates</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
bm coefficient
</td>
<td style="text-align:right;">
0.37
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.19
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
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
bm nw_t_stat
</td>
<td style="text-align:right;">
5.05
</td>
<td style="text-align:right;">
4.71
</td>
<td style="text-align:right;">
3.29
</td>
<td style="text-align:right;">
3.10
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
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
log_bm coefficient
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
0.39
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.24
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
log_bm nw_t_stat
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
5.87
</td>
<td style="text-align:right;">
5.78
</td>
<td style="text-align:right;">
3.72
</td>
<td style="text-align:right;">
3.97
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
beta coefficient
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.20
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.07
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.15
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.02
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
beta nw_t_stat
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-1.62
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.42
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-1.34
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.16
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
size coefficient
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.12
</td>
<td style="text-align:right;">
-0.12
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.12
</td>
<td style="text-align:right;">
-0.12
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
size nw_t_stat
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-2.67
</td>
<td style="text-align:right;">
-2.25
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-2.51
</td>
<td style="text-align:right;">
-2.25
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
(Intercept) coefficient
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
1.33
</td>
<td style="text-align:right;">
1.44
</td>
<td style="text-align:right;">
1.20
</td>
<td style="text-align:right;">
1.30
</td>
<td style="text-align:right;">
1.66
</td>
<td style="text-align:right;">
1.68
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
(Intercept) nw_t_stat
</td>
<td style="text-align:right;">
2.14
</td>
<td style="text-align:right;">
2.95
</td>
<td style="text-align:right;">
2.57
</td>
<td style="text-align:right;">
3.06
</td>
<td style="text-align:right;">
3.57
</td>
<td style="text-align:right;">
3.88
</td>
<td style="text-align:right;">
3.36
</td>
<td style="text-align:right;">
3.68
</td>
</tr>
<tr grouplength="2">
<td colspan="9" style="border-bottom: 1px solid;">
<strong>summary statistics</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
adj_r_squared
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
n
</td>
<td style="text-align:right;">
3150.26
</td>
<td style="text-align:right;">
3150.26
</td>
<td style="text-align:right;">
3150.26
</td>
<td style="text-align:right;">
3150.26
</td>
<td style="text-align:right;">
3150.26
</td>
<td style="text-align:right;">
3150.26
</td>
<td style="text-align:right;">
3150.26
</td>
<td style="text-align:right;">
3150.26
</td>
</tr>
</tbody>
</table>
<p>Across all specifications, I find a strong positive relation between BM and future excess returns, regardless of which set of controls is included or whether BM or its log-transformed version is used. Consistent with results from earlier posts, I detect no relation between beta and future stock returns and a strong negative relation between firm size and future stock returns. These results provide strong evidence of a value premium and no evidence that this premium is a manifestation of the relations between beta, market capitalization and future stock returns.</p>

</dl>
