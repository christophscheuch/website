---
title: 'Tidy Asset Pricing - Part IV: The Value Premium'
subtitle: 
summary: 'On the relation of book-to-market ratio and future stock returns'
authors:
- admin
tags:
- Asset Pricing
categories:
- Asset Pricing
date: "2020-02-26T00:00:00Z"
lastmod: "2020-02-26T00:00:00Z"
featured: false
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

<p><b>Update 2022-02-11: check out our new book <a href="https://tidy-finance.org/">Tidy Finance with R</a> for an updated and improved implementation of the content below. </b></p>

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
856.15
</td>
<td style="text-align:right;">
4097.58
</td>
<td style="text-align:right;">
16.99
</td>
<td style="text-align:right;">
453.04
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
4.94
</td>
<td style="text-align:right;">
27.75
</td>
<td style="text-align:right;">
98.09
</td>
<td style="text-align:right;">
384.36
</td>
<td style="text-align:right;">
3227.64
</td>
<td style="text-align:right;">
99261.72
</td>
<td style="text-align:right;">
3779.89
</td>
</tr>
<tr>
<td style="text-align:left;">
bm
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
1.30
</td>
<td style="text-align:right;">
13.48
</td>
<td style="text-align:right;">
476.48
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.41
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
1.14
</td>
<td style="text-align:right;">
2.25
</td>
<td style="text-align:right;">
45.29
</td>
<td style="text-align:right;">
3779.89
</td>
</tr>
<tr>
<td style="text-align:left;">
log_bm
</td>
<td style="text-align:right;">
-0.48
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
-0.73
</td>
<td style="text-align:right;">
5.72
</td>
<td style="text-align:right;">
-6.41
</td>
<td style="text-align:right;">
-2.00
</td>
<td style="text-align:right;">
-0.95
</td>
<td style="text-align:right;">
-0.38
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
3.30
</td>
<td style="text-align:right;">
3779.84
</td>
</tr>
<tr>
<td style="text-align:left;">
me
</td>
<td style="text-align:right;">
1801.58
</td>
<td style="text-align:right;">
8290.59
</td>
<td style="text-align:right;">
15.38
</td>
<td style="text-align:right;">
349.23
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
8.62
</td>
<td style="text-align:right;">
46.20
</td>
<td style="text-align:right;">
187.48
</td>
<td style="text-align:right;">
781.51
</td>
<td style="text-align:right;">
6892.07
</td>
<td style="text-align:right;">
212293.66
</td>
<td style="text-align:right;">
3779.89
</td>
</tr>
<tr>
<td style="text-align:left;">
mktcap
</td>
<td style="text-align:right;">
1941.59
</td>
<td style="text-align:right;">
8921.13
</td>
<td style="text-align:right;">
15.16
</td>
<td style="text-align:right;">
338.47
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
8.21
</td>
<td style="text-align:right;">
46.79
</td>
<td style="text-align:right;">
198.64
</td>
<td style="text-align:right;">
853.28
</td>
<td style="text-align:right;">
7512.70
</td>
<td style="text-align:right;">
229728.20
</td>
<td style="text-align:right;">
3766.14
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
0.51
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
0.00
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
-0.10
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
0.96
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
-0.18
</td>
<td style="text-align:right;">
-0.29
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.35
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
-0.09
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.35
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
0.34
</td>
<td style="text-align:right;">
0.46
</td>
<td style="text-align:right;">
0.57
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
1.09
</td>
<td style="text-align:right;">
1.34
</td>
<td style="text-align:right;">
2.68
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Log(BM)
</td>
<td style="text-align:right;">
-1.98
</td>
<td style="text-align:right;">
-1.15
</td>
<td style="text-align:right;">
-0.84
</td>
<td style="text-align:right;">
-0.63
</td>
<td style="text-align:right;">
-0.45
</td>
<td style="text-align:right;">
-0.29
</td>
<td style="text-align:right;">
-0.13
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.79
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
MktCap
</td>
<td style="text-align:right;">
3303.06
</td>
<td style="text-align:right;">
3008.75
</td>
<td style="text-align:right;">
2590.34
</td>
<td style="text-align:right;">
2031.89
</td>
<td style="text-align:right;">
1768.70
</td>
<td style="text-align:right;">
1595.45
</td>
<td style="text-align:right;">
1276.90
</td>
<td style="text-align:right;">
1132.78
</td>
<td style="text-align:right;">
915.48
</td>
<td style="text-align:right;">
618.58
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Number of Stocks
</td>
<td style="text-align:right;">
555.87
</td>
<td style="text-align:right;">
381.03
</td>
<td style="text-align:right;">
344.00
</td>
<td style="text-align:right;">
326.27
</td>
<td style="text-align:right;">
323.71
</td>
<td style="text-align:right;">
325.71
</td>
<td style="text-align:right;">
330.87
</td>
<td style="text-align:right;">
350.88
</td>
<td style="text-align:right;">
378.26
</td>
<td style="text-align:right;">
473.68
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
% NYSE Stocks
</td>
<td style="text-align:right;">
30.89
</td>
<td style="text-align:right;">
38.77
</td>
<td style="text-align:right;">
42.16
</td>
<td style="text-align:right;">
43.75
</td>
<td style="text-align:right;">
44.36
</td>
<td style="text-align:right;">
44.58
</td>
<td style="text-align:right;">
43.75
</td>
<td style="text-align:right;">
41.80
</td>
<td style="text-align:right;">
39.25
</td>
<td style="text-align:right;">
32.43
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Beta
</td>
<td style="text-align:right;">
1.07
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
0.82
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
0.68
</td>
<td style="text-align:right;">
0.63
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
0.29
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
1.14
</td>
<td style="text-align:right;">
1.45
</td>
<td style="text-align:right;">
2.95
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Log(BM)
</td>
<td style="text-align:right;">
-2.20
</td>
<td style="text-align:right;">
-1.31
</td>
<td style="text-align:right;">
-0.95
</td>
<td style="text-align:right;">
-0.69
</td>
<td style="text-align:right;">
-0.48
</td>
<td style="text-align:right;">
-0.29
</td>
<td style="text-align:right;">
-0.11
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.87
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
MktCap
</td>
<td style="text-align:right;">
3178.81
</td>
<td style="text-align:right;">
3136.80
</td>
<td style="text-align:right;">
2738.19
</td>
<td style="text-align:right;">
2105.06
</td>
<td style="text-align:right;">
1787.95
</td>
<td style="text-align:right;">
1479.09
</td>
<td style="text-align:right;">
1228.90
</td>
<td style="text-align:right;">
1063.14
</td>
<td style="text-align:right;">
880.77
</td>
<td style="text-align:right;">
603.66
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Number of Stocks
</td>
<td style="text-align:right;">
379.38
</td>
<td style="text-align:right;">
378.87
</td>
<td style="text-align:right;">
379.04
</td>
<td style="text-align:right;">
378.82
</td>
<td style="text-align:right;">
378.81
</td>
<td style="text-align:right;">
379.19
</td>
<td style="text-align:right;">
378.90
</td>
<td style="text-align:right;">
378.85
</td>
<td style="text-align:right;">
378.96
</td>
<td style="text-align:right;">
379.47
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
% NYSE Stocks
</td>
<td style="text-align:right;">
28.50
</td>
<td style="text-align:right;">
35.84
</td>
<td style="text-align:right;">
40.43
</td>
<td style="text-align:right;">
42.75
</td>
<td style="text-align:right;">
43.66
</td>
<td style="text-align:right;">
44.23
</td>
<td style="text-align:right;">
41.99
</td>
<td style="text-align:right;">
40.58
</td>
<td style="text-align:right;">
37.64
</td>
<td style="text-align:right;">
31.47
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Beta
</td>
<td style="text-align:right;">
1.08
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.63
</td>
</tr>
</tbody>
</table>
<p>By construction, the average BM ratio increases across portfolios. Market capitalization, betas and the number of stocks in each portfolio decrease across portfolios. These patterns are the same regardless of which breakpoints are used.</p>
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
0.29
</td>
<td style="text-align:right;">
0.57
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.82
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.12
</td>
<td style="text-align:right;">
1.28
</td>
<td style="text-align:right;">
0.99
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
2.13
</td>
<td style="text-align:right;">
2.64
</td>
<td style="text-align:right;">
3.04
</td>
<td style="text-align:right;">
3.37
</td>
<td style="text-align:right;">
3.76
</td>
<td style="text-align:right;">
3.97
</td>
<td style="text-align:right;">
4.18
</td>
<td style="text-align:right;">
4.45
</td>
<td style="text-align:right;">
4.13
</td>
<td style="text-align:right;">
5.30
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
-0.43
</td>
<td style="text-align:right;">
-0.08
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.41
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.59
</td>
<td style="text-align:right;">
0.73
</td>
<td style="text-align:right;">
1.16
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
-2.67
</td>
<td style="text-align:right;">
-0.64
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
1.40
</td>
<td style="text-align:right;">
2.03
</td>
<td style="text-align:right;">
2.85
</td>
<td style="text-align:right;">
3.27
</td>
<td style="text-align:right;">
3.78
</td>
<td style="text-align:right;">
4.14
</td>
<td style="text-align:right;">
3.78
</td>
<td style="text-align:right;">
5.95
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
0.45
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
0.52
</td>
<td style="text-align:right;">
0.57
</td>
<td style="text-align:right;">
0.55
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.82
</td>
<td style="text-align:right;">
0.37
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
2.11
</td>
<td style="text-align:right;">
3.18
</td>
<td style="text-align:right;">
2.82
</td>
<td style="text-align:right;">
3.06
</td>
<td style="text-align:right;">
3.04
</td>
<td style="text-align:right;">
3.35
</td>
<td style="text-align:right;">
3.34
</td>
<td style="text-align:right;">
3.58
</td>
<td style="text-align:right;">
4.13
</td>
<td style="text-align:right;">
3.78
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
-0.11
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.41
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
-1.24
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
1.52
</td>
<td style="text-align:right;">
1.80
</td>
<td style="text-align:right;">
1.73
</td>
<td style="text-align:right;">
2.76
</td>
<td style="text-align:right;">
2.33
</td>
<td style="text-align:right;">
2.08
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
0.17
</td>
<td style="text-align:right;">
0.46
</td>
<td style="text-align:right;">
0.59
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
1.04
</td>
<td style="text-align:right;">
1.15
</td>
<td style="text-align:right;">
1.33
</td>
<td style="text-align:right;">
1.16
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
0.52
</td>
<td style="text-align:right;">
1.57
</td>
<td style="text-align:right;">
2.31
</td>
<td style="text-align:right;">
2.85
</td>
<td style="text-align:right;">
3.29
</td>
<td style="text-align:right;">
3.76
</td>
<td style="text-align:right;">
3.96
</td>
<td style="text-align:right;">
4.24
</td>
<td style="text-align:right;">
4.52
</td>
<td style="text-align:right;">
4.17
</td>
<td style="text-align:right;">
5.69
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
-0.57
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.52
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
1.35
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
-3.21
</td>
<td style="text-align:right;">
-1.64
</td>
<td style="text-align:right;">
-0.31
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
1.87
</td>
<td style="text-align:right;">
2.85
</td>
<td style="text-align:right;">
3.26
</td>
<td style="text-align:right;">
3.84
</td>
<td style="text-align:right;">
4.25
</td>
<td style="text-align:right;">
3.84
</td>
<td style="text-align:right;">
6.27
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
0.46
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.55
</td>
<td style="text-align:right;">
0.53
</td>
<td style="text-align:right;">
0.55
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
0.80
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.41
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
1.93
</td>
<td style="text-align:right;">
2.16
</td>
<td style="text-align:right;">
2.96
</td>
<td style="text-align:right;">
2.78
</td>
<td style="text-align:right;">
2.93
</td>
<td style="text-align:right;">
3.63
</td>
<td style="text-align:right;">
3.31
</td>
<td style="text-align:right;">
3.77
</td>
<td style="text-align:right;">
4.09
</td>
<td style="text-align:right;">
3.79
</td>
<td style="text-align:right;">
1.93
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
-0.13
</td>
<td style="text-align:right;">
-0.13
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.47
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
-1.15
</td>
<td style="text-align:right;">
-1.95
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
2.10
</td>
<td style="text-align:right;">
1.56
</td>
<td style="text-align:right;">
2.28
</td>
<td style="text-align:right;">
2.45
</td>
<td style="text-align:right;">
2.46
</td>
<td style="text-align:right;">
2.10
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
0.30
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.21
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
4.79
</td>
<td style="text-align:right;">
3.44
</td>
<td style="text-align:right;">
3.37
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
0.38
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.28
</td>
<td style="text-align:right;">
0.25
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
5.73
</td>
<td style="text-align:right;">
5.77
</td>
<td style="text-align:right;">
3.75
</td>
<td style="text-align:right;">
4.08
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
beta coefficient
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.21
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.09
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.16
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
beta nw_t_stat
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-1.69
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.53
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-1.39
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.25
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
-0.11
</td>
<td style="text-align:right;">
-0.11
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-0.10
</td>
<td style="text-align:right;">
-0.11
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
-2.46
</td>
<td style="text-align:right;">
-2.02
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-2.29
</td>
<td style="text-align:right;">
-2.03
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
(Intercept) coefficient
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
1.08
</td>
<td style="text-align:right;">
1.41
</td>
<td style="text-align:right;">
1.51
</td>
<td style="text-align:right;">
1.35
</td>
<td style="text-align:right;">
1.45
</td>
<td style="text-align:right;">
1.74
</td>
<td style="text-align:right;">
1.77
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
(Intercept) nw_t_stat
</td>
<td style="text-align:right;">
3.37
</td>
<td style="text-align:right;">
5.25
</td>
<td style="text-align:right;">
3.21
</td>
<td style="text-align:right;">
3.90
</td>
<td style="text-align:right;">
5.39
</td>
<td style="text-align:right;">
6.63
</td>
<td style="text-align:right;">
4.26
</td>
<td style="text-align:right;">
4.75
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
3730.47
</td>
<td style="text-align:right;">
3730.47
</td>
<td style="text-align:right;">
3730.47
</td>
<td style="text-align:right;">
3730.47
</td>
<td style="text-align:right;">
3730.47
</td>
<td style="text-align:right;">
3730.47
</td>
<td style="text-align:right;">
3730.47
</td>
<td style="text-align:right;">
3730.47
</td>
</tr>
</tbody>
</table>
<p>Across all specifications, I find a strong positive relation between BM and future excess returns, regardless of which set of controls is included or whether BM or its log-transformed version is used. Consistent with results from earlier posts, I detect no relation between beta and future stock returns and a strong negative relation between firm size and future stock returns. These results provide strong evidence of a value premium and no evidence that this premium is a manifestation of the relations between beta, market capitalization and future stock returns.</p>

</dl>
