---
title: 'Tidy Asset Pricing - Part V: The Fama-French 3-Factor Model'
subtitle: 
summary: 'A replication effort of the famous Fama-French factors'
authors:
- admin
tags:
- Asset Pricing
categories:
- Asset Pricing
date: "2020-02-28T00:00:00Z"
lastmod: "2020-02-28T00:00:00Z"
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

<p>This note is an effort to replicate the famous <a href="https://en.wikipedia.org/wiki/Fama%E2%80%93French_three-factor_model">Fama-French three-factor model</a>, in particular the three factors that enter its equation: the market factor, the size factor (small firms outperform larger firms), and the value factor (firms with high book-to-market equity ratios outperform firms with low ratios). Throughout this note, I build on earlier posts that explored empirical asset pricing approaches in a similar direction. Namely, <a href="https://christophscheuch.github.io/post/asset-pricing/crsp-sample/">I constructed the CRSP stock return sample</a>, <a href="https://christophscheuch.github.io/post/asset-pricing/beta/">I estimated stock-specific market betas</a>, and <a href="https://christophscheuch.github.io/post/asset-pricing/size/">I analyzed the relation between stock returns and size</a> and <a href="https://christophscheuch.github.io/post/asset-pricing/value/">I replicated the value premium</a>. I keep the description of individual steps very short and refer to these notes for more details.</p>
<p>At the end of this post, I demontrate that my effort yields a decent result: my version of the <code>mkt</code> factor exhibits a 99% correlation with the market factor provided on Kenneth French’s website. Similarly, my <code>smb</code> and <code>hml</code> factors show a 98% and 95% correlation, respectively, with the original data. <a href="https://sites.google.com/site/waynelinchang/home">Wayne Chang</a> provides an R script on his website that supposedly achieves higher correlations. However, I have to admit that I have not yet tried to run his code because I find it very hard to grasp what is going on by just glancing over it. Maybe I’ll devote some more time to his code at a later stage.</p>
<p>The usual disclaimer applies, so the text below references an opinion and is for information purposes only and I do not intend to provide any investment advice. The usual disclaimer applies, so the text below references an opinion and is for information purposes only and I do not intend to provide any investment advice.</p>
<p>I mainly use the following packages throughout this note:</p>
<pre class="r"><code>library(tidyverse)
library(lubridate)  # for working with dates
library(kableExtra) # for nicer html tables</code></pre>
<div id="prepare-crsp-data" class="section level2">
<h2>Prepare CRSP Data</h2>
<p>This note is self-contained in the sense that everything starts from the raw data. First, I need to construct the sample of monthly stock returns. The code chunk below exhibits two differences to the <a href="https://christophscheuch.github.io/post/asset-pricing/crsp-sample/">earlier post</a>: I drop observations before 1962 as most papers in empirical asset pricing start in 1963 due to data availability; I drop stocks that are listed on exchanges other than NYSE, AMEX or NASDAQ since Fama and French stress that they focus their seminal paper on these stocks.</p>
<pre class="r"><code># read raw monthly crsp file
crspa_msf &lt;- read_csv(&quot;raw/crspa_msf.csv&quot;)
colnames(crspa_msf) &lt;- tolower(colnames(crspa_msf))

# parse only relevant variables
crsp &lt;- crspa_msf %&gt;%
  transmute(permno = as.integer(permno),     # security identifier
            date = ymd(date),                # month identifier
            ret = as.numeric(ret) * 100,     # return (converted to percent)
            shrout = as.numeric(shrout),     # shares outstanding (in thousands)
            altprc = as.numeric(altprc),     # last traded price in a month
            exchcd = as.integer(exchcd),     # exchange code
            shrcd = as.integer(shrcd),       # share code
            siccd = as.integer(siccd),       # industry code
            dlret = as.numeric(dlret) * 100, # delisting return (converted to percent)
            dlstcd = as.integer(dlstcd)      # delisting code
  ) 

# analysis of fama french (1993) starts in 1963 so I only need data after 1962
crsp &lt;- crsp %&gt;%
  filter(date &gt;= &quot;1962-01-01&quot;)

# keep only US-based common stocks (10 and 11)
crsp &lt;- crsp %&gt;%
  filter(shrcd %in% c(10, 11)) %&gt;%
  select(-shrcd)

# keep only distinct obervations to avoid multiple counting
crsp &lt;- crsp %&gt;%
  distinct()

# compute market cap
# note: altprc is the negative of average of bid and ask from last traded price
#       for which the data is available if there is no last traded price
crsp &lt;- crsp %&gt;%
  mutate(mktcap = abs(shrout * altprc) / 1000, # in millions of dollars
         mktcap = if_else(mktcap == 0, as.numeric(NA), mktcap)) 

# define exchange labels and keep only NYSE, AMEX and NASDAQ stocks
crsp &lt;- crsp %&gt;%
  mutate(exchange = case_when(exchcd %in% c(1, 31) ~ &quot;NYSE&quot;,
                              exchcd %in% c(2, 32) ~ &quot;AMEX&quot;,
                              exchcd %in% c(3, 33) ~ &quot;NASDAQ&quot;,
                              TRUE ~ &quot;Other&quot;)) %&gt;%
  filter(exchange != &quot;Other&quot;)

# adjust delisting returns (see shumway, 1997)
crsp &lt;- crsp %&gt;%
  mutate(ret_adj = case_when(!is.na(dlret) ~ dlret,
                             is.na(dlret) &amp; !is.na(dlstcd) ~ -100,
                             is.na(dlret) &amp; (dlstcd %in% c(500, 520, 580, 584) | 
                                               (dlstcd &gt;= 551 &amp; dlstcd &lt;= 574)) ~ -30,
                             TRUE ~ ret)) %&gt;%
  select(-c(dlret, dlstcd))</code></pre>
</div>
<div id="prepare-crspcompustat-merged-data" class="section level2">
<h2>Prepare CRSP/Compustat Merged Data</h2>
<p>The next code chunk is explained in <a href="https://christophscheuch.github.io/post/asset-pricing/value/">this post</a> with two additions: I do not include firms until they have appeared in Compustat for two years, just as in <a href="https://www.sciencedirect.com/science/article/abs/pii/0304405X93900235">Fama and French (1993)</a>. However, this extra filter only has a negligible impact on the overall results. Moreover, the construction of book equity is now based on the <a href="https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/variable_definitions.html">instructions provided on Kenneth French’s website</a> as opposed to the slightly simpler implementation in <a href="https://www.wiley.com/en-us/Empirical+Asset+Pricing%3A+The+Cross+Section+of+Stock+Returns-p-9781118095041">Bali, Engle and Murray (2016)</a>.</p>
<pre class="r"><code># read raw crsp/compustat merged file
ccmfunda &lt;- read_csv(&quot;raw/ccmfunda.csv&quot;)
colnames(ccmfunda) &lt;- tolower(colnames(ccmfunda))

# select and parse relevant variables
compustat &lt;- ccmfunda %&gt;%
  transmute(
    gvkey = as.integer(gvkey),         # firm identifier
    permno = as.integer(lpermno),      # stock identifier
    datadate = ymd(datadate),          # date of report
    linktype = as.character(linktype), # link type
    linkenddt = ymd(linkenddt),        # date when link ends to be valid
    seq = as.numeric(seq),             # stockholders&#39; equity
    ceq = as.numeric(ceq),             # total common/ordinary equity
    at = as.numeric(at),               # total assets
    lt = as.numeric(lt),               # total liabilities
    txditc = as.numeric(txditc),       # deferred taxes and investment tax credit
    txdb = as.numeric(txdb),           # deferred taxes
    itcb = as.numeric(itcb),           # investment tax credit
    pstkrv = as.numeric(pstkrv),       # preferred stock redemption value
    pstkl = as.numeric(pstkl),         # preferred stock liquidating value
    pstk = as.numeric(pstk),           # preferred stock par value
    indfmt = as.character(indfmt),     # industry format
    datafmt = as.character(datafmt)    # data format
  ) 

# make sure that only correct industry and data format is used
compustat &lt;- compustat %&gt;%
  filter(indfmt == &quot;INDL&quot; &amp; datafmt == &quot;STD&quot;) %&gt;%
  select(-c(indfmt, datafmt))

# check that only valid links are used
compustat &lt;- compustat %&gt;%
  filter(linktype %in% c(&quot;LU&quot;, &quot;LC&quot;))

# check that links are still active at time of information release
compustat &lt;- compustat %&gt;%
  filter(datadate &lt;= linkenddt | is.na(linkenddt))

# keep only distinct observations
compustat &lt;- compustat %&gt;%
  distinct()

# calculate book value of preferred stock and equity
compustat &lt;- compustat %&gt;%
  mutate(be = coalesce(seq, ceq + pstk, at - lt) + 
           coalesce(txditc, txdb + itcb, 0) - 
           coalesce(pstkrv, pstkl, pstk, 0),
         be = if_else(be &lt; 0, as.numeric(NA), be)) %&gt;%
  select(gvkey, permno, datadate, be) 

# determine year for matching and keep only one obs per year
compustat &lt;- compustat %&gt;%
  mutate(year = year(datadate)) %&gt;%
  group_by(permno, year) %&gt;%
  filter(datadate == max(datadate)) %&gt;%
  ungroup()

# keep only observations once a firm has been included for two years
compustat &lt;- compustat %&gt;%
  group_by(gvkey) %&gt;%
  mutate(first_year = min(year)) %&gt;%
  ungroup() %&gt;%
  filter(year &gt; first_year + 1) %&gt;%
  select(-gvkey)

# kick out unnecessary rows with missing values
compustat &lt;- compustat %&gt;%
  na.omit()

# determine reference date for matching (june of next calendar year)
compustat &lt;- compustat %&gt;%
  mutate(reference_date = ymd(paste0(year + 1, &quot;-06-01&quot;))) %&gt;%
  select(-year)</code></pre>
</div>
<div id="construct-stock-sample" class="section level2">
<h2>Construct Stock Sample</h2>
<p>The stock sample used for the construction of the factors also builds on <a href="https://christophscheuch.github.io/post/asset-pricing/value/">this post</a>. Note that the relevant returns for the factor construction are the adjusted returns in the current month. In the end, only stocks that have valid book equity data from year <span class="math inline">\(y-1\)</span>, market equity data at the end of year <span class="math inline">\(y-1\)</span> and market capitalization in June of year <span class="math inline">\(y\)</span> enter the factor construction procedure. Moreover, the market capitalization of June of year <span class="math inline">\(y\)</span> is used to construct value-weighted returns until May of year <span class="math inline">\(y+1\)</span>. I also restrict the sample to start in 1963, consistent with Fama and French.</p>
<pre class="r"><code># select relevant variables
stocks &lt;- crsp %&gt;%
  select(permno, date, exchange, ret = ret_adj, mktcap) %&gt;%
  na.omit()

# define reference date for each stock (i.e. new sorting starts in june of year y)
stocks &lt;- stocks %&gt;%
  mutate(reference_date = ymd(if_else(month(date) &lt; 6, 
                                      paste0(year(date) - 1, &quot;-06-01&quot;), 
                                      paste0(year(date), &quot;-06-01&quot;)))) 

# add book equity data for year y-1 which is used starting in june of year y
stocks_be &lt;- read_rds(&quot;data/compustat.rds&quot;) %&gt;%
  mutate(reference_date = ymd(paste0(year(datadate) + 1, &quot;-06-01&quot;))) 

stocks &lt;- stocks %&gt;%
  left_join(stocks_be, by = c(&quot;permno&quot;, &quot;reference_date&quot;))

# add market equity data from the end of year y-1 which is used for bm ratio in june of year y
stocks_me &lt;- stocks %&gt;%
  filter(month(date) == 12) %&gt;%
  mutate(reference_date = ymd(paste0(year(date) + 1, &quot;-06-01&quot;))) %&gt;%
  select(permno, reference_date, me = mktcap)

stocks &lt;- stocks %&gt;%
  left_join(stocks_me, by = c(&quot;permno&quot;, &quot;reference_date&quot;))

# compute book-to-market ratio
stocks &lt;- stocks %&gt;%
  mutate(bm = be / me)

# add market cap of june of year y which is used for value-weighted returns
stocks_weight &lt;- stocks %&gt;%
  filter(month(date) == 6) %&gt;%
  select(permno, reference_date, mktcap_weight = mktcap)

stocks &lt;- stocks %&gt;%
  left_join(stocks_weight, by = c(&quot;permno&quot;, &quot;reference_date&quot;))

# only keep stocks that have all the necessary data 
# (i.e. market equity data for december y-1 and june y, and book equity data for y-1)
stocks &lt;- stocks %&gt;%
  na.omit() %&gt;%
  filter(date &gt;= &quot;1963-01-01&quot;) # fama-french paper starts here</code></pre>
</div>
<div id="size-sorts" class="section level2">
<h2>Size Sorts</h2>
<p>To construct the size portfolios, Fama and French independently sort all stocks into two portfolios according to the median of all NYSE stocks in the June of each year. Portfolios are then kept in these portfolios until June of the following year. This is exactly what happens in the code chunk below.</p>
<pre class="r"><code># in june of each year all NYSE stocks are ranked on size to get the median
size_breakpoints &lt;- stocks %&gt;%
  filter(month(date) == 6 &amp; exchange == &quot;NYSE&quot;) %&gt;%
  select(reference_date, mktcap) %&gt;%
  group_by(reference_date) %&gt;%
  summarize(size_median = median(mktcap))

# also in june all stocks are sorted into 2 portfolios
size_sorts &lt;- stocks %&gt;%
  filter(month(date) == 6) %&gt;%
  left_join(size_breakpoints, by = &quot;reference_date&quot;) %&gt;%
  mutate(size_portfolio = case_when(mktcap &gt; size_median ~ &quot;B&quot;,
                                    mktcap &lt;= size_median ~ &quot;S&quot;,
                                    TRUE ~ as.character(NA))) %&gt;%
  select(permno, reference_date, size_portfolio)

# add size portfolio assignment back to stock data
stocks &lt;- stocks %&gt;% 
  left_join(size_sorts, by = c(&quot;permno&quot;, &quot;reference_date&quot;))</code></pre>
<p>Now each stock in my sample received an assignment to a size portfolio of either big <code>&quot;B&quot;</code> or small <code>&quot;S&quot;</code>.</p>
</div>
<div id="value-sorts" class="section level2">
<h2>Value Sorts</h2>
<p>Fama and French determine the value portfolios based on 30% and 70% breakpoints again using NYSE stocks only in June of each year. Each stock is then independently sorted into either high BM <code>&quot;H&quot;</code>, medium bm <code>&quot;M&quot;</code> or low bm <code>&quot;L&quot;</code>.</p>
<pre class="r"><code># calculate value breakpoints using NYSE stocks
value_breakpoints &lt;- stocks %&gt;%
  filter(month(date) == 6 &amp; exchange == &quot;NYSE&quot;) %&gt;%
  select(reference_date, bm) %&gt;%
  group_by(reference_date) %&gt;%
  summarize(value_q30 = quantile(bm, 0.3),
            value_q70 = quantile(bm, 0.7))

# also in june all stocks are sorted into 3 portfolios
value_sorts &lt;- stocks %&gt;%
  filter(month(date) == 6) %&gt;%
  left_join(value_breakpoints, by = &quot;reference_date&quot;) %&gt;%
  mutate(value_portfolio = case_when(bm &gt; value_q70 ~ &quot;H&quot;,
                                     bm &lt;= value_q70 &amp; bm &gt; value_q30 ~ &quot;M&quot;, 
                                     bm &lt;= value_q30 ~ &quot;L&quot;,
                                     TRUE ~ as.character(NA))) %&gt;%
  select(permno, reference_date, value_portfolio)

# add value portfolio assignment back to stock data
stocks &lt;- stocks %&gt;% 
  left_join(value_sorts, by = c(&quot;permno&quot;, &quot;reference_date&quot;))</code></pre>
</div>
<div id="construct-factor-portfolios" class="section level2">
<h2>Construct Factor Portfolios</h2>
<p>To construct the <code>smb</code> and <code>hml</code> portfolios, I simply compute value-weighted returns for all combinations of value and size portfolios and then take the difference of the resulting portfolios as described by Fama and French.</p>
<pre class="r"><code>portfolios &lt;- stocks %&gt;%
  group_by(date, size_portfolio, value_portfolio) %&gt;%
  summarize(ret_vw = weighted.mean(ret, mktcap_weight)) %&gt;%
  ungroup() %&gt;%
  mutate(portfolio = paste0(size_portfolio, &quot;/&quot;, value_portfolio))

factors &lt;- portfolios %&gt;%
  group_by(date) %&gt;%
  summarize(smb = mean(ret_vw[portfolio %in% c(&quot;S/H&quot;, &quot;S/M&quot;, &quot;S/L&quot;)]) - 
              mean(ret_vw[portfolio %in% c(&quot;B/H&quot;, &quot;B/M&quot;, &quot;B/L&quot;)]),
            hml = mean(ret_vw[portfolio %in% c(&quot;S/H&quot;, &quot;B/H&quot;)]) - 
              mean(ret_vw[portfolio %in% c(&quot;S/L&quot;, &quot;B/L&quot;)]))</code></pre>
<p>Next, I also add the <code>mkt</code> factor as the monthly weighted average return across all stocks. I kick out the last month in my sample as it exhibits about a close to -100% return for whatever reason.</p>
<pre class="r"><code>factors &lt;- factors %&gt;%
  left_join(stocks %&gt;%
              group_by(date) %&gt;%
              summarize(mkt = weighted.mean(ret, mktcap_weight)), by = &quot;date&quot;) %&gt;%
  select(date, mkt, smb, hml) %&gt;% # rearrange columns
  filter(date &lt; &quot;2018-12-01&quot;) # something weird happens here with &#39;mkt&#39; at end of sample period</code></pre>
<p>Finally, I also add the corresponding factors from <a href="https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html">Ken French’s website</a>.</p>
<pre class="r"><code>factors_ff &lt;- read_csv(&quot;raw/F-F_Research_Data_Factors.csv&quot;, skip = 3)

factors_ff &lt;- factors_ff %&gt;%
  transmute(date = ymd(paste0(X1, &quot;01&quot;)),
            rf_ff = as.numeric(RF),
            mkt_ff = as.numeric(`Mkt-RF`) + as.numeric(RF),
            smb_ff = as.numeric(SMB),
            hml_ff = as.numeric(HML)) %&gt;%
  filter(date &lt;= max(crsp$date)) %&gt;%
  mutate(date = floor_date(date, &quot;month&quot;)) %&gt;%
  select(-rf_ff)

factors &lt;- factors %&gt;%
  mutate(date = floor_date(date, &quot;month&quot;)) %&gt;%
  left_join(factors_ff, by = &quot;date&quot;)</code></pre>
</div>
<div id="replication-results" class="section level2">
<h2>Replication Results</h2>
<p>In the last section, I analyze how well I am able to replicate the original Fama-French factors. First, let us look at summary statistics across all 666 months.</p>
<pre class="r"><code>factors %&gt;%
  select(-date) %&gt;%
  pivot_longer(cols = everything(), names_to = &quot;measure&quot;) %&gt;%
  group_by(measure) %&gt;%
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
  ungroup() %&gt;%
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
hml
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
2.98
</td>
<td style="text-align:right;">
-0.08
</td>
<td style="text-align:right;">
7.58
</td>
<td style="text-align:right;">
-15.60
</td>
<td style="text-align:right;">
-3.93
</td>
<td style="text-align:right;">
-1.34
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
1.67
</td>
<td style="text-align:right;">
4.98
</td>
<td style="text-align:right;">
16.72
</td>
<td style="text-align:right;">
666
</td>
</tr>
<tr>
<td style="text-align:left;">
hml_ff
</td>
<td style="text-align:right;">
0.33
</td>
<td style="text-align:right;">
2.80
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
5.10
</td>
<td style="text-align:right;">
-11.18
</td>
<td style="text-align:right;">
-3.87
</td>
<td style="text-align:right;">
-1.21
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
1.71
</td>
<td style="text-align:right;">
5.20
</td>
<td style="text-align:right;">
12.87
</td>
<td style="text-align:right;">
666
</td>
</tr>
<tr>
<td style="text-align:left;">
mkt
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
4.44
</td>
<td style="text-align:right;">
-0.41
</td>
<td style="text-align:right;">
5.08
</td>
<td style="text-align:right;">
-22.10
</td>
<td style="text-align:right;">
-6.28
</td>
<td style="text-align:right;">
-1.47
</td>
<td style="text-align:right;">
1.15
</td>
<td style="text-align:right;">
3.71
</td>
<td style="text-align:right;">
7.64
</td>
<td style="text-align:right;">
17.50
</td>
<td style="text-align:right;">
666
</td>
</tr>
<tr>
<td style="text-align:left;">
mkt_ff
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
4.36
</td>
<td style="text-align:right;">
-0.51
</td>
<td style="text-align:right;">
5.03
</td>
<td style="text-align:right;">
-22.64
</td>
<td style="text-align:right;">
-6.45
</td>
<td style="text-align:right;">
-1.65
</td>
<td style="text-align:right;">
1.22
</td>
<td style="text-align:right;">
3.71
</td>
<td style="text-align:right;">
7.42
</td>
<td style="text-align:right;">
16.61
</td>
<td style="text-align:right;">
666
</td>
</tr>
<tr>
<td style="text-align:left;">
smb
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
3.02
</td>
<td style="text-align:right;">
0.52
</td>
<td style="text-align:right;">
5.58
</td>
<td style="text-align:right;">
-12.11
</td>
<td style="text-align:right;">
-4.11
</td>
<td style="text-align:right;">
-1.57
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
2.07
</td>
<td style="text-align:right;">
4.84
</td>
<td style="text-align:right;">
15.99
</td>
<td style="text-align:right;">
666
</td>
</tr>
<tr>
<td style="text-align:left;">
smb_ff
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
3.06
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
8.41
</td>
<td style="text-align:right;">
-16.86
</td>
<td style="text-align:right;">
-4.24
</td>
<td style="text-align:right;">
-1.51
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
2.02
</td>
<td style="text-align:right;">
4.87
</td>
<td style="text-align:right;">
21.70
</td>
<td style="text-align:right;">
666
</td>
</tr>
</tbody>
</table>
<p>The distributions of my factors and the original ones seem to be quite similar for all three factors, but still far from perfect. Kolmogorov-Smirnov tests confirm that there is no statistically significant difference in the distributions.</p>
<pre class="r"><code># kolmogorov-smirnov test
ks.test(factors$mkt, factors$mkt_ff)</code></pre>
<pre><code>## 
##  Two-sample Kolmogorov-Smirnov test
## 
## data:  factors$mkt and factors$mkt_ff
## D = 0.016517, p-value = 1
## alternative hypothesis: two-sided</code></pre>
<pre class="r"><code>ks.test(factors$smb, factors$smb_ff)</code></pre>
<pre><code>## 
##  Two-sample Kolmogorov-Smirnov test
## 
## data:  factors$smb and factors$smb_ff
## D = 0.022523, p-value = 0.9959
## alternative hypothesis: two-sided</code></pre>
<pre class="r"><code>ks.test(factors$hml, factors$hml_ff)</code></pre>
<pre><code>## 
##  Two-sample Kolmogorov-Smirnov test
## 
## data:  factors$hml and factors$hml_ff
## D = 0.028529, p-value = 0.9492
## alternative hypothesis: two-sided</code></pre>
<p>I am even more interested in the correlations of the factors, which I provide in the next table.</p>
<pre class="r"><code>compute_correlations &lt;- function(data, ..., upper = TRUE) {
  cor_matrix &lt;- data %&gt;%
    select(...) %&gt;%
    cor(use = &quot;complete.obs&quot;, method = &quot;pearson&quot;) 
  if (upper == TRUE) {
    cor_matrix[upper.tri(cor_matrix, diag = FALSE)] &lt;- NA
  }
  return(cor_matrix)
}
compute_correlations(factors, mkt, mkt_ff, smb, smb_ff, hml, hml_ff) %&gt;% 
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;))</code></pre>
<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
mkt
</th>
<th style="text-align:right;">
mkt_ff
</th>
<th style="text-align:right;">
smb
</th>
<th style="text-align:right;">
smb_ff
</th>
<th style="text-align:right;">
hml
</th>
<th style="text-align:right;">
hml_ff
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
mkt
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
mkt_ff
</td>
<td style="text-align:right;">
0.99
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
smb
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.32
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
smb_ff
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.98
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
hml
</td>
<td style="text-align:right;">
-0.29
</td>
<td style="text-align:right;">
-0.29
</td>
<td style="text-align:right;">
-0.20
</td>
<td style="text-align:right;">
-0.22
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
hml_ff
</td>
<td style="text-align:right;">
-0.24
</td>
<td style="text-align:right;">
-0.26
</td>
<td style="text-align:right;">
-0.15
</td>
<td style="text-align:right;">
-0.19
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>
<p>The <code>mkt</code> factor exhibits a correlation of 99% which is a pretty good fit. The <code>smb</code> factor also shows a satisfying fit with 98%. The <code>hml</code> factor only has a correlation of 95% with its original counterpart which might be driven by different sample construction procedures that I am not yet aware of or some ex-post changes in the CRSP or Compustat data.</p>
<p>As a last step, I present the time series of cumulative returns over the full sample period.</p>
<pre class="r"><code>factors %&gt;%
  pivot_longer(-date, names_to = &quot;portfolio&quot;, values_to = &quot;return&quot;) %&gt;%
  group_by(portfolio) %&gt;%
  arrange(date) %&gt;%
  mutate(cum_return = cumsum(return)) %&gt;%
  ungroup() %&gt;%
  mutate(portfolio = toupper(portfolio),
         portfolio = gsub(&quot;_FF&quot;, &quot; (Fama-French)&quot;, portfolio)) %&gt;%
  ggplot(aes(x = date, y = cum_return)) +
  geom_line(aes(color = portfolio)) +
  labs(x = &quot;&quot;, y = &quot;Cumulative Returns (in %)&quot;, color = &quot;Portfolio&quot;) + 
  scale_x_date(expand = c(0, 0), date_breaks = &quot;10 years&quot;, date_labels = &quot;%Y&quot;) +
  theme_classic()</code></pre>
<p><img src="data:image/png;base64,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" width="672" style="display: block; margin: auto;" /></p>
<p>There seems to be a considerable divergence between my factors and the original data over time which is less severe for the <code>smb</code> factor. Please feel free to share any suggestions in the comments below on how to improve the fit. Overall, I am nonetheless somewhat surprised how quickly one can set up a decent replication of the three famous Fama-French factors.</p>


</dl>
