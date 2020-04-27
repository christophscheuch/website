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
-3.92
</td>
<td style="text-align:right;">
-1.34
</td>
<td style="text-align:right;">
0.26
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
-4.12
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
4.81
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
  mutate(cum_return = cumsum(log(1+return/100))*100) %&gt;%
  ungroup() %&gt;%
  mutate(portfolio = toupper(portfolio),
         portfolio = gsub(&quot;_FF&quot;, &quot; (Fama-French)&quot;, portfolio)) %&gt;%
  ggplot(aes(x = date, y = cum_return)) +
  geom_line(aes(color = portfolio)) +
  labs(x = &quot;&quot;, y = &quot;Cumulative Log Returns (in %)&quot;, color = &quot;Portfolio&quot;) + 
  scale_x_date(expand = c(0, 0), date_breaks = &quot;10 years&quot;, date_labels = &quot;%Y&quot;) +
  theme_classic()</code></pre>
<p><img src="data:image/png;base64,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" width="672" style="display: block; margin: auto;" /></p>
<p>There seems to be a considerable divergence between my factors and the original data over time which is less severe for the <code>smb</code> factor. Please feel free to share any suggestions in the comments below on how to improve the fit. Overall, I am nonetheless somewhat surprised how quickly one can set up a decent replication of the three famous Fama-French factors.</p>

</dl>
