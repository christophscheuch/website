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
lastmod: "2020-11-08T00:00:00Z"
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

<p><strong>Update 2020-11-08: I extended the data to end of 2019 and corrected a mistake in the adjusted returns computation.</strong></p>
<p>This note is an effort to replicate the famous <a href="https://en.wikipedia.org/wiki/Fama%E2%80%93French_three-factor_model">Fama-French three-factor model</a>, in particular the three factors that enter its equation: the market factor, the size factor (small firms outperform larger firms), and the value factor (firms with high book-to-market equity ratios outperform firms with low ratios). Throughout this note, I build on earlier posts that explored empirical asset pricing approaches in a similar direction. Namely, <a href="https://christophscheuch.github.io/post/asset-pricing/crsp-sample/">I constructed the CRSP stock return sample</a>, <a href="https://christophscheuch.github.io/post/asset-pricing/beta/">I estimated stock-specific market betas</a>, and <a href="https://christophscheuch.github.io/post/asset-pricing/size/">I analyzed the relation between stock returns and size</a> and <a href="https://christophscheuch.github.io/post/asset-pricing/value/">I replicated the value premium</a>. I keep the description of individual steps very short and refer to these notes for more details.</p>
<p>At the end of this post, I demonstrate that my effort yields a decent result: my version of the <code>mkt</code> factor exhibits a 99% correlation with the market factor provided on Kenneth French’s website. Similarly, my <code>smb</code> and <code>hml</code> factors show a 97% and 95% correlation, respectively, with the original data. <a href="https://sites.google.com/site/waynelinchang/home">Wayne Chang</a> provides an R script on his website that supposedly achieves higher correlations. However, I have to admit that I have not yet tried to run his code because I find it very hard to grasp what is going on by just glancing over it. Maybe I’ll devote some more time to his code at a later stage.</p>
<p>The usual disclaimer applies, so the text below references an opinion and is for information purposes only and I do not intend to provide any investment advice.</p>
<p>I mainly use the following packages throughout this note:</p>
<pre class="r"><code>library(tidyverse)
library(lubridate)  # for working with dates
library(kableExtra) # for nicer html tables</code></pre>
<div id="prepare-crsp-data" class="section level1">
<h1>Prepare CRSP Data</h1>
<p>This note is self-contained in the sense that everything starts from the raw data downloaded from WRDS. Note that your replication efforts might end up differently if you use another data source! First, I need to construct the sample of monthly stock returns. The code chunk below exhibits two differences to the <a href="https://christophscheuch.github.io/post/asset-pricing/crsp-sample/">earlier post</a>: I drop observations before 1962 as most papers in empirical asset pricing start in 1963 due to data availability; I drop stocks that are listed on exchanges other than NYSE, AMEX or NASDAQ since Fama and French stress that they focus their seminal paper on these stocks.</p>
<pre class="r"><code># Read raw monthly crsp file
tbl.crsp_msf &lt;- read_csv(&quot;raw/crsp_msf.csv&quot;)
colnames(tbl.crsp_msf) &lt;- tolower(colnames(tbl.crsp_msf))

# Parse only relevant variables
tbl.crsp &lt;- tbl.crsp_msf %&gt;%
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

# Analysis of fama french (1993) starts in 1963 so I only need data after 1962
tbl.crsp &lt;- tbl.crsp %&gt;%
  filter(date &gt;= &quot;1962-01-01&quot;)

# Keep only US-based common stocks (10 and 11)
tbl.crsp &lt;- tbl.crsp %&gt;%
  filter(shrcd %in% c(10, 11)) %&gt;%
  select(-shrcd)

# Keep only distinct observations to avoid multiple counting
tbl.crsp &lt;- tbl.crsp %&gt;%
  distinct()

# Compute market cap
# Note: altprc is the negative of average of bid and ask from last traded price
#       for which the data is available if there is no last traded price
tbl.crsp &lt;- tbl.crsp %&gt;%
  mutate(mktcap = abs(shrout * altprc) / 1000, # in millions of dollars
         mktcap = if_else(mktcap == 0, as.numeric(NA), mktcap)) 

# Define exchange labels and keep only NYSE, AMEX and NASDAQ stocks
tbl.crsp &lt;- tbl.crsp %&gt;%
  mutate(exchange = case_when(exchcd %in% c(1, 31) ~ &quot;NYSE&quot;,
                              exchcd %in% c(2, 32) ~ &quot;AMEX&quot;,
                              exchcd %in% c(3, 33) ~ &quot;NASDAQ&quot;,
                              TRUE ~ &quot;Other&quot;)) %&gt;%
  filter(exchange != &quot;Other&quot;)

# Adjust delisting returns (see Shumway, 1997)
tbl.crsp &lt;- tbl.crsp %&gt;%
  mutate(ret_adj = case_when(is.na(dlstcd) ~ ret,
                              !is.na(dlstcd) &amp; !is.na(dlret) ~ dlret,
                              dlstcd %in% c(500, 520, 580, 584) | 
                                               (dlstcd &gt;= 551 &amp; dlstcd &lt;= 574) ~ -30,
                              TRUE ~ -100)) %&gt;%
  select(-c(dlret, dlstcd))</code></pre>
</div>
<div id="prepare-crspcompustat-merged-data" class="section level1">
<h1>Prepare CRSP/Compustat Merged Data</h1>
<p>The next code chunk is explained in <a href="https://christophscheuch.github.io/post/asset-pricing/value/">this post</a> with two additions: I do not include firms until they have appeared in Compustat for two years, just as in <a href="https://www.sciencedirect.com/science/article/abs/pii/0304405X93900235">Fama and French (1993)</a>. However, this extra filter only has a negligible impact on the overall results. Moreover, the construction of book equity is now based on the <a href="https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/variable_definitions.html">instructions provided on Kenneth French’s website</a> as opposed to the slightly simpler implementation in <a href="https://www.wiley.com/en-us/Empirical+Asset+Pricing%3A+The+Cross+Section+of+Stock+Returns-p-9781118095041">Bali, Engle and Murray (2016)</a>.</p>
<pre class="r"><code># Read raw CRSP/Compustat merged file
tbl.ccmfund &lt;- read_csv(&quot;raw/ccmfund.csv&quot;)
colnames(tbl.ccmfund) &lt;- tolower(colnames(tbl.ccmfund))

# Select and parse relevant variables
tbl.compustat &lt;- tbl.ccmfund %&gt;%
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

# Make sure that only correct industry and data format is used
tbl.compustat &lt;- tbl.compustat %&gt;%
  filter(indfmt == &quot;INDL&quot; &amp; datafmt == &quot;STD&quot;) %&gt;%
  select(-c(indfmt, datafmt))

# Check that only valid links are used
tbl.compustat &lt;- tbl.compustat %&gt;%
  filter(linktype %in% c(&quot;LU&quot;, &quot;LC&quot;))

# Check that links are still active at time of information release
tbl.compustat &lt;- tbl.compustat %&gt;%
  filter(datadate &lt;= linkenddt | is.na(linkenddt))

# Keep only distinct observations
tbl.compustat &lt;- tbl.compustat %&gt;%
  distinct()

# Calculate book value of preferred stock and equity
tbl.compustat &lt;- tbl.compustat %&gt;%
  mutate(be = coalesce(seq, ceq + pstk, at - lt) + 
           coalesce(txditc, txdb + itcb, 0) - 
           coalesce(pstkrv, pstkl, pstk, 0),
         be = if_else(be &lt; 0, as.numeric(NA), be)) %&gt;%
  select(gvkey, permno, datadate, be) 

# Determine year for matching and keep only last observation per year
tbl.compustat &lt;- tbl.compustat %&gt;%
  mutate(year = year(datadate)) %&gt;%
  group_by(permno, year) %&gt;%
  filter(datadate == max(datadate)) %&gt;%
  ungroup()

# Keep only observations once a firm has been included for two years
tbl.compustat &lt;- tbl.compustat %&gt;%
  group_by(gvkey) %&gt;%
  mutate(first_year = min(year)) %&gt;%
  ungroup() %&gt;%
  filter(year &gt; first_year + 1) %&gt;%
  select(-c(gvkey, first_year))

# Kick out unnecessary rows with missing values
tbl.compustat &lt;- tbl.compustat %&gt;%
  na.omit()

# Determine reference date for matching (i.e. June of next calendar year)
tbl.compustat &lt;- tbl.compustat %&gt;%
  mutate(reference_date = ymd(paste0(year + 1, &quot;-06-01&quot;))) %&gt;%
  select(-year)</code></pre>
</div>
<div id="construct-stock-sample" class="section level1">
<h1>Construct Stock Sample</h1>
<p>The stock sample used for the construction of the factors also builds on <a href="https://christophscheuch.github.io/post/asset-pricing/value/">this post</a>. Note that the relevant returns for the factor construction are the adjusted returns in the current month. In the end, only stocks that have valid book equity data from year <span class="math inline">\(y-1\)</span>, market equity data at the end of year <span class="math inline">\(y-1\)</span> and market capitalization in June of year <span class="math inline">\(y\)</span> enter the factor construction procedure. Moreover, the market capitalization of June of year <span class="math inline">\(y\)</span> is used to construct value-weighted returns until May of year <span class="math inline">\(y+1\)</span>. I also restrict the sample to start in 1963, consistent with Fama and French.</p>
<pre class="r"><code># Select relevant variables
tbl.stocks &lt;- tbl.crsp %&gt;%
  select(permno, date, exchange, ret = ret_adj, mktcap) %&gt;%
  na.omit()

# Define reference date for each stock (i.e. new sorting starts in June of year y)
tbl.stocks &lt;- tbl.stocks %&gt;%
  mutate(reference_date = ymd(if_else(month(date) &lt; 6, 
                                      paste0(year(date) - 1, &quot;-06-01&quot;), 
                                      paste0(year(date), &quot;-06-01&quot;)))) 

# Add book equity data for year y-1 which is used starting in June of year y
tbl.stocks &lt;- tbl.stocks %&gt;%
  left_join(tbl.compustat, by = c(&quot;permno&quot;, &quot;reference_date&quot;))

# Add market equity data from the end of year y-1 which is used for bm ratio in June of year y
tbl.stocks_me &lt;- tbl.stocks %&gt;%
  filter(month(date) == 12) %&gt;%
  mutate(reference_date = ymd(paste0(year(date) + 1, &quot;-06-01&quot;))) %&gt;%
  select(permno, reference_date, me = mktcap)

tbl.stocks &lt;- tbl.stocks %&gt;%
  left_join(tbl.stocks_me, by = c(&quot;permno&quot;, &quot;reference_date&quot;))

# Compute book-to-market ratio
tbl.stocks &lt;- tbl.stocks %&gt;%
  mutate(bm = be / me)

# Add market cap of June of year y which is used for value-weighted returns
tbl.stocks_weight &lt;- tbl.stocks %&gt;%
  filter(month(date) == 6) %&gt;%
  select(permno, reference_date, mktcap_weight = mktcap)

tbl.stocks &lt;- tbl.stocks %&gt;%
  left_join(tbl.stocks_weight, by = c(&quot;permno&quot;, &quot;reference_date&quot;))

# Only keep stocks that have all the necessary data 
# (i.e. market equity data for December y-1 and June y, and book equity data for y-1)
tbl.stocks &lt;- tbl.stocks %&gt;%
  na.omit() %&gt;%
  filter(date &gt;= &quot;1963-01-01&quot;) # Fama-French paper starts here</code></pre>
</div>
<div id="size-sorts" class="section level1">
<h1>Size Sorts</h1>
<p>To construct the size portfolios, Fama and French independently sort all stocks into two portfolios according to the median of all NYSE stocks in the June of each year. Portfolios are then kept in these portfolios until June of the following year. This is exactly what happens in the code chunk below.</p>
<pre class="r"><code># In June of each year, all NYSE stocks are ranked on size to get the median
tbl.size_breakpoints &lt;- tbl.stocks %&gt;%
  filter(month(date) == 6 &amp; exchange == &quot;NYSE&quot;) %&gt;%
  select(reference_date, mktcap) %&gt;%
  group_by(reference_date) %&gt;%
  summarize(size_median = median(mktcap))

# Also in June, all stocks are sorted into 2 portfolios
tbl.size_sorts &lt;- tbl.stocks %&gt;%
  filter(month(date) == 6) %&gt;%
  left_join(tbl.size_breakpoints, by = &quot;reference_date&quot;) %&gt;%
  mutate(size_portfolio = case_when(mktcap &gt; size_median ~ &quot;B&quot;,
                                    mktcap &lt;= size_median ~ &quot;S&quot;,
                                    TRUE ~ as.character(NA))) %&gt;%
  select(permno, reference_date, size_portfolio)

# Add size portfolio assignment back to stock data
tbl.stocks &lt;- tbl.stocks %&gt;% 
  left_join(tbl.size_sorts, by = c(&quot;permno&quot;, &quot;reference_date&quot;))</code></pre>
<p>Now each stock in my sample received an assignment to a size portfolio of either big <code>&quot;B&quot;</code> or small <code>&quot;S&quot;</code>.</p>
</div>
<div id="value-sorts" class="section level1">
<h1>Value Sorts</h1>
<p>Fama and French determine the value portfolios based on 30% and 70% breakpoints again using NYSE stocks only in June of each year. Each stock is then independently sorted into either high BM <code>&quot;H&quot;</code>, medium bm <code>&quot;M&quot;</code> or low bm <code>&quot;L&quot;</code>.</p>
<pre class="r"><code># Calculate value breakpoints using NYSE stocks
tbl.value_breakpoints &lt;- tbl.stocks %&gt;%
  filter(month(date) == 6 &amp; exchange == &quot;NYSE&quot;) %&gt;%
  select(reference_date, bm) %&gt;%
  group_by(reference_date) %&gt;%
  summarize(value_q30 = quantile(bm, 0.3),
            value_q70 = quantile(bm, 0.7))

# Also in June, all stocks are sorted into 3 portfolios
tbl.value_sorts &lt;- tbl.stocks %&gt;%
  filter(month(date) == 6) %&gt;%
  left_join(tbl.value_breakpoints, by = &quot;reference_date&quot;) %&gt;%
  mutate(value_portfolio = case_when(bm &gt; value_q70 ~ &quot;H&quot;,
                                     bm &lt;= value_q70 &amp; bm &gt; value_q30 ~ &quot;M&quot;, 
                                     bm &lt;= value_q30 ~ &quot;L&quot;,
                                     TRUE ~ as.character(NA))) %&gt;%
  select(permno, reference_date, value_portfolio)

# Add value portfolio assignment back to stock data
tbl.stocks &lt;- tbl.stocks %&gt;% 
  left_join(tbl.value_sorts, by = c(&quot;permno&quot;, &quot;reference_date&quot;))</code></pre>
</div>
<div id="construct-factor-portfolios" class="section level1">
<h1>Construct Factor Portfolios</h1>
<p>To construct the <code>smb</code> and <code>hml</code> portfolios, I simply compute value-weighted returns for all combinations of value and size portfolios and then take the difference of the resulting portfolios as described by Fama and French.</p>
<pre class="r"><code>tbl.portfolios &lt;- tbl.stocks %&gt;%
  group_by(date, size_portfolio, value_portfolio) %&gt;%
  summarize(ret_vw = weighted.mean(ret, mktcap_weight)) %&gt;%
  ungroup() %&gt;%
  mutate(portfolio = paste0(size_portfolio, &quot;/&quot;, value_portfolio))

tbl.factors &lt;- tbl.portfolios %&gt;%
  group_by(date) %&gt;%
  summarize(smb = mean(ret_vw[portfolio %in% c(&quot;S/H&quot;, &quot;S/M&quot;, &quot;S/L&quot;)]) - 
              mean(ret_vw[portfolio %in% c(&quot;B/H&quot;, &quot;B/M&quot;, &quot;B/L&quot;)]),
            hml = mean(ret_vw[portfolio %in% c(&quot;S/H&quot;, &quot;B/H&quot;)]) - 
              mean(ret_vw[portfolio %in% c(&quot;S/L&quot;, &quot;B/L&quot;)]))</code></pre>
<p>Next, I also add the <code>mkt</code> factor as the monthly weighted average return across all stocks. I kick out the last month in my sample as it exhibits about a close to -100% return for whatever reason.</p>
<pre class="r"><code>tbl.factors &lt;- tbl.factors %&gt;%
  left_join(tbl.stocks %&gt;%
              group_by(date) %&gt;%
              summarize(mkt = weighted.mean(ret, mktcap_weight)), by = &quot;date&quot;) %&gt;%
  select(date, mkt, smb, hml) %&gt;% # rearrange columns
  filter(date &lt; &quot;2019-12-01&quot;) # something weird is going on with returns in the last month</code></pre>
<p>Finally, I also add the corresponding factors from <a href="https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html">Ken French’s website</a>.</p>
<pre class="r"><code>tbl.factors_ff &lt;- read_csv(&quot;raw/F-F_Research_Data_Factors.csv&quot;, skip = 3)

tbl.factors_ff &lt;- tbl.factors_ff %&gt;%
  transmute(date = ymd(paste0(X1, &quot;01&quot;)),
            rf_ff = as.numeric(RF),
            mkt_ff = as.numeric(`Mkt-RF`) + as.numeric(RF),
            smb_ff = as.numeric(SMB),
            hml_ff = as.numeric(HML)) %&gt;%
  filter(date &lt;= max(tbl.crsp$date)) %&gt;%
  mutate(date = floor_date(date, &quot;month&quot;)) %&gt;%
  select(-rf_ff)

tbl.factors &lt;- tbl.factors %&gt;%
  mutate(date = floor_date(date, &quot;month&quot;)) %&gt;%
  left_join(tbl.factors_ff, by = &quot;date&quot;)</code></pre>
</div>
<div id="replication-results" class="section level1">
<h1>Replication Results</h1>
<p>In the last section, I analyze how well I am able to replicate the original Fama-French factors. First, let us look at summary statistics across all 678 months.</p>
<pre class="r"><code>tab.summary &lt;- tbl.factors %&gt;%
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
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;))
tab.summary</code></pre>
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
0.25
</td>
<td style="text-align:right;">
2.84
</td>
<td style="text-align:right;">
-0.09
</td>
<td style="text-align:right;">
6.54
</td>
<td style="text-align:right;">
-15.10
</td>
<td style="text-align:right;">
-3.88
</td>
<td style="text-align:right;">
-1.29
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
1.65
</td>
<td style="text-align:right;">
4.78
</td>
<td style="text-align:right;">
14.94
</td>
<td style="text-align:right;">
678
</td>
</tr>
<tr>
<td style="text-align:left;">
hml_ff
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
2.81
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
4.90
</td>
<td style="text-align:right;">
-11.06
</td>
<td style="text-align:right;">
-4.02
</td>
<td style="text-align:right;">
-1.31
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
1.71
</td>
<td style="text-align:right;">
5.18
</td>
<td style="text-align:right;">
12.60
</td>
<td style="text-align:right;">
678
</td>
</tr>
<tr>
<td style="text-align:left;">
mkt
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
4.39
</td>
<td style="text-align:right;">
-0.41
</td>
<td style="text-align:right;">
4.98
</td>
<td style="text-align:right;">
-21.86
</td>
<td style="text-align:right;">
-6.52
</td>
<td style="text-align:right;">
-1.38
</td>
<td style="text-align:right;">
1.22
</td>
<td style="text-align:right;">
3.76
</td>
<td style="text-align:right;">
7.58
</td>
<td style="text-align:right;">
17.10
</td>
<td style="text-align:right;">
678
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
4.37
</td>
<td style="text-align:right;">
-0.52
</td>
<td style="text-align:right;">
4.97
</td>
<td style="text-align:right;">
-22.64
</td>
<td style="text-align:right;">
-6.68
</td>
<td style="text-align:right;">
-1.65
</td>
<td style="text-align:right;">
1.25
</td>
<td style="text-align:right;">
3.74
</td>
<td style="text-align:right;">
7.43
</td>
<td style="text-align:right;">
16.61
</td>
<td style="text-align:right;">
678
</td>
</tr>
<tr>
<td style="text-align:left;">
smb
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
2.93
</td>
<td style="text-align:right;">
0.52
</td>
<td style="text-align:right;">
5.76
</td>
<td style="text-align:right;">
-12.23
</td>
<td style="text-align:right;">
-4.06
</td>
<td style="text-align:right;">
-1.55
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
1.98
</td>
<td style="text-align:right;">
4.66
</td>
<td style="text-align:right;">
16.40
</td>
<td style="text-align:right;">
678
</td>
</tr>
<tr>
<td style="text-align:left;">
smb_ff
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
3.04
</td>
<td style="text-align:right;">
0.44
</td>
<td style="text-align:right;">
7.88
</td>
<td style="text-align:right;">
-16.72
</td>
<td style="text-align:right;">
-4.22
</td>
<td style="text-align:right;">
-1.58
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
4.89
</td>
<td style="text-align:right;">
21.13
</td>
<td style="text-align:right;">
678
</td>
</tr>
</tbody>
</table>
<p>The distributions of my factors and the original ones seem to be quite similar for all three factors, but still far from perfect. Kolmogorov-Smirnov tests confirm that there is no statistically significant difference in the distributions.</p>
<pre class="r"><code># Kolmogorov-Smirnov test
ks.test(tbl.factors$mkt, tbl.factors$mkt_ff)</code></pre>
<pre><code>## 
##  Two-sample Kolmogorov-Smirnov test
## 
## data:  tbl.factors$mkt and tbl.factors$mkt_ff
## D = 0.022124, p-value = 0.9964
## alternative hypothesis: two-sided</code></pre>
<pre class="r"><code>ks.test(tbl.factors$smb, tbl.factors$smb_ff)</code></pre>
<pre><code>## 
##  Two-sample Kolmogorov-Smirnov test
## 
## data:  tbl.factors$smb and tbl.factors$smb_ff
## D = 0.020649, p-value = 0.9987
## alternative hypothesis: two-sided</code></pre>
<pre class="r"><code>ks.test(tbl.factors$hml, tbl.factors$hml_ff)</code></pre>
<pre><code>## 
##  Two-sample Kolmogorov-Smirnov test
## 
## data:  tbl.factors$hml and tbl.factors$hml_ff
## D = 0.025074, p-value = 0.9834
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
tab.correlations &lt;- compute_correlations(tbl.factors, mkt, mkt_ff, smb, smb_ff, hml, hml_ff) %&gt;% 
  kable(digits = 2) %&gt;%
  kable_styling(bootstrap_options = c(&quot;striped&quot;, &quot;hover&quot;, &quot;condensed&quot;, &quot;responsive&quot;))
tab.correlations</code></pre>
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
0.27
</td>
<td style="text-align:right;">
0.31
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
0.25
</td>
<td style="text-align:right;">
0.30
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
hml
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
-0.25
</td>
<td style="text-align:right;">
-0.14
</td>
<td style="text-align:right;">
-0.19
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
-0.22
</td>
<td style="text-align:right;">
-0.25
</td>
<td style="text-align:right;">
-0.12
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
<p>The <code>mkt</code> factor exhibits a correlation of 99% which is a pretty good fit. The <code>smb</code> factor also shows a satisfying fit with 97%. The <code>hml</code> factor only has a correlation of 95% with its original counterpart which might be driven by different sample construction procedures that I am not yet aware of or some ex-post changes in the CRSP or Compustat data that I capture, but Fama-French do not include.</p>
<p>As a last step, I present the time series of cumulative returns over the full sample period.</p>
<pre class="r"><code>fig.factors &lt;- tbl.factors %&gt;%
  pivot_longer(-date, names_to = &quot;portfolio&quot;, values_to = &quot;return&quot;) %&gt;%
  group_by(portfolio) %&gt;%
  arrange(date) %&gt;%
  mutate(cum_return = cumsum(log(1+return/100))*100) %&gt;%
  ungroup() %&gt;%
  mutate(portfolio = toupper(portfolio),
         portfolio = gsub(&quot;_FF&quot;, &quot; (Fama-French)&quot;, portfolio)) %&gt;%
  ggplot(aes(x = date, y = cum_return)) +
  geom_line(aes(color = portfolio)) +
  labs(x = NULL, y = NULL, color = &quot;Portfolio&quot;,
       title = &quot;Cumulative Log Returns (in %) of Original and Replicated Fama-French Factors&quot;) + 
  scale_x_date(expand = c(0, 0), date_breaks = &quot;10 years&quot;, date_labels = &quot;%Y&quot;) +
  theme_classic()
fig.factors</code></pre>
<p><img src="data:image/png;base64,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" width="768" style="display: block; margin: auto;" /></p>
<p>There seems to be a considerable divergence between my factors and the original data over time which is less severe for the <code>smb</code> factor. Please feel free to share any suggestions in the comments below on how to improve the fit. Overall, I am nonetheless somewhat surprised how quickly one can set up a decent replication of the three famous Fama-French factors.</p>

</dl>
