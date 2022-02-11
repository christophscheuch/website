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

<p><b>Update 2022-02-11: check out our new book <a href="https://tidy-finance.org/">Tidy Finance with R</a> for an updated and improved implementation of the content below. </b></p>

<p>This note is an effort to replicate the famous <a href="https://en.wikipedia.org/wiki/Fama%E2%80%93French_three-factor_model">Fama-French three-factor model</a>, in particular the three factors that enter its equation: the market factor, the size factor (small firms outperform larger firms), and the value factor (firms with high book-to-market equity ratios outperform firms with low ratios). Throughout this note, I build on earlier posts that explored empirical asset pricing approaches in a similar direction. Namely, <a href="https://christophscheuch.github.io/post/asset-pricing/crsp-sample/">I constructed the CRSP stock return sample</a>, <a href="https://christophscheuch.github.io/post/asset-pricing/beta/">I estimated stock-specific market betas</a>, and <a href="https://christophscheuch.github.io/post/asset-pricing/size/">I analyzed the relation between stock returns and size</a> and <a href="https://christophscheuch.github.io/post/asset-pricing/value/">I replicated the value premium</a>. I keep the description of individual steps very short and refer to these notes for more details.</p>
<p>At the end of this post, I demonstrate that my effort yields a decent result: my version of the <code>mkt</code> factor exhibits a 99% correlation with the market factor provided on Kenneth French’s website. Similarly, my <code>smb</code> and <code>hml</code> factors show a 97% and 95% correlation, respectively, with the original data. <a href="https://sites.google.com/site/waynelinchang/home">Wayne Chang</a> provides an R script on his website that supposedly achieves higher correlations. However, I have to admit that I have not yet tried to run his code because I find it very hard to grasp what is going on by just glancing over it. Maybe I’ll devote some more time to his code at a later stage.</p>
<p>The usual disclaimer applies, so the text below references an opinion and is for information purposes only and I do not intend to provide any investment advice.</p>
<p>I mainly use the following packages throughout this note:</p>
<pre class="r"><code>library(tidyverse)  # for grammar
library(scales)     # for nicer axes of figures
library(viridis)    # for nicer colors in figures
library(vroom)      # for fast csv reading
library(lubridate)  # for working with dates
library(moments)    # for skewness and kurtosis 
library(kableExtra) # for nicer html tables</code></pre>
<div id="prepare-crsp-data" class="section level1">
<h1>Prepare CRSP Data</h1>
<p>This note is self-contained in the sense that everything starts from the raw data downloaded from WRDS. Note that your replication efforts might end up differently if you use another data source! First, I need to construct the sample of monthly stock returns. The code chunk below exhibits two differences to the <a href="https://christophscheuch.github.io/post/asset-pricing/crsp-sample/">earlier post</a>: I drop observations before 1962 as most papers in empirical asset pricing start in 1963 due to data availability; I drop stocks that are listed on exchanges other than NYSE, AMEX or NASDAQ since Fama and French stress that they focus their seminal paper on these stocks.</p>
<pre class="r"><code># Read raw monthly crsp file
tbl.crsp_msf &lt;- vroom(&quot;raw/crsp_msf.csv&quot;,
                       col_types = cols(.default = col_character()))
colnames(tbl.crsp_msf) &lt;- str_to_lower(colnames(tbl.crsp_msf))

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
  distinct(permno, date, .keep_all = TRUE) # remove duplicates 

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
tbl.ccmfund &lt;- vroom(&quot;raw/ccmfund.csv&quot;,
                     col_types = cols(.default = col_character()))
colnames(tbl.ccmfund) &lt;- str_to_lower(colnames(tbl.ccmfund))

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
<pre class="r"><code>fun.compute_correlations &lt;- function(data, ..., upper = TRUE) {
  cor_matrix &lt;- data %&gt;%
    select(...) %&gt;%
    cor(use = &quot;complete.obs&quot;, method = &quot;pearson&quot;) 
  if (upper == TRUE) {
    cor_matrix[upper.tri(cor_matrix, diag = FALSE)] &lt;- NA
  }
  return(cor_matrix)
}
tab.correlations &lt;- fun.compute_correlations(tbl.factors, mkt, mkt_ff, smb, smb_ff, hml, hml_ff) %&gt;% 
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
  mutate(cum_return = cumsum(log(1+return/100))) %&gt;%
  ungroup() %&gt;%
  mutate(portfolio = str_to_upper(portfolio),
         portfolio = str_replace(portfolio, &quot;_FF&quot;, &quot; (Fama-French)&quot;)) %&gt;%
  ggplot(aes(x = date, y = cum_return, color = portfolio)) +
  geom_line(aes()) +
  labs(x = NULL, y = NULL, color = NULL,
       title = &quot;Cumulative log returns of original and replicated Fama-French factors&quot;) + 
  scale_x_date(expand = c(0, 0), date_breaks = &quot;10 years&quot;, date_labels = &quot;%Y&quot;) +
  scale_y_continuous(labels = percent, breaks = pretty_breaks()) + 
  theme_classic() +
  scale_color_viridis_d()</code></pre>
<center>
<img src="data:image/png;base64,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" width="768" />
</center>
<p>There seems to be a considerable divergence between my factors and the original data over time which is less severe for the <code>smb</code> factor. Please feel free to share any suggestions in the comments below on how to improve the fit. Overall, I am nonetheless somewhat surprised how quickly one can set up a decent replication of the three famous Fama-French factors.</p>

</dl>
