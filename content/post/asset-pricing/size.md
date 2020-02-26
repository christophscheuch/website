---
title: 'Tidy Asset Pricing - Part III: Size and Stock Returns'
subtitle: 
summary: 'On the relation of firm size and future stock returns'
authors:
- admin
tags:
- Academic
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
1025.13
</td>
<td style="text-align:right;">
4821.76
</td>
<td style="text-align:right;">
14.56
</td>
<td style="text-align:right;">
323.14
</td>
<td style="text-align:right;">
0.52
</td>
<td style="text-align:right;">
5.64
</td>
<td style="text-align:right;">
28.03
</td>
<td style="text-align:right;">
108.52
</td>
<td style="text-align:right;">
446.89
</td>
<td style="text-align:right;">
3891.08
</td>
<td style="text-align:right;">
131274.34
</td>
<td style="text-align:right;">
3159.74
</td>
</tr>
<tr>
<td style="text-align:left;">
mktcap_cpi
</td>
<td style="text-align:right;">
1870.46
</td>
<td style="text-align:right;">
8497.58
</td>
<td style="text-align:right;">
14.56
</td>
<td style="text-align:right;">
323.14
</td>
<td style="text-align:right;">
2.81
</td>
<td style="text-align:right;">
19.87
</td>
<td style="text-align:right;">
80.13
</td>
<td style="text-align:right;">
257.45
</td>
<td style="text-align:right;">
937.67
</td>
<td style="text-align:right;">
6992.14
</td>
<td style="text-align:right;">
225227.62
</td>
<td style="text-align:right;">
3159.74
</td>
</tr>
<tr>
<td style="text-align:left;">
mktcap_ff
</td>
<td style="text-align:right;">
1018.02
</td>
<td style="text-align:right;">
4736.35
</td>
<td style="text-align:right;">
14.34
</td>
<td style="text-align:right;">
311.39
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
6.02
</td>
<td style="text-align:right;">
28.67
</td>
<td style="text-align:right;">
109.70
</td>
<td style="text-align:right;">
448.02
</td>
<td style="text-align:right;">
3883.85
</td>
<td style="text-align:right;">
126975.24
</td>
<td style="text-align:right;">
3052.09
</td>
</tr>
<tr>
<td style="text-align:left;">
mktcap_ff_cpi
</td>
<td style="text-align:right;">
1860.41
</td>
<td style="text-align:right;">
8368.99
</td>
<td style="text-align:right;">
14.34
</td>
<td style="text-align:right;">
311.39
</td>
<td style="text-align:right;">
3.29
</td>
<td style="text-align:right;">
20.51
</td>
<td style="text-align:right;">
80.92
</td>
<td style="text-align:right;">
258.84
</td>
<td style="text-align:right;">
937.75
</td>
<td style="text-align:right;">
6978.79
</td>
<td style="text-align:right;">
218628.94
</td>
<td style="text-align:right;">
3052.09
</td>
</tr>
<tr>
<td style="text-align:left;">
size
</td>
<td style="text-align:right;">
3.92
</td>
<td style="text-align:right;">
1.80
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
2.99
</td>
<td style="text-align:right;">
-1.35
</td>
<td style="text-align:right;">
1.16
</td>
<td style="text-align:right;">
2.63
</td>
<td style="text-align:right;">
3.79
</td>
<td style="text-align:right;">
5.09
</td>
<td style="text-align:right;">
7.07
</td>
<td style="text-align:right;">
10.37
</td>
<td style="text-align:right;">
3159.74
</td>
</tr>
<tr>
<td style="text-align:left;">
size_cpi
</td>
<td style="text-align:right;">
5.44
</td>
<td style="text-align:right;">
1.80
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
2.99
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
2.68
</td>
<td style="text-align:right;">
4.16
</td>
<td style="text-align:right;">
5.32
</td>
<td style="text-align:right;">
6.61
</td>
<td style="text-align:right;">
8.59
</td>
<td style="text-align:right;">
11.89
</td>
<td style="text-align:right;">
3159.74
</td>
</tr>
<tr>
<td style="text-align:left;">
size_ff
</td>
<td style="text-align:right;">
3.94
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
1.22
</td>
<td style="text-align:right;">
2.66
</td>
<td style="text-align:right;">
3.81
</td>
<td style="text-align:right;">
5.10
</td>
<td style="text-align:right;">
7.07
</td>
<td style="text-align:right;">
10.35
</td>
<td style="text-align:right;">
3052.09
</td>
</tr>
<tr>
<td style="text-align:left;">
size_ff_cpi
</td>
<td style="text-align:right;">
5.46
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
0.50
</td>
<td style="text-align:right;">
2.74
</td>
<td style="text-align:right;">
4.18
</td>
<td style="text-align:right;">
5.33
</td>
<td style="text-align:right;">
6.62
</td>
<td style="text-align:right;">
8.59
</td>
<td style="text-align:right;">
11.87
</td>
<td style="text-align:right;">
3052.09
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
<p><img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAABUAAAAPACAMAAADDuCPrAAABelBMVEUAAAAAADoAAGYAOjoAOmYAOpAAZpAAZrYAv8QzMzM6AAA6OgA6Ojo6OmY6OpA6Zjo6ZmY6ZpA6ZrY6kLY6kNtNTU1NTW5NTY5Nbm5Nbo5NbqtNjshmAABmADpmOgBmOjpmOmZmOpBmZjpmZmZmZpBmkGZmkJBmkLZmkNtmtttmtv9uTU1ubk1ubm5ubo5ujqtujshuq8huq+R8rgCOTU2Obk2Obm6Oq6uOq8iOq+SOyOSOyP+QOgCQOjqQZjqQZmaQkDqQkGaQkLaQtraQttuQ27aQ29uQ2/+rbk2rbm6rjm6ryOSr5P+2ZgC2Zjq2Zma2kDq2kGa2kJC2tpC2tra2ttu2tv+229u22/+2///HfP/Ijk3Ijm7Iq27IyKvI5P/I///bkDrbkGbbtmbbtpDbtrbbttvb27bb29vb2//b///kq27kyI7kyKvk5Mjk///4dm3/tmb/yI7/25D/27b/29v/5Kv/5Mj/5OT//7b//8j//9v//+T///8RkgQkAAAACXBIWXMAAB2HAAAdhwGP5fFlAAAgAElEQVR4nO29jb8UN3qoWceG5UwbZzGh7Wub4A1M7AB344mPnczGxL4zd3aHdeY4duKsPTB3mWOYGRsml3MAw+Km//ct1adUVaoPlVQlVT/P72dOd58qSV24Hl5Jr1TRFgAAjIjmbgAAQKggUAAAQxAoAIAhCBQAwBAECgBgCAIFADAEgQIAGIJAAQAMQaAAAIYgUAAAQxAoAIAhCBQAwBAECgBgyHwCjXA3AIQNAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEFOL/fjh29mrF59dX6/fv1N/8+yj9et//zB5efuth/WaESgAhI2pxW6vM4H++OFa8Mbvq29+/PCth0+vJ0c9vf5xQ80IFADCxsxiL26vc4HeXr91Z/vs5jqJMeU3D9axNW+//pttcwCKQAEgdIws9p8frXOBPr2ehZvClMqbxJ2JRR+tGwJQBAoAoWNisQfr9c/+nAn0QfHzg8qbQqAvbjYFoAgUAELHSKBv/kscVaauvJ1Fl8l79U0ShsafNAegCBQAQsfUYplAX9xMBjlF7/2th8qbYgy0FoD+JAOBAkDYuBNoPgtfC0ARKAAsA3sCfeP3yps0D/TnD0UA+uKz9fr96jgoXXgACBx3EWhx4Mcvbr5x59mH1ZkkBAoAgeNaoCIATdLpH1R78ggUAAJnpEBbZuHz4z5Os5oeiT+UmhEoAITNWIE+yLyY5YF+oHyYBqB5Nv3bagkIFAACZ6xA9SuRssM+3hKBAsAyGSvQFzfXbxbL35U36W/FK8ZAAWCRjBXo9pm8G5PyZpuvgheTS8zCA8DiGC3Q7bPP1mWWp/KmWAVPHigALBF2pAcAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDEChAf27dmrsF4BUIFEDDraoub92qfQS7DQIF0CDL8lbyDoGCCgIFaCaTZfJnEXsiUJBBoLAk7PmtCDaTn7e25VtbNcACQKCwJCzaLTdlU7cdi0IKAoUF4aKbjUBBDwKF5SB3uh1XlA+MOq4IPAeBwmK4JfW6ndd0i6wmQKCwICSdTSU2DLrjIFAIniaJTWbQaaoBT0GgEDxNYSBigylAoBA4txwMRdIxh34gUAicFtmZexCBQi8QKIRGTy3eumUuUOez+AS5CwGBQmgU6ulw0OCefZEENTAAHe7CWxh0ISBQCI1ydw/bAjXMj0eguwsChcHMfOf3FWhx/ICSjZaCItDdBYHCUG717UM7q3/o8b1DSsMvZNAiBLoMECgMRbrzi7WT81Tf+4zewaqpQHufV7oTgS4BBApDkW7/6u4dEzjBIOezaVao9rQO8xY1lNZyYO3iQcggUBhGuY/GtjTBhNu126lD2i45fz+yvCG1Zn/2P7GpLMPzwCoIFPpTqvPWLWkcb7JNkKxx61b5jKNSZjbK7ay2cpjFVNXg/haWAAKF/tR2O5LjUOcBqJ3Sq8rPPGqlaIPU/RGLpWqjEDU7g3MQKPSndbs4t5GPjQXvzXPfY1YsVUsa7jDzb1W5IGxOOgsIFPrTeoe6vX1t6MHFtiOVCtSfQ84ZW1/yn3lRYAYChd7MGeJYqXqiIG1YLerBvU69pfwk8pwRBAq9GXRz26nRZmGTMczTlWOlYZJep9BznxUECr3pJ1BbMzJ5jcvQw5BU++KF5pxbFYGOaBaMBIFCb4YlO46t69Z2ITMjt/qMvTZlNlXPMt3sBNyBQME+w27wxidylHlS4cpCzpXq+hbyIRqDTrlcAXqCQME+w+7wyVOipmLgXFK3Yw2KBbcgUJibptTMZUhi1GxS5Zc9j4NpQaAwL02SWYwi7H2RxVyShYFAYS7IYuzNUkLy5YFAYTqqiw9na0hgLCIZYZkgUJiO6qzyfC0JC66UtwQpUP45noOhF71hX42WCJS/UQiRIAXKv8i9sHyVBgs0S1rMk2/KIprSGcvcxxFNBJiYkAWKRluxvffQ8K0ulbPKveTkvZgrxTPWB2GBQBdLrxWEg8ozOUERaPGqYQ9O+UkhAIGAQJfNnBGo1Gvvt5iRv1IIDQS6C1ja3sP8DAQKyyQ4gd5SwhqbDVoKt+oXxkrP2KCIQTlLCBTCIziByjO53G4qt5rHES0NhXK1Aaog0AWh2+6MUB3ADQh0QXA9AKYlbIFabM4S6CVQLAtgjaAFCio9BcqVA7BEYAJteOwBDIMLB2CNUAWKBkzhygFYA4HuDOPGPrjeAHWCEigrpccwUqBceYAaCHQxdF0bBApgm6AECm10C3SAA8nFB+hBKAJtvH+5qWV67dZhWBj+BGgiaIFiUBlrAlUXeam7egKABAJdDD0Emj9do/XIbEMSpVguNEAT4Qh0dB9+6RLo//36CNSgWICdIxyB9v9UW8Sg7SmDY8BXak1nuKVuKMrwJ4CWUATazFCBll3YBVrBokC3C/+3BsASOyrQnQ+rOgQqv9z1SwWgJ3yBDhj5Q6AFLV+/TLjn0VMq+/v7czcBPCNsgQqUBTZt97pkTQTag/wicaVyEChUWYJApTipa365YTc8/KCBf2WqIFCoEoRAW2/k7D6XBNp0eNWv5TwSlgANVWEiUKgShkDbftck0PoJDQGq7hFsACl1gc7UEPCWEATaEYCWYectfVCZD+fdUkZCO0uHXWafkBM6CECgnetmJIFqz5An4KUPb+2yQHf2i/cFgUIXixLotlugDYts5CGAgBn+BZgkaie2pxDofvYmkSlKBYXgBZofUBneHFLIMnryBl8g+O/slESe+/nIJwKFJpYg0OYjmiaM2ssIWydGAkWhWvZTeebd+PLnvM0Cv1iEQDtP6yGK+uBoYJhFoNWzQvzmLtnflgJNP9hHolASgEANGWqC0DdrMoxAawK11JzlUEah6TsECiV+C3REPGR26i15XiksTJqMQHuBQEHHggVqdtatkfXOxmiBLiMZwQXVdCYECjnLFujg04tUfNNq58NIoNXtAfBnhuLIikBJD4UC3wVqXLxhPLVzAqkIdM6m+EQ15NxX3iBQyPBboGMhoOqkaYMqaBUoyaBQsAsCDdkMzpteGQMNHVtqaxGoxVogeJYu0MAnRqZsesCXqWQagaJQyFi2QIOfWUagA3Ej0Hqx6TolPLrzINCO8+e1yoTVL8KfrgTa8Pt9puNh+QLdjpPgzAKdZr897SaqAWLJaZ2FsLUIJHgsUEvyMizGi4z6W/I20W5rWQb7k/Wq2VoEBF4LdJJ2tNbuh1t6NSLcONsiUy60RJ6AQFtqn2f+qVHc3fv1jWvqYgQqmFag9OJ3GwTqW/VNu5k0yryyEwgCzZhcoBh0h0Gg+uonrl+Zy2kILxFoP2wIrU8Z+TEIdJdBoL5Ur0aeDbV37DyHQDMmEug+AgUE6g+Vrnv92xexafN12bHL1Yi1qXEECv3wWKC+MJGZ+jy1Kf3ROMO0nJn0EVhz2ZAuPOwyCLQTXwS61USnCLRgUoECINA+OFNTar7ee0bRd+8CgcLEINBuXBnqVr7QaNSme/izwF5GEQKFfiDQbhwp6tatPAQdUQs995z9fXvzOgPPR7e7CwLtxp1AvSklfPblVZwTJ7cj0N2lzWLf5XzvpOZdFSjKc0H1uW9mZZidi0B3F53FnnwSlbz0Wxc1ByNQyyBQN6gPfjMvYXj4ikB3F43Fnl+MZhboci3DsOUEmOxrt59v8mlQmVlVEDwaix1F0al3v8r5j/sualar7liouAia17nDQBrcU//IQFH7+TbzFhrUoyoMugSaBbq5EZ1xXnNedfMKxaAto9l9rmMxJvSjwT0NnxhGoCZiM1IuAl0EzQJ9fnHv185rzqq+1bTzZnW5YmA0bgUibRQS8nfzgZrlmmxkMpQ5Zux0kqrAM3QCdTLsqdYcmEB7tkfqpZdLL8vPQqf6uF8fmqHpDw9t3P6EVkOgS0HXhZ8uAm3c7NJD2fR8roY0ztmys1K4KA/N8EOgmvHOgZJSEkknAIEuAu0k0gXnNasCdV3daIYLdFv22Z21anI8EWifY/YHPSJp3GM2SR7dUTQCjUPQq65rVgSq4F//fdvbgqpA5cXuy6CYZ5k4YFNaILel7bjBAjX+RuhwR9F04T+9HEV7565k/NRtGlOt++6lcIa1StpjztPvY0jupP39uVJxqgJtacNwyU8jUHS7GHSTSNF8ifS++sbPVk1LniqZ+9MDgba1ob9A82IQKAxiXoE2PyfNU4GCMl0zWyp4OVjZmffePzF+7JdBoDvKrLsx1WLNAHu7wTV4FOp683kFut85lzRgaeZ0Ai2iYiy6BPwSqLfd9xzto952BB/m4AuBdkpvwLzQ2O9SjAB0loNAFwUCHUJD8xaZ7KlFWesoGWBKGUgC7X985zGjv0DfNCgEuij0Av3hy7NRtHf2PSebgW6bx0B955Y+5SrA0YfhFNPv28wAuQSm7M4PXmHUo0Qbre+xkj4fkx2XMgUeoRXoUTGF5CilPtD9QDUh6C7oU8lfqnRZPRZoR2G955n6FNXtz60UOju6Zoh5QnQWE/58+dyVy684M2ioAr1VfZPnzM/UoCkpI6iqOL2+aduz7a3Fz/0EOuQE43bAVGgs9ngVnf4mefXkRuRmXXygAlWQnqe5EwItkAS6L4nA000uW5plU6CdW+jVfsuuoMGjsdhhdDpffeRqb9AoCl85u2XNJvLpkKkEalRBmz+nnM5pEug0NYMreuzG9Hh1um0p57N/WK/X799J37z47Hrx5tlH69f//mHy8vZbD+s1I9CFIA3+uReCJYHu76tDEfZoWf1U/xiBhk6P/UDbNwd9Ghsz5vXfiDc/fpi8eeP3yeu3Hj69/nZ6zMcNNSPQZSDvA+dnBLqt5dyrE2Hjm6UUrY0sJxEoTp6UsQJ9cXP91p3ts/hPEWPelt48WMfWvJ2ItSkAXYRAd2x9vE4L0+WDmpevnin33G23ecgmAQg0dLTPRLpWvDmJWrrwT69n4aYwpfImcWdi0UfrhgA0cIHu0NR7SXvP1P1o4qhJazlfYN8fgZrW3j4tBlMxdhLp0TrppMeB6MdCl8mb+OcHkkBf3GwKQAOfhd/BmfeOe3OCCZFRQ5Zy6mpFoG7CwJ4ZXqYCrQ+1TjUMDRL6NKZTXyev/nSpNY1JDTqzUDOx6u38k+YAdAkC3TXa7s2J/Dmikv0iU0A1mzuBKuXrajGsPZu5K7/U1CkFkNCWSB+dPXu2aylSMQb6tnidTiXFVo1jznwMtBaA/iQjUIHeUn7sFK0J6e7v2/ECLccask/kH1YpAsQiUNTWYixQ1dJypTAZWovdXWUrOTue7fHid8nE+88eVgWaz8LXAtDQBVomz+8YMwegY93gaN1Pe41llKgXqFlmVinQYtAVgU6P3mKbe5fjCPTcrzoe5/H0o0Sgb95RBCr69SIP9OcPRQD64rP1+v3qOGiwXfhdHP1M6BgC7XGQw/p7nD554noh0Pbh28bfdI2cFpd7X13HgECnZazFnl4XwWcchsbuVCPQjDgAfXHzjTvPPqzOJIUr0Fs7sfNSnV73pqsb2I4bpjWokiDbLtChKaJqTkG/c8ABYy12W0y4Jz/fbhSoCECTdPoH1Z58sAIV4ScCHXWQUeXhyWG/c3+77DBJr+ouA21FS2ePaCOMo2qxzafiGZzxnzL6p3KqzlRm4bfZy4/TrKZHmWnLmoMVKLTgMAINkh7NVpaUyq/0Z+wTdXpC1WLPL4pHyPV+qFx14j2V5INClskUfJZN/7Z6KgJdJK7ioUA90TujvjyhmHEaOGoKczBSoEoXXk0KTXiU5tcTge4OCFRieKuz6aD2k8O8GItkrMUerctJJJEU+ma5MH6bBaDbpY2B7ijz3rVhOsNQoB2Zo4FejEUy2mIP1ilJfPlM2o1JkOaAin7+gmbhdxXuWqfsV3rx/Y6FmdFsJvKpNG/0+PJftOWC/k+xH+jfZfuBPvtsLaV85ouQlpUHuqtw1zpFurwtoWf2e/4qfGH0fqDmNSPQsOh/13J/G9DDivnkO9fXH3oI9PEKgcKg23Ypd/jBQeXtgeZACzQJtJrjyUIj76hZrDIBn9D6SA/jmhFoWAwSqN37fN/VE4DbqQrTqUCLvUeUj5pWKOFQj6hb7KQu0GsNJ46vGYGGxZAb1+5NPnIbJlMOphVown7NoNVfIVCvqFts869Xrlxe7Z0r1iG9+7WbmhFoWAy6cUff5cVgX8dOHA6p+XJ6gcoRaZ5ij0B9oscYqKuaEeiCGXuXl0+qa+jZTkyhTff+bNn0roxAnTcC+tMjjclVzQh0wYwXaLbHhge+mMCbKc3fttiqbv4rATXmsxgChZTGiZJ8K00PrDGlQBs/zX7nwaWAKi0W23yXce+vSWMCd+immpv3yZyUxJ3ij/i/uTrw5WbJ4B86iz35pN9mIiNqRqBh4ewO9lQNmTsPMn9O5NA6JM/7jMZiajboaQQKQ2/h0BcuHcjSzM05wTx8DV+GgqEJjcWOomjvnEhmurzqeqqccc0INCx2TqDJn2kkWvlwWovOP5IBWjSz8DfE6qP4z2vCpU4WIiHQ0HAh0DBDq1l68uAlujzQvV9vhTvFI+EPWYkEW6cCraz49s2psjDzEVEAQWsi/Ul0pvjTfs0INCyGeq1xcwzNB8ovfPMnAgUtHQIVvffnF9lMBIwFKp9W67E3CdS7AFRZFI9AQUY3Bpp04dON7NgPFATWBdq8hbAf/jxQp44QKDSjsdhhMvqZDoWyHyiY0EOg+5Vjq8fPR20fppZfwg6jsdjjVfTaN2Ia/oKQKV14MEbdkW1/f7+p364cNFnTWmgVKAaFHJ3FDpP1RydRtLeKkmjUfs0I1CleeKgSdKb2bB/49DIC7f072DG0FvtWdNw3h842pEegjpl/J476/pV6gUpnTdG0USBQKGix2B9jb27unj37npud7RCoU7L94OxFdMPLyapvEKi2YX6EnzXUPCYECgVsZ7dQihmc+QTauJ1SGYQ27zzkpUEriaAIFHIQ6DIpH6Dji0CVUU5dYX76s5LUVP0AdhiNxX74vnztaHt6BOqSQls+RKCV5ZrNAvVq07ba05A6fg+7im4lUnReekMeaGhoMoQsFTn0tH4C9WD/5IIuQSJQSNEKtJx7R6DhoSwttzMfP0Kg2/1y6khfXkgCxaCQohdodOqb/A0CDRlLE0lGZUgZS70E6gvdfkz3qseju452M5H/FkXJengEGg7N+UFzClQyaFAC7Sbdqx6B7jr63ZjurqLoav7GRc3mAg3rVpuO/EHAFYFanYwf3KTyVbdAA/qLnWVvevCOlu3sHscGPb/1U6Dh3GhT0izQ7Zxu0mweovk7DOjvFYGCoG0/0CeXkqkk/wQa0o02JY37u6e/8EGglV8ELtD0TwS647RuqLy5IaaSEGgotF2WmaJ2fa1eCxQxQj9aBbrdfh5Fe/8nAg2E1ssy9pqNm0RyWYcD+ggUyUKnQMXzjZON7RzUPFKgDIRWab8i8wh0WA3e/JX2FygW3W26BJpMJfkl0Ow28+h284SeAh2TEe8Uj/5Cewj0gGkk0D4TSVr+/uSiVwItZpkRaIWOyzF2x2KutkqeRE8y/U7Tw2J+bSaCQHX0vRwI1AqFNxHoLhPcdnaFNhFoBQQ6KQgUtnWBbj69Eseb8Z8ynkWg8q68UNDzcpj+w8PVVlEedAy7StViz5MRz2QzkRJ/xkCV/ju3tELX5dgv91h2Ubwf2LEZToSeBCfQ/CcCreJYoGFgZ0oHgUJPAhsDlbZHgwpEoFsEChODQBdDf4E6Kd4PEChMSmACzQnjbvaLQqCjTvecOdSHbneYhln4Oj7NwqcsehzPETsh0FnAoLtLwyRSHX8mkXK8egJZIOT5C6NO3wGG6xCB7i6BCnSr2fgSuuAfnk4G+9BAuUNPAE8JaAxUSVxCoDV6XI2evfhwB0gOxoSD+ZkTBJTErEshKIHuI9AWuq/G/vIFejBiHn7CbUF4Ht1SCEigyo2NQGtYE2jAqfYHo0LQ9HSLzWmrCIEughaLbb7LuPfX3o2B6h7+s8tY68LvNyg0kEs9XqDThKEIdCnoLPbkk52YRArEC73o8116ZjLtV3vxoVynkSGkqddMZpEQ6CLQWEydjD/tn0ATRtzWC1wY3v+b9FJtkAIdiWn4iQx3Fo3FjqJo79zllfgv2rvqpmaDWXh7SLuSWCx1Vux+k90T6IjuPwLdWTSP9LghHggf/3lNuPS0i4VIngh0QW6w9UWaEu4Xc5FamFCgCHcx6B4qt/frrXDnhfjPQ6FRBzX7INBdEEMjLV9c3a26olO/7/xRD8ocI1CDqmARtD6V8yQ6U/xpv2YEOiNdAt1XjywF6vWtP+pBmalAp8pi8v1SQj86BCp6788vOunDWxDouEmkYjn9Lq6rHyjQAr/v+tECNf5+w85DoItBNwaadOEfr4RHpYfEW615kECbN6Afpr3i6FQRxab2+/v7S5qN7wUCbSthkvP8vpTQD43FDpPRz3QoNNWo/ZrnFGhuzDT4XIZAB32DdoFutQJd+m2PQGEQGos9XkWvfSOm4S8ImXrQhW823EBnaAU6tCgvsSnQ4kd6naSDue0bMbgqXMgloLPYYbL+6CSK9lZREo3ar3loBNr7U20RNYGWf6gHBIlLgZZHhyFQk4DQQTNg8Wgt9q3ouG8Ok4VIHuSBNsvNTKDl8Kcaewbej7clUPmI4inS2YflCkTf5kHUHCS/e9QeXTYYSYvF/hh7c3P37Nn3nPhzYoHK45y6s3ZJoP2KKwY6yiymgyLfZ9y+HbYRy9hHnY5AwYBQtrMbL9BtT4EG69DBAu3Yk0lcsvyVkgZ6kG5aNNXORf0Ya0ALp/cuYnyyAPiCJo3pU+k5co8v/8Xck0jjY8NOgeYharhR6HCBdhm0flwh0O3EQZtMY7W1xgxs3NjvcjBgM7zsIAy6AFoT6RveWKzZL4Fu99UljOExsOW6r1r3pkagsxmgp6QGCtSsLWqVw2pFoAugh0A9yAOdQqDFUcFPx/dDd01LbzafJ+2ZOZMAellqhvgYge4gNYs1PZZz9jzQVqEV+TZtR+bZ8l0pn/s7I1Ddt+z67sVtP9v939NSxUBDn2NHNaissLs2rLks6hY7qQt09t2Y9Fosf9kl0G2vnPnKrPOyMZqYK2ZA5puGr9faPCqa/jlgVHIcPfazZyP6pVG32OZfr1wRWylfyXn3azc1Wxdo94Ki7qGAHRNo4/Iu/QmlM2cVaL3apnZkiU0TCrRPPZWHjuDTwOkxBuqq5v4C1fpMTfa2JtAgMWh55XK0x/CCcse3zAO+3P+adlRs1XL6NN+j+s8OAWnw9EhjclWzhRTUWuKRNlQtjw/WkB1YE2jLCfkkfHHj+3L/twh0xOnWqWYvzJYJBrYIJZFeQynQ8u5vyM/ZHyrQADU7XqDl6lYdpUDzDwbXOZ4BzulxaBlUW6J7ZOMAgS6HqsU2n16Jg8/4Txkn4agtgdam1hskOXy5e3gGNWmxcjWyPNjOk+a+63sNgHb9RjrE8oqqToEeKAK1V7HcAJiKqsWeXxS7MFVymWbPA9VSbAuyVfrxWoEOKHhkyybHUKDK8EevMnpMNjulZiidhvqtmLT9dXoItPm1zQbAVAQhUP1tXW62Jt/+CLT3Ofty5N67jLILPyBVyBa9Z/97NC6151QCbUi/QqCBE8QYaMt9rQo0H+ocUMDwKj3FsMVGAi2Ya1uMYg6rw47Sj9pvs9/ZziVoKW8SgTKsOilLEGj+omlOubOA4VVOyJCcARsC7aAp2XKe9ZxyMqp+BFQ+trkIN7LRltqcrQoB026x71zW3FugvTRSm0Eep8DwBGpcRf2VjuZ09XlnlLob0BYOTitQTbo/BIzeYvfeEcOfe69946rmAQLte5Aq0DH28UKgtajaSR1ljlfXsY0CsN2goZgayKG5tAKdthkwATqLbW4UU0jn596R3qgDvr8/zqCmVdukc2WQjTpGCrT7V7axtGy9a4x0fPnWDwX/0FhM+HPv3H//6t8ur6LojJua+wq024NNs+77+1Z6wJMbtNZm5y3oX0HLdMyE0/FWvCfNlbsW6LSGxMfTorHYURT9ZRp4br6Yezem7hu8STqyQI3nV2bYX9lYoMYtHf3PTCGjacZDi+VQYxdq5gW4EWg+hTXxOlEEOi2atfA3pEcZH7oJQa0k0mc0hKDypJKpISZfOp81On+4SPFZ33PNax1BdYMhxxxIAu1Xa+3AMgnKduOkGuRMq7TJlms4UN/OllK22+h2Y9r7dfHGgx3pu2gI28ZlMeUum1SgufX3tzMIdMQXnaOPOmQ96QwCVSo5cKE2BOoHYTwTqYv2fq+RGzKrTBqBSmGzjwL16tYc0pj6A+fyz601p6nSYmyjo3Nt1IwmgSLP6dF14ZUIdPZHenTSJlAzs+Qem0ygak2TCrTn6T3uT09vYblVUtffdWuVVZ16vRkLtDgvHyTw8+ovGu0k0pnG1zZr7inQUQqrpdcPOHNsz3ZwhV0fDDx/ZPV12pdNHvReoz4HZcPa1i45q3T4YV2nNsrZ28u/XPRpTHn655GbvUR8F+iUwWdWX9cH1d8PO7yjbgsC7TjEBsblz6IWxdrth9UO6NPgnuWDQ3Q70ov8z3PvffXv4uerTnYFDUagE4m0p0C1ie/jBNrnn4t2gXYeYgPz8udQTFll+6Yn29EChbnQTSLVH81pOxCdSKDj7Te/QNVdO8vsVmstG04242IAACAASURBVF/URPfy6GomdY483Np63FCBSqO4A/JhwQHLF2goi5HaqpH3jc4HZ+0K1FJBbrHgislTrvrUd9CQbd/0mfzL/EXrceAc/7ezm/3m9kCgDd60s9a/T90eMX755rS5WD2Nr/Fsv1489pwV/wXa4yk9bvHELkkQut0Wz8zbt7hVU69y+t2qUySnG589dbTWc8JfXTk/bGYIgc6L/wKd3V+zNyBjv1TnfvmR/OsxRXczu0BH5h8FMlwoCbTX4e5aAj1osdjmu4x7fz3XSqQ86losQ7IEGnKNign57UjPhyHQkUUHYM+tlYFemA6dxZ584sND5abOxZycfPF7z2M1ApWXzrsjeIF6TTkv1EOhS74QgaGxmDoNf3rHBequEY2PZW47uvJBUcgMG+814/De3g2B9pkYWvSVCAv9fqB75y6vxH/R3lU3NfcSqJOqh+LO40MFqvvYH4E6u7mX3rUtN1PqXqu/7CsRFNr9QE/fF39eEy51speI3c1EHONCTvnedfv9R0IbjpGSmuw30Qh3AnVTrl8o31Lzlcn89IjW/UCPkm2VD2fekd4DHAl0f1sGjj2qaJLk/uS7RnXh6OZeePyZ0y7QPErdjWsRBK37gZ4k+zCd+L8jvWts+qnMQZJzOU1rmNCcY/YWmqz20FEFWvlnY6IdW2AAHQIVvffnF+faD9SbqMqmpcqZczn9yLiG6a7RvPftDlij6Ss2RaQ7cCnCoXVD5fRhHvPtSO9Pv3RrqTFNk0bZGKaF0t2CQB2DQANEY7HDZPQzHQqd65lIHk2MCIY3Rjdk2VSSV1+1Ge5bxzQKVP4lfwP+obHY41X02jfZwzkP3UzDL1Gg+XL1vJdefr6/LT/z6mu5gBvdiM4d7JCof+gsdpisPzqJor1VJD3i2GbN3QJ1Ua0x/abJ96U9Pval5xrJ/xr49Q+DA5glNuKgbRmCm2d7wli0FvtWdNw3h8lCpHnyQD3TTC+BbhsFup9THOfXV7MPt7kZbcOg+QjohM2BTlos9sfYm5u7Z8++58SfQaUxCfoLdKsKtPhE3jrJUSMhbJoFytyRv/i/nZ0vDBCo/EkmUWfNmo5Zb+AdsYdGoHTevQWB9qWfQKuvFjRnNPENLOeM7/Sg6k5/ee9pttgP36c/H7/6Kzf99214Au02ofR79TFwrho0LRPexwf5Gpzsj902SOvkEsxLk8WeXMrn3cWmTG72YgpvEmnb1CLlE+V1QzQaOMM0NvrpG/mybwS65RL4S4PF7q6ifPX7F2IS3kkS0xIFqv5icQIdhp07nn0zMrgKnlK32EnszFNfZ282n8fvnGzGtDMCXX7OUiNjxCdPOyNQ8JmaxcRe9HLMeeToiR5hClT3UKKmY8uTHDbJWwZ4T7NrGwIF/6lZ7KiSOL+5wUqknCaByvPtYESrQAF8pmox4Uu1y37iaClSl0D982erQJcfaE6oM8wJgVC1WNyDT3ayK3m8muepnF4KqVmgHj3TzSGTBYToE4KhQaAVXdY/sVRziAKttqoQ6PID0MECNRYuXXcIBgQ6kFqzih1EZmnOhEwlUDKXIBwaxkDrXfi5HunhI42pTD4K9MD2AhYLAtW58aDPQQD+UbPYYXXS/Sia5KFy3glIQ5NA9xs/n5nZd9+V1hJVPqsdhzIhUGoCrU66T5bG5J2BmgmjlYJUTPO5Sd3GsuFN+REChUDpTKQ/nCqRvrIPnIsqreO99D0I7ioP6u0+BiAcmpdyns9j0CefOFsMrxdofVtNXwmjlXYwnhPqKLWWRW9WDcAcNMzkHIkdRE798quvvvryknjpZAS0KtDKM4MCMZO/zbQf01koUdOBt14PwFQ0TYV/u4pK9v7WVc0tAvXYTMrSTX+bad9Cw8WWrmVvKKJVpAgUAqIxlyjpuKf6PP+9s5o1At33O6uybObWY4E6sJCpQA8UN+p2SC63nkegEBC6ZMw//funV979ypk9t3WByo8A9tdMqkC9neuqxn3zWCnbDbkq0PKjA/XYLRuAQmD48kwkdQQ0EIH6NlTbLJ+5BapULgu0KUUAfUJQeCfQrC/vl5kkVIHO25YqeoHOI6YGdcuP6GiKNvEnBMX8Ak0DTvkhbDM1qB/7xUjDzA1poPn5t/PlguqzPmvDo1uCTwgRHwTqX1+4Da8F2txft6KmsYXUZFmNP1nSCeHhhUA9dlIdb1tZTnAfVH9hoxM/9iFHih9ZEw/LYHaBylNGQSTQe9vCfGam9ddjizc766C6Ll+3KxMChbCYW6D78rrNcJYg+UiHfeYUaOHRbfs0EQKFwJhfoNvtNjCBetpGxwI1o0GgSBKWw3iBvvjd9fX6v/xT9uaz+M37d5LXzz5av/73D5OXt996WK+5XnU4Ag2hmRXm8VYuUPm18gMgZJoFuvn0p+WeoI8v/0XLjvTPPlwn/Ey8+TF988bvk9dvPXx6/W3x8dPrHzfU3CTQIU2fjTAFWmBorvFnqRU35QrgVAiMZoE+vyjtAaq8qfLi5vrNO9sX/2P9+m/id7fXb93ZPru5FgHng3Vszdvpxw0BKAKdC1OBjrNbbUVSo0AxKIRFD4E+XrUI9FEabsa6fFtEmlnsKbSZuDOx6KN1QwCaCtRQRDPfaGGMNOhAoAC2aNyRvor+oXJxACrJMbFo8vMDSaAvbjYFoCEL1M9YueuitGwmZ6P47tM7lh0hUAiP5h3pK1zTnv7jh2kAmnI7s+kjIdJEoOKT5gB0jEDn37EnYIGaXrvRaVA9Spv97xVgGHWBbv71ypXLq71zV3Le/Vp/+tPrbz38839dr9/8l60IR5MRz/TTYgy0FoD+JCNggXrZhXcsUMvPSLZZGMBc9BgDbSNW5WfpLPwHVYHms/C1ANSGQE3OsskuChQAqvRIY2rjkUhgerh98TsxCy8JVHTsRR7ozx+KAPRFLNn3q+Ogo7rw6Q+Tc3eXUUOg9rXL3x4sgNZE+k3njvSPktBzKwY7365EoMURH7+4+cadZx9WZ5IsCJR70AiT6+ZAoPztQfjoBXrvHfFE+Od/9V5bLPr0uuTMJoGKADRJp39Q7ckLgRr4U7rvuAXN8EKgs49jA1hAJ9DN52L6PRboxehUy3BolvmZvVBm4bfZy4/TrKZHWaha1my2ilRaT80taEb3dWvI0SQCBaijs9hhFJ36m9VLv938c1saaNlrf7ROJt5TST4oZJlMwWfZ9G+rp0aR0Vz2Qb5lMFmDFfrmgZoIFACa0Aj0JIquZnPxd1cteaDJ2Gf684PKSqSEZApeF4GaCvQgkyi3uUK3QLPd5LovG1cWoBcagR5GF4pkpqPojP78p9fF5kvpLHy2MD5bCy9Ic0B1Y6Bm2ZQIVEPn5ei5mxwrggD6okljurH360KgrWvht4+uJ2mgryd6fCbtxpT8MrGm6Oc3zcKPSwJFoBV6CLSXG5NFl1xbgB60JdJnAu3Iqn8mtgD9uzv5m7WU8pkvQtLngRrQfyQPVA76BZf5IHP1MwCoMl6gxjXPtxn+znKgeWxn9aBenwGArgsvJo4yc560TcOPqNlIoHTcxzBgBULH7scAsNVOIiUTR6lAY5m2TCKNqHmEQLmfR9FnugmBAnSjsdjjVXT+fiLQJ5ciMaHkoOZxAmWuuEr/qzFEoET8AHp0FjuKoujsau/cK/HPC25qNhToFoE20/9yDMlkQqAAerQW+3aVb6fsxp9mAi0H8RBolSGXo+3YAyXXHoEC6NFb7Icvz8b2fPm1b1zVPEKgW9K969gUqPyOqwygY75cItKYLDNIoL3Lydd/AkAdBAoVmp/3BgB1NIn0/4/87t7/tuREetzQjCxNBArQiG4lUrkB0+bzyL+VSBbv6N2VQ48FSfmc3c5eI4BWdALdyw1671Lkj0ClLS377SzUo8ClyGHY9+i5opNtrwDa0Aj0RpQaNNmY/pSTifixAh23IEnJh1KKDhUnAl3SvzAA9tFY7Hm6/ujeKor2/tZRzZ4IVPogaFXYEqj0aeFQgCYs7TO0+fJ8tdxs9bhYB/mX6U4ch207cois9SJfffOlWP7z6q+ay1Y5GpvmrrOYMOj/9UncDn/yQOVgqM+uQj3LzH4mpYesisEC1VzBukDHtgyWiiWBnlR32zjMPnh+8fT9x6vk9eNVy3MxTuQFP4/zJUCnfttUtoozgQqDxuxdHVd8W80GAtW+M6EMPr3tqDptU02g9YDTx2sC/uBGoJvDKPvgSGwKd5jsxdEagB5FZ4rfbm5Ep0XQd+9SUshsAk0M+r+42Mcur3mcQEeRzo+o4wE+4rRh2QRRtTZfrwX4hxOBimnr7IPEnYlFT9oezKZosNh98/lFcfZ8AhUGfclV/307bx6oEnwd1ENbbxzi0qAagQL0pSLQe++Ixd/vCX+dRBfurqJTv06moU99k3nqySdx9/rVVCpPxMFnryYhY2HMbbKN0fm7VYFubtQC0CfFAGPSgS8KKB/hdhhdKMt+Io9HCke/fDU9WjTs8UpUVLRoCFWLbT69UnA57sInL37qz4bKGeNu9eyxdI0T8D0fHDQRjgV6UBOoN98cAkAV6OfZ2GPacz63Eg9ETw22dznzVPL7JMEnH6k8UxPoqV8VYWMi0MNYoPUA9KQsSxXoiZx1WZQtHZ5uNZcNmiYCjf15TW7REKoWe34xquNNHmjBuPnhUqBN+vBpOn6A0UzaXBWoR98cpuO4D00nKgI9ifbExPfdKO05pz3pI/Hhk0tR+pTf6Pz97eYL4ZPYbGJ2/d6qsZt9UhkDrQegse1e+16Et8kAqdwRj2vZe+/7SlHK4fGbuO67idbEmfEZ17Zqi/oTqEC3B8OH65Qt2homTCR9eKGRgY0wE6iVYiBs7Aj0MLVY+jigk9Rs6RuhrAtl5zqVluQprUDzWfh6AJqXlc7XKyOZT5Lp71d/+b1UlHJ49kZEtuLMuI0Xtlu1Rf0JaDORsTd7ZSu8pnOlcdFBTXPCHAI98Gf4F/ynNon0wx9+EevrWjGV83iVho5Crumz0rMPY2ud+jo/SyvQJA/0vfsiAN18EkWvyVPt17IjRQXqVNAmmYbKDk+KUg4v3gjiM0vxly3qT0gCrd3bA5xxcCB1V7XzRB4I1HhtlJFAbRQCO4sq0DT0i2SBnkhBZzoemXdpk3HIU79MlKgXaP722ubGS988uVh05ItwMW1BfS79TyKZXvzqJH26m3S4EmoelRmkcov6o7HYobP8+bLmQQJtmd/oddMrAm07bkCZTsgaOuF8TjFuwTw8DEMRqJiE2Xv13a+zLnyiwBOl166MCd59JXl5/n6nQEUAmqTTHxWhY1GxTqAxTy6KT0/Kx2Nuc4FKjY6teS4f9ZRa1B/dZiKtaVdW8FSgs66MV1eWVl85qlIe+EWgMADZReLpvfe35RhoJtA0aKwNeyZn/Ns7afjXIVAxAnqUHpd7Uh+BSrUklXdGoGekxKeiRf3RCdTJvJFa8xwC7Tx2ToMczC9QgAHInsi9lAZfmQLVMdB6VLb5opCcjPJBMgV/JBW6bRsDlWopBdo6BqqGi2mL+tNssWK81yGDBTqScvuRtqJmFWhjjOy6RR6M+0KgNAn0RA4qM1k9v5jOwp/OhjxFlzw9MzFsu0BPstlyOQJtmYU/KpKGkt/pZ+GP0tcXMtMqLeqPxmJH6XpSl9gQ6OA8Jq8XK477jqO/la+XBbyl3oWPAzilV34k0pnKPFBhlburZGlR8vrJjVRyFWfJAk1zQCtjoFli5w+fNOeBXr2fLD1Kc0zT06XD0zzQe6tCzEkik9Ki/mgs9kN8HV4+ly9J8mAlUotcBt/4fpqiGn8ObaWf3wqWjDQvdC1bEBSd/qIM/PKlQNlKpGw5UCQ2mMvX/Ygtkx6vIlWhskDTHFDRJ5Zm4WtLi2p5oPKKJxHkNqxEKmPXJPqUW9Qf7SSSV4n0Ok9KAm31h4GaJvdRpcLBE+IIFKZGEWi+wvxEGdZM18Ifymvh0406k9fpwnmxnadOoPkiJDUPdKsubq/mgaZr8tNM+qzs1rXwaddeblFvAhGoZoJY2g+jl0AHrAD1SKD9XGreYNQLjjl0n9UzE2Ek0rdNoBcCbfGAwUTzANfaQV/VyL59v6qRKNgnm5kxXCYZAksQaPVF6zHD5mT8CM4mEagX3xSWRdyVFTNLh8NSg0IiHIHqf1d9YVyUclyvo6ZBXiNULgiw20IECg64m83nLDUAbRPo5ruMe3899xhov+hyVHMG1jo1xQam5T4odlvn0XeFBSEmdPbcLwyfDZ3FkkkrbyaRdHKs9cjt5nn6JJVsHcDBQbEqtfrVZ2oYwA6je6yxMg1/enaBapBMUgxZ2nzC0ZTb03fUku8BvS169PIXna6ZAFCiXYkU7Z27vBL/uXoy5wCBat0gCzT/o0GgJm6RxxyHnz24mu5Gqu2oTHGNFCjyBTBCtxZeTJulC1mPHM2g2ReoNp/JyA/lUKNjgfYc0mxoh0WBYlAAA3SJ9NISU0dJsDYEKmXglAIddH6PSj0RaMMB5ZnjBYpBAYbTup3dSZ9H0xvXbEWgW1lznamiviFPBvVoZPMRPXc67WiHr5cIwGc6BCp6788vOunDT/dceH/dMFBbjYfbUR8CBTCgdT/QdCGWo92VrQq0NZvJUzkM73k3deItfTlPrxGA3+ieiSQ96TPfadR2zTYEWizKaVjt2PjwYn+Q9sgv0rA6z2gqxEHbAKAXGouJDUi/STca3TpayGpVoAeNHxcitVKJZaQFRblEjYqx1yIAGIjOYofJ+qOTKNpbDXzKUu+aewu0zRE9BDoet/PwWwJJgEDRWuxb0XHfHCYLkWbOA+2WS02gdlMbna86PyCQBAiRFov9Mfbm5u7ZswO3aO5ds0WB2jpJX5i3OgaA+QhhOzsfjGPNoONGPAHAJ5YuUHvis1cOO38ALIQQBGqAtEjcP4FaLA0A5qRqsc2/XqnjwWONB9J/k6PeJY5pRFmI16mpADCMmkBvRHVmTaQ3c81MglK3nNNvn4RAAZZAzWKHUfTy2QqvhifQmagKtMxHPbCcWgUTc3w8dwvAQ2oWOxLPn/96ipoXL9DyfbbpEgINClWZxxgU6tQt9qd3okkcGppAe7XjoJLDX25X58vXgN6oykSg0ECjxe5ditw/Sm+67ezs0Ge/TiXQzOWJQAPlWHEmAoUGNBbbfPmKa4cuUaDbqjIRaMAgUOhEb7Efvlw5dejyBHpQCjR15raYPmLtUYBkAs3MiUChgVaLPfkkcei7ThwamkC7FVpGmgdEngtAFujxMQKtY2mv9c2X5+UypdzJJ5eivb9Ms9AP2/Y0+lbeMy7tPr/6q3rZdY7GbjXXZbE/fTJzHqhH/hngwnK9u0fth2FIAj3OmLtJnmFJoMpT1x6vSoGKpwk9Xp1JP255tOWJOOFCpYDoVPlcNy3OBfrkF6uZBeqPgQY8vc2jVoMpx6o6MWgNFwKV3xyJBwIfJo8Xag1Aj6IzxW/FI9lFl/nepT6PxHQr0HQYNNo7P9dSTr+23RgiULctgSlAoF24EOihpLTEnYlFT1qfrS5r8CTfvzh9INF8Ak1HEtzNIoUn0IZtQQ8aX6bHum8POAWBdlER6D2RQv5ysn/wSXTh7io69evt5vO4L/1N5qlkUuXVVChPxMFnr+arx3PNZc+zTCkEurlRC0CfiMHFRE5JB74o4Kh4Fau4LLs8fJvmab58NT1aNOzxSlRUtGgIujSmJBXUaR5TmAKtfZL/qP7Op3aDEblAtwhUgyrQz7Oxx7TnfG4lnmWRGmzvcuap5Pd718rX8cGqQJ9fPP3vl3LLJgI9jAVaD0BPyrJUgZ7II45F2dLh6WrLbNA0EWjsz2tyi4bQnEj/jmt7bkOchdfvDtIgegQaPJlAt4ufiC8fX9v+s4Yi0JNoT0x8343SnnPakz4SHz65lNjq+cXo/P3t5gthuNhsYnb93qrazS6mgK5tyzHQegAqnnr5vQhvk3hV7ojHtey9973UqjPVw+M3cd13E9GKM+Mzrm3VFvWnYSnnJxPYcxukQOsg0OWS2hOB9hVoNngZiyiJChMPpW+Esi6UnetUWpKnZIHG5o0t+8Mnyfn5LHw9AM3LOkx+KiOZT5LO86u//F4qWzk8eyMiW3Fm+uxhtUX9adxMxL09t4sRKA/oWCq5NLeLF6gxtUmkH/7wi0tRKtAkZny8SkNHIddidFN8GFvrVLHdhjrxnqtOWE3kgb53XwSgmziue02ear+WnSoqUKeCsvHH9PCkbOXw4k1a3YVS/KcMNgBp2s7ulCcbKnsZxDVFml42FEaSSTN9iUCbUAWahn6RLNATKeiUthqOz0rGIU/9MjFL00z5ifQw4DgA3dx46ZsnF4uPinAxbUF9Lv1PYgpc/CopWzlcCTWPygxSuUX98XlDZS+9pBGoj02FUVQFuj1mT9AKikDF6OXeq+9+nXXhEyeeKL12RSh3kyQf0V9vFOjjVVG0CECTdPqjInQsKtYJNObJRfFpJlDpcKXRsTXP5aOeUov6U7WY9C09EKiHWmqYiD/wLFkAbHBcE2ixQygezZBdFIdeSTb7RhVoGjTWhj2TM/7tnTT8axZoEW6KEdCj9Ljck/oIVKolqbwzAj0jJT4VLeqPzw+V81+g9h+9BL7QtJEIAlWRBZp7KZ3TzpyojoHWk+E3XxSSyz4oRyvLzND4kCOp0G3bGKhUSynQ1jHQrMVKi/rjsUD7r/uZEqVJUgM9bCqMAYF20yTQEzmozGT1/GI6C386G/IUXfL0zMSwcgR6qJyYHn5tW41AW2bhj4oOc/I7/Sz8Ufr6QmZapUX98Vqgk7RjDAE0EQzJBJq9LsyZduxnbJdP1LvwcQCn9MqPRDpSmQcqVqnfXSVLi5LXT26kkiud9XglBiHjM/KP0hzQyhholtiZZTvV8kCv3k+WHqU5punp0uFpHui9VSHmJJFJaVF/EOgYAmgiGNK0mXKxqHOmNvmGNGNyLVsQFJ3+ogz88knpbCVSthwoEhvM5QnzYsukx6uoVGi2TOilPJMyzQEVOVDSLHxtaVEtDzSSVjyJILdhJVIZuybRp9yi/iBQgCYQaDeKQPMV5ifKsGa6Fv5QXgufbtSZvE4XzovtPEs3iiXp5f5F+SIkNQ90qy5ur+aBpmvy00z6rOzWtfBp115uUW88FqifoPUdQfM4D2luHnpz2LqZUsgg0IEg0B0BgVogm5kxXCYZAgh0IAh0R2gXKAbtRdzHFzNLh8NSg0ICgQ4kF+iBb3vtgV1UgUovjxFof+5m8zlLDUA9FqjncmL90bLROhKBDkJM6EyyOdFMIFBDDg4GPOEDQkOvSAQKErXNRD6tbsU0125MnssJgS4avSF5PidI+LuZiOdy8rx5MI42QyJQKPBXoL6COHeBToFiUBD4Ogbqb+/Y24aBRboFikFh65FAq7sc+eupg+IPWCwIFHrhj0Ar+2x6LFAe4rF82gVazMOj0V2nRaCb7zLu/fUEY6BKn93vB7V5rXewQqcZESgIdAJNNi+ZchIpF2gIuUEhtBHG0C1GBAoCjUDVyfjTkwrU/wjP79bBeBAo9EMj0KMo2jt3eSX+i/auuqm5UrXyfCHPFeV582AsCBT60SxQsb39/ey5JEeOdlKpCbT8w3uBwsLpJVDm4kEj0Gz/vnS7ZkeboTbmgSbiPPB7DgmWDwKFfugEmswbpfvyNz212UbNStW5MAk9wQMQKPSjQ6Ci9/78opM+vCxQprXBK3qIkXR62OrHQJMufLohv/zsUps1l1Wn+xqhUPAFBAr90MzCp0/RS4dC8yfO2645rTqg7CXYmWnn/gLdjesBOjQCFQ+i/yZ94PzW0QNNUoHKnXc68v6zI8YYItDduCLQjG4l0mGy/ugkivZWkfLQZXs1N87Cu6gJ7IFAy0OOj5lJ2nm0a+G/FR33zWGyEMlxHqiSQg9esyO26ClQ+SfsJC2bifwx9ubm7tmz77l5ImmjQJ3UBPbYEVsMEOjOjAtDEz5sZ4c2g2FXwi0ECv3QpDF9Kj1H7vHlv3A4ieSgZHAGApUOQaDQnkjf8MZizdksvIOiwRUIVDrkuHjltjHgMT0E6jQPFIEGBQJtOGQ3LkkTlmKrzZfnq+Xmq8c3n6wikVEpeHIp2vvLtDN82Dax/a2cOLT58pUoil79VXNFKkcG+UY1gTY9ltNpHqiDosEVu9JvHfTtln0p2rAk0NpuG4f5B5mNklrEkvLHq+QXj1ct+xudiDNyEz5eZQ47VW7uocWKQNP6VRzuxoRAg0IW6IK1Mey7LflKtONGoCJ5MvvgMDr9zfZJsrlmrLfYQ4fJGvPWAPQoOlP8VuzLKcLXe5f67ItkR6Cbf71yRWylfCXn3a8Hl9qr5kCfC7/TKAJdrjeGCnS5V6IdJwKNZZcLNBs9TJeUJ+5MLHrSGtLJGjzJe89pEZMINK3OybCnWjMCDQ5p4SICNT58OVQ8ce+dWH0vJ3njJ9GFu6vo1K+3m8/j7vM3mZqeiBHNV7MRTXHw2atJlFgYc5s8DOP83eztUfHzgiTQzY1aAJo8wi0ZKk070GeKwvJXh9GFsqLy8G0q7JevFtXE1hYVFc3roEcakyOEQOnAh0Up0GWvYESg/VAF+nk24pd2ls+txORJKq29y5makt/vXStfxwdXBXrqV0WkmO/lnrxPBCo+qQegJ2XBqkBP5AdiFhVJhyfCzgdNE4HG/rwmN6+DeRPpEWhY7IRAh3+10K/Efkbr66YTFYGeRHtirvtulHaW087zkfjwyaVEUM8vRufvbzdfCKnFMhMT6vdWjT3r7INsV02hs7iwfAy0HoCKrY++F7Gu9ByNvIHR3nvfV8pVDo/fxA25m4hWnBmfcW2rNq8VvUB/+PJsLOmzUvV2QaDhIUwhJKp05peGwT8NgV8KOwJNt8DMnqR2ksosfSMsdUHtkGcPDUrpJ9B8Fr4egB4VAeuZbWUkU7g7il795fdSWGBKuwAAIABJREFUucrh2ZvD9PFvF9IN6NTmtaIV6FExB+9kLyYEGiKpQLM93JYaghp8rYVeiU5qcyU//OEXl6JUoEmYmJhvm8pV0WEsqlPF5HQPgYp6RB7oe/dFALr5JIpek6fa855+OlsvG2uTzEllhyflKocXbwTxmeW/Aqf6zZ3rBCr8+fK5K5dfcWZQJpHCoyrQRXoDgfZGFWga7UWyQE+koDMdgszzOpMA7dQvEwv2i0CL313b3Hjpmyflg4aKcDFtTn0u/U8imV78KilXOVwJNY/KeFFuXiv6DZVPZ5NlN6K+0ewwEGh4KAJdXAhaDO+anrlzVFYsRtHeq+9+nXXhEwWeKL12WaDbu68kL8/fHyZQEYAm6fRHRehYtEIn0JgnF8WnmUClw5VvEFvzXD7qKTWvFe0jPaQWT/FUTggBVaBL80axQbLBmQ6aEwCyfoQn7m/LMdBMoKlHasOeyRn/9k4a8ekEqs7C57+6lirypPCkPgKVqkxa0hmBnpESn4rmtdL6ULkUOX62CAINj+UL1Cy7YGEXojeyQHMVpdPYmfLUMdB6/vvmi8JrMnLgKv9MA9A8m74wnXYMVKqyFGjrGGjWfKV5rcy6GxNzSGGRWXPpAjU5035rQqBJoCdyUJn56fnFdBb+dDbkKXrh6ZmJYbUCVVYiZb+5tq1GoC2z8EdFHmjyO/0s/FH6+kJmWqV5rSBQ6E1doIsSh/ljNhd2IXpT78LHMZvSKz8SMyhlHqiYWLm7SlYTJa+f3Ei9VtFUfraYDC/WwqcfiFeVMdAssfOHT5rzQK/eT5YepQmn6enS4Wke6L1VIeYkkUlpXiu6LrwUx9a+nh0QaHA0CHRJ4hgl0CVdiN5I80LX8l2ITn9Rxnr56p9sJVK2AigSe8rlS33ELkmPV5Ud34qQ9Im0G1P6i2vbdIRRmoWvLS2q5YHKy59ExNuwEqmMXZPoU25eK7NOIiHQsECgLWdab04AKALNF5WfKMOa6Vr4Q3ktfLo3Z/I6XTgvdvBsFmi2al2egk9+KnmgW3VxezUPNF2gn2bSZxW1roVPu/Zy89rQpzFliaR/ukQaE6QsXqCm32jnFn8O5bA+f7QQ2hLpo7Nnz7pbioRAQ6N8jC8CrZ6JQBupzwItDa3F7uZbOe91b+lkVjMCDYyaQBcmgjHLUxFoI3EfX8wsHbqZRvEBvcU29y7HEei5X7n65lHECGhYLFygx5MKdFnBu5a7WRS21ADUi+fCQxjkApXSdrzTgHl7jketTkWgGsQczl4+X7NAECj0Rbrlyyf6eqYB8waNy2xFoDtK1WLPL0ZO0uYbakaggdFwy3ungZECHVHvwMO9u3JgBAKFviBQa2cj0KWAQKEvTbe8bxqYT6CDnyTv26UDExAo9KVZoF5pYERgN/KLDKoYgS4GBAo9abzh8z00J29NM/MJdNBFQKCLAYFCTzQ3vPn6HQfMKNAsv6t/Xb5cMxgDAoWetAnUFxvML9BepSDQxdAg0Dqu9gOFkOgj0LmlYCxQC0E0At1FECj0RC/QbS7Q2bvyxrGwjVGI/rvhIdDF0CDQvXNXKvyUZyJBjwh0/rFQBArTwhgo9KRFoNmfCBSB7hp2BPr0+lsPkxcvPru+Xr9/J3n97KP163+ffnw7+7VSMwJ1if3bU1divguH8X7uFjEVqJWWI9AdxIpAX9xcp4b88cO14I3fJ6/fevj0+tvi46fXP26oGYG6xL7LtAXmvfegBWqlcgS6c1gR6IN1JtDb67fubJ+lOn2wjq15+/XfbJsDUATqFvs2ay3uOJ9LmtUKAybC1dMQKJhhQ6BPr2cCfXo9iz2FNhN3JhZ9tG4IQBGoW+zHg90C7TrIOYZisnmlepWEQBeDBYHGHfh/TMdAH6yTHnv88wNJoC9uNgWgCNQt2ayO3RLbfumNQIe3YS6BYtDwqVps84ev/mNg0tLt9dvZJNLtLNR8JESaCFR80hyAIlCnGKqku0jtL4+Vn/PggUB71e5J0heMZ7zFHq3FZJEQ6IubyYhnNimfj4HWAtCfZCBQhzgICHsJdF6DmgvUdht6HJPtw2KvZpiB0RZLBjybBJrPwtcCUAQ6AQ4CwvaijoujEGjPY/zayAqMGG2x22K8syZQMZkk8kB//lAEoC8+W6/fr46D0oV3iIuAsO8Esw8CHTRJM+Fcm3qID4lfMI6xFnuQzL83RaAZcQD64uYbd559WJ1JQqAOKe9Le3doz4LmF2g2uohAwT0jLfb0euJMvUBFAJqk0z+o9uQRqEPK23IGgc6nBEmgA4YX7Qu0o0QEuhxGWuzBuiDutSuz8Nvs5cdpVtMj8YdSMwK1jazN8tXEAp0zBC1r7p/Tbl+gnVXL3QMEGjZWBfogk+SDQpbJFHyWTf+2eioCtU56N1bG/3ZIoHLNPdXkIAjcIYE+v2hl56HNl+eld/feiaK9177JKpA21HxyKdr7yzTH8vB0S67lt6soulCW/Up8/qu/aqioxlF5Wm9qeaA39n4d//jh+0GlZH12ZSVSQjIFTwQ6Eccl8ofWSrd7nHUqFWfrOjuSBxw4rKXESnIZAk04ic6Ubz5PlZloaPt4VQr0+cXT9x+vzqQfX2srLSoFmhcQnfpttaI6NgSaXpKhFyYT6Iub6zeLtfDbbRaAbhkDnYjjfPBPuStt3aK+C7TqouxKTC9Q/bBJVpc0wBJ2IqgDgZ5Ee1fjYPNGGnTKvzmKYmseJmJtDUCPojPFbzc3otMilr13KSloEoGKFhoKdPtM2o1JkOaAisklZuEnQCtQK46YaVKmN9Vqs3mkrvx/NwJtLrIm0LmXbo3EvkBj411LS05+HkpKS9yZWPQkaglAFQ2eRJlqU69NIFCh7P/47k8XX/r6u5LO/nwx7f7ss7WU8pkvQiIPdCKO8wSeukDH36b9iwhJoG583yLQao0LEqgYvYxefk8o6yS6cHcVnfr1dhN3yU99k6npySdxj/rVdHjziTj47NVEOTFnKiUm6swGFFMKgW5u1ALQJ5/ERSTjpkkHvvDkUfEqLq+sqDx8m0Sn0ctX06NFKx+vREVF8zqoWeyIZyKFShFOzS5Qt07Q2qn2vsuPzsK/6mhCUd0UAp3OyKpAs9HLrLN8Lnbl6fuptPYuZ2pKhzevla/jgxWBFiQCfX7x9L9fypWbCPQwFmg9AD0pC1YFeiK7q6hIOrwQ3oVtJtDYn9fk5nVQs9gmvwwINDDq3cP8czuTFQNKcNsv1ZRe+7Q7n96lQKWy85d5cyoCtT+CMPCEWxmt75tOVAR6Eu2Jue67UdpZTjvPR+LDJ5eiVIfR+fvbzRfCJ7HMxIT6vZWmZ512uYspoGvbcgy0HoDGh732vTBXEq/KHXHxiLf3yg50WpFyePwmbsjdxHHizHTsQGleKw0W23z31b+v9v77VyVD92fqBQK1jH7mYg6BOjRoY+ENVXYItBaqW0QRaHH1GwWatsJeQwyuvR2BZuOV6SjmSSqzbEgzttSFsj+dekpSU4NAj7I4Vij3h0+idGImnYWvB6B5wYfJT2UkU7g7DmF/+b1UkXJ49kZEtuLMuMEXtlu1ea00W8zS4HB7zQjULr2nfi2Xb+PoQegE2vBRq0Adal4WqDRPpRWozv5GVU83qlrzxA9/+MWlKBVoEiY+XqXRopBrMaApPoxFderr/Ky6QE/yYDJXnbCayAN9774IQDefRNFr8lT7tewsUZs6FbS5lyg0PTypSDm8eLNNqrtQ/itQNq+VZottPnXyJGO1ZgRqlzZTtP16dPkWjh6ERqBNx7UI1Mn8u1q6VE9HBLoIgabRXiQL9EQKOtMhyHxMMBl6PPXLRDM1gZ6s9pQos5hMT15f29x46ZsnF4uPinAxbU59Lv1PIple/CqpSDlcCTWPygxSuXmtzGcxBGoZ/X1z3P7r0eWPP3oQTYbQfKYXqK2pNQ2FFI+PpZfFb6rH6gZwjWo2Os0IRaBiwHLv1Xe/zrrwiRNPlF67Mqly95XkZdxFrwn0KKp0oB+vinpEAJqk0x8VoWPRCp1AY55cFJ9mApUOV75BbM1z+ain1LxW9Bb74cuz8QU5+96wNUn9QaCW6bhzli5QbRs0v3Iu0K1izaLZSxVoHGAmCewbVaBpnFgb9kzO+Ld30oivItDPq/4sRgK2aQ7oUXpS7kl9BCpVmbSkMwI9IyU+Fc1rRWuxMp1peHJpLxCoZXZJoE2pBkNacnx87HYz40aBFlVXE66acrAMDTqlPxWB5ipKp7EzJ6pjoPX8980XhdeKjw7TZZdbZbSysJqYgj+Sati2jYFKVZYCbR0DzTP45ea1orOY8OfL565cfsWZQRGoZXZZoG2yaY5XfRJoQyNbp7/aKzY4yZQmgZ7IQWXmp+cX01n409mQp+iFp2cmhlUEeig561ApJT332rYagbbMwh8VKZjJ7/Sz8Efp6wuZaZXmtaKx2ONVuoY0WZXad0Z/GAjUMj0FanyHDRWo0/HF2kdthzcUIP1wR2nBikA1x0o/wxNo2oWPYzalV54MZ5Z5oEIqd1fJaqLkdWyXM+oc0ZEc88UaOn9fnJ5/luaAVsZAs8TOLNuplgd69X6y9ChNOE1Plw5P80DvrQoxJ4lMSvNa0VhM+mdg012IEQjUMr0FaniLDTzPa4F2n2UDRaDSrLzuWOnXJgKVIt6JkOaFrmVrgKLTX5SxXr76J1uJlK0AisSecnmOvOiuP15F5Zr1osQ0WEwnnbJVl1kOqEiIkmbha0uLanmgkbT8SUS8DSuRytg1iT7l5rWiSWOS16B2h7FGIFDLdAq09QYeXf7o4wcUPEigTRM0llvUUnFt5kifFbDNj8z+kgb/i2UctpqiCDRfVH6iDGuma+EP5bXw6d6cyet04bzYwTNzzEm5/jEpQCxJ3ztfBnPpYWoe6FZd3F7NA00X6Kdz4VlFrWvh06693Lw2eiTSO8qqR6CW6bp1Rgp08HkOBVr7Lu11NQ0wTkF3x109Nj9QGT4dUFtSn0lDnXPYun9SyCDQxeCbQJ15ShFoOWTYvyHTCVTKTxoiUKOlD8dKdX6QTcb0XxkZHLouvPQvxknnVL5ZzQjULrsi0Ka4rkugx8qb6SzTv6qhAq3/o2Cc+eSQuI8vZpYO3SjEB5hEWgx+CtT+TW0k0GPljfcCVT7qUXLe8fdNoNu72RTOUgPQljSmbDH9ny6RxhQG/QVqlh5jINDjCQTao7erONPDOE2Qt2qAQCtfasT8oEPkR8QtkbZE+ujs2bPuliIhUMv0E6ipPwzOcijQ8o8s9upsiTpJ4x+lQLcDBHosvfNToEtHa7G7+V6me93b2pvVjEDt0legZmOTBifVBWrjDs9DzuKL9FCi9KW9lYyZQPN/SLz9WktHb7HNvctxBHruV65GfxGoZawKtHKMWdhWRIhSoDS8lGqZhTUlgfZqy7HU+feQ+lhEW1uPS4P6GlTvBGxntxi6BVpEK30KO668NRbo1qZAy+is51ih1JZjvyO1ut9b2iqps8cQBjgDgS6GTjXMJdCtVYGWDRpUWiHQkQ1wyACBKjNOHn+l5YNAF0MPgR73eNJvcWzL2wEtkic3LNztxgItozVv6S9QdUrM46+0fBDoYugl0PpAW8Nx2waBmrZIrrC/v1uLTH8OFofnPfhhApVe+vyVlg8CXQzdN1I/geaDa8PK1jXpWNJxIVDjW14KvUwa4/Mc0rYuQ60cFYG6bRO0g0AXQ/9bqVOgtfhupEC3xVzxaIGanrndBjDTUrsyCNR3EOhisCfQegd5EQL1nwaBNn5hBOoNCHQxDLiVWgfOmuar/RCo35PoFjAQKMwLAl0Mw5J6jiW91cqRBNpr2qmlGiXfplmgfYW6/BlnBBocrRbbuHqkcVIzArXLwKQebQ6MRYE2pB3Vp8J7a3EHBaox6MIvQ0joLSa2UXnpt8//qntXe8OaEahdHArUXFwagR5vlY/7Fb98gTbMczX/DS38MgSEzmLiUSaJQC92P1fJsGYE2s2QW8WiQMsnlRfWM6SPQPuWvwMCbUD+ynKnALxAZ7HDKDr1N6uXfrv558jRbtIItJtBxrAp0G0p0HHeGi5QfV07L9BiQHm2xkAVjcVOouhq9jCk5DHOLmpGoJ14IdBhrWipUBao7NVeAh3S118U0nc+HjkgDfbRPtLjQvE0uSMe6TEVtenpYblJg2oq9vJRPtsqQaNFgW5lgW7bBdrkyl31p3xxEKh/tD0XPhNo9mQ96zUj0BrV6emByZ0Dq6o5qSZQu8oaKtDaXP1uigOB+kzbY40zgfJY46moOWM75G4ZKtCapmr3p3WBFrVIwwTVYb1jOed+q35sry0BUf372NHL4CkI1B/qQdd2gBYH3ld5ZfKETr1GF1FfRaBKa7bqdJNylvWGBII6m7ezgbintD0XPjMnz4WfhFp0MXDO1VygRXda2warHJem1gk0b49ylvWGBMKxMl6NQP1CY7Fk4igVKM+Fn4b8PpE+kH/0OH9wfcpP3eyNE5oEWrZHGiBVz9hJ1MEWBOoX+ufCn7+fCPQJz4Wfhlq/1bFA1Xq3kkDNyhlY6bZRoIrBVYFO0Co/qQl07gaBRNtz4c+u9s69wnPhJ6JhCkf+0eP8UfVOL9DjLoHKsthlbSg9AwTqF1qLfZs/F96RPxFohcJj2/SWGSLQcd6TBTrRDVpMtRfvG0f45AmunQaB+oreYj98eTa258uvfeOqZgSqIHdcj49lgXbfM+PixuLWlN44pikbpyrVok1ThcUeIw8Rz9wUUGA/UF+o5fFUBNp254wTTI8KbDNAoJMNK/jMzl8Ab0GgvqBOH1XGBzuCzLGG8Umglc+OESh4jMZiP3wn46ZmBKqgpF9WJlC6LDJWMMfH4yU8sMJ6dc1SbdIqgC/oViJFKi/b31YZgaq0B5jtg2AWBDqxpIYIlIkT8JaeAo3sJ4OOEOgi76eOMc58WLRRJmOvR5O73NJToNvph2cBBqBZyvmHL2Jl/vSrr776dBXtvffVL5LHe1iueYxAF3hD9RNoY3d29OXwRKCL/HuFRaOxWByC5umfdxN1PrloOx8Ugaq0fidJn42h2/i6/RDopG0AGE3bhsoZ6YbK1rcUQaAq3d/puJyPV7PNAxRoQ6sX+bcKC6dtQ+WMxyuhTuvbKiNQld4CrYRqNi6GFwLd4eXuECxt+4Eqb6zvCopAM3ovMXEp0Mkv6bL+DmFX0QlUiUARqEuO5ZWUPQ50JNDRhQyucuIaAeyjHQM9U3nNGKgrBgs0Pfi49uGoJkwv0IkrBHCA/rHG51Nfbj6PxO70d1eezMIvK7H6uMzsHCTQMqHJyqVgAhzACJ3FDqMoevXKlStiR6YzSWK97Ux6Y4FaXjUzqzrybzN4ELK4CggUYD50FhOBZ8Zf3hcC3btmu2Y/BDqvOtrSO3ucvEWgAHPSuR/oe9/Hrzef/tKXtfCWBTqzOuQVRkYnWwqg8SeACcFtZ2dXoPYUZFx/nplkeDLqA5iPnRfovEFo4W+zJiBQgFlpsdgm3w703l9b3kckrdlYoBatUdprDhGN/h4IFGBWdBZ78om0lZ3tjZjSmn0S6ITPU5PqRqAAYaPfjUnitF8CtbP/kFTQsfJuIo7LZPhRxYwvAgAM0VjsKIr2zl1eif+ivatuap5foHn8Js2Fjy13SAumqwsAnKDbjUks3Iz/vCZcankNZ16zPwLNM4K20tuxFfSo33EVAOCa1s1EjpLlm4eR7Rz6tGYjgVrpaxfRZlFOEYPm2ekO7Tb3zD8A2KJ1O7uTbBuRM40Hja3ZA4GqBRe/cKu3ueb8AcA2HQIVvffnF5304ecTaLMhJxTovMn7AGCL1h3pHe0EmtU8r0D1v3G83xPrhwAWQ+szkdKhUOsP88hqNhRo+acp+rPlWaUxNbRWjj8BloLGYo9X0WvfiGn4C0KmXnXhyz9NT28VqFrRqMqaK8CdAIuhZT/QOO48iaK9VWR7K+WsZhOBHh+XPwz332gXaMNLKzlTxQsECrActBb7VnTcN4fJQiR/8kAlgQ5zkZwx3+u8oqbRzisypJg+AlgYLRb7Y+zNzd2zZ99z4s+xAh0otnKtUc/TLAv0GHkCLI/AtrMbJ9AhEzhKrDu4nZqqR5UDAL6hSWP69Kdl2Pn48l94M4kkC7RdbBVfGQi0GC8dn7ZvI5AFAO9oTaRveGOxZqcCrRhL1ucQgR4bT1epbUafAEukh0B9ygMdIFB1Iv14aBConDHKf3mbjQsAAE+pWUzdCTTbDzS8LrwaOZr1ou0KFAAWR91iJ3WB+rMbUz+BFpvLV9cVmQp08KlqMaZnAoDf1C22+dcrV8RWyldy3v3aTc0GApV01iLQ0nujF2Y2ZtYPPBmBAiyVHmOgrmoeKlA1HpxGoEqphmcgUICl0iONyVXNJgJVXvcRqD2HGQnUQhYUAHhLQIn0BgLNz5lFoPkQKgIFWCptFsufC//d905qdiXQY28EukWgAIsmoOfC9xfodi6BKtlOlhYyAYC39HouvC8ClV+XaqoKShZokRBv3Ey5ykHHHBfJp+PrBgAv0T8X/tS7X+X8hxeJ9HqBHlePcyHQbY9loKpAmxoHAAtC+1x4J0/iVGoeKdCt6ihpzVB1Fac1iXWWVBcoACyY1ufCu63ZhUALXaqjpZaou7jy/liKfhEowOIJJZG+MmfUKNDGDUOcC1T+QHoqMn13gOXT+lhjtzXbF2iTtCwLdFsLOavz7o3HAcAS0U4iOXmQnFLzQIGqoV4p0GNZoI1njmhkvawWgVYGXu1VCwB+orFYHIJedV3zOIEWP6RUzymkVRfocf1XCBRgJ9Cthb8cReWGTE4Wxg8SqMZIFYFOQS+BAsBOoJtEivxKpO8Q6IQRX1WgZdsIOgF2jTAEquufeyHQeu4pAOwGQezGpFVTudx8ToFup1U4APhC8AItFWajUUPbUkxmIVCAHaTVYhsn+9jlNdsU6HT950aBTihwAPAHvcXuvSMGP5//1XuO9qa3K9DJQKAAkKOz2ObzdPbo+cXolJtVnTYEOoO4mtJRmYEH2E10FjuMolN/s3rpt5t/dvRY+GUJlCl4gF1EY7GTKLqabSlydzX7c+H9Emhl8TsA7Cwaix2KtfDZnkxHbvYGtSVQK40ZAAIFgIy23ZgygT5ezZ1Ij0ABwEfa9gPNBOpoc9DeAm2T5CwCZeQTABL8F2jrMOccBkOgAJCifSbStcKcJ26m4fsL1EHloyi3DvGvbQAwIdoNlc/kAnX1gLkFCNS/pgHAlGgs9ngVnb+fCPTJpcjN4z3CF6h/LQOASdFZ7CiKorOrvXOvxD/dPN2j/ySSk+pHgEABIEFrsW9X+W6gjp6OFHQEij0BoG0zkR++PBvb8+XXvnFVcz+B+igqNq8DAIH3+4F6KSr8CQBbBGoG/gSAbbdAv3NXMwIFgLDRWmzz5avJYqTI1SBoL4F62lf2s1UAMDE6i52sonQ1ZxTtOdnNrpdAPfUnAIBAn0ifLuD8wyer+RLp0ScA+Ix2P9BTec99+qWcPCgDAIJAtxuTFHVOvh+oJFAH9QIAWKJtO7umNxZrbhHocf60dwf1AgBYws8INNsrDoECgM9ox0DPNL62WXO7QFlvDgC+o38q52vfJ69++Dya+qmcCBQAgqDlufDR3tmzZ8WeTE4CUAQKAKGjs9jmi3w3u72/dVRzD4G6qRkAwAr6bPbNvctxBHruly6eh5TU3Fx1bk7iTwDwHb92Yzo+LrbaRKAA4Du6WXhn+yiXNSdVq5ZUBeq6BQAAo9DlgbqZeVdqzgQqexKBAkBA9FiJ5KrmXKCSKLMM+uyl6xYAAIyiWaCbG252YFJqVgWahZ2uawUAsIZmDPQoOu16ELQm0GMECgBBoRHoD19E0cvnrmT81EUqEwIFgMDRTiLJONxMJEn7TF8gUAAIi/EC/c9/WK9ff/9O+ubFZ9fX6+zNs4/Wr//9w+Tl7bce1msWVR8jUAAIltGJ9L9bJ7z+G/Hmxw+TN2/8Pnn91sOn198WHz+9/nFDzalAi+kjBAoAgTFWoI/Wr/9THGzeTKV5e/3WHfFGBJwP1rE1bydibQpAESgAhE6rQDffd53+4uY6CS7j0PNjEWlmsafQZuLOxKKP1g0BaINAjxEoAISEXqD33hGDn8//6r22KfgfP0yUKWLPD4Qukx57/PMDSaAvbjYFoIpAj7PJJAQKAAGh3c7u83T26PnF6FSfOfhEoLezUPOREGkiUPFJcwCaCLQMPhEoAARHy4bKp/5m9dJvN/+cPSC+naTX/uJmOpUUd+XjmDMfA60FoD/JqAuU5ZsAEBL6R3pczVbE31312Fgk6byrAs1n4WsBaFWgaS5T/gIAIBS0D5W7UGwpctT9TI9HSRqTJFAxMiryQH/+UASgLz5br9+vjoPGXXhlHfwWgQJAWLRtJpIJtPuxxo+uvy6iTDUCzX+3/vjFzTfuPPuwOpMURbUuOwIFgJBo284uE2jn3nYPsjT6JoGKADRJp39Q7ckLgVYKwp8AEBIWBPq7zJ+VWfht9vLjNKvpkfhDqbkuUACAkNB14cXEUWbOk9Zp+Be312/+Pnv9IJPkg0KWyRR8lk3/tnoiAgWAwNHuB3omF2gs07ZJpNvrcnBTWYmUkEzBE4ECwCLRCPTxKjp/PxHok0tR2+70DyR/ioWdbxZr4dMPxCvtGOjItgMAzIoukf4oiqKzq71zr8Q/L+hPz7ZfEoge+jNpNyZBmgMqJpcaZ+HHNx8AYD60a+G/XeW7gbb4MzakItDts8/WUspnvghJlwcKABAyeov98OXZ2J4vO3tAPAIFgMCZz2IIFAACR2Oxzo1ALdSMQAEgbBostvlcDH++fNXFozjlmhEoAIRN3WKP89mjXvuAjqgZgQJA2NQsljyQ8+X/VaQv9dgHdEzNCBQAwqZmsaMoTZy/Fwei3fuAjqkZgQJA2FQttrmRezM2aec+oKNqRqAAEDatdNR0AAAL2UlEQVQNAs1Wbj5eue3DI1AACJyqxZ5fjLLN68pXjmpGoAAQNggUAMAQBAoAYAgCBQAwBIECABiCQAEADEGgAACGNAi0jhORIlAACBwECgBgSG0l0qdX6vzUxYokBAoAgcOO9AAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAyxbLEXn11fr9+/k7x+9tH69b9/mLy8/dbDes0IFADCxq7FfvxwLXjj98nrtx4+vf62+Pjp9Y8bakagABA2di12e/3Wne2zm2sRcD5Yx9a8/fpvts0BKAIFgNCxarGn17PYU2gzcWdi0UfrhgAUgQJA6Fi12IP129nPDySBvrjZFIAiUAAIHasWu52Fmo+ESBOBik+aA1AECgChY9NiL24mI56iKx/HnPkYaC0A/UkGAgWAsHEn0HwWvhaAIlAAWAaOBComk0Qe6M8figD0xWfr9fvVcVC68AAQOO4i0Iw4AH1x8407zz6sziQhUAAIHNcCFQFokk7/oNqTR6AAEDjuZuG32cuP06ymR+IPpWYECgBhYzkP9APlZxqA5tn0b6sHI1AACBx3K5ESkil4IlAAWCRWLfbi5vrNYi18+oF4xRgoACwSuxZ7Ju3GJEhzQMXkErPwALA4LFvs2WdrKeUzX4REHigALBF2pAcAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhiBQAABDECgAgCEIFADAEAQKAGAIAgUAMASBAgAYgkABAAxBoAAAhswpUABwwGz39A4y48W2/z/OT35iv0zKnqnwUMv2oOHz3dO7x6Iu9k9+QtlTlh1sw7koYAkEStl+Fh5q2eE2HAxAoJTtZ+Ghlh1uw8EABErZfhYeatnhNhwMQKCU7WfhoZYdbsPBAARK2X4WHmrZ4TYcDECglO1n4aGWHW7DwYBFCRQAYEoQKACAIQgUAMAQBAoAYAgCBQAwBIECABiCQAEADEGgAACGhCvQHz98O3v17B/W69d//jB9858flW9+/HCd8Mbv7ZQteHr9rYcOylaKs93uF7+7vl7/l3+qVzS68Bc31zmiQNsNF2/W798Z1/CWskf+j/KfooisedsXn10v2qq8MStcX7b8lcz/NsEO4Qr09jr7n+jP6f9Dbyb/Dz2Q3zy9bvi/V3PZgtgYqUAtl60UZ7nsZ9ld9rNR7W4svCJQNxfl9d+Manhz2fqr35vfraXmZSpLS1DeGBWuL1v5SuZ/m2CHUAX64vY6+58o/n/orTtxkJWI7en11+NA69lH6e8e5f+fWSk7IRZ0+tpy2UpxdsuOJfdm/OZ/pHejYdmtFyW57iMK1zc8fvPsZsMVGl12y9Xvy6N18v/azdRet+W23h7b8Jay5a9k/LcJtghUoKKjnv2/czv7/+r2+mPxxwfi9dPr2f94H1gsOy14XXxktWylOLtlP8rikwfJL83Kbr0oieo+sN/w7G/xxw8TOdstu+Xq9yT+zsn3j8PDaltHN7ylbOUrmf5tgjXCFGgcB/7sz+n/RPn/a8o/xj9++EbSv0z/j7NXdvzBP6ZjoJbLVoqzW3bxZkTZXRf8wdrFRXkkf+jootSvfl/S/8e2mcQeZBfjQe2NSeEtZctfyfRvE+wRqEDf/Jfy9sr+H8pnd8rXP3741v8b/2v9d3c0pQwv+/b67eyl5bKV4uyWXdyMW/OyOy54GihZvyhK5GW37JarP5hEcrdlIStvRhVeL1v+SiMbDhYIU6CCFoH++Xryv1w+wi5HYKPKfhQHWtlLy2UrxdktW/z35/+6Xsf33aiyWy54HiJZvijFGOjb1stuufpDSfzuqPB62cpXGtdwsEHwAi1mJPNRrfjF+vV/SY9Y/+zh9v/7bD24n9NcdvK/c+FSq2UrxdktO27wZ+ld9sGodusveD44Z/2Cv0jnon/m4ILrr/5QHiiDAHWBjim8Xrb6lUY1HGwQvkDjf4Xj/4nEvZZ2VV/83//79fXr/8e2DIyGj7Q3l52Uk/1fbLlspTi7ZT9KFRS/EXeZednaC54E5skL2xf86UdpqtEd+2Xrr/7g0sVllST3xu+VN+OueK1s9SuNaThYIXyB5qmf/yiNgf7ndblT80hOuDEv+0GW/6IUZqnsxuLslP1ond1dRfKgWdnahquzVPYaXkpOjq9sXfDOq9+z8Ouviy/fEoGOaHhD2ZWvZN5wsMMCBJrkdfzdHf3/reW/3GPKzlIdKwK1U3ZzcVbbXWn58LK1Da8XZemi3G4yv7UL3nX1e/Egk3sPgQ4uvLHs6lcybThYYgkCbXhf+V/XOGiR3mchi0CRnI2ym4uzU3Zxc1XkbB6BVt7Xk7ntNFznIasXXH/1+1AGx/pZeNPCNWXXCzZpONhiOQIV8UrRm0wDgab/j43LVgRqueyWHEULZRdTPKOuSWPh8k9nDXfwl1mpyKzsF7fLNb4PskKzPNDyjWHh2rLlssb8bYIdwhfog3zxkbjdysnVt4s39TE6s7JTsn/sLZetFOem7Dyn0LRs3UWRkrmtN1zqwlsuW3/1+3NbGifSr0QyK1xbtvKVRvxtgh3CF2i6bPg/ryd3hDLvkL559pH5fIlSdkqZB2qzbKU4+2W/f2f8NdFdFClR33LDH63d/WXqr35vHsiHpxsOZOvVlTdGhevLVr7SiL9NsEP4As03rsn3+EhIpi/zXvcbgxdqNJedkA83WS5bKc5y2Y+u27gmuosij7+5uSipqS2Xrb/6Pcl3klunC9OfyTsmKW8MCm8rW/lK5n+bYIcFCHQr1tlku11m2zzmWycmb342/F9nTdmCwheWy1aKs1222E7y70ZeE13hSsKD5Yb/T7Ef6NiGa8rWX/2+xSqSiy9y/Or9/C9QeTO48Nayla9k/LcJdghXoAAAM4NAAQAMQaAAAIYgUAAAQxAoAIAhCBQAwBAECgBgCAIFADAEgQIAGIJAAQAMQaAAAIYgUAAAQxAoAIAhCBQAwBAECgBgCAIFADAEgQIAGIJAAQAMQaAAAIYgUAAAQxAoAIAhCBQAwBAECgBgCAIFADAEgQIAGIJAAQAMQaAAAIYgUAAAQxAoAIAhCBQAwBAECgBgCAIFADAEgQIAGIJAAQAMQaAAAIYgUAAAQxAoAIAhCBQAwBAECgBgCAIFADAEgQIAGIJAAQAMQaAAAIYgUAAAQxAoAIAhCBQAwBAECgBgCAIFADAEgQIAGIJAAQAMQaAAAIYgUAAAQxAoAIAhCBQAwJD/H9lVI8CzvbBBAAAAAElFTkSuQmCC" width="672" style="display: block; margin: auto;" /></p>
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
25.35
</td>
<td style="text-align:right;">
99.91
</td>
<td style="text-align:right;">
181.20
</td>
<td style="text-align:right;">
287.58
</td>
<td style="text-align:right;">
437.49
</td>
<td style="text-align:right;">
654.42
</td>
<td style="text-align:right;">
996.57
</td>
<td style="text-align:right;">
1672.01
</td>
<td style="text-align:right;">
3340.98
</td>
<td style="text-align:right;">
16572.63
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
1.17
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
1417.91
</td>
<td style="text-align:right;">
387.87
</td>
<td style="text-align:right;">
266.52
</td>
<td style="text-align:right;">
217.58
</td>
<td style="text-align:right;">
186.88
</td>
<td style="text-align:right;">
162.59
</td>
<td style="text-align:right;">
150.40
</td>
<td style="text-align:right;">
141.68
</td>
<td style="text-align:right;">
134.12
</td>
<td style="text-align:right;">
129.67
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
% NYSE Stocks
</td>
<td style="text-align:right;">
43.31
</td>
<td style="text-align:right;">
56.33
</td>
<td style="text-align:right;">
64.19
</td>
<td style="text-align:right;">
69.66
</td>
<td style="text-align:right;">
74.94
</td>
<td style="text-align:right;">
81.16
</td>
<td style="text-align:right;">
85.12
</td>
<td style="text-align:right;">
88.54
</td>
<td style="text-align:right;">
92.15
</td>
<td style="text-align:right;">
94.56
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
26.20
</td>
<td style="text-align:right;">
101.63
</td>
<td style="text-align:right;">
181.51
</td>
<td style="text-align:right;">
287.22
</td>
<td style="text-align:right;">
433.62
</td>
<td style="text-align:right;">
645.69
</td>
<td style="text-align:right;">
980.72
</td>
<td style="text-align:right;">
1645.79
</td>
<td style="text-align:right;">
3291.40
</td>
<td style="text-align:right;">
16201.58
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
62.51
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Number of Stocks
</td>
<td style="text-align:right;">
1360.22
</td>
<td style="text-align:right;">
363.88
</td>
<td style="text-align:right;">
255.50
</td>
<td style="text-align:right;">
209.14
</td>
<td style="text-align:right;">
180.02
</td>
<td style="text-align:right;">
158.73
</td>
<td style="text-align:right;">
146.55
</td>
<td style="text-align:right;">
139.21
</td>
<td style="text-align:right;">
131.69
</td>
<td style="text-align:right;">
127.42
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
% NYSE Stocks
</td>
<td style="text-align:right;">
44.23
</td>
<td style="text-align:right;">
57.62
</td>
<td style="text-align:right;">
65.21
</td>
<td style="text-align:right;">
70.72
</td>
<td style="text-align:right;">
76.06
</td>
<td style="text-align:right;">
81.82
</td>
<td style="text-align:right;">
85.80
</td>
<td style="text-align:right;">
88.73
</td>
<td style="text-align:right;">
92.39
</td>
<td style="text-align:right;">
94.79
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
<p>For illustrative purposes, I also plot the cumulative value-weighted excess returns over time to get a better feeling for the data.</p>
<pre class="r"><code>portfolios_mktcap %&gt;%
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
1.52
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
0.51
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
3.49
</td>
<td style="text-align:right;">
2.86
</td>
<td style="text-align:right;">
2.78
</td>
<td style="text-align:right;">
3.04
</td>
<td style="text-align:right;">
3.01
</td>
<td style="text-align:right;">
3.02
</td>
<td style="text-align:right;">
2.89
</td>
<td style="text-align:right;">
3.15
</td>
<td style="text-align:right;">
2.91
</td>
<td style="text-align:right;">
2.63
</td>
<td style="text-align:right;">
-3.23
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
-0.02
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
-0.08
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
-0.08
</td>
<td style="text-align:right;">
-0.14
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
2.20
</td>
<td style="text-align:right;">
-0.09
</td>
<td style="text-align:right;">
-0.36
</td>
<td style="text-align:right;">
-0.21
</td>
<td style="text-align:right;">
-0.35
</td>
<td style="text-align:right;">
-0.36
</td>
<td style="text-align:right;">
-0.73
</td>
<td style="text-align:right;">
-0.27
</td>
<td style="text-align:right;">
-0.69
</td>
<td style="text-align:right;">
-1.61
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
1.37
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
0.80
</td>
<td style="text-align:right;">
0.79
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
<td style="text-align:right;">
0.52
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
3.44
</td>
<td style="text-align:right;">
2.82
</td>
<td style="text-align:right;">
3.01
</td>
<td style="text-align:right;">
2.97
</td>
<td style="text-align:right;">
3.02
</td>
<td style="text-align:right;">
3.08
</td>
<td style="text-align:right;">
2.99
</td>
<td style="text-align:right;">
3.03
</td>
<td style="text-align:right;">
2.96
</td>
<td style="text-align:right;">
2.63
</td>
<td style="text-align:right;">
-3.15
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
CAPM Alpha
</td>
<td style="text-align:right;">
0.43
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
-0.08
</td>
<td style="text-align:right;">
-0.14
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
1.88
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
-0.20
</td>
<td style="text-align:right;">
-0.34
</td>
<td style="text-align:right;">
-0.25
</td>
<td style="text-align:right;">
-0.44
</td>
<td style="text-align:right;">
-0.44
</td>
<td style="text-align:right;">
-0.72
</td>
<td style="text-align:right;">
-1.44
</td>
<td style="text-align:right;">
-2.66
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
1.25
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.51
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
3.00
</td>
<td style="text-align:right;">
2.86
</td>
<td style="text-align:right;">
2.81
</td>
<td style="text-align:right;">
3.05
</td>
<td style="text-align:right;">
3.01
</td>
<td style="text-align:right;">
3.01
</td>
<td style="text-align:right;">
2.91
</td>
<td style="text-align:right;">
3.19
</td>
<td style="text-align:right;">
2.91
</td>
<td style="text-align:right;">
2.69
</td>
<td style="text-align:right;">
-2.49
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
-0.02
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
-0.07
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
-0.08
</td>
<td style="text-align:right;">
-0.11
</td>
<td style="text-align:right;">
-0.38
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
t-Stat
</td>
<td style="text-align:right;">
1.17
</td>
<td style="text-align:right;">
-0.13
</td>
<td style="text-align:right;">
-0.28
</td>
<td style="text-align:right;">
-0.20
</td>
<td style="text-align:right;">
-0.35
</td>
<td style="text-align:right;">
-0.37
</td>
<td style="text-align:right;">
-0.68
</td>
<td style="text-align:right;">
-0.23
</td>
<td style="text-align:right;">
-0.67
</td>
<td style="text-align:right;">
-1.00
</td>
<td style="text-align:right;">
-1.74
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
1.00
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0.79
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
0.51
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
2.79
</td>
<td style="text-align:right;">
2.69
</td>
<td style="text-align:right;">
2.99
</td>
<td style="text-align:right;">
3.01
</td>
<td style="text-align:right;">
3.04
</td>
<td style="text-align:right;">
3.11
</td>
<td style="text-align:right;">
3.02
</td>
<td style="text-align:right;">
3.11
</td>
<td style="text-align:right;">
2.95
</td>
<td style="text-align:right;">
2.70
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
0.05
</td>
<td style="text-align:right;">
-0.06
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
-0.07
</td>
<td style="text-align:right;">
-0.11
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
0.27
</td>
<td style="text-align:right;">
-0.39
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
-0.16
</td>
<td style="text-align:right;">
-0.28
</td>
<td style="text-align:right;">
-0.16
</td>
<td style="text-align:right;">
-0.33
</td>
<td style="text-align:right;">
-0.24
</td>
<td style="text-align:right;">
-0.65
</td>
<td style="text-align:right;">
-0.99
</td>
<td style="text-align:right;">
-0.90
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
1.63
</td>
<td style="text-align:right;">
1.66
</td>
<td style="text-align:right;">
1.54
</td>
<td style="text-align:right;">
1.60
</td>
</tr>
<tr>
<td style="text-align:left;">
t-stat
</td>
<td style="text-align:right;">
4.18
</td>
<td style="text-align:right;">
4.85
</td>
<td style="text-align:right;">
4.10
</td>
<td style="text-align:right;">
4.77
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
-3.69
</td>
<td style="text-align:right;">
-3.16
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
-3.39
</td>
<td style="text-align:right;">
-2.84
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
-0.68
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
-1.06
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
2944.15
</td>
<td style="text-align:right;">
2944.15
</td>
<td style="text-align:right;">
2944.15
</td>
<td style="text-align:right;">
2944.15
</td>
</tr>
</tbody>
</table>
<p>The regression results show a statisticall significant negative relation between firm size and abnormal stock returns. The results also show that controling for market beta neither yields a statistically significant relation to stock returns, nor does it impact the coefficient estimate on firm size. This result contradicts the fundamental prediction of the CAPM that market beta is the only determinant of cross-sectional variation in expected returns. Also note that the explanatory power of size and beta is still very low (see, e.g., <a href="https://onlinelibrary.wiley.com/doi/abs/10.1111/0022-1082.00144">Elton, 1999</a>).</p>

</dl>
