---
title: 'Volatility in Cryptocurrency Markets'
subtitle: ''
summary: 'An application of the spotvolatility estimator from [Hautsch et al (2019)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3302159) to other cryptocurrencies'
authors:
- admin
tags:
- Blockchain
categories:
- Blockchain
date: "2019-12-10T00:00:00Z"
lastmod: "2019-12-10T00:00:00Z"
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

In [Hautsch, et al. (2019)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3302159), we focus on the market for Bitcoin (BTC) against US Dollar (USD) and estimate the corresponding spotvolatilities for 16 different exchanges. In this note, I replicate the estimation procedure for the spotvolatilities of other cryptocurrencies, namely Ether (ETH), Litecoin (LTC), and Ripple (XRP) against USD. 

Before I proceed to the results, I briefly sketch the estimation procedure. To estimate the spot volatility, I follow the approach of [Kristensen (2010)](https://www.jstor.org/stable/40388620). For each market $i$, asset $j$, and minute $t$, I estimate $\sigma\_{i,j,t}^2$ by 
$$\widehat{{\sigma}\_{i,j,t}}^2(h\_T) = \sum\limits\_{l=1}^\infty K\left(l - t, h\_T\right)\left(b\_{i,j,l} - b\_{i,j,l-1}\right)^2,$$
where $K\left(l - t, h\_T\right)$ is a one-sided Gaussian kernel smoother with bandwidth $h_T$ and $b\_{i,j,l}$ corresponds to the quoted bid price on market $i$ for asset $j$ at time $l$. 
The choice of the bandwidth $h\_T$ involves a trade-off between the variance and the bias of the estimator. Using too many observations introduces a bias if the volatility is time-varying, whereas shrinking the estimation window through a lower bandwidth increases the variance of the estimator. [Kristensen (2010)](https://www.jstor.org/stable/40388620) hence proposes to choose $h\_T$ such that information on day $T-1$ is used for the estimation on day $T$, i.e., the bandwith on any day is the result of minimizing the integrated squared error of estimates on the previous day.

I employ the data that I collected together with my colleague [Stefan Voigt](http://www.voigtstefan.me/). Since January 2018, we gather the first 25 levels of all open buy and sell orders of a couple of exchanges on a minute level using our [R package](https://github.com/christophscheuch/CryptoX). The spotvolatility estimation procedure from above, however, only rests on the first level of bids. To ensure comparability across asset pairs, I focus on exchanges in our sample that feature trading of all four asset pairs (Binance, Bbitfinex, Bitstamp, Cex, Gate, Kraken, Lykke, Poloniex, xBTCe). 

Now, let us take a look at the resulting volatility estimates for all asset pairs. The app below shows the average daily spotvolatility estimate across all exchanges (solid lines) and corresponding range of average daily spotvolatility estimates (shaded areas). Overall, all four asset pairs exhibit a strong correlation over the last two years. Feel free to play around with the app by comparing asset pairs seperately or focussing on subperiods.

<html>
<head><title>Shiny App Iframe</title></head>
<body>
<iframe id="example1" src="https://christophscheuch.shinyapps.io/Spotvolas/" style="border: none; width: 100%; height: 850px" frameborder="0"></iframe>
</body>
</html>
