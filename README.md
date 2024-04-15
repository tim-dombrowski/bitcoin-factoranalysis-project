# Bitcoin Factor Analysis Project

### Overview

This project aims to apply some traditional asset pricing models to the return series of bitcoin (BTC) and some related assets. In addition to the traditional Capital Asset Pricing Model (CAPM), we'll apply the Fama/French three-factor model and the Fama/French five-factor model. Beyond these,  we'll also consider some additional factors that may help explain the returns of bitcoin and other cryptocurrencies, such as the mining hashrate, difficulty, and BTC trading volume. For a more in-depth analysis on this topic across a wider range of the crypto market, see [Liu, Tsyvinski, and Wu (2022), "Common Risk Factors in Cryptocurrency," *Journal of Finance*, 77(2): 1133-1177.](https://doi.org/10.1111/jofi.13119)

### Other Bitcoin-Related Projects

For some other R Notebook project demos related to bitcoin, you can check out the following repositories:

* [bitcoin-timeseries-project](https://github.com/tim-dombrowski/bitcoin-timeseries-project) - This project focuses exploring the time series properties of bitcoin price data, including autocorrelation, stationarity, and seasonality.

* [bitcoin-miningstock-project]() - This project explores the relationship between the stock prices of publicly traded bitcoin mining companies and the price of bitcoin, as well as other mining-related metrics.

### Repository Structure

The data work for this project demo is contained in the R Notebook directory of this repository. On GitHub, the webpage within that folder should display the README.md file, which contains the compiled output of the R Notebook. If you wish to explore the source code locally, then you can open the [btcfactors.Rmd](https://github.com/tim-dombrowski/bitcoin-factoranalysis-project/blob/main/R%20Notebook/btcfactors.Rmd) file in RStudio and execute the code chunks to replicate the data work. Note the `output: html_notebook` line in the header of that file, which indicates that the R Markdown document is an R Notebook. 

After running the code chunks in RStudio and making any desired changes, you can then create a copy that will generate a copy that will appear on GitHub. To do this, save a copy of the R Notebook and name it README.Rmd. Then, change the header line for the output type to `output: github_document`. This will switch the file from being an R Notebook to an R Markdown file that will compile into a generic [Markdown](https://www.markdownguide.org/) file (.md). This format (along with the README name) will automatically be recognized by GitHub and displayed in-browser. This will also replace the Preview button with an option to Knit the Markdown file. This knitting process will re-run all the code chunks and generate a new README.md file inside of the R Notebook folder, which will display on GitHub.
