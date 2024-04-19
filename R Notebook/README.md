Bitcoin Factor Analysis
================
Last updated: 2024-04-18

## Preliminary Work: Install/Load Packages

To try and ensure that this R Notebook will run successfully, we’ll use
the [renv
package](https://cran.r-project.org/web/packages/renv/index.html) to
create a project-specific library of packages. This will allow us to
install the packages that we need for this project without affecting any
other projects that we may be working on. Additionally, the project
library will track the specific versions of the dependency packages so
that any updates to those packages will not break this project.

The code chunk below will first install the renv package if it is not
already installed. Then we will load the package. Next, we’ll use the
`restore()` function to install any packages listed in the renv.lock
file. Once these packages are installed, we can load them into the R
session using the `library()` commands. Below the code chunk, we’ll list
out the packages that will be used in the project demo. And if you run
into any trouble using renv, then you can use the second code chunk
below and that should be an even more reliable approach to install the
required packages.

``` r
# Install renv package if not already installed
if(!"renv" %in% installed.packages()[,"Package"]) install.packages("renv")
# Load renv package
library(renv)
# Use restore() to install any packages listed in the renv.lock file
renv::restore(clean=TRUE, lockfile="../renv.lock")
# Load in the packages
library(quantmod)
library(tidyverse)
library(corrplot)
library(jsonlite)
library(tseries)
library(rmarkdown)
```

- The [quantmod package](https://cran.r-project.org/package=quantmod)
  contains tools for importing and analyzing financial data.
- The [tidyverse package](https://www.tidyverse.org/) contains a suite
  of packages for data manipulation and visualization.
- The [corrplot package](https://cran.r-project.org/package=corrplot)
  lets us create correlation plots.
- The [jsonlite package](https://cran.r-project.org/package=jsonlite)
  lets us more easily import JSON data.
- The [tseries package](https://cran.r-project.org/package=tseries)
  contains additional time series analysis functions that we will
  explore.
- The [rmarkdown package](https://cran.r-project.org/package=rmarkdown)
  is used to generate this R Notebook.

Since the rmarkdown functionality is built into RStudio, this last one
is automatically loaded when you open RStudio. So no need to use the
`library()` function for it. Another observation to make about the code
chunk above is that it is labeled as `setup`, which is a special name,
which the R Notebook will recognize and automatically run prior to
running any other code chunk. This is useful for loading in packages and
setting up other global options that will be used throughout the
notebook.

Then if you wish to try and update the versions of the various R
packages in the lock file, you can use the `renv::update()` function to
update the packages in the project library. However, it is possible that
these updates could break the code in this notebook. If so, you may need
to adapt the code to work with the updated packages.

My recommendation is to first run through the code using the versions of
the packages in the lock file. Then if you want to try and update the
packages, you can do so and then run through the code again to see if it
still works. If not, you can always revert back to the lock file
versions using the `renv::restore()` function.

If you update the packages and get everything working successfully, then
you can update the lock file using the `renv::snapshot()` function. This
will update the lock file with the versions of the packages that are
currently installed in the project library. Then you can commit the
updated lock file to the repository so that others can use the updated
versions of the packages.

### Alternative Package Installation Code

If you run into any trouble using renv in the code chunk above, then you
can use the code chunk below to install the required packages for this
analysis. This method will first check if you have already installed the
packages. If any are missing, it will then install them. Then it will
load the packages into the R session. A potential flaw in this approach
compared to using renv is that it will simply install the latest
versions of the packages, which could potentially break some of the code
in this notebook if any of the updates aren’t backwards compatible.

As long as you have downloaded the entire project repository, the renv
chunk above will likely be managing the packages. Thus, the `eval=FALSE`
option is used to prevent this chunk from running unless manually
executed. So if you only downloaded this one Rmd file, this code chunk
should take care of installing the packages for you.

``` r
# Create list of packages needed for this exercise, omit geckor since its not on CRAN
list.of.packages = c("quantmod","tidyverse","corrplot","jsonlite","tseries","rmarkdown")
# Check if any have not yet been installed
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# If any need to be installed, install them
if(length(new.packages)) install.packages(new.packages)
# Load in the packages
library(quantmod)
library(tidyverse)
library(corrplot)
library(jsonlite)
library(tseries)
```

## Data Imports

This section will build out a cleaned dataset for use in the subsequent
analysis. To start, we’ll import the bitcoin price data from Yahoo
Finance using the quantmod package. Additionally, we’ll collect data for
a few other similar assets to compare with BTC. Then we’ll import the
factor data from Kenneth French’s website to see if known equity factors
explain bitcoin returns. Lastly, we’ll import some more bitcoin-specific
data to make a more tailored asset pricing model.

### Asset Data Import and Cleaning

The `getSymbols()` function from the quantmod package let’s use import
the daily price data for the various assets. The `src="yahoo"` argument
specifies that we want to pull the data from Yahoo Finance. The `from`
and `to` arguments specify the date range for the data. Rather than
assigning a variable name to the output of `getSymbols()`, the function
creates a new variable for each ticker in the list, named after the
ticker. The last few lines of the code chunk below show how to rename
the variable to something more manageable.

Currently, the additional assets beyond bitcoin (BTC) include:

- Grayscale Bitcoin Trust/ETF (GBTC)
- Microstrategy stock (MSTR)
- Ethereum (ETH)
- Wrapped Bitcoin (WBTC)

To simplify the analysis and make the asset data more comparable, we’ll
start the data on January 30, 2019, which is when WBTC (the most recent
asset currently) has its earliest observation.

``` r
startdate = "2019-01-30"
tickers = c("BTC-USD","GBTC","MSTR","ETH-USD","WBTC-USD")
getSymbols(tickers,
           src="yahoo",
           from=startdate,
           to=Sys.Date())
```

    ## Warning: BTC-USD contains missing values. Some functions will not work if
    ## objects contain missing values in the middle of the series. Consider using
    ## na.omit(), na.approx(), na.fill(), etc to remove or replace them.

    ## Warning: ETH-USD contains missing values. Some functions will not work if
    ## objects contain missing values in the middle of the series. Consider using
    ## na.omit(), na.approx(), na.fill(), etc to remove or replace them.

    ## Warning: WBTC-USD contains missing values. Some functions will not work if
    ## objects contain missing values in the middle of the series. Consider using
    ## na.omit(), na.approx(), na.fill(), etc to remove or replace them.

``` r
BTC = `BTC-USD`
ETH = `ETH-USD`
WBTC = `WBTC-USD`
```

Now that we have the daily price series, let’s extract the close prices
into a univariate xts object. Then we can rename the column to `"Close"`
to keep variable names simple. After that, let’s aggregate the daily
prices to a monthly series. The xts package (which is a dependency of
quantmod) allows us to easily convert the daily price data into a
monthly OHLC series using `to.monthly()`. The `name=NULL` option in
those functions prevents the variable names from including the name of
the original object.

``` r
# Create daily series and rename column
BTCdaily = BTC$`BTC-USD.Close` 
names(BTCdaily) = "Close"
# Convert to monthly series
BTCmonthOHLC = to.monthly(BTCdaily$Close, name=NULL)
```

    ## Warning in to.period(x, "months", indexAt = indexAt, name = name, ...): missing
    ## values removed from data

``` r
BTCmonth = BTCmonthOHLC$Close
# Same for GBTC
GBTCdaily = GBTC$GBTC.Close
names(GBTCdaily) = "Close"
GBTCmonthOHLC = to.monthly(GBTCdaily$Close, name=NULL)
GBTCmonth = GBTCmonthOHLC$Close
# Same for MSTR
MSTRdaily = MSTR$MSTR.Close
names(MSTRdaily) = "Close"
MSTRmonthOHLC = to.monthly(MSTRdaily$Close, name=NULL)
MSTRmonth = MSTRmonthOHLC$Close
# Same for ETH
ETHdaily = ETH$`ETH-USD.Close` 
names(ETHdaily) = "Close"
ETHmonthOHLC = to.monthly(ETHdaily$Close, name=NULL)
```

    ## Warning in to.period(x, "months", indexAt = indexAt, name = name, ...): missing
    ## values removed from data

``` r
ETHmonth = ETHmonthOHLC$Close
# Same for WBTC
WBTCdaily = WBTC$`WBTC-USD.Close` 
names(WBTCdaily) = "Close"
WBTCmonthOHLC = to.monthly(WBTCdaily$Close, name=NULL)
```

    ## Warning in to.period(x, "months", indexAt = indexAt, name = name, ...): missing
    ## values removed from data

``` r
WBTCmonth = WBTCmonthOHLC$Close
```

Lastly, we’ll convert the monthly price series into annualized return
series. This can be done directly by composing the `diff()` and `log()`
functions. The `log()` function will take the natural logarithm of the
prices, and then the `diff()` function will subtract the previous
period’s value from the current period’s value. Then to make the series
more comparable, we’ll annualize the returns by multiplying by 12.
Lastly, we’ll adjust the units to percentages.

``` r
# Calculate annualized returns
BTCmonth$AnnRet = diff(log(BTCmonth$Close))*12*100
GBTCmonth$AnnRet = diff(log(GBTCmonth$Close))*12*100
MSTRmonth$AnnRet = diff(log(MSTRmonth$Close))*12*100
ETHmonth$AnnRet = diff(log(ETHmonth$Close))*12*100
WBTCmonth$AnnRet = diff(log(WBTCmonth$Close))*12*100
```

### Fama/French Factors

To model the asset returns, we’ll import the Fama/French Factors. The
earlier [Fama and French
(1993)](https://doi.org/10.1016/0304-405X(93)90023-5) paper introduced a
three-factor model that includes SMB (Small Minus Big) as a ‘size’
factor and HML (High Minus Low) as a ‘value’/‘growth’ factor. See [the
3-Factors
webpage](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/f-f_factors.html)
for more detail. The data is also available there at the url in the data
chunk below. Since these are available as compressed folders, we must
first download the file (we’ll put it in a sub-folder, Factor Data) and
then decompress (unzip) the file before reading in the data. *Note the
extra entry in the .gitignore file for the repository. This prevents git
from syncing the datasets to the repo.*

``` r
ff3url = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"
# Create subdirectory for file downloads
subdirectory = "Factor Data"
dir.create(subdirectory, showWarnings=FALSE)
# Define the file paths
zip_filepath = file.path(subdirectory, "FF3-factors.zip")
csv_filepath = file.path(subdirectory, "FF3-factors.csv")
# Download the zip file
download.file(ff3url, destfile=zip_filepath)
# Extract the CSV file from the zip file
unzip(zip_filepath, exdir=subdirectory)
# The file name in the line below comes from the name of the file within the zipped file
file.rename("Factor Data/F-F_Research_Data_Factors.CSV", csv_filepath)
```

    ## [1] TRUE

``` r
FF3 = read_csv(csv_filepath,
               col_types = cols(...1 = col_date(format = "%Y%m")), 
               skip = 2)
```

    ## New names:
    ## • `` -> `...1`

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

The code chunk above reports a warning about some parsing errors. So
let’s investigate with `problems()`.

``` r
problems(FF3)
```

    ## # A tibble: 105 × 5
    ##      row   col expected       actual                           file             
    ##    <int> <int> <chr>          <chr>                            <chr>            
    ##  1  1174     1 5 columns      1 columns                        C:/Users/tdrgv/P…
    ##  2  1174     1 date like %Y%m Annual Factors: January-December C:/Users/tdrgv/P…
    ##  3  1175     2 a double       Mkt-RF                           C:/Users/tdrgv/P…
    ##  4  1175     3 a double       SMB                              C:/Users/tdrgv/P…
    ##  5  1175     4 a double       HML                              C:/Users/tdrgv/P…
    ##  6  1175     5 a double       RF                               C:/Users/tdrgv/P…
    ##  7  1176     1 date like %Y%m 1927                             C:/Users/tdrgv/P…
    ##  8  1177     1 date like %Y%m 1928                             C:/Users/tdrgv/P…
    ##  9  1178     1 date like %Y%m 1929                             C:/Users/tdrgv/P…
    ## 10  1179     1 date like %Y%m 1930                             C:/Users/tdrgv/P…
    ## # ℹ 95 more rows

Note how it refers us to the bottom of the data frame. These are the
annual observations that are included below the monthly data. Since
those annual dates do not import and are missing, we can use
`complete.cases()` to easily identify the appropriate columns to retain.
Then the date variable imports with an unusual name of `...1`, so we’ll
fix that, and then reformat to an xts object. Lastly, we use the
`paste()` function to generate a string of `"2019-01-30/"`. This type of
indexing is part of the xts package, and the `/` separates a start date
from an end date.

``` r
# Trim annual observations from bottom of date frame (dates import as missing)
FF3 = FF3[complete.cases(FF3),]
# Fix date variable name
FF3 = FF3 |> rename(Date=...1)
FF3 = FF3 |> rename(MktRF=`Mkt-RF`)
# Reformat to xts object
FF3xts = xts(FF3[,-1], order.by=FF3$Date)
# Remove data prior to first BTC observation
FF3xts = FF3xts[paste(startdate,"/",sep="")]
# Annualize factor data to match annualized return scaling (use RF as reference point)
FF3xts = FF3xts*12
```

More recently, [Fama and French
(2015)](https://doi.org/10.1016/j.jfineco.2014.10.010) includes two
additional factors: RMW (Robust Minus Weak) as a ‘profitability’ factor
and CMA (Conservative Minus Aggressive) factor. The [5-Factors
webpage](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/f-f_5_factors_2x3.html)
has more detail.

``` r
ff5url = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip"
# Define the file paths
zip_filepath = file.path(subdirectory, "FF5-factors.zip")
csv_filepath = file.path(subdirectory, "FF5-factors.csv")
# Download the zip file
download.file(ff5url, destfile=zip_filepath)
# Extract the CSV file from the zip file
unzip(zip_filepath, exdir=subdirectory)
file.rename("Factor Data/F-F_Research_Data_5_Factors_2x3.CSV", csv_filepath)
```

    ## [1] TRUE

``` r
FF5 = read_csv(csv_filepath,
               col_types = cols(...1 = col_date(format = "%Y%m")), 
               skip = 2)
```

    ## New names:
    ## • `` -> `...1`

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

``` r
# Trim annual observations from bottom of date frame (dates import as missing)
FF5 = FF5[complete.cases(FF5),]
# Fix date variable name
FF5 = FF5 |> rename(Date=...1)
FF5 = FF5 |> rename(MktRF=`Mkt-RF`)
# Reformat numbers to numeric type and annualize
FF5$MktRF = as.numeric(FF5$MktRF)*12
FF5$SMB = as.numeric(FF5$SMB)*12
FF5$HML = as.numeric(FF5$HML)*12
FF5$RMW = as.numeric(FF5$RMW)*12
FF5$CMA = as.numeric(FF5$CMA)*12
FF5$RF = as.numeric(FF5$RF)*12
# Reformat to xts object
FF5xts = xts(FF5[,-1], order.by=FF5$Date)
# Remove data prior to first BTC observation
FF5xts = FF5xts[paste(startdate,"/",sep="")]
```

## Summary Statistics

Before running the factor model regressions, let’s first calculate some
summary statistics and generate some basic plots for the assets. We’ll
start by consolidating the asset prices into a single xts object and the
returns into another.

``` r
assetprices = merge(BTCmonth$Close,
                    GBTCmonth$Close,
                    MSTRmonth$Close,
                    ETHmonth$Close,
                    WBTCmonth$Close)
colnames(assetprices) = c("BTC","GBTC","MSTR","ETH","WBTC")
assetreturns = merge(BTCmonth$AnnRet,
                     GBTCmonth$AnnRet,
                     MSTRmonth$AnnRet,
                     ETHmonth$AnnRet,
                     WBTCmonth$AnnRet)
colnames(assetreturns) = c("BTC","GBTC","MSTR","ETH","WBTC")
# Drop first row from returns table (since it's NA)
assetreturns = assetreturns[-1,]
```

### Univariate Plots

First, lets visualize each of the price and return series. For prices,
we’ll use line plots. For returns, we’ll use bar plots.

``` r
# Plot the asset prices
ggplot(data=assetprices, aes(x=index(assetprices), y=BTC)) + geom_line() + xlab("")
```

    ## Warning: The `trans` argument of `continuous_scale()` is deprecated as of ggplot2 3.5.0.
    ## ℹ Please use the `transform` argument instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](README_files/figure-gfm/tsplots-1.png)<!-- -->

``` r
ggplot(data=assetprices, aes(x=index(assetprices), y=GBTC)) + geom_line() + xlab("")
```

![](README_files/figure-gfm/tsplots-2.png)<!-- -->

``` r
ggplot(data=assetprices, aes(x=index(assetprices), y=MSTR)) + geom_line() + xlab("")
```

![](README_files/figure-gfm/tsplots-3.png)<!-- -->

``` r
ggplot(data=assetprices, aes(x=index(assetprices), y=ETH)) + geom_line() + xlab("")
```

![](README_files/figure-gfm/tsplots-4.png)<!-- -->

``` r
ggplot(data=assetprices, aes(x=index(assetprices), y=WBTC)) + geom_line() + xlab("")
```

![](README_files/figure-gfm/tsplots-5.png)<!-- -->

``` r
# Plot the asset returns
ggplot(data=assetreturns, aes(x=index(assetreturns), y=BTC)) + geom_col() + xlab("")
```

![](README_files/figure-gfm/tsplots-6.png)<!-- -->

``` r
ggplot(data=assetreturns, aes(x=index(assetreturns), y=GBTC)) + geom_col() + xlab("")
```

![](README_files/figure-gfm/tsplots-7.png)<!-- -->

``` r
ggplot(data=assetreturns, aes(x=index(assetreturns), y=MSTR)) + geom_col() + xlab("")
```

![](README_files/figure-gfm/tsplots-8.png)<!-- -->

``` r
ggplot(data=assetreturns, aes(x=index(assetreturns), y=ETH)) + geom_col() + xlab("")
```

![](README_files/figure-gfm/tsplots-9.png)<!-- -->

``` r
ggplot(data=assetreturns, aes(x=index(assetreturns), y=WBTC)) + geom_col() + xlab("")
```

![](README_files/figure-gfm/tsplots-10.png)<!-- -->

Another way to visualize the returns of each series is to use a density
plot. This will allow us to compare the distribution of returns for the
assets. The `facet_wrap()` function will allow us to compare the assets
side-by-side. *This component of the analysis was recommended by GitHub
Copilot.*

``` r
# Combine returns into a single data frame
assetreturnsdf = data.frame(index(assetreturns), assetreturns)
colnames(assetreturnsdf) = c("Date","BTC","GBTC","MSTR","ETH","WBTC")
assetreturnsdf = assetreturnsdf |> pivot_longer(cols=-Date, names_to="Asset", values_to="Return")
# Plot the density of returns
ggplot(data=assetreturnsdf, aes(x=Return, fill=Asset)) + 
  geom_density(alpha=0.5) + 
  facet_wrap(~Asset)
```

![](README_files/figure-gfm/densityplots-1.png)<!-- -->

### Univariate Statistics

Next, let’s compute some univariate statistics for the asset returns.
We’ll calculate the mean, standard deviation, and the ratio of those two
for a measure of risk-adjusted return.

``` r
# Asset average annual returns
assetsEr = colMeans(assetreturns, na.rm=TRUE)
assetsEr |> round(2)
```

    ##   BTC  GBTC  MSTR   ETH  WBTC 
    ## 54.76 49.73 42.60 63.39 54.72

``` r
# Asset standard deviations
assetsSd = apply(assetreturns, 2, sd, na.rm=TRUE)
assetsSd |> round(2)
```

    ##    BTC   GBTC   MSTR    ETH   WBTC 
    ## 237.62 274.06 307.17 292.08 238.62

``` r
# Asset risk-adjusted returns
(assetsEr/assetsSd) |> round(4)
```

    ##    BTC   GBTC   MSTR    ETH   WBTC 
    ## 0.2304 0.1815 0.1387 0.2170 0.2293

### Multivariate Statistics

At the multivariate level, we can calculate the correlation matrix for
the asset returns. This will allow us to see how the assets move
together. We can also create a correlation plot to visualize the
relationships.

``` r
cor(assetreturns) |> round(4)
```

    ##         BTC   GBTC   MSTR    ETH   WBTC
    ## BTC  1.0000 0.9603 0.6564 0.7829 0.9986
    ## GBTC 0.9603 1.0000 0.6579 0.7703 0.9585
    ## MSTR 0.6564 0.6579 1.0000 0.6236 0.6521
    ## ETH  0.7829 0.7703 0.6236 1.0000 0.7816
    ## WBTC 0.9986 0.9585 0.6521 0.7816 1.0000

``` r
# Create a correlation plot for the daily returns
corrplot(cor(assetreturns), method="color")
```

![](README_files/figure-gfm/assetcorrs-1.png)<!-- -->

## Factor Model Regressions

### CAPM and Fama/French 3-Factor Models

Since the FF3 factor model is an extension of the traditional CAPM and
includes the data for applying CAPM, let’s start with those two. First,
let’s collect all the necessary data into a single xts object. Then
we’ll run the regressions for each asset.

``` r
# Compile xts object with all asset returns and factors
assets_ff3 = merge(assetreturns,
                   FF3xts)
# Correct variable names
colnames(assets_ff3) = c("BTC","GBTC","MSTR","ETH","WBTC","MktRF","SMB","HML","RF")
# Calculate risk premiums for assets
assets_ff3$BTCxs = assets_ff3$BTC - assets_ff3$RF
assets_ff3$GBTCxs = assets_ff3$GBTC - assets_ff3$RF
assets_ff3$MSTRxs = assets_ff3$MSTR - assets_ff3$RF
assets_ff3$ETHxs = assets_ff3$ETH - assets_ff3$RF
assets_ff3$WBTCxs = assets_ff3$WBTC - assets_ff3$RF
# Trim any observations with missing data (FF factors can be 1-2 months behind asset data)
assets_ff3 = assets_ff3[complete.cases(assets_ff3),]
```

Before running the regressions, let’s calculate the correlation matrix
and plot for the asset risk premiums and the factors.

``` r
cor(assets_ff3[,-c(1:5)]) |> round(2)
```

    ##        MktRF   SMB   HML    RF BTCxs GBTCxs MSTRxs ETHxs WBTCxs
    ## MktRF   1.00  0.31  0.05 -0.02  0.48   0.49   0.57  0.55   0.47
    ## SMB     0.31  1.00  0.07 -0.15  0.22   0.19   0.48  0.29   0.21
    ## HML     0.05  0.07  1.00 -0.13 -0.07  -0.10  -0.16 -0.02  -0.07
    ## RF     -0.02 -0.15 -0.13  1.00  0.06   0.15   0.10 -0.07   0.06
    ## BTCxs   0.48  0.22 -0.07  0.06  1.00   0.96   0.65  0.78   1.00
    ## GBTCxs  0.49  0.19 -0.10  0.15  0.96   1.00   0.66  0.77   0.96
    ## MSTRxs  0.57  0.48 -0.16  0.10  0.65   0.66   1.00  0.63   0.65
    ## ETHxs   0.55  0.29 -0.02 -0.07  0.78   0.77   0.63  1.00   0.78
    ## WBTCxs  0.47  0.21 -0.07  0.06  1.00   0.96   0.65  0.78   1.00

``` r
corrplot(cor(assets_ff3[,-c(1:5)]), method="color")
```

![](README_files/figure-gfm/ff3stats-1.png)<!-- -->

#### CAPM Regressions

Now let’s start with the CAPM regressions for the full series of each
asset. The `lm()` function fits a linear regression model of the asset
risk premium on the market risk premium.

``` r
# Bitcoin CAPM regression
CAPMreg_BTC = lm(BTCxs~MktRF, data=assets_ff3)
summary(CAPMreg_BTC)
```

    ## 
    ## Call:
    ## lm(formula = BTCxs ~ MktRF, data = assets_ff3)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -560.8 -130.5  -29.3  128.4  676.7 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  31.2609    27.7030   1.128 0.263706    
    ## MktRF         1.7368     0.4177   4.158 0.000105 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 211.9 on 59 degrees of freedom
    ## Multiple R-squared:  0.2266, Adjusted R-squared:  0.2135 
    ## F-statistic: 17.29 on 1 and 59 DF,  p-value: 0.0001054

``` r
# GBTC CAPM regression
CAPMreg_GBTC = lm(GBTCxs~MktRF, data=assets_ff3)
summary(CAPMreg_GBTC)
```

    ## 
    ## Call:
    ## lm(formula = GBTCxs ~ MktRF, data = assets_ff3)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -555.22 -147.29    5.79  159.72  766.60 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   21.841     31.701   0.689    0.494    
    ## MktRF          2.080      0.478   4.352 5.43e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 242.4 on 59 degrees of freedom
    ## Multiple R-squared:  0.243,  Adjusted R-squared:  0.2302 
    ## F-statistic: 18.94 on 1 and 59 DF,  p-value: 5.427e-05

``` r
# MSTR CAPM regression
CAPMreg_MSTR = lm(MSTRxs~MktRF, data=assets_ff3)
summary(CAPMreg_MSTR)
```

    ## 
    ## Call:
    ## lm(formula = MSTRxs ~ MktRF, data = assets_ff3)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -511.92 -142.33   -9.15  122.98  688.64 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   4.0883    32.0481   0.128    0.899    
    ## MktRF         2.6056     0.4832   5.392 1.28e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 245.1 on 59 degrees of freedom
    ## Multiple R-squared:  0.3301, Adjusted R-squared:  0.3188 
    ## F-statistic: 29.08 on 1 and 59 DF,  p-value: 1.284e-06

``` r
# Ethereum CAPM regression
CAPMreg_ETH = lm(ETHxs~MktRF, data=assets_ff3)
summary(CAPMreg_ETH)
```

    ## 
    ## Call:
    ## lm(formula = ETHxs ~ MktRF, data = assets_ff3)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -501.50 -143.32  -17.76  134.36  774.33 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  32.5004    32.4091   1.003     0.32    
    ## MktRF         2.4736     0.4887   5.062 4.35e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 247.8 on 59 degrees of freedom
    ## Multiple R-squared:  0.3028, Adjusted R-squared:  0.291 
    ## F-statistic: 25.62 on 1 and 59 DF,  p-value: 4.347e-06

``` r
# Wrapped Bitcoin CAPM regression
CAPMreg_WBTC = lm(WBTCxs~MktRF, data=assets_ff3)
summary(CAPMreg_WBTC)
```

    ## 
    ## Call:
    ## lm(formula = WBTCxs ~ MktRF, data = assets_ff3)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -561.59 -126.83  -28.51  138.14  689.52 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  31.2274    27.8600   1.121 0.266886    
    ## MktRF         1.7381     0.4201   4.138 0.000113 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 213.1 on 59 degrees of freedom
    ## Multiple R-squared:  0.2249, Adjusted R-squared:  0.2118 
    ## F-statistic: 17.12 on 1 and 59 DF,  p-value: 0.0001129

#### Fama/French 3-Factor Models

Now let’s apply the 3-factor model.

``` r
# Bitcoin FF3 regression
FF3reg_BTC = lm(BTCxs~MktRF+SMB+HML, data=assets_ff3)
summary(FF3reg_BTC)
```

    ## 
    ## Call:
    ## lm(formula = BTCxs ~ MktRF + SMB + HML, data = assets_ff3)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -526.0 -138.0  -17.6  119.2  666.1 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  32.9405    28.1627   1.170 0.247011    
    ## MktRF         1.6593     0.4432   3.744 0.000423 ***
    ## SMB           0.5691     0.8243   0.690 0.492690    
    ## HML          -0.4060     0.4903  -0.828 0.411116    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 213.5 on 57 degrees of freedom
    ## Multiple R-squared:  0.2413, Adjusted R-squared:  0.2013 
    ## F-statistic: 6.042 on 3 and 57 DF,  p-value: 0.001205

``` r
# GBTC FF3 regression
FF3reg_GBTC = lm(GBTCxs~MktRF+SMB+HML, data=assets_ff3)
summary(FF3reg_GBTC)
```

    ## 
    ## Call:
    ## lm(formula = GBTCxs ~ MktRF + SMB + HML, data = assets_ff3)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -510.73 -165.46   -4.15  154.82  751.11 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  22.2441    32.1382   0.692 0.491660    
    ## MktRF         2.0436     0.5057   4.041 0.000161 ***
    ## SMB           0.3876     0.9406   0.412 0.681806    
    ## HML          -0.6387     0.5595  -1.142 0.258423    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 243.6 on 57 degrees of freedom
    ## Multiple R-squared:  0.2615, Adjusted R-squared:  0.2226 
    ## F-statistic: 6.727 on 3 and 57 DF,  p-value: 0.0005781

``` r
# MSTR FF3 regression
FF3reg_MSTR = lm(MSTRxs~MktRF+SMB+HML, data=assets_ff3)
summary(FF3reg_MSTR)
```

    ## 
    ## Call:
    ## lm(formula = MSTRxs ~ MktRF + SMB + HML, data = assets_ff3)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -520.77 -126.04   -3.91  126.88  667.26 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  14.6957    29.1546   0.504  0.61616    
    ## MktRF         2.1655     0.4588   4.720 1.57e-05 ***
    ## SMB           2.9149     0.8533   3.416  0.00118 ** 
    ## HML          -1.0988     0.5075  -2.165  0.03459 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 221 on 57 degrees of freedom
    ## Multiple R-squared:  0.4737, Adjusted R-squared:  0.446 
    ## F-statistic:  17.1 on 3 and 57 DF,  p-value: 4.848e-08

``` r
# Ethereum FF3 Regression
FF3reg_ETH = lm(ETHxs~MktRF+SMB+HML, data=assets_ff3)
summary(FF3reg_ETH)
```

    ## 
    ## Call:
    ## lm(formula = ETHxs ~ MktRF + SMB + HML, data = assets_ff3)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -571.53 -133.64    7.76  106.75  765.07 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  36.7973    32.8309   1.121    0.267    
    ## MktRF         2.3007     0.5166   4.453    4e-05 ***
    ## SMB           1.1062     0.9609   1.151    0.254    
    ## HML          -0.2843     0.5715  -0.497    0.621    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 248.9 on 57 degrees of freedom
    ## Multiple R-squared:  0.3208, Adjusted R-squared:  0.2851 
    ## F-statistic: 8.974 on 3 and 57 DF,  p-value: 5.826e-05

``` r
# Wrapped Bitcoin FF3 Regression
FF3reg_WBTC = lm(WBTCxs~MktRF+SMB+HML, data=assets_ff3)
summary(FF3reg_WBTC)
```

    ## 
    ## Call:
    ## lm(formula = WBTCxs ~ MktRF + SMB + HML, data = assets_ff3)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -526.60 -137.44  -22.74  135.25  678.95 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  32.6876    28.3424   1.153 0.253596    
    ## MktRF         1.6690     0.4460   3.742 0.000426 ***
    ## SMB           0.5200     0.8295   0.627 0.533228    
    ## HML          -0.4073     0.4934  -0.825 0.412536    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 214.9 on 57 degrees of freedom
    ## Multiple R-squared:  0.2385, Adjusted R-squared:  0.1984 
    ## F-statistic: 5.951 on 3 and 57 DF,  p-value: 0.001329

### Fama/French 5-Factor Models

Next, we’ll replicate that process with the FF5 factors. First, merge
the data together and trim missing observations.

``` r
# Compile xts object with all asset returns and factors
assets_ff5 = merge(assetreturns,
                   FF5xts)
# Correct variable names
colnames(assets_ff5) = c("BTC","GBTC","MSTR","ETH","WBTC","MktRF","SMB","HML","RMW","CMA","RF")
# Calculate risk premiums for assets
assets_ff5$BTCxs = assets_ff5$BTC - assets_ff5$RF
assets_ff5$GBTCxs = assets_ff5$GBTC - assets_ff5$RF
assets_ff5$MSTRxs = assets_ff5$MSTR - assets_ff5$RF
assets_ff5$ETHxs = assets_ff5$ETH - assets_ff5$RF
assets_ff5$WBTCxs = assets_ff5$WBTC - assets_ff5$RF
# Trim any observations with missing data (FF factors can be 1-2 months behind asset data)
assets_ff5 = assets_ff5[complete.cases(assets_ff5),]
```

Compute and visualize correlations between assets and factors:

``` r
cor(assets_ff5[,-c(1:5)]) |> round(2)
```

    ##        MktRF   SMB   HML   RMW   CMA    RF BTCxs GBTCxs MSTRxs ETHxs WBTCxs
    ## MktRF   1.00  0.34  0.05  0.08 -0.20 -0.02  0.48   0.49   0.57  0.55   0.47
    ## SMB     0.34  1.00  0.36 -0.38  0.03 -0.17  0.20   0.16   0.41  0.26   0.20
    ## HML     0.05  0.36  1.00  0.19  0.66 -0.13 -0.07  -0.10  -0.16 -0.02  -0.07
    ## RMW     0.08 -0.38  0.19  1.00  0.18 -0.10 -0.14  -0.13  -0.35 -0.08  -0.15
    ## CMA    -0.20  0.03  0.66  0.18  1.00 -0.21 -0.23  -0.26  -0.28 -0.17  -0.22
    ## RF     -0.02 -0.17 -0.13 -0.10 -0.21  1.00  0.06   0.15   0.10 -0.07   0.06
    ## BTCxs   0.48  0.20 -0.07 -0.14 -0.23  0.06  1.00   0.96   0.65  0.78   1.00
    ## GBTCxs  0.49  0.16 -0.10 -0.13 -0.26  0.15  0.96   1.00   0.66  0.77   0.96
    ## MSTRxs  0.57  0.41 -0.16 -0.35 -0.28  0.10  0.65   0.66   1.00  0.63   0.65
    ## ETHxs   0.55  0.26 -0.02 -0.08 -0.17 -0.07  0.78   0.77   0.63  1.00   0.78
    ## WBTCxs  0.47  0.20 -0.07 -0.15 -0.22  0.06  1.00   0.96   0.65  0.78   1.00

``` r
corrplot(cor(assets_ff5[,-c(1:5)]), method="color")
```

![](README_files/figure-gfm/ff5stats-1.png)<!-- -->

Run the FF5 factor regressions:

``` r
# Bitcoin FF5 regression
FF5reg_BTC = lm(BTCxs~MktRF+SMB+HML+RMW+CMA, data=assets_ff5)
summary(FF5reg_BTC)
```

    ## 
    ## Call:
    ## lm(formula = BTCxs ~ MktRF + SMB + HML + RMW + CMA, data = assets_ff5)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -511.22 -118.27  -17.83  119.43  677.16 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  41.8481    28.7661   1.455 0.151416    
    ## MktRF         1.7243     0.4789   3.601 0.000682 ***
    ## SMB          -0.2021     1.0041  -0.201 0.841199    
    ## HML           0.1674     0.7772   0.215 0.830264    
    ## RMW          -1.3442     1.0899  -1.233 0.222687    
    ## CMA          -0.8743     1.1105  -0.787 0.434487    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 213.2 on 55 degrees of freedom
    ## Multiple R-squared:   0.27,  Adjusted R-squared:  0.2037 
    ## F-statistic: 4.069 on 5 and 55 DF,  p-value: 0.003255

``` r
# GBTC FF5 regression
FF5reg_GBTC = lm(GBTCxs~MktRF+SMB+HML+RMW+CMA, data=assets_ff5)
summary(FF5reg_GBTC)
```

    ## 
    ## Call:
    ## lm(formula = GBTCxs ~ MktRF + SMB + HML + RMW + CMA, data = assets_ff5)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -482.6 -159.1    3.4  168.1  769.0 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  32.8657    32.7390   1.004 0.319836    
    ## MktRF         2.1273     0.5450   3.903 0.000261 ***
    ## SMB          -0.6290     1.1428  -0.550 0.584252    
    ## HML           0.1861     0.8845   0.210 0.834095    
    ## RMW          -1.6069     1.2404  -1.295 0.200559    
    ## CMA          -1.1835     1.2639  -0.936 0.353150    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 242.6 on 55 degrees of freedom
    ## Multiple R-squared:  0.2933, Adjusted R-squared:  0.229 
    ## F-statistic: 4.565 on 5 and 55 DF,  p-value: 0.001491

``` r
# MSTR FF5 regression
FF5reg_MSTR = lm(MSTRxs~MktRF+SMB+HML+RMW+CMA, data=assets_ff5)
summary(FF5reg_MSTR)
```

    ## 
    ## Call:
    ## lm(formula = MSTRxs ~ MktRF + SMB + HML + RMW + CMA, data = assets_ff5)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -389.54 -168.75    2.18  116.46  580.47 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  26.7342    29.0601   0.920   0.3616    
    ## MktRF         2.4716     0.4838   5.109 4.19e-06 ***
    ## SMB           1.4971     1.0144   1.476   0.1457    
    ## HML          -1.1280     0.7851  -1.437   0.1565    
    ## RMW          -2.6695     1.1010  -2.425   0.0186 *  
    ## CMA           0.1120     1.1218   0.100   0.9208    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 215.4 on 55 degrees of freedom
    ## Multiple R-squared:  0.5178, Adjusted R-squared:  0.474 
    ## F-statistic: 11.81 on 5 and 55 DF,  p-value: 8.799e-08

``` r
# Ethereum FF5 Regression
FF5reg_ETH = lm(ETHxs~MktRF+SMB+HML+RMW+CMA, data=assets_ff5)
summary(FF5reg_ETH)
```

    ## 
    ## Call:
    ## lm(formula = ETHxs ~ MktRF + SMB + HML + RMW + CMA, data = assets_ff5)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -537.18 -150.21    7.31  128.43  764.13 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  40.6452    34.1676   1.190 0.239320    
    ## MktRF         2.3816     0.5688   4.187 0.000103 ***
    ## SMB           0.5211     1.1927   0.437 0.663892    
    ## HML          -0.1846     0.9231  -0.200 0.842252    
    ## RMW          -0.7839     1.2945  -0.606 0.547303    
    ## CMA          -0.2469     1.3190  -0.187 0.852180    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 253.2 on 55 degrees of freedom
    ## Multiple R-squared:  0.3216, Adjusted R-squared:   0.26 
    ## F-statistic: 5.215 on 5 and 55 DF,  p-value: 0.0005467

``` r
# Wrapped Bitcoin FF5 Regression
FF5reg_WBTC = lm(WBTCxs~MktRF+SMB+HML+RMW+CMA, data=assets_ff5)
summary(FF5reg_WBTC)
```

    ## 
    ## Call:
    ## lm(formula = WBTCxs ~ MktRF + SMB + HML + RMW + CMA, data = assets_ff5)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -509.87 -130.97  -20.42  123.23  688.76 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  41.9920    28.8989   1.453 0.151887    
    ## MktRF         1.7579     0.4811   3.654 0.000577 ***
    ## SMB          -0.3133     1.0088  -0.311 0.757280    
    ## HML           0.1800     0.7808   0.230 0.818559    
    ## RMW          -1.4775     1.0949  -1.349 0.182716    
    ## CMA          -0.8180     1.1156  -0.733 0.466519    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 214.2 on 55 degrees of freedom
    ## Multiple R-squared:  0.2699, Adjusted R-squared:  0.2036 
    ## F-statistic: 4.067 on 5 and 55 DF,  p-value: 0.003263

### Other Bitcoin-Related Factors

#### Trading Volume Growth as a Factor

The asset trading volume is included in the raw data retrieved from
Yahoo Finance. This next code chunk will extract the daily trading
volume for each asset and convert it to monthly averages (average daily
volume by month). Then we’ll calculate the annualized growth rate in
trading volume to resolve any non-stationarity.

``` r
# Extract trading volume column from BTC data
volumeBTCdaily = BTC$`BTC-USD.Volume`
# Rename column
names(volumeBTCdaily) = "Volume"
# Create data frame from xts object (for dplyr::mutate)
volumeBTCdailydf = data.frame(Date=index(volumeBTCdaily), volumeBTCdaily)
# Convert daily trading volume to daily averages by month
volumeBTCmonthdf =  mutate(volumeBTCdailydf, Date=floor_date(Date,"month")) |> 
  group_by(Date) |> summarise(avgDailyVolume=mean(Volume, na.rm=TRUE))
# Convert to xts object
volumeBTCmonth = xts(volumeBTCmonthdf[,-1], order.by=volumeBTCmonthdf$Date)
# Compute annualized growth rate in trading volume
volumeBTCmonth$VolGrowthBTC = diff(log(volumeBTCmonth$avgDailyVolume))*12*100
# Repeat process for GBTC
volumeGBTCdaily = GBTC$GBTC.Volume
names(volumeGBTCdaily) = "Volume"
volumeGBTCdailydf = data.frame(Date=index(volumeGBTCdaily), volumeGBTCdaily)
volumeGBTCmonthdf =  mutate(volumeGBTCdailydf, Date=floor_date(Date,"month")) |> 
  group_by(Date) |> summarise(avgDailyVolume=mean(Volume, na.rm=TRUE))
volumeGBTCmonth = xts(volumeGBTCmonthdf[,-1], order.by=volumeGBTCmonthdf$Date)
volumeGBTCmonth$VolGrowthGBTC = diff(log(volumeGBTCmonth$avgDailyVolume))*12*100
# Repeat process for MSTR
volumeMSTRdaily = MSTR$MSTR.Volume
names(volumeMSTRdaily) = "Volume"
volumeMSTRdailydf = data.frame(Date=index(volumeMSTRdaily), volumeMSTRdaily)
volumeMSTRmonthdf =  mutate(volumeMSTRdailydf, Date=floor_date(Date,"month")) |> 
  group_by(Date) |> summarise(avgDailyVolume=mean(Volume, na.rm=TRUE))
volumeMSTRmonth = xts(volumeMSTRmonthdf[,-1], order.by=volumeMSTRmonthdf$Date)
volumeMSTRmonth$VolGrowthMSTR = diff(log(volumeMSTRmonth$avgDailyVolume))*12*100
# Repeat process for ETH
volumeETHdaily = ETH$`ETH-USD.Volume`
names(volumeETHdaily) = "Volume"
volumeETHdailydf = data.frame(Date=index(volumeETHdaily), volumeETHdaily)
volumeETHmonthdf =  mutate(volumeETHdailydf, Date=floor_date(Date,"month")) |> 
  group_by(Date) |> summarise(avgDailyVolume=mean(Volume, na.rm=TRUE))
volumeETHmonth = xts(volumeETHmonthdf[,-1], order.by=volumeETHmonthdf$Date)
volumeETHmonth$VolGrowthETH = diff(log(volumeETHmonth$avgDailyVolume))*12*100
# Repeat process for WBTC
volumeWBTCdaily = WBTC$`WBTC-USD.Volume`
names(volumeWBTCdaily) = "Volume"
volumeWBTCdailydf = data.frame(Date=index(volumeWBTCdaily), volumeWBTCdaily)
volumeWBTCmonthdf =  mutate(volumeWBTCdailydf, Date=floor_date(Date,"month")) |> 
  group_by(Date) |> summarise(avgDailyVolume=mean(Volume, na.rm=TRUE))
volumeWBTCmonth = xts(volumeWBTCmonthdf[,-1], order.by=volumeWBTCmonthdf$Date)
volumeWBTCmonth$VolGrowthWBTC = diff(log(volumeWBTCmonth$avgDailyVolume))*12*100
```

To validate the usage of the annualized growth rates, let’s run an
Augmented Dickey-Fuller test for stationarity on the volume levels and
growth rate series.

``` r
# Test for stationarity in BTV volume growth
adf.test(volumeBTCmonth$avgDailyVolume)
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  volumeBTCmonth$avgDailyVolume
    ## Dickey-Fuller = -2.5297, Lag order = 3, p-value = 0.3603
    ## alternative hypothesis: stationary

``` r
adf.test(volumeBTCmonth$VolGrowthBTC[-1])
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  volumeBTCmonth$VolGrowthBTC[-1]
    ## Dickey-Fuller = -3.9695, Lag order = 3, p-value = 0.01679
    ## alternative hypothesis: stationary

``` r
# Test for stationarity in GBTC volume growth
adf.test(volumeGBTCmonth$avgDailyVolume)
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  volumeGBTCmonth$avgDailyVolume
    ## Dickey-Fuller = -2.0932, Lag order = 3, p-value = 0.5372
    ## alternative hypothesis: stationary

``` r
adf.test(volumeGBTCmonth$VolGrowthGBTC[-1])
```

    ## Warning in adf.test(volumeGBTCmonth$VolGrowthGBTC[-1]): p-value smaller than
    ## printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  volumeGBTCmonth$VolGrowthGBTC[-1]
    ## Dickey-Fuller = -5.0733, Lag order = 3, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
# Test for stationarity in MSTR volume growth
adf.test(volumeMSTRmonth$avgDailyVolume)
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  volumeMSTRmonth$avgDailyVolume
    ## Dickey-Fuller = -1.6978, Lag order = 3, p-value = 0.6974
    ## alternative hypothesis: stationary

``` r
adf.test(volumeMSTRmonth$VolGrowthMSTR[-1])
```

    ## Warning in adf.test(volumeMSTRmonth$VolGrowthMSTR[-1]): p-value smaller than
    ## printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  volumeMSTRmonth$VolGrowthMSTR[-1]
    ## Dickey-Fuller = -4.4028, Lag order = 3, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
# Test for stationarity in ETH volume growth
adf.test(volumeETHmonth$avgDailyVolume)
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  volumeETHmonth$avgDailyVolume
    ## Dickey-Fuller = -1.9681, Lag order = 3, p-value = 0.5879
    ## alternative hypothesis: stationary

``` r
adf.test(volumeETHmonth$VolGrowthETH[-1])
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  volumeETHmonth$VolGrowthETH[-1]
    ## Dickey-Fuller = -4.0102, Lag order = 3, p-value = 0.015
    ## alternative hypothesis: stationary

``` r
# Test for stationarity in WBTC volume growth
adf.test(volumeWBTCmonth$avgDailyVolume)
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  volumeWBTCmonth$avgDailyVolume
    ## Dickey-Fuller = -1.7257, Lag order = 3, p-value = 0.6861
    ## alternative hypothesis: stationary

``` r
adf.test(volumeWBTCmonth$VolGrowthWBTC[-1])
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  volumeWBTCmonth$VolGrowthWBTC[-1]
    ## Dickey-Fuller = -3.08, Lag order = 3, p-value = 0.1376
    ## alternative hypothesis: stationary

Now let’s merge the volume growth data to the asset returns and FF5
factor data. Then we’ll generate an updated correlation matrix and plot.
Then we’ll run regressions of the asset returns on the volume growth
rates and market risk premiums.

``` r
# Select relevant variables and merge into a single xts object
assets_volregs = merge(assets_ff5,
                       volumeBTCmonth$VolGrowthBTC,
                       volumeGBTCmonth$VolGrowthGBTC,
                       volumeMSTRmonth$VolGrowthMSTR,
                       volumeETHmonth$VolGrowthETH,
                       volumeWBTCmonth$VolGrowthWBTC, 
                       join="inner")
```

    ## Warning in merge.xts(assets_ff5, volumeBTCmonth$VolGrowthBTC,
    ## volumeGBTCmonth$VolGrowthGBTC, : 'join' only applicable to two object merges

``` r
# Compute correlations and plot
cor(assets_volregs[,-c(1:5)]) |> round(2)
```

    ##               MktRF   SMB   HML   RMW   CMA    RF BTCxs GBTCxs MSTRxs ETHxs
    ## MktRF          1.00  0.34  0.05  0.08 -0.20 -0.02  0.48   0.49   0.57  0.55
    ## SMB            0.34  1.00  0.36 -0.38  0.03 -0.17  0.20   0.16   0.41  0.26
    ## HML            0.05  0.36  1.00  0.19  0.66 -0.13 -0.07  -0.10  -0.16 -0.02
    ## RMW            0.08 -0.38  0.19  1.00  0.18 -0.10 -0.14  -0.13  -0.35 -0.08
    ## CMA           -0.20  0.03  0.66  0.18  1.00 -0.21 -0.23  -0.26  -0.28 -0.17
    ## RF            -0.02 -0.17 -0.13 -0.10 -0.21  1.00  0.06   0.15   0.10 -0.07
    ## BTCxs          0.48  0.20 -0.07 -0.14 -0.23  0.06  1.00   0.96   0.65  0.78
    ## GBTCxs         0.49  0.16 -0.10 -0.13 -0.26  0.15  0.96   1.00   0.66  0.77
    ## MSTRxs         0.57  0.41 -0.16 -0.35 -0.28  0.10  0.65   0.66   1.00  0.63
    ## ETHxs          0.55  0.26 -0.02 -0.08 -0.17 -0.07  0.78   0.77   0.63  1.00
    ## WBTCxs         0.47  0.20 -0.07 -0.15 -0.22  0.06  1.00   0.96   0.65  0.78
    ## VolGrowthBTC   0.00  0.05 -0.05 -0.21 -0.20  0.05  0.17   0.19   0.14  0.21
    ## VolGrowthGBTC  0.10 -0.06  0.02  0.05 -0.01  0.12  0.09   0.19  -0.08  0.05
    ## VolGrowthMSTR  0.09  0.06 -0.02 -0.15  0.05 -0.02  0.03   0.04   0.17 -0.07
    ## VolGrowthETH   0.10  0.10  0.01 -0.14 -0.10  0.04  0.13   0.16   0.22  0.36
    ## VolGrowthWBTC -0.06 -0.05 -0.08 -0.01 -0.16 -0.11  0.05   0.04  -0.05  0.03
    ##               WBTCxs VolGrowthBTC VolGrowthGBTC VolGrowthMSTR VolGrowthETH
    ## MktRF           0.47         0.00          0.10          0.09         0.10
    ## SMB             0.20         0.05         -0.06          0.06         0.10
    ## HML            -0.07        -0.05          0.02         -0.02         0.01
    ## RMW            -0.15        -0.21          0.05         -0.15        -0.14
    ## CMA            -0.22        -0.20         -0.01          0.05        -0.10
    ## RF              0.06         0.05          0.12         -0.02         0.04
    ## BTCxs           1.00         0.17          0.09          0.03         0.13
    ## GBTCxs          0.96         0.19          0.19          0.04         0.16
    ## MSTRxs          0.65         0.14         -0.08          0.17         0.22
    ## ETHxs           0.78         0.21          0.05         -0.07         0.36
    ## WBTCxs          1.00         0.16          0.09          0.02         0.12
    ## VolGrowthBTC    0.16         1.00          0.49          0.33         0.85
    ## VolGrowthGBTC   0.09         0.49          1.00          0.31         0.39
    ## VolGrowthMSTR   0.02         0.33          0.31          1.00         0.28
    ## VolGrowthETH    0.12         0.85          0.39          0.28         1.00
    ## VolGrowthWBTC   0.04         0.56          0.24          0.40         0.49
    ##               VolGrowthWBTC
    ## MktRF                 -0.06
    ## SMB                   -0.05
    ## HML                   -0.08
    ## RMW                   -0.01
    ## CMA                   -0.16
    ## RF                    -0.11
    ## BTCxs                  0.05
    ## GBTCxs                 0.04
    ## MSTRxs                -0.05
    ## ETHxs                  0.03
    ## WBTCxs                 0.04
    ## VolGrowthBTC           0.56
    ## VolGrowthGBTC          0.24
    ## VolGrowthMSTR          0.40
    ## VolGrowthETH           0.49
    ## VolGrowthWBTC          1.00

``` r
corrplot(cor(assets_volregs[,-c(1:5)]), method="color")
```

![](README_files/figure-gfm/volregs-1.png)<!-- -->

``` r
# Bitcoin Volume Growth regression
volreg_BTC = lm(BTCxs~MktRF+VolGrowthWBTC, data=assets_volregs)
summary(volreg_BTC)
```

    ## 
    ## Call:
    ## lm(formula = BTCxs ~ MktRF + VolGrowthWBTC, data = assets_volregs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -571.20 -127.11  -18.14  133.27  647.69 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   26.06843   28.84452   0.904 0.369863    
    ## MktRF          1.75420    0.42037   4.173 0.000102 ***
    ## VolGrowthWBTC  0.02508    0.03665   0.684 0.496498    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 212.8 on 58 degrees of freedom
    ## Multiple R-squared:  0.2328, Adjusted R-squared:  0.2064 
    ## F-statistic: 8.801 on 2 and 58 DF,  p-value: 0.0004593

``` r
# GBTC Volume Growth regression
volreg_GBTC = lm(GBTCxs~MktRF+VolGrowthGBTC, data=assets_volregs)
summary(volreg_GBTC)
```

    ## 
    ## Call:
    ## lm(formula = GBTCxs ~ MktRF + VolGrowthGBTC, data = assets_volregs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -577.74 -148.95    3.41  160.23  740.11 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   18.66899   31.60801   0.591    0.557    
    ## MktRF          2.01992    0.47742   4.231 8.38e-05 ***
    ## VolGrowthGBTC  0.07857    0.06022   1.305    0.197    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 241 on 58 degrees of freedom
    ## Multiple R-squared:  0.2646, Adjusted R-squared:  0.2393 
    ## F-statistic: 10.44 on 2 and 58 DF,  p-value: 0.0001345

``` r
# MSTR Volume Growth regression
volreg_MSTR = lm(MSTRxs~MktRF+VolGrowthMSTR, data=assets_volregs)
summary(volreg_MSTR)
```

    ## 
    ## Call:
    ## lm(formula = MSTRxs ~ MktRF + VolGrowthMSTR, data = assets_volregs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -576.33 -145.77   -0.05  133.48  679.00 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    2.64081   31.99040   0.083    0.934    
    ## MktRF          2.55752    0.48380   5.286 1.97e-06 ***
    ## VolGrowthMSTR  0.07227    0.06324   1.143    0.258    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 244.5 on 58 degrees of freedom
    ## Multiple R-squared:  0.3449, Adjusted R-squared:  0.3223 
    ## F-statistic: 15.27 on 2 and 58 DF,  p-value: 4.714e-06

``` r
# Ethereum Volume Growth regression
volreg_ETH = lm(ETHxs~MktRF+VolGrowthETH, data=assets_volregs)
summary(volreg_ETH)
```

    ## 
    ## Call:
    ## lm(formula = ETHxs ~ MktRF + VolGrowthETH, data = assets_volregs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -489.92 -117.72    1.22  154.91  653.38 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  26.52818   30.52529   0.869  0.38840    
    ## MktRF         2.33089    0.46176   5.048 4.72e-06 ***
    ## VolGrowthETH  0.24156    0.08144   2.966  0.00437 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 232.9 on 58 degrees of freedom
    ## Multiple R-squared:  0.3946, Adjusted R-squared:  0.3737 
    ## F-statistic:  18.9 on 2 and 58 DF,  p-value: 4.774e-07

``` r
# Wrapped Bitcoin Volume Growth regression
volreg_WBTC = lm(WBTCxs~MktRF+VolGrowthWBTC, data=assets_volregs)
summary(volreg_WBTC)
```

    ## 
    ## Call:
    ## lm(formula = WBTCxs ~ MktRF + VolGrowthWBTC, data = assets_volregs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -570.35 -120.99  -15.55  140.34  665.04 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   26.85036   29.04285   0.925 0.359052    
    ## MktRF          1.75283    0.42326   4.141 0.000113 ***
    ## VolGrowthWBTC  0.02114    0.03691   0.573 0.568917    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 214.3 on 58 degrees of freedom
    ## Multiple R-squared:  0.2293, Adjusted R-squared:  0.2027 
    ## F-statistic: 8.627 on 2 and 58 DF,  p-value: 0.0005248

#### Bitcoin Mining Hashrate as a Factor

Now we’ll use the [mempool.space
API](https://mempool.space/docs/api/rest) to retrieve data regarding the
Bitcoin mining industry. Specifically, we’ll extract the daily hashrate
data for the Bitcoin network.

``` r
# Build endpoint url for hashrates
mempoolbase = "https://mempool.space/api/v1/"
hashrateendpt = "mining/hashrate/pools/"
hashrateurl = paste(mempoolbase, hashrateendpt, sep="")
# Make API call and read JSON response
hashrateresponse = fromJSON(hashrateurl)
```

The API response contains two data tables: `hashrates` has daily
hashrates at daily frequency and the `difficulty` table has observations
at the end of each Bitcoin epoch. Since an epoch in Bitcoin is defined
by 2,016 blocks, this is roughly once every other week. However, short
term variation in mining speed can lead to non-uniform spacing between
observations.

``` r
# Extract hashrate table and difficulty table
hashratedf = hashrateresponse$hashrates
difficultydf = hashrateresponse$difficulty
# Reformat dates from unix time to R date
hashratedf$date = hashratedf$timestamp |> as.POSIXct() |> as.Date()
difficultydf$date = difficultydf$time |> as.POSIXct() |> as.Date()
# Convert daily hashrate avgs to monthly average
hashratemonthdf =  mutate(hashratedf, date=floor_date(date,"month")) |> 
  group_by(date) |> summarise(avgHashrate=mean(avgHashrate, na.rm=TRUE))
# Reformat to monthly xts object
hashratemonth = xts(hashratemonthdf[,-1], order.by=as.yearmon(hashratemonthdf$date))
# Compute annualized growth rate in hashrate
hashratemonth$HashrateGrowth = diff(log(hashratemonth$avgHashrate))*12*100
# Merge to asset and FF5 factors
assets_hashregs = merge(assets_ff5, hashratemonth, join="inner")
```

As with the trading volume levels and growth rates, let’s run some
stationarity tests on the hashrate levels and growth rates.

``` r
# Test for stationarity in hashrate growth
adf.test(assets_hashregs$avgHashrate)
```

    ## Warning in adf.test(assets_hashregs$avgHashrate): p-value greater than printed
    ## p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  assets_hashregs$avgHashrate
    ## Dickey-Fuller = 1.9729, Lag order = 3, p-value = 0.99
    ## alternative hypothesis: stationary

``` r
adf.test(assets_hashregs$HashrateGrowth[-1])
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  assets_hashregs$HashrateGrowth[-1]
    ## Dickey-Fuller = -3.8918, Lag order = 3, p-value = 0.02043
    ## alternative hypothesis: stationary

Now let’s generate update the updated correlation matrix and plot:

``` r
# Compute correlations and plot
cor(assets_hashregs[,-c(1:5)]) |> round(2)
```

    ##                MktRF   SMB   HML   RMW   CMA    RF BTCxs GBTCxs MSTRxs ETHxs
    ## MktRF           1.00  0.34  0.05  0.08 -0.20 -0.02  0.48   0.49   0.57  0.55
    ## SMB             0.34  1.00  0.36 -0.38  0.03 -0.17  0.20   0.16   0.41  0.26
    ## HML             0.05  0.36  1.00  0.19  0.66 -0.13 -0.07  -0.10  -0.16 -0.02
    ## RMW             0.08 -0.38  0.19  1.00  0.18 -0.10 -0.14  -0.13  -0.35 -0.08
    ## CMA            -0.20  0.03  0.66  0.18  1.00 -0.21 -0.23  -0.26  -0.28 -0.17
    ## RF             -0.02 -0.17 -0.13 -0.10 -0.21  1.00  0.06   0.15   0.10 -0.07
    ## BTCxs           0.48  0.20 -0.07 -0.14 -0.23  0.06  1.00   0.96   0.65  0.78
    ## GBTCxs          0.49  0.16 -0.10 -0.13 -0.26  0.15  0.96   1.00   0.66  0.77
    ## MSTRxs          0.57  0.41 -0.16 -0.35 -0.28  0.10  0.65   0.66   1.00  0.63
    ## ETHxs           0.55  0.26 -0.02 -0.08 -0.17 -0.07  0.78   0.77   0.63  1.00
    ## WBTCxs          0.47  0.20 -0.07 -0.15 -0.22  0.06  1.00   0.96   0.65  0.78
    ## avgHashrate     0.02 -0.04  0.06 -0.07 -0.08  0.76  0.00   0.07   0.15 -0.03
    ## HashrateGrowth -0.02  0.00  0.25 -0.10  0.15  0.17  0.14   0.10   0.02  0.15
    ##                WBTCxs avgHashrate HashrateGrowth
    ## MktRF            0.47        0.02          -0.02
    ## SMB              0.20       -0.04           0.00
    ## HML             -0.07        0.06           0.25
    ## RMW             -0.15       -0.07          -0.10
    ## CMA             -0.22       -0.08           0.15
    ## RF               0.06        0.76           0.17
    ## BTCxs            1.00        0.00           0.14
    ## GBTCxs           0.96        0.07           0.10
    ## MSTRxs           0.65        0.15           0.02
    ## ETHxs            0.78       -0.03           0.15
    ## WBTCxs           1.00        0.00           0.14
    ## avgHashrate      0.00        1.00           0.07
    ## HashrateGrowth   0.14        0.07           1.00

``` r
corrplot(cor(assets_hashregs[,-c(1:5)]), method="color")
```

![](README_files/figure-gfm/hashcors-1.png)<!-- -->

Now let’s run some factor models with the hashrate growth rates as a
factor.

``` r
# Bitcoin Hashrate Growth regression
hashreg_BTC = lm(BTCxs~MktRF+HashrateGrowth, data=assets_hashregs)
summary(hashreg_BTC)
```

    ## 
    ## Call:
    ## lm(formula = BTCxs ~ MktRF + HashrateGrowth, data = assets_hashregs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -554.63 -134.70  -10.14  114.61  651.60 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     11.2375    31.5633   0.356    0.723    
    ## MktRF            1.7476     0.4154   4.207 9.08e-05 ***
    ## HashrateGrowth   0.3858     0.2970   1.299    0.199    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 210.6 on 58 degrees of freedom
    ## Multiple R-squared:  0.2485, Adjusted R-squared:  0.2226 
    ## F-statistic: 9.589 on 2 and 58 DF,  p-value: 0.0002525

``` r
# GBTC Hashrate Growth regression
hashreg_GBTC = lm(GBTCxs~MktRF+HashrateGrowth, data=assets_hashregs)
summary(hashreg_GBTC)
```

    ## 
    ## Call:
    ## lm(formula = GBTCxs ~ MktRF + HashrateGrowth, data = assets_hashregs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -549.86 -170.16   16.98  160.75  744.80 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      4.4789    36.3419   0.123    0.902    
    ## MktRF            2.0897     0.4783   4.370 5.22e-05 ***
    ## HashrateGrowth   0.3345     0.3420   0.978    0.332    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 242.5 on 58 degrees of freedom
    ## Multiple R-squared:  0.2553, Adjusted R-squared:  0.2296 
    ## F-statistic: 9.943 on 2 and 58 DF,  p-value: 0.0001936

``` r
# MSTR Hashrate Growth regression
hashreg_MSTR = lm(MSTRxs~MktRF+HashrateGrowth, data=assets_hashregs)
summary(hashreg_MSTR)
```

    ## 
    ## Call:
    ## lm(formula = MSTRxs ~ MktRF + HashrateGrowth, data = assets_hashregs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -505.24 -142.02  -15.59  120.61  683.16 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     -1.2098    37.0139  -0.033    0.974    
    ## MktRF            2.6085     0.4871   5.355 1.53e-06 ***
    ## HashrateGrowth   0.1021     0.3483   0.293    0.771    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 247 on 58 degrees of freedom
    ## Multiple R-squared:  0.3311, Adjusted R-squared:  0.308 
    ## F-statistic: 14.36 on 2 and 58 DF,  p-value: 8.614e-06

``` r
# Ethereum Hashrate Growth regression
hashreg_ETH = lm(ETHxs~MktRF+HashrateGrowth, data=assets_hashregs)
summary(hashreg_ETH)
```

    ## 
    ## Call:
    ## lm(formula = ETHxs ~ MktRF + HashrateGrowth, data = assets_hashregs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -494.32 -136.73   -9.91  134.84  739.89 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      5.0678    36.7252   0.138    0.891    
    ## MktRF            2.4883     0.4833   5.149 3.27e-06 ***
    ## HashrateGrowth   0.5285     0.3456   1.529    0.132    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 245.1 on 58 degrees of freedom
    ## Multiple R-squared:  0.3298, Adjusted R-squared:  0.3067 
    ## F-statistic: 14.27 on 2 and 58 DF,  p-value: 9.11e-06

``` r
# Wrapped Bitcoin Hashrate Growth regression
hashreg_WBTC = lm(WBTCxs~MktRF+HashrateGrowth, data=assets_hashregs)
summary(hashreg_WBTC)
```

    ## 
    ## Call:
    ## lm(formula = WBTCxs ~ MktRF + HashrateGrowth, data = assets_hashregs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -555.19 -134.14  -10.87  128.18  663.46 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     10.4702    31.7133   0.330    0.742    
    ## MktRF            1.7493     0.4173   4.192 9.58e-05 ***
    ## HashrateGrowth   0.3999     0.2984   1.340    0.185    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 211.6 on 58 degrees of freedom
    ## Multiple R-squared:  0.2482, Adjusted R-squared:  0.2223 
    ## F-statistic: 9.574 on 2 and 58 DF,  p-value: 0.0002552
