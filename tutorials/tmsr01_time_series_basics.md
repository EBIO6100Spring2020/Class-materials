# Time series models: the basics
A sampling of sections from Hyndman & Athanasopoulos (2018) are handy for getting the basics of time series models (in a frequentist framework).

Our projects have time series that mostly are too short for standard time series models. Where they might come in useful is in forecasting external predictor variables for which longer time series are available, such as climate, or NDVI. Two mainstream and effective approaches are:

1. [Exponential smoothing](https://otexts.com/fpp2/expsmooth.html) - Ch 7
2. [ARIMA models](https://otexts.com/fpp2/arima.html) - Ch 8

Both are straightforward to do using facilities in the `forecast` package using the functions `ets` and `auto.arima` as described in the above chapters. You could do both and compare them, as described in [8.10](https://otexts.com/fpp2/arima-ets.html). The `forecast` package is described in [3.3](https://otexts.com/fpp2/the-forecast-package-in-r.html).

In addition, autoregressive models (the AR part of ARIMA), could be useful as a component of more complex models for our short time series, so if you check out only one part of this book, see
* [8.3 Autoregressive models](https://otexts.com/fpp2/AR.html).

Graphical tools that could be useful to us for either long or short series include:
* [2.7](https://otexts.com/fpp2/lag-plots.html) - lag plots
* [2.8](https://otexts.com/fpp2/autocorrelation.html) - autocorrelation function
* [3.3](https://otexts.com/fpp2/residuals.html) - residual diagnostics

Simple forecasting models that don't need long time series, such as the average method or random walk, are described in [3.1](https://otexts.com/fpp2/simple-methods.html).

For a more in depth treatment of time series models, see the classic text by Chatfield, now in a 7th edition
* Chatfield & Xing (2019) The Analysis of Time-series: An Introduction with R. CRC Press.
* [website](https://people.bath.ac.uk/mascc/TS.html)
* [R tutorial](people.bath.ac.uk/mascc/TSA.usingR.doc)
* The 2004 6th edition is also still good and available from the library
