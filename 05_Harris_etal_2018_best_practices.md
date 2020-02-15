# Best practices
From [Harris et al. 2018](https://peerj.com/articles/4278/).

* Compare multiple models
* Use time-series data when possible
* Account for uncertainty
* Use sensible predictors
* Model unknowns as random effects
* Forecast and assess for different horizons
* Account for observation uncertainty
* Validate via hindcasts
* Publicly archive forecasts

They also illustrate:
* Simple baseline models such as the long term average or random walk (aka "naive"), which in their data typically outcompete more complex models
* [ARIMA](https://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average) time-series models
* A few types of species distribution models (SDMs), including two machine learning approaches (boosted regression trees, random forests)
* Incorporating climate forecasts using [CMIP](https://www.wcrp-climate.org/wgcm-cmip) scenarios
* Full pipeline from data to evaluating and reporting forecasts

Their code is archived here:\
https://github.com/weecology/bbs-forecasting
