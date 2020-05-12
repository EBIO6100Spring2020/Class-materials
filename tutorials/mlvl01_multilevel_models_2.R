#' ---
#' title: Multilevel model of radon levels II
#' author: Brett Melbourne
#' date: 28 Oct 2018 (updated 27 Feb 2020)
#' output:
#'     github_document:
#'         pandoc_args: --webtex
#' ---

#' Chapter 12 of Gelman & Hill\
#' See `data/radon_MN_about.txt` for data source\
#' 

#' This is part II. In part I, we did EDA and considered a variance components
#' model for homes within counties, introducing the concept of partial pooling
#' and shrinkage (G&H 12.2). Here, we will consider a house-level (G&H 12.3-4)
#' predictor of radon. In part III, we will consider a county-level predictor.
#' 

#+ results=FALSE, message=FALSE, warning=FALSE
library(lme4)      #max lik multilevel: lmer(), glmer() etc
library(arm)       #for se.ranef()
library(ggplot2)
library(gridExtra) #arranging multiple plots
library(dplyr)


#' Read in data and manipulate as required for analysis
radon_dat <- read.csv("data/radon_MN.csv", as.is=TRUE)
radon_dat$log_radon <- log(ifelse(radon_dat$radon==0,0.1,radon_dat$radon))
radon_dat$county <- factor(radon_dat$county)
head(radon_dat)


#' ### G&H 12.3. Analysis with predictor (floor) at the house level
#'
#' "House level" means house **scale**. This is the spatial scale at which the
#' predictor applies. The predictor at the house level is *floor* (basement or
#' first). There is no "house" identifier in the dataset, so we are assuming
#' that a house was either measured in the basement *or* on the first floor but
#' never both. Thus, there were no within-house measurements and the floor is an
#' attribute of the house that was measured (akin to house color, or house
#' size). If multiple measurements had been made within a house, we would need
#' to include another grouping variable, *house*, in the model.
#'
#' G&H p255 include *floor* as a continuous variable. It makes no difference
#' whether it is continuous or a factor for this particular model, it is just a
#' different parameterization. We will follow their analysis.
#'
#' Create the continuous variable - call it `floor_x`` to be consistent with
#' p255.
radon_dat$floor_x <- ifelse(radon_dat$floor=="basement",0,1)

#' #### Complete pooling
#'
#' Complete pooling is the overall regression mean. Again, we omit any
#' data structure or grouping variables.
poolfit <- lm(log_radon ~ floor_x, data=radon_dat)

#' Check the fitted model (diagnostic plots)
#+ warning=FALSE
plot(poolfit,1:5,ask=FALSE)
#' No problems here that we haven't already discovered.

summary(poolfit)
#' Since "basement" is coded as 0, the `(Intercept)` is the estimated mean radon
#' in the basement. Since "first" is 1, the slope `floor_x` (or $\beta_1$)
#' here is estimating how radon in the first floor differs from the basement. We
#' see that the first floor on average has log radon 0.61 **lower** than the
#' basement since the sign is negative.
#'
#' Save $\beta$s in dataframe for use with ggplot:
cp_pred_df <- data.frame(ovrl_b0=coef(poolfit)[1],ovrl_b1=coef(poolfit)[2])

#' #### No pooling
#'
#' In the no pooling analysis we will first investigate a model that allows the
#' mean radon to vary among counties but assumes that the relationship between
#' basement and first floor stays the same among counties. This model is the
#' **fixed effects** model, similar to the previous analysis we did without the
#' *floor* predictor but now we include `floor_x`. We use the means
#' parameterization (i.e. -1 to remove the overall intercept, which by default
#' would otherwise arbitrarily be radon in the basement of the first county,
#' sorted alphabetically).
npfit <- lm( log_radon ~ -1 + floor_x + county, data=radon_dat )

plot(npfit,1:5,ask=FALSE)
#' Nothing terribly concerning in the diagnostics. If we were going to use this
#' model in a critical application we ought to investigate the high leverage
#' points but for now we'll note the warnings and wait to see if the problem
#' persists as we close in on a good model.

summary(npfit)
#' As in the complete pooling model, the slope (`floor_x`, or $\beta_1$) is
#' negative, indicating lower radon on the first floor. The estimate is similar
#' to, but a little different from, the complete pooling model. The difference
#' is because we've included another variable (*county*) in the model, so the
#' new estimate is after adjusting for radon levels among counties. The `county`
#' estimates are the mean radon level in the basement for each county. In this
#' parameterization, the `county` estimates are the y-intercept for each county.
#' The *p*-values are meaningless because they are testing a hypothesis we have
#' no interest in (i.e. log(radon) = 0, or radon = 1, in county basements).
#' 

#' Plot the fitted model (G&H Fig. 12.2) for 8 selected counties
np_pred_df <- data.frame(coef(summary(npfit))[-1,1:2],
                         rep(coef(npfit)[1],85),
                         unique(radon_dat$county))
names(np_pred_df) <- c("cty_b0","cty_b0_se","b1","county")
display8 <- c("LAC QUI PARLE","AITKIN","KOOCHICHING","DOUGLAS","CLAY","STEARNS",
              "RAMSEY","ST LOUIS")
radon_dat %>%
    filter(county %in% display8) %>%
    ggplot() +
    geom_abline(mapping=aes(slope=b1,intercept=cty_b0),
                data=filter(np_pred_df,county %in% display8),
                col="blue") +
    geom_point(mapping=aes(x=jitter(floor_x,0.2),y=log_radon)) +
    scale_x_continuous(breaks=c(0,1)) +
    facet_wrap(facets = ~ county,ncol=4) +
    labs(x="floor",
         y="ln(radon)",
         title="No pooling: estimates from linear model fit")
#' The features to notice here are that the slope (measuring the difference in
#' radon between floors) is the same across counties but the estimated intercept
#' differs. These features are due to the model we specified.
#' 

#' Plot the estimated intercepts (G&H Fig. 12.3a)\
#' We will save this plot to compare to the partial pooling model. We first need
#' a jittered sample-size variable for plotting.
sample_size_df <- 
    radon_dat %>%
    group_by(county) %>%
    summarize(sample_size=n()) %>%
    mutate(sample_size_jit=jitter(sample_size)) #jitter added for plotting
np_pred_df <- cbind(np_pred_df,sample_size_df[,-1])

gh12.3a <- 
    ggplot(data=np_pred_df) +
    geom_hline(mapping=aes(yintercept=ovrl_b0),data=cp_pred_df,col="blue") +
    geom_point(mapping=aes(x=sample_size_jit,y=cty_b0)) +
    geom_linerange(mapping=aes(x=sample_size_jit,ymin=cty_b0-cty_b0_se,ymax=cty_b0+cty_b0_se)) +
    scale_x_continuous(trans="log",breaks=c(1,3,10,30,100)) +
    ylim(-0.1,3.5) +
    labs(x="Sample size in county j",y="Estimated intercept in county j",
         title="No pooling: estimates from linear model fit")
gh12.3a


#' #### Partial pooling: multilevel model
#'
#' In the multilevel model, we model the variation among counties in the
#' intercept:
ppfit <- lmer( log_radon ~ floor_x + (1|county), REML=FALSE, data=radon_dat )
#' The deviations of the county intercept from the mean intercept will be modeled as a
#' Normally distributed random variable.
#' 

#' Residual plot looks fine:
plot(ppfit)

#` Examine the parameter estimates:
summary(ppfit)
#' As in the model without a predictor, in the summary under `Random effects` we
#' now see estimates for two levels (or scales) of variance, `county` (among
#' counties) and `Residual` (among houses within counties). Under
#' `Fixed effects` we have the estimate for the common slope `floor_x`, while
#' the `(Intercept)` is the estimated mean radon at the county scale (see below
#' for more discussion of exactly how to interpret this mean).
#' 

#' Plot the fitted model (G&H Fig. 12.4) for 8 selected counties
pp_pred_df <- data.frame(coef(ppfit)$county,
                         se.ranef(ppfit)$county[,1],
                         sample_size_df$sample_size_jit,
                         unique(radon_dat$county))
names(pp_pred_df) <- c("cty_b0","b1","cty_b0_se","sample_size_jit","county")
pp_mean_df <- data.frame(ovrl_b0=fixef(ppfit)[1],
                         ovrl_b1=fixef(ppfit)[2])
radon_dat %>%
    filter(county %in% display8) %>%
    ggplot() +
    #no pooling line
    geom_abline(mapping=aes(slope=b1,intercept=cty_b0),
                data=filter(np_pred_df,county %in% display8),
                col="blue") +
    #partial pooling line
    geom_abline(mapping=aes(slope=b1,intercept=cty_b0),
                data=filter(pp_pred_df,county %in% display8),
                col="blue",
                lty=2) +
    #complete pooling line
    geom_abline(mapping=aes(slope=ovrl_b1,intercept=ovrl_b0),
                data=cp_pred_df,
                col="red") +
    #data
    geom_point(mapping=aes(x=jitter(floor_x,0.2),y=log_radon)) +
    scale_x_continuous(breaks=c(0,1)) +
    facet_wrap(facets = ~ county,ncol=4) +
    labs(x="floor",y="ln(radon)",
         title="Partial pooling (dashed): multilevel model, max likelihood estimates")
#' Partial pooling estimates (dashed blue line) compared to the no pooling
#' estimates (solid blue line), and the complete pooling estimate (red line).
#' The thing to notice here is that the partial pooling estimate is shrunk away
#' from the no pooling estimate toward the complete pooling estimate. This
#' shrinkage is greatest when there are few data points in a county (e.g. Lac
#' Qui Parle). When there are lots of data within a county, the partial pooling
#' estimate remains close to the no pooling estimate (e.g. St Louis).
#'

#' Plot the the estimated intercepts (G&H Fig 12.3).\
#' Plot for partial pooling estimates:
gh12.3b <- 
    ggplot(data=pp_pred_df) +
    geom_hline(mapping=aes(yintercept=ovrl_b0),
               data=cp_pred_df,
               col="blue") +
    geom_hline(mapping=aes(yintercept=ovrl_b0),
               data=pp_mean_df,
               col="blue",
               lty=2) +
    geom_point(mapping=aes(x=sample_size_jit,y=cty_b0)) +
    geom_linerange(mapping=aes(x=sample_size_jit,
                               ymin=cty_b0-cty_b0_se,
                               ymax=cty_b0+cty_b0_se)) +
    scale_x_continuous(trans="log",breaks=c(1,3,10,30,100)) +
    ylim(-0.1,3.5) +
    labs(x="Sample size in county j",
         y="Estimated intercept in county j",
         title="Partial pooling: multilevel model, max likelihood")

#' Plot no pooling vs partial pooling side by side
#+ fig.width=14, fig.height=7
grid.arrange(gh12.3a, gh12.3b, nrow = 1) 
#' The estimates for the county intercepts are shrunk toward the county-scale
#' mean (dashed blue line). The county-scale mean is the mean intercept among
#' counties estimated by the multilevel model. Notice that the county mean is
#' higher than the complete pooling intercept (solid blue line). This is because
#' the complete pooling intercept is influenced by the six or so most-sampled
#' counties (they make up a large amount of the total dataset used to fit the
#' complete pooling model), which happen to have lower radon than the mean
#' across counties. In other words, the complete pooling estimate is weighted by
#' the number of samples in a county, whereas the partial pooling estimate is
#' not. This brings up an interesting situation: what if we wanted to estimate
#' the mean radon level for houses in Minnesota? The multilevel model allows us
#' to do that. It would be the weighted mean across counties, where the weights
#' are the number of houses in a county (not the number of sampled houses).
#' Counties with more houses contribute more to the mean radon level in
#' Minnesota houses. The county-scale mean is not the "grand mean" or the mean
#' for Minnesota houses, it is the mean among Minnesota counties.
#' 

#' #### Partial pooling, Bayesian fit
#'
#' The Bayesian fit of the multilevel model using `stan_lmer()` is
#' straightforward and gives almost the same result as `lmer()`. The code is
#' substantially similar to the situation we consider next - adding a
#' county-level predictor - so we'll just skip right to that. See Part III.
#'