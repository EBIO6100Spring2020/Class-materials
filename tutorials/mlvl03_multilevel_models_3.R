#' ---
#' title: Multilevel model of radon levels III
#' author: Brett Melbourne
#' date: 1 Nov 2018 (updated 27 Feb 2020)
#' output:
#'     github_document:
#'         pandoc_args: --webtex
#' ---

#' Chapter 12 of Gelman & Hill\
#' See `data/radon_MN_about.txt` and\
#'     `data/radon_MN_U_about.txt` for data source\
#' 

#' This is part III. Part I was EDA and introduction to partial pooling and
#' shrinkage (G&H 12.2). Part II considered a house-level predictor (G&H
#' 12.3-4). Here, we add a county-level predictor (G&H 12.6).
#' 

#+ results=FALSE, message=FALSE, warning=FALSE
library(lme4)      #max lik multilevel: lmer(), glmer() etc
library(arm)       #for se.ranef()
library(ggplot2)
library(gridExtra) #arranging multiple plots
library(dplyr)
library(rstan)     #for extract()
library(rstanarm)  #Bayesian multilevel: stan_lmer(), stan_glmer() etc
options(mc.cores = parallel::detectCores())
theme_set(theme_grey()) #rstanarm overrides default ggplot theme: set it back

#' Read in data and manipulate as required for analysis (see Parts I & II)
radon_dat <- read.csv("data/radon_MN.csv", as.is=TRUE)
radon_dat$log_radon <- log(ifelse(radon_dat$radon==0,0.1,radon_dat$radon))
radon_dat$county <- factor(radon_dat$county)
radon_dat$floor_x <- ifelse(radon_dat$floor=="basement",0,1)
head(radon_dat)


#' ### G&H 12.6. Analysis with a county level predictor (uranium)
#'
#' The predictor at the county level is uranium. That is, measurement of uranium
#' was not done house by house. The dataset has just one value for uranium per
#' county. 
uranium_dat <- read.csv("data/radon_MN_U.csv", as.is=TRUE)
head(uranium_dat)

#' Log uranium
uranium_dat$logu <- log(uranium_dat$uppm)

#' Plot of the uranium data\
#' This is a predictor variable, so we are not necessarily interested in its
#' distribution (thus, I do not choose a histogram here).
plot(x=sort(uranium_dat$logu),
     y=1:85,
     yaxt="n",
     ylab="County (ordered)",
     xlab="Log( uranium (ppm) )")
#' I don't see anything untoward in this plot.
#'
#' Perhaps counterintuitively, we now add these county-scale uranium data to the
#' dataframe that contains all our other data. The data will then be in a tidy
#' data format. Each row contains an observation for the level of radon in a
#' house but we add a column for the county-scale uranium associated with each
#' house. This means that the county-scale uranium values are repeated multiple
#' times in the dataset. This might feel like cheating - aren't we pretending
#' there are more uranium data than we actually have? No, we are not. There is
#' no problem here because uranium will be a predictor variable. Furthermore,
#' in the multilevel model, we will be estimating the effect of uranium
#' (*logu*) at the appropriate scale by including *county* as a grouping
#' variable.
radon_dat <- merge(radon_dat,uranium_dat,by="county",all.x=TRUE)
radon_dat[sample(1:nrow(radon_dat),50),] #print a sample of 50 rows to check
#' Alternatively, we could have used the function `left_join` from `dplyr`\
#'     `left_join(radon_dat, uranium_dat, by = "county")`
#' or, more literally    
#' `for ( i in 1:nrow(radon_dat) ) {
#'     radon_dat$logu[i] <- uranium_dat$logu[uranium_dat$county==radon_dat$county[i]]
#' }`
#' 


#' #### Partial pooling: multilevel model
#'
#' In the multilevel model, we model the variation among counties in the
#' intercept but now we allow the intercept to be a function of the uranium
#' level in the county.

ppfit <- lmer( log_radon ~ floor_x + logu + (1|county), REML=FALSE, data=radon_dat )

#' The deviations of the county intercepts from the county-scale mean intercept
#' will be modeled as a Normally distributed random variable.
#'
#' Residual plot looks fine.
plot(ppfit)

#' As in the model without a predictor, in the summary we have estimates for two
#' levels (or scales) of variance, county (among counties) and residual (among
#' houses within counties):
summary(ppfit)
#' Compared to our previous analysis without uranium (*logu*) as a county-level
#' predictor, the county-level variance is now markedly reduced. The variance
#' (0.02) is now about five times less than without uranium as a predictor
#' (0.1) because *logu* is accounting for most of the county-level variance.
#' 

#' The following will extract the fixed effects (the estimates of $\beta_0$,
#' $\beta_1$, $\beta_2$):
fixef(ppfit)

#' The following will extract the random effects (or county errors, i.e. the
#' deviations of each county from $\beta_0$, the county-scale mean):
ranef(ppfit)$county

#' The function `coef()` will return the county coefficients. Here are the first
#' six:
head(coef(ppfit)$county)
#' The first coefficient column here (`(Intercept)`) is the sum of the overall
#' intercept $\beta_0$ and the county random effects. That is:
head( fixef(ppfit)[1] + ranef(ppfit)$county )
#' We will use `coef()` next to form a dataframe for plotting.
#' 


#' Plot the fitted model (G&H Fig. 12.5) for 8 selected counties:

pp_pred_df <- data.frame(coef(ppfit)$county,
                         se.ranef(ppfit)$county[,1],
                         unique(radon_dat$county))
names(pp_pred_df) <- c("cty_b0","b1","b2","cty_b0_se","county")

# Calculate the intercepts for each county (this will be the log(radon) level in
# a basement):
pp_pred_df$cty_b0 <- pp_pred_df$cty_b0 + pp_pred_df$b2 * uranium_dat$logu

# Add uranium data to the county-scale results dataframe
pp_pred_df <- cbind(pp_pred_df,uranium_dat[,-1])


display8 <- c("LAC QUI PARLE","AITKIN","KOOCHICHING","DOUGLAS","CLAY","STEARNS",
              "RAMSEY","ST LOUIS")
radon_dat %>%
    filter(county %in% display8) %>%
    ggplot() +
    geom_abline(mapping=aes(slope=b1,intercept=cty_b0),
                data=filter(pp_pred_df,county %in% display8),
                col="blue",
                lty=2) +
    geom_point(mapping=aes(x=jitter(floor_x,0.2),y=log_radon)) +
    scale_x_continuous(breaks=c(0,1)) +
    facet_wrap(facets = ~ county,ncol=4) +
    labs(x="floor",
         y="ln(radon)",
         title="Partial pooling: multilevel model, max likelihood estimates")

#' These estimates are not very different from the analysis that did not include
#' uranium as a predictor (see Part II). The difference is that the intercepts
#' in each panel are now predicted by the uranium level in a county.
#' 


#' Plot the estimated intercepts (G&H Fig. 12.6):
gh12.6 <- 
    ggplot(data=pp_pred_df) +
    geom_abline(intercept=fixef(ppfit)[1],slope=fixef(ppfit)[3],col="blue") +
    geom_point(mapping=aes(x=logu,y=cty_b0)) +
    geom_linerange(mapping=aes(x=logu,ymin=cty_b0-cty_b0_se,ymax=cty_b0+cty_b0_se)) +
    ylim(0.5,2.05) +
    labs(x="Uranium level in county (log-ppm)",
         y="Estimated intercept in county j",
         title="Partial pooling: multilevel model, max likelihood estimates")
gh12.6
#' This plot is the same as in Fig. 12.6 of G&H, except that their plot appears
#' to be for the radon level on the first floor rather than the basement (thus
#' their plot is not strictly for the intercepts as labelled on the y-axis).
#' 

#' #### Bayesian fit of multilevel model

#' The Bayesian fit is straightforward but may take a minute or two.
ppfit_bayes <- stan_lmer( log_radon ~ floor_x + logu + (1|county), data=radon_dat )

#' Print a selection of columns from the summary
print(summary(ppfit_bayes)[,c("mean","sd","n_eff","Rhat")],digits=3)
#' We see that the `Rhat`s are all close to 1, suggesting the chains have
#' converged, and the effective number of replicates `n_eff` is high or maximal.
#' The parameter estimates are very similar to the maximum likelihood fit. The
#' parameterization is important though: `sigma` with a small "s" is the
#' standard deviation at the lowest scale, whereas `Sigma` with a big "S" is the
#' variance at the county scale. You need to square `sigma` to obtain the
#' variance and compare with the residual variance in the frequentist analysis,
#' whereas `Sigma` is directly comparable with the county variance in the
#' frequentist analysis.
#' 

#' Diagnostics: inspect convergence, histograms for posteriors etc
#+ eval=FALSE
launch_shinystan(ppfit_bayes)
#' The diagnostics look good. Trace plots show convergence. Histograms of
#' parameters are all fairly smooth and symmetric. The one exception is the
#' county `Sigma`, which has a longer right-tailed distribution. That's because
#' `Sigma` is a variance, which typically have such a distribution.
#' 

#' Extract posterior samples:
samples <- extract(ppfit_bayes$stanfit)
names(samples)
str(samples$alpha) #Samples of overall mean. Matrix: samples by row, 1 col
str(samples$b) #Samples of county deviations. Matrix: samples by row, 86 cols
#' I'm still not sure what the 86th b parameter is but the first 85 are the
#' county samples.
str(samples$beta) #Samples of $\beta$s. Matrix: samples by row, 2 cols.

#' County means in basements are alpha + b + $\beta_2$ * logu, so we'll first
#' derive posterior samples for the county mean and then calculate summaries of
#' their posterior distributions:
# Derive posterior samples for county means
countysamples <- samples$b[,1:85] * NA
for ( i in 1:85 ) {
    countysamples[,i] <- samples$alpha + samples$b[,i] + samples$beta[,2] * uranium_dat$logu[i]
}
# Now calculate mean and standard deviation of the posterior distributions for
# the county means.
countypostmns <- rep(NA,85)
countypostses <- rep(NA,85)
for ( i in 1:85 ) {
    countypostmns[i] <- mean(countysamples[,i])
    countypostses[i] <- sd(countysamples[,i])
}

#' Plot of posterior means and standard deviations (and compare to the maximum
#' likelihood fit):
#+ fig.width=14, fig.height=7
ppbayes_pred_df <- data.frame(cty_mn=countypostmns,cty_se=countypostses)
ppbayes_pred_df <- cbind(ppbayes_pred_df,uranium_dat) #Add U to the dataframe

gh12.6_bayes <-
    ggplot(data=ppbayes_pred_df) +
    geom_abline(intercept=mean(samples$alpha),
                slope=mean(samples$beta[,2]),
                col="blue",
                lty=2) +
    geom_point(mapping=aes(x=logu,
                           y=cty_mn)) +
    geom_linerange(mapping=aes(x=logu,
                               ymin=cty_mn-cty_se,
                               ymax=cty_mn+cty_se)) +
    ylim(0.5,2.05) +
    labs(x="Uranium level in county (log-ppm)",
         y="mean ln(radon) in county j",
         title="Partial pooling: multilevel model, Bayesian")
grid.arrange(gh12.6, gh12.6_bayes, nrow = 1)
#' Once again, the maximum likelihood and Bayesian model estimates are very
#' similar. However, the Bayesian model suggests slightly higher uncertainty
#' (larger standard errors) in the estimate of radon level in each county (I
#' think this is because the uncertainty in the variance parameters is
#' propagated to the posterior distribution for the county means, whereas the
#' variance parameters in the frequentist analysis, once estimated, are assumed
#' fixed).
#' 