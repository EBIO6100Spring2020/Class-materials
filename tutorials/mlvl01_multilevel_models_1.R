#' ---
#' title: Multilevel model of radon levels I
#' author: Brett Melbourne
#' date: 14 Oct 2018 (updated 25 Feb 2020)
#' output: github_document
#' ---

#' This script can be rendered to a reproducible report.\
#' `rmarkdown::render("mlvl01_multilevel_models_1.R")`, or *Ctrl+Shift+K* in RStudio.\
#'

#' Reading: Chapter 12 of Gelman & Hill\
#' See `data/radon_MN_about.txt` for data source\
#' This is part I: EDA and variance components model (G&H 12.2).\
#' We fit a simple variance components model of the average that accounts for
#' grouping structures (in other language, it is purely a random effects model
#' and does not have any fixed effects).
#' 

#' Requires packages `rstan` and `rstanarm`. Installation instructions in
#' [stan01_install.md](stan01_install.md).
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

#' Read in data
radon_dat <- read.csv("data/radon_MN.csv", as.is=TRUE)

#' ### Exploratory data analysis

#' Here we get to know the dataset, spot any problems or unusual features, and
#' do some informal graphical modeling by exploring how the data are structured
#' into groups and focusing on distributions. Thus, distributional plots like
#' histograms, boxplots, and density plots feature prominently.
#' 

#' #### Inspect the data

#' e.g. in each column: what class? what unique values? what range?

#+ results=FALSE
head(radon_dat)
for ( c in 1:ncol(radon_dat) ) {
    print(paste("Column name:",names(radon_dat)[c]))
    print(class(radon_dat[,c]))
    print(unique(radon_dat[,c]))
    print(range(radon_dat[,c]))
}
#' There are 3 points with zero radon recorded. They are all on the first floor:
sum(radon_dat$radon == 0)
radon_dat %>%
    filter(radon==0)

#' #### Graphical views of the data

#' Histogram of radon levels
ggplot(data=radon_dat) +
    geom_histogram(mapping = aes(x=radon),bins=50)

#' From the histogram we see that radon levels are positive, continuous, and
#' skewed with a long right tail, so perhaps a lognormal or gamma distribution or
#' similar will be appropriate for modeling these data.
#'
#' With ggplot it is easy to group a histogram by floor. We see that there are
#' far fewer measurements taken on the first floor and that basements have
#' higher radon levels. Grouped histograms are stacked by default
#' (position="stack"). To compare the two groups, we don't want to stack them
#' but overlay them, so we want position="identity".
ggplot(data=radon_dat) +
    geom_histogram(mapping = aes(x=radon,fill=floor),position="identity",
                   bins=50,alpha=0.5)

#' Another way to view this difference between floors is with boxplots. We see
#' that radon is higher in basements.
ggplot(data=radon_dat) +
    geom_boxplot(mapping = aes(y=radon, x=floor)) 

#' Violin plots combine the information in boxplots and histograms. The shape is
#' a density smoother. Tufte might complain of too much data ink. ;)
ggplot(data=radon_dat) +
    geom_violin(mapping = aes(y=radon,x=floor,fill=floor,col=floor))

#' Exploration of structured data is where `ggplot` shines. Let's look at radon
#' levels by county and floor. We want to see all the data. To combat overlap, I
#' have jittered the points on the y-axis (the y-axis does not measure anything)
#' by appending (using `mutate()`) a new column called `yjit`, and I've added
#' some transparency (`alpha`). This plot was quite rapid to put together and to
#' try variations and modifications etc. Thus, we are able to get a sense of all
#' of the features of this dataset without much coding effort. You may need a
#' large screen or window that fills your laptop screen. This plot will take a
#' moment to compile.
#' 

#+ fig.width=14, fig.height=10, dev='svg'
radon_dat %>%
    mutate(yjit=jitter(0*radon)) %>%
    ggplot() +
    geom_point(mapping = aes(x=radon,col=floor,y=yjit),shape=1,alpha=0.5) +
    facet_wrap(facets = ~ county) +
    ylim(-0.1,0.1) +
    scale_x_continuous(breaks=c(0,20,40),limits=c(0,50)) +
    theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),
          axis.title.y=element_blank(),panel.grid=element_blank())

#' From this plot we can easily see many data features. There aren't any points
#' that seem especially like outliers or obvious errors. The data have probably
#' been well cleaned. Most measurements are in basements. Some counties have a
#' lot more data. Which counties have the most data? Calculate sample size in
#' each county and sort by decreasing:
radon_dat %>%
    group_by(county) %>%
    summarize(sample_size=n()) %>%
    arrange(-sample_size)

#' We see 5 counties with more than 40 houses sampled. Let's extract these
#' (using `filter()`) to get a better sense of how radon distributions within
#' counties might vary between counties. We'll also restrict to basement data.
lrgst5cty <- radon_dat %>%
    group_by(county) %>%
    filter(n() > 40,floor=="basement")

#' Histograms for these five counties. Anoka and St Louis counties are more
#' concentrated around low radon levels whereas the other counties have broader
#' and higher radon levels. So, there appears to be variation among counties.
ggplot(data=lrgst5cty) +
    geom_histogram(mapping = aes(x=radon,y=stat(density)),bins=36) +
    facet_wrap(facets = ~ county)

#' Can we see this with overlapping histograms? Not very well.
ggplot(data=lrgst5cty) +
    geom_histogram(mapping = aes(x=radon,y=stat(density),fill=county),
                   position="identity",bins=36,alpha=0.6)

#' OK so that plot was a dud. No worries, we can try a different view to show the
#' data more clearly. A density plot is a better view and confirms our impression
#' from above.
ggplot(data=lrgst5cty) +
    geom_density(mapping = aes(x=radon,col=county))

#' #### Consider lognormal distribution

#' We are getting close to being ready for model fitting now. I suspect a
#' lognormal distribution could be useful to model variation. The easiest way to
#' do that is to simply transform the data (but we could certainly model a
#' lognormal or gamma directly too). Gelman & Hill log transform, so we'll do
#' that too. However, there are zeros in the radon data and we can't log those.
#' To deal with this Gelman & Hill add 0.1 to the zeros only. This makes sense
#' because that is the resolution of the measurement devices (notice that the
#' radon data are in increments of 0.1).
radon_dat <- mutate(radon_dat,log_radon=log(ifelse(radon==0,0.1,radon)))

#' Take a look at the transformed data in those five most-sampled counties
#'
#' First make a data frame with the estimated normal distribution for the five
#' counties. There is probably a clever way to do this entirely with `dplyr`
#' using `spread()` but here I've done part via a for loop.
#'
#' Summary statistics for 5 counties
sm5cty <-
    radon_dat %>%
    group_by(county) %>%
    filter(n() > 40,floor=="basement") %>%
    summarize(mean=mean(log_radon),sd=sd(log_radon),min=min(log_radon),max=max(log_radon))
sm5cty

#' Normal distribution fitted to the log-transformed data for 5 counties
#' (nothing fancy here: the estimated parameters of the distribution are just
#' the mean and standard deviation of the data) and collected into a dataframe.
norm_df <- NULL
for ( i in 1:5 ) {
    x <- seq(sm5cty$min[i],sm5cty$max[i],length.out = 100)
    y <- dnorm(x, sm5cty$mean[i], sm5cty$sd[i])
    norm_df <- rbind(norm_df,data.frame(x,y,county=sm5cty$county[i]))
}
rm(x,y) #clean up
head(norm_df)

#' Now plot the transformed data with density smoother (blue) and fitted normal
#' (red). The log transformation seems very good:
radon_dat %>%
    group_by(county) %>%
    filter(n() > 40,floor=="basement") %>%
    ggplot() +
    geom_histogram(mapping = aes(x=log_radon,y=stat(density)),bins=25) +
    geom_density(mapping = aes(x=log_radon),col="blue") +
    geom_line(data=norm_df, mapping = aes(x=x,y=y),col="red") +
    facet_wrap(facets = ~ county)

#' And now for the whole dataset (all counties combined) split by floor.
sm_byfloor <-
    radon_dat %>%
    group_by(floor) %>%
    summarize(mean=mean(log_radon),sd=sd(log_radon),min=min(log_radon),max=max(log_radon))

#' Normal fitted for 2 floors
norm_df <- NULL
for ( i in 1:2 ) {
    x <- seq(sm_byfloor$min[i],sm_byfloor$max[i],length.out = 100)
    y <- dnorm(x, sm_byfloor$mean[i], sm_byfloor$sd[i])
    norm_df <- rbind(norm_df,data.frame(x,y,floor=sm_byfloor$floor[i]))
}
rm(x,y)
head(norm_df)

#' Now plot the transformed data with density smoother (blue) and fitted normal
#' (red).
radon_dat %>%
    group_by(floor) %>%
    ggplot() +
    geom_histogram(mapping = aes(x=log_radon,y=stat(density)),bins=25) +
    geom_density(mapping = aes(x=log_radon),col="blue") +
    geom_line(data=norm_df, mapping = aes(x=x,y=y),col="red") +
    facet_wrap(facets = ~ floor)

#' The log transformation seems excellent also for the entire dataset grouped by
#' floor. You can see a small artifact introduced by adding 0.1 to the zeros
#' (this is the little leftmost spike in the first floor histogram) but this
#' should not be harmful. Do remember that the important thing is not that the
#' dependent variable *per se* is Normal, it is that the **errors**, **or
#' stochasticity**, are Normal. By grouping the data in the various ways that
#' they are structured by independent variables and then looking at the
#' distributions we are doing an informal modeling - i.e. graphically "fitting"
#' floor or county and then examining the distribution after accounting for the
#' expectation. Here, we have in essence looked at a main effect of *county* and
#' a main effect of *floor*, which are the strongest signals to appear in the
#' data from our graphical exploration.
#' 

#' ### G&H 12.2. Multilevel analysis with no predictors

#' Our aim here is to look at some models for the mean. We'll look at three
#' models:\
#' 1. Complete pooling - the simplest model for the overall mean\
#' 2. No pooling - county means, considering counties as fixed effects\
#' 3. Partial pooling - county means, considering counties as random effects\
#' G&H prefer to not use the terms fixed and random but I use them here because
#' many of you will know it this way already. See G&H for more discussion of
#' this.
#' 

#' We will broadly follow Gelman & Hill's analysis in Chapter 12 with some
#' elaborations here and there, and we'll use `rstanarm` instead of BUGS.
#'
#' Start by converting county to a factor
radon_dat <- mutate(radon_dat,county=factor(county))

#' #### Complete pooling
#'
#' In this case, complete pooling is just the overall mean. That is, we omit any
#' data structure or grouping variables.
poolmean <- mean(radon_dat$log_radon)
poolmean
cp_pred_df <- data.frame(poolmean) #df for use with ggplot

#' #### No pooling 

#' You can think of **no pooling** as separately calculating an estimate of the
#' mean for each county. For example, tabulate the means (and sd and se) for
#' each county:
lnrad_mean_var <- 
    radon_dat %>%
    group_by(county) %>%
    summarize(sample_size=n(),cty_mn=mean(log_radon),cty_sd=sd(log_radon)) %>%
    mutate(cty_se=cty_sd/sqrt(sample_size)) %>%
    mutate(sample_size_jit=jitter(sample_size)) #jitter added for plotting

#' Whenever I do a calculation or summary operation I like to look at the whole
#' result to check that everything makes sense and scan for problems.
print(lnrad_mean_var,n=Inf) #n=Inf to print all rows

#' We see there are three counties with only one sample, so we were not able to
#' calculate a standard deviation for those. We could fix this (by estimating
#' from sample size and sd from the other counties) but let's not worry at this
#' stage. Plot what we've got:
ggplot(data=lnrad_mean_var) +
    geom_hline(mapping=aes(yintercept=poolmean),data=cp_pred_df,col="blue") +
    geom_point(mapping=aes(x=sample_size_jit,y=cty_mn)) +
    geom_linerange(mapping=aes(x=sample_size_jit,ymin=cty_mn-cty_se,ymax=cty_mn+cty_se)) +
    scale_x_continuous(trans="log",breaks=c(1,3,10,30,100)) +
    labs(x="Sample size in county j",y="mean ln(radon) in county j",
         title="No pooling: separate analyses by county")

#' This plot is very similar to G&H Fig. 12.1a but not the same. The blue line
#' is the completely pooled estimate (the overall mean). Some of the standard
#' errors are larger than G&H 12.1a because we have calculated them
#' independently for each county. The three points to the left without an
#' interval are the ones we couldn't calculate a se for.
#'
#' Now we'll do as G&H did in Ch 12. This is also a **no pooling** analysis for
#' the county means. This analysis does not pool information about the **means**
#' but it does pool information about the **uncertainty** (the error of each
#' observation contributes to an estimate of the mean residual error). This is
#' sometimes called the **fixed effects model**, where here `county` is the
#' fixed effect. To fit this model in a frequentist paradigm we can use `lm()`,
#' which is implicitly a GLM with Normal distribution and identity link. We fit
#' `county` as a categorical variable, which gives us estimated means for each
#' county where the maximum likelihood estimate is just the mean of the
#' within-county samples. We use the means parameterization (i.e without the
#' intercept, thus "-1"):
npfit <- lm( log_radon ~ -1 + county, data=radon_dat )

#' Check the fitted model (diagnostic plots)
#+ warning=FALSE
plot(npfit,1:5,ask=FALSE)
#' The extended left tail, including the 0 + 0.1 hack, is evident in the QQ plot
#' (that's where it peels off the line at the left hand side) but otherwise the
#' diagnostics look good. Let's also look at a residuals histogram compared to
#' the Normal distribution:
r <- residuals(npfit)
x <- seq(min(r),max(r),length.out=100)
y <- dnorm(x,mean(r),sd(r))
res_df <- data.frame(residuals=r)
norm_df <- data.frame(x=x,y=y)
rm(r,x,y)
ggplot() +
    geom_histogram(mapping=aes(x=residuals,y=stat(density)),data=res_df,bins=60) +
    geom_line(mapping=aes(x=x,y=y),col="red",data=norm_df)
#' So, Normal looks like a good approximation for the errors.
#' 
#' Plot the fitted model.
np_pred_df <- data.frame(coef(summary(npfit))[,1:2],lnrad_mean_var$sample_size_jit)
names(np_pred_df) <- c("cty_mn","cty_se","sample_size_jit")
gh12.1a <- 
    ggplot(data=np_pred_df) +
    geom_hline(mapping=aes(yintercept=poolmean),data=cp_pred_df,col="blue") +
    geom_point(mapping=aes(x=sample_size_jit,y=cty_mn)) +
    geom_linerange(mapping=aes(x=sample_size_jit,ymin=cty_mn-cty_se,ymax=cty_mn+cty_se)) +
    scale_x_continuous(trans="log",breaks=c(1,3,10,30,100)) +
    ylim(-0.1,3.3) +
    labs(x="Sample size in county j",y="mean ln(radon) in county j",
         title="No pooling: estimates from linear model fit")
gh12.1a
#' Apart from some unimportant details, this is the same as G&H Fig. 12.1a. The
#' blue line is the complete pooling model (i.e. the overall mean).
#' 

#' #### Partial pooling & shrinkage in multilevel model
#'
#' In the **complete pooling** model (i.e. the overall mean) we did not include
#' variation among counties, while in the **no pooling** model, we estimated the
#' county means separately, whether literally by separate analyses or in the
#' fixed effects model. In the **partial pooling** model the estimates for the
#' mean in each county are a balance between the information in a county sample
#' and information from other counties. To achieve this, we formulate a
#' **multilevel model**. In the multilevel model we consider two levels for
#' means: an overall mean and means for counties. Each of the two levels of
#' these means has an associated stochastic process so that there are two
#' variance components, a between-county variance associated with the overall
#' mean and a within-county variance associated with the county means. To fit
#' this model in a frequentist paradigm we can use `lmer()` from the package
#' `lme4`. This model is implicitly a generalized linear mixed model (GLMM) with
#' Normal distribution, identity link, and two levels of stochasticity:

ppfit <- lmer( log_radon ~ 1 + (1|county), REML=FALSE, data=radon_dat )

#' The `1` part of the above model specifies the overall mean (the intercept
#' term) while the `+ (1|county)` part specifies that the intercepts for each
#' county should be random variables (more specifically the deviations, or
#' "random effects", of county means from the overall mean will be modeled as a
#' Normally distributed random variable). `REML=FALSE` says to fit by ordinary
#' maximum likelihood rather than the default residual maximum likelihood.
#'
#' By default, we get limited diagnostics for `lmer()`. Just residuals vs
#' fitted. The residual plot looks good though.
plot(ppfit)

#' In the summary we now see estimates for two levels (or strata) of variance,
#' county (among counties) and residual (among houses within counties):
summary(ppfit)
#' The random effects table shows that the variance at the houses-within county
#' level, the residual variance (0.6), is about 6 times greater than the
#' variance at the between-county level (0.09). In other words, most of the
#' variance in radon is at a small spatial scale, i.e. between houses. Keep in
#' mind that the house-level variance includes radon measurement error in
#' addition to natural variability among houses.
#'
#' Plot the fitted model
pp_pred_df <- data.frame(coef(ppfit)$county,se.ranef(ppfit)$county[,1],lnrad_mean_var$sample_size_jit)
names(pp_pred_df) <- c("cty_mn","cty_se","sample_size_jit")
pp_mean_df <- data.frame(ovrl_mn=summary(ppfit)$coefficients[1],ovrl_se=summary(ppfit)$coefficients[2])
gh12.1b <- 
    ggplot(data=pp_pred_df) +
    geom_hline(mapping=aes(yintercept=poolmean),data=cp_pred_df,col="blue") +
    geom_hline(mapping=aes(yintercept=ovrl_mn),data=pp_mean_df,col="blue",lty=2) +
    geom_point(mapping=aes(x=sample_size_jit,y=cty_mn)) +
    geom_linerange(mapping=aes(x=sample_size_jit,ymin=cty_mn-cty_se,ymax=cty_mn+cty_se)) +
    scale_x_continuous(trans="log",breaks=c(1,3,10,30,100)) +
    ylim(-0.1,3.3) +
    labs(x="Sample size in county j",y="mean ln(radon) in county j",
         title="Partial pooling: multilevel model, max likelihood")

#' Add a reference point to no pooling and partial pooling plots to illustrate
#' shrinkage:
gh12.1a_ref <- gh12.1a + 
    geom_point(mapping=aes(x=sample_size_jit,y=cty_mn),data=np_pred_df[36,],pch=1,cex=10,col="red")
gh12.1b_ref <- gh12.1b + 
    geom_point(mapping=aes(x=sample_size_jit,y=cty_mn),data=pp_pred_df[36,],pch=1,cex=10,col="red")

#' Plot side by side
#+ fig.width=14, fig.height=7
grid.arrange(gh12.1a_ref, gh12.1b_ref, nrow = 1) 

#' The right panel is the fitted multilevel model compared to our previous fit
#' of the no pooling model in the left panel. In the multilevel model the
#' estimates for the mean in each county are a balance between the sample mean
#' and the overall mean, depending on the within-county sample size. That is,
#' the information in a particular county is pooled with the information from
#' other counties. You can see how this works by comparing the multilevel
#' (partial pooling) model in the right panel to the no pooling model in the
#' left panel. If there are more observations for a given county, there is more
#' information at the county level, so the estimate of the county mean in the
#' multilevel model remains close to the sample mean for the county. If there
#' are fewer observations, information from the other counties will pull an
#' estimate for a particular county toward the overall mean, like county 36,
#' which is circled in red. This is called **shrinkage**. The estimate shrinks
#' toward the overall mean. The other thing to note is the dashed blue line.
#' This is the estimated overall mean from the multilevel model, which is also a
#' balance of the information at different levels. You can see that it is higher
#' than the simpler (but naive) overall mean of the data (solid blue line).
#' 

#' #### Partial pooling, Bayesian fit of multilevel model

#' Figure 12.1b in G&H was actually from a Bayesian version of the multilevel
#' model fitted using BUGS. Compared to the maximum likelihood model we just
#' fitted, this model had flat priors for the three model parameters (overall
#' mean and the two variances). The Bayesian version of this model is
#' accomplished easily with the `stan_lmer()` function of `rstanarm`. We will
#' use the weakly informative priors of `stan_lmer()` by default rather than the
#' flat priors in the BUGS fit of G&H. The difference in analyses is neglible as
#' the data overwhelm the priors in this case.

ppfit_bayes <- stan_lmer( log_radon ~ 1 + (1|county), data=radon_dat )
print(summary(ppfit_bayes)[,c("mean","sd","n_eff","Rhat")],digits=3)

#' Diagnostics: inspect convergence, histograms for posteriors etc
#+ eval=FALSE
launch_shinystan(ppfit_bayes)

#' Extract posterior samples
samples <- extract(ppfit_bayes$stanfit)
names(samples)
str(samples$alpha) #Samples of overall mean. Matrix: samples by row, 1 col
str(samples$b) #Samples of county deviations. Matrix: samples by row, 86 cols
#' I'm not sure what the 86th b parameter is (hmm, weird) but the first 85 are
#' the county samples.
#'
#' Algorithm for posterior samples of the county means. This is an example where
#' we want to get the posterior distribution for a **derived quantity** - the
#' county means. We merely need to add the samples for the overall mean
#' (`alpha`) to the samples for the county deviations (`b`).
countysamples <- samples$b[,1:85] * NA
for ( i in 1:85 ) {
    countysamples[,i] <- samples$b[,i] + samples$alpha
}
# Now calculate mean and standard deviation of the posterior distributions for
# the county means.
countypostmns <- rep(NA,85)
countypostses <- rep(NA,85)
for ( i in 1:85 ) {
    countypostmns[i] <- mean(countysamples[,i])
    countypostses[i] <- sd(countysamples[,i])
}

#' Plot of posterior means and standard deviations
#+ fig.width=14, fig.height=7
ppbayes_pred_df <- data.frame(countypostmns,countypostses,lnrad_mean_var$sample_size_jit)
names(ppbayes_pred_df) <- c("cty_mn","cty_se","sample_size_jit")
ppbayes_mean_df <- data.frame(ovrl_mn=mean(samples$alpha),ovrl_se=sd(samples$alpha))
gh12.1b_bayes <-
    ggplot(data=ppbayes_pred_df) +
    geom_hline(mapping=aes(yintercept=ovrl_mn),data=ppbayes_mean_df,col="blue",lty=2) +
    geom_point(mapping=aes(x=sample_size_jit,y=cty_mn)) +
    geom_linerange(mapping=aes(x=sample_size_jit,ymin=cty_mn-cty_se,ymax=cty_mn+cty_se)) +
    scale_x_continuous(trans="log",breaks=c(1,3,10,30,100)) +
    ylim(-0.1,3.3) +
    labs(x="Sample size in county j",y="mean ln(radon) in county j",
         title="Partial pooling: multilevel model, Bayesian")
grid.arrange(gh12.1b, gh12.1b_bayes, nrow = 1)
#' The maximum likelihood and Bayesian model estimates are practically
#' identical. This is not surprising, since the priors in the Bayesian model
#' were weak and thus most of the information is in the likelihood.
