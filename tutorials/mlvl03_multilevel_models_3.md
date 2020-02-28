Multilevel model of radon levels III
================
Brett Melbourne
1 Nov 2018 (updated 27 Feb 2020)

Chapter 12 of Gelman & Hill  
See `data/radon_MN_about.txt` and  
`data/radon_MN_U_about.txt` for data source  

This is part III. Part I was EDA and introduction to partial pooling and
shrinkage (G\&H 12.2). Part II considered a house-level predictor (G\&H
12.3-4). Here, we add a county-level predictor (G\&H 12.6).

``` r
library(lme4)      #max lik multilevel: lmer(), glmer() etc
library(arm)       #for se.ranef()
library(ggplot2)
library(gridExtra) #arranging multiple plots
library(dplyr)
library(rstan)     #for extract()
library(rstanarm)  #Bayesian multilevel: stan_lmer(), stan_glmer() etc
options(mc.cores = parallel::detectCores())
theme_set(theme_grey()) #rstanarm overrides default ggplot theme: set it back
```

Read in data and manipulate as required for analysis (see Parts I & II)

``` r
radon_dat <- read.csv("data/radon_MN.csv", as.is=TRUE)
radon_dat$log_radon <- log(ifelse(radon_dat$radon==0,0.1,radon_dat$radon))
radon_dat$county <- factor(radon_dat$county)
radon_dat$floor_x <- ifelse(radon_dat$floor=="basement",0,1)
head(radon_dat)
```

    ##      floor radon county log_radon floor_x
    ## 1    first   2.2 AITKIN 0.7884574       1
    ## 2 basement   2.2 AITKIN 0.7884574       0
    ## 3 basement   2.9 AITKIN 1.0647107       0
    ## 4 basement   1.0 AITKIN 0.0000000       0
    ## 5 basement   3.1  ANOKA 1.1314021       0
    ## 6 basement   2.5  ANOKA 0.9162907       0

### G\&H 12.6. Analysis with a county level predictor (uranium)

The predictor at the county level is uranium. That is, measurement of
uranium was not done house by house. The dataset has just one value for
uranium per county.

``` r
uranium_dat <- read.csv("data/radon_MN_U.csv", as.is=TRUE)
head(uranium_dat)
```

    ##      county     uppm
    ## 1    AITKIN 0.502054
    ## 2     ANOKA 0.428565
    ## 3    BECKER 0.892741
    ## 4  BELTRAMI 0.552472
    ## 5    BENTON 0.866849
    ## 6 BIG STONE 1.472640

Log uranium

``` r
uranium_dat$logu <- log(uranium_dat$uppm)
```

Plot of the uranium data  
This is a predictor variable, so we are not necessarily interested in
its distribution (thus, I do not choose a histogram here).

``` r
plot(x=sort(uranium_dat$logu),
     y=1:85,
     yaxt="n",
     ylab="County (ordered)",
     xlab="Log( uranium (ppm) )")
```

![](mlvl03_multilevel_models_3_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

I don’t see anything untoward in this plot.

Perhaps counterintuitively, we now add these county-scale uranium data
to the dataframe that contains all our other data. The data will then be
in a tidy data format. Each row contains an observation for the level of
radon in a house but we add a column for the county-scale uranium
associated with each house. This means that the county-scale uranium
values are repeated multiple times in the dataset. This might feel like
cheating - aren’t we pretending there are more uranium data than we
actually have? No, we are not. There is no problem here because uranium
will be a predictor variable. Furthermore, in the multilevel model, we
will be estimating the effect of uranium (*logu*) at the appropriate
scale by including *county* as a grouping variable.

``` r
radon_dat <- merge(radon_dat,uranium_dat,by="county",all.x=TRUE)
radon_dat[sample(1:nrow(radon_dat),50),] #print a sample of 50 rows to check
```

    ##          county    floor radon   log_radon floor_x     uppm        logu
    ## 790     STEARNS    first   2.6  0.95551145       1 1.123440  0.11639541
    ## 645       SCOTT    first   2.0  0.69314718       1 1.065150  0.06311563
    ## 726    ST LOUIS basement   1.1  0.09531018       0 0.622088 -0.47467372
    ## 833      WADENA basement   2.9  1.06471074       0 0.510671 -0.67202973
    ## 526     OLMSTED basement   2.3  0.83290912       0 1.275260  0.24315008
    ## 251     GOODHUE basement  14.3  2.66025954       0 1.217270  0.19661065
    ## 426        LAKE basement   1.5  0.40546511       0 0.500776 -0.69159638
    ## 914      WRIGHT basement   9.5  2.25129180       0 0.913909 -0.09002427
    ## 567        POLK basement   9.6  2.26176310       0 1.304880  0.26611108
    ## 610        RICE basement   9.7  2.27212589       0 1.213060  0.19314609
    ## 643       SCOTT basement   1.2  0.18232156       0 1.065150  0.06311563
    ## 472      MCLEOD basement   4.4  1.48160454       0 1.150760  0.14042259
    ## 342    HENNEPIN basement   7.6  2.02814825       0 0.907991 -0.09652081
    ## 605     REDWOOD basement   7.4  2.00148000       0 1.442420  0.36632226
    ## 908      WRIGHT basement   2.1  0.74193734       0 0.913909 -0.09002427
    ## 256     GOODHUE basement  43.5  3.77276094       0 1.217270  0.19661065
    ## 165      DAKOTA    first   2.8  1.02961942       1 0.976144 -0.02414516
    ## 84   BLUE EARTH basement   1.8  0.58778666       0 1.312080  0.27161366
    ## 14        ANOKA basement   0.4 -0.91629073       0 0.428565 -0.84731286
    ## 50        ANOKA basement   1.7  0.53062825       0 0.428565 -0.84731286
    ## 905      WRIGHT basement   6.7  1.90210753       0 0.913909 -0.09002427
    ## 339    HENNEPIN basement   4.6  1.52605630       0 0.907991 -0.09652081
    ## 644       SCOTT basement   3.2  1.16315081       0 1.065150  0.06311563
    ## 41        ANOKA basement   1.4  0.33647224       0 0.428565 -0.84731286
    ## 289    HENNEPIN basement   7.2  1.97408103       0 0.907991 -0.09652081
    ## 523     OLMSTED basement  10.0  2.30258509       0 1.275260  0.24315008
    ## 13        ANOKA basement   1.4  0.33647224       0 0.428565 -0.84731286
    ## 722    ST LOUIS basement   2.3  0.83290912       0 0.622088 -0.47467372
    ## 672    ST LOUIS basement   3.9  1.36097655       0 0.622088 -0.47467372
    ## 89        BROWN basement   6.7  1.90210753       0 1.319930  0.27757870
    ## 411 KOOCHICHING basement   1.7  0.53062825       0 0.414025 -0.88182892
    ## 724    ST LOUIS basement   1.2  0.18232156       0 0.622088 -0.47467372
    ## 897      WINONA    first   1.8  0.58778666       1 1.589170  0.46321187
    ## 19        ANOKA basement   5.9  1.77495235       0 0.428565 -0.84731286
    ## 903      WINONA basement  11.8  2.46809953       0 1.589170  0.46321187
    ## 68       BENTON basement   2.7  0.99325177       0 0.866849 -0.14289048
    ## 225     DOUGLAS basement   7.4  2.00148000       0 1.168490  0.15571232
    ## 748    ST LOUIS    first   0.9 -0.10536052       1 0.622088 -0.47467372
    ## 378     HUBBARD basement   1.5  0.40546511       0 0.669920 -0.40059698
    ## 556        PINE basement   1.9  0.64185389       0 0.849343 -0.16329217
    ## 486  MILLE LACS basement   5.7  1.74046617       0 0.810536 -0.21005952
    ## 244    FREEBORN basement   4.2  1.43508453       0 1.251330  0.22420699
    ## 642       SCOTT    first   2.1  0.74193734       1 1.065150  0.06311563
    ## 312    HENNEPIN    first   0.7 -0.35667494       1 0.907991 -0.09652081
    ## 637       SCOTT    first  19.3  2.96010510       1 1.065150  0.06311563
    ## 653   SHERBURNE basement   2.5  0.91629073       0 0.504879 -0.68343648
    ## 737    ST LOUIS basement   2.7  0.99325177       0 0.622088 -0.47467372
    ## 604     REDWOOD basement   8.9  2.18605128       0 1.442420  0.36632226
    ## 517      NORMAN basement   8.1  2.09186406       0 1.303720  0.26522172
    ## 448        LYON basement   5.8  1.75785792       0 1.483990  0.39473441

Alternatively, we could have used the function `left_join` from
`dplyr`  
`left_join(radon_dat, uranium_dat, by = "county")` or, more literally  
`for ( i in 1:nrow(radon_dat) ) { radon_dat$logu[i] <-
uranium_dat$logu[uranium_dat$county==radon_dat$county[i]] }`

#### Partial pooling: multilevel model

In the multilevel model, we model the variation among counties in the
intercept but now we allow the intercept to be a function of the uranium
level in the county.

``` r
ppfit <- lmer( log_radon ~ floor_x + logu + (1|county), REML=FALSE, data=radon_dat )
```

The deviations of the county intercepts from the county-scale mean
intercept will be modeled as a Normally distributed random variable.

Residual plot looks fine.

``` r
plot(ppfit)
```

![](mlvl03_multilevel_models_3_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

As in the model without a predictor, in the summary we have estimates
for two levels (or scales) of variance, county (among counties) and
residual (among houses within counties):

``` r
summary(ppfit)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: log_radon ~ floor_x + logu + (1 | county)
    ##    Data: radon_dat
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   2132.8   2156.9  -1061.4   2122.8      914 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9976 -0.6163  0.0307  0.6561  3.3794 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  county   (Intercept) 0.02127  0.1458  
    ##  Residual             0.57499  0.7583  
    ## Number of obs: 919, groups:  county, 85
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  1.46427    0.03714  39.421
    ## floor_x     -0.66644    0.06865  -9.708
    ## logu         0.72320    0.08965   8.067
    ## 
    ## Correlation of Fixed Effects:
    ##         (Intr) flor_x
    ## floor_x -0.361       
    ## logu     0.154 -0.011

Compared to our previous analysis without uranium (*logu*) as a
county-level predictor, the county-level variance is now markedly
reduced. The variance (0.02) is now about five times less than without
uranium as a predictor (0.1) because *logu* is accounting for most of
the county-level variance.

The following will extract the fixed effects (the estimates of
![\\beta\_0](https://latex.codecogs.com/png.latex?%5Cbeta_0 "\\beta_0"),
![\\beta\_1](https://latex.codecogs.com/png.latex?%5Cbeta_1 "\\beta_1"),
![\\beta\_2](https://latex.codecogs.com/png.latex?%5Cbeta_2
"\\beta_2")):

``` r
fixef(ppfit)
```

    ## (Intercept)     floor_x        logu 
    ##   1.4642651  -0.6664446   0.7232005

The following will extract the random effects (or county errors,
i.e. the deviations of each county from
![\\beta\_0](https://latex.codecogs.com/png.latex?%5Cbeta_0 "\\beta_0"),
the county-scale mean):

``` r
ranef(ppfit)$county
```

    ##                     (Intercept)
    ## AITKIN            -0.0179049646
    ## ANOKA              0.0132967844
    ## BECKER             0.0110287605
    ## BELTRAMI           0.1000862746
    ## BENTON             0.0074905025
    ## BIG STONE         -0.0230891716
    ## BLUE EARTH         0.1172682716
    ## BROWN              0.0383440996
    ## CARLTON           -0.0611033289
    ## CARVER             0.0005928622
    ## CASS               0.0587970451
    ## CHIPPEWA           0.0087706179
    ## CHISAGO            0.0192949042
    ## CLAY               0.0887147182
    ## CLEARWATER        -0.0141832028
    ## COOK              -0.0299062147
    ## COTTONWOOD        -0.0612961592
    ## CROW WING          0.0313021082
    ## DAKOTA            -0.0783189392
    ## DODGE              0.0145052120
    ## DOUGLAS            0.0377374372
    ## FARIBAULT         -0.1906698259
    ## FILLMORE          -0.0269580937
    ## FREEBORN           0.1156793668
    ## GOODHUE            0.1148136374
    ## HENNEPIN          -0.0314492884
    ## HOUSTON           -0.0132744002
    ## HUBBARD            0.0053968669
    ## ISANTI             0.0135324630
    ## ITASCA            -0.0169968453
    ## JACKSON            0.0519435081
    ## KANABEC           -0.0244053653
    ## KANDIYOHI          0.0667917792
    ## KITTSON            0.0095636948
    ## KOOCHICHING       -0.0078642023
    ## LAC QUI PARLE      0.0855960534
    ## LAKE              -0.1418612366
    ## LAKE OF THE WOODS  0.1120497587
    ## LE SUEUR           0.0206570544
    ## LINCOLN            0.0665355006
    ## LYON               0.0480987242
    ## MAHNOMEN          -0.0075429414
    ## MARSHALL           0.0244664210
    ## MARTIN            -0.1133098250
    ## MCLEOD            -0.0932052926
    ## MEEKER            -0.0416670504
    ## MILLE LACS        -0.0313718324
    ## MORRISON          -0.0636186203
    ## MOWER              0.0132526911
    ## MURRAY             0.0264114643
    ## NICOLLET           0.0671959487
    ## NOBLES             0.0169995019
    ## NORMAN            -0.0422783607
    ## OLMSTED           -0.1564745281
    ## OTTER TAIL         0.0642765225
    ## PENNINGTON        -0.0345269495
    ## PINE              -0.1076988162
    ## PIPESTONE          0.0047696912
    ## POLK               0.0048996592
    ## POPE              -0.0267387607
    ## RAMSEY            -0.0044843913
    ## REDWOOD            0.0379257059
    ## RENVILLE          -0.0086726605
    ## RICE               0.0691085586
    ## ROCK              -0.0376774382
    ## ROSEAU             0.1105065722
    ## SCOTT              0.0898010649
    ## SHERBURNE          0.0274041021
    ## SIBLEY            -0.0506964985
    ## ST LOUIS          -0.2115118356
    ## STEARNS           -0.0313500424
    ## STEELE            -0.0214618672
    ## STEVENS           -0.0008935432
    ## SWIFT             -0.0909601372
    ## TODD               0.0274960756
    ## TRAVERSE           0.0220113029
    ## WABASHA            0.0495946771
    ## WADENA             0.0446047001
    ## WASECA            -0.1310090674
    ## WASHINGTON        -0.0213916091
    ## WATONWAN           0.1075370170
    ## WILKIN             0.0212238466
    ## WINONA            -0.0613104677
    ## WRIGHT             0.0785885364
    ## YELLOW MEDICINE   -0.0368282910

The function `coef()` will return the county coefficients. Here are the
first six:

``` r
head(coef(ppfit)$county)
```

    ##           (Intercept)    floor_x      logu
    ## AITKIN       1.446360 -0.6664446 0.7232005
    ## ANOKA        1.477562 -0.6664446 0.7232005
    ## BECKER       1.475294 -0.6664446 0.7232005
    ## BELTRAMI     1.564351 -0.6664446 0.7232005
    ## BENTON       1.471756 -0.6664446 0.7232005
    ## BIG STONE    1.441176 -0.6664446 0.7232005

The first coefficient column here (`(Intercept)`) is the sum of the
overall intercept
![\\beta\_0](https://latex.codecogs.com/png.latex?%5Cbeta_0 "\\beta_0")
and the county random effects. That is:

``` r
head( fixef(ppfit)[1] + ranef(ppfit)$county )
```

    ##           (Intercept)
    ## AITKIN       1.446360
    ## ANOKA        1.477562
    ## BECKER       1.475294
    ## BELTRAMI     1.564351
    ## BENTON       1.471756
    ## BIG STONE    1.441176

We will use `coef()` next to form a dataframe for plotting.

Plot the fitted model (G\&H Fig. 12.5) for 8 selected counties:

``` r
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
```

![](mlvl03_multilevel_models_3_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

These estimates are not very different from the analysis that did not
include uranium as a predictor (see Part II). The difference is that the
intercepts in each panel are now predicted by the uranium level in a
county.

Plot the estimated intercepts (G\&H Fig. 12.6):

``` r
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
```

![](mlvl03_multilevel_models_3_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

This plot is the same as in Fig. 12.6 of G\&H, except that their plot
appears to be for the radon level on the first floor rather than the
basement (thus their plot is not strictly for the intercepts as labelled
on the y-axis).

#### Bayesian fit of multilevel model

The Bayesian fit is straightforward but may take a minute or two.

``` r
ppfit_bayes <- stan_lmer( log_radon ~ floor_x + logu + (1|county), data=radon_dat )
```

Print a selection of columns from the summary

``` r
print(summary(ppfit_bayes)[,c("mean","sd","n_eff","Rhat")],digits=3)
```

    ##                                              mean     sd n_eff  Rhat
    ## (Intercept)                              1.46e+00 0.0378  2143 1.002
    ## floor_x                                 -6.68e-01 0.0683  5357 1.000
    ## logu                                     7.27e-01 0.0938  2764 1.000
    ## b[(Intercept) county:AITKIN]            -2.36e-02 0.1493  5334 1.000
    ## b[(Intercept) county:ANOKA]              1.35e-02 0.1028  3236 1.000
    ## b[(Intercept) county:BECKER]             1.45e-02 0.1522  5439 1.000
    ## b[(Intercept) county:BELTRAMI]           1.15e-01 0.1483  3296 1.001
    ## b[(Intercept) county:BENTON]             1.10e-02 0.1456  5412 1.000
    ## b[(Intercept) county:BIG_STONE]         -2.96e-02 0.1504  4825 1.001
    ## b[(Intercept) county:BLUE_EARTH]         1.23e-01 0.1298  3018 1.003
    ## b[(Intercept) county:BROWN]              4.60e-02 0.1507  4435 1.001
    ## b[(Intercept) county:CARLTON]           -6.64e-02 0.1332  5351 1.000
    ## b[(Intercept) county:CARVER]            -3.45e-04 0.1423  5645 0.999
    ## b[(Intercept) county:CASS]               7.06e-02 0.1548  4602 1.000
    ## b[(Intercept) county:CHIPPEWA]           1.02e-02 0.1514  5596 0.999
    ## b[(Intercept) county:CHISAGO]            2.36e-02 0.1430  5351 1.000
    ## b[(Intercept) county:CLAY]               9.77e-02 0.1264  3624 1.000
    ## b[(Intercept) county:CLEARWATER]        -1.60e-02 0.1485  5275 0.999
    ## b[(Intercept) county:COOK]              -3.49e-02 0.1542  5399 1.000
    ## b[(Intercept) county:COTTONWOOD]        -7.46e-02 0.1556  4900 1.000
    ## b[(Intercept) county:CROW_WING]          3.38e-02 0.1307  5544 1.000
    ## b[(Intercept) county:DAKOTA]            -7.99e-02 0.0846  3992 1.000
    ## b[(Intercept) county:DODGE]              1.79e-02 0.1527  5320 0.999
    ## b[(Intercept) county:DOUGLAS]            4.06e-02 0.1329  4420 0.999
    ## b[(Intercept) county:FARIBAULT]         -2.26e-01 0.1786  1748 1.001
    ## b[(Intercept) county:FILLMORE]          -3.33e-02 0.1606  5342 0.999
    ## b[(Intercept) county:FREEBORN]           1.29e-01 0.1436  3591 1.000
    ## b[(Intercept) county:GOODHUE]            1.24e-01 0.1279  3639 1.000
    ## b[(Intercept) county:HENNEPIN]          -3.25e-02 0.0711  3656 1.001
    ## b[(Intercept) county:HOUSTON]           -1.76e-02 0.1436  5283 1.000
    ## b[(Intercept) county:HUBBARD]            5.11e-03 0.1428  5571 1.000
    ## b[(Intercept) county:ISANTI]             1.67e-02 0.1543  4686 1.000
    ## b[(Intercept) county:ITASCA]            -2.10e-02 0.1327  5709 0.999
    ## b[(Intercept) county:JACKSON]            5.89e-02 0.1455  5256 1.000
    ## b[(Intercept) county:KANABEC]           -2.75e-02 0.1491  5482 0.999
    ## b[(Intercept) county:KANDIYOHI]          7.96e-02 0.1592  4101 1.001
    ## b[(Intercept) county:KITTSON]            1.06e-02 0.1458  5046 1.000
    ## b[(Intercept) county:KOOCHICHING]       -1.01e-02 0.1435  5573 1.001
    ## b[(Intercept) county:LAC_QUI_PARLE]      1.08e-01 0.1658  3299 0.999
    ## b[(Intercept) county:LAKE]              -1.60e-01 0.1543  2700 1.001
    ## b[(Intercept) county:LAKE_OF_THE_WOODS]  1.34e-01 0.1618  3567 1.000
    ## b[(Intercept) county:LE_SUEUR]           2.04e-02 0.1421  5187 1.000
    ## b[(Intercept) county:LINCOLN]            7.85e-02 0.1555  3777 1.000
    ## b[(Intercept) county:LYON]               5.41e-02 0.1385  5379 0.999
    ## b[(Intercept) county:MAHNOMEN]          -1.00e-02 0.1574  4642 1.000
    ## b[(Intercept) county:MARSHALL]           2.32e-02 0.1364  5234 1.000
    ## b[(Intercept) county:MARTIN]            -1.31e-01 0.1545  2977 1.001
    ## b[(Intercept) county:MCLEOD]            -1.02e-01 0.1299  3761 1.000
    ## b[(Intercept) county:MEEKER]            -4.96e-02 0.1486  5206 1.001
    ## b[(Intercept) county:MILLE_LACS]        -3.99e-02 0.1593  4187 0.999
    ## b[(Intercept) county:MORRISON]          -7.34e-02 0.1358  4623 0.999
    ## b[(Intercept) county:MOWER]              1.37e-02 0.1272  5664 1.000
    ## b[(Intercept) county:MURRAY]             3.43e-02 0.1576  4819 0.999
    ## b[(Intercept) county:NICOLLET]           8.21e-02 0.1537  3581 1.000
    ## b[(Intercept) county:NOBLES]             1.76e-02 0.1514  5365 0.999
    ## b[(Intercept) county:NORMAN]            -5.32e-02 0.1472  4682 1.000
    ## b[(Intercept) county:OLMSTED]           -1.64e-01 0.1272  2956 1.002
    ## b[(Intercept) county:OTTER_TAIL]         7.19e-02 0.1412  4768 1.000
    ## b[(Intercept) county:PENNINGTON]        -4.13e-02 0.1519  4928 1.000
    ## b[(Intercept) county:PINE]              -1.26e-01 0.1567  3033 1.000
    ## b[(Intercept) county:PIPESTONE]          5.21e-03 0.1481  5210 1.001
    ## b[(Intercept) county:POLK]               6.37e-03 0.1477  4659 1.000
    ## b[(Intercept) county:POPE]              -3.24e-02 0.1550  5561 1.000
    ## b[(Intercept) county:RAMSEY]            -4.44e-03 0.1039  4707 1.000
    ## b[(Intercept) county:REDWOOD]            4.15e-02 0.1426  5746 1.000
    ## b[(Intercept) county:RENVILLE]          -9.41e-03 0.1475  5092 1.000
    ## b[(Intercept) county:RICE]               7.51e-02 0.1311  4751 0.999
    ## b[(Intercept) county:ROCK]              -5.11e-02 0.1619  4596 1.000
    ## b[(Intercept) county:ROSEAU]             1.17e-01 0.1359  3477 1.001
    ## b[(Intercept) county:SCOTT]              9.66e-02 0.1287  4240 1.000
    ## b[(Intercept) county:SHERBURNE]          3.01e-02 0.1355  4397 1.001
    ## b[(Intercept) county:SIBLEY]            -6.28e-02 0.1556  3865 1.000
    ## b[(Intercept) county:ST_LOUIS]          -2.09e-01 0.0852  1955 1.002
    ## b[(Intercept) county:STEARNS]           -3.15e-02 0.1081  4892 1.000
    ## b[(Intercept) county:STEELE]            -2.54e-02 0.1314  5984 0.999
    ## b[(Intercept) county:STEVENS]            2.54e-03 0.1612  5412 1.000
    ## b[(Intercept) county:SWIFT]             -1.08e-01 0.1575  3287 1.000
    ## b[(Intercept) county:TODD]               3.19e-02 0.1554  5851 0.999
    ## b[(Intercept) county:TRAVERSE]           2.55e-02 0.1454  5120 1.000
    ## b[(Intercept) county:WABASHA]            5.32e-02 0.1408  5034 1.000
    ## b[(Intercept) county:WADENA]             5.37e-02 0.1468  3981 1.000
    ## b[(Intercept) county:WASECA]            -1.57e-01 0.1673  2236 1.001
    ## b[(Intercept) county:WASHINGTON]        -2.51e-02 0.0935  4453 1.000
    ## b[(Intercept) county:WATONWAN]           1.32e-01 0.1675  2933 1.000
    ## b[(Intercept) county:WILKIN]             2.29e-02 0.1609  5083 1.001
    ## b[(Intercept) county:WINONA]            -6.99e-02 0.1311  4603 1.000
    ## b[(Intercept) county:WRIGHT]             8.30e-02 0.1298  4619 0.999
    ## b[(Intercept) county:YELLOW_MEDICINE]   -4.60e-02 0.1587  4879 1.000
    ## sigma                                    7.59e-01 0.0186  3262 1.000
    ## Sigma[county:(Intercept),(Intercept)]    2.74e-02 0.0166   960 1.004
    ## mean_PPD                                 1.22e+00 0.0348  5172 1.000
    ## log-posterior                           -1.18e+03 9.3602   739 1.003

We see that the `Rhat`s are all close to 1, suggesting the chains have
converged, and the effective number of replicates `n_eff` is high or
maximal. The parameter estimates are very similar to the maximum
likelihood fit. The parameterization is important though: `sigma` with a
small “s” is the standard deviation at the lowest scale, whereas `Sigma`
with a big “S” is the variance at the county scale. You need to square
`sigma` to obtain the variance and compare with the residual variance in
the frequentist analysis, whereas `Sigma` is directly comparable with
the county variance in the frequentist analysis.

Diagnostics: inspect convergence, histograms for posteriors etc

``` r
launch_shinystan(ppfit_bayes)
```

The diagnostics look good. Trace plots show convergence. Histograms of
parameters are all fairly smooth and symmetric. The one exception is the
county `Sigma`, which has a longer right-tailed distribution. That’s
because `Sigma` is a variance, which typically have such a distribution.

Extract posterior samples:

``` r
samples <- extract(ppfit_bayes$stanfit)
names(samples)
```

    ## [1] "alpha"    "beta"     "b"        "aux"      "theta_L"  "mean_PPD" "lp__"

``` r
str(samples$alpha) #Samples of overall mean. Matrix: samples by row, 1 col
```

    ##  num [1:4000, 1] 1.44 1.47 1.47 1.51 1.49 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ iterations: NULL
    ##   ..$           : NULL

``` r
str(samples$b) #Samples of county deviations. Matrix: samples by row, 86 cols
```

    ##  num [1:4000, 1:86] -0.0705 -0.1191 -0.0998 -0.141 -0.0171 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ iterations: NULL
    ##   ..$           : NULL

I’m still not sure what the 86th b parameter is but the first 85 are the
county samples.

``` r
str(samples$beta) #Samples of $\beta$s. Matrix: samples by row, 2 cols.
```

    ##  num [1:4000, 1:2] -0.722 -0.604 -0.773 -0.686 -0.805 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ iterations: NULL
    ##   ..$           : NULL

County means in basements are alpha + b +
![\\beta\_2](https://latex.codecogs.com/png.latex?%5Cbeta_2 "\\beta_2")
\* logu, so we’ll first derive posterior samples for the county mean and
then calculate summaries of their posterior distributions:

``` r
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
```

Plot of posterior means and standard deviations (and compare to the
maximum likelihood fit):

``` r
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
```

![](mlvl03_multilevel_models_3_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

Once again, the maximum likelihood and Bayesian model estimates are very
similar. However, the Bayesian model suggests slightly higher
uncertainty (larger standard errors) in the estimate of radon level in
each county (I think this is because the uncertainty in the variance
parameters is propagated to the posterior distribution for the county
means, whereas the variance parameters in the frequentist analysis, once
estimated, are assumed fixed).
