# Stan
[Stan](https://mc-stan.org/) is a platform for Bayesian statistical modeling. Official R packages from the Stan project include `rstan` and `rstanarm`.

## Install rstan
[RStan](https://mc-stan.org/users/interfaces/rstan.html) is a standalone installation of Stan as an R package (i.e. you don't need to separately install Stan). Follow instructions at https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started under the sections:
*  Installation of RStan
*  Checking the C++ Toolchain
*  Configuration of the C++ Toolchain

Test your installation works by following the instructions in the section **How to Use RStan** up to and including **Example 1: Eight Schools**.

Don't worry about understanding the code that is saved to the file `8schools.stan` and don't be intimidated by it. This is Stan's underlying code language, an advanced option that you might get into later. You can start off by using higher level interfaces (like `rstanarm`, see below) that abstract this away.

You might need to wait 1 min or so for the `stan` call to compile. There might be some error messages (probably all harmless). In particular, you could get a warning message involving CXX11 or that there were divergent transitions. These are nothing to worry about. If all is well, after `print(fit)` you should see output like this (your exact numbers may vary due to stochastic simulation but they should be very similar).
```
Inference for Stan model: 8schools.
4 chains, each with iter=1000; warmup=500; thin=1;
post-warmup draws per chain=500, total post-warmup draws=2000.

           mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
mu         7.67    0.15 4.96  -2.90   4.55   7.78  10.92  17.11  1142    1
tau        6.53    0.20 5.47   0.32   2.55   5.19   9.00  20.32   755    1
eta[1]     0.38    0.02 0.93  -1.47  -0.25   0.40   1.03   2.15  2000    1
eta[2]     0.02    0.02 0.87  -1.76  -0.55   0.03   0.61   1.74  2000    1
eta[3]    -0.17    0.02 0.94  -1.96  -0.80  -0.17   0.42   1.70  2000    1
eta[4]     0.00    0.02 0.94  -1.95  -0.57  -0.01   0.60   1.98  2000    1
eta[5]    -0.31    0.02 0.88  -1.88  -0.92  -0.36   0.25   1.55  1738    1
eta[6]    -0.22    0.02 0.86  -1.95  -0.79  -0.20   0.36   1.40  2000    1
eta[7]     0.37    0.02 0.87  -1.32  -0.18   0.40   0.92   2.02  2000    1
eta[8]     0.07    0.02 0.92  -1.78  -0.54   0.06   0.68   1.92  2000    1
theta[1]  11.11    0.19 8.01  -1.82   5.81  10.09  15.06  30.80  1719    1
theta[2]   7.77    0.14 6.20  -4.23   3.95   7.78  11.49  20.92  2000    1
theta[3]   6.16    0.17 7.52 -10.37   2.03   6.57  11.06  20.35  2000    1
theta[4]   7.68    0.15 6.72  -5.93   3.49   7.75  11.69  21.27  2000    1
theta[5]   5.10    0.14 6.21  -9.10   1.66   5.66   9.49  15.66  2000    1
theta[6]   6.06    0.15 6.64  -7.58   2.15   6.38  10.19  18.50  2000    1
theta[7]  10.65    0.15 6.68  -1.39   6.32  10.03  14.46  25.74  2000    1
theta[8]   8.31    0.18 7.88  -8.13   4.08   8.22  12.41  24.51  1947    1
lp__     -39.50    0.10 2.60 -45.09 -41.09 -39.31 -37.71 -34.98   719    1

Samples were drawn using NUTS(diag_e) at Thu Sep 20 07:25:36 2018.
For each parameter, n_eff is a crude measure of effective sample size,
and Rhat is the potential scale reduction factor on split chains (at
convergence, Rhat=1).
```

## Install rstanarm
This is an R package. Install from CRAN in the usual way. `rstanarm` stands for "RStan for Applied Regression Modeling". It wraps `rstan` in such a way that models are expressed just as they are in the corresponding frequentist tools lm(), glm(), glmer() etc. It is especially handy for multilevel linear models (aka linear mixed models and generalized linear mixed models).
