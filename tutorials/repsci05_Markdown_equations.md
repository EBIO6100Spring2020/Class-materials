# Equations in markdown and GitHub
You can write equations using latex syntax that will render beautifully in many markdown applications and websites that are [MathJax](https://www.mathjax.org/) configured. Unfortunately equations are painful in GitHub as it doesn't natively render them. As a workaround for GitHub markdown, add the following to your Rmarkdown:
```
---
output:
    github_document:
        pandoc_args: --webtex
---
```
This workaround substitutes pictures for equations. You need to have [pandoc](https://pandoc.org/installing.html) and a latex engine (e.g. [miktex](https://miktex.org/)) installed as well.

In markdown you need to surround equations with $ ... $ for inline equations (small symbols), or with $$ ... $$ for "display" style (separate paragraph, centered, large symbols).

For example, this:
```
$y_i \sim \mathrm{Normal}(\mu_i,\sigma)$
```
produces this:

$y_i \sim \mathrm{Normal}(\mu_i,\sigma)$

The above won't display an equation on GitHub (just the latex) - you have to view it in a markdown editor capable of showing equations.

The following is produced after knitting from Rmarkdown with the above YAML hack and displays an equation on the GitHub webpage:

![y\_i \\sim
\\mathrm{Normal}(\\mu\_i,\\sigma)](https://latex.codecogs.com/png.latex?y_i%20%5Csim%20%5Cmathrm%7BNormal%7D%28%5Cmu_i%2C%5Csigma%29
"y_i \\sim \\mathrm{Normal}(\\mu_i,\\sigma)")

A handy online GUI for making equations\
https://www.codecogs.com/latex/eqneditor.php

Good guide to syntax here:\
https://www.sharelatex.com/learn/Mathematical_expressions
