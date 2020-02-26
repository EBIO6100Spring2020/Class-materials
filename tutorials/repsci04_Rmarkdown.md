# R markdown

## Github markdown from Rmarkdown
Github markdown is useful for displaying files nicely formatted on GitHub. If you are working with Rmarkdown, to output GitHub markdown, include the following at the beginning of the `.Rmd` file:
```r
---
output: github_document
---
```
This will produce a `.md` file and a folder of figures (as well as a `.html` file).

Then, in RStudio do:
* *File > Knit Document*

or just click the *Compile notebook* icon, or do *Ctrl-Shift-K*.
