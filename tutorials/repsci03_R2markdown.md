# Automatic reports - option 1: Markdown from R scripts
Did you know that you can produce markdown from ordinary R code? To output GitHub markdown, include the following at the beginning of the `.R` file:
```r
#' ---
#' output: github_document
#' ---
```
Then, in RStudio do:
* *File > Knit Document*

or just click the *Compile notebook* icon, or do *Ctrl-Shift-K*.

This will produce a `.md` file and a folder of figures. If you want to get fancier you can have more control over the look of this report by using the commenting symbol `#'` in place of the usual `#` symbol. Any text after the `#'` symbol will produce report text while any text after the `#` symbol will produce code comments. You can also control code chunks using `#+` to provide [chunking options](https://yihui.name/knitr/options/) as in Rmarkdown. See the [Render an R script](http://happygitwithr.com/r-test-drive.html) topic in Happy Git with R.

This can be a great way to work that preserves the workflow as an ordinary R script.
