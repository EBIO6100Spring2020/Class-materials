# Reproducible workflows
This a general introduction. We can add tutorials relevant to forecasting and open science.

For our forecasting projects, we want other scientists to be able to reproduce the forecast and understand what we did, ideally many years or decades later. Equally, we would hope to return to these analyses again and again, so we'll need to repeatedly reproduce whatever we do. Getting to an exactly reproducible analysis can be a challenge because technology and software changes over time, many modern computational approaches are stochastic, or complex computing infrastructure might be needed. But there are seven simple principles that get us most of the way there.
1. Document everything.
2. Do everything using code.
3. Use open source file formats.
4. Organize files in one location.
5. Track changes to files.
6. Archive final working versions.
7. Backup all files.

## 1. Document everything
The role of documentation is obvious but it is perhaps the hardest to stay on top of. Documentation takes time and effort and is easy to neglect. Furthermore, it is easy to overlook the need for documentation when we're "in the zone" of a project because then it might seem obvious what we are doing and why we are doing it that way. A good rule of thumb is to document the obvious. The main goal is to include enough documentation in our code and descriptive files that what we did and what we were thinking is clear.

Documentation includes metadata. A `readme.txt` or `about.txt` file (plain text `.txt` or markdown `.md` are good choices for file format) is typically the master metadata file and contains a guide to the project, file structure, and other metadata file locations. The main job of metadata is to document the provenance of data and especially to describe in detail each column or attribute of a data file, such as collection methods, definitions, units, precision, and taxonomic information. There are several templates for metadata. One useful one is Ecological Markup Language (EML, knb.ecoinformatics.org/tools/eml). EML is based on and compatible with the Dublin Core metadata specification (dublincore.org) and includes open source software to help generate metadata. We might encounter other formats too.

## 2. Do everything using code
Already, simply by writing code for the analysis, we are most of the way there. Code automatically documents the steps in data manipulation and analysis. It doesn't matter what language the code is or whether we use several languages in the same project. The key is to avoid doing things "by hand" as much as possible. Avoid menu-driven operations. Avoid cut and paste. Avoid intermediate steps that require hand-editing files. However, be sensible and practical. If something needs to be done by hand, document the steps in enough detail to reproduce them exactly.

### Acquiring data
A reproducible workflow starts with data entry and preparation. For forecasting, we are mostly acquiring existing datasets prepared by other people. Therefore, we want to make some effort to understand how the data were collected and prepared. For NEON, consult the documentation. Consider downloading data using code rather than downloading by hand.

### Preparing data
Once datasets are acquired, don't modify them directly. Treat them as "read only". Use code to make derivative versions (such as subsetting, reordering rows or columns, renaming variables, creating new variables, etc). For example, code could include R or Python scripts or SQL queries. Analysis workflows that require intermediate data steps are sometimes called pipelines. Construct pipelines with code.

### Producing reports
In the context of forecasting, it can be a good idea to automatically generate documents that report the forecast, especially if our intent is to repeatedly update the forecast in the future.

### Portability and reproducibility of code
It is worth thinking about the portability and universality of our code. Is our code easily ported to another programming language? This is an aspect of reproducibility. Will a scientist 20 years in the future be able to understand our code? Consider that the code language we use now will likely not be popular in future. Yes, R will probably be dead! Will a scientist in another discipline be readily able to understand our code? Consider that the coding languages common in our discipline may not be common in others. Will someone who codes in another language be able to understand our code? For example, can we give our R code to a scientist who uses Matlab or C or Python so that they can readily understand or implement our forecasting algorithms in their language?

Such a notion of scientific portability and reproducibility leads to decisions about appropriate coding style for different situations. Software developers have strong opinions about coding style as applied to developing software. Considerations for scientists might be different. What are our priorities as scientists? For example, in R we can choose among at least three major [programming paradigms](https://en.wikipedia.org/wiki/Programming_paradigm): functional programming, procedural programming, or object-oriented programming. Each may be more appropriate in different situations. For example, a procedural programming style (e.g. `for`, `while`, `if`, `if/else`) is readily ported to all popular languages in science, so is a good candidate for universal communication of ideas and algorithms among scientists. Procedural programs automatically describe how a model operates, so this style can be a good choice for representing many types of natural process models. R is designed as a functional programming language and as such involves idioms and functions specific to R (e.g. `apply()`, `%>%`). In this way, it is less scientifically portable. On the other hand, this functional approach is incredibly useful and time saving in many data visualization (e.g. `ggplot()` for graphics) and analysis (e.g. `lm()` for linear models) tasks because, by design, a functional style focuses on what is to be accomplished (e.g. produce a plot) rather than how it is done. Fortunately, we can mix paradigms to most clearly communicate the science. A good principle for reproducible scientific code is that any scientist that does not code in our language can easily reproduce the science in another language. This is the same principle as being able to reproduce a result in a different lab - the method should be clear and not obscured in lab-specific jargon.

<!-- Rmarkdown pros and cons -->

## 3. Use open source file formats
Data and analyses should, to the greatest extent possible, use open source, cross-platform, file formats and software. Open source software has a better chance of being able to be used by anyone now and in the future. Open source software is transparent. Free and open source software (FOSS) like R and Python belongs to the community. Open source file formats, and text files in particular, ensure that files can always be opened in the future and across any operating system. Small datasets are best in standard text formats such as `.csv`, `.xml`, `.json` etc. Code of course is already text. Large and/or long term datasets are sometimes more conveniently managed in database systems - open source options include MySQL, MariaDB, sqlite, postgresql. For geographic data, qgis is open source.

## 4. Organize files in one location
For small projects, such as are typically associated with a single published paper, a single project directory can be sufficient. One directory per paper is a logical organization because the project archive can be self-contained along with the paper. It can then also be one Git repository. If using RStudio, the directory can also be an RStudio project.

It is hard to give rigid advice on the organization of files within this directory. Much depends on the project. For many projects, one directory containing individual files of data, analysis scripts, and results can be all that is needed. For more complex projects it can make sense to have a sub-directory structure. Here is one possibility among many:
* analysis scripts in the top-level directory with the following sub-directories
* source - for functions and other helper code that is sourced by scripts
* data_raw - for all raw data files
* data_derived - for any datasets derived (using code) from the raw data
* output - for output like tables and figures

Or scripts can be organized into sub-project sub-directories. Organizing files in one location also makes it easy to refer to files in scripts using relative rather than absolute paths. Use relative paths so that code will work from any location on any computer. Don't use absolute paths in scripts such as
```
C:/user/jane/janescoolstuff/experiment2/data/exp2dat.csv
```
This will break the script on a different user's computer. Instead, use relative paths, such as
```
data/exp2dat.csv
```
Anyone can then run the code without needing to modify the file paths. This is especially important when collaborating via a repository.

Two other R specific considerations for project organization:
1. Don't use `setwd()` in scripts. This is not portable and will break the script on another person's computer. If set up as an RStudio project, RStudio will be in the correct working directory when the project is opened.
2. Don't save the R workspace. Start clean each time. In Tools > Global options > General, set "save workspace" to "Never" and uncheck everything except "Automatically notify me of updates to RStudio". This ensures that all your work derives from code and provides a test of the code each time you work on the script.

Read Hadley Wickham's [chapter on project workflows in RStudio](http://r4ds.had.co.nz/workflow-projects.html) in *R for Data Science* for more R specific advice. His chapter concurs with the advice given above.

<!-- Might consider filenames with no spaces -->

## 5. Track changes to files
Version control is like "track changes" for code and plain text documents. It is a way to record the history of a project, particularly changes made to data and code. If necessary, code or data can be reverted back to previous time points. Git is the dominant version control tool, and is free and open source. Together with GitHub, Git facilitates collaboration and open science. See the [Git tutorials](https://github.com/EBIO6100Spring2020/Class-materials/blob/master/tutorials/git00_resources.md).

## 6. Archive final working versions.
By using Git, we'll be set to easily archive and/or publish final working versions of our forecasts. These can simply be a specific commit in our project history. There are a few  important archiving considerations.
* When stochastic analysis methods are used, such as bootstrapping, simulation, or MCMC, use the `set.seed()` function so that random number generation is repeatable.
* Record detailed version information for all software and packages. In R, this is especially easy to do using the function `sessionInfo()`. The R project via [CRAN](http://cran.stat.ucla.edu/) archives all versions of R and packages so that it is always possible to go back to old versions if that is needed to reproduce an analysis.
* If we use R packages from GitHub (rather than CRAN), these will not be archived by the R project. We'll need to include entire copies of these in the archive. Assume that any project on GitHub is ephemeral and could be removed or altered by the author at any time.

Nevertheless, the above still might not be sufficient. If we want to be paranoid, we could include a copy of R itself and/or a copy of all the packages we used. The R package `packrat` aims to help with this. More advanced still, we could virtualize or containerize the entire analysis environment in a virtual machine or using [docker](https://arxiv.org/abs/1410.0846) or [lxd](https://linuxcontainers.org/). This could be especially effective if the forecasting pipeline ties many different scripts and software together. Some open source solutions directed at containerized reproducibility are emerging in this area, such as [singularity](https://www.sylabs.io/). We can explore these possibilities if necessary.

## 7. Backups
Version control is not a backup and neither is pushing to GitHub. While these certainly help protect us from losing files and are very reliable, they are not a guarantee. For example, if a local Git repository is accidentally corrupted and then pushed to GitHub (or vice versa), the situation could become unrecoverable. Git is complicated and things can go wrong. Everyone should have a proper backup solution as well, such as time machine for OSX, the built in backups (file history) for Windows 10, or the many excellent open source options in Linux (e.g. rsnapshot, borgmatic). The 3-2-1 rule of backups is a good standard: 3 copies, 2 media, 1 offsite. In practice, this could be the original, a local backup to a separate disk or server, and a backup to the cloud. Note that a Dropbox or Google Drive mirror of your files is not a backup since any corrupted files will be mirrored to the cloud. Similarly, a RAID volume, while providing some nice redundancy, does not count as a backup by itself. Backups should be made **automatically** and **frequently** (at least daily) to all locations. Backups should include the hidden files in Git repositories. Backups should also be tested to make sure files can be restored as expected. Our work is not reproducible if our files are irreversibly corrupted or lost altogether. Fortunately, with multiple collaborators, there will be a lot of redundancy if everyone backs up their files.
