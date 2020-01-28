# Git resources
A suggested tutorial sequence follows:
* git01_setup.md
* git02_basics.md
* git03_branching.md
* git04_teamwork.md
* git05_workflow.md

Here we list the resources used in those tutorials.

## Happy Git with R
Jenny Bryan's **Happy Git with R** (https://happygitwithr.com/) is a great resource for getting started and for troubleshooting. It has particularly good advice for data science workflows, which are different from software development workflows (which are the target of most tutorials), and specifically for R and R markdown.

## The Pro Git book
**Pro Git** (https://git-scm.com/book/en/v2) by Chacon & Straub is the official Git book. It's very clearly written, practical, and an excellent resource. Return to it often.

## Codecademy tutorials
Codecademy (https://www.codecademy.com/pt-BR/learn/learn-git) is a good place to start for tutorials (recommended by Hadley Wickham, which is something I guess - I found it to be good but had some bugs - see below for tips). You need to make a free account. I just used an alias and throwaway email address. The first lesson in each of the major topics is a good overview (and you get free access to more practice for 7 days if you want more practice):
  * Basic Git Workflow
  * How to Backtrack in Git
  * Git branching
  * Git teamwork

### Bugs in Codecadamy tutorials
A few times I found their instructions for what to do next were wrong, such as
```
cd my-quizzes
```
You might find that you are in the wrong place and first need to move up a directory level using
```
cd ..
```
then, to check where you are
```
ls
```
then
```
cd my-quizzes
```
Another time the instructions said "You are on your local master branch" but in fact by then we were on another branch and their instructions failed. You can tell which branch you are on with
```
git branch
```
and you might need to switch branches first with
```
git checkout master
```
Also, in the bash window you might need to type q sometimes to get back to the bash prompt $.

## Software carpentry
You can get another take on Git-GitHub from software carpentry at https://swcarpentry.github.io/git-novice/. I found this to be not as polished as Codecademy but it's pretty good. The relevant topics are:
* Tracking changes
* Exploring history
* Remotes in GitHub
* Collaborating
* Conflicts

## Hadley Wickham's RStudio guide
Hadley Wickham's guide to Rstudio with Git and GitHub http://r-pkgs.had.co.nz/git.html is especially good for the Git tools built into RStudio. It is written from the perspective of R package development but approaches discussed are essentially the same for data science projects.

## Earth Lab tutorials
Here are two lessons from Earth Analytics' git/github tutorials that could be useful for some. They have user-friendly step-by-step instructions with visuals. They're for using Jupyter notebooks with Python, but the interaction with the command line is the same.
* [Version control](https://www.earthdatascience.org/courses/earth-analytics-bootcamp/git-github-version-control/guided-activity-version-control/)
* [Pull request](https://www.earthdatascience.org/courses/earth-analytics-bootcamp/git-github-version-control/guided-activity-pull-request/)
