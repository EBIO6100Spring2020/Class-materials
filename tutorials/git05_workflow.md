# Git workflow for collaborating on forecasting projects

Here is a suggested Git workflow that will allow us all to work together on the same repository while minimizing conflicts between file versions. Feel free to improve or suggest other strategies!

* Use GitHub Issues to assign tasks
* Pull master from GitHub (to update your local repo)
* Make a new branch for a task in your local repo
* Work on the task and commit changes to your local task branch
* If no discussion needed
  * Pull master again from GitHub to update
  * Merge the new task branch into master
  * Push master to GitHub
* Else if discussion needed
  * Push task branch to GitHub
  * Submit pull request in GitHub
  * Discuss as necessary
  * Merge task branch into master

Good discussion of some data science and R centric workflows is https://happygitwithr.com/workflows-intro.html.
