# Git teamwork
Using Git and GitHub is transformative for teamwork in a coding project.

## Tutorials
### Codecademy
https://www.codecademy.com/pt-BR/learn/learn-git
  * Git teamwork

A summary of the git commands covered (text from their tutorials):\
`git clone` Creates a local copy of a remote.\
`git remote -v` Lists a Git project's remotes.\
`git fetch` Fetches work from the remote into the local copy.\
`git merge origin/master` Merges origin/master into your local branch.\
`git push origin <branch_name>` Pushes a local branch to the origin remote.

### Software carpentry
https://swcarpentry.github.io/git-novice/
* Remotes in GitHub
* Collaborating
* Conflicts

### Try with GitHub
Try doing some of the things in the tutorials with a test remote repo on GitHub (in your personal GitHub area, not in the class Organization area).

For example, practice resolving a conflict on local and remote branches. First set up a conflict:
  * initiate a repo on GitHub with a `README.md` file
  * clone the repo to your computer and make some changes to `README.md` without pushing
  * use the web interface of GitHub to change `README.md` directly on the website or to create a new file, or upload a file through the GitHub website interface

Now changes have been made to the repo in two places (origin/master, which is on GitHub, and master, which is on your computer) and if you try to push from your computer, it will fail. You'll need to pull first and reconcile merge conflicts (`git pull` is a combination of `git fetch` and `git merge origin/master`).

## Further reading
[Pro Git Chapter 5 Distributed Git](https://git-scm.com/book/en/v2/Distributed-Git-Distributed-Workflows) is really good for the concepts and various ways to go about teamwork. We will probably mostly use the "centralized workflow".
