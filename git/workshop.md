# Git Workshop

## Ammending Commits

### Git Reset (git reset --soft HEAD~3)
* soft - undo the commit, leave files staged
* mixed - undo the commit, unstage the files
* hard - undo the commit, delete the files

### Git Ammend (git commit --ammend)
* like reset --soft for just the last commit, but takes you to commit message

### Revert (git commit --revert)
* creates a new commit that undoes the last commit in order to not change
  history

### Getting ammended history back
1. git reflog
2. git checkout -b temp 2ab5c (from reflog)
3. git checkout master
4. git merge temp

## Looking For Commits

### Rev-Parse (git rev-parse ab12)
* tries to parse string to actual commit

## Stashing Changes

### Stashing temp contents
1. git stash - stashes wd changes
2. make other changes
3. git stash list
4. git stash pop / git stash apply (doesn't remove from stash stack)
