---
layout: post
title: Adding forgotten files to your commits
---

<p class="meta">12 June 2014 - Redwood City, CA</p>

#### To alter the last commit

Remember that `git commit --amend` will add forgotten changes to the *__last__*
commit in your logs. Don't do this if you've already pushed the commit somwhere,
unless you know what you're doing.

#### To alter a commit further back

Other notes that I've collected over the internet. If you are looking to change
a commit further back in history, look into `git rebase`. Guidelines from
stackoverflow which I'll repost:

1. Use `git rebase -i HEAD~10` (or whatever you need to see far enough back).
2. Mark the commit in question (`a0865...`) for edit.
3. Save the rebase file, and git will drop back to the shell and wait for you to fix that commit.
4. Add your file with `git add`.
5. Amend the commit with `git commit --amend`.
6. Do a `git rebase --continue` which will rewrite the rest of your commits against the new one.


#### To change the commit message for either of these

While this method is mainly for changing files and the commit messages will
default to the initial commit message, you can also change the commit message
explicitly by adding an `-m` flag: `git commit --amend -m "New commit message"`.

#### `git commit --amend`'s equivalent

According to the official docs, the ammend command is also the same as:

    git reset --soft HEAD^
    # do something else to come up with the right tree ...
    git commit -c ORIG_HEAD

So you can acheive the same results with these as well!
