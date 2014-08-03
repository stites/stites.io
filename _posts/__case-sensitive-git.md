By default - at least on macs - git is case insensitive, but case preserving. Meaning that the first time you write a file is how that file will live for the duration of its life in the git tree.

It's supposed to help you, but I've never used any of this to my benefit. Arguably, case-sensitivity and that kind of granular attention-to-detail should be in the nature of software development.

You can override the defaults once with a force command: `git mv --force file File`, or hack around this issue by moving the file to a temporary file first, so that git picks up on the change: `git mv file temp` followed by `git mv temp File`.

If you're like me and need to have this fixed because you are changing capitalization of files in git all over the place as you tweak your file structure, just turn off this feature glabally with

    git config --global core.ignorecase false

Take a look at [git's config man-docs][git] as well to add some personal customization and automation to your workflow.

( Also consider adding the alias of `ignore = update-index --assume-unchanged` and its counter, `no-ignore = update-index --no-assume-unchanged` )

[git]: http://git-scm.com/book/en/Customizing-Git-Git-Configuration
