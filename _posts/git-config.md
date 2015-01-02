### useful git configs
rerere.autoupdate
rerere.enabled
core.eol = 'lf' defaults to 'native'
core.whitespace  treats these as errors:
  defualted: blank-at-eol,space-before-tab,
  not: tab-in-indent
       tabwidth=2    tells how many character positions a tab occupies; this is relevant for indent-with-non-tab and when Git fixes tab-in-indent errors.
       color.diff always
       color.branch always
       color.grep always
       color.interactive always
       color.showbranch always
       color.status always


color.ui always    -- gets the above
column.ui always
format.pretty
grep.lineNumber true If set to true, enable -n option by default.

grep.patternType perl
Set the default matching behavior. Using a value of basic, extended, fixed, or perl will enable the --basic-regexp, --extended-regexp, --fixed-strings, or --perl-regexp option accordingly, while the value default will return to the default matching behavior


defaulted to true:
core.preloadindex
core.quotepath: http://git.kaarsemaker.net/git/commit/9378c16135100fb65ad575cd35074af166de1cab/
