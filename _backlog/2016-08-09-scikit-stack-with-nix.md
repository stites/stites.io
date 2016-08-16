Build your own Nix Developer Environment: Python ML style
=========================================================

let's build a scikit stack with nix development environments. Why would we do this?

+ nix is going to be lighter than vagrant any day of the week.
+ the usual arguments for nix with respect to the fact that it is a pure-functional package manager
+ nix development environments seem much more powerful than vagrant since they can be packaged and
  shipped

There's a problem, however, since most current resources out there don't include the stack that I
usually like to work with (detailed below) — or else I just haven't been able to find a simple
out-of-the-box solution to my problem. On top of this little hitch, I've never built my own nix
development environment. Hopefully by blogging as I go, I might be able to help someone else later
down the road.

### Packages in my data science stack

To start, I'll need:
+ python 2.7+
+ numpy
+ pandas
+ jupyter (aka ipython)
+ matplotlib
+ scikit-learn

Normally, this would be a `sudo pip install numpy pandas matplotlib jupyter scikit-learn`. Now we
must start with a script:

```
```


