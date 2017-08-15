Machine learning review
==========================

Parametric
----------

Parametric ML algorithms are ones in which the number of parameters are
independent of the number of training examples. First you select a form for the
function, then you learn coefficients from the data. Think polynomials.

These are usually thought of as linear machine learning algorithms, however not
all are "linear." Examples include logistic regression, linear discriminant
analysis (LDA), and perceptrons.

Parametric ML is usually simpler, easier to understand, fast to learn, and
require less data. That said, they are constrained by their form, limited
complexity, and usually do not result in a good fit.

Nonparametric
-------------

This class of algorithm makes few/weak assumptions about the final form of the
function. This means they are free to learn the form from the data. This is most
useful when you have a lot of data, little-to-no prior knowledge, and when you
don't want to worry about choosing the correct features.

Examples include decision trees (CART, C4.5), naive bayes, SVMs, and neural
nets. Flexibility is their primary strength and allows them to make accurate
predictions based on few assumptions. As a result, however, this type of
algorithm requires more data, is slower to train, may be difficult to explain,
and has a higher chance of overfitting the data - as a result of the algorithm
being the thing in control of your function's final form.

