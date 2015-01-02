These questions are really made to be loose evaluations to understand how much
a candidate understands about the holistics of the d3 library. They're by no
means a hard determination of the candidate's ablilty to make beautiful
visualizations: I would argue that also requires a good deal of CSS knowledge or
the ablilty to snipe sections of stying from other d3 libraries. The main
purpose of these questions is to differentiates how much a candidate
copy-pastes code (there is a lot of that in the d3 world - it's how everyone
starts and you can still do a decent job with only that), and how much they can
go it alone. Most of these answers are opinion based and the most important
part is that they can answer them with a sound argument.

#### If you could compare d3 to another library what would that library be?
> "Readers familiar with other DOM frameworks such as jQuery or Prototype
> should immediately recognize similarities with D3." - [d3][prop]

#### When would you want to use d3 over other libraries?

> "D3 is not a monolithic framework that seeks to provide every conceivable
> feature. Instead, D3 solves the crux of the problem: efficient manipulation
> of documents based on data." - [d3][intro]

> "You can easily see that there is a trade-off between ease of use [not d3] and
> freedom [d3]. In the first case, you are stuck with a black box, in the
> second, with very complicated code." - [datameer][datameer]

You are looking for the explaination of the tradeoff, primarily.

#### What kind of other libraries have you used, or have heard of, for dataviz?

Looking for nvd3, or highcharts. Highcharts is jquery based but more robust,
nvd3 is a lot more specific to d3 - but is a lot newer (they've had no updates
since they are trying to restructure). There are more but, I think, these are
the other competing libraries - at least one of them should be mentioned. There
are a bunch [listed on wikipedia][wiki].

#### What is the general workflow of creating an svg element in d3?
You have to `enter` a DOM element (which can be existing or not-existing), then
attach data to it (with the `data` method), then attach the desired DOM element
to that (which is real this time). Then `exit`. [d3's intro section on this][ee]
and [mbostock's blogpost in more depth][mbostock].

#### Do you have a favorite visualization?
[There are a ton here](http://bl.ocks.org/mbostock) all from mbostock, the
creator of d3 (now working at the NYTimes). This is a fun question and begs the
followup: _can you explain that visualization to me?_ Since data viz is all
about explaining complexities - this is important.

#### Ask about their front-end experience.
d3 is, in the end, just another front end tool and you will probably make better
visualizations if you understand how to style and manipulate the DOM.

#### if they use angular: why is angular and d3 such a big deal?
Because d3 manipulates the DOM, and angular is supposed to manage all of that.
Thus d3, to properly update and interacte with angular, needs to be "injected"
into the digest cycle (with a factory or service) and a directive needs to be
writted to display the visualization. There are libraries that do the injecting,
but the directives are all custom - since d3 is used to build custom things.


[datameer]: http://www.datameer.com/blog/uncategorized/whats-behind-our-business-infographics-designer-d3-js-of-course-2.html
[prop]: http://d3js.org/#properties
[intro]: http://d3js.org/#introduction
[ee]: http://d3js.org/#enter-exit
[mbostock]:http://bost.ocks.org/mike/selection/
[wiki]: https://en.wikipedia.org/wiki/D3.js
