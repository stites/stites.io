---
layout: post
title: A Succinct look at Pseudo-Classical Selectors
---

{{ page.title }}
================

<p class="meta">08 Dec 2013 - San Fransisco, CA</p>

Tomorrow I'll be presenting to my peers about pseudo-classical selectors in CSS. I find this pretty amusing since I tend to avoid making my websites look pretty - however since starting to attend Daniel Chao's CSS classes in the after-hours of our sprints at HackReactor - I've started to find something pleasant about pretty websites. The takeaway so far has been: CSS is hard, voodoo, and mangled together in many instances - but if you put in just a few hours of attention into your CSS the benefits of a beautiful website can be amazing!

Here's a sneak peak at what I'll be presenting on. I should probably mention that the idea of presenting on this subject was David Hall's idea, and the remainder of this post is by him, plus some edits to fill in gaps.

## Pseudo-classical Selectors in CSS

Pseudo-classical selectors (which I'll refer to as PCSs in this post) are pretty common and you've probably seen/used them already. I've used `:active` and `:hover` before - back when I wasn't really into CSS at all. I assume that you've probably come across these guy as well, if not others. Essentially, pseudo classes are appended to your CSS selectors in your stylesheet. All PCSs are supported in IE9+, so that's a chunk of IE browsers that you don't have to worry about. PCSs look something like this:

    button {
    /* the normal stuff */
    }

    button:hover {
    /* the cool effects you want to add */
    /* with :hover as your PCS */
    }

A cool way to think about them is that they're kind of like event-handlers for your CSS. Although, in reality, PCSs are a mix of events and selections which you can narrow down. The

#### Legacy
These are the pseudo class selectors you encounter the most:

    :hover
change styles on an element when the use hovers over it. The most obvious use is anchors or buttons, but this can be used on most anything. I will build header bar nav's out of `ul`/`li`s and use `:hover` to change the color of the text.

    :active
change styles when an anchor or button is active (in use). I rarely use this one.

    :visited
How to style an anchor whose remote link you have already visited. Most useful when adding links that are straight text, to keep it from turning purple after being clicked. For instance `a:visited { text-decoration:none, color: #000 }`

    :focus
change styles when a field has focus - when you click inside an input form field, it then has focus. Change the color of the border, add an inset box shadow, etc, to let the user know they have the field selected. Speaking of which:

>     input[type="text"]

Is useful because, instead of adding a separate class to every field in your form, you can use use `input[type=""]` to target only specific input types in your form. For instance `text`, `textarea`, or `submit`.

###### Narrow down your selections:

    :nth-child()
Selects only the nth child of a given selector within the parent element. The most common types to put in the parens are even and odd, but you can also use some simple built in math to select every third, or whatnot. In essence what you enclose in the parens can handle simple math formulas using `n` as your variable. A quick example would be `li:nth-child(2n)` which would select all even instance, although `li:nth-child(even)` would work just as well. These formulas can be expanded to include every third:`li:nth-child(3n+3)` or a specific instance, like the fifth `li` tag: `li:nth-child(5)`. Here is a good article from CSS-tricks which is more in-depth on [how n-th child works](http://css-tricks.com/how-nth-child-works/).

Practically, I use this all the time to give styled variations in when elements are being repeated over and over. For instance, if I have an `ul` with 20 `li`'s in it listing some generic information from my database. I can use
>      li:nth-child(even) {  background-color #EEE }

to make the even occurrences have a different background color, for clear separation

    :nth-last-child()
Same as above, but works from the bottom up instead of the top down

    :first-child
Selects the first child element from your selection. In the above example about a list, I could use this to make the margin-top on the first element larger than the margin-top of all other elements, so there is extra offset on the top

    :last-child
Opposite of first-child

###### Style your paragraphs and text:

    :first-letter
Selects the first letter from the inner text of the element(s) selected. Make the first letter larger and bolder for emphasis, etc

    :first-line
Same as first-letter, but affects the first line of text different from all proceeding

###### Style your form fields:

    :checked
change the styles of all checked checkboxes. Like if you made a to-do list, and wanted checked items to have a strikethrough style

    :disabled
Only affects disabled form fields, buttons etc. I disable buttons all the type based on the condition of the DOM, or of a given backbone model's contents, or whatever. By default things just get grayed out, or have less opacity.

    :enabled
Opposite of above

###### Other useful selectors:

    :selection
change the color of the highlighting around text you have selected with your mouse. This is actually less useful to me than this cool little hack I found:
>     .unselectable {
>       -moz-user-select: none;
>       -webkit-user-select: none;
>       -ms-user-select: none;
>     }

Add this to your main styles. Now any element you give a class of `unselectable` to can't be selected with a mouse. SUPER useful when your nav contains text, and you dont want users to be selecting the nav text when a click on a nav item turns into a double click selection.

    :empty
Styles any element of the given selector with NO children or text. Useful when you are pulling information from an unknown source, like a database, and you might possibly get zero results. Instead of having an element appear with nothing in it, just set `element:empty { display:none }`

    :not()
Selects every one of your selector elements which does not match what you put in the parens. So `p:not(.dont-style-me)` would select all `p` tags that do not have the class `.dont-style-me`

    :before & :after
These two are super powerful and also super confusing. They insert styles before and after your selected element. Twitter bootstrap uses these extensivly to force styling on your page. I have used them to great effect in a number of cases. One example: I had block quotes from a client, and wanted large quote marks before and after the text. I used `:before` and `:after` to place the quote marks, and style/position them separate from the text itself, without needing to add new HTML elements for the quotes.

###### Chaining
Just as a note, pseudo classes can be chained, if you need more than one for a particular style definition.

###### Other useful shit

    !important
Use this to force a style to take precedence. Usually CSS styles are given precedence in the order they appear down the line. Styles that appear later overwrite earlier ones, and inline styles generally trump all. If you have a style that NEEDS to be the one that wins, use !important next to it. Its sort of hacky, and can cause problems later, but is good when you need to get shit fixed fast.

    box-shadow: inset
Most people know about the box-show attribute in CSS, to give a drop shadow to an element, but fewer people know that you can prepend the style with inset to make the box shadow appear inside the element instead of outside it. Use this to make your form text fields look depressed when clicked. Its awesome.

    ::-webkit-input-placeholder & ::-moz-placeholder
These can change the style of your form field placeholder text. I use it in conjunction with :focus to make placeholder text disapear when you click on a field.

**CSS Media Queries**

    @media all and (max-width: 1200px) and (min-width: 991px)
Add these at the bottom of your styles page to change how the CSS is rendered as your page gets narrower. This is the best way to handle responsive design. You will need mutiple of these to handle all the different situations, going from the largest to smallest in `px` order. Check out: [CSS Tricks - css media queries](http://css-tricks.com/css-media-queries/)