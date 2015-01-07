This is a [pretty long article](http://eisenbergeffect.bluespire.com/all-about-angular-2-0/) written by a former member of the Angular2.0 team. The counter at the bottom estimates an hour-long read, so I figure'd I'd write some (as objective as possible) sparknotes:

##### Motivations for Angular 2.0

> When AngularJS was first created, almost
> five years ago, it was not originally
> intended for developers.

+ The web is no longer a jQuery cesspool
+ 5 years ago (angular's creation) mobile was barely a thing
+ [Learning angular looks like this](http://www.bennadel.com/resources/uploads/2013/feelings_about_angularjs_over_time.png)

##### Features and Design of Angular 2.0

+ it uses AtScript (note that you don't have to write AtScript to use it)
+ AtScript does some typechecking
+ They're trying to unify AngularJS and AngularDart — which is still a
  hit-or-miss
+ Dependency Injection will be better
+ Not everything has to be a singleton
+ Child Injectors (new feature) will add inheritance to services
+ Dynamic Loading will really be asynchronous this time.
+ Directives will not use a "kitchen-sink" approach:

> In Angular 2.0 there are three types
> of directives.
>
> + _Component Directive_ - Creates a custom component composed of
>   a View and a Controller. You can use it as a custom HTML element.
>   Also, the router can map routes to Components.
> + _Decorator Directive_ - Decorates an existing HTML element with 
>   additional behavior. A classic example is ng-show.
> + _Template Directive_ - Transforms HTML into a reusable template.
>    The directive author can control when and how the template is
>    instantiated and inserted into the DOM. Examples include ng-if
>    and ng-repeat.

+ Controllers will be less of a dumping ground:

> You may have heard that Controllers are dead in Angular 2.0.
> Well, that's not exactly true. In reality, Controllers are
> one part of what we are calling a Component. The Component has
> a View and a Controller.

+ "Components" are just classes, so with the new dependency injection,
  this makes controllers look a lot cleaner with the inheritance
  from Child Injectors.

+ Templating will have more syntax and, thus, will be given more 
  responsibility.

__Pros:__
> There's some pretty nice things going on in there to keep a small memory
> footprint, reduce garbage and enable super fast template instantiation.

__Cons:__
Directives are different and "the annotations are a bit verbose." 
Also, they break the [Separated Presentation principle](https://en.wikipedia.
org/wiki/Separation_of_presentation_and_content).

Example:

    @ComponentDirective({
        selector:'tab-container',
        directives:[NgRepeat]
    })
    export class TabContainer {  
        constructor(panes:Query<Pane>) {
            this.panes = panes;
        }

        select(selectedPane:Pane) { ... }
    }

> Earlier on I mentioned that this was necessary for the compiler to know what
> needed to be loaded before compiling the template. But, this breaks one of 
> the primary benefits that is usually gained by using MVC, MVVM or any
> separated presentation pattern. Lest you think this is just theoretical, let
> me point out some of the consequences:
>
> + It is no longer possible to implement ng-include. The compiler requires a
>   ComponentDirective in order to compile HTML. Therefor you cannot compile
>   HTML on its own and include it into a View.
>
> + It's painful if you want to have multiple potential views for the same
>   component. Imagine that you have a component but you want to use a 
>   different view for phone than for desktop. You need to aggregate all the 
>   directives, filters, etc. that you use across all of your views and make
>   sure they are all represented in the single component's metadata. This is
>   a maintenance nightmare. You can no longer reliably remove anything from
>   the dependency list without checking all views. It's also easier to forget
>   adding  something.
>
> + It's not possible to have multiple runtime views for the same component.
>   Imagine that you are configuring your router with a set of routes. Several
>   of the routes can use the same "controller" but you need different views.
>   You can't really do that. Sorry.
>
> + It's completely impossible to enable ad hoc composition of screens. This
>   makes data-driven UI construction more complicated in the least. You can't
>   just render varying combinations of views and controllers (view models).
>   This limits reusability by discouraging compositional approaches to UI.
>   It forces you to subclass controllers in order to get different views.
>
> Fortunately, the design is still undergoing lots of changes.

The author has opinions on stuff and has voiced his opinion.

Templating Synax is still up in the air. This is the reason it was introduced, 
but there are technical problems and the community hates it. If you have 
recommendations, go to [the related issue][ngIssue].

The author has more opinions, which he states.

##### The most interesting part: two-way data binding might be canned!

> I don't know if you noticed it, but there isn't a single example of 
> two-way databinding in this entire article. In fact, none of the syntaxes 
> I've explained above include any way of specifying various binding options 
> such as directionality, triggers, debounce, etc. So, how do you bind to 
> an input element and push data back into your model? How do you bind to a 
> custom Web Component that needs to update your model?
>
> There is intense debate within the Angular team as to whether Angular 
> 2.0 needs two-way databinding or not. If you've read the public design 
> documents (including this one) or watched the ngEurope presentation on
> Angular 2.0 Core or the Q&A, you may have picked this up. I strongly
> support keeping two-way databinding. To me it seems that it's part of the
> soul of Angular. I have yet to see a proposal that provides an elegant 
> alternative and until I do I will continue to argue in favor of keeping 
> two-way databinding.
> 

>  You may wonder why this is even being considered.

>  I've heard some explanations related to enforcing DAG for data flow. This
> idea has been recently made popular by ReactJS. But frankly, you can't
> completely enforce that. I can break it by simply using an event aggregator.
> This is a pattern that is very common in composite applications. I think you
> should teach people about DAG and help them to adhere to it when possible,
> but you can't force them. That makes it hard to do their job.

> I've heard another argument that centers around inadequate validation 
> capabilities. But this isn't a reason to remove two-way binding. You can 
> easily layer validation systems on top of the low level two-way binding 
> capabilities.

> I think one of the big problems relates to the actual implementation of 
> binding in Angular which uses dirty checking. Since dirty checking is used, 
> every time you make a check, you have to check twice. The reason is that 
> if the first check results in a change, then those changes might 
> result in other changes as a side effect. So, you have to check a
> second time to be sure. Now, if there are changes after the second check,
> then you have to check a third time...and so on. This is what is referred to
> as model stabilization. Yeah, it's a pain for a dirty checked system. But
> removing two-way binding does not solve the problem. You also need to
> remove watches altogether so that no one can trigger arbitrary code based on > a change in an expression. That's pretty obvious which is why removal of
> watches is also being considered. But that still doesn't solve the problem 
> because an event aggregator can always get around that...and frankly 
> sometimes you need to. Databinding is a powerful tool and people do make 
> mistakes. But I think we can handle it. I know that's true for many of you.


> Maybe you disagree with me and you think "good riddance to two-way binding." 
> There are definitely people who have that opinion. However, I suspect that 
> most Angular, Durandal, Knockout, Ember, etc. users agree with me. 
> Fortunately, the team hasn't made up their mind on any of this. They are 
> just trying to consider all the possibilities. So, there's no need to worry. 
> However, you need to help me if you love two-way binding. I think it would 
> be great for the rest of the Angular team to hear how much you love two-way 
> binding.

> On the other hand, if you think two-way binding is a bad idea, you are 
> invited to help us investigate alternatives. So far I haven't seen an 
> equally nice alternative, but maybe you've got some ideas. If that's the 
> case, I invite you to share those with us. If we can work together to come 
> up with something that's even better...that would be amazing.

##### The Router
+ Does what a router should do:
> + Simple JSON-based Route Config
> + Optional Convention over Configuration
> + Static, Parameterized and Splat Route Patterns
> + Query String Support
> + Use Push State or Hashchange
> + Navigation Model (For Generating a Navigation UI)
> + Document Title Updates
> + 404 Route Handling
> + History Manipulation

##### Child Routers
they exist.

##### Screen Activation (part of the routers)
you now have access to the following:
> + `canActivate` - Allow/Prevent navigating to the new controller.
> + `activate` - Respond to successful navigation to the new controller.
> + `canDeactivate` - Allow/Prevent navigation away from the old controller.
> + `deactivate` - Respond to successful navigation away from the old
>   controller.

...and the router is also going to Angular 1.3
