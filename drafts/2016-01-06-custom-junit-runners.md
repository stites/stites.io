---
date: 1900-01-01
---


---
layout: post
title: Java Throwback: Custom JUnit Runners
---

{{ page.title }}
================

<p class="meta">06 Jan 2016 - Belmont, CA</p>

JUnit is used heavily at Bina and, especially in order to automate some of the
integration tests, we structure tests just as abstractly as the code, with a heavy
reliance on the type system and with use of AOP where nessecary. Unfortunately,
JUnit has some minor flaws here and there which make it unfriendly to this whole
endeavor.

One of these minutiae -- I say "minutiae" because we _can_ work around it, but it's a
little annoying -- include the fact that JUnit has the default assumption that all
tests must have a `void` return type. If we can get rid of this constraint, then we
wind up with the ability to have abstract test classes with extendable test methods.
This is where writing a custom JUnit runner comes in handy.

Ultimately, you want to subtype off of `BlockJUnit4ClassRunner`, the default runner,
This can be done by subtyping a runner you're working with or just subtyping
`BlockJUnit4ClassRunner` directly. What you're shooting for, however, is to override
`validatePublicVoidNoArgMethods` off of the inherited `ParentRunner`:

    import org.junit.runners.BlockJUnit4ClassRunner;
    import org.junit.runners.model.FrameworkMethod;
    import org.junit.runners.model.InitializationError;

    import java.lang.annotation.Annotation;
    import java.util.List;

    public class ReturnableTestRunner extends BlockJUnit4ClassRunner {
      public ReturnableTestRunner(Class<?> klass) throws InitializationError {
        super(klass);
      }

      @Override // although we won't change anything right now.
      protected void validatePublicVoidNoArgMethods(
        Class<? extends Annotation> annotation,
        boolean isStatic,
        List<Throwable> errors
      ) {
        List<FrameworkMethod> methods = getTestClass().getAnnotatedMethods(annotation);

        for (FrameworkMethod eachTestMethod : methods) {
          // override this logic
          eachTestMethod.validatePublicVoidNoArg(isStatic, errors);
        }
      }
    }

However this isn't the only use of `validatePublicVoidNoArgMethods`: JUnit docs
inform us that this adds to the errors list for the class if:
  + any method is not public
  + there exist methods which take parameters
  + a method returns something other than void (what we want to change)
  + a method is static
  + a method is not static if `isStatic` is true.

So we really need to dig a little deeper into what is happening in
`FrameworkMethod#validatePublicVoidNoArg`. Here it is:

    \\ FrameworkMethod.java#validatePublicVoidNoArg
    public void validatePublicVoidNoArg(boolean isStatic, List<Throwable> errors) {
      validatePublicVoid(isStatic, errors);
      if (method.getParameterTypes().length != 0) {
        errors.add(new Exception("Method " + method.getName() + " should have no parameters"));
      }
    }

and once more for `validatePublicVoid`:

    \\ FrameworkMethod.java#validatePublicVoid
    public void validatePublicVoid(boolean isStatic, List<Throwable> errors) {
      if (isStatic() != isStatic) {
        String state = isStatic ? "should" : "should not";
        errors.add(new Exception("Method " + method.getName() + "() " + state + " be static"));
      }
      if (!isPublic()) {
        errors.add(new Exception("Method " + method.getName() + "() should be public"));
      }
      if (method.getReturnType() != Void.TYPE) {
        errors.add(new Exception("Method " + method.getName() + "() should be void"));
      }
    }

So let's build up a solution to this problem. It might look something like:

    import org.junit.runners.BlockJUnit4ClassRunner;
    import org.junit.runners.model.FrameworkMethod;
    import org.junit.runners.model.InitializationError;

    import java.lang.annotation.Annotation;
    import java.util.List;

    public class ReturnableTestRunner extends BlockJUnit4ClassRunner {
      public ReturnableTestRunner(Class<?> klass) throws InitializationError {
        super(klass);
      }

      @Override
      public void validatePublicVoid(boolean isStatic, List<Throwable> errors) {
        if (isStatic() != isStatic) {
          String state = isStatic ? "should" : "should not";
          errors.add(new Exception("Method " + method.getName() + "() " + state + " be static"));
        }
        if (!isPublic()) {
          errors.add(new Exception("Method " + method.getName() + "() should be public"));
        }
        // if (method.getReturnType() != Void.TYPE) {
        //   errors.add(new Exception("Method " + method.getName() + "() should be void"));
        // }
      }
    }

But this isn't ideal as we're overriding a method and forcing it to lie about what it
does. As greg has aptly shown, it would be much easier to break out our own method
with a couple of functional interfaces to make this look nice and clean:

    import org.junit.runners.BlockJUnit4ClassRunner;
    import org.junit.runners.model.FrameworkMethod;
    import org.junit.runners.model.InitializationError;

    import java.lang.annotation.Annotation;
    import java.util.List;
    import java.util.function.Function;

    public class ReturnableTestRunner extends BlockJUnit4ClassRunner {
      public ReturnableTestRunner(Class<?> klass) throws InitializationError {
        super(klass);
      }

      protected validate(
        FrameworkMethod method,
        Function<? super FrameworkMethod, ? extends Boolean> accessor,
        String name,
        Boolean check,
        List<Throwable> errors
      ) {
        if ((check != null) && (accessor.apply(method) != check)) {
          errors.add(
            new Exception("Method "+method.getMethod().getName()+"() "+(check ? "should" : "should not")+" "+name)
          );
        }
      }

      public static void validatePublicNoArg(
        FrameworkMethod method,
        Boolean isStatic,
        Boolean isPublic,
        Boolean isVoid,
        List<Throwable> errors
      ) {
        validate(method, m -> m.isStatic(), "be static", isStatic, errors);
        validate(method, m -> m.isPublic(), "be public", isPublic, errors);
        validate(method, m -> m.getMethod().getReturnType() == Void.TYPE, "have return type of void", isVoid, errors);
        validate(method, m -> method.getMethod().getParameterTypes().length != 0, "have arguments", false, errors);
      }

      @Override
      protected void validatePublicVoidNoArgMethods(
        Class<? extends Annotation> annotation,
        boolean isStatic,
        List<Throwable> errors
      ) {
        List<FrameworkMethod> methods = getTestClass().getAnnotatedMethods(annotation);

        for (FrameworkMethod eachTestMethod : methods) {
          validatePublicNoArg(eachTestMethod, isStatic, errors);
        }
      }
    }

