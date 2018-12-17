# convexjlr 0.81.9000

# convexjlr 0.8.1

* The fourth release on CRAN.

# convexjlr 0.8.0.9000

* Updates for `Julia` v0.7 and v1.0.
* Drop `XRJulia` support, as it does not work with `Julia` v0.7 and v1.0.

# convexjlr 0.7.1.9000

* Default `SCS` solver doesn't have `verbose = FALSE` default option any more.
* Users can choose `ECOS` as the solver for convex problems.
* Users can set a bunch of options for both `SCS` and `ECOS` solvers.

# convexjlr 0.7.0.9000

* The users can set maximal iteration times for the convex problem solver in `cvx_optim`.
* Bug correction for handling of `diag`.

# convexjlr 0.7.0

* The third release on CRAN.
* Remove deprecated `setup` function.
* Use `JuliaCall` as the default backend.

# convexjlr 0.6.1.9000

* Fix deprecation warnings from `JuliaCall` backend.
* Fix some little bugs.
* Add the option in `convex_setup` to set the path to `julia` binary.

# convexjlr 0.6.1

* The second release on CRAN.

# convexjlr 0.6.0.9000

* Supports multiple ways to connect to `julia`, one way is through package `XRJulia`,
  and the other way is to use package `JuliaCall`. The difference is as follows:
  - `XRJulia` connects to `julia`, which is the default for `convexjlr`,
    the advantage is the simplicity of the installation process, once you have a working
    R and working julia, it should be okay to use `convexjlr` in this way. Note that
    if you have the latest Julia version (v0.6.0) installed, then you have to use the
    latest version of `XRJulia`.
  - `JuliaCall` embeds `julia` in R,
    the advantage is the performance, for example,
    if your convex problem involves large matrice or long vectors,
    you may wish to use `JuliaCall` backend for `convexjlr`;
    the disadvantage is the installation process, since embedding `julia` needs
    compilations.

# convexjlr 0.5.1.9000

* Added a `NEWS.md` file to track changes to the package.
* Re-organize tests.
* Deprecate `setup`, should use `convex_setup`.

# convexjlr 0.5.1

* A patch release on CRAN.

# convexjlr 0.5.0

* The first release on CRAN.
