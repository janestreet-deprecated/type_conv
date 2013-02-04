`Type_conv` - Support Library for Preprocessor Type Conversions
===============================================================

---------------------------------------------------------------------------

What is `type_conv`?
--------------------

The `type_conv` mini-library factors out functionality needed by different
preprocessors that generate code from type specifications.  Example libraries
currently depending on `type_conv`:

  * `bin_prot`
  * `comparelib`
  * `fieldslib`
  * `ounit`
  * `pipebang`
  * `sexplib`
  * `typehash`
  * `variantslib`

This functionality cannot otherwise be duplicated without losing the ability
to use these preprocessors simultaneously.

Documentation
-------------

API-documentation can be built during installation (`make doc`).
The API-documentation of the latest release can also be found
[online](http://mmottl.bitbucket.org/projects/type_conv/api/).

---------------------------------------------------------------------------

Contact Information and Contributing
------------------------------------

In the case of bugs, feature requests, contributions and similar, please
contact the maintainers:

  * Jane Street Capital, LLC <opensource@janestreet.com>

Up-to-date information should be available at:
* <https://github.com/janestreet/type_conv>
* <https://bitbucket.org/janestreet/type_conv>
