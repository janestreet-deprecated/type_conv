## 112.01.00

- Updated ast matching for 4.02

## 111.13.00

- Removed some unused-value warnings when `with` is used in signatures.

    Removed warnings in cases like:

        include (module_expr : sig type t with bin_io end)

## 109.60.00

- Compatibility with warning 7 (method override)

## 109.53.00

Bump version number

## 109.52.00

- Removed comments from pretty-printed types in `type_conv` error
  messages.

## 109.47.00

- Made `type nonrec` work when a type has both an equation and a representation.

  For example:

  ```ocaml
  type t = A of t
  module T = struct
    type nonrec t = t = A of t
  end
  ```

## 109.41.00

- Fixed the generated code of `typerep` and `sexplib` on sum types containing `True` or `False`.

  Without this fix, `typerep` would wrong constructor names for
  `Blang.t`, for instance.

  `Variantslib` has the same problem but applying the same fix there
  would still not make the generated code compile because the generated
  code would contain labels and variable named `true` or `false`.

  Other syntax extensions should not be affected because they don't
  build strings from constructor names.

## 109.28.00

- Fixed an issue with `type_conv` in the toplevel.

  Used AST filters for the `_no_unused_value_warning_` machinery.
  `type_conv` modifies the parser but it didn't work well in the
  toplevel.

  Removed the `parsing_mli` reference, an instead always add the
  special `_no_unused_value_warning_` type and just strip it for
  signature items.

## 109.20.00

- Removed some warnings caused by generated signatures.

  1. In signatures on local modules.
  2. When there are duplicate signature items like in this example:

    ```ocaml
    module Warnings : sig
      type t = private { foo : int } with fields (** used to say unused value foo *)
      val foo : string
    end = struct
      type t = { foo : int } with fields
      let foo = "a"
    end
    ```

  3. In the signatures of all the parameters of functors that take multiple
     parameters; this used to work only for the last parameter.

## 109.08.00

- Fixed type_conv to stop dropping parens in arguments such as:

    type t = {
      a : int with default(1), sexp_drop_if(fun x -> (x + 1) * 2 = 4)
    } with sexp

## 2012-07-15

- Added support for record field annotations and defaults.
- Fixes for upcoming OCaml 4.00 release.

## 2011-09-15

- Fixes to improve package dependency resolution.

## 2011-08-02

- Added missing module type case for "module type of".

## 2011-07-04

- Merged with Jane Street version.  API changes:

    Removed functions:

    * Gen.ide
    * Gen.idp

    Removed location arguments from:

    * type_is_recursive
    * drop_variance_annotations

## 2010-12-22

- Merged with Jane Street version.  No code changes.

## 2010-09-25

- Added a missing type case to type_is_recursive. Thanks to Michael
  Wawrzoniak <mhw@cs.princeton.edu> for this patch!

## 2010-07-07

- Major changes for compatibility with OCaml 3.12.

## 2010-06-03

- Improved determination of type_conv paths. Thanks to Jacques Le
  Normand <rathereasy@gmail.com> for this patch!

## 2009-09-19

- Added missing type cases for supporting variant types.

## 2009-01-14

- Added support for type converters that take arguments. Thanks to
  Jérémie Dimino <jeremie@dimino.org> for this patch!

    Added support for deprecated OCaml syntax, since the compiler still
    supports it, too.

## 2008-10-22

- Fixed bug preprocessing labeled arguments in function types.

## 2008-10-18

- Fix for upcoming OCaml release 3.11.

## 2008-10-07

- Added a patch to improve handling of type conversion paths.

    Thanks to David Rajchenbach-Teller <David.Teller@ens-lyon.org> for
    the patch!

## 2008-08-20

- Added support for exception converters.

## 2008-07-25

- Fixed bug concerning variance annotations in type
  definitions within structures.

## 2008-03-17

- Improved META file and support of toplevel interpreter.

## 2008-02-11

- Added support for handling variance annotations in signatures, and for
  empty types.

## 2007-10-14

- Initial release.

