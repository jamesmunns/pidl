# Ideas for a Postcard IDL

* Primitives
    * bool
    * i8
    * i16
    * i32
    * i64
    * i128
    * u8
    * u16
    * u32
    * u64
    * u128
    * f32
    * f64
    * unit
* Questionable Dupes
    * char (is this just string for now?)
    * byte array (this is just `[u8]`)
    * option (this is just `enum`)
        * BUT! It supports a generic.
* Less primitive
    * string
    * unit struct
    * newtype struct
    * seq
    * tuple
    * tuple struct
    * map
    * struct
    * enum (!)
* Variants
    * unit variant
    * newtype variant
    * tuple variant
    * struct variant

# Syntax

The goal at "stage one" is to find all types, which should each have a unique name.

We are certainly going to need to be able to import other files and packages.

## Importing

We're going to want some kind of import syntax. Probably from:

* Path import (relative/absolute)
* Github repo (and path?)
    * Specify tag/hash/branch?
* Github Gist?
* HTTP file?
* Make a postcard package manager?

We could leave this in some kind of "frontmatter"

```
[imports]
alpha   = { path = "../alpha"                                                               }
beta    = { path = "/types/beta"                                                            }
gamma   = { git  = "https://github.com/example/types", rev = "ABCD..."                      }
delta   = { git  = "https://github.com/example/types", tag = "some-tag/v1"                  }
epsilon = { gist = "https://gist.github.com/jamesmunns/b4102912006ac80c4e49121c0fa87c06"    }
zeta    = { http = "https://onevariable.com/example/types/v1/demo"                          }
eta     = "some hash? idk"
```

Eventually we'll want to be able to use these imports. This could be in the form
of scoped imports:

```
epsilon::ExampleType
```

Or maybe importing items into scope?

```
use epsilon::{ExampleType, OtherType};
```

## Type Names

As a schema language, the currency here is *the name of a type*.

## Aliases

We're going to need some kind of syntax for aliases. One thing to be
careful of: **should IDL aliases be different than things
like newtypes**?

I think yes:

```
type Example = [u8]
// different to:
Example([u8])
```

Do we need templated aliases?

## "Implicit" definitions

Anywhere that can reference a type can also create an "implicitable"
type definition. For example:

```
struct Example([Contained])
```

This creates a new specific type: `[Contained]`, without requiring a specific
aliasing stype.

Types that can be created implicitly:

* tuples
* seq
* template instantiations?

## Stages of compilation:

* Gather list of dependencies/imports
* Find a list of type definitions by name
* For types that can generate implicit types, generate implicit types
* ...?
* End up with a list of NamedTypes

## Generics/Templates

We might need a first class concept of generics. The core example
is `Option<T>`. We might want to have a concept of "template" types:
They are not types *yet*, until they've been instantiated.

```
template<T> enum Option {
    Some(T),
    None,
}
```

Templates are implicitable - they can be instantiated in a defintion:

```
struct Demo {
    field: Option<u8>,
}
```

Any type that contains another type can be templated:

* struct
    * standard struct
    * Newtype Struct
    * tuple struct
* enums containing any of the following variants:
    * newtype variant
    * tuple variant
    * struct variant
* seq
* tuple
* option
* map

## Primitives

All of these are named types, referred to as such:

* `bool`
* `i8`
* `i16`
* `i32`
* `i64`
* `i128`
* `u8`
* `u16`
* `u32`
* `u64`
* `u128`
* `f32`
* `f64`
* `()` or `unit`
* `String`
    * This MIGHT have some kind of bounding
    * Still "primitive" because it's not a composite/discriminated type

## unit struct

Has a name, and no body. Somewhat different (maybe?) to a newtype
struct that contains a single `()`.

```
struct Example
```

## newtype struct

Has a name, with a single body element.

```
struct Example(Contained)
```

## seq

Doesn't have a name (really?), we can implicitly make a seq from
any other type, like:

```
[Example]
```

!!! Implicit-able !!!

## tuple

Doesn't have a name, we can implicitly make a tuple from any other
type, like:

```
(Example, Example)
```

We might want to require "at least one comma" for it to be valid, so:

```
(Example,)  // valid
(Example)   // not valid
```

!!! Implicit-able !!!

## tuple struct

Does have a name, same other rules as tuples

```
Example(FieldA, FieldB) // this is a tuple struct!
Example(Field)          // this is a newtype struct!
```

## map

Maps are sort of just a template? Like, they require generics

```
template<K, V> Map([(K, V)])
```

## struct

struct as a keyword is weird. It actually makes multiple things:

* `struct UnitStruct`
* `struct NewTypeStruct(Example)`
* `struct TupleStruct(ExampleA, ExampleB)`
* `struct RealStruct { ... }`

The last one is the only one that we care about here, and it is
differentated by having curly braces

```
struct Example {
    field: OtherType,
}
```

## enum (!)

Enums are enums

```
enum Example {
    UnitVariant,
    NewtypeVariant(Example),
    TupleVariant(Example1, Example2),
    StructVariant {
        field: Example,
    },
}
```

# File syntax

* Top Level
    * Template definitions
        * Declaration of template parameters
        * Type definition (template flavored)
            * struct
                * Unit
                * Newtype
                    * one anon field
                * Tuple
                    * N anon fields
                * Regular
                    * N named fields
            * enum
                * unit variant
                * newtype variant
                    * one anon field
                * tuple variant
                    * N anon fields
                * struct variant
                    * N named fields
            * seq
            * tuple
            * option
            * map
    * Type Aliases (maybe)
    * Type Definitions
        * struct
            * Unit
            * Newtype
                * one anon field
            * Tuple
                * N anon fields
            * Regular
                * N named fields
        * enum
            * unit variant
            * newtype variant
                * one anon field
            * tuple variant
                * N anon fields
            * struct variant
                * N named fields

