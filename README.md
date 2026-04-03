# flex

A batteries-included alternative for mathematics in Haskell.

The main focus of this library is to provide the foundation for improving the status quo
of the `Num`/`Floating`/`RealFrac` "hierarchy" that exists in `base-4.x`. It also includes
the basics for lenses; an alternative framework for `Functor`/`Foldable`/`Traversable` as well as
their indexed variants; number systems like `Dual`, `Perplex`, `Quaternion` and `Minkowski`;
discrete probability distributions; bilinear and sesquilinear forms; representing numbers in different bases;
and Lawvere theories for algebraic structures.

To get started, see `Flex.Math.Numbers` and `Flex.Math.Module`. For category theory, see `Flex.Math.Category`.
For lenses and other optics, see `Flex.Math.Optics`. For vectors and matrices, see `Flex.Math.Matrix`.
For Lawvere theories and their uses, see `Flex.Math.Variety` and `tests/Main.hs`.
