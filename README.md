Discount
=========

This is a small Haskell application that applies a discount and calculates the
resulting total for an order.

Two principles I've tried to follow while developing this application are
making illegal states unrepresentable and providing useful error messages. For
example, product quantities are represented as Naturals, which eliminates the
possibility of having a negative quantity of some product, which would make no
sense. If a product ID is not found in the database, an error message
indicating which product was not found is provided.

I assume that looking up the discounts on each invocation of the function is
not too computationally intensive and that there is a good reason for using
integer keys for products and discounts instead of a more meaningful
identifier.

Some questions I have are about possible extensions to handle multiple
discounts and whether the rounding logic is sound, but in the absence of
additional requirements I'm happy with this solution for now. I couldn't get
the formatting quite right but otherwise I think this solution meets the stated
requirements.

The most convenient way of running this application is to use the `stack` tool
to run e.g. `stack test`, but if desired Cabal or Nix can be used as well.
