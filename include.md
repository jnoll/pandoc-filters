
# Block Includes

## test_table.xml (xml class)

~~~~~ {.xml include="test_table.xml"}
~~~~~ 

## test_table.xml (no class)

Note that without a class, the contents will just be indented as
unknown code:

~~~~~ {include="test_table.xml"}
~~~~~ 

## test_table.csv

~~~~~ {include="test_table.csv"}
~~~~~ 

# DIV Includes

This alternate include syntax based on div seems more natural.

<div class="code haskell" include="Setup.hs"></div>

The plain form, without a class.

<div class="code" include="Setup.hs"></div>

<div include="sample.md"></div>

