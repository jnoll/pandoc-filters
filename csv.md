
A table from CSV:

~~~~~ {.csv .table include="test_table.csv" caption="CSV Test Table" widths="[0.3, 0.6, 0.1]"}
~~~~~


`test_table.xml` as a table, using *.table* class:

~~~~~ {.xml .table include="test_table.xml" caption="XML Test Table" widths="[0.3, 0.6, 0.1]" columns='["id", "desc", "release"]' root="frontend" child="footure"}
~~~~~

`test_table.xml` as a list, using *.list* class:

~~~~~ {.xml .list include="test_table.xml"  columns='["id", "desc", "release"]' root="frontend" child="footure"}
~~~~~

XML code block as a table:

~~~~~ {.xml .table  caption="XML Test Table (immediate)" widths="[0.3, 0.6, 0.1]" columns='["id", "desc", "release"]' root="features" child="footure"}
<features>
  <feature>
    <id>feature 1</id>
    <desc>Do the processing for feature 1</desc>
    <release>1</release>
  </feature>
  <feature>
    <id>feature 2</id>
    <desc>Do the processing for feature 2</desc>
    <release>2</release>
  </feature>
  <feature>
    <id>feature 3</id>
    <desc>Do the processing for feature 3</desc>
    <release>3</release>
  </feature>
</features>
~~~~~

XML code block as a list:

~~~~~ {.xml .list  columns='["id", "desc", "release"]' root="features"}
<features>
  <feature>
    <id>feature 1</id>
    <desc>Do the processing for feature 1</desc>
    <release>1</release>
  </feature>
  <feature>
    <id>feature 2</id>
    <desc>Do the processing for feature 2</desc>
    <release>2</release>
  </feature>
  <feature>
    <id>feature 3</id>
    <desc>Do the processing for feature 3</desc>
    <release>3</release>
  </feature>
</features>
~~~~~

Include file contents:

~~~~~ {.xml include="test_table.xml"}
~~~~~ 

Note that without a class, the contents will just be indented as
unknown code:

~~~~~ {include="test_table.xml"}
~~~~~ 

This alternate include syntax based on div seems more natural.

<div class="code haskell" include="Setup.hs"></div>

The plain form, without a class.

<div class="code" include="Setup.hs"></div>

<div include="sample.md"></div>
