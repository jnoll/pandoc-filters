# XML tables

`test_table.xml` as a table, using *.table* class:

~~~~~ {.xml .table include="test_table.xml" caption="XML Test Table (root=frontend)" widths="[0.3, 0.6, 0.1]" columns='["id", "DESC", "release"]' root="frontend"}
~~~~~

~~~~~ {.xml .table include="test_table.xml" caption="XML Test Table (root=deeper)" widths="[0.3, 0.6, 0.1]" columns='["id", "desc", "release"]' root="deeper"}
~~~~~


`test_table.xml` as a list, using *.list* class:

~~~~~ {.xml .list include="test_table.xml"  columns='["id", "desc", "release"]' root="frontend"}
~~~~~

Engine features from `test_table.xml` as a table, using *.table* class,
with explicit headers:

~~~~~ {.xml .table include="test_table.xml" columns='["id", "desc", "release"]' widths="[0.3, 0.6, 0.1]" align='["c", "l", "r"]' headers='["Feature", "Description", "Rel"]' root="engine"}
~~~~~

~~~~~ {.xml .table  caption="XML Test Table (immediate)" widths="[0.3, 0.6, 0.1]" columns='["id", "description", "release"]' root="features" child="feature"}
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

# XML code block as a list:

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
