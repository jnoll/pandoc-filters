`test_table.xml` as a table, using *.table* class:

~~~~~ {.xml .table include="test_table.xml" caption="XML Test Table" widths="[0.3, 0.6, 0.1]" columns='["id", "desc", "release"]' root="frontend"}
~~~~~


`test_table.xml` as a list, using *.list* class:

~~~~~ {.xml .list include="test_table.xml"  columns='["id", "desc", "release"]' root="frontend"}
~~~~~

Engine features from `test_table.xml` as a table, using *.table* class,
with explicit headers:

~~~~~ {.xml .table include="test_table.xml" columns='["id", "desc", "release"]' widths="[0.3, 0.6, 0.1]" align='["c", "l", "r"]' headers='["Feature", "Description", "Rel"]' root="engine"}
~~~~~

