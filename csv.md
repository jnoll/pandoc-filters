
# A table from CSV

~~~~~ {.csv .table include="test_table.csv" caption="CSV Test Table" widths="[0.3, 0.6, 0.1]"}
~~~~~

# "Immediate" table

~~~~~ {.csv .table  caption="CSV Test Table" widths="[0.3, 0.6, 0.1]"}
First,Second,Third
A1,A2,A3
B1,B2,B3
C1,C2,C3
"-*- mode: compilation; default-directory: /home/jnoll/projects/src/pandoc-filters/ -*-",Compilation started at Mon Jan 26 20:55:51,"Deprecated: Please use the new exceptions variant, Control.Exception.try"
~~~~~

# An immediate DSV table

~~~~~ {.csv .table  caption="CSV Test Table" widths="[0.3, 0.6, 0.1]" delim="|"}
First|Second|Third
A1|A2|A3
B1|B2|B3
C1|C2|C3
-*- mode: compilation; default-directory: /home/jnoll/projects/src/pandoc-filters/ -*-|Compilation started at Mon Jan 26 20:55:51|Deprecated: Please use the new exceptions variant, Control.Exception.try
~~~~~

# Kanban-style pivot table

~~~~~ {.csv .pivot include="kanban.csv" caption="CSV Pivot Table" widths="[0.33, 0.33, 0.33]" pivot_col="State" value_col="Task"}
~~~~~
