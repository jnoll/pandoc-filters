all: CSVTable.o

%.o: %.hs 
	ghc -c --make  $<


test: test_csv test_xml test_include test_select test_bibentry
test_include: 
	pandoc -t json csv.md | stack exec include | pandoc -f json -t markdown
test_csv: 
	pandoc -t json csv.md | stack exec include | stack exec csv_to_table | pandoc -f json -t markdown
test_xml: 
	pandoc -t json xml.md | stack exec include | stack exec csv_to_table | pandoc -f json -t latex -o test_xml.pdf
test_select:
	pandoc -t json select_div.md | stack exec select_div -- --id other --class sometimes  | pandoc -f json -t plain
test_bibentry:
	pandoc -t json bibentry.md | stack exec bibentry -- --csl my-style.csl --bib refs.bib  | pandoc -f json -t plain --wrap=none
