all: test
build: 
	cabal build
install: 
	cabal install
%: %.hs 
	cabal exec ghc -- --make  $<


test: test_csv test_xml test_include
test_include: include
	pandoc -t json csv.md | include | pandoc -f json -t markdown
test_csv: csv_to_table 
	pandoc -t json csv.md | include | csv_to_table | pandoc -f json -t markdown
test_xml: csv_to_table
	pandoc -t json xml.md | include | csv_to_table | pandoc -f json -t latex -o test_xml.pdf

csv_to_table: XMLTable.hs CSVTable.hs Tables.hs