all: test

%: %.hs XMLTable.hs
	cabal exec ghc -- --make  $<

test: test_csv test_xml test_include
test_include: include
	pandoc -t json csv.md | include | pandoc -f json -t markdown
test_csv: csv_to_table 
	pandoc -t json csv.md | include | csv_to_table | pandoc -f json -t markdown
run_test_xml: xml_to_table
	pandoc -t json xml.md | xml_to_table | pandoc -f json -t markdown