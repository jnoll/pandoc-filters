all: build
build:
	stack build

%.o: %.hs 
	ghc -c --make  $<


test: test_csv test_xml test_include test_select test_bibentry test_to_img test_subst test_beamer
test_include: 
	pandoc -t json csv.md | stack exec include | pandoc -f json -t markdown
test_csv: 
	pandoc -t json csv.md | stack exec include | stack exec csv_to_table | pandoc -f json -t markdown
test_xml: 
	pandoc -t json xml.md | stack exec include | stack exec csv_to_table | pandoc -f json -t latex -o test_xml.pdf
test_select:
	pandoc -t json select_div.md | stack exec select_element -- --id other --id yes --class sometimes --class bar  | pandoc -f json -t plain
test_bibentry:
	pandoc -t json bibentry.md | stack exec bibentry -- --csl my-style.csl --bib refs.bib  | pandoc -f json -t plain --wrap=none
test_to_img:
	pandoc -t json to_img.md | stack exec to_img  | pandoc -f json -t latex --wrap=none -o to_img.pdf
test_subst:
	pandoc -t json subst.md | stack exec subst words.dat  | pandoc -f json -t plain
test_beamer:
	pandoc -t json beamer.md | stack exec beamer  | pandoc -f json -s -t beamer --wrap=none -o beamer.pdf

# For this to work, the --filter switch must be used so output format is passed.
test_color:
	pandoc -s -t latex --wrap=none --filter color color.md -o color.pdf
