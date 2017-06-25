
# simple PlantUML diagram

~~~~~ { .plantuml }
@startuml
start
partition a_partition {
: simple ; 
: plantuml ;
}
end
@enduml
~~~~~ 

# simple ditaa diagram

~~~~~ { .ditaa }
+--------+      +-------+ 
| simple +----->| ditaa |
+--------+      +-------+ 
~~~~~ 


# Named PlantUML diagram

~~~~~ { .plantuml filename="plantuml_test.png" }
@startuml
start
partition a_partition {
: named ; 
: file ; 
: plantuml ;
}
end
@enduml
~~~~~ 

# Named ditaa diagram

~~~~~ { .ditaa filename="ditaa_test.png" }
+-------+      +-------+      +-------+ 
| named +----->| file  |----->| ditaa |
+-------+      +-------+      +-------+ 
~~~~~ 

