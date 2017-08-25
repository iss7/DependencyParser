# Nivre's Dependency Parser
This project is an implementation of Nivre's dependency parser, done in OCaml. A parser finds relations between words in a sentence. Dependency parsing links words via binary grammatical relations called dependency relations. The implemented parser will work for any language that has the same dependency relations as English; it was tested with simple sentences in English, Spanish, and Portuguese. 
# Getting Started
[Insert instructions for installation/running] 
# Parsing a Sentence
To run the parser, call the function depParse, with the appropriate input. depParse takes as input a list of Words, where a Word is a tuple string * pos, and pos  is a part of speech (allowed parts of speech are listed in depParser.ml). Some sample inputs are given in depParser.ml for English, Spanish, and Portuguese. The first element in the list must be Word ("Root", Root), because a dependency parser links this root "word" to the root of the sentence. 
# Interpreting Output
The output of the parser is a 3-tuple of int * int * arc list. The first int is the number of "steps" of the parser; this is the number of calls to one of the core functions of the parser (left arc, right arc, shift, reduce). The second number is the amount of times the parser had to "backtrack", or the number of impossible parses the parser reached before finding an acceptable parse. The list is the set of dependency arcs for the sentence. The type of arc will be either LeftArc or RightArc, meaning the two words are connected with a left or right arc, respectively. The first word in the arc is the head of the arc and the second word is the dependent. The arc is drawn with an arrow from the head to the dependent. Examples of dependency graphs, as well as an analysis of the parser and results, can be found in "Differences in Dependency Parsing across Languages". 
# Author
Isabel Sharp
