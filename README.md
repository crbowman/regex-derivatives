 
Regex Derivatives

The derivative of a set S of strings, with respect to a symbol c, is the subset of 
all strings in S that start with c, after having c removed. From this we can take the derivative
of a set of strings with respect to a string by taking succesive derivations with respect to 
the symbols in the string. Eventually the derivation will either return the set with the 
empty string, in which case we have a match, or it will return the empty set, in which case we don't have a match.

This technique can be applied to regular expressions that are build out of three operators, concatenation, 
alternation(union), and repetition(Kleene closure). By defining the derivative of each of these operators, we can 
take the derivative of any regular expression constructed out of these three operations. 
