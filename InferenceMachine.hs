module InferenceMachine where


{- inference.txt
Below follows a challenging example for machine inference
for those who may be particular interested in theorem proving.

The pigeon hole principle states (in one version)
that if we have n+1 items to be put into n boxes, at least
one of the boxes has to contain more that one item
(cf. WIKI pigeon hole principle).
Trivially as it seems, it nevertheless
presents a challenge to propositional theorem provers.

It also presents a methodological challenge to AI research,
namely to understand why humans (even children) deal so
easily with this problem type, perhaps by mental imagery,
see eg. http://plato.stanford.edu/entries/mental-imagery/ .

You may try your inference machine on the tiny pigeon
hole problem with n=2, say boxes 1 and 2, and items a, b ,c:
--
Pigeon hole sentences stated in propositional logic have
been used to obtain deep results in theoretical computer
science. Here is a reference:
A. Haken: The Intractability of Resolution. Theor. Comput. Sci. 39 (1985).
-}
