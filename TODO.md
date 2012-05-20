# TODO

* Work on defrule syntax. Eventually I want to make Clearley self-hosting, as a POC.
* Less verbose tokenization and character classes. Top-down tokenizers.
* Eventually, we want the parser to use some kind of NDFA, for performance reasons. I haven't explored this, it could take a lot of rewriting!
* Also test reliable disambiguity (LR vs LL in particular) when the above is implemented.
