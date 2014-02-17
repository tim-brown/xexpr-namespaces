#lang scribble/manual
@(require (for-label racket xml/xml))
@title{xexpr-namespaces}

This module removes ``xmlns'' attributes from an @(racket xexpr) document, and replaces the tags to which
they refer with ``normalised'' versions, which include the namespace on every element.

How this is done is up to the user, but (struct ns:tag) is provided to allow for,
e.g. (struct ns:tag _ _) clauses in matches.

The reverse process allows a consistent mapping of namespaced tags to xmlns: attributes. This
allows matches to be performed on xexprs, where the tag has a guaranteed value of a symbol of the
form nmspc:tag, where nmspc is (optionally) derived for a dictionary of some sort.

@section{TODO: Cruft to be Sorted Out}

@subsection{Tests}
@itemlist[
         @item{xml-normalise-namespace}    
             ]

@subsection{Documentation}
@subsubsection{References}
@url{http://www.w3.org/TR/2006/REC-xml-names11-20060816/}