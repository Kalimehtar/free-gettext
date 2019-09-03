#lang scribble/manual
@(require (for-label gettext racket/base))

@title{gettext}
@author+email["Roman Klochkov" "kalimehtar@mail.ru"]

@defmodule[gettext]

@section{Introduction}

This package is an implementation of
@(hyperlink "https://www.gnu.org/software/gettext/manual/gettext.html"
            "GNU gettext").
It is implemented from scratch
purely in Scheme and thus available with a BSD license. It is also easier to use,
allowing you to optionally load .po files directly without compiling them to .mo files first,
and has many advanced features such as multi-language locales and cascaded message domains.

@(racketblock
  (require gettext)
  (textdomain "myapp")
  (print (gettext "Hello, world!")))

The above is the most basic usage and is equivalent to the GNU gettext module.

@defproc*[([(textdomain [name string?]) procedure?]
           [(textdomain) string?])]{
@racket[textdomain] sets the name of the current ``domain'', which is the name of the message catalog
 (typically the name of the application). With no arguments it returns the name
 of that domain.                                          
}

@defproc[(gettext [message string?]) string?]{
@racket[gettext] then fetches the translation of the message in that domain in the current locale
 (from the LANG environment variable), or returns the original string if the message
 can't be found.

Apart from actually creating the message files (which you can put off indefinitely),
 that's all you need to internationalize your message strings. Since every natural language
 string needs to be translated, a common idiom is to give a short name to @racket[gettext]:}

@(racketblock
  (define __ gettext)
  (print (__"Hello, world!")))

Alternately, you could make , a prefix for all message strings:

@(racketblock
  (define-syntax unquote
    (syntax-rules ()
      ((_ str) (gettext str))))

  (print ,"Hello, world!"))

@section{Plural forms}

There is also a procedure @racket[ngettext] for working with plural forms:

@defproc[(ngettext [msg-singular string?]
                   [msg-plural string?]
                   [n number?]) string?]{
@(racketblock (format #t (ngettext "~D file removed" "~D files removed" n) n))
In the case of English (or if no translation is found), this applies the familiar
 logic of using the first string (singular) if @racket[n] is 1, and the second string (plural)
 otherwise. Not all languages have two plural forms, however, and the @racket[ngettext] interface
 allows you to write message files with the proper inflections for any language and any
 value of @racket[n]. The exact details of how to write a message file is beyond the scope of
 this document - see the GNU gettext documentation.}