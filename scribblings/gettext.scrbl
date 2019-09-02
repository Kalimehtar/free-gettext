#lang scribble/manual
@require[@for-label gettext racket/base]]

@title{gettext}
@author+email["Roman Klochkov" "kalimehtar@mail.ru"]

@defmodule[gettext]

free-gettext is an implementation of GNU Gettext. It is implemented from scratch
purely in Scheme and thus available with a BSD license. free-gettext is also easier to use,
allowing you to optionally load .po files directly without compiling them to .mo files first,
and has many advanced features such as multi-language locales and cascaded message domains.

@defproc*[([(textdomain [name string?]) procedure?]
           [(textdomain) string?])]{
@racket[textdomain] sets the name of the current "domain", which is the name of the message catalog
 (typically the name of the application). With no arguments it returns the name
 of that domain.                                          
}

@defproc[(gettext [message string?]) string?]{
@racket[gettext] then fetches the translation of the message in that domain in the current locale
 (from the LANG environment variable), or returns the original string if the message
 can't be found.

Apart from actually creating the message files (which you can put off indefinitely),
 that's all you need to internationalize your message strings. Since every natural language
 string needs to be translated, a common idiom is to give a short name to @racket[gettext]:
 (define _ gettext)}
