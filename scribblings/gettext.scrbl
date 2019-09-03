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

@defproc*[([(textdomain [name (or/c string? (listof string?))]) procedure?]
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

@section{Multiple Domains}

As with GNU gettext, the following are provided:
@defproc[(dgettext [domain (or/c string? (listof string?))]
                   [message string?])
         string?]
@defproc[(dcgettext [domain (or/c string? (listof string?))]
                    [message string?]
                    [locale string?])
         string?]
@defproc[(dngettext [domain (or/c string? (listof string?))]
                    [msg-singular string?]
                    [msg-plural string?]
                    [n number?]) string?]
@defproc[(dcngettext [domain (or/c string? (listof string?))]
                     [msg-singular string?]
                     [msg-plural string?]
                     [n number?]
                     [locale string?]) string?]

These let you lookup messages in domains other than that specified by @racket[textdomain].
This is just a clumsy way to make up for inadequacies in the traditional gettext
design -- if you want to work with multiple domains, you should use the cascaded
domains described below.

@defproc[(bindtextdomain [domain string?] [dirname string?]) void?]{
 Override the directory to look for the given domain in.}

@section{Cascaded Domains}

A major inconvenience of GNU gettext is that it's essentially a one message file per
application design. Related applications, applications with the same menu entries,
or same error messages, all of these need to have their own duplicated translations
of all the same messages.

This package, however, lets you specify a list of strings as the text domain, and for
any message lookup these domains are searched in order.

@(racketblock
  (textdomain '("myapp" "gimp"))  @code:comment{search 1st myapp, then gimp}
  (gettext "/File/Close")         @code:comment{"Close" from gimp unless overridden})

You can thus share messages freely between applications, and effectively
have collections of message dictionaries.

@section{First-class Lookup Interface}

One of the most common types of application to write these days is a web application,
for which gettext is poorly suited. Gettext assumes a single locale, but for any kind of
server the clients may each have their own locale. free-gettext therefore provides a way
to generate separate first class gettext procedures.

@defproc[((make-gettext [domain (or/c string? (listof string?))]
                        [locale (or/c #f string? (listof string?)) #f]
                        [dirs (or/c #f string? (listof string?)) #f]
                        [cdir (or/c #f string?) #f]
                        [cached? boolean? #t]
                        [lookup-cached? boolean? #t])) procedure?]{
@racket[domain] is the same as the first argument to @racket[textdomain],
 and may be similarly cascaded.

@racket[locale] is the locale, as a string or list of strings of the form
 @nonbreaking{LANG[_REGION][.ENCODING]},
 and defaults to the LANG or LC_ALL environment variable, or C if neither is set. Multiple
 locales are also searched in order, which can be useful when you have incomplete translations
 in similar languages.

@racket[dirs] (again a string or list of strings) is the search path of directories which should
 hold the LOCALE/CDIR/ directories which contain the actual message catalogs. This is always
 appended with the system default, e.g. "/usr/share/locale", and may also inherit from the
 GETTEXT_PATH colon-delimited environment variable.

@racket[cdir] is the category directory, defaulting to either the LC_CATEGORY environment
 variable or the appropriate system default (e.g. LC_MESSAGES).
 You generally won't need to specify this.

@racket[cached?] means to cache individual messages, and defaults to #t. This is a natural
 default (GNU gettext similarly caches messages), but during development it can be handy
 to disable caching if you intend to edit messages while coding.

@racket[lookup-cached?] means to cache the lookup dispatch generated by these parameters,
 and defaults to #t. Thus by default multiple calls to @racket[make-gettext] with the same parameters
 return the same object, and they in turn share the same message cache.

@racket[make-gettext] returns a dispatch closure with the following parameters:

@subsection{Procedures}

@itemlist[
 @item{(<self> 'getter) - returns a gettext-style procedure}
 @item{(<self> 'ngetter) - returns an ngettext-style procedure}
 @item{(<self> 'setter) - returns a procedure for manually setting message translations}]
}