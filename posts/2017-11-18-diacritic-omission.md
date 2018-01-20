---
title: The diacritic omission phenomenon
author: joomy
tags: language, turkish
description: Omitting the diacritics is a common phenomenon in languages such as Turkish and Serbo-Croatian. Then why isn't there any research on this?
---

Languages that have recently switched to Latin alphabet usually employ a higher
number of diacritics to cover all the phonemes.  Turkish has *ç*, *ş*, *ğ*, *ı*
(upper case *I*), *i* (upper case *İ*), *ö*, *ü*.  Serbo-Croatian (or Bosnian
and Croatian in this case) has *č*, *ć*, *dž*, *đ*, *š*, *ž*.  Polish, despite
not switching recently, has *ą*, *ć*, *ę*, *ł*, *ń*, *ó*, *ś*, *ź*, *ż*.  In
this blog post, I'll mostly talk about Turkish since I'm a native speaker of
it, but some of the observations here occur in Serbo-Croatian and Polish as
well, in varying degrees.

The phenomenon I'll describe is the omission of diacritics in typing.
In Turkish, *ç* would become *c*, *ş* would
become *s*, *ğ* would become *g*, *ı* and *i* would both become *i*, *ö* would
become *o*, and *ü* would become *u*.
Especially in the age of text messages on phones that did not support
characters with diacritics, most people are used to typing
with their closest [ASCII](https://en.wikipedia.org/wiki/ASCII) equivalents,
which means all diacritics got removed.

Similarly, in the early days of Turkish internet sphere, when displaying
Turkish characters was non-trivial, most web sites also didn't allow input in
Turkish characters. One of the most significant examples of this is [Ekşi
Sözlük](https://en.wikipedia.org/wiki/Ek%C5%9Fi_S%C3%B6zl%C3%BCk), a unique
social media web site from 1999 that is still very active today.
Their solution was to replace *ş* with the *$* symbol, even in their logo and
in all the texts, until the Turkish character support got stable on the web.

Now we have smartphones that can easily change different keyboards and that
support Unicode characters, the number of people who type without diacritics is
dropping. However if one doesn't have a Turkish keyboard on their computer
(like me), they would still have to type without Turkish characters.
Moreover, there is a lot of Turkish text written without the diacritics for the
reasons mentioned above. This raises a number of interesting linguistic
questions for me.

# Ambiguity and disambiguation

The problem with the diacritic omission is that Turkish already has distinct
letters *c*, *s*, *g*, *i*, *o* and *u*, which means those letters would now be
overloaded with two phonemes. One can usually infer what the actual word. For
example, there is no such word as "catal", so it has to be "çatal" (fork).
However, it's not all clear in some other cases.

The most (in)famous case of ambiguity is the distinction between "sıkıldım" (I
am bored) and "sikildim" (I got f'ed). Unfortunately it's not only a lewd
cliché joke; [two people died because of this confusion back in
2008](https://gizmodo.com/382026/a-cellphones-missing-dot-kills-two-people-puts-three-more-in-jail).
One way to resolve the ambiguity in typing is to rely on vowel harmony. Since
Turkish is an agglutinative language and suffixes can take different forms
depending on the preceding vowel, other grammatical forms of the words above
differ. For example, the infinitive forms "sıkılmak" (to be bored) and
"sikilmek" (to get f'ed) differ by the last vowel.
So even if the former word is spelled without diacritics, the reader can infer
the meaning and pronunciation from the ending.

Here are some other pairs of words that are hard to disambiguate when spelled without diacritics:

* *acı (pain)*, *açı* (angle)
* *acil *(urgent), *açıl* (open up)
* *hali *(state, condition), *halı* (carpet)
* kustu (threw up), küstü (offended by someone) ([thanks Melisa](https://twitter.com/MelisaOlgun/status/932110828366651392))

There are probably many more examples that I cannot think of right now. Feel
free to comment if you have any interesting examples in mind.

# How is this relevant to linguistics?

As far as I know, there is very little, if any, linguistic work on this phenomenon.
There is a software called "Turkish text deasciifier" that takes Turkish text
written without diacritics and tries to add diacritics.[^fn-1]
It is surprisingly successful, however it tries to disambiguate on based on a
patterns table. I personally use this tool when I absolutely need to type
something in proper Turkish.

Another topic that interests me greatly is, how well do we understand text
written without diacritics? How does it affect our speed of comprehension?
Does it differ according to ages? Is it something that is learned, or can
someone who has seen such a text for the first time understand it easily?
How much difficulty do the non-native speakers of the language have with texts
without diacritics? Especially if they are reading the text out loud, they have
to know all the words and the vowel harmony rules by heart. Considering
[Turkish can have really long words](https://en.wikipedia.org/wiki/Longest_word_in_Turkish) like
*Çekoslovakyalılaştıramadıklarımızdanmışsınız*, seeing it without diacritics,
  in the form *Cekoslovakyalilastiramadiklarimizdanmissiniz*, requires the
  reader to mentally parse the word before reading.

A relevant question is, what about the writing systems that have the extra
letters but often don't use them, even in formal writing?  For example, Russian
spelling often replaces *ё* (yo) with *е* (ye/e), even in newspapers.  Ottoman
Turkish had ك (k), گ (g) and ڭ (nasal n) but usually they were all just spelled
as ك (the first one). How is the comprehension speed affected then?

I'm curious about these questions and I'd really like to read more about how
diacritics affect comprehension, and how it affects non-native speakers. I
also wouldn't mind seeing how far machine learning can be taken to disambiguate
such texts.

[^fn-1]: Originally developed by Gökhan Tür, later inspired an open-source version developed by Deniz Yüret, which is then ported to many programming languages as executables, libraries and editor modes. ([Emacs Lisp](https://github.com/emres/turkish-mode), [Vim](https://github.com/joom/turkish-deasciifier.vim), [Python](https://github.com/emres/turkish-deasciifier), [Java](https://github.com/ahmetalpbalkan/turkish-deasciifier-java), [Scala](https://github.com/dbarisakkurt/turkish-deasciifier), [JavaScript](https://github.com/f/deasciifier), [Haskell](https://github.com/joom/turkish-deasciifier.hs), [Swift](https://github.com/ayberkt/deasciifier))
