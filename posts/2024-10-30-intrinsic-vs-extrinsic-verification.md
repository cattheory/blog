---
title: Intrinsic vs. extrinsic verification
author: joomy
tags: history, proof assistants
description: Tracing the origin of the terms "intrinsic" and "extrinsic" in formal verification and programming languages.
---

<div class="gray">I wrote [a Mastodon post](https://functional.cafe/@joomy/112484764331818587) about this topic a few months ago, but when this topic came up when I was at [SPLASH 2024](https://2024.splashcon.org/) last week and I could not find my Mastodon post due to its unusable search feature (though to be fair searching in a federated setting is a tough problem), I decided to turn it into a quick, more permanent blog post I can easily find and share.</div>
<hr>

I am writing a paper and I once again found myself explaining intrinsic vs. extrinsic style of verification in dependently typed programs. (intrinsic = proof baked into the program, extrinsic = program and proof written separately) I never know what to cite for this concept, so I decided to dig a bit deeper to find the origin of these terms.

The first one that I found was John Reynolds’s 1998 book [Theories of Programming Languages](https://www.cs.cmu.edu/~jcr/tpl.html). In fact, in his 2004 paper [The Meaning of Types: From Intrinsic to Extrinsic Semantics](https://tidsskrift.dk/brics/article/view/20167/17787), he assures us that this book contains the first use of these terms by saying

> The terms 'intrinsic' and 'extrinsic' are recent coinages by the author[1, Chapter 15], but the concepts are much older. The intrinsic view is associated with Alonzo Church, and has been called 'ontological' by Leivant[2]. The extrinsic view is associated with Haskell Curry, and has been called 'semantical' by Leivant.

At this point this is still a type theory discussion about how to define the semantics of a language.

Another one I found was the paper by Benton, Hur, Kennedy and McBride from 2011, titled [Strongly Typed Term Representations in Coq](https://doi.org/10.1007/s10817-011-9219-0). They say in the abstract: 

> There are two approaches to formalizing the syntax of typed object languages in a proof assistant or programming language. The extrinsic approach is to first define a type that encodes untyped object expressions and then make a separate definition of typing judgements over the untyped terms. The intrinsic approach is to make a single definition that captures well-typed object expressions, so ill-typed expressions cannot even be expressed.  

This is now a discussion about mechanizing a language in proof assistants, which is closer to how I use it, but it’s still a type theory term at this point. It is referring to defining the terms of a language in a way that is well-typed by construction.

Another paper worth mentioning is by Affeldt and Sakaguchi, from 2014, titled [An Intrinsic Encoding of a Subset of C and its Application to TLS Network Packet Processing](https://jfr.unibo.it/article/view/4317/3950) They do, however, still use the term in the type theory sense, where they define C terms in an explicitly well-typed way. They also observe that 

> An intrinsic encoding also helps when developing the verification framework because having to deal with only correctly-typed programs reduces the number of error cases to be treated when developing the semantics and the related lemmas.

Right now the earliest paper I found that uses/contrasts the terms "intrinsic" vs. "extrinsic" outside of the type theory sense is the first paper I worked on when I was an undergrad. It is the 2016 paper [Intrinsic Verification of a Regular Expression Matcher](https://joomy.korkutblech.com/papers/regexp2016.pdf), where we explain this dichotomy (or rather, spectrum) for any program, and not just programming language interpreters. However, I remember getting the impression from [my advisor at the time](https://dlicata.wescreates.wesleyan.edu/) that this was folklore, so I think there must be some other paper out there that uses these terms in a general program verification way. Or maybe this isn't even a meaningful distinction? Any ideas?
