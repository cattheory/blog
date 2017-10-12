---
title: Starting a Haskell student forum at Wesleyan
author: joomy
tags: haskell, wesleyan, teaching
description: I will be giving lectures on Functional Programming in Haskell next semester.
---

I recently learned that the student forum proposal [Max Dietz](http://twitter.com/MaximumDietz)
and I wrote was accepted, which means that Max and I will be giving lectures on
Functional Programming in Haskell in Fall 2015 semester.

Just in case the concept of a student forum is unclear, I should explain it.
A [student forum at Wesleyan](http://www.wesleyan.edu/registrar/enrollment/student_forum.html)
is a course for credit that is based on discussions led by students.
In a class like this, we plan to have a lecture based routine.

Even though the initial plan was accepted, I think it would be a good idea to
share the syllabus with the Haskell community and review the topics that we are
going to talk about in class. Here we go:

***


## Student Forum Proposal: Functional Programming in Haskell (Computer Science Course)

### Description, purpose and rationale

This class is an advanced course to learn advanced and real-life functional programming in Haskell.
At the end of the class, students will have fluent knowledge of the Haskell programming language,
and they will be able to write libraries, command line and web applications in Haskell.

This student forum is supposed to build on COMP212 Computer Science II.
Hence, it will be assumed that the students have a working knowledge of Standard ML
and basic functional programming concepts such as immutability and higher-order functions.
This will allow the class to move faster on concepts like pattern matching,
static typing, parametric polymorphism, type inference etc. and have more
interesting material towards the end of the course.

### Topics to be discussed, assignments and reading list

Certain parts of the course will be based on [UPenn CIS 194](http://www.seas.upenn.edu/~cis194/spring13/lectures.html).
“Real World Haskell” can be a good reading material for the course,
since it moves faster than other books. “Learn You a Haskell” is another good
book that we can use in certain areas, since it follows a more step-by-step approach.

### Weekly plan of the course

| Week | Topic | Reading | Assignment |
|:-----|:------|:--------|:-----------|
| 1 | Haskell syntax and base types | Learn Haskell Fast and Hard | - |
| 2 | Algebraic data types, records, pattern matching, lazy evaluation | [RWH Ch. 3](http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html) | Implementation of a priority queue |
| 3 | Higher-order functions, point-free style, type classes | - | Adding certain type classes (Monoid, Foldable) to the priority queue |
| 4 | Modules, Cabal basics. | [LYAH Ch. 7](http://learnyouahaskell.com/modules) | Converting the priority queue into a Cabal project with abstract types |
| 5 | Input and output (IO monad) | [LYAH Ch. 9](http://learnyouahaskell.com/input-and-output) | Write a command line tool. |
| 6 | Functors, applicative functors and monads | [Functors, Applicative Functors and Monads in Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html), [LYAH Ch. 12](http://learnyouahaskell.com/a-fistful-of-monads) | Implement a monad to keep the score in a game, write a simple game with it. |
| 7 | Reader, Writer, State monads | [Three Useful Monads](http://adit.io/posts/2013-06-10-three-useful-monads.html), [LYAH Ch. 13](http://learnyouahaskell.com/for-a-few-monads-more) | Update the game to use the monads learned this week. |
| 8 | Monad transformers | [A Gentle Introduction to Monad Transformers](https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md) | Add error handling with monad transformers to the game. |
| 9 | Parser combinators (Parsec) | [Error Handling - FP Complete](https://www.fpcomplete.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling), [RWH Ch. 16](http://book.realworldhaskell.org/read/using-parsec.html) | Writing a basic HTML or XML parser |
| 10 | Property-based testing with QuickCheck | [RWH Ch. 11](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html) | For two topological sort programs (one correct, one incorrect) write QuickCheck tests to find the minimal counterexample. |
| 11 | Lenses | [A Little Lens Starter Tutorial](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial) | Change your command line game to use lenses. |
| 12 | Yesod | [Yesod Book (as a reference)](http://www.yesodweb.com/book) | Basic website with CRUD. |
| 13 | Template Haskell | [Template Haskell 101](https://www.fpcomplete.com/user/marcin/template-haskell-101), [24 Days of GHC Extensions](https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html) | Use TH to create a [toJson](https://www.fpcomplete.com/user/Geraldus/algebraic-data-types-adts-with-aeson) function for basic ADTs |
| 14 | Other Language Extensions, working on projects. | - | Final Project |

### The method of evaluation of the work

There are going to be weekly assignments for the students that will be graded by the student leaders, also using automated tests if possible.  Code will be graded upon not only compilation and that it fulfills the specifications, but also that it makes good use of Haskell and FP style.  The goal is that by having students complete both simple Haskell code assignment and assignments that involve utilizing Haskell libraries they will feel comfortable using functional programming and Haskell in their future projects.

### The role of the faculty advisor

Students will consult [Prof. James Lipton](http://jlipton.web.wesleyan.edu) on the workload of the class, the grading policies, possible assignments and reading material.

Also, [Prof. Dan Licata](http://dlicata.web.wesleyan.edu/) was very nice to accept our suggestion to give a guest lecture on [view patterns](https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns), which he implemented.

***

This was the syllabus. If you have any comments, suggestions or criticisms, please let us know. Some of the topics we decided to talk about are not essential (such as Template Haskell) and even controversial. (ahem. lenses. ahem.) We would like to hear your opinion on that. Also, we are open to new homework assignment ideas. Current assignments are not very specific, and we tried to have a reasonable amount of workload for students by keeping the number of time consuming assignments low and having concise assignments in other weeks.
