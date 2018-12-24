# Update: #
 * 1-6 are done,  7 and 8 gives error on Monoid subgroup :(

# README #

This README would normally document whatever steps are necessary to get your application up and running.

### What is this repository for? ###

* Quick summary
* Version
* [Learn Markdown](https://bitbucket.org/tutorials/markdowndemo)

### How do I get set up? ###

* Summary of set up
* Configuration
* Dependencies
* Database configuration
* How to run tests
* Deployment instructions

### Contribution guidelines ###

* Writing tests
* Code review
* Other guidelines

### Who do I talk to? ###

* Repo owner or admin
* Other community or team contact

### Ways to compile haskell code:

* if you run the program on the fly:
  runhaskell helloworld.hs
  To feed a text file:
  cat haiku.txt | runhaskell helloworld.hs


* if you want to run with stack:
  ghci stack
  :load helloworld.hs
  main

* if the file does not have a module name: --preferred one
  ghc --make helloworld
  ./helloworld
  To feed a text file to the Haskell executable:
  cat haiku.txt | ./helloworld
