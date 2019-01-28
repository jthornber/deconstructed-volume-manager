- [ ] read up on 2 phase commit

Go into detail about how the dm activation and transaction manager ensure
atomicity between them.

- [ ] write process control library

  Should just be a thin wrapper around an existing Haskell lib.


- [ ] Write the transaction manager
- [ ] Write discover

Future
======

- [ ] Need to make sure we validate JSON properly

  There is a 'JSON Schema' specification.  No upto date Haskell lib though.
  Putting this on the back burner for now.
      
Done
====

- [X] select a format for passing rich data around.  This will
      be used for the config and metadata too.  Just use JSON?
      There will be Haskell libraries for it already, and it's
      more easily read by humans that the dreaded XML.
      
      JSON it is.
      
- [X] Choose JSON library.
      Aeson seems to be far and away the most popular.
      
