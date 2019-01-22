Notes on the deconstructed volume manager
==========================================

- Split a _minimal_ volume manager into separate, small processes
  that have clearly defined tasks.
- Try and keep these processes stateless (eg, metadata is passed around) to aid
  testing.
- Write in Haskell because it's the language that I think most clearly in.  Less
  boiler plate.
- Not trying to recreate the lvm command line.  This is meant to be a tiny tools.
- Not supporting clusters.
- Errors are indicated by the return code of the child processes.
- rich data needs to be passed around between processes.  Use the javascript thing.


Components
============

dmexec
------

Low level driver of device mapper.  Executes simple sequences of dmsetup 
style commands.  Can branch depending on success of these operations, allowing
recovery to be specified up front.  Need some form of reporting, to indicate
failures and return status.

I think this is the only program that needs to be memory locked.

dmactivate
----------

Builds the dmexec programs given a representation of the current dm state, and
the desired dm state.  Much like libdevmapper.so.

vol_activate
------------

Builds desired dm states from higher level abstractions like LVs.

metadata_manip
--------------

This is actually a suite of tools that manipulate the metadata.
eg,
  extend and lv
  create an lv
  create a mirror
  etc.
  
transaction_manager
--------------------

Handles ACID for the metadata.  Does not understand the metadata.  Uses
same ring buffer approach as LVM.  Needs to implement 2 phase commit to
sync with dm.

commands:
  verify <devs> <vg name>  - checks a set of devs provide a complete vg
  recover <devs> <vg name>  - rewrites metadata to handle missing PVs
  read <devs> <vg name>  - verifies, then prints metadata on stdout
  update <devs> <vg name> <mutator process>
    verifies, forks mutator, writes metadata to mutator, reads new metadata,
    commits.
    
All commands read from stdin, <devs> <vg name>, which is provided by the discovery
unit.

metadata_report
---------------

May be more than one excutable.

Reports on the current configuration.  Also needs to read status of dm devs
to give activation state.

No update, so this can be implemented as a simple pipeline:

  discover <vg name> | xargs tm read <vg name> | report <vg name>

discovery
---------

Nasty discovery code goes in here.



