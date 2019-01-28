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
==========

replicator
----------

The replicator is responsible for storing the same block of data in
multiple disk regions in a transactional manner.

The number of regions can get very high for enterprise installations (many
thousands), so we need a way of passing in a JSON file containing regions.
We could pass the JSON via stdin, which would be good for testing; but
I suspect that when we deploy we will want to log all input and output
between processes.

dvm_replicate read <region file> <dest path>
dvm_replicate prepare <region file> <source path>
dvm_replicate abort <region file>
dvm_replicate commit <region file>

'prepare' should always be followed by either 'abort' or 'commit'.  If we're
in a prepared state we should make it clear to the user.  Should we deny reads?
Would the controller ever need to read in order to complete recovery?  I suspect
we could always abort.

If abort fails some regions will be left in the prepared state and
recovery will need human intervention (eg, failing device, read only
device).

'prepare' will examine the new file, calculating a hash for each page.
Then write new pages to each region and prepare a shadow table.

commit copies the shadow table over the active table.

abort wipes the shadow table.

Region split into 4k blocks.  First block is superblock.  Page tables in
separate blocks (with identifiers and timestamps).  Superblock is less
than 512 bytes so we get an atomic disk write.  SB points to active
page table.  Also records total length so the file doesn't have to be
a multiple of 4k.

We do NOT store any expectations of how many regions etc there are in
the superblocks.  The SBs are just concerned with page allocation for
the current region.

There is no need for any form of recovery.  If the regions are out of sync all
operations will fail.  User can recover by calling replicate with suitable subsets
of regions.

discovery
---------

Interfaces with OS to be aware of devices (eg, udev).  Scans for labels.  Builds
region lists.  Spots missing PVs.  Potential to be stateful, so could be a pain.

Use regex filters like in LVM.

    discover find my-volume-group
    
This component manages labels.

    discover add <vg> <dev> [<region>*]
    discover rm <vg> <dev>

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
    phase 1 commit, phase 1 activate, phase 2 commit, phase 2 activate.
    
    
    
  expand
  reduce
    
All commands read from stdin, <devs> <vg name>, which is provided by the discovery
unit.

metadata_report
---------------

May be more than one excutable.

Reports on the current configuration.  Also needs to read status of dm devs
to give activation state.

No update, so this can be implemented as a simple pipeline:

  discover <vg name> | xargs tm read <vg name> | report <vg name>

Tool config
-----------

Tools need config to be passed to them.  This is specified on their
command line using the -c <config> option.  The config contains sections
of common config (eg, data formats to use), and tool specific options (eg, 
only discovery needs to know about the dev filter regexes).

The tools do not look in a standard place for the config unlike lvm which
always uses /etc/lvm/config.  This helps keep them stateless, and simplifies
testing.  There will be a top level wrapper script 'dvm' that is allowed to
introduce state, so long as it explicitly passes it down to the underlying
tools.




