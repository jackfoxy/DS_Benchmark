##Overview

*DS\_Benchmark* is a system for benchmarking data structures in F#. It isolates actions to be benchmarked to the greatest extent possible during the timing process and produces tab-delimited timing information for import into Excel (see Future Direction below). 

Each timing cycle executes <del>100</del> 50 timings and takes the best <del>85</del> 40 to calculate min, max, median, and deviation. In other words the timings are not amortized, but most frequent best case.

##Usage

Execute Console1.exe with no parameter for help. Help is always complete and up-to-date with the latest documentation. (Usage in this readme file is not kept up-to-date.)

IMPORTANT NOTE: CD the bin folder from which you execute Console1 in the shell. Setting the environment path does not work for now because of temp file handling. Hope to fix this <del>soon</del> someday.

###Data Structure
The following structures are currently available for timings:

FSharp.Collections

>Array

>List

>Map

>Set

FSharpx.DataStructures

>BankersDeque (double-ended queue)

>BatchedDeque (double-ended queue)

>Deque (double-ended queue)

>RealTimeDeque (double-ended queue)

>DList (differenc list)

>LeftistHeap (heap)

>BankersQueue (queue)

>BatchedQueue (queue)

>BootstrappedQueue (queue)

>HoodMelvilleQueue (queue)

>ImplicitQueue (queue)

>PhysicistQueue (queue)

>RealTimeQueue (queue)

>AltBinaryRandomAccessList (random access list)

>BinaryRandomAccessList (random access list)

>SkewbinaryRandomAccessList (random access list)

>PersistentVector (vector)

>TransientVector (vector)

Naive Data Structures

>Stack

Power Pack

>HashmultiMap

>LazyList

###Size
Size, or count of elements in the initialization data.

###Initialization Data
A name consisting of three parts specifying the structure, data type, and ordering of the generated init data.

>**array...** -- F# array.

>**list...** -- F# list.

>**nocalcseq...** -- F# sequence, with precalculated string or integer values.

>**seq...** -- F# sequence constructed from range and/or function comprehensions. The "cost" of this in a timing is arbitrary and sometimes extreme.

>**...int...** -- Ordered integers, ranging from one to the requested count.

>**...string...** -- Ordered string data, ranging from one to 26 bytes long.

>**...asc** -- The int or string data is ordered ascending.

>**...dsc** -- The int or string data is ordered descending.

>**...rnd** -- The int or string data has been randomized. There is only a single occurrence of any value.

>**...rnddup** -- The int or string data has been randomized and there are two occurrences of every value (unless you request an odd number of data elements!).

###Action
>**addone** -- Start with an empty data structure and add elements from the initialization data one at a time, usually performed in a tail-recursive loop.

>**append** -- Initializes two structures of the same type and length of data (not within the timing) then appends one to the other.

>**init** -- Initializes the structure from the initialization data using the appropriate structure function member, typically create, ofArray, ofSeq, etc.

>**iterate** -- Initializes a structure (not within the timing) then iterates through every element, using a fold, if implemented by the structure, otherwise a tail recursive loop. No action is taken other than to assign the element to a value.

>**iterateseq** -- Initializes a structure (not within the timing) then does Seq.fold on a very simple function to excercise the structure's IEnumerable interface.

>**lookuprand** -- Initializes a structure (not within the timing) then performs the desired number of random lookups into the structure. Lookups are key-based for structures having a key, otherwise index based. If the number of lookups is not specified, the default is 10,000. A lookup does nothing more than assign the element to a value.

>**new()** -- Available only for data structures that have a constructor which consumes the initialization data.

> **tailtoempty** -- Initializes a structure (not within the timing) then iterates tail until empty."

>**updaterand** -- Similar to lookuprand, except the action updates the element found.

###Generating timing scripts from template
Script template parameters are ! delimited.

dataStructure!size!initData!action!additional parameters

Example:<br>
".\*!1000!.\*?dsc.\*|.\*?rnd.\*!new.\*|init"

>**dataStructure** -- regular expression

>**size** -- integer

>**initData** --  regular expression

>**action** --  regular expression

>**additional parameters** -- (optional) used by lookuprand and updaterand Actions to control number of lookups/updates, defaults to 10,000  

###Output
Output to console for individual timings, or to tab-delimited file for multiple.

>Data Structure name 

>Initialization Data name (the name is descriptive of the data structure, type, and composition)

>Size (number of elements in the initialization data)

>Action

>Operator (description of the mechanism the data structure uses to perform the action)

>Max (most ticks timed to perform the action in 85 of 100 tries)

>Min (least ticks timed)

>Median (ticks)

>Max Deviation (ticks)

>Max Deviation Pct (relative to median)

##Architecture

**Bench** -- Console app project executed from the DS\_Benchmark library to execute one individual timing.

**Console1** -- Documentation and several useful working examples.

**DS\_Benchmark** -- Library of all the major functionality. Calls Bench.exe to execute individual timings, which in turn calls back into this library.

>Infrastructure.fs -- Common types and utility functions.

>(library name).fs -- Module for each structure in the library with the routines to do action timings.

>Benchmark.fs -- functions to generate initialization data and construct and route timing requests.

>Generators.fs -- Generates timing scripts from templates and runs scripts.

**DS\_Benchmark.Tests** -- Unit tests.

**NaiveDataStructures** -- Library of "naive" functional data structures. Use this library as a sandbox for testing data structures. For instance copy source code for FSharpx.Core.DataStructure structures to this library to profile alternate algorithms against the current FSharpx.

##Future Direction
Persist timing results. (high priority)

Automate analysis. (high priority)

Consume multiple script templates.

Generate multiple sizes from one script template.

Integrate FsCheck input generations. This was my original intention, but I dove into the project without learning FsCheck well enough.

Use reflection to suppress unsupported actions in script generation.

##Contribute to the project
Contact me if you are interested in contributing to the project. I will consider pull requests along the following guidelines:

>Adding new data structures and actions is encouraged.

>Try to be as faithful as possible to the existing coding style. 

>When there are no ordering dependencies, order types and members in alphabetic order.

>Use meaningful value names (I know I'm not always faithful to this).

###Adding a Data Structure

>1) Create file for data structure library, if needed, and position before benchmark.fs in DS\_Benchmark project. Add module for new data structure and as many getTime... let bindings as necessary to accommodate Array, List, and Sequence initialization data collections. Include logic for all Actions to be recognized.

>2) In Infrastructure.fs add static member to type DataStructure for the new data structure.

>3) In Benchmark.fs, Benchmark module's getTime let binding, add matches for the new getTime... logic.

>5) Create file in test project for data structure library, if needed. Add unit tests for new data structure (should be able to clone existing tests).

###Adding an Action:

>1) In Infrastructure.fs add new action literal to type Action.

>2) Add new operators to Operator type in Infrastructure.fs.

>3) In the respective data structure library files (CoreCollections.fs, FSharpx.fs, etc.), for each data structure implementing the action in it's respective module add a match for the new action in each "getTime" function (some data structures have different functions for initialization from different kinds of collections).

>4) For type actions resulting in a data structure construction pass the resulting data structure as the result to Utility.getTimeResult. Otherwise pass the integer count of actions performed or count (or length) of items in the resulting data structure, or pass to Utility.getTime the function performing an operation on the data structure.

>5) Add unit test for new action for each data structure.

##Open Issues:

>1) Ugly and slow random and random duplicate logic. Need to create random stack data structure.

>2) Despite over 200 unit tests, much code is still uncovered. 

>3) Create a build script (Fake?) to put the Bench.exe in the DS\_Benchmark and Console1 bins and have DS\_Benchmark look for it there first. That would be a little cleaner than the current implementation.

>4) Eliminate "for" loops in favor of recursion.

##Release Notes

###v1.4.1

>1) referencing FSharpx.Collections.Experimental

###v1.4

>1) corrected FSharpx.Core references to v1.7

###v1.4

>1) Further architectural clean-up in conjuction with benchmarking recent FSharpx.Collections.

>2) NaiveDataStructures has evolved into my data structure sandbox. Take anything there "with a grain of salt".

>3) You will need to manually reference FSharpx.Core in DS_Benchmark and DS_Benchmark.Tests to the latest NuGet package.

>4) As always reference the latest documentation in Console1.exe

###v1.3

>1) Architectural clean-up.

>2) Continue removing structures from PurelyFunctionalDataStructures as they are added to FSharpx.Core.DataStructures.

>3) More data structures from FSharpx.Core.DataStructures.

>4) Scripts now support comments.

>5) Added integration_test.txt script to root.

###v1.2

>1) Removing structures from PurelyFunctionalDataStructures as they are added to FSharpx.Core.DataStructures.

>2) Several new actions.

>3) Added Dequeus from FSharpx

###v1.1

>1) New project added: PurelyFunctionalDataStructures is Julien's implementation of most of the structures in Okasaki, 1998. Known issue is Release build of this project does not work with Optimize Code. Apparently some of Julien's usages of Lazy do not work correctly under F# 3.0. I have not debugged further. No structures currently implemented for getTime.

>2) AltBinaryRandomAccessList (under project NaiveDataStructures, not PurelyFunctionalDataStructures) implemented for getTime.

>3) Several new Actions added to Core List and/or AltBinaryRandomAccessList, but only for load by arrayintasc data. Access console1 from command line with no parameters for current documentation.