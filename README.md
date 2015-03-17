# DelphiWebScript Project #

DWScript is an object-oriented scripting engine for Delphi based on the [Delphi](http://en.wikipedia.org/wiki/Embarcadero_Delphi) language, with extensions borrowed from other Pascal languages ([FreePascal](http://www.freepascal.org/), [Prism](http://prismwiki.codegear.com/en/Main_Page), etc.). It introduces a few Pascal language extensions of its own as well.

Goals of the project go beyond web usages, and cover general purpose scripting.

To get started see FirstSteps or [Language], for news see the [posts tagged "DWS"](http://delphitools.info/tag/dws/) at [DelphiTools.info](http://delphitools.info/). You can also have a look at the [FAQ]. If you have other questions related to DWScript programming, usage or integration, [StackOverflow](http://stackoverflow.com/questions/tagged/dwscript) is a good place to ask them. For bugs, use the [Issues tracker](https://bitbucket.org/egrange/dwscript/issues).

This project is currently maintained by Eric Grange (http://delphitools.info), and sponsored by [Creative IT](http://creative-it.net), it is based on original DWS2 by Matthias Ackermann (http://sourceforge.net/projects/dws/) initiated way back in the last century.

DWScript lead platform is currently Delphi XE, compatibility with Delphi 2009 & Delphi 2010 is maintained, but some features may be restricted (RTTI f.i.). Delphi XE2, XE3 & XE4 are supported as well.

## Licensing ##

DWScript is released under Mozilla Public License 1.1 which, in short, means that it's free to use in open or closed source projects, free or commercial, provided you honor the following requirements:
 * any application that makes use of DWScript, or is compiled by DWScript should "conspicuously" include a reference to DWScript in its credits, and include or link to the DWScript source files.
 * modification to the source have to be made public under MPL.
If you can't or don't want to satisfy the above requirements, contact the project manager for use under other license schemes.

## DWScript 2.3 ##

DWS 2.3 is the development version.

Plans for 2.3 are to improve on the language as well as the support tools (mini-IDE, docs etc.).

Unit test coverage goal for 2.3 is 95% overall, with core units at 100%.

## DWScript 2.2 ##

DWS 2.2 is current "stable" release version.

It features improved threading support (simultaneous executions per program), improved error reporting (incl. script-side stack traces), contracts programming (in progress), higher level debugger support classes, and a variety of language enhancements.

Unit test coverage for 2.2 is 90% overall.

## DWScript 2.1 ##

DWS 2.1 is available from the [downloads](http://code.google.com/p/dwscript/downloads/list) page. This version adds a lot of language features over original DWS2, as well as significant execution speedups, reduced memory usage, many fixes and improved error detection. 

There is also now a growing unit test suite, which as of 2.1 ensures a [code coverage](http://code.google.com/p/delphi-code-coverage/) of 76%.

It can be found in the SVN under "branches/stable-2.1".