# YangDotNet: Yang parser and type generator for .NET

Parser and .NET type generator for YANG model.

## Pre-requisites

Currently, the dev environment is VS 2017 Preview, which means that we require F# compiler version 10.1.1.
Eventually, we will support the production tools (VS 2017 and F# compiler); hopefully, when we reach that
point the output binaries should also be of use with the tool-chain shipped with VS 2015. However, we do
not plan to test and support previous versions of Visual Studio.

## Build instructions

First, run `.\init.ps1` from PowerShell. This will download `paket` and setup a few other
things. It will also load the `YangDevHelper` PowerShell module that provides some commands
that are useful during development. It is advised (although not required) to initialize
your working (Power)shell with this script.

Running the `.\build.bat` should compile the main project and run the unit tests.
The output is placed in the `.\build` directory.
This uses the `FAKE` build system (which is based on F\#).
Alternatively, (if you initialize with the `init.ps1` script) you can also use
the commands `build` and `qb` for the full build, or a quick build (only the main project)
respectively.

To run the unit tests, use `.\build.bat Test`.
To get some statistics on the code coverage of the unit tests use the `Show-YangCodeCoverage`
command (this is part of `YangDevHelper`). To see a list of the active to-do items
use the command `Get-YangWorkItems`.


You can also open the Visual Studio solution file in `.\src\YANG\Yang.sln`. However, before doing
so do restore the dependencies if necessary. We use the `paket` system for package management.
So, you will need to do:

```cmd
.paket\paket.exe restore
```

## Status

Very early: Just skeleton files and pointers to interesting resources.

## Folder structure

This repository contains the following directories (in addition to `src` which contains the source code):

- `Models-External`: Contains repos to sources of YANG models. These are not linked
  as submodules, since some of them are quite big, and they are not always needed
  for development. To retrieve them run the `Download.ps1` PowerShell script from
  that directory.

- `Documents`: Contains various resources and notes related to the project.

- `Documents\References`: Contains useful reference material (e.g. specs).

The build output is written in:

- `build`: final binaries (release builds by default)

- `test`: binaries for testing (debug build)

## License

See the `LICENSE` file. We are using the Apache License.
Let us know, if you have other specific needs.
