# YangDotNet: Yang parser and type generator for .NET

Parser and .NET type generator for YANG model.

## Build instructions

Running the `.\build.bat` should do the job. The output will be placed in the `.\build` directory.
This uses the `FAKE` build system (which is based on F\#).

To run the unit tests, use `.\build.bat Test`.

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
