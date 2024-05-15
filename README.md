# yang-fsharp: Yang parser and type generator for .NET

Parser and .NET type generator for YANG model.

Fork of YangDotNet. The original project and solution files only worked with Visual Studio and the
target framework was .NET Framework which targets Windows only. This project targets .NET 8 which is
platform independent.

## Pre-requisites

.net 8 in a Linux host.
paket - install `dotnet tool install paket`

## Build instructions


### After changing paket.dependencies

```
dotnet paket install
```

### After cloning or switching branches

```
dotnet paket restore
```

### After changing the source code

```
dotnet build
```

## Status

Modeling of YANG is complete at the statement level. We need to finish other constructs,
such as conditionals, etc. Overall, most of the YANG language is already modelled accurately.
There is also some basic support for processing statements, which will be enriched with time.

The very basic parser that parses YANG models without interpreting them, can correctly
parse an extensive set of models (over 12K files), which exist in the `Models-External`
directory. We have identified a few problems, but these are due to incorrect YANG models,
and there are fixes for them. We may need to consider creating a robust extension that
pre-processes YANG models and corrects a few of the common errors.

The proper parser parses Juniper configuration models (version 16.x), excluding the
constructs that are not currently captured by the model (see above).

For the model processing pipeline, there are some tools in various levels of maturity.
There is support for discovering type and group definitions and use; this tool will need
to be extended to also discover prefixes, and to also deal with sub-modules.
There is some preliminary work on a tool that will simplify XML parsing for the purpose
of mapping data to objects.

The C\# source code generator is in early phases. Currently, the goal is to specify
what we want to achieve which is both end-user friendly, but also easy to generate
automatically. The Type Provider is in an earlier stage at the moment.

## Folder structure

This repository contains the following directories (in addition to `src` which contains the source code):

- `Models-External`: Contains repos to sources of YANG models. These are not linked
  as submodules, since some of them are quite big, and they are not always needed
  for development. To retrieve them run the `Download.ps1` PowerShell script from
  that directory.

- `Documents`: Contains various resources and notes related to the project.

- `Documents\References`: Contains useful reference material (e.g. specs).

- `Reports`: Automatically generated reports of unit tests and test coverage are
             placed in this directory.

The build output is written in:

- `build`: final binaries (release builds by default)

- `test`: binaries for testing (debug build)

The rest of the directories are:

- `node_modules`: Use by `node.js` to put some structure on the `git commit` messages.

- `packages`: Used by `paket` and contains external packages used for development.

- `paket-files`: Also used by `paket` and contains external source code.

## Source code pointers

The main file that contains our model of the YANG language is
**`src\YANG\Model\Statements.fs`**.

For the YANG parser the gist of the implementation is in:

- **`src\YANG\Parser\Statements.fs`**, and

- **`src\YANG\Parser\BodyStatements.fs`**

## License

See the `LICENSE` file. We are using the Apache License.
Let us know, if you have other specific needs.
