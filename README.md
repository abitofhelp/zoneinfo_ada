# Starter Library with Hybrid DDD/Clean/Hexagonal Architecture

[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE) [![Ada](https://img.shields.io/badge/Ada-2022-blue.svg)](https://ada-lang.io) [![SPARK](https://img.shields.io/badge/SPARK-Proved-brightgreen.svg)](https://www.adacore.com/about-spark) [![Alire](https://img.shields.io/badge/Alire-2.0+-blue.svg)](https://alire.ada.dev)

**Version:** 1.0.0  
**Date:** December 02, 2025  
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>  
**Status:** Released  

> A canonical Ada 2022 library demonstrating hexagonal architecture with functional error handling, SPARK-compatible design, and embedded-safe patterns.

## Overview

zoneinfo is a demonstration library showcasing **hybrid DDD/Clean/Hexagonal architecture** with dependency inversion, ports & adapters, and Result monad error handling in Ada 2022. This is a library-only crate designed to be embedded in applications, with support for both desktop and embedded platforms.

## SPARK Formal Verification

<table>
<tr>
<td width="120"><strong>Status</strong></td>
<td><img src="https://img.shields.io/badge/SPARK-Proved-brightgreen.svg" alt="SPARK Proved"></td>
</tr>
<tr>
<td><strong>Scope</strong></td>
<td>Domain + Application Layers</td>
</tr>
<tr>
<td><strong>Mode</strong></td>
<td>gnatprove --mode=check (SPARK legality verified)</td>
</tr>
</table>

The **domain and application layers** are formally verified using SPARK Ada, providing mathematical guarantees of:

- **No runtime errors** - Division by zero, overflow, range violations
- **No uninitialized data** - All variables properly initialized before use
- **Contract compliance** - Pre/postconditions proven correct
- **Data flow integrity** - No aliasing or information flow violations

### Verification Command

```bash
make spark-check    # Run SPARK legality verification
```

### Verified Packages

| Layer | SPARK_Mode | Description |
|-------|-----------|-------------|
| `Domain.*` | On | Value objects (Instant, Zoned, Civil, etc.) |
| `Application.Command.*` | On | Commands |
| `Application.Port.*` | On | Inbound/outbound port interfaces |
| `Application.UseCase.*` | On | Use case implementations |

Infrastructure and API layers use `SPARK_Mode => Off` as they perform I/O operations.

## Getting Started

### Clone with Submodules

This repository uses git submodules for shared tooling. Clone with:

```bash
git clone --recurse-submodules https://github.com/abitofhelp/zoneinfo.git
```

Or if already cloned without submodules:

```bash
git submodule update --init --recursive
# Or: make submodule-init
```

## Features

- ✅ 4-layer hexagonal architecture (Domain, Application, Infrastructure, API)
- ✅ Public API facade with stable interface
- ✅ Generic I/O plugin pattern for platform portability
- ✅ Result monad error handling (via `functional` crate)
- ✅ Embedded safety restrictions (no implicit heap allocations)
- ✅ Static dispatch via generics (zero runtime overhead)
- ✅ Desktop platform support (Console I/O)
- ✅ Library_Standalone with explicit Library_Interface

## Platform Support

| Platform | Status | Notes |
|----------|--------|-------|
| **Desktop** | ✅ Full | Console I/O via `API.Desktop` |
| **Embedded** | 🔧 Custom | Requires Writer port implementation |

### Embedded Platform Support

This library uses a **three-package API pattern** with dependency injection for platform portability:

| Package | Purpose |
|---------|---------|
| `API.Operations` | Generic operations (SPARK-safe, no I/O dependencies) |
| `API.Desktop` | Composition root for desktop (Console_Writer) |
| `API` | Public facade (uses Desktop by default) |

**Default**: Desktop platforms use console I/O via `API.Desktop`.

**For embedded platforms**, create your own composition root:

```ada
--  1. Implement the Writer port for your platform
function UART_Write (Message : String) return Unit_Result.Result;

--  2. Instantiate operations with your writer
package Embedded_Ops is new Zoneinfo.API.Operations (Writer => UART_Write);

--  3. Use operations directly
Result : constant Unit_Result.Result := Embedded_Ops.Greet (Cmd);
```

See **[All About Our API](docs/guides/all_about_our_api.md)** for detailed architecture and implementation guidance.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Zoneinfo.API                       │
│              (Public Facade - Stable Interface)             │
├─────────────────────────────────────────────────────────────┤
│  API.Operations     │     API.Desktop     │   (API.Embedded)│
│  (Generic I/O)      │ (Console_Writer DI) │   (Future UART) │
├─────────────────────┼─────────────────────┼─────────────────┤
│                    Application Layer                        │
│     Use Cases  │  Ports (Writer)  │  Commands (Greet)       │
├─────────────────────────────────────────────────────────────┤
│                   Infrastructure Layer                      │
│              Adapters (Console_Writer)                      │
├─────────────────────────────────────────────────────────────┤
│                      Domain Layer                           │
│   Value Objects (Person) │ Errors │ Unit │ Result Monad    │
└─────────────────────────────────────────────────────────────┘
```

## Quick Start

### Building

```bash
# Build debug library
make build

# Build release library
make build-release

# Using Alire directly
alr build
```

### Using in Your Project

Add to your `alire.toml`:

```toml
[[depends-on]]
zoneinfo = "*"
```

In your Ada code:

```ada
with Zoneinfo.API;

procedure Main is
   use Zoneinfo.API;

   --  Create a greet command
   Cmd : constant Greet_Command := Create_Greet_Command ("World");

   --  Execute the greeting operation
   Result : constant Unit_Result.Result := Greet (Cmd);
begin
   if Unit_Result.Is_Ok (Result) then
      --  Success! Message was printed to console
      null;
   else
      --  Handle error
      declare
         Err : constant Error_Type := Unit_Result.Error_Info (Result);
      begin
         --  Process error...
         null;
      end;
   end if;
end Main;
```

## Quick Snippets

All operations use `Zoneinfo.API` and return Result types. See `/examples` for complete programs.

```ada
with Zoneinfo.API; use Zoneinfo.API;

--  Create a validated person
Result : constant Person_Result.Result := Create_Person ("Alice");

--  Get person's name
Name : constant String := Get_Name (Person);

--  Create a greet command
Cmd : constant Greet_Command := Create_Greet_Command ("World");

--  Execute the greeting operation
Result : constant Unit_Result.Result := Greet (Cmd);

--  Custom I/O adapter (for embedded platforms)
package My_Ops is new Zoneinfo.API.Operations (Writer => UART_Write);
```

## Testing

```bash
# Run all tests
make test-all

# Build tests
make build-tests

# Run unit tests only
./test/bin/unit_runner

# Run integration tests only
./test/bin/integration_runner
```

## Documentation

- 📚 **[Documentation Index](docs/index.md)** - Full documentation
- 🚀 **[Quick Start Guide](docs/quick_start.md)** - Get started in minutes
- 🏗️ **[All About Our API](docs/guides/all_about_our_api.md)** - API architecture and platform customization
- 📋 **[Software Requirements](docs/formal/software_requirements_specification.md)** - Formal requirements
- 📐 **[Software Design](docs/formal/software_design_specification.md)** - Architecture details
- 🧪 **[Software Test Guide](docs/formal/software_test_guide.md)** - Testing strategy
- 📝 **[CHANGELOG](CHANGELOG.md)** - Release history

## Code Standards

This project follows:
- **Ada Agent** (`~/.claude/agents/ada.md`) - Ada 2022 standards
- **Architecture Agent** (`~/.claude/agents/architecture.md`) - DDD/Clean/Hexagonal
- **Functional Agent** (`~/.claude/agents/functional.md`) - Result/Option patterns
- **SPARK Agent** (`~/.claude/agents/spark.md`) - Embedded safety patterns

## Submodule Management

This project uses git submodules for shared Python tooling:

- `scripts/python` - Build, release, and architecture scripts
- `test/python` - Shared test fixtures and configuration

### Workflow

```
hybrid_python_scripts (source repo)
         │
         │ git push (manual)
         ▼
      GitHub
         │
         │ make submodule-update (in each consuming repo)
         ▼
┌─────────────────────────────────┐
│  1. Pull new submodule commit   │
│  2. Stage reference change      │
│  3. Commit locally              │
│  4. Push to remote              │
└─────────────────────────────────┘
```

### Commands

```bash
# After fresh clone
make submodule-init

# Pull latest from submodule repos
make submodule-update

# Check current submodule commits
make submodule-status
```

### Bulk Update (all repositories)

```bash
python3 ~/Python/src/github.com/abitofhelp/git/update_submodules.py

# Options:
#   --dry-run   Show what would happen without changes
#   --no-push   Update locally but do not push to remote
```

## Contributing

This project is not open to external contributions at this time.

## AI Assistance & Authorship

This project — including its source code, tests, documentation, and other deliverables — is designed, implemented, and maintained by human developers, with Michael Gardner as the Principal Software Engineer and project lead.

We use AI coding assistants (such as OpenAI GPT models and Anthropic Claude Code) as part of the development workflow to help with:

- drafting and refactoring code and tests,
- exploring design and implementation alternatives,
- generating or refining documentation and examples,
- and performing tedious and error-prone chores.

AI systems are treated as tools, not authors. All changes are reviewed, adapted, and integrated by the human maintainers, who remain fully responsible for the architecture, correctness, and licensing of this project.

## License

Copyright © 2025 Michael Gardner, A Bit of Help, Inc.

Licensed under the BSD-3-Clause License. See [LICENSE](LICENSE) for details.

## Author

Michael Gardner
A Bit of Help, Inc.
https://github.com/abitofhelp

## Project Status

**Status**: Released (v1.0.0)

- ✅ Core library structure
- ✅ 4-layer hexagonal architecture
- ✅ Public API facade with three-package pattern
- ✅ Desktop platform support (Console_Writer)
- ✅ Full test suite (see CHANGELOG)
- ✅ Comprehensive documentation
- ✅ SPARK_Mode boundaries defined
- ⬜ Embedded platform composition roots (documented, not yet implemented)
- ✅ Alire publication
