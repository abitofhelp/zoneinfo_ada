# Software Design Specification (SDS)

**Version:** 2.0.0  
**Date:** December 10, 2025  
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>  
**Status:** Released  

---

## 1. Introduction

### 1.1 Purpose

This Software Design Specification (SDS) describes the internal architecture, package structure, and design decisions for **Hybrid_Lib_Ada**.

### 1.2 Scope

This document covers:
- 4-layer hexagonal architecture
- Package hierarchy and dependencies
- Type definitions and contracts
- Static dependency injection via generics
- SPARK verification boundaries

### 1.3 References

- Software Requirements Specification (SRS)
- [All About Our API](../guides/all_about_our_api.md) - Detailed API architecture guide
- Ada 2022 Reference Manual
- SPARK 2014 Reference Manual

---

## 2. Architectural Overview

### 2.1 Layer Architecture

Hybrid_Lib_Ada uses a **4-layer library architecture** (Domain, Application, Infrastructure, API):

```
        Consumer Application
                ↓
        ┌───────────────────────────────────────────────┐
        │              API LAYER (Public Facade)        │
        │  ┌─────────────────┬────────────────────────┐ │
        │  │    api/         │    api/desktop/        │ │
        │  │  (facade)       │  (composition root)    │ │
        │  │  + operations/  │  Wires infrastructure  │ │
        │  │                 │                        │ │
        │  │  Depends on:    │  Depends on:           │ │
        │  │  App + Domain   │  ALL layers            │ │
        │  └─────────────────┴────────────────────────┘ │
        └───────────────────────┬───────────────────────┘
                                │
        ┌───────────────────────▼───────────────────────┐
        │              INFRASTRUCTURE LAYER             │
        │  Adapters: Console_Writer                     │
        │  Implements ports defined in Application      │
        │  Depends on: Application + Domain             │
        └───────────────────────┬───────────────────────┘
                                │
        ┌───────────────────────▼───────────────────────┐
        │               APPLICATION LAYER               │
        │  Use Cases: Greet | Commands: Greet_Command   │
        │  Ports: Writer (outbound)                     │
        │  Depends on: Domain only                      │
        └───────────────────────┬───────────────────────┘
                                │
        ┌───────────────────────▼───────────────────────┐
        │                 DOMAIN LAYER                  │
        │  Value Objects: Person | Error: Result monad  │
        │  Depends on: NOTHING (zero dependencies)      │
        └───────────────────────────────────────────────┘
```

### 2.2 Dependency Rules

**Critical**: The API layer has TWO distinct areas with different dependency rules:

| Component | May Depend On |
|-----------|---------------|
| Domain | Nothing (zero dependencies) |
| Application | Domain only |
| Infrastructure | Application, Domain |
| **API facade (`api/`)** | **Application + Domain ONLY** |
| **API composition roots (`api/desktop/`)** | ALL layers (including Infrastructure) |

The API facade (`Hybrid_Lib_Ada.API`, `API.Operations`) re-exports types and provides the public interface but does NOT import Infrastructure. The composition root (`API.Desktop`) wires Infrastructure adapters to the generic Operations.

### 2.3 Hexagonal Pattern

```
           ┌──────────────────────────────────────┐
           │          Application Core            │
           │                                      │
    ┌──────┤  Domain ← Application               │
    │      │                                      │
    │      └──────────────────────────────────────┘
    │                       │
    │                       │ Ports
    ▼                       ▼
┌────────┐           ┌────────────┐
│ Writer │◄──────────│ Greet Use  │
│  Port  │           │   Case     │
└────────┘           └────────────┘
    ▲
    │ Implements
┌────────────────┐
│ Console_Writer │ (Infrastructure)
└────────────────┘
```

---

## 3. Package Structure

### 3.1 Directory Layout

```
src/
├── hybrid_lib_ada.ads              # Root package
├── version/
│   └── hybrid_lib_ada-version.ads  # Version information
│
├── domain/
│   ├── domain.ads                  # Domain layer root
│   ├── domain-unit.ads             # Unit type (void equivalent)
│   ├── error/
│   │   ├── domain-error.ads        # Error type definition
│   │   └── domain-error-result.ads # Generic Result monad
│   └── value_object/
│       ├── domain-value_object.ads
│       ├── domain-value_object-option.ads
│       └── domain-value_object-person.ads
│
├── application/
│   ├── application.ads             # Application layer root
│   ├── error/
│   │   └── application-error.ads   # Re-exports Domain.Error
│   ├── command/
│   │   ├── application-command.ads
│   │   └── application-command-greet.ads
│   ├── port/
│   │   ├── application-port.ads
│   │   ├── inbound/
│   │   │   └── application-port-inbound.ads
│   │   └── outbound/
│   │       ├── application-port-outbound.ads
│   │       └── application-port-outbound-writer.ads
│   └── usecase/
│       ├── application-usecase.ads
│       └── application-usecase-greet.ads
│
├── infrastructure/
│   ├── infrastructure.ads          # Infrastructure layer root
│   └── adapter/
│       ├── infrastructure-adapter.ads
│       └── infrastructure-adapter-console_writer.ads
│
└── api/
    ├── hybrid_lib_ada-api.ads      # Public facade
    ├── hybrid_lib_ada-api.adb
    ├── operations/
    │   ├── hybrid_lib_ada-api-operations.ads  # SPARK-safe
    │   └── hybrid_lib_ada-api-operations.adb
    └── desktop/
        ├── hybrid_lib_ada-api-desktop.ads     # Composition root
        └── hybrid_lib_ada-api-desktop.adb
```

### 3.2 Package Descriptions

#### 3.2.1 Domain Layer

| Package | Purpose | SPARK |
|---------|---------|-------|
| `Domain` | Layer root | On |
| `Domain.Unit` | Unit type for void operations | On |
| `Domain.Error` | Error type with Kind + Message | On |
| `Domain.Error.Result` | Generic Result[T] monad | On |
| `Domain.Value_Object` | Value object root | On |
| `Domain.Value_Object.Option` | Option[T] monad | On |
| `Domain.Value_Object.Person` | Person value object | On |

#### 3.2.2 Application Layer

| Package | Purpose | SPARK |
|---------|---------|-------|
| `Application` | Layer root | On |
| `Application.Error` | Re-exports Domain.Error | On |
| `Application.Command` | Command root | On |
| `Application.Command.Greet` | Greet command DTO | On |
| `Application.Port` | Port root | On |
| `Application.Port.Inbound` | Inbound port root | On |
| `Application.Port.Outbound` | Outbound port root | On |
| `Application.Port.Outbound.Writer` | Writer port definition | On |
| `Application.Usecase` | Use case root | On |
| `Application.Usecase.Greet` | Greet use case | On |

#### 3.2.3 Infrastructure Layer

| Package | Purpose | SPARK |
|---------|---------|-------|
| `Infrastructure` | Layer root | Off |
| `Infrastructure.Adapter` | Adapter root | Off |
| `Infrastructure.Adapter.Console_Writer` | Console output adapter | Off |

#### 3.2.4 API Layer

| Package | Purpose | SPARK |
|---------|---------|-------|
| `Hybrid_Lib_Ada` | Library root | Off |
| `Hybrid_Lib_Ada.Version` | Version information | Off |
| `Hybrid_Lib_Ada.API` | Public facade | Off |
| `Hybrid_Lib_Ada.API.Operations` | SPARK-safe operations | On |
| `Hybrid_Lib_Ada.API.Desktop` | Desktop composition root | Off |

---

## 4. Type Definitions

### 4.1 Domain Types

#### 4.1.1 Error_Kind

```ada
type Error_Kind is
  (Validation_Error,  -- Input validation failed
   Parse_Error,       -- Malformed data/parsing failures
   Not_Found_Error,   -- Resource not found
   IO_Error,          -- I/O operation failed
   Internal_Error);   -- Unexpected internal error
```

#### 4.1.2 Error_Type

```ada
type Error_Type is record
   Kind    : Error_Kind;
   Message : Error_Strings.Bounded_String;  -- Max 512 chars
end record;
```

#### 4.1.3 Result (Generic)

```ada
generic
   type T is private;
package Domain.Error.Result.Generic_Result is
   type Result is private;

   function Ok (Value : T) return Result;
   function Error (Kind : Error_Kind; Message : String) return Result;

   function Is_Ok (R : Result) return Boolean;
   function Is_Error (R : Result) return Boolean;
   function Value (R : Result) return T
     with Pre => Is_Ok (R);
   function Error_Info (R : Result) return Error_Type
     with Pre => Is_Error (R);
end Generic_Result;
```

#### 4.1.4 Person

```ada
type Person is private;

function Create (Name : String) return Person_Result.Result;
function Get_Name (P : Person) return String;
function Is_Valid_Person (P : Person) return Boolean;
```

### 4.2 Application Types

#### 4.2.1 Greet_Command

```ada
type Greet_Command is private;

function Create (Name : String) return Greet_Command;
function Get_Name (Cmd : Greet_Command) return String;
```

#### 4.2.2 Writer Port

```ada
generic
   with function Write (Message : String) return Unit_Result.Result;
package Generic_Writer is
   function Write_Message (Message : String) return Unit_Result.Result
   renames Write;
end Generic_Writer;
```

### 4.3 API Types

All public types are re-exported from `Hybrid_Lib_Ada.API`:

```ada
subtype Person_Type is Domain.Value_Object.Person.Person;
subtype Greet_Command is Application.Command.Greet.Greet_Command;
subtype Error_Type is Domain.Error.Error_Type;
subtype Error_Kind is Domain.Error.Error_Kind;
```

---

## 5. Design Patterns

### 5.1 Static Dependency Injection

Hybrid_Lib_Ada uses Ada generics for static (compile-time) dependency injection:

```ada
--  1. Port defines generic signature
generic
   with function Write (Message : String) return Unit_Result.Result;
package Generic_Writer is ...

--  2. Use case is generic, parameterized by Writer
generic
   with function Writer (Message : String) return Unit_Result.Result;
package Application.Usecase.Greet is ...

--  3. Composition root instantiates with concrete adapter
package Console_Ops is new Hybrid_Lib_Ada.API.Operations
  (Writer => Infrastructure.Adapter.Console_Writer.Write);
```

**Benefits:**

| Benefit | Description |
|---------|-------------|
| Zero runtime overhead | Monomorphization at compile time |
| SPARK compatible | No runtime dispatching |
| Type safe | Compiler verifies contracts |
| Testable | Mock writers for unit tests |

### 5.2 Three-Package API Pattern

```
┌─────────────────────────────────────────────────────────────┐
│                      User Code                               │
│   with Hybrid_Lib_Ada.API;                                  │
│   Result := API.Greet (Cmd);                                │
└────────────────────────────┬────────────────────────────────┘
                             │
┌────────────────────────────▼────────────────────────────────┐
│                  Hybrid_Lib_Ada.API                          │
│                  (Thin Facade)                               │
│  - Re-exports types                                          │
│  - Delegates Greet to Desktop                               │
│  - SPARK_Mode: Off                                          │
└────────────────────────────┬────────────────────────────────┘
                             │ delegates
┌────────────────────────────▼────────────────────────────────┐
│              Hybrid_Lib_Ada.API.Desktop                      │
│              (Composition Root)                              │
│  - Instantiates Operations with Console_Writer               │
│  - SPARK_Mode: Off (I/O wiring)                             │
│  - Located in api/desktop/ (arch_guard exception)           │
└────────────────────────────┬────────────────────────────────┘
                             │ instantiates
┌────────────────────────────▼────────────────────────────────┐
│            Hybrid_Lib_Ada.API.Operations                     │
│            (SPARK-Safe Operations)                           │
│  - Generic, parameterized by Writer                          │
│  - SPARK_Mode: On (formally verifiable)                     │
│  - Depends ONLY on Application/Domain                        │
└─────────────────────────────────────────────────────────────┘
```

### 5.3 Result Monad Pattern

All fallible operations return `Result[T]`:

```ada
function Create (Name : String) return Person_Result.Result;
--  Returns Ok(Person) or Error(Validation_Error, "message")

function Greet (Cmd : Greet_Command) return Unit_Result.Result;
--  Returns Ok(Unit) or Error(IO_Error, "message")
```

---

## 6. Error Handling Strategy

### 6.1 Error Propagation

Errors flow through use case orchestration:

```ada
function Execute (Cmd : Greet_Command) return Unit_Result.Result is
   Person_Res : constant Person_Result.Result :=
     Person.Create (Get_Name (Cmd));
begin
   if Person_Result.Is_Error (Person_Res) then
      --  Propagate domain validation error
      return Unit_Result.Error (...);
   end if;

   --  Write greeting via port
   return Writer (Format_Greeting (Person_Result.Value (Person_Res)));
end Execute;
```

### 6.2 No Exceptions Policy

| Situation | Handling |
|-----------|----------|
| Validation failure | Return Error result |
| I/O failure | Return Error result |
| Unexpected error | Return Internal_Error result |
| Programmer error | Assert/raise (debug only) |

### 6.3 Exception Boundary Specification

This section defines the mandatory rules for exception handling across all architectural layers.

#### 6.3.1 Architecture and Exception Flow

```
┌─────────────────────────────────────────────────────────┐
│                    OUTSIDE WORLD                        │
│         (User CLI, Files, Network, OS, Hardware)        │
└─────────────────────────────────────────────────────────┘
                    ↓ input              ↑ output
┌─────────────────────────────────────────────────────────┐
│                   INFRASTRUCTURE                        │
│              (Adapters, I/O, External)                  │
│                                                         │
│    External calls go out / responses come in            │
│    ✅ Functional.Try.Try_To_Result REQUIRED             │
│    ❌ Manual exception handlers FORBIDDEN               │
│    Converts ALL exceptions → Result[T, Error]           │
└─────────────────────────────────────────────────────────┘
                    ↓                    ↑
┌─────────────────────────────────────────────────────────┐
│                    APPLICATION                          │
│              (Use Cases, Orchestration)                 │
│                                                         │
│    ❌ exception keyword FORBIDDEN                       │
│    Works with Result + Error ONLY                       │
└─────────────────────────────────────────────────────────┘
                    ↓                    ↑
┌─────────────────────────────────────────────────────────┐
│                      DOMAIN                             │
│            (Entities, Value Objects, Logic)             │
│                                                         │
│    ❌ exception keyword FORBIDDEN                       │
│    Works with Result + Error ONLY                       │
│    Self-protecting types prevent invalid states         │
└─────────────────────────────────────────────────────────┘
                    ↓                    ↑
┌─────────────────────────────────────────────────────────┐
│                   API (Public Facade)                   │
│                                                         │
│    ❌ exception keyword FORBIDDEN                       │
│    Returns Result types to consumers                    │
└─────────────────────────────────────────────────────────┘
```

#### 6.3.2 Exception Handling Rules by Layer

| Layer | Location | `Functional.Try` | Manual `exception` | Rationale |
|-------|----------|------------------|-------------------|-----------|
| **Infrastructure** | `src/infrastructure/**` | **REQUIRED** | **FORBIDDEN** | Boundary to external world; must convert exceptions |
| **Domain** | `src/domain/**` | N/A | **FORBIDDEN** | Core logic; Result types only |
| **Application** | `src/application/**` | N/A | **FORBIDDEN** | Orchestration; Result types only |
| **API** | `src/api/**` | N/A | **FORBIDDEN** | Public facade; Result types only |
| **Test** | `test/**` | Optional | **ALLOWED** | Tests may verify exception scenarios |

#### 6.3.3 Required Pattern: Functional.Try at Boundaries

All Infrastructure adapters that perform I/O or call external APIs **MUST** use `Functional.Try.Try_To_Result` or `Functional.Try.Try_To_Result_With_Param`:

```ada
with Functional.Try;
with Ada.Exceptions;

package body Infrastructure.Adapter.Console_Writer is
   use Ada.Exceptions;

   --  Step 1: Inner function that may raise exceptions
   function Write_Action (Message : String) return Unit is
   begin
      Ada.Text_IO.Put_Line (Message);  -- May raise IO exceptions
      return Unit_Value;
   end Write_Action;

   --  Step 2: Exception mapper converts to domain error
   function Map_Exception
     (Exc : Exception_Occurrence) return Domain.Error.Error_Type
   is
   begin
      return Domain.Error.Create
        (Kind    => Domain.Error.IO_Error,
         Message => "Console write failed: " & Exception_Name (Exc));
   end Map_Exception;

   --  Step 3: Instantiate Functional.Try wrapper
   function Write_With_Try is new
     Functional.Try.Try_To_Result_With_Param
       (T             => Unit,
        E             => Domain.Error.Error_Type,
        Param         => String,
        Result_Pkg    => Unit_Functional_Result,
        Map_Exception => Map_Exception,
        Action        => Write_Action);

   --  Step 4: Public function uses the safe wrapper
   function Write (Message : String) return Unit_Result.Result is
   begin
      return To_Domain_Result (Write_With_Try (Message));
   end Write;

end Infrastructure.Adapter.Console_Writer;
```

#### 6.3.4 Prohibited Pattern: Manual Exception Handlers

The following pattern is **FORBIDDEN** in Infrastructure (and all other layers):

```ada
--  ❌ PROHIBITED: Manual exception handling
function Write (Message : String) return Unit_Result.Result is
begin
   Ada.Text_IO.Put_Line (Message);
   return Unit_Result.Ok (Unit_Value);
exception
   when E : others =>
      return Unit_Result.Error
        (IO_Error, "Write failed: " & Exception_Message (E));
end Write;
```

**Why prohibited:**
- Inconsistent error mapping across codebase
- Easy to forget exception handlers
- No single point of control for exception conversion
- Violates separation of concerns (I/O action vs error handling)
- Harder to audit and verify

#### 6.3.5 Core Layer Exception Philosophy

If an exception occurs in Domain, Application, or API layers, it indicates a **BUG**, not a runtime condition to handle:

- **Type definition is wrong** (constraint too narrow)
- **Validation is missing** at the boundary
- **Logic error** in the code
- **SPARK contracts incomplete** (if using SPARK)

Such exceptions should **crash loudly** so the bug can be found and fixed (or proven away with SPARK).

#### 6.3.6 Validation Enforcement

The release preparation script validates these rules:

1. **Infrastructure layer**: Must have `with Functional.Try;` if any I/O operations exist
2. **Infrastructure layer**: Must NOT have manual `exception` handlers
3. **Domain/Application/API layers**: Must NOT have `exception` keyword at all
4. **Test layer**: Exempt from validation

Violations cause the release preparation to fail.

---

## 7. Build Configuration

### 7.1 GPR Projects

| Project | Purpose |
|---------|---------|
| `hybrid_lib_ada.gpr` | Public library (Library_Interface restricted) |
| `hybrid_lib_ada_internal.gpr` | Internal (unrestricted, for tests/examples) |

### 7.2 Build Profiles

| Profile | Target | Features |
|---------|--------|----------|
| `standard` | Desktop/server | Full features |
| `embedded` | Ravenscar embedded | Tasking safe |
| `baremetal` | Zero footprint | Minimal runtime |

---

## 8. Design Decisions

### 8.1 API.Operations as Child vs Sibling

**Decision:** `API.Operations` (child) instead of `API_Operations` (sibling)

**Rationale:**
- Clean hierarchy is more idiomatic Ada
- SPARK works either way
- Preelaborate adds minimal value for consumers

### 8.2 No Heap Allocation

**Decision:** All types use bounded strings and stack allocation

**Rationale:**
- Embedded system compatibility
- SPARK compatibility
- Deterministic behavior

### 8.3 Static vs Dynamic Polymorphism

**Decision:** Static polymorphism via generics

**Rationale:**
- SPARK compatible
- Zero runtime overhead
- Compile-time type safety

---

## 9. Appendices

### A. Package Dependency Graph

```
Hybrid_Lib_Ada.API
    ├── Hybrid_Lib_Ada.API.Desktop
    │       └── Infrastructure.Adapter.Console_Writer
    │               └── Application.Port.Outbound.Writer
    │                       └── Domain.Error.Result
    │                               └── Domain.Error
    │                                       └── Domain
    └── Domain.Value_Object.Person
            └── Domain.Error.Result
                    └── Domain.Error
```

### B. Change History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 2.0.1 | 2025-12-11 | Michael Gardner | Added Section 6.3 Exception Boundary Specification with architecture diagram, layer rules, required/prohibited patterns |
| 2.0.0 | 2025-12-09 | Michael Gardner | Complete regeneration for v2.0.0; corrected Error_Kind (5 values); updated package structure |
| 1.0.0 | 2025-11-29 | Michael Gardner | Initial release |
