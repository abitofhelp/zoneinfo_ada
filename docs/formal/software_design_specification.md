# Software Design Specification

**Version:** 1.0.0<br>
**Date:** 2025-12-15<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## 1. Introduction

### 1.1 Purpose

This Software Design Specification (SDS) describes the internal architecture, package structure, and design decisions for **Zoneinfo**, a timezone-aware datetime manipulation library for Ada 2022.

### 1.2 Scope

This document covers:
- 4-layer hexagonal architecture (Domain, Application, Infrastructure, API)
- Three datetime value objects (Instant, Zoned, Civil) with Duration and Zone_ID
- Pluggable clock port pattern for platform abstraction
- Package hierarchy and dependencies across all layers
- Type definitions, contracts, and invariants
- Static dependency injection via generics
- SPARK verification boundaries and mixed-mode design

### 1.3 References

- Software Requirements Specification (SRS)
- Ada 2022 Reference Manual (ISO/IEC 8652:2023)
- SPARK 2014 Reference Manual
- Domain-Driven Design (Eric Evans, 2003)
- Clean Architecture (Robert C. Martin, 2017)
- Hexagonal Architecture (Alistair Cockburn, 2005)

---

## 2. Architectural Overview

### 2.1 Layer Architecture

Zoneinfo uses a **4-layer library architecture** following Domain-Driven Design, Clean Architecture, and Hexagonal Architecture principles:

```
┌─────────────────────────────────────────────────────────────┐
│                        API Layer                             │
│  Public facade + composition roots + SPARK operations        │
│  - API (re-exports Domain types)                            │
│  - API.Desktop (default composition root)                   │
│  - API.Discovery (timezone enumeration)                     │
│  - API.Operations (SPARK-safe pure operations)              │
│  - API.Format / API.Parse (formatting utilities)            │
│  src/api/                                                    │
└─────────────────────────────┬───────────────────────────────┘
                              │ depends on
┌─────────────────────────────▼───────────────────────────────┐
│                   Infrastructure Layer                       │
│  Adapters implementing outbound ports                        │
│  - Desktop_Clock (Ada.Calendar adapter)                     │
│  - Tzif_Adapter (tzif library adapter)                      │
│  - Console_Writer (Ada.Text_IO adapter)                     │
│  - Mock adapters for testing                                │
│  src/infrastructure/                                         │
└─────────────────────────────┬───────────────────────────────┘
                              │ implements
┌─────────────────────────────▼───────────────────────────────┐
│                    Application Layer                         │
│  Use cases and port definitions                              │
│  - UseCase.Get_Now (current time retrieval)                 │
│  - UseCase.Timezone_Ops (conversions)                       │
│  - UseCase.Discovery (timezone queries)                     │
│  - Port.Outbound.Clock (clock port signature)               │
│  - Port.Outbound.Timezone (timezone port signature)         │
│  - Port.Outbound.Writer (output port signature)             │
│  src/application/                                            │
└─────────────────────────────┬───────────────────────────────┘
                              │ depends on
┌─────────────────────────────▼───────────────────────────────┐
│                      Domain Layer                            │
│  Pure value objects and business logic                       │
│  - Value_Object.Instant (epoch nanoseconds)                 │
│  - Value_Object.Zoned (Instant + Zone_ID)                   │
│  - Value_Object.Civil (calendar components)                 │
│  - Value_Object.Duration_Type (time spans)                  │
│  - Value_Object.Zone_ID (IANA identifiers)                  │
│  - Error (error types and Result monad)                     │
│  src/domain/                                                 │
└─────────────────────────────────────────────────────────────┘
                              │
                              │ queries (via Timezone_Port)
┌─────────────────────────────▼───────────────────────────────┐
│                     tzif (external)                          │
│  IANA timezone database queries                              │
│  - UTC offset calculation                                    │
│  - DST transition handling                                   │
│  - Civil ↔ Instant conversion                               │
└─────────────────────────────────────────────────────────────┘
```

### 2.2 Dependency Rules

| Layer | May Depend On | SPARK Mode |
|-------|---------------|------------|
| **Domain** | Nothing (zero external dependencies) | On (check) |
| **Application** | Domain only | On (check) |
| **Infrastructure** | Application, Domain, external libraries | Off (uses Ada.Calendar, etc.) |
| **API** | All layers (composition root) | Mixed (Operations: On, others: Off) |

**Key Principle**: Dependencies point inward. Inner layers know nothing about outer layers.

### 2.3 Hexagonal Pattern (Port and Adapter)

Zoneinfo uses the Hexagonal Architecture pattern to abstract external dependencies:

```
           ┌──────────────────────────────────────┐
           │          Application Core            │
           │   (Use Cases + Port Signatures)      │
           │                                       │
           │   Get_Now <────┐    Timezone_Ops    │
           │                │                     │
           └────────┬───────┴──────┬──────────────┘
                    │              │
          Needs: Clock_Port    Needs: Timezone_Port
                    │              │
        ┌───────────▼──────┐  ┌───▼──────────────┐
        │ Desktop_Clock    │  │  Tzif_Adapter    │
        │ (Ada.Calendar)   │  │  (tzif library)  │
        └──────────────────┘  └──────────────────┘
           Infrastructure         Infrastructure
```

**Ports** (Application layer):
- `Clock_Port` - Abstract clock interface (signature package)
- `Timezone_Port` - Abstract timezone data interface
- `Writer_Port` - Abstract output interface

**Adapters** (Infrastructure layer):
- `Desktop_Clock` - Implements Clock_Port using Ada.Calendar
- `Tzif_Adapter` - Implements Timezone_Port using tzif library
- `Console_Writer` - Implements Writer_Port using Ada.Text_IO
- Mock adapters for testing

---

## 3. Package Structure

### 3.1 Directory Layout

```
src/
├── zoneinfo.ads                        # Root package
├── version/
│   └── zoneinfo-version.ads            # Version information
│
├── domain/                              # Domain Layer (SPARK On)
│   ├── domain.ads                       # Root domain package
│   ├── domain-unit.ads                  # Unit type (void equivalent)
│   ├── types/
│   │   ├── domain-types.ads             # Type utilities
│   │   └── domain-types-option.ads      # Option monad
│   ├── value_object/
│   │   ├── domain-value_object.ads
│   │   ├── domain-value_object-instant.ads
│   │   ├── domain-value_object-zoned.ads
│   │   ├── domain-value_object-civil.ads
│   │   ├── domain-value_object-duration_type.ads
│   │   ├── domain-value_object-zone_id.ads
│   │   └── domain-value_object-source_info.ads
│   └── error/
│       ├── domain-error.ads             # Error types
│       ├── domain-error-result.ads      # Result monad
│       └── domain-error-unit_result.ads # Result[Unit]
│
├── application/                         # Application Layer (SPARK On)
│   ├── application.ads
│   ├── command/
│   │   └── application-command.ads      # Command pattern types
│   ├── port/
│   │   ├── application-port.ads
│   │   ├── inbound/
│   │   │   └── application-port-inbound.ads
│   │   └── outbound/
│   │       ├── application-port-outbound.ads
│   │       ├── application-port-outbound-clock.ads
│   │       ├── application-port-outbound-timezone.ads
│   │       └── application-port-outbound-writer.ads
│   └── usecase/
│       ├── application-usecase.ads
│       ├── application-usecase-get_now.ads
│       ├── application-usecase-timezone_ops.ads
│       └── application-usecase-discovery.ads
│
├── infrastructure/                      # Infrastructure Layer (SPARK Off)
│   ├── infrastructure.ads
│   ├── zoneinfo-tzif_lib.ads            # tzif library binding
│   └── adapter/
│       ├── infrastructure-adapter.ads
│       ├── infrastructure-adapter-desktop_clock.ads
│       ├── infrastructure-adapter-tzif.ads
│       ├── infrastructure-adapter-console_writer.ads
│       └── infrastructure-adapter-discovery.ads
│
└── api/                                 # API Layer (Mixed SPARK)
    ├── zoneinfo-api.ads                 # Main API facade
    ├── desktop/
    │   └── zoneinfo-api-desktop.ads     # Desktop composition root
    ├── operations/
    │   └── zoneinfo-api-operations.ads  # SPARK pure operations
    ├── discovery/
    │   └── zoneinfo-api-discovery.ads   # Timezone discovery
    ├── format/
    │   └── zoneinfo-api-format.ads      # ISO 8601 formatting
    └── parse/
        └── zoneinfo-api-parse.ads       # Zone_ID parsing
```

### 3.2 Package Descriptions by Layer

#### 3.2.1 Domain Layer Packages

| Package | SPARK Mode | Purpose |
|---------|------------|---------|
| **Domain** | On | Root domain package |
| **Domain.Unit** | On | Unit type for Result[void] pattern |
| **Domain.Types** | On | Shared type utilities |
| **Domain.Types.Option** | On | Option monad for optional values |
| **Domain.Value_Object** | On | Root value object package |
| **Domain.Value_Object.Instant** | On | Absolute time (epoch nanoseconds) |
| **Domain.Value_Object.Zoned** | On | Instant with timezone context |
| **Domain.Value_Object.Civil** | On | Calendar components (timezone-blind) |
| **Domain.Value_Object.Duration_Type** | On | Time spans (seconds + nanos) |
| **Domain.Value_Object.Zone_ID** | On | IANA timezone identifiers |
| **Domain.Value_Object.Source_Info** | On | Timezone data source metadata |
| **Domain.Error** | On | Error types and Error_Kind enum |
| **Domain.Error.Result** | On | Result monad for error handling |
| **Domain.Error.Unit_Result** | On | Result[Unit] for void operations |

#### 3.2.2 Application Layer Packages

| Package | SPARK Mode | Purpose |
|---------|------------|---------|
| **Application** | On | Root application package |
| **Application.Command** | On | Command pattern types |
| **Application.Port** | On | Root port package |
| **Application.Port.Inbound** | On | Inbound port signatures |
| **Application.Port.Outbound** | On | Root outbound port package |
| **Application.Port.Outbound.Clock** | On | Clock port signature (documentation) |
| **Application.Port.Outbound.Timezone** | On | Timezone port signature (documentation) |
| **Application.Port.Outbound.Writer** | On | Writer port signature |
| **Application.UseCase** | On | Root use case package |
| **Application.UseCase.Get_Now** | On | Current time retrieval use case |
| **Application.UseCase.Timezone_Ops** | On | Zoned ↔ Civil conversion use case |
| **Application.UseCase.Discovery** | On | Timezone discovery use case |

#### 3.2.3 Infrastructure Layer Packages

| Package | SPARK Mode | Purpose |
|---------|------------|---------|
| **Infrastructure** | Off | Root infrastructure package |
| **Zoneinfo.Tzif_Lib** | Off | tzif library binding |
| **Infrastructure.Adapter** | Off | Root adapter package |
| **Infrastructure.Adapter.Desktop_Clock** | Off | Ada.Calendar clock adapter |
| **Infrastructure.Adapter.Tzif** | Off | tzif library timezone adapter |
| **Infrastructure.Adapter.Console_Writer** | Off | Ada.Text_IO output adapter |
| **Infrastructure.Adapter.Discovery** | Off | Timezone discovery adapter |

#### 3.2.4 API Layer Packages

| Package | SPARK Mode | Purpose |
|---------|------------|---------|
| **Zoneinfo.API** | Off | Public API facade (re-exports Domain types) |
| **Zoneinfo.API.Desktop** | Off | Default composition root (wires adapters) |
| **Zoneinfo.API.Operations** | On | SPARK-safe pure operations |
| **Zoneinfo.API.Discovery** | Off | Timezone discovery facade |
| **Zoneinfo.API.Format** | Off | ISO 8601 formatting |
| **Zoneinfo.API.Parse** | Off | Zone_ID parsing |

---

## 4. Type Definitions

### 4.1 Domain Types

#### 4.1.1 Instant

```ada
type Instant is record
   Epoch_Nanos : Integer_64;
end record;
```

**Purpose**: Represents an absolute moment in time as nanoseconds since Unix epoch (1970-01-01 00:00:00 UTC).

**Invariants**:
- None (all Integer_64 values are valid)
- Range: approximately ±292 years from epoch

**Operations**:
- Construction: `From_Unix_Epoch (Seconds, Nanos)`, `From_Epoch_Nanos`
- Arithmetic: `Add`, `Subtract`, `Diff` (with Duration)
- Comparison: `=`, `<`, `<=`, `>`, `>=`
- Conversion: `To_Unix_Epoch` (returns seconds + nanos record)

#### 4.1.2 Zoned

```ada
type Zoned is record
   Instant_Value : Instant;
   Zone          : Zone_ID;
end record;
```

**Purpose**: Represents an Instant with timezone context. Does NOT cache UTC offset.

**Invariants**:
- Zone must be a valid Zone_ID
- tzif is queried for all timezone calculations (no caching)

**Operations**:
- Construction: `Create (Instant, Zone_ID)`
- Accessors: `To_Instant`, `Get_Zone`
- Timezone change: `With_Zone (New_Zone)` (preserves Instant)
- Comparison: `=` (requires same Instant AND Zone), `<` (based on Instant)

**Design Decision**: No cached UTC offset ensures tzif remains single source of truth for all timezone data.

#### 4.1.3 Civil

```ada
type Civil is record
   Year       : Year_Number;        -- 1..9999
   Month      : Month_Number;       -- 1..12
   Day        : Day_Number;         -- 1..31
   Hour       : Hour_Number;        -- 0..23
   Minute     : Minute_Number;      -- 0..59
   Second     : Second_Number;      -- 0..59
   Nanosecond : Nanosecond_Number;  -- 0..999_999_999
end record;
```

**Purpose**: Represents timezone-blind calendar components (wall-clock time).

**Invariants**:
- All components must be in range
- Day must be valid for given month/year (leap year aware)
- No leap second support (Second max is 59)

**Operations**:
- Construction: `Create (Year, Month, Day, Hour, Minute, Second, Nanosecond)`
- Accessors: `Get_Year`, `Get_Month`, `Get_Day`, `Get_Hour`, `Get_Minute`, `Get_Second`, `Get_Nanosecond`
- Queries: `Is_Leap_Year`, `Days_In_Month`
- Comparison: `=`, `<`, `<=`, `>`, `>=` (chronological)

**Ambiguity**: Civil times are ambiguous without timezone context:
- DST gap: Civil time may not exist (spring-forward)
- DST overlap: Civil time may occur twice (fall-back)

#### 4.1.4 Duration_Type

```ada
type Duration_Type is record
   Seconds     : Integer_64;
   Nanoseconds : Nanoseconds_Range;  -- 0..999_999_999
end record;
```

**Purpose**: Represents a time span with nanosecond precision.

**Invariants**:
- Nanoseconds must be in range 0..999_999_999
- Sign is carried by Seconds field (negative durations allowed)

**Operations**:
- Construction: `From_Seconds`, `From_Milliseconds`, `From_Nanoseconds`
- Arithmetic: `Add`, `Subtract`, `Negate`, `Abs`
- Comparison: `=`, `<`, `<=`, `>`, `>=`
- Conversion: `To_Seconds`, `To_Milliseconds`, `To_Nanoseconds`

#### 4.1.5 Zone_ID

```ada
type Zone_ID is record
   Id : Bounded_String_63;  -- IANA timezone identifier
end record;
```

**Purpose**: Represents an IANA timezone identifier (e.g., "America/New_York", "UTC").

**Invariants**:
- Max length: 63 characters (bounded string)
- Content: IANA timezone database identifier
- Validity checked by tzif on use

**Operations**:
- Construction: `From_String (Str)` (returns Result)
- Constants: `UTC` (pre-defined)
- Accessors: `To_String`
- Comparison: `=`

#### 4.1.6 Error_Type

```ada
type Error_Type is record
   Kind    : Error_Kind;
   Message : Bounded_String_255;
end record;

type Error_Kind is
  (Validation_Error,      -- Invalid input parameters
   Timezone_Error,        -- Invalid Zone_ID
   Overflow_Error,        -- Arithmetic overflow
   Ambiguous_Time_Error,  -- DST fall-back overlap
   Gap_Time_Error,        -- DST spring-forward gap
   IO_Error,              -- I/O failure
   Internal_Error);       -- Unexpected system state
```

**Purpose**: Represents all error conditions in the library.

**Design**: Error_Kind enables pattern matching, Message provides context.

### 4.2 Application Types

#### 4.2.1 Port Signatures

**Clock_Port**:
```ada
--  Signature (defined via generic formal parameters):
--  function Now return Instant_Result.Result;
```

**Timezone_Port**:
```ada
--  Signature (defined via generic formal parameters):
--  function To_Civil (I : Instant; Zone : Zone_ID) return Civil;
--  function To_Instant (C : Civil; Zone : Zone_ID) return Instant_Result.Result;
--  function Is_Valid_Zone (Zone : Zone_ID) return Boolean;
--  function Get_UTC_Offset (I : Instant; Zone : Zone_ID) return Duration_Type;
```

**Writer_Port**:
```ada
--  Signature (defined via generic formal parameters):
--  procedure Put_Line (Msg : String);
```

### 4.3 API Types

The API layer does not define new types. It re-exports Domain types for public consumption:

```ada
--  Zoneinfo.API
subtype Instant is Domain.Value_Object.Instant.Instant;
subtype Zoned is Domain.Value_Object.Zoned.Zoned;
subtype Civil is Domain.Value_Object.Civil.Civil;
subtype Duration_Type is Domain.Value_Object.Duration_Type.Duration_Type;
subtype Zone_ID is Domain.Value_Object.Zone_ID.Zone_ID;
subtype Error_Type is Domain.Error.Error_Type;

package Instant_Result renames Domain.Value_Object.Instant.Instant_Result;
package Zoned_Result renames Domain.Value_Object.Zoned.Zoned_Result;
--  ... etc.
```

---

## 5. Design Patterns

### 5.1 Static Dependency Injection via Generics

**Problem**: Application layer needs clock and timezone functionality without depending on Infrastructure.

**Solution**: Generic signature packages define required interfaces; API layer instantiates with concrete adapters.

**Example**:
```ada
--  Application layer defines WHAT is needed
generic
   with function Now return Instant_Result.Result;
package Application.UseCase.Get_Now is
   function Execute return Instant_Result.Result;
end Application.UseCase.Get_Now;

--  Infrastructure layer provides HOW
package body Infrastructure.Adapter.Desktop_Clock is
   function Now return Instant_Result.Result is ... end Now;
end Infrastructure.Adapter.Desktop_Clock;

--  API layer wires them together
package Get_Now_UC is new Application.UseCase.Get_Now
  (Now => Infrastructure.Adapter.Desktop_Clock.Now);
```

**Benefits**:
- Compile-time dependency injection (no runtime overhead)
- Type-safe interface contracts
- Testability via mock adapters
- SPARK-compatible design

### 5.2 Three-Package API Pattern

**Structure**:
1. **Zoneinfo.API** - Re-exports Domain types
2. **Zoneinfo.API.Desktop** - Composition root (wires adapters to use cases)
3. **Zoneinfo.API.Operations** - SPARK-safe pure operations

**Benefits**:
- Clean public interface (users only `with Zoneinfo.API`)
- Platform-specific composition roots (API.Desktop, API.Embedded.*)
- SPARK-safe subset available (API.Operations)

### 5.3 Result Monad for Error Handling

**Problem**: No exceptions in Domain/Application layers, but operations can fail.

**Solution**: All fallible operations return `Result[T]` where Result is:
```ada
type Result (Is_Ok : Boolean := False) is record
   case Is_Ok is
      when True  => Value : T;
      when False => Error : Error_Type;
   end case;
end record;
```

**Usage**:
```ada
--  Create and check
Zone_Result := Zone_ID.From_String ("America/New_York");
if Zone_ID_Result.Is_Ok (Zone_Result) then
   Zone := Zone_ID_Result.Value (Zone_Result);
else
   Error := Zone_ID_Result.Error (Zone_Result);
end if;

--  Railway-oriented programming (bind operations)
Result := Instant.From_Unix_Epoch (Seconds, Nanos)
  .And_Then (lambda (I) => Instant.Add (I, Duration));
```

### 5.4 Value Object Pattern

**All domain types are value objects**:
- Immutable after creation
- Equality based on values, not identity
- No setters (only constructors and "with" operations)
- Passed by value (small records)

**Example**:
```ada
--  Immutable: Cannot modify fields
I1 : constant Instant := Instant.From_Epoch_Nanos (1000);

--  "With" operations create new values
Z1 : constant Zoned := Zoned.Create (I1, NY_Zone);
Z2 : constant Zoned := Zoned.With_Zone (Z1, London_Zone);
--  Z1 unchanged, Z2 is new value
```

### 5.5 Port and Adapter (Hexagonal Architecture)

**Ports** (Application layer):
- Define WHAT the application needs (interface)
- Signature packages (generic formal parameters)
- Technology-agnostic

**Adapters** (Infrastructure layer):
- Provide HOW to fulfill the need (implementation)
- Concrete packages implementing port signatures
- Technology-specific (Ada.Calendar, tzif, etc.)

**Composition** (API layer):
- Wires adapters to ports at compile time
- Static dispatch (no runtime polymorphism)

### 5.6 Functional.Try for Exception Boundaries

**Problem**: Infrastructure adapters use Ada standard library (Ada.Calendar, Ada.Text_IO) which raises exceptions.

**Solution**: All Infrastructure operations wrapped in Functional.Try:

```ada
--  From global CLAUDE.md rule:
with Functional.Try;

function Risky_Action (Param : Param_Type) return T;

function Map_Exception
  (Exc : Exception_Occurrence) return Domain.Error.Error_Type;

function Safe_Action is new
  Functional.Try.Try_To_Result_With_Param
    (T             => Result_Type,
     E             => Domain.Error.Error_Type,
     Param         => Param_Type,
     Result_Pkg    => My_Result_Package,
     Map_Exception => Map_Exception,
     Action        => Risky_Action);

--  Public API calls safe wrapper
function Public_Operation (P : Param_Type) return Result is
begin
   return Safe_Action (P);
end Public_Operation;
```

**Benefits**:
- NO manual exception handlers in Infrastructure
- Consistent exception → Domain.Error mapping
- Auditability (single exception boundary mechanism)

---

## 6. Error Handling Strategy

### 6.1 No Exceptions Policy

**Domain and Application layers**:
- MUST NOT raise exceptions
- All operations return Result[T] or success type
- SPARK-compatible error handling

**Infrastructure layer**:
- MAY raise exceptions (uses Ada standard library)
- ALL exceptions MUST be caught at adapter boundary using Functional.Try
- Converted to Domain.Error.Error_Type before returning to Application

**API layer**:
- Re-exports Result types
- No new exceptions introduced

### 6.2 Error Propagation

**Railway-Oriented Programming**:
```ada
--  Operation sequence (short-circuits on first error)
function Process return Result is
   Zone_Result : Zone_ID_Result.Result;
   Instant_Result : Instant.Instant_Result.Result;
   Zoned_Result : Zoned.Zoned_Result.Result;
begin
   Zone_Result := Zone_ID.From_String ("America/New_York");
   if not Zone_ID_Result.Is_Ok (Zone_Result) then
      return ...;  --  Propagate error
   end if;

   Instant_Result := API.Desktop.Now_UTC;
   if not Instant_Result.Is_Ok (Instant_Result) then
      return ...;  --  Propagate error
   end if;

   --  Success path
   return Zoned.Create (Instant_Result.Value (...), Zone_Result.Value (...));
end Process;
```

### 6.3 Error Context

All errors include:
- **Error_Kind**: Enum for pattern matching
- **Message**: Human-readable description (bounded string)

Example messages:
- "Invalid Zone_ID: 'Invalid/Zone' not found in timezone database"
- "Overflow in Instant arithmetic: result exceeds Integer_64 range"
- "Gap time error: 2025-03-09 02:30:00 doesn't exist in America/New_York (DST gap)"

---

## 7. Build Configuration

### 7.1 GPR Projects

**Main project**: `zoneinfo.gpr`
- Compiles all source code
- Exports library interface
- Dependencies: functional, tzif, gnatcoll

**SPARK project**: `zoneinfo_spark.gpr`
- Runs SPARK verification on Domain + Application
- Mode: gnatprove --mode=check (legality only)
- Infrastructure excluded (SPARK_Mode => Off)

**Test projects**:
- `test/unit/zoneinfo_unit_tests.gpr` - Unit tests
- `test/integration/zoneinfo_integration_tests.gpr` - Integration tests

### 7.2 Build Profiles

| Profile | Purpose | Flags |
|---------|---------|-------|
| **development** | Development builds | `-g`, `-O0`, assertions on |
| **release** | Production builds | `-O2`, assertions off |
| **validation** | Test builds | `-O1`, coverage enabled |

**Alire configuration**:
```toml
[build-switches]
"*".Ada_Version = "Ada2022"
"*".Style_Checks = "yes"
```

---

## 8. Design Decisions

### 8.1 Why No Cached UTC Offset in Zoned?

**Decision**: Zoned does NOT cache UTC offset; all timezone queries go through Timezone_Port → tzif.

**Rationale**:
- **Single source of truth**: tzif is authoritative for all timezone data
- **Simplicity**: No cache invalidation logic needed
- **Correctness**: Eliminates risk of stale offset data
- **Performance**: Timezone conversions delegated to tzif (already optimized)

**Trade-off**: Extra indirection for Civil conversions (acceptable for correctness).

### 8.2 Why Static Dependency Injection (Generics)?

**Decision**: Use Ada generics for dependency injection instead of runtime polymorphism (tagged types).

**Rationale**:
- **SPARK compatibility**: Generics work in SPARK, tagged types have limitations
- **Performance**: Static dispatch (zero runtime overhead)
- **Type safety**: Compile-time interface verification
- **Simplicity**: No dynamic dispatch complexity

**Trade-off**: More verbose generic instantiations (acceptable for correctness and performance).

### 8.3 Why Separate API.Operations?

**Decision**: Provide API.Operations as a SPARK-safe pure operations package.

**Rationale**:
- **SPARK users**: Need provable operations without Infrastructure dependencies
- **Subset principle**: SPARK-safe subset of full API
- **Formal verification**: API.Operations can be fully proven

**Usage**: SPARK-critical code uses API.Operations; general code uses API.Desktop.

### 8.4 Why Bounded Strings Everywhere?

**Decision**: All strings use GNATCOLL.Strings.Bounded_String (no unbounded strings).

**Rationale**:
- **Embedded compatibility**: No heap allocation
- **SPARK compatibility**: Bounded types provable
- **Predictability**: Compile-time size limits
- **Safety**: No dynamic allocation failures

**Trade-off**: String length limits (63 for Zone_ID, 255 for errors) - acceptable for use cases.

### 8.5 Why Three Datetime Types?

**Decision**: Provide three distinct types (Instant, Zoned, Civil) instead of one unified type.

**Rationale**:
- **Type safety**: Prevents mixing timezone-aware and timezone-blind times
- **Explicitness**: Forces developers to think about timezone context
- **Correctness**: Compiler catches timezone misuse errors
- **DDD principle**: Each type models a distinct concept

**Example error prevented**:
```ada
--  Compiler error: Cannot mix Instant and Civil
I : Instant := ...;
C : Civil := ...;
if I = C then ...  --  TYPE ERROR (good!)
```

### 8.6 Why Result Monad Instead of Exceptions?

**Decision**: Use Result[T] monad for all error handling in Domain/Application layers.

**Rationale**:
- **SPARK compatibility**: Exceptions not allowed in SPARK
- **Explicitness**: Errors are part of type signature (forces handling)
- **Composability**: Railway-oriented programming patterns
- **No control flow via exceptions**: Errors are values, not control flow

**Trade-off**: More verbose error handling code (acceptable for correctness).

---

## 9. Appendices

### Appendix A: Package Dependency Graph

```
Zoneinfo.API.Desktop
    │
    ├─> Zoneinfo.API (re-exports)
    │       │
    │       └─> Domain.* (value objects)
    │
    ├─> Infrastructure.Adapter.Desktop_Clock
    │       │
    │       └─> Application.Port.Outbound.Clock
    │               │
    │               └─> Domain.Value_Object.Instant
    │
    ├─> Infrastructure.Adapter.Tzif
    │       │
    │       ├─> Application.Port.Outbound.Timezone
    │       │       │
    │       │       └─> Domain.* (Instant, Civil, Zone_ID, Duration)
    │       │
    │       └─> Zoneinfo.Tzif_Lib (external)
    │
    └─> Application.UseCase.* (Get_Now, Timezone_Ops, etc.)
            │
            └─> Domain.* (value objects)
```

**Key Observations**:
- All arrows point inward (toward Domain)
- Infrastructure depends on Application (ports)
- API depends on everything (composition root)
- Domain has ZERO outward dependencies

### Appendix B: SPARK Verification Strategy

| Layer | SPARK Mode | Verification Level | Notes |
|-------|------------|-------------------|-------|
| **Domain** | On | check | Flow analysis + legality |
| **Application** | On | check | Flow analysis + legality |
| **Infrastructure** | Off | N/A | Uses Ada.Calendar, Ada.Text_IO |
| **API** (facade) | Off | N/A | Re-exports |
| **API.Operations** | On | prove (future) | Pure operations, no I/O |

**Commands**:
```bash
make spark-check   # gnatprove --mode=check (current)
make spark-prove   # gnatprove --mode=prove (future goal)
```

**Current Status** (v1.0.0):
- Domain + Application: Passes --mode=check
- API.Operations: Passes --mode=check (prove goal for v2.0)

### Appendix C: Change History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-12-15 | Initial release - regenerated from source |

---

**Document Control:**
- Version: 1.0.0
- Last Updated: 2025-12-15
- Status: Released
