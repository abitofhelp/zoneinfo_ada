# Software Design Specification

**Version:** 1.1.0<br>
**Date:** 2025-12-16<br>
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
- Bounded array types for zone listing (Zone_List, Search_Results)
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
│  - API.Discovery (zone enumeration via bounded arrays)      │
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
│  - Discovery (populates bounded arrays)                     │
│  src/infrastructure/                                         │
└─────────────────────────────┬───────────────────────────────┘
                              │ implements
┌─────────────────────────────▼───────────────────────────────┐
│                    Application Layer                         │
│  Use cases and port definitions                              │
│  - UseCase.Get_Now (current time retrieval)                 │
│  - UseCase.Timezone_Ops (conversions)                       │
│  - UseCase.Discovery (bounded array population)             │
│  - Port.Outbound.Clock (clock port signature)               │
│  - Port.Outbound.Timezone (timezone port signature)         │
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
│  - Value_Object.Zone_ID (IANA identifiers + bounded arrays) │
│  - Error (error types and Result monad - 7 operations)      │
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
| **Domain** | Nothing (zero external dependencies) | On (specs) |
| **Application** | Domain only | On (specs) |
| **Infrastructure** | Application, Domain, external libraries | Off |
| **API** | All layers (composition root) | Mixed (Operations: On) |

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
- `Discovery` - Populates bounded arrays from tzif

---

## 3. Package Structure

### 3.1 Directory Layout

```
src/
├── zoneinfo.ads                        # Root package
├── version/
│   └── zoneinfo-version.ads            # Version information (auto-generated)
│
├── domain/                              # Domain Layer (SPARK On - specs)
│   ├── domain.ads                       # Root domain package
│   ├── domain-unit.ads                  # Unit type (void equivalent)
│   ├── value_object/
│   │   ├── domain-value_object.ads
│   │   ├── domain-value_object-instant.ads
│   │   ├── domain-value_object-zoned.ads
│   │   ├── domain-value_object-civil.ads
│   │   ├── domain-value_object-duration_type.ads
│   │   ├── domain-value_object-zone_id.ads    # Includes Zone_List, Search_Results
│   │   └── domain-value_object-source_info.ads
│   └── error/
│       ├── domain-error.ads             # Error types
│       ├── domain-error-result.ads      # Result monad (7 operations)
│       └── domain-error-unit_result.ads # Result[Unit]
│
├── application/                         # Application Layer (SPARK On - specs)
│   ├── application.ads
│   ├── port/
│   │   └── outbound/
│   │       ├── application-port-outbound-clock.ads
│   │       ├── application-port-outbound-timezone.ads
│   │       └── application-port-outbound-writer.ads
│   └── usecase/
│       ├── application-usecase-get_now.ads
│       ├── application-usecase-timezone_ops.ads
│       └── application-usecase-discovery.ads  # Returns bounded arrays
│
├── infrastructure/                      # Infrastructure Layer (SPARK Off)
│   ├── infrastructure.ads
│   └── adapter/
│       ├── infrastructure-adapter-desktop_clock.ads
│       ├── infrastructure-adapter-tzif.ads
│       ├── infrastructure-adapter-console_writer.ads
│       └── infrastructure-adapter-discovery.ads  # Populates bounded arrays
│
└── api/                                 # API Layer (Mixed SPARK)
    ├── zoneinfo-api.ads                 # Main API facade
    ├── desktop/
    │   └── zoneinfo-api-desktop.ads     # Desktop composition root
    ├── operations/
    │   └── zoneinfo-api-operations.ads  # SPARK pure operations
    ├── discovery/
    │   └── zoneinfo-api-discovery.ads   # Returns Zone_List/Search_Results
    ├── format/
    │   └── zoneinfo-api-format.ads
    └── parse/
        └── zoneinfo-api-parse.ads
```

### 3.2 Package Descriptions by Layer

#### 3.2.1 Domain Layer Packages

| Package | SPARK Mode | Purpose |
|---------|------------|---------|
| **Domain** | On (spec) | Root domain package |
| **Domain.Unit** | On | Unit type for Result[void] pattern |
| **Domain.Value_Object.Instant** | On (spec) | Absolute time (epoch nanoseconds) |
| **Domain.Value_Object.Zoned** | On (spec) | Instant with timezone context |
| **Domain.Value_Object.Civil** | On (spec) | Calendar components (timezone-blind) |
| **Domain.Value_Object.Duration_Type** | On (spec) | Time spans (seconds + nanos) |
| **Domain.Value_Object.Zone_ID** | On (spec) | IANA identifiers + Zone_List + Search_Results |
| **Domain.Value_Object.Source_Info** | On (spec) | Timezone data source metadata |
| **Domain.Error** | On (spec) | Error types and Error_Kind enum |
| **Domain.Error.Result** | On (spec) | Result monad (7 essential operations) |

#### 3.2.2 Application Layer Packages

| Package | SPARK Mode | Purpose |
|---------|------------|---------|
| **Application** | On (spec) | Root application package |
| **Application.Port.Outbound.Clock** | On (spec) | Clock port signature |
| **Application.Port.Outbound.Timezone** | On (spec) | Timezone port signature |
| **Application.Port.Outbound.Writer** | On (spec) | Writer port signature |
| **Application.UseCase.Get_Now** | On (spec) | Current time retrieval |
| **Application.UseCase.Timezone_Ops** | On (spec) | Zoned ↔ Civil conversion |
| **Application.UseCase.Discovery** | On (spec) | Zone listing (bounded arrays) |

#### 3.2.3 Infrastructure Layer Packages

| Package | SPARK Mode | Purpose |
|---------|------------|---------|
| **Infrastructure** | Off | Root infrastructure package |
| **Infrastructure.Adapter.Desktop_Clock** | Off | Ada.Calendar clock adapter |
| **Infrastructure.Adapter.Tzif** | Off | tzif library timezone adapter |
| **Infrastructure.Adapter.Console_Writer** | Off | Ada.Text_IO output adapter |
| **Infrastructure.Adapter.Discovery** | Off | Bounded array population |

#### 3.2.4 API Layer Packages

| Package | SPARK Mode | Purpose |
|---------|------------|---------|
| **Zoneinfo.API** | Off | Public API facade (re-exports Domain types) |
| **Zoneinfo.API.Desktop** | Off | Default composition root |
| **Zoneinfo.API.Operations** | On | SPARK-safe pure operations |
| **Zoneinfo.API.Discovery** | Off | Returns Zone_List_Result, Search_Results_Result |
| **Zoneinfo.API.Format** | Off | ISO 8601 formatting |
| **Zoneinfo.API.Parse** | Off | Zone_ID and datetime parsing |

---

## 4. Type Definitions

### 4.1 Domain Types

#### 4.1.1 Instant

```ada
type Instant is record
   Epoch_Nanos : Integer_64;
end record;
```

**Purpose**: Represents an absolute moment in time as nanoseconds since Unix epoch.

**Operations**: `From_Unix_Epoch`, `From_Epoch_Nanos`, `Add`, `Subtract`, `Diff`, comparison operators

#### 4.1.2 Zoned

```ada
type Zoned is record
   Instant_Value : Instant;
   Zone          : Zone_ID;
end record;
```

**Purpose**: Represents an Instant with timezone context. Does NOT cache UTC offset.

**Design Decision**: tzif is single source of truth for all timezone calculations.

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

**Purpose**: Timezone-blind calendar components (wall-clock time).

#### 4.1.4 Duration_Type

```ada
type Duration_Type is record
   Seconds     : Integer_64;
   Nanoseconds : Nanoseconds_Range;  -- 0..999_999_999
end record;
```

**Purpose**: Time span with nanosecond precision.

#### 4.1.5 Zone_ID

```ada
type Zone_ID is record
   Id : Bounded_String_63;
end record;
```

**Purpose**: IANA timezone identifier (e.g., "America/New_York", "UTC").

#### 4.1.6 Zone_List (v1.1.0)

```ada
type Zone_List_Index is range 0 .. Max_Zone_List_Size;
type Zone_ID_Array is array (Positive range 1 .. Max_Zone_List_Size) of Zone_ID;

type Zone_List is record
   Items : Zone_ID_Array;
   Count : Zone_List_Index := 0;
end record;
```

**Purpose**: Bounded array for `List_All_Zones` results. SPARK-compatible (no access types).

**Capacity**: `Max_Zone_List_Size` (750 in standard profile)

#### 4.1.7 Search_Results (v1.1.0)

```ada
type Search_Result_Index is range 0 .. Max_Search_Results;
type Search_ID_Array is array (Positive range 1 .. Max_Search_Results) of Zone_ID;

type Search_Results is record
   Items : Search_ID_Array;
   Count : Search_Result_Index := 0;
end record;
```

**Purpose**: Bounded array for `Find_By_*` results. SPARK-compatible (no access types).

**Capacity**: `Max_Search_Results` (100 in standard profile)

#### 4.1.8 Error_Type and Result

```ada
type Error_Kind is
  (Validation_Error,      -- Invalid input parameters
   Timezone_Error,        -- Invalid Zone_ID
   Overflow_Error,        -- Arithmetic overflow or capacity exceeded
   Ambiguous_Time_Error,  -- DST fall-back overlap
   Gap_Time_Error,        -- DST spring-forward gap
   IO_Error,              -- I/O failure
   Internal_Error);       -- Unexpected system state

type Error_Type is record
   Kind    : Error_Kind;
   Message : Bounded_String_255;
end record;
```

**Result Operations (7 essential)**:
- `Ok (Value)` - Construct success
- `Error (Kind, Message)` - Construct error
- `From_Error (Err)` - Convert Error_Type to Result
- `Is_Ok (R)` - Check success
- `Is_Error (R)` - Check failure
- `Value (R)` - Extract value (Pre: Is_Ok)
- `Error_Info (R)` - Extract error (Pre: Is_Error)

**Design Decision**: Combinators (Map, And_Then, etc.) removed for SPARK prover compatibility. Use `Functional.Result` in Infrastructure layer for advanced operations.

---

## 5. Design Patterns

### 5.1 Static Dependency Injection via Generics

**Problem**: Application layer needs clock and timezone functionality without depending on Infrastructure.

**Solution**: Generic signature packages define required interfaces; API layer instantiates with concrete adapters.

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

### 5.2 Result Monad for Error Handling

**Problem**: Exceptions are not SPARK-compatible and have hidden control flow.

**Solution**: Result monad pattern - all errors are explicit values.

```ada
Parse_Result : constant Civil_Result.Result := From_ISO_8601 (Input);

if Civil_Result.Is_Ok (Parse_Result) then
   Civil_Time := Civil_Result.Value (Parse_Result);
else
   Err := Civil_Result.Error_Info (Parse_Result);
   --  Handle error based on Err.Kind
end if;
```

### 5.3 Bounded Arrays for SPARK Compatibility (v1.1.0)

**Problem**: Callbacks (`access procedure`) are not SPARK-compatible.

**Solution**: Replace callbacks with bounded arrays.

```ada
--  Old pattern (v1.0.0 - removed):
--  procedure List_All_Zones (Yield : Zone_Callback);

--  New pattern (v1.1.0):
function List_All_Zones (Source : Source_Info) return Zone_List_Result.Result;

--  Usage:
Zones_Result := List_All_Zones (Source);
if Zone_List_Result.Is_Ok (Zones_Result) then
   for I in 1 .. Zone_List_Result.Value (Zones_Result).Count loop
      Process (Zone_List_Result.Value (Zones_Result).Items (I));
   end loop;
end if;
```

---

## 6. SPARK Verification

### 6.1 Verification Boundaries

| Layer | Mode | Rationale |
|-------|------|-----------|
| **Domain specs** | SPARK_Mode => On | Core value objects, pure logic |
| **Application specs** | SPARK_Mode => On | Use case contracts, port signatures |
| **Infrastructure** | SPARK_Mode => Off | Uses Ada.Calendar, Ada.Text_IO, tzif |
| **API.Operations** | SPARK_Mode => On | Pure operations, no I/O |
| **Other API** | SPARK_Mode => Off | Composition roots with I/O |

### 6.2 Verification Commands

```bash
# Legality check (fast)
make spark-check

# Full proof (slow)
make spark-prove
```

### 6.3 Design for SPARK

- **No access types** in Domain/Application layers
- **Bounded collections** instead of callbacks
- **Result discriminant** with preconditions (Value requires Is_Ok)
- **7 essential operations** in Domain.Error.Result (combinators removed to avoid prover crash)

---

## 7. Memory Planning

### 7.1 Bounded Type Sizes

| Type | Size | Notes |
|------|------|-------|
| Zone_ID | 72 bytes | Bounded_String_63 + overhead |
| Zone_List | ~54KB | 750 × Zone_ID + count |
| Search_Results | ~7.2KB | 100 × Zone_ID + count |
| Error_Type | ~260 bytes | Kind + Bounded_String_255 |

### 7.2 Configuration

Capacity is configurable per profile in `config/profiles/<profile>/zoneinfo_config.ads`:

```ada
Max_Zone_List_Size : constant := 750;
Max_Search_Results : constant := 100;
```

---

## 8. Testing Strategy

### 8.1 Test Coverage

| Category | Count | Location |
|----------|-------|----------|
| **Unit Tests** | 335 | test/unit/ |
| **Integration Tests** | 154 | test/integration/ |
| **Total** | 489 | All passing |

### 8.2 Test Organization

```
test/
├── unit/
│   ├── test_domain_*.adb           # Domain layer tests
│   ├── test_application_*.adb      # Application layer tests
│   └── unit_runner.adb             # AUnit test runner
└── integration/
    ├── test_api_*.adb              # API integration tests
    └── integration_runner.adb      # AUnit test runner
```

---

## 9. Dependencies

| Crate | Version | Purpose |
|-------|---------|---------|
| **functional** | ^4.0.0 | Result/Option/Try monads |
| **tzif** | ^3.0.2 | IANA timezone database access |
| **gnatcoll** | ^25.0.0 | Bounded strings, utilities |

**Compiler**: GNAT 14+ (Ada 2022)

---

**Document Control:**
- Version: 1.1.0
- Last Updated: 2025-12-16
- Status: Released
