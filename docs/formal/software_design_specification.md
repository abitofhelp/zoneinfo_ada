# Software Design Specification

**Version:** 1.0.0
**Date:** 2025-12-03
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root
**Copyright:** (c) 2025 Michael Gardner, A Bit of Help, Inc.
**Status:** In Development

---

## 1. Introduction

### 1.1 Purpose

This Software Design Specification (SDS) describes the internal architecture, package structure, and design decisions for **Zoneinfo**, a timezone-aware datetime manipulation library for Ada 2022.

### 1.2 Scope

This document covers:
- 4-layer hexagonal architecture
- Three datetime kinds (Instant, Zoned, Civil)
- Pluggable clock port pattern
- Package hierarchy and dependencies
- Type definitions and contracts
- Static dependency injection via generics
- SPARK verification boundaries

### 1.3 References

- Software Requirements Specification (SRS)
- [Domain Types Diagram](../diagrams/domain_types.svg)
- [Clock Port Pattern Diagram](../diagrams/clock_port.svg)
- [Library Architecture](../common/diagrams/library_architecture.svg)
- Ada 2022 Reference Manual
- SPARK 2014 Reference Manual

---

## 2. Architectural Overview

### 2.1 Layer Architecture

Zoneinfo uses a **4-layer library architecture** (Domain, Application, Infrastructure, API):

```
┌─────────────────────────────────────────────────────────────┐
│                        API Layer                             │
│  Public facade + composition roots + SPARK operations        │
│  - API.Desktop (default)                                     │
│  - API.Discovery (TZif source management)                   │
│  - API.Embedded.STM32F769I (reference)                      │
│  - API.Operations (SPARK-safe)                              │
│  src/api/                                                    │
└─────────────────────────────┬───────────────────────────────┘
                              │ depends on
┌─────────────────────────────▼───────────────────────────────┐
│                   Infrastructure Layer                       │
│  Clock adapters implementing Clock_Port                      │
│  - Desktop_Clock (Ada.Calendar)                             │
│  - STM32F769I_Clock (embedded RTC)                          │
│  - Mock_Clock (testing)                                     │
│  src/infrastructure/                                         │
└─────────────────────────────┬───────────────────────────────┘
                              │ implements
┌─────────────────────────────▼───────────────────────────────┐
│                    Application Layer                         │
│  Use cases, ports (Clock_Port signature)                     │
│  - Now, Now_Zoned, Now_UTC use cases                        │
│  src/application/                                            │
└─────────────────────────────┬───────────────────────────────┘
                              │ depends on
┌─────────────────────────────▼───────────────────────────────┐
│                      Domain Layer                            │
│  Pure types: Instant, Zoned, Civil, Duration, Zone_ID        │
│  Result/Option monads (copied from functional)               │
│  src/domain/                                                 │
└─────────────────────────────────────────────────────────────┘
                              │
                              │ queries
┌─────────────────────────────▼───────────────────────────────┐
│                     tzif (external)                          │
│  Timezone data queries and DST calculations                  │
└─────────────────────────────────────────────────────────────┘
```

### 2.2 Dependency Rules

| Layer | May Depend On |
|-------|---------------|
| Domain | Nothing (pure, zero external dependencies) |
| Application | Domain only |
| Infrastructure | Application, Domain |
| API | All layers (composition root) |

### 2.3 Hexagonal Pattern (Clock Port)

```
           ┌──────────────────────────────────────┐
           │          Application Core            │
           │                                      │
    ┌──────┤  Domain ← Application               │
    │      │                                      │
    │      └──────────────────────────────────────┘
    │                       │
    │                       │ Clock_Port
    │                       ▼
    │                ┌────────────┐
    │                │ Now_Zoned  │
    │                │  Use Case  │
    │                └────────────┘
    │                       ▲
    │                       │ Generic instantiation
    │      ┌────────────────┼────────────────┐
    │      │                │                │
    ▼      ▼                ▼                ▼
┌──────────────┐   ┌──────────────┐   ┌──────────────┐
│Desktop_Clock │   │STM32F769I    │   │ Mock_Clock   │
│(Ada.Calendar)│   │   _Clock     │   │ (Testing)    │
└──────────────┘   └──────────────┘   └──────────────┘
```

---

## 3. Domain Type Design

### 3.1 Three DateTime Kinds

Zoneinfo provides three distinct datetime representations:

| Type | Purpose | Timezone Awareness |
|------|---------|-------------------|
| **Instant** | Absolute moment in time | None (epoch-based) |
| **Zoned** | Instant with timezone context | Full |
| **Civil** | Calendar components | None (timezone-blind) |

### 3.2 Conversion Rules

```
Instant ──────────────────► Zoned (add zone)
   ▲                           │
   │ (extract)                 │
   └───────────────────────────┘
                               │
                               ▼ (always succeeds)
                            Civil
                               │
                               ▼ (may fail: DST gaps/overlaps)
                            Zoned
```

| Conversion | Always Succeeds? | Failure Mode |
|------------|------------------|--------------|
| Instant → Zoned | Yes | N/A |
| Zoned → Instant | Yes | N/A |
| Zoned → Civil | Yes | N/A |
| Civil → Zoned | **No** | Ambiguous_Time_Error, Gap_Time_Error |

### 3.3 Equality Semantics

| Comparison | Semantics |
|------------|-----------|
| Instant = Instant | Same epoch nanoseconds |
| Zoned = Zoned | Same Instant AND same Zone_ID |
| Civil = Civil | Same calendar components |
| Duration = Duration | Same seconds and nanoseconds |

**Important:** Two Zoned values representing the same instant but in different timezones are **NOT equal**:
```ada
NYC_Noon  : Zoned := ...;  -- 2025-12-03T12:00:00 America/New_York
LA_9AM    : Zoned := ...;  -- 2025-12-03T09:00:00 America/Los_Angeles
-- Same instant, but NYC_Noon /= LA_9AM
-- To compare instants: To_Instant(NYC_Noon) = To_Instant(LA_9AM)
```

---

## 4. Package Structure

### 4.1 Directory Layout

```
src/
├── zoneinfo.ads                    # Root package
│
├── domain/
│   ├── domain.ads                  # Domain layer root
│   ├── error/
│   │   ├── domain-error.ads        # Error type definition
│   │   └── result/
│   │       └── domain-error-result.ads  # Generic Result monad
│   ├── unit/
│   │   └── domain-unit.ads         # Unit type (void equivalent)
│   └── value_object/
│       ├── instant/
│       │   └── domain-value_object-instant.ads
│       ├── zoned/
│       │   └── domain-value_object-zoned.ads
│       ├── civil/
│       │   └── domain-value_object-civil.ads
│       ├── duration_type/
│       │   └── domain-value_object-duration_type.ads
│       └── zone_id/
│           └── domain-value_object-zone_id.ads
│
├── application/
│   ├── application.ads             # Application layer root
│   ├── port/
│   │   ├── clock_port/
│   │   │   └── application-port-clock_port.ads
│   │   └── timezone_port/
│   │       └── application-port-timezone_port.ads
│   └── usecase/
│       ├── now/
│       │   └── application-usecase-now.ads
│       ├── now_zoned/
│       │   └── application-usecase-now_zoned.ads
│       ├── now_utc/
│       │   └── application-usecase-now_utc.ads
│       └── to_civil/
│           └── application-usecase-to_civil.ads
│
├── infrastructure/
│   ├── infrastructure.ads          # Infrastructure layer root
│   └── adapter/
│       ├── desktop_clock/
│       │   └── infrastructure-adapter-desktop_clock.ads
│       ├── stm32f769i_clock/
│       │   └── infrastructure-adapter-stm32f769i_clock.ads
│       ├── mock_clock/
│       │   └── infrastructure-adapter-mock_clock.ads
│       └── tzif_adapter/
│           └── infrastructure-adapter-tzif_adapter.ads
│
└── api/
    ├── zoneinfo-api.ads            # Public facade
    ├── operations/
    │   └── zoneinfo-api-operations.ads  # SPARK-safe
    ├── desktop/
    │   └── zoneinfo-api-desktop.ads     # Desktop composition root
    ├── discovery/
    │   └── zoneinfo-api-discovery.ads   # TZif source discovery root
    └── embedded/
        └── stm32f769i/
            └── zoneinfo-api-embedded-stm32f769i.ads
```

### 4.2 Package Descriptions

#### 4.2.1 Domain Layer

| Package | Purpose | SPARK |
|---------|---------|-------|
| `Domain` | Layer root | On |
| `Domain.Error` | Error type with Kind + Message | On |
| `Domain.Error.Result` | Generic Result[T] monad | On |
| `Domain.Unit` | Unit type for void operations | On |
| `Domain.Value_Object.Instant` | Epoch-based absolute time | On |
| `Domain.Value_Object.Zoned` | Instant + Zone_ID | On |
| `Domain.Value_Object.Civil` | Calendar components | On |
| `Domain.Value_Object.Duration_Type` | Time span | On |
| `Domain.Value_Object.Zone_ID` | IANA timezone identifier | On |

#### 4.2.2 Application Layer

| Package | Purpose | SPARK |
|---------|---------|-------|
| `Application` | Layer root | On |
| `Application.Port.Clock_Port` | Clock signature (generic formal) | On |
| `Application.Port.Timezone_Port` | Timezone data signature (generic formal) | On |
| `Application.Usecase.Now` | Get current Instant | On |
| `Application.Usecase.Now_Zoned` | Get current time in timezone | On |
| `Application.Usecase.Now_UTC` | Get current UTC time | On |
| `Application.Usecase.To_Civil` | Convert Zoned to Civil (uses Timezone_Port) | On |

#### 4.2.3 Infrastructure Layer

| Package | Purpose | SPARK |
|---------|---------|-------|
| `Infrastructure` | Layer root | Off |
| `Infrastructure.Adapter.Desktop_Clock` | Ada.Calendar adapter | Off |
| `Infrastructure.Adapter.STM32F769I_Clock` | Embedded RTC adapter | Off |
| `Infrastructure.Adapter.Mock_Clock` | Testing adapter | Off |
| `Infrastructure.Adapter.Tzif_Adapter` | tzif crate adapter (Timezone_Port) | Off |

#### 4.2.4 API Layer

| Package | Purpose | SPARK |
|---------|---------|-------|
| `Zoneinfo` | Library root | Off |
| `Zoneinfo.API` | Public facade, type re-exports | Off |
| `Zoneinfo.API.Operations` | SPARK-safe pure operations | On |
| `Zoneinfo.API.Desktop` | Desktop composition root | Off |
| `Zoneinfo.API.Discovery` | TZif source discovery and query operations | Off |
| `Zoneinfo.API.Embedded.STM32F769I` | Embedded composition root | Off |

---

## 5. Type Definitions

### 5.1 Domain Types

#### 5.1.1 Instant

```ada
type Instant is private;

--  Construction
function From_Unix_Epoch (Seconds : Integer_64;
                          Nanos   : Nanoseconds_Type := 0) return Instant_Result;
function Now return Instant_Result;  -- Via clock port

--  Extraction
function To_Unix_Epoch (I : Instant) return Unix_Epoch_Type;
function Epoch_Nanos (I : Instant) return Integer_64;

--  Arithmetic
function Add (I : Instant; D : Duration_Type) return Instant_Result;
function Subtract (I : Instant; D : Duration_Type) return Instant_Result;
function Diff (A, B : Instant) return Duration_Type;

--  Comparison
function "=" (A, B : Instant) return Boolean;
function "<" (A, B : Instant) return Boolean;
```

#### 5.1.2 Zoned

```ada
type Zoned is private;

--  Construction
function Create (I : Instant; Zone : Zone_ID) return Zoned;

--  Extraction (Domain layer - no tzif dependency)
function To_Instant (Z : Zoned) return Instant;
function Get_Zone (Z : Zoned) return Zone_ID;

--  Timezone change (preserves instant)
function With_Zone (Z : Zoned; New_Zone : Zone_ID) return Zoned;

--  Comparison (both Instant AND Zone must match)
function "=" (A, B : Zoned) return Boolean;

--  NOTE: To_Civil is NOT in Domain layer.
--  It requires timezone data from tzif via Timezone_Port.
--  See Application.Port.Timezone_Port for To_Civil.
```

#### 5.1.3 Civil

```ada
type Civil is private;

--  Construction
function Create (Year   : Year_Number;
                 Month  : Month_Number;
                 Day    : Day_Number;
                 Hour   : Hour_Number   := 0;
                 Minute : Minute_Number := 0;
                 Second : Second_Number := 0;
                 Nano   : Nanoseconds_Type := 0) return Civil_Result;

--  Extraction
function Get_Year (C : Civil) return Year_Number;
function Get_Month (C : Civil) return Month_Number;
function Get_Day (C : Civil) return Day_Number;
function Get_Hour (C : Civil) return Hour_Number;
function Get_Minute (C : Civil) return Minute_Number;
function Get_Second (C : Civil) return Second_Number;
function Get_Nanosecond (C : Civil) return Nanoseconds_Type;

--  Conversion (may fail for DST gaps/overlaps)
function To_Zoned (C : Civil; Zone : Zone_ID) return Zoned_Result;
```

#### 5.1.4 Duration_Type

```ada
type Duration_Type is private;

--  Construction
function From_Seconds (S : Integer_64) return Duration_Type;
function From_Millis (Ms : Integer_64) return Duration_Type;
function From_Nanos (Ns : Integer_64) return Duration_Type;

--  Extraction
function Seconds (D : Duration_Type) return Integer_64;
function Nanoseconds (D : Duration_Type) return Nanoseconds_Type;
function To_Nanos (D : Duration_Type) return Integer_64;

--  Arithmetic
function Add (A, B : Duration_Type) return Duration_Type;
function Negate (D : Duration_Type) return Duration_Type;
function Is_Negative (D : Duration_Type) return Boolean;
```

#### 5.1.5 Zone_ID

```ada
type Zone_ID is private;

--  Constants
UTC : constant Zone_ID;

--  Construction
function From_String (Name : String) return Zone_ID_Result;

--  Extraction
function To_String (Z : Zone_ID) return String;
function Is_UTC (Z : Zone_ID) return Boolean;
```

#### 5.1.6 Error Types

```ada
type Error_Kind is
  (Validation_Error,      -- Input validation failed
   Timezone_Error,        -- Invalid or unknown timezone
   Overflow_Error,        -- Arithmetic overflow
   Ambiguous_Time_Error,  -- DST overlap (multiple instants)
   Gap_Time_Error,        -- DST gap (no valid instant)
   IO_Error,              -- I/O operation failed
   Internal_Error);       -- Unexpected internal error

type Error_Type is record
   Kind    : Error_Kind;
   Message : Error_String;  -- Bounded string
end record;
```

### 5.2 Application Types

#### 5.2.1 Clock_Port Signature

```ada
--  Generic formal package defining time source contract
generic
   with function Now return Instant_Result is <>;
   with function Now_Monotonic return Monotonic_Result is <>;
package Clock_Port is
   --  Re-export for convenience
   function Get_Now return Instant_Result renames Now;
end Clock_Port;
```

#### 5.2.2 Timezone_Port Signature

**Design Decision: tzif as Single Source of Truth**

All timezone data operations (UTC offset lookup, Civil conversion, DST handling) go through the Timezone_Port, which is implemented by a Tzif_Adapter backed by the tzif crate. This ensures:

1. **Single source of truth** - tzif database is authoritative for all timezone data
2. **No cached offsets** - Zoned does not cache UTC offsets; they're computed on demand
3. **Consistent DST handling** - All gap/overlap detection uses tzif
4. **Pure Domain layer** - Domain types have zero external dependencies

```ada
--  Generic formal package defining timezone data contract
generic
   --  Get UTC offset for an instant in a timezone (seconds east of UTC)
   with function Get_UTC_Offset
     (I : Instant; Zone : Zone_ID) return Offset_Result is <>;

   --  Convert Zoned to Civil (always succeeds - instant + offset → calendar)
   with function To_Civil (Z : Zoned) return Civil is <>;

   --  Convert Civil to Zoned (may fail: DST gaps/overlaps)
   with function To_Zoned
     (C : Civil; Zone : Zone_ID) return Zoned_Result is <>;

   --  Validate timezone identifier against tzif database
   with function Is_Valid_Zone (Zone : Zone_ID) return Boolean is <>;

package Timezone_Port is
   --  Re-exports for convenience
end Timezone_Port;
```

**Implementation:** `Infrastructure.Adapter.Tzif_Adapter` implements Timezone_Port using the tzif crate.

### 5.3 API Types

All public types are re-exported from `Zoneinfo.API`:

```ada
--  Domain types
subtype Instant is Domain.Value_Object.Instant.Instant;
subtype Zoned is Domain.Value_Object.Zoned.Zoned;
subtype Civil is Domain.Value_Object.Civil.Civil;
subtype Duration_Type is Domain.Value_Object.Duration_Type.Duration_Type;
subtype Zone_ID is Domain.Value_Object.Zone_ID.Zone_ID;

--  Result types
subtype Instant_Result is Domain.Value_Object.Instant.Instant_Result.Result;
subtype Zoned_Result is Domain.Value_Object.Zoned.Zoned_Result.Result;
--  etc.

--  Constants
UTC : Zone_ID renames Domain.Value_Object.Zone_ID.UTC;
```

---

## 6. Pluggable Clock Port Pattern

### 6.1 Overview

The Clock Port pattern enables pluggable time sources via Ada generics:

```ada
--  1. Port defines generic signature (Application layer)
generic
   with function Now return Instant_Result is <>;
package Application.Port.Clock_Port is ...

--  2. Use case is generic, parameterized by Clock_Port
generic
   with package Clock is new Clock_Port (<>);
package Application.Usecase.Now_Zoned is
   function Execute (Zone : Zone_ID) return Zoned_Result;
end Now_Zoned;

--  3. Composition root instantiates with concrete adapter
package Desktop_Clock_Port is new Clock_Port
  (Now => Infrastructure.Adapter.Desktop_Clock.Now);

package Desktop_Now_Zoned is new Application.Usecase.Now_Zoned
  (Clock => Desktop_Clock_Port);
```

### 6.2 Benefits

| Benefit | Description |
|---------|-------------|
| Zero runtime overhead | Monomorphization at compile time |
| SPARK compatible | No runtime dispatching |
| Platform flexibility | Same use cases, different clock sources |
| Testable | Mock_Clock for deterministic tests |
| Embedded-friendly | No vtables, no heap |

### 6.3 Clock Adapters

| Adapter | Platform | Time Source |
|---------|----------|-------------|
| Desktop_Clock | Desktop/Server | Ada.Calendar, Ada.Real_Time |
| STM32F769I_Clock | Embedded | RTC registers, SysTick |
| Mock_Clock | Testing | Fixed/controllable time |

---

## 7. Three-Package API Pattern

### 7.1 Structure

```
┌─────────────────────────────────────────────────────────────┐
│                      User Code                               │
│   with Zoneinfo.API.Desktop;                                │
│   Result := API.Desktop.Now_UTC;                            │
└────────────────────────────┬────────────────────────────────┘
                             │
┌────────────────────────────▼────────────────────────────────┐
│               Zoneinfo.API.Desktop                          │
│               (Composition Root)                             │
│  - Wires Desktop_Clock adapter                              │
│  - Instantiates use cases                                   │
│  - SPARK_Mode: Off (I/O wiring)                            │
└────────────────────────────┬────────────────────────────────┘
                             │ instantiates
┌────────────────────────────▼────────────────────────────────┐
│            Zoneinfo.API.Operations                          │
│            (SPARK-Safe Operations)                           │
│  - Pure computation (Add, Diff, To_Civil, etc.)            │
│  - No clock dependency                                       │
│  - SPARK_Mode: On (formally verifiable)                    │
└─────────────────────────────────────────────────────────────┘
```

### 7.2 SPARK Verification Boundary

| Package | SPARK_Mode | Reason |
|---------|------------|--------|
| Domain.* | On | Pure domain logic |
| Application.* | On | Business logic |
| API.Operations | On | Pure computation |
| API.Desktop | Off | I/O wiring |
| API.Embedded.* | Off | Hardware I/O |
| Infrastructure.* | Off | I/O operations |

### 7.3 Platform-Specific Composition Roots

| Platform | Composition Root | Clock Adapter |
|----------|------------------|---------------|
| Desktop | `API.Desktop` | Desktop_Clock |
| STM32F769I | `API.Embedded.STM32F769I` | STM32F769I_Clock |
| Testing | (via Mock_Clock instantiation) | Mock_Clock |

---

## 8. Error Handling Strategy

### 8.1 Result Monad Pattern

All fallible operations return `Result[T]`:

```ada
function Now_Zoned (Zone : Zone_ID) return Zoned_Result;
--  Returns Ok(Zoned) or Error(Timezone_Error, "message")

function To_Zoned (C : Civil; Zone : Zone_ID) return Zoned_Result;
--  Returns Ok(Zoned) or Error(Ambiguous_Time_Error, "message")
--  or Error(Gap_Time_Error, "message")
```

### 8.2 DST Error Handling

| Scenario | Error | Resolution Options |
|----------|-------|-------------------|
| DST Gap | Gap_Time_Error | Adjust forward, reject |
| DST Overlap | Ambiguous_Time_Error | Choose earlier/later, reject |

### 8.3 No Exceptions Policy

| Situation | Handling |
|-----------|----------|
| Invalid timezone | Return Timezone_Error result |
| DST gap | Return Gap_Time_Error result |
| DST overlap | Return Ambiguous_Time_Error result |
| Arithmetic overflow | Return Overflow_Error result |
| I/O failure | Return IO_Error result |
| Programmer error | Assert/raise (debug only) |

---

## 9. Build Configuration

### 9.1 GPR Projects

| Project | Purpose |
|---------|---------|
| `zoneinfo.gpr` | Public library (restricted interfaces) |
| `zoneinfo_internal.gpr` | Internal (unrestricted, for tests) |

### 9.2 Build Profiles

| Profile | Target | Features |
|---------|--------|----------|
| `standard` | Desktop/server | Full features, Desktop_Clock |
| `embedded` | Ravenscar embedded | STM32F769I_Clock |
| `baremetal` | Zero footprint | Minimal runtime |

---

## 10. Design Decisions

### 10.1 Three DateTime Kinds

**Decision:** Instant, Zoned, Civil instead of a single DateTime type

**Rationale:**
- Clear semantics for each use case
- Prevents timezone confusion bugs
- Explicit conversions make intent clear
- Matches modern datetime library design (Java 8+, Rust chrono)

### 10.2 Duration as Record

**Decision:** Duration_Type as `(Seconds : Integer_64; Nanoseconds : Nanoseconds_Type)` instead of subtype of Ada.Calendar.Duration

**Rationale:**
- Loose coupling to Ada.Calendar
- Works in embedded (no Ada.Calendar)
- SPARK compatible
- Explicit nanosecond precision

### 10.3 Zoned Equality

**Decision:** Zoned equality requires same Instant AND same Zone_ID

**Rationale:**
- Preserves timezone context
- Prevents subtle bugs when comparing times
- Use To_Instant for pure temporal comparison

### 10.4 Result/Option Copying

**Decision:** Copy Result/Option from functional crate into Domain layer

**Rationale:**
- Loose coupling (no runtime dependency)
- Domain layer remains independent
- Easier SPARK verification
- Follows architecture agent guidance

### 10.5 Clock Port as Generic Formal

**Decision:** Generic formal package instead of tagged type interface

**Rationale:**
- SPARK compatible (no dispatching)
- Zero runtime overhead
- Compile-time binding
- Embedded-friendly (no vtables)

### 10.6 ISO 8601 Format Buffer Constraints

**Decision:** ISO 8601 datetime formatting uses UTC offset "Z" form to guarantee buffer safety.

**Rationale:**
- All Zoned values represent absolute moments in time (Instant + Zone context)
- UTC offset "Z" form (1 char) vs expanded form "+HH:MM" (6 chars) saves 5 chars
- Guarantees Zone_ID (max 64 chars) fits in Datetime_String buffer (96 chars)
- Fixed overhead with Z form = 32 chars, leaving exactly 64 chars for zone name
- SPARK-provable bounds: `Zone_ID_Length + 32 <= Max_Datetime_Length`

**Buffer Analysis:**
```
Component              Z-Form    Expanded Form
─────────────────────────────────────────────
Date (YYYY-MM-DD)      10 chars  10 chars
T separator            1 char    1 char
Time (HH:MM:SS)        8 chars   8 chars
Nanos (.999999999)     10 chars  10 chars
Offset                 1 char    6 chars
Zone brackets ([])     2 chars   2 chars
─────────────────────────────────────────────
Fixed overhead         32 chars  37 chars
Max Zone_ID            64 chars  59 chars
Total                  96 chars  96 chars
```

**Constraint:**
- Format API functions (`To_ISO_8601_With_Offset`, `To_ISO_8601_Full`) produce "Z" for UTC offset
- Precondition on `To_ISO_8601_Full`: `Offset = Duration_Type.Zero OR Zone_ID_Length <= 59`
- This ensures SPARK can prove `To_Bounded_String` never raises `Constraint_Error`

**Design Implication:**
Applications requiring non-UTC offset display with near-max-length Zone_IDs (>59 chars) would need a larger buffer. Since real IANA zone IDs are ~35 chars max, this constraint has no practical impact.

### 10.7 External Library Name Shadowing Pattern

**Problem:**
When a local package name matches the root of an external library (e.g., `Infrastructure.Adapter.Tzif` vs the external `TZif` crate), Ada's visibility rules cause the local package to shadow the external library inside that package body. Direct references like `TZif.Domain.Value_Object` become ambiguous or point to the wrong unit.

**Solution: Library-Unit Renaming Alias**

Create a *library-level renaming unit* inside your crate that aliases the external library under an unambiguous name:

```ada
--  File: src/zoneinfo-tzif_lib.ads
pragma Ada_2022;
--  Purpose: Provides an alias for the external TZif library to avoid
--           name shadowing by Infrastructure.Adapter.Tzif

with TZif;
package Zoneinfo.TZif_Lib renames TZif;
```

Then in the shadowing package, `with` and use the alias:

```ada
--  File: src/infrastructure/adapter/infrastructure-adapter-tzif.adb
with Zoneinfo.TZif_Lib.API;
with Zoneinfo.TZif_Lib.Domain.Value_Object.Transition_Info;

package body Infrastructure.Adapter.Tzif is
   package TZif_Api renames Zoneinfo.TZif_Lib.API;
   -- ...
end Infrastructure.Adapter.Tzif;
```

**Why this works:**
- The renaming unit is compiled at library level, where `TZif` is unambiguous
- The renamed package `Zoneinfo.TZif_Lib` is a stable alias visible everywhere
- Inside `Infrastructure.Adapter.Tzif`, the alias has no name collision

**Alternatives considered:**

| Approach | Why It Fails |
|----------|--------------|
| `Standard.TZif` | Ada's `Standard` prefix only works for predefined units, not external libraries |
| `with TZif; package X renames TZif` in body | Package renames inside a body have elaboration/visibility limitations |
| Rename the adapter | Loses naming consistency (the adapter *is* a TZif adapter) |
| Rename the external library | Requires modifying the external crate |

**Files using this pattern:**
- `src/zoneinfo-tzif_lib.ads` - The renaming alias
- `src/infrastructure/adapter/infrastructure-adapter-tzif.adb` - Uses the alias

---

## 11. Appendices

### A. Package Dependency Graph

```
Zoneinfo.API.Desktop
    ├── Infrastructure.Adapter.Desktop_Clock
    │       └── Application.Port.Clock_Port
    │               └── Domain.Value_Object.Instant
    │                       └── Domain.Error.Result
    │                               └── Domain.Error
    │
    ├── Application.Usecase.Now_Zoned
    │       ├── Application.Port.Clock_Port
    │       └── Domain.Value_Object.Zoned
    │               ├── Domain.Value_Object.Instant
    │               └── Domain.Value_Object.Zone_ID
    │
    └── Zoneinfo.API.Operations
            └── Domain.Value_Object.*
```

### B. External Dependencies

| Crate | Version | Purpose |
|-------|---------|---------|
| functional | ^1.0 | Result/Option source (copied) |
| tzif | ^1.0 | Timezone data queries |

#### Platform Abstraction via tzif

Zoneinfo inherits cross-platform support from the tzif library, which provides:

**POSIX Systems (Linux, macOS, BSD)**:
- TZif files from `/usr/share/zoneinfo/` (pre-installed)
- System timezone detection via `/etc/localtime` symlink

**Windows (10/Server 2022+)**:
- Win32 `GetDynamicTimeZoneInformation` API for timezone detection
- CLDR-based Windows-to-IANA timezone name mapping
- User-provided path to IANA tzdata directory

The `Infrastructure.Adapter.Tzif_Adapter` delegates all timezone data operations to the tzif crate, which handles platform-specific details transparently.

### C. Change History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2025-12-03 | Michael Gardner | Initial zoneinfo-specific SDS |
