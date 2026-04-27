# Software Requirements Specification

**Doc Version:** 1.1.1<br>
**Applies to zoneinfo_ada:** ^1.1<br>
**Last Updated:** 2026-04-26<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2026 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## 1. Introduction

### 1.1 Purpose

This Software Requirements Specification (SRS) defines the functional and non-functional requirements for **Zoneinfo**, an Ada 2022 library providing timezone-aware datetime manipulation with pluggable time sources for desktop and embedded systems.

### 1.2 Scope

Zoneinfo provides:

- Three datetime value objects: Instant, Zoned, and Civil
- Duration type for time spans with nanosecond precision
- Pluggable clock port for desktop and embedded time sources
- Timezone conversions using IANA timezone identifiers
- SPARK-compatible design for formal verification
- Embedded-safe patterns (no heap allocation, bounded types only)
- Bounded array types for zone listing (Zone_List, Search_Results)

The library builds upon the tzif library for timezone data queries and DST calculations, providing a higher-level API for datetime operations.

### 1.3 Definitions and Acronyms

| Term | Definition |
|------|------------|
| **Instant** | Absolute moment in time represented as nanoseconds since Unix epoch (1970-01-01 00:00:00 UTC) |
| **Zoned** | Instant combined with an IANA timezone identifier (e.g., America/New_York) |
| **Civil** | Timezone-blind calendar components (year, month, day, hour, minute, second, nanosecond) |
| **Duration** | Time span represented as seconds and nanoseconds |
| **Zone_ID** | IANA timezone identifier (e.g., "America/New_York", "UTC", "Europe/London") |
| **Zone_List** | Bounded array of Zone_IDs for List_All_Zones results |
| **Search_Results** | Bounded array of Zone_IDs for Find_By_* results |
| **Clock Port** | Abstract time source interface for pluggable implementations (desktop, embedded, mock) |
| **SPARK** | Ada subset enabling formal verification with mathematical proof of correctness |
| **Result Monad** | Functional pattern for error handling without exceptions (from functional library) |
| **DST** | Daylight Saving Time |
| **IANA** | Internet Assigned Numbers Authority (maintains timezone database) |
| **TZif** | Time Zone Information Format (RFC 8536) |

### 1.4 References

- Ada 2022 Reference Manual (ISO/IEC 8652:2023)
- SPARK 2014 Reference Manual
- IANA Time Zone Database (https://www.iana.org/time-zones)
- RFC 8536 - Time Zone Information Format (TZif)
- Domain-Driven Design (Eric Evans, 2003)
- Clean Architecture (Robert C. Martin, 2017)
- Hexagonal Architecture (Alistair Cockburn, 2005)

### 1.5 Dependencies

| Crate | Version | Purpose |
|-------|---------|---------|
| **functional** | ^4.0.0 | Result and Option monads for error handling |
| **tzif** | ^3.0.3 | Timezone data queries and DST calculations |
| **gnatcoll** | ^25.0.0 | Utility library for string operations |

---

## 2. Overall Description

### 2.1 Product Perspective

Zoneinfo is a library designed to be imported by Ada applications requiring timezone-aware datetime operations. It integrates with the tzif library for timezone data and provides a higher-level API abstraction.

```
┌─────────────────────────────────────────────────────────┐
│                   Client Application                     │
│                                                          │
│   with Zoneinfo.API.Desktop;                            │
│   Now_Result := API.Desktop.Now_UTC;                    │
│   Zoned_Result := API.Desktop.Now_Zoned (Zone);         │
└────────────────────────┬────────────────────────────────┘
                         │
┌────────────────────────▼────────────────────────────────┐
│                      Zoneinfo                            │
│                                                          │
│  API Layer → Application Layer → Domain Layer            │
│       ↓              ↓                                   │
│  Infrastructure Layer (Clock adapters)                   │
│       ↓                                                  │
│     tzif (timezone data)                                 │
└─────────────────────────────────────────────────────────┘
```

### 2.2 Product Features

| Feature Category | Description |
|------------------|-------------|
| **Time Retrieval** | Get current time as Instant, Zoned, or UTC from pluggable clock sources |
| **Time Conversion** | Convert between Instant, Zoned, and Civil representations |
| **Time Arithmetic** | Add/subtract Duration from Instant with overflow protection |
| **Timezone Operations** | Change timezone, query UTC offset, handle DST transitions |
| **Duration Calculation** | Calculate time differences between Instants |
| **Timezone Discovery** | List available timezones as bounded arrays, search by pattern/region/regex |
| **Format/Parse** | Format datetime as ISO 8601, parse timezone identifiers |

### 2.3 User Classes and Characteristics

| User Type | Characteristics | Example Use Case |
|-----------|-----------------|------------------|
| **Desktop Developer** | Building server/desktop applications with timezone support | Web application with per-user timezone preferences |
| **Embedded Developer** | Resource-constrained systems requiring time operations | IoT device logging events with timezone awareness |
| **SPARK Developer** | Requires formal verification of time-critical logic | Safety-critical system with proven time calculations |

### 2.4 Operating Environment

| Environment | Requirements |
|-------------|--------------|
| **Desktop Systems** | Linux, macOS, Windows (via Ada.Calendar clock source) |
| **Embedded Systems** | ARM Cortex-M (reference: STM32F769I with RTC) |
| **Ada Compiler** | GNAT 14+ with Ada 2022 support |
| **Build System** | Alire package manager |
| **SPARK Toolchain** | gnatprove for formal verification (optional) |

### 2.5 Design and Implementation Constraints

| Constraint | Description |
|------------|-------------|
| **No Heap Allocation** | All types must be stack-allocated for embedded compatibility |
| **Bounded Strings** | Use GNATCOLL.Strings.Bounded_String, not unbounded strings |
| **Bounded Arrays** | Use bounded Zone_List/Search_Results for zone listings |
| **No Exceptions in Domain** | Use Result monad for all error conditions |
| **SPARK Compatibility** | Domain and Application layer specs must have SPARK_Mode => On |
| **Layer Dependencies** | Strict dependency rules: Domain → Application → Infrastructure → API |
| **Single Source of Truth** | tzif is the only source for timezone data (no caching in Domain) |

### 2.6 Assumptions and Dependencies

- IANA timezone database files are available at runtime (managed by tzif)
- System clock is reasonably accurate (embedded systems may use RTC)
- Nanosecond precision is sufficient (no leap second support)
- Integer_64 nanoseconds provide adequate range (±292 years from epoch)
- Zone database contains ≤750 timezones (Max_Zone_List_Size)

---

## 3. Functional Requirements

### 3.1 Domain Layer Requirements

#### FR-01: Instant Value Object

The system SHALL provide an Instant type representing an absolute moment in time.

- **FR-01.1** Instant SHALL be stored as nanoseconds since Unix epoch (Integer_64)
- **FR-01.2** Instant SHALL support construction from epoch seconds and nanoseconds
- **FR-01.3** Instant SHALL support comparison operators (=, <, <=, >, >=)
- **FR-01.4** Instant SHALL support arithmetic with Duration (Add, Subtract, Diff)
- **FR-01.5** Instant arithmetic SHALL return Result types for overflow protection
- **FR-01.6** Instant SHALL provide conversion to Unix epoch components

#### FR-02: Zoned Value Object

The system SHALL provide a Zoned type representing an Instant with timezone context.

- **FR-02.1** Zoned SHALL contain an Instant and a Zone_ID
- **FR-02.2** Zoned SHALL support equality based on both Instant and Zone_ID
- **FR-02.3** Zoned SHALL support timezone change while preserving the absolute instant
- **FR-02.4** Zoned SHALL NOT cache UTC offset (tzif is single source of truth)
- **FR-02.5** Zoned SHALL support comparison based on underlying Instant

#### FR-03: Civil Value Object

The system SHALL provide a Civil type representing timezone-blind calendar components.

- **FR-03.1** Civil SHALL contain year, month, day, hour, minute, second, nanosecond
- **FR-03.2** Civil SHALL validate component ranges at construction
- **FR-03.3** Civil SHALL validate day-in-month for given year/month (including leap years)
- **FR-03.4** Civil SHALL support comparison operators
- **FR-03.5** Civil SHALL provide leap year detection
- **FR-03.6** Civil SHALL provide days-in-month calculation

#### FR-04: Duration Value Object

The system SHALL provide a Duration_Type representing time spans.

- **FR-04.1** Duration SHALL be stored as seconds (Integer_64) and nanoseconds (0..999_999_999)
- **FR-04.2** Duration SHALL support construction from seconds, milliseconds, nanoseconds
- **FR-04.3** Duration SHALL support arithmetic operations (Add, Subtract, Negate)
- **FR-04.4** Duration SHALL support comparison operators
- **FR-04.5** Duration SHALL support conversion to total seconds/milliseconds/nanoseconds
- **FR-04.6** Duration SHALL provide absolute value calculation

#### FR-05: Zone_ID Value Object

The system SHALL provide a Zone_ID type for IANA timezone identifiers.

- **FR-05.1** Zone_ID SHALL use bounded strings (max 63 characters)
- **FR-05.2** Zone_ID SHALL support construction from string with validation
- **FR-05.3** Zone_ID SHALL provide UTC constant
- **FR-05.4** Zone_ID SHALL support equality comparison
- **FR-05.5** Zone_ID SHALL provide conversion to string

#### FR-06: Zone Collections

The system SHALL provide bounded array types for zone listing operations.

- **FR-06.1** Zone_List SHALL be a bounded array of Zone_IDs with Count field
- **FR-06.2** Zone_List capacity SHALL be configurable via Zoneinfo_Config.Max_Zone_List_Size
- **FR-06.3** Search_Results SHALL be a bounded array of Zone_IDs with Count field
- **FR-06.4** Search_Results capacity SHALL be configurable via Zoneinfo_Config.Max_Search_Results
- **FR-06.5** Zone collections SHALL be SPARK-compatible (no access types)

#### FR-07: Error Handling

The system SHALL provide comprehensive error types for all failure modes.

- **FR-07.1** Error_Type SHALL contain Error_Kind and bounded error message
- **FR-07.2** Error_Kind SHALL distinguish: Validation, Timezone, Overflow, Ambiguous_Time, Gap_Time, IO, Internal
- **FR-07.3** Result monad SHALL provide 7 essential operations: Ok, Error, From_Error, Is_Ok, Is_Error, Value, Error_Info
- **FR-07.4** NO exceptions SHALL be raised in Domain or Application layers

### 3.2 Application Layer Requirements

#### FR-08: Clock Port

The system SHALL define a Clock_Port interface for pluggable time sources.

- **FR-08.1** Clock_Port SHALL define Now_UTC returning Result[Instant]
- **FR-08.2** Clock_Port SHALL define Now_Zoned taking Zone_ID, returning Result[Zoned]
- **FR-08.3** Clock_Port SHALL be implemented by Infrastructure adapters
- **FR-08.4** Clock_Port SHALL use Result monad for error propagation

#### FR-09: Timezone Port

The system SHALL define a Timezone_Port interface for timezone operations.

- **FR-09.1** Timezone_Port SHALL delegate to tzif for all timezone data
- **FR-09.2** Timezone_Port SHALL provide Civil to Instant conversion for given Zone_ID
- **FR-09.3** Timezone_Port SHALL provide Instant to Civil conversion for given Zone_ID
- **FR-09.4** Timezone_Port SHALL handle DST transitions (gaps and overlaps)
- **FR-09.5** Timezone_Port SHALL validate Zone_ID existence

#### FR-10: Use Cases

The system SHALL provide use cases for core datetime operations.

- **FR-10.1** Get_Now use case SHALL retrieve current time via Clock_Port
- **FR-10.2** Timezone_Ops use case SHALL perform Zoned ↔ Civil conversions via Timezone_Port
- **FR-10.3** Discovery use case SHALL return bounded arrays of zones via Timezone_Port

### 3.3 Infrastructure Layer Requirements

#### FR-11: Desktop Clock Adapter

The system SHALL provide a Desktop_Clock adapter using Ada.Calendar.

- **FR-11.1** Desktop_Clock SHALL implement Clock_Port interface
- **FR-11.2** Desktop_Clock SHALL convert Ada.Calendar.Time to Instant
- **FR-11.3** Desktop_Clock SHALL use Functional.Try for exception handling
- **FR-11.4** Desktop_Clock SHALL map Ada.Calendar exceptions to Domain errors

#### FR-12: TZif Adapter

The system SHALL provide a tzif adapter implementing Timezone_Port.

- **FR-12.1** TZif adapter SHALL delegate timezone queries to tzif library
- **FR-12.2** TZif adapter SHALL use Functional.Try for exception handling
- **FR-12.3** TZif adapter SHALL map tzif exceptions to Domain errors
- **FR-12.4** TZif adapter SHALL validate Zone_ID before queries

#### FR-13: Discovery Adapter

The system SHALL provide a Discovery adapter for zone listing.

- **FR-13.1** Discovery adapter SHALL populate Zone_List from tzif
- **FR-13.2** Discovery adapter SHALL populate Search_Results from tzif pattern matching
- **FR-13.3** Discovery adapter SHALL return Overflow_Error if results exceed capacity

### 3.4 API Layer Requirements

#### FR-14: API Facade

The system SHALL provide a public API facade re-exporting Domain types.

- **FR-14.1** API SHALL re-export Instant, Zoned, Civil, Duration_Type, Zone_ID
- **FR-14.2** API SHALL re-export Zone_List, Search_Results, and their Result types
- **FR-14.3** API SHALL re-export Error types and Error_Kind constants
- **FR-14.4** API SHALL provide package renames for convenience

#### FR-15: API.Desktop

The system SHALL provide API.Desktop as the default composition root.

- **FR-15.1** API.Desktop SHALL wire Desktop_Clock to application use cases
- **FR-15.2** API.Desktop SHALL provide Now_UTC, Now_Zoned, Now functions
- **FR-15.3** API.Desktop SHALL provide Zoned ↔ Civil conversion operations
- **FR-15.4** API.Desktop SHALL provide timezone operations (UTC_Offset, With_Zone)

#### FR-16: API.Operations

The system SHALL provide SPARK-safe pure operations in API.Operations.

- **FR-16.1** API.Operations SHALL be SPARK Mode => On
- **FR-16.2** API.Operations SHALL provide Instant arithmetic operations
- **FR-16.3** API.Operations SHALL provide Duration operations
- **FR-16.4** API.Operations SHALL NOT depend on Infrastructure layer

#### FR-17: API.Discovery

The system SHALL provide timezone discovery operations returning bounded arrays.

- **FR-17.1** List_All_Zones SHALL return Zone_List_Result.Result
- **FR-17.2** Find_By_Pattern SHALL return Search_Results_Result.Result
- **FR-17.3** Find_By_Region SHALL return Search_Results_Result.Result
- **FR-17.4** Find_By_Regex SHALL return Search_Results_Result.Result
- **FR-17.5** Find_My_Id SHALL return Zone_ID_Result.Result
- **FR-17.6** NO callback types (access procedure) SHALL be used

#### FR-18: API.Format and API.Parse

The system SHALL provide formatting and parsing operations.

- **FR-18.1** API.Format SHALL format datetime as ISO 8601 string
- **FR-18.2** API.Format SHALL format durations as ISO 8601 and human-readable
- **FR-18.3** API.Parse SHALL parse ISO 8601 datetime strings
- **FR-18.4** API.Parse SHALL parse ISO 8601 and human-readable durations

---

## 4. Non-Functional Requirements

### NFR-01: Performance

| Requirement | Criteria |
|-------------|----------|
| **NFR-01.1** | Instant arithmetic SHALL complete in O(1) time |
| **NFR-01.2** | Duration operations SHALL complete in O(1) time |
| **NFR-01.3** | Timezone conversions SHALL complete in O(log n) time (delegated to tzif) |
| **NFR-01.4** | No heap allocation SHALL occur in any operation |

### NFR-02: Reliability

| Requirement | Criteria |
|-------------|----------|
| **NFR-02.1** | All fallible operations SHALL use Result monad |
| **NFR-02.2** | All exceptions at boundaries SHALL be caught by Functional.Try |
| **NFR-02.3** | No operation SHALL cause unhandled exception propagation |
| **NFR-02.4** | Overflow detection SHALL occur before arithmetic operations |

### NFR-03: Portability

| Requirement | Criteria |
|-------------|----------|
| **NFR-03.1** | Domain layer SHALL have ZERO platform-specific code |
| **NFR-03.2** | Platform abstractions SHALL be in Infrastructure layer only |
| **NFR-03.3** | Library SHALL compile on Linux, macOS, Windows via GNAT |
| **NFR-03.4** | Embedded targets SHALL be supported via Clock_Port adapters |

### NFR-04: Maintainability

| Requirement | Criteria |
|-------------|----------|
| **NFR-04.1** | Layer dependencies SHALL be enforced by package structure |
| **NFR-04.2** | All public APIs SHALL have comprehensive docstrings |
| **NFR-04.3** | Test coverage SHALL achieve ≥90% statement+decision coverage |
| **NFR-04.4** | CHANGELOG SHALL document all breaking changes |

### NFR-05: Usability

| Requirement | Criteria |
|-------------|----------|
| **NFR-05.1** | API.Desktop SHALL provide sensible defaults for common use cases |
| **NFR-05.2** | Error messages SHALL indicate root cause and suggested resolution |
| **NFR-05.3** | Quick Start guide SHALL enable first program in <10 minutes |
| **NFR-05.4** | All examples SHALL compile and run successfully |

### NFR-06: SPARK Verification

| Requirement | Criteria |
|-------------|----------|
| **NFR-06.1** | Domain layer specs SHALL have SPARK_Mode => On |
| **NFR-06.2** | Application layer specs SHALL have SPARK_Mode => On |
| **NFR-06.3** | Domain + Application SHALL pass gnatprove --mode=check |
| **NFR-06.4** | Infrastructure layer SPARK_Mode => Off (uses Ada.Calendar, Ada.Text_IO) |
| **NFR-06.5** | API.Operations SHALL be provable with --mode=prove |

### NFR-07: Testability

| Requirement | Criteria |
|-------------|----------|
| **NFR-07.1** | Unit tests SHALL cover all Domain and Application layer packages |
| **NFR-07.2** | Integration tests SHALL cover all Infrastructure adapters |
| **NFR-07.3** | Test Coverage: 335 unit + 154 integration = 489 total tests |
| **NFR-07.4** | All unit and integration tests SHALL pass |

---

## 5. System Requirements

### 5.1 Hardware Requirements

| Component | Desktop | Embedded (Reference) |
|-----------|---------|---------------------|
| **CPU** | x86_64, ARM64 | ARM Cortex-M7 |
| **RAM** | ≥ 4 MB available | ≥ 64 KB available |
| **Storage** | ≥ 100 MB for timezone database | ≥ 512 KB for subset |
| **RTC** | System clock | Hardware RTC (STM32F769I) |

### 5.2 Software Requirements

| Component | Version | Purpose |
|-----------|---------|---------|
| **Alire** | 2.0+ | Package manager and build system |
| **GNAT** | 14+ | Ada 2022 compiler |
| **gnatprove** | 14.2+ | SPARK formal verification (optional) |
| **Make** | Any | Convenience targets |
| **Python** | 3.8+ | Release automation scripts |

---

## 6. Interface Requirements

### 6.1 User Interfaces

Not applicable. Zoneinfo is a library with programmatic API only.

### 6.2 Software Interfaces

#### 6.2.1 tzif Library Interface

| Operation | Purpose |
|-----------|---------|
| **Query_UTC_Offset** | Get UTC offset for Zone_ID at given Instant |
| **Instant_To_Civil** | Convert Instant to Civil components for given Zone_ID |
| **Civil_To_Instant** | Convert Civil to Instant for given Zone_ID (handles DST) |
| **List_Timezones** | Enumerate available IANA timezone identifiers |
| **Validate_Zone_ID** | Check if Zone_ID exists in timezone database |

#### 6.2.2 functional Library Interface

| Operation | Purpose |
|-----------|---------|
| **Result Monad** | Generic_Result for error handling |
| **Option Monad** | Generic_Option for optional values |
| **Try Pattern** | Map_To_Result for exception boundary conversion |

### 6.3 Hardware Interfaces

#### 6.3.1 Desktop System Clock

- Uses Ada.Calendar.Clock for current time retrieval
- Precision: implementation-defined (typically milliseconds)

#### 6.3.2 Embedded RTC (STM32F769I Reference)

- Uses STM32 RTC peripheral registers
- Precision: seconds (hardware limitation)
- Requires RTC initialization in board support package

---

## 7. Verification and Validation

### 7.1 Verification Methods

| Requirement Category | Verification Method |
|---------------------|---------------------|
| **Functional Requirements** | Unit tests, integration tests |
| **Performance Requirements** | Benchmarks, profiling |
| **SPARK Requirements** | gnatprove formal verification |
| **Portability Requirements** | Multi-platform CI builds |
| **Error Handling** | Negative test cases |

### 7.2 Requirements Traceability

See Software Test Plan (STP) Section 8 for complete Requirements → Tests traceability matrix.

### 7.3 Test Coverage Goals

| Test Category | Location | Goal |
|---------------|----------|------|
| **Unit Tests** | test/unit/ | ≥90% statement+decision coverage |
| **Integration Tests** | test/integration/ | All adapters and use cases |
| **SPARK Checks** | Domain + Application | 100% legality verification |

---

## 8. Appendices

### Appendix A: Glossary

| Term | Definition |
|------|------------|
| **Adapter** | Infrastructure component implementing a Port interface |
| **Bounded Type** | Type with compile-time size limit (no heap allocation) |
| **Composition Root** | API layer package wiring adapters to ports (dependency injection) |
| **DST Gap** | Non-existent civil time during spring-forward transition |
| **DST Overlap** | Ambiguous civil time during fall-back transition |
| **Epoch** | Unix epoch: 1970-01-01 00:00:00 UTC |
| **Hexagonal Architecture** | Port-and-adapter pattern for platform abstraction |
| **Port** | Application layer interface defining required capabilities |
| **Railway-Oriented Programming** | Composing operations via Result monad chaining |
| **Value Object** | Immutable domain object identified by its values (DDD pattern) |

### Appendix B: Datetime Type Summary

| Type | Purpose | Example |
|------|---------|---------|
| **Instant** | Absolute time, epoch-based | 1700000000 seconds + 500000000 nanos |
| **Zoned** | Instant + timezone | Instant + "America/New_York" |
| **Civil** | Wall-clock components | 2025-12-16 14:30:00.0 (timezone-blind) |
| **Duration** | Time span | 3600 seconds + 0 nanos (1 hour) |
| **Zone_ID** | IANA timezone identifier | "America/New_York", "UTC", "Europe/London" |
| **Zone_List** | Bounded zone array | List_All_Zones result (up to 750 zones) |
| **Search_Results** | Bounded search array | Find_By_* result (up to 100 zones) |

### Appendix C: Error Kind Reference

| Error_Kind | Description | Example Trigger |
|------------|-------------|----------------|
| **Validation_Error** | Invalid input parameters | Civil day 32, month 13 |
| **Timezone_Error** | Invalid Zone_ID | Zone_ID "Invalid/Zone" |
| **Overflow_Error** | Arithmetic overflow or capacity exceeded | Instant + Duration exceeds Integer_64, Zone_List full |
| **Ambiguous_Time_Error** | DST fall-back overlap | Civil time occurs twice |
| **Gap_Time_Error** | DST spring-forward gap | Civil time doesn't exist |
| **IO_Error** | Input/output failure | Clock read failure, timezone file missing |
| **Internal_Error** | Unexpected system state | Library internal invariant violation |

### Appendix D: Layer Responsibilities

| Layer | Responsibility | SPARK Mode |
|-------|---------------|------------|
| **Domain** | Value objects, bounded collections, business rules | On (specs) |
| **Application** | Use cases, port definitions | On (specs) |
| **Infrastructure** | Adapters, external libraries | Off (uses Ada.Calendar, etc.) |
| **API** | Public facade, composition roots | Operations: On, Others: Off |

### Appendix E: Memory Planning

| Constant | Standard Profile | Purpose |
|----------|-----------------|---------|
| **Max_Zone_List_Size** | 750 | List_All_Zones capacity |
| **Max_Search_Results** | 100 | Find_By_* capacity |
| **Zone_ID_Size_Bytes** | 72 | Memory per Zone_ID |
| **Zone_List_Memory_Bytes** | ~54KB | Total Zone_List memory |
| **Search_Results_Memory_Bytes** | ~7.2KB | Total Search_Results memory |

---

**Document Control:**
- Version: 1.1.0
- Last Updated: 2025-12-16
- Status: Released
