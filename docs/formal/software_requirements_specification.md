# Software Requirements Specification

**Version:** 1.0.0<br>
**Date:** 2025-12-15<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
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

The library builds upon the tzif library for timezone data queries and DST calculations, providing a higher-level API for datetime operations.

### 1.3 Definitions and Acronyms

| Term | Definition |
|------|------------|
| **Instant** | Absolute moment in time represented as nanoseconds since Unix epoch (1970-01-01 00:00:00 UTC) |
| **Zoned** | Instant combined with an IANA timezone identifier (e.g., America/New_York) |
| **Civil** | Timezone-blind calendar components (year, month, day, hour, minute, second, nanosecond) |
| **Duration** | Time span represented as seconds and nanoseconds |
| **Zone_ID** | IANA timezone identifier (e.g., "America/New_York", "UTC", "Europe/London") |
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
| **functional** | 4.0.0+ | Result and Option monads for error handling |
| **tzif** | 3.0.1+ | Timezone data queries and DST calculations |
| **gnatcoll** | 25.0.0+ | Utility library for string operations |

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
| **Timezone Discovery** | List available timezones, validate Zone_IDs |
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
| **Ada Compiler** | GNAT 13+ with Ada 2022 support |
| **Build System** | Alire package manager |
| **SPARK Toolchain** | gnatprove for formal verification (optional) |

### 2.5 Design and Implementation Constraints

| Constraint | Description |
|------------|-------------|
| **No Heap Allocation** | All types must be stack-allocated for embedded compatibility |
| **Bounded Strings** | Use GNATCOLL.Strings.Bounded_String, not unbounded strings |
| **No Exceptions in Domain** | Use Result monad for all error conditions |
| **SPARK Compatibility** | Domain and Application layers must be SPARK-compatible |
| **Layer Dependencies** | Strict dependency rules: Domain → Application → Infrastructure → API |
| **Single Source of Truth** | tzif is the only source for timezone data (no caching in Domain) |

### 2.6 Assumptions and Dependencies

- IANA timezone database files are available at runtime (managed by tzif)
- System clock is reasonably accurate (embedded systems may use RTC)
- Nanosecond precision is sufficient (no leap second support)
- Integer_64 nanoseconds provide adequate range (±292 years from epoch)

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

#### FR-06: Error Handling

The system SHALL provide comprehensive error types for all failure modes.

- **FR-06.1** Error_Type SHALL contain Error_Kind and bounded error message
- **FR-06.2** Error_Kind SHALL distinguish: Validation, Timezone, Overflow, Ambiguous_Time, Gap_Time, IO, Internal
- **FR-06.3** Result monad SHALL be used for all fallible operations
- **FR-06.4** NO exceptions SHALL be raised in Domain or Application layers

### 3.2 Application Layer Requirements

#### FR-07: Clock Port

The system SHALL define a Clock_Port interface for pluggable time sources.

- **FR-07.1** Clock_Port SHALL define Now_UTC returning Result[Instant]
- **FR-07.2** Clock_Port SHALL define Now_Zoned taking Zone_ID, returning Result[Zoned]
- **FR-07.3** Clock_Port SHALL be implemented by Infrastructure adapters
- **FR-07.4** Clock_Port SHALL use Result monad for error propagation

#### FR-08: Timezone Port

The system SHALL define a Timezone_Port interface for timezone operations.

- **FR-08.1** Timezone_Port SHALL delegate to tzif for all timezone data
- **FR-08.2** Timezone_Port SHALL provide Civil to Instant conversion for given Zone_ID
- **FR-08.3** Timezone_Port SHALL provide Instant to Civil conversion for given Zone_ID
- **FR-08.4** Timezone_Port SHALL handle DST transitions (gaps and overlaps)
- **FR-08.5** Timezone_Port SHALL validate Zone_ID existence

#### FR-09: Use Cases

The system SHALL provide use cases for core datetime operations.

- **FR-09.1** Get_Now use case SHALL retrieve current time via Clock_Port
- **FR-09.2** Timezone_Ops use case SHALL perform Zoned ↔ Civil conversions via Timezone_Port
- **FR-09.3** Discovery use case SHALL list available timezones via Timezone_Port

### 3.3 Infrastructure Layer Requirements

#### FR-10: Desktop Clock Adapter

The system SHALL provide a Desktop_Clock adapter using Ada.Calendar.

- **FR-10.1** Desktop_Clock SHALL implement Clock_Port interface
- **FR-10.2** Desktop_Clock SHALL convert Ada.Calendar.Time to Instant
- **FR-10.3** Desktop_Clock SHALL use Functional.Try for exception handling
- **FR-10.4** Desktop_Clock SHALL map Ada.Calendar exceptions to Domain errors

#### FR-11: Embedded Clock Adapter (Reference)

The system SHALL provide an STM32F769I_Clock reference adapter.

- **FR-11.1** Embedded clock SHALL implement Clock_Port interface
- **FR-11.2** Embedded clock SHALL use hardware RTC (Real-Time Clock)
- **FR-11.3** Embedded clock SHALL be SPARK-compatible where possible
- **FR-11.4** Embedded clock design SHALL guide other embedded adaptations

#### FR-12: TZif Adapter

The system SHALL provide a tzif adapter implementing Timezone_Port.

- **FR-12.1** TZif adapter SHALL delegate timezone queries to tzif library
- **FR-12.2** TZif adapter SHALL use Functional.Try for exception handling
- **FR-12.3** TZif adapter SHALL map tzif exceptions to Domain errors
- **FR-12.4** TZif adapter SHALL validate Zone_ID before queries

#### FR-13: Console Writer Adapter

The system SHALL provide a Console_Writer adapter for output operations.

- **FR-13.1** Console_Writer SHALL implement Writer_Port interface
- **FR-13.2** Console_Writer SHALL use Ada.Text_IO for console output
- **FR-13.3** Console_Writer SHALL use Functional.Try for exception handling

### 3.4 API Layer Requirements

#### FR-14: API Facade

The system SHALL provide a public API facade re-exporting Domain types.

- **FR-14.1** API SHALL re-export Instant, Zoned, Civil, Duration_Type, Zone_ID
- **FR-14.2** API SHALL re-export Result types for all domain types
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

The system SHALL provide timezone discovery operations.

- **FR-17.1** API.Discovery SHALL list available timezones
- **FR-17.2** API.Discovery SHALL validate Zone_ID existence
- **FR-17.3** API.Discovery SHALL delegate to tzif for timezone database

#### FR-18: API.Format and API.Parse

The system SHALL provide formatting and parsing operations.

- **FR-18.1** API.Format SHALL format datetime as ISO 8601 string
- **FR-18.2** API.Format SHALL format Zone_ID to string
- **FR-18.3** API.Parse SHALL parse Zone_ID from string with validation

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

### NFR-06: Platform Abstraction

| Requirement | Criteria |
|-------------|----------|
| **NFR-06.1** | Clock_Port SHALL abstract all time source implementations |
| **NFR-06.2** | Timezone_Port SHALL abstract tzif library dependency |
| **NFR-06.3** | Writer_Port SHALL abstract console/file/embedded output |
| **NFR-06.4** | API composition roots SHALL wire adapters to ports |

### NFR-07: SPARK Verification

| Requirement | Criteria |
|-------------|----------|
| **NFR-07.1** | Domain layer SHALL pass gnatprove --mode=check |
| **NFR-07.2** | Application layer SHALL pass gnatprove --mode=check |
| **NFR-07.3** | Infrastructure layer SPARK_Mode => Off (uses Ada.Calendar, Ada.Text_IO) |
| **NFR-07.4** | API.Operations SHALL be provable with --mode=prove |

### NFR-08: Testability

| Requirement | Criteria |
|-------------|----------|
| **NFR-08.1** | Unit tests SHALL cover all Domain and Application layer packages |
| **NFR-08.2** | Integration tests SHALL cover all Infrastructure adapters |
| **NFR-08.3** | Mock_Clock adapter SHALL enable deterministic testing |
| **NFR-08.4** | All 510 tests (356 unit + 154 integration) SHALL pass |

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
| **GNAT** | 13+ | Ada 2022 compiler |
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
| **Try Pattern** | Try_To_Result for exception boundary conversion |

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

See Software Test Guide (STG) Section 8 for complete Requirements → Tests traceability matrix.

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
| **Civil** | Wall-clock components | 2025-12-15 14:30:00.0 (timezone-blind) |
| **Duration** | Time span | 3600 seconds + 0 nanos (1 hour) |
| **Zone_ID** | IANA timezone identifier | "America/New_York", "UTC", "Europe/London" |

### Appendix C: Error Kind Reference

| Error_Kind | Description | Example Trigger |
|------------|-------------|----------------|
| **Validation_Error** | Invalid input parameters | Civil day 32, month 13 |
| **Timezone_Error** | Invalid Zone_ID | Zone_ID "Invalid/Zone" |
| **Overflow_Error** | Arithmetic overflow | Instant + Duration exceeds Integer_64 |
| **Ambiguous_Time_Error** | DST fall-back overlap | Civil time occurs twice |
| **Gap_Time_Error** | DST spring-forward gap | Civil time doesn't exist |
| **IO_Error** | Input/output failure | Clock read failure, timezone file missing |
| **Internal_Error** | Unexpected system state | Library internal invariant violation |

### Appendix D: Layer Responsibilities

| Layer | Responsibility | SPARK Mode |
|-------|---------------|------------|
| **Domain** | Value objects, business rules | On (check) |
| **Application** | Use cases, port definitions | On (check) |
| **Infrastructure** | Adapters, external libraries | Off (uses Ada.Calendar, etc.) |
| **API** | Public facade, composition roots | Operations: On, Others: Off |

---

**Document Control:**
- Version: 1.0.0
- Last Updated: 2025-12-15
- Status: Released
