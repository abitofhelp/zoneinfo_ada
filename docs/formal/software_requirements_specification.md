# Software Requirements Specification

**Version:** 1.0.0
**Date:** 2025-12-03
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root
**Copyright:** (c) 2025 Michael Gardner, A Bit of Help, Inc.
**Status:** In Development

---

## 1. Introduction

### 1.1 Purpose

This Software Requirements Specification (SRS) defines the functional and non-functional requirements for **Zoneinfo**, an Ada 2022 library providing timezone-aware datetime manipulation with pluggable time sources for desktop and embedded systems.

### 1.2 Scope

Zoneinfo provides:

- Three datetime kinds: Instant, Zoned, and Civil
- Duration type for time spans
- Pluggable clock port for desktop and embedded time sources
- Timezone conversions using IANA timezone identifiers
- SPARK-compatible design for formal verification
- Embedded-safe patterns (no heap allocation, bounded types only)

### 1.3 Definitions

| Term | Definition |
|------|------------|
| **Instant** | Absolute moment in time, epoch-based (nanoseconds since Unix epoch) |
| **Zoned** | Instant combined with a timezone identifier |
| **Civil** | Timezone-blind calendar components (year, month, day, hour, minute, second, nanosecond) |
| **Duration** | Time span represented as seconds and nanoseconds |
| **Zone_ID** | IANA timezone identifier (e.g., "America/New_York", "UTC") |
| **Clock Port** | Abstract time source interface for pluggable implementations |
| **SPARK** | Ada subset for formal verification |
| **Result Monad** | Functional pattern for error handling without exceptions |

### 1.4 References

- Ada 2022 Reference Manual (ISO/IEC 8652:2023)
- SPARK 2014 Reference Manual
- IANA Time Zone Database (<https://www.iana.org/time-zones>)
- RFC 8536 - Time Zone Information Format (TZif)
- Domain-Driven Design (Eric Evans, 2003)
- Clean Architecture (Robert C. Martin, 2017)

### 1.5 Dependencies

| Crate | Purpose |
|-------|---------|
| **functional** | Result and Option monads for error handling |
| **tzif** | Timezone data queries and DST calculations |

---

## 2. Overall Description

### 2.1 Product Perspective

Zoneinfo is a library designed to be imported by Ada applications requiring timezone-aware datetime operations. It builds upon the tzif library for timezone data and provides a higher-level API for datetime manipulation.

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

### 2.2 Product Functions

| Function Category | Description |
|-------------------|-------------|
| **Time Retrieval** | Get current time as Instant, Zoned, or UTC |
| **Time Conversion** | Convert between Instant, Zoned, and Civil |
| **Time Arithmetic** | Add/subtract Duration from Instant |
| **Timezone Operations** | Change timezone, query UTC offset |
| **Duration Calculation** | Calculate difference between Instants |

### 2.3 User Characteristics

| User Type | Characteristics |
|-----------|-----------------|
| **Desktop Developer** | Building server/desktop applications with timezone support |
| **Embedded Developer** | Building firmware requiring time operations without standard runtime |
| **SPARK Developer** | Requiring formally verifiable time operations |
| **Library Consumer** | Integrating datetime functionality into Ada applications |

### 2.4 Constraints

| Constraint | Rationale |
|------------|-----------|
| Ada 2022 | Required for modern language features |
| GNAT 14+ | Required compiler version |
| No Heap Allocation | Embedded system compatibility |
| Bounded Types Only | Predictable memory usage, SPARK compatibility |
| SPARK Subset | Formal verification capability for pure operations |

### 2.5 Assumptions and Dependencies

- Alire 2.0+ available for dependency management
- `functional` crate available (Result/Option monad implementation)
- `tzif` crate available (timezone data queries)
- GNAT toolchain installed via Alire
- For embedded: target-specific RTC hardware available

#### 2.5.1 Platform-Specific Notes

**POSIX Systems (Linux, macOS, BSD)**:
- TZif files typically pre-installed at `/usr/share/zoneinfo/`
- System timezone detected via `/etc/localtime` symlink
- No additional configuration required for most systems

**Windows (10/Server 2022+)**:
- User must provide path to IANA tzdata directory
- Download tzdata from <https://www.iana.org/time-zones>
- System timezone detected via Win32 API (`GetDynamicTimeZoneInformation`)
- Windows timezone names automatically mapped to IANA zone IDs via CLDR data
- Underlying tzif library handles Windows platform abstraction

---

## 3. Functional Requirements

### 3.1 Domain Layer Requirements

#### REQ-DOM-001: Instant Value Object

**Description:** The system SHALL provide an Instant value object representing an absolute moment in time.

**Acceptance Criteria:**

- Instant is immutable after creation
- Internally stored as nanoseconds since Unix epoch (Integer_64)
- Provides conversion to/from Unix epoch (seconds, nanoseconds)
- Supports addition and subtraction of Duration
- Supports difference calculation returning Duration
- Comparable and hashable

#### REQ-DOM-002: Zoned Value Object

**Description:** The system SHALL provide a Zoned value object representing an Instant with timezone context.

**Acceptance Criteria:**

- Zoned is immutable after creation
- Contains an Instant and a Zone_ID
- Provides conversion to Instant (extraction)
- Provides conversion to Civil (always succeeds)
- Provides timezone change operation (With_Zone)
- Two Zoned values with same Instant but different zones are NOT equal

#### REQ-DOM-003: Civil Value Object

**Description:** The system SHALL provide a Civil value object representing timezone-blind calendar components.

**Acceptance Criteria:**

- Civil is immutable after creation
- Contains: Year, Month, Day, Hour, Minute, Second, Nanosecond
- All components use bounded types with valid ranges
- Provides accessor functions for all components
- Conversion to Zoned may fail (DST gaps/overlaps)

#### REQ-DOM-004: Duration_Type Value Object

**Description:** The system SHALL provide a Duration_Type value object representing a time span.

**Acceptance Criteria:**

- Duration_Type is immutable after creation
- Contains: Seconds (Integer_64) and Nanoseconds (0..999_999_999)
- Supports positive and negative durations
- Provides construction from seconds, milliseconds, nanoseconds
- Provides conversion to total seconds or nanoseconds
- Supports addition of durations
- Supports negation

#### REQ-DOM-005: Zone_ID Value Object

**Description:** The system SHALL provide a Zone_ID value object representing an IANA timezone identifier.

**Acceptance Criteria:**

- Zone_ID is immutable after creation
- Contains bounded string for timezone name
- Provides UTC constant for convenience
- Validation via tzif library
- Provides Is_UTC query function

#### REQ-DOM-006: Error Types

**Description:** The system SHALL define structured error types for all failure modes.

**Acceptance Criteria:**

- Error includes Kind enumeration
- Error includes human-readable Message
- Error Kinds include: Validation_Error, Timezone_Error, Overflow_Error, Ambiguous_Time_Error, Gap_Time_Error, IO_Error, Internal_Error

#### REQ-DOM-007: Result Monad

**Description:** The system SHALL use Result[T] for all fallible operations.

**Acceptance Criteria:**

- Result is either Ok(value) or Error(error_info)
- No exceptions raised for expected errors
- Type-safe value extraction
- Copied from functional crate into Domain layer (loose coupling)

### 3.2 Application Layer Requirements

#### REQ-APP-001: Clock Port Signature

**Description:** The system SHALL define a Clock_Port signature for time source abstraction.

**Acceptance Criteria:**

- Generic formal package defining Now() -> Result[Instant]
- Optionally defines Now_Monotonic() -> Result[Monotonic_Instant]
- No implementation details in signature
- Compile-time binding via generic instantiation

#### REQ-APP-002: Now Use Case

**Description:** The system SHALL provide a use case for retrieving current time as Instant.

**Acceptance Criteria:**

- Generic over Clock_Port
- Returns Result[Instant]
- Delegates to clock adapter

#### REQ-APP-003: Now_Zoned Use Case

**Description:** The system SHALL provide a use case for retrieving current time in a specified timezone.

**Acceptance Criteria:**

- Generic over Clock_Port
- Accepts Zone_ID parameter
- Returns Result[Zoned]
- Combines clock time with timezone

#### REQ-APP-004: Now_UTC Use Case

**Description:** The system SHALL provide a convenience use case for retrieving current UTC time.

**Acceptance Criteria:**

- Generic over Clock_Port
- Returns Result[Zoned] with UTC zone
- Shorthand for Now_Zoned(Zone_ID.UTC)

### 3.3 Infrastructure Layer Requirements

#### REQ-INF-001: Desktop_Clock Adapter

**Description:** The system SHALL provide a Desktop_Clock adapter using Ada.Calendar.

**Acceptance Criteria:**

- Implements Clock_Port contract
- Uses Ada.Calendar for wall clock time
- Uses Ada.Real_Time for monotonic time (if needed)
- Default adapter for desktop/server applications
- Returns appropriate errors on failure

#### REQ-INF-002: STM32F769I_Clock Adapter

**Description:** The system SHALL provide a reference embedded clock adapter for STM32F769I.

**Acceptance Criteria:**

- Implements Clock_Port contract
- Reads RTC hardware registers directly
- Uses SysTick for monotonic time
- No heap allocation, no standard runtime
- Interrupt-safe implementation
- Serves as reference for other embedded targets

#### REQ-INF-003: Mock_Clock Adapter

**Description:** The system SHALL provide a Mock_Clock adapter for testing.

**Acceptance Criteria:**

- Implements Clock_Port contract
- Allows setting fixed time
- Allows advancing time by duration
- Enables deterministic tests
- Supports time travel scenarios

### 3.4 API Layer Requirements

#### REQ-API-001: API.Desktop Composition Root

**Description:** The system SHALL provide a desktop composition root.

**Acceptance Criteria:**

- Wires Desktop_Clock adapter
- Provides Now(), Now_Zoned(Zone), Now_UTC() functions
- SPARK_Mode(Off) for I/O wiring
- Default entry point for desktop applications

#### REQ-API-002: API.Embedded.STM32F769I Composition Root

**Description:** The system SHALL provide an embedded composition root for STM32F769I.

**Acceptance Criteria:**

- Wires STM32F769I_Clock adapter
- Provides same interface as API.Desktop
- SPARK_Mode(Off) for hardware I/O
- Reference implementation for embedded targets

#### REQ-API-003: API.Operations (SPARK-Safe)

**Description:** The system SHALL provide SPARK-verifiable pure operations.

**Acceptance Criteria:**

- SPARK_Mode(On) for entire package
- No clock dependency (pure computation)
- Operations: Add, Diff, To_Civil, To_Zoned, etc.
- Generic over domain types only
- Formally verifiable

#### REQ-API-004: Type Re-exports

**Description:** The system SHALL re-export domain types through the API layer.

**Acceptance Criteria:**

- API packages expose: Instant, Zoned, Civil, Duration_Type, Zone_ID
- API packages expose: Error, Result types
- Consumers need only import API packages

---

## 4. Non-Functional Requirements

### 4.1 Performance

| Requirement | Target |
|-------------|--------|
| Time retrieval latency | < 1ms (excluding hardware access) |
| Timezone conversion | < 100µs |
| Memory allocation | Zero heap allocation |
| Stack usage | < 4KB per operation |

### 4.2 Reliability

| Requirement | Description |
|-------------|-------------|
| Error handling | All errors returned via Result, no exceptions |
| Overflow protection | All arithmetic checked for overflow |
| Timezone validation | All Zone_IDs validated via tzif |
| DST handling | Ambiguous/gap times reported as errors |

### 4.3 Portability

| Requirement | Description |
|-------------|-------------|
| Compiler | GNAT 14+ |
| Desktop Platforms | Linux, macOS, Windows |
| Embedded Platforms | STM32F769I (reference), extensible to others |
| Runtime | Full runtime (desktop), Zero-footprint (embedded) |

### 4.4 Maintainability

| Requirement | Description |
|-------------|-------------|
| Architecture | 4-layer hexagonal (Domain/Application/Infrastructure/API) |
| Coupling | Inward dependencies only, loose coupling to dependencies |
| Testing | Unit tests for each layer, mock clock for deterministic tests |
| Documentation | Full API documentation, architecture diagrams |

### 4.5 Security

| Requirement | Description |
|-------------|-------------|
| Input validation | All timezone identifiers validated |
| Bounded types | All strings bounded, prevents buffer overflow |
| No dynamic memory | Prevents use-after-free, double-free |
| SPARK compatible | Enables formal verification of pure operations |

### 4.6 SPARK Formal Verification (NFR-06)

| ID | Requirement |
|----|-------------|
| NFR-06.1 | Domain and Application layers SHALL pass SPARK legality checking (gnatprove --mode=check) |
| NFR-06.2 | All domain packages SHALL use `SPARK_Mode => On` |
| NFR-06.3 | Application port interfaces SHALL use `SPARK_Mode => On` |
| NFR-06.4 | No runtime errors provable in domain layer (overflow, range, division) |
| NFR-06.5 | All domain variables SHALL be properly initialized before use |
| NFR-06.6 | Pre/postconditions on domain operations SHALL be proven correct |
| NFR-06.7 | SPARK verification SHALL be runnable via `make spark-check` |
| NFR-06.8 | Infrastructure/API layers may use `SPARK_Mode => Off` for I/O operations |

**Verification Scope:**

| Layer | SPARK_Mode | Rationale |
|-------|-----------|-----------|
| Domain.* | On | Pure business logic, provable |
| Application.Command.* | On | Commands, provable |
| Application.Port.* | On | Interface contracts |
| Application.UseCase.* | On | Use case logic |
| Infrastructure.* | Off | I/O operations |
| API.* | Off | Facade over infrastructure |

---

## 5. Interface Requirements

### 5.1 User Interfaces

None - this is a library, not an application.

### 5.2 Software Interfaces

#### 5.2.1 Alire Integration

```toml
[[depends-on]]
zoneinfo = "*"
```

#### 5.2.2 Desktop API Usage

```ada
with Zoneinfo.API.Desktop;
use Zoneinfo.API.Desktop;

-- Get current UTC time
UTC_Now : constant Zoned_Result := Now_UTC;

-- Get current time in a specific timezone
Zone    : constant Zone_ID := Zone_ID.From_String ("America/New_York");
Local   : constant Zoned_Result := Now_Zoned (Zone);

-- Convert to civil time
if Local.Is_Ok then
   Civil_Time : constant Civil := To_Civil (Local.Value);
end if;
```

#### 5.2.3 Pure Operations (SPARK-Safe)

```ada
with Zoneinfo.API.Operations;
use Zoneinfo.API.Operations;

-- Add duration to instant
New_Time : constant Instant_Result := Add (Some_Instant, Some_Duration);

-- Calculate difference
Diff : constant Duration_Type := Diff (End_Time, Start_Time);
```

### 5.3 Hardware Interfaces

#### 5.3.1 STM32F769I RTC Interface

- RTC registers for wall clock time
- SysTick for monotonic time
- Direct register access (no HAL dependency)

---

## 6. Traceability Matrix

| Requirement | Design Component | Test |
|-------------|------------------|------|
| REQ-DOM-001 | Domain.Value_Object.Instant | test_instant.adb |
| REQ-DOM-002 | Domain.Value_Object.Zoned | test_zoned.adb |
| REQ-DOM-003 | Domain.Value_Object.Civil | test_civil.adb |
| REQ-DOM-004 | Domain.Value_Object.Duration_Type | test_duration.adb |
| REQ-DOM-005 | Domain.Value_Object.Zone_ID | test_zone_id.adb |
| REQ-DOM-006 | Domain.Error | test_error.adb |
| REQ-DOM-007 | Domain.Error.Result | test_result.adb |
| REQ-APP-001 | Application.Port.Clock_Port | test_clock_port.adb |
| REQ-APP-002 | Application.UseCase.Now | test_now.adb |
| REQ-APP-003 | Application.UseCase.Now_Zoned | test_now_zoned.adb |
| REQ-APP-004 | Application.UseCase.Now_UTC | test_now_utc.adb |
| REQ-INF-001 | Infrastructure.Adapter.Desktop_Clock | test_desktop_clock.adb |
| REQ-INF-002 | Infrastructure.Adapter.STM32F769I_Clock | test_embedded_clock.adb |
| REQ-INF-003 | Infrastructure.Adapter.Mock_Clock | test_mock_clock.adb |
| REQ-API-001 | Zoneinfo.API.Desktop | test_api_desktop.adb |
| REQ-API-002 | Zoneinfo.API.Embedded.STM32F769I | test_api_embedded.adb |
| REQ-API-003 | Zoneinfo.API.Operations | test_api_operations.adb |
| REQ-API-004 | Zoneinfo.API.* | test_api_types.adb |

---

## 7. Appendices

### A. Datetime Conversion Reference

```
Instant ──────────────────► Zoned (add zone)
   ▲                           │
   │                           │
   └───────────────────────────┘ (extract instant)
                               │
                               ▼
                            Civil (always succeeds)
                               │
                               ▼
                            Zoned (may fail: DST gaps/overlaps)
```

### B. UTC Convenience

- `Zone_ID.UTC` - Constant for UTC zone
- `Now_UTC()` - Shorthand for `Now_Zoned(Zone_ID.UTC)`
- `To_UTC(Zoned)` - Convert any Zoned to UTC

### C. Glossary

See Section 1.3 Definitions.

### D. Change History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2025-12-03 | Michael Gardner | Initial zoneinfo-specific SRS |
