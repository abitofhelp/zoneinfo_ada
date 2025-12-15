# Software Requirements Specification (SRS)

**Version:** 2.1.0
**Date:** December 14, 2025  
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>  
**Status:** Released  

---

## 1. Introduction

### 1.1 Purpose

This Software Requirements Specification (SRS) defines the functional and non-functional requirements for **Hybrid_Lib_Ada**, a canonical Ada 2022 library demonstrating hybrid DDD/Clean/Hexagonal architecture with functional error handling.

### 1.2 Scope

Hybrid_Lib_Ada provides:
- A reusable library for greeting operations
- Demonstration of DDD/Clean/Hexagonal architecture in Ada
- Functional error handling via Result monad
- SPARK-compatible design for formal verification
- Embedded-safe patterns (no heap allocation)

### 1.3 Definitions and Acronyms

| Term | Definition |
|------|------------|
| DDD | Domain-Driven Design - strategic and tactical patterns for complex software |
| Hexagonal Architecture | Ports & Adapters pattern isolating business logic from infrastructure |
| Result Monad | Functional pattern for error handling without exceptions |
| SPARK | Ada subset for formal verification |
| Value Object | Immutable domain object defined by its attributes |

### 1.4 References

- Ada 2022 Reference Manual (ISO/IEC 8652:2023)
- SPARK 2014 Reference Manual
- Domain-Driven Design (Eric Evans, 2003)
- Clean Architecture (Robert C. Martin, 2017)

---

## 2. Overall Description

### 2.1 Product Perspective

Hybrid_Lib_Ada is a standalone library designed to be imported by Ada applications implementing hexagonal (ports and adapters) architecture with clean separation between domain logic, application use cases, and infrastructure adapters.

```
┌─────────────────────────────────────────────────────────┐
│                   Client Application                     │
│                                                          │
│   with Hybrid_Lib_Ada.API;                              │
│   Result := API.Greet (API.Create_Greet_Command ("X")); │
└────────────────────────┬────────────────────────────────┘
                         │
┌────────────────────────▼────────────────────────────────┐
│                   Hybrid_Lib_Ada                         │
│                                                          │
│  API Layer → Application Layer → Domain Layer            │
│       ↓                                                  │
│  Infrastructure Layer (adapters)                         │
└─────────────────────────────────────────────────────────┘
```

**Architecture Layers:**

| Layer | Purpose |
|-------|---------|
| Domain | Pure business logic, value objects, error types |
| Application | Use cases, commands, ports (interfaces) |
| Infrastructure | Adapters for I/O operations |
| API | Public facade with stable interface |

### 2.2 Product Features

1. **Greeting Operations**: Generate personalized greetings via clean architecture
2. **Domain Modeling**: Person value object with validation
3. **Error Handling**: Railway-oriented programming with Result monads
4. **Platform Abstraction**: Generic I/O plugin pattern for portability
5. **Formal Verification**: SPARK-compatible domain layer

### 2.3 User Classes

| User Class | Description |
|------------|-------------|
| Library Consumers | Ada developers integrating library functionality |
| Architecture Students | Learning hexagonal architecture in Ada |
| Embedded Developers | Requiring heap-free, SPARK-compatible patterns |

### 2.4 Operating Environment

| Requirement | Specification |
|-------------|---------------|
| Platforms | POSIX (Linux, macOS, BSD), Windows 11, Embedded |
| Ada Compiler | GNAT FSF 13+ or GNAT Pro |
| Ada Version | Ada 2022 |
| Dependencies | functional ^3.0.0 |

### 2.5 Constraints

| Constraint | Rationale |
|------------|-----------|
| Ada 2022 | Required for modern language features |
| GNAT 13+ | Required compiler version |
| No Heap Allocation | Embedded system compatibility |
| SPARK Subset | Formal verification capability |

---

## 3. Functional Requirements

### 3.1 Domain Layer (FR-01)

**Priority**: High
**Description**: Provide core domain entities and value objects.

| ID | Requirement |
|----|-------------|
| FR-01.1 | Person value object with bounded name (1-100 characters) |
| FR-01.2 | Person is immutable after creation |
| FR-01.3 | Empty names rejected with Validation_Error |
| FR-01.4 | Structured error types with Kind enumeration and Message |
| FR-01.5 | Result monad for all fallible operations |
| FR-01.6 | Error Kinds: Validation_Error, Parse_Error, Not_Found_Error, IO_Error, Internal_Error |

**Acceptance Criteria (FR-01):**
- Person value object validates name on creation
- Result is either Ok(value) or Error(error_info)
- No exceptions raised for expected errors
- Type-safe value extraction

### 3.2 Application Layer (FR-02)

**Priority**: High
**Description**: Provide use cases and port definitions.

| ID | Requirement |
|----|-------------|
| FR-02.1 | Greet_Command data transfer object for greeting requests |
| FR-02.2 | Greet use case orchestrating greeting workflow |
| FR-02.3 | Writer outbound port for output operations |
| FR-02.4 | Static polymorphism via generic function signatures |
| FR-02.5 | Returns Result[Unit] for all operations |

**Acceptance Criteria (FR-02):**
- Greet_Command encapsulates name, is immutable after creation
- Greet use case: accepts command, creates Person, generates greeting, writes via port
- Writer port: Generic signature Write(Message) -> Result[Unit]
- Port is Application-owned, Infrastructure-implemented

### 3.3 Infrastructure Layer (FR-03)

**Priority**: High
**Description**: Provide platform-specific adapters implementing ports.

| ID | Requirement |
|----|-------------|
| FR-03.1 | Console_Writer adapter for standard output |
| FR-03.2 | Implements Writer port contract |
| FR-03.3 | Returns Ok on success, IO_Error on failure |

**Acceptance Criteria (FR-03):**
- Console_Writer writes to standard output
- All I/O errors captured and returned as Result

### 3.4 API Layer (FR-04)

**Priority**: High
**Description**: Provide stable public interface.

| ID | Requirement |
|----|-------------|
| FR-04.1 | Single `Hybrid_Lib_Ada.API` package for imports |
| FR-04.2 | Re-exports Domain types (Person, Error, Unit) |
| FR-04.3 | Re-exports Application types (Greet_Command, Unit_Result) |
| FR-04.4 | API.Operations with SPARK_Mode(On) for verifiable logic |
| FR-04.5 | API.Desktop composition root wiring Console_Writer |

**Acceptance Criteria (FR-04):**
- API.Operations is generic, parameterized by Writer port
- No Infrastructure dependencies in Operations
- API.Desktop wires Console_Writer, uses SPARK_Mode(Off) for I/O wiring

### 3.5 Error Handling (FR-05)

**Priority**: High
**Description**: Railway-oriented error handling without exceptions.

| ID | Requirement |
|----|-------------|
| FR-05.1 | Use Result monad for all fallible operations |
| FR-05.2 | Provide descriptive error messages |
| FR-05.3 | Error codes for all failure modes |
| FR-05.4 | No exceptions in library code |

---

## 4. Non-Functional Requirements

### 4.1 Performance (NFR-01)

| ID | Requirement |
|----|-------------|
| NFR-01.1 | Operation latency < 1ms (excluding I/O) |
| NFR-01.2 | Zero heap allocation |
| NFR-01.3 | Stack usage < 4KB per call |

### 4.2 Reliability (NFR-02)

| ID | Requirement |
|----|-------------|
| NFR-02.1 | All errors returned via Result monad, no exceptions |
| NFR-02.2 | All inputs validated at domain boundary |
| NFR-02.3 | No uncaught exceptions possible |

### 4.3 Portability (NFR-03)

| ID | Requirement |
|----|-------------|
| NFR-03.1 | Support POSIX platforms (Linux, macOS, BSD) |
| NFR-03.2 | Support Windows platforms |
| NFR-03.3 | Support embedded platforms via custom adapters |
| NFR-03.4 | No platform-specific code in domain layer |
| NFR-03.5 | No infrastructure types exposed in application layer ports |
| NFR-03.6 | Platform adapters selectable via GPR configuration |

### 4.4 Maintainability (NFR-04)

| ID | Requirement |
|----|-------------|
| NFR-04.1 | Hexagonal architecture with clear boundaries |
| NFR-04.2 | Comprehensive documentation (docstrings) |
| NFR-04.3 | > 90% test coverage |
| NFR-04.4 | Zero compiler warnings |

### 4.5 Usability (NFR-05)

| ID | Requirement |
|----|-------------|
| NFR-05.1 | Clear, intuitive API |
| NFR-05.2 | Working examples for all use cases |
| NFR-05.3 | Comprehensive error messages |

### 4.6 Platform Abstraction (NFR-06)

| ID | Requirement |
|----|-------------|
| NFR-06.1 | Application layer defines abstract outbound ports using pure function signatures |
| NFR-06.2 | Infrastructure layer provides platform-specific adapters implementing ports |
| NFR-06.3 | Composition roots (API.Desktop, API.Windows, API.Embedded) wire adapters to ports |
| NFR-06.4 | Domain types used in port signatures, not infrastructure types |
| NFR-06.5 | New platforms addable without modifying application layer |
| NFR-06.6 | All platform adapters testable via mock implementations |

### 4.7 SPARK Formal Verification (NFR-07)

| ID | Requirement |
|----|-------------|
| NFR-07.1 | Domain layer shall pass SPARK legality checking |
| NFR-07.2 | All domain packages shall use `SPARK_Mode => On` |
| NFR-07.3 | No runtime errors provable in domain layer (overflow, range, division) |
| NFR-07.4 | All domain variables shall be properly initialized before use |
| NFR-07.5 | Pre/postconditions on domain operations shall be proven correct |
| NFR-07.6 | SPARK legality verification shall be runnable via `make spark-check` |
| NFR-07.7 | SPARK proof verification shall be runnable via `make spark-prove` |
| NFR-07.8 | Infrastructure/API layers may use `SPARK_Mode => Off` for I/O operations |

**Verification Scope:**

| Layer | SPARK_Mode | Rationale |
|-------|-----------|-----------|
| Domain | On | Pure business logic, provable |
| Application | On | Operations, inbound ports, outbound ports |
| Infrastructure | Off | I/O operations |
| API | Off | Facade over infrastructure |

### 4.8 Testability (NFR-08)

| ID | Requirement |
|----|-------------|
| NFR-08.1 | Unit tests for all domain packages |
| NFR-08.2 | Integration tests for cross-layer interactions |
| NFR-08.3 | Mock adapters for testing without I/O |
| NFR-08.4 | Test runners with pass/fail reporting |

---

## 5. System Requirements

### 5.1 Hardware Requirements

| Category | Requirement |
|----------|-------------|
| CPU | Any modern processor |
| RAM | 64 MB minimum |
| Disk | 10 MB minimum |

### 5.2 Software Requirements

| Category | Requirement |
|----------|-------------|
| Operating System | Linux, macOS, BSD, Windows 11 |
| Compiler | GNAT FSF 13+ or GNAT Pro |
| Build System | Alire 2.0+ |

---

## 6. Interface Requirements

### 6.1 User Interfaces

None - this is a library, not an application.

### 6.2 Software Interfaces

#### 6.2.1 Alire Integration

```toml
[[depends-on]]
hybrid_lib_ada = "*"
```

#### 6.2.2 Ada API

```ada
with Hybrid_Lib_Ada.API;
use Hybrid_Lib_Ada.API;

Cmd    : constant Greet_Command := Create_Greet_Command ("Name");
Result : constant Unit_Result.Result := Greet (Cmd);
```

### 6.3 Hardware Interfaces

None specified - library is hardware-agnostic.

---

## 7. Verification and Validation

### 7.1 Verification Methods

| Method | Description |
|--------|-------------|
| Code Review | All code reviewed before merge |
| Static Analysis | Zero compiler warnings |
| Dynamic Testing | All tests must pass |
| Coverage Analysis | > 90% line coverage |

### 7.2 Traceability Matrix

| Requirement | Design | Test |
|-------------|--------|------|
| FR-01.1 | Domain.Value_Object.Person | test_domain_person.adb |
| FR-01.4 | Domain.Error | test_domain_error_result.adb |
| FR-01.5 | Domain.Error.Result | test_domain_error_result.adb |
| FR-02.1 | Application.Command.Greet | test_application_command_greet.adb |
| FR-02.2 | Application.Usecase.Greet | test_application_usecase_greet.adb |
| FR-02.3 | Application.Port.Outbound.Writer | test_application_usecase_greet.adb |
| FR-03.1 | Infrastructure.Adapter.Console_Writer | test_api_greet.adb |
| FR-04.1 | Hybrid_Lib_Ada.API | test_api_greet.adb |
| FR-04.4 | Hybrid_Lib_Ada.API.Operations | test_api_operations.adb |
| FR-04.5 | Hybrid_Lib_Ada.API.Desktop | test_api_greet.adb |

---

## 8. Appendices

### 8.1 Glossary

See Section 1.3 Definitions and Acronyms.

---

**Document Control:**
- Version: 2.1.0
- Last Updated: December 14, 2025
- Status: Released

**Change History:**

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 2.1.0 | 2025-12-14 | Michael Gardner | Remove Section 8.2 Project Statistics (metrics belong in CHANGELOG) |
| 2.0.0 | 2025-12-09 | Michael Gardner | Complete regeneration for v2.0.0; corrected Error_Kind values (5 not 6); added NFR-08 Testability |
| 1.0.0 | 2025-12-08 | Michael Gardner | Initial release |
