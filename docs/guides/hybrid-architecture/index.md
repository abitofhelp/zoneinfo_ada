# Hexagonal Architecture Guide (Hybrid Architecture)

**Version:** 1.0.0-rc2  
**Date:** November 16, 2025  
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Unreleased  

---

## About This Guide

This comprehensive guide documents the **Hexagonal Architecture** (also known as **Ports and Adapters** or **Hybrid Architecture**) used in the ZoneInfo library. These guides are generic architectural patterns that apply to any Ada 2022 project implementing clean architecture principles.

**What is Hexagonal Architecture?**

Hexagonal Architecture ensures business logic remains completely independent of technical details through strict layer separation and dependency inversion. The core principle: dependencies always point inward toward the domain.

```
┌─────────────────────────────────┐
│     Application Layer           │
│   (Use Cases, Ports)            │
│                                  │
│  ┌───────────────────────────┐  │
│  │   Domain Layer            │  │
│  │  (Business Logic)         │  │
│  └───────────────────────────┘  │
└─────────────────────────────────┘
         ↑              ↑
         │              │
  ┌──────┴──────┐  ┌───┴────────┐
  │Infrastructure│  │Infrastructure│
  │(File System) │  │  (Parsing)  │
  └──────────────┘  └─────────────┘
```

---

## Architecture Guides

### Core Architecture (Essential Reading)

1. **[Architecture Overview](architecture-overview.md)**
   Start here! Explains hexagonal architecture principles, dependency inversion via Ada generics, and the overall architectural vision.

2. **[Domain Layer](domain-layer.md)**
   The heart of the application. Pure business logic with zero external dependencies. Learn about value objects, entities, and domain services.

3. **[Application Layer](application-layer.md)**
   Orchestrates use cases by coordinating domain logic. Defines port interfaces that infrastructure implements.

4. **[Infrastructure Layer](infrastructure-layer.md)**
   Implements ports with concrete adapters. Handles all I/O: files, network, databases, console, etc.

### Extended Architecture (For Application Developers)

5. **[Bootstrap Module](bootstrap-module.md)**
   The composition root where all dependencies are wired together. **Note:** ZoneInfo is a library - application developers implement their own bootstrap layer.

6. **[Presentation Layer](presentation-layer.md)**
   User-facing boundary (CLI, HTTP, GUI). **Note:** ZoneInfo is a library - application developers implement their own presentation layer.

---

## ZoneInfo-Specific Architecture

ZoneInfo implements a **3-layer architecture** as a reusable library:

### ✅ Layers Used in ZoneInfo

- **Domain Layer** (`src/domain/`)
  Pure timezone business logic: Zone IDs, Transitions, POSIX TZ rules, error types

- **Application Layer** (`src/application/`)
  Use cases: Find_By_Id, Get_Transition_At_Epoch, Export_Cache, etc.
  Defines ports for adapters

- **Infrastructure Layer** (`src/infrastructure/`)
  Adapters: ZoneInfo parser, file I/O, cache serialization, platform detection

### ❌ Layers NOT in ZoneInfo (Implemented by Library Consumers)

- **Bootstrap Layer**
  Applications consuming ZoneInfo implement their own composition root to wire dependencies

- **Presentation Layer**
  Applications consuming ZoneInfo implement their own CLI/HTTP/GUI interfaces

**Example:** The ZoneInfo examples in `examples/` show how applications wire ZoneInfo library components together (Bootstrap) and present results to users (Presentation).

---

## Key Architectural Principles

### 1. **Domain Purity** ⭐
The Domain layer has **ZERO external dependencies** - not even the `Functional` library. This ensures business logic is:
- Portable (reusable anywhere)
- Testable (no mocking required)
- Maintainable (easy to understand)
- Pure (same inputs → same outputs)

### 2. **Dependency Inversion**
Dependencies flow inward toward the domain:
```
Infrastructure → Application → Domain
```
The Application layer defines **what** it needs (ports), Infrastructure provides **how** (adapters).

### 3. **Railway-Oriented Programming**
Explicit error handling using `Domain.Error.Result` monad with combinators:
- `And_Then` - Monadic bind for chaining operations
- `Map_Error` - Transform errors
- `With_Context` - Add context to error messages

No exceptions for control flow - exceptions only at infrastructure boundaries.

### 4. **Port/Adapter Pattern**
Application defines interfaces (ports), Infrastructure implements them (adapters):
```ada
-- Application defines port (interface)
package Application.Port.Outbound.Zone_Repository is
   type Zone_Repository_Port is interface;
   procedure Find_By_Id (...) is abstract;
end Application.Port.Outbound.Zone_Repository;

-- Infrastructure implements adapter
package Infrastructure.Adapter.Zone_Repository_Adapter is
   type Zone_Repository_Adapter is new Zone_Repository_Port with ...;
   overriding procedure Find_By_Id (...);
end Infrastructure.Adapter.Zone_Repository_Adapter;
```

### 5. **Automated Architecture Validation**
ZoneInfo enforces architecture boundaries automatically:
- GPR project configuration prevents invalid dependencies
- `scripts/validate_architecture.py` validates layer isolation
- CI pipeline runs architecture checks on every commit

---

## Diagrams

All architectural diagrams are available in the [diagrams/](diagrams/) directory:

- `architecture-layers.svg` - Layer dependency diagram
- `component-view.svg` - Component relationships
- `domain-model.svg` - Domain model structure
- `error-handling.svg` - Error flow and conversion
- `package-structure.svg` - Package organization
- `use-case-flow.svg` - Data flow through layers
- And more...

---

## For Application Developers Using ZoneInfo

If you're building an application that uses ZoneInfo, you'll need to:

1. **Implement Bootstrap Layer**
   Wire ZoneInfo components together in your main procedure. See `examples/` for patterns.

2. **Implement Presentation Layer**
   Create CLI/HTTP/GUI interfaces that call ZoneInfo use cases. Examples show CLI patterns.

3. **Follow Architecture Principles**
   Your application should also use hexagonal architecture for consistency and testability.

**Resources:**
- See ZoneInfo `examples/` directory for reference implementations
- Read [Bootstrap Module Guide](bootstrap-module.md) for composition patterns
- Read [Presentation Layer Guide](presentation-layer.md) for UI patterns

---

## Reading Order

**For Library Users (Using ZoneInfo in applications):**
1. Architecture Overview
2. Domain Layer (understand ZoneInfo's business logic)
3. Application Layer (understand ZoneInfo's use cases)
4. Bootstrap Module (learn how to wire ZoneInfo)
5. Presentation Layer (learn how to present ZoneInfo results)

**For Library Contributors (Developing ZoneInfo):**
1. Architecture Overview
2. Domain Layer (understand purity constraints)
3. Application Layer (understand port definitions)
4. Infrastructure Layer (understand adapter patterns)

**For Architecture Students (Learning hexagonal architecture):**
1. Architecture Overview (big picture)
2. Domain Layer (business logic isolation)
3. Application Layer (use case orchestration)
4. Infrastructure Layer (adapter implementation)
5. Bootstrap Module (dependency composition)
6. Presentation Layer (user interface boundary)

---

## External Resources

These guides are based on patterns from:
- **Hexagonal Architecture** (Alistair Cockburn)
- **Clean Architecture** (Robert C. Martin)
- **Domain-Driven Design** (Eric Evans)
- **Railway-Oriented Programming** (Scott Wlaschin)

Adapted for Ada 2022 with emphasis on:
- Generic programming over OOP for dependency injection
- Compile-time dependency resolution
- Strong type safety with Ada's type system
- Zero-cost abstractions

---

## Contributing

This architecture guide is a living document. As we learn from projects like ZoneInfo, we refine the patterns and update this guide.

**Future Plans:**
- Publish as GitHub Pages documentation
- Generate PDF for distribution
- Include in Hybrid Architecture projects as reference
- Continuously update from real-world experience

**Feedback:** support@abitofhelp.com

---

## Document History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0   | Nov 10, 2025 | Initial release - Imported from Hybrid project for ZoneInfo |

---

**Next:** Start with [Architecture Overview](architecture-overview.md) →
