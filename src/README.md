# ZoneInfo Source Code

Source code for the ZoneInfo library, organized using **Hexagonal Architecture** (Ports & Adapters) principles with strict layer separation.

## Architecture Overview

```
src/
├── api/              # Public API Layer (Facade)
├── domain/           # Domain Layer (Business Logic)
│   ├── entity/       # Domain Entities
│   ├── error/        # Error Handling (Result types)
│   ├── service/      # Domain Services
│   └── value_object/ # Value Objects (immutable data)
├── application/      # Application Layer (Use Cases)
│   ├── port/         # Ports (interfaces)
│   └── usecase/      # Use Case Implementations
└── infrastructure/   # Infrastructure Layer (Adapters)
    └── adapter/      # Concrete Adapters
```

## Layer Responsibilities

### 1. API Layer (`api/`)

**Purpose**: Public facade providing simple, stable interface to clients

**Key Files**:
- `zoneinfo-api.ads` - Public API specification
- `zoneinfo-api.adb` - API implementation (delegates to application layer)

**Responsibilities**:
- Expose minimal, stable public interface
- Hide internal complexity
- Provide convenience functions
- Type conversion and validation

**Dependencies**: ✅ Can depend on Domain and Application layers

**Example**:
```ada
-- Public API function
function Get_UTC_Offset
  (Zone : Zone_Id;
   At_Time : Ada.Calendar.Time) return UTC_Offset_Result;
```

**Design Pattern**: **Facade Pattern**

### 2. Domain Layer (`domain/`)

**Purpose**: Core business logic, pure functions, no external dependencies

**Subdirectories**:

#### `domain/entity/` - Domain Entities

Entities with identity that persist over time:
- `domain-entity-zone.ads` - Timezone entity

#### `domain/error/` - Error Handling

Railway-Oriented Programming with Result types:
- `domain-error.ads` - Base error types
- `domain-error-result.ads` - Result monad implementation

#### `domain/service/` - Domain Services

Business logic that doesn't fit in entities:
- `domain-service-timezone_lookup.ads` - Timezone lookup logic

#### `domain/value_object/` - Value Objects

Immutable data types representing domain concepts:
- `domain-value_object-zone_id.ads` - Timezone identifier
- `domain-value_object-utc_offset.ads` - UTC offset value
- `domain-value_object-epoch_seconds.ads` - Unix timestamp

**Characteristics**:
- ✅ Pure functions (no side effects)
- ✅ Immutable data structures
- ✅ No external dependencies (only Ada standard library)
- ✅ Railway-Oriented Programming (Result types)
- ✅ Self-contained business rules

**Design Patterns**:
- **Value Object Pattern** - Immutable domain concepts
- **Result Monad** - Functional error handling
- **Domain Service** - Stateless business logic

### 3. Application Layer (`application/`)

**Purpose**: Orchestrate domain objects to implement use cases

**Subdirectories**:

#### `application/port/` - Ports (Interfaces)

Defines interfaces for external dependencies:
- `application-port-inbound.ads` - Inbound ports (use cases)
- `application-port-outbound.ads` - Outbound ports (repositories, services)

#### `application/usecase/` - Use Cases

Concrete implementations of application workflows:
- Coordinate domain objects
- Implement business processes
- Call infrastructure through ports

**Responsibilities**:
- Implement use cases
- Define ports (interfaces)
- Coordinate domain objects
- Transaction boundaries (if applicable)

**Dependencies**:
- ✅ Can depend on Domain layer
- ✅ Depends on Port interfaces (not implementations)
- ❌ Must NOT depend directly on Infrastructure

**Design Patterns**:
- **Port/Adapter (Hexagonal)** - Dependency inversion
- **Use Case Pattern** - Application workflows
- **Dependency Injection** - Ports injected at startup

### 4. Infrastructure Layer (`infrastructure/`)

**Purpose**: Implement technical concerns and external integrations

**Subdirectories**:

#### `infrastructure/adapter/` - Concrete Adapters

Implementations of ports defined in application layer:
- `infrastructure-adapter-file_system.ads` - File system operations
- `infrastructure-adapter-tzif_parser.ads` - TZif file parsing
- `infrastructure-cache.ads` - Caching implementation

**Responsibilities**:
- Implement port interfaces
- Handle I/O operations
- Parse external data formats (TZif files)
- Manage caching and performance optimization
- System-specific code

**Dependencies**:
- ✅ Can depend on Domain layer
- ✅ Implements Application ports
- ✅ Can use external libraries (TZif library)

**Design Patterns**:
- **Adapter Pattern** - Implement port interfaces
- **Repository Pattern** - Data access
- **Cache Pattern** - Performance optimization

## Dependency Rules

### Allowed Dependencies

```
API → Application → Domain
  ↓
Infrastructure → Domain
  ↓
(Implements) Application Ports
```

### Forbidden Dependencies

```
Domain ❌→ Application (Domain must be independent)
Domain ❌→ Infrastructure (Domain must be pure)
Application ❌→ Infrastructure (Use ports instead)
```

## Key Design Principles

### 1. Dependency Inversion

Inner layers don't know about outer layers:
- Domain is completely independent
- Application defines ports (interfaces)
- Infrastructure implements ports

### 2. Railway-Oriented Programming

All operations return Result types:

```ada
-- Success path
Zone_Result : constant Zone_Id_Result :=
  Domain.Value_Object.Zone_Id.Result.Create ("America/New_York");

if Domain.Value_Object.Zone_Id.Result.Is_Ok (Zone_Result) then
   Zone : constant Zone_Id :=
     Domain.Value_Object.Zone_Id.Result.Value (Zone_Result);
   -- Continue on success track
else
   Error : constant Domain_Error :=
     Domain.Value_Object.Zone_Id.Result.Error (Zone_Result);
   -- Handle error track
end if;
```

### 3. Immutability

Value objects are immutable:
```ada
-- Cannot modify after creation
Zone : constant Zone_Id := ...;  -- Immutable
```

### 4. Pure Functions

Domain functions have no side effects:
```ada
-- Pure function - always returns same output for same input
function Calculate_Offset
  (Zone : Zone_Id;
   At_Time : Epoch_Seconds) return UTC_Offset;
```

## Package Naming Convention

### Pattern

`Layer.Sublayer.Component`

### Examples

- `Domain.Value_Object.Zone_Id` - Domain layer value object
- `Domain.Error.Result` - Domain error handling
- `Application.Port.Inbound.Find_By_Id` - Application inbound port
- `Infrastructure.Adapter.File_System` - Infrastructure adapter

## Building Source Code

### Standard Build

```bash
# Build library
alr build

# Build with specific mode
alr build -- -Xmode=release    # Optimized build
alr build -- -Xmode=debug      # Debug build
```

### GPR Project Structure

Main project file: `zoneinfo.gpr`

**Imports**:
- `config/zoneinfo_config.gpr` - Build configuration
- TZif library dependency (via Alire)

**Library Configuration**:
- Type: Stand-alone library
- Public Interface: API + Domain layers only
- Private: Application + Infrastructure layers

## Testing Source Code

### Unit Tests

Test individual components:
```bash
alr test
```

### Integration Tests

Test component interactions:
```bash
alr test
```

### Code Coverage

```bash
# Generate coverage report
alr exec -- gnatcov coverage ...
```

## Code Style

### Ada 2022 Standards

- Ada 2022 language features enabled
- Full warnings enabled (`-gnatwa`)
- Style checks enabled (`-gnatyyy`)
- Maximum line length: 120 characters

### Coding Conventions

**Naming**:
- Types: `Type_Name` (capitalized words)
- Variables: `Variable_Name` (capitalized words)
- Constants: `Constant_Name` (capitalized words)
- Functions: `Function_Name` (verb phrases)

**Style**:
- 3-space indentation
- No tabs (spaces only)
- End labels required
- Maximum line length: 120 characters

## Key Source Files

### Public API

- `api/zoneinfo-api.ads` - Main public interface
- `api/zoneinfo-api.adb` - API implementation

### Domain Core

- `domain/value_object/zone_id.ads` - Timezone identifier
- `domain/value_object/utc_offset.ads` - UTC offset value
- `domain/error/result.ads` - Result monad
- `domain/service/timezone_lookup.ads` - Lookup logic

### Infrastructure

- `infrastructure/adapter/file_system.ads` - File operations
- `infrastructure/adapter/tzif_parser.ads` - TZif parsing
- `infrastructure/cache.ads` - Caching system

## Architecture Diagrams

See `docs/diagrams/` for:
- Hexagonal architecture diagram
- Layer dependency diagram
- Component interaction diagrams

## Related Documentation

- [Hexagonal Architecture Guide](../docs/guides/hybrid-architecture/)
- [Functional Programming Guide](../docs/guides/functional/)
- [API Documentation](../docs/index.md)
- [Examples](../examples/README.md)

---

**Copyright**: © 2025 Michael Gardner, A Bit of Help, Inc.
**License**: BSD-3-Clause
