# ZoneInfo Development Guides

**Version:** 1.0.0-rc2  
**Date:** November 16, 2025  
**SPDX-License-Identifier:** BSD-3-Clause  
**License File:** See the LICENSE file in the project root.  
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Unreleased  

---

## Overview

This directory contains development guides for understanding and working with the ZoneInfo library architecture, build system, and development practices.

---

## Architecture Guides

### 📚 [Hexagonal Architecture (Hybrid Architecture)](hybrid-architecture/index.md)
**Comprehensive guide to Hexagonal Architecture principles** - Start here for understanding the architectural foundations:
- Architecture overview and core principles
- Domain, Application, and Infrastructure layers
- Bootstrap and Presentation patterns (for library consumers)
- Complete with 16 visual diagrams
- Generic patterns applicable to any Ada 2022 project

### [Architecture Enforcement](architecture_enforcement.md)
How the hexagonal architecture is enforced and validated in ZoneInfo:
- Layer dependency rules
- Architecture validation tools
- Compile-time enforcement
- Common violations and how to avoid them

### [Architecture Mapping](architecture_mapping.md)
Complete mapping of ZoneInfo packages to architecture layers:
- Domain layer packages
- Application layer packages
- Infrastructure layer packages
- Cross-cutting concerns

### [Ports Mapping](ports_mapping.md)
Detailed guide to ZoneInfo inbound and outbound ports:
- Port definitions and responsibilities
- Port implementations
- Adapter pattern usage
- Use case orchestration

---

## Design and Implementation Guides

### [Error Handling Strategy](error_handling_strategy.md)
Comprehensive error handling approach:
- Railway-Oriented Programming
- Result monad patterns
- Error type design
- Error propagation best practices

### [Cache Requirements](cache_requirements.md)
Complete cache system design and requirements:
- Cache architecture
- Export/import functionality
- Performance considerations
- Thread safety

---

## Development Process Guides

### [Build Profiles](build_profiles.md)
Build system configuration and profiles:
- Development vs release builds
- Optimization settings
- Platform-specific configurations
- Alire build profiles

### [Test Infrastructure](test_infrastructure.md)
Test philosophy and infrastructure setup:
- Test strategy and philosophy
- AUnit framework configuration
- Test coverage targeting (95%+)
- Integration-first approach
- Fixture management

### [Test Organization](test_organization.md)
Practical test structure and procedures:
- Test directory layout
- Running tests (unit, integration, E2E)
- Writing new tests
- Test patterns and examples
- Coverage analysis procedures

### [Release Checklist](release_checklist.md)
Complete release preparation procedures:
- Version updates
- Documentation generation
- Testing requirements
- Release validation steps

---

## Related Documentation

- 📖 [Formal Documentation](../index.md) - SRS, SDS, Test Guide
- 🎨 [UML Diagrams](../diagrams/index.md) - Visual architecture documentation
- 📝 [Main README](../../README.md) - Project overview

---

## Contributing

When adding new guides:
1. Follow the naming convention: `TOPIC_NAME.md`
2. Add metadata header (version, date, copyright)
3. Link from this index
4. Keep focused on one topic
5. Include examples where applicable

For questions or improvements to these guides, please file an issue on GitHub.
