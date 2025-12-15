# Zoneinfo Library Documentation

**Version:** 1.0.0
**Date:** December 03, 2025
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** (c) 2025 Michael Gardner, A Bit of Help, Inc.
**Status:** In Development

Timezone-aware datetime manipulation library for Ada 2022.

## Quick Start

- [Quick Start Guide](./quick_start.md)

## Formal Documentation

- [Software Requirements Specification](./formal/software_requirements_specification.md)
- [Software Design Specification](./formal/software_design_specification.md)
- [Software Test Guide](./formal/software_test_guide.md)

## Guides

- [Architecture Enforcement](./guides/architecture_enforcement.md)

## Project-Specific Diagrams

- [Domain Types](./diagrams/domain_types.svg) - Three DateTime kinds (Instant, Zoned, Civil) and Duration
- [Clock Port Pattern](./diagrams/clock_port.svg) - Pluggable time source for desktop vs embedded

## Shared Documentation

See [common/](./common/) for shared library documentation including:

- [Library Architecture](./common/diagrams/library_architecture.svg)
- [API Re-export Pattern (Ada)](./common/diagrams/ada/api_reexport_pattern_ada.svg)
- [Error Handling Flow (Ada)](./common/diagrams/ada/error_handling_flow_ada.svg)
- [Package Structure (Ada)](./common/diagrams/ada/package_structure_ada.svg)
- [Static Dispatch (Ada)](./common/diagrams/ada/static_dispatch_ada.svg)
- [Three Package API (Ada)](./common/diagrams/ada/three_package_api_ada.svg)
