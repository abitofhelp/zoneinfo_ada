pragma Ada_2022;
--  =========================================================================
--  Application.Usecase - Parent package for use cases
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Parent package for application use cases (application services).
--    Use cases orchestrate domain logic and coordinate infrastructure.
--
--  Architecture Notes:
--    - Part of the APPLICATION layer
--    - Use cases = Application business logic orchestration
--    - Depends on: Domain (for entities/logic), Ports (for I/O)
--    - Does NOT depend on: Infrastructure or API layer
--    - Use cases ARE the inbound ports (application's public API)
--
--  Design Pattern:
--    Use case pattern from Clean Architecture:
--    1. Accept command DTO from API layer
--    2. Orchestrate domain objects
--    3. Call output ports for side effects
--    4. Return Result to API layer
--
--  See Also:
--    Application.Usecase.Get_Now - Get current time use case
--    Application.Usecase.Timezone_Ops - Timezone operations use case
--    Application.Command - DTOs passed to use cases
--  =========================================================================

package Application.Usecase
  with Pure, SPARK_Mode => On
is

end Application.Usecase;
