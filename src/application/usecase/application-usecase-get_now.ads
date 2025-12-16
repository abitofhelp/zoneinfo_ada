pragma Ada_2022;
--  =========================================================================
--  Application.Usecase.Get_Now - Get Current Time Use Case
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Generic use case for obtaining the current time. Parameterized by a
--    clock adapter to enable dependency injection at compile time.
--
--  Architecture Notes:
--    - GENERIC USE CASE: Takes Clock adapter as formal parameter
--    - Static dispatch: No runtime overhead for port calls
--    - Testable: Instantiate with Mock_Clock for deterministic tests
--
--  Usage:
--    --  In API.Desktop (composition root):
--    package Get_Now_UC is new Application.Usecase.Get_Now
--      (Now => Infrastructure.Adapter.Desktop_Clock.Now);
--
--    --  Then call:
--    Result := Get_Now_UC.Execute;
--
--  See Also:
--    Application.Port.Outbound.Clock - Clock port signature
--    Infrastructure.Adapter.Desktop_Clock - Production clock
--  =========================================================================

with Domain.Value_Object.Instant;
with Domain.Value_Object.Zoned;
with Domain.Value_Object.Zone_ID;

generic
   --  Clock port: function to get current time
   with function Now return Domain.Value_Object.Instant.Instant_Result.Result;
package Application.Usecase.Get_Now
  with Preelaborate, SPARK_Mode => On
is
   --  Re-export types for convenience
   subtype Instant is Domain.Value_Object.Instant.Instant;
   subtype Zoned is Domain.Value_Object.Zoned.Zoned;
   subtype Zone_ID is Domain.Value_Object.Zone_ID.Zone_ID;
   package Instant_Result renames Domain.Value_Object.Instant.Instant_Result;
   package Zoned_Result renames Domain.Value_Object.Zoned.Zoned_Result;

   --  ========================================================================
   --  Use Case Operations
   --  ========================================================================

   --  Get the current time as an Instant (timezone-agnostic).
   --  Returns:
   --    Ok(Instant) - Current time as epoch nanoseconds
   --    Error(IO_Error) - Clock read failure
   function Execute return Instant_Result.Result;

   --  Get the current time as a Zoned value in the specified timezone.
   --  Note: This does NOT convert to civil time (that requires Timezone_Port).
   --        It simply pairs the current Instant with the Zone_ID.
   --  Returns:
   --    Ok(Zoned) - Current time with timezone context
   --    Error(IO_Error) - Clock read failure
   function Execute_Zoned (Zone : Zone_ID) return Zoned_Result.Result;

   --  Get the current UTC time as a Zoned value.
   --  Convenience function equivalent to Execute_Zoned(UTC).
   function Execute_UTC return Zoned_Result.Result;

end Application.Usecase.Get_Now;
