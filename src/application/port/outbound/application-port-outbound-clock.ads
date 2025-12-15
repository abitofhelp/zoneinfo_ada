pragma Ada_2022;
--  =========================================================================
--  Application.Port.Outbound.Clock - Clock Port Signature
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Defines the Clock_Port signature package - the interface that any
--    clock implementation must satisfy. This is an OUTBOUND port that
--    abstracts how the application obtains the current time.
--
--  Architecture Notes:
--    - SIGNATURE PACKAGE: Defines required interface via formal parameters
--    - Implemented by: Infrastructure adapters (Desktop_Clock, Mock_Clock)
--    - Used by: Application use cases via generic instantiation
--    - Enables: Dependency injection at compile time (static dispatch)
--
--  Design Pattern:
--    Application: "I need something that can get the current time"
--    Infrastructure: "I have Desktop_Clock that reads Ada.Calendar.Clock"
--    API/Desktop: Instantiates use cases with Desktop_Clock adapter
--
--  Implementations:
--    - Infrastructure.Adapter.Desktop_Clock - Real system clock
--    - Test.Mock.Mock_Clock - Controllable clock for testing
--
--  Usage (in generic use case):
--    generic
--       with package Clock is new Application.Port.Outbound.Clock (<>);
--    package Application.UseCase.Get_Now is ...
--
--  See Also:
--    Application.Port.Outbound.Timezone - Timezone data port
--    Infrastructure.Adapter.Desktop_Clock - Production implementation
--  =========================================================================

with Domain.Value_Object.Instant;

package Application.Port.Outbound.Clock
  with Preelaborate
is
   --  ========================================================================
   --  Clock_Port Signature
   --  ========================================================================
   --
   --  Any clock adapter must provide this function:
   --
   --  function Now return Instant_Result.Result;
   --
   --  Returns:
   --    Ok(Instant) - Current time as nanoseconds since Unix epoch
   --    Error(IO_Error) - If clock read fails
   --
   --  This package serves as documentation. The actual signature is
   --  defined in generic formal parameters of use cases.
   --  ========================================================================

   --  Re-export types for convenience in implementations
   subtype Instant is Domain.Value_Object.Instant.Instant;
   package Instant_Result renames Domain.Value_Object.Instant.Instant_Result;

end Application.Port.Outbound.Clock;
