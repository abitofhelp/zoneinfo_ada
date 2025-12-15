pragma Ada_2022;
--  =========================================================================
--  Infrastructure.Adapter.Desktop_Clock - System Clock Adapter
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Concrete adapter implementing Clock_Port using Ada.Calendar.Clock.
--    Provides the current system time as an Instant (epoch nanoseconds).
--
--  Architecture Notes:
--    - ADAPTER: Implements Clock_Port interface
--    - Uses Ada.Calendar.Clock for actual time
--    - Converts Ada.Calendar.Time to Instant (epoch nanoseconds)
--    - Handles exceptions by returning Error results
--
--  Platform:
--    - Desktop/server environments with full Ada runtime
--    - Ada.Calendar available (not bare-metal)
--
--  Usage:
--    --  In API.Desktop composition root:
--    package Get_Now_UC is new Application.Usecase.Get_Now
--      (Now => Infrastructure.Adapter.Desktop_Clock.Now);
--
--  See Also:
--    Application.Port.Outbound.Clock - Port interface
--    Application.Usecase.Get_Now - Use case that uses this adapter
--  =========================================================================

with Domain.Value_Object.Instant;

package Infrastructure.Adapter.Desktop_Clock is
   --  Re-export for convenience
   subtype Instant is Domain.Value_Object.Instant.Instant;
   package Instant_Result renames Domain.Value_Object.Instant.Instant_Result;

   --  ========================================================================
   --  Clock_Port Implementation
   --  ========================================================================

   --  Get the current system time as an Instant.
   --  Returns:
   --    Ok(Instant) - Current time as nanoseconds since Unix epoch
   --    Error(IO_Error) - If reading system clock fails
   function Now return Instant_Result.Result;

end Infrastructure.Adapter.Desktop_Clock;
