pragma Ada_2022;
--  =========================================================================
--  Application.Port.Outbound.Timezone - Timezone Port Signature
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Defines the Timezone_Port signature package - the interface that any
--    timezone data provider must satisfy. This is an OUTBOUND port that
--    abstracts access to timezone information (tzif database).
--
--  Architecture Notes:
--    - SIGNATURE PACKAGE: Defines required interface via formal parameters
--    - Implemented by: Infrastructure adapters (Tzif_Adapter)
--    - Used by: Application use cases via generic instantiation
--    - Enables: Dependency injection at compile time (static dispatch)
--    - tzif is the SINGLE SOURCE OF TRUTH for all timezone data
--
--  Design Pattern:
--    Application: "I need to convert Instant+Zone to Civil time"
--    Infrastructure: "I have Tzif_Adapter that reads TZif binary files"
--    API/Desktop: Instantiates use cases with Tzif_Adapter
--
--  Required Operations:
--    - To_Civil: Instant + Zone_ID -> Civil (always succeeds)
--    - To_Instant: Civil + Zone_ID -> Result[Instant]
--        May fail with Gap_Time_Error or Ambiguous_Time_Error
--    - Is_Valid_Zone: Zone_ID -> Boolean (validate against tzif database)
--    - Get_UTC_Offset: Instant + Zone_ID -> Duration_Type
--
--  Implementations:
--    - Infrastructure.Adapter.Tzif_Adapter - Real tzif database access
--    - Test.Mock.Mock_Timezone - Fixed offsets for testing
--
--  See Also:
--    Application.Port.Outbound.Clock - Clock port
--    Infrastructure.Adapter.Tzif_Adapter - Production implementation
--  =========================================================================

with Domain.Value_Object.Instant;
with Domain.Value_Object.Civil;
with Domain.Value_Object.Zone_ID;
with Domain.Value_Object.Duration_Type;

package Application.Port.Outbound.Timezone
  with Preelaborate
is
   --  ========================================================================
   --  Type Re-exports for Implementations
   --  ========================================================================

   subtype Instant is Domain.Value_Object.Instant.Instant;
   subtype Civil is Domain.Value_Object.Civil.Civil;
   subtype Zone_ID is Domain.Value_Object.Zone_ID.Zone_ID;
   subtype Duration_Type is Domain.Value_Object.Duration_Type.Duration_Type;

   package Instant_Result renames Domain.Value_Object.Instant.Instant_Result;
   package Civil_Result renames Domain.Value_Object.Civil.Civil_Result;

   --  ========================================================================
   --  Timezone_Port Signature Documentation
   --  ========================================================================
   --
   --  Any timezone adapter must provide these operations:
   --
   --  function To_Civil
   --    (I    : Instant;
   --     Zone : Zone_ID) return Civil;
   --  --  Convert absolute time to civil time in the given timezone.
   --  --  Always succeeds (every instant maps to exactly one civil time).
   --
   --  function To_Instant
   --    (C    : Civil;
   --     Zone : Zone_ID) return Instant_Result.Result;
   --  --  Convert civil time to absolute time.
   --  --  May fail:
   --  --    - Gap_Time_Error: Civil time doesn't exist (DST spring-forward)
   --  --    - Ambiguous_Time_Error: Civil time occurs twice (DST fall-back)
   --  --    - Timezone_Error: Invalid zone identifier
   --
   --  function Is_Valid_Zone (Zone : Zone_ID) return Boolean;
   --  --  Check if zone exists in tzif database.
   --
   --  function Get_UTC_Offset
   --    (I    : Instant;
   --     Zone : Zone_ID) return Duration_Type;
   --  --  Get UTC offset for the given instant in the given timezone.
   --  --  Returns the offset to ADD to UTC to get local time.
   --
   --  The actual signature is defined in generic formal parameters.
   --  ========================================================================

end Application.Port.Outbound.Timezone;
