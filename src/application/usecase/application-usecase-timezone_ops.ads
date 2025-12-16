pragma Ada_2022;
--  =========================================================================
--  Application.Usecase.Timezone_Ops - Timezone Operations Use Case
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Generic use case for timezone operations. Parameterized by a timezone
--    adapter to enable dependency injection at compile time.
--
--  Architecture Notes:
--    - GENERIC USE CASE: Takes Timezone adapter as formal parameters
--    - Static dispatch: No runtime overhead for port calls
--    - Testable: Instantiate with Mock_Timezone for deterministic tests
--    - tzif is the single source of truth (accessed via adapter)
--
--  Usage:
--    --  In API.Desktop (composition root):
--    package TZ_Ops is new Application.Usecase.Timezone_Ops
--      (To_Civil       => Infrastructure.Adapter.Tzif.To_Civil,
--       To_Instant     => Infrastructure.Adapter.Tzif.To_Instant,
--       Is_Valid_Zone  => Infrastructure.Adapter.Tzif.Is_Valid_Zone,
--       Get_UTC_Offset => Infrastructure.Adapter.Tzif.Get_UTC_Offset);
--
--    --  Then call:
--    Civil_Time := TZ_Ops.Convert_To_Civil (Some_Zoned);
--
--  See Also:
--    Application.Port.Outbound.Timezone - Timezone port signature
--    Infrastructure.Adapter.Tzif - Production timezone adapter
--  =========================================================================

with Domain.Value_Object.Instant;
with Domain.Value_Object.Civil;
with Domain.Value_Object.Zoned;
with Domain.Value_Object.Zone_ID;
with Domain.Value_Object.Duration_Type;

generic
   --  Timezone port: convert Instant to Civil
   with function To_Civil
     (I    : Domain.Value_Object.Instant.Instant;
      Zone : Domain.Value_Object.Zone_ID.Zone_ID)
      return Domain.Value_Object.Civil.Civil;

   --  Timezone port: convert Civil to Instant (may fail)
   with function To_Instant
     (C    : Domain.Value_Object.Civil.Civil;
      Zone : Domain.Value_Object.Zone_ID.Zone_ID)
      return Domain.Value_Object.Instant.Instant_Result.Result;

   --  Timezone port: validate zone exists
   with function Is_Valid_Zone
     (Zone : Domain.Value_Object.Zone_ID.Zone_ID) return Boolean;

   --  Timezone port: get UTC offset at a given instant
   with function Get_UTC_Offset
     (I    : Domain.Value_Object.Instant.Instant;
      Zone : Domain.Value_Object.Zone_ID.Zone_ID)
      return Domain.Value_Object.Duration_Type.Duration_Type;

package Application.Usecase.Timezone_Ops
  with Preelaborate, SPARK_Mode => On
is
   --  Re-export types for convenience
   subtype Instant is Domain.Value_Object.Instant.Instant;
   subtype Civil is Domain.Value_Object.Civil.Civil;
   subtype Zoned is Domain.Value_Object.Zoned.Zoned;
   subtype Zone_ID is Domain.Value_Object.Zone_ID.Zone_ID;
   subtype Duration_Type is Domain.Value_Object.Duration_Type.Duration_Type;

   package Instant_Result renames Domain.Value_Object.Instant.Instant_Result;
   package Zoned_Result renames Domain.Value_Object.Zoned.Zoned_Result;

   --  ========================================================================
   --  Instant/Zoned to Civil Conversion
   --  ========================================================================

   --  Convert a Zoned datetime to Civil (wall clock) time.
   --  This uses the timezone adapter to look up the correct offset.
   --  Always succeeds: every instant maps to exactly one civil time.
   function Convert_To_Civil (Z : Zoned) return Civil;

   --  Convert an Instant to Civil time in the specified timezone.
   function Convert_To_Civil
     (I    : Instant;
      Zone : Zone_ID) return Civil;

   --  ========================================================================
   --  Civil to Instant/Zoned Conversion
   --  ========================================================================

   --  Convert a Civil time to Zoned in the specified timezone.
   --  May fail:
   --    - Gap_Time_Error: Time doesn't exist (DST spring-forward gap)
   --    - Ambiguous_Time_Error: Time occurs twice (DST fall-back)
   --    - Timezone_Error: Invalid timezone
   function Convert_To_Zoned
     (C    : Civil;
      Zone : Zone_ID) return Zoned_Result.Result;

   --  Convert a Civil time to Instant in the specified timezone.
   --  Same failure modes as Convert_To_Zoned.
   function Convert_To_Instant
     (C    : Civil;
      Zone : Zone_ID) return Instant_Result.Result;

   --  ========================================================================
   --  Zone Validation
   --  ========================================================================

   --  Check if a Zone_ID is valid according to the tzif database.
   function Validate_Zone (Zone : Zone_ID) return Boolean;

   --  ========================================================================
   --  UTC Offset Query
   --  ========================================================================

   --  Get the UTC offset for a Zoned datetime.
   --  Returns the duration to ADD to UTC to get local time.
   --  Example: America/New_York during EST returns -5 hours.
   function Get_Offset (Z : Zoned) return Duration_Type;

   --  Get the UTC offset for an Instant in a given timezone.
   function Get_Offset
     (I    : Instant;
      Zone : Zone_ID) return Duration_Type;

end Application.Usecase.Timezone_Ops;
