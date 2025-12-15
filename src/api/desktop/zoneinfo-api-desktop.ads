pragma Ada_2022;
--  ===========================================================================
--  Zoneinfo.API.Desktop - Desktop Platform Composition Root
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Desktop/server platform composition root. Wires infrastructure adapters
--    (Desktop_Clock, Tzif_Adapter) to provide ready-to-use datetime
--    operations for desktop applications.
--
--  Architecture:
--    - COMPOSITION ROOT for libraries (4-layer model)
--    - SPARK-Mode Off: Contains I/O wiring, not formally verifiable
--    - Instantiates use cases with Desktop_Clock and Tzif adapters
--    - Provides convenient API for common datetime operations
--
--  Target Platform:
--    - Linux / macOS / Windows
--    - Full Ada runtime with Ada.Calendar
--
--  Usage:
--    with Zoneinfo.API.Desktop;
--    use Zoneinfo.API.Desktop;
--
--    --  Get current time
--    Now_Result : constant Instant_Result.Result := Now;
--    UTC_Result : constant Zoned_Result.Result := Now_UTC;
--
--    --  Convert between representations
--    Civil_Time : constant Civil := To_Civil (Some_Zoned);
--    Zoned_Result : constant Zoned_Result.Result :=
--      To_Zoned (Some_Civil, Some_Zone);
--
--  ===========================================================================

pragma SPARK_Mode (Off);

with Domain.Value_Object.Instant;
with Domain.Value_Object.Civil;
with Domain.Value_Object.Zoned;
with Domain.Value_Object.Zone_ID;
with Domain.Value_Object.Duration_Type;

with Application.Usecase.Get_Now;
with Application.Usecase.Timezone_Ops;
with Infrastructure.Adapter.Desktop_Clock;
with Infrastructure.Adapter.Tzif;

package Zoneinfo.API.Desktop is
   --  ========================================================================
   --  Type Re-exports for Convenience
   --  ========================================================================

   subtype Instant is Domain.Value_Object.Instant.Instant;
   subtype Civil is Domain.Value_Object.Civil.Civil;
   subtype Zoned is Domain.Value_Object.Zoned.Zoned;
   subtype Zone_ID is Domain.Value_Object.Zone_ID.Zone_ID;
   subtype Duration_Type is Domain.Value_Object.Duration_Type.Duration_Type;

   package Instant_Result renames Domain.Value_Object.Instant.Instant_Result;
   package Zoned_Result renames Domain.Value_Object.Zoned.Zoned_Result;

   --  ========================================================================
   --  Wired Use Cases (Internal)
   --  ========================================================================

   --  Get_Now use case instantiated with Desktop_Clock
   package Clock_UC is new Application.Usecase.Get_Now
     (Now => Infrastructure.Adapter.Desktop_Clock.Now);

   --  Timezone_Ops use case instantiated with Tzif adapter
   package TZ_UC is new Application.Usecase.Timezone_Ops
     (To_Civil       => Infrastructure.Adapter.Tzif.To_Civil,
      To_Instant     => Infrastructure.Adapter.Tzif.To_Instant,
      Is_Valid_Zone  => Infrastructure.Adapter.Tzif.Is_Valid_Zone,
      Get_UTC_Offset => Infrastructure.Adapter.Tzif.Get_UTC_Offset);

   --  ========================================================================
   --  Clock Operations
   --  ========================================================================

   --  Get the current time as an Instant.
   function Now return Instant_Result.Result
     renames Clock_UC.Execute;

   --  Get the current time as a Zoned value in the specified timezone.
   function Now_Zoned (Zone : Zone_ID) return Zoned_Result.Result
     renames Clock_UC.Execute_Zoned;

   --  Get the current UTC time as a Zoned value.
   function Now_UTC return Zoned_Result.Result
     renames Clock_UC.Execute_UTC;

   --  ========================================================================
   --  Timezone Conversion Operations
   --  ========================================================================

   --  Convert a Zoned datetime to Civil (wall clock) time.
   function To_Civil (Z : Zoned) return Civil
     renames TZ_UC.Convert_To_Civil;

   --  Convert an Instant to Civil time in the specified timezone.
   function To_Civil
     (I    : Instant;
      Zone : Zone_ID) return Civil
     renames TZ_UC.Convert_To_Civil;

   --  Convert a Civil time to Zoned in the specified timezone.
   --  May fail with Gap_Time_Error or Ambiguous_Time_Error.
   function To_Zoned
     (C    : Civil;
      Zone : Zone_ID) return Zoned_Result.Result
     renames TZ_UC.Convert_To_Zoned;

   --  Convert a Civil time to Instant in the specified timezone.
   function To_Instant
     (C    : Civil;
      Zone : Zone_ID) return Instant_Result.Result
     renames TZ_UC.Convert_To_Instant;

   --  ========================================================================
   --  Zone Validation
   --  ========================================================================

   --  Check if a Zone_ID is valid according to the tzif database.
   function Is_Valid_Zone (Zone : Zone_ID) return Boolean
     renames TZ_UC.Validate_Zone;

   --  ========================================================================
   --  UTC Offset Query
   --  ========================================================================

   --  Get the UTC offset for a Zoned datetime.
   function Get_Offset (Z : Zoned) return Duration_Type
     renames TZ_UC.Get_Offset;

   --  Get the UTC offset for an Instant in a given timezone.
   function Get_Offset
     (I    : Instant;
      Zone : Zone_ID) return Duration_Type
     renames TZ_UC.Get_Offset;

   --  ========================================================================
   --  Timezone Change
   --  ========================================================================

   --  Create a new Zoned with a different timezone.
   --  The Instant (absolute time) is preserved; only the zone changes.
   --
   --  Example: Converting from America/New_York to Europe/London
   --  preserves the absolute time but the Civil representation
   --  (from To_Civil) will show the local time in the new zone.
   function With_Zone
     (Z        : Zoned;
      New_Zone : Zone_ID) return Zoned
     renames Domain.Value_Object.Zoned.With_Zone;

   --  ========================================================================
   --  Zoned Accessors
   --  ========================================================================

   --  Extract the underlying Instant from a Zoned value.
   function To_Instant (Z : Zoned) return Instant
     renames Domain.Value_Object.Zoned.To_Instant;

   --  Get the Zone_ID from a Zoned value.
   function Get_Zone (Z : Zoned) return Zone_ID
     renames Domain.Value_Object.Zoned.Get_Zone;

   --  ========================================================================
   --  Zone_ID Utilities
   --  ========================================================================

   --  Get the UTC Zone_ID constant.
   function UTC return Zone_ID
     renames Domain.Value_Object.Zone_ID.UTC;

   --  Create a Zone_ID from a string.
   --  Note: This validates format only; use Is_Valid_Zone to check
   --  against the tzif database.
   package Zone_ID_Result renames Domain.Value_Object.Zone_ID.Zone_ID_Result;

   function Zone_From_String (S : String) return Zone_ID_Result.Result
     renames Domain.Value_Object.Zone_ID.From_String;

   --  Convert Zone_ID to string.
   function Zone_To_String (Zone : Zone_ID) return String
     renames Domain.Value_Object.Zone_ID.To_String;

end Zoneinfo.API.Desktop;
