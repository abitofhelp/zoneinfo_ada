pragma Ada_2022;
--  ===========================================================================
--  Zoneinfo.API - Public Library Interface
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Public API facade for the Zoneinfo library. Re-exports domain types
--    for timezone-aware datetime operations.
--
--  Architecture:
--    - Facade layer: Re-exports Domain types
--    - Platform-specific APIs in child packages (API.Desktop, etc.)
--    - Pure operations in API.Operations (SPARK-safe)
--
--  Usage:
--    with Zoneinfo.API;
--    use Zoneinfo.API;
--
--    --  Create domain types
--    Zone : constant Zone_ID := Zone_ID_Result.Value
--             (Zone_ID_Pkg.From_String ("America/New_York"));
--    D    : constant Duration_Type := Duration_Pkg.From_Seconds (3600);
--
--  ===========================================================================

--  Import domain types
with Domain.Value_Object.Instant;
with Domain.Value_Object.Zoned;
with Domain.Value_Object.Civil;
with Domain.Value_Object.Duration_Type;
with Domain.Value_Object.Zone_ID;
with Domain.Error;
with Domain.Unit;

package Zoneinfo.API
  with Preelaborate
is

   --  ========================================================================
   --  Re-export Domain Types
   --  ========================================================================

   --  Instant - Absolute moment in time (epoch nanoseconds)
   subtype Instant is Domain.Value_Object.Instant.Instant;
   package Instant_Result renames Domain.Value_Object.Instant.Instant_Result;
   package Instant_Pkg renames Domain.Value_Object.Instant;

   --  Zoned - Instant with timezone context
   subtype Zoned is Domain.Value_Object.Zoned.Zoned;
   package Zoned_Result renames Domain.Value_Object.Zoned.Zoned_Result;
   package Zoned_Pkg renames Domain.Value_Object.Zoned;

   --  Civil - Timezone-blind calendar components
   subtype Civil is Domain.Value_Object.Civil.Civil;
   package Civil_Result renames Domain.Value_Object.Civil.Civil_Result;
   package Civil_Pkg renames Domain.Value_Object.Civil;

   --  Duration_Type - Time span
   subtype Duration_Type is Domain.Value_Object.Duration_Type.Duration_Type;
   package Duration_Pkg renames Domain.Value_Object.Duration_Type;

   --  Zone_ID - IANA timezone identifier
   subtype Zone_ID is Domain.Value_Object.Zone_ID.Zone_ID;
   package Zone_ID_Result renames Domain.Value_Object.Zone_ID.Zone_ID_Result;
   package Zone_ID_Pkg renames Domain.Value_Object.Zone_ID;

   --  ========================================================================
   --  Re-export Error Types
   --  ========================================================================

   subtype Error_Type is Domain.Error.Error_Type;
   subtype Error_Kind is Domain.Error.Error_Kind;
   package Error_Strings renames Domain.Error.Error_Strings;

   --  Error kind constants for pattern matching
   Validation_Error     : Error_Kind renames Domain.Error.Validation_Error;
   Timezone_Error       : Error_Kind renames Domain.Error.Timezone_Error;
   Overflow_Error       : Error_Kind renames Domain.Error.Overflow_Error;
   Ambiguous_Time_Error : Error_Kind renames Domain.Error.Ambiguous_Time_Error;
   Gap_Time_Error       : Error_Kind renames Domain.Error.Gap_Time_Error;
   IO_Error             : Error_Kind renames Domain.Error.IO_Error;
   Internal_Error       : Error_Kind renames Domain.Error.Internal_Error;

   --  ========================================================================
   --  Re-export Unit Type
   --  ========================================================================

   subtype Unit is Domain.Unit.Unit;
   Unit_Value : Unit renames Domain.Unit.Unit_Value;

   --  ========================================================================
   --  Convenience Constants
   --  ========================================================================

   --  UTC timezone constant
   function UTC return Zone_ID renames Zone_ID_Pkg.UTC;

end Zoneinfo.API;
