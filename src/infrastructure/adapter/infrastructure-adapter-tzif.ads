pragma Ada_2022;
--  =========================================================================
--  Infrastructure.Adapter.Tzif - TZif Database Adapter
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Concrete adapter implementing Timezone_Port using tzif binary files.
--    Provides timezone conversion operations by reading IANA TZif data.
--
--  Architecture Notes:
--    - ADAPTER: Implements Timezone_Port interface
--    - Uses tzif library for TZif file parsing
--    - tzif is SINGLE SOURCE OF TRUTH for all timezone data
--    - Handles DST transitions, historical changes, leap seconds
--
--  Platform:
--    - Desktop/server environments with filesystem access
--    - Requires /usr/share/zoneinfo or equivalent tzdata
--
--  Usage:
--    --  In API.Desktop composition root:
--    package TZ_Ops is new Application.Usecase.Timezone_Ops
--      (To_Civil       => Infrastructure.Adapter.Tzif.To_Civil,
--       To_Instant     => Infrastructure.Adapter.Tzif.To_Instant,
--       Is_Valid_Zone  => Infrastructure.Adapter.Tzif.Is_Valid_Zone,
--       Get_UTC_Offset => Infrastructure.Adapter.Tzif.Get_UTC_Offset);
--
--  See Also:
--    Application.Port.Outbound.Timezone - Port interface
--    Application.Usecase.Timezone_Ops - Use case that uses this adapter
--  =========================================================================

with Domain.Value_Object.Instant;
with Domain.Value_Object.Civil;
with Domain.Value_Object.Zone_ID;
with Domain.Value_Object.Duration_Type;

package Infrastructure.Adapter.Tzif is
   --  Re-export types for convenience
   subtype Instant is Domain.Value_Object.Instant.Instant;
   subtype Civil is Domain.Value_Object.Civil.Civil;
   subtype Zone_ID is Domain.Value_Object.Zone_ID.Zone_ID;
   subtype Duration_Type is Domain.Value_Object.Duration_Type.Duration_Type;

   package Instant_Result renames Domain.Value_Object.Instant.Instant_Result;

   --  ========================================================================
   --  Timezone_Port Implementation
   --  ========================================================================

   --  Convert an Instant to Civil time in the specified timezone.
   --  Always succeeds: every instant maps to exactly one civil time.
   --  Uses tzif library to get correct UTC offset for the zone.
   function To_Civil
     (I    : Instant;
      Zone : Zone_ID) return Civil;

   --  Convert a Civil time to Instant in the specified timezone.
   --  May fail:
   --    - Gap_Time_Error: Time doesn't exist (DST spring-forward)
   --    - Ambiguous_Time_Error: Time occurs twice (DST fall-back)
   --    - Timezone_Error: Invalid timezone identifier
   function To_Instant
     (C    : Civil;
      Zone : Zone_ID) return Instant_Result.Result;

   --  Check if a Zone_ID exists in the tzif database.
   --  Uses tzif library to validate against system tzdata.
   function Is_Valid_Zone (Zone : Zone_ID) return Boolean;

   --  Get the UTC offset for an Instant in the specified timezone.
   --  Returns the duration to ADD to UTC to get local time.
   --  Uses tzif library to query offset including DST.
   function Get_UTC_Offset
     (I    : Instant;
      Zone : Zone_ID) return Duration_Type;

end Infrastructure.Adapter.Tzif;
