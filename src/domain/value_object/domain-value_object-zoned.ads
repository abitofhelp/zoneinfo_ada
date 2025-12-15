pragma Ada_2022;
--  =========================================================================
--  Domain.Value_Object.Zoned - Instant with timezone context
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Defines the Zoned value object representing an Instant combined with
--    a timezone identifier. This is the primary type for user-facing
--    datetime operations.
--
--  Usage:
--    --  Create from Instant and Zone
--    Z := Create (Some_Instant, Some_Zone);
--
--    --  Extract components
--    I := To_Instant (Z);
--    Zone := Get_Zone (Z);
--
--    --  Change timezone (preserves instant)
--    New_Zoned := With_Zone (Z, New_Zone_ID);
--
--  Design Notes:
--    - Value object pattern: immutable after creation
--    - Contains Instant + Zone_ID only (no cached UTC offset)
--    - tzif is the single source of truth for all timezone data
--    - To_Civil requires Application layer (uses Timezone_Port -> tzif)
--    - Two Zoned values with same Instant but different zones are NOT equal
--    - SPARK-compatible design
--
--  Timezone Data:
--    The Domain layer does NOT cache UTC offsets. All timezone calculations
--    (including To_Civil) are performed via the Timezone_Port in the
--    Application layer, which delegates to tzif. This ensures tzif remains
--    the single source of truth for timezone data.
--
--  See Also:
--    Domain.Value_Object.Instant - The underlying absolute time
--    Domain.Value_Object.Zone_ID - The timezone identifier
--    Application.Port.Timezone_Port - Port for timezone operations (tzif)
--  =========================================================================

with Domain.Value_Object.Instant;
use Domain.Value_Object.Instant;
with Domain.Value_Object.Zone_ID;
with Domain.Error.Result;

package Domain.Value_Object.Zoned
  with Preelaborate
is
   --  ========================================================================
   --  Re-exports for convenience
   --  ========================================================================

   subtype Instant is Domain.Value_Object.Instant.Instant;
   subtype Zone_ID is Domain.Value_Object.Zone_ID.Zone_ID;

   --  ========================================================================
   --  Zoned Value Object
   --  ========================================================================

   --  Represents an Instant with timezone context.
   --  Note: Does NOT store UTC offset. All timezone calculations go through
   --  the Timezone_Port in the Application layer (backed by tzif).
   type Zoned is record
      Instant_Value : Instant;
      Zone          : Zone_ID;
   end record;

   --  ========================================================================
   --  Comparison (must be declared before type freezes)
   --  ========================================================================

   --  Two Zoned values are equal only if they have the same Instant AND
   --  the same Zone_ID. Same instant in different zones are NOT equal.
   overriding function "=" (Left, Right : Zoned) return Boolean
   with Inline;

   --  Comparison is based on the underlying Instant (absolute time)
   function "<" (Left, Right : Zoned) return Boolean
   with Inline;

   function "<=" (Left, Right : Zoned) return Boolean
   with Inline;

   function ">" (Left, Right : Zoned) return Boolean
   with Inline;

   function ">=" (Left, Right : Zoned) return Boolean
   with Inline;

   --  Result type for Zoned operations
   package Zoned_Result is new
     Domain.Error.Result.Generic_Result (T => Zoned);

   --  ========================================================================
   --  Constructor
   --  ========================================================================

   --  Create a Zoned from an Instant and Zone_ID.
   function Create
     (Instant_Value : Instant;
      Zone          : Zone_ID) return Zoned
   with Inline;

   --  ========================================================================
   --  Accessors
   --  ========================================================================

   --  Extract the underlying Instant
   function To_Instant (Self : Zoned) return Instant
   with Inline;

   --  Get the timezone identifier
   function Get_Zone (Self : Zoned) return Zone_ID
   with Inline;

   --  ========================================================================
   --  Timezone Change
   --  ========================================================================

   --  Create a new Zoned with a different timezone.
   --  The Instant is preserved (same point in time).
   --
   --  Example: Converting from America/New_York to Europe/London
   --  preserves the absolute time but the Civil representation
   --  (computed via Application layer) will differ.
   function With_Zone
     (Self     : Zoned;
      New_Zone : Zone_ID) return Zoned
   with
     Inline,
     Post => To_Instant (With_Zone'Result) = To_Instant (Self);

end Domain.Value_Object.Zoned;
