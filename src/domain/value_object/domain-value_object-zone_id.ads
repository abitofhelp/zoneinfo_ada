pragma Ada_2022;
--  =========================================================================
--  Domain.Value_Object.Zone_ID - Timezone identifier value object
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Defines the Zone_ID value object representing an IANA timezone
--    identifier (e.g., "America/New_York", "Europe/London", "UTC").
--
--  Usage:
--    Zone_Result := From_String ("America/New_York");
--    if Zone_ID_Result.Is_Ok (Zone_Result) then
--       Zone := Zone_ID_Result.Value (Zone_Result);
--       Name := To_String (Zone);
--    end if;
--
--    --  UTC constant for convenience
--    UTC_Zone : constant Zone_ID := UTC;
--
--  Design Notes:
--    - Value object pattern: immutable after creation
--    - Uses bounded strings (memory safe, no heap allocation)
--    - Validation deferred to tzif library at application layer
--    - UTC constant provided for convenience
--    - SPARK-compatible design
--
--  Validation Note:
--    Zone_ID itself only validates string format (non-empty, bounded length).
--    Full IANA timezone validation against the tzif database occurs at the
--    Application layer when the Zone_ID is used for timezone operations.
--
--  See Also:
--    Domain.Value_Object.Zoned - Uses Zone_ID for timezone context
--  =========================================================================

with Ada.Strings.Bounded;
with Domain.Error.Result;
with Zoneinfo_Config;

package Domain.Value_Object.Zone_ID
  with Preelaborate
is
   --  ========================================================================
   --  String Type
   --  ========================================================================

   --  Maximum length for IANA timezone identifiers
   --  Longest known: "America/Argentina/ComodRivadavia" (32 chars)
   --  Size configured per profile in Zoneinfo_Config
   Max_Zone_ID_Length : constant := Zoneinfo_Config.Max_Zone_ID_Length;

   package Zone_ID_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => Max_Zone_ID_Length);

   --  ========================================================================
   --  Zone_ID Value Object
   --  ========================================================================

   --  Represents an IANA timezone identifier.
   --  Examples: "UTC", "America/New_York", "Europe/London"
   type Zone_ID is record
      Value : Zone_ID_Strings.Bounded_String;
   end record;

   --  ========================================================================
   --  Comparison (must be declared before type freezes)
   --  ========================================================================

   --  Zone_IDs are equal if their string values are equal (case-sensitive)
   overriding function "=" (Left, Right : Zone_ID) return Boolean
   with Inline;

   --  Result type for Zone_ID creation
   package Zone_ID_Result is new
     Domain.Error.Result.Generic_Result (T => Zone_ID);

   --  ========================================================================
   --  Constants
   --  ========================================================================

   --  UTC timezone constant for convenience
   --  This is the most commonly used timezone and deserves a constant
   function UTC return Zone_ID
   with
     Inline,
     Post => Is_UTC (UTC'Result);

   --  ========================================================================
   --  Smart Constructor
   --  ========================================================================

   --  Create a Zone_ID from a string
   --  Validation:
   --    - Must not be empty
   --    - Must not exceed Max_Zone_ID_Length
   --
   --  Note: This only validates string format. Full IANA timezone validation
   --  occurs at the Application layer via tzif library.
   function From_String (S : String) return Zone_ID_Result.Result
   with
     Post =>
       (if S'Length = 0 or else S'Length > Max_Zone_ID_Length
        then Zone_ID_Result.Is_Error (From_String'Result)
        else Zone_ID_Result.Is_Ok (From_String'Result));

   --  Create a Zone_ID from a string (unchecked)
   --  Use only when input is valid (e.g., from TZif conversion)
   --  Precondition ensures string is valid length
   function Make_Zone_ID (S : String) return Zone_ID
   with
     Pre => S'Length > 0 and then S'Length <= Max_Zone_ID_Length,
     Post => To_String (Make_Zone_ID'Result) = S,
     Inline;

   --  ========================================================================
   --  Accessors
   --  ========================================================================

   --  Get the string representation of the timezone identifier
   function To_String (Self : Zone_ID) return String
   with
     Inline,
     Post =>
       To_String'Result'Length > 0
       and then To_String'Result'Length <= Max_Zone_ID_Length;

   --  ========================================================================
   --  Queries
   --  ========================================================================

   --  Check if this is the UTC timezone
   function Is_UTC (Self : Zone_ID) return Boolean
   with Inline;

   --  Check if Zone_ID has valid format (non-empty, bounded)
   function Is_Valid (Self : Zone_ID) return Boolean
   with Inline;

end Domain.Value_Object.Zone_ID;
