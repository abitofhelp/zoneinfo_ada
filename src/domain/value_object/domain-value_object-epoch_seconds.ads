pragma Ada_2022;
--  ===========================================================================
--  Domain.Value_Object.Epoch_Seconds
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Epoch Seconds value object - immutable domain data.
--
--  Responsibilities:
--    - Define Epoch Seconds type and operations
--    - Provide constructors and accessors
--
--  Key Types:
--    Epoch_Seconds_Type
--    Epoch_Seconds_Type
--
--  Dependencies:
--    Inline
--    Inline
--    Inline
--
--  ===========================================================================

package Domain.Value_Object.Epoch_Seconds is

   pragma Pure;

   --  ========================================================================
   --  Epoch_Seconds Type
   --  ========================================================================

   --  Private type ensures type safety (can't mix with regular integers)
   type Epoch_Seconds_Type is private;

   --  ========================================================================
   --  Constructors
   --  ========================================================================

   --  Create from raw seconds value
   --  No validation needed - all Long_Long_Integer values are valid
   function From_Seconds (Seconds : Long_Long_Integer) return Epoch_Seconds_Type
   with Inline;

   --  Common epoch values as constants
   Unix_Epoch : constant Epoch_Seconds_Type;  -- 0 (1970-01-01 00:00:00 UTC)
   Year_2000  : constant Epoch_Seconds_Type;  -- 946684800 (2000-01-01 00:00:00 UTC)
   Year_2025  : constant Epoch_Seconds_Type;  -- 1735689600 (2025-01-01 00:00:00 UTC)

   --  ========================================================================
   --  Query Operations
   --  ========================================================================

   --  Convert to raw seconds value
   function To_Seconds (Epoch : Epoch_Seconds_Type) return Long_Long_Integer
   with Inline;

   --  Convert to human-readable string (for debugging)
   --  Format: "+123456789" or "-123456789"
   function To_String (Epoch : Epoch_Seconds_Type) return String;

   --  ========================================================================
   --  Operators (FR-11.3: Intuitive usage like Ada.Calendar)
   --  ========================================================================

   --  Comparison operators
   overriding function "=" (Left, Right : Epoch_Seconds_Type) return Boolean
   with Inline;

   function "<" (Left, Right : Epoch_Seconds_Type) return Boolean
   with Inline;

   function "<=" (Left, Right : Epoch_Seconds_Type) return Boolean
   with Inline;

   function ">" (Left, Right : Epoch_Seconds_Type) return Boolean
   with Inline;

   function ">=" (Left, Right : Epoch_Seconds_Type) return Boolean
   with Inline;

   --  Arithmetic operators (for duration calculations)
   --  Add/subtract seconds to move forward/backward in time
   function "+" (Left : Epoch_Seconds_Type; Right : Long_Long_Integer)
                 return Epoch_Seconds_Type
   with Inline;

   function "-" (Left : Epoch_Seconds_Type; Right : Long_Long_Integer)
                 return Epoch_Seconds_Type
   with Inline;

   --  Calculate duration between two epochs (in seconds)
   function "-" (Left, Right : Epoch_Seconds_Type) return Long_Long_Integer
   with Inline;

private

   --  ========================================================================
   --  Private Implementation
   --  ========================================================================

   --  Simple wrapper around Long_Long_Integer for type safety
   type Epoch_Seconds_Type is record
      Seconds : Long_Long_Integer := 0;
   end record;

   --  Constants
   Unix_Epoch : constant Epoch_Seconds_Type := (Seconds => 0);
   Year_2000  : constant Epoch_Seconds_Type := (Seconds => 946_684_800);
   Year_2025  : constant Epoch_Seconds_Type := (Seconds => 1_735_689_600);

end Domain.Value_Object.Epoch_Seconds;
