pragma Ada_2022;
--  ===========================================================================
--  Domain.Value_Object.Dst_Info
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Dst Info value object - immutable domain data.
--
--  Responsibilities:
--    - Define Dst Info type and operations
--    - Provide constructors and accessors
--
--  Key Types:
--    Disambiguation_Strategy
--    provides
--    Ambiguity_Info_Type
--
--  ===========================================================================

with Domain.Value_Object.UTC_Offset;

package Domain.Value_Object.DST_Info is

   pragma Elaborate_Body;

   use Domain.Value_Object.UTC_Offset;

   --  ========================================================================
   --  Disambiguation Strategy for Ambiguous Times
   --  ========================================================================

   --  Strategy for resolving ambiguous local times during DST fall-back.
   --
   --  When clocks fall back (e.g., 02:00 → 01:00), local times between
   --  01:00-02:00 occur twice:
   --    - Once during DST (e.g., 01:30 EDT = UTC-4)
   --    - Once during standard time (e.g., 01:30 EST = UTC-5)
   --
   --  Example: America/New_York, 2024-11-03 01:30:00
   --    - Prefer_Earlier: Choose EDT (UTC-4) - the first occurrence
   --    - Prefer_Later:   Choose EST (UTC-5) - the second occurrence
   --    - Raise_On_Ambiguous: Return error requiring user to handle ambiguity
   type Disambiguation_Strategy is
     (Prefer_Earlier,      --  Use DST offset (first occurrence)
      Prefer_Later,        --  Use standard offset (second occurrence)
      Raise_On_Ambiguous); --  Return error for ambiguous times

   --  Default strategy: Prefer_Earlier (matches Python zoneinfo "fold=0" behavior)
   Default_Disambiguation : constant Disambiguation_Strategy := Prefer_Earlier;

   --  ========================================================================
   --  Ambiguity Information Type
   --  ========================================================================

   --  Detailed information about DST ambiguity at a specific time.
   --
   --  This type provides complete information about whether a local time
   --  is ambiguous (occurs twice) or in a gap (never exists), along with
   --  the possible UTC offsets.
   type Ambiguity_Info_Type is record
      --  True if this local time occurs twice (DST fall-back)
      Is_Ambiguous : Boolean := False;

      --  True if this local time never exists (DST spring-forward gap)
      Is_Gap : Boolean := False;

      --  UTC offset for first occurrence (or only occurrence if not ambiguous)
      --  For fall-back: This is the DST offset (e.g., UTC-4)
      Earlier_Offset : UTC_Offset_Type;

      --  UTC offset for second occurrence (only valid if Is_Ambiguous = True)
      --  For fall-back: This is the standard offset (e.g., UTC-5)
      Later_Offset : UTC_Offset_Type;

      --  Offset that would apply immediately after gap (only valid if Is_Gap = True)
      --  For spring-forward: The DST offset (e.g., UTC-4)
      Post_Gap_Offset : UTC_Offset_Type;
   end record;

   --  Constructor for unambiguous time (normal case)
   function Make_Unambiguous_Info
     (Offset : UTC_Offset_Type)
      return Ambiguity_Info_Type;

   --  Constructor for ambiguous time (DST fall-back)
   function Make_Ambiguous_Info
     (Earlier_Offset : UTC_Offset_Type;
      Later_Offset   : UTC_Offset_Type)
      return Ambiguity_Info_Type;

   --  Constructor for gap time (DST spring-forward)
   function Make_Gap_Info
     (Pre_Gap_Offset  : UTC_Offset_Type;
      Post_Gap_Offset : UTC_Offset_Type)
      return Ambiguity_Info_Type;

   --  ========================================================================
   --  Query Functions
   --  ========================================================================

   --  Check if time is normal (not ambiguous, not in gap)
   function Is_Normal_Time
     (Info : Ambiguity_Info_Type)
      return Boolean
   is (not Info.Is_Ambiguous and not Info.Is_Gap);

   --  Get the offset to use based on disambiguation strategy
   --
   --  For ambiguous times:
   --    - Prefer_Earlier: Returns Earlier_Offset (DST)
   --    - Prefer_Later:   Returns Later_Offset (standard)
   --
   --  For normal times: Returns the only applicable offset
   --
   --  For gap times: Returns Post_Gap_Offset
   function Get_Offset_For_Strategy
     (Info     : Ambiguity_Info_Type;
      Strategy : Disambiguation_Strategy := Default_Disambiguation)
      return UTC_Offset_Type
   is (if Info.Is_Ambiguous then
         (case Strategy is
            when Prefer_Earlier | Raise_On_Ambiguous => Info.Earlier_Offset,
            when Prefer_Later => Info.Later_Offset)
       elsif Info.Is_Gap then
         Info.Post_Gap_Offset
       else
         Info.Earlier_Offset);

   --  Check if strategy would raise an error
   function Should_Raise_Error
     (Info     : Ambiguity_Info_Type;
      Strategy : Disambiguation_Strategy)
      return Boolean
   is (Strategy = Raise_On_Ambiguous and then Info.Is_Ambiguous);

end Domain.Value_Object.DST_Info;
