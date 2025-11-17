pragma Ada_2022;
--  ===========================================================================
--  Zoneinfo.Api
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Api interface and type definitions.
--
--  Key Types:
--    Zone_Array
--
--  ===========================================================================

package ZoneInfo.API is

   pragma Elaborate_Body;

   --  ========================================================================
   --  Core Timezone Operations
   --  ========================================================================

   --  Get UTC offset for a specific time in a specific timezone
   --
   --  For ambiguous times (DST fall-back), uses Prefer_Earlier strategy by default.
   --  For full control over ambiguous times, use Get_UTC_Offset_With_Disambiguation.
   --
   --  Parameters:
   --    Zone: Timezone identifier (e.g., "America/New_York")
   --    Time: The time to query (Ada.Calendar.Time)
   --
   --  Returns:
   --    Result containing UTC_Offset, or Error if:
   --      - Zone not found in database
   --      - Time outside valid range
   --      - Timezone data parsing failed
   --
   --  Example:
   --    Zone_Result := Create_Zone_Id("America/New_York");
   --    if Is_Ok(Zone_Result) then
   --       Offset_Result := Get_UTC_Offset(Value(Zone_Result), Clock);
   --       if Is_Ok(Offset_Result) then
   --          -- Use Value(Offset_Result)
   --       end if;
   --    end if;
   function Get_UTC_Offset
     (Zone : ZoneInfo.Zone_Id;
      T    : ZoneInfo.Time)
      return ZoneInfo.UTC_Offset_Result;

   --  Get UTC offset with explicit disambiguation strategy for ambiguous times
   --
   --  This function provides full control over how ambiguous local times are handled.
   --  Use this when you need to explicitly choose between earlier/later occurrences
   --  during DST fall-back transitions.
   --
   --  Parameters:
   --    Zone: Timezone identifier
   --    T: Time to query
   --    Strategy: How to handle ambiguous times:
   --       - Prefer_Earlier: Use DST offset (first occurrence) [default]
   --       - Prefer_Later: Use standard offset (second occurrence)
   --       - Raise_On_Ambiguous: Return error for ambiguous times
   --
   --  Returns:
   --    Result containing UTC_Offset, or Error if:
   --      - Zone not found
   --      - Time outside valid range
   --      - Strategy is Raise_On_Ambiguous and time is ambiguous
   --
   --  Example:
   --    --  2024-11-03 01:30 in NYC is ambiguous (occurs twice)
   --    Strategy := ZoneInfo.Prefer_Later;  --  Choose standard time
   --    Offset_Result := Get_UTC_Offset_With_Disambiguation(NYC, T, Strategy);
   function Get_UTC_Offset_With_Disambiguation
     (Zone     : ZoneInfo.Zone_Id;
      T        : ZoneInfo.Time;
      Strategy : ZoneInfo.Disambiguation_Strategy := ZoneInfo.Prefer_Earlier)
      return ZoneInfo.UTC_Offset_Result;

   --  Get detailed ambiguity information for a specific time
   --
   --  Determines whether a local time is normal, ambiguous (occurs twice),
   --  or in a gap (never exists), and provides all possible UTC offsets.
   --
   --  Parameters:
   --    Zone: Timezone identifier
   --    T: Time to query
   --
   --  Returns:
   --    Result containing Ambiguity_Info with:
   --      - Is_Ambiguous: True if time occurs twice (DST fall-back)
   --      - Is_Gap: True if time never exists (DST spring-forward)
   --      - Earlier_Offset: First occurrence offset (or only offset if normal)
   --      - Later_Offset: Second occurrence offset (valid if Is_Ambiguous)
   --      - Post_Gap_Offset: Offset after gap (valid if Is_Gap)
   --
   --  Example:
   --    Info_Result := Get_Ambiguity_Info(NYC, T);
   --    if Is_Ok(Info_Result) then
   --       Info := Value(Info_Result);
   --       if Info.Is_Ambiguous then
   --          Put_Line("Ambiguous! Could be " &
   --                   To_String(Info.Earlier_Offset) & " or " &
   --                   To_String(Info.Later_Offset));
   --       elsif Info.Is_Gap then
   --          Put_Line("Gap! Time doesn't exist");
   --       else
   --          Put_Line("Normal time, offset: " &
   --                   To_String(Info.Earlier_Offset));
   --       end if;
   --    end if;
   function Get_Ambiguity_Info
     (Zone : ZoneInfo.Zone_Id;
      T    : ZoneInfo.Time)
      return ZoneInfo.Ambiguity_Info_Result;

   --  Convert time from one timezone to another
   --
   --  Parameters:
   --    T: Time in source timezone
   --    From_Zone: Source timezone
   --    To_Zone: Target timezone
   --
   --  Returns:
   --    Result containing converted time, or Error
   --
   --  Example:
   --    NYC := Create_Zone_Id("America/New_York").Value;
   --    London := Create_Zone_Id("Europe/London").Value;
   --    Result := Convert_Time(Clock, NYC, London);
   function Convert_Time
     (T         : ZoneInfo.Time;
      From_Zone : ZoneInfo.Zone_Id;
      To_Zone   : ZoneInfo.Zone_Id)
      return ZoneInfo.Time_Result;

   --  Check if a local time is ambiguous due to DST fall-back
   --
   --  When clocks fall back (e.g., 02:00 → 01:00), local times
   --  between 01:00-02:00 occur twice (once in each offset).
   --
   --  Returns:
   --    True if the time occurs twice in the given timezone
   function Is_Ambiguous_Time
     (Zone : ZoneInfo.Zone_Id;
      T    : ZoneInfo.Time)
      return Boolean;

   --  Check if a local time is in a DST gap (spring forward)
   --
   --  When clocks spring forward (e.g., 02:00 → 03:00), times
   --  between 02:00-03:00 never exist.
   --
   --  Returns:
   --    True if the time never exists in the given timezone
   function Is_Gap_Time
     (Zone : ZoneInfo.Zone_Id;
      T    : ZoneInfo.Time)
      return Boolean;

   --  ========================================================================
   --  Timezone Discovery
   --  ========================================================================

   --  Find the local system's timezone identifier
   --
   --  Detects the timezone from system configuration:
   --    - Linux: /etc/localtime symlink
   --    - macOS: System preferences
   --
   --  Returns:
   --    Result containing Zone_Id, or Error if detection fails
   --
   --  Example:
   --    Local_Result := Find_Local_Timezone;
   --    if Is_Ok(Local_Result) then
   --       Local_Zone := Value(Local_Result);
   --       -- Local_Zone might be "America/New_York"
   --    end if;
   function Find_Local_Timezone return ZoneInfo.Zone_Id_Result;

   --  ========================================================================
   --  Timezone Information
   --  ========================================================================

   --  Get timezone abbreviation at a specific time
   --
   --  Returns the standard abbreviation (e.g., "EST", "PDT", "GMT")
   --  that applies at the given time in the given timezone.
   --
   --  Parameters:
   --    Zone: Timezone identifier
   --    T: Time to query
   --
   --  Returns:
   --    Abbreviation string (3-5 characters typically)
   --
   --  Example:
   --    Abbr := Get_Timezone_Abbreviation(NYC_Zone, Clock);
   --    -- Abbr might be "EST" in winter or "EDT" in summer
   function Get_Timezone_Abbreviation
     (Zone : ZoneInfo.Zone_Id;
      T    : ZoneInfo.Time)
      return String;

   --  Check if Daylight Saving Time is active at a specific time
   --
   --  Parameters:
   --    Zone: Timezone identifier
   --    T: Time to query
   --
   --  Returns:
   --    True if DST is active, False otherwise
   --
   --  Example:
   --    if Is_DST_Active(NYC_Zone, Clock) then
   --       Put_Line("Currently observing Daylight Saving Time");
   --    end if;
   function Is_DST_Active
     (Zone : ZoneInfo.Zone_Id;
      T    : ZoneInfo.Time)
      return Boolean;

   --  ========================================================================
   --  Timezone Search and Discovery
   --  ========================================================================

   --  Get total count of available timezones
   --
   --  Returns:
   --    Number of timezones in the IANA database
   --
   --  Example:
   --    Count := Get_Timezone_Count;
   --    -- Might return 600+ zones
   function Get_Timezone_Count return Natural;

   --  List all available timezone identifiers
   --
   --  Returns an array of all timezone IDs in the IANA database,
   --  sorted alphabetically (e.g., "Africa/Cairo", "America/New_York").
   --
   --  Parameters:
   --    Descending: If True, sort Z-A instead of A-Z (default False)
   --
   --  Returns:
   --    Array of Zone_Id values
   --
   --  Example:
   --    Zones := List_All_Timezones;
   --    for Z of Zones loop
   --       Put_Line(To_String(Z));
   --    end loop;
   --
   --  Note: May return 600+ zones, consider using search functions
   --        for more targeted queries.
   type Zone_Array is array (Positive range <>) of ZoneInfo.Zone_Id;

   function List_All_Timezones
     (Descending : Boolean := False)
      return Zone_Array;

   --  Search for timezones matching a pattern
   --
   --  Finds all timezone IDs containing the search pattern as a substring.
   --
   --  Parameters:
   --    Pattern: Substring to search for (case-sensitive)
   --
   --  Returns:
   --    Array of matching Zone_Id values
   --
   --  Examples:
   --    NYC_Zones := Find_Timezones_By_Pattern("New_York");
   --    -- Returns: America/New_York
   --
   --    America_Zones := Find_Timezones_By_Pattern("America/");
   --    -- Returns: America/New_York, America/Chicago, America/Los_Angeles, ...
   --
   --    Pacific_Zones := Find_Timezones_By_Pattern("Pacific/");
   --    -- Returns: Pacific/Auckland, Pacific/Fiji, ...
   function Find_Timezones_By_Pattern
     (Pattern : String)
      return Zone_Array;

   --  Search for timezones in a specific geographic region
   --
   --  Finds all timezone IDs starting with the region prefix.
   --  Common regions: America, Europe, Asia, Africa, Pacific, Atlantic, Indian, Arctic, Antarctic
   --
   --  Parameters:
   --    Region: Geographic region (e.g., "America", "Europe", "Asia")
   --
   --  Returns:
   --    Array of matching Zone_Id values
   --
   --  Examples:
   --    European_Zones := Find_Timezones_By_Region("Europe");
   --    -- Returns: Europe/London, Europe/Paris, Europe/Berlin, ...
   --
   --    Asian_Zones := Find_Timezones_By_Region("Asia");
   --    -- Returns: Asia/Tokyo, Asia/Shanghai, Asia/Dubai, ...
   function Find_Timezones_By_Region
     (Region : String)
      return Zone_Array;

   --  ========================================================================
   --  Initialization
   --  ========================================================================

   --  Initialize the timezone database from system location
   --
   --  Discovers and loads timezone data from platform-specific paths:
   --    - Linux/macOS: /usr/share/zoneinfo
   --    - Windows: Registry or bundled data
   --
   --  This must be called before using timezone operations.
   --  Safe to call multiple times (idempotent).
   procedure Initialize;

   --  Check if timezone database is initialized
   function Is_Initialized return Boolean;

end ZoneInfo.API;
