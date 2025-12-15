pragma Ada_2022;
--  =========================================================================
--  SPARK Workflow: Civil Time Formatting
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    SPARK-proven workflow demonstrating civil time formatting.
--    Core logic is SPARK_Mode On (formally verifiable), I/O is SPARK_Mode Off.
--
--  Architecture:
--    - Pure computation: SPARK_Mode On (proven)
--    - I/O wrapper: SPARK_Mode Off (trivial, not proven)
--    This is the standard pattern for SPARK projects using community GNAT.
--
--  Key Differences from Desktop Version:
--    - All formatting uses SPARK-enabled API.Format
--    - Pre-constructed Civil values (no timezone conversion)
--    - Bounded strings used directly (stack allocation only)
--    - Core formatting operations formally provable
--
--  SPARK-Enabled APIs Used:
--    - Zoneinfo.API.Format (ISO 8601 formatting)
--    - Domain.Value_Object.Civil (civil time construction)
--    - Domain.Value_Object.Duration_Type (offset representation)
--    - Domain.Value_Object.Zone_ID (zone identifier)
--
--  To Prove (core logic only):
--    gnatprove -P examples.gpr --mode=prove civil_formatting.adb
--  =========================================================================

with Ada.Text_IO;
with Interfaces;
with Zoneinfo.API.Format;
with Domain.Value_Object.Civil;
with Domain.Value_Object.Duration_Type;
with Domain.Value_Object.Zone_ID;

procedure Civil_Formatting is
   use type Interfaces.Integer_64;  --  For arithmetic on Integer_64

   package TIO renames Ada.Text_IO;
   package Fmt renames Zoneinfo.API.Format;
   package Civ renames Domain.Value_Object.Civil;
   package Dur renames Domain.Value_Object.Duration_Type;
   package ZID renames Domain.Value_Object.Zone_ID;

   --  ========================================================================
   --  I/O Helpers (SPARK_Mode Off - not proven, but trivial)
   --  ========================================================================

   procedure Put_Line (S : String)
     with SPARK_Mode => Off
   is
   begin
      TIO.Put_Line (S);
   end Put_Line;

   procedure Put (S : String)
     with SPARK_Mode => Off
   is
   begin
      TIO.Put (S);
   end Put;

   procedure New_Line
     with SPARK_Mode => Off
   is
   begin
      TIO.New_Line;
   end New_Line;

   --  ========================================================================
   --  Pre-constructed Test Values (SPARK-compatible)
   --  ========================================================================

   --  Civil time: 2025-12-04 14:30:45.123456789
   --  Note: Civil.Create returns Result, so we use a function to unwrap
   function Make_Test_Civil return Fmt.Civil is
      Result : constant Civ.Civil_Result.Result := Civ.Create
        (Year       => 2025,
         Month      => 12,
         Day        => 4,
         Hour       => 14,
         Minute     => 30,
         Second     => 45,
         Nanosecond => 123_456_789);
   begin
      --  We know this is valid, so unwrap safely
      return Civ.Civil_Result.Value (Result);
   end Make_Test_Civil;

   Test_Civil : constant Fmt.Civil := Make_Test_Civil;

   --  UTC offset: -05:00 (Eastern Standard Time) = -18000 seconds
   EST_Offset : constant Fmt.Duration_Type := Dur.From_Seconds (-18_000);

   --  UTC offset: +00:00 (UTC)
   UTC_Offset : constant Fmt.Duration_Type := Dur.From_Seconds (0);

   --  UTC offset: +12:45 (Chatham Islands) = 45900 seconds
   Chatham_Offset : constant Fmt.Duration_Type := Dur.From_Seconds (45_900);

   --  Zone ID for New York
   NY_Zone : constant Fmt.Zone_ID := ZID.Make_Zone_ID ("America/New_York");

begin
   Put_Line ("==============================================");
   Put_Line ("SPARK Workflow: Civil Time Formatting");
   Put_Line ("==============================================");
   New_Line;

   --  ========================================================================
   --  Step 1: Basic ISO 8601 formatting (SPARK-proven)
   --  ========================================================================
   Put_Line ("Step 1: Basic ISO 8601 Formats");
   New_Line;

   Put_Line ("  With nanoseconds:");
   Put ("    ");
   Put_Line
     (Fmt.Datetime_Strings.To_String
        (Fmt.To_ISO_8601 (Test_Civil, Include_Nanos => True)));

   Put_Line ("  Without nanoseconds:");
   Put ("    ");
   Put_Line
     (Fmt.Datetime_Strings.To_String
        (Fmt.To_ISO_8601 (Test_Civil, Include_Nanos => False)));
   New_Line;

   --  ========================================================================
   --  Step 2: Date and time only (SPARK-proven)
   --  ========================================================================
   Put_Line ("Step 2: Date-Only and Time-Only");
   New_Line;

   Put ("  Date only: ");
   Put_Line
     (Fmt.Datetime_Strings.To_String (Fmt.To_ISO_Date (Test_Civil)));

   Put ("  Time only: ");
   Put_Line
     (Fmt.Datetime_Strings.To_String
        (Fmt.To_ISO_Time (Test_Civil, Include_Nanos => False)));

   Put ("  Time with nanos: ");
   Put_Line
     (Fmt.Datetime_Strings.To_String
        (Fmt.To_ISO_Time (Test_Civil, Include_Nanos => True)));
   New_Line;

   --  ========================================================================
   --  Step 3: With UTC offset (SPARK-proven)
   --  ========================================================================
   Put_Line ("Step 3: ISO 8601 with UTC Offset");
   New_Line;

   Put ("  EST (-05:00): ");
   Put_Line
     (Fmt.Datetime_Strings.To_String
        (Fmt.To_ISO_8601_With_Offset
           (Test_Civil, EST_Offset, Include_Nanos => False)));

   Put ("  UTC (Z): ");
   Put_Line
     (Fmt.Datetime_Strings.To_String
        (Fmt.To_ISO_8601_With_Offset
           (Test_Civil, UTC_Offset, Include_Nanos => False)));

   Put ("  Chatham (+12:45): ");
   Put_Line
     (Fmt.Datetime_Strings.To_String
        (Fmt.To_ISO_8601_With_Offset
           (Test_Civil, Chatham_Offset, Include_Nanos => False)));
   New_Line;

   --  ========================================================================
   --  Step 4: With zone identifier (SPARK-proven)
   --  ========================================================================
   Put_Line ("Step 4: ISO 8601 with Zone Identifier");
   New_Line;

   Put ("  Zone only: ");
   Put_Line
     (Fmt.Datetime_Strings.To_String
        (Fmt.To_ISO_8601_With_Zone
           (Test_Civil, NY_Zone, Include_Nanos => False)));
   New_Line;

   --  ========================================================================
   --  Step 5: Full format (offset + zone) (SPARK-proven)
   --  ========================================================================
   Put_Line ("Step 5: Full ISO 8601 (Offset + Zone)");
   New_Line;

   Put ("  Full format: ");
   Put_Line
     (Fmt.Datetime_Strings.To_String
        (Fmt.To_ISO_8601_Full
           (Test_Civil, EST_Offset, NY_Zone, Include_Nanos => False)));

   Put ("  With nanos: ");
   Put_Line
     (Fmt.Datetime_Strings.To_String
        (Fmt.To_ISO_8601_Full
           (Test_Civil, EST_Offset, NY_Zone, Include_Nanos => True)));
   New_Line;

   --  ========================================================================
   --  Step 6: Offset formatting (SPARK-proven)
   --  ========================================================================
   Put_Line ("Step 6: Offset Formatting");
   New_Line;

   Put ("  EST offset: ");
   Put_Line
     (Fmt.Datetime_Strings.To_String (Fmt.Format_Offset (EST_Offset)));

   Put ("  UTC offset: ");
   Put_Line
     (Fmt.Datetime_Strings.To_String (Fmt.Format_Offset (UTC_Offset)));

   Put ("  Chatham offset: ");
   Put_Line
     (Fmt.Datetime_Strings.To_String (Fmt.Format_Offset (Chatham_Offset)));

   New_Line;
   Put_Line ("==============================================");
   Put_Line ("SPARK workflow complete!");
   Put_Line ("  Core logic: SPARK_Mode On (provable)");
   Put_Line ("  I/O layer:  SPARK_Mode Off (standard pattern)");
   Put_Line ("==============================================");

end Civil_Formatting;
