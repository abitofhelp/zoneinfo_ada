pragma Ada_2022;
--  ===========================================================================
--   Timezone Search and Discovery
--  ===========================================================================
--  Demonstrates:
--    - Get_Timezone_Count: Count all available timezones
--    - Find_Timezones_By_Pattern: Search by substring
--    - Find_Timezones_By_Region: Search by geographic region
--    - List_All_Timezones: Enumerate all timezones
--  ===========================================================================

with Ada.Text_IO;
with ZoneInfo;
with ZoneInfo.API;
with Domain.Value_Object.Zone_Id;

procedure Search_Timezones is

   use Ada.Text_IO;
   use ZoneInfo;

   procedure Section_Header (Title : String) is
   begin
      Put_Line ("");
      Put_Line ("========================================");
      Put_Line ("  " & Title);
      Put_Line ("========================================");
   end Section_Header;

begin
   Put_Line ("========================================================");
   Put_Line ("  ZoneInfo.API - Timezone Search and Discovery");
   Put_Line ("========================================================");
   Put_Line ("");
   Put_Line ("Demonstrating timezone search capabilities...");

   --  Initialize the timezone database
   ZoneInfo.API.Initialize;

   --  ========================================================================
   --  Test 1: Get Total Count
   --  ========================================================================
   Section_Header ("Total Timezone Count");
   declare
      Count : constant Natural := ZoneInfo.API.Get_Timezone_Count;
   begin
      Put_Line ("Total available timezones:" & Count'Image);
      Put_Line ("(IANA database contains 600+ timezones)");
   end;

   --  ========================================================================
   --  Test 2: Search by Pattern - Find New York
   --  ========================================================================
   Section_Header ("Search Pattern: 'New_York'");
   declare
      Results : constant ZoneInfo.API.Zone_Array :=
        ZoneInfo.API.Find_Timezones_By_Pattern ("New_York");
   begin
      Put_Line ("Found" & Results'Length'Image & " matching timezone(s):");
      for Z of Results loop
         Put_Line ("  - " & Domain.Value_Object.Zone_Id.To_String (Z));
      end loop;
   end;

   --  ========================================================================
   --  Test 3: Search by Pattern - Pacific Timezones
   --  ========================================================================
   Section_Header ("Search Pattern: 'Pacific/'");
   declare
      Results : constant ZoneInfo.API.Zone_Array :=
        ZoneInfo.API.Find_Timezones_By_Pattern ("Pacific/");
      Max_Display : constant := 10;
   begin
      Put_Line ("Found" & Results'Length'Image & " matching timezone(s)");
      Put_Line ("Showing first" & Integer'Min (Max_Display, Results'Length)'Image & ":");
      for I in Results'First .. Integer'Min (Results'First + Max_Display - 1, Results'Last) loop
         Put_Line ("  - " & Domain.Value_Object.Zone_Id.To_String (Results (I)));
      end loop;
      if Results'Length > Max_Display then
         Put_Line ("  ... and" & Integer'Image (Results'Length - Max_Display) & " more");
      end if;
   end;

   --  ========================================================================
   --  Test 4: Search by Region - Europe
   --  ========================================================================
   Section_Header ("Search Region: 'Europe'");
   declare
      Results : constant ZoneInfo.API.Zone_Array :=
        ZoneInfo.API.Find_Timezones_By_Region ("Europe");
      Max_Display : constant := 10;
   begin
      Put_Line ("Found" & Results'Length'Image & " European timezone(s)");
      Put_Line ("Showing first" & Integer'Min (Max_Display, Results'Length)'Image & ":");
      for I in Results'First .. Integer'Min (Results'First + Max_Display - 1, Results'Last) loop
         Put_Line ("  - " & Domain.Value_Object.Zone_Id.To_String (Results (I)));
      end loop;
      if Results'Length > Max_Display then
         Put_Line ("  ... and" & Integer'Image (Results'Length - Max_Display) & " more");
      end if;
   end;

   --  ========================================================================
   --  Test 5: Search by Region - America
   --  ========================================================================
   Section_Header ("Search Region: 'America'");
   declare
      Results : constant ZoneInfo.API.Zone_Array :=
        ZoneInfo.API.Find_Timezones_By_Region ("America");
      Max_Display : constant := 10;
   begin
      Put_Line ("Found" & Results'Length'Image & " American timezone(s)");
      Put_Line ("Showing first" & Integer'Min (Max_Display, Results'Length)'Image & ":");
      for I in Results'First .. Integer'Min (Results'First + Max_Display - 1, Results'Last) loop
         Put_Line ("  - " & Domain.Value_Object.Zone_Id.To_String (Results (I)));
      end loop;
      if Results'Length > Max_Display then
         Put_Line ("  ... and" & Integer'Image (Results'Length - Max_Display) & " more");
      end if;
   end;

   --  ========================================================================
   --  Test 6: Search by Region - Asia
   --  ========================================================================
   Section_Header ("Search Region: 'Asia'");
   declare
      Results : constant ZoneInfo.API.Zone_Array :=
        ZoneInfo.API.Find_Timezones_By_Region ("Asia");
      Max_Display : constant := 10;
   begin
      Put_Line ("Found" & Results'Length'Image & " Asian timezone(s)");
      Put_Line ("Showing first" & Integer'Min (Max_Display, Results'Length)'Image & ":");
      for I in Results'First .. Integer'Min (Results'First + Max_Display - 1, Results'Last) loop
         Put_Line ("  - " & Domain.Value_Object.Zone_Id.To_String (Results (I)));
      end loop;
      if Results'Length > Max_Display then
         Put_Line ("  ... and" & Integer'Image (Results'Length - Max_Display) & " more");
      end if;
   end;

   Put_Line ("");
   Put_Line ("========================================================");
   Put_Line ("[SUCCESS] All search operations completed!");
   Put_Line ("========================================================");

end Search_Timezones;
