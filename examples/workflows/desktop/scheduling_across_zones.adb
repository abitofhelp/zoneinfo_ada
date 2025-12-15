pragma Ada_2022;
--  =========================================================================
--  Workflow: Scheduling Meetings Across Timezones
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Real-world scenario: Schedule a meeting and show times across zones:
--    1. Meeting starts at 2pm New York
--    2. Meeting duration is 90 minutes
--    3. Show start/end times in multiple zones (Tokyo, London, Sydney)
--    4. Calculate what time participants in each zone need to join
--
--  This is a common business use case for timezone libraries.
--  =========================================================================

with Ada.Text_IO;
with Zoneinfo.API.Desktop;
with Zoneinfo.API.Parse;
with Zoneinfo.API.Format;
with Zoneinfo.API.Operations;

procedure Scheduling_Across_Zones is
   package TIO renames Ada.Text_IO;
   package Desktop renames Zoneinfo.API.Desktop;
   package Parse renames Zoneinfo.API.Parse;
   package Fmt renames Zoneinfo.API.Format;
   package Ops renames Zoneinfo.API.Operations;

   --  Show meeting time in a specific zone
   procedure Show_Zone_Time
     (Label         : String;
      Meeting_Start : Desktop.Instant;
      Meeting_End   : Desktop.Instant;
      Zone_Name     : String)
   is
      Zone_R : constant Desktop.Zone_ID_Result.Result :=
        Desktop.Zone_From_String (Zone_Name);
   begin
      if Desktop.Zone_ID_Result.Is_Ok (Zone_R) then
         declare
            Zone : constant Desktop.Zone_ID :=
              Desktop.Zone_ID_Result.Value (Zone_R);
            Start_Civil : constant Desktop.Civil :=
              Desktop.To_Civil (Meeting_Start, Zone);
            End_Civil : constant Desktop.Civil :=
              Desktop.To_Civil (Meeting_End, Zone);
            Offset : constant Desktop.Duration_Type :=
              Desktop.Get_Offset (Meeting_Start, Zone);
         begin
            TIO.Put_Line ("  " & Label & ":");
            TIO.Put_Line ("    Start: " &
              Fmt.To_String (Fmt.To_ISO_8601_With_Offset (Start_Civil, Offset))
            );
            TIO.Put_Line ("    End:   " &
              Fmt.To_String (Fmt.To_ISO_8601_With_Offset (End_Civil, Offset)));
            TIO.Put_Line ("    Zone:  " & Zone_Name);
         end;
      else
         TIO.Put_Line ("  " & Label & ": Error - invalid zone " & Zone_Name);
      end if;
   end Show_Zone_Time;

begin
   TIO.Put_Line ("==============================================");
   TIO.Put_Line ("Workflow: Scheduling Meetings Across Timezones");
   TIO.Put_Line ("==============================================");
   TIO.New_Line;

   --  ===================================================================
   --  Step 1: Define meeting in New York time
   --  ===================================================================
   TIO.Put_Line ("Step 1: Defining meeting...");
   TIO.Put_Line ("  Meeting: Project Review");
   TIO.Put_Line ("  Scheduled: December 15, 2025 at 2:00 PM New York");
   TIO.Put_Line ("  Duration: 90 minutes");
   TIO.New_Line;

   declare
      --  Parse meeting start time
      Civil_Parse : constant Parse.Civil_Result.Result :=
        Parse.From_ISO_8601 ("2025-12-15T14:00:00");

      --  Parse meeting duration (90 minutes)
      Dur_Parse : constant Parse.Duration_Result.Result :=
        Parse.From_ISO_Duration ("PT1H30M");

      --  Get New York zone
      NY_Zone_R : constant Desktop.Zone_ID_Result.Result :=
        Desktop.Zone_From_String ("America/New_York");
   begin
      if Parse.Civil_Result.Is_Ok (Civil_Parse) and then
         Parse.Duration_Result.Is_Ok (Dur_Parse) and then
         Desktop.Zone_ID_Result.Is_Ok (NY_Zone_R)
      then
         declare
            Meeting_Civil : constant Desktop.Civil :=
              Parse.Civil_Result.Value (Civil_Parse);
            Meeting_Duration : constant Desktop.Duration_Type :=
              Parse.Duration_Result.Value (Dur_Parse);
            NY_Zone : constant Desktop.Zone_ID :=
              Desktop.Zone_ID_Result.Value (NY_Zone_R);

            --  Convert to Zoned in New York
            Zoned_R : constant Desktop.Zoned_Result.Result :=
              Desktop.To_Zoned (Meeting_Civil, NY_Zone);
         begin
            if Desktop.Zoned_Result.Is_Ok (Zoned_R) then
               declare
                  Meeting_Zoned : constant Desktop.Zoned :=
                    Desktop.Zoned_Result.Value (Zoned_R);
                  Meeting_Start : constant Desktop.Instant :=
                    Desktop.To_Instant (Meeting_Zoned);

                  --  Calculate meeting end time
                  End_R : constant Ops.Instant_Result.Result :=
                    Ops."+" (Meeting_Start, Meeting_Duration);
               begin
                  if Ops.Instant_Result.Is_Ok (End_R) then
                     declare
                        Meeting_End : constant Desktop.Instant :=
                          Ops.Instant_Result.Value (End_R);
                        NY_Offset : constant Desktop.Duration_Type :=
                          Desktop.Get_Offset (Meeting_Zoned);
                     begin
                        --  ================================================
                        --  Step 2: Show meeting in organizer's timezone
                        --  ================================================
                        TIO.Put_Line
                          ("Step 2: Organizer's view (New York)...");
                        TIO.Put_Line ("  Start: " &
                          Fmt.To_String (Fmt.To_ISO_8601_Full
                            (Desktop.To_Civil (Meeting_Zoned),
                             NY_Offset, NY_Zone)));
                        TIO.Put_Line ("  End:   " &
                          Fmt.To_String (Fmt.To_ISO_8601_With_Offset
                            (Desktop.To_Civil (Meeting_End, NY_Zone),
                             NY_Offset)));
                        TIO.Put_Line ("  Duration: " &
                          Fmt.To_String
                            (Fmt.To_Human_Duration (Meeting_Duration)));
                        TIO.New_Line;

                        --  ================================================
                        --  Step 3: Show times for other zones
                        --  ================================================
                        TIO.Put_Line ("Step 3: Participant times...");
                        TIO.New_Line;

                        Show_Zone_Time ("Tokyo Office",
                          Meeting_Start, Meeting_End, "Asia/Tokyo");
                        TIO.New_Line;

                        Show_Zone_Time ("London Office",
                          Meeting_Start, Meeting_End, "Europe/London");
                        TIO.New_Line;

                        Show_Zone_Time ("Sydney Office",
                          Meeting_Start, Meeting_End, "Australia/Sydney");
                        TIO.New_Line;

                        Show_Zone_Time ("UTC Reference",
                          Meeting_Start, Meeting_End, "UTC");
                        TIO.New_Line;

                        --  ================================================
                        --  Step 4: Calculate time differences
                        --  ================================================
                        TIO.Put_Line ("Step 4: Timezone offsets from NY...");

                        declare
                           Tokyo_R : constant Desktop.Zone_ID_Result.Result :=
                             Desktop.Zone_From_String ("Asia/Tokyo");
                           London_R : constant Desktop.Zone_ID_Result.Result :=
                             Desktop.Zone_From_String ("Europe/London");
                        begin
                           if Desktop.Zone_ID_Result.Is_Ok (Tokyo_R) then
                              declare
                                 Tokyo_Off : constant Desktop.Duration_Type :=
                                   Desktop.Get_Offset (Meeting_Start,
                                     Desktop.Zone_ID_Result.Value (Tokyo_R));
                                 Diff : constant Desktop.Duration_Type :=
                                   Ops."-" (Tokyo_Off, NY_Offset);
                              begin
                                 TIO.Put_Line ("  Tokyo is " &
                                   Fmt.To_String
                                     (Fmt.To_Human_Duration (Diff)) &
                                   " ahead of New York");
                              end;
                           end if;

                           if Desktop.Zone_ID_Result.Is_Ok (London_R) then
                              declare
                                 London_Off : constant Desktop.Duration_Type :=
                                   Desktop.Get_Offset (Meeting_Start,
                                     Desktop.Zone_ID_Result.Value (London_R));
                                 Diff : constant Desktop.Duration_Type :=
                                   Ops."-" (London_Off, NY_Offset);
                              begin
                                 TIO.Put_Line ("  London is " &
                                   Fmt.To_String
                                     (Fmt.To_Human_Duration (Diff)) &
                                   " ahead of New York");
                              end;
                           end if;
                        end;
                     end;
                  end if;
               end;
            else
               TIO.Put_Line ("  Error: Could not create meeting time");
            end if;
         end;
      else
         TIO.Put_Line ("  Error: Could not parse meeting details");
      end if;
   end;

   TIO.New_Line;
   TIO.Put_Line ("==============================================");
   TIO.Put_Line ("Workflow complete!");
   TIO.Put_Line ("==============================================");

end Scheduling_Across_Zones;
