pragma Ada_2022;
--  =========================================================================
--  Workflow: Epoch/Instant Conversions with Duration Math
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates working with Instant (epoch-based timestamps):
--    1. Parse an ISO 8601 datetime string
--    2. Add a Duration (parsed from ISO duration string)
--    3. Convert to different timezone and format output
--    4. Calculate Duration between two times
--    5. Show round-trip conversions
--
--  Instant represents absolute time (like Unix timestamp).
--  Civil represents human-readable wall-clock time.
--  =========================================================================

with Ada.Text_IO;
with Zoneinfo.API.Desktop;
with Zoneinfo.API.Parse;
with Zoneinfo.API.Format;
with Zoneinfo.API.Operations;

procedure Epoch_And_Duration is
   package TIO renames Ada.Text_IO;
   package Desktop renames Zoneinfo.API.Desktop;
   package Parse renames Zoneinfo.API.Parse;
   package Fmt renames Zoneinfo.API.Format;
   package Ops renames Zoneinfo.API.Operations;
begin
   TIO.Put_Line ("==============================================");
   TIO.Put_Line ("Workflow: Epoch/Instant with Duration Math");
   TIO.Put_Line ("==============================================");
   TIO.New_Line;

   --  ===================================================================
   --  Step 1: Parse an ISO 8601 datetime string
   --  ===================================================================
   TIO.Put_Line ("Step 1: Parsing ISO 8601 datetime...");

   declare
      Parse_Result : constant Parse.Civil_Result.Result :=
        Parse.From_ISO_8601 ("2025-12-04T14:30:00");
   begin
      if Parse.Civil_Result.Is_Ok (Parse_Result) then
         declare
            Start_Civil : constant Desktop.Civil :=
              Parse.Civil_Result.Value (Parse_Result);
         begin
            TIO.Put_Line
              ("  Parsed: " & Fmt.To_String (Fmt.To_ISO_8601 (Start_Civil)));

            --  Convert Civil to Instant via UTC
            declare
               Start_Instant_R : constant Desktop.Instant_Result.Result :=
                 Desktop.To_Instant (Start_Civil, Desktop.UTC);
            begin
               if Desktop.Instant_Result.Is_Ok (Start_Instant_R) then
                  declare
                     Start_Instant : constant Desktop.Instant :=
                       Desktop.Instant_Result.Value (Start_Instant_R);
                  begin
                     TIO.Put_Line ("  As epoch: " &
                       Fmt.To_String (Fmt.To_Epoch_String (Start_Instant)));

                     --  ====================================================
                     --  Step 2: Parse and add a Duration
                     --  ====================================================
                     TIO.New_Line;
                     TIO.Put_Line ("Step 2: Adding duration PT3H30M...");

                     declare
                        Dur_Parse : constant Parse.Duration_Result.Result :=
                          Parse.From_ISO_Duration ("PT3H30M");
                     begin
                        if Parse.Duration_Result.Is_Ok (Dur_Parse) then
                           declare
                              Add_Dur : constant Desktop.Duration_Type :=
                                Parse.Duration_Result.Value (Dur_Parse);
                              Later_R : constant Ops.Instant_Result.Result :=
                                Ops."+" (Start_Instant, Add_Dur);
                           begin
                              TIO.Put_Line ("  Duration: " &
                                Fmt.To_String (Fmt.To_Human_Duration (Add_Dur))
                              );

                              if Ops.Instant_Result.Is_Ok (Later_R) then
                                 declare
                                    Later_Instant : constant Desktop.Instant :=
                                      Ops.Instant_Result.Value (Later_R);
                                    Later_Civil : constant Desktop.Civil :=
                                      Desktop.To_Civil
                                        (Later_Instant, Desktop.UTC);
                                 begin
                                    TIO.Put_Line ("  Result: " &
                                      Fmt.To_String
                                        (Fmt.To_ISO_8601 (Later_Civil)));

                                    --  =================================
                                    --  Step 3: Convert to New York
                                    --  =================================
                                    TIO.New_Line;
                                    TIO.Put_Line
                                      ("Step 3: Converting to New York...");

                                    declare
                                       NY_R : constant
                                         Desktop.Zone_ID_Result.Result :=
                                           Desktop.Zone_From_String
                                             ("America/New_York");
                                    begin
                                       if Desktop.Zone_ID_Result.Is_Ok (NY_R)
                                       then
                                          declare
                                             NY_Zone : constant
                                               Desktop.Zone_ID :=
                                                 Desktop.Zone_ID_Result.Value
                                                   (NY_R);
                                             NY_Civil : constant Desktop.Civil
                                               := Desktop.To_Civil
                                                    (Later_Instant, NY_Zone);
                                             NY_Off : constant
                                               Desktop.Duration_Type :=
                                                 Desktop.Get_Offset
                                                   (Later_Instant, NY_Zone);
                                          begin
                                             TIO.Put_Line
                                               ("  New York time: " &
                                                Fmt.To_String
                                                  (Fmt.To_ISO_8601_Full
                                                    (NY_Civil, NY_Off,
                                                     NY_Zone)));

                                             --  ==========================
                                             --  Step 4: Calc difference
                                             --  ==========================
                                             TIO.New_Line;
                                             TIO.Put_Line
                                               ("Step 4: Duration between...");

                                             declare
                                                Diff : constant
                                                  Desktop.Duration_Type :=
                                                    Ops."-"
                                                      (Later_Instant,
                                                       Start_Instant);
                                             begin
                                                TIO.Put_Line
                                                  ("  Difference: " &
                                                   Fmt.To_String
                                                     (Fmt.To_Human_Duration
                                                        (Diff)));
                                                TIO.Put_Line
                                                  ("  ISO format: " &
                                                   Fmt.To_String
                                                     (Fmt.To_ISO_Duration
                                                        (Diff)));
                                             end;

                                             --  ==========================
                                             --  Step 5: Round-trip
                                             --  ==========================
                                             TIO.New_Line;
                                             TIO.Put_Line
                                               ("Step 5: Round-trip check...");

                                             declare
                                                Back_R : constant
                                                  Desktop.Instant_Result.Result
                                                    := Desktop.To_Instant
                                                         (NY_Civil, NY_Zone);
                                             begin
                                                if Desktop.Instant_Result.Is_Ok
                                                     (Back_R)
                                                then
                                                   declare
                                                      RT : constant
                                                        Desktop.Instant :=
                                                        Desktop.Instant_Result
                                                          .Value (Back_R);
                                                   begin
                                                      TIO.Put_Line
                                                        ("  Original epoch: " &
                                                         Fmt.To_String
                                                          (Fmt.To_Epoch_String
                                                             (Later_Instant)));
                                                      TIO.Put_Line
                                                        ("  Round-trip:     " &
                                                         Fmt.To_String
                                                          (Fmt.To_Epoch_String
                                                             (RT)));
                                                   end;
                                                end if;
                                             end;
                                          end;
                                       end if;
                                    end;
                                 end;
                              end if;
                           end;
                        end if;
                     end;
                  end;
               end if;
            end;
         end;
      else
         TIO.Put_Line ("  Parse error!");
      end if;
   end;

   TIO.New_Line;
   TIO.Put_Line ("==============================================");
   TIO.Put_Line ("Workflow complete!");
   TIO.Put_Line ("==============================================");

end Epoch_And_Duration;
