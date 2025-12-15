pragma Ada_2022;
--  =========================================================================
--  Workflow: Zoned Datetime Arithmetic
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates arithmetic operations on Zoned datetime values:
--    1. Create a Zoned datetime
--    2. Add duration using "+" operator
--    3. Subtract duration using "-" operator
--    4. Calculate difference between two Zoned values
--    5. Show how DST transitions are handled
--
--  Key Types:
--    Instant - Absolute time (like Unix timestamp)
--    Civil - Human-readable wall-clock time (no timezone)
--    Zoned - Datetime in a particular timezone
--
--  Uses operator overloading for intuitive datetime math.
--  =========================================================================

with Ada.Text_IO;
with Zoneinfo.API.Desktop;
with Zoneinfo.API.Parse;
with Zoneinfo.API.Format;
with Zoneinfo.API.Operations;

procedure Zoned_Arithmetic is
   package TIO renames Ada.Text_IO;
   package Desktop renames Zoneinfo.API.Desktop;
   package Parse renames Zoneinfo.API.Parse;
   package Fmt renames Zoneinfo.API.Format;
   package Ops renames Zoneinfo.API.Operations;

   NY_Zone : Desktop.Zone_ID;
   Start_Zoned : Desktop.Zoned;
   Start_Offset : Desktop.Duration_Type;
   Start_Instant : Desktop.Instant;
   Two_Hours : Desktop.Duration_Type;
   M30 : Desktop.Duration_Type;  --  30 minutes
begin
   TIO.Put_Line ("==============================================");
   TIO.Put_Line ("Workflow: Zoned Datetime Arithmetic");
   TIO.Put_Line ("==============================================");
   TIO.New_Line;

   --  ===================================================================
   --  Step 1: Create a Zoned datetime from parsed string
   --  ===================================================================
   TIO.Put_Line ("Step 1: Creating Zoned datetime...");

   declare
      NY_R : constant Desktop.Zone_ID_Result.Result :=
        Desktop.Zone_From_String ("America/New_York");
      Parse_R : constant Parse.Civil_Result.Result :=
        Parse.From_ISO_8601 ("2025-03-09T01:30:00");  --  Before DST
   begin
      if not Desktop.Zone_ID_Result.Is_Ok (NY_R)
        or else not Parse.Civil_Result.Is_Ok (Parse_R)
      then
         TIO.Put_Line ("  Error: Could not parse zone or time");
         return;
      end if;

      NY_Zone := Desktop.Zone_ID_Result.Value (NY_R);

      declare
         Start_Civil : constant Desktop.Civil :=
           Parse.Civil_Result.Value (Parse_R);
         Zoned_R : constant Desktop.Zoned_Result.Result :=
           Desktop.To_Zoned (Start_Civil, NY_Zone);
      begin
         if not Desktop.Zoned_Result.Is_Ok (Zoned_R) then
            TIO.Put_Line ("  Error: Could not create Zoned (DST gap?)");
            return;
         end if;

         Start_Zoned := Desktop.Zoned_Result.Value (Zoned_R);
         Start_Offset := Desktop.Get_Offset (Start_Zoned);
         Start_Instant := Desktop.To_Instant (Start_Zoned);

         TIO.Put_Line ("  Start time: " &
           Fmt.To_String (Fmt.To_ISO_8601_Full
             (Desktop.To_Civil (Start_Zoned), Start_Offset, NY_Zone)));
      end;
   end;

   --  ===================================================================
   --  Step 2: Add 2 hours using "+" operator
   --  ===================================================================
   TIO.New_Line;
   TIO.Put_Line ("Step 2: Adding 2 hours...");

   declare
      Two_Hours_R : constant Parse.Duration_Result.Result :=
        Parse.From_ISO_Duration ("PT2H");
   begin
      if not Parse.Duration_Result.Is_Ok (Two_Hours_R) then
         TIO.Put_Line ("  Error: Could not parse duration");
         return;
      end if;

      Two_Hours := Parse.Duration_Result.Value (Two_Hours_R);

      declare
         Later_R : constant Ops.Instant_Result.Result :=
           Ops."+" (Start_Instant, Two_Hours);
      begin
         if Ops.Instant_Result.Is_Ok (Later_R) then
            declare
               Later_Instant : constant Desktop.Instant :=
                 Ops.Instant_Result.Value (Later_R);
               Later_Civil : constant Desktop.Civil :=
                 Desktop.To_Civil (Later_Instant, NY_Zone);
               Later_Off : constant Desktop.Duration_Type :=
                 Desktop.Get_Offset (Later_Instant, NY_Zone);
            begin
               TIO.Put_Line ("  +2 hours: " &
                 Fmt.To_String (Fmt.To_ISO_8601_Full
                   (Later_Civil, Later_Off, NY_Zone)));

               --  Check for DST change
               if Fmt.To_String (Fmt.Format_Offset (Start_Offset)) /=
                  Fmt.To_String (Fmt.Format_Offset (Later_Off))
               then
                  TIO.Put_Line ("  ** DST transition! **");
                  TIO.Put_Line ("     Offset: " &
                    Fmt.To_String (Fmt.Format_Offset (Start_Offset)) &
                    " -> " &
                    Fmt.To_String (Fmt.Format_Offset (Later_Off)));
               end if;

               --  ========================================================
               --  Step 3: Subtract 30 minutes
               --  ========================================================
               TIO.New_Line;
               TIO.Put_Line ("Step 3: Subtracting 30 min...");

               declare
                  M30_R : constant Parse.Duration_Result.Result :=
                    Parse.From_ISO_Duration ("PT30M");
               begin
                  if Parse.Duration_Result.Is_Ok (M30_R) then
                     M30 := Parse.Duration_Result.Value (M30_R);

                     declare
                        Earlier_R : constant Ops.Instant_Result.Result :=
                          Ops."-" (Later_Instant, M30);
                     begin
                        if Ops.Instant_Result.Is_Ok (Earlier_R) then
                           declare
                              Earlier : constant Desktop.Instant :=
                                Ops.Instant_Result.Value (Earlier_R);
                              Earlier_C : constant Desktop.Civil :=
                                Desktop.To_Civil (Earlier, NY_Zone);
                              Earlier_O : constant Desktop.Duration_Type :=
                                Desktop.Get_Offset (Earlier, NY_Zone);
                           begin
                              TIO.Put_Line ("  -30 min: " &
                                Fmt.To_String (Fmt.To_ISO_8601_Full
                                  (Earlier_C, Earlier_O, NY_Zone)));

                              --  ========================================
                              --  Step 4: Duration between times
                              --  ========================================
                              TIO.New_Line;
                              TIO.Put_Line ("Step 4: Duration between...");

                              declare
                                 Diff : constant Desktop.Duration_Type :=
                                   Ops."-" (Earlier, Start_Instant);
                              begin
                                 TIO.Put_Line ("  Elapsed: " &
                                   Fmt.To_String
                                     (Fmt.To_Human_Duration (Diff)));
                                 TIO.Put_Line ("  ISO: " &
                                   Fmt.To_String
                                     (Fmt.To_ISO_Duration (Diff)));
                              end;

                              --  ========================================
                              --  Step 5: Duration arithmetic
                              --  ========================================
                              TIO.New_Line;
                              TIO.Put_Line ("Step 5: Duration arithmetic...");

                              declare
                                 Comb : constant Desktop.Duration_Type :=
                                   Ops."+" (Two_Hours, M30);
                                 Subt : constant Desktop.Duration_Type :=
                                   Ops."-" (Two_Hours, M30);
                                 Neg : constant Desktop.Duration_Type :=
                                   Ops."-" (M30);
                              begin
                                 TIO.Put_Line ("  2h + 30m = " &
                                   Fmt.To_String
                                     (Fmt.To_Human_Duration (Comb)));
                                 TIO.Put_Line ("  2h - 30m = " &
                                   Fmt.To_String
                                     (Fmt.To_Human_Duration (Subt)));
                                 TIO.Put_Line ("  -(30m)   = " &
                                   Fmt.To_String
                                     (Fmt.To_Human_Duration (Neg)));
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

   TIO.New_Line;
   TIO.Put_Line ("==============================================");
   TIO.Put_Line ("Workflow complete!");
   TIO.Put_Line ("==============================================");

end Zoned_Arithmetic;
