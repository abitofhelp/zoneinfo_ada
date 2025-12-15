pragma Ada_2022;
--  =========================================================================
--  SPARK Workflow: Duration Formatting
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    SPARK-proven workflow demonstrating duration formatting.
--    Core logic is SPARK_Mode On (formally verifiable), I/O is SPARK_Mode Off.
--
--  Architecture:
--    - Pure computation: SPARK_Mode On (proven)
--    - I/O wrapper: SPARK_Mode Off (trivial, not proven)
--    This is the standard pattern for SPARK projects using community GNAT.
--
--  Key Differences from Desktop Version:
--    - All formatting uses SPARK-enabled API.Format
--    - Pre-constructed Duration values
--    - Bounded strings used directly (stack allocation only)
--    - Core formatting operations formally provable
--
--  SPARK-Enabled APIs Used:
--    - Zoneinfo.API.Format (duration formatting)
--    - Zoneinfo.API.Operations (duration arithmetic)
--    - Domain.Value_Object.Duration_Type (duration construction)
--
--  To Prove (core logic only):
--    gnatprove -P examples.gpr --mode=prove duration_formatting.adb
--  =========================================================================

with Ada.Text_IO;
with Interfaces;
with Zoneinfo.API.Format;
with Zoneinfo.API.Operations;
with Domain.Value_Object.Duration_Type;

procedure Duration_Formatting is
   use type Interfaces.Integer_64;  --  For arithmetic on Integer_64

   package TIO renames Ada.Text_IO;
   package Fmt renames Zoneinfo.API.Format;
   package Ops renames Zoneinfo.API.Operations;
   package Dur renames Domain.Value_Object.Duration_Type;

   subtype Duration_Type is Ops.Duration_Type;

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
   --  Pre-constructed Test Durations (SPARK-compatible)
   --  ========================================================================

   --  1 hour 30 minutes 45 seconds = 5445 seconds
   D_1h30m45s : constant Duration_Type := Dur.From_Seconds (5_445);

   --  2 days 12 hours = 216000 seconds
   D_2d12h : constant Duration_Type := Dur.From_Seconds (216_000);

   --  5 minutes 30 seconds (negative) = -330 seconds
   D_Neg_5m30s : constant Duration_Type := Dur.From_Seconds (-330);

   --  Zero duration
   D_Zero : constant Duration_Type := Dur.From_Seconds (0);

   --  45.5 seconds (with nanoseconds) = 45500 milliseconds
   D_45_5s : constant Duration_Type := Dur.From_Millis (45_500);

   --  1 week (7 days) = 604800 seconds
   D_1_Week : constant Duration_Type := Dur.From_Seconds (604_800);

begin
   Put_Line ("==============================================");
   Put_Line ("SPARK Workflow: Duration Formatting");
   Put_Line ("==============================================");
   New_Line;

   --  ========================================================================
   --  Step 1: ISO 8601 Duration Format (SPARK-proven)
   --  ========================================================================
   Put_Line ("Step 1: ISO 8601 Duration Format (PnDTnHnMnS)");
   New_Line;

   Put ("  1h 30m 45s: ");
   Put_Line
     (Fmt.Duration_Strings.To_String (Fmt.To_ISO_Duration (D_1h30m45s)));

   Put ("  2d 12h: ");
   Put_Line
     (Fmt.Duration_Strings.To_String (Fmt.To_ISO_Duration (D_2d12h)));

   Put ("  -5m 30s: ");
   Put_Line
     (Fmt.Duration_Strings.To_String (Fmt.To_ISO_Duration (D_Neg_5m30s)));

   Put ("  Zero: ");
   Put_Line
     (Fmt.Duration_Strings.To_String (Fmt.To_ISO_Duration (D_Zero)));

   Put ("  45.5s: ");
   Put_Line
     (Fmt.Duration_Strings.To_String (Fmt.To_ISO_Duration (D_45_5s)));

   Put ("  1 week: ");
   Put_Line
     (Fmt.Duration_Strings.To_String (Fmt.To_ISO_Duration (D_1_Week)));
   New_Line;

   --  ========================================================================
   --  Step 2: Human-Readable Format (SPARK-proven)
   --  ========================================================================
   Put_Line ("Step 2: Human-Readable Format");
   New_Line;

   Put ("  1h 30m 45s: ");
   Put_Line
     (Fmt.Duration_Strings.To_String (Fmt.To_Human_Duration (D_1h30m45s)));

   Put ("  2d 12h: ");
   Put_Line
     (Fmt.Duration_Strings.To_String (Fmt.To_Human_Duration (D_2d12h)));

   Put ("  -5m 30s: ");
   Put_Line
     (Fmt.Duration_Strings.To_String (Fmt.To_Human_Duration (D_Neg_5m30s)));

   Put ("  Zero: ");
   Put_Line
     (Fmt.Duration_Strings.To_String (Fmt.To_Human_Duration (D_Zero)));

   Put ("  45.5s: ");
   Put_Line
     (Fmt.Duration_Strings.To_String (Fmt.To_Human_Duration (D_45_5s)));

   Put ("  1 week: ");
   Put_Line
     (Fmt.Duration_Strings.To_String (Fmt.To_Human_Duration (D_1_Week)));
   New_Line;

   --  ========================================================================
   --  Step 3: Duration Arithmetic with Formatting (SPARK-proven)
   --  ========================================================================
   Put_Line ("Step 3: Duration Arithmetic Results");
   New_Line;

   declare
      Sum  : constant Duration_Type := Ops.Add (D_1h30m45s, D_2d12h);
      Diff : constant Duration_Type := Ops.Subtract (D_2d12h, D_1h30m45s);
      Neg  : constant Duration_Type := Ops.Negate (D_1h30m45s);
   begin
      Put ("  (1h30m45s) + (2d12h) = ");
      Put_Line
        (Fmt.Duration_Strings.To_String (Fmt.To_Human_Duration (Sum)));

      Put ("  (2d12h) - (1h30m45s) = ");
      Put_Line
        (Fmt.Duration_Strings.To_String (Fmt.To_Human_Duration (Diff)));

      Put ("  -(1h30m45s) = ");
      Put_Line
        (Fmt.Duration_Strings.To_String (Fmt.To_Human_Duration (Neg)));
   end;
   New_Line;

   --  ========================================================================
   --  Step 4: Using Operators (SPARK-proven)
   --  ========================================================================
   Put_Line ("Step 4: Using Duration Operators");
   New_Line;

   declare
      use Ops;
      Op_Sum : constant Duration_Type := D_1h30m45s + D_45_5s;
      Op_Neg : constant Duration_Type := -D_1h30m45s;
   begin
      Put ("  (1h30m45s) + (45.5s) via '+': ");
      Put_Line
        (Fmt.Duration_Strings.To_String (Fmt.To_Human_Duration (Op_Sum)));

      Put ("  -(1h30m45s) via unary '-': ");
      Put_Line
        (Fmt.Duration_Strings.To_String (Fmt.To_Human_Duration (Op_Neg)));
   end;

   New_Line;
   Put_Line ("==============================================");
   Put_Line ("SPARK workflow complete!");
   Put_Line ("  Core logic: SPARK_Mode On (provable)");
   Put_Line ("  I/O layer:  SPARK_Mode Off (standard pattern)");
   Put_Line ("==============================================");

end Duration_Formatting;
