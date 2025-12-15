pragma Ada_2022;
--  =========================================================================
--  SPARK Workflow: Instant and Duration Arithmetic
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    SPARK-proven workflow demonstrating instant and duration arithmetic.
--    Core logic is SPARK_Mode On (formally verifiable), I/O is SPARK_Mode Off.
--
--  Architecture:
--    - Pure computation: SPARK_Mode On (proven)
--    - I/O wrapper: SPARK_Mode Off (trivial, not proven)
--    This is the standard pattern for SPARK projects using community GNAT.
--
--  Key Differences from Desktop Version:
--    - All computation uses SPARK-enabled APIs
--    - Bounded strings used directly (no heap allocation)
--    - Pre-constructed domain values (no live clock/TZif)
--    - Core operations formally provable with gnatprove
--
--  SPARK-Enabled APIs Used:
--    - Zoneinfo.API.Operations (Instant/Duration arithmetic)
--    - Zoneinfo.API.Format (bounded string formatting)
--
--  To Prove (core logic only):
--    gnatprove -P examples.gpr --mode=prove instant_arithmetic.adb
--  =========================================================================

with Ada.Text_IO;
with Zoneinfo.API.Operations;
with Zoneinfo.API.Format;
with Domain.Value_Object.Instant;
with Domain.Value_Object.Duration_Type;

procedure Instant_Arithmetic is
   package TIO renames Ada.Text_IO;
   package Ops renames Zoneinfo.API.Operations;
   package Fmt renames Zoneinfo.API.Format;

   --  Import types
   subtype Instant is Ops.Instant;
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
   --  Pre-constructed Test Values (SPARK-compatible constants)
   --  ========================================================================
   --  In SPARK code, we work with known values rather than live clock data.
   --  These could come from configuration, parsed input, or constants.

   --  Base instant: 2025-01-15 12:00:00 UTC (epoch seconds = 1736942400)
   --  Using From_Epoch_Nanos with seconds * 1_000_000_000
   Base_Epoch_Nanos : constant := 1_736_942_400 * 1_000_000_000;
   Base_Instant : constant Instant :=
     Domain.Value_Object.Instant.From_Epoch_Nanos (Base_Epoch_Nanos);

   --  Duration: 3 hours, 30 minutes, 45 seconds = 12645 seconds
   Three_Hours : constant Duration_Type :=
     Domain.Value_Object.Duration_Type.From_Seconds (12_645);

   --  Duration: 1 day (86400 seconds)
   One_Day : constant Duration_Type :=
     Domain.Value_Object.Duration_Type.From_Seconds (86_400);

begin
   Put_Line ("==============================================");
   Put_Line ("SPARK Workflow: Instant & Duration Arithmetic");
   Put_Line ("==============================================");
   New_Line;

   --  ========================================================================
   --  Step 1: Display base instant
   --  ========================================================================
   Put_Line ("Step 1: Base Instant");
   Put ("  Epoch seconds: ");
   Put_Line
     (Fmt.Datetime_Strings.To_String
        (Fmt.To_Epoch_String (Base_Instant, Include_Nanos => False)));
   New_Line;

   --  ========================================================================
   --  Step 2: Add duration to instant (SPARK-proven operation)
   --  ========================================================================
   Put_Line ("Step 2: Add 3h 30m 45s to base instant");

   declare
      Add_Result : constant Ops.Instant_Result.Result :=
        Ops.Add (Base_Instant, Three_Hours);
   begin
      if Ops.Instant_Result.Is_Ok (Add_Result) then
         declare
            New_Instant : constant Instant :=
              Ops.Instant_Result.Value (Add_Result);
         begin
            Put ("  Result epoch: ");
            Put_Line
              (Fmt.Datetime_Strings.To_String
                 (Fmt.To_Epoch_String (New_Instant, Include_Nanos => False)));
         end;
      else
         Put_Line ("  Error: Overflow in addition");
      end if;
   end;
   New_Line;

   --  ========================================================================
   --  Step 3: Subtract duration from instant (SPARK-proven operation)
   --  ========================================================================
   Put_Line ("Step 3: Subtract 1 day from base instant");

   declare
      Sub_Result : constant Ops.Instant_Result.Result :=
        Ops.Subtract (Base_Instant, One_Day);
   begin
      if Ops.Instant_Result.Is_Ok (Sub_Result) then
         declare
            Earlier : constant Instant :=
              Ops.Instant_Result.Value (Sub_Result);
         begin
            Put ("  Result epoch: ");
            Put_Line
              (Fmt.Datetime_Strings.To_String
                 (Fmt.To_Epoch_String (Earlier, Include_Nanos => False)));
         end;
      else
         Put_Line ("  Error: Underflow in subtraction");
      end if;
   end;
   New_Line;

   --  ========================================================================
   --  Step 4: Calculate difference between instants (SPARK-proven)
   --  ========================================================================
   Put_Line ("Step 4: Calculate difference (base + 1 day) - base");

   declare
      Add_Day_Result : constant Ops.Instant_Result.Result :=
        Ops.Add (Base_Instant, One_Day);
   begin
      if Ops.Instant_Result.Is_Ok (Add_Day_Result) then
         declare
            Later : constant Instant :=
              Ops.Instant_Result.Value (Add_Day_Result);
            Diff : constant Duration_Type := Ops.Diff (Base_Instant, Later);
         begin
            Put ("  ISO Duration: ");
            Put_Line
              (Fmt.Duration_Strings.To_String (Fmt.To_ISO_Duration (Diff)));
            Put ("  Human format: ");
            Put_Line
              (Fmt.Duration_Strings.To_String (Fmt.To_Human_Duration (Diff)));
         end;
      else
         Put_Line ("  Error: Could not compute later instant");
      end if;
   end;
   New_Line;

   --  ========================================================================
   --  Step 5: Duration arithmetic (SPARK-proven)
   --  ========================================================================
   Put_Line ("Step 5: Duration arithmetic");

   declare
      Sum : constant Duration_Type := Ops.Add (Three_Hours, One_Day);
      Neg : constant Duration_Type := Ops.Negate (Three_Hours);
   begin
      Put ("  3h30m45s + 1d = ");
      Put_Line
        (Fmt.Duration_Strings.To_String (Fmt.To_Human_Duration (Sum)));
      Put ("  -(3h30m45s) = ");
      Put_Line
        (Fmt.Duration_Strings.To_String (Fmt.To_Human_Duration (Neg)));
   end;
   New_Line;

   --  ========================================================================
   --  Step 6: Using operators (Result-returning, SPARK-proven)
   --  ========================================================================
   Put_Line ("Step 6: Using + and - operators");

   declare
      use Ops;
      Op_Result : constant Ops.Instant_Result.Result :=
        Base_Instant + Three_Hours;
   begin
      if Ops.Instant_Result.Is_Ok (Op_Result) then
         Put ("  base + duration via '+': ");
         Put_Line
           (Fmt.Datetime_Strings.To_String
              (Fmt.To_Epoch_String
                 (Ops.Instant_Result.Value (Op_Result),
                  Include_Nanos => False)));
      else
         Put_Line ("  Error in operator");
      end if;
   end;

   New_Line;
   Put_Line ("==============================================");
   Put_Line ("SPARK workflow complete!");
   Put_Line ("  Core logic: SPARK_Mode On (provable)");
   Put_Line ("  I/O layer:  SPARK_Mode Off (standard pattern)");
   Put_Line ("==============================================");

end Instant_Arithmetic;
