pragma Ada_2022;
--  =========================================================================
--  Infrastructure.Adapter.Desktop_Clock - Implementation
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Implementation Notes:
--    Uses Functional.Try.Map_To_Result for declarative exception mapping.
--  =========================================================================

with Ada.Calendar;
with Interfaces;
with Domain.Error;
with Functional.Try.Map_To_Result;

package body Infrastructure.Adapter.Desktop_Clock is

   use Interfaces;
   use Domain.Error;

   --  Unix epoch as Ada.Calendar.Time
   --  Note: Ada.Calendar epoch is implementation-defined, but GNAT uses
   --  1901-01-01. We use Time_Of to create a reference point.
   Unix_Epoch : constant Ada.Calendar.Time :=
     Ada.Calendar.Time_Of
       (Year    => 1970,
        Month   => 1,
        Day     => 1,
        Seconds => 0.0);

   Nanos_Per_Second : constant Integer_64 := 1_000_000_000;

   --  ========================================================================
   --  Internal: Raw Now Action (may raise exceptions)
   --  ========================================================================

   function Raw_Now return Instant_Result.Result is
      Current_Time  : Ada.Calendar.Time;
      Epoch_Seconds : Integer_64;
      Epoch_Nanos   : Integer_64;
      Frac_Nanos    : Integer_64;
   begin
      --  Get current time from system clock
      Current_Time := Ada.Calendar.Clock;

      --  Calculate seconds since Unix epoch
      --  Note: This subtraction works because both times are Ada.Calendar.Time
      declare
         use type Ada.Calendar.Time;
         Delta_Duration : constant Duration := Current_Time - Unix_Epoch;
      begin
         --  Extract whole seconds and fractional part
         --  Duration has sub-second precision (nanoseconds with GNAT)
         Epoch_Seconds := Integer_64 (Long_Long_Integer (Delta_Duration));

         --  Get fractional nanoseconds
         --  Duration'Small is the precision (typically 10^-9 for GNAT)
         Frac_Nanos := Integer_64
           (Long_Long_Integer ((Delta_Duration - Duration (Epoch_Seconds)) *
                               Duration (Nanos_Per_Second)));

         --  Handle potential negative fractional part
         if Frac_Nanos < 0 then
            Epoch_Seconds := Epoch_Seconds - 1;
            Frac_Nanos := Frac_Nanos + Nanos_Per_Second;
         end if;

         Epoch_Nanos := Epoch_Seconds * Nanos_Per_Second + Frac_Nanos;
      end;

      return Instant_Result.Ok
        (Domain.Value_Object.Instant.From_Epoch_Nanos (Epoch_Nanos));
   end Raw_Now;

   --  ========================================================================
   --  Internal: Make_Error for clock operation
   --  ========================================================================

   function Make_Now_Error
     (Kind : Domain.Error.Error_Kind; Message : String)
      return Instant_Result.Result
   is
   begin
      return Instant_Result.Error (Kind => Kind, Message => Message);
   end Make_Now_Error;

   --  ========================================================================
   --  Instantiate Map_To_Result for Now Operation
   --  ========================================================================

   package Try_Now is new Functional.Try.Map_To_Result
     (Error_Kind_Type    => Domain.Error.Error_Kind,
      Result_Type        => Instant_Result.Result,
      Make_Error         => Make_Now_Error,
      Default_Error_Kind => Domain.Error.IO_Error,
      Action             => Raw_Now);

   --  Map Time_Error to IO_Error; all others use default (IO_Error)
   Now_Mappings : constant Try_Now.Mapping_Array :=
     [(Ada.Calendar.Time_Error'Identity, Domain.Error.IO_Error)];

   --  ========================================================================
   --  Clock_Port Implementation
   --  ========================================================================

   function Now return Instant_Result.Result is
   begin
      return Try_Now.Run (Now_Mappings);
   end Now;

end Infrastructure.Adapter.Desktop_Clock;
