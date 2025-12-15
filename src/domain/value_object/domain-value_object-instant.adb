pragma Ada_2022;
--  =========================================================================
--  Domain.Value_Object.Instant - Implementation
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  =========================================================================

with Interfaces;
use Interfaces;
with Domain.Error;
use Domain.Error;

package body Domain.Value_Object.Instant is

   package DT renames Domain.Value_Object.Duration_Type;

   Nanos_Per_Second : constant Integer_64 := 1_000_000_000;

   --  ========================================================================
   --  Constructors
   --  ========================================================================

   function From_Unix_Epoch
     (Seconds : Integer_64;
      Nanos   : Nanoseconds_Range := 0) return Instant_Result.Result
   is
      --  Maximum seconds before multiplication would overflow Integer_64
      --  Integer_64'Last / Nanos_Per_Second = 9_223_372_036 seconds
      --  This is approximately ±292 years from epoch
      Max_Seconds : constant Integer_64 :=
        Integer_64'Last / Nanos_Per_Second;
      Min_Seconds : constant Integer_64 :=
        Integer_64'First / Nanos_Per_Second;

      Seconds_As_Nanos : Integer_64;
      Total_Nanos      : Integer_64;
   begin
      --  Check for overflow in Seconds * Nanos_Per_Second
      if Seconds > Max_Seconds then
         return Instant_Result.Error
           (Kind    => Overflow_Error,
            Message => "Epoch seconds too large, would overflow");
      end if;

      if Seconds < Min_Seconds then
         return Instant_Result.Error
           (Kind    => Overflow_Error,
            Message => "Epoch seconds too negative, would overflow");
      end if;

      --  Safe to multiply now
      Seconds_As_Nanos := Seconds * Nanos_Per_Second;

      --  Check for overflow when adding nanoseconds
      --  Since Nanos is always non-negative (0..999_999_999), we only
      --  need to check positive overflow
      if Seconds_As_Nanos > Integer_64'Last - Integer_64 (Nanos) then
         return Instant_Result.Error
           (Kind    => Overflow_Error,
            Message => "Epoch nanoseconds total would overflow");
      end if;

      Total_Nanos := Seconds_As_Nanos + Integer_64 (Nanos);
      return Instant_Result.Ok ((Epoch_Nanos => Total_Nanos));
   end From_Unix_Epoch;

   function From_Epoch_Nanos (Nanos : Integer_64) return Instant is
   begin
      return (Epoch_Nanos => Nanos);
   end From_Epoch_Nanos;

   --  ========================================================================
   --  Accessors
   --  ========================================================================

   function Get_Epoch_Nanos (Self : Instant) return Integer_64 is
   begin
      return Self.Epoch_Nanos;
   end Get_Epoch_Nanos;

   function To_Unix_Epoch (Self : Instant) return Unix_Epoch_Type is
      S : Integer_64;
      N : Nanoseconds_Range;
   begin
      if Self.Epoch_Nanos >= 0 then
         S := Self.Epoch_Nanos / Nanos_Per_Second;
         N := Nanoseconds_Range (Self.Epoch_Nanos mod Nanos_Per_Second);
      else
         --  Floor division for negative values
         S := (Self.Epoch_Nanos - Nanos_Per_Second + 1) / Nanos_Per_Second;
         N := Nanoseconds_Range (Self.Epoch_Nanos - S * Nanos_Per_Second);
      end if;

      return (Seconds => S, Nanoseconds => N);
   end To_Unix_Epoch;

   --  ========================================================================
   --  Arithmetic with Duration
   --  ========================================================================

   function Add
     (Self : Instant;
      D    : Duration_Type) return Instant_Result.Result
   is
      Duration_Nanos : constant Integer_64 := DT.To_Nanos (D);
      New_Nanos      : Integer_64;
   begin
      --  Check for overflow
      if Duration_Nanos > 0 and then
         Self.Epoch_Nanos > Integer_64'Last - Duration_Nanos
      then
         return Instant_Result.Error
           (Kind    => Overflow_Error,
            Message => "Instant addition would overflow");
      end if;

      if Duration_Nanos < 0 and then
         Self.Epoch_Nanos < Integer_64'First - Duration_Nanos
      then
         return Instant_Result.Error
           (Kind    => Overflow_Error,
            Message => "Instant addition would underflow");
      end if;

      New_Nanos := Self.Epoch_Nanos + Duration_Nanos;
      return Instant_Result.Ok ((Epoch_Nanos => New_Nanos));
   end Add;

   function Subtract
     (Self : Instant;
      D    : Duration_Type) return Instant_Result.Result
   is
   begin
      --  Subtract is Add with negated duration
      return Add (Self, DT.Negate (D));
   end Subtract;

   function Diff
     (Self  : Instant;
      Other : Instant) return Duration_Type
   is
      Delta_Nanos : constant Integer_64 :=
        Other.Epoch_Nanos - Self.Epoch_Nanos;
   begin
      return DT.From_Nanos (Delta_Nanos);
   end Diff;

   --  ========================================================================
   --  Arithmetic Operators
   --  ========================================================================

   function "+" (Left : Instant; Right : Duration_Type)
     return Instant_Result.Result
   is
   begin
      return Add (Left, Right);
   end "+";

   function "-" (Left : Instant; Right : Duration_Type)
     return Instant_Result.Result
   is
   begin
      return Subtract (Left, Right);
   end "-";

   function "-" (Left, Right : Instant) return Duration_Type is
   begin
      return Diff (Right, Left);
   end "-";

   --  ========================================================================
   --  Comparison
   --  ========================================================================

   overriding function "=" (Left, Right : Instant) return Boolean is
   begin
      return Left.Epoch_Nanos = Right.Epoch_Nanos;
   end "=";

   function "<" (Left, Right : Instant) return Boolean is
   begin
      return Left.Epoch_Nanos < Right.Epoch_Nanos;
   end "<";

   function "<=" (Left, Right : Instant) return Boolean is
   begin
      return Left.Epoch_Nanos <= Right.Epoch_Nanos;
   end "<=";

   function ">" (Left, Right : Instant) return Boolean is
   begin
      return Left.Epoch_Nanos > Right.Epoch_Nanos;
   end ">";

   function ">=" (Left, Right : Instant) return Boolean is
   begin
      return Left.Epoch_Nanos >= Right.Epoch_Nanos;
   end ">=";

end Domain.Value_Object.Instant;
