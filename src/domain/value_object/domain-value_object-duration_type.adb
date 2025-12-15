pragma Ada_2022;
--  =========================================================================
--  Domain.Value_Object.Duration_Type - Implementation
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  =========================================================================

package body Domain.Value_Object.Duration_Type is

   --  ========================================================================
   --  Constructors
   --  ========================================================================

   function From_Seconds (S : Integer_64) return Duration_Type is
   begin
      return (Seconds => S, Nanoseconds => 0);
   end From_Seconds;

   function From_Millis (Ms : Integer_64) return Duration_Type is
      S  : Integer_64;
      Ns : Nanoseconds_Range;
   begin
      if Ms >= 0 then
         S  := Ms / 1_000;
         Ns := Nanoseconds_Range ((Ms mod 1_000) * Nanos_Per_Milli);
      else
         --  Floor division for negative values
         S  := (Ms - 999) / 1_000;
         Ns := Nanoseconds_Range ((Ms - S * 1_000) * Nanos_Per_Milli);
      end if;
      return (Seconds => S, Nanoseconds => Ns);
   end From_Millis;

   function From_Micros (Us : Integer_64) return Duration_Type is
      S  : Integer_64;
      Ns : Nanoseconds_Range;
   begin
      if Us >= 0 then
         S  := Us / 1_000_000;
         Ns := Nanoseconds_Range ((Us mod 1_000_000) * Nanos_Per_Micro);
      else
         --  Floor division for negative values
         S  := (Us - 999_999) / 1_000_000;
         Ns := Nanoseconds_Range ((Us - S * 1_000_000) * Nanos_Per_Micro);
      end if;
      return (Seconds => S, Nanoseconds => Ns);
   end From_Micros;

   function From_Nanos (Ns : Integer_64) return Duration_Type is
      S    : Integer_64;
      N    : Nanoseconds_Range;
   begin
      if Ns >= 0 then
         S := Ns / Nanos_Per_Second;
         N := Nanoseconds_Range (Ns mod Nanos_Per_Second);
      else
         --  Floor division for negative values
         S := (Ns - Nanos_Per_Second + 1) / Nanos_Per_Second;
         N := Nanoseconds_Range (Ns - S * Nanos_Per_Second);
      end if;
      return (Seconds => S, Nanoseconds => N);
   end From_Nanos;

   function Create
     (Seconds     : Integer_64;
      Nanoseconds : Nanoseconds_Range) return Duration_Type
   is
   begin
      return (Seconds => Seconds, Nanoseconds => Nanoseconds);
   end Create;

   --  ========================================================================
   --  Accessors
   --  ========================================================================

   function Get_Seconds (Self : Duration_Type) return Integer_64 is
   begin
      return Self.Seconds;
   end Get_Seconds;

   function Get_Nanoseconds (Self : Duration_Type) return Nanoseconds_Range is
   begin
      return Self.Nanoseconds;
   end Get_Nanoseconds;

   --  ========================================================================
   --  Conversions
   --  ========================================================================

   function To_Seconds (Self : Duration_Type) return Integer_64 is
   begin
      return Self.Seconds;
   end To_Seconds;

   function To_Millis (Self : Duration_Type) return Integer_64 is
   begin
      return Self.Seconds * 1_000 +
             Integer_64 (Self.Nanoseconds) / Nanos_Per_Milli;
   end To_Millis;

   function To_Nanos (Self : Duration_Type) return Integer_64 is
   begin
      return Self.Seconds * Nanos_Per_Second + Integer_64 (Self.Nanoseconds);
   end To_Nanos;

   --  ========================================================================
   --  Arithmetic
   --  ========================================================================

   function Add (Left, Right : Duration_Type) return Duration_Type is
      Total_Nanos : constant Integer_64 :=
        To_Nanos (Left) + To_Nanos (Right);
   begin
      return From_Nanos (Total_Nanos);
   end Add;

   function Subtract (Left, Right : Duration_Type) return Duration_Type is
      Total_Nanos : constant Integer_64 :=
        To_Nanos (Left) - To_Nanos (Right);
   begin
      return From_Nanos (Total_Nanos);
   end Subtract;

   function Negate (Self : Duration_Type) return Duration_Type is
   begin
      return From_Nanos (-To_Nanos (Self));
   end Negate;

   --  ========================================================================
   --  Arithmetic Operators
   --  ========================================================================

   function "+" (Left, Right : Duration_Type) return Duration_Type is
   begin
      return Add (Left, Right);
   end "+";

   function "-" (Left, Right : Duration_Type) return Duration_Type is
   begin
      return Subtract (Left, Right);
   end "-";

   function "-" (Right : Duration_Type) return Duration_Type is
   begin
      return Negate (Right);
   end "-";

   --  ========================================================================
   --  Queries
   --  ========================================================================

   function Is_Negative (Self : Duration_Type) return Boolean is
   begin
      return Self.Seconds < 0;
   end Is_Negative;

   function Is_Zero (Self : Duration_Type) return Boolean is
   begin
      return Self.Seconds = 0 and then Self.Nanoseconds = 0;
   end Is_Zero;

   --  ========================================================================
   --  Comparison
   --  ========================================================================

   overriding function "=" (Left, Right : Duration_Type) return Boolean is
   begin
      return Left.Seconds = Right.Seconds
             and then Left.Nanoseconds = Right.Nanoseconds;
   end "=";

   function "<" (Left, Right : Duration_Type) return Boolean is
   begin
      if Left.Seconds = Right.Seconds then
         return Left.Nanoseconds < Right.Nanoseconds;
      else
         return Left.Seconds < Right.Seconds;
      end if;
   end "<";

   function "<=" (Left, Right : Duration_Type) return Boolean is
   begin
      return Left < Right or else Left = Right;
   end "<=";

   function ">" (Left, Right : Duration_Type) return Boolean is
   begin
      return Right < Left;
   end ">";

   function ">=" (Left, Right : Duration_Type) return Boolean is
   begin
      return Right <= Left;
   end ">=";

end Domain.Value_Object.Duration_Type;
