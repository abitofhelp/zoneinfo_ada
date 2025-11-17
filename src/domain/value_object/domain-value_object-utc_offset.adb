pragma Ada_2022;
--  ===========================================================================
--  Domain.Value_Object.Utc_Offset
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Utc Offset value object - immutable domain data.
--
--  ===========================================================================

package body Domain.Value_Object.UTC_Offset is

   --  ========================================================================
   --  Query Operations
   --  ========================================================================

   function To_Seconds (Offset : UTC_Offset_Type) return Integer is
   begin
      return Offset.Seconds;
   end To_Seconds;

   function To_Minutes (Offset : UTC_Offset_Type) return Integer is
   begin
      return Offset.Seconds / Seconds_Per_Minute;
   end To_Minutes;

   function To_String (Offset : UTC_Offset_Type) return String is
      Abs_Seconds : constant Integer := abs Offset.Seconds;
      H           : constant Integer := Abs_Seconds / Seconds_Per_Hour;
      M           : constant Integer := (Abs_Seconds rem Seconds_Per_Hour) / Seconds_Per_Minute;
      Sign_Char   : constant Character := (if Offset.Seconds >= 0 then '+' else '-');
      Hour_Str    : constant String := (if H < 10 then "0" else "") & H'Image (2 .. H'Image'Last);
      Min_Str     : constant String := (if M < 10 then "0" else "") & M'Image (2 .. M'Image'Last);
   begin
      return Sign_Char & Hour_Str & ":" & Min_Str;
   end To_String;

   function Hours (Offset : UTC_Offset_Type) return Integer is
   begin
      return Offset.Seconds / Seconds_Per_Hour;
   end Hours;

   function Minutes (Offset : UTC_Offset_Type) return Integer is
   begin
      return (abs Offset.Seconds rem Seconds_Per_Hour) / Seconds_Per_Minute *
             (if Offset.Seconds < 0 then -1 else 1);
   end Minutes;

   --  ========================================================================
   --  Operators
   --  ========================================================================

   overriding function "=" (Left, Right : UTC_Offset_Type) return Boolean is
   begin
      return Left.Seconds = Right.Seconds;
   end "=";

   function "<" (Left, Right : UTC_Offset_Type) return Boolean is
   begin
      return Left.Seconds < Right.Seconds;
   end "<";

   function "-" (Offset : UTC_Offset_Type) return UTC_Offset_Type is
   begin
      return UTC_Offset_Type'(Seconds => -Offset.Seconds);
   end "-";

end Domain.Value_Object.UTC_Offset;
