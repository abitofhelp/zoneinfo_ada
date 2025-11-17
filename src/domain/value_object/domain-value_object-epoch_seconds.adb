pragma Ada_2022;
--  ===========================================================================
--  Domain.Value_Object.Epoch_Seconds
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Epoch Seconds value object - immutable domain data.
--
--  ===========================================================================

package body Domain.Value_Object.Epoch_Seconds is

   --  ========================================================================
   --  Constructors
   --  ========================================================================

   function From_Seconds (Seconds : Long_Long_Integer) return Epoch_Seconds_Type is
   begin
      return Epoch_Seconds_Type'(Seconds => Seconds);
   end From_Seconds;

   --  ========================================================================
   --  Query Operations
   --  ========================================================================

   function To_Seconds (Epoch : Epoch_Seconds_Type) return Long_Long_Integer is
   begin
      return Epoch.Seconds;
   end To_Seconds;

   function To_String (Epoch : Epoch_Seconds_Type) return String is
      Seconds_Str : constant String := Epoch.Seconds'Image;
   begin
      --  Add explicit + sign for positive values (Image includes space)
      if Epoch.Seconds >= 0 then
         return "+" & Seconds_Str (2 .. Seconds_Str'Last);
      else
         return Seconds_Str;
      end if;
   end To_String;

   --  ========================================================================
   --  Comparison Operators
   --  ========================================================================

   overriding function "=" (Left, Right : Epoch_Seconds_Type) return Boolean is
   begin
      return Left.Seconds = Right.Seconds;
   end "=";

   function "<" (Left, Right : Epoch_Seconds_Type) return Boolean is
   begin
      return Left.Seconds < Right.Seconds;
   end "<";

   function "<=" (Left, Right : Epoch_Seconds_Type) return Boolean is
   begin
      return Left.Seconds <= Right.Seconds;
   end "<=";

   function ">" (Left, Right : Epoch_Seconds_Type) return Boolean is
   begin
      return Left.Seconds > Right.Seconds;
   end ">";

   function ">=" (Left, Right : Epoch_Seconds_Type) return Boolean is
   begin
      return Left.Seconds >= Right.Seconds;
   end ">=";

   --  ========================================================================
   --  Arithmetic Operators
   --  ========================================================================

   function "+" (Left : Epoch_Seconds_Type; Right : Long_Long_Integer)
                 return Epoch_Seconds_Type is
   begin
      return Epoch_Seconds_Type'(Seconds => Left.Seconds + Right);
   end "+";

   function "-" (Left : Epoch_Seconds_Type; Right : Long_Long_Integer)
                 return Epoch_Seconds_Type is
   begin
      return Epoch_Seconds_Type'(Seconds => Left.Seconds - Right);
   end "-";

   function "-" (Left, Right : Epoch_Seconds_Type) return Long_Long_Integer is
   begin
      return Left.Seconds - Right.Seconds;
   end "-";

end Domain.Value_Object.Epoch_Seconds;
