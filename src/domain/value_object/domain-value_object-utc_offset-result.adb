pragma Ada_2022;
--  ===========================================================================
--  Domain.Value_Object.Utc_Offset.Result
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Result value object - immutable domain data.
--
--  ===========================================================================

package body Domain.Value_Object.UTC_Offset.Result is

   use Domain.Error;

   --  ========================================================================
   --  Validation Helpers
   --  ========================================================================

   --  Check if string contains only digits (no exceptions)
   function Is_Numeric_String (S : String) return Boolean is
   begin
      if S'Length = 0 then
         return False;
      end if;

      for C of S loop
         if C not in '0' .. '9' then
            return False;
         end if;
      end loop;

      return True;
   end Is_Numeric_String;

   --  ========================================================================
   --  Smart Constructors
   --  ========================================================================

   function From_Seconds (Seconds : Integer) return Result is
   begin
      if Seconds not in Min_Offset_Seconds .. Max_Offset_Seconds then
         return Error
           (Kind    => Validation_Error,
            Message => "UTC offset" & Seconds'Image &
                      " seconds out of valid range [" &
                      Min_Offset_Seconds'Image & ".." &
                      Max_Offset_Seconds'Image & "]");
      end if;

      return Ok (UTC_Offset_Type'(Seconds => Seconds));
   end From_Seconds;

   function From_Hours_Minutes (Hours : Integer; Minutes : Integer := 0)
                                return Result is
      Total_Seconds : constant Integer :=
        (Hours * Seconds_Per_Hour) + (Minutes * Seconds_Per_Minute);
   begin
      return From_Seconds (Total_Seconds);
   end From_Hours_Minutes;

   function From_String (Offset_Str : String) return Result is
      Trimmed : constant String := Offset_Str;
   begin
      --  Handle "Z" (Zulu time = UTC)
      if Trimmed = "Z" or Trimmed = "z" then
         return Ok (UTC);
      end if;

      --  Validate format: [+/-]HH:MM or [+/-]HHMM
      if Trimmed'Length < 5 or Trimmed'Length > 6 then
         return Error
           (Kind    => Validation_Error,
            Message => "Invalid UTC offset format: " & Offset_Str);
      end if;

      --  Extract sign
      if Trimmed (Trimmed'First) not in '+' | '-' then
         return Error
           (Kind    => Validation_Error,
            Message => "UTC offset must start with + or -: " & Offset_Str);
      end if;

      declare
         Sign     : constant Integer := (if Trimmed (Trimmed'First) = '+' then 1 else -1);
         Hour_Str : String (1 .. 2);
         Min_Str  : String (1 .. 2);
         H        : Integer;
         M        : Integer;
      begin
         --  Parse format [+/-]HH:MM
         if Trimmed'Length = 6 and then Trimmed (Trimmed'First + 3) = ':' then
            Hour_Str := Trimmed (Trimmed'First + 1 .. Trimmed'First + 2);
            Min_Str  := Trimmed (Trimmed'First + 4 .. Trimmed'First + 5);
         --  Parse format [+/-]HHMM
         elsif Trimmed'Length = 5 then
            Hour_Str := Trimmed (Trimmed'First + 1 .. Trimmed'First + 2);
            Min_Str  := Trimmed (Trimmed'First + 3 .. Trimmed'First + 4);
         else
            return Error
              (Kind    => Validation_Error,
               Message => "Invalid UTC offset format: " & Offset_Str);
         end if;

         --  Convert to integers (NO EXCEPTIONS - pure functional)
         --  Validate numeric format before conversion
         if not Is_Numeric_String (Hour_Str) or not Is_Numeric_String (Min_Str) then
            return Error
              (Kind    => Validation_Error,
               Message => "Invalid numeric values in UTC offset: " & Offset_Str);
         end if;

         H := Integer'Value (Hour_Str);
         M := Integer'Value (Min_Str);

         --  Apply sign and create offset
         return From_Hours_Minutes (Sign * H, Sign * M);
      end;
   end From_String;

end Domain.Value_Object.UTC_Offset.Result;
