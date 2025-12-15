pragma Ada_2022;
--  ===========================================================================
--  Zoneinfo.API.Format - Implementation
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  ===========================================================================

pragma SPARK_Mode (On);

with Domain.Value_Object.Civil;
use Domain.Value_Object.Civil;

package body Zoneinfo.API.Format is

   use type Domain.Value_Object.Duration_Type.Integer_64;

   --  ========================================================================
   --  Internal Helper Functions
   --  ========================================================================

   --  Pad integer to 2 digits with leading zero
   function Pad2 (N : Natural) return String
   with
     Pre  => N <= 99,
     Post => Pad2'Result'Length = 2
   is
      S : constant String := Natural'Image (N);
   begin
      if N < 10 then
         return "0" & S (S'Last);
      else
         return S (S'First + 1 .. S'Last);
      end if;
   end Pad2;

   --  Pad integer to 4 digits with leading zeros
   function Pad4 (N : Natural) return String
   with
     Pre  => N <= 9999,
     Post => Pad4'Result'Length = 4
   is
      S : constant String := Natural'Image (N);
      Len : constant Natural := S'Length - 1;  -- Remove leading space
   begin
      case Len is
         when 1 => return "000" & S (S'Last);
         when 2 => return "00" & S (S'First + 1 .. S'Last);
         when 3 => return "0" & S (S'First + 1 .. S'Last);
         when others => return S (S'First + 1 .. S'Last);
      end case;
   end Pad4;

   --  Format nanoseconds as decimal fraction (omit trailing zeros)
   function Format_Nanos (Nanos : Nanosecond_Number) return String is
   begin
      if Nanos = 0 then
         return "";
      end if;

      declare
         S : constant String :=
           Domain.Value_Object.Duration_Type.Integer_64'Image
             (Domain.Value_Object.Duration_Type.Integer_64 (Nanos));
         --  Remove leading space
         Raw : constant String := S (S'First + 1 .. S'Last);
         --  Pad to 9 digits
         Padded : String (1 .. 9) := (others => '0');
         Start  : constant Natural := 10 - Raw'Length;
      begin
         Padded (Start .. 9) := Raw;
         --  Trim trailing zeros
         for I in reverse 1 .. 9 loop
            if Padded (I) /= '0' then
               return "." & Padded (1 .. I);
            end if;
         end loop;
         return "";
      end;
   end Format_Nanos;

   --  ========================================================================
   --  Civil Formatting
   --  ========================================================================

   function To_ISO_8601
     (C             : Civil;
      Include_Nanos : Boolean := True) return Datetime_String
   is
      Date_Part : constant String :=
        Pad4 (C.Year) & "-" & Pad2 (C.Month) & "-" & Pad2 (C.Day);
      Time_Part : constant String :=
        Pad2 (C.Hour) & ":" & Pad2 (C.Minute) & ":" & Pad2 (C.Second);
      Nano_Part : constant String :=
        (if Include_Nanos then Format_Nanos (C.Nanosecond) else "");
   begin
      return Datetime_Strings.To_Bounded_String
        (Date_Part & "T" & Time_Part & Nano_Part);
   end To_ISO_8601;

   function To_ISO_8601_With_Offset
     (C             : Civil;
      Offset        : Duration_Type;
      Include_Nanos : Boolean := True) return Datetime_String
   is
      Base   : constant Datetime_String := To_ISO_8601 (C, Include_Nanos);
      Suffix : constant Datetime_String := Format_Offset (Offset);
   begin
      return Datetime_Strings.To_Bounded_String
        (Datetime_Strings.To_String (Base) &
         Datetime_Strings.To_String (Suffix));
   end To_ISO_8601_With_Offset;

   function To_ISO_8601_With_Zone
     (C             : Civil;
      Zone          : Zone_ID;
      Include_Nanos : Boolean := True) return Datetime_String
   is
      Base : constant Datetime_String := To_ISO_8601 (C, Include_Nanos);
      Zone_Str : constant String :=
        Domain.Value_Object.Zone_ID.To_String (Zone);
   begin
      return Datetime_Strings.To_Bounded_String
        (Datetime_Strings.To_String (Base) & "[" & Zone_Str & "]");
   end To_ISO_8601_With_Zone;

   function To_ISO_8601_Full
     (C             : Civil;
      Offset        : Duration_Type;
      Zone          : Zone_ID;
      Include_Nanos : Boolean := True) return Datetime_String
   is
      Base   : constant Datetime_String := To_ISO_8601 (C, Include_Nanos);
      Suffix : constant Datetime_String := Format_Offset (Offset);
      Zone_Str : constant String :=
        Domain.Value_Object.Zone_ID.To_String (Zone);
   begin
      return Datetime_Strings.To_Bounded_String
        (Datetime_Strings.To_String (Base) &
         Datetime_Strings.To_String (Suffix) &
         "[" & Zone_Str & "]");
   end To_ISO_8601_Full;

   --  ========================================================================
   --  Date-Only Formatting
   --  ========================================================================

   function To_ISO_Date (C : Civil) return Datetime_String is
   begin
      return Datetime_Strings.To_Bounded_String
        (Pad4 (C.Year) & "-" & Pad2 (C.Month) & "-" & Pad2 (C.Day));
   end To_ISO_Date;

   --  ========================================================================
   --  Time-Only Formatting
   --  ========================================================================

   function To_ISO_Time
     (C             : Civil;
      Include_Nanos : Boolean := True) return Datetime_String
   is
      Time_Part : constant String :=
        Pad2 (C.Hour) & ":" & Pad2 (C.Minute) & ":" & Pad2 (C.Second);
      Nano_Part : constant String :=
        (if Include_Nanos then Format_Nanos (C.Nanosecond) else "");
   begin
      return Datetime_Strings.To_Bounded_String (Time_Part & Nano_Part);
   end To_ISO_Time;

   --  ========================================================================
   --  Instant Formatting
   --  ========================================================================

   function To_Epoch_String
     (I             : Instant;
      Include_Nanos : Boolean := True) return Datetime_String
   is
      use Domain.Value_Object.Instant;
      package DT renames Domain.Value_Object.Duration_Type;
      Epoch : constant Unix_Epoch_Type := To_Unix_Epoch (I);
      Sec_Str : constant String := DT.Integer_64'Image (Epoch.Seconds);
   begin
      if Include_Nanos and then Epoch.Nanoseconds > 0 then
         declare
            Nanos : constant Nanosecond_Number :=
              Nanosecond_Number (Epoch.Nanoseconds);
            Nano_Str : constant String := Format_Nanos (Nanos);
         begin
            --  Remove leading space from Integer_64'Image
            return Datetime_Strings.To_Bounded_String
              (Sec_Str (Sec_Str'First + 1 .. Sec_Str'Last) & Nano_Str);
         end;
      else
         return Datetime_Strings.To_Bounded_String
           (Sec_Str (Sec_Str'First + 1 .. Sec_Str'Last));
      end if;
   end To_Epoch_String;

   --  ========================================================================
   --  Duration Formatting
   --  ========================================================================

   function To_ISO_Duration (D : Duration_Type) return Duration_String is
      use Domain.Value_Object.Duration_Type;
      Secs  : Integer_64 := Get_Seconds (D);
      Nanos : constant Nanoseconds_Range := Get_Nanoseconds (D);
      Is_Neg : constant Boolean := Is_Negative (D);

      Result : Duration_Strings.Bounded_String :=
        Duration_Strings.Null_Bounded_String;

      Days    : Integer_64;
      Hours   : Integer_64;
      Minutes : Integer_64;
      Seconds : Integer_64;
   begin
      --  Handle negative duration
      if Is_Neg then
         Secs := -Secs;
         if Nanos > 0 then
            Secs := Secs - 1;  -- Borrow from next second for negative
         end if;
         Duration_Strings.Append (Result, "-");
      end if;

      --  Start with period designator
      Duration_Strings.Append (Result, "P");

      --  Extract components
      Days := Secs / 86_400;
      Secs := Secs mod 86_400;
      Hours := Secs / 3_600;
      Secs := Secs mod 3_600;
      Minutes := Secs / 60;
      Seconds := Secs mod 60;

      --  Format days
      if Days > 0 then
         declare
            S : constant String := Integer_64'Image (Days);
         begin
            Duration_Strings.Append (Result, S (S'First + 1 .. S'Last) & "D");
         end;
      end if;

      --  Format time components
      if Hours > 0 or else Minutes > 0
        or else Seconds > 0 or else Nanos > 0
      then
         Duration_Strings.Append (Result, "T");

         if Hours > 0 then
            declare
               S : constant String := Integer_64'Image (Hours);
            begin
               Duration_Strings.Append
                 (Result, S (S'First + 1 .. S'Last) & "H");
            end;
         end if;

         if Minutes > 0 then
            declare
               S : constant String := Integer_64'Image (Minutes);
            begin
               Duration_Strings.Append
                 (Result, S (S'First + 1 .. S'Last) & "M");
            end;
         end if;

         if Seconds > 0 or else Nanos > 0 then
            declare
               S : constant String := Integer_64'Image (Seconds);
               Nano_Str : constant String :=
                 (if Nanos > 0
                  then Format_Nanos (Nanosecond_Number (Nanos))
                  else "");
            begin
               Duration_Strings.Append
                 (Result, S (S'First + 1 .. S'Last) & Nano_Str & "S");
            end;
         end if;
      elsif Days = 0 then
         --  Zero duration: "PT0S"
         Duration_Strings.Append (Result, "T0S");
      end if;

      return Result;
   end To_ISO_Duration;

   function To_Human_Duration (D : Duration_Type) return Duration_String is
      use Domain.Value_Object.Duration_Type;
      Secs  : Integer_64 := Get_Seconds (D);
      Nanos : constant Nanoseconds_Range := Get_Nanoseconds (D);
      Is_Neg : constant Boolean := Is_Negative (D);

      Result : Duration_Strings.Bounded_String :=
        Duration_Strings.Null_Bounded_String;

      Days    : Integer_64;
      Hours   : Integer_64;
      Minutes : Integer_64;
      Seconds : Integer_64;
      Has_Content : Boolean := False;
   begin
      --  Handle negative duration
      if Is_Neg then
         Secs := -Secs;
         if Nanos > 0 then
            Secs := Secs - 1;
         end if;
         Duration_Strings.Append (Result, "-");
      end if;

      --  Extract components
      Days := Secs / 86_400;
      Secs := Secs mod 86_400;
      Hours := Secs / 3_600;
      Secs := Secs mod 3_600;
      Minutes := Secs / 60;
      Seconds := Secs mod 60;

      --  Format each component
      if Days > 0 then
         declare
            S : constant String := Integer_64'Image (Days);
         begin
            Duration_Strings.Append (Result, S (S'First + 1 .. S'Last) & "d");
            Has_Content := True;
         end;
      end if;

      if Hours > 0 then
         if Has_Content then
            Duration_Strings.Append (Result, " ");
         end if;
         declare
            S : constant String := Integer_64'Image (Hours);
         begin
            Duration_Strings.Append (Result, S (S'First + 1 .. S'Last) & "h");
            Has_Content := True;
         end;
      end if;

      if Minutes > 0 then
         if Has_Content then
            Duration_Strings.Append (Result, " ");
         end if;
         declare
            S : constant String := Integer_64'Image (Minutes);
         begin
            Duration_Strings.Append (Result, S (S'First + 1 .. S'Last) & "m");
            Has_Content := True;
         end;
      end if;

      if Seconds > 0 or else Nanos > 0 or else not Has_Content then
         if Has_Content then
            Duration_Strings.Append (Result, " ");
         end if;
         declare
            S : constant String := Integer_64'Image (Seconds);
            Nano_Str : constant String :=
              (if Nanos > 0
               then Format_Nanos (Nanosecond_Number (Nanos))
               else "");
         begin
            Duration_Strings.Append
              (Result, S (S'First + 1 .. S'Last) & Nano_Str & "s");
         end;
      end if;

      return Result;
   end To_Human_Duration;

   --  ========================================================================
   --  Helper Functions
   --  ========================================================================

   function Format_Offset (Offset : Duration_Type) return Datetime_String is
      use Domain.Value_Object.Duration_Type;
      Secs : Integer_64 := Get_Seconds (Offset);
      Is_Neg : constant Boolean := Secs < 0;
      Sign : constant String := (if Is_Neg then "-" else "+");
      Hours   : Natural;
      Minutes : Natural;
   begin
      --  UTC offset zero: use "Z"
      if Secs = 0 and then Get_Nanoseconds (Offset) = 0 then
         return Datetime_Strings.To_Bounded_String ("Z");
      end if;

      --  Get absolute value
      if Is_Neg then
         Secs := -Secs;
      end if;

      Hours := Natural (Secs / 3_600);
      Minutes := Natural ((Secs mod 3_600) / 60);

      return Datetime_Strings.To_Bounded_String
        (Sign & Pad2 (Hours) & ":" & Pad2 (Minutes));
   end Format_Offset;

end Zoneinfo.API.Format;
