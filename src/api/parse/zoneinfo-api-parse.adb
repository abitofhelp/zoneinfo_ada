pragma Ada_2022;
--  ===========================================================================
--  Zoneinfo.API.Parse - Implementation
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  ===========================================================================

pragma SPARK_Mode (Off);

with Interfaces;
with Domain.Value_Object.Civil;
use Domain.Value_Object.Civil;
with Domain.Error;

package body Zoneinfo.API.Parse is

   use type Interfaces.Integer_64;

   --  Local subtype for convenience
   subtype Int64 is Interfaces.Integer_64;

   --  ========================================================================
   --  Internal Helper Functions
   --  ========================================================================

   function Is_Digit (C : Character) return Boolean is
     (C >= '0' and then C <= '9');

   function Parse_Int (S : String) return Integer is
      Result : Integer := 0;
   begin
      if S'Length = 0 then
         return -1;
      end if;

      for I in S'Range loop
         if not Is_Digit (S (I)) then
            return -1;
         end if;
         Result := Result * 10 + (Character'Pos (S (I)) - Character'Pos ('0'));
      end loop;

      return Result;
   exception
      when Constraint_Error =>
         return -1;
   end Parse_Int;

   function Parse_Int64 (S : String) return Int64 is
      Result : Int64 := 0;
   begin
      if S'Length = 0 then
         return -1;
      end if;

      for I in S'Range loop
         if not Is_Digit (S (I)) then
            return -1;
         end if;
         Result := Result * 10 +
           Int64 (Character'Pos (S (I)) - Character'Pos ('0'));
      end loop;

      return Result;
   exception
      when Constraint_Error =>
         return -1;
   end Parse_Int64;

   function Parse_Nanos
     (S : String) return Domain.Value_Object.Duration_Type.Nanoseconds_Range
   is
      use Domain.Value_Object.Duration_Type;
      Padded : String (1 .. 9) := [others => '0'];
      Len    : constant Natural := Natural'Min (S'Length, 9);
   begin
      if S'Length = 0 then
         return 0;
      end if;

      Padded (1 .. Len) := S (S'First .. S'First + Len - 1);

      declare
         Result : constant Int64 := Parse_Int64 (Padded);
      begin
         if Result < 0 or else Result > 999_999_999 then
            return 0;
         end if;
         return Nanoseconds_Range (Result);
      end;
   end Parse_Nanos;

   function Index (S : String; C : Character) return Natural is
   begin
      for I in S'Range loop
         if S (I) = C then
            return I;
         end if;
      end loop;
      return 0;
   end Index;

   --  ========================================================================
   --  Civil Parsing
   --  ========================================================================

   function From_ISO_8601 (S : String) return Civil_Result.Result is
      T_Pos   : constant Natural := Index (S, 'T');
      Dot_Pos : Natural;

      Year, Month, Day       : Integer;
      Hour, Minute, Second   : Integer := 0;
      Nanos : Domain.Value_Object.Duration_Type.Nanoseconds_Range := 0;
   begin
      if S'Length < 10 then
         return Civil_Result.Error
           (Domain.Error.Validation_Error, "String too short for ISO 8601");
      end if;

      if S (S'First + 4) /= '-' or else S (S'First + 7) /= '-' then
         return Civil_Result.Error
           (Domain.Error.Validation_Error, "Invalid date format");
      end if;

      Year := Parse_Int (S (S'First .. S'First + 3));
      Month := Parse_Int (S (S'First + 5 .. S'First + 6));
      Day := Parse_Int (S (S'First + 8 .. S'First + 9));

      if Year < 1 or else Month < 1 or else Month > 12
         or else Day < 1 or else Day > 31
      then
         return Civil_Result.Error
           (Domain.Error.Validation_Error, "Invalid date values");
      end if;

      if T_Pos > 0 then
         declare
            Time_Str : constant String := S (T_Pos + 1 .. S'Last);
         begin
            if Time_Str'Length < 8 then
               return Civil_Result.Error
                 (Domain.Error.Validation_Error, "Time string too short");
            end if;

            if Time_Str (Time_Str'First + 2) /= ':'
               or else Time_Str (Time_Str'First + 5) /= ':'
            then
               return Civil_Result.Error
                 (Domain.Error.Validation_Error, "Invalid time format");
            end if;

            Hour := Parse_Int
              (Time_Str (Time_Str'First .. Time_Str'First + 1));
            Minute := Parse_Int
              (Time_Str (Time_Str'First + 3 .. Time_Str'First + 4));
            Second := Parse_Int
              (Time_Str (Time_Str'First + 6 .. Time_Str'First + 7));

            if Hour < 0 or else Hour > 23
               or else Minute < 0 or else Minute > 59
               or else Second < 0 or else Second > 59
            then
               return Civil_Result.Error
                 (Domain.Error.Validation_Error, "Invalid time values");
            end if;

            Dot_Pos := Index (Time_Str, '.');
            if Dot_Pos > 0 then
               declare
                  Nano_Start : constant Positive := Dot_Pos + 1;
                  Nano_End   : Natural := Time_Str'Last;
               begin
                  for I in Nano_Start .. Time_Str'Last loop
                     if not Is_Digit (Time_Str (I)) then
                        Nano_End := I - 1;
                        exit;
                     end if;
                  end loop;

                  if Nano_End >= Nano_Start then
                     Nanos := Parse_Nanos (Time_Str (Nano_Start .. Nano_End));
                  end if;
               end;
            end if;
         end;
      end if;

      return Domain.Value_Object.Civil.Create
        (Year       => Year_Number (Year),
         Month      => Month_Number (Month),
         Day        => Day_Number (Day),
         Hour       => Hour_Number (Hour),
         Minute     => Minute_Number (Minute),
         Second     => Second_Number (Second),
         Nanosecond => Nanos);
   end From_ISO_8601;

   function From_ISO_8601_With_Offset
     (S : String) return Civil_Offset_Result.Result
   is
      Offset_Pos : Natural := 0;
      Civil_End  : Natural := S'Last;
   begin
      for I in S'First + 10 .. S'Last loop
         if S (I) = '+' or else S (I) = '-' or else S (I) = 'Z' then
            Offset_Pos := I;
            Civil_End := I - 1;
            exit;
         elsif S (I) = '[' then
            Civil_End := I - 1;
            exit;
         end if;
      end loop;

      declare
         Civil_Res : constant Civil_Result.Result :=
           From_ISO_8601 (S (S'First .. Civil_End));
      begin
         if Civil_Result.Is_Error (Civil_Res) then
            return Civil_Offset_Result.Error
              (Civil_Result.Error_Info (Civil_Res).Kind,
               Domain.Error.Error_Strings.To_String
                 (Civil_Result.Error_Info (Civil_Res).Message));
         end if;

         if Offset_Pos = 0 then
            return Civil_Offset_Result.Ok
              ((Time       => Civil_Result.Value (Civil_Res),
                Offset     => Domain.Value_Object.Duration_Type.Zero,
                Has_Offset => False));
         end if;

         declare
            Offset_Res : constant Duration_Result.Result :=
              Parse_Offset (S (Offset_Pos .. S'Last));
         begin
            if Duration_Result.Is_Error (Offset_Res) then
               return Civil_Offset_Result.Error
                 (Domain.Error.Validation_Error, "Invalid offset format");
            end if;

            return Civil_Offset_Result.Ok
              ((Time       => Civil_Result.Value (Civil_Res),
                Offset     => Duration_Result.Value (Offset_Res),
                Has_Offset => True));
         end;
      end;
   end From_ISO_8601_With_Offset;

   function From_ISO_8601_With_Zone
     (S : String) return Civil_Zone_Result.Result
   is
      Zone_Start : constant Natural := Index (S, '[');
      Zone_End   : Natural := 0;
   begin
      if Zone_Start = 0 then
         return Civil_Zone_Result.Error
           (Domain.Error.Validation_Error, "No zone bracket found");
      end if;

      Zone_End := Index (S (Zone_Start .. S'Last), ']');
      if Zone_End = 0 then
         return Civil_Zone_Result.Error
           (Domain.Error.Validation_Error, "Missing closing bracket");
      end if;

      declare
         Civil_End : Natural := Zone_Start - 1;
      begin
         for I in reverse S'First + 10 .. Zone_Start - 1 loop
            if S (I) = '+' or else S (I) = '-' or else S (I) = 'Z' then
               Civil_End := I - 1;
               exit;
            end if;
         end loop;

         declare
            Civil_Res : constant Civil_Result.Result :=
              From_ISO_8601 (S (S'First .. Civil_End));
            Zone_Str  : constant String :=
              S (Zone_Start + 1 .. Zone_End - 1);
            Zone_Res  : constant Zone_ID_Result.Result :=
              Domain.Value_Object.Zone_ID.From_String (Zone_Str);
         begin
            if Civil_Result.Is_Error (Civil_Res) then
               return Civil_Zone_Result.Error
                 (Civil_Result.Error_Info (Civil_Res).Kind,
                  Domain.Error.Error_Strings.To_String
                    (Civil_Result.Error_Info (Civil_Res).Message));
            end if;

            if Zone_ID_Result.Is_Error (Zone_Res) then
               return Civil_Zone_Result.Error
                 (Domain.Error.Validation_Error, "Invalid zone ID");
            end if;

            return Civil_Zone_Result.Ok
              ((Time => Civil_Result.Value (Civil_Res),
                Zone => Zone_ID_Result.Value (Zone_Res)));
         end;
      end;
   end From_ISO_8601_With_Zone;

   function From_ISO_8601_Full (S : String) return Civil_Full_Result.Result is
      Zone_Start     : constant Natural := Index (S, '[');
      Offset_Pos     : Natural := 0;
      Civil_End      : Natural := S'Last;
      Has_Offset_Val : Boolean := False;
      Has_Zone_Val   : Boolean := False;
      Offset_Val     : Duration_Type :=
        Domain.Value_Object.Duration_Type.Zero;
      Zone_Val       : Zone_ID;
   begin
      if Zone_Start > 0 then
         Has_Zone_Val := True;
         declare
            Zone_End : constant Natural :=
              Index (S (Zone_Start .. S'Last), ']');
         begin
            if Zone_End > 0 then
               declare
                  Zone_Str : constant String :=
                    S (Zone_Start + 1 .. Zone_End - 1);
                  Zone_Res : constant Zone_ID_Result.Result :=
                    Domain.Value_Object.Zone_ID.From_String (Zone_Str);
               begin
                  if Zone_ID_Result.Is_Error (Zone_Res) then
                     return Civil_Full_Result.Error
                       (Domain.Error.Validation_Error, "Invalid zone ID");
                  end if;
                  Zone_Val := Zone_ID_Result.Value (Zone_Res);
               end;
            end if;
         end;
         Civil_End := Zone_Start - 1;
      end if;

      for I in S'First + 10 .. Civil_End loop
         if S (I) = '+' or else S (I) = '-' or else S (I) = 'Z' then
            Offset_Pos := I;
            exit;
         end if;
      end loop;

      if Offset_Pos > 0 then
         Has_Offset_Val := True;
         declare
            Offset_End : constant Natural := Civil_End;
            Offset_Res : constant Duration_Result.Result :=
              Parse_Offset (S (Offset_Pos .. Offset_End));
         begin
            if Duration_Result.Is_Error (Offset_Res) then
               return Civil_Full_Result.Error
                 (Domain.Error.Validation_Error, "Invalid offset");
            end if;
            Offset_Val := Duration_Result.Value (Offset_Res);
         end;
         Civil_End := Offset_Pos - 1;
      end if;

      declare
         Civil_Res : constant Civil_Result.Result :=
           From_ISO_8601 (S (S'First .. Civil_End));
      begin
         if Civil_Result.Is_Error (Civil_Res) then
            return Civil_Full_Result.Error
              (Civil_Result.Error_Info (Civil_Res).Kind,
               Domain.Error.Error_Strings.To_String
                 (Civil_Result.Error_Info (Civil_Res).Message));
         end if;

         return Civil_Full_Result.Ok
           ((Time       => Civil_Result.Value (Civil_Res),
             Offset     => Offset_Val,
             Zone       => Zone_Val,
             Has_Offset => Has_Offset_Val,
             Has_Zone   => Has_Zone_Val));
      end;
   end From_ISO_8601_Full;

   --  ========================================================================
   --  Date-Only Parsing
   --  ========================================================================

   function From_ISO_Date (S : String) return Civil_Result.Result is
   begin
      return From_ISO_8601 (S & "T00:00:00");
   end From_ISO_Date;

   --  ========================================================================
   --  Time-Only Parsing
   --  ========================================================================

   function From_ISO_Time (S : String) return Civil_Result.Result is
   begin
      return From_ISO_8601 ("1970-01-01T" & S);
   end From_ISO_Time;

   --  ========================================================================
   --  Duration Parsing
   --  ========================================================================

   function From_ISO_Duration (S : String) return Duration_Result.Result is
      Is_Negative : Boolean := False;
      Pos         : Positive := S'First;

      Days    : Int64 := 0;
      Hours   : Int64 := 0;
      Minutes : Int64 := 0;
      Seconds : Int64 := 0;
      Nanos   : Domain.Value_Object.Duration_Type.Nanoseconds_Range := 0;

      Num_Start : Natural := 0;
   begin
      if S'Length = 0 then
         return Duration_Result.Error
           (Domain.Error.Validation_Error, "Empty duration string");
      end if;

      if S (Pos) = '-' then
         Is_Negative := True;
         Pos := Pos + 1;
      end if;

      if Pos > S'Last or else S (Pos) /= 'P' then
         return Duration_Result.Error
           (Domain.Error.Validation_Error, "Duration must start with P");
      end if;
      Pos := Pos + 1;

      while Pos <= S'Last loop
         if S (Pos) = 'T' then
            --  Time section marker; just advance
            Pos := Pos + 1;
         elsif Is_Digit (S (Pos)) or else S (Pos) = '.' then
            if Num_Start = 0 then
               Num_Start := Pos;
            end if;
            Pos := Pos + 1;
         else
            if Num_Start = 0 then
               return Duration_Result.Error
                 (Domain.Error.Validation_Error, "Expected number");
            end if;

            declare
               Num_Str : constant String := S (Num_Start .. Pos - 1);
               Dot_Pos : constant Natural := Index (Num_Str, '.');
               Int_Part : Int64;
            begin
               if Dot_Pos > 0 then
                  Int_Part := Parse_Int64
                    (Num_Str (Num_Str'First .. Dot_Pos - 1));
                  if Dot_Pos < Num_Str'Last then
                     Nanos := Parse_Nanos
                       (Num_Str (Dot_Pos + 1 .. Num_Str'Last));
                  end if;
               else
                  Int_Part := Parse_Int64 (Num_Str);
               end if;

               case S (Pos) is
                  when 'D' => Days := Int_Part;
                  when 'H' => Hours := Int_Part;
                  when 'M' => Minutes := Int_Part;
                  when 'S' => Seconds := Int_Part;
                  when others =>
                     return Duration_Result.Error
                       (Domain.Error.Validation_Error, "Unknown unit");
               end case;
            end;

            Num_Start := 0;
            Pos := Pos + 1;
         end if;
      end loop;

      declare
         Total_Secs : constant Int64 :=
           Days * 86_400 + Hours * 3_600 + Minutes * 60 + Seconds;
         Result : Duration_Type :=
           Domain.Value_Object.Duration_Type.Create (Total_Secs, Nanos);
      begin
         if Is_Negative then
            Result := Domain.Value_Object.Duration_Type.Negate (Result);
         end if;

         return Duration_Result.Ok (Result);
      end;
   end From_ISO_Duration;

   function From_Human_Duration (S : String) return Duration_Result.Result is
      Is_Negative : Boolean := False;
      Pos         : Positive := S'First;

      Days    : Int64 := 0;
      Hours   : Int64 := 0;
      Minutes : Int64 := 0;
      Seconds : Int64 := 0;
      Nanos   : Domain.Value_Object.Duration_Type.Nanoseconds_Range := 0;

      Num_Start   : Natural := 0;
   begin
      if S'Length = 0 then
         return Duration_Result.Error
           (Domain.Error.Validation_Error, "Empty duration string");
      end if;

      if S (Pos) = '-' then
         Is_Negative := True;
         Pos := Pos + 1;
      end if;

      while Pos <= S'Last loop
         if S (Pos) = ' ' then
            Pos := Pos + 1;
         elsif Is_Digit (S (Pos)) or else S (Pos) = '.' then
            if Num_Start = 0 then
               Num_Start := Pos;
            end if;
            Pos := Pos + 1;
         else
            if Num_Start = 0 then
               Pos := Pos + 1;
            else
               declare
                  Num_Str : constant String := S (Num_Start .. Pos - 1);
                  Dot_Pos : constant Natural := Index (Num_Str, '.');
                  Int_Part : Int64;
               begin
                  if Dot_Pos > 0 then
                     Int_Part := Parse_Int64
                       (Num_Str (Num_Str'First .. Dot_Pos - 1));
                     if Dot_Pos < Num_Str'Last then
                        Nanos := Parse_Nanos
                          (Num_Str (Dot_Pos + 1 .. Num_Str'Last));
                     end if;
                  else
                     Int_Part := Parse_Int64 (Num_Str);
                  end if;

                  case S (Pos) is
                     when 'd' | 'D' => Days := Int_Part;
                     when 'h' | 'H' => Hours := Int_Part;
                     when 'm' | 'M' => Minutes := Int_Part;
                     when 's' | 'S' => Seconds := Int_Part;
                     when others => null;
                  end case;
               end;

               Num_Start := 0;
               Pos := Pos + 1;
            end if;
         end if;
      end loop;

      declare
         Total_Secs : constant Int64 :=
           Days * 86_400 + Hours * 3_600 + Minutes * 60 + Seconds;
         Result : Duration_Type :=
           Domain.Value_Object.Duration_Type.Create (Total_Secs, Nanos);
      begin
         if Is_Negative then
            Result := Domain.Value_Object.Duration_Type.Negate (Result);
         end if;

         return Duration_Result.Ok (Result);
      end;
   end From_Human_Duration;

   --  ========================================================================
   --  Offset Parsing
   --  ========================================================================

   function Parse_Offset (S : String) return Duration_Result.Result is
      Is_Negative : Boolean := False;
      Hours       : Integer := 0;
      Minutes     : Integer := 0;
   begin
      if S'Length = 0 then
         return Duration_Result.Error
           (Domain.Error.Validation_Error, "Empty offset");
      end if;

      if S = "Z" or else S = "z" then
         return Duration_Result.Ok (Domain.Value_Object.Duration_Type.Zero);
      end if;

      if S (S'First) = '-' then
         Is_Negative := True;
      elsif S (S'First) /= '+' then
         return Duration_Result.Error
           (Domain.Error.Validation_Error, "Invalid offset start");
      end if;

      declare
         Offset_Str : constant String := S (S'First + 1 .. S'Last);
         Colon_Pos  : constant Natural := Index (Offset_Str, ':');
      begin
         if Colon_Pos > 0 then
            Hours := Parse_Int
              (Offset_Str (Offset_Str'First .. Colon_Pos - 1));
            Minutes := Parse_Int
              (Offset_Str (Colon_Pos + 1 .. Offset_Str'Last));
         elsif Offset_Str'Length = 4 then
            Hours := Parse_Int
              (Offset_Str (Offset_Str'First .. Offset_Str'First + 1));
            Minutes := Parse_Int
              (Offset_Str (Offset_Str'First + 2 .. Offset_Str'Last));
         elsif Offset_Str'Length = 2 then
            Hours := Parse_Int (Offset_Str);
         else
            return Duration_Result.Error
              (Domain.Error.Validation_Error, "Invalid offset format");
         end if;
      end;

      if Hours < 0 or else Hours > 23
         or else Minutes < 0 or else Minutes > 59
      then
         return Duration_Result.Error
           (Domain.Error.Validation_Error, "Invalid offset values");
      end if;

      declare
         Total_Secs : Int64 := Int64 (Hours) * 3_600 + Int64 (Minutes) * 60;
      begin
         if Is_Negative then
            Total_Secs := -Total_Secs;
         end if;

         return Duration_Result.Ok
           (Domain.Value_Object.Duration_Type.From_Seconds (Total_Secs));
      end;
   end Parse_Offset;

end Zoneinfo.API.Parse;
