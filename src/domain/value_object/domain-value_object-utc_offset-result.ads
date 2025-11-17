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
--  Responsibilities:
--    - Define Result type and operations
--    - Provide constructors and accessors
--    - Validate domain constraints
--
--  Key Types:
--    Result
--
--  Dependencies:
--    Pre => Hours in -14 .. 14 and Minutes in -59 .. 59
--
--  ===========================================================================

with Domain.Error;
with Domain.Error.Result;

package Domain.Value_Object.UTC_Offset.Result is

   pragma Elaborate_Body;

   --  ========================================================================
   --  Specialized Result Type for UTC_Offset
   --  ========================================================================

   package Impl is new Domain.Error.Result.Generic_Result
     (T => Domain.Value_Object.UTC_Offset.UTC_Offset_Type);

   --  Public alias for the specialized Result type
   subtype Result is Impl.Result;

   --  Re-export core Result operations for convenience

   function Ok
     (Value : Domain.Value_Object.UTC_Offset.UTC_Offset_Type)
      return Result
   renames Impl.Ok;

   function Error
     (Kind    : Domain.Error.Error_Kind;
      Message : String)
      return Result
   renames Impl.Error;

   function Is_Ok (Self : Result) return Boolean
     renames Impl.Is_Ok;

   function Is_Error (Self : Result) return Boolean
     renames Impl.Is_Error;

   function Value (Self : Result)
      return Domain.Value_Object.UTC_Offset.UTC_Offset_Type
   renames Impl.Value;

   function Error_Info (Self : Result)
      return Domain.Error.Error_Type
   renames Impl.Error_Info;

   --  ========================================================================
   --  Smart Constructors
   --  ========================================================================

   --  Create from total seconds
   function From_Seconds (Seconds : Integer) return Result;

   --  Create from hours and minutes
   --  Hours: -14..14, Minutes: -59..59
   function From_Hours_Minutes (Hours : Integer; Minutes : Integer := 0)
                                return Result
   with Pre => Hours in -14 .. 14 and Minutes in -59 .. 59;

   --  Create from ISO 8601 string format (e.g., "-05:00", "+05:30", "Z")
   --  "Z" is interpreted as UTC (0 offset)
   function From_String (Offset_Str : String) return Result;

end Domain.Value_Object.UTC_Offset.Result;
