pragma Ada_2022;
--  ===========================================================================
--  Domain.Error
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Error interface and type definitions.
--
--  Key Types:
--    Error_Kind
--    Error_Type
--
--  ===========================================================================

with Ada.Strings.Bounded;

package Domain.Error is

   pragma Preelaborate;

   --  ========================================================================
   --  Error String Type
   --  ========================================================================

   --  Using bounded string for error messages (memory safe, no heap)
   package Error_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 512);

   --  ========================================================================
   --  Error Kind Enumeration
   --  ========================================================================

   --  Categories of errors that can occur in the application
   type Error_Kind is
     (Validation_Error,      --  Domain validation failures (invalid input)
      Time_Out_Of_Range,     --  Time value outside valid range
      Conversion_Error,      --  Failed to convert between representations
      Ambiguous_Time,        --  Time is ambiguous (occurs twice during DST fall-back)
      Infrastructure_Error,  --  Infrastructure failures (I/O, file not found)
      Resource_Error,        --  Resource exhaustion (out of memory, stack overflow)
      Internal_Error);       --  Internal errors (precondition violations, shouldn't happen)

   --  ========================================================================
   --  Error Type Record
   --  ========================================================================

   --  Concrete error type used throughout the application
   --  Combines error category with descriptive message
   type Error_Type is record
      Kind    : Error_Kind;
      Message : Error_Strings.Bounded_String;
   end record;

end Domain.Error;
