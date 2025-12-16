pragma Ada_2022;
--  =========================================================================
--  Domain.Error - Error handling types and utilities
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Public facade for domain error handling. Defines error types used
--    throughout the zoneinfo library for consistent error reporting.
--    Includes zoneinfo-specific error kinds for datetime operations.
--
--  Architecture Notes:
--    - Part of the DOMAIN layer (innermost, zero dependencies)
--    - Error types are concrete (not generic) for consistency
--    - Used with Domain.Error.Result monad for functional error handling
--    - All errors use bounded strings (no heap allocation)
--
--  Usage:
--    with Domain.Error;           -- Gets error types
--    with Domain.Error.Result;    -- Gets Result[T] monad
--
--    use Domain.Error;  -- Makes Error_Kind, Error_Type visible
--
--  Design Pattern:
--    Domain error types as building blocks for Result monad:
--    - Error_Kind: Enumeration of error categories
--    - Error_Type: Record containing kind + message
--    - Result[T]: Either monad that wraps T or Error_Type
--
--  See Also:
--    Domain.Error.Result - Generic Result monad using these error types
--  =========================================================================

with Ada.Strings.Bounded;
with Zoneinfo_Config;

package Domain.Error
  with Preelaborate, SPARK_Mode => On
is

   --  ========================================================================
   --  Error String Type
   --  ========================================================================

   --  Using bounded string for error messages (memory safe, no heap)
   --  Size configured per profile in Zoneinfo_Config
   package Error_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length
       (Max => Zoneinfo_Config.Max_Error_Length);

   --  ========================================================================
   --  Error Kind Enumeration
   --  ========================================================================

   --  Categories of errors that can occur in the application
   --  Zoneinfo-specific error kinds for datetime operations
   type Error_Kind is
     (Validation_Error,       --  Domain validation failures (invalid input)
      Parse_Error,            --  Malformed data (corrupted TZif, bad magic)
      Not_Found_Error,        --  Resource not found (file, zone, types)
      Configuration_Error,    --  Invalid configuration or setup
      Timezone_Error,         --  Invalid timezone identifier or lookup failure
      Overflow_Error,         --  Arithmetic overflow in datetime calculations
      Ambiguous_Time_Error,   --  Civil time has multiple instants (fall-back)
      Gap_Time_Error,         --  Civil time doesn't exist (spring-forward)
      IO_Error,               --  I/O operations (file, network, clock access)
      Internal_Error);        --  Unexpected internal errors (catch-all)

   --  ========================================================================
   --  Error Type Record
   --  ========================================================================

   --  Concrete error type used throughout the application
   --  Combines error category with descriptive message
   type Error_Type is record
      Kind    : Error_Kind;
      Message : Error_Strings.Bounded_String;
   end record;

   --  ========================================================================
   --  Constructors and Accessors
   --  ========================================================================

   --  Create an error with the given kind and message
   function Create (Kind : Error_Kind; Message : String) return Error_Type
   with Inline;

   --  Get the error kind
   function Get_Kind (E : Error_Type) return Error_Kind
   with Inline;

   --  Get the error message as bounded string
   function Get_Message (E : Error_Type) return Error_Strings.Bounded_String
   with Inline;

   --  Default value (for SPARK-compatible bounded containers)
   function Default_Error return Error_Type
   with Inline;

end Domain.Error;
