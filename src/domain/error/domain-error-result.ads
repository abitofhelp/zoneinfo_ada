pragma Ada_2022;
--  ===========================================================================
--  Domain.Error.Result
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Result interface and type definitions.
--
--  Key Types:
--    T
--    T
--    Result
--    Result_State
--    Result
--
--  Dependencies:
--    Inline
--    Inline
--    Inline
--
--  ===========================================================================

package Domain.Error.Result is

   pragma Preelaborate;

   --  ========================================================================
   --  Generic Result Type: Either monad for T or Error_Type
   --  ========================================================================

   --  This generic package must be instantiated for each success type T
   --  Example: package String_Result is new Generic_Result (T => String);
   --
   --  Flow:
   --  1. Operations that can fail return Result[T] instead of raising
   --  2. Caller checks Is_Ok/Is_Error before extracting value
   --  3. Forces explicit error handling at compile time

   generic
      type T is private;  --  The success value type
   package Generic_Result is

      --  Opaque result type - internal representation hidden
      type Result is private;

      --  Constructors
      function Ok (Value : T) return Result
      with Inline;

      function Error (Kind : Error_Kind; Message : String) return Result
      with Inline;

      --  Query functions
      function Is_Ok (Self : Result) return Boolean
      with Inline;

      function Is_Error (Self : Result) return Boolean
      with Inline;

      --  Value extraction (caller must check Is_Ok/Is_Error first)
      function Value (Self : Result) return T
      with Pre => Is_Ok (Self), Inline;

      function Error_Info (Self : Result) return Error_Type
      with Pre => Is_Error (Self), Inline;

      --  =====================================================================
      --  Combinators (Railway-Oriented Programming)
      --  =====================================================================

      --  Same-type bind: T -> Result[T]
      --  Applies F only when Self is Ok; otherwise propagates error
      function And_Then
        (Self : Result;
         F    : not null access function (X : T) return Result)
         return Result
      with Inline;

      --  Note: Map and And_Then_To for type transformation require
      --  instantiation at the call site with both Result types.
      --  See Infrastructure layer for usage examples.

      --  Map_Error: transform the error value (E -> E)
      generic
         with function G (E : Error_Type) return Error_Type;
      function Map_Error (Self : Result) return Result;

      --  With_Context: enrich the error with a location/breadcrumb
      --  Requires a domain function to append context to errors
      generic
         with function Add (E : Error_Type; Where : String) return Error_Type;
      function With_Context (Self : Result; Where : String) return Result;

   private

      --  Internal representation: discriminated record (tagged union pattern)
      type Result_State is (Ok_State, Error_State);

      type Result (State : Result_State := Error_State) is record
         case State is
            when Ok_State =>
               Success_Value : T;

            when Error_State =>
               Error_Value : Error_Type;
         end case;
      end record;

   end Generic_Result;

end Domain.Error.Result;
