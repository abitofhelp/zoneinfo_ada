pragma Ada_2022;
--  =========================================================================
--  Domain.Error - Implementation
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  =========================================================================

package body Domain.Error is

   function Create (Kind : Error_Kind; Message : String) return Error_Type is
   begin
      return
        (Kind    => Kind,
         Message => Error_Strings.To_Bounded_String (Message));
   end Create;

   function Get_Kind (E : Error_Type) return Error_Kind is
     (E.Kind);

   function Get_Message (E : Error_Type) return Error_Strings.Bounded_String is
     (E.Message);

   function Default_Error return Error_Type is
     ((Kind => Internal_Error, Message => Error_Strings.Null_Bounded_String));

end Domain.Error;
