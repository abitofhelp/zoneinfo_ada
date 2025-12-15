pragma Ada_2022;
--  =========================================================================
--  Domain.Value_Object.Zoned - Implementation
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  =========================================================================

package body Domain.Value_Object.Zoned is

   --  ========================================================================
   --  Constructor
   --  ========================================================================

   function Create
     (Instant_Value : Instant;
      Zone          : Zone_ID) return Zoned
   is
   begin
      return (Instant_Value => Instant_Value, Zone => Zone);
   end Create;

   --  ========================================================================
   --  Accessors
   --  ========================================================================

   function To_Instant (Self : Zoned) return Instant is
   begin
      return Self.Instant_Value;
   end To_Instant;

   function Get_Zone (Self : Zoned) return Zone_ID is
   begin
      return Self.Zone;
   end Get_Zone;

   --  ========================================================================
   --  Timezone Change
   --  ========================================================================

   function With_Zone
     (Self     : Zoned;
      New_Zone : Zone_ID) return Zoned
   is
   begin
      return (Instant_Value => Self.Instant_Value, Zone => New_Zone);
   end With_Zone;

   --  ========================================================================
   --  Comparison
   --  ========================================================================

   overriding function "=" (Left, Right : Zoned) return Boolean is
      use Domain.Value_Object.Zone_ID;
   begin
      return Left.Instant_Value = Right.Instant_Value
             and then Left.Zone = Right.Zone;
   end "=";

   function "<" (Left, Right : Zoned) return Boolean is
   begin
      return Left.Instant_Value < Right.Instant_Value;
   end "<";

   function "<=" (Left, Right : Zoned) return Boolean is
   begin
      return Left.Instant_Value <= Right.Instant_Value;
   end "<=";

   function ">" (Left, Right : Zoned) return Boolean is
   begin
      return Left.Instant_Value > Right.Instant_Value;
   end ">";

   function ">=" (Left, Right : Zoned) return Boolean is
   begin
      return Left.Instant_Value >= Right.Instant_Value;
   end ">=";

end Domain.Value_Object.Zoned;
