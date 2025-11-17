pragma Ada_2022;
--  ===========================================================================
--  Domain.Value_Object.Dst_Info
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Dst Info value object - immutable domain data.
--
--  ===========================================================================

package body Domain.Value_Object.DST_Info is

   function Make_Unambiguous_Info
     (Offset : UTC_Offset_Type)
      return Ambiguity_Info_Type
   is
   begin
      return (Is_Ambiguous    => False,
              Is_Gap          => False,
              Earlier_Offset  => Offset,
              Later_Offset    => Offset,
              Post_Gap_Offset => Offset);
   end Make_Unambiguous_Info;

   function Make_Ambiguous_Info
     (Earlier_Offset : UTC_Offset_Type;
      Later_Offset   : UTC_Offset_Type)
      return Ambiguity_Info_Type
   is
   begin
      return (Is_Ambiguous    => True,
              Is_Gap          => False,
              Earlier_Offset  => Earlier_Offset,
              Later_Offset    => Later_Offset,
              Post_Gap_Offset => Later_Offset);
   end Make_Ambiguous_Info;

   function Make_Gap_Info
     (Pre_Gap_Offset  : UTC_Offset_Type;
      Post_Gap_Offset : UTC_Offset_Type)
      return Ambiguity_Info_Type
   is
   begin
      return (Is_Ambiguous    => False,
              Is_Gap          => True,
              Earlier_Offset  => Pre_Gap_Offset,
              Later_Offset    => Post_Gap_Offset,
              Post_Gap_Offset => Post_Gap_Offset);
   end Make_Gap_Info;

end Domain.Value_Object.DST_Info;
