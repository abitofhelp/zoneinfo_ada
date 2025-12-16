pragma Ada_2022;
--  ===========================================================================
--  Application.Usecase.Discovery - Implementation
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Implementation of Discovery use case operations. Delegates all
--    operations to the injected port functions.
--
--  ===========================================================================

package body Application.Usecase.Discovery
  with SPARK_Mode => On
is

   --  ========================================================================
   --  Source Management Operations
   --  ========================================================================

   function Discover_Sources
     (Search_Paths : Path_List) return Source_Info_Result.Result
   is
   begin
      return Port_Discover_Sources (Search_Paths);
   end Discover_Sources;

   function Load_Source
     (Path : Path_String) return Source_Info_Result.Result
   is
   begin
      return Port_Load_Source (Path);
   end Load_Source;

   function Validate_Source
     (Path : Path_String) return Unit_Result.Result
   is
   begin
      return Port_Validate_Source (Path);
   end Validate_Source;

   --  ========================================================================
   --  Timezone Query Operations
   --  ========================================================================

   function Find_My_Id return Zone_ID_Result.Result is
   begin
      return Port_Find_My_Id;
   end Find_My_Id;

   function Get_Version
     (Source : Source_Info) return Version_Result.Result
   is
   begin
      return Port_Get_Version (Source);
   end Get_Version;

   function List_All_Zones
     (Source     : Source_Info;
      Descending : Boolean := False) return Zone_List_Result.Result
   is
   begin
      return Port_List_All_Zones (Source, Descending);
   end List_All_Zones;

   --  ========================================================================
   --  Pattern-Based Search Operations
   --  ========================================================================

   function Find_By_Pattern
     (Pattern : String) return Search_Results_Result.Result
   is
   begin
      return Port_Find_By_Pattern (Pattern);
   end Find_By_Pattern;

   function Find_By_Region
     (Region : String) return Search_Results_Result.Result
   is
   begin
      return Port_Find_By_Region (Region);
   end Find_By_Region;

   function Find_By_Regex
     (Regex : String) return Search_Results_Result.Result
   is
   begin
      return Port_Find_By_Regex (Regex);
   end Find_By_Regex;

end Application.Usecase.Discovery;
