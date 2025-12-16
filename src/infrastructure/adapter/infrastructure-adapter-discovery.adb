pragma Ada_2022;
--  ===========================================================================
--  Infrastructure.Adapter.Discovery - Implementation
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Implements type conversion between Domain types and TZif types.
--    TZif is accessed via Zoneinfo.TZif_Lib alias to avoid name shadowing.
--    All public functions use Domain types; TZif is encapsulated here.
--
--  Architecture Notes:
--    - Type Conversion Layer: Domain ↔ TZif at adapter boundary
--    - TZif is PRIVATE to this body - not exposed in spec
--    - Converts results from TZif to Domain types before returning
--    - Returns bounded arrays for SPARK-compatible zone listing
--    - Returns Overflow_Error if zone count exceeds configured capacity
--
--  ===========================================================================

--  TZif library access (via alias to avoid name collision)
with Zoneinfo.TZif_Lib.API;
with Zoneinfo.TZif_Lib.Domain.Value_Object.Source_Info;
with Zoneinfo.TZif_Lib.Domain.Value_Object.Zone_Id;
with Zoneinfo.TZif_Lib.Domain.Error;
with Domain.Unit;

package body Infrastructure.Adapter.Discovery is

   --  ========================================================================
   --  TZif Package Renames (Private to this body)
   --  ========================================================================

   package TZif renames Zoneinfo.TZif_Lib.API;
   package TZif_Source renames
     Zoneinfo.TZif_Lib.Domain.Value_Object.Source_Info;
   package TZif_Zone renames Zoneinfo.TZif_Lib.Domain.Value_Object.Zone_Id;
   package TZif_Err renames Zoneinfo.TZif_Lib.Domain.Error;

   --  ========================================================================
   --  Type Conversion Helpers: Domain → TZif
   --  ========================================================================

   --  Convert Domain Path_String to TZif Discover_Port Path_String
   function To_TZif_Discover_Path
     (Domain_Path : Path_String) return TZif.Discover_Port.Path_String
   is
   begin
      return
        TZif.Discover_Port.Make_Path (Path_Strings.To_String (Domain_Path));
   end To_TZif_Discover_Path;

   --  Convert Domain Source_Info to TZif Source_Info_Type
   function To_TZif_Source
     (Domain_Source : Source_Info) return TZif_Source.Source_Info_Type
   is
      use Domain.Value_Object.Source_Info;
      ULID_Str    : constant String := To_String (Get_ULID (Domain_Source));
      Path_Str    : constant String :=
        Path_Strings.To_String (Get_Path (Domain_Source));
      Version_Str : constant String :=
        Version_Strings.To_String (Get_Version (Domain_Source));
   begin
      return
        TZif_Source.Make_Source_Info
          (ULID       => TZif_Source.Make_ULID (ULID_Str),
           Path       => TZif_Source.Make_Path (Path_Str),
           Version    => TZif_Source.Make_Version (Version_Str),
           Zone_Count => Get_Zone_Count (Domain_Source));
   end To_TZif_Source;

   --  ========================================================================
   --  Type Conversion Helpers: TZif → Domain
   --  ========================================================================

   --  Convert TZif Source_Info_Type to Domain Source_Info
   function To_Domain_Source
     (TZif_Src : TZif_Source.Source_Info_Type) return Source_Info
   is
      use Domain.Value_Object.Source_Info;
      ULID_Str    : constant String :=
        TZif_Source.To_String (TZif_Source.Get_ULID (TZif_Src));
      Path_Str    : constant String :=
        TZif_Source.To_String (TZif_Source.Get_Path (TZif_Src));
      Version_Str : constant String :=
        TZif_Source.To_String (TZif_Source.Get_Version (TZif_Src));
   begin
      return
        Make_Source_Info
          (ULID       => Make_ULID (ULID_Str),
           Path       => Make_Path (Path_Str),
           Version    => Make_Version (Version_Str),
           Zone_Count => TZif_Source.Get_Zone_Count (TZif_Src));
   end To_Domain_Source;

   --  Convert TZif Zone_Id_Type to Domain Zone_ID
   function To_Domain_Zone
     (TZif_Id : TZif_Zone.Zone_Id_Type) return Zone_ID
   is
   begin
      return
        Domain.Value_Object.Zone_ID.Make_Zone_ID
          (TZif_Zone.To_String (TZif_Id));
   end To_Domain_Zone;

   --  Convert TZif Get_Version_Port.Version_String to Domain Version_String
   function To_Domain_Version
     (TZif_Ver : TZif.Get_Version_Port.Version_String) return Version_String
   is
   begin
      return
        Domain.Value_Object.Source_Info.Make_Version
          (TZif.Get_Version_Port.Version_Strings.To_String (TZif_Ver));
   end To_Domain_Version;

   --  Map TZif Error_Kind to Domain Error_Kind
   function Map_Error_Kind
     (TZif_Kind : TZif_Err.Error_Kind) return Domain.Error.Error_Kind
   is
   begin
      case TZif_Kind is
         when TZif_Err.IO_Error =>
            return Domain.Error.IO_Error;
         when TZif_Err.Parse_Error =>
            return Domain.Error.Parse_Error;
         when TZif_Err.Validation_Error =>
            return Domain.Error.Validation_Error;
         when TZif_Err.Not_Found_Error =>
            return Domain.Error.Not_Found_Error;
         when TZif_Err.Resource_Error =>
            return Domain.Error.Internal_Error;
         when TZif_Err.Internal_Error =>
            return Domain.Error.Internal_Error;
      end case;
   end Map_Error_Kind;

   --  Convert TZif Error to Domain Error
   function To_Domain_Error (TZif_E : TZif_Err.Error_Type) return Error_Type is
      TZif_Kind : constant TZif_Err.Error_Kind := TZif_E.Kind;
      TZif_Msg  : constant String :=
        TZif_Err.Error_Strings.To_String (TZif_E.Message);
   begin
      return Domain.Error.Create (Map_Error_Kind (TZif_Kind), TZif_Msg);
   end To_Domain_Error;

   --  ========================================================================
   --  Source Management Operations
   --  ========================================================================

   function Discover_Sources
     (Search_Paths : Path_List) return Source_Info_Result.Result
   is
      --  Convert Domain paths to TZif paths using TZif's vector type
      TZif_Path_Vec    : TZif.Discover_Port.Path_List;
      Append_Result    : TZif.Discover_Port.Path_Vectors.Unit_Result;
   begin
      --  Add each path to the TZif path vector
      for I in Search_Paths'Range loop
         TZif.Discover_Port.Path_Vectors.Append
           (TZif_Path_Vec,
            To_TZif_Discover_Path (Search_Paths (I)),
            Append_Result);
         --  Ignore append failures (vector full) - continue with what we have
      end loop;

      declare
         TZif_Result : constant TZif.Discovery_Result :=
           TZif.Discover_Sources (TZif_Path_Vec);
      begin
         if TZif.Discover_Port.Discovery_Result_Package.Is_Ok (TZif_Result)
         then
            --  Get first source from discovered list
            declare
               Data : constant TZif.Discover_Port.Discovery_Data_Type :=
                 TZif.Discover_Port.Discovery_Result_Package.Value
                   (TZif_Result);
               Sources : constant TZif.Discover_Port.Source_Info_List :=
                 Data.Sources;
               use TZif.Discover_Port.Source_Info_Vectors;
            begin
               if Length (Sources) > 0 then
                  return
                    Source_Info_Result.Ok
                      (To_Domain_Source (Unchecked_Element (Sources, 1)));
               else
                  return
                    Source_Info_Result.Error
                      (Domain.Error.Not_Found_Error,
                       "No timezone sources found in search paths");
               end if;
            end;
         else
            return
              Source_Info_Result.From_Error
                (To_Domain_Error
                   (TZif.Discover_Port.Discovery_Result_Package.Error_Info
                      (TZif_Result)));
         end if;
      end;
   end Discover_Sources;

   function Load_Source
     (Path : Path_String) return Source_Info_Result.Result
   is
      TZif_Path   : constant TZif.Path_String :=
        TZif.Load_Port.Path_Strings.To_Bounded_String
          (Path_Strings.To_String (Path));
      TZif_Result : constant TZif.Load_Source_Result :=
        TZif.Load_Source (TZif_Path);
   begin
      if TZif.Load_Port.Load_Source_Result_Package.Is_Ok (TZif_Result) then
         declare
            Val : constant TZif_Source.Source_Info_Type :=
              TZif.Load_Port.Load_Source_Result_Package.Value (TZif_Result);
         begin
            return Source_Info_Result.Ok (To_Domain_Source (Val));
         end;
      else
         return
           Source_Info_Result.From_Error
             (To_Domain_Error
                (TZif.Load_Port.Load_Source_Result_Package.Error_Info
                   (TZif_Result)));
      end if;
   end Load_Source;

   function Validate_Source
     (Path : Path_String) return Unit_Result.Result
   is
      TZif_Path   : constant TZif.Validate_Path_String :=
        TZif.Validate_Port.Path_Strings.To_Bounded_String
          (Path_Strings.To_String (Path));
      TZif_Result : constant TZif.Validation_Result :=
        TZif.Validate_Source (TZif_Path);
   begin
      if TZif.Validate_Port.Validation_Result_Package.Is_Ok (TZif_Result) then
         return Unit_Result.Ok (Domain.Unit.Unit_Value);
      else
         return
           Unit_Result.From_Error
             (To_Domain_Error
                (TZif.Validate_Port.Validation_Result_Package.Error_Info
                   (TZif_Result)));
      end if;
   end Validate_Source;

   --  ========================================================================
   --  Timezone Query Operations
   --  ========================================================================

   function Find_My_Id return Zone_ID_Result.Result is
      TZif_Result : constant TZif.My_Zone_Result := TZif.Find_My_Id;
   begin
      if TZif.Is_Ok (TZif_Result) then
         return Zone_ID_Result.Ok (To_Domain_Zone (TZif.Value (TZif_Result)));
      else
         declare
            Err : constant TZif_Err.Error_Type :=
              TZif.Find_My_Id_Port.Result_Zone_Id.Error_Info (TZif_Result);
         begin
            return Zone_ID_Result.From_Error (To_Domain_Error (Err));
         end;
      end if;
   end Find_My_Id;

   function Get_Version
     (Source : Source_Info) return Version_Result.Result
   is
      TZif_Source_Val : constant TZif_Source.Source_Info_Type :=
        To_TZif_Source (Source);
      TZif_Result     : constant TZif.Version_Result :=
        TZif.Get_Version (TZif_Source_Val);
   begin
      if TZif.Get_Version_Port.Version_Result_Package.Is_Ok (TZif_Result) then
         return
           Version_Result.Ok
             (To_Domain_Version
                (TZif.Get_Version_Port.Version_Result_Package.Value
                   (TZif_Result)));
      else
         return
           Version_Result.From_Error
             (To_Domain_Error
                (TZif.Get_Version_Port.Version_Result_Package.Error_Info
                   (TZif_Result)));
      end if;
   end Get_Version;

   function List_All_Zones
     (Source     : Source_Info;
      Descending : Boolean := False) return Zone_List_Result.Result
   is
      use Domain.Value_Object.Zone_ID;
      TZif_Source_Val : constant TZif_Source.Source_Info_Type :=
        To_TZif_Source (Source);
      TZif_Result     : constant TZif.Zone_List_Result :=
        TZif.List_All_Zones (TZif_Source_Val, Descending);
   begin
      if TZif.List_Zones_Port.List_All_Zones_Result_Package.Is_Ok (TZif_Result)
      then
         declare
            Zones : constant TZif.List_Zones_Port.Zone_Id_List :=
              TZif.List_Zones_Port.List_All_Zones_Result_Package.Value
                (TZif_Result);
            Zone_Count : constant Natural :=
              TZif.List_Zones_Port.Zone_Id_Vectors.Length (Zones);
         begin
            --  Check capacity before collecting
            if Zone_Count > Max_Zone_List_Size then
               return
                 Zone_List_Result.Error
                   (Domain.Error.Overflow_Error,
                    "Zone count" & Zone_Count'Image &
                    " exceeds Max_Zone_List_Size" &
                    Max_Zone_List_Size'Image &
                    "; increase Zoneinfo_Config.Max_Zone_List_Size");
            end if;

            --  Collect zones into bounded array
            declare
               Result_List : Zone_List (Count => Zone_Count);
            begin
               for I in 1 .. Zone_Count loop
                  Result_List.Items (I) :=
                    To_Domain_Zone
                      (TZif.List_Zones_Port.Zone_Id_Vectors.Unchecked_Element
                         (Zones, I));
               end loop;
               return Zone_List_Result.Ok (Result_List);
            end;
         end;
      else
         return
           Zone_List_Result.From_Error
             (To_Domain_Error
                (TZif.List_Zones_Port.List_All_Zones_Result_Package.Error_Info
                   (TZif_Result)));
      end if;
   end List_All_Zones;

   --  ========================================================================
   --  Pattern-Based Search Operations
   --  ========================================================================

   function Find_By_Pattern
     (Pattern : String) return Search_Results_Result.Result
   is
      use Domain.Value_Object.Zone_ID;

      --  Accumulator for callback collection
      Results  : Search_Results (Count => Max_Search_Results);
      Count    : Natural := 0;
      Overflow : Boolean := False;

      --  Callback to collect zones into array
      procedure Collect_Zone (Name : TZif.Find_Pattern_Port.Zone_Name_String)
      is
         Zone_Str : constant String :=
           TZif.Find_Pattern_Port.Zone_Name_Strings.To_String (Name);
      begin
         if Count >= Max_Search_Results then
            Overflow := True;
         else
            Count := Count + 1;
            Results.Items (Count) := Make_Zone_ID (Zone_Str);
         end if;
      end Collect_Zone;

      TZif_Pattern : constant TZif.Pattern_String :=
        TZif.Find_Pattern_Port.Pattern_Strings.To_Bounded_String (Pattern);
      TZif_Result  : TZif.Pattern_Result;
   begin
      TZif_Result := TZif.Find_By_Pattern
        (TZif_Pattern, Collect_Zone'Unrestricted_Access);

      if not TZif.Find_Pattern_Port.Find_By_Pattern_Result_Package.Is_Ok
               (TZif_Result)
      then
         declare
            Err : constant TZif_Err.Error_Type :=
              TZif.Find_Pattern_Port.Find_By_Pattern_Result_Package.Error_Info
                (TZif_Result);
         begin
            return Search_Results_Result.From_Error (To_Domain_Error (Err));
         end;
      end if;

      if Overflow then
         return
           Search_Results_Result.Error
             (Domain.Error.Overflow_Error,
              "Pattern '" & Pattern &
              "' matched more than Max_Search_Results" &
              Max_Search_Results'Image &
              "; refine pattern or increase Zoneinfo_Config.Max_Search_Results"
             );
      end if;

      --  Return with actual count
      return Search_Results_Result.Ok
        ((Count => Count, Items => Results.Items));
   end Find_By_Pattern;

   function Find_By_Region
     (Region : String) return Search_Results_Result.Result
   is
      use Domain.Value_Object.Zone_ID;

      --  Accumulator state for callback collection
      Count    : Natural := 0;
      Overflow : Boolean := False;
      Results  : Search_Results (Count => Max_Search_Results);

      --  Callback to collect zones into bounded array
      procedure Collect_Zone (Name : TZif.Find_Region_Port.Zone_Name_String)
      is
         Zone_Str : constant String :=
           TZif.Find_Region_Port.Zone_Name_Strings.To_String (Name);
      begin
         if Count >= Max_Search_Results then
            Overflow := True;
         else
            Count := Count + 1;
            Results.Items (Count) := Make_Zone_ID (Zone_Str);
         end if;
      end Collect_Zone;

      TZif_Region : constant TZif.Region_String :=
        TZif.Find_Region_Port.Region_Strings.To_Bounded_String (Region);
      TZif_Result : TZif.Region_Result;
   begin
      TZif_Result :=
        TZif.Find_By_Region
          (TZif_Region, Collect_Zone'Unrestricted_Access);

      if not TZif.Find_Region_Port.Find_By_Region_Result_Package.Is_Ok
               (TZif_Result)
      then
         declare
            Err : constant TZif_Err.Error_Type :=
              TZif.Find_Region_Port.Find_By_Region_Result_Package.Error_Info
                (TZif_Result);
         begin
            return Search_Results_Result.From_Error (To_Domain_Error (Err));
         end;
      end if;

      if Overflow then
         return
           Search_Results_Result.Error
             (Domain.Error.Overflow_Error,
              "Region '" & Region &
              "' matched more than Max_Search_Results" &
              Max_Search_Results'Image &
              "; refine query or increase Zoneinfo_Config.Max_Search_Results");
      end if;

      return Search_Results_Result.Ok
        ((Count => Count, Items => Results.Items));
   end Find_By_Region;

   function Find_By_Regex
     (Regex : String) return Search_Results_Result.Result
   is
      use Domain.Value_Object.Zone_ID;

      --  Accumulator state for callback collection
      Count    : Natural := 0;
      Overflow : Boolean := False;
      Results  : Search_Results (Count => Max_Search_Results);

      --  Callback to collect zones into bounded array
      procedure Collect_Zone (Name : TZif.Find_Regex_Port.Zone_Name_String)
      is
         Zone_Str : constant String :=
           TZif.Find_Regex_Port.Zone_Name_Strings.To_String (Name);
      begin
         if Count >= Max_Search_Results then
            Overflow := True;
         else
            Count := Count + 1;
            Results.Items (Count) := Make_Zone_ID (Zone_Str);
         end if;
      end Collect_Zone;

      TZif_Regex  : constant TZif.Regex_String :=
        TZif.Find_Regex_Port.Regex_Strings.To_Bounded_String (Regex);
      TZif_Result : TZif.Regex_Result;
   begin
      TZif_Result :=
        TZif.Find_By_Regex (TZif_Regex, Collect_Zone'Unrestricted_Access);

      if not TZif.Find_Regex_Port.Find_By_Regex_Result_Package.Is_Ok
               (TZif_Result)
      then
         return
           Search_Results_Result.From_Error
             (To_Domain_Error
                (TZif.Find_Regex_Port.Find_By_Regex_Result_Package.Error_Info
                   (TZif_Result)));
      end if;

      if Overflow then
         return
           Search_Results_Result.Error
             (Domain.Error.Overflow_Error,
              "Regex '" & Regex &
              "' matched more than Max_Search_Results" &
              Max_Search_Results'Image &
              "; refine pattern or increase Zoneinfo_Config.Max_Search_Results"
             );
      end if;

      return Search_Results_Result.Ok
        ((Count => Count, Items => Results.Items));
   end Find_By_Regex;

end Infrastructure.Adapter.Discovery;
