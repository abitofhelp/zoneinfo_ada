pragma Ada_2022;
--  ==========================================================================
--  Zoneinfo_Config - Standard Profile
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Configuration profile for desktop/server environments.
--    Maximum flexibility, generous string limits.
--
--  Target Platform:
--    - Linux / macOS / Windows
--    - RAM: 1+ GB
--    - Full Ada runtime
--
--  Design Philosophy:
--    Generous limits suitable for desktop applications with no memory
--    pressure. All bounded strings sized for typical use cases plus margin.
--  ==========================================================================

package Zoneinfo_Config is

   pragma Pure;

   --  =======================================================================
   --  Profile Metadata
   --  =======================================================================

   Profile_Name    : constant String := "standard";
   Target_Platform : constant String := "Desktop/Server";

   --  =======================================================================
   --  Build Profile (Alire standard)
   --  =======================================================================

   type Build_Profile_Kind is (release, validation, development);
   Build_Profile : constant Build_Profile_Kind := development;

   --  =======================================================================
   --  Bounded String Configuration
   --  =======================================================================

   --  Maximum length of person names
   --  Standard profile: 128 characters (generous for international names)
   Max_Name_Length : constant := 128;

   --  Maximum length of greeting messages
   --  Standard profile: 256 characters
   Max_Message_Length : constant := 256;

   --  Maximum length of error messages
   --  Standard profile: 512 characters
   Max_Error_Length : constant := 512;

   --  =======================================================================
   --  Discovery Configuration
   --  =======================================================================

   --  Maximum length of filesystem paths
   --  POSIX PATH_MAX: typically 4096
   --  Standard profile: 4096 characters
   Max_Path_Length : constant := 4_096;

   --  Maximum length of timezone database version strings (e.g., "2024b")
   --  Standard profile: 32 characters
   Max_Version_Length : constant := 32;

   --  Maximum search paths for source discovery
   --  Typical usage: 1-10 paths
   --  Standard profile: 100 paths
   Max_Search_Paths : constant := 100;

   --  Maximum discovered timezone sources
   --  Typical system: 1-3 sources
   --  Standard profile: 100 sources
   Max_Sources : constant := 100;

   --  Maximum zone IDs in a list result
   --  IANA tzdata: ~600 zones
   --  Standard profile: 1000 zones
   Max_Zone_Ids : constant := 1_000;

   --  =======================================================================
   --  Runtime Configuration
   --  =======================================================================

   --  Enable assertions and contracts in this profile
   Enable_Contracts : constant Boolean := True;

   --  Enable debug output in this profile
   Enable_Debug : constant Boolean := True;

end Zoneinfo_Config;
