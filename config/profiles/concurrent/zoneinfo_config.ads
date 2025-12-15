pragma Ada_2022;
--  ==========================================================================
--  Zoneinfo_Config - Concurrent Profile
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Configuration profile for multi-threaded applications.
--    Optimized for concurrent access patterns on multi-core systems.
--
--  Target Platform:
--    - Linux / macOS / Windows (multi-core)
--    - RAM: 1+ GB
--    - Full Ada runtime with tasking
--
--  Design Philosophy:
--    Generous limits for high-throughput concurrent servers.
--    Same sizing as standard profile but designed for tasking.
--  ==========================================================================

package Zoneinfo_Config is

   pragma Pure;

   --  =======================================================================
   --  Profile Metadata
   --  =======================================================================

   Profile_Name    : constant String := "concurrent";
   Target_Platform : constant String := "Multi-threaded Server";

   --  =======================================================================
   --  Build Profile (Alire standard)
   --  =======================================================================

   type Build_Profile_Kind is (release, validation, development);
   Build_Profile : constant Build_Profile_Kind := development;

   --  =======================================================================
   --  Bounded String Configuration
   --  =======================================================================

   --  Maximum length of person names
   --  Concurrent profile: 128 characters (generous for international names)
   Max_Name_Length : constant := 128;

   --  Maximum length of greeting messages
   --  Concurrent profile: 256 characters
   Max_Message_Length : constant := 256;

   --  Maximum length of error messages
   --  Concurrent profile: 512 characters
   Max_Error_Length : constant := 512;

   --  =======================================================================
   --  Discovery Configuration
   --  =======================================================================

   --  Maximum length of filesystem paths
   --  Concurrent profile: 4096 characters
   Max_Path_Length : constant := 4_096;

   --  Maximum length of timezone database version strings
   --  Concurrent profile: 32 characters
   Max_Version_Length : constant := 32;

   --  Maximum search paths for source discovery
   --  Concurrent profile: 100 paths
   Max_Search_Paths : constant := 100;

   --  Maximum discovered timezone sources
   --  Concurrent profile: 100 sources
   Max_Sources : constant := 100;

   --  Maximum zone IDs in a list result
   --  Concurrent profile: 1000 zones
   Max_Zone_Ids : constant := 1_000;

   --  =======================================================================
   --  Runtime Configuration
   --  =======================================================================

   --  Enable assertions and contracts in this profile
   Enable_Contracts : constant Boolean := True;

   --  Enable debug output in this profile
   Enable_Debug : constant Boolean := True;

end Zoneinfo_Config;
