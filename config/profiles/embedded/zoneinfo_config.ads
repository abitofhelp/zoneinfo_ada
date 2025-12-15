pragma Ada_2022;
--  ==========================================================================
--  Zoneinfo_Config - Embedded Profile
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Configuration profile for Ravenscar-compatible embedded systems.
--    Balanced configuration for memory-constrained devices.
--
--  Target Hardware:
--    - STM32F769 or similar (Cortex-M7 @ 200+ MHz)
--    - RAM: 512KB - 1MB
--    - Ravenscar runtime
--
--  Design Philosophy:
--    Conservative sizing with safety margins for IoT devices and
--    industrial control systems.
--  ==========================================================================

package Zoneinfo_Config is

   pragma Pure;

   --  =======================================================================
   --  Profile Metadata
   --  =======================================================================

   Profile_Name    : constant String := "embedded";
   Target_Platform : constant String := "Embedded (Ravenscar)";

   --  =======================================================================
   --  Build Profile (Alire standard)
   --  =======================================================================

   type Build_Profile_Kind is (release, validation, development);
   Build_Profile : constant Build_Profile_Kind := release;

   --  =======================================================================
   --  Bounded String Configuration
   --  =======================================================================

   --  Maximum length of person names
   --  Embedded profile: 64 characters (conservative for memory)
   Max_Name_Length : constant := 64;

   --  Maximum length of greeting messages
   --  Embedded profile: 128 characters
   Max_Message_Length : constant := 128;

   --  Maximum length of error messages
   --  Embedded profile: 256 characters
   Max_Error_Length : constant := 256;

   --  =======================================================================
   --  Discovery Configuration
   --  =======================================================================

   --  Maximum length of filesystem paths
   --  Embedded profile: 256 characters (shorter paths typical)
   Max_Path_Length : constant := 256;

   --  Maximum length of timezone database version strings
   --  Embedded profile: 16 characters
   Max_Version_Length : constant := 16;

   --  Maximum search paths for source discovery
   --  Embedded profile: 10 paths
   Max_Search_Paths : constant := 10;

   --  Maximum discovered timezone sources
   --  Embedded profile: 5 sources
   Max_Sources : constant := 5;

   --  Maximum zone IDs in a list result
   --  Embedded profile: 100 zones (subset for memory)
   Max_Zone_Ids : constant := 100;

   --  =======================================================================
   --  Runtime Configuration
   --  =======================================================================

   --  Enable assertions and contracts in this profile
   Enable_Contracts : constant Boolean := True;

   --  Disable debug output for production embedded
   Enable_Debug : constant Boolean := False;

end Zoneinfo_Config;
