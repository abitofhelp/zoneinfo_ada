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

   --  Maximum length of error messages
   --  Standard profile: 512 characters
   Max_Error_Length : constant := 512;

   --  Maximum length of IANA timezone identifiers (e.g., "America/New_York")
   --  Longest known: ~30 characters; 64 provides margin
   Max_Zone_ID_Length : constant := 64;

   --  Maximum length of formatted datetime strings (ISO 8601 with zone)
   --  Example: "2025-12-15T14:30:00.123456789-05:00[America/New_York]"
   Max_Datetime_Length : constant := 96;

   --  Maximum length of formatted duration strings (ISO 8601)
   --  Example: "P1Y2M3DT4H5M6.789S"
   Max_Duration_Length : constant := 48;

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

end Zoneinfo_Config;
