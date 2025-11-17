pragma Ada_2022;
--  ===========================================================================
--  Zoneinfo.Infrastructure.Zoneinfo_Parser
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    StarterLib binary format parser implementation.
--
--  Supported Versions:
--    - StarterLib version 1 (legacy)
--    - StarterLib version 2 (64-bit)
--    - StarterLib version 3 (with extensions)
--
--  Key Types:
--    Parse_Result_Type
--
--  Dependencies:
--    ZoneInfo.Domain.Error.Result
--    ZoneInfo.Domain.StarterLib_Data
--
--  ===========================================================================

with Ada.Streams.Stream_IO;
with ZoneInfo.Domain.Error.Result;
with ZoneInfo.Domain.StarterLib_Data;

package ZoneInfo.Infrastructure.StarterLib_Parser is

   use ZoneInfo.Domain.StarterLib_Data;

   --  ========================================================================
   --  Result Type
   --  ========================================================================

   --  Result type for parse operations using domain Result monad
   package Parse_Result is new
     Domain.Error.Result.Generic_Result (T => StarterLib_Data_Type);

   subtype Parse_Result_Type is Parse_Result.Result;

   --  ========================================================================
   --  Parser Functions
   --  ========================================================================

   --  Parse ZoneInfo file from an open stream
   --  Returns Result containing either parsed data or error
   function Parse_From_Stream
     (Stream : not null Ada.Streams.Stream_IO.Stream_Access)
      return Parse_Result_Type;

   --  Parse ZoneInfo file from file path
   --  Opens file, parses, and closes file
   function Parse_From_File (File_Path : String) return Parse_Result_Type;

end ZoneInfo.Infrastructure.StarterLib_Parser;
