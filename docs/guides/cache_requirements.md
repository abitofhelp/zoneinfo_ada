# Cache Import/Export Requirements

**Version:** 1.0.0-rc2  
**Date:** November 16, 2025  
**SPDX-License-Identifier:** BSD-3-Clause  
**License File:** See the LICENSE file in the project root.  
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Unreleased  


**Status**: In Progress
**Format Decision**: JSON (initial implementation)
**Performance Target**: Measure load times with ~600 timezone files, optimize to binary if needed

---

## Design Decisions

### JSON vs Binary Format

**Decision**: Use JSON format initially
- **Rationale**:
  - Human-readable for debugging and disaster recovery
  - Can inspect/edit cache files with standard tools (jq, text editors)
  - Performance measured with realistic data (~600 zones) before considering binary
  - Cache can always be regenerated from ZoneInfo files if corrupted
  - Easier to implement and debug during initial development

**Performance Target**:
- Measure import time with full cache (~600 zones)
- If load time > 100ms, consider switching to binary format
- If load time acceptable, keep JSON for maintainability

---

## Requirements

### REQ-CACHE-001: Cache Architecture
The system SHALL maintain two separate in-memory caches:
- **Source_Cache**: Maps ULID → Source_Info
  - Stores metadata about discovered timezone sources (directories)
  - Key: ULID (26-char unique identifier)
  - Value: Source_Info (path, version, zone_count)

- **Zone_Cache**: Maps (ULID, Zone_Id) → StarterLib_Data
  - Stores parsed timezone data
  - Key: Composite of Source ULID + Zone ID (e.g., "America/Los_Angeles")
  - Value: Parsed StarterLib_Data (transitions, types, leap seconds, POSIX TZ)

**Rationale**: Separating source metadata from zone data allows:
- Efficient source validation without loading all zone data
- Tracking which zones came from which sources
- Removing invalid sources and their zones together

---

### REQ-CACHE-002: Export Format Structure
Export file SHALL use JSON format with the following structure:

```json
{
  "header": {
    "magic": "ZONEINFO_CACHE",
    "version": 1,
    "created_at": "2025-01-09T12:34:56Z",
    "platform": "darwin",
    "library_version": "0.1.0"
  },
  "sources": [
    {
      "ulid": "01HQZX3J7K9M2N4P5Q6R7S8T9V",
      "path": "/usr/share/zoneinfo",
      "version": "2024b",
      "zone_count": 600
    }
  ],
  "zones": [
    {
      "source_ulid": "01HQZX3J7K9M2N4P5Q6R7S8T9V",
      "zone_id": "America/Los_Angeles",
      "starterlib_data": { /* parsed data */ }
    }
  ]
}
```

**Fields**:
- `magic`: String literal "ZONEINFO_CACHE" for file type identification
- `version`: Integer format version (start at 1)
- `created_at`: ISO 8601 timestamp
- `platform`: OS identifier (darwin/linux/windows)
- `library_version`: ZoneInfo library version that created the cache

---

### REQ-CACHE-003: Export Data Content

**Source Information** - For each source, export SHALL include:
- `ulid`: 26-character ULID (unique identifier)
- `path`: Absolute filesystem path to zoneinfo directory
- `version`: tzdata version string (e.g., "2024b")
- `zone_count`: Number of zones discovered in this source

**Zone Information** - For each cached zone, export SHALL include:
- `source_ulid`: Which source this zone came from
- `zone_id`: Timezone identifier (e.g., "America/Los_Angeles")
- `starterlib_data`: Full parsed ZoneInfo data structure including:
  - Transitions array (timestamp + type index pairs)
  - Timezone types (UTC offset, DST flag, abbreviation)
  - Leap seconds (if present)
  - POSIX TZ string (if present)
  - Header metadata

**TBD**: Exact JSON schema for StarterLib_Data serialization
- Need to determine how to represent Ada types in JSON
- Need to handle bounded strings, vectors, discriminated records

---

### REQ-CACHE-004: Export Function

```ada
function Export_Cache
  (Path      : Path_String;
   Overwrite : Boolean := False)
  return Export_Cache_Result;
```

**Behavior**:
1. Collect all Source_Info from Source_Cache
2. Collect all (ULID, Zone_Id, StarterLib_Data) from Zone_Cache
3. Serialize to JSON format per REQ-CACHE-002
4. Write to file at `Path`
5. Return Export_Stats_Type with counts

**Return Value**: `Result[Export_Stats_Type]`
```ada
type Export_Stats_Type is record
   Sources_Exported : Natural := 0;
   Zones_Exported   : Natural := 0;
end record;
```

**Error Cases**:
- `Err(FileExists)` - File exists and Overwrite = False
- `Err(NotWritable)` - Insufficient permissions
- `Err(IOError)` - Filesystem error during write
- `Err(SerializationError)` - Failed to convert data to JSON

---

### REQ-CACHE-005: Import Validation

Import operation SHALL validate:

1. **File Existence**: Cache file exists at specified path
2. **JSON Validity**: File contains valid JSON
3. **Magic Header**: `header.magic` equals "ZONEINFO_CACHE"
4. **Version Compatibility**: `header.version` is supported (currently: version 1)
5. **Source Paths**: Each source's filesystem path still exists
6. **Data Integrity**: All required fields present and correctly typed

**Source Validation Process**:
- For each source in cache file:
  - Check if `path` exists on current filesystem
  - If path missing: Mark source as REMOVED
  - If path exists: Mark source as VALID
- Only load zones from VALID sources
- Report removed source count in Import_Stats

**Rationale**: Source paths may change between export and import due to:
- OS reinstall or upgrade
- Filesystem reorganization
- Moving cache file between machines
- Unmounted volumes

---

### REQ-CACHE-006: Import Function

```ada
function Import_Cache
  (Path : Path_String)
  return Import_Cache_Result;
```

**Behavior**:
1. Read JSON file from `Path`
2. Parse and validate JSON structure
3. Validate header (magic, version)
4. Validate each source's filesystem path
5. Deserialize valid sources into Source_Cache
6. Deserialize zones (only from valid sources) into Zone_Cache
7. Return Import_Stats_Type with counts

**Return Value**: `Result[Import_Stats_Type]`
```ada
type Import_Stats_Type is record
   Sources_Loaded  : Natural := 0;  -- Successfully loaded
   Zones_Loaded    : Natural := 0;  -- Successfully loaded
   Sources_Removed : Natural := 0;  -- Paths no longer exist
end record;
```

**Error Cases**:
- `Err(NotFound)` - Cache file doesn't exist
- `Err(InvalidFormat)` - JSON parse error or wrong structure
- `Err(VersionMismatch)` - Unsupported cache format version
- `Err(IOError)` - Filesystem error during read
- `Err(DeserializationError)` - Failed to convert JSON to Ada types

---

### REQ-CACHE-007: Cache Data Structures & Concurrency

**Concurrency Requirement**:
The cache SHALL support concurrent access with these patterns:
- **Multiple simultaneous reads**: Many tasks reading different zones concurrently
- **Reads during writes**: Reads continue while cache manager adds new entries
- **Cache updates**: Cache manager adds newly parsed zones without blocking readers

**Critical Observation**: Unlike the repository (immutable value objects, read-only files),
the cache DOES have concurrency issues because:
- Multiple tasks reading from cache (concurrent reads - safe)
- Cache manager adding new parsed zones (write operation - needs synchronization)
- Race condition: Reader queries zone that's being inserted

**Solution**: Protected object with entry-less functions for lock-free reads
```ada
protected type Zone_Cache is
   -- Lock-free read (no suspension)
   function Get (ULID : ULID_Type; Zone_Id : Zone_Id_Type) return Option[StarterLib_Data];

   -- Synchronized write (entries suspend during update)
   procedure Insert (ULID : ULID_Type; Zone_Id : Zone_Id_Type; Data : StarterLib_Data);

   -- Lock-free read for export
   function Get_All return Zone_Map;
private
   Cache : Zone_Map_Type;
end Zone_Cache;
```

**Design Decisions**:
1. ✅ **Use protected objects** - Required for thread safety
2. ✅ **Entry-less functions for reads** - Lock-free, scalable concurrent reads
3. ✅ **Procedures for writes** - Serialize cache updates
4. ✅ **Two caches needed**:
   - Source_Cache (protected object for Source_Info)
   - Zone_Cache (protected object for StarterLib_Data)

3. Cache size limits and eviction policy?
   - Should there be a maximum cache size?
   - LRU eviction? FIFO? No eviction?
   - Decision: Start with no limits, measure memory usage with 600 zones

4. Cache lifecycle?
   - When is cache populated? (lazy on first query? eager at startup?)
   - When is cache invalidated? (never? TTL? manual?)
   - Decision: Parse-once pattern - cache persists for program lifetime

---

### REQ-CACHE-008: StarterLib_Data JSON Serialization [TBD]

**Open Questions**:

1. How to serialize StarterLib_Data to JSON?
   - StarterLib_Data contains:
     - Header (StarterLib_Header_Type)
     - Transitions (Vector of Transition_Type)
     - Timezone Types (Vector of Timezone_Type_Record)
     - Leap Seconds (Vector of Leap_Second_Type)
     - POSIX TZ String (Bounded_String)

2. Should we serialize full transition arrays?
   - Pro: Complete data, no need to re-parse
   - Con: Large file size (~500-1000 transitions per zone)
   - Consideration: JSON is already verbose, this could be significant

3. Should we preserve all ZoneInfo metadata?
   - Header fields (counts, version)
   - Leap seconds (rarely used)
   - POSIX TZ strings (needed for future dates)

4. Alternative approaches?
   - Option A: Serialize full parsed structure (faithful cache)
   - Option B: Serialize minimal data + re-parse on import (zoneinfo)
   - Option C: Store raw ZoneInfo binary in Base64 (defeats JSON purpose)

**Recommendation**: Serialize full structure initially (Option A)
- Measure file size with 600 zones
- Measure import/export performance
- Optimize later if needed

---

### REQ-CACHE-009: Error Handling

All cache operations SHALL use Result monad pattern:
- No exceptions thrown across API boundaries
- All errors return `Result[T]` with error information
- Error messages SHALL be human-readable
- Error messages SHALL include context (file path, operation)

**Error Categories**:
```ada
-- From Domain.Error
type Error_Kind is (
   File_Not_Found,       -- Cache file doesn't exist
   Parse_Error,          -- JSON parse error
   IO_Error,             -- Filesystem error
   Infrastructure_Error, -- Generic infrastructure failure
   Validation_Error      -- Data validation failed
);
```

---

### REQ-CACHE-010: Testing Requirements

**Unit Tests** SHALL cover:
- Export with empty cache (0 sources, 0 zones)
- Export with single source, single zone
- Export with multiple sources, multiple zones
- Export with overwrite = true/false
- Import with valid cache file
- Import with missing cache file
- Import with invalid JSON
- Import with wrong magic header
- Import with unsupported version
- Import with missing source paths

**Integration Tests** SHALL cover:
- Round-trip: Export then Import, verify data identical
- Import with partial source validation (some paths missing)
- Export/Import with ~600 zones (performance measurement)
- Concurrent access during import/export (if caches are shared)

**Performance Tests** SHALL measure:
- Export time with 600 zones
- Import time with 600 zones
- JSON file size with 600 zones
- Memory usage after import

**Success Criteria**:
- Import time < 100ms for 600 zones (target)
- No data corruption or loss in round-trip
- Graceful handling of all error cases

---

## Implementation Plan

### Phase 1: Infrastructure (Pending)
- [ ] Design cache data structures (maps, protected objects?)
- [ ] Implement Source_Cache
- [ ] Implement Zone_Cache
- [ ] Define JSON schema for StarterLib_Data

### Phase 2: Export (Pending)
- [ ] Implement JSON serialization for Source_Info
- [ ] Implement JSON serialization for StarterLib_Data
- [ ] Implement Export_Cache function
- [ ] Write export unit tests
- [ ] Write export integration tests

### Phase 3: Import (Pending)
- [ ] Implement JSON deserialization for Source_Info
- [ ] Implement JSON deserialization for StarterLib_Data
- [ ] Implement Import_Cache function with validation
- [ ] Write import unit tests
- [ ] Write import integration tests

### Phase 4: Performance Validation (Pending)
- [ ] Measure export time with 600 zones
- [ ] Measure import time with 600 zones
- [ ] Measure JSON file size
- [ ] Measure memory usage
- [ ] Decide if binary format needed

---

## Decisions from zoneinfo_before_starterlib Project

Based on comprehensive scan of `/Users/mike/Ada/github.com/abitofhelp/zoneinfo_before_starterlib`:

### ✅ RESOLVED: Cache Architecture
**Decision**: Two-tier cache in **Infrastructure Layer**, used by Repository
- **Source_Cache**: Maps ULID → Source_Info (lightweight metadata)
- **Zone_Cache**: Maps (ULID, Zone_Id) → StarterLib_Data (LRU-managed, parsed zones)
- Both caches managed by Repository implementation
- Developer explicitly calls import/export; otherwise Repository manages cache lifecycle

**Implementation Location**:
```
src/infrastructure/
  cache/
    infrastructure-cache-source_cache.ads/adb     -- Protected type
    infrastructure-cache-zone_cache.ads/adb       -- Protected LRU cache
```

### ✅ RESOLVED: Thread Safety
**Decision**: YES - Protected objects required
- **Reason**: Concurrent reads + cache manager writes = race conditions
- **Pattern**: Entry-less functions for lock-free reads, procedures for writes
- **Evidence**: zoneinfo used protected types for calculation_cache with concurrent lookups

### ✅ RESOLVED: Cache Policies
**Decision**: Configurable LRU with reasonable defaults
```ada
type Cache_Policy is (Disabled, LRU, Unbounded);
Default_Policy : constant Cache_Policy := LRU;
Default_Zone_Cache_Size : constant := 100;  -- zones
```

### ✅ RESOLVED: Cache Behavior Patterns
From zoneinfo ARCHITECTURE_ALIGNMENT_PLAN.md:

**Lazy Loading Everywhere**: Zone files parsed on first access, not at discovery
**Cache Purging**: Only 2 operations purge cache:
  - `import_cache()`: Purges, then loads from file
  - `discover_sources()`: Always purges (forces fresh directory scan)

**Cache Population**:
  - Sources added by: discover_sources(), load_source(), import_cache()
  - Zones added on-demand by: find_by_id(), find_by_pattern(), etc. (cache miss → parse → store)

**Cache Reads** (no writes):
  - export_cache(): Dumps in-memory cache to file
  - validate_source(): Read-only validation check

### ✅ RESOLVED: Performance Targets
From zoneinfo SRS:
- Cache hit lookup: ≤ 5 microseconds (p95)
- Cache miss + parse: ≤ 25 microseconds (p99)
- Target hit rate: ≥ 80%
- LRU eviction: O(1) insertion and lookup

## JSON Library Options (from Alire)

Ada has several mature JSON libraries available:

### 1. **GNATCOLL.JSON** (AdaCore) ⭐ RECOMMENDED
- **Crate**: `gnatcoll` version 25.0.0
- **License**: GPL-3.0 with GCC exception (compatible with BSD-3-Clause)
- **Pros**:
  - Industry standard (AdaCore)
  - Part of GNAT Components Collection (already widely used)
  - Well-documented, mature, stable
  - Used in production Ada projects
  - Good performance
- **Cons**:
  - Larger dependency (full GNATCOLL core)
- **Website**: https://github.com/adacore/gnatcoll-core

### 2. **json-ada** (onox)
- **Crate**: `json` version 6.0.0
- **License**: Apache-2.0
- **Pros**:
  - Lightweight, standalone JSON library
  - Ada 2012 features
  - Simpler than GNATCOLL
- **Cons**:
  - Less widely used than GNATCOLL
  - Smaller community
- **Website**: https://github.com/onox/json-ada

### 3. **utilada** (Stephane Carrez)
- **Crate**: `utilada` version 2.8.2
- **License**: Apache-2.0
- **Pros**:
  - Comprehensive utility library (logging, streams, serialization)
  - Includes JSON serialization/deserialization
  - Well-maintained, documented
- **Cons**:
  - Heavy dependency (many features we don't need)
  - Overkill for just JSON
- **Website**: https://gitlab.com/stcarrez/ada-util

### 4. **yeison** (Alejandro Mosteo)
- **Crate**: `yeison` version 0.1.0
- **License**: MIT
- **Pros**:
  - Modern Ada 2022 features
  - JSON-like data structure (also supports YAML, TOML)
  - Lightweight
- **Cons**:
  - Very new (v0.1.0)
  - Limited production track record
- **Website**: https://github.com/mosteo/yeison

### **Recommendation**: GNATCOLL.JSON

**Rationale**:
1. Industry standard from AdaCore
2. Proven in production environments
3. Good balance of features and performance
4. License compatible with project (GPL + exception = OK for library)
5. Already used by many Ada projects (ecosystem fit)

**Required GNATCOLL.JSON Features**:
- `GNATCOLL.JSON` - Core JSON types and parsing
- `JSON_Value` - Create JSON objects and arrays
- `Create_Object` - Build JSON objects programmatically
- `Set_Field` - Add fields to JSON objects
- `Get` - Extract values from JSON
- `Write` - Serialize JSON to file/string
- `Read` - Parse JSON from file/string

**Integration Steps**:
1. Add `gnatcoll` dependency: `alr with gnatcoll`
2. Add `with "gnatcoll";` to GPR files that need JSON
3. Import `GNATCOLL.JSON` in serialization modules

**GPT-5 Confirmation**: ✅ GNATCOLL.JSON is the right choice
- Mature, UTF-8 aware, widely used, actively maintained by AdaCore
- Tree-based API (`JSON_Value`) with `Read`/`Write` helpers and pretty-print
- GPL-3.0 + GCC runtime exception (safe for linking in BSD-3-Clause projects)
- Well-documented: https://docs.adacore.com/live/wave/gnatcoll-core/html/gnatcoll-core_ug/json.html

**Example Pattern from GPT-5**:
```ada
with GNATCOLL.JSON; use GNATCOLL.JSON;

-- Serialize to JSON
function To_JSON (Stats : Export_Stats_Type) return JSON_Value is
   Obj : JSON_Value := Create_Object;
begin
   Obj.Set_Field ("sources_exported", Create (Integer (Stats.Sources_Exported)));
   Obj.Set_Field ("zones_exported", Create (Integer (Stats.Zones_Exported)));
   return Obj;
end To_JSON;

-- Deserialize from JSON
function From_JSON (J : JSON_Value) return Export_Stats_Type is
   Obj : constant JSON_Value := Get (J);
begin
   return (
      Sources_Exported => Natural (Obj.Get ("sources_exported").Get_Int),
      Zones_Exported   => Natural (Obj.Get ("zones_exported").Get_Int)
   );
end From_JSON;

-- Write to file with pretty printing
procedure Write_Cache (Path : String; Cache_Data : JSON_Value) is
   F : File_Type;
begin
   Create (F, Out_File, Path);
   Put_Line (F, Write (Cache_Data, Compact => False)); -- Pretty JSON
   Close (F);
end Write_Cache;

-- Read from file
function Read_Cache (Path : String) return JSON_Value is
   -- Use GNATCOLL.Mmap for efficient large file reading if needed
   Content : constant String := Slurp_File (Path);
begin
   return Read (Content);
end Read_Cache;
```

## Open Issues / Decisions Needed

1. ~~**Cache Architecture**: Where do Source_Cache and Zone_Cache live?~~ ✅ **RESOLVED: Infrastructure layer**
2. ~~**Thread Safety**: Do we need protected objects for caches?~~ ✅ **RESOLVED: Yes, protected types**
3. ~~**JSON Library**: Use GNATCOLL.JSON or implement custom serialization?~~ ✅ **RESOLVED: GNATCOLL.JSON**
4. **StarterLib_Data Schema**: Exact JSON structure for complex Ada types?
5. ~~**Cache Persistence**: Should cache be automatically saved on program exit?~~ ✅ **RESOLVED: No, explicit export_cache() call**
6. ~~**Cache Discovery**: Should library auto-load cache on startup?~~ ✅ **RESOLVED: No, explicit import_cache() call**
7. **Binary vs JSON**: Performance measurement threshold for switching to binary (suggest: if load time > 100ms)
8. **Cache Wrapper**: Implement Cached_Provider decorator pattern from zoneinfo?

---

## References

- UML Diagrams: `/docs/diagrams/export_cache.puml`, `import_cache.puml`, `startup_with_cache.puml`
- Port Definitions: `application-port-inbound-export_cache.ads`, `application-port-inbound-import_cache.ads`
- Value Objects: `domain-value_object-cache_stats.ads`, `domain-value_object-source_info.ads`
- Current Stubs: `infrastructure-adapter-file_system-repository.adb:941` (Export), TBD (Import)
