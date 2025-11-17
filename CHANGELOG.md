# Changelog

All notable changes to ZoneInfo - IANA Timezone Operations Library for Ada 2022 will be documented in this file.

The format is based on [Common Changelog](https://common-changelog.org),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [1.0.0-rc2] - 2025-11-16

_Stub release for Alire name reservation (non-functional)._

**IMPORTANT**: This release is a buildable stub implementation created solely to reserve the "zoneinfo" name in the Alire package index while the TZif dependency is pending review. This version will build successfully but **all API operations return errors** indicating the missing TZif dependency.

### Changed

- Replaced full API implementation with stub that returns `Infrastructure_Error` for all operations
- Updated description to "IANA timezone operations (PREVIEW - requires tzif pending review)"
- Commented out TZif dependency in `alire.toml` and `config/zoneinfo_config.gpr`
- Bumped version from 1.0.0-rc1 to 1.0.0-rc2

### Removed

- Temporary removal of TZif dependency (will be restored in v1.0.0)
- Temporary removal of functional implementations (stub returns errors)

### Notes

**For Users**: Do not use this release - it is non-functional. Wait for v1.0.0 which will be published after TZif is approved in Alire.

**To Restore Full Functionality**:
1. Restore `src/api/zoneinfo-api.adb` from `.backup` file
2. Uncomment TZif dependency in `alire.toml`
3. Uncomment `with "tzif.gpr"` in `config/zoneinfo_config.gpr`
4. Update version to 1.0.0

---

## License & Copyright

- **License**: BSD-3-Clause
- **Copyright**: © 2025 Michael Gardner, A Bit of Help, Inc.
- **SPDX-License-Identifier**: BSD-3-Clause
