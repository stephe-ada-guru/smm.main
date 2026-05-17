# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

SMM (Stephe's Music Manager) is an Ada application that manages a SQLite database of music metadata and provides both a command-line interface and an Apache CGI web server. It also syncs the database with an Android app via a TCP socket protocol.

## Build System

The project uses [Alire](https://alire.ada.dev/) for dependency management and `make` via `Alire.make` (in the root directory) for build targets.

**Build (release mode, default):**
```
make -f Alire.make
```

**Build in development mode:**
```
ALIRE_BUILD_ARGS=--development make -f Alire.make
```

**Install binaries to `~/.local/bin/`:**
```
make -f Alire.make install
```

**Executables** are placed in `build/bin/`. The GPR project (`build/smm_alire.gpr`) builds four main programs:
- `smm.exe` — CLI tool (entry point: `source/smm-driver.adb`)
- `smm-server_driver.exe` — Apache CGI web server
- `smm-show_id3.exe` — ID3 tag viewer
- `smm-db_sync_server.exe` — TCP socket server for Android database sync

**Tests** are built via `build/smm_test.gpr` (uses `test/` sources). To build tests, switch the GPR file in `prj-alire.el` (or pass `smm_test.gpr` to `alr build`).

## Running Tests

Run all tests:
```
make -f Alire.make t_all
```

Run a single test suite and/or routine:
```
build/bin/test_all_harness.exe [<verbose 0|1> [test_name [routine_name [verbosity]]]]
# e.g.:
build/bin/test_all_harness.exe 1 "SMM.Database.Test" "Test_Insert"
```

`test_one_harness.exe` runs only `SMM.Database_Remote.IP.Test` (the network sync tests). These require `smm-db_sync_server.exe` to be running separately:
```
make -f Alire.make t3      # runs test_one_harness against a local sync server
make -f Alire.make t4      # runs smm-db_sync_server standalone with test_db_sync.config
```

Create fresh test databases:
```
make -f Alire.make empty_database_test_1
make -f Alire.make empty_database_test_2
```

## Environment Variables (for make targets)

| Variable | Default (from `prj-alire.el`) | Purpose |
|---|---|---|
| `SERVER_DATA` | `/var/www/html/music_server_data` | Directory for web server assets and db |
| `SERVER_IP` | `127.0.0.1` | IP for db sync server |
| `SERVER_PORT` | `16#9003#` | Port for db sync server |

## Architecture

### Database (`source/smm-database.{ads,adb}`)

Central abstraction over a SQLite3 database (via GNATCOLL). The `Song` table schema is defined in `source/create_schema.sql` (schema version 2). Times are stored as TEXT in `'YYYY-MM-DD HH:MM:SS'` local-time format (not UTC), because of a GNATCOLL bug with DATETIME comparisons. All database access goes through the `SMM.Database.Database` type and its `Cursor` type for iteration.

`Category` is a comma-separated string of labels (e.g., `"instrumental,best"`). The fields `Play_Before` / `Play_After` are foreign keys to Song.ID used to enforce playlist ordering.

### CLI (`source/smm-driver.adb`)

Dispatches on a `Command_Type` enum. Opens the database at the hard-coded path `/var/www/html/music_server_data/smm.db` (constant `DB_File_Name` in `source/smm.ads`). Key sub-commands:
- `update` / `import` — read ID3/M4A metadata from files and write to db
- `update_playlist` — build a least-recently-heard playlist file
- `check` — compare filesystem to db
- `compare_playlist` — compare db against Spotify or HTML playlist
- `history` — output download-interval histogram data for gnuplot

### Web Server (`source/smm-server.{ads,adb}`)

Implements an Apache CGI script. The executable (`smm-server_driver.exe`) must be placed in `/usr/lib/cgi-bin/`. It reads a config file at `build/smm_server_devel.config` (keys: `Root`, `DB_Filename`, `Server_Data`). Static web assets (CSS, JS, PNG icons) are served from `SERVER_DATA`.

### Database Sync (`source/smm-database_remote.ads`, `smm-database.diff.{ads,adb}`, `smm-db_sync_server.adb`)

Implements a two-phase sync protocol between the local Linux database and an Android app over TCP sockets (default port `16#9001#`). The abstract type `SMM.Database_Remote.Database` is implemented by:
- `smm-database_remote-disk.{ads,adb}` — wraps the local `SMM.Database.Database`
- `smm-database_remote-ip.{ads,adb}` — communicates over TCP with a remote `smm-db_sync_server`

`SMM.Database.Diff` computes incremental differences (`Inc_Diff`) or full initialization (`Init_Remote`) as JSON arrays, then applies them via `Apply`. Messages on the wire are length-prefixed strings (`Network_String` in `smm-database_remote.ads`).

### Metadata Parsing

- `source/smm-id3.{ads,adb}` — ID3v2 tag reader for `.mp3` files
- `source/smm-m4a.{ads,adb}` — M4A/AAC metadata reader
- `source/smm-metadata.{ads,adb}` — shared stream utilities and frame type definitions

### External Dependencies (via Alire)

- `stephes_ada_library` (pinned to `../org.stephe_leake.sal`) — SAL utilities (config files, progress, web utils, etc.)
- `aunit_ext` (pinned to `../org.stephe_leake.aunit_ext`) — AUnit extensions
- `gnatcoll`, `gnatcoll_iconv`, `gnatcoll_sqlite` — GNOME Ada collection (JSON, SQL/SQLite)
- `utilada_curl` — HTTP client for Spotify API access

## Config File Format

Config files (e.g., `smm_test_1.config`, `build/smm_server_devel.config`) are parsed by `SAL.Config_Files`. Keys are bare names; values follow `=`. Example:
```
Database_File=smm_test_1.db
Server_IP=127.0.0.1
Server_Port=16#9002#
```

## Coding Conventions

- Ada 2022 syntax is used throughout; requires GNAT FSF ≥ 11 or Community ≥ 2021.
- Package hierarchy mirrors file names: `SMM.Database.Diff` is in `smm-database-diff.{ads,adb}`.
- Executables use `.exe` suffix even on Linux (simplifies Makefiles).
- `+` / `-` operators alias `To_Unbounded_String` / `To_String` throughout (defined in `smm.ads`).
- Verbosity is controlled via the global `SMM.Verbosity : Integer` (0 = silent).
- Style checks are enforced by the compiler switch `Style_Checks` from `standard_common_alire`.
