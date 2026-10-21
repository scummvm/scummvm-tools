# MACS2 Tools

Tools for the MACS2 engine (Schatz im Silbersee).

## Translation Workflow

The game was originally released in German only. Translations are managed via standard PO files.

### 1. Extract strings to a PO template

```bash
create_macs2_translation extract RESOURCE.MCS macs2.pot
```

This produces a `.pot` file with all game strings grouped by dialog context. The format uses `msgctxt` to identify the source (scene or object) and `\n` to separate lines within a dialog unit.

Hotspot overlay labels are appended as one entry per unique German noun (`msgctxt "hotspotlabel"`). Object and NPC overlay names use `msgctxt "objectlabel"`. Both lists are embedded in `create_macs2_translation.cpp` and use the **full game** entries (`kSceneHotspotNames[]` and `kObjectNames[]` in `engines/macs2/hotspot_names.cpp`). Keys are CP850 and must match the engine tables exactly. The DOS demo reuses the source labels; the engine maps its different object IDs and scene string indices to the full-game translation entries, so the same package works with both releases.

When packing, hotspot and object labels share one CP850 lookup table in the DAT file (duplicate German strings need only one translation).

Action-bar / HUD chrome uses a separate DAT section (`msgctxt "uilabel"`). Keys are the German source strings from `engines/macs2/actionbar.cpp` (for example `Benutze %s mit %s`). Translators may reorder words and `%s` / `%d` placeholders. Rebuild the DAT after changing these strings.

The `\n` separators correspond to individual display lines in the game. Keep the same number of lines as the original.

### 2. Pack into binary

```bash
create_macs2_translation pack en.po sis-translation-en.dat
create_macs2_translation pack sv.po sis-translation-sv.dat
```

### 3. Install

Name each package `sis-translation-<lang>.dat`, where `sis` is the game ID and `<lang>` is the language code (`en`, `sv`, etc.). The output filename is supplied explicitly to the pack command.

Place the packages in the full DOS game or DOS demo directory alongside `RESOURCE.MCS`. Use the same packages built from the full-game PO files for both releases. Multiple languages can be installed together. In ScummVM, open the game's **Game Options** and select a language under **Translation** on the **Game** tab. **Original** uses the German game text. The selection is saved per target.

The full game is detected as the original German `sis-de` and the DOS demo as `sis-demo-de`, regardless of the installed packages. Packages are not embedded in or bundled with ScummVM. The legacy `macs2_translation.dat` filename is no longer used. The DOS demo-only chapter-end message retains German text because it has no full-game translation entry. These packages are not offered for the Amiga demo.
