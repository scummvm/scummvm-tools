# MACS2 Tools

Tools for the MACS2 engine (Schatz im Silbersee).

## Translation Workflow

The game was originally released in German only. Translations are managed via standard PO files.

### 1. Extract strings to a PO template

```bash
create_macs2_translation extract RESOURCE.MCS macs2.pot
```

This produces a `.pot` file with all game strings grouped by dialog context. The format uses `msgctxt` to identify the source (scene or object) and `\n` to separate lines within a dialog unit.

The `\n` separators correspond to individual display lines in the game. Keep the same number of lines as the original.

### 3. Pack into binary

```bash
create_macs2_translation pack en.po macs2_translation.dat
```

### 4. Install

Place `macs2_translation.dat` in the game directory alongside `RESOURCE.MCS`. ScummVM will detect the translation and offer the translated language variant.

The file is also distributed via `dists/engine-data/macs2_translation.dat` in the ScummVM source tree.
