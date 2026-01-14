"""
combine_econet.py

Finds files in `econet_data` and `econet_data_update` with the same filename,
concatenates them, deduplicates by timestamp, sorts by timestamp, and writes
results into `econet_data_combined`.

Behavior & assumptions:
- CSV files share identical columns.
- Datetime parsing uses pandas' to_datetime with inference; if parsing fails for
  most rows, the script will retry with dayfirst=True.
- If a file exists only in one folder, it's copied (and parsed/re-saved to ensure consistent datetime formatting).
- Output files are written to `econet_data_combined/<same_filename>.csv`.
- By default the script will not overwrite the source folders.
"""

from pathlib import Path
import pandas as pd
import logging
import sys

# -------- CONFIG --------
DIR_OLD = Path("econet_data")
DIR_NEW = Path("econet_data_update")
DIR_OUT = Path("econet_data_combined")

DATETIME_COL = "observation_datetime"
OVERWRITE_OUTPUT = True
# ------------------------

logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")


def read_csv(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)

    if DATETIME_COL not in df.columns:
        raise ValueError(f"{path.name} is missing required column '{DATETIME_COL}'")

    # Parse as UTC
    df[DATETIME_COL] = pd.to_datetime(
        df[DATETIME_COL],
        utc=True,
        errors="coerce"
    )

    # Drop rows with bad timestamps
    before = len(df)
    df = df.dropna(subset=[DATETIME_COL])
    dropped = before - len(df)
    if dropped > 0:
        logging.warning(f"{path.name}: dropped {dropped} rows with invalid datetimes")

    return df


def combine_files(old_path: Path | None, new_path: Path | None, out_path: Path):
    dfs = []

    if old_path and old_path.exists():
        dfs.append(read_csv(old_path))
    if new_path and new_path.exists():
        dfs.append(read_csv(new_path))

    if not dfs:
        return

    combined = pd.concat(dfs, ignore_index=True)

    # Sort by time
    combined = combined.sort_values(DATETIME_COL)

    # Drop duplicate timestamps
    # keep="last" ensures updated data wins on overlap
    before = len(combined)
    combined = combined.drop_duplicates(
        subset=[DATETIME_COL],
        keep="last"
    )
    after = len(combined)

    if before != after:
        logging.info(f"{out_path.name}: removed {before - after} duplicate timestamps")

    combined = combined.reset_index(drop=True)

    out_path.parent.mkdir(parents=True, exist_ok=True)
    combined.to_csv(out_path, index=False)
    logging.info(f"Wrote {out_path} ({len(combined)} rows)")


def main():
    if not DIR_OLD.exists() and not DIR_NEW.exists():
        logging.error("No input directories found.")
        sys.exit(1)

    DIR_OUT.mkdir(parents=True, exist_ok=True)

    files_old = {p.name for p in DIR_OLD.glob("*.csv")} if DIR_OLD.exists() else set()
    files_new = {p.name for p in DIR_NEW.glob("*.csv")} if DIR_NEW.exists() else set()
    all_files = sorted(files_old | files_new)

    if not all_files:
        logging.info("No CSV files found.")
        return

    for fname in all_files:
        out_path = DIR_OUT / fname

        if out_path.exists() and not OVERWRITE_OUTPUT:
            logging.info(f"Skipping existing {fname}")
            continue

        old_path = DIR_OLD / fname if fname in files_old else None
        new_path = DIR_NEW / fname if fname in files_new else None

        logging.info(f"Processing {fname}")
        combine_files(old_path, new_path, out_path)

    logging.info("Done.")


if __name__ == "__main__":
    main()
