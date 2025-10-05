@echo off
setlocal
cd /d "%~dp0"

if not exist ".venv\Scripts\python.exe" (
  echo Creating virtual environment...
  py -3 -m venv .venv 2>nul || python -m venv .venv
)

call ".venv\Scripts\activate.bat"
python -m pip install --upgrade pip
pip install -r requirements.txt

echo Running experiment and condition aggregation...
python scripts\run_experiment.py --config stimuli\experiment_config.yml
python scripts\run_conditions.py --delta-util 0.22 --delta-frac 0.33

echo Making figures and poster artifacts...
python scripts\make_condition_plots.py
python scripts\make_poster_text.py
python scripts\make_poster_table.py

echo Done. See outputs\ for CSVs, plots, and Table 1.
pause
