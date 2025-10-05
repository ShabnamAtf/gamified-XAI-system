@echo off
setlocal
REM Always run from the folder where this .bat is located
cd /d "%~dp0"

REM Create venv if missing
if not exist ".venv\Scripts\python.exe" (
  echo Creating virtual environment...
  py -3 -m venv .venv 2>nul || python -m venv .venv
)

REM Activate venv (cmd-compatible, no PowerShell policy issues)
call ".venv\Scripts\activate.bat"

REM Install deps
python -m pip install --upgrade pip
pip install -r requirements.txt

REM Optional: choose port via first arg, default 8501
set PORT=%1
if "%PORT%"=="" set PORT=8501

echo Launching Streamlit on port %PORT% ...
python -m streamlit run app.py --server.address 0.0.0.0 --server.port %PORT%
