#!/bin/bash
# D2K-Compliant Entrypoint
#
# This script activates the Conda environment and then executes the R script
# specified by the $R_SCRIPT environment variable.
# It passes all command-line arguments ("$@") directly to the R script.

# If any command fails (non-zero exit code), stop immediately. Prevents silent failures. Makes debugging easier. Ensures Docker exits properly if something goes wrong.
set -e # Exit immediately if a command exits with a non-zero status.

# 1. Activate the Conda 'base' environment (which has R and all packages)
source activate base

# 2. Execute the R script provided by the R_SCRIPT environment variable,
#    passing all arguments ("$@") from the 'docker run' command to it.
exec /opt/conda/bin/Rscript /app/src/$R_SCRIPT "$@"
