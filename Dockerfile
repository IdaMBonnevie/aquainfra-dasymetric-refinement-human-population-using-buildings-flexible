# D2K-Compliant Dockerfile: AquaINFRA Dasymetric refinement of human population (OPTIMIZED + CRAN FIX)
#
# This configuration strictly adheres to the execution requirements of the D2K User Guide
# by using a dedicated ENTRYPOINT to receive arguments from the 'docker run' command.
#
# OPTIMIZATIONS APPLIED:
# 1. Uses 'mamba' for faster Conda environment solving.
# 2. Installs 'eurostat' from CRAN to get the latest version and fix API errors.

# 1. Start from a miniconda base image
# Provides Python, Conda, and a Linux base environment to build on
FROM continuumio/miniconda3:latest

# 2. Set the working directory
WORKDIR /app

# 3. Copy the environment definition file
# Brings in the Conda environment specification that defines required packages
COPY .binder/environment.yml /app/environment.yml

# 4. Set the shell to BASH for robust command execution
# Ensures complex RUN commands (like chaining with &&) work reliably
SHELL ["/bin/bash", "-c"]

# 5. (OPTIMIZED) Install Mamba for faster environment solving
# Installs Mamba, a faster alternative to Conda for dependency resolution
RUN conda install -n base -c conda-forge mamba -y

# 6. (OPTIMIZED) Create the Conda environment using Mamba
# Installs all dependencies defined in environment.yml. This installs all packages *except* eurostat. Cleans cache to reduce image size.
RUN mamba env update -n base -f /app/environment.yml -y && \
    mamba clean --all -f -y

# 7. (FIX) Install the latest 'eurostat' from CRAN
# This bypasses the conda-forge version issue and fixes the '410 Gone' API error.
RUN /opt/conda/bin/R -e "install.packages('eurostat', repos='https://cloud.r-project.org/', dependencies=TRUE)"

# 8. Copy R scripts (reusable functions) into the standard D2K source folder
# Adds reusable R scripts into the container
COPY src/ /app/src/

# 9. Copy the entrypoint script and fix its line endings
# Makes script executable. Removes Windows CRLF line endings (prevents Linux execution errors). 
COPY entrypoint.sh /app/entrypoint.sh
RUN chmod +x /app/entrypoint.sh && sed -i 's/\r$//' /app/entrypoint.sh

# 10. Set the R library path explicitly
# Ensures R looks in the correct library directory inside Conda
ENV R_LIBS_USER="/opt/conda/lib/R/library"

# 11. (D2K-COMPLIANT FIX) Set the ENTRYPOINT
# This tells Docker to run our script and pass the 'docker run' args to it. Defines the main executable for the container. This makes the container behave like a command-line tool.
ENTRYPOINT ["/app/entrypoint.sh"]

# 12. Set a default empty CMD (best practice with ENTRYPOINT)
# Allows arguments to be passed cleanly without overriding the entrypoint.
CMD []
