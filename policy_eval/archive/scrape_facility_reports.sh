#!/bin/bash
#SBATCH --begin=now
#SBATCH --job-name=scrapecafacilities
#SBATCH --mail-type=ALL
#SBATCH --mail-user=hongjinl@law.stanford.edu
#SBATCH --output=./logs/%x_%j.out
#SBATCH --partition=owners
#SBATCH --time=18:00:00
#SBATCH --cpus-per-task=30
#SBATCH --mem=30GB

singularity exec $GROUP_HOME/singularity/epa_11-3-21.sif python3 scrape_facility_reports.py
