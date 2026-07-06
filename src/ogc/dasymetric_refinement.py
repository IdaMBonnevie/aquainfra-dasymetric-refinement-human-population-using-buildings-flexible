import logging
import subprocess
import json
import os

from pygeoapi.process.base import BaseProcessor, ProcessorExecuteError

# how to import python modules containing a hyphen:
import importlib
docker_utils = importlib.import_module("pygeoapi.process.human-population-toolbox.src.ogc.docker_utils")

LOGGER = logging.getLogger(__name__)

script_title_and_path = __file__
metadata_title_and_path = script_title_and_path.replace('.py', '.json')
PROCESS_METADATA = json.load(open(metadata_title_and_path))


class DasymetricRefinementProcessor(BaseProcessor):

    def __init__(self, processor_def):
        super().__init__(processor_def, PROCESS_METADATA)
        self.supports_outputs = True
        self.process_id = self.metadata["id"]
        self.my_job_id = 'nothing-yet'
        self.image_name = 'human-population-toolbox:20251201'
        self.script_name = 'dasymetric_refinement.R'

    def set_job_id(self, job_id: str):
        self.my_job_id = job_id

    def __repr__(self):
        return f'<DasymetricRefinementProcessor> {self.name}'

    def execute(self, data, outputs=None):

        config_file_path = os.environ.get('AQUAINFRA_CONFIG_FILE', "./config.json")
        with open(config_file_path, 'r') as configFile:
            configJSON = json.load(configFile)
        self.docker_executable = configJSON["docker_executable"]
        self.download_dir = configJSON["download_dir"].rstrip('/')
        self.download_url = configJSON["download_url"].rstrip('/')

        # Where to store output data (will be mounted read-write into container):
        output_dir = f'{self.download_dir}/out/{self.process_id}/job_{self.my_job_id}'
        output_url = f'{self.download_url}/out/{self.process_id}/job_{self.my_job_id}'
        os.makedirs(output_dir, exist_ok=True)

        # User inputs
        in_refinement_type = data.get('refinement_type')
        in_inputFile1_corineCLC_rds = data.get('inputFile1_corineCLC_rds')
        in_inputFile2_corineYear_rds = data.get('inputFile2_corineYear_rds')
        in_inputFile3_lauInCatchment_rds = data.get('inputFile3_lauInCatchment_rds')
        in_inputFile4_popFocusYear_rds = data.get('inputFile4_popFocusYear_rds')
        in_inputFile5_catchment_gpkg = data.get('inputFile5_catchment_gpkg')
        in_inputFile6_weightTable_rds = data.get('inputFile6_weightTable_rds')
        in_inputFile7_buildings_rds = data.get('inputFile7_buildings_rds')
        in_inputFile8_censusGrid_rds = data.get('inputFile8_censusGrid_rds')

        # Optional: path to a threshold found by an earlier run (e.g. the 2021 run)
        # to reuse instead of re-running the search. The R script creates this file
        # (containing NA) if it doesn't already exist, so it's safe to point this at
        # a fresh path when there's no previous run to reuse.
        in_inputFile9_bestThresholdIfExisting_rds = data.get('inputFile9_bestThresholdIfExisting_rds')

        # Check user inputs
        if in_refinement_type is None:
            raise ProcessorExecuteError('Missing parameter "refinement_type". Please provide a refinement_type.')
        if in_refinement_type not in ('simple', 'weighted'):
            raise ProcessorExecuteError(
                f'Invalid parameter "refinement_type": "{in_refinement_type}". '
                f'Allowed values are "simple" and "weighted".'
            )
        if in_inputFile1_corineCLC_rds is None:
            raise ProcessorExecuteError('Missing parameter "inputFile1_corineCLC_rds". Please provide a inputFile1_corineCLC_rds.')
        if in_inputFile2_corineYear_rds is None:
            raise ProcessorExecuteError('Missing parameter "inputFile2_corineYear_rds". Please provide a inputFile2_corineYear_rds.')
        if in_inputFile3_lauInCatchment_rds is None:
            raise ProcessorExecuteError('Missing parameter "inputFile3_lauInCatchment_rds". Please provide a inputFile3_lauInCatchment_rds.')
        if in_inputFile4_popFocusYear_rds is None:
            raise ProcessorExecuteError('Missing parameter "inputFile4_popFocusYear_rds". Please provide a inputFile4_popFocusYear_rds.')
        if in_inputFile5_catchment_gpkg is None:
            raise ProcessorExecuteError('Missing parameter "inputFile5_catchment_gpkg". Please provide a inputFile5_catchment_gpkg.')
        if in_inputFile6_weightTable_rds is None:
            raise ProcessorExecuteError('Missing parameter "inputFile6_weightTable_rds". Please provide a inputFile6_weightTable_rds.')
        if in_inputFile7_buildings_rds is None:
            raise ProcessorExecuteError('Missing parameter "inputFile7_buildings_rds". Please provide a inputFile7_buildings_rds.')
        if in_inputFile8_censusGrid_rds is None:
            raise ProcessorExecuteError('Missing parameter "inputFile8_censusGrid_rds". Please provide a inputFile8_censusGrid_rds.')
        # If no existing-threshold file was supplied, point at a fresh path in this
        # job's output dir — the R script will initialize it (as NA) since it doesn't
        # exist yet, which correctly triggers the internal search for pop_focus_year "2021".
        if in_inputFile9_bestThresholdIfExisting_rds is None:
            best_threshold_seed_filename = 'best_threshold_seed-%s.rds' % self.my_job_id
            in_inputFile9_bestThresholdIfExisting_rds = f'{output_dir}/{best_threshold_seed_filename}'

        # Where to store output data
        refinement_rds_filename = 'refinement-%s.rds' % self.my_job_id
        refinement_rds_filepath = f'{output_dir}/{refinement_rds_filename}'
        refinement_rds_link = f'{output_url}/{refinement_rds_filename}'

        refinement_tif_filename = 'refinement-%s.tif' % self.my_job_id
        refinement_tif_filepath = f'{output_dir}/{refinement_tif_filename}'
        refinement_tif_link = f'{output_url}/{refinement_tif_filename}'

        cell_statistics_filename = 'cell_statistics-%s.rds' % self.my_job_id
        cell_statistics_filepath = f'{output_dir}/{cell_statistics_filename}'
        cell_statistics_link = f'{output_url}/{cell_statistics_filename}'

        corine_final_filename = 'corine_final-%s.rds' % self.my_job_id
        corine_final_filepath = f'{output_dir}/{corine_final_filename}'
        corine_final_link = f'{output_url}/{corine_final_filename}'
 
        best_threshold_filename = 'best_threshold-%s.rds' % self.my_job_id
        best_threshold_filepath = f'{output_dir}/{best_threshold_filename}'
        best_threshold_link = f'{output_url}/{best_threshold_filename}'

        # Assemble args for script (order must match the R script's commandArgs):
        # <refinement_type> <corineCLC_rds_path> <corine_year_rds_path> <lau_in_catchment_rds_path>
        # <pop_focus_year_rds_path> <catchment_gpkg_path> <weight_table_rds_path>
        # <buildings_rds_path> <census_grid_rds_path> <best_threshold_if_existing_rds_path>
        # <output_refinement_rds_path> <output_refinement_tif_path> <output_cell_statistics_rds_path>
        # <output_corine_final_rds_path> <output_best_threshold_rds_path>
        script_args = [
            in_refinement_type,
            in_inputFile1_corineCLC_rds,
            in_inputFile2_corineYear_rds,
            in_inputFile3_lauInCatchment_rds,
            in_inputFile4_popFocusYear_rds,
            in_inputFile5_catchment_gpkg,
            in_inputFile6_weightTable_rds,
            in_inputFile7_buildings_rds,
            in_inputFile8_censusGrid_rds,
            in_inputFile9_bestThresholdIfExisting_rds,
            refinement_rds_filepath,
            refinement_tif_filepath,
            cell_statistics_filepath,
            corine_final_filepath,
            best_threshold_filepath
        ]

        # Run docker container:
        returncode, stdout, stderr, user_err_msg = docker_utils.run_docker_container(
            self.docker_executable,
            self.image_name,
            self.script_name,
            output_dir,
            script_args
        )

        if not returncode == 0:
            user_err_msg = "no message" if len(user_err_msg) == 0 else user_err_msg
            err_msg = 'Running docker container failed: %s' % user_err_msg
            raise ProcessorExecuteError(user_msg = err_msg)
        else:
            response_object = {
                "outputs": {
                    "refinement_rds": {
                        "title": self.metadata['outputs']['refinement_rds']['title'],
                        "description": self.metadata['outputs']['refinement_rds']['description'],
                        "href": f'{refinement_rds_link}'
                    },
                    "refinement_tif": {
                        "title": self.metadata['outputs']['refinement_tif']['title'],
                        "description": self.metadata['outputs']['refinement_tif']['description'],
                        "href": f'{refinement_tif_link}'
                    },
                    "cell_statistics": {
                        "title": self.metadata['outputs']['cell_statistics']['title'],
                        "description": self.metadata['outputs']['cell_statistics']['description'],
                        "href": f'{cell_statistics_link}'
                    },
                    "corine_final": {
                        "title": self.metadata['outputs']['corine_final']['title'],
                        "description": self.metadata['outputs']['corine_final']['description'],
                        "href": f'{corine_final_link}'
                    },
                    "best_threshold": {
                        "title": self.metadata['outputs']['best_threshold']['title'],
                        "description": self.metadata['outputs']['best_threshold']['description'],
                        "href": f'{best_threshold_link}'
                    }
                }
            }
            return 'application/json', response_object
