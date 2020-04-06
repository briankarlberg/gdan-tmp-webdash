## Web Dashboard For Viewing GDAN TMP Machine Learning Results

Example format for post-selection feature files and predictions files:
https://www.synapse.org/#!Synapse:syn8011998/wiki/411602

### Set up
-activate virtual environment
  ```
  git clone url
  ```
-create 3 directories:
  -data/predictions
  -data/feature-sets
  -data/v8-feature-matrices

### Start the Shiny Application

- Add prediction file(s) to: `data/predictions`
- Add feature set file(s) to: `data/feature-sets`
- Add raw feature set file(s) to: `data/feature-sets`
- From data directory:
  ```
  Rscript load_sqlite.R ./v8-feature-matrices/ ./features.sqlite
  ```
- From main directory:
  ```
  docker-compose up
  ```

The app will be available at [http://localhost:3838](http://localhost:3838).

## (Optional) Download Data From Synapse

If you want to be able to examine feature values in the app do the following:

- Install the synapse python client: 
  ```
  pip install synapseclient boto3
  ```
- Download the V8 tarball: 

  ```
  cd data
  bash download_data.sh
  ```
- Prepare the feature database: 

  ```
  Rscript load_sqlite.R ./v8-feature-matrices/ ./features.sqlite
  ```

*Expected Data Directory Layout*

```
data/
├── features.sqlite
├── feature-sets
│   ├── featuresets_v8.tsv
│   └── ...
└── predictions
    ├── ACC_randomforest_200127.tsv
    └── ...
```
