## Web Dashboard For Viewing GDAN TMP Machine Learning Results
https://www.synapse.org/#!Synapse:syn8011998/wiki/411602

## Download Data From Synapse

Project Home for CCG_TMP_AWG - https://www.synapse.org/#!Synapse:syn8011998/wiki/411602

- Install the synapse python client: `pip install synapseclient`
- Download the V8 tarball: 

  ```
  cd data
  bash download_data.sh
  ```
- Add prediction file(s) to: `data/predictions`
- Add feature set file(s) to: `data/feature-sets`
- Prepare the feature database: 

  ```
  Rscript load_sqlite.R
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
    └── ,,,
```

### Start the Shiny Application

```
docker-compose up -d
```

The app will be available at [http://localhost:3838](http://localhost:3838).


