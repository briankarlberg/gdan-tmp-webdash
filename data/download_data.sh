#!/usr/bin/env bash

set -e

synapse get syn21578003

(mkdir tmpdir v8-feature-matrices v8-cv-matrices v8-qc || true) 2> /dev/null

tar -xzf TMP_v8_20200203.tar.gz -C ./tmpdir

mv ./tmpdir/*_CVfolds_*.tsv ./v8-cv-matrices/

mv ./tmpdir/*.png ./v8-qc/
mv ./tmpdir/v8_*.txt ./v8-qc/

mv ./tmpdir/*_*.tsv ./v8-feature-matrices/

rm -rf tmpdir
