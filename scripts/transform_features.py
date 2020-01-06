import argparse
import csv
from glob import glob

from etl.emitter import new_emitter
from etl import (Cancer, Feature, Cancer_Feature)


def transform(files,
              emitter_prefix=None,
              emitter_directory="."):

    emitter = new_emitter(name="json",
                          directory=emitter_directory,
                          prefix=emitter_prefix)

    emitted_features = {}
    for input_matrix in files:
        print("processing: %s" % input_matrix)
        i = 0
        fh = open(input_matrix, "r")
        for line in csv.DictReader(fh, delimiter="\t"):
            if i != 0:
                fh.close()
                break
            keys = list(line.keys())
            cancer_id = keys[0]
            cancer = Cancer(gid=Cancer.make_gid(cancer_id))
            emitter.emit_vertex(cancer)
            for fid in keys[2:]:
                parts = fid.split(":")
                feature = Feature(
                    gid=Feature.make_gid(fid),
                    datatype=parts[0],
                    platform_id1=parts[1],
                    platform_id2=parts[2],
                    feature_id1=parts[3],
                    feature_id2=parts[4]
                )
                if feature.gid() not in emitted_features:
                    emitter.emit_vertex(feature)
                emitter.emit_edge(
                    Cancer_Feature(
                        _from=Cancer.make_gid(cancer_id),
                        _to=feature.gid()
                    ),
                    emit_backref=True
                )
                emitted_features[feature.gid()] = None
            i += 1
    emitter.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        '--input-matrix', '-i',
        type=str,
        required=True,
        help='matrix (used as pattern for glob)'
    )
    args = parser.parse_args()
    files = glob(args.input_matrix)
    files = [f for f in files if "CVfolds" not in f]
    transform(files)
