import argparse
import csv
import json

from etl.emitter import new_emitter
from etl import (Feature, FeatureSet, FeatureSet_Feature)


def transform_one(input_matrix,
                  emitter_prefix,
                  emitter_directory="."):

    emitter = new_emitter(name="json",
                          directory=emitter_directory,
                          prefix=emitter_prefix)

    with open(input_matrix, "r") as fh:
        for line in csv.DictReader(filter(lambda row: row[0] != '#', fh), delimiter="\t"):
            # cancers = json.loads(line["TCGA_Projects"])
            features = json.loads(line["Features"])

            fs = FeatureSet(
                gid=FeatureSet.make_gid(line["Feature_Set_ID"])
            )
            emitter.emit_vertex(fs)
            for f in features:
                emitter.emit_edge(
                    FeatureSet_Feature(
                        _from=fs.gid(),
                        _to=Feature.make_gid(f)
                    ),
                    emit_backref=True
                )

    emitter.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        '--input-file', '-i',
        type=str,
        required=True,
        help='feature set matrix'
    )
    parser.add_argument(
        '--emitter-prefix', '-p',
        type=str,
        required=True,
        help='emitter prefix'
    )
    parser.add_argument(
        '--emitter-dir', '-d',
        type=str,
        default='.',
        help='emitter prefix'
    )
    args = parser.parse_args()
    transform_one(args.input_matrix, args.emitter_prefix, args.emitter_dir)
