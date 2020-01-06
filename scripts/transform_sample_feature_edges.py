import argparse
import csv

from etl.emitter import new_emitter
from etl import (Sample, Feature, Sample_Feature)


def transform_one(input_matrix,
                  emitter_prefix,
                  emitter_directory="."):

    emitter = new_emitter(name="json",
                          directory=emitter_directory,
                          prefix=emitter_prefix)

    with open(input_matrix, "r") as fh:
        for line in csv.DictReader(fh, delimiter="\t"):
            cancer_id = list(line.keys())[0]
            sample_id = line[cancer_id]
            sample = Sample(gid=Sample.make_gid(sample_id))
            for fid, value in line.items():
                if fid in [cancer_id, "Labels"]:
                    continue
                emitter.emit_edge(
                    Sample_Feature(
                        _from=sample.gid(),
                        _to=Feature.make_gid(fid),
                        value=float(value)
                    ),
                    emit_backref=True
                )

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
    parser.add_argument(
        '--emitter-prefix', '-p',
        type=str,
        required=True,
        help='emitter prefix'
    )
    args = parser.parse_args()
    transform_one(args.input_matrix, args.emitter_prefix)
