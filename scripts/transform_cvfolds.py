import argparse
import csv
from glob import glob
from multiprocessing import Pool
from itertools import product
import os

from etl.emitter import new_emitter
from etl import (Cancer, Subtype, Sample, Split,
                 Cancer_Subtype, Sample_Subtype, Sample_Split)


def transform_one(input_matrix,
                  emitter_prefix,
                  emitter_directory="."):

    emitter = new_emitter(name="json",
                          directory=emitter_directory,
                          prefix=emitter_prefix)

    emitted_subtypes = {}
    i = 0
    with open(input_matrix, "r") as fh:
        for line in csv.DictReader(fh, delimiter="\t"):
            cancer_id = list(line.keys())[0]
            subtype_id = line["Labels"]
            sample_id = line[cancer_id]

            cancer = Cancer(gid=Cancer.make_gid(cancer_id))

            if i == 0:
                emitter.emit_vertex(cancer)
                for rf in list(line.keys())[2:]:
                    parts = rf.split(":")
                    repeat = int(parts[0][1:])
                    fold = int(parts[1][1:])
                    split = Split(
                        gid=Split.make_gid("%s:%s" % (cancer_id, rf)),
                        repeat=repeat,
                        fold=fold
                    )
                    emitter.emit_vertex(split)
                i = 1

            subtype = Subtype(gid=Subtype.make_gid("%s:%s" % (cancer_id, subtype_id)))
            if subtype.gid() not in emitted_subtypes:
                emitter.emit_vertex(subtype)
                emitter.emit_edge(
                    Cancer_Subtype(
                        _from=cancer.gid(),
                        _to=subtype.gid()
                    ),
                    emit_backref=True
                )
                emitted_subtypes[subtype.gid()] = None

            sample = Sample(gid=Sample.make_gid(sample_id))
            emitter.emit_vertex(sample)
            emitter.emit_edge(
                Sample_Subtype(
                    _from=sample.gid(),
                    _to=subtype.gid()
                ),
                emit_backref=True
            )
            for rf, value in line.items():
                if rf in [cancer_id, "Labels"]:
                    continue
                value = int(value)
                emitter.emit_edge(
                    Sample_Split(
                        _from=sample.gid(),
                        _to=Split.make_gid("%s:%s" % (cancer_id, rf)),
                        type="testing" if value == 1 else "training"
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
        help='CVfold matrix (used as pattern for glob)'
    )
    parser.add_argument(
        '--max-procs', '-p',
        type=int,
        default=4,
        help='number of files to process in parallel'
    )
    args = parser.parse_args()
    files = glob(args.input_matrix)
    projects = [os.path.basename(f).split('_')[0] for f in files]
    wp = Pool(args.max_procs)
    results = wp.starmap(transform_one, product(files, projects))
    print(results)
    # transform_one(args.input_matrix, args.prefix)
