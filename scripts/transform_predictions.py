import argparse
import csv
import json

from etl.emitter import new_emitter
from etl import (Sample, Split, Model, Prediction, Cancer, Subtype,
                 Sample_Prediction, Split_Prediction,  Model_Prediction,
                 Model_Split, Model_Cancer)


def transform_one(input_matrix,
                  emitter_prefix,
                  emitter_directory="."):

    emitter = new_emitter(name="json",
                          directory=emitter_directory,
                          prefix=emitter_prefix)

    emitted_models = {}
    i = 0
    with open(input_matrix, "r") as fh:
        for line in csv.DictReader(filter(lambda row: row[0]!='#', fh), delimiter="\t"):
            cancer_id = list(line.keys())[0]
            sample_id = line[cancer_id]
            repeat = line["Repeat"][1:] if line["Repeat"].startswith("R") else line["Repeat"]
            fold = line["Fold"][1:] if line["Fold"].startswith("F") else line["Fold"]
            split_id = "%s:R%s:F%s" % (cancer_id, repeat, fold)

            if i == 0:
                for model_id in list(line.keys())[5:]:
                    model = Model(
                        gid=Model.make_gid("%s:%s" % (cancer_id, model_id)),
                    )
                    emitter.emit_vertex(model)
                i = 1

            for model_id, pred_val in line.items():
                if model_id in [cancer_id, "Repeat", "Fold", "Label", "Test"]:
                    continue

                model = Model(
                    gid=Model.make_gid("%s:%s" % (cancer_id, model_id)),
                )
                if model.gid() not in emitted_models:
                    emitter.emit_vertex(model)
                    emitter.emit_edge(
                        Model_Cancer(
                            _from=model.gid(),
                            _to=Cancer.make_gid(cancer_id)
                        ),
                        emit_backref=True
                    )

                metadata = None
                prediction = None
                if model_id.endswith("|p") or pred_val.startswith("{"):
                    metadata = json.loads(pred_val)
                    max_prob = 0
                    for subtype, prob in metadata["classification"].items():
                        if prob > max_prob:
                            prediction = subtype
                            max_prob = prob
                else:
                    prediction = pred_val

                prediction = Prediction(
                    gid=Prediction.make_gid("%s:%s:%s:%s" % (cancer_id, model_id, split_id, sample_id)),
                    predicted_value=Subtype.make_gid("%s:%s" % (cancer_id, prediction)),
                    actual_value=Subtype.make_gid("%s:%s" % (cancer_id, line["Label"])),
                    metadata=metadata,
                    type="testing" if line["Test"] == "1" else "training",
                    repeat=int(repeat),
                    fold=int(fold)
                )
                emitter.emit_vertex(prediction)

                emitter.emit_edge(
                    Model_Prediction(
                        _from=model.gid(),
                        _to=prediction.gid(),
                    ),
                    emit_backref=True
                )
                emitter.emit_edge(
                    Sample_Prediction(
                        _from=Sample.make_gid(sample_id),
                        _to=prediction.gid(),
                    ),
                    emit_backref=True
                )
                emitter.emit_edge(
                    Model_Split(
                         _from=model.gid(),
                        _to=Split.make_gid(split_id),
                    ),
                    emit_backref=True
                )
                emitter.emit_edge(
                    Split_Prediction(
                        _from=Split.make_gid(split_id),
                        _to=prediction.gid(),
                     ),
                    emit_backref=True
                )

                # TODO mode of predicitons for given sample / model?
                # TODO: model -> feature edges

    emitter.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        '--input-matrix', '-i',
        type=str,
        required=True,
        help='CVfold matrix'
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
