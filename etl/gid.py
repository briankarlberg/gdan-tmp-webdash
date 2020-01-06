
def vertex_gid(label: str, submitter_id: str):
    return "{}:{}".format(label, submitter_id)


def edge_gid(label: str, from_gid: str, to_gid: str):
    return "(%s)-%s->(%s)" % (from_gid, label, to_gid)
