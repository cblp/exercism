# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring

from dataclasses import dataclass
from typing import NamedTuple, Optional


class Record(NamedTuple):
    record_id: int
    parent_id: int


@dataclass
class Node:
    node_id: int
    children: list["Node"]

    def __init__(self, node_id: int):
        self.node_id = node_id
        self.children = []


def BuildTree(records: list[Record]) -> Optional[Node]:
    # pylint: disable=invalid-name

    if not records:
        return None

    # normalize
    records.sort()

    # validate
    for expected_id, (record_id, parent_id) in enumerate(sorted(records)):
        if record_id != expected_id:
            raise ValueError("Record id is invalid or out of order.")
        if record_id < parent_id:
            raise ValueError(
                "Node parent_id should be smaller than its record_id."
            )
        if record_id == parent_id and record_id != 0:
            raise ValueError(
                "Only root should have equal record and parent id."
            )

    # build
    nodes = [Node(r.record_id) for r in records]
    for record_id, parent_id in records[1:]:
        nodes[parent_id].children.append(nodes[record_id])
    return nodes[0]
