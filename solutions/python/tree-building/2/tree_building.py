# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring

from dataclasses import dataclass, field
from typing import Optional


@dataclass(frozen=True, order=True)
class Record:
    record_id: int
    parent_id: int


@dataclass
class Node:
    node_id: int
    children: list["Node"] = field(default_factory=list)


def BuildTree(records: list[Record]) -> Optional[Node]:
    # pylint: disable=invalid-name

    if not records:
        return None

    # normalize
    records.sort()

    # validate
    for expected_id, r in enumerate(sorted(records)):
        if r.record_id != expected_id:
            raise ValueError("Record id is invalid or out of order.")
        if r.record_id < r.parent_id:
            raise ValueError(
                "Node parent_id should be smaller than its record_id."
            )
        if r.record_id == r.parent_id and r.record_id != 0:
            raise ValueError(
                "Only root should have equal record and parent id."
            )

    # build
    nodes = [Node(r.record_id) for r in records]
    for r in records[1:]:
        nodes[r.parent_id].children.append(nodes[r.record_id])
    return nodes[0]
