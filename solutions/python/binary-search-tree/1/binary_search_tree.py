from dataclasses import dataclass
from typing import Optional


@dataclass
class TreeNode:
    data: str
    left: Optional["TreeNode"] = None
    right: Optional["TreeNode"] = None

    def __str__(self) -> str:
        left = f", left={self.left}" if self.left else ""
        right = f", right={self.right}" if self.right else ""
        return f"TreeNode(data={self.data}{left}{right})"

    def add(self, data: str) -> None:
        if data <= self.data:
            if self.left:
                self.left.add(data)
            else:
                self.left = TreeNode(data)
        else:
            if self.right:
                self.right.add(data)
            else:
                self.right = TreeNode(data)


def sorted_data(node: Optional[TreeNode]) -> list[str]:
    return (
        sorted_data(node.left) + [node.data] + sorted_data(node.right)
        if node
        else []
    )


class BinarySearchTree:
    root: Optional[TreeNode]

    def __init__(self, tree_data: list[str]):
        self.root = None
        for datum in tree_data:
            self.add(datum)

    def add(self, data: str) -> None:
        if self.root is None:
            self.root = TreeNode(data)
        else:
            self.root.add(data)

    def data(self) -> Optional[TreeNode]:
        return self.root

    def sorted_data(self) -> list[str]:
        return sorted_data(self.root)
