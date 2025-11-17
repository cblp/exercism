from dataclasses import dataclass
from typing import Optional


class StackUnderflowError(Exception):
    def __init__(self) -> None:
        super().__init__("Insufficient number of items in stack")


@dataclass
class Definition:
    body: list[str]
    env: "Environment"


class Environment(dict[str, Definition]):

    def define(self, name: str, body: list[str]) -> None:
        if try_int(name) is not None:
            raise ValueError("illegal operation")
        self[name] = Definition(body, Environment(self.copy()))


def try_int(s: str) -> Optional[int]:
    try:
        return int(s)
    except ValueError:
        return None


class Interpreter:
    stack: list[int]

    def __init__(self) -> None:
        self.stack = []

    def push(self, x: int) -> None:
        return self.stack.append(x)

    def pop(self) -> int:
        if not self.stack:
            raise StackUnderflowError
        return self.stack.pop()

    def index(self, i: int) -> int:
        if len(self.stack) < i + 1:
            raise StackUnderflowError
        return self.stack[-i - 1]

    def interpret_op(self, op: str, env: Environment) -> None:
        i = try_int(op)
        if i is not None:
            self.push(i)
        elif op in env:
            self.interpret_ops(env[op].body, env[op].env)
        elif op == "+":
            self.push(self.pop() + self.pop())
        elif op == "-":
            b = self.pop()
            a = self.pop()
            self.push(a - b)
        elif op == "*":
            self.push(self.pop() * self.pop())
        elif op == "/":
            b = self.pop()
            if b == 0:
                raise ZeroDivisionError("divide by zero")
            a = self.pop()
            self.push(a // b)
        elif op == "dup":
            self.push(self.index(0))
        elif op == "drop":
            self.pop()
        elif op == "over":
            self.push(self.index(1))
        elif op == "swap":
            a = self.pop()
            b = self.pop()
            self.push(a)
            self.push(b)
        else:
            raise ValueError("undefined operation")

    def interpret_ops(self, ops: list[str], env: Environment) -> None:
        for op in ops:
            self.interpret_op(op, env)


def evaluate(input_data: list[str]) -> list[int]:
    interpreter = Interpreter()
    env = Environment()
    for line in input_data:
        ops = line.lower().split()
        if ops[0] == ":":
            assert ops[-1] == ";"
            name, *body = ops[1:-1]
            env.define(name, body)
        else:
            interpreter.interpret_ops(ops, env)
    return interpreter.stack
