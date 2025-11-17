def evaluate(expr: list[str]) -> int:
    match expr[-3:]:
        case [value]:
            return int(value)
        case ["multiplied", "by", b]:
            return evaluate(expr[:-3]) * int(b)
        case ["divided", "by", b]:
            return evaluate(expr[:-3]) // int(b)
        case [_, "plus", b]:
            return evaluate(expr[:-2]) + int(b)
        case [_, "minus", b]:
            return evaluate(expr[:-2]) - int(b)
        case [_, "cubed"]:
            raise ValueError("unknown operation")
        case _:
            raise ValueError("syntax error")


def answer(question: str) -> int:
    question = question.removeprefix("What is ").removesuffix("?")
    return evaluate(question.split())
