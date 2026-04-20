pub type Value = i32;
type Result<T = ()> = std::result::Result<T, Error>;

#[derive(Clone, Debug)]
struct Definition {
    name: &'static str,
    code: Vec<&'static str>,
    environment_top: usize,
}

#[derive(Default)]
pub struct Forth {
    stack: Vec<Value>,
    environment: Vec<Definition>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    DivisionByZero,
    StackUnderflow,
    UnknownWord,
    InvalidWord,
}

use Error::*;

impl Forth {
    pub fn new() -> Forth {
        Self::default()
    }

    pub fn stack(&self) -> &[Value] {
        &self.stack
    }

    pub fn eval(&mut self, input: &'static str) -> Result {
        match input.split_ascii_whitespace().collect::<Vec<_>>()[..] {
            [":", name, ref body @ .., ";"] => {
                if name.parse::<Value>().is_ok() {
                    return Err(InvalidWord);
                }
                self.environment.push(Definition {
                    name,
                    code: body.to_vec(),
                    environment_top: self.environment.len(),
                });
            }
            ref ops => self.eval_ops(ops, self.environment.len())?,
        }
        Ok(())
    }

    fn eval_ops(&mut self, ops: &[&str], environment_top: usize) -> Result {
        ops.iter()
            .try_for_each(|op| self.eval_op(op, environment_top))
    }

    fn eval_op(&mut self, op: &str, environment_top: usize) -> Result {
        if let Ok(n) = op.parse() {
            self.stack.push(n);
            return Ok(());
        }
        if let Some(definition) = self.environment[0..environment_top]
            .iter()
            .rev()
            .find(|d| d.name.eq_ignore_ascii_case(op))
            .cloned()
        {
            return self.eval_ops(&definition.code, definition.environment_top);
        }
        match op.to_ascii_lowercase().as_str() {
            "-" => self.pop2_push(|x, y| [x - y]),
            "*" => self.pop2_push(|x, y| [x * y]),
            "+" => self.pop2_push(|x, y| [x + y]),
            "/" => self.pop2_apply(safe_div),
            "dup" => self.pop1_push(|x| [x, x]),
            "drop" => self.pop1_push(|_| []),
            "over" => self.pop2_push(|x, y| [x, y, x]),
            "swap" => self.pop2_push(|x, y| [y, x]),
            _ => Err(UnknownWord),
        }
    }

    fn pop(&mut self) -> Result<Value> {
        self.stack.pop().ok_or(StackUnderflow)
    }

    fn pop1_push<const N: usize>(
        &mut self,
        f: fn(Value) -> [Value; N],
    ) -> Result {
        let x = self.pop()?;
        self.stack.extend(f(x));
        Ok(())
    }

    fn pop2_push<const N: usize>(
        &mut self,
        f: fn(Value, Value) -> [Value; N],
    ) -> Result {
        let y = self.pop()?;
        let x = self.pop()?;
        self.stack.extend(f(x, y));
        Ok(())
    }

    fn pop2_apply(&mut self, f: fn(Value, Value) -> Result<Value>) -> Result {
        let y = self.pop()?;
        let x = self.pop()?;
        self.stack.push(f(x, y)?);
        Ok(())
    }
}

fn safe_div(x: Value, y: Value) -> Result<Value> {
    x.checked_div(y).ok_or(DivisionByZero)
}
