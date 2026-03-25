#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    SpanTooLong,
    InvalidDigit(char),
}

pub fn lsp(string_digits: &str, span: usize) -> Result<u64, Error> {
    if span == 0 {
        return Ok(1);
    }
    let nums: Vec<u8> = string_digits
        .chars()
        .map(|c| {
            if c.is_numeric() {
                Ok(c as u8 - b'0')
            } else {
                Err(Error::InvalidDigit(c))
            }
        })
        .collect::<Result<_, _>>()?;
    if span > nums.len() {
        return Err(Error::SpanTooLong);
    }
    Ok(nums
        .windows(span)
        .map(|xs| xs.iter().map(|x| *x as u64).product())
        .max()
        .unwrap())
}
