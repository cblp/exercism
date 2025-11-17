pub fn is_leap_year(year: u64) -> bool {
    let divisible_by = |n| year % n == 0;
    divisible_by(4) & !(divisible_by(100) & !divisible_by(400))
}
