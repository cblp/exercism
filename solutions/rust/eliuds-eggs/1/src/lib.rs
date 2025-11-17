pub fn egg_count(mut display_value: u32) -> usize {
    let mut ones: usize = 0;
    while display_value != 0 {
        ones += (display_value & 1) as usize;
        display_value >>= 1;
    }
    ones
}
