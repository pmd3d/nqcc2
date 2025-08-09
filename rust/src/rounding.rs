pub fn round_away_from_zero(n: i32, x: i32) -> i32 {
    if x % n == 0 {
        x
    } else if x < 0 {
        x - n - (x % n)
    } else {
        x + n - (x % n)
    }
}
