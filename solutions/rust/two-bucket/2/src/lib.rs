use std::{
    cmp::min,
    collections::{HashSet, VecDeque},
};

#[derive(PartialEq, Eq, Debug)]
pub enum Bucket {
    One,
    Two,
}

use Bucket::*;

/// A struct to hold your results in.
#[derive(PartialEq, Eq, Debug)]
pub struct BucketStats {
    /// The total number of "moves" it should take to reach the desired number
    /// of liters, including the first fill.
    pub moves: u8,

    /// Which bucket should end up with the desired number of liters?
    /// (Either "one" or "two")
    pub goal_bucket: Bucket,

    /// How many liters are left in the other bucket?
    pub other_bucket: u8,
}

#[derive(Debug)]
struct State {
    moves: u8,
    one: u8,
    two: u8,
}

/// Solve the bucket problem
pub fn solve(
    capacity_one: u8,
    capacity_two: u8,
    goal: u8,
    start_bucket: &Bucket,
) -> Option<BucketStats> {
    let mut visited = HashSet::new(); // TODO from([(0, 0)])?
    let mut q = VecDeque::from([match start_bucket {
        One => State {
            moves: 1,
            one: capacity_one,
            two: 0,
        },
        Two => State {
            moves: 1,
            one: 0,
            two: capacity_two,
        },
    }]);
    let forbidden_state = match start_bucket {
        One => (0, capacity_two),
        Two => (capacity_one, 0),
    };
    while let Some(state) = q.pop_front() {
        // check if state is goal
        if state.one == goal {
            return Some(BucketStats {
                moves: state.moves,
                goal_bucket: One,
                other_bucket: state.two,
            });
        }
        if state.two == goal {
            return Some(BucketStats {
                moves: state.moves,
                goal_bucket: Two,
                other_bucket: state.one,
            });
        }

        // build next moves
        visited.insert((state.one, state.two));
        let mut add = |one, two| {
            if !visited.contains(&(one, two)) && (one, two) != forbidden_state {
                q.push_back(State {
                    moves: state.moves + 1,
                    one,
                    two,
                });
            }
        };

        // fill one
        if state.one < capacity_one {
            // pour two -> one
            if state.two > 0 {
                let d = min(capacity_one - state.one, state.two);
                add(state.one + d, state.two - d);
            }
            add(capacity_one, state.two);
        }
        // fill two
        if state.two < capacity_two {
            // pour one -> two
            if state.one > 0 {
                let d = min(capacity_two - state.two, state.one);
                add(state.one - d, state.two + d);
            }
            add(state.one, capacity_two);
        }
        // empty one
        if state.one > 0 {
            add(0, state.two);
        }
        // empty two
        if state.two > 0 {
            add(state.one, 0);
        }
    }
    None
}
