#![warn(clippy::all, clippy::pedantic)]

use crate::parser;
use glam::IVec3;
use std::iter;

pub fn solve(input: &str, n_steps: usize) -> i32 {
    let parsed_input = parser::parse_input(input).expect("input to parse").1;

    let mut moonsystem = MoonSystem {
        moons: set_up_moons(parsed_input),
    };

    let output = moonsystem.nth(n_steps - 1).unwrap();
    output.moons.iter().map(|m| m.total_energy()).sum()
}

fn set_up_moons(parsed_input: Vec<IVec3>) -> Vec<Moon> {
    parsed_input
        .into_iter()
        .map(|pos| Moon {
            pos: pos,
            vel: IVec3 { x: 0, y: 0, z: 0 },
        })
        .collect::<Vec<Moon>>()
}

// types

#[derive(Debug, Clone, PartialEq)]
struct MoonSystem {
    moons: Vec<Moon>,
}

impl Iterator for MoonSystem {
    type Item = MoonSystem;

    fn next(&mut self) -> Option<Self::Item> {
        // Update the velocities for each moon based on the gravity calculations
        for i in 0..self.moons.len() {
            for j in i..self.moons.len() {
                let left = &self.moons[i];
                let right = &self.moons[j];

                let vec_between = right.pos - left.pos;

                self.moons[i].update_vel(vec_between);
                self.moons[j].update_vel(-vec_between);
            }
        }

        // Update the positions of each moon based on their velocites
        for m in self.moons.iter_mut() {
            m.update_pos();
        }

        Some(self.clone())
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Moon {
    pos: IVec3,
    vel: IVec3,
}

impl Moon {
    fn update_vel(&mut self, v: IVec3) {
        let v = v.signum();
        self.vel += v;
    }

    fn update_pos(&mut self) {
        self.pos += self.vel;
    }

    fn total_energy(&self) -> i32 {
        let pot = self.pos.abs().element_sum();
        let kin = self.vel.abs().element_sum();
        pot * kin
    }
}

// fn update_moons(ms: MoonSystem) -> MoonSystem {
//     let updated_vel = update_moon_gravity(ms);
//     update_moon_positions(updated_vel)
// }
//
// fn update_moon_gravity(ms: MoonSystem) -> MoonSystem {
//     todo!()
// }
//
// fn update_moon_positions(ms: MoonSystem) -> MoonSystem {
//     todo!()
// }

#[cfg(test)]
mod tests {
    use super::*;
    use glam::IVec3;

    #[test]
    fn test_example_input_1() {
        let input = "<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>";
        let expected = 179;
        assert_eq!(expected, solve(input, 10));
    }

    #[test]
    fn test_example_input_2() {
        let input = "<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>";
        let expected = 1940;
        assert_eq!(expected, solve(input, 100));
    }

    #[test]
    fn test_update_moon() {
        let mut input = Moon {
            pos: IVec3 { x: 1, y: 1, z: 1 },
            vel: IVec3 { x: 1, y: 1, z: 1 },
        };
        let expected = Moon {
            pos: IVec3 { x: 1, y: 1, z: 1 },
            vel: IVec3 { x: 2, y: 2, z: 2 },
        };
        let update = IVec3 { x: 1, y: 2, z: 3 };
        input.update_vel(update);
        assert_eq!(expected, input)
    }

    #[test]
    fn test_total_energy() {
        let test_moon = Moon {
            pos: IVec3::new(2, 1, -3),
            vel: IVec3::new(-3, -2, 1),
        };

        let expected_energy = 36;
        assert_eq!(expected_energy, test_moon.total_energy());
    }

    #[test]
    fn test_step() {
        let input = "<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>";
        let parsed_input = parser::parse_input(input).expect("input to parse").1;
        let mut moonsystem = MoonSystem {
            moons: set_up_moons(parsed_input),
        };
        let expected = MoonSystem {
            moons: vec![
                Moon {
                    pos: IVec3::new(2, -1, 1),
                    vel: IVec3::new(3, -1, -1),
                },
                Moon {
                    pos: IVec3::new(3, -7, -4),
                    vel: IVec3::new(1, 3, 3),
                },
                Moon {
                    pos: IVec3::new(1, -7, 5),
                    vel: IVec3::new(-3, 1, -3),
                },
                Moon {
                    pos: IVec3::new(2, 2, 0),
                    vel: IVec3::new(-1, -3, 1),
                },
            ],
        };
        assert_eq!(moonsystem.next().unwrap(), expected)
    }
}
