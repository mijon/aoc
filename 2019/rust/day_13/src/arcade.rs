#![allow(dead_code)]
use glam::IVec2;
use std::cmp::max;
use std::collections::HashMap;
use std::convert::From;

#[derive(Debug, PartialEq, Clone)]
pub enum Tile {
    Empty,
    Wall,
    Block,
    Paddle,
    Ball,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Arcade {
    height: usize,
    width: usize,
    tiles: HashMap<IVec2, Tile>,
    pub score: i32,
}

// impls
impl Into<char> for Tile {
    fn into(self) -> char {
        match self {
            Tile::Empty => ' ',
            Tile::Wall => '#',
            Tile::Block => '.',
            Tile::Paddle => '-',
            Tile::Ball => 'o',
        }
    }
}

impl From<i32> for Tile {
    fn from(item: i32) -> Self {
        match item {
            0 => Tile::Empty,
            1 => Tile::Wall,
            2 => Tile::Block,
            3 => Tile::Paddle,
            4 => Tile::Ball,
            _ => panic!("Expected a tile number between 0 and 4"),
        }
    }
}

impl Arcade {
    pub fn new() -> Self {
        Arcade {
            height: 0,
            width: 0,
            tiles: HashMap::new(),
            score: 0,
        }
    }

    pub fn add_tile(&mut self, x: i32, y: i32, tile_num: i32) {
        let new_tile = Tile::from(tile_num);

        self.height = max(self.height, y as usize);
        self.width = max(self.width, x as usize);
        self.tiles.insert(IVec2::new(x, y), new_tile);
    }

    pub fn get_tiles(&self) -> HashMap<IVec2, Tile> {
        self.tiles.clone()
    }

    pub fn draw(&self) -> String {
        let mut screen = Vec::new();
        screen.push(format!("Score: {}", self.score));

        for i in 0..=self.height {
            let mut row: String = String::new();
            for j in 0..=self.width {
                row.push(
                    self.tiles
                        .get(&IVec2::new(j as i32, i as i32))
                        .unwrap_or(&Tile::Empty)
                        .to_owned()
                        .into(),
                );
            }
            screen.push(row);
        }
        screen.join("\n")
    }

    pub fn update_score(&mut self, score: i32) {
        self.score = score
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_tile() {
        let mut empty_arcade = Arcade::new();
        let target_location = IVec2::new(10, 4);
        let arcade_with_one_tile = Arcade {
            height: 4,
            width: 10,
            tiles: HashMap::from([(target_location, Tile::Wall)]),
            score: 0,
        };
        empty_arcade.add_tile(10, 4, 1);
        assert_eq!(arcade_with_one_tile, empty_arcade)
    }
}
