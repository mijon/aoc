use itertools::Itertools;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct SIF {
    pub nlayers: usize,
    pub width: usize,
    pub height: usize,
    pub layers: Vec<SIFLayer>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SIFLayer {
    pub height: usize,
    pub width: usize,
    pub data: Vec<Vec<u32>>,
}

impl SIFLayer {
    pub fn new(data: Vec<Vec<u32>>) -> Self {
        SIFLayer {
            height: data.len(),
            width: data[0].len(),
            data,
        }
    }

    pub fn count(&self, v: i32) -> i32 {
        self.data
            .iter()
            .map(|row| row.iter().filter(|&n| *n == v as u32).count() as i32)
            .sum()
    }
}

impl SIF {
    pub fn new(data_str: &str, height: i32, width: i32) -> Self {
        let data: Vec<u32> = data_str
            .chars()
            .map(|c| c.to_digit(10).expect("Expected number"))
            .collect();
        let layerlength = height as usize * width as usize;
        let layer_data: Vec<SIFLayer> = data
            .into_iter()
            .chunks(layerlength)
            .into_iter()
            .map(|l| chunk_layer(l.collect(), width as usize))
            .collect();

        let height = layer_data[0].height;
        let width = layer_data[0].width;

        SIF {
            nlayers: layer_data.len(),
            layers: layer_data,
            height,
            width,
        }
    }

    pub fn overlay(&self) -> SIFLayer {
        self.layers
            .clone()
            .into_iter()
            .reduce(|l1: SIFLayer, l2: SIFLayer| add_siff_layers(l1, l2))
            .expect("Should produce a SIF Layer")
    }
}

// Functions

pub fn chunk_layer(input: Vec<u32>, width: usize) -> SIFLayer {
    let chunked = input
        .into_iter()
        .chunks(width)
        .into_iter()
        // .map(|c| c.collect())
        .map(std::iter::Iterator::collect)
        .collect();

    SIFLayer::new(chunked)
}

pub fn add_pixels(p1: u32, p2: u32) -> u32 {
    match (p1, p2) {
        (0, _) => 0,
        (1, _) => 1,
        (2, p) => p,
        (_, _) => panic!(),
    }
}

pub fn add_siff_layers(l1: SIFLayer, l2: SIFLayer) -> SIFLayer {
    let mut output = Vec::new();

    for i in 0..l1.height {
        let mut row_vec = Vec::new();
        for j in 0..l1.width {
            row_vec.push(add_pixels(l1.data[i][j], l2.data[i][j]));
        }
        output.push(row_vec);
    }
    SIFLayer {
        height: l1.height,
        width: l1.width,
        data: output,
    }
}

fn pixel_to_char(p: u32) -> &'static str {
    match p {
        0 => "#",
        1 => " ",
        2 => "_",
        _ => panic!("Incorrect pixel!"),
    }
}

fn sif_layer_to_string(l: SIFLayer) -> String {
    let mut output = String::new();
    for i in 0..l.height {
        let mut cur_row = String::new();
        for j in 0..l.width {
            cur_row.push_str(pixel_to_char(l.data[i][j]));
        }
        cur_row.push_str("\n");
        output.push_str(&cur_row);
    }
    output
}

impl fmt::Display for SIFLayer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", sif_layer_to_string(self.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_chunk_to_layer() {
        let input = vec![1, 2, 3, 4, 5, 6, 7, 8, 9];
        let expected = SIFLayer {
            height: 3,
            width: 3,
            data: vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9]],
        };
        assert_eq!(chunk_layer(input, 3), expected)
    }

    #[test]
    fn test_read_sif() {
        let input = "123456789012";
        let expected = SIF {
            nlayers: 2,
            layers: vec![
                SIFLayer {
                    height: 2,
                    width: 3,
                    data: vec![vec![1, 2, 3], vec![4, 5, 6]],
                },
                SIFLayer {
                    height: 2,
                    width: 3,
                    data: vec![vec![7, 8, 9], vec![0, 1, 2]],
                },
            ],
            height: 2,
            width: 3,
        };
        assert_eq!(expected, SIF::new(input, 2, 3))
    }

    #[test]
    fn test_example_overlay() {
        let example_sif = SIF::new("0222112222120000", 2, 2);
        let expected_output = SIFLayer {
            height: 2,
            width: 2,
            data: vec![vec![0, 1], vec![1, 0]],
        };
        assert_eq!(expected_output, example_sif.overlay())
    }
}
