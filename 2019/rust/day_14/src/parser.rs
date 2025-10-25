#![allow(dead_code)]
use nom::Parser;
use nom::character::complete::{alpha0, i32, newline};
use nom::multi::separated_list1;
use nom::{IResult, bytes::complete::tag};

// Types
#[derive(Debug, PartialEq, Clone)]
pub struct QuantItem {
    pub quantity: i32,
    pub item: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Recipe {
    pub input: Vec<QuantItem>,
    pub output: QuantItem,
}

// Parsers
fn p_quantitem(input: &str) -> IResult<&str, QuantItem> {
    let (input, quantity) = i32.parse(input)?;
    let (input, _) = tag(" ").parse(input)?;
    let (input, item) = alpha0.parse(input)?;

    Ok((
        input,
        QuantItem {
            quantity: quantity,
            item: item.into(),
        },
    ))
}

fn p_recipe(input: &str) -> IResult<&str, Recipe> {
    let (input, ins) = separated_list1(tag(", "), p_quantitem).parse(input)?;
    let (input, _) = tag(" => ").parse(input)?;
    let (input, out) = p_quantitem.parse(input)?;

    Ok((
        input,
        Recipe {
            input: ins,
            output: out,
        },
    ))
}

pub fn parse(input: &str) -> IResult<&str, Vec<Recipe>> {
    let output = separated_list1(newline, p_recipe).parse(input)?;
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_quantitem() {
        let input = "10 ORE";
        let expected_qi = QuantItem {
            quantity: 10,
            item: "ORE".into(),
        };
        let expected = Ok(("", expected_qi));
        assert_eq!(expected, p_quantitem(input))
    }

    #[test]
    fn test_parse_recipe_one_input() {
        let input = "10 ORE => 10 A";
        let expected_recipe = Recipe {
            input: vec![QuantItem {
                quantity: 10,
                item: "ORE".into(),
            }],
            output: QuantItem {
                quantity: 10,
                item: "A".into(),
            },
        };
        let expected = Ok(("", expected_recipe));
        assert_eq!(expected, p_recipe(input))
    }

    #[test]
    fn test_parse_recipe_many_inputs() {
        let input = "7 A, 1 B => 1 C";
        let expected_recipe = Recipe {
            input: vec![
                QuantItem {
                    quantity: 7,
                    item: "A".into(),
                },
                QuantItem {
                    quantity: 1,
                    item: "B".into(),
                },
            ],
            output: QuantItem {
                quantity: 1,
                item: "C".into(),
            },
        };
        let expected = Ok(("", expected_recipe));
        assert_eq!(expected, p_recipe(input))
    }
}
