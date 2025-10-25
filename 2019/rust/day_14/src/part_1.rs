use crate::parser::{self, QuantItem, Recipe};

pub fn solve(input: &str) -> i32 {
    let recipe_book = parser::parse(input).unwrap().1;

    // let mut fuel = recipe_book.iter().filter(|r| r.output.item == "FUEL");
    // println!("{:?}", fuel.next());

    let fuel = get_recipe_for("FUEL", &recipe_book).unwrap().output;
    sum_ingredients(fuel, &recipe_book)
}

fn sum_ingredients(target: QuantItem, recipe_book: &Vec<Recipe>) -> i32 {
    println!("Summing for: {target:?}");
    // if target.item.as_str() == "ORE" {
    //     println!("!!!! hit the ore test branch");
    //     return target.quantity;
    // }

    // get the recipe that matches the target item
    let target_recipe = get_recipe_for(&target.item, recipe_book).unwrap();

    // then get the inputs
    let target_inputs = target_recipe.input;

    // then loop over the inputs multiplying the quantity against the sum_ingredients of
    // Let's say I need 12 x, but the recipe makes x in batches of 5. I therefore need to
    // make three batches (= 15 x) to have enough x.

    let mut result = 0;
    for qi in target_inputs {
        if let Some(tmp_look_ahead_recipe) = get_recipe_for(&qi.item, recipe_book) {
            if qi.item == "A".to_string() {
                println!("test");
            }

            println!("\tnum: {}", target_recipe.output.quantity);
            println!("\tdenom: {}", tmp_look_ahead_recipe.output.quantity);

            let factor = roundup_int_div(
                target_recipe.output.quantity,
                tmp_look_ahead_recipe.output.quantity,
            );
            let ingreds = sum_ingredients(qi, recipe_book);
            println!("Factor is {factor}");
            println!("num ingredients is {ingreds}");
            result += factor * ingreds;
        } else {
            result += target.quantity
        }
    }
    result
}

fn get_recipe_for(target: &str, recipe_book: &[Recipe]) -> Option<Recipe> {
    // println!("-- Starting again to look for {:?}", target);
    recipe_book
        .iter()
        .find(|r| r.output.item == target)
        // .inspect(|c| println!("{:?}", c))
        .cloned()
}

fn roundup_int_div(num: i32, denom: i32) -> i32 {
    if num % denom == 0 {
        num / denom
    } else if num < denom {
        1 + num / denom
    } else if num > denom {
        denom
    } else {
        panic!("whoops")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_recipe() {
        let input = "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL";
        let target_item = "B";
        let expected_recipe = Recipe {
            input: vec![QuantItem {
                quantity: 1,
                item: "ORE".to_string(),
            }],
            output: QuantItem {
                quantity: 1,
                item: "B".to_string(),
            },
        };
        let recipe_book = parser::parse(input).unwrap().1;
        assert_eq!(
            expected_recipe,
            get_recipe_for(target_item, &recipe_book).unwrap()
        )
    }
    #[test]
    fn test_example_1() {
        let input = "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL";
        assert_eq!(solve(input), 31)
    }

    #[test]
    fn test_example_2() {
        let input = "9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL";
        assert_eq!(solve(input), 165)
    }

    #[test]
    fn test_example_3() {
        let input = "157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT";
        assert_eq!(solve(input), 13312)
    }

    #[test]
    fn test_example_4() {
        let input = "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF";
        assert_eq!(solve(input), 180697)
    }

    #[test]
    fn test_example_5() {
        let input = "171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX";
        assert_eq!(solve(input), 2210736)
    }

    #[test]
    fn test_integer_div() {
        let a = 12;
        let b = 5;
        assert_eq!(3, roundup_int_div(a, b))
    }
}
