use std::fs;
use std::io;
use std::io::Write;
use std::path::Path;

use intcode::IntcodeState;
const PROMPT: &'static str = ">> ";

#[derive(Debug, PartialEq)]
enum ReplState {
    Active,
    Terminated,
}

#[derive(Debug)]
struct Repl {
    state: ReplState,
    loaded_program: Option<IntcodeState>,
}

impl Repl {
    fn end(mut self) -> Self {
        self.state = ReplState::Terminated;
        self
    }

    /// Load an intcode file into the repl
    fn load_program(mut self, path: &Path) -> Repl {
        if let Some(program) = fs::read_to_string(path).ok() {
            self.loaded_program = Some(IntcodeState::new(&program, vec![0]))
        } else {
            self.loaded_program = None
        }
        self
    }
}

pub fn repl() {
    let mut repl = Repl {
        state: ReplState::Active,
        loaded_program: None,
    };

    while repl.state == ReplState::Active {
        print!("{}", PROMPT);
        std::io::stdout().flush().unwrap();
        let mut input = String::new();

        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        if input == "/exit\n" {
            repl = repl.end();
        }

        if input == "/print\n" {
            println!("{repl:?}");
        }

        if input.len() > 5 && input[0..=4] == *"/load" {
            let mut info = input.split_whitespace();
            let _ = info.next();
            let path = Path::new(info.next().expect("expected path"));
            repl = repl.load_program(path);
        }

        // TODO: Implement the following:
        // - Ability to set the input
        // - on any other input, run the intcode until an input is needed (might need to
        // implement run_until_before)
        // - Print any outputs to the console and clear the output from the IntcodeState (to
        // avoid cluttering).
    }
}

fn main() {
    repl()
}
