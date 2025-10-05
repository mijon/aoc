use color_eyre::Result;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind};
use intcode::{IntcodeState, Opcode, ParameterMode, num_params, parse_intcode};
// use itertools::Position;
// use itertools::Itertools;
use ratatui::{
    DefaultTerminal, Frame,
    layout::{Constraint, Direction, Layout},
    style::{Style, Styled, Stylize},
    text::{Line, Span, ToSpan},
    widgets::{Block, Borders, Paragraph, Wrap},
};

fn main() -> Result<()> {
    color_eyre::install()?;
    let mut terminal = ratatui::init();
    let result = App::default().run(&mut terminal);
    ratatui::restore();
    result
}

#[derive(Debug, Default)]
pub struct App {
    intcode_state: Option<intcode::IntcodeState>,
    exit: bool,
    history: Vec<IntcodeState>,
    input_mode: InputMode,
    code_nums: CodeNums,
}

#[derive(Debug, Default)]
enum CodeNums {
    #[default]
    NoShow,
    Show,
}

#[derive(Debug, Default)]
enum InputMode {
    #[default]
    Viewing,
    LoadingFile,
}

impl App {
    pub fn run(&mut self, terminal: &mut DefaultTerminal) -> Result<()> {
        while !self.exit {
            terminal.draw(|frame| self.draw(frame))?;
            self.handle_events()?;
        }
        Ok(())
    }

    fn draw(&self, frame: &mut Frame) {
        // ---- Layout ----
        // TODO: Refactor this out to make it simpler
        let outer_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Constraint::Fill(1), Constraint::Max(1)])
            .split(frame.area());

        let main_window_horiz_split = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(vec![Constraint::Percentage(20), Constraint::Percentage(80)])
            .split(outer_layout[0]);

        let inner_left_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![
                Constraint::Max(4),
                Constraint::Fill(1),
                Constraint::Fill(1),
            ])
            .split(main_window_horiz_split[0]);

        let inner_right_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Constraint::Percentage(70), Constraint::Percentage(30)])
            .split(main_window_horiz_split[1]);

        // ---- Layout ----
        // Two conditions that we match on, The Some case where we have an intcode program loaded
        // and the None case where we do not
        match &self.intcode_state {
            Some(v) => {
                frame.render_widget(
                    Paragraph::new(display_intcode(v, &self))
                        .block(
                            Block::new()
                                .title(" Intcode Program ".to_span().into_centered_line())
                                .border_type(ratatui::widgets::BorderType::Rounded)
                                .borders(Borders::ALL),
                        )
                        .wrap(Wrap { trim: true }),
                    inner_right_layout[0],
                );
                frame.render_widget(
                    Paragraph::new(format!("{:?}", v.output)).block(
                        Block::new()
                            .border_type(ratatui::widgets::BorderType::Rounded)
                            .borders(Borders::ALL),
                    ),
                    inner_left_layout[2],
                );
                frame.render_widget(
                    Paragraph::new(format!("Head: {}\nRB: {}", v.head, v.relative_base)).block(
                        Block::new()
                            .border_type(ratatui::widgets::BorderType::Rounded)
                            .borders(Borders::ALL),
                    ),
                    inner_left_layout[0],
                );
                frame.render_widget(
                    Paragraph::new(format!("{:?}", v.input)).block(
                        Block::new()
                            .border_type(ratatui::widgets::BorderType::Rounded)
                            .borders(Borders::ALL),
                    ),
                    inner_left_layout[1],
                );

                frame.render_widget(
                    // Paragraph::new(format!("{:?}\n{:?}", v, parse_intcode(v.program[v.head])))
                    Paragraph::new(describe_intcode(v)).block(
                        Block::new()
                            .border_type(ratatui::widgets::BorderType::Rounded)
                            .borders(Borders::ALL),
                    ),
                    inner_right_layout[1],
                );
            }
            None => {
                frame.render_widget(
                    Paragraph::new("Top Right").block(
                        Block::new()
                            .title(" Intcode Program ".to_span().into_centered_line())
                            .border_type(ratatui::widgets::BorderType::Rounded)
                            .borders(Borders::ALL),
                    ),
                    inner_right_layout[0],
                );

                frame.render_widget(
                    Paragraph::new("Top Left").block(
                        Block::new()
                            .border_type(ratatui::widgets::BorderType::Rounded)
                            .borders(Borders::ALL),
                    ),
                    inner_left_layout[0],
                );
                frame.render_widget(
                    Paragraph::new("Bottom Left").block(
                        Block::new()
                            .border_type(ratatui::widgets::BorderType::Rounded)
                            .borders(Borders::ALL),
                    ),
                    inner_left_layout[2],
                );
                frame.render_widget(
                    Paragraph::new("Middle Left").block(
                        Block::new()
                            .border_type(ratatui::widgets::BorderType::Rounded)
                            .borders(Borders::ALL),
                    ),
                    inner_left_layout[1],
                );

                frame.render_widget(
                    Paragraph::new("Bottom Right").block(
                        Block::new()
                            .border_type(ratatui::widgets::BorderType::Rounded)
                            .borders(Borders::ALL),
                    ),
                    inner_right_layout[1],
                );
            }
        };
        match self.input_mode {
            InputMode::Viewing => {
                frame.render_widget(Paragraph::new("input here"), outer_layout[1])
            }
            InputMode::LoadingFile => todo!(),
        };
    }

    fn handle_events(&mut self) -> Result<()> {
        match event::read()? {
            Event::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                self.handle_key_event(key_event)
            }
            _ => {}
        };
        Ok(())
    }

    fn handle_key_event(&mut self, key_event: KeyEvent) {
        match key_event.code {
            KeyCode::Char('q') => self.exit(),
            KeyCode::Char('o') => self.open_file(),
            KeyCode::Char('c') => self.close_file(),
            KeyCode::Char('n') => self.step_forward(),
            KeyCode::Char('p') => self.step_backward(),
            KeyCode::Char('t') => self.toggle_code_nums(),
            _ => {}
        }
    }

    fn step_forward(&mut self) {
        if let Some(s) = self.intcode_state.clone() {
            self.history.push(s.clone());
            let new_state = s.step_intcode();
            self.intcode_state = Some(new_state);
        }
    }

    fn exit(&mut self) {
        self.exit = true;
    }

    fn open_file(&mut self) {
        // self.intcode_state = Some(IntcodeState::new("3,9,8,9,10,9,4,9,99,-1,8", vec![1000]));

        let mut inputstate = IntcodeState::new(
            // "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99",
            "1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1102,1,3,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1101,39,0,1004,1101,0,37,1013,1101,0,28,1001,1101,0,38,1005,1101,23,0,1008,1102,1,0,1020,1102,1,26,1010,1102,31,1,1009,1101,29,0,1015,1102,459,1,1024,1101,33,0,1007,1101,0,30,1016,1101,32,0,1002,1102,1,494,1027,1101,0,216,1029,1101,497,0,1026,1101,0,303,1022,1102,1,21,1018,1102,1,36,1006,1102,1,27,1014,1102,296,1,1023,1102,454,1,1025,1102,35,1,1003,1101,22,0,1017,1102,225,1,1028,1102,1,20,1011,1101,1,0,1021,1101,0,24,1000,1101,0,25,1019,1101,0,34,1012,109,13,21102,40,1,0,1008,1013,40,63,1005,63,203,4,187,1106,0,207,1001,64,1,64,1002,64,2,64,109,5,2106,0,10,4,213,1001,64,1,64,1105,1,225,1002,64,2,64,109,-3,1206,6,241,1001,64,1,64,1105,1,243,4,231,1002,64,2,64,109,-17,2108,30,4,63,1005,63,259,1106,0,265,4,249,1001,64,1,64,1002,64,2,64,109,14,2108,35,-9,63,1005,63,283,4,271,1105,1,287,1001,64,1,64,1002,64,2,64,109,13,2105,1,-2,1001,64,1,64,1106,0,305,4,293,1002,64,2,64,109,-28,1208,5,32,63,1005,63,327,4,311,1001,64,1,64,1106,0,327,1002,64,2,64,109,12,2102,1,0,63,1008,63,31,63,1005,63,353,4,333,1001,64,1,64,1105,1,353,1002,64,2,64,109,7,21102,41,1,-6,1008,1010,40,63,1005,63,373,1105,1,379,4,359,1001,64,1,64,1002,64,2,64,109,-4,2102,1,-6,63,1008,63,35,63,1005,63,403,1001,64,1,64,1105,1,405,4,385,1002,64,2,64,109,11,21107,42,43,-4,1005,1019,427,4,411,1001,64,1,64,1105,1,427,1002,64,2,64,109,-10,1206,7,445,4,433,1001,64,1,64,1105,1,445,1002,64,2,64,109,10,2105,1,1,4,451,1105,1,463,1001,64,1,64,1002,64,2,64,109,-14,21108,43,42,4,1005,1013,479,1106,0,485,4,469,1001,64,1,64,1002,64,2,64,109,12,2106,0,6,1106,0,503,4,491,1001,64,1,64,1002,64,2,64,109,-10,2107,30,-2,63,1005,63,521,4,509,1106,0,525,1001,64,1,64,1002,64,2,64,109,-7,2101,0,-4,63,1008,63,26,63,1005,63,549,1001,64,1,64,1106,0,551,4,531,1002,64,2,64,109,13,21107,44,43,-3,1005,1014,571,1001,64,1,64,1105,1,573,4,557,1002,64,2,64,109,-6,21108,45,45,1,1005,1012,591,4,579,1106,0,595,1001,64,1,64,1002,64,2,64,109,8,1205,2,609,4,601,1106,0,613,1001,64,1,64,1002,64,2,64,109,-11,1208,-6,34,63,1005,63,629,1106,0,635,4,619,1001,64,1,64,1002,64,2,64,109,-15,2107,33,9,63,1005,63,651,1106,0,657,4,641,1001,64,1,64,1002,64,2,64,109,9,1207,2,38,63,1005,63,677,1001,64,1,64,1106,0,679,4,663,1002,64,2,64,109,8,21101,46,0,0,1008,1010,45,63,1005,63,703,1001,64,1,64,1106,0,705,4,685,1002,64,2,64,109,-5,1201,-3,0,63,1008,63,32,63,1005,63,727,4,711,1106,0,731,1001,64,1,64,1002,64,2,64,109,-6,1207,8,34,63,1005,63,753,4,737,1001,64,1,64,1106,0,753,1002,64,2,64,109,29,1205,-8,765,1106,0,771,4,759,1001,64,1,64,1002,64,2,64,109,-18,1202,-6,1,63,1008,63,39,63,1005,63,797,4,777,1001,64,1,64,1106,0,797,1002,64,2,64,109,8,21101,47,0,0,1008,1018,47,63,1005,63,823,4,803,1001,64,1,64,1105,1,823,1002,64,2,64,109,-12,2101,0,-3,63,1008,63,35,63,1005,63,845,4,829,1106,0,849,1001,64,1,64,1002,64,2,64,109,-9,1201,5,0,63,1008,63,30,63,1005,63,869,1105,1,875,4,855,1001,64,1,64,1002,64,2,64,109,8,1202,-2,1,63,1008,63,34,63,1005,63,899,1001,64,1,64,1105,1,901,4,881,4,64,99,21101,27,0,1,21101,0,915,0,1105,1,922,21201,1,45467,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21101,942,0,0,1106,0,922,21201,1,0,-1,21201,-2,-3,1,21102,1,957,0,1105,1,922,22201,1,-1,-2,1105,1,968,22101,0,-2,-2,109,-3,2106,0,0",
            vec![0],
        );
        inputstate.head = 0;
        self.intcode_state = Some(inputstate);
    }

    fn close_file(&mut self) {
        self.intcode_state = None;
    }

    fn step_backward(&mut self) {
        if self.history.len() > 0 {
            let prev_state = self.history.pop().unwrap();
            self.intcode_state = Some(prev_state);
        }
    }

    fn toggle_code_nums(&mut self) {
        self.code_nums = match &self.code_nums {
            CodeNums::Show => CodeNums::NoShow,
            CodeNums::NoShow => CodeNums::Show,
        }
    }
}

fn describe_intcode(v: &IntcodeState) -> String {
    // format!("intcode {:?}", v.get_opcode(v.head))
    let op = v.get_opcode(v.head);
    match op {
        Opcode::Add(pm0, pm1, pm2) => {
            format!(
                "{:?}\n{}\n{}\n{}",
                op,
                describe_arg(v, pm0, 1),
                describe_arg(v, pm1, 2),
                describe_arg(v, pm2, 3)
            )
        }
        Opcode::Multiply(pm0, pm1, pm2) => {
            format!(
                "{:?}\n{}\n{}\n{}",
                op,
                describe_arg(v, pm0, 1),
                describe_arg(v, pm1, 2),
                describe_arg(v, pm2, 3)
            )
        }
        Opcode::Input(pm0) => {
            format!("{:?}\n{}", op, describe_arg(v, pm0, 1),)
        }
        Opcode::Output(pm0) => {
            format!("{:?}\n{}", op, describe_arg(v, pm0, 1),)
        }
        Opcode::JumpIfTrue(pm0, pm1) => {
            format!(
                "{:?}\n{}\n{}",
                op,
                describe_arg(v, pm0, 1),
                describe_arg(v, pm1, 2),
            )
        }
        Opcode::JumpIfFalse(pm0, pm1) => {
            format!(
                "{:?}\n{}\n{}",
                op,
                describe_arg(v, pm0, 1),
                describe_arg(v, pm1, 2),
            )
        }
        Opcode::LessThan(pm0, pm1, pm2) => {
            format!(
                "{:?}\n{}\n{}\n{}",
                op,
                describe_arg(v, pm0, 1),
                describe_arg(v, pm1, 2),
                describe_arg(v, pm2, 3)
            )
        }
        Opcode::Equals(pm0, pm1, pm2) => {
            format!(
                "{:?}\n{}\n{}\n{}",
                op,
                describe_arg(v, pm0, 1),
                describe_arg(v, pm1, 2),
                describe_arg(v, pm2, 3)
            )
        }
        Opcode::UpdateRelativeBase(pm0) => {
            format!("{:?}\n{}", op, describe_arg(v, pm0, 1))
        }
        Opcode::Stop => format!("{:?}", op),
        Opcode::Value(_) => format!("value"),
    }
}

fn describe_arg(v: &IntcodeState, pm: ParameterMode, head_offset: i64) -> String {
    let idx = v.head + head_offset;
    match pm {
        ParameterMode::Immediate => format!("value at {}: {}", idx, v.get_opcode_num(idx)),
        ParameterMode::Position => format!(
            "value at {}: {}",
            v.get_opcode_num(idx),
            v.get_opcode_num(v.get_opcode_num(idx))
        ),
        ParameterMode::Relative => format!(
            "value at {}: {}",
            v.relative_base + v.get_opcode_num(idx),
            v.get_opcode_num(v.get_opcode_num(v.relative_base + head_offset))
        ),
    }
}

fn display_intcode<'a>(v: &'a IntcodeState, app: &'a App) -> Line<'a> {
    let head = v.head;
    let head_style = Style::new().yellow().on_blue();

    // TODO: Also will need to implement looking into the code and styling the parameters and other
    // parts
    let program = v.prog_to_vec();
    let mut span_vec = vec![];
    for i in 0..program.len() {
        match app.code_nums {
            CodeNums::Show => {
                // format!("{}: ", i).into(),
                // format!("{}", v.program[i]).into(),
                let prog_str = pad_to_width(i, v.get_opcode_num(i as i64));
                span_vec.push(Span::raw(prog_str));
            }
            CodeNums::NoShow => {
                let prog_str = format!("{}", v.get_opcode_num(i as i64));
                span_vec.push(Span::raw(prog_str));
            }
        };

        if i < (program.len() - 1) {
            span_vec.push(Span::from(", "))
        }
    }

    // Formatting for the various highlights

    let opcode = v.get_opcode(v.head);
    let num_params = num_params(opcode) as usize;

    span_vec[head as usize * 2] = span_vec[head as usize * 2].clone().set_style(head_style);
    // TODO: this broke for my day 7 input?
    let styles = style_opcode(opcode);
    for i in 1..=num_params {
        span_vec[(head as usize + i) * 2] = span_vec[(head as usize + i) * 2]
            .clone()
            .set_style(styles[i - 1]);
    }

    Line::from(span_vec)
}

fn pad_to_width(i: usize, ref_str: i64) -> String {
    let i_str = format!("{}", i as i32);
    let width = format!("{}", ref_str).len() as i32;
    let n_spaces = width - (i_str.len() as i32);

    let mut output = String::new();
    for _ in 1..=n_spaces {
        output.push_str(" ");
    }
    output.push_str(&i_str);
    output
}

fn style_opcode(o: Opcode) -> Vec<Style> {
    match o {
        Opcode::Add(a, b, c) => vec![
            style_parameter_mode(a),
            style_parameter_mode(b),
            style_parameter_mode(c),
        ],
        Opcode::Multiply(a, b, c) => vec![
            style_parameter_mode(a),
            style_parameter_mode(b),
            style_parameter_mode(c),
        ],
        Opcode::Input(a) => vec![style_parameter_mode(a)],
        Opcode::Output(a) => vec![style_parameter_mode(a)],
        Opcode::JumpIfTrue(a, b) => vec![style_parameter_mode(a), style_parameter_mode(b)],
        Opcode::JumpIfFalse(a, b) => vec![style_parameter_mode(a), style_parameter_mode(b)],
        Opcode::LessThan(a, b, c) => vec![
            style_parameter_mode(a),
            style_parameter_mode(b),
            style_parameter_mode(c),
        ],
        Opcode::Equals(a, b, c) => vec![
            style_parameter_mode(a),
            style_parameter_mode(b),
            style_parameter_mode(c),
        ],
        Opcode::Stop => vec![],
        Opcode::Value(_) => vec![],
        Opcode::UpdateRelativeBase(a) => vec![style_parameter_mode(a)],
    }
}

fn style_parameter_mode(pm: ParameterMode) -> Style {
    match pm {
        ParameterMode::Immediate => Style::new().red(),
        ParameterMode::Position => Style::new().blue(),
        ParameterMode::Relative => Style::new().green(),
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use ratatui::style::Style;
//
//     #[test]
//     fn render() {
//         let app = App::default();
//         let mut buf = Buffer::empty(Rect::new(0, 0, 50, 4));
//
//         app.render(buf.area, &mut buf);
//
//         let mut expected = Buffer::with_lines(vec![
//             "┏━━━━━━━━━━━━━ Counter App Tutorial ━━━━━━━━━━━━━┓",
//             "┃                    Value: 0                    ┃",
//             "┃                                                ┃",
//             "┗━ Decrement <Left> Increment <Right> Quit <Q> ━━┛",
//         ]);
//         let title_style = Style::new().bold();
//         let counter_style = Style::new().yellow();
//         let key_style = Style::new().blue().bold();
//         expected.set_style(Rect::new(14, 0, 22, 1), title_style);
//         expected.set_style(Rect::new(28, 1, 1, 1), counter_style);
//         expected.set_style(Rect::new(13, 3, 6, 1), key_style);
//         expected.set_style(Rect::new(30, 3, 7, 1), key_style);
//         expected.set_style(Rect::new(43, 3, 4, 1), key_style);
//
//         assert_eq!(buf, expected);
//     }
// }
