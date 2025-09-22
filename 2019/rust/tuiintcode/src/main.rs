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
    widgets::{Block, Borders, Paragraph},
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

        let penultimate_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(vec![Constraint::Percentage(20), Constraint::Percentage(80)])
            .split(outer_layout[0]);

        let inner_left_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(penultimate_layout[0]);

        let inner_right_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Constraint::Percentage(70), Constraint::Percentage(30)])
            .split(penultimate_layout[1]);

        // ---- Layout ----
        // Two conditions that we match on, The Some case where we have an intcode program loaded
        // and the None case where we do not
        match &self.intcode_state {
            Some(v) => {
                frame.render_widget(
                    Paragraph::new(display_intcode(v)).block(
                        Block::new()
                            .title(" Intcode Program ".to_span().into_centered_line())
                            .border_type(ratatui::widgets::BorderType::Rounded)
                            .borders(Borders::ALL),
                    ),
                    inner_right_layout[0],
                );
                frame.render_widget(
                    Paragraph::new("Bottom Left").block(
                        Block::new()
                            .border_type(ratatui::widgets::BorderType::Rounded)
                            .borders(Borders::ALL),
                    ),
                    inner_left_layout[1],
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
                    Paragraph::new(format!("{:?}\n{:?}", v, parse_intcode(v.program[v.head])))
                        .block(
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
                    Paragraph::new("Bottom Left").block(
                        Block::new()
                            .border_type(ratatui::widgets::BorderType::Rounded)
                            .borders(Borders::ALL),
                    ),
                    inner_left_layout[1],
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
        self.intcode_state = Some(IntcodeState::new(
            "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0",
            vec![1000],
        ));
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
}

fn display_intcode(v: &IntcodeState) -> Line<'_> {
    let head = v.head;
    let head_style = Style::new().yellow().on_blue();

    // TODO: Also will need to implement looking into the code and styling the parameters and other
    // parts
    let mut span_vec = vec![];
    for i in 0..v.program.len() {
        let prog_str = format!("{}", v.program[i]);
        span_vec.push(Span::raw(prog_str));
        if i < (v.program.len() - 1) {
            span_vec.push(Span::from(", "))
        }
    }

    // Formatting for the various highlights

    let opcode = parse_intcode(v.program[head]);
    let num_params = num_params(opcode) as usize;

    span_vec[head * 2] = span_vec[head * 2].clone().set_style(head_style);
    let styles = style_opcode(opcode);
    for i in 1..=num_params {
        span_vec[(head + i) * 2] = span_vec[(head + i) * 2].clone().set_style(styles[i - 1]);
    }

    Line::from(span_vec)
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
    }
}

fn style_parameter_mode(pm: ParameterMode) -> Style {
    match pm {
        ParameterMode::Immediate => Style::new().red(),
        ParameterMode::Position => Style::new().blue(),
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
