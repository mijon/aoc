use color_eyre::Result;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind};
use intcode::IntcodeState;
// use itertools::Itertools;
use ratatui::{
    DefaultTerminal, Frame,
    buffer::Buffer,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Style, Stylize},
    symbols::border,
    text::{Line, Span, Text},
    widgets::{Block, Borders, Paragraph, Widget},
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
    counter: i32,
    exit: bool,
    history: Vec<IntcodeState>,
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
        // TODO: Refactor this out to make it simpler
        let outer_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(vec![Constraint::Percentage(20), Constraint::Percentage(80)])
            .split(frame.area());

        let inner_left_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(outer_layout[0]);

        let inner_right_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Constraint::Percentage(70), Constraint::Percentage(30)])
            .split(outer_layout[1]);

        // frame.render_widget(self, inner_right_layout[0]);
        frame.render_widget(
            Paragraph::new("Bottom Left").block(Block::new().borders(Borders::ALL)),
            inner_left_layout[1],
        );
        frame.render_widget(
            Paragraph::new("Top Left").block(Block::new().borders(Borders::ALL)),
            inner_left_layout[0],
        );
        frame.render_widget(
            match &self.intcode_state {
                Some(v) => {
                    Paragraph::new(display_intcode(v)).block(Block::new().borders(Borders::ALL))
                }
                None => Paragraph::new("Top Right").block(Block::new().borders(Borders::ALL)),
            },
            inner_right_layout[0],
        );
        frame.render_widget(
            match &self.intcode_state {
                Some(v) => {
                    Paragraph::new(format!("{:?}", v)).block(Block::new().borders(Borders::ALL))
                }
                None => Paragraph::new("Bottom Right").block(Block::new().borders(Borders::ALL)),
            },
            inner_right_layout[1],
        );
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
        self.intcode_state = Some(IntcodeState::new("3,0,4,0,99", vec![0]));
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
        if i == head {
            span_vec.push(Span::styled(prog_str, head_style))
        } else {
            span_vec.push(Span::raw(prog_str))
        }
        if i < (v.program.len() - 1) {
            span_vec.push(Span::from(", "))
        }
    }

    Line::from(span_vec)
}

impl Widget for &App {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let title = Line::from(" IntCode Viewer ".bold());
        let instructions = Line::from(vec![
            " Open File ".into(),
            "<O>".blue().bold(),
            " Quit ".into(),
            "<Q> ".blue().bold(),
        ]);
        let block = Block::bordered()
            .title(title.centered())
            .title_bottom(instructions.centered())
            .border_set(border::THICK);

        let counter_text = Text::from(vec![Line::from(vec![
            "Value: ".into(),
            self.counter.to_string().yellow(),
        ])]);

        Paragraph::new(counter_text)
            .centered()
            .block(block)
            .render(area, buf);
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
