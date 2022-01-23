extern crate mal;
use mal::Editor;
use mal::ReadlineError;
fn main() {
    let mut rl = Editor::<()>::new();
    loop {
        let r = rl.readline("user> ");
        match r {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if !line.is_empty() {
                    println!("{}", line);
                }
            }
            Err(ReadlineError::Interrupted) => {
                continue;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("{}", err);
                break;
            }
        }
    }
}
